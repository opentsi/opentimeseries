#' Write One or More Time Series to a Local OpenTSI Archive
#'
#' Writes one or more time series to a local git-backed archive using git
#' commits as vintage snapshots. The archive directory is initialised as a git
#' repository automatically on the first call.
#'
#' Three calling conventions are supported:
#' \itemize{
#'   \item \strong{Vintage history} — \code{x} has a \code{vintage_date} column
#'     (e.g. output of \code{\link{read_ts_history}}): one commit per unique
#'     vintage date, backdated to that date. Already-committed vintage dates are
#'     skipped (idempotent).
#'   \item \strong{Snapshot} — \code{x} has no \code{vintage_date} column: all
#'     series are written in a single commit timestamped to \code{vintage_date}
#'     (default \code{Sys.Date()}). No commit is made if nothing changed.
#' }
#'
#' @param x One of:
#'   \itemize{
#'     \item Long-format \code{data.table}/\code{data.frame} with columns
#'       \code{id}, \code{vintage_date}, \code{date}, \code{value} — as
#'       returned by \code{\link{read_ts_history}}. Triggers one backdated
#'       commit per unique \code{vintage_date}.
#'     \item Long-format \code{data.table}/\code{data.frame} with columns
#'       \code{id}, \code{date}, \code{value} — as returned by
#'       \code{\link{read_open_ts}}. Written as a single snapshot.
#'     \item Named \code{list} of \code{data.frame}s/\code{data.table}s with
#'       \code{date} and \code{value} columns. Written as a single snapshot.
#'     \item Single \code{data.frame}/\code{data.table} with \code{date} and
#'       \code{value} columns. \code{series} parameter required.
#'     \item Base-R \code{ts} object (annual, quarterly, or monthly).
#'       \code{series} parameter required.
#'   }
#' @param series Character scalar; series identifier (CSV filename without
#'   extension). Required only for single data frame or \code{ts} input.
#' @param dataset Character scalar; dataset identifier used as the archive
#'   subdirectory (e.g. \code{"ch.kof.globalbaro"}).
#' @param archive_path Character scalar; parent directory for local archives.
#'   Defaults to \code{"~/.local_archives"}.
#' @param vintage_date A \code{Date} or date string used as the commit
#'   timestamp when \code{x} has no \code{vintage_date} column. Defaults to
#'   \code{Sys.Date()}.
#' @param commit_message Character scalar; git commit message.
#'   Defaults to \code{"timeseries updated"}.
#'
#' @return Invisibly returns the paths of the written CSV files.
#'
#' @importFrom data.table as.data.table fwrite
#' @importFrom fs dir_create dir_exists path path_expand
#' @importFrom gert git_add git_commit git_init git_log git_signature git_status
#' @export
#'
#' @examples
#' \dontrun{
#' # Single series snapshot
#' dt <- data.frame(
#'   date  = seq(as.Date("2020-01-01"), by = "month", length.out = 12),
#'   value = rnorm(12)
#' )
#' write_local_open_ts(dt, series = "leading", dataset = "ch.kof.globalbaro")
#'
#' # Snapshot from read_open_ts output — one commit, all series
#' long_dt <- read_open_ts(c("leading", "coincident"),
#'                         remote_archive = "opentsi/ch.kof.globalbaro")
#' write_local_open_ts(long_dt, dataset = "ch.kof.globalbaro")
#'
#' # Full vintage history from read_ts_history — one backdated commit per vintage
#' hist <- read_ts_history("leading", remote_archive = "opentsi/ch.kof.globalbaro")
#' write_local_open_ts(hist, dataset = "ch.kof.globalbaro")
#' }
write_local_open_ts <- function(
  x,
  series       = NULL,
  dataset,
  archive_path  = "~/.local_archives",
  vintage_date  = Sys.Date(),
  commit_message = "timeseries updated"
) {
  if (!is.character(dataset) || length(dataset) != 1L || nchar(dataset) == 0L) {
    stop("`dataset` must be a single non-empty character string.")
  }

  repo_path <- path(path_expand(archive_path), dataset)
  csv_dir   <- path(repo_path, "data-raw", "csv")

  dir_create(repo_path, recurse = TRUE)
  if (!dir_exists(path(repo_path, ".git"))) {
    message(sprintf("Initialising new local archive at '%s'.", repo_path))
    git_init(repo_path)
  }
  dir_create(csv_dir, recurse = TRUE)

  dt_in <- if (is.list(x) && !is.data.frame(x)) NULL else as.data.table(x)

  if (!is.null(dt_in) && "vintage_date" %in% names(dt_in)) {
    write_vintage_history(dt_in, csv_dir, repo_path, commit_message)
  } else {
    sl <- to_series_list(x, series)
    write_vintage_snapshot(sl, as.Date(vintage_date), csv_dir, repo_path,
                           commit_message)
  }
}


# --- internal: one backdated commit per unique vintage_date ------------------

write_vintage_history <- function(dt, csv_dir, repo_path, commit_message) {
  dt <- as.data.table(dt)
  dt[, vintage_date := as.Date(vintage_date)]

  existing <- committed_dates(repo_path)
  vintage_dates <- sort(unique(dt$vintage_date))

  csv_paths <- character(0)

  for (vd in as.character(vintage_dates)) {
    vd <- as.Date(vd)

    if (vd %in% existing) {
      message(sprintf("Vintage %s already committed — skipping.", vd))
      next
    }

    subset_dt <- dt[vintage_date == vd]
    ids <- unique(subset_dt$id)
    sl  <- lapply(setNames(ids, ids), function(i) {
      normalise_ts_input(subset_dt[id == i])
    })

    paths <- write_csvs(sl, csv_dir)
    csv_paths <- c(csv_paths, paths)

    rel_paths <- file.path("data-raw", "csv", paste0(names(sl), ".csv"))
    git_add(files = rel_paths, repo = repo_path)

    staged <- git_status(repo = repo_path)
    if (!any(staged$staged)) next

    git_commit(
      message   = commit_message,
      author    = make_sig(vd),
      committer = make_sig(vd),
      repo      = repo_path
    )
    message(sprintf("Committed vintage %s (%d series).", vd, length(sl)))
  }

  invisible(csv_paths)
}


# --- internal: single commit for all series at one vintage date --------------

write_vintage_snapshot <- function(series_list, vintage_date, csv_dir,
                                   repo_path, commit_message) {
  csv_paths <- write_csvs(series_list, csv_dir)
  rel_paths <- file.path("data-raw", "csv", paste0(names(series_list), ".csv"))

  git_add(files = rel_paths, repo = repo_path)
  staged <- git_status(repo = repo_path)
  staged <- staged[staged$staged, ]

  if (nrow(staged) == 0L) {
    message(sprintf(
      "%d series in '%s' unchanged — no commit made.",
      length(series_list), basename(dirname(dirname(csv_dir)))
    ))
    return(invisible(csv_paths))
  }

  git_commit(
    message   = commit_message,
    author    = make_sig(vintage_date),
    committer = make_sig(vintage_date),
    repo      = repo_path
  )
  invisible(csv_paths)
}


# --- helpers -----------------------------------------------------------------

write_csvs <- function(series_list, csv_dir) {
  paths <- file.path(csv_dir, paste0(names(series_list), ".csv"))
  for (i in seq_along(series_list)) fwrite(series_list[[i]], paths[i])
  paths
}

make_sig <- function(date) {
  git_signature(
    name  = "opentimeseries",
    email = "opentimeseries@local",
    time  = as.POSIXct(paste(as.character(as.Date(date)), "12:00:00"),
                       tz = "UTC")
  )
}

committed_dates <- function(repo_path) {
  log <- tryCatch(git_log(repo = repo_path, max = 10000L),
                  error = function(e) NULL)
  if (is.null(log) || nrow(log) == 0L) return(as.Date(character(0)))
  as.Date(log$time, tz = "UTC")
}

to_series_list <- function(x, series) {
  if (is.list(x) && !is.data.frame(x)) {
    if (is.null(names(x)) || any(names(x) == "")) {
      stop("When `x` is a list, all elements must be named (each name becomes a series identifier).")
    }
    return(lapply(x, normalise_ts_input))
  }

  dt <- if (inherits(x, "ts")) ts_to_dt(x) else as.data.table(x)

  if ("id" %in% names(dt)) {
    ids <- unique(dt$id)
    return(lapply(setNames(ids, ids), function(i) normalise_ts_input(dt[dt$id == i])))
  }

  if (is.null(series) || !nzchar(series)) {
    stop("`series` is required when `x` is a single data frame or ts object without an `id` column.")
  }
  setNames(list(normalise_ts_input(dt)), series)
}

normalise_ts_input <- function(x) {
  dt <- if (inherits(x, "ts")) ts_to_dt(x) else as.data.table(x)
  if ("time" %in% names(dt) && !"date" %in% names(dt)) {
    names(dt)[names(dt) == "time"] <- "date"
  }
  if (!all(c("date", "value") %in% names(dt))) {
    stop("`x` must have columns 'date' and 'value' (or 'time' and 'value').")
  }
  dt[, c("date", "value"), with = FALSE]
}

ts_to_dt <- function(x) {
  tt   <- as.numeric(time(x))
  freq <- frequency(x)

  dates <- if (freq == 1L) {
    as.Date(sprintf("%04d-01-01", as.integer(tt)))
  } else if (freq == 4L) {
    year    <- as.integer(floor(tt))
    quarter <- as.integer(round((tt - year) * 4)) + 1L
    month   <- (quarter - 1L) * 3L + 1L
    as.Date(sprintf("%04d-%02d-01", as.integer(year), month))
  } else if (freq == 12L) {
    year  <- as.integer(floor(tt))
    month <- as.integer(round((tt - year) * 12)) + 1L
    as.Date(sprintf("%04d-%02d-01", as.integer(year), month))
  } else {
    stop(sprintf(
      "Unsupported ts frequency %d. Convert to a data.frame with 'date' and 'value' columns first.",
      freq
    ))
  }

  data.table::data.table(date = dates, value = as.numeric(x))
}
