#' Read Time Series from a Local OpenTSI Archive
#'
#' Reads one or more time series previously written by
#' \code{\link{write_local_open_ts}} from a local git-backed archive. Supports
#' point-in-time reads: supply \code{date} to retrieve the version of the series
#' as it existed at or before that date. Returns the same \code{data.table}
#' format as \code{\link{read_open_ts}}.
#'
#' @param series Character vector of series identifiers to read
#'   (e.g. \code{"leading"} or \code{c("leading", "coincident")}).
#' @param dataset Character scalar; the dataset identifier
#'   (e.g. \code{"ch.kof.globalbaro"}).
#' @param archive_path Character scalar; parent directory for local archives.
#'   Defaults to \code{"~/.local_archives"}.
#' @param date \code{Date} or date-coercible string; the point in time for
#'   which to retrieve the series. The most recent commit at or before this
#'   date is used. Defaults to \code{Sys.Date()} (latest version).
#' @param lastn Integer; number of recent commits to inspect when searching
#'   for the right version. Increase when querying old dates. Defaults to
#'   \code{100}.
#' @param show_vintage_dates Logical; if \code{TRUE} adds \code{query_date}
#'   \code{vintage_date} column (the git commit author date). Defaults to \code{FALSE}.
#' @param rbind_dt Logical; if \code{TRUE} (default) returns a single bound
#'   \code{data.table}. If \code{FALSE} returns a named list of
#'   \code{data.table}s, one per series.
#'
#' @return A \code{data.table} with columns \code{id}, \code{date}, and
#'   \code{value}, or a named list of such tables when \code{rbind_dt = FALSE}.
#'
#' @importFrom data.table fread rbindlist setcolorder
#' @importFrom fs dir_exists path path_expand
#' @importFrom gert git_branch_checkout git_branch_create git_branch_list git_log git_reset_hard
#' @export
#'
#' @examples
#' \dontrun{
#' # Latest version
#' out <- read_local_open_ts("leading", dataset = "ch.kof.globalbaro")
#'
#' # Point-in-time read
#' out <- read_local_open_ts(
#'   series       = "leading",
#'   dataset      = "ch.kof.globalbaro",
#'   date         = "2024-01-01",
#'   archive_path = "~/.local_archives"
#' )
#' }
read_local_open_ts <- function(
  series,
  dataset,
  archive_path = "~/.local_archives",
  date = Sys.Date(),
  lastn = 100,
  show_vintage_dates = FALSE,
  rbind_dt = TRUE
) {
  if (!is.character(series) || length(series) == 0L) {
    stop("`series` must be a non-empty character vector.")
  }
  if (!is.character(dataset) || length(dataset) != 1L || nchar(dataset) == 0L) {
    stop("`dataset` must be a single non-empty character string.")
  }
  if (anyNA(suppressWarnings(as.Date(as.character(date))))) {
    stop("`date` must be a valid date, e.g. \"2024-01-01\" or Sys.Date().")
  }

  repo_path <- path(path_expand(archive_path), dataset)

  if (!dir_exists(path(repo_path, ".git"))) {
    stop(sprintf(
      "No local archive found for dataset '%s' at '%s'. Run write_local_open_ts() to create one first.",
      dataset, path_expand(archive_path)
    ))
  }

  # --- resolve commit by date ---
  # Always read the log from the default branch so point-in-time reads work
  # correctly even when the repo is currently checked out at an older commit
  # on the opentimeseries-read branch.
  all_branches   <- git_branch_list(repo = repo_path)$name
  default_branch <- all_branches[all_branches %in% c("main", "master")][1L]
  log    <- git_log(repo = repo_path, ref = default_branch, max = lastn)
  target <- as.POSIXct(paste(as.character(date), "23:59:59"), tz = "UTC")
  valid  <- log[as.POSIXct(log$time, tz = "UTC") <= target, ]

  if (nrow(valid) == 0L) {
    stop(sprintf(
      "No commits found at or before '%s' in local archive '%s'.\nTry an earlier date or increase `lastn` (currently %d).",
      as.Date(date), dataset, lastn
    ))
  }

  valid       <- valid[order(as.POSIXct(valid$time, tz = "UTC"), decreasing = TRUE), ]
  commit_sha  <- valid$commit[1L]
  commit_date <- as.Date(valid$time[1L])

  tmp_branch <- "opentimeseries-read"
  if (tmp_branch %in% git_branch_list(repo = repo_path)$name) {
    git_branch_checkout(tmp_branch, repo = repo_path)
    git_reset_hard(commit_sha, repo = repo_path)
  } else {
    git_branch_create(tmp_branch, ref = commit_sha, checkout = TRUE, repo = repo_path)
  }

  # --- read series ---
  l <- vector("list", length(series))
  for (i in seq_along(series)) {
    csv_path <- path(repo_path, "data-raw", "csv", paste0(series[i], ".csv"))
    dt <- tryCatch(
      {
        result <- fread(csv_path)
        if (!"value" %in% names(result)) stop("unexpected format: no 'value' column")
        if ("time" %in% names(result) && !"date" %in% names(result)) {
          names(result)[names(result) == "time"] <- "date"
        }
        result
      },
      error = function(e) {
        stop(sprintf(
          "Could not read series '%s' from local archive '%s'.\nPath: %s\nOriginal error: %s",
          series[i], dataset, csv_path, conditionMessage(e)
        ))
      }
    )
    dt$id <- series[i]
    if (show_vintage_dates) {
      dt[, vintage_date := commit_date]
      setcolorder(dt, neworder = c("id", "vintage_date", "date", "value"))
    } else {
      setcolorder(dt, neworder = c("id", "date", "value"))
    }
    l[[i]] <- dt
  }
  names(l) <- series

  if (rbind_dt) rbindlist(l) else l
}
