#' Read the Full Version History of a Single Time Series
#'
#' Fetches every available vintage of one time series from a GitHub-hosted
#' Open Time Series archive. Each vintage corresponds to one Git commit, so
#' the result contains one row per observation date per commit, making it
#' straightforward to compare how a series has been revised over time.
#'
#' For small numbers of vintages (`lastn <= 10`) the function fetches directly
#' from the GitHub raw-content API. For 11-20 vintages a warning is issued
#' recommending the cache. Above 20 vintages `cache = TRUE` is required — the
#' function errors with a ready-to-copy call using the cache.
#'
#' @param series character scalar; the time series subkey to query
#'   (e.g. `"coincident"`). Must be length 1.
#' @param remote_archive character scalar; the GitHub archive in
#'   `"owner/repo"` format. Defaults to `"opentsi/ch.kof.globalbaro"`.
#'   Ignored when \code{archive_path} is supplied.
#' @param dataset character scalar; the dataset identifier for a local archive
#'   (e.g. \code{"ch.kof.globalbaro"}). Required when \code{archive_path} is
#'   set.
#' @param archive_path character scalar or \code{NULL} (default). When
#'   supplied, reads from the local archive at
#'   \code{archive_path/dataset} instead of GitHub.
#' @param from `Date` or date-coercible string; earliest vintage date to
#'   include. Defaults to `NULL` (no lower bound).
#' @param to `Date` or date-coercible string; latest vintage date to include.
#'   Defaults to `Sys.Date()`.
#' @param lastn integer; number of vintages to return. Defaults to `20`.
#'   Values above 20 require `cache = TRUE`.
#' @param consolidate logical; if `TRUE` (default) only one vintage per period
#'   (as defined by `by`) is returned — the most recent commit in that period.
#'   Set to `FALSE` to get one vintage per calendar day with no further
#'   consolidation.
#' @param by character scalar; period to consolidate by when
#'   `consolidate = TRUE`. One of `"month"` (default), `"quarter"`, `"year"`,
#'   or `"day"`.
#' @param cache logical; if `TRUE` the repository is cloned to `cache_dir`
#'   and all reads are served from the local clone. Required for
#'   `lastn > 20`. Defaults to `FALSE`.
#' @param cache_dir character scalar; parent directory for the local clone.
#'   Defaults to `"~/.cache/opentimeseries"`.
#' @param update logical; if `TRUE` and a local clone exists, runs
#'   `git pull` before reading. Ignored when `cache = FALSE`. Defaults to
#'   `FALSE`.
#'
#' @return A `data.table` with columns `id`, `vintage_date`, `date`, `value`.
#'   `vintage_date` is the commit date of that version; `date` is the
#'   observation date within the series. Commits where the series file does
#'   not yet exist are silently skipped.
#'
#' @importFrom data.table fread rbindlist setcolorder
#' @importFrom gert git_clone git_log git_pull git_branch_list git_branch_create git_branch_checkout git_reset_hard
#' @importFrom fs dir_create dir_exists path path_expand
#' @export
#'
#' @examples
#' \dontrun{
#' # Fetch the last 10 vintages directly (no clone needed)
#' dt <- read_ts_history("coincident", lastn = 10)
#'
#' # Fetch full history from a local clone
#' dt <- read_ts_history(
#'   "coincident",
#'   lastn  = 200,
#'   cache  = TRUE,
#'   update = TRUE
#' )
#'
#' # Restrict to a date window
#' dt <- read_ts_history(
#'   "coincident",
#'   from  = "2023-01-01",
#'   to    = "2024-01-01",
#'   lastn = 50,
#'   cache = TRUE
#' )
#'
#' # Pivot to a revision triangle
#' triangle(dt)
#' }
read_ts_history <- function(
  series,
  remote_archive = "opentsi/ch.kof.globalbaro",
  dataset     = NULL,
  archive_path = NULL,
  from        = NULL,
  to          = Sys.Date(),
  lastn       = 100,
  consolidate = TRUE,
  by          = "month",
  cache       = FALSE,
  cache_dir   = "~/.cache/opentimeseries",
  update      = FALSE
) {
  # --- input validation ---
  if (!is.character(series) || length(series) != 1L || nchar(trimws(series)) == 0L) {
    stop("`series` must be a single non-empty string (e.g. \"coincident\").")
  }
  if (!is.character(remote_archive) || length(remote_archive) != 1L ||
      !grepl("^[^/]+/[^/]+$", remote_archive)) {
    stop(
      "`remote_archive` must be a single string in \"owner/repo\" format ",
      "(e.g. \"opentsi/ch.kof.globalbaro\")."
    )
  }
  if (!is.null(from) && anyNA(suppressWarnings(as.Date(as.character(from))))) {
    stop("`from` must be a valid date or NULL.")
  }
  if (anyNA(suppressWarnings(as.Date(as.character(to))))) {
    stop("`to` must be a valid date.")
  }
  by <- match.arg(by, c("month", "quarter", "year", "day"))

  # --- period key helper ---
  period_key <- function(dates) {
    d <- as.Date(dates)
    switch(by,
      "day"     = format(d, "%Y-%m-%d"),
      "month"   = format(d, "%Y-%m"),
      "quarter" = paste0(format(d, "%Y"), "-Q", ceiling(as.integer(format(d, "%m")) / 3)),
      "year"    = format(d, "%Y")
    )
  }

  # --- internal helper: read one CSV source and attach metadata ---
  read_vintage <- function(csv_source, vintage_date, series_id) {
    dt <- tryCatch(
      {
        result <- fread(csv_source, showProgress = FALSE)
        if (!"value" %in% names(result)) stop("no 'value' column")
        if ("time" %in% names(result) && !"date" %in% names(result)) {
          names(result)[names(result) == "time"] <- "date"
        }
        result
      },
      error = function(e) NULL
    )
    if (is.null(dt)) return(NULL)
    dt[, id           := series_id]
    dt[, vintage_date := as.Date(vintage_date)]
    setcolorder(dt, c("id", "vintage_date", "date", "value"))
    dt
  }

  # -------------------------------------------------------------------------
  # LOCAL ARCHIVE PATH
  # -------------------------------------------------------------------------
  if (!is.null(archive_path)) {
    if (is.null(dataset) || !nzchar(dataset)) {
      stop("`dataset` is required when `archive_path` is supplied.")
    }
    repo_path <- path(path_expand(archive_path), dataset)
    if (!dir_exists(path(repo_path, ".git"))) {
      stop(sprintf(
        "No local archive found for dataset '%s' at '%s'. Run write_local_open_ts() first.",
        dataset, path_expand(archive_path)
      ))
    }

    all_branches   <- git_branch_list(repo = repo_path)$name
    default_branch <- all_branches[all_branches %in% c("main", "master")][1L]
    log <- git_log(repo = repo_path, ref = default_branch, max = 10000L)
    log <- log[order(as.POSIXct(log$time, tz = "UTC"), decreasing = TRUE), ]

    to_posix <- as.POSIXct(paste(as.character(to), "23:59:59"), tz = "UTC")
    log <- log[as.POSIXct(log$time, tz = "UTC") <= to_posix, ]
    if (!is.null(from)) {
      from_posix <- as.POSIXct(paste(as.character(from), "00:00:00"), tz = "UTC")
      log <- log[as.POSIXct(log$time, tz = "UTC") >= from_posix, ]
    }
    log <- log[!duplicated(as.Date(log$time, tz = "UTC")), ]
    if (consolidate) log <- log[!duplicated(period_key(log$time)), ]
    log <- head(log, lastn)

    if (nrow(log) == 0L) {
      stop(sprintf(
        "No commits found in the specified date range for '%s' in '%s'.",
        series, dataset
      ))
    }

    tmp_branch <- "opentimeseries-read"
    if (tmp_branch %in% git_branch_list(repo = repo_path)$name) {
      git_branch_checkout(tmp_branch, repo = repo_path)
      git_reset_hard(log$commit[1L], repo = repo_path)
    } else {
      git_branch_create(tmp_branch, ref = log$commit[1L], checkout = TRUE,
                        repo = repo_path)
    }

    l <- vector("list", nrow(log))
    for (i in seq_len(nrow(log))) {
      git_reset_hard(log$commit[i], repo = repo_path)
      csv_path <- path(repo_path, "data-raw", "csv", paste0(series, ".csv"))
      l[[i]] <- read_vintage(csv_path, as.Date(log$time[i], tz = "UTC"), series)
    }

    result <- rbindlist(Filter(Negate(is.null), l))
    if (nrow(result) == 0L) {
      stop(sprintf("Series '%s' was not found in any commit in '%s'.", series, dataset))
    }
    return(result)
  }

  # --- remote-path threshold enforcement ---
  if (!cache) {
    if (lastn > 20L) {
      stop(sprintf(
        paste0(
          "Fetching %d vintages via the GitHub API would make %d individual HTTP requests.\n",
          "Use cache = TRUE for large history reads:\n\n",
          "  read_ts_history(\"%s\", remote_archive = \"%s\", lastn = %d, cache = TRUE)"
        ),
        lastn, lastn, series, remote_archive, lastn
      ))
    } else if (lastn > 10L) {
      warning(sprintf(
        "%d vintages requested via GitHub API (%d HTTP requests). Consider cache = TRUE.",
        lastn, lastn
      ), call. = FALSE)
    }
  }

  # -------------------------------------------------------------------------
  # LOCAL CACHE PATH
  # -------------------------------------------------------------------------
  if (cache) {
    repo_cache <- path(path_expand(cache_dir), basename(remote_archive))
    dir_create(repo_cache, recurse = TRUE)

    if (!dir_exists(path(repo_cache, ".git"))) {
      message(sprintf(
        "Cloning '%s' into local cache, this may take a moment...",
        remote_archive
      ))
      git_clone(
        url  = sprintf("https://github.com/%s", remote_archive),
        path = repo_cache
      )
    } else if (update) {
      git_pull(repo = repo_cache)
    }

    log <- git_log(repo = repo_cache, max = lastn * 5L)

    to_posix <- as.POSIXct(paste(as.character(to), "23:59:59"), tz = "UTC")
    log <- log[as.POSIXct(log$time, tz = "UTC") <= to_posix, ]
    if (!is.null(from)) {
      from_posix <- as.POSIXct(paste(as.character(from), "00:00:00"), tz = "UTC")
      log <- log[as.POSIXct(log$time, tz = "UTC") >= from_posix, ]
    }
    # always dedup to one per calendar day first; then consolidate by period
    log <- log[!duplicated(as.Date(log$time)), ]
    if (consolidate) log <- log[!duplicated(period_key(log$time)), ]
    log <- head(log, lastn)

    if (nrow(log) == 0L) {
      stop(sprintf(
        "No commits found in the specified date range for '%s'. Try widening `from`/`to` or increasing `lastn`.",
        remote_archive
      ))
    }

    tmp_branch <- "opentimeseries-read"
    if (tmp_branch %in% git_branch_list(repo = repo_cache)$name) {
      git_branch_checkout(tmp_branch, repo = repo_cache)
      git_reset_hard(log$commit[1L], repo = repo_cache)
    } else {
      git_branch_create(tmp_branch, ref = log$commit[1L], checkout = TRUE, repo = repo_cache)
    }

    l <- vector("list", nrow(log))
    for (i in seq_len(nrow(log))) {
      git_reset_hard(log$commit[i], repo = repo_cache)
      csv_path <- path(repo_cache, "data-raw", "csv", paste0(series, ".csv"))
      l[[i]] <- read_vintage(csv_path, as.Date(log$time[i]), series)
    }

    result <- rbindlist(Filter(Negate(is.null), l))
    if (nrow(result) == 0L) {
      stop(sprintf(
        "Series '%s' was not found in any commit in the specified range.",
        series
      ))
    }
    return(result)
  }

  # -------------------------------------------------------------------------
  # REMOTE / GITHUB API PATH
  # -------------------------------------------------------------------------
  commit_result <- tryCatch(
    get_commit_dates(remote_archive = remote_archive, lastn = lastn * 5L),
    error = function(e) {
      stop(sprintf(
        paste0(
          "Could not fetch commit history from archive '%s'.\n",
          "Check the archive name and your internet connection.\n",
          "Original error: %s"
        ),
        remote_archive, conditionMessage(e)
      ))
    }
  )

  commits <- commit_result$commits

  to_posix <- as.POSIXct(paste(as.character(to), "23:59:59"), tz = "UTC")
  commits <- commits[as.POSIXct(commits$date, tz = "UTC") <= to_posix, ]
  if (!is.null(from)) {
    from_posix <- as.POSIXct(paste(as.character(from), "00:00:00"), tz = "UTC")
    commits <- commits[as.POSIXct(commits$date, tz = "UTC") >= from_posix, ]
  }
  # always dedup to one per calendar day first; then consolidate by period
  commits <- commits[!duplicated(as.Date(commits$date)), ]
  if (consolidate) commits <- commits[!duplicated(period_key(commits$date)), ]
  commits <- head(commits, lastn)

  if (nrow(commits) == 0L) {
    stop(sprintf(
      "No commits found in the specified date range for '%s'. Try widening `from`/`to` or increasing `lastn`.",
      remote_archive
    ))
  }

  l <- vector("list", nrow(commits))
  for (i in seq_len(nrow(commits))) {
    url <- generate_gh_url(
      series_path    = series,
      remote_archive = remote_archive,
      sha            = commits$hash[i]
    )
    l[[i]] <- read_vintage(url, as.Date(commits$date[i]), series)
  }

  result <- rbindlist(Filter(Negate(is.null), l))
  if (nrow(result) == 0L) {
    stop(sprintf(
      "Series '%s' was not found in any commit in the specified range.",
      series
    ))
  }
  result
}
