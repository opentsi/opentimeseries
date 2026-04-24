#' Read Time Series from an Open Time Series Archive
#'
#' Reads one or more time series from a remote GitHub-hosted Open Time Series
#' (OpenTSI) archive. The function resolves the most recent Git commit at or
#' before `date` and fetches each requested series from that snapshot, making
#' it straightforward to retrieve reproducible, point-in-time data.
#'
#' @param series character vector of time series subkeys to query within the
#'   archive (e.g. `"coincident"` or `c("coincident", "leading")`). Defaults
#'   to `NULL`, which fetches all series available in the archive.
#' @param date `Date` or date-coercible string giving the point in time for
#'   which to retrieve the series. The function selects the most recent commit
#'   at or before this date. Defaults to `Sys.Date()`.
#' @param remote_archive character scalar; the GitHub archive in
#'   `"owner/repo"` format (e.g. `"opentsi/kofethz"`). Defaults to
#'   `"opentsi"`.
#' @param rbind_dt logical; if `TRUE` (default) the per-series `data.table`s
#'   are row-bound into a single `data.table`. If `FALSE` a named list is
#'   returned instead.
#' @param wide logical; reserved for future use (currently unused). Defaults
#'   to `TRUE`.
#' @param add_suffix logical; if `TRUE` the query date is appended to each
#'   series `id` separated by a dot. Defaults to `FALSE`.
#' @param lastn integer; number of recent commits to inspect when searching
#'   for the right version. Increase this value when querying old dates that
#'   fall outside the default window. Defaults to `100`.
#' @param show_vintage_dates logical; if `TRUE` two extra columns are added:
#'   `query_date` (the value of `date`) and `commit_date` (the date of the
#'   matched Git commit). Defaults to `FALSE`.
#'
#' @return When `rbind_dt = TRUE` (default): a `data.table` with at least
#'   columns `id`, `date`, and `value`. When `show_vintage_dates = TRUE` the
#'   columns `query_date` and `commit_date` are prepended. When
#'   `rbind_dt = FALSE`: a named list of such `data.table`s, one per key.
#'
#' @importFrom data.table fread rbindlist setcolorder
#' @export
#'
#' @examples
#' \dontrun{
#' # Fetch a single series at today's version
#' dt <- read_open_ts(
#'   series         = "coincident",
#'   remote_archive = "opentsi/ch.kof.globalbaro"
#' )
#' head(dt)
#'
#' # Fetch multiple series as of a specific past date, with vintage columns
#' dt <- read_open_ts(
#'   series = c(
#'     "coincident",
#'     "leading"
#'   ),
#'   date = "2024-01-01",
#'   remote_archive = "opentsi/ch.kof.globalbaro",
#'   show_vintage_dates = TRUE
#' )
#'
#' # Return a named list instead of a single bound table
#' lst <- read_open_ts(
#'   series         = "coincident",
#'   remote_archive = "opentsi/ch.kof.globalbaro",
#'   rbind_dt       = FALSE
#' )
#' }
read_open_ts <- function(
  series = NULL,
  date = Sys.Date(),
  remote_archive = "opentsi",
  rbind_dt = TRUE,
  wide = TRUE,
  add_suffix = FALSE,
  lastn = 100,
  show_vintage_dates = FALSE
) {
  # --- input validation ---
  if (!is.character(remote_archive) || length(remote_archive) != 1 ||
    !grepl("^[^/]+/[^/]+$", remote_archive)) {
    stop(
      "`remote_archive` must be a single string in \"owner/repo\" format ",
      "(e.g. \"opentsi/ch.kof.globalbaro\")."
    )
  }
  if (anyNA(suppressWarnings(as.Date(as.character(date))))) {
    stop(
      "`date` must be a valid date. ",
      "Use a Date object (e.g. Sys.Date()) or an ISO string (e.g. \"2024-01-01\")."
    )
  }

  if (!is.null(series) && (!is.character(series) || length(series) == 0)) {
    stop("`series` must be a non-empty character vector of time series subkeys.")
  }

  # --- resolve commit (also discovers the default branch) ---
  commit_result <- tryCatch(
    get_commit_dates(remote_archive = remote_archive, lastn = lastn),
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

  branch <- commit_result$branch
  commit_res <- get_commit_by_date(commit_result$commits, d = date)

  if (nrow(commit_res) == 0) {
    stop(sprintf(
      paste0(
        "No commits found at or before '%s' in archive '%s'.\n",
        "Try an earlier date or increase `lastn` (currently %d)."
      ),
      as.Date(date), remote_archive, lastn
    ))
  }

  commit_sha <- commit_res$hash
  commit_date <- commit_res$date

  # --- resolve NULL series to all keys in archive ---
  if (is.null(series)) {
    meta <- tryCatch(
      read_meta(remote_archive, ref = branch),
      error = function(e) {
        stop(sprintf(
          paste0(
            "Could not read metadata from '%s' (branch: %s) to determine available series.\n",
            "Original error: %s"
          ),
          remote_archive, branch, conditionMessage(e)
        ))
      }
    )
    paths <- get_paths(meta)
    if (length(paths) == 0) {
      # Flat archive: single series at root, no subdirectory hierarchy
      # find name of the series, if only 1 at root
      series <- ""
    } else {
      series <- sapply(paths, paste, collapse = ".")
    }
  }
  # print(series)

  # --- build URLs and fetch ---
  series_paths <- key_to_path(series)
  gh_urls <- generate_gh_url(
    series_path    = series_paths,
    remote_archive = remote_archive,
    sha            = commit_sha
  )

  l <- vector("list", length(gh_urls))
  for (i in seq_along(gh_urls)) {
    dt <- tryCatch(
      {
        result <- fread(gh_urls[i])
        if (!"value" %in% names(result)) {
          stop("series not found or has unexpected format")
        }
        if ("time" %in% names(result) && !"date" %in% names(result)) {
          names(result)[names(result) == "time"] <- "date"
        }
        result
      },
      error = function(e) {
        stop(sprintf(
          paste0(
            "Could not read series '%s' from archive '%s'.\n",
            "Check that the key exists in the archive.\n",
            "URL attempted: %s\n",
            "Original error: %s"
          ),
          series[i], remote_archive, gh_urls[i], conditionMessage(e)
        ))
      }
    )
    dt$id <- if (nchar(series[i]) == 0) remote_archive else series[i]
    if (show_vintage_dates) {
      dt[, query_date := as.Date(date)]
      dt[, commit_date := as.Date(commit_date)]
      setcolorder(dt, neworder = c("id", "query_date", "commit_date", "date", "value"))
    } else {
      setcolorder(dt, neworder = c("id", "date", "value"))
    }
    if (add_suffix) {
      dt[, id := sprintf("%s.%s", dt$id, date)]
    }
    l[[i]] <- dt
  }
  names(l) <- series

  if (rbind_dt) rbindlist(l) else l
}
