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
#'   `"owner/repo"` format (e.g. `"opentsi/ch.kof.kofbarometer"`). Defaults to
#'   `"opentsi/ch.kof.globalbaro"`.
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
#' @param cache logical; if `TRUE` the repository is cloned to `cache_dir`
#'   once and all reads are served from the local clone. Recommended when
#'   querying many series to avoid GitHub API rate limits. Defaults to `FALSE`.
#' @param cache_dir character scalar; parent directory for the local clone.
#'   A sub-directory named after the repository is created automatically.
#'   Defaults to `"~/.cache/opentimeseries"`.
#' @param update logical; if `TRUE` and the local clone already exists, a
#'   `git pull` is run before reading. Ignored when `cache = FALSE`. Defaults
#'   to `FALSE`.
#'
#' @return When `rbind_dt = TRUE` (default): a `data.table` with at least
#'   columns `id`, `date`, and `value`. When `show_vintage_dates = TRUE` the
#'   columns `query_date` and `commit_date` are prepended. When
#'   `rbind_dt = FALSE`: a named list of such `data.table`s, one per key.
#'
#' @importFrom data.table fread rbindlist setcolorder
#' @importFrom gert git_clone git_log git_checkout git_pull
#' @importFrom fs dir_create dir_exists path path_expand dir_ls
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
#'   series = c("coincident", "leading"),
#'   date = "2024-01-01",
#'   remote_archive = "opentsi/ch.kof.globalbaro",
#'   show_vintage_dates = TRUE
#' )
#'
#' # Fetch all series from a local clone (avoids rate limits for large archives)
#' dt <- read_open_ts(
#'   date           = "2024-01-01",
#'   remote_archive = "opentsi/ch.kof.globalbaro",
#'   cache          = TRUE
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
  remote_archive = "opentsi/ch.kof.globalbaro",
  rbind_dt = TRUE,
  wide = TRUE,
  add_suffix = FALSE,
  lastn = 100,
  show_vintage_dates = FALSE,
  cache = FALSE,
  cache_dir = "~/.cache/opentimeseries",
  update = FALSE
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

    # --- find commit by date from local log ---
    log    <- git_log(repo = repo_cache, max = lastn)
    target <- as.POSIXct(paste(as.character(date), "23:59:59"), tz = "UTC")
    valid  <- log[as.POSIXct(log$time, tz = "UTC") <= target, ]

    if (nrow(valid) == 0L) {
      stop(sprintf(
        paste0(
          "No commits found at or before '%s' in local clone of '%s'.\n",
          "Try increasing `lastn` (currently %d)."
        ),
        as.Date(date), remote_archive, lastn
      ))
    }

    commit_sha  <- valid$commit[1L]
    commit_date <- as.Date(valid$time[1L])

    git_checkout(branch = commit_sha, repo = repo_cache)

    # --- resolve NULL series from local filesystem ---
    if (is.null(series)) {
      csv_files <- dir_ls(path(repo_cache, "data-raw", "csv"), glob = "*.csv")
      series    <- sub("\\.csv$", "", basename(csv_files))
    }

    if (length(series) > 5L) {
      warning(sprintf(
        "%d series requested from local cache.",
        length(series)
      ), call. = FALSE)
    }

    # --- read locally ---
    l <- vector("list", length(series))
    for (i in seq_along(series)) {
      csv_path <- path(repo_cache, "data-raw", "csv", paste0(series[i], ".csv"))
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
            "Could not read series '%s' from local cache.\nPath: %s\nOriginal error: %s",
            series[i], csv_path, conditionMessage(e)
          ))
        }
      )
      dt$id <- series[i]
      if (show_vintage_dates) {
        dt[, query_date  := as.Date(date)]
        dt[, commit_date := commit_date]
        setcolorder(dt, neworder = c("id", "query_date", "commit_date", "date", "value"))
      } else {
        setcolorder(dt, neworder = c("id", "date", "value"))
      }
      if (add_suffix) dt[, id := sprintf("%s.%s", dt$id, date)]
      l[[i]] <- dt
    }
    names(l) <- series
    return(if (rbind_dt) rbindlist(l) else l)
  }

  # -------------------------------------------------------------------------
  # REMOTE / GITHUB API PATH
  # -------------------------------------------------------------------------

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

  branch     <- commit_result$branch
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

  commit_sha  <- commit_res$hash
  commit_date <- commit_res$date

  # --- resolve NULL series to all keys in archive ---
  if (is.null(series)) {
    keys_df <- tryCatch(
      list_open_ts_keys(remote_archive, ref = branch),
      error = function(e) {
        stop(sprintf(
          paste0(
            "Could not fetch series list from archive '%s' (branch: %s).\n",
            "Original error: %s"
          ),
          remote_archive, branch, conditionMessage(e)
        ))
      }
    )
    series <- if (is.null(keys_df) || nrow(keys_df) == 0) "" else keys_df$key
  }

  if (length(series) > 20L && !isTRUE(series == "")) {
    warning(sprintf(
      paste0(
        "%d series requested via GitHub API (%d individual requests). ",
        "Consider using cache = TRUE to avoid hitting rate limits."
      ),
      length(series), length(series)
    ), call. = FALSE)
  }

  # --- build URLs and fetch ---
  gh_urls <- generate_gh_url(
    series_path    = series,
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
      dt[, query_date  := as.Date(date)]
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
