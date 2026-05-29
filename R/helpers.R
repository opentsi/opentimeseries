#' Get a List of All Tags from a Remote Time Series Archive
#'
#' For now, opentimeseries only supports GitHub as an arvchive host.
#' We're working to suport other hosts including self-hosted git platforms.
#'
#' @importFrom jsonlite fromJSON
#' @export
tags_list <- function(remote_archive) {
  api_url <- sprintf(
    "https://api.github.com/repos/%s/tags",
    remote_archive
  )
  tags <- fromJSON(api_url, simplifyVector = FALSE)
  sapply(tags, "[[", "name")
}


#' Get Commit Hashes and Dates from Remote Repository
#'
#' Tries `main` then `master` and returns both the commit table and the
#' branch name that succeeded, so callers can reuse it (e.g. for `read_meta`).
#'
#' @return A named list with `$commits` (a `data.table` of `hash` and `date`)
#'   and `$branch` (the branch name that was found, either `"main"` or
#'   `"master"`).
#' @importFrom httr2 request req_headers req_error req_perform resp_body_json
#' @export
get_commit_dates <- function(remote_archive = "opentsi/kofethz",
                             lastn = 100) {
  for (branch in c("main", "master")) {
    url <- sprintf(
      "https://api.github.com/repos/%s/commits?sha=%s&per_page=%d",
      remote_archive, branch, lastn
    )
    resp <- request(url) |>
      req_headers(Accept = "application/vnd.github.v3+json") |>
      req_error(is_error = \(r) FALSE) |>
      req_perform()
    if (resp$status_code == 200) break
  }

  if (resp$status_code != 200) {
    stop(sprintf(
      "Archive '%s' not found or not accessible (HTTP %d).",
      remote_archive, resp$status_code
    ))
  }

  res <- resp_body_json(resp)

  commits <- rbindlist(lapply(res, function(x) {
    list(
      hash = x$sha,
      date = as.POSIXct(x$commit$author$date,
        format = "%Y-%m-%dT%H:%M:%SZ",
        tz = "UTC"
      )
    )
  }))

  list(commits = commits, branch = branch)
}

# dd = commit dates, d = date
get_commit_by_date <- function(dd, d) {
  # TODO:
  # I guess we're not quite there yet.
  # date conversion are tricky and dependent on the default time zone on
  # your system which may differ from what git does...
  # Github gives back Zulu (hence the Z) time zone = UTC.
  # so we're doing the right thing by doing all comparisons in UTC

  # if d only yyyy-mm-dd not hh:mm:ss -> convert d to 23:59:59
  if (!grepl("\\d{2}:\\d{2}:\\d{2}$", as.POSIXct(d))) {
    d <- paste(d, "23:59:59")
  }
  filtered <- dd[as.POSIXct(d) > dd$date, ]
  index_of_max <- which.max(as.numeric(filtered$date))
  return(filtered[index_of_max, ])
}

#' Find the Past Version That Is Closest to a Given Date
#'
#' @export
find_version <- function(
    ref_date, tags,
    return_as_tag = FALSE) {
  d <- as.Date(gsub("v", "", tags))
  out <- max(d[ref_date > d])
  if (return_as_tag) {
    out <- sprintf("v%s", out)
  }
  out
}


#' Show Entire History of a Series in a Wide Format Table
#'
#'
#' @importFrom data.table dcast
#' @export
triangle <- function(dt) {
  d <- dcast(dt, formula = date ~ id, value.var = "value")
  names(d) <- gsub("(.+)(v[0-9]{4})", "\\2", names(d))
  d
}

#' Pull the Latest Changes into a Local Archive Cache
#'
#' Runs `git pull` on a previously cloned archive. Call this to refresh the
#' local cache without re-cloning from scratch. Equivalent to
#' `read_open_ts(..., cache = TRUE, update = TRUE)` but usable independently.
#'
#' @param remote_archive character scalar in `"owner/repo"` format.
#' @param cache_dir character scalar; parent cache directory.
#'   Defaults to `"~/.cache/opentimeseries"`.
#'
#' @importFrom gert git_pull
#' @importFrom fs path path_expand dir_exists
#' @export
update_cache <- function(remote_archive,
                         cache_dir = "~/.cache/opentimeseries") {
  repo_cache <- path(path_expand(cache_dir), basename(remote_archive))
  if (!dir_exists(path(repo_cache, ".git"))) {
    stop(sprintf(
      "No local clone found for '%s'. Run read_open_ts() with cache = TRUE first.",
      remote_archive
    ))
  }
  git_pull(repo = repo_cache)
  invisible(repo_cache)
}


#' Wipe the Local Cache for One or All Archives
#'
#' Deletes the local clone(s) from the cache directory. After wiping, the next
#' call to `read_open_ts(..., cache = TRUE)` will re-clone automatically.
#'
#' @param remote_archive character scalar in `"owner/repo"` format, or `NULL`
#'   (default) to wipe the entire cache directory.
#' @param cache_dir character scalar; parent cache directory.
#'   Defaults to `"~/.cache/opentimeseries"`.
#'
#' @importFrom fs path path_expand dir_exists dir_delete
#' @export
wipe_cache <- function(remote_archive = NULL,
                       cache_dir = "~/.cache/opentimeseries") {
  base <- path_expand(cache_dir)
  target <- if (is.null(remote_archive)) {
    base
  } else {
    path(base, basename(remote_archive))
  }
  if (!dir_exists(target)) {
    message("Cache directory does not exist, nothing to wipe.")
    return(invisible(NULL))
  }
  dir_delete(target)
  message(sprintf("Wiped cache: %s", target))
  invisible(NULL)
}


generate_gh_url <- function(
    series_path,
    base_url = "https://raw.githubusercontent.com/",
    remote_archive,
    sha) {
  ifelse(
    nchar(series_path) == 0,
    sprintf("%s%s/%s/data-raw/csv/series.csv", base_url, remote_archive, sha),
    sprintf("%s%s/%s/data-raw/csv/%s.csv", base_url, remote_archive, sha, series_path)
  )
}

#' @export
key_to_path <- function(key,
                        root_folder = "../ts_archive",
                        remote = FALSE) {
  l <- strsplit(key, "\\.")
  sapply(l, function(x) {
    if (remote) {
      o <- do.call(file.path, as.list(x))
      file.path(o, "series.csv")
    } else {
      do.call(file.path, as.list(x))
    }
  })
}









