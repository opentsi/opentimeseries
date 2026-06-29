tags_list <- function(remote_archive) {
  api_url <- sprintf(
    "https://api.github.com/repos/%s/tags",
    remote_archive
  )
  tags <- jsonlite::fromJSON(api_url, simplifyVector = FALSE)
  sapply(tags, "[[", "name")
}


#' @importFrom httr2 request req_headers req_error req_perform resp_body_json
get_commit_dates <- function(remote_archive = "opentsi/kofethz",
                             lastn = 100) {
  token <- Sys.getenv("GITHUB_TOKEN", unset = Sys.getenv("GITHUB_PAT", unset = Sys.getenv("GH_TOKEN", unset = "")))

  for (branch in c("main", "master")) {
    url <- sprintf(
      "https://api.github.com/repos/%s/commits?sha=%s&per_page=%d",
      remote_archive, branch, lastn
    )
    req <- request(url) |>
      req_headers(Accept = "application/vnd.github.v3+json") |>
      req_error(is_error = \(r) FALSE)
    if (nzchar(token)) {
      req <- req |> req_headers(Authorization = paste("Bearer", token))
    }
    resp <- req_perform(req)
    if (resp$status_code == 200) break
  }

  if (resp$status_code != 200) {
    hint <- if (resp$status_code == 403L || resp$status_code == 429L) {
      " You may have hit the GitHub API rate limit. Set the GITHUB_TOKEN environment variable to increase the limit."
    } else {
      ""
    }
    stop(sprintf(
      "Archive '%s' not found or not accessible (HTTP %d).%s",
      remote_archive, resp$status_code, hint
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
  if (!grepl("\\d{2}:\\d{2}:\\d{2}$", as.POSIXct(d))) {
    d <- paste(d, "23:59:59")
  }
  filtered <- dd[as.POSIXct(d) > dd$date, ]
  index_of_max <- which.max(as.numeric(filtered$date))
  return(filtered[index_of_max, ])
}

find_version <- function(ref_date, tags, return_as_tag = FALSE) {
  d <- as.Date(gsub("v", "", tags))
  out <- max(d[ref_date > d])
  if (return_as_tag) {
    out <- sprintf("v%s", out)
  }
  out
}


#' Pivot a Single-Series Version History into a Revision Triangle
#'
#' Takes the long-format output of \code{\link{read_ts_history}} (columns
#' \code{id}, \code{vintage_date}, \code{date}, \code{value}) and pivots it
#' wide so that each column is one vintage and each row is one observation date.
#'
#' @param dt a \code{data.table} as returned by \code{read_ts_history}.
#'
#' @return A wide \code{data.table} with \code{date} as the row key and one
#'   column per \code{vintage_date}.
#'
#' @importFrom data.table dcast
#' @export
triangle_ts_history <- function(dt) {
  d <- dcast(dt, formula = date ~ vintage_date, value.var = "value")
  names(d)[-1] <- format(as.Date(names(d)[-1]), "v%Y_%m_%d")
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
