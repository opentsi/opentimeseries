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
#'
#' @importFrom httr2 request req_headers req_perform resp_body_json
#' @export
get_commit_dates <- function(remote_archive = "opentsi/kofethz",
                             lastn = 5) {
  url <- sprintf(
    "https://api.github.com/repos/%s/commits?sha=main&per_page=%d",
    remote_archive, lastn
  )
  res <- request(url) |>
    req_headers(Accept = "application/vnd.github.v3+json") |>
    req_perform() |>
    resp_body_json()

  rbindlist(lapply(res, function(x) {
    list(
      hash = x$sha,
      date = as.POSIXct(x$commit$author$date,
        format = "%Y-%m-%dT%H:%M:%SZ",
        tz = "UTC"
      )
    )
  }))
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
  filtered[index_of_max, ]$hash
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

# https://raw.githubusercontent.com/opentsi/kofethz/4d10e6332b2c37c4427df444be960f4d7e2cdec7/ch/kof/globalbaro/coincident/series.csv
generate_gh_url <- function(
    series_path,
    base_url = "https://raw.githubusercontent.com/",
    remote_archive,
    sha) {
  full_url <- sprintf(
    "%s%s/%s/%s/series.csv",
    base_url,
    remote_archive,
    sha,
    series_path
  )
  full_url
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









