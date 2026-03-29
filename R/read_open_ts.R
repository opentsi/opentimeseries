#' Read Time Series from an Open Time Series Archive
#'
#' This function reads time series from an Open Time Series Archive. It is
#' a convenient way to read directly from a remote Git repository or from local
#' open time series archive. As a convenience function it is uses reasonable defaults
#' using the official archives. Hence calls need to be adapted if you want to use your
#' archives.
#'
#' @param series character vector containing the queried time series keys (without the leading country.provider.provider key elements). Defaults to NULL fetches all time series within a dataset.
#' @param date date used to determine the version of a set of time series.
#' Defaults to Sys.Date().
#' @param remote_archive character contains the username/repo of a GitHub archive. Defaults to opentsi
#' @param rbind_dt boolean should data.tables be row bound to a single data.table?
#' Defaults to TRUE.
#' @param add_suffix boolean should a version suffix be added to the id? Defaults to FALSE.
#' @importFrom data.table fread rbindlist setcolorder
#' @export
read_open_ts <- function(
  series = NULL,
  date = Sys.Date(),
  remote_archive = "opentsi",
  rbind_dt = TRUE,
  wide = TRUE,
  add_suffix = FALSE,
  lastn = 100, # last n commits
  show_vintage_dates = FALSE){
  UseMethod("read_open_ts", object = series)
}


# #' @exportS3Method opentimeseries::read_open_ts
# read_open_ts.character <- function(
#     ts_keys,
#     date_time = Sys.time(),
#     repo,
#     remote_org = "opentsi" # set it to NULL to interpret repo as local path
# ){
#   if(is.null(remote_org)){
#     class(ts_keys) <- c("local_keys", "character")
#   } else {
#     class(ts_keys) <- c("remote_keys", "character")
#   }
# }


# #' @exportS3Method opentimeseries::read_open_ts
# read_open_ts.remote_keys <- function(
#     ts_keys,
#     date_time = Sys.time(),
#     repo,
#     remote_org = "opentsi" # set it to NULL to interpret repo as local path
# ){

# }

# #' @exportS3Method opentimeseries::read_open_ts
# read_open_ts.local_keys <- function(
#     ts_keys,
#     date_time = Sys.time(),
#     repo,
#     remote_org = "opentsi" # set it to NULL to interpret repo as local path
#     ){

# }
read_open_ts <- function(
  series = NULL,
  date = Sys.Date(),
  remote_archive = "opentsi",
  rbind_dt = TRUE,
  wide = TRUE,
  add_suffix = FALSE,
  lastn = 100, # last n commits
  show_vintage_dates = FALSE 
){
  # If date is not NULL return the correct
  # specific version
  # TODO: here, if lastn == N then check all commits
  commit_dates <- get_commit_dates(remote_archive = remote_archive,
                                   lastn = lastn)
  commit_res <- get_commit_by_date(commit_dates, d = date)
  # get date of commit 
  commit_sha <- commit_res$hash
  commit_date <- commit_res$date

  # conditional if no series (key) exists:
  # TODO: check metadata here for keys
  series_paths <- key_to_path(series)
  gh_urls <- generate_gh_url(series_path = series_paths,
       remote_archive = remote_archive,
       sha = commit_sha)
  l <- list()
  for (i in seq_along(gh_urls)){
    dt <- fread(gh_urls[i])
    dt$id <- series[i] # here coincident
    # TODO: think about whether date set by user should be date or date of commit
    if(show_vintage_dates){
      dt[, query_date := as.Date(date)]
      dt[, commit_date := as.Date(commit_date)]
      setcolorder(dt, neworder = c("id","query_date", "commit_date","value"))
    }else{
      setcolorder(dt, neworder = c("id","value"))
    }
    if(add_suffix){
      dt[, id := sprintf("%s.%s", dt$id, date)]
    }
    l[[i]] <- dt
  }
  names(l) <- series
  if(rbind_dt){
    rbindlist(l)
    } else{
    l
  }
}

