#' Read Time Series from an Open Time Series Archive
#'
#' This function reads time series from an Open Time Series Archive. It is
#' a convenient way to read directly from a remote Git repository or from local
#' open time series archive. As a convenience function it is uses reasonable defaults
#' using the official archives. Hence calls need to be adapted if you want to use your
#' archives.
#'
#' @param series character vector containing the queried time series keys.
#' @param date date used to determine the version of a set of time series.
#' Defaults to Sys.Date().
#' @param remote_archive character contains the username/repo of a GitHub archive. Defaults to rseed-koflab/ch.kof.
#' @param rbind_dt boolean should data.tables be row bound to a single data.table?
#' Defaults to TRUE.
#' @param add_suffix boolean should a version suffix be added to the id? Defaults to FALSE.  
#' @importFrom data.table fread rbindlist setcolorder
#' @export
read_open_ts <- function(series,
  date = Sys.Date(),
  remote_archive = "opentsi/kofethz",
  rbind_dt = TRUE,
  wide = TRUE,
  add_suffix = FALSE,
  lastn = NULL
){
  tags <- tags_list(remote_archive = remote_archive)
  if(!is.null(lastn)){
    tags <- tags[1:lastn]
  }
  if(is.null(date)){
    if(length(series) > 1) warning("Nested looping not implemented, only using first time series. Setting date to NULL reads all versions of a specific series.")
    # for the sake of readability and avoiding heterogeneous output
    # we do not implemented nested loops that loop over series AND versions.
    # TODO: expand.grid might be a nice way to implement nested tags/series. 
    series_paths <- key_to_path(series[1])
    gh_urls <- generate_gh_url(series_path = series_paths,
      remote_archive = remote_archive, tag = tags)
    l <- list()
    for (i in seq_along(gh_urls)){
      dt <- fread(gh_urls[i])
      dt$id <- series[1]
      dt[, date := as.Date(date)]
      setcolorder(dt, neworder = c("id","date","value"))
      l[[i]] <- dt
      if(rbind_dt){
        # when merging all versions into one dt
        # we need a suffix
        dt[, id := sprintf("%s.%s", dt$id, tags[i])]
      }
    }

    if(rbind_dt){
      rbl <- rbindlist(l)
      if(wide){
        return(triangle(rbl))
      }
      return(rbl)
    }

    names(l) <- tags
    return(l)
  }

  # If date is not NULL return the correct 
  # specific version
  series_paths <- key_to_path(series)
  gh_urls <- generate_gh_url(series_path = series_paths,
       remote_archive = remote_archive,
       tag = find_version(date, tags = tags, return_as_tag = TRUE))
  l <- list()
  for (i in seq_along(gh_urls)){
    dt <- fread(gh_urls[i])
    dt$id <- series[i]
    dt[, date := as.Date(date)]
    setcolorder(dt, neworder = c("id","date","value"))
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