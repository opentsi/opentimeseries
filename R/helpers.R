
#' Get a List of All Tags from a Remote Time Series Archive
#' 
#' For now, opentimeseries only supports GitHub as an arvchive host.
#' We're working to suport other hosts including self-hosted git platforms. 
#' 
#' @importFrom jsonlite fromJSON
#' @export
tags_list <- function(remote_archive){
  api_url <- sprintf("https://api.github.com/repos/%s/tags",
  remote_archive)
  tags <- fromJSON(api_url, simplifyVector = FALSE)
  sapply(tags, "[[", "name")
}


#' Find the Past Version That Is Closest to a Given Date
#' 
#' @export
find_version <- function(ref_date, tags,
   return_as_tag = FALSE){
  d <- as.Date(gsub("v","", tags))
  out <- max(d[ref_date > d])
  if(return_as_tag){
    out <- sprintf("v%s",out)
  }
  out
}


#' Show Entire History of a Series in a Wide Format Table 
#' 
#' 
#' @importFrom data.table dcast
#' @export
triangle <- function(dt){
  d <- dcast(dt, formula = date ~ id, value.var = "value")
  names(d) <- gsub("(.+)(v[0-9]{4})","\\2",names(d))
  d
}


generate_gh_url <- function(series_path,
   base_url = "https://raw.githubusercontent.com/",
   remote_archive,
   tag){
  full_url <- sprintf("%s%s/%s/%s/series.csv",
    base_url,
    remote_archive,
    tag,
    series_path
  )
  full_url
}


key_to_path <- function(key, root_folder = "../ts_archive", remote = FALSE) {
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






