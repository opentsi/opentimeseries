#' Read Time Series from an Open Time Series Archive
#' 
#' This function reads time series from an Open Time Series Archive. It is
#' a convenient way to read directly from a remote Git repository or from local 
#' open time series archive. As a convenience function it is uses reasonable defaults
#' using the official archives. Hence calls need to be adapted if you want to use your
#' archives. 
#' @param series character vector containing the queried time series keys.
#' @param date date used to determine the version of a set of time series.
#' Defaults to Sys.Date().
#' @param remote_archive character contains the username/repo of a GitHub archive. Defaults to rseed-koflab/ch.kof.
#' @param local_archive character contains relative path to a local repo. When not set to NULL it overwrites reading
#' from remote. Defaults to NULL.
#' @importFrom data.table fread rbindlist setcolorder
#' @export
read_open_ts <- function(series,
  date = Sys.Date(),
  remote_archive = "rseed-koflab/ch.kof",
  branch = "main",
  local_archive = NULL,
  rbind_dt = TRUE){
# TODO: if a ts is not present in a particular version we need to look the 
# earliest appearance of a series, possibly a bisect style approach. 
# for starters we can at least issue a warning which requested series are not present 
# in particular version

GITHUB_RAW_CONTENT_URL <- "https://raw.githubusercontent.com/"
l <- lapply(series, function(x){
full_url <- sprintf("%s%s/%s/%s",
 GITHUB_RAW_CONTENT_URL,
 remote_archive,
 branch,
 key_to_path(x, remote = TRUE))
dt <- fread(full_url)
dt$id <- x
setcolorder(dt, neworder = c("id","date","value"))
dt
})
names(l) <- series
if(rbind_dt){
rbindlist(l)
} else{
l
}


}
