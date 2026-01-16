#' List Keys and Minimal Descriptions of all Time Series in an Archive
#'
#' @param repo character archive repo name
#' @param remote_org character name of the GitHub Org that hosts the time series.
#' Set to NULL to interpret repo as a local path a repository.
#' @export
list_time_series <- function(repo, remote_org = "opentsi"){
  UseMethod("list_time_series", object = repo)
}



list_time_series.character <- function(repo, remote_org = "opentsi"){
  if(is.null(remote_org)){
    class(repo) <- c("local_repo", "character")
  } else {
    class(repo) <- c("remote_repo", "character")
  }
}


#' @importFrom data.table fread
#' @importFrom fs dir_info
list_time_series.local_repo <- function(repo, remote_org = "opentsi"){
  series <- fread(
    file.path(repo, "data-raw","index.csv")
  )
  series
}


#
list_time_series.remote_repo <- function(repo, remote_org = "opentsi"){
  base_url <- "https://raw.githubusercontent.com/"
  remote_archive <- file.path(remote_org, repo)
  # might want to pass a time here to get a SHA

}
