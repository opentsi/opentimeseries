#' Get public Open Time Series Initiative Arvchives from GitHub
#'
#' @return A list of dictionaries containing information about each time series archive
#' in the opentsi GitHub organization. Each dictionary has the keys "name", "description", and "url".
#' @importFrom gh get_github_repos
list_opentsi_archives <- function() {
  repos <- get_github_repos(org = "opentsi", type = "public")
  excludes <- grepl("opentimeseries|deloRean|opentsi.github.io", names(repos))
  repos[!excludes]
}