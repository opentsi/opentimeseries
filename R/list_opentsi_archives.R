#' Get public Open Time Series Initiative Archives from GitHub
#'
#' @return A list of dictionaries containing information about each time series archive
#' in the opentsi GitHub organization. Each dictionary has the keys "name", "description", and "url".
#' @importFrom httr2 request req_perform resp_body_json
#' @export
list_opentsi_archives <- function(pp = 100) {
  resp <- httr2::request(
    sprintf("https://api.github.com/orgs/opentsi/repos?type=public&per_page=%d",pp)) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  names <- vapply(resp, `[[`, character(1), "name")
  names[!grepl("opentimeseries|deloRean|opentsi.github.io|.github", names)]
}
