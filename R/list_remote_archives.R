
#' List All OPENTSI Compliant Archives of an Organization
#' 
#' @param org character name of an organization 
#' @param max_n numeric max number of archives to be listed
#' @param git_platform character can only be set to GitHub for now.
#' @importFrom httr2 request req_perform resp_body_json
#' @export
list_remote_archives <- function(org = "opentsi", max_n = 100, git_platform = "github"){

  if(git_platform == "github"){
    provider_url <- "https://api.github.com"

  }

  resp <- request(
    sprintf("%s/orgs/%s/repos?type=public&per_page=%d", provider_url, org, max_n)
  ) |>
    req_perform() |>
    resp_body_json()

  archive_names <- vapply(resp, `[[`, character(1), "name")
  iso_2d <- tolower(get_iso_3166_alpha_2_countries())
  # because archive must start with 2 digit country codes we can easily distinguish
  # them from other repos of the same org
  archives <- grep(paste(sprintf("^%s\\.", iso_2d),collapse = "|"), archive_names, value = T)
  archives 
}