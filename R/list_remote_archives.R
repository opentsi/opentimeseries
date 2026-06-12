#' List OpenTSI Archives
#'
#' Lists available OpenTSI-compliant archives, either from a GitHub organisation
#' or from a local directory of archives created with
#' \code{\link{write_local_open_ts}}.
#'
#' @param org Character scalar; GitHub organisation name. Defaults to
#'   \code{"opentsi"}. Ignored when \code{archive_path} is supplied.
#' @param archive_path Character scalar or \code{NULL} (default). When
#'   \code{NULL} the function queries the GitHub API. When a path is supplied
#'   it lists subdirectories of that path that are git repositories.
#' @param max_n Integer; maximum number of remote repos to retrieve per API
#'   page. Defaults to \code{100}. Ignored for local archives.
#' @param git_platform Character; currently only \code{"github"} is supported.
#'   Ignored for local archives.
#'
#' @return A character vector of archive names.
#'
#' @importFrom httr2 request req_perform resp_body_json
#' @importFrom fs dir_exists dir_ls path path_expand
#' @export
#'
#' @examples
#' \dontrun{
#' # Remote
#' list_archives()
#' list_archives(org = "myorg")
#'
#' # Local
#' list_archives(archive_path = "~/.local_archives")
#' }
list_archives <- function(org = "opentsi",
                          archive_path = NULL,
                          max_n = 100,
                          git_platform = "github") {
  if (!is.null(archive_path)) {
    base <- path_expand(archive_path)
    if (!dir_exists(base)) {
      stop(sprintf("archive_path '%s' does not exist.", base))
    }
    dirs <- dir_ls(base, type = "directory")
    has_git <- vapply(dirs, function(d) dir_exists(path(d, ".git")), logical(1))
    return(basename(dirs[has_git]))
  }

  if (git_platform == "github") {
    provider_url <- "https://api.github.com"
  } else {
    stop(sprintf("git_platform '%s' is not supported.", git_platform))
  }

  resp <- request(
    sprintf("%s/orgs/%s/repos?type=public&per_page=%d", provider_url, org, max_n)
  ) |>
    req_perform() |>
    resp_body_json()

  archive_names <- vapply(resp, `[[`, character(1), "name")
  iso_2d <- tolower(get_iso_3166_alpha_2_countries())
  archives <- grep(
    paste(sprintf("^%s\\.", iso_2d), collapse = "|"),
    archive_names,
    value = TRUE
  )
  archives
}
