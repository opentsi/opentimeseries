
#' List Available Time Series Keys from an Open Time Series Archive
#'
#' Fetches the index of available time series keys from a GitHub-hosted
#' open time series archive by reading its \code{data-raw/index.md} file.
#'
#' @param remote_archive A character string specifying the GitHub repository
#'   in \code{"owner/repo"} format. Defaults to
#'   \code{"opentsi/ch.kof.globalbaro"}.
#' @param ref Git ref (branch, tag, or commit SHA) to read from. Defaults to
#'   \code{"main"}.
#'
#' @return A data frame with a single column \code{key} containing the
#'   available time series keys. Returns \code{NULL} invisibly if no keys
#'   are found.
#'
#' @details The function reads the \code{data-raw/index.md} file from the
#'   specified GitHub repository and parses all markdown links under the
#'   \code{## Index of Time Series} header.
#'
#' @importFrom httr2 request req_error req_perform resp_body_string
#' @examples
#' \dontrun{
#' list_open_ts_keys("opentsi/ch.kof.globalbaro")
#' }
#'
#' @export
list_open_ts_keys <- function(remote_archive = "opentsi/ch.kof.globalbaro",
                              ref = NULL) {
  branches <- if (is.null(ref)) c("main", "master") else ref

  resp <- NULL
  for (branch in branches) {
    url <- sprintf(
      "https://raw.githubusercontent.com/%s/%s/data-raw/index.md",
      remote_archive, branch
    )
    resp <- request(url) |>
      req_error(is_error = \(r) FALSE) |>
      req_perform()
    if (resp$status_code == 200) break
  }

  if (resp$status_code != 200) {
    stop(sprintf(
      "Could not fetch index from '%s' (tried: %s, last HTTP %d).",
      remote_archive, paste(branches, collapse = ", "), resp$status_code
    ))
  }

  decoded_text <- resp_body_string(resp)
  lines <- strsplit(decoded_text, "\n")[[1]]

  header_idx <- grep("^## Index of Time Series", lines)
  if (length(header_idx) == 0) {
    stop("Could not find the header '## Index of Time Series' in the index.md file.")
  }

  index_content <- lines[(header_idx + 1):length(lines)]

  matches <- regexec("\\[(.*?)\\]\\((.*?)\\)", index_content)
  parsed_list <- regmatches(index_content, matches)
  valid_matches <- Filter(function(x) length(x) == 3, parsed_list)

  if (length(valid_matches) == 0) {
    message("No keys or links found under the header.")
    return(invisible(NULL))
  }

  sapply(valid_matches, "[[", 2)
}
