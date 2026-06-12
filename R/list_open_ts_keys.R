#' List Available Time Series in a Dataset
#'
#' Returns the series identifiers available in a dataset, either from a
#' GitHub-hosted archive or from a local archive created with
#' \code{\link{write_local_open_ts}}.
#'
#' @param dataset Character scalar. For remote archives: the GitHub repository
#'   in \code{"owner/repo"} format (e.g. \code{"opentsi/ch.kof.globalbaro"}).
#'   For local archives: the dataset name (e.g. \code{"ch.kof.globalbaro"}).
#' @param archive_path Character scalar or \code{NULL} (default). When
#'   \code{NULL} the function reads from the remote GitHub archive. When a path
#'   is supplied it reads from the local archive at
#'   \code{archive_path/dataset/data-raw/csv/}.
#' @param ref Git ref (branch, tag, or commit SHA) for remote archives.
#'   Defaults to \code{NULL}, which tries \code{"main"} then \code{"master"}.
#'   Ignored for local archives.
#'
#' @return A character vector of series identifiers.
#'
#' @importFrom httr2 request req_error req_perform resp_body_string
#' @importFrom fs dir_exists dir_ls path path_expand
#' @export
#'
#' @examples
#' \dontrun{
#' # Remote
#' list_series("opentsi/ch.kof.globalbaro")
#'
#' # Local
#' list_series("ch.kof.globalbaro", archive_path = "~/.local_archives")
#' }
list_series <- function(dataset, archive_path = NULL, ref = NULL) {
  if (!is.null(archive_path)) {
    csv_dir <- path(path_expand(archive_path), dataset, "data-raw", "csv")
    if (!dir_exists(csv_dir)) {
      stop(sprintf(
        "No local archive found for dataset '%s' in '%s'.",
        dataset, path_expand(archive_path)
      ))
    }
    csv_files <- dir_ls(csv_dir, glob = "*.csv")
    return(sub("\\.csv$", "", basename(csv_files)))
  }

  branches <- if (is.null(ref)) c("main", "master") else ref

  resp <- NULL
  for (branch in branches) {
    url <- sprintf(
      "https://raw.githubusercontent.com/%s/%s/data-raw/index.md",
      dataset, branch
    )
    resp <- request(url) |>
      req_error(is_error = \(r) FALSE) |>
      req_perform()
    if (resp$status_code == 200) break
  }

  if (resp$status_code != 200) {
    stop(sprintf(
      "Could not fetch index from '%s' (tried: %s, last HTTP %d).",
      dataset, paste(branches, collapse = ", "), resp$status_code
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
