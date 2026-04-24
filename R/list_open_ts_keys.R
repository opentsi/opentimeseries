
#' List Available Time Series Keys from an Open Time Series Archive
#'
#' Fetches the index of available time series keys from a GitHub-hosted
#' open time series archive by reading its \code{data-raw/index.md} file.
#'
#' @param remote_archive A character string specifying the GitHub repository
#'   in \code{"owner/repo"} format. Defaults to
#'   \code{"minnaheim/ch.kof.globalbaro"}.
#'
#' @return A data frame with a single column \code{key} containing the
#'   available time series keys. Returned invisibly. Returns \code{NULL}
#'   invisibly if no keys are found.
#'
#' @details The function reads the \code{data-raw/index.md} file from the
#'   specified GitHub repository via the GitHub API and parses all markdown
#'   links under the \code{## Index of Time Series} header.
#'
#' @examples
#' \dontrun{
#' list_open_ts_keys()
#' list_open_ts_keys("opentsi/ch.kof.globalbaro")
#' }
#'
#' @export
list_open_ts_keys <- function(remote_archive = "minnaheim/ch.kof.globalbaro"){
  # github api go to index.md
    url <- sprintf(
    "https://api.github.com/repos/%s/contents/data-raw/index.md",
    remote_archive
  )

  res <- httr::GET(url)
  httr::stop_for_status(res)

  # read md content, aka key, and link 
  json_data <- httr::content(res)
  encoded_content <- gsub("\n", "", json_data$content)
  decoded_text <- rawToChar(base64enc::base64decode(encoded_content))

  lines <- readLines(textConnection(decoded_text))
  # print(lines)
  header_idx <- grep("^## Index of Time Series", lines)
  
  if (length(header_idx) == 0) {
    stop("Could not find the header '## Index of Time Series' in the index.md file.")
  }
  
  # Get everything after the header
  index_content <- lines[(header_idx + 1):length(lines)]
  
  # Extract keys and links using regex
  # Assuming the format is [key](link) or similar markdown table/list
  # This regex looks for [label](url) patterns
  matches <- regexec("\\[(.*?)\\]\\((.*?)\\)", index_content)
  parsed_list <- regmatches(index_content, matches)
  
  # Filter out lines that didn't match and convert to data frame
  valid_matches <- Filter(function(x) length(x) == 3, parsed_list)
  
  if (length(valid_matches) == 0) {
    message("No keys or links found under the header.")
    return(invisible(NULL))
  }
  
  df <- data.frame(
    key = sapply(valid_matches, "[[", 2),
    stringsAsFactors = FALSE
  )
  
  # 6. Print the data and metadata info
  print(df)
  
  message("If you want to get more metadata from this remote_archive, use the read_meta function")
  
  return(invisible(df))
}


