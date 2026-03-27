
#' @importFrom digest digest
#' @export
is_update_needed <- function(checksum_input){

  request_checksum <- digest(checksum_input, algo = "sha256")

  checksum_equal <- compare_checksums(
    rq_checksum = request_checksum
    )
  return(!checksum_equal)
}


#' @importFrom jsonlite fromJSON
compare_checksums <- function(rq_checksum){
  mi <- fromJSON("inst/metadata.json")
  mi$update_checksum == rq_checksum
}


#' @importFrom jsonlite fromJSON
#' @export
update_checksum <- function(cs, json_path="inst/metadata.json"){
  if (file.exists(json_path)) {
    mi <- fromJSON(json_path)
  } else {
    stop("Metadata file does not exist. Fix your repository.")
  }

  if(mi$update_checksum == ""){
    stop("No initial checksum found. Archive initialisation was likely never fully completed.")
  }
  # cannot parse empty string as json
  # mi <- fromJSON("")
  mi$update_checksum <- cs
  json_content <- toJSON(mi, pretty = TRUE, auto_unbox = TRUE)
  writeLines(json_content, json_path)
  return(TRUE)
}
