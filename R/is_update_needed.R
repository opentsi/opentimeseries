
#' @importFrom digest digest
#' @export
is_update_needed <- function(request_checksum){
  checksum_equal <- compare_checksums(
    rq_checksum = request_checksum
    )
  return(!checksum_equal)
}


#' @importFrom jsonlite function
compare_checksums <- function(rq_checksum){
  mi <- fromJSON("inst/metadata.json")
  mi$update_checksum == rq_checksum
}


#' @importFrom jsonlite fromJSON
#' @export
update_checksum <- function(cs, json_path="inst/metadata.json"){
  mi <- fromJSON("")
  mi$update_checksum <- cs
  json_content <- toJSON(mi, pretty = TRUE, auto_unbox = TRUE)
  writeLines(json_content, json_path)
}
