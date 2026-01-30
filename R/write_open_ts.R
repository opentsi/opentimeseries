#' @param tsx a single time series, list of of time series or data.table long format time series data.table with 3 columns: id, time, value.
#' Contains one or more time series.
#' @export
write_open_ts <- function(tsx, ...) {
  UseMethod("write_open_ts")
}


#'@exportS3Method opentimeseries::write_open_ts
write_open_ts.default <- function(x) {
  stop(
    sprintf("No write_open_ts() method for objects of class: %s",
            paste(class(x), collapse = ", ")),
    call. = FALSE
  )
}


#'@exportS3Method opentimeseries::write_open_ts
write_open_ts.data.table <- function(x) {
  # validate ids as ts_keys
  # unique ts_keys are names of lists
  # split tsdt by id into single dt
  # single dt do NOT contain ids
  # lapply write_current_dt to disk
  # target: series.csv in data-raw/key-to-path()
  repo_name <- paste0(repo_name, ".")
  current_version_dt$id <- gsub(repo_name, "", current_version_dt$id)
  split_list <- split(current_version_dt, f = current_version_dt$id)
  for(i in names(split_list)){
    dir_create(
      file.path("data-raw",i)
    )
    fwrite(split_list[[i]],
           file = file.path("data-raw",
                            key_to_path(i),
                            "series.csv")
    )
  }
}
