#' @param x data.table long format time series data.table with 3 columns: id, time, value.
#' Contains one or more time series.
write_open_ts <- function(x, ...) {
  UseMethod("write_open_ts")
}


write_open_ts.default <- function(x) {
  stop(
    sprintf("No write_open_ts() method for objects of class: %s",
            paste(class(x), collapse = ", ")),
    call. = FALSE
  )
}


write_open_ts.data.table <- function(x) {
  # validate ids as ts_keys
  # unique ts_keys are names of lists
  # split tsdt by id into single dt
  # single dt do NOT contain ids
  # lapply write_current_dt to disk
  # target: series.csv in data-raw/key-to-path()



}
