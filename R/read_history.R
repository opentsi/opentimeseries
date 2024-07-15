#' Read Entire History of a Time Series
#' 
#' Alias for the read_open_ts function with date parameter set to NULL. 
#' 
#' @export
read_history <- function(series,
                     remote_archive = "opentsi/demo",
                     rbind_dt = TRUE,
                     add_suffix = TRUE,
                     wide = TRUE,
                     lastn = NULL
                    ){
   read_open_ts(series = series, date = NULL, 
               remote_archive = remote_archive,
               rbind_dt = rbind_dt,
               add_suffix = add_suffix,
               lastn = lastn
              )
}