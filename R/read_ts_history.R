#' Read Entire History of a Time Series
#' 
#' Alias for the read_open_ts function with date parameter set to NULL. 
#' 
#' @export
read_history <- function(series,
                     remote_archive = "opentsi/ch.kof.globalbaro",
                     rbind_dt = TRUE,
                     add_suffix = TRUE,
                     wide = TRUE,
                     lastn = NULL
                    ){
   read_open_ts(series = series, date = NULL, 
               remote_archive = remote_archive,
               rbind_dt = rbind_dt,
               add_suffix = add_suffix,
               lastn = lastn,
               wide = wide
              )
  # check if series = NULL or series vector > 20 values, if TRUE, then
  # stop and tell user to narrow series down, else too much 

  # should return same object as read_open_ts, i.e. xts?

}