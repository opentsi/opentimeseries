library(kofdata)
tsl <- get_time_series(c("ch.kof.barometer","ch.kof.globalbaro.leading"))



create_ts_folders <- function(ts_keys){
  provider <- gsub("^([a-z]+\\.[a-z]+)\\.(.*)","\\1",ts_keys)
  folder_paths <- gsub("^([a-z]+\\.[a-z]+)\\.(.*)","\\2",ts_keys)
  key_to_path(folder_paths[2])
}



create_ts_folders(names(tsl))


xx <- read_open_ts("ch.kof.globalbaro.leading")








# • Use `requireNamespace("tsbox", quietly = TRUE)` to test if package is installed
# • Then directly refer to functions with `tsbox::fun()




a202407 <- archive_read("ch.kof.globalbaro.leading",
                   date = "2024-07-01")
a202407$id <- sprintf("%s.202407", a202407$id)


a202307 <- archive_read("ch.kof.globalbaro.leading",
                        date = "2023-07-01")
a202307$id <- sprintf("%s.202307", a202307$id)




ts_plot(rbind(a202307,a202407))

library(tsbox)

ts_tslist(xx)


ts_plot(xx)
tsbox::ts_ts(tsbox::ts_dt(xx))



# https://github.com/opentsi/kofethz/releases/tag/v2025-06-01
#
#
# GITHUB_RAW_CONTENT_URL <- "https://raw.githubusercontent.com/"
#
#
# https://raw.githubusercontent.com/opentsi/kofethz/main/ch/kof/globalbaro/leading/series.csv
#
#
#
# https://raw.githubusercontent.com/opentsi/kofethz/v2025-06-01/ch/kof/globalbaro/coincident/series.csv
#
#
#
# https://github.com/opentsi/kofethz/blob/v2025-06-01/ch/kof/globalbaro/coincident/series.csv
#
#
# https://raw.githubusercontent.com/opentsi/kofethz/releases/tag/v2025-06-01/ch/kof/globalbaro/leading/series.csv
#
#
#
#
# #solution
#
# https://raw.githubusercontent.com/opentsi/kofethz/v2025-06-01/ch/kof/globalbaro/coincident/series.csv
#
#
# https://api.github.com/repos/opentsi/kofethz/tags
# https://raw.githubusercontent.com/opentsi/kofethz/tag/v2025-06-01/ch/kof/globalbaro/incident/series.csv
# https://raw.githubusercontent.com/opentsi/kofethz/tag/v2024-07-01/ch/kof/globalbaro/incident/series.csv
#
#
# https://raw.githubusercontent.com/opentsi/kofethz/v2025-06-01/ch/kof/globalbaro/coincident/series.csv
#
# https://raw.githubusercontent.com/opentsi/kofethz/v2024-07-01/ch/kof/globalbaro/coincident/series.csv
