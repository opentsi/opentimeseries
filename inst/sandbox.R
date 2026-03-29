# TODO: create look up function which checks which functions are in the "remote_archive"
# use metadata, incl. keys and title -> lookup function exists, if it uses metadataZ

# trying out the read_open_ts function
# doesnt work yet, series arg is missing
devtools::load_all()
# debug(read_open_ts)

baro <- read_open_ts(
  series = "barometer",
  date = as.Date("2026-01-20"),
  remote_archive = "minnaheim/ch.kof.barometer",
  show_vintage_dates = TRUE
)

# trying out the read_open_ts function
globalbaro <- read_open_ts(
  # change series to key?
  series = "coincident",
  date = as.Date("2025-01-20"),
  remote_archive = "minnaheim/ch.kof.globalbaro"
)

View(globalbaro) # works

# ODER:
read_open_ts(
  keys = c("coincident", "leading"),
  series = "ch.kof.globalbaro",
  repo = "opentsi")

# gh_url must be:
# https://raw.githubusercontent.com/minnaheim/ch.kof.globalbaro/a553ed0609dc6239420e901e84bca966bb2a0ce7/data-raw/coincident/series.csv