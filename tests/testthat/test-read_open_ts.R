library(data.table)

# ---------------------------------------------------------------------------
# Helpers used by read_open_ts (unit tests – no network required)
# ---------------------------------------------------------------------------

test_that("key_to_path converts dot-separated keys to file paths", {
  expect_equal(key_to_path("ch.kof.globalbaro.coincident"),
               "ch/kof/globalbaro/coincident")
  expect_equal(key_to_path(c("a.b.c", "x.y.z")),
               c("a/b/c", "x/y/z"))
  # Single-segment key
  expect_equal(key_to_path("onlyone"), "onlyone")
})

test_that("get_commit_dates returns a list with $commits and $branch", {
  skip_if_offline()

  result <- get_commit_dates("minnaheim/ch.kof.barometer", lastn = 5)

  expect_type(result, "list")
  expect_named(result, c("commits", "branch"))
  expect_s3_class(result$commits, "data.table")
  expect_true(result$branch %in% c("main", "master"))
  expect_true(all(c("hash", "date") %in% names(result$commits)))
})

test_that("get_commit_by_date returns the latest commit before the date", {
  commits <- data.table(
    hash = c("sha_old", "sha_mid", "sha_new"),
    date = as.POSIXct(c("2023-01-01", "2023-06-01", "2024-01-01"))
  )

  res <- get_commit_by_date(commits, d = as.Date("2023-07-01"))
  expect_equal(res$hash, "sha_mid")
})

test_that("get_commit_by_date returns empty when date precedes all commits", {
  commits <- data.table(
    hash = c("sha_a", "sha_b"),
    date = as.POSIXct(c("2024-01-01", "2024-06-01"))
  )
  res <- get_commit_by_date(commits, d = as.Date("2020-01-01"))
  expect_equal(nrow(res), 0)
})

# ---------------------------------------------------------------------------
# Input validation errors (no network required)
# ---------------------------------------------------------------------------

test_that("read_open_ts with series = NULL fetches all series from archive", {
  skip_if_offline()

  dt <- read_open_ts(remote_archive = "opentsi/ch.kof.globalbaro")

  expect_s3_class(dt, "data.table")
  expect_true(nrow(dt) > 0)
  expect_true(all(c("id", "value") %in% names(dt)))
  # NULL series should return more than one key
  expect_gt(length(unique(dt$id)), 1L)
})

test_that("read_open_ts errors when series is not a character vector", {
  expect_error(
    read_open_ts(series = 123, remote_archive = "opentsi/ch.kof.globalbaro"),
    regexp = "`series` must be a non-empty character vector"
  )
})

test_that("read_open_ts errors when series is an empty character vector", {
  expect_error(
    read_open_ts(series = character(0), remote_archive = "opentsi/ch.kof.globalbaro"),
    regexp = "`series` must be a non-empty character vector"
  )
})

test_that("read_open_ts errors when date is not a valid date", {
  expect_error(
    read_open_ts(series = "coincident", date = "not-a-date",
                 remote_archive = "opentsi/ch.kof.globalbaro"),
    regexp = "`date` must be a valid date"
  )
})

test_that("read_open_ts errors when remote_archive has no slash", {
  expect_error(
    read_open_ts(series = "coincident", remote_archive = "noslash"),
    regexp = "owner/repo"
  )
})

test_that("read_open_ts errors when remote_archive has multiple slashes", {
  expect_error(
    read_open_ts(series = "coincident", remote_archive = "a/b/c"),
    regexp = "owner/repo"
  )
})

test_that("read_open_ts errors when remote_archive is not a single string", {
  expect_error(
    read_open_ts(series = "coincident",
                 remote_archive = c("opentsi/ch.kof.globalbaro", "opentsi/other")),
    regexp = "owner/repo"
  )
})

# ---------------------------------------------------------------------------
# Network-dependent integration tests
# ---------------------------------------------------------------------------

test_that("read_open_ts returns a data.table for a known series", {
  skip_if_offline()

  dt <- read_open_ts(
    series         = "coincident",
    remote_archive = "opentsi/ch.kof.globalbaro"
  )

  expect_s3_class(dt, "data.table")
  expect_true(nrow(dt) > 0)
  expect_true(all(c("id", "value") %in% names(dt)))
  expect_equal(unique(dt$id), "coincident")
})

test_that("read_open_ts returns a list when rbind_dt = FALSE", {
  skip_if_offline()

  lst <- read_open_ts(
    series         = "coincident",
    remote_archive = "opentsi/ch.kof.globalbaro",
    rbind_dt       = FALSE
  )

  expect_type(lst, "list")
  expect_named(lst, "coincident")
  expect_s3_class(lst[[1]], "data.table")
})

test_that("show_vintage_dates adds query_date and commit_date columns", {
  skip_if_offline()

  dt <- read_open_ts(
    series             = "coincident",
    remote_archive     = "opentsi/ch.kof.globalbaro",
    show_vintage_dates = TRUE
  )

  expect_true(all(c("id", "query_date", "commit_date", "value") %in% names(dt)))
  expect_s3_class(dt$query_date,  "Date")
  expect_s3_class(dt$commit_date, "Date")
})

test_that("add_suffix appends the date to the id", {
  skip_if_offline()

  query_date <- as.Date("2024-01-01")
  dt <- read_open_ts(
    series         = "barometer",
    date           = query_date,
    remote_archive = "minnaheim/ch.kof.barometer"
    # ,
    # add_suffix     = TRUE
  )

  expect_true(all(grepl(as.character(query_date), dt$id)))
})

test_that("read_open_ts errors with a clear message for a non-existent archive", {
  skip_if_offline()

  expect_error(
    read_open_ts(
      series         = "coincident",
      remote_archive = "opentsi/this-repo-does-not-exist-xyz"
    ),
    regexp = "Could not fetch commit history"
  )
})

test_that("read_open_ts errors with a clear message for a non-existent series key", {
  skip_if_offline()

  local_mocked_bindings(
    get_commit_dates = function(...) {
      list(
        commits = data.table(
          hash = "abc123fake",
          date = as.POSIXct("2024-01-01", tz = "UTC")
        ),
        branch = "main"
      )
    },
    .package = "opentimeseries"
  )

  expect_error(
    read_open_ts(
      series         = "does-not-exist-at-all",
      remote_archive = "opentsi/ch.kof.globalbaro"
    ),
    regexp = "Could not read series"
  )
})
