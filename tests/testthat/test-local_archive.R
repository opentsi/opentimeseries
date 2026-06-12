library(data.table)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

make_dt <- function(n = 12, start = "2020-01-01", by = "month") {
  data.frame(
    date  = seq(as.Date(start), by = by, length.out = n),
    value = seq_len(n) * 1.0
  )
}

# Returns a fresh temp dir and registers cleanup with the calling test frame.
local_archive_dir <- function(env = parent.frame()) {
  d <- tempfile("opentsi_test_")
  withr::defer(unlink(d, recursive = TRUE), envir = env)
  d
}

# ---------------------------------------------------------------------------
# write_local_open_ts — input dispatch
# ---------------------------------------------------------------------------

test_that("single data.frame writes a CSV with correct content", {
  arch <- local_archive_dir()
  dt   <- make_dt()
  write_local_open_ts(dt, series = "leading", dataset = "ch.test",
                      archive_path = arch)

  csv <- file.path(arch, "ch.test", "data-raw", "csv", "leading.csv")
  expect_true(file.exists(csv))
  result <- fread(csv)
  expect_equal(result$value, dt$value)
  expect_equal(as.character(result$date), as.character(dt$date))
})

test_that("ts object is accepted and written correctly", {
  arch <- local_archive_dir()
  x    <- ts(1:12, start = c(2020, 1), frequency = 12)
  write_local_open_ts(x, series = "monthly", dataset = "ch.test",
                      archive_path = arch)

  csv <- file.path(arch, "ch.test", "data-raw", "csv", "monthly.csv")
  expect_true(file.exists(csv))
  result <- fread(csv)
  expect_equal(nrow(result), 12L)
  expect_equal(result$value, as.numeric(x))
})

test_that("named list writes all series in one commit", {
  arch <- local_archive_dir()
  lst  <- list(leading = make_dt(), coincident = make_dt(start = "2019-01-01"))
  write_local_open_ts(lst, dataset = "ch.test", archive_path = arch)

  for (nm in names(lst)) {
    expect_true(file.exists(
      file.path(arch, "ch.test", "data-raw", "csv", paste0(nm, ".csv"))
    ))
  }
  log <- gert::git_log(repo = file.path(arch, "ch.test"))
  expect_equal(nrow(log), 1L)
})

test_that("long-format data.table with id column writes all series in one commit", {
  arch <- local_archive_dir()
  long <- rbind(
    data.table(id = "leading",   date = seq(as.Date("2020-01-01"), by = "month", length.out = 6), value = 1:6),
    data.table(id = "coincident", date = seq(as.Date("2020-01-01"), by = "month", length.out = 6), value = 7:12)
  )
  write_local_open_ts(long, dataset = "ch.test", archive_path = arch)

  expect_true(file.exists(file.path(arch, "ch.test", "data-raw", "csv", "leading.csv")))
  expect_true(file.exists(file.path(arch, "ch.test", "data-raw", "csv", "coincident.csv")))
  log <- gert::git_log(repo = file.path(arch, "ch.test"))
  expect_equal(nrow(log), 1L)
})

# ---------------------------------------------------------------------------
# write_local_open_ts — git commit behaviour
# ---------------------------------------------------------------------------

test_that("first write creates exactly one commit", {
  arch <- local_archive_dir()
  write_local_open_ts(make_dt(), series = "s1", dataset = "ch.test",
                      archive_path = arch)

  log <- gert::git_log(repo = file.path(arch, "ch.test"))
  expect_equal(nrow(log), 1L)
})

test_that("writing the same data twice produces no second commit (idempotent)", {
  arch <- local_archive_dir()
  dt   <- make_dt()
  write_local_open_ts(dt, series = "s1", dataset = "ch.test", archive_path = arch)
  expect_message(
    write_local_open_ts(dt, series = "s1", dataset = "ch.test", archive_path = arch),
    "unchanged"
  )
  log <- gert::git_log(repo = file.path(arch, "ch.test"))
  expect_equal(nrow(log), 1L)
})

test_that("writing changed data produces a second commit", {
  arch <- local_archive_dir()
  dt1  <- make_dt()
  dt2  <- make_dt()
  dt2$value <- dt2$value * 2

  write_local_open_ts(dt1, series = "s1", dataset = "ch.test", archive_path = arch)
  write_local_open_ts(dt2, series = "s1", dataset = "ch.test", archive_path = arch)

  log <- gert::git_log(repo = file.path(arch, "ch.test"))
  expect_equal(nrow(log), 2L)
})

test_that("batch write with mixed changed/unchanged series produces one commit covering only changes", {
  arch <- local_archive_dir()
  dt1  <- make_dt()
  dt2  <- make_dt(start = "2019-01-01")

  write_local_open_ts(list(a = dt1, b = dt2), dataset = "ch.test", archive_path = arch)

  dt1_changed <- dt1
  dt1_changed$value <- dt1_changed$value + 99
  write_local_open_ts(list(a = dt1_changed, b = dt2), dataset = "ch.test",
                      archive_path = arch)

  log <- gert::git_log(repo = file.path(arch, "ch.test"))
  expect_equal(nrow(log), 2L)
})

# ---------------------------------------------------------------------------
# write_local_open_ts — input validation
# ---------------------------------------------------------------------------

test_that("errors when series is missing for a single data.frame", {
  arch <- local_archive_dir()
  expect_error(
    write_local_open_ts(make_dt(), dataset = "ch.test", archive_path = arch),
    regexp = "`series` is required"
  )
})

test_that("errors when list elements are unnamed", {
  arch <- local_archive_dir()
  expect_error(
    write_local_open_ts(list(make_dt(), make_dt()), dataset = "ch.test",
                        archive_path = arch),
    regexp = "named"
  )
})

# ---------------------------------------------------------------------------
# write_local_open_ts — vintage history (vintage_date column)
# ---------------------------------------------------------------------------

test_that("vintage history: one commit per unique vintage_date", {
  arch <- local_archive_dir()

  hist <- rbind(
    data.table(id = "leading", vintage_date = as.Date("2024-01-01"),
               date = seq(as.Date("2020-01-01"), by = "month", length.out = 6),
               value = 1:6),
    data.table(id = "leading", vintage_date = as.Date("2024-02-01"),
               date = seq(as.Date("2020-01-01"), by = "month", length.out = 7),
               value = c(1:6, 7))
  )
  write_local_open_ts(hist, dataset = "ch.test", archive_path = arch)

  log <- gert::git_log(repo = file.path(arch, "ch.test"))
  expect_equal(nrow(log), 2L)

  # author dates should match the vintage dates
  expect_setequal(as.Date(log$time, tz = "UTC"),
                  c(as.Date("2024-01-01"), as.Date("2024-02-01")))
})

test_that("vintage history: already-committed vintage is skipped on re-import", {
  arch <- local_archive_dir()

  hist <- data.table(
    id           = "leading",
    vintage_date = as.Date("2024-01-01"),
    date         = seq(as.Date("2020-01-01"), by = "month", length.out = 6),
    value        = 1:6
  )
  write_local_open_ts(hist, dataset = "ch.test", archive_path = arch)
  expect_message(
    write_local_open_ts(hist, dataset = "ch.test", archive_path = arch),
    "already committed"
  )
  log <- gert::git_log(repo = file.path(arch, "ch.test"))
  expect_equal(nrow(log), 1L)
})

test_that("vintage history: reader returns correct vintage for backdated commits", {
  arch <- local_archive_dir()

  hist <- rbind(
    data.table(id = "s1", vintage_date = as.Date("2023-01-01"),
               date = seq(as.Date("2020-01-01"), by = "month", length.out = 6),
               value = 1:6),
    data.table(id = "s1", vintage_date = as.Date("2024-01-01"),
               date = seq(as.Date("2020-01-01"), by = "month", length.out = 7),
               value = c(1:6, 99))
  )
  write_local_open_ts(hist, dataset = "ch.test", archive_path = arch)

  old <- read_local_open_ts("s1", dataset = "ch.test", archive_path = arch,
                             date = "2023-06-01")
  expect_equal(nrow(old), 6L)
  expect_equal(old$value, 1:6)

  new <- read_local_open_ts("s1", dataset = "ch.test", archive_path = arch,
                             date = "2024-06-01")
  expect_equal(nrow(new), 7L)
  expect_equal(new$value[7L], 99)
})

# ---------------------------------------------------------------------------
# read_local_open_ts — round-trip
# ---------------------------------------------------------------------------

test_that("read_local_open_ts returns id/date/value data.table matching what was written", {
  arch <- local_archive_dir()
  dt   <- make_dt()
  write_local_open_ts(dt, series = "leading", dataset = "ch.test", archive_path = arch)

  out <- read_local_open_ts("leading", dataset = "ch.test", archive_path = arch)

  expect_s3_class(out, "data.table")
  expect_named(out, c("id", "date", "value"))
  expect_equal(unique(out$id), "leading")
  expect_equal(out$value, dt$value)
})

test_that("rbind_dt = FALSE returns a named list", {
  arch <- local_archive_dir()
  lst  <- list(leading = make_dt(), coincident = make_dt(start = "2019-01-01"))
  write_local_open_ts(lst, dataset = "ch.test", archive_path = arch)

  out <- read_local_open_ts(c("leading", "coincident"), dataset = "ch.test",
                             archive_path = arch, rbind_dt = FALSE)
  expect_type(out, "list")
  expect_named(out, c("leading", "coincident"))
  expect_s3_class(out[["leading"]], "data.table")
})

test_that("multiple series returned as a single bound data.table by default", {
  arch <- local_archive_dir()
  lst  <- list(s1 = make_dt(), s2 = make_dt(start = "2019-01-01"))
  write_local_open_ts(lst, dataset = "ch.test", archive_path = arch)

  out <- read_local_open_ts(c("s1", "s2"), dataset = "ch.test", archive_path = arch)
  expect_s3_class(out, "data.table")
  expect_equal(sort(unique(out$id)), c("s1", "s2"))
})

# ---------------------------------------------------------------------------
# read_local_open_ts — point-in-time reads
# ---------------------------------------------------------------------------

test_that("point-in-time read: date before any commits produces a clear error", {
  arch <- local_archive_dir()
  write_local_open_ts(make_dt(), series = "s1", dataset = "ch.test",
                      archive_path = arch)
  expect_error(
    read_local_open_ts("s1", dataset = "ch.test", archive_path = arch,
                       date = as.Date("2000-01-01")),
    regexp = "No commits found"
  )
})

test_that("point-in-time read: default date returns latest values after two writes", {
  arch <- local_archive_dir()
  dt1  <- make_dt()
  dt2  <- make_dt()
  dt2$value <- dt2$value * 10

  write_local_open_ts(dt1, series = "s1", dataset = "ch.test", archive_path = arch)
  write_local_open_ts(dt2, series = "s1", dataset = "ch.test", archive_path = arch)

  log <- gert::git_log(repo = file.path(arch, "ch.test"))
  expect_equal(nrow(log), 2L)

  out <- read_local_open_ts("s1", dataset = "ch.test", archive_path = arch)
  expect_equal(out$value, dt2$value)
})

test_that("show_vintage_dates adds vintage_date column", {
  arch <- local_archive_dir()
  write_local_open_ts(make_dt(), series = "s1", dataset = "ch.test",
                      archive_path = arch)

  out <- read_local_open_ts("s1", dataset = "ch.test", archive_path = arch,
                             show_vintage_dates = TRUE)
  expect_true(all(c("id", "vintage_date", "date", "value") %in% names(out)))
  expect_false("query_date"  %in% names(out))
  expect_false("commit_date" %in% names(out))
  expect_s3_class(out$vintage_date, "Date")
})

# ---------------------------------------------------------------------------
# read_ts_history — local archive mode
# ---------------------------------------------------------------------------

test_that("read_ts_history reads full vintage history from a local archive", {
  arch <- local_archive_dir()

  hist <- rbind(
    data.table(id = "s1", vintage_date = as.Date("2023-01-01"),
               date = seq(as.Date("2020-01-01"), by = "month", length.out = 6),
               value = 1:6),
    data.table(id = "s1", vintage_date = as.Date("2024-01-01"),
               date = seq(as.Date("2020-01-01"), by = "month", length.out = 7),
               value = c(1:6, 99))
  )
  write_local_open_ts(hist, dataset = "ch.test", archive_path = arch)

  out <- read_ts_history("s1", dataset = "ch.test", archive_path = arch,
                          consolidate = FALSE)

  expect_s3_class(out, "data.table")
  expect_named(out, c("id", "vintage_date", "date", "value"))
  expect_equal(sort(unique(out$vintage_date)),
               c(as.Date("2023-01-01"), as.Date("2024-01-01")))
  expect_equal(nrow(out[vintage_date == as.Date("2023-01-01")]), 6L)
  expect_equal(nrow(out[vintage_date == as.Date("2024-01-01")]), 7L)
})

test_that("read_ts_history local mode errors clearly when dataset is missing", {
  arch <- local_archive_dir()
  expect_error(
    read_ts_history("s1", archive_path = arch),
    regexp = "`dataset` is required"
  )
})

# ---------------------------------------------------------------------------
# read_local_open_ts — error handling
# ---------------------------------------------------------------------------

test_that("errors with a clear message when dataset does not exist", {
  arch <- local_archive_dir()
  expect_error(
    read_local_open_ts("s1", dataset = "no.such.dataset", archive_path = arch),
    regexp = "No local archive found"
  )
})

test_that("errors with a clear message when series CSV does not exist", {
  arch <- local_archive_dir()
  write_local_open_ts(make_dt(), series = "s1", dataset = "ch.test",
                      archive_path = arch)
  expect_error(
    read_local_open_ts("no_such_series", dataset = "ch.test", archive_path = arch),
    regexp = "Could not read series"
  )
})
