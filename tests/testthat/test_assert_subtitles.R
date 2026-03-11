# Helper to build a minimal valid subtitles object
test_that("valid subtitles pass silently", {
  s <- make_valid_subtitles()
  expect_invisible(.assert_subtitles(s))
})

test_that("non‑subtitles class triggers error", {
  not_sub <- data.frame(ID = 1L)
  class(not_sub) <- "data.frame"
  expect_error(
    object = .assert_subtitles(not_sub),
    regexp = "must inherit from class \"subtitles\""
  )
})

test_that("missing required columns are reported", {
  s <- make_valid_subtitles()
  s$ID <- NULL # drop a required column
  expect_error(
    object = .assert_subtitles(s),
    regexp = "A subtitles object must have an 'ID' column."
  )
  s <- make_valid_subtitles()
  s$Timecode_in <- NULL # drop a required column
  expect_error(
    object = .assert_subtitles(s),
    regexp = "A subtitles object must have a 'Timecode_in' column."
  )
  s <- make_valid_subtitles()
  s$Timecode_out <- NULL # drop a required column
  expect_error(
    object = .assert_subtitles(s),
    regexp = "A subtitles object must have a 'Timecode_out' column."
  )
  s <- make_valid_subtitles()
  s$Text_content <- NULL # drop a required column
  expect_error(
    object = .assert_subtitles(s),
    regexp = "A subtitles object must have a 'Text_content' column."
  )
})

test_that("non‑hms timecode columns raise an error", {
  s <- make_valid_subtitles()
  s$Timecode_in <- as.numeric(s$Timecode_in) # break the class
  expect_error(
    object = .assert_subtitles(s),
    regexp = "The 'Timecode_in' column of a Subtitle object must inherit from class \"hms\"."
  )
  s <- make_valid_subtitles()
  s$Timecode_out <- as.numeric(s$Timecode_out) # break the class
  expect_error(
    object = .assert_subtitles(s),
    regexp = "The 'Timecode_out' column of a Subtitle object must inherit from class \"hms\"."
  )
})

test_that("subtitles with 'subtitles' class but not data.frame triggers error", {
  x <- structure(list(), class = "subtitles")
  expect_error(
    .assert_subtitles(x),
    regexp = "must inherit from class \"data.frame\""
  )
})

# ── extract_metadata ──────────────────────────────────────────────────────────

test_that("extract_metadata returns an empty tibble when no extra columns", {
  s <- make_valid_subtitles()
  # remove the extra 'test' column added by make_valid_subtitles
  s$test <- NULL
  meta <- extract_metadata(s)
  expect_equal(ncol(meta), 0L)
})

test_that("extract_metadata returns only non-core columns", {
  s <- make_valid_subtitles() # has a 'test' column
  meta <- extract_metadata(s)
  expect_true("test" %in% colnames(meta))
  expect_false("ID" %in% colnames(meta))
  expect_false("Timecode_in" %in% colnames(meta))
  expect_false("Timecode_out" %in% colnames(meta))
  expect_false("Text_content" %in% colnames(meta))
})

test_that("extract_metadata preserves multiple metadata columns", {
  s <- make_valid_subtitles()
  s$Season <- 1L
  s$Episode <- 2L
  meta <- extract_metadata(s)
  expect_true(all(c("test", "Season", "Episode") %in% colnames(meta)))
})
