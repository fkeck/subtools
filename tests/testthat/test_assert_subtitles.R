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
