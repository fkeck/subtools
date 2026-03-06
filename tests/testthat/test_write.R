test_that("write_subtitles creates a file", {
  s <- make_valid_subtitles()
  tmp <- tempfile(fileext = ".srt")
  on.exit(unlink(tmp))

  expect_no_error(write_subtitles(x = s, file = tmp))
  expect_true(file.exists(tmp))
})

test_that("write_subtitles produces a round-trip readable file", {
  f <- system.file("extdata", "ex_webvtt.vtt", package = "subtools")
  s <- read_subtitles(file = f)
  tmp <- tempfile(fileext = ".srt")
  on.exit(unlink(tmp))

  write_subtitles(x = s, file = tmp)
  s2 <- read_subtitles(file = tmp)

  expect_equal(s$Text_content, s2$Text_content)
  expect_equal(s$Timecode_in, s2$Timecode_in)
  expect_equal(s$Timecode_out, s2$Timecode_out)
})

test_that("write_subtitles errors on non-subtitles input", {
  tmp <- tempfile(fileext = ".srt")
  expect_error(write_subtitles(x = data.frame(), file = tmp), "subtitles")
})

test_that("write_subtitles errors on unwritable location", {
  s <- make_valid_subtitles()
  expect_error(
    write_subtitles(x = s, file = "/nonexistent/dir/file.srt"),
    "writable"
  )
})
