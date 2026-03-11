# get_mkv_info ─────────────────────────────────────────────────────────────────

test_that("get_mkv_info errors on a non-existent file", {
  expect_error(get_mkv_info("/nonexistent/file.mkv"), "Invalid input")
})

# read_subtitles_mkv ───────────────────────────────────────────────────────────

test_that("read_subtitles_mkv errors if file does not exist", {
  expect_error(read_subtitles_mkv("/nonexistent/file.mkv"), "file not found")
})
