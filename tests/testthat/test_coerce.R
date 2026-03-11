test_that("get_raw_text returns a single string from a subtitles object", {
  s <- make_valid_subtitles()
  result <- get_raw_text(s)
  expect_type(result, "character")
  expect_length(result, 1L)
})

test_that("get_raw_text collapses with default space", {
  s <- make_valid_subtitles()
  result <- get_raw_text(s)
  # multiple rows → joined by space
  expect_true(grepl(" ", result))
})

test_that("get_raw_text respects custom collapse argument", {
  s <- make_valid_subtitles()
  result_newline <- get_raw_text(s, collapse = "\n")
  expect_true(grepl("\n", result_newline))
  result_pipe <- get_raw_text(s, collapse = "|")
  expect_true(grepl("|", result_pipe, fixed = TRUE))
})

test_that("get_raw_text works on a multisubtitles object", {
  s <- make_valid_subtitles()
  multi <- bind_subtitles(s, s, collapse = FALSE)
  result <- get_raw_text(multi)
  expect_type(result, "character")
  expect_length(result, 1L)
})

test_that("get_raw_text multisubtitles result contains text from all elements", {
  s <- make_valid_subtitles()
  multi <- bind_subtitles(s, s, collapse = FALSE)
  result_multi <- get_raw_text(multi)
  result_single <- get_raw_text(s)
  # multi has twice the rows, so must be longer
  expect_gt(nchar(result_multi), nchar(result_single))
})
