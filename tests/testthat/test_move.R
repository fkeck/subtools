test_that("move_subtitles works as expected", {
  test_object <- make_valid_subtitles()
  test_object <- move_subtitles(test_object, lag = 1)

  expect_equal(object = test_object$Timecode_in, hms::as_hms(c(1, 5, 11) + 1))
  expect_equal(object = test_object$Timecode_out, hms::as_hms(c(4, 9, 14) + 1))
  expect_s3_class(
    object = test_object$Timecode_in,
    class = c("hms", "difftime"),
    exact = TRUE
  )
})

test_that("move_subtitles works with multisubtitles", {
  s <- make_valid_subtitles()
  multi <- bind_subtitles(s, s, collapse = FALSE, sequential = FALSE)
  result <- move_subtitles(multi, lag = 1)

  expect_s3_class(result, "multisubtitles")
  expect_equal(result[[1]]$Timecode_in, hms::as_hms(c(1, 5, 11) + 1))
  expect_equal(result[[2]]$Timecode_in, hms::as_hms(c(1, 5, 11) + 1))
})

test_that("move_subtitles detects problems as expected", {
  test_object <- make_valid_subtitles()
  expect_error(move_subtitles(test_object, lag = "1"))
  expect_error(move_subtitles(test_object, lag = 1:2))
  expect_error(move_subtitles(data.frame(), lag = 1))
})
