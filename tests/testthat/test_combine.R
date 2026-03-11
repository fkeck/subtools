test_that("bind_subtitles returns a subtitles object when collapse = TRUE", {
  s <- make_valid_subtitles()
  result <- bind_subtitles(s, s)
  expect_s3_class(result, "subtitles")
})

test_that("bind_subtitles returns a multisubtitles object when collapse = FALSE", {
  s <- make_valid_subtitles()
  result <- bind_subtitles(s, s, collapse = FALSE)
  expect_s3_class(result, "multisubtitles")
  expect_length(result, 2L)
})

test_that("bind_subtitles sequential = FALSE leaves timecodes unchanged", {
  s <- make_valid_subtitles()
  result <- bind_subtitles(s, s, collapse = FALSE, sequential = FALSE)
  expect_equal(result[[1]]$Timecode_in, s$Timecode_in)
  expect_equal(result[[2]]$Timecode_in, s$Timecode_in)
  expect_equal(result[[1]]$Timecode_out, s$Timecode_out)
  expect_equal(result[[2]]$Timecode_out, s$Timecode_out)
})

test_that("bind_subtitles sequential = TRUE shifts second subtitle by max Timecode_out of first", {
  s <- make_valid_subtitles()
  # s has Timecode_in = c(1,5,11) and Timecode_out = c(4,9,14)
  # max(Timecode_out) of s = 14, so second s should be shifted by 14
  result <- bind_subtitles(s, s, collapse = FALSE, sequential = TRUE)
  expect_equal(result[[1]]$Timecode_in, s$Timecode_in)
  expect_equal(result[[1]]$Timecode_out, s$Timecode_out)
  expect_equal(result[[2]]$Timecode_in, hms::as_hms(c(1, 5, 11) + 14))
  expect_equal(result[[2]]$Timecode_out, hms::as_hms(c(4, 9, 14) + 14))
})

test_that("bind_subtitles sequential = TRUE accumulates offsets across three subtitles", {
  s <- make_valid_subtitles()
  # max Timecode_out of s = 14
  # offset[1] = 0, offset[2] = 14, offset[3] = 14 + 14 = 28
  result <- bind_subtitles(s, s, s, collapse = FALSE, sequential = TRUE)
  expect_equal(result[[1]]$Timecode_in, hms::as_hms(c(1, 5, 11) + 0))
  expect_equal(result[[2]]$Timecode_in, hms::as_hms(c(1, 5, 11) + 14))
  expect_equal(result[[3]]$Timecode_in, hms::as_hms(c(1, 5, 11) + 28))
})

test_that("bind_subtitles sequential collapsed result has correct row count", {
  s <- make_valid_subtitles()
  result <- bind_subtitles(s, s)
  expect_equal(nrow(result), nrow(s) * 2L)
})

test_that("bind_subtitles sequential collapsed timecodes are correct", {
  s <- make_valid_subtitles()
  result <- bind_subtitles(s, s)
  expect_equal(result$Timecode_in, hms::as_hms(c(1, 5, 11, 1 + 14, 5 + 14, 11 + 14)))
  expect_equal(result$Timecode_out, hms::as_hms(c(4, 9, 14, 4 + 14, 9 + 14, 14 + 14)))
})

test_that("bind_subtitles timecodes remain hms/difftime class after sequential shift", {
  s <- make_valid_subtitles()
  result <- bind_subtitles(s, s, collapse = FALSE, sequential = TRUE)
  expect_s3_class(result[[2]]$Timecode_in, c("hms", "difftime"), exact = TRUE)
  expect_s3_class(result[[2]]$Timecode_out, c("hms", "difftime"), exact = TRUE)
})

test_that("bind_subtitles unpacks multisubtitles input", {
  s <- make_valid_subtitles()
  multi <- bind_subtitles(s, s, collapse = FALSE, sequential = FALSE)
  result <- bind_subtitles(multi, collapse = FALSE, sequential = FALSE)
  expect_s3_class(result, "multisubtitles")
  expect_length(result, 2L)
})

test_that("bind_subtitles rejects non-subtitles input", {
  expect_error(bind_subtitles(data.frame()))
})
