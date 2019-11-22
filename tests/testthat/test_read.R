library(subtools)
context("Reading subtitles files")

f_srt <- system.file("extdata", "test_lorem_subrip.srt", package = "subtools")
test_that("Reading SubRip format", {
  expect_is(read_subtitles(f_srt), "Subtitles")
  expect_is(read_subtitles(f_srt), "data.frame")
  expect_is(read_subtitles(f_srt), "tbl_df")
  expect_is(read_subtitles(f_srt), "tbl")
  expect_equal(ncol(read_subtitles(f_srt)), 4)
  expect_equal(nrow(read_subtitles(f_srt)), 6)
})


f_subv <- system.file("extdata", "test_lorem_subviewer.sub", package = "subtools")
test_that("Reading SubViewer format", {
  expect_is(read_subtitles(f_subv), "Subtitles")
  expect_is(read_subtitles(f_subv), "data.frame")
  expect_is(read_subtitles(f_subv), "tbl_df")
  expect_is(read_subtitles(f_subv), "tbl")
  expect_equal(ncol(read_subtitles(f_subv)), 4)
  expect_equal(nrow(read_subtitles(f_subv)), 6)
})


f_subm <- system.file("extdata", "test_lorem_microdvd.sub", package = "subtools")
test_that("Reading MicroDVD format", {
  expect_is(read_subtitles(f_subm), "Subtitles")
  expect_is(read_subtitles(f_subm), "data.frame")
  expect_is(read_subtitles(f_subm), "tbl_df")
  expect_is(read_subtitles(f_subm), "tbl")
  expect_equal(ncol(read_subtitles(f_subm)), 4)
  expect_equal(nrow(read_subtitles(f_subm)), 6)
})


f_ass <- system.file("extdata", "test_lorem_substation.ass", package = "subtools")
test_that("Reading SubStation Alpha format", {
  expect_is(read_subtitles(f_ass), "Subtitles")
  expect_is(read_subtitles(f_ass), "data.frame")
  expect_is(read_subtitles(f_ass), "tbl_df")
  expect_is(read_subtitles(f_ass), "tbl")
  expect_equal(ncol(read_subtitles(f_ass)), 4)
  expect_equal(nrow(read_subtitles(f_ass)), 6)
})


f_vtt <- system.file("extdata", "test_lorem_webvtt.vtt", package = "subtools")
test_that("Reading WebVTT format", {
  expect_is(read_subtitles(f_vtt), "Subtitles")
  expect_is(read_subtitles(f_vtt), "data.frame")
  expect_is(read_subtitles(f_vtt), "tbl_df")
  expect_is(read_subtitles(f_vtt), "tbl")
  expect_equal(ncol(read_subtitles(f_vtt)), 4)
  expect_equal(nrow(read_subtitles(f_vtt)), 6)
})


test_that("All example files produce same object", {
  expect_equal(read_subtitles(f_srt), read_subtitles(f_subv))
  expect_equal(read_subtitles(f_srt), read_subtitles(f_ass))
  expect_equal(read_subtitles(f_srt), read_subtitles(f_vtt))
})
# microdvd is not tested because it is hard to find timecode (frames)
# that exactly match the others (time)

