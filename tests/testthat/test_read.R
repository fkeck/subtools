library(subtools)
context("Reading subtitles files")

f <- system.file("extdata", "ex_subrip.srt", package = "subtools")
test_that("Reading SubRip format", {
  expect_is(read.subtitles(f), "Subtitles")
  expect_equal(length(read.subtitles(f)), 2)
  expect_is(read.subtitles(f)$subtitles, "data.frame")
  expect_equal(ncol(read.subtitles(f)$subtitles), 4)
  expect_equal(nrow(read.subtitles(f)$subtitles), 6)
  expect_is(read.subtitles(f)$metadata, "list")
})


f <- system.file("extdata", "ex_subviewer.sub", package = "subtools")
test_that("Reading SubViewer format", {
  expect_is(read.subtitles(f), "Subtitles")
  expect_equal(length(read.subtitles(f)), 2)
  expect_is(read.subtitles(f)$subtitles, "data.frame")
  expect_equal(ncol(read.subtitles(f)$subtitles), 4)
  expect_equal(nrow(read.subtitles(f)$subtitles), 6)
  expect_is(read.subtitles(f)$metadata, "list")
})


f <- system.file("extdata", "ex_microdvd.sub", package = "subtools")
test_that("Reading MicroDVD format", {
  expect_is(read.subtitles(f), "Subtitles")
  expect_equal(length(read.subtitles(f)), 2)
  expect_is(read.subtitles(f)$subtitles, "data.frame")
  expect_equal(ncol(read.subtitles(f)$subtitles), 4)
  expect_equal(nrow(read.subtitles(f)$subtitles), 6)
  expect_is(read.subtitles(f)$metadata, "list")
})


f <- system.file("extdata", "ex_substation.ass", package = "subtools")
test_that("Reading SubStation Alpha format", {
  expect_is(read.subtitles(f), "Subtitles")
  expect_equal(length(read.subtitles(f)), 2)
  expect_is(read.subtitles(f)$subtitles, "data.frame")
  expect_equal(ncol(read.subtitles(f)$subtitles), 4)
  expect_equal(nrow(read.subtitles(f)$subtitles), 6)
  expect_is(read.subtitles(f)$metadata, "list")
})


f <- system.file("extdata", "ex_webvtt.vtt", package = "subtools")
test_that("Reading WebVTT format", {
  expect_is(read.subtitles(f), "Subtitles")
  expect_equal(length(read.subtitles(f)), 2)
  expect_is(read.subtitles(f)$subtitles, "data.frame")
  expect_equal(ncol(read.subtitles(f)$subtitles), 4)
  expect_equal(nrow(read.subtitles(f)$subtitles), 2)
  expect_is(read.subtitles(f)$metadata, "list")
})
