test_that("Reading SubRip format", {
  f_srt <- system.file("extdata", "test_lorem_subrip.srt", package = "subtools")
  test_object_srt <- read_subtitles(f_srt)
  expect_s3_class(
    object = test_object_srt,
    class = c("subtitles", "data.frame", "tbl_df", "tbl")
  )
  expect_equal(ncol(test_object_srt), 4)
  expect_equal(nrow(test_object_srt), 6)
})


test_that("Reading SubViewer format", {
  f_subv <- system.file(
    "extdata",
    "test_lorem_subviewer.sub",
    package = "subtools"
  )
  test_object_subv <- read_subtitles(f_subv)
  expect_s3_class(
    object = test_object_subv,
    class = c("subtitles", "data.frame", "tbl_df", "tbl")
  )
  expect_equal(ncol(test_object_subv), 4)
  expect_equal(nrow(test_object_subv), 6)
})


test_that("Reading MicroDVD format", {
  f_subm <- system.file(
    "extdata",
    "test_lorem_microdvd.sub",
    package = "subtools"
  )
  test_object_subm <- read_subtitles(f_subm)
  expect_s3_class(
    object = test_object_subm,
    class = c("subtitles", "data.frame", "tbl_df", "tbl")
  )
  expect_equal(ncol(test_object_subm), 4)
  expect_equal(nrow(test_object_subm), 6)
})


test_that("Reading SubStation Alpha format", {
  f_ass <- system.file(
    "extdata",
    "test_lorem_substation.ass",
    package = "subtools"
  )
  test_object_ass <- read_subtitles(f_ass)
  expect_s3_class(
    object = test_object_ass,
    class = c("subtitles", "data.frame", "tbl_df", "tbl")
  )
  expect_equal(ncol(test_object_ass), 4)
  expect_equal(nrow(test_object_ass), 6)
})


test_that("Reading WebVTT format", {
  f_vtt <- system.file("extdata", "test_lorem_webvtt.vtt", package = "subtools")
  test_object_vtt <- read_subtitles(f_vtt)
  expect_s3_class(
    object = test_object_vtt,
    class = c("subtitles", "data.frame", "tbl_df", "tbl")
  )
  expect_equal(ncol(test_object_vtt), 4)
  expect_equal(nrow(test_object_vtt), 6)
})


test_that("All example files produce same object", {
  f_srt <- system.file("extdata", "test_lorem_subrip.srt", package = "subtools")
  f_subv <- system.file(
    "extdata",
    "test_lorem_subviewer.sub",
    package = "subtools"
  )
  f_ass <- system.file(
    "extdata",
    "test_lorem_substation.ass",
    package = "subtools"
  )
  f_vtt <- system.file("extdata", "test_lorem_webvtt.vtt", package = "subtools")
  expect_equal(read_subtitles(f_srt), read_subtitles(f_subv))
  expect_equal(read_subtitles(f_srt), read_subtitles(f_ass))
  expect_equal(read_subtitles(f_srt), read_subtitles(f_vtt))
})
# microdvd is not tested because it is hard to find timecode (frames)
# that exactly match the others (time)


test_that("read_subtitles errors on non-existent file", {
  expect_error(
    read_subtitles("non_existent_file.srt"),
    "file not found"
  )
})

test_that("read_subtitles_season errors on non-existent dir", {
  expect_error(
    read_subtitles_season("non_existent_dir"),
    "dir not found"
  )
})

test_that("read_subtitles_serie errors on non-existent dir", {
  expect_error(
    read_subtitles_serie("non_existent_dir"),
    "dir not found"
  )
})

test_that("read_subtitles_multiseries errors on non-existent dir", {
  expect_error(
    read_subtitles_multiseries("non_existent_dir"),
    "dir not found"
  )
})

test_that("read_subtitles_mkv errors on non-existent file", {
  expect_error(
    read_subtitles_mkv("non_existent_file.mkv"),
    "file not found"
  )
})
