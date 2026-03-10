test_that("unnest_token uses the proper method for subtitles class object", {
  f <- system.file("extdata", "ex_webvtt.vtt", package = "subtools")
  s <- read_subtitles(file = f, metadata = data.frame(test = "Test"))
  testthat::expect_no_error(
    unnest_tokens(tbl = s)
  )
})

test_that("unnest_tokens works without metadata", {
  f <- system.file("extdata", "ex_webvtt.vtt", package = "subtools")
  s <- read_subtitles(file = f)
  testthat::expect_no_error(
    unnest_tokens(tbl = s)
  )
})

test_that("unnest_tokens default output column is Text_content", {
  f <- system.file("extdata", "ex_webvtt.vtt", package = "subtools")
  s <- read_subtitles(file = f, metadata = data.frame(test = "Test"))
  result <- unnest_tokens(tbl = s)
  expect_true("Text_content" %in% colnames(result))
  expect_s3_class(result, "subtitles")
})
test_that("unnest_token.subtitles works as expected", {
  skip_on_ci()
  skip_on_cran()
  f <- system.file("extdata", "ex_webvtt.vtt", package = "subtools")
  s <- read_subtitles(file = f, metadata = data.frame(test = "Test"))
  testthat::expect_snapshot(
    unnest_tokens(tbl = s)
  )
})
