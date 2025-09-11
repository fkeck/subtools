test_that("unnest_token uses the proper method for subtitles class object", {
  f <- system.file("extdata", "ex_webvtt.vtt", package = "subtools")
  s <- read_subtitles(f, metadata = data.frame(test = "Test"))
  testthat::expect_no_error(
    unnest_tokens(s)
  )
})
test_that("unnest_token.subtitles works as expected", {
  skip_on_ci()
  skip_on_cran()
  f <- system.file("extdata", "ex_webvtt.vtt", package = "subtools")
  s <- read_subtitles(f, metadata = data.frame(test = "Test"))
  testthat::expect_snapshot(
    unnest_tokens(s)
  )
})
