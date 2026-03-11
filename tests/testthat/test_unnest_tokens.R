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

test_that("unnest_tokens with explicit output column name", {
  s <- make_valid_subtitles()
  result <- unnest_tokens(s, output = Word, input = Text_content)
  expect_true("Word" %in% colnames(result))
  expect_false("Text_content" %in% colnames(result)) # dropped by default
})

test_that("unnest_tokens with explicit output and drop = FALSE keeps input column", {
  s <- make_valid_subtitles()
  result <- unnest_tokens(s, output = Word, input = Text_content, drop = FALSE)
  expect_true("Word" %in% colnames(result))
  expect_true("Text_content" %in% colnames(result))
})

test_that("unnest_tokens with time.remapping = FALSE still returns a subtitles object", {
  s <- make_valid_subtitles()
  result <- unnest_tokens(s, time.remapping = FALSE)
  expect_s3_class(result, "subtitles")
  expect_true("Text_content" %in% colnames(result))
})

test_that("unnest_tokens.default is dispatched for non-subtitles objects", {
  # unnest_tokens.default forwards to tidytext but has a known NSE-forwarding
  # limitation; we just verify that dispatch reaches the default method (not
  # the .assert_subtitles path) by checking the error is not a subtitles-class error.
  df <- data.frame(
    id   = 1:2,
    text = c("hello world", "foo bar"),
    stringsAsFactors = FALSE
  )
  err <- tryCatch(
    unnest_tokens(df, output = word, input = text),
    error = function(e) e
  )
  expect_false(grepl("must inherit from class", conditionMessage(err)))
})
