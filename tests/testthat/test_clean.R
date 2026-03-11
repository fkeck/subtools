# ── clean_tags ─────────────────────────────────────────────────────────────────

test_that("clean_tags removes srt HTML-style tags", {
  s <- make_valid_subtitles()
  s$Text_content[1] <- "<i>Hello</i> world"
  result <- clean_tags(s, format = "srt")
  expect_false(grepl("<", result$Text_content[1]))
  expect_true(grepl("Hello", result$Text_content[1]))
})

test_that("clean_tags removes ASS/SSA curly-brace tags", {
  f <- system.file("extdata", "ex_substation.ass", package = "subtools")
  s <- read_subtitles(f)
  s$Text_content[1] <- "{\\an8}Some text"
  result <- clean_tags(s, format = "ass")
  expect_false(grepl("\\{", result$Text_content[1]))
  expect_true(grepl("Some text", result$Text_content[1]))
})

test_that("clean_tags format = 'all' removes both srt and ass tags", {
  s <- make_valid_subtitles()
  s$Text_content[1] <- "<i>{\\an8}Mixed</i>"
  result <- clean_tags(s, format = "all")
  expect_false(grepl("<|\\{", result$Text_content[1]))
})

test_that("clean_tags accepts webvtt as an alias for srt tag removal", {
  s <- make_valid_subtitles()
  s$Text_content[1] <- "<b>Bold</b>"
  result <- clean_tags(s, format = "vtt")
  expect_false(grepl("<", result$Text_content[1]))
})

test_that("clean_tags clean.empty = FALSE keeps empty rows", {
  s <- make_valid_subtitles()
  s$Text_content[1] <- "<i></i>"
  result_keep <- clean_tags(s, format = "srt", clean.empty = FALSE)
  expect_equal(nrow(result_keep), nrow(s))
  result_drop <- clean_tags(s, format = "srt", clean.empty = TRUE)
  expect_lt(nrow(result_drop), nrow(s))
})

test_that("clean_tags works on a multisubtitles object", {
  s <- make_valid_subtitles()
  multi <- bind_subtitles(s, s, collapse = FALSE)
  result <- clean_tags(multi, format = "srt")
  expect_s3_class(result, "multisubtitles")
  expect_length(result, 2L)
})

test_that("clean_tags errors on non-subtitles input", {
  expect_error(clean_tags(data.frame()), "subtitles")
})


# ── clean_captions ─────────────────────────────────────────────────────────────

test_that("clean_captions removes parenthesised text", {
  s <- make_valid_subtitles()
  s$Text_content[1] <- "Hello (background noise) world"
  result <- clean_captions(s)
  expect_false(grepl("\\(", result$Text_content[1]))
  expect_true(grepl("Hello", result$Text_content[1]))
})

test_that("clean_captions removes square-bracket text", {
  s <- make_valid_subtitles()
  s$Text_content[1] <- "Hello [MUSIC] world"
  result <- clean_captions(s)
  expect_false(grepl("\\[", result$Text_content[1]))
})

test_that("clean_captions clean.empty = FALSE keeps rows that become empty", {
  s <- make_valid_subtitles()
  s$Text_content[1] <- "(caption only)"
  result_keep <- clean_captions(s, clean.empty = FALSE)
  expect_equal(nrow(result_keep), nrow(s))
  result_drop <- clean_captions(s, clean.empty = TRUE)
  expect_lt(nrow(result_drop), nrow(s))
})

test_that("clean_captions works on a multisubtitles object", {
  s <- make_valid_subtitles()
  multi <- bind_subtitles(s, s, collapse = FALSE)
  result <- clean_captions(multi)
  expect_s3_class(result, "multisubtitles")
  expect_length(result, 2L)
})

test_that("clean_captions errors on non-subtitles input", {
  expect_error(clean_captions(list()), "subtitles")
})


# ── clean_patterns ─────────────────────────────────────────────────────────────

test_that("clean_patterns removes text matching a regex", {
  s <- make_valid_subtitles()
  s$Text_content[1] <- "SPEAKER: Hello world"
  result <- clean_patterns(s, pattern = "^SPEAKER: ")
  expect_false(grepl("SPEAKER", result$Text_content[1]))
  expect_true(grepl("Hello", result$Text_content[1]))
})

test_that("clean_patterns clean.empty = FALSE keeps rows that become empty", {
  s <- make_valid_subtitles()
  s$Text_content[1] <- "DELETE_ME"
  result_keep <- clean_patterns(s, pattern = "DELETE_ME", clean.empty = FALSE)
  expect_equal(nrow(result_keep), nrow(s))
  result_drop <- clean_patterns(s, pattern = "DELETE_ME", clean.empty = TRUE)
  expect_lt(nrow(result_drop), nrow(s))
})

test_that("clean_patterns works on a multisubtitles object", {
  s <- make_valid_subtitles()
  multi <- bind_subtitles(s, s, collapse = FALSE)
  result <- clean_patterns(multi, pattern = "^>>")
  expect_s3_class(result, "multisubtitles")
  expect_length(result, 2L)
})

test_that("clean_patterns errors on non-subtitles input", {
  expect_error(clean_patterns(42, pattern = "x"), "subtitles")
})
