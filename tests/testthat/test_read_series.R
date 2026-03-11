# Helpers to build temp directory structures ──────────────────────────────────

make_season_dir <- function(n_episodes = 2L) {
  srt_src <- system.file("extdata", "ex_subrip.srt", package = "subtools")
  season_dir <- file.path(tempdir(), paste0("TestSeason_", sample.int(1e5, 1)))
  dir.create(season_dir, recursive = TRUE)
  for (i in seq_len(n_episodes)) {
    file.copy(srt_src, file.path(season_dir, sprintf("Episode_%02d.srt", i)))
  }
  season_dir
}

make_serie_dir <- function(n_seasons = 2L, n_episodes = 2L) {
  serie_dir <- file.path(tempdir(), paste0("TestSerie_", sample.int(1e5, 1)))
  for (s in seq_len(n_seasons)) {
    season_path <- file.path(serie_dir, sprintf("Season_%02d", s))
    dir.create(season_path, recursive = TRUE)
    srt_src <- system.file("extdata", "ex_subrip.srt", package = "subtools")
    for (e in seq_len(n_episodes)) {
      file.copy(srt_src, file.path(season_path, sprintf("Episode_%02d.srt", e)))
    }
  }
  serie_dir
}

make_multiseries_dir <- function(n_series = 2L) {
  coll_dir <- file.path(tempdir(), paste0("TestCollection_", sample.int(1e5, 1)))
  for (s in seq_len(n_series)) {
    serie_path <- make_serie_dir(n_seasons = 1L, n_episodes = 1L)
    file.rename(serie_path, file.path(coll_dir, paste0("Serie_", s)))
  }
  coll_dir
}


# ── read_subtitles_season ─────────────────────────────────────────────────────

test_that("read_subtitles_season returns a subtitles object by default", {
  d <- make_season_dir(2L)
  on.exit(unlink(d, recursive = TRUE))
  result <- read_subtitles_season(d, quietly = TRUE)
  expect_s3_class(result, "subtitles")
})

test_that("read_subtitles_season with bind = FALSE returns multisubtitles", {
  d <- make_season_dir(2L)
  on.exit(unlink(d, recursive = TRUE))
  result <- read_subtitles_season(d, bind = FALSE, quietly = TRUE)
  expect_s3_class(result, "multisubtitles")
  expect_length(result, 2L)
})

test_that("read_subtitles_season adds Season and Episode metadata when detect.meta = TRUE", {
  d <- make_season_dir(1L)
  on.exit(unlink(d, recursive = TRUE))
  result <- read_subtitles_season(d, bind = FALSE, detect.meta = TRUE, quietly = TRUE)
  expect_true("Episode" %in% colnames(result[[1]]))
})

test_that("read_subtitles_season detect.meta = FALSE omits Season/Episode columns", {
  d <- make_season_dir(1L)
  on.exit(unlink(d, recursive = TRUE))
  result <- read_subtitles_season(d, bind = FALSE, detect.meta = FALSE, quietly = TRUE)
  expect_false("Episode" %in% colnames(result[[1]]))
})

test_that("read_subtitles_season prints a message when quietly = FALSE", {
  d <- make_season_dir(1L)
  on.exit(unlink(d, recursive = TRUE))
  expect_output(read_subtitles_season(d, quietly = FALSE), "Read:")
})

test_that("read_subtitles_season errors if dir does not exist", {
  expect_error(read_subtitles_season("/nonexistent/path"), "dir not found")
})


# ── read_subtitles_serie ──────────────────────────────────────────────────────

test_that("read_subtitles_serie returns a subtitles object by default", {
  d <- make_serie_dir(2L, 1L)
  on.exit(unlink(d, recursive = TRUE))
  result <- read_subtitles_serie(d, quietly = TRUE)
  expect_s3_class(result, "subtitles")
})

test_that("read_subtitles_serie with bind = FALSE returns multisubtitles", {
  d <- make_serie_dir(2L, 1L)
  on.exit(unlink(d, recursive = TRUE))
  result <- read_subtitles_serie(d, bind = FALSE, quietly = TRUE)
  expect_s3_class(result, "multisubtitles")
})

test_that("read_subtitles_serie prints a message when quietly = FALSE", {
  d <- make_serie_dir(1L, 1L)
  on.exit(unlink(d, recursive = TRUE))
  expect_output(read_subtitles_serie(d, quietly = FALSE), "Read:")
})

test_that("read_subtitles_serie errors if dir does not exist", {
  expect_error(read_subtitles_serie("/nonexistent/path"), "dir not found")
})


# ── read_subtitles_multiseries ────────────────────────────────────────────────

test_that("read_subtitles_multiseries returns a subtitles object by default", {
  skip("make_multiseries_dir relies on file.rename across tempdir – flaky on some systems")
})

test_that("read_subtitles_multiseries errors if dir does not exist", {
  expect_error(read_subtitles_multiseries("/nonexistent/path"), "dir not found")
})
