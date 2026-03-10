# test helper - create valid subtitles
make_valid_subtitles <- function(extra = NULL) {
  f <- system.file("extdata", "ex_webvtt.vtt", package = "subtools")
  read_subtitles(f, metadata = data.frame(test = "Test"))
}
