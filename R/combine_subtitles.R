#' Bind subtitles
#'
#' Bind \code{subtitles} or/and \code{multisubtitles} objects.
#'
#' @param ... \code{subtitles} or \code{multisubtitles} objects to be bound.
#' @param collapse logical. If \code{TRUE}, subtitles are combined in a single \code{subtitles} object.
#' @param sequential logical. If \code{TRUE} (default) timecodes
#' are recalculated to follow concatenation.
#'
#' @returns A \code{subtitles} object if \code{collapse = TRUE} (default).
#' A \code{multisubtitles} object if \code{collapse = FALSE}.
#'
#' @examples
#' f1 <- system.file("extdata", "ex_subrip.srt", package = "subtools")
#' s1 <- read_subtitles(f1, metadata = tibble::tibble(Season = 1, Episode = 2))
#' f2 <- system.file("extdata", "ex_substation.ass", package = "subtools")
#' s2 <- read_subtitles(f2, metadata = tibble::tibble(Season = 2))
#' bind_subtitles(s1, s2)
#' bind_subtitles(s1, s2, collapse = FALSE)
#' @export
#'
bind_subtitles <- function(..., collapse = TRUE, sequential = TRUE) {
  input <- list(...)
  sl <- list()
  for (i in seq_along(input)) {
    if (is(input[[i]], "multisubtitles")) {
      sl <- append(sl, unlist(input[i], recursive = FALSE))
    } else {
      sl <- append(sl, input[i])
    }
  }

  lapply(sl, .assert_subtitles)

  if (isTRUE(sequential)) {
    offsets <- c(
      0,
      cumsum(vapply(
        sl[-length(sl)],
        function(x) max(x$Timecode_out),
        numeric(1)
      ))
    )
    sl <- mapply(
      function(s, offset) {
        s$Timecode_in <- hms::as_hms(s$Timecode_in + offset)
        s$Timecode_out <- hms::as_hms(s$Timecode_out + offset)
        s
      },
      sl,
      offsets,
      SIMPLIFY = FALSE
    )
  }

  if (isTRUE(collapse)) {
    sl <- do.call(dplyr::bind_rows, lapply(sl, function(x) x))
    res <- subtitles(
      text = sl$Text_content,
      timecode.in = sl$Timecode_in,
      timecode.out = sl$Timecode_out,
      id = sl$ID,
      metadata = extract_metadata(sl)
    )
  } else {
    res <- sl
    class(res) <- "multisubtitles"
  }

  return(res)
}
