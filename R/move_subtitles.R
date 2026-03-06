#' Move subtitles
#' Move subtitles forward or backward in \code{subtitles}.
#'
#' @param x \code{subtitles} or \code{multisubtitles} objects to be moved.
#' @param lag \code{numeric.} Number of seconds the subtitles should be moved,
#'   forward if positive and backward if negative.
#'
#' @returns A \code{subtitles} or \code{multisubtitles} object.
#' @importFrom hms as_hms
#' @examples
#'   f1 <- system.file("extdata", "ex_subrip.srt", package = "subtools")
#'   s1 <- read_subtitles(f1, metadata = tibble::tibble(Season = 1, Episode = 2))
#'   s2 <- move_subtitles(s1, 2.7)
#' @export
#'
move_subtitles <- function(x, lag) {
    if (!(is(x, "subtitles") || is(x, "multisubtitles"))) {
        stop("x must be a 'subtitles' or a 'multisubtitles' object.")
    }
    if (!is.numeric(lag) || (length(lag) != 1L && length(lag) != nrow(x))) {
        stop("lag must be a vector of numbers of length 1 or equal to nrow(x)")
    }

    if (is(x, "multisubtitles")) {
        x <- lapply(x, move_subtitles, lag = lag)
        class(x) <- "multisubtitles"
    } else {
        x$Timecode_in <- as_hms(x$Timecode_in + lag)
        x$Timecode_out <- as_hms(x$Timecode_out + lag)
    }

    return(x)
}
