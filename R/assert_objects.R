#' Validate a subtitles object
#'
#' Checks that \code{x} inherits from both \code{"subtitles"} and \code{"data.frame"},
#' has the four required columns (\code{ID}, \code{Timecode_in}, \code{Timecode_out},
#' \code{Text_content}), and that the two timecode columns are of class \code{"hms"}.
#' Stops with an informative error message if any check fails.
#'
#' @param x an object to validate.
#'
#' @returns Called for its side effects (validation). Returns \code{NULL} invisibly.
#'
#' @noRd
.assert_subtitles <- function(x) {
  if (!is(x, "subtitles")) {
    stop("A subtitles object must inherit from class \"subtitles\".")
  }
  if (!is(x, "data.frame")) {
    stop("A subtitles object must inherit from class \"data.frame\".")
  }

  if (!"ID" %in% colnames(x)) {
    stop("A subtitles object must have an 'ID' column.")
  }
  if (!"Timecode_in" %in% colnames(x)) {
    stop("A subtitles object must have a 'Timecode_in' column.")
  }
  if (!"Timecode_out" %in% colnames(x)) {
    stop("A subtitles object must have a 'Timecode_out' column.")
  }
  if (!"Text_content" %in% colnames(x)) {
    stop("A subtitles object must have a 'Text_content' column.")
  }

  if (!is(x$Timecode_in, "hms")) {
    stop(
      "The 'Timecode_in' column of a Subtitle object must inherit from class \"hms\"."
    )
  }
  if (!is(x$Timecode_out, "hms")) {
    stop(
      "The 'Timecode_out' column of a Subtitle object must inherit from class \"hms\"."
    )
  }
}

#' Extract metadata columns from a subtitles object
#'
#' Returns all columns that are not part of the core subtitle structure
#' (\code{ID}, \code{Timecode_in}, \code{Timecode_out}, \code{Text_content}).
#'
#' @param x a \code{subtitles} object.
#'
#' @returns A data frame (or tibble) containing only the metadata columns of \code{x}.
#' If there are no metadata columns, an empty data frame is returned.
#'
#' @noRd
extract_metadata <- function(x) {
  sub_names <- c("ID", "Timecode_in", "Timecode_out", "Text_content")
  md_names <- setdiff(colnames(x), sub_names)
  res <- x[, md_names]
  return(res)
}
