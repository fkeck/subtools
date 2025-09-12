# .assert_subtitles
# .extract_metadata

# Sanity check for subtitles objects
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

#extract metadata columns from a subtitles object
extract_metadata <- function(x) {
  sub_names <- c("ID", "Timecode_in", "Timecode_out", "Text_content")
  md_names <- setdiff(colnames(x), sub_names)
  res <- x[, md_names]
  return(res)
}
