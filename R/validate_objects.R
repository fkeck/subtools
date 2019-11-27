# .validate_subtitles
# .validate_metadata

# Sanity check for subtitles objects
.validate_subtitles <- function(x) {

  if(!is(x, "subtitles")) stop("A subtitles object must inherit from subtitles.")
  if(!is(x, "data.frame")) stop("A subtitles object must inherit from data.frame.")

  if(!"ID" %in% colnames(x)) stop("A subtitles object must have an 'ID' column.")
  if(!"Timecode_in" %in% colnames(x)) stop("A subtitles object must have an 'Timecode_in' column.")
  if(!"Timecode_out" %in% colnames(x)) stop("A subtitles object must have an 'Timecode_out' column.")
  if(!"Text_content" %in% colnames(x)) stop("A subtitles object must have an 'Text_content' column.")

  if(!is(x$Timecode_in, "hms")) stop("The 'Timecode_in' column of a Subtitle object must inherit from hms.")
  if(!is(x$Timecode_out, "hms")) stop("The 'Timecode_out' column of a Subtitle object must inherit from hms.")

}

#extract metadata columns from a subtitles object
extract_metadata <- function(x){
  sub_names <- c("ID", "Timecode_in", "Timecode_out", "Text_content")
  md_names <- setdiff(colnames(x), sub_names)
  res <- x[, md_names]
  return(res)
}
