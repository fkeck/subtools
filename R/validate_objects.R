# .validate_subtitles
# .validate_metadata


extract_metadata <- function(x){
  sub_names <- c("ID", "Timecode_in", "Timecode_out", "Text_content")
  md_names <- setdiff(colnames(x), sub_names)
  res <- x[, md_names]
  return(res)
}
