
write.subtitles <- function(x, file, format = "srt"){
  
  if(!is(x, "Subtitles")){
    stop("x must be a 'Subtitles' object.")
  }
  tc <- paste(x$subtitles$Timecode.in, x$subtitles$Timecode.out, sep = " --> ")
  tc <- gsub("\\.", ",", tc)
  res <- paste(x$subtitles$ID, tc, x$subtitles$Text, sep = "\n", collapse = "\n\n")
  
  writeLines(res, file)
  
}