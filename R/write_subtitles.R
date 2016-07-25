#' Write subtitles
#' 
#' This function writes a \code{Subtitles} object in a file.
#'
#' @param x an object of class \code{Subtitles}.
#' @param file a character string naming a file for writing.
#' @param format a character string giving the file format. Not used (only SubRip format is implemented).
#'
#' @export
#'
write.subtitles <- function(x, file, format = "srt"){
  
  if(!is(x, "Subtitles")){
    stop("x must be a 'Subtitles' object.")
  }
  tc <- paste(x$subtitles$Timecode.in, x$subtitles$Timecode.out, sep = " --> ")
  tc <- gsub("\\.", ",", tc)
  res <- paste(x$subtitles$ID, tc, x$subtitles$Text, sep = "\n", collapse = "\n\n")
  
  writeLines(res, file)
  
}