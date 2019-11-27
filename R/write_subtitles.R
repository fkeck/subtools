#' Write subtitles
#'
#' This function writes a \code{subtitles} object in a file.
#'
#' @param x an object of class \code{subtitles}.
#' @param file a character string naming a file for writing.
#' @param format a character string giving the file format.
#' Not used (only SubRip format is currently implemented).
#' @param encoding the name of the encoding to be used.
#'
#' @export
#'
write_subtitles <- function(x, file, format = "srt", encoding = "UTF-8"){

  if(!is(x, "subtitles")){
    stop("x must be a 'subtitles' object.")
  }
  tc <- paste(x$Timecode_in, x$Timecode_out, sep = " --> ")
  tc <- gsub("\\.", ",", tc)
  res <- paste(x$ID, tc, x$Text_content, sep = "\n", collapse = "\n\n")

  con <- file(file, encoding = encoding)
  writeLines(res, con)
  close(con)

}
