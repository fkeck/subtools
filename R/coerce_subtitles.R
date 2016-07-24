
#' Get subtitles text
#'
#' This function returns the raw text content of subtitles objects as a character string.
#'
#' @param x an object of class \code{Subtitles} or \code{MultiSubtitles}.
#' @param collapse a character string to separate the subtitles lines.
#'
#' @return A character string.
#' @export
#'
rawText <- function(x, collapse = " "){
  if(inherits(x, "Subtitles")){
    res <- x$subtitles$Text
    res <- paste(res, collapse = collapse)
  }
  if(inherits(x, "MultiSubtitles")){
    res <- lapply(x, function(x) x$subtitles$Text)
    res <- do.call("c", res)
    res <- paste(res, collapse = collapse)
  }
  return(res)
}
