
#' Get subtitles text
#'
#' This function extracts the raw text content of subtitles objects as a character string.
#'
#' @param x an object of class \code{subtitles} or \code{multisubtitles}.
#' @param collapse a character string to separate the subtitles lines.
#'
#' @return A character string.
#'
#' @examples
#' f <- system.file("extdata", "ex_subrip.srt", package = "subtools")
#' s <- read_subtitles(f)
#' get_raw_text(s)
#' cat(get_raw_text(s, collapse = "\n"))
#'
#' @export
#'
get_raw_text <- function(x, collapse = " "){
  if(inherits(x, "subtitles")){
    .validate_subtitles(x)
    res <- x$Text_content
    res <- paste(res, collapse = collapse)
  }
  if(inherits(x, "multisubtitles")){
    lapply(x, .validate_subtitles)
    res <- lapply(x, function(x) x$Text_content)
    res <- do.call("c", res)
    res <- paste(res, collapse = collapse)
  }
  return(res)
}


