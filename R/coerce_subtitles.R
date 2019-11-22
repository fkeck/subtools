
#' Get subtitles text
#'
#' This function extracts the raw text content of subtitles objects as a character string.
#'
#' @param x an object of class \code{Subtitles} or \code{MultiSubtitles}.
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
  if(inherits(x, "Subtitles")){
    .validate_subtitles(x)
    res <- x$Text_content
    res <- paste(res, collapse = collapse)
  }
  if(inherits(x, "MultiSubtitles")){
    lapply(x, .validate_subtitles)
    res <- lapply(x, function(x) x$Text_content)
    res <- do.call("c", res)
    res <- paste(res, collapse = collapse)
  }
  return(res)
}


