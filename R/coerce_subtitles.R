
#' Title
#'
#' @param x xxx
#' @param collapse xxx
#'
#' @return xxx
#' @export
#'
rawText <- function(x, collapse = " "){
  if(inherits(x, "Subtitles")){
    res <- x$subtitles$Text
    res <- paste(res, collapse = collapse)
  }
  if(inherits(x, "MultiSubtitles")){
    res <- lapply(x, function(x) x$substitles$Text)
    res <- do.call("c", res)
    res <- paste(res, collapse = collapse)
  }
  return(res)
}
