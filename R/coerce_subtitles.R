
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
rawText <- function(x){
  if(inherits(x, "Subtitles")){
    res <- x$subtitles$Text
    res <- paste(res, collapse = " ")
  }
  if(inherits(x, "MultiSubtitles")){
    res <- lapply(x, function(x) x$substitles$Text)
    res <- do.call("c", res)
    res <- paste(res, collapse = " ")
  }
  return(res)
}
