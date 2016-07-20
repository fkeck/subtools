
rawText <- function(x){
  if(inherits(x, "Subtitles")){
    res <- x[ , "Text"]
    res <- paste(res, collapse = " ")
  }
  if(inherits(x, "MultiSubtitles")){
    res <- lapply(x, function(x) x[ , "Text"])
    res <- do.call("c", res)
    res <- paste(res, collapse = " ")
  }
  return(res)
}