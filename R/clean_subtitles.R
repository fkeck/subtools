# Utilities functions

cleanTags <- function(x, format = "srt"){
  
  format <- match.arg(format, choices = c("srt", "sub", "ssa", "ass", "webvtt", "all"), several.ok = FALSE)
  
  if(format %in% c("srt", "all")){
    x <- gsub("<.+?>", "", x)
  }
  
  if(format %in% c("ass", "ssa", "all")){
    x <- gsub("\\{\\\\.+?\\}", "", x)
  }
  
  return(x)
}