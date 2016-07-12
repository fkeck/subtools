
cleanTags <- function(x, format = "srt"){

  format <- match.arg(format, choices = c("srt", "subrip",
                                          "sub", "subviewer", "microdvd",
                                          "ssa", "ass", "substation",
                                          "vtt", "webvtt", "all"), several.ok = FALSE)

  if(format %in% c("srt", "subrip", "all")){
    x <- gsub("<.+?>", "", x)
  }

  if(format %in% c("ass", "ssa", "substation", "all")){
    x <- gsub("\\{\\\\.+?\\}", "", x)
  }

  return(x)
}
