
#' Clean subtitles
#'
#' Functions to clean subtitles. \code{clean_tags} cleans formatting tags.
#' \code{clean_captions} cleans close captions, i.e all text enclosed in parentheses or squared brackets.
#' \code{clean_patterns} provides a more general and flexible cleaning based on regular expressions.
#'
#' @param x a \code{subtitles} or \code{multisubtitles} object.
#' @param format the original format of the \code{subtitles} objects.
#' @param pattern a character string containing a regular expression to be matched and cleaned.
#' @param clean.empty logical. Should empty remaining lines ("") deleted after cleaning.
#'
#' @return A \code{subtitles} or \code{multisubtitles} object.
#' @export
#' @rdname clean
clean_tags <- function(x, format = "srt", clean.empty = TRUE){

  if(!(is(x, "subtitles")|is(x, "multisubtitles"))){
    stop("x must be a 'subtitles' or a 'multisubtitles' object.")
  }

  if(is(x, "multisubtitles")){

    x <- lapply(x, clean_tags, format = format, clean.empty = clean.empty)
    class(x) <- "multisubtitles"

  } else {
    .validate_subtitles(x)
    format <- match.arg(format,
                        choices = c("srt", "subrip",
                                    "sub", "subviewer", "microdvd",
                                    "ssa", "ass", "substation",
                                    "vtt", "webvtt", "all"),
                        several.ok = FALSE)

    if(format %in% c("srt", "subrip", "vtt", "webvtt", "all")){
      x$Text_content <- gsub("<.+?>", "", x$Text_content)
    }

    if(format %in% c("ass", "ssa", "substation", "all")){
      x$Text_content <- gsub("\\{\\\\.+?\\}", "", x$Text_content)
    }


    if(clean.empty){
      x <- x[x$Text_content != "", ]
    }
  }
  return(x)
}


#' @rdname clean
#' @export
clean_captions <- function(x, clean.empty = TRUE){

  if(!(is(x, "subtitles")|is(x, "multisubtitles"))){
    stop("x must be a 'subtitles' or a 'multisubtitles' object.")
  }

  if(is(x, "multisubtitles")){

    x <- lapply(x, clean_captions, clean.empty = clean.empty)
    class(x) <- "multisubtitles"

  } else {
    .validate_subtitles(x)

    x$Text_content <- gsub("\\(.+?\\)", "", x$Text_content)
    x$Text_content <- gsub("\\[.+?\\]", "", x$Text_content)

    if(clean.empty){
      x <- x[x$Text_content != "", ]
    }
  }
  return(x)
}


#' @rdname clean
#' @export
clean_patterns <- function(x, pattern, clean.empty = TRUE){

  if(!(is(x, "subtitles")|is(x, "multisubtitles"))){
    stop("x must be a 'subtitles' or a 'multisubtitles' object.")
  }

  if(is(x, "multisubtitles")){

    x <- lapply(x, clean_patterns, pattern = pattern, clean.empty = clean.empty)
    class(x) <- "multisubtitles"

  } else {
    .validate_subtitles(x)

    x$Text_content <- gsub(pattern, "", x$Text_content)

    if(clean.empty){
      x <- x[x$Text_content != "", ]
    }
  }
  return(x)
}


