
#' Clean subtitles
#'
#' Functions to clean subtitles. \code{clean_tags} cleans formatting tags.
#' \code{clean_captions} cleans close captions, i.e all text enclosed in parentheses or squared brackets.
#' \code{clean_patterns} provides a more general and flexible cleaning based on regular expressions.
#'
#' @param x a \code{Subtitles} or \code{MultiSubtitles} object.
#' @param format the original format of the \code{Subtitles} objects.
#' @param pattern a character string containing a regular expression to be matched and cleaned.
#' @param clean.empty logical. Should empty remaining lines ("") deleted after cleaning.
#'
#' @return A \code{Subtitles} or \code{MultiSubtitles} object.
#' @export
#' @rdname clean
clean_tags <- function(x, format = "srt", clean.empty = TRUE){

  if(!(is(x, "Subtitles")|is(x, "MultiSubtitles"))){
    stop("x must be a 'Subtitles' or a 'MultiSubtitles' object.")
  }

  if(is(x, "MultiSubtitles")){

    x <- lapply(x, clean_tags, format = format, clean.empty = clean.empty)
    class(x) <- "MultiSubtitles"

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

  if(!(is(x, "Subtitles")|is(x, "MultiSubtitles"))){
    stop("x must be a 'Subtitles' or a 'MultiSubtitles' object.")
  }

  if(is(x, "MultiSubtitles")){

    x <- lapply(x, clean_captions, clean.empty = clean.empty)
    class(x) <- "MultiSubtitles"

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

  if(!(is(x, "Subtitles")|is(x, "MultiSubtitles"))){
    stop("x must be a 'Subtitles' or a 'MultiSubtitles' object.")
  }

  if(is(x, "MultiSubtitles")){

    x <- lapply(x, clean_patterns, pattern = pattern, clean.empty = clean.empty)
    class(x) <- "MultiSubtitles"

  } else {
    .validate_subtitles(x)

    x$Text_content <- gsub(pattern, "", x$Text_content)

    if(clean.empty){
      x <- x[x$Text_content != "", ]
    }
  }
  return(x)
}


