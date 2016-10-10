
#' Clean subtitles
#'
#' Functions to clean subtitles. \code{cleanTags} cleans formatting tags.
#' \code{cleanCaptions} cleans close captions.
#' \code{cleanPatterns} provides a more general and flexible cleaning based on regular expressions.
#'
#' @param x a \code{Subtitles} or \code{MultiSubtitles} object.
#' @param format the original format of the \code{Subtitles} objects.
#' @param pattern a character string containing a regular expression to be matched and cleaned.
#' @param clean.empty logical. Should empty remaining lines ("") deleted after cleaning.
#'
#' @return A \code{Subtitles} or \code{MultiSubtitles} object.
#' @export
#' @rdname clean
cleanTags <- function(x, format = "srt", clean.empty = TRUE){

  if(!(is(x, "Subtitles")|is(x, "MultiSubtitles"))){
    stop("x must be a 'Subtitles' or a 'MultiSubtitles' object.")
  }

  if(is(x, "MultiSubtitles")){

    x <- lapply(x, cleanTags, format = format, clean.empty = clean.empty)
    class(x) <- "MultiSubtitles"

  } else {

    format <- match.arg(format,
                        choices = c("srt", "subrip",
                                    "sub", "subviewer", "microdvd",
                                    "ssa", "ass", "substation", "all"),
                        several.ok = FALSE)

    if(format %in% c("srt", "subrip", "all")){
      x$subtitles$Text <- gsub("<.+?>", "", x$subtitles$Text)
    }

    if(format %in% c("ass", "ssa", "substation", "all")){
      x$subtitles$Text <- gsub("\\{\\\\.+?\\}", "", x$subtitles$Text)
    }

    if(clean.empty){
      x$subtitles <- x$subtitles[x$subtitles$Text != "", ]
    }
  }
  return(x)
}


#' @rdname clean
#' @export
cleanCaptions <- function(x, clean.empty = TRUE){

  if(!(is(x, "Subtitles")|is(x, "MultiSubtitles"))){
    stop("x must be a 'Subtitles' or a 'MultiSubtitles' object.")
  }

  if(is(x, "MultiSubtitles")){

    x <- lapply(x, cleanCaptions, clean.empty = clean.empty)
    class(x) <- "MultiSubtitles"

  } else {

    x$subtitles$Text <- gsub("\\( .+? \\)", "", x$subtitles$Text)

    if(clean.empty){
      x$subtitles <- x$subtitles[x$subtitles$Text != "", ]
    }
  }
  return(x)
}


#' @rdname clean
#' @export
cleanPatterns <- function(x, pattern, clean.empty = TRUE){

  if(!(is(x, "Subtitles")|is(x, "MultiSubtitles"))){
    stop("x must be a 'Subtitles' or a 'MultiSubtitles' object.")
  }

  if(is(x, "MultiSubtitles")){

    x <- lapply(x, cleanPatterns, pattern = pattern, clean.empty = clean.empty)
    class(x) <- "MultiSubtitles"

  } else {

    x$subtitles$Text <- gsub(pattern, "", x$subtitles$Text)

    if(clean.empty){
      x$subtitles <- x$subtitles[x$subtitles$Text != "", ]
    }
  }
  return(x)
}


#' Reorganize subtitles as sentences
#'
#' This function reorganizes a \code{Subtitles} object
#' in order that each subtitle line is a complete sentence.
#'
#' @param x a \code{Subtitles} object.
#'
#' @return A \code{Subtitles} object.
#'
#' @examples
#' f <- system.file("extdata", "ex_subrip.srt", package = "subtools")
#' s <- read.subtitles(f)
#' sentencify(s)
#'
#' @export
#'
sentencify <- function(x){
  ended <- grep("[\\.\\?!]\"$|[\\.\\?!]$", x$subtitles$Text)
  f <- as.factor(findInterval(1:length(x$subtitles$Text)-1, ended))
  new.txt <- split(x$subtitles$Text, f)
  new.txt <- sapply(new.txt, paste, collapse = " ", USE.NAMES = FALSE)

  new.tcin <- split(x$subtitles$Timecode.in, f)
  new.tcin <- sapply(new.tcin, min, USE.NAMES = FALSE)

  new.tcout <- split(x$subtitles$Timecode.out, f)
  new.tcout <- sapply(new.tcout, max, USE.NAMES = FALSE)

  new.txt <- strsplit(new.txt, split = "(?<=[\\.\\?!]|--) (?=[-A-Z])", perl = TRUE)
  new.txt.length <- sapply(new.txt, length)

  fun <- function(tcin, tcout, n){
    d <- .diff_timecodes(tcin, tcout)
    d <- .div_timecode(d, n)
    if(n == 1){
      r1 <- tcin
      r2 <- .add_timecodes(tcin, d)
    } else {
      r1 <- r2 <- rep(NA, n)
      for(i in 1:n){
        if(i == 1){
          r1[i] <- tcin
          r2[i] <- .add_timecodes(tcin, d)
        } else {
          r1[i] <- .add_timecodes(r1[i-1], d)
          r2[i] <- .add_timecodes(r1[i], d)
        }
      }
      r1 <- unlist(lapply(r1, .add_timecodes, y = "00:00:00.010"))
      r2 <- unlist(lapply(r2, .diff_timecodes, y = "00:00:00.010"))
    }
    return(list(r1, r2))
  }

  tc <- mapply(function(tcin, tcout, n) fun(tcin, tcout, n),
               tcin = new.tcin, tcout = new.tcout, n = new.txt.length,
               SIMPLIFY = FALSE)
  new.tcin <- unlist(lapply(tc, function(x) x[[1]]))
  new.tcout <- unlist(lapply(tc, function(x) x[[2]]))
  new.txt <- unlist(new.txt)
  new.id <- seq_len(length(new.txt))
  res <- Subtitles(text = new.txt, timecode.in = new.tcin,
                   timecode.out = new.tcout, id = new.id,
                   metadata = x$metadata)
  return(res)
}


