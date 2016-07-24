
#' Clean subtitles
#'
#' Functions to clean subtitles. \code{cleanTags} cleans formatting tags.
#' \code{cleanCaptions} cleans close captions.
#' \code{cleanPatterns} provide a more general and flexible cleaning function base on regular expressions.
#'
#' @param x a \code{Subtitles} object.
#' @param format the original format of the \code{Subtitles} object.
#' @pattern a character string containing a regular expression to be matched and clean.
#' @param clean.empty logical.Should empty remaining lines ("") deleted after cleaning.
#'
#' @return A \code{Subtitles} object.
#' @export
#' @rdname clean
cleanTags <- function(x, format = "srt", clean.empty = TRUE){

  if(!is(x, "Subtitles")){
    stop("x must be a 'Subtitles' object.")
  }

  format <- match.arg(format, choices = c("srt", "subrip",
                                          "sub", "subviewer", "microdvd",
                                          "ssa", "ass", "substation",
                                          "vtt", "webvtt", "all"), several.ok = FALSE)

  if(format %in% c("srt", "subrip", "all")){
    x$subtitles$Text <- gsub("<.+?>", "", x$subtitles$Text)
  }

  if(format %in% c("ass", "ssa", "substation", "all")){
    x$subtitles$Text <- gsub("\\{\\\\.+?\\}", "", x$subtitles$Text)
  }

  if(clean.empty){
    x$subtitles <- x$subtitles[x$subtitles$Text != "", ]
  }

  return(x)
}


#' @rdname clean
cleanCaptions <- function(x, clean.empty = TRUE){

  if(!is(x, "Subtitles")){
    stop("x must be a 'Subtitles' object.")
  }
  x$subtitles$Text <- gsub("\\( .+? \\)", "", x$subtitles$Text)

  if(clean.empty){
    x$subtitles <- x$subtitles[x$subtitles$Text != "", ]
  }
  return(x)
}


#' @rdname clean
cleanPatterns <- function(x, pattern, clean.empty = TRUE){

  if(!is(x, "Subtitles")){
    stop("x must be a 'Subtitles' object.")
  }
  x$subtitles$Text <- gsub(pattern, "", x$subtitles$Text)

  if(clean.empty){
    x$subtitles <- x$subtitles[x$subtitles$Text != "", ]
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
#' @export
#'
sentencify <- function(x){
  ended <- grep("[\\.\\?!♪]\"$|[\\.\\?!♪]$", x$subtitles$Text)
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


