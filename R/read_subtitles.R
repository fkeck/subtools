#' Convert an R object to a `Subtitles` object
#'
#' @md
#' @param an R object that can be coerced into a `Subtitles` object
#' @param format a character string specifying the format of the subtitles.
#' Four formats can be read: \code{"subrip"}, \code{"substation"}, \code{"microdvd"}, \code{"subviewer"} (v.2) and \code{"webvtt"}.
#' Default is \code{"auto"} which tries to detect automatically the format of the file from its extension.
#' @param clean.tags logical. If \code{"TRUE"} (default), formating tags are deleted from subtitles using \code{\link{cleanTags}}.
#' @param metadata a named list of metadata to be attached to the subtitles.
#' @param frame.rate a numeric value giving the frame rate in frames per second. Only relevant for MicroDVD format.
#' If \code{NA} (default), the function tries to extract the frame.rate from the file.
#' If it fails, the frame rate is set at 24p (23.976).
#' @param encoding the name of the encoding to be used.
#' @param ... passed on to downstream methods
#' @export
#' @examples
#' as_subtitle(
#'   c("WEBVTT",
#'     "X-TIMESTAMP-MAP=MPEGTS:181083,LOCAL:00:00:00.000",
#'     "",
#'     "3",
#'     "00:00:21.199 --> 00:00:22.333", ">> FEMALE SPEAKER:",
#'     "Don't stay up too late.",
#'     "",
#'     ""
#'   ), format = "webvtt"
#' )
as_subtitle <- function(x, format = "auto", clean.tags = TRUE, metadata = list(),
                        frame.rate = NA, encoding = "UTF-8", ...){
  UseMethod("as_subtitle", x)
}

#' @rdname as_subtitle
#' @export
as_subtitle.default <- function(x, format = "auto", clean.tags = TRUE, metadata = list(),
                                frame.rate = NA, encoding = "UTF-8", ...){

  subs <- x
  i <- 1
  while(subs[i] == ""){i <- i + 1}
  j <- length(subs)
  while(subs[j] == ""){j <- j - 1}
  subs <- subs[seq.int(i, j)]


  format <- match.arg(format, choices = c("srt", "subrip",
                                          "sub", "subviewer", "microdvd",
                                          "ssa", "ass", "substation",
                                          "vtt", "webvtt","auto"),
                      several.ok = FALSE)

  if(format == "sub"){                  #This is a very light test to solve the .sub extension
    if(substr(subs[1], start = 1L, stop = 1L) == "{"){
      format <- "microdvd"
    } else {
      format <- "subviewer"
    }
  }


  if(format %in% c("srt", "subrip")){
    subs.newlines <- c(0, which(subs == ""))
    subs.n.li <- subs.newlines + 1
    subs.time.li <- subs.newlines + 2
    subs.txt.li <- mapply(seq, from = subs.time.li + 1, to = c(subs.newlines[-1], length(subs) + 1) - 1)

    subs.n <- subs[subs.n.li]
    subs.time <- subs[subs.time.li]
    subs.txt <- sapply(subs.txt.li, function(x) paste(subs[x], collapse = " "))

    subs.time <- strsplit(subs.time, split = " --> ")
    timecode.in <- sapply(subs.time, function(x) x[1])
    timecode.out <- sapply(subs.time, function(x) x[2])

  }

  if(format %in% c("ssa", "ass", "substation")){
    subs.events.h <- subs[grep("^Format:.*Start,.*Text", subs)]

    subs.events.li <- grep("^Dialogue:", subs)
    subs.events <- subs[subs.events.li]

    comma.pos <- gregexpr(",", subs.events)
    comma.min <- min(sapply(comma.pos, length))

    subs.txt <- substr(subs.events, start = comma.pos[[1]][comma.min] + 1, stop = 10^5L)

    subs.events <- substr(subs.events, start = 1, stop = comma.pos[[1]][comma.min])
    subs.events <- paste(subs.events, collapse = "\n")
    subs.events <- paste(subs.events.h, subs.events, sep = "\n")
    subs.events <- read.csv(textConnection(subs.events), header = TRUE,
                            sep = ",", quote = "", stringsAsFactors = FALSE, fill = T)

    subs.txt <- gsub("\\\\[Nn]", " ", subs.txt)
    timecode.in <- subs.events[, "Start"]
    timecode.out <- subs.events[, "End"]
    subs.n <- order(timecode.in)
  }

  if(format == "microdvd"){

    if(is.na(frame.rate)){
      if(grepl("\\{[10]\\}\\{[10]\\}([0-9\\.]+)$", subs[1])){
        frame.rate <- as.numeric(gsub("(\\{.+\\})+", "", subs[1]))
        subs <- subs[-1]
      } else {
        frame.rate <- 23.976
      }
    }

    subs <- gsub("\\|(\\{.+\\})+", "|", subs)

    timecode <- regmatches(subs, regexpr("^\\{[0-9]+\\}\\{[0-9]+\\}", subs))
    timecode <- strsplit(timecode, split = "\\}\\{")
    timecode <- lapply(timecode, gsub, pattern = "[\\{\\}]", replacement = "")
    timecode.in <- sapply(timecode, function(x) x[1])
    timecode.in <- as.numeric(timecode.in) / frame.rate
    timecode.in <- .s_to_hms(timecode.in)
    timecode.out <- sapply(timecode, function(x) x[2])
    timecode.out <- as.numeric(timecode.out) / frame.rate
    timecode.out <- .s_to_hms(timecode.out)

    subs.txt <- gsub("(\\{.+\\})+", "", subs)
    subs.txt <- gsub("\\|", " ", subs.txt)

    subs.n <- order(timecode.in)
  }

  if(format == "subviewer"){

    subs <- subs[!grepl("^\\[.+\\]", subs)]
    subs <- subs[seq(min(which(subs != "")), max(which(subs != "")))]
    subs.newlines <- c(0, which(subs == ""))
    subs.time.li <- subs.newlines + 1
    subs.txt.li <- subs.newlines + 2

    subs.txt <- subs[subs.txt.li]
    subs.txt <- gsub("\\[br\\]", " ", subs.txt)

    subs.time <- subs[subs.time.li]
    subs.time <- strsplit(subs.time, split = ",")
    timecode.in <- sapply(subs.time, function(x) x[1])
    timecode.out <- sapply(subs.time, function(x) x[2])

    subs.n <- order(timecode.in)

  }

  if(format %in% c("webvtt", "vtt")){

    if(!grepl("^WEBVTT", subs[1])) {
      stop("Invalid WebVTT format")
    }

    subs <- subs[-1]      # Remove header
    subs <- c(subs, "")   # Add a virtual last line

    subs.newlines <- c(0, which(subs == ""))
    blocks.delim <- cbind(subs.newlines[-length(subs.newlines)] + 1, subs.newlines[-1] - 1)
    blocks <- apply(blocks.delim, 1, function(x) subs[x[1]:x[2]])


    # Select Cue blocks only (drop REGION, STYLE, NOTE)
    test.cuetiming <- function(x){
      if (is.na(x)){
        res <- FALSE
      } else {
        res <- grepl("^(?:[0-9]{2, }:)?[0-9]{2}:[0-9]{2}.[0-9]{3}[[:blank:]]+-->[[:blank:]]+(?:[0-9]{2, }:)?[0-9]{2}:[0-9]{2}.[0-9]{3}", x)
      }
      return(res)
    }

    blocks <- blocks[sapply(blocks, function(x) any(sapply(x , function(y) test.cuetiming(y))))]

    blocks.tc.pos <- sapply(blocks, function(x) which(sapply(x , function(y) test.cuetiming(y))))

    subs.n <- mapply(function(x, y, z) ifelse(y == 1, z, x[1]),
                     x = blocks,
                     y = blocks.tc.pos,
                     z = seq(1, length(blocks))) # If cues are not labelled, they are numbered

    #Extract and clean timing
    subs.time <- mapply(function(x, y) x[y], x = blocks, y = blocks.tc.pos)
    subs.time <- regmatches(subs.time, regexpr("^(?:[0-9]{2, }:)?[0-9]{2}:[0-9]{2}.[0-9]{3}[[:blank:]]+-->[[:blank:]]+(?:[0-9]{2, }:)?[0-9]{2}:[0-9]{2}.[0-9]{3}", subs.time))

    # Extract text content and deal with escape sequences
    subs.txt <- mapply(function(x, y) paste(x[(y + 1) : length(x)], collapse = " "),
                       x = blocks,
                       y = blocks.tc.pos)
    subs.txt <- gsub("&amp;", "&", subs.txt)
    subs.txt <- gsub("&lt;", "<", subs.txt)
    subs.txt <- gsub("&gt;", ">", subs.txt)
    subs.txt <- gsub("&lrm;|&rlm;", "", subs.txt) # Left-to-right mark and right-to-left mark are not supported
    subs.txt <- gsub("&nbsp;", " ", subs.txt)

    subs.time <- strsplit(subs.time, split = "[[:blank:]]+-->[[:blank:]]+")
    timecode.in <- sapply(subs.time, function(x) x[1])
    timecode.out <- sapply(subs.time, function(x) x[2])
  }

  res <- Subtitles(text = subs.txt, timecode.in = timecode.in,
                   timecode.out = timecode.out, id = subs.n,
                   metadata = metadata)

  if(clean.tags){
    res <- cleanTags(res, format = format)
  }

  return(res)

}

#' @rdname as_subtitle
#' @export
as_subtitle.character <- as_subtitle.default

#' Read subtitles
#'
#' Reads subtitles from a file.
#'
#' @param file the name of the file which the subtitles are to be read from.
#' If it does not contain an absolute path, the file name is relative to the current working directory.
#' @inheritParams as_subtitle
#'
#' @details The support of WebVTT is basic and experimental.
#'
#' @return
#' An object of class \code{Subtitles} (see \code{\link{Subtitles}}).
#'
#' @examples
#'
#' # read a SubRip file
#' f <- system.file("extdata", "ex_subrip.srt", package = "subtools")
#' f <- system.file("extdata", "ex_webvtt.vtt", package = "subtools")
#' read.subtitles(f)
#'
#' # snake case
#' read_subtitles(
#'   system.file("extdata", "ex_webvtt.vtt", package = "subtools"),
#'   format = "webvtt"
#' )
#'
#' @export
#'
read.subtitles <- function(file, format = "auto", clean.tags = TRUE, metadata = list(), frame.rate = NA, encoding = "UTF-8"){

  if(format == "auto"){
    format <- .extr_extension(file)
  }

  con <- file(file, encoding = encoding)
  subs <- readLines(con, warn = FALSE, encoding = encoding)
  close(con)

  as_subtitle(
    x = subs,
    format = format,
    clean.tags = clean.tags,
    metadata = metadata,
    frame.rate = frame.rate,
    encoding = encoding
  )

}

#' @rdname read.subtitles
#' @export
#' @noRd
read_subtitles <- read.subtitles

#' Create a \code{Subtitles} object
#'
#' This function creates objects of class \code{Subtitles}.
#'
#' @param text a character vector of subtitles text content.
#' @param timecode.in a character vector giving the time that the subtitles appear on the screen.
#' The format must be "HH:MM:SS.mS".
#' @param timecode.out a character vector giving the time that the subtitles disappear.
#' The format must be "HH:MM:SS.mS".
#' @param id a vector of numeric ID for subtitles.
#' If not provided it is generated automatically from \code{timecode.in} order.
#' @param metadata a named list of metadata to be attached to the subtitles.
#'
#' @return a \code{Subtitles} object i.e. a list of 2 elements:
#' \describe{
#'   \item{\code{subtitles}}{a \code{data.frame} with 4 columns containing IDs, timecodes and text of the subtitles.}
#'   \item{\code{metadata}}{a named list of metadata attached to the subtitles.}
#' }
#' @export
#'
Subtitles <- function(text, timecode.in, timecode.out, id, metadata = list()){
  if(missing(id)){
    id <- order(timecode.in)
  }
  subtitles <- data.frame(id, timecode.in, timecode.out, text, stringsAsFactors = FALSE)
  names(subtitles) <- c("ID", "Timecode.in", "Timecode.out", "Text")
  subtitles[ ,"Timecode.in"] <- .format_subtime(subtitles[ ,"Timecode.in"])
  subtitles[ ,"Timecode.out"] <- .format_subtime(subtitles[ ,"Timecode.out"])

  res <- list(subtitles = subtitles, metadata = metadata)
  class(res) <- "Subtitles"
  return(res)
}


#' Print methods for subtitles
#'
#' @param x a \code{Subtitles} or \code{MultiSubtitles} object.
#' @param printlen the maximum number of subtitles to print.
#' @param ... further arguments passed to or from other methods.
#'
#' @export
#' @rdname print_sub
print.Subtitles <- function(x, printlen = 1000L, ...){
  xlen <- dim(x$subtitles)[1]
  if(printlen > xlen){
    print(x$subtitles, ...)
  } else {
    print(x$subtitles[seq_len(printlen), ], ...)
    cat("-----", xlen - printlen, "lines omitted.")
  }
}

#' @rdname print_sub
#' @export
print.MultiSubtitles <- function(x, printlen = 10L, ...){
  cat("MultiSubtitles object:\n")
  for(i in 1:length(x)){
    cat("Subtitles object [[", i, "]]\n", sep = "")
    print.Subtitles(x[[i]], printlen = printlen)
    cat("\n\n")
  }
}

#' Extract parts of \code{Subtitles} objects
#'
#' @param x a \code{Subtitles} object.
#' @param i a vector of elements to extract.
#' Can be numeric, character, or logical.
#'
#' @return A \code{Subtitles} object.
#'
#' @export
#'
#'
`[.Subtitles` <- function(x, i){
  if (!missing(i)) {
    x$subtitles <- x$subtitles[i, ]
  }
  return(x)
}


#' Summary methods for subtitles
#'
#' @param object a \code{Subtitles} or \code{MultiSubtitles} object.
#' @param ... further arguments passed to or from other methods.
#'
#' @export
#' @rdname summary_sub
summary.Subtitles <- function(object, ...){
  cat("Subtitles object")
  cat("\n  Text lines:", dim(object$subtitles)[1])
  cat("\n  Duration:", .diff_timecodes(object$subtitles$Timecode.out[dim(object$subtitles)[1]], object$subtitles$Timecode.in[1]))
  cat("\n  Metadata:", length(object$metadata), "tags:", paste0(names(object$metadata), collapse = ", "))
}

#' @rdname summary_sub
#' @export
summary.MultiSubtitles <- function(object, ...){
  cat("MultiSubtitles object with", length(object), "Subtitles.")
}
