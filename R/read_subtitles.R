
#' Read subtitles
#'
#' Reads subtitles from a file.
#'
#' @param file the name of the file which the subtitles are to be read from.
#' If it does not contain an absolute path, the file name is relative to the current working directory.
#' @param format a character string specifying the format of the subtitles.
#' Five formats can be read: \code{"subrip"}, \code{"substation"}, \code{"microdvd"}, \code{"subviewer"} (v.2) and \code{"webvtt"}.
#' Default is \code{"auto"} which tries to detect automatically the format of the file from its extension.
#' @param clean.tags logical. If \code{"TRUE"}, formating tags are deleted from subtitles using \code{\link{cleanTags}}.
#' @param metadata a named list of metadata to be attached to the subtitles.
#' @param frame.rate a numeric value giving the frame rate in frames per second. Only relevant for MicroDVD format.
#' If \code{NA} (default), the function tries to extract the frame.rate from the file.
#' If it fails the frame rate is set at 24p (23.976).
#' @param encoding the name of the encoding to be used.
#'
#'
#' @return
#' An object of class \code{Subtitles} (see \code{\link{Subtitles}}).
#'
#' @export
#'
read.subtitles <- function(file, format = "auto", clean.tags = TRUE, metadata = list(), frame.rate = NA, encoding = "UTF-8"){

  con <- file(file, encoding = encoding)
  subs <- readLines(con, warn = FALSE, encoding = encoding)
  close(con)
  
  i <- 1
  while(subs[i] == ""){i <- i + 1}
  j <- length(subs)
  while(subs[j] == ""){j <- j - 1}
  subs <- subs[seq.int(i, j)]


  format <- match.arg(format, choices = c("srt", "subrip",
                                          "sub", "subviewer", "microdvd",
                                          "ssa", "ass", "substation",
                                          "vtt", "webvtt", "auto"),
                      several.ok = FALSE)
  if(format == "auto"){
    format <- .extr_extension(file)
  }
  if(format == "sub"){                  #This is a very light test
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
  
  res <- Subtitles(text = subs.txt, timecode.in = timecode.in,
                   timecode.out = timecode.out, id = subs.n,
                   metadata = metadata)

  if(clean.tags){
    res <- cleanTags(res, format = format)
  }

  return(res)
}



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


#' Print \code{Subtitles} objects
#'
#' @param x a \code{Subtitles} object.
#' @param printlen the maximum number of subtitles to print.
#'
#' @export
#'
print.Subtitles <- function(x, printlen = 1000L){
  cat("Subtitles object:\n")
  xlen <- dim(x$subtitles)[1]
  if(printlen > xlen){
    print(x$subtitles)
  } else {
    print(x$subtitles[seq_len(printlen), ])
    cat("-----", xlen - printlen, "lines omitted.")
  }
}

#' Extract parts of \code{Subtitles} objects
#'
#' @param x a \code{Subtitles} object.
#' @param i elements to extract or replace.
#' Can be numeric, character, or logical.
#' 
#' @return A \code{Subtitles} object.
#'
#' @export
#'
`[.Subtitles` <- function(x, i){
  if (!missing(i)) {
    x$subtitles <- x$subtitles[i, ]
  }
  return(x)
}

