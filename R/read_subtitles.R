
#' Read subtitles
#'
#' Reads subtitles from a file.
#'
#' @param file the name of the file which the subtitles are to be read from.
#' If it does not contain an absolute path, the file name is relative to the current working directory.
#' @param format a character string specifying the format of the subtitles.
#' Five formats can be read: \code{"subrip"}, \code{"substation"}, \code{"microdvd"}, \code{"subviewer"} and \code{"webvtt"}.
#' Default is \code{"auto"} which tries to detect automatically the format of the file from its extension.
#'
#' @param clean.tags logical. If \code{"TRUE"}, formating tags are deleted from subtitles using \code{\link{cleanTags}}.
#' @param meta.data a list of metadata to be attached to the subtitles.
#'
#' @return
#' A \code{data.frame} with 4 columns (class \code{Subtitles}).
#'
#' @export
#'
#' @examples
read.subtitles <- function(file, format = "auto", clean.tags = TRUE, meta.data = list()){

  subs <- readLines(file, warn = FALSE)
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
    if(substr(subs[1] == "{")){
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


  res <- data.frame(subs.n, timecode.in, timecode.out, subs.txt, stringsAsFactors = FALSE)
  names(res) <- c("ID", "Timecode.in", "Timecode.out", "Text")
  res[ ,"Timecode.in"] <- .format_subtime(res[ ,"Timecode.in"])
  res[ ,"Timecode.out"] <- .format_subtime(res[ ,"Timecode.out"])
  class(res) <- c("Subtitles", "data.frame")

  if(clean.tags){
    res$Text <- cleanTags(res$Text, format = format)
  }

  if(length(meta.data) > 0){
    attr(res, "metadata") <- meta.data
  }

  return(res)
}



