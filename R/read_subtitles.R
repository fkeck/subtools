#' Convert an R object to a `subtitles` object
#'
#' @md
#' @param x an R object that can be coerced into a `subtitles` object
#' @param format a character string specifying the format of the subtitles.
#' Four formats can be read: \code{"subrip"}, \code{"substation"}, \code{"microdvd"}, \code{"subviewer"} (v.2) and \code{"webvtt"}.
#' Default is \code{"auto"} which tries to detect automatically the format of the file from its extension.
#' @param clean.tags logical. If \code{"TRUE"} (default), formating tags are deleted from subtitles using \code{\link{clean_tags}}.
#' @param metadata a one-row dataframe or tibble, or any object that can be coerced
#' into a one-row tibble by \code{link[tibble]{as_tibble}}.
#' @param frame.rate a numeric value giving the frame rate in frames per second. Only relevant for MicroDVD format.
#' If \code{NA} (default), the function tries to extract the frame.rate from the file.
#' If it fails, the frame rate is set at 24p (23.976).
#' @param encoding the name of the encoding to be used. Default is "\code{auto}" and
#' uses \code{\link[readr]{guess_encoding}} to detect encoding.
#' @param ... passed on to downstream methods.
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
as_subtitle <- function(x, format = "auto", clean.tags = TRUE, metadata = data.frame(),
                        frame.rate = NA, encoding = "auto", ...){
  UseMethod("as_subtitle", x)
}

#' @rdname as_subtitle
#' @export
as_subtitle.default <- function(x, format = "auto", clean.tags = TRUE, metadata = data.frame(),
                                frame.rate = NA, encoding = "auto", ...){

  # Trim empty lines at the beginning and end of the file
  subs <- x
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

  # .sub can be microdvd or subviewer
  #This is a very light test to solve the .sub extension
  if(format == "sub"){
    if(substr(subs[1], start = 1L, stop = 1L) == "{"){
      format <- "microdvd"
    } else {
      format <- "subviewer"
    }
  }


  if(format %in% c("srt", "subrip")){

    # Get rid off multiple empty new lines
    empty_li <- which(subs == "")
    to_del <- empty_li[c(empty_li, -1) == c(-1, empty_li + 1)]
    if(length(to_del) > 0) {
      subs <- subs[-to_del]
    }

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
    timecode.in <- as.character(hms::hms(seconds = timecode.in))
    timecode.out <- sapply(timecode, function(x) x[2])
    timecode.out <- as.numeric(timecode.out) / frame.rate
    timecode.out <- as.character(hms::hms(seconds = timecode.out))

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

    blocks <- blocks[sapply(blocks, function(x) any(sapply(x , function(y) .test_cuetiming(y))))]

    blocks.tc.pos <- sapply(blocks, function(x) which(sapply(x , function(y) .test_cuetiming(y))))

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

  res <- subtitles(text = subs.txt, timecode.in = timecode.in,
                   timecode.out = timecode.out, id = subs.n,
                   metadata = metadata)

  if(clean.tags){
    res <- clean_tags(res, format = format)
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
#' An object of class \code{subtitles} (see \code{\link{subtitles}}).
#'
#' @examples
#'
#' # read a SubRip file
#' f <- system.file("extdata", "ex_subrip.srt", package = "subtools")
#' f <- system.file("extdata", "ex_webvtt.vtt", package = "subtools")
#' read_subtitles(f)
#'
#' @export
read_subtitles <- function(file, format = "auto", clean.tags = TRUE, metadata = data.frame(), frame.rate = NA, encoding = "auto"){

  if(format == "auto") {
    format <- .extr_extension(file)
  }

  if(encoding == "auto") {
    encoding <- readr::guess_encoding(file)$encoding[1]
  }

  if(encoding == "ASCII") {
    subs <- readLines(file)
  } else {
    con <- file(file, encoding = encoding)
    subs <- readLines(con, warn = FALSE, encoding = encoding)
    close(con)
  }

  as_subtitle(
    x = subs,
    format = format,
    clean.tags = clean.tags,
    metadata = metadata,
    frame.rate = frame.rate,
    encoding = encoding
  )

}


#' Create a \code{subtitles} object
#'
#' A \code{subtitles} is a special form of \code{tibble}.
#'
#' @param text a character vector of subtitles text content.
#' @param timecode.in a character vector giving the time that the subtitles appear on the screen.
#' The format must be "HH:MM:SS.mS".
#' @param timecode.out a character vector giving the time that the subtitles disappear.
#' The format must be "HH:MM:SS.mS".
#' @param id a vector of IDs for subtitles.
#' If not provided it is generated automatically from \code{timecode.in} order.
#' @param metadata a one-row dataframe or tibble, or any object that can be coerced
#' into a one-row tibble by \code{link[tibble]{as_tibble}}.
#'
#' @return a \code{subtitles} object i.e. a \code{tibble} with at least 4 columns containing IDs,
#' timecodes and text of the subtitles and optionally metadata in extra columns.
#'
#' @export
#'
subtitles <- function(text, timecode.in, timecode.out, id, metadata = data.frame()){
  if(missing(id)){
    warning("ID missing: A numerical sequence was used instead.")
    id <- order(timecode.in)
  }
  subtitles <- tibble::tibble(ID = as.character(id),
                              Timecode_in = as.character(timecode.in),
                              Timecode_out = as.character(timecode.out),
                              Text_content = as.character(text)
                              )
  subtitles <- tibble::remove_rownames(subtitles)
  # subtitles$Timecode_in <- sapply(subtitles$Timecode_in, .format_subtime)
  # subtitles$Timecode_out <- sapply(subtitles$Timecode_out, .format_subtime)

  subtitles$Timecode_in <- .format_subtime(subtitles$Timecode_in)
  subtitles$Timecode_out <- .format_subtime(subtitles$Timecode_out)

  if (nrow(metadata) == 1){
    metadata <- tibble::as_tibble(metadata)
    metadata <- metadata[rep(1, nrow(subtitles)), ]
    subtitles <- dplyr::bind_cols(subtitles, metadata)
  } else if (nrow(metadata) == nrow(subtitles)){
    metadata <- tibble::as_tibble(metadata)
    subtitles <- dplyr::bind_cols(subtitles, metadata)
  }

  class(subtitles) <- c("subtitles", class(subtitles))
  .validate_subtitles(subtitles)
  return(subtitles)
}


#' Print method for multisubtitles
#'
#' @param x a \code{multisubtitles} object.
#' @param printlen the maximum number of subtitles to print.
#' @param ... further arguments passed to or from other methods.
#'
#' @examples
#' f <- system.file("extdata", "ex_subrip.srt", package = "subtools")
#' s <- read_subtitles(f)
#' bind_subtitles(s, s, collapse = FALSE)
#'
#' @export
print.multisubtitles <- function(x, printlen = 10L, ...){
  cat("A multisubtitles object with", length(x), "elements\n")
  toprint <- ifelse(length(x) > printlen, printlen, length(x))
  for(i in 1:toprint){
    cat("subtitles object [[", i, "]]\n", sep = "")
    print(x[[i]])
    cat("\n\n")
  }
  if(length(x) > printlen) {
    cat("...and ", printlen - length(x), "other subtitles elements")
  }
}



#' Get basic informations for subtitle objects
#'
#' @param x a \code{subtitles} or \code{multisubtitles} object.
#'
#' @examples
#' s <- read_subtitles(
#'   system.file("extdata", "ex_subrip.srt", package = "subtools")
#' )
#' get_subtitles_info(s)
#'
#' @export
get_subtitles_info <- function(x){

  if(is(x, "subtitles")){
    cat("subtitles object")
    cat("\n  Text lines:", nrow(x))
    cat("\n  Duration:", as.character(hms::as_hms(x$Timecode_out[nrow(x)] - x$Timecode_in[1])))

    metadata <- setdiff(names(x), c("ID", "Timecode_in", "Timecode_out", "Text_content"))
    cat("\n  Metadata:", length(metadata))
    if(length(metadata) > 0) cat("tags:", paste0(metadata, collapse = ", "))
  }

  if(is(x, "multisubtitles")){
    cat("multisubtitles object with", length(x), "subtitles.")
  }
}

