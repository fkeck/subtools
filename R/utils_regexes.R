#' Extract the file name from a path
#'
#' Strips trailing slashes and returns the last path component.
#'
#' @param x a character vector of file or directory paths.
#'
#' @returns A character vector of file names, the same length as \code{x}.
#'
#' @noRd
.extr_filename <- function(x) {
  x <- gsub("/+$", "", x)
  x <- regmatches(x, regexpr("([^/]+$)", x))
  return(x)
}

#' Extract the extension from a file name
#'
#' Returns the part of the file name after the last dot (lower-case alphanumeric only).
#'
#' @param x a character vector of file names or paths.
#'
#' @returns A character vector of file extensions (without the leading dot),
#' the same length as \code{x}.
#'
#' @noRd
.extr_extension <- function(x) {
  x <- sub("[?#].*$", "", x)
  is_url <- grepl("^[[:alpha:]][[:alnum:].+-]*://", x)
  x[is_url] <- sub("^[[:alpha:]][[:alnum:].+-]*://[^/]+/?", "", x[is_url])
  x <- tolower(x)
  x <- regmatches(x, regexpr("(?<=\\.)[0-9a-z]+$", x, perl = TRUE))
  return(x)
}

#' Extract season number from file paths
#'
#' Parses the file name component of each path and attempts to detect the season
#' number using four common naming conventions:
#' \code{S01}, \code{SEASON.2}, \code{S03E05}, and \code{2X05}.
#' Returns \code{NA} for paths where no convention matches.
#'
#' @param x a character vector of file or directory paths.
#'
#' @returns A numeric vector of season numbers, the same length as \code{x}.
#'
#' @noRd
# Guess subtitle format from subtitle content when no file extension is
# available, e.g. when parsing literal text.
.guess_subtitle_format <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & x != ""]

  if (length(x) == 0) {
    stop("Could not detect subtitle format from empty input.", call. = FALSE)
  }

  if (grepl("^WEBVTT", x[[1]])) {
    return("webvtt")
  }

  if (any(grepl("^Format:.*Start,.*Text", x)) && any(grepl("^Dialogue:", x))) {
    return("substation")
  }

  if (grepl("^\\{[0-9]+\\}\\{[0-9]+\\}", x[[1]])) {
    return("microdvd")
  }

  if (any(grepl("[[:blank:]]+-->[[:blank:]]+", x))) {
    return("subrip")
  }

  subviewer_time <- paste0(
    "^[0-9]{2}:[0-9]{2}:[0-9]{2}[\\.,][0-9]{2,3},",
    "[0-9]{2}:[0-9]{2}:[0-9]{2}[\\.,][0-9]{2,3}$"
  )
  if (any(grepl(subviewer_time, x))) {
    return("subviewer")
  }

  stop("Could not detect subtitle format; please specify `format`.", call. = FALSE)
}

# Extract Season number
.extr_snum <- function(x) {
  x <- .extr_filename(x)
  x <- toupper(x)

  res <- vector(mode = "character", length = length(x))

  mode0 <- grepl("[ -_\\.]S[0-9]+", x) # S01
  mode1 <- grepl("SEASON.{1}[0-9]+", x) # SEASON.2
  mode2 <- grepl("S[0-9]+E[0-9]+", x) # S03E05
  mode3 <- grepl("[ -_\\.][0-9]+X[0-9]+", x) # 2X05

  mode0.r <- unlist(regmatches(
    x,
    gregexpr("(?<=[ -_\\.]S)[0-9]+", x, perl = TRUE)
  ))
  mode1.r <- unlist(regmatches(
    x,
    gregexpr("(?<=SEASON.{1})[0-9]+", x, perl = TRUE)
  ))
  mode2.r <- unlist(regmatches(x, regexpr("S[0-9]+E[0-9]+", x)))
  mode2.r <- unlist(regmatches(
    mode2.r,
    gregexpr("(?<=S).*(?=E)", mode2.r, perl = TRUE)
  ))
  mode3.r <- unlist(regmatches(
    x,
    gregexpr("(?<=[ -_\\.])[0-9]+(?=X[0-9]+)", x, perl = TRUE)
  ))

  res[mode0] <- mode0.r
  res[mode1] <- mode1.r
  res[mode2] <- mode2.r
  res[mode3] <- mode3.r
  res <- as.numeric(res)
  return(res)
}

#' Extract episode number from file paths
#'
#' Parses the file name component of each path and attempts to detect the episode
#' number using four common naming conventions:
#' \code{E01}, \code{EPISODE.2}, \code{S03E05}, and \code{2x05}.
#' Returns \code{NA} for paths where no convention matches.
#'
#' @param x a character vector of file or directory paths.
#'
#' @returns A numeric vector of episode numbers, the same length as \code{x}.
#'
#' @noRd
.extr_enum <- function(x) {
  x <- .extr_filename(x)
  x <- toupper(x)

  res <- vector(mode = "character", length = length(x))

  mode0 <- grepl("[ -_\\.]E[0-9]+", x) # E01
  mode1 <- grepl("EPISODE.{1}[0-9]+", x) # EPISODE.2
  mode2 <- grepl("S[0-9]+E[0-9]+", x) # S03E05
  mode3 <- grepl("[ -_\\.][0-9]+X[0-9]+", x) # 2x05

  mode0.r <- unlist(regmatches(
    x,
    gregexpr("(?<=[ -_\\.]E)[0-9]+", x, perl = TRUE)
  ))
  mode1.r <- unlist(regmatches(
    x,
    gregexpr("(?<=EPISODE.{1})[0-9]+", x, perl = TRUE)
  ))
  mode2.r <- unlist(regmatches(x, regexpr("S[0-9]+E[0-9]+", x)))
  mode2.r <- unlist(regmatches(
    mode2.r,
    gregexpr("(?<=E).*", mode2.r, perl = TRUE)
  ))
  mode3.r <- unlist(regmatches(
    x,
    gregexpr("(?<=[0-9]X)[0-9]+", x, perl = TRUE)
  ))

  res[mode0] <- mode0.r
  res[mode1] <- mode1.r
  res[mode2] <- mode2.r
  res[mode3] <- mode3.r
  res <- as.numeric(res)
  return(res)
}


#' Test whether a string is a WebVTT cue timing line
#'
#' Used during WebVTT parsing to identify cue blocks and filter out
#' \code{REGION}, \code{STYLE}, and \code{NOTE} blocks.
#'
#' @param x a single character string (or \code{NA}).
#'
#' @returns A single logical: \code{TRUE} if \code{x} matches the WebVTT cue timing
#' pattern, \code{FALSE} otherwise (including when \code{x} is \code{NA}).
#'
#' @noRd
.test_cuetiming <- function(x) {
  if (is.na(x)) {
    res <- FALSE
  } else {
    res <- grepl(
      "^(?:[0-9]{2, }:)?[0-9]{2}:[0-9]{2}.[0-9]{3}[[:blank:]]+-->[[:blank:]]+(?:[0-9]{2, }:)?[0-9]{2}:[0-9]{2}.[0-9]{3}",
      x
    )
  }
  return(res)
}
