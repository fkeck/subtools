# Extract file name from dir paths
.extr_filename <- function(x) {
  x <- gsub("/+$", "", x)
  x <- regmatches(x, regexpr("([^/]+$)", x))
  return(x)
}

# Extract extension from file name
.extr_extension <- function(x) {
  x <- sub("[?#].*$", "", x)
  is_url <- grepl("^[[:alpha:]][[:alnum:].+-]*://", x)
  x[is_url] <- sub("^[[:alpha:]][[:alnum:].+-]*://[^/]+/?", "", x[is_url])
  x <- tolower(x)
  x <- regmatches(x, regexpr("(?<=\\.)[0-9a-z]+$", x, perl = TRUE))
  return(x)
}

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

# Extract Episode number
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


# For WebVTT parsing: select cue blocks only (drop REGION, STYLE, NOTE)
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
