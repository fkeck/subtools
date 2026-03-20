#' Parse subtitle timecode strings into hms objects
#'
#' Accepts timecode strings in \code{HH:MM:SS.mS} or \code{MM:SS.mS} format
#' (comma as decimal separator is also accepted). Missing hour components are
#' padded to zero.
#'
#' @param x a character vector of timecode strings.
#'
#' @returns An \code{hms} vector (from the \pkg{hms} package), the same length as \code{x}.
#'
#' @noRd
.format_subtime <- function(x) {
  x <- gsub(",", ".", x)
  x <- strsplit(x, split = ":")

  l <- lapply(x, length)
  x <- mapply(
    function(x, l) c(rep("00", -l + 3), x),
    x = x,
    l = l,
    SIMPLIFY = FALSE
  )

  x.h <- sapply(x, function(x) as.numeric(x[[1]]))
  x.m <- sapply(x, function(x) as.numeric(x[[2]]))
  x.s <- sapply(x, function(x) as.numeric(x[[3]]))

  res <- hms::hms(seconds = x.s, minutes = x.m, hours = x.h)
  return(res)
}
