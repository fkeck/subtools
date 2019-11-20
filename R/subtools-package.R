#' Read and Manipulate Video Subtitles
#'
#' A collection of functions to read, write and manipulate video subtitles.
#' Supported formats include "srt", "subrip", "sub", "subviewer", "microdvd",
#' "ssa", "ass", "substation", "vtt", and "webvtt".
#'
#' @keywords internal
#' @importFrom tm Corpus VectorSource
#' @importFrom methods is
#' @importFrom utils read.csv
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows bind_cols
#' @importFrom readr guess_encoding
#'
#' @name subtools
"_PACKAGE"
