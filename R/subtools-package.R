#' Read and Manipulate Video Subtitles
#'
#' A collection of functions to read, write and manipulate video subtitles.
#' Supported formats include "srt", "subrip", "sub", "subviewer", "microdvd",
#' "ssa", "ass", "substation", "vtt", and "webvtt".
#'
#' @keywords internal
#' @importFrom methods is
#' @importFrom utils read.csv
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows bind_cols
#' @importFrom readr guess_encoding
#' @importFrom hms hms
#' @importFrom tidytext unnest_tokens
#' @importFrom rlang .data as_label
#'
#' @name subtools
"_PACKAGE"
