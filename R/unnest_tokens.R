

#' Split a column into tokens
#'
#' This function extends unnest_tokens to subtitles objects. The main difference with the data.frame method
#' is the possibility to perform timecode remapping according to the split of the input column.
#'
#' @inheritParams tidytext::unnest_tokens
#' @param time.remapping a logical. If \code{TRUE} (default), subtitle timecodes are recalculated
#' to take into account the split of the input column.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' f <- system.file("extdata", "ex_webvtt.vtt", package = "subtools")
#' s <- read_subtitles(f)
#'
#' require(tidytext)
#' unnest_tokens(s, Word, Text_content)
#' unnest_tokens(s, Word, Text_content, token = "lines")
unnest_tokens.subtitles <- function(tbl, output, input, token = "words",
                                    format = c("text", "man", "latex", "html", "xml"),
                                    time.remapping = TRUE, to_lower = TRUE, drop = TRUE,
                                    collapse = NULL, ...){
  quo_output <- dplyr::enquo(output)
  quo_input <- dplyr::enquo(input)

  .validate_subtitles(tbl)

  # We drop the subtitles class because unnest_tokens.data.frame
  # is not exported by tidytext
  class(tbl) <- c("tbl_df", "tbl", "data.frame")

  if(time.remapping) {

    tbl_IDX <- seq_len(nrow(tbl))
    tbl_DFT <- tbl$Timecode_out - tbl$Timecode_in

    tbl$.INTERNAL_unnest_tokens.subtitles_IDX <- tbl_IDX
    res <- tidytext::unnest_tokens(tbl = tbl, output = !!quo_output,
                                   input = !!quo_input, token = token,
                                   format = format, to_lower = to_lower,
                                   drop = FALSE, collapse = collapse, ...)

    expand_tbl <- match(res$.INTERNAL_unnest_tokens.subtitles_IDX, tbl_IDX)

    res_nchar <- nchar(res[[as_label(quo_output)]])
    res_char_tot <- unlist(tapply(res_nchar, res$.INTERNAL_unnest_tokens.subtitles_IDX,
                                  function(x) rep(sum(x), length(x))), use.names = FALSE)
    res_char_in <- unlist(tapply(res_nchar, res$.INTERNAL_unnest_tokens.subtitles_IDX,
                                 function(x) c(0, cumsum(x)[-length(x)])), use.names = FALSE)
    res_char_out <- unlist(tapply(res_nchar, res$.INTERNAL_unnest_tokens.subtitles_IDX,
                                  function(x) cumsum(x)), use.names = FALSE)

    time_char <- tbl_DFT[expand_tbl] / res_char_tot
    res$Timecode_out <- hms::as_hms(as.numeric(round(res$Timecode_in + res_char_out * time_char, digits = 4)))
    res$Timecode_in <- hms::as_hms(as.numeric(round(res$Timecode_in + res_char_in * time_char + 0.001, digits = 4)))

    res$.INTERNAL_unnest_tokens.subtitles_IDX <- NULL

    if(drop) {
      res <- dplyr::select(res, -!!quo_input)
    }

  } else {
    res <- tidytext::unnest_tokens(tbl = tbl, output = !!quo_output,
                                   input = !!quo_input, token = token,
                                   format = format, to_lower = to_lower,
                                   drop = drop, collapse = collapse, ...)
  }

  class(res) <- c("subtitles", class(res))
  return(res)
}
