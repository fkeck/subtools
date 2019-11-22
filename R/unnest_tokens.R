

#' Split a column into tokens
#'
#' This function extends unnest_tokens to Subtitles objects. The main difference with the data.frame method
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
unnest_tokens.Subtitles <- function(tbl, output, input, token = "words",
                                    format = c("text", "man", "latex", "html", "xml"),
                                    time.remapping = TRUE, to_lower = TRUE, drop = TRUE,
                                    collapse = NULL, ...){
  quo_output <- dplyr::enquo(output)
  quo_input <- dplyr::enquo(input)

  .validate_subtitles(tbl)

  # We drop the Subtitles class because unnest_tokens.data.frame
  # is not exported by tidytext
  class(tbl) <- c("tbl_df", "tbl", "data.frame")

  if(time.remapping) {

    tbl$.INTERNAL_unnest_tokens.Subtitles_IDX <- seq_len(nrow(tbl))

    res <- tidytext::unnest_tokens(tbl = tbl, output = !!quo_output,
                                  input = !!quo_input, token = token,
                                  format = format, to_lower = to_lower,
                                  drop = FALSE, collapse = collapse, ...)

    res_internal <- res

    res_internal <- dplyr::group_by(res_internal, .data$.INTERNAL_unnest_tokens.Subtitles_IDX)
    res_internal <- dplyr::mutate(res_internal,
      nchar = nchar(as_label(quo_output)),
      char_in = c(0, cumsum(nchar)[-length(as_label(quo_output))]),
      char_out = cumsum(nchar),
      diff_w = (.data$Timecode_out - .data$Timecode_in) / sum(nchar),
      recalc_timecode_out = hms::as_hms(round(.data$Timecode_in + .data$char_out * .data$diff_w, digits = 4)),
      recalc_timecode_in = hms::as_hms(round(.data$Timecode_in + .data$char_in * .data$diff_w + 0.001, digits = 4))
    )
    res$Timecode_in <- res_internal$recalc_timecode_in
    res$Timecode_out <- res_internal$recalc_timecode_out
    res$.INTERNAL_unnest_tokens.Subtitles_IDX <- NULL

    if(drop) {
      res <- dplyr::select(res, -!!quo_input)
    }

  } else {
    res <- tidytext::unnest_tokens(tbl = tbl, output = !!quo_output,
                                    input = !!quo_input, token = token,
                                    format = format, to_lower = to_lower,
                                    drop = drop, collapse = collapse, ...)
  }

  class(res) <- c("Subtitles", class(res))
  return(res)
}
