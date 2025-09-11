#' Split a column into tokens
#'
#' This function extends unnest_tokens to subtitles objects. The main difference with the data.frame method
#' is the possibility to perform timecode remapping according to the split of the input column.
#'
#' @inheritParams unnest_tokens.default
#' @param time.remapping a logical. If \code{TRUE} (default), subtitle timecodes are recalculated
#' to take into account the split of the input column.
#'
#' @returns A tibble.
#' @rdname unnest_tokens
#' @export
#' @examples
#' f <- system.file("extdata", "ex_webvtt.vtt", package = "subtools")
#' s <- read_subtitles(f, metadata = data.frame(test = "Test"))
#'
#' #require(tidytext)
#' unnest_tokens(s)
#' unnest_tokens(s, Word, Text_content, drop = FALSE)
#' unnest_tokens(s, Word, Text_content, token = "lines")
#'
unnest_tokens.subtitles <- function(
  tbl,
  output,
  input,
  token = "words",
  format = c("text", "man", "latex", "html", "xml"),
  to_lower = TRUE,
  drop = TRUE,
  collapse = NULL,
  ...,
  time.remapping = TRUE
) {
  if (isTRUE(missing(input))) {
    quo_input <- "Text_content"
    quo_input_lab <- "Text_content"
  } else {
    quo_input <- dplyr::enquo(input)
    quo_input_lab <- rlang::as_label(quo_input)
  }

  if (isTRUE(missing(output))) {
    quo_output <- "Text_content"
    quo_output_lab <- "Text_content"
  } else {
    quo_output <- dplyr::enquo(output)
    quo_output_lab <- rlang::as_label(quo_output)
  }

  if (isTRUE(missing(input)) && isTRUE(missing(output))) {
    drop <- FALSE
  }

  .validate_subtitles(tbl)

  # We drop the subtitles class because unnest_tokens.data.frame
  # is not exported by tidytext
  class(tbl) <- c("tbl_df", "tbl", "data.frame")

  if (isTRUE(time.remapping)) {
    tbl_IDX <- seq_len(nrow(tbl))
    tbl_DFT <- tbl$Timecode_out - tbl$Timecode_in

    tbl$.INTERNAL_unnest_tokens.subtitles_IDX <- tbl_IDX
    res <- tidytext::unnest_tokens(
      tbl = tbl,
      output = !!quo_output,
      input = !!quo_input,
      token = token,
      format = format,
      to_lower = to_lower,
      drop = FALSE,
      collapse = collapse,
      ...
    )

    expand_tbl <- match(res$.INTERNAL_unnest_tokens.subtitles_IDX, tbl_IDX)

    res_nchar <- nchar(res[[quo_output_lab]])
    res_char_tot <- unlist(
      tapply(
        res_nchar,
        res$.INTERNAL_unnest_tokens.subtitles_IDX,
        function(x) {
          rep(sum(x), length(x))
        }
      ),
      use.names = FALSE
    )
    res_char_in <- unlist(
      tapply(
        res_nchar,
        res$.INTERNAL_unnest_tokens.subtitles_IDX,
        function(x) {
          c(0, cumsum(x)[-length(x)])
        }
      ),
      use.names = FALSE
    )
    res_char_out <- unlist(
      tapply(
        res_nchar,
        res$.INTERNAL_unnest_tokens.subtitles_IDX,
        function(x) {
          cumsum(x)
        }
      ),
      use.names = FALSE
    )

    time_char <- tbl_DFT[expand_tbl] / res_char_tot
    res$Timecode_out <- hms::as_hms(as.numeric(round(
      res$Timecode_in + res_char_out * time_char,
      digits = 4
    )))
    res$Timecode_in <- hms::as_hms(as.numeric(round(
      res$Timecode_in + res_char_in * time_char + 0.001,
      digits = 4
    )))

    res$.INTERNAL_unnest_tokens.subtitles_IDX <- NULL

    if (isTRUE(drop)) {
      res <- dplyr::select(res, -!!quo_input)
    }
  } else {
    res <- tidytext::unnest_tokens(
      tbl = tbl,
      output = !!quo_output,
      input = !!quo_input,
      token = token,
      format = format,
      to_lower = to_lower,
      drop = drop,
      collapse = collapse,
      ...
    )
  }

  cn <- colnames(res)
  if ((!isTRUE(drop)) && (quo_input_lab != quo_output_lab)) {
    cn_head <- c(
      "ID",
      "Timecode_in",
      "Timecode_out",
      quo_input_lab,
      quo_output_lab
    )
  } else {
    cn_head <- c("ID", "Timecode_in", "Timecode_out", quo_output_lab)
  }
  cn_meta <- setdiff(cn, cn_head)
  res <- res[, c(cn_head, cn_meta)]

  class(res) <- c("subtitles", class(res))
  return(res)
}

#' Generic for `unnest_tokens`
#'
#' This wrapper turns the function \code{tidytext::unnest_tokens} into an S3 generic.
#' The default method (\code{unnest_tokens.default}) delegates to the original implementation.
#' See "?unnest_tokens.subtitles" for the \code{subtools} specific documentation.
#' @inheritParams tidytext::unnest_tokens
#' @export
unnest_tokens <- function(
  tbl,
  output,
  input,
  token = "words",
  format = c("text", "man", "latex", "html", "xml"),
  to_lower = TRUE,
  drop = TRUE,
  collapse = NULL,
  ...
) {
  UseMethod("unnest_tokens")
}

#' Default method for `unnest_tokens`
#'
#' It simply calls the original `tidytext::unnest_tokens`.
#'
#' @inheritParams unnest_tokens
#' @rdname unnest_tokens
#' @export
unnest_tokens.default <- function(
  tbl,
  output,
  input,
  token = "words",
  format = c("text", "man", "latex", "html", "xml"),
  to_lower = TRUE,
  drop = TRUE,
  collapse = NULL,
  ...
) {
  tidytext::unnest_tokens(
    tbl,
    output,
    input,
    token = "words",
    format = c("text", "man", "latex", "html", "xml"),
    to_lower = TRUE,
    drop = TRUE,
    collapse = NULL,
    ...
  ) # forward to the real implementation
}
