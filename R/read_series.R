
#' Read series subtitles
#'
#' These functions read one or several subtitles files organized in directories.
#' They are especially designed to import subtitles of series with multiple episodes.
#'
#' @param dir the name of the directory which the subtitles are to be read from (see Details).
#' @param quietly a logical. If \code{FALSE} (default), a message indicating the number of imported files is printed.
#' @param format a character string specifying the format of the subtitles
#' (default is "\code{auto}", see \code{\link{read.subtitles}} for details).
#' @param ... further arguments to be passed to \code{\link{read.subtitles}}.
#'
#' @details These functions read subtitles files at different levels from a 3-levels directory (see the tree below).
#' The function \code{read.subtitles.multiseries} reads everything recursively from "Series_Collection".
#' The function \code{read.subtitles.serie} reads everything recursively from a serie folder (e.g. "Serie_A").
#' The function \code{read.subtitles.season} reads everything from a season folder (e.g. "Season_1").
#' To read a specific episode file (e.g. "Episode_1.srt), use \code{\link{read.subtitles}}.
#'\preformatted{
#'Series_Collection
#'|-- Serie_A
#'|   |-- Season_1
#'|   |   |-- Episode_1.srt
#'|-- Serie_B
#'|   |-- Season_1
#'|   |   |-- Episode_1.srt
#'|   |-- Season_2
#'|   |   |-- Episode_1.srt
#'|   |   |-- Episode_2.srt}
#'
#' @return An object of class \code{MultiSubtitles};
#' i.e. a list of \code{\link{Subtitles}} objects.
#' @export
#' @rdname read_series
read.subtitles.season <- function(dir, quietly = FALSE, format = "auto", ...){

  file.list <- list.files(dir, full.names = TRUE)
  file.list <- file.list[grep(".srt$|.sub$|.ssa$|.ass$|.vtt$", file.list)]
  n.sub <- length(file.list)

  metadata <- data.frame(season = rep(.extr_filename(dir), n.sub),
                          season_num = .extr_snum(file.list),
                          episode_num = .extr_enum(file.list),
                          stringsAsFactors = FALSE)
  res <- vector(mode = "list", length = n.sub)
  for(i in 1:n.sub){
    res[[i]] <- read.subtitles(file.list[i], metadata = as.list(metadata[i, ]), format = format, ...)
  }

  if(!quietly){
  cat(paste("Read:", n.sub, "episodes"))
  }

  class(res) <- "MultiSubtitles"
  invisible(res)
}


#' @rdname read_series
read.subtitles.serie <- function(dir, quietly = FALSE, format = "auto", ...){
  file.list <- dir(dir, full.names = TRUE)
  n.season <- length(file.list)

  res <- vector(mode = "list", length = n.season)
  for(i in 1:n.season){
    res[[i]] <- read.subtitles.season(file.list[i], quietly = TRUE, format = format, ...)
    res[[i]] <- lapply(res[[i]], function(x){x$metadata$serie <- .extr_filename(dir); return(x)})
  }
  res <- unlist(res, recursive = FALSE)

  if(!quietly){
    cat(paste("Read:", n.season, "seasons,", length(res), "episodes"))
  }

  class(res) <- "MultiSubtitles"
  invisible(res)
}


#' @rdname read_series
read.subtitles.multiseries <- function(dir, quietly = FALSE, format = "auto", ...){
  file.list <- dir(dir, full.names = TRUE)
  n.series <- length(file.list)

  res <- vector(mode = "list", length = n.series)
  for(i in 1:n.series){
    res[[i]] <- read.subtitles.serie(file.list[i], quietly = TRUE, format = format, ...)
  }
  res <- unlist(res, recursive = FALSE)

  if(!quietly){
    cat(paste("Read:", n.series, "series,", length(res), "episodes"))
  }

  class(res) <- "MultiSubtitles"
  invisible(res)
}


