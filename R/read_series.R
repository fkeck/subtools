
#' Read series subtitles
#'
#' These functions read one or several subtitles files organized in directories.
#' They are designed to import subtitles of series with multiple episodes.
#'
#' @param dir the name of the directory which the subtitles are to be read from (see Details).
#' @param detect.meta a logical. If \code{TRUE} (default), the function tries to automatically detect
#' metadata (Serie, Season and Episode) from file names. This will overide user metadata with the same name.
#' @param quietly a logical. If \code{FALSE} (default), a message indicating the number of imported files is printed.
#' @param format a character string specifying the format of the subtitles
#' (default is "\code{auto}", see \code{\link{read_subtitles}} for details).
#' @param bind a logical. If \code{TRUE} (default), subtitles are binded with \code{\link{bind_subtitles}}
#' @param ... further arguments to be passed to \code{\link{read_subtitles}}.
#'
#' @details These functions read subtitles files at different levels from a 3-levels directory (see the tree below).
#' The function \code{read_subtitles_multiseries} reads everything recursively from "Series_Collection".
#' The function \code{read_subtitles_serie} reads everything recursively from a serie folder (e.g. "Serie_A").
#' The function \code{read_subtitles_season} reads everything from a season folder (e.g. "Season_1").
#' To read a specific episode file (e.g. "Episode_1.srt), use \code{\link{read_subtitles}}.
#'
#'
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
#' @return If \code{bind} is set on \code{TRUE} a \code{Subtitles} object,
#' otherwise an object of class \code{MultiSubtitles};
#' i.e. a list of \code{Subtitles} objects.
#' @export
#' @rdname read_series
read_subtitles_season <- function(dir, format = "auto", bind = TRUE, detect.meta = TRUE, quietly = FALSE, ...){

  file.list <- list.files(dir, full.names = TRUE)
  file.list <- file.list[grep(".srt$|.sub$|.ssa$|.ass$|.vtt$", file.list)]
  n.sub <- length(file.list)

  res <- vector(mode = "list", length = n.sub)
  for(i in 1:n.sub){
    res[[i]] <- read_subtitles(file.list[i], format = format, ...)
    if(detect.meta){
      res[[i]]$Season <- .extr_snum(file.list[i])
      res[[i]]$Episode <- .extr_enum(file.list[i])
    }
  }

  if(!quietly){
  cat(paste("Read:", n.sub, "episodes\n"))
  }

  # Reorder by episode number (if episode numbers were correctly extracted)
  if(detect.meta){
    enum <- .extr_enum(file.list)
    if(is.numeric(enum) & all(!is.na(enum)) & anyDuplicated(enum) == 0) {
      res <- res[enum]
    }
  }

  class(res) <- "MultiSubtitles"

  if(bind) {
    res <- bind_subtitles(res)
  }

  return(res)
}


#' @rdname read_series
#' @export
read_subtitles_serie <- function(dir, format = "auto", bind = TRUE, detect.meta = TRUE, quietly = FALSE, ...){
  file.list <- dir(dir, full.names = TRUE)
  n.season <- length(file.list)

  res <- vector(mode = "list", length = n.season)
  for(i in 1:n.season){
    res[[i]] <- read_subtitles_season(file.list[i], bind = FALSE, detect.meta = TRUE,
                                      quietly = TRUE, format = format, ...)
    if(detect.meta){
      res[[i]] <- lapply(res[[i]], function(x){x$Serie <- .extr_filename(dir); return(x)})
    }
  }

  # Reorder by season number (if season numbers were correctly extracted)
  if(detect.meta){
    snum <- .extr_snum(file.list)
    if(is.numeric(snum) & all(!is.na(snum)) & anyDuplicated(snum) == 0) {
      res <- res[snum]
    }
  }
  res <- unlist(res, recursive = FALSE)

  if(!quietly){
    cat(paste("Read:", n.season, "seasons,", length(res), "episodes\n"))
  }

  class(res) <- "MultiSubtitles"

  if(bind) {
    res <- bind_subtitles(res)
  }

  return(res)
}


#' @rdname read_series
#' @export
read_subtitles_multiseries <- function(dir, format = "auto", bind = TRUE, detect.meta = TRUE, quietly = FALSE, ...){
  file.list <- dir(dir, full.names = TRUE)
  n.series <- length(file.list)

  res <- vector(mode = "list", length = n.series)
  for(i in 1:n.series){
    res[[i]] <- read_subtitles_serie(file.list[i], bind = FALSE, detect.meta = TRUE, quietly = TRUE, format = format, ...)
  }
  res <- unlist(res, recursive = FALSE)

  if(!quietly){
    cat(paste("Read:", n.series, "series,", length(res), "episodes"))
  }

  class(res) <- "MultiSubtitles"

  if(bind) {
    res <- bind_subtitles(res)
  }

  return(res)
}

