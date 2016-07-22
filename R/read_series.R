
#' Title
#'
#' @param dir
#' @param quietly
#' @param format
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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


