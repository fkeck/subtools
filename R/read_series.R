
read.subtitles.season <- function(dir, quietly = FALSE, format = "auto", ...){

  file.list <- list.files(dir, full.names = TRUE)
  file.list <- file.list[grep(".srt$|.sub$|.ssa$|.ass$|.vtt$", file.list)]
  n.sub <- length(file.list)

  meta.data <- data.frame(season = rep(.extr_filename(dir), n.sub),
                          season_num = .extr_snum(file.list),
                          episode_num = .extr_enum(file.list),
                          stringsAsFactors = FALSE)
  res <- vector(mode = "list", length = n.sub)
  for(i in 1:n.sub){
    res[[i]] <- read.subtitles(file.list[i], meta.data = meta.data[i, ], format = format, ...)
  }

  if(!quietly){
  cat(paste("Read:", n.sub, "episodes"))
  }
  invisible(res)
}


read.subtitles.serie <- function(dir, quietly = FALSE, format = "auto", ...){
  file.list <- dir(dir, full.names = TRUE)
  n.season <- length(file.list)

  res <- vector(mode = "list", length = n.season)
  for(i in 1:n.season){
    res[[i]] <- read.subtitles.season(file.list[i], quietly = TRUE, format = format, ...)
    res[[i]] <- lapply(res[[i]], function(x){attr(x, "Serie") <- .extr_filename(dir); return(x)})
  }
  res <- unlist(res, recursive = FALSE)

  if(!quietly){
    cat(paste("Read:", n.season, "seasons,", length(res), "episodes"))
  }
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
  invisible(res)
}

##### TESTS
# INRA
file <- "/home/francois/Google Drive/Sync work/blog/Rsubs/subs/True Blood/Season 1//true.blood.s01e07.Burning House of Love.dvdrip.xvid-reward.eng.ass"
dir.season <- "/home/francois/Google Drive/Sync work/blog/Rsubs/subs/True Blood/Season 1/"
dir.serie <- "/home/francois/Google Drive/Sync work/blog/Rsubs/subs//True Blood/"
dir.mseries <- "/home/francois/Google Drive/Sync work/blog/Rsubs/subs/"

# Laptop
file <- "/home/francois/Téléchargements/got3/Game.of.Thrones.S03E09.HDTV.en..srt"
dir <- "/home/francois/Téléchargements/got3"


a <- read.subtitles(file, format="srt")
a <- read.subtitles.season(dir = dir.serie, format="auto")
a <- read.subtitles.serie(dir = dir.serie)
a <- read.subtitles.multiseries(dir = dir.mseries)
