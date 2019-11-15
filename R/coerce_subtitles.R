
#' Get subtitles text
#'
#' This function returns the raw text content of subtitles objects as a character string.
#'
#' @param x an object of class \code{Subtitles} or \code{MultiSubtitles}.
#' @param collapse a character string to separate the subtitles lines.
#'
#' @return A character string.
#'
#' @examples
#' f <- system.file("extdata", "ex_subrip.srt", package = "subtools")
#' s <- read.subtitles(f)
#' rawText(s)
#' cat(rawText(s, collapse = "\n"))
#'
#' @export
#'
rawText <- function(x, collapse = " "){
  if(inherits(x, "Subtitles")){
    res <- x$subtitles$Text
    res <- paste(res, collapse = collapse)
  }
  if(inherits(x, "MultiSubtitles")){
    res <- lapply(x, function(x) x$subtitles$Text)
    res <- do.call("c", res)
    res <- paste(res, collapse = collapse)
  }
  return(res)
}


#' Convert subtitles to a tm corpus
#'
#' This function converts \code{Subtitles} and \code{MultiSubtitles} objects
#' to virtual corpora as defined by package \pkg{tm}.
#'
#' @param x an object of class \code{Subtitles} or \code{MultiSubtitles}.
#' @param collapse a character string to separate the subtitles lines.
#'
#' @return An object of class \code{VCorpus}.
#' @seealso \code{\link[tm]{Corpus}}, \code{\link[tm]{VCorpus}}.
#' @export
#'
tmCorpus <- function(x, collapse = " "){

  if(is(x, "Subtitles")){
    txt <- rawText(x)
    meta <- x$metadata
    meta.df <- as.data.frame(meta)
    attr(meta.df, which = "row.names") <- 1L
  }

  if(is(x, "MultiSubtitles")){
    txt <- lapply(x, rawText, collapse = collapse)
    meta <- lapply(x, function(x) x$metadata)
    meta.df.names <- unique(unlist(lapply(meta, names)))
    meta.df <- data.frame(matrix(NA, nrow = length(meta), ncol = length(meta.df.names)))
    colnames(meta.df) <- meta.df.names
    for(i in 1:dim(meta.df)[1]){
      meta.df[i, ][names(meta[[i]])] <- meta[[i]]
    }
    attr(meta.df, which = "row.names") <- seq_len(length(x))
  }

  txt <- VectorSource(txt)
  res <- Corpus(txt)
  res$dmeta <- meta.df

  return(res)
}


#' Convert subtitles to a dataframe
#'
#' This function converts \code{Subtitles} and \code{MultiSubtitles} objects
#' to a dataframe with one subtitle line per row; text, timecodes,
#' and (expanded) meta-data as columns.
#'
#' @param x an object of class \code{Subtitles} or \code{MultiSubtitles}.
#'
#' @return A data frame.
#' @export
#'
subDataFrame <- function(x){
  if(is(x, "Subtitles")){
    res <- x$subtitles
    if(length(x$metadata) > 0){
      res <- cbind(res, sapply(x$metadata, function(x) rep(x, nrow(res))))
    }
  }

  if(is(x, "MultiSubtitles")){
    res <- lapply(x, function(x) x$subtitles)
    res <- do.call("rbind", res)

    meta <- lapply(x, function(x) x$metadata)
    meta.df.names <- unique(unlist(lapply(meta, names)))
    meta.df <- data.frame(matrix(NA, nrow = length(meta), ncol = length(meta.df.names)))
    colnames(meta.df) <- meta.df.names
    for(i in 1:dim(meta.df)[1]){
      meta.df[i, ][names(meta[[i]])] <- meta[[i]]
    }
    meta.df <- meta.df[rep(seq_len(nrow(meta.df)), sapply(x, function(x) nrow(x$subtitles))), ]
    res <- cbind(res, meta.df)
  }

  return(res)
}



#' Convert subtitles to a tibble
#'
#' This function converts \code{Subtitles} and \code{MultiSubtitles} objects
#' to a \code{tibble} with one subtitle line per row; text, timecodes,
#' and (expanded) meta-data as columns.
#'
#' @param x an object of class \code{Subtitles} or \code{MultiSubtitles}.
#'
#' @return A \code{tibble}.
#' @export
as_tibble.Subtitles <- function(x) {
  res <- subDataFrame(x)
  res <- as_tibble(res)
  return(res)
}
