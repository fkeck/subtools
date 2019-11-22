

#' Get informations about subtitles embedded in MKV files
#'
#' This function uses \code{mkvmerge} to extract tracks data from an MKV file.
#' You must have \code{mkvmerge} installed on your computer.
#'
#' @param file a character string giving the path to the MKV file.
#' @param mkvmerge.exec a character string giving the path to the \code{mkvmerge} executable.
#' @param print.info print basic informations about subtitle tracks. Default is \code{TRUE}.
#'
#' @return A list with complete data about the MKV is invisibly returned.
#' If the MKV has at least 1 subtitles track and \code{print.info} is \code{TRUE}, basic informations are printed.
#' Otherwise it returns a warning.
#'
#' @references
#' \url{https://mkvtoolnix.download/downloads.html}
#'
#' @export
#'
get_mkv_info <- function(file, mkvmerge.exec = "mkvmerge", print.info = TRUE){

  if(file.exists(file)){
    file <- normalizePath(file)
    file <- gsub(" ", "\\ ", file, fixed = TRUE)
  } else {
    stop("Invalid input: file must be a valid file path.")
  }

  comm <- paste(mkvmerge.exec, file, "-i -F json")
  mkv.info <- system(comm, intern = TRUE)
  mkv.info <- jsonlite::fromJSON(mkv.info)



  # A short summary to be returned in the console
  mkv.info.sub <- mkv.info$tracks[mkv.info$tracks$type == "subtitles", ]
  if (nrow(mkv.info.sub) < 1L) {
    warning("Subtitles tracks not found")
  } else {
    mkv.info.sub <- cbind(mkv.info.sub[, "id"],
                          mkv.info.sub$properties[, "language"],
                          mkv.info.sub[, "codec"],
                          mkv.info.sub$properties[, c("codec_id", "default_track")])
    names(mkv.info.sub) <- c("ID", "Language", "Codec", "Codec ID", "Default track")
    if(print.info) print(mkv.info.sub)
  }
  # Return invisibly the complete data
  invisible(mkv.info)
}


#' Extract subtitles embedded in MKV files
#'
#' This function uses \code{mkvextract} to extract subtitles from an MKV file.
#' You must have \code{mkvmerge} and \code{mkvextract} installed on your computer.
#'
#'
#' @param file a character string giving the path to the MKV file.
#' @param id An integer giving the ID of the tracks to be extracted.
#' Can be a vector of length > 1 to extract several tracks.
#' If \code{NA} (default), the default subtitle tracks will be extracted
#' @param mkvextract.exec a character string giving the path to the \code{mkvextract} executable.
#' @param mkvmerge.exec  character string giving the path to the \code{mkvmerge} executable.
#'
#' @details The function \code{\link{get_mkv_info}} is a simple way to identify the ID of subtitles tracks from a MKV file.
#'
#' Not all the subtitle formats supported by \code{mkvextract} can be read by \code{subtools}.
#' See \code{\link{read_subtitles}} for the list of formats currently supported.
#'
#' @return An object of class \code{Subtitles} (see \code{\link{Subtitles}}).
#' If several tracks are requested (via \code{id}), an object of class \code{MultiSubtitles};
#' i.e. a list of \code{\link{Subtitles}} objects.
#'
#' @references
#' \url{https://mkvtoolnix.download/downloads.html}
#'
#' @export
#'
read_subtitles_mkv <- function(file, id = 2,
                               mkvextract.exec = "mkvextract",
                               mkvmerge.exec = "mkvmerge"){

  info <- get_mkv_info(file, mkvmerge.exec = mkvmerge.exec, print.info = FALSE)$tracks

  if (file.exists(file)) {
    file <- normalizePath(file)
    file <- gsub(" ", "\\ ", file, fixed = TRUE)
  } else {
    stop("Invalid input: file must be a valid file path.")
  }

  sub.list <- list()

  if(length(id) == 1){
    if (is.na(id)) {
      id <- info$id[info$type == "subtitles" & info$properties$default_track]
    }
  }

  for (i in seq_along(id)){

    codec.i <- info$properties$codec_id[info$id == id[i]]

    if (!codec.i %in% c("S_TEXT/UTF8", "S_TEXT/ASCII",
                        "S_TEXT/SSA", "S_SSA", "S_TEXT/ASS", "S_ASS",
                        "S_TEXT/WEBVTT")) {
      warning("Subtitle codec ", codec.i, " is not supported by subtools yet. Tracks ID:", id[i], " skipped.")
      sub.list <- sub.list[-i]

    } else {

      if (codec.i == "S_TEXT/UTF8" | codec.i == "S_TEXT/ASCII") ext.i <- ".srt"
      if (codec.i == "S_TEXT/SSA" | codec.i == "S_SSA") ext.i <- ".ssa"
      if (codec.i == "S_TEXT/ASS" | codec.i == "S_ASS") ext.i <- ".ass"
      if (codec.i == "S_TEXT/WEBVTT") ext.i <- ".vtt"

      sub.file <- tempfile(fileext = ext.i)

      comm <- paste(mkvextract.exec, " tracks ", file, " ", id[i], ":", sub.file, sep = "")
      system(comm, ignore.stdout = TRUE)
      sub.list[[i]] <- read_subtitles(sub.file)
    }
  }

  if(length(sub.list) > 1){
    class(sub.list) <- "MultiSubtitles"
  } else {
    sub.list <- sub.list[[1]]
  }

  return(sub.list)

}
