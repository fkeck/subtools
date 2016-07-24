#' Title
#'
#' @param ... xxx
#' @param collapse xxx
#' @param sequential xxx
#'
#' @return xxx
#' @export
#'
combineSubs <- function(..., collapse = TRUE, sequential = TRUE){
  input <- list(...)
  sl <- list()
    for(i in 1:length(input)){
    if(is(input[[i]], "MultiSubtitles")){
      sl <- append(sl, unlist(input[i], recursive = FALSE))
    } else {
      sl <- append(sl, input[i])
    }
  }

  if(sequential){
    id.max <- 0
    tcout.max <- "00:00:00.000"
    for(i in 1:length(sl)){
      sl[[i]]$subtitles$ID <- as.numeric(sl[[i]]$subtitles$ID) + id.max
      id.max <- max(sl[[i]]$subtitles$ID)

      sl[[i]]$subtitles$Timecode.in <- sapply(sl[[i]]$subtitles$Timecode.in,
                                              .add_timecodes,
                                              y = tcout.max, USE.NAMES = FALSE)
      sl[[i]]$subtitles$Timecode.out <- sapply(sl[[i]]$subtitles$Timecode.out,
                                               .add_timecodes,
                                              y = tcout.max, USE.NAMES = FALSE)
      tcout.max <- max(sl[[i]]$subtitles$Timecode.out)
    }
  }

  if(collapse){
    sl <- do.call("rbind", lapply(sl, function(x) x$subtitles))
    res <- Subtitles(text = sl$Text, timecode.in = sl$Timecode.in,
                     timecode.out = sl$Timecode.out, id = sl$ID,
                     metadata = list())
  } else {
    res <- sl
    class(res) <- "MultiSubtitles"
  }

  return(res)
}

