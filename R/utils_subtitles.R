#FIX NEW CLASS
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
      sl[[i]][ , "ID"] <- as.numeric(sl[[i]][ , "ID"]) + id.max
      id.max <- max(sl[[i]][ , "ID"])

      sl[[i]][ , "Timecode.in"] <- sapply(sl[[i]][ , "Timecode.in"], .add_timecodes,
                                          y = tcout.max, USE.NAMES = FALSE)
      sl[[i]][ , "Timecode.out"] <- sapply(sl[[i]][ , "Timecode.out"], .add_timecodes,
                                          y = tcout.max, USE.NAMES = FALSE)
      tcout.max <- max(sl[[i]][ , "Timecode.out"])
    }
  }

  if(collapse){
    sl <- do.call("rbind", sl)
    class(sl) <- "Subtitles"
  } else {
    class(sl) <- "MultiSubtitles"
  }

  return(sl)
}

