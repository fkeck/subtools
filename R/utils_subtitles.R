combineSubs <- function(..., collapse = TRUE, sequential = TRUE){
  sl <- list(...)
  k <- list()
    for(i in 1:length(sl)){
    if(is(sl[[i]], "MultiSubtitles")){
      k <- append(k, unlist(sl[i], recursive = FALSE))
    } else {
      k <- append(k, sl[i])
    }
  }
  sl <- k
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
  res <- do.call("rbind", sl)
  return(res)
}

sl <- list(a, b)
