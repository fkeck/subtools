
cleanTags <- function(x, format = "srt"){

  format <- match.arg(format, choices = c("srt", "subrip",
                                          "sub", "subviewer", "microdvd",
                                          "ssa", "ass", "substation",
                                          "vtt", "webvtt", "all"), several.ok = FALSE)

  if(format %in% c("srt", "subrip", "all")){
    x <- gsub("<.+?>", "", x)
  }

  if(format %in% c("ass", "ssa", "substation", "all")){
    x <- gsub("\\{\\\\.+?\\}", "", x)
  }

  return(x)
}


sentencify <- function(x){
  ended <- grep("[\\.\\?!♪]\"$|[\\.\\?!♪]$", x$Text)
  f <- as.factor(findInterval(1:length(x$Text)-1, ended))
  new.txt <- split(x$Text, f)
  new.txt <- sapply(new.txt, paste, collapse = " ", USE.NAMES = FALSE)

  new.tcin <- split(x$Timecode.in, f)
  new.tcin <- sapply(new.tcin, min, USE.NAMES = FALSE)

  new.tcout <- split(x$Timecode.out, f)
  new.tcout <- sapply(new.tcout, max, USE.NAMES = FALSE)

  new.txt <- strsplit(new.txt, split = "(?<=[\\.\\?!]|--) (?=[-A-Z])", perl = TRUE)
  new.txt.length <- sapply(new.txt, length)

  fun <- function(tcin, tcout, n){
    d <- .diff_timecodes(tcin, tcout)
    d <- .div_timecode(d, n)
    if(n == 1){
      r1 <- tcin
      r2 <- .add_timecodes(tcin, d)
    } else {
      r1 <- r2 <- rep(NA, n)
      for(i in 1:n){
        if(i == 1){
          r1[i] <- tcin
          r2[i] <- .add_timecodes(tcin, d)
        } else {
          r1[i] <- .add_timecodes(r1[i-1], d)
          r2[i] <- .add_timecodes(r1[i], d)
        }
      }
    }
    return(list(r1, r2))
  }
  mapply(function(tcin, tcout, n) fun(tcin, tcout, n),
         tcin = new.tcin, tcout = new.tcout, n = new.txt.length,
         SIMPLIFY = FALSE)


  new.txt <- unlist(new.txt)
  }
}


x <- a [[7]]
