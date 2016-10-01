# Format time as HH:MM:SS.mS
.format_subtime <- function(x){
  x <- gsub(",", ".", x)
  x <- strsplit(x, split = ":")

  l <- lapply(x, length)
  x <- mapply(function(x, l) c(rep("00", -l + 3), x), x = x, l = l, SIMPLIFY = FALSE)

  x.h <- sapply(x, function(x) as.numeric(x[1]))
  x.m <- sapply(x, function(x) as.numeric(x[2]))
  x.s <- sapply(x, function(x) as.numeric(x[3]))

  x.h <- sprintf("%02d", x.h)
  x.m <- sprintf("%02d", x.m)
  x.s <- sprintf("%06.3f", x.s)

  res <- paste(x.h, x.m, x.s, sep = ":")
  return(res)
}


# Add two timecodes HH:MM:SS
.add_timecodes <- function(x, y){

  x <- as.numeric(strsplit(x, split = ":")[[1]])
  y <- as.numeric(strsplit(y, split = ":")[[1]])

  ss <- (x[3] + y[3]) %% 60
  ssm <- (x[3] + y[3]) %/% 60

  mm <- (x[2] + y[2] + ssm) %% 60
  mmh <- (x[2] + y[2] + ssm) %/% 60

  hh <- x[1] + y[1] + mmh

  res <-   paste(sprintf("%02d", hh),
                 sprintf("%02d", mm),
                 sprintf("%06.3f", ss),
                 sep = ":")
  return(res)
}

# Difference between two timecodes
.diff_timecodes <- function(x, y){

  xm <- max(c(x, y))
  ym <- min(c(x, y))
  x <- as.numeric(strsplit(xm, split = ":")[[1]])
  y <- as.numeric(strsplit(ym, split = ":")[[1]])

  ss <- (x[3] - y[3])
  if(ss < 0){
    ss <- 60 + ss
    ssm <- 1
  } else {
    ssm <- 0
  }

  mm <- x[2] - y[2] - ssm
  if(mm < 0){
    mm <- 60 + mm
    mmh <- 1
  } else {
    mmh <- 0
  }

  hh <- x[1] - y[1] - mmh

  res <-   paste(sprintf("%02d", hh),
                 sprintf("%02d", mm),
                 sprintf("%06.3f", ss),
                 sep = ":")
  return(res)
}

# Divides timecode by an integer (k)
.div_timecode <- function(x, k){
  x <- as.numeric(strsplit(x, split = ":")[[1]])

  hh <- x[1] %/% k
  hh.m <- (x[1] / k - hh) * 60

  mm <- (x[2] %/% k) + hh.m
  mm.s <- ((x[2] / k) - (x[2] %/% k)) * 60

  ss <- (x[3] / k) + mm.s

  res <-   paste(sprintf("%02d", hh),
                 sprintf("%02d", mm),
                 sprintf("%06.3f", ss),
                 sep = ":")
  return(res)
}

# Converts seconds to HH:MM:SS (vectorized)
.s_to_hms <- function(x){
  hh <- x %/% 3600
  mm <- (x %% 3600) %/% 60
  ss <- (x %% 3600) %% 60
  res <- paste(hh, mm, ss, sep = ":")
  res <- .format_subtime(res)
  return(res)
}
