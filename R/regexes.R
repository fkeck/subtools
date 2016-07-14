
# Extract file name from dir paths
.extr_filename <- function(x){
  x <- gsub("/+$", "", x)
  x <- regmatches(x, regexpr("([^/]+$)", x))
  return(x)
}

# Extract extension from file name
.extr_extension <- function(x){
  x <- regmatches(x, regexpr("(?<=\\.)[0-9a-z]+$", x, perl = TRUE))
  return(x)
}

# Extract Season number
.extr_snum <- function(x){
  x <- .extr_filename(x)
  x <- toupper(x)

  res <- vector(mode = "character", length = length(x))

  mode0 <- grepl("[ -_\\.]S[0-9]+", x)  # S01
  mode1 <- grepl("SEASON.{1}[0-9]+", x)  # SEASON.2
  mode2 <- grepl("S[0-9]+E[0-9]+", x)    # S03E05

  mode0.r <- unlist(regmatches(x, gregexpr("(?<=[ -_\\.]S)[0-9]+", x, perl = TRUE)))
  mode1.r <- unlist(regmatches(x, gregexpr("(?<=SEASON.{1})[0-9]+", x, perl = TRUE)))
  mode2.r <- unlist(regmatches(x, regexpr("S[0-9]+E[0-9]+", x)))
  mode2.r <- unlist(regmatches(mode2.r, gregexpr("(?<=S).*(?=E)", mode2.r, perl = TRUE)))

  res[mode0] <- mode0.r
  res[mode1] <- mode1.r
  res[mode2] <- mode2.r
  res <- as.numeric(res)
  return(res)
}

# Extract Episode number
.extr_enum <- function(x){
  x <- .extr_filename(x)
  x <- toupper(x)

  res <- vector(mode = "character", length = length(x))

  mode0 <- grepl("[ -_\\.]E[0-9]+", x)  # E01
  mode1 <- grepl("EPISODE.{1}[0-9]+", x)  # EPISODE.2
  mode2 <- grepl("S[0-9]+E[0-9]+", x)    # S03E05

  mode0.r <- unlist(regmatches(x, gregexpr("(?<=[ -_\\.]E)[0-9]+", x, perl = TRUE)))
  mode1.r <- unlist(regmatches(x, gregexpr("(?<=EPISODE.{1})[0-9]+", x, perl = TRUE)))
  mode2.r <- unlist(regmatches(x, regexpr("S[0-9]+E[0-9]+", x)))
  mode2.r <- unlist(regmatches(mode2.r, gregexpr("(?<=E).*", mode2.r, perl = TRUE)))

  res[mode0] <- mode0.r
  res[mode1] <- mode1.r
  res[mode2] <- mode2.r
  res <- as.numeric(res)
  return(res)
}


# Format time
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

