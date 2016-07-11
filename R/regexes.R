
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


