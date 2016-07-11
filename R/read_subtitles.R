
read.subtitles <- function(file, format = "srt", clean.tags = TRUE, meta.data = list()){

  if(format == "auto"){
    format <- .extr_extension(file)
  }
  format <- match.arg(format, choices = c("srt", "sub", "ssa", "ass", "webvtt", "auto"), several.ok = FALSE)

  subs <- readLines(file, warn = FALSE)

  i <- 1
  while(subs[i] == ""){i <- i + 1}
  j <- length(subs)
  while(subs[j] == ""){j <- j - 1}
  subs <- subs[seq.int(i, j)]

  if(format == "srt"){
    subs.newlines <- c(0, which(subs == ""))
    subs.n.li <- subs.newlines + 1
    subs.time.li <- subs.newlines + 2
    subs.txt.li <- mapply(seq, from = subs.time.li + 1, to = c(subs.newlines[-1], length(subs) + 1) - 1)

    subs.n <- subs[subs.n.li]
    subs.time <- subs[subs.time.li]
    subs.txt <- sapply(subs.txt.li, function(x) paste(subs[x], collapse = " "))

    subs.time <- strsplit(subs.time, split = " --> ")
    timecode.in <- sapply(subs.time, function(x) x[1])
    timecode.out <- sapply(subs.time, function(x) x[2])
    res$Timecode.in <- gsub(",", ".", res$Timecode.in)
    res$Timecode.out <- gsub(",", ".", res$Timecode.out)

  }

  if(format %in% c("ssa", "ass")){
    subs.events.h <- subs[grep("^Format:.*Start,.*Text", subs)]

    subs.events.li <- grep("^Dialogue:", subs)
    subs.events <- subs[subs.events.li]

    comma.pos <- gregexpr(",", subs.events)
    comma.min <- min(sapply(comma.pos, length))

    subs.txt <- substr(subs.events, start = comma.pos[[1]][comma.min] + 1, stop = 10^5L)

    subs.events <- substr(subs.events, start = 1, stop = comma.pos[[1]][comma.min])
    subs.events <- paste(subs.events, collapse = "\n")
    subs.events <- paste(subs.events.h, subs.events, sep = "\n")
    subs.events <- read.csv(textConnection(subs.events), header = TRUE,
                            sep = ",", quote = "", stringsAsFactors = FALSE, fill = T)

    subs.txt <- gsub("\\\\[Nn]", " ", subs.txt)
    timecode.in <- subs.events[, "Start"]
    timecode.out <- subs.events[, "End"]
    subs.n <- order(timecode.in)
  }

  res <- data.frame(subs.n, timecode.in, timecode.out, subs.txt, stringsAsFactors = FALSE)
  names(res) <- c("ID", "Timecode.in", "Timecode.out", "Text")

  if(clean.tags){
    res$Text <- cleanTags(res$Text, format = format)
  }

  if(any(names(meta.data) %in% c("class", "names", "row.names"))){
    stop("Forbidden metadata tag.")
  }
  if(length(meta.data) > 0){
    for(i in 1:length(meta.data)){
      attr(res, names(meta.data)[i]) <- meta.data[[i]]
    }
  }
  return(res)
}


# strptime(res$Timecode.in[20], format = "%H:%M:%S")
# pp <- as.difftime(res$Timecode.in, format = "%H:%M:%S", units = "sec")





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
