## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  comment = "#>"
)

library(subtools)

## ----read-srt-----------------------------------------------------------------
f_srt <- system.file("extdata", "ex_subrip.srt", package = "subtools")
subs <- read_subtitles(file = f_srt)
subs

## ----read-vtt-----------------------------------------------------------------
f_vtt <- system.file("extdata", "ex_webvtt.vtt", package = "subtools")
read_subtitles(file = f_vtt, format = "webvtt")

## ----read-ass-----------------------------------------------------------------
f_ass <- system.file("extdata", "ex_substation.ass", package = "subtools")
read_subtitles(file = f_ass, format = "substation")

## ----metadata-----------------------------------------------------------------
subs_meta <- read_subtitles(
  file = f_srt,
  metadata = tibble::tibble(Season = 1L, Episode = 3L, Language = "en")
)
subs_meta

## ----as-subtitle--------------------------------------------------------------
raw <- c(
  "1",
  "00:00:01,000 --> 00:00:03,500",
  "Hello, world.",
  "",
  "2",
  "00:00:04,000 --> 00:00:06,000",
  "This is subtools."
)
as_subtitle(x = raw, format = "srt")

## ----info---------------------------------------------------------------------
s <- read_subtitles(
  file = system.file("extdata", "ex_subrip.srt", package = "subtools")
)
get_subtitles_info(x = s)

## ----raw-text-----------------------------------------------------------------
transcript <- get_raw_text(x = s)
transcript

# One line per subtitle, separated by newlines
cat(get_raw_text(x = s, collapse = "\n"))

## ----dplyr--------------------------------------------------------------------
library(dplyr)

# Lines spoken after the first 30 seconds
s |>
  filter(Timecode_in > hms::as_hms("00:00:30"))

# Duration of each subtitle cue (in seconds)
s |>
  mutate(duration_s = as.numeric(Timecode_out - Timecode_in)) |>
  select(ID, Text_content, duration_s)

## ----clean-tags---------------------------------------------------------------
tagged <- as_subtitle(
  x = c(
    "1",
    "00:00:01,000 --> 00:00:03,000",
    "<i>This is <b>important</b>.</i>",
    "",
    "2",
    "00:00:04,000 --> 00:00:06,000",
    "<font color=\"red\">Warning!</font>"
  ),
  format = "srt",
  clean.tags = FALSE   # keep tags so we can demonstrate cleaning
)
tagged$Text_content

clean_tags(x = tagged)$Text_content

## ----clean-captions-----------------------------------------------------------
bb <- read_subtitles(
  file = system.file("extdata", "ex_breakingbad.srt", package = "subtools"),
  clean.tags = FALSE
)
bb$Text_content

clean_captions(x = bb)$Text_content

## ----clean-patterns-----------------------------------------------------------
# Remove speaker labels such as "WALTER:" or "JESSE:"
s_labeled <- as_subtitle(
  x = c(
    "1", "00:00:01,000 --> 00:00:03,000", "WALTER: We need to cook.",
    "",
    "2", "00:00:04,000 --> 00:00:06,000", "JESSE: Yeah, Mr. White!"
  ),
  format = "srt", clean.tags = FALSE
)

clean_patterns(x = s_labeled, pattern = "^[A-Z]+: ")$Text_content

## ----clean-chain--------------------------------------------------------------
s_clean <- read_subtitles(file = f_srt, clean.tags = FALSE) |>
  clean_tags() |>
  clean_captions() |>
  clean_patterns(pattern = "^-\\s*")   # remove leading dialogue dashes

s_clean$Text_content

## ----bind-collapse------------------------------------------------------------
s1 <- read_subtitles(
  file = system.file("extdata", "ex_subrip.srt", package = "subtools"),
  metadata = tibble::tibble(Episode = 1L)
)
s2 <- read_subtitles(
  file = system.file("extdata", "ex_rushmore.srt", package = "subtools"),
  metadata = tibble::tibble(Episode = 2L)
)

combined <- bind_subtitles(s1, s2)
nrow(combined)
range(combined$Timecode_in)

## ----bind-list----------------------------------------------------------------
multi <- bind_subtitles(s1, s2, collapse = FALSE)
class(multi)
print(multi)

## ----info-multi---------------------------------------------------------------
get_subtitles_info(x = multi)

## ----read-series-demo, eval=FALSE---------------------------------------------
# # Read a single season
# season1 <- read_subtitles_season(dir = "BreakingBad/Season_01/")
# 
# # Read an entire series (all seasons)
# bb_all <- read_subtitles_serie(dir = "BreakingBad/")
# 
# # Read multiple series at once
# collection <- read_subtitles_multiseries(dir = "Series_Collection/")

## ----move---------------------------------------------------------------------
subs_shifted <- move_subtitles(x = subs, lag = 2.5)

# Compare first cue before and after
subs$Timecode_in[1]
subs_shifted$Timecode_in[1]

## ----move-multi---------------------------------------------------------------
multi_shifted <- move_subtitles(x = multi, lag = -1.0)
multi_shifted[[1]]$Timecode_in[1]

## ----write, eval=FALSE--------------------------------------------------------
# write_subtitles(x = subs_shifted, file = "synced_episode.srt")

## ----unnest-words-------------------------------------------------------------
words <- unnest_tokens(tbl = subs)
words

## ----unnest-ngrams------------------------------------------------------------
# Bigrams
bigrams <- unnest_tokens(tbl = subs, output = Word, input = Text_content,
                         token = "ngrams", n = 2)
bigrams$Word

## ----word-freq----------------------------------------------------------------
library(dplyr)

words |>
  count(Text_content, sort = TRUE) |>
  head(10)

## ----cross-episode------------------------------------------------------------
ep1 <- read_subtitles(
  file = system.file("extdata", "ex_breakingbad.srt", package = "subtools"),
  metadata = tibble::tibble(Episode = 1L)
)
ep2 <- read_subtitles(
  file = system.file("extdata", "ex_rushmore.srt", package = "subtools"),
  metadata = tibble::tibble(Episode = 2L)
)
ep3 <- read_subtitles(
  file = system.file("extdata", "ex_webvtt.vtt", package = "subtools"),
  metadata = tibble::tibble(Episode = 3L)
)

corpus <- bind_subtitles(ep1, ep2, ep3)

token_counts <- unnest_tokens(corpus) |>
  count(Episode, Text_content, sort = TRUE)

token_counts |>
  slice_max(n, n = 5, by = Episode)

## ----tfidf--------------------------------------------------------------------
token_counts |>
  tidytext::bind_tf_idf(Text_content, Episode, n) |>
  arrange(Episode, desc(tf_idf)) |>
  slice_max(tf_idf, n = 5, by = Episode)

## ----timeline, fig.width = 7, fig.height = 3----------------------------------
words_ep1 <- unnest_tokens(tbl = ep1) |>
  mutate(minute = as.numeric(Timecode_in) / 60)

if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  ggplot(words_ep1, aes(x = minute)) +
    geom_histogram(binwidth = 0.5, fill = "steelblue", colour = "white") +
    labs(
      title = "Word density over time",
      x     = "Time (minutes)",
      y     = "Word count"
    ) +
    theme_minimal()
}

