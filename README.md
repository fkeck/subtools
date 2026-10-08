
<!-- README.md is generated from README.Rmd. Please edit that file -->

# subtools

<img src="man/figures/subtools.png" width="120" align="right" />

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/fkeck/subtools/actions/workflows/check-standard.yml/badge.svg)](https://github.com/fkeck/subtools/actions/workflows/check-standard.yml)
![Code Coverage:
82%](https://img.shields.io/badge/code_coverage-82%25-green)

### Read, write and manipulate subtitles in R

Hi! Here, you will find some basic informations to get started with
`subtools`. For more details, you can check the package documentation.

Subtools is a R package to read, write and manipulate subtitles in R.
This then allows the full range of tools offered by the R ecosystem to
be used for the analysis of subtitles. With version `1.0`, `subtools`
integrates the main principles of the tidyverse and integrates directly
with `tidytext` for a tidy approach of subtitle text mining.

### Install

You can install the package directly from CRAN with
`install.packages("subtools")` or get the latest, in development,
version with:

``` r
pak::pkg_install("fkeck/subtools@dev")
```

``` r
library(subtools)
```

### Reading subtitles

The main goal of subtools is to provide a seamless way to import
subtitle files directly into R. This task can be performed with the
function `read_subtitles()`:

``` r
rushmore_sub <- read_subtitles("ex_Rushmore.srt")
oss_sub <- read_subtitles("ex_OSS_117.srt")
```

``` r
rushmore_sub
#> # A tibble: 4 × 4
#>   ID    Timecode_in Timecode_out Text_content
#>   <chr> <time>      <time>       <chr>
#> 1 180   20'40.969"  20'48.269"   Rushmore deserves an aquarium. A first class a…
#> 2 181   20'48.269"  20'50.870"   - I don't know. What do you think, Ernie - Aqu…
#> 3 182   20'50.946"  20'57.370"   - What kind of fish? - Barracudas. Stingrays. …
#> 4 183   20'58.051"  21'01.770"   - Piranhas? Really? - Yes, I'm talking to a gu…

oss_sub
#> # A tibble: 3 × 4
#>   ID    Timecode_in Timecode_out Text_content
#>   <chr> <time>      <time>       <chr>
#> 1 264   20'22.967"  20'27.427"   Si vous voulez. Ça sera surtout l'occasion de …
#> 2 265   20'30.347"  20'32.297"   Et non pas le gratin de pommes de terre.
#> 3 266   20'35.587"  20'37.697"   Parce que ça ressemble à carotte, cairote.
```

The function `read_subtitles()` returns an object of class `subtitles`.
This is a simple `tibble` with at least four columns (“`ID`”,
“`Timecode_in`”, “`Timecode_out`” and “`Text_content`”).

The metadata are handled by adding extra-columns which can be used
during the analysis. You can add metadata by adding columns manually
(e.g. using `mutate()`). You can also provide a 1-row data.frame of
metadata to the function `read_subtitles()`.

``` r
bb_meta <- data.frame(Name = "Breaking Bad", Season = 1, Episode = 1)
bb_sub <- read_subtitles("ex_Breaking_Bad.srt", metadata = bb_meta)
```

``` r
bb_sub
#> # A tibble: 5 × 7
#>   ID    Timecode_in Timecode_out Text_content               Name  Season Episode
#>   <chr> <time>      <time>       <chr>                      <chr>  <dbl>   <dbl>
#> 1 5     01'09.236"  01'12.780"   Oh, my God. Christ!        Brea…      1       1
#> 2 6     01'15.993"  01'18.661"   Shit.                      Brea…      1       1
#> 3 7     01'18.829"  01'21.205"   [SIRENS WAILING IN DISTAN… Brea…      1       1
#> 4 8     01'24.918"  01'27.378"   Oh, God. Oh, my God.       Brea…      1       1
#> 5 9     01'27.546"  01'30.840"   Oh, my God. Oh, my God. T… Brea…      1       1
```

##### Series

If you want to analyse subtitles of series with different seasons and
episodes, you will have to import many files at once. The
`read_subtitles_season()`, `read_subtitles_serie()` and
`read_subtitles_multiseries()` functions can make your life much easier,
by making it possible to automatically import files and extract metadata
from a structured directory. You can check the manual for more details.

##### MKV

Finally if you have a collection of movies in .mkv format, you can
extract the subtitle tracks of MKV files with `read_subtitles_mkv()`.

### Cleaning subtitles

Often, the workflow begins with a cleaning step to get rid of irrelevant
information that might be present in text content. Three functions can
be used for this task. First, `clean_tags()` cleans formatting tags. By
default, this function is automatically executed by the
`read_subtitles*()` functions, so you probably don’t need to run it
again. Second, `clean_captions()` can be used to suppress closed
captions, i.e. descriptions of non-speech elements in parentheses or
squared brackets. Finally, `clean_patterns()` is a more general function
to clean subtitles based on regex pattern matching.

``` r
bb_sub
#> # A tibble: 5 × 7
#>   ID    Timecode_in Timecode_out Text_content               Name  Season Episode
#>   <chr> <time>      <time>       <chr>                      <chr>  <dbl>   <dbl>
#> 1 5     01'09.236"  01'12.780"   Oh, my God. Christ!        Brea…      1       1
#> 2 6     01'15.993"  01'18.661"   Shit.                      Brea…      1       1
#> 3 7     01'18.829"  01'21.205"   [SIRENS WAILING IN DISTAN… Brea…      1       1
#> 4 8     01'24.918"  01'27.378"   Oh, God. Oh, my God.       Brea…      1       1
#> 5 9     01'27.546"  01'30.840"   Oh, my God. Oh, my God. T… Brea…      1       1

bb_sub_clean <- clean_captions(bb_sub)
bb_sub_clean
#> # A tibble: 4 × 7
#>   ID    Timecode_in Timecode_out Text_content               Name  Season Episode
#>   <chr> <time>      <time>       <chr>                      <chr>  <dbl>   <dbl>
#> 1 5     01'09.236"  01'12.780"   Oh, my God. Christ!        Brea…      1       1
#> 2 6     01'15.993"  01'18.661"   Shit.                      Brea…      1       1
#> 3 8     01'24.918"  01'27.378"   Oh, God. Oh, my God.       Brea…      1       1
#> 4 9     01'27.546"  01'30.840"   Oh, my God. Oh, my God. T… Brea…      1       1
```

### Binding subtitles

Sometimes you will need to bind several subtitle objects together. This
can be achieved with the function `bind_subtitles()`. This function is
very similar to `bind_rows` from `dplyr` (they both bind rows of
tibbles), but `bind_subtitles()` allows to recalculate timecodes to
follow concatenation order (this can be disabled by setting `sequential`
to `FALSE`).

``` r
bind_subtitles(rushmore_sub, oss_sub, bb_sub_clean)
#> # A tibble: 11 × 7
#>    ID    Timecode_in Timecode_out Text_content              Name  Season Episode
#>    <chr> <time>      <time>       <chr>                     <chr>  <dbl>   <dbl>
#>  1 180   20'40.969"  20'48.269"   Rushmore deserves an aqu… <NA>      NA      NA
#>  2 181   20'48.269"  20'50.870"   - I don't know. What do … <NA>      NA      NA
#>  3 182   20'50.946"  20'57.370"   - What kind of fish? - B… <NA>      NA      NA
#>  4 183   20'58.051"  21'01.770"   - Piranhas? Really? - Ye… <NA>      NA      NA
#>  5 264   41'24.737"  41'29.197"   Si vous voulez. Ça sera … <NA>      NA      NA
#>  6 265   41'32.117"  41'34.067"   Et non pas le gratin de … <NA>      NA      NA
#>  7 266   41'37.357"  41'39.467"   Parce que ça ressemble à… <NA>      NA      NA
#>  8 5     42'48.703"  42'52.247"   Oh, my God. Christ!       Brea…      1       1
#>  9 6     42'55.460"  42'58.128"   Shit.                     Brea…      1       1
#> 10 8     43'04.385"  43'06.845"   Oh, God. Oh, my God.      Brea…      1       1
#> 11 9     43'07.013"  43'10.307"   Oh, my God. Oh, my God. … Brea…      1       1
```

Some functions under certain conditions can also return a list of
subtitle objects (class `multisubtitles`). The function
`bind_subtitles()` can also be used on such object to bind each elements
into a new subtitle object, i.e. something similar to
`do.call(rbind, x)`.

``` r
multi_sub <- bind_subtitles(rushmore_sub, bb_sub_clean, collapse = FALSE, sequential = FALSE)
multi_sub
#> A multisubtitles object with 2 elements
#> subtitles object [[1]]
#> # A tibble: 4 × 4
#>   ID    Timecode_in Timecode_out Text_content
#>   <chr> <time>      <time>       <chr>
#> 1 180   20'40.969"  20'48.269"   Rushmore deserves an aquarium. A first class a…
#> 2 181   20'48.269"  20'50.870"   - I don't know. What do you think, Ernie - Aqu…
#> 3 182   20'50.946"  20'57.370"   - What kind of fish? - Barracudas. Stingrays. …
#> 4 183   20'58.051"  21'01.770"   - Piranhas? Really? - Yes, I'm talking to a gu…
#>
#>
#> subtitles object [[2]]
#> # A tibble: 4 × 7
#>   ID    Timecode_in Timecode_out Text_content               Name  Season Episode
#>   <chr> <time>      <time>       <chr>                      <chr>  <dbl>   <dbl>
#> 1 5     01'09.236"  01'12.780"   Oh, my God. Christ!        Brea…      1       1
#> 2 6     01'15.993"  01'18.661"   Shit.                      Brea…      1       1
#> 3 8     01'24.918"  01'27.378"   Oh, God. Oh, my God.       Brea…      1       1
#> 4 9     01'27.546"  01'30.840"   Oh, my God. Oh, my God. T… Brea…      1       1

bind_subtitles(multi_sub)
#> # A tibble: 8 × 7
#>   ID    Timecode_in Timecode_out Text_content               Name  Season Episode
#>   <chr> <time>      <time>       <chr>                      <chr>  <dbl>   <dbl>
#> 1 180   20'40.969"  20'48.269"   Rushmore deserves an aqua… <NA>      NA      NA
#> 2 181   20'48.269"  20'50.870"   - I don't know. What do y… <NA>      NA      NA
#> 3 182   20'50.946"  20'57.370"   - What kind of fish? - Ba… <NA>      NA      NA
#> 4 183   20'58.051"  21'01.770"   - Piranhas? Really? - Yes… <NA>      NA      NA
#> 5 5     22'11.006"  22'14.550"   Oh, my God. Christ!        Brea…      1       1
#> 6 6     22'17.763"  22'20.431"   Shit.                      Brea…      1       1
#> 7 8     22'26.688"  22'29.148"   Oh, God. Oh, my God.       Brea…      1       1
#> 8 9     22'29.316"  22'32.610"   Oh, my God. Oh, my God. T… Brea…      1       1
```

### Tidying subtitles

The [tidy text format](https://www.tidytextmining.com/tidytext.html) as
defined by Julia Silge and David Robinson is a table with
one-token-per-row, a token being a meaningful unit of text, such as a
word or a sentence. The objects returned by `read_subtitles*()` are in
some ways already tidy (each row being a subtitle block associated with
a timecode). However, this unit is not always the most relevant for data
analysis. To perform tokenization, the `tidytext` package provides the
generic function `unnest_tokens()`. The package `subtools` adds a new
method to `unnest_tokens()` to handle subtitles objects. The main
difference with the `data.frame` method is the possibility to perform
timecode remapping according to the tokenisation process.

``` r
rushmore_sub
#> # A tibble: 4 × 4
#>   ID    Timecode_in Timecode_out Text_content
#>   <chr> <time>      <time>       <chr>
#> 1 180   20'40.969"  20'48.269"   Rushmore deserves an aquarium. A first class a…
#> 2 181   20'48.269"  20'50.870"   - I don't know. What do you think, Ernie - Aqu…
#> 3 182   20'50.946"  20'57.370"   - What kind of fish? - Barracudas. Stingrays. …
#> 4 183   20'58.051"  21'01.770"   - Piranhas? Really? - Yes, I'm talking to a gu…

unnest_tokens(rushmore_sub)
#> # A tibble: 49 × 4
#>    ID    Timecode_in Timecode_out Text_content
#>    <chr> <time>      <time>       <chr>       
#>  1 180   20'40.9700" 20'41.4858"  rushmore    
#>  2 180   20'41.4868" 20'42.0026"  deserves    
#>  3 180   20'42.0036" 20'42.1318"  an          
#>  4 180   20'42.1328" 20'42.6486"  aquarium    
#>  5 180   20'42.6496" 20'42.7132"  a           
#>  6 180   20'42.7142" 20'43.0363"  first       
#>  7 180   20'43.0373" 20'43.3593"  class       
#>  8 180   20'43.3603" 20'43.8761"  aquarium    
#>  9 180   20'43.8771" 20'44.1991"  where       
#> 10 180   20'44.2001" 20'44.8451"  scientists  
#> 11 180   20'44.8461" 20'45.0389"  can         
#> 12 180   20'45.0399" 20'45.4911"  lecture     
#> 13 180   20'45.4921" 20'45.6849"  and         
#> 14 180   20'45.6859" 20'46.2017"  students    
#> 15 180   20'46.2027" 20'46.3955"  can         
#> 16 180   20'46.3965" 20'46.8478"  observe     
#> 17 180   20'46.8488" 20'47.2354"  marine      
#> 18 180   20'47.2364" 20'47.4938"  life        
#> 19 180   20'47.4948" 20'47.6230"  in          
#> 20 180   20'47.6240" 20'47.8168"  its         
#> 21 180   20'47.8178" 20'48.2690"  natural     
#> 22 181   20'48.2700" 20'48.3393"  i           
#> 23 181   20'48.3403" 20'48.6908"  don't       
#> 24 181   20'48.6918" 20'48.9720"  know        
#> 25 181   20'48.9730" 20'49.2532"  what        
#> 26 181   20'49.2542" 20'49.3938"  do          
#> 27 181   20'49.3948" 20'49.6046"  you         
#> 28 181   20'49.6056" 20'49.9561"  think       
#> 29 181   20'49.9571" 20'50.3076"  ernie       
#> 30 181   20'50.3086" 20'50.8700"  aquarium    
#> 31 182   20'50.9470" 20'51.4402"  what        
#> 32 182   20'51.4412" 20'51.9343"  kind        
#> 33 182   20'51.9353" 20'52.1814"  of          
#> 34 182   20'52.1824" 20'52.6755"  fish        
#> 35 182   20'52.6765" 20'53.9109"  barracudas  
#> 36 182   20'53.9119" 20'55.0228"  stingrays   
#> 37 182   20'55.0238" 20'56.3817"  hammerheads 
#> 38 182   20'56.3827" 20'57.3700"  piranhas    
#> 39 183   20'58.0520" 20'58.6840"  piranhas    
#> 40 183   20'58.6850" 20'59.1588"  really      
#> 41 183   20'59.1598" 20'59.3962"  yes         
#> 42 183   20'59.3972" 20'59.6336"  i'm         
#> 43 183   20'59.6346" 21'00.1874"  talking     
#> 44 183   21'00.1884" 21'00.3457"  to          
#> 45 183   21'00.3467" 21'00.4248"  a           
#> 46 183   21'00.4258" 21'00.6622"  guy         
#> 47 183   21'00.6632" 21'00.8205"  in          
#> 48 183   21'00.8215" 21'01.2161"  south       
#> 49 183   21'01.2171" 21'01.7700"  america

unnest_tokens(bb_sub_clean, token = "sentences")
#> # A tibble: 8 × 7
#>   ID    Timecode_in Timecode_out Text_content         Name        Season Episode
#>   <chr> <time>      <time>       <chr>                <chr>        <dbl>   <dbl>
#> 1 5     01'09.2370" 01'11.4018"  oh, my god.          Breaking B…      1       1
#> 2 5     01'11.4028" 01'12.7800"  christ!              Breaking B…      1       1
#> 3 6     01'15.9940" 01'18.6610"  shit.                Breaking B…      1       1
#> 4 8     01'24.9190" 01'25.9538"  oh, god.             Breaking B…      1       1
#> 5 8     01'25.9548" 01'27.3780"  oh, my god.          Breaking B…      1       1
#> 6 9     01'27.5470" 01'28.4087"  oh, my god.          Breaking B…      1       1
#> 7 9     01'28.4097" 01'29.2714"  oh, my god.          Breaking B…      1       1
#> 8 9     01'29.2724" 01'30.8400"  think, think, think. Breaking B…      1       1
```

Note that unlike the `data.frame` method, the `input` and `output`
arguments are optional. This is because here the `Text_content` column
can be assumed to be the column of interest.

Once your data are ready, you can analyse them. I recommend you to have
a look at [Text Mining with R: A Tidy
Approach](https://www.tidytextmining.com/) by Julia Silge and David
Robinson. This is a great place to get started with text mining in R.

### Applications

A list of cool projects using `subtools`.

Note that these project used the branch 0.x of `subtools`. The API is
totally different from `subtools 1.0`.

[*You beautiful, naïve, sophisticated newborn
series*](https://masalmon.eu/2017/11/05/newborn-serie/) by
[ma_salmon](https://x.com/ma_salmon)

[*A tidy text analysis of Rick and
Morty*](http://tamaszilagyi.com/blog/2017/2017-10-07-tidyrick/) by
[tudosgar](https://x.com/tudosgar)

[*Term Frequencies by
Season*](https://x.com/tdawry/status/919055698427809792) by
[tdawry](https://x.com/tdawry)
