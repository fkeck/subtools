
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/subtools.png" width="120" align="right" />

# subtools

[![Build
Status](https://travis-ci.org/fkeck/subtools.svg?branch=master)](https://travis-ci.org/fkeck/subtools)

### Read, write and manipulate subtitles in R

Hi\! Welcome on the readme of `subtools`. Here, you will find some basic
informations to get started with subtools. For more details, you can
check the package documentation.

Subtools is a R package to read, write and manipulate subtitles in R.
This then allows the full range of tools offered by the R ecosystem to
be used for the analysis of subtitles. With version `1.0`, `subtools`
integrates the main principles of the tidyverse and integrates directly
with `tidytext` for a tidy approach of subtitle text mining.

### Install

To install the package from Github you can use devtools:

    devtools::install_github("fkeck/subtools")

``` r
library(subtools)
library(tidytext)
```

### Reading subtitles

The main objective of subtools is to allow the importation of subtitle
files directly into R. This task can be easily performed with the
function `read_subtitles()`:

``` r
oss_sub <- read_subtitles("ex_OSS_117.srt")
rushmore_sub <- read_subtitles("ex_Rushmore.srt")
bb_sub <- read_subtitles("ex_Breaking_Bad.srt")
```

``` r
oss_sub
#> # A tibble: 3 x 4
#>   ID    Timecode_in Timecode_out Text_content                              
#>   <chr> <time>      <time>       <chr>                                     
#> 1 264   20'22.967"  20'27.427"   Si vous voulez. Ça sera surtout l'occasio…
#> 2 265   20'30.347"  20'32.297"   Et non pas le gratin de pommes de terre.  
#> 3 266   20'35.587"  20'37.697"   Parce que ça ressemble à carotte, cairote.

rushmore_sub
#> # A tibble: 4 x 4
#>   ID    Timecode_in Timecode_out Text_content                              
#>   <chr> <time>      <time>       <chr>                                     
#> 1 180   20'40.969"  20'48.269"   Rushmore deserves an aquarium. A first cl…
#> 2 181   20'48.269"  20'50.870"   - I don't know. What do you think, Ernie …
#> 3 182   20'50.946"  20'57.370"   - What kind of fish? - Barracudas. Stingr…
#> 4 183   20'58.051"  21'01.770"   - Piranhas? Really? - Yes, I'm talking to…

bb_sub
#> # A tibble: 5 x 4
#>   ID    Timecode_in Timecode_out Text_content                              
#>   <chr> <time>      <time>       <chr>                                     
#> 1 5     01'09.236"  01'12.780"   Oh, my God. Christ!                       
#> 2 6     01'15.993"  01'18.661"   Shit.                                     
#> 3 7     01'18.829"  01'21.205"   [SIRENS WAILING IN DISTANCE]              
#> 4 8     01'24.918"  01'27.378"   Oh, God. Oh, my God.                      
#> 5 9     01'27.546"  01'30.840"   Oh, my God. Oh, my God. Think, think, thi…
```

The function `read_subtitles()` returns an object of class `subtitles`.
This is a simple `tibble` with at least four columns (“`ID`”,
“`Timecode_in`”, “`Timecode_out`” and “`Text_content`”).

If you want to analyze subtitles of series with different seasons and
episodes, you will have to import many files at once. The
`read_subtitles_season()`, `read_subtitles_serie()` and
`read_subtitles_multiseries()` functions can make your life much easier,
by making it possible to automatically import files and extract metadata
from a structured directory. You can check the manual for more details.

Finally if you have a collection of movies in .mkv format, you can
extract the subtitle tracks of MKV files with `read_subtitles_mkv()`.

### Cleaning subtitles

Often, the workflow begins with a cleaning step to get rid of irrelevant
information that might be present in text content. Three functions can
be used for this task. First, `clean_tags()` cleans formatting tags. By
default, this function is automatically executed by the
`read_subtitles*()` functions, so you probably don’t need to run it
again. Second, `clean_captions()` can be used to supress closed
captions, i.e. descriptions of non-speech elements in parentheses or
squared brackets. Finally, `clean_patterns()` is a more general function
to clean subtitles based on regex pattern matching.

``` r
bb_sub
#> # A tibble: 5 x 4
#>   ID    Timecode_in Timecode_out Text_content                              
#>   <chr> <time>      <time>       <chr>                                     
#> 1 5     01'09.236"  01'12.780"   Oh, my God. Christ!                       
#> 2 6     01'15.993"  01'18.661"   Shit.                                     
#> 3 7     01'18.829"  01'21.205"   [SIRENS WAILING IN DISTANCE]              
#> 4 8     01'24.918"  01'27.378"   Oh, God. Oh, my God.                      
#> 5 9     01'27.546"  01'30.840"   Oh, my God. Oh, my God. Think, think, thi…

bb_sub_clean <- clean_captions(bb_sub)
bb_sub_clean
#> # A tibble: 4 x 4
#>   ID    Timecode_in Timecode_out Text_content                              
#>   <chr> <time>      <time>       <chr>                                     
#> 1 5     01'09.236"  01'12.780"   Oh, my God. Christ!                       
#> 2 6     01'15.993"  01'18.661"   Shit.                                     
#> 3 8     01'24.918"  01'27.378"   Oh, God. Oh, my God.                      
#> 4 9     01'27.546"  01'30.840"   Oh, my God. Oh, my God. Think, think, thi…
```

### Binding subtitles

Sometimes you will need to bind several subtitle objects together. This
can be achieved with the function `bind_subtitles()`. This function is
very similar to `bind_rows` from `dplyr` (they both bind rows of
tibbles), but `bind_subtitles()` allows to recalculate timecodes to
follow concatenation order (this can be disabled by setting `sequential`
to `FALSE`).

``` r
bind_subtitles(oss_sub, rushmore_sub, bb_sub_clean)
#> # A tibble: 11 x 4
#>    ID    Timecode_in Timecode_out Text_content                             
#>    <chr> <time>      <time>       <chr>                                    
#>  1 264   20'22.967"  20'27.427"   Si vous voulez. Ça sera surtout l'occasi…
#>  2 265   20'30.347"  20'32.297"   Et non pas le gratin de pommes de terre. 
#>  3 266   20'35.587"  20'37.697"   Parce que ça ressemble à carotte, cairot…
#>  4 446   41'18.666"  41'25.966"   Rushmore deserves an aquarium. A first c…
#>  5 447   41'25.966"  41'28.567"   - I don't know. What do you think, Ernie…
#>  6 448   41'28.643"  41'35.067"   - What kind of fish? - Barracudas. Sting…
#>  7 449   41'35.748"  41'39.467"   - Piranhas? Really? - Yes, I'm talking t…
#>  8 454   42'48.703"  42'52.247"   Oh, my God. Christ!                      
#>  9 455   42'55.460"  42'58.128"   Shit.                                    
#> 10 457   43'04.385"  43'06.845"   Oh, God. Oh, my God.                     
#> 11 458   43'07.013"  43'10.307"   Oh, my God. Oh, my God. Think, think, th…
```

Some functions under certain conditions can also return a list of
subtitle objects (class `multisubtitles`). The function
`bind_subtitles()` can also be used on such object to bind each elements
into a new subtitle object, i.e. something similar to `do.call(rbind,
x)`.

``` r
multi_sub <- bind_subtitles(oss_sub, bb_sub_clean, collapse = FALSE, sequential = FALSE)
multi_sub
#> A MultiSubtitles object with 2 elements
#> Subtitles object [[1]]
#> # A tibble: 3 x 4
#>   ID    Timecode_in Timecode_out Text_content                              
#>   <chr> <time>      <time>       <chr>                                     
#> 1 264   20'22.967"  20'27.427"   Si vous voulez. Ça sera surtout l'occasio…
#> 2 265   20'30.347"  20'32.297"   Et non pas le gratin de pommes de terre.  
#> 3 266   20'35.587"  20'37.697"   Parce que ça ressemble à carotte, cairote.
#> 
#> 
#> Subtitles object [[2]]
#> # A tibble: 4 x 4
#>   ID    Timecode_in Timecode_out Text_content                              
#>   <chr> <time>      <time>       <chr>                                     
#> 1 5     01'09.236"  01'12.780"   Oh, my God. Christ!                       
#> 2 6     01'15.993"  01'18.661"   Shit.                                     
#> 3 8     01'24.918"  01'27.378"   Oh, God. Oh, my God.                      
#> 4 9     01'27.546"  01'30.840"   Oh, my God. Oh, my God. Think, think, thi…

bind_subtitles(multi_sub)
#> # A tibble: 7 x 4
#>   ID    Timecode_in Timecode_out Text_content                              
#>   <chr> <time>      <time>       <chr>                                     
#> 1 264   20'22.967"  20'27.427"   Si vous voulez. Ça sera surtout l'occasio…
#> 2 265   20'30.347"  20'32.297"   Et non pas le gratin de pommes de terre.  
#> 3 266   20'35.587"  20'37.697"   Parce que ça ressemble à carotte, cairote.
#> 4 271   21'46.933"  21'50.477"   Oh, my God. Christ!                       
#> 5 272   21'53.690"  21'56.358"   Shit.                                     
#> 6 274   22'02.615"  22'05.075"   Oh, God. Oh, my God.                      
#> 7 275   22'05.243"  22'08.537"   Oh, my God. Oh, my God. Think, think, thi…
```

### Tidying subtitles

The tidy text format as defined by Julia Silge and David Robinson is a
table with one-token-per-row, a token being a meaningful unit of text,
such as a word or a sentence. The objects returned by
`read_subtitles*()` are in some ways already tidy (each row being a
subtitle block associated with a timecode). However, this unit is not
always the most relevant for data analysis. To perform tokenization, the
`tidytext` package provides the generic function `unnest_tokens()`. The
package `subtools` adds a new method to `unnest_tokens()` to handle
subtitles objects. The main difference with the `data.frame` method is
the possibility to perform timecode remapping according to the
tokenisation process.

``` r
rushmore_sub
#> # A tibble: 4 x 4
#>   ID    Timecode_in Timecode_out Text_content                              
#>   <chr> <time>      <time>       <chr>                                     
#> 1 180   20'40.969"  20'48.269"   Rushmore deserves an aquarium. A first cl…
#> 2 181   20'48.269"  20'50.870"   - I don't know. What do you think, Ernie …
#> 3 182   20'50.946"  20'57.370"   - What kind of fish? - Barracudas. Stingr…
#> 4 183   20'58.051"  21'01.770"   - Piranhas? Really? - Yes, I'm talking to…

unnest_tokens(rushmore_sub, output = Text_content, input = Text_content, drop = FALSE)
#> # A tibble: 49 x 4
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
#> # … with 39 more rows

unnest_tokens(bb_sub_clean, output = Text_content, input = Text_content, drop = FALSE, token = "sentences")
#> # A tibble: 8 x 4
#>   ID    Timecode_in Timecode_out Text_content        
#>   <chr> <time>      <time>       <chr>               
#> 1 5     01'09.2370" 01'11.4018"  oh, my god.         
#> 2 5     01'11.4028" 01'12.7800"  christ!             
#> 3 6     01'15.9940" 01'18.6610"  shit.               
#> 4 8     01'24.9190" 01'25.9538"  oh, god.            
#> 5 8     01'25.9548" 01'27.3780"  oh, my god.         
#> 6 9     01'27.5470" 01'28.4087"  oh, my god.         
#> 7 9     01'28.4097" 01'29.2714"  oh, my god.         
#> 8 9     01'29.2724" 01'30.8400"  think, think, think.
```

### Applications

A list of cool projects using subtools.

[*You beautiful, naïve, sophisticated newborn
series*](http://www.masalmon.eu/2017/11/05/newborn-serie/) by
[ma\_salmon](https://twitter.com/ma_salmon)

[*A tidy text analysis of Rick and
Morty*](http://tamaszilagyi.com/blog/a-tidy-text-analysis-of-rick-and-morty/)
by [tudosgar](https://twitter.com/tudosgar)

*Rick and Morty and Tidy Data Principles*
([part 1](http://pacha.hk/2017-10-13_rick_and_morty_tidy_data.html))
([part 2](http://pacha.hk/2017-10-22_rick_and_morty_tidy_data_2.html))
([part 3](http://pacha.hk/2017-11-06_rick_and_morty_tidy_data_3.html))
by [pachamaltese](https://twitter.com/pachamaltese)

[*Term Frequencies by
Season*](https://twitter.com/tdawry/status/919055698427809792) by
[tdawry](https://twitter.com/tdawry)
