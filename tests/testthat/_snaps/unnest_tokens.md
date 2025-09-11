# unnest_token.subtitles works as expected

    Code
      unnest_tokens(s)
    Output
      tibble [36, 5]
      ID           chr 1 1 1 1 2 2 2
      Timecode_in  hms 00:00:01.001 00:00:01.626 00:00:02.251 00:~
      Timecode_out hms 00:00:01.625 00:00:02.25 00:00:03 00:00:04~
      Text_content chr never drink liquid nitrogen it will perfor~
      test         chr Test Test Test Test Test Test Test

