# unnest_token.subtitles works as expected

    Code
      unnest_tokens(s)
    Output
      # A tibble: 36 x 5
         ID              Timecode_in Timecode_out Text_content test 
         <chr>           <time>      <time>       <chr>        <chr>
       1 1               00'01.0010" 00'01.6250"  never        Test 
       2 1               00'01.6260" 00'02.2500"  drink        Test 
       3 1               00'02.2510" 00'03.0000"  liquid       Test 
       4 1               00'03.0010" 00'04.0000"  nitrogen     Test 
       5 2               00'05.0010" 00'05.2162"  it           Test 
       6 2               00'05.2172" 00'05.6486"  will         Test 
       7 2               00'05.6496" 00'06.6216"  perforate    Test 
       8 2               00'06.6226" 00'07.0541"  your         Test 
       9 2               00'07.0551" 00'07.8108"  stomach      Test 
      10 2               00'07.8118" 00'08.1351"  you          Test 
      11 2               00'08.1361" 00'08.6757"  could        Test 
      12 2               00'08.6767" 00'09.0000"  die          Test 
      13 A dangerous cue 00'11.0010" 00'11.0989"  dès          Test 
      14 A dangerous cue 00'11.0999" 00'11.2308"  noël         Test 
      15 A dangerous cue 00'11.2318" 00'11.2967"  où           Test 
      16 A dangerous cue 00'11.2977" 00'11.3626"  un           Test 
      17 A dangerous cue 00'11.3636" 00'11.5604"  zéphyr       Test 
      18 A dangerous cue 00'11.5614" 00'11.6593"  haï          Test 
      19 A dangerous cue 00'11.6603" 00'11.7253"  me           Test 
      20 A dangerous cue 00'11.7263" 00'11.8242"  vêt          Test 
      21 A dangerous cue 00'11.8252" 00'11.8901"  de           Test 
      22 A dangerous cue 00'11.8911" 00'12.1209"  glaçons      Test 
      23 A dangerous cue 00'12.1219" 00'12.3846"  würmiens     Test 
      24 A dangerous cue 00'12.3856" 00'12.4505"  je           Test 
      25 A dangerous cue 00'12.4515" 00'12.5824"  dîne         Test 
      26 A dangerous cue 00'12.5834" 00'12.8462"  d’exquis     Test 
      27 A dangerous cue 00'12.8472" 00'13.0110"  rôtis        Test 
      28 A dangerous cue 00'13.0120" 00'13.0769"  de           Test 
      29 A dangerous cue 00'13.0779" 00'13.2088"  bœuf         Test 
      30 A dangerous cue 00'13.2098" 00'13.2747"  au           Test 
      31 A dangerous cue 00'13.2757" 00'13.3736"  kir          Test 
      32 A dangerous cue 00'13.3746" 00'13.4066"  à            Test 
      33 A dangerous cue 00'13.4076" 00'13.5385"  l’aÿ         Test 
      34 A dangerous cue 00'13.5395" 00'13.7033"  d’âge        Test 
      35 A dangerous cue 00'13.7043" 00'13.8022"  mûr          Test 
      36 A dangerous cue 00'13.8032" 00'14.0000"  cætera       Test 

