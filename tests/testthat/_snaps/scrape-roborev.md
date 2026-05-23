# .parse_roborev_show snapshot is stable for multi-finding fixture

    Code
      snap_input
    Output
      # A tibble: 4 x 2
        severity primary_file                
        <chr>    <chr>                       
      1 high     R/scrape_data.R             
      2 high     R/process_data.R            
      3 medium   R/canonicalize.R            
      4 low      tests/testthat/test-export.R

