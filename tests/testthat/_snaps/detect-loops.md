# detect_loops() detects all 5 synthetic loop patterns (snapshot)

    Code
      snap
    Output
      # A tibble: 5 x 3
        tier  primary_file             cycles
        <chr> <chr>                     <int>
      1 watch .github/workflows/ci.yml      3
      2 watch R/merge_helper.R              3
      3 watch R/read_buoys.R                3
      4 watch R/rotate_log.R                3
      5 watch R/sanitize.R                  3

