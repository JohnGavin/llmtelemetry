# export_dashboard_data.R CI fallback copies legacy_ccusage_daily.json (not ccusage_daily_all.json)

    Code
      block_lines
    Output
       [1] "  fallback_daily_src <- file.path(extdata, \"legacy_ccusage_daily.json\")"                    
       [2] "  fallback_daily_dst <- file.path(out_dir, \"legacy_ccusage_daily.json\")"                    
       [3] "  if (file.exists(fallback_daily_src)) {"                                                     
       [4] "    file.copy(fallback_daily_src, fallback_daily_dst, overwrite = TRUE)"                      
       [5] "    cat(\"  -> copied legacy_ccusage_daily.json (CI fallback)\\n\")"                          
       [6] "  } else if (!file.exists(fallback_daily_dst)) {"                                             
       [7] "    write_json_atomic(list(), fallback_daily_dst, auto_unbox = TRUE)"                         
       [8] "    cat(\"  -> legacy_ccusage_daily.json not found, wrote empty (CI fallback)\\n\")"          
       [9] "  }"                                                                                          
      [10] "  # Always write empty sessions (raw sessionIds must not appear in public output)."           
      [11] "  write_json_atomic(list(), file.path(out_dir, \"ccusage_sessions.json\"), auto_unbox = TRUE)"
      [12] "  cat(\"  -> wrote empty ccusage_sessions.json (CI fallback; cmonitor-rs not available)\\n\")"
      [13] "  # Blocks: use committed inst/extdata snapshot if present, else write empty array."          
      [14] "  # This keeps the \"Time Blocks\" dashboard page populated in CI. (#141)"                    
      [15] "  # (#281 Phase 2): renamed to legacy_ccusage_blocks.json"                                    
      [16] "  fallback_blocks_src <- file.path(extdata, \"legacy_ccusage_blocks.json\")"                  

# codexbar_cost_per_day.json column names match expected schema snapshot

    Code
      sort(names(result))
    Output
      [1] "cost_usd"     "date"         "model"        "total_tokens"

