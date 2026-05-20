# export_dashboard_data.R CI fallback copies ccusage_daily.json (not ccusage_daily_all.json)

    Code
      block_lines
    Output
       [1] "  fallback_daily_src <- file.path(extdata, \"ccusage_daily.json\")"                             
       [2] "  fallback_daily_dst <- file.path(out_dir, \"ccusage_daily.json\")"                             
       [3] "  if (file.exists(fallback_daily_src)) {"                                                       
       [4] "    file.copy(fallback_daily_src, fallback_daily_dst, overwrite = TRUE)"                        
       [5] "    cat(\"  -> copied ccusage_daily.json (CI fallback)\\n\")"                                   
       [6] "  } else if (!file.exists(fallback_daily_dst)) {"                                               
       [7] "    write_json(list(), fallback_daily_dst, auto_unbox = TRUE)"                                  
       [8] "    cat(\"  -> ccusage_daily.json not found, wrote empty (CI fallback)\\n\")"                   
       [9] "  }"                                                                                            
      [10] "  if (!file.exists(file.path(out_dir, \"ccusage_sessions.json\"))) {"                           
      [11] "    write_json(list(), file.path(out_dir, \"ccusage_sessions.json\"), auto_unbox = TRUE)"       
      [12] "    cat(\"  -> wrote empty ccusage_sessions.json (CI fallback; cmonitor-rs not available)\\n\")"
      [13] "  }"                                                                                            
      [14] "  if (!file.exists(file.path(out_dir, \"ccusage_blocks.json\"))) {"                             
      [15] "    write_json(list(), file.path(out_dir, \"ccusage_blocks.json\"), auto_unbox = TRUE)"         
      [16] "    cat(\"  -> wrote empty ccusage_blocks.json (CI fallback; cmonitor-rs not available)\\n\")"  

