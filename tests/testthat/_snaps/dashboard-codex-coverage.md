# SOURCE_PALETTE snapshot — values are stable

    Code
      pal
    Output
         claude    gemini     codex 
      "#d97757" "#4285f4" "#10a37f" 

# codex_daily.json snapshot — column names are stable

    Code
      sort(names(d))
    Output
       [1] "cached_input_tokens"     "canonical_project"      
       [3] "date"                    "est_cost_usd"           
       [5] "input_tokens"            "model"                  
       [7] "n_turns"                 "output_tokens"          
       [9] "reasoning_output_tokens" "source"                 
      [11] "total_tokens"           

# codex_sessions.json snapshot — column names are stable

    Code
      sort(names(s))
    Output
      [1] "canonical_project" "ended_at"          "est_cost_usd"     
      [4] "model"             "n_turns"           "source"           
      [7] "started_at"        "thread_id"         "total_tokens"     

