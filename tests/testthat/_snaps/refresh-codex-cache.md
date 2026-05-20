# parse_otel_turn_line snapshot — structure is stable

    Code
      sort(names(result))
    Output
       [1] "cached_input_tokens"     "input_tokens"           
       [3] "model"                   "non_cached_input_tokens"
       [5] "output_tokens"           "reasoning_effort"       
       [7] "reasoning_output_tokens" "thread_id"              
       [9] "timestamp"               "total_tokens"           

# compute_cost snapshot — error message wording is stable

    Code
      withCallingHandlers(compute_cost(100L, 0L, 50L, "nonexistent-model",
        MINIMAL_PRICING), warning = function(w) {
        cat("WARNING:", conditionMessage(w), "\n")
        invokeRestart("muffleWarning")
      })
    Output
      WARNING: Model 'nonexistent-model' not found in codex_pricing.json. est_cost_usd = NA. 
      [1] NA

# aggregate_daily snapshot — column names are stable

    Code
      names(result)
    Output
       [1] "date"                    "canonical_project"      
       [3] "model"                   "source"                 
       [5] "input_tokens"            "cached_input_tokens"    
       [7] "output_tokens"           "reasoning_output_tokens"
       [9] "total_tokens"            "n_turns"                
      [11] "est_cost_usd"           

# canonicalize_project snapshot — output for known paths is stable

    Code
      canonicalize_project(paths)
    Output
      [1] "llmtelemetry" "llm"          "mycare"       NA            

# join_roborev snapshot — column names after join are stable

    Code
      sort(names(result))
    Output
      [1] "canonical_project" "model"             "repo_id"          
      [4] "repo_name"         "source"            "thread_id"        

# merge logic aborts when new sessions tibble is empty and existing has rows

    Code
      guard_fn(0L, 5L, "codex_sessions.json")
    Condition
      Error in `guard_fn()`:
      x Refusing to write codex_sessions.json: row count would shrink.
      i Existing rows: 5; new combined rows: 0.
      i This indicates an upstream parse failure — aborting to protect history.

