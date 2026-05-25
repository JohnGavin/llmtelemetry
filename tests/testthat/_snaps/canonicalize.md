# mixed dash-underscore snapshot

    Code
      .canonicalize_project_local(inputs)
    Output
      [1] "llmtelemetry" "llmtelemetry"

# dash-form, slash-form, underscore-path, and bare-underscore snapshot

    Code
      result
    Output
      [1] "llmtelemetry"       "llmtelemetry"       "llmtelemetry"      
      [4] "llm"                "urban_planning"     "irish_buoy_network"

# .shorten_project_local snapshot — output for known forms is stable

    Code
      result
    Output
      [1] "llmtelemetry"   "llmtelemetry"   "llm"            "llm"           
      [5] "urban_planning"

