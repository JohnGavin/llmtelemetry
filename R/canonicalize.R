# Internal helpers for project-name canonicalization.
#
# Used by rollup_sessions(), append_sessions_from_staging(),
# rollup_costs(), append_costs_from_staging(),
# rollup_git_commits(), and append_git_commits_from_staging().
#
# Hook-emitted project names arrive in dash-form
# (e.g. "docs-gh-llmtelemetry"); .shorten_project_local() strips the common
# prefix before .canonicalize_project_local() runs.
#
# TODO: replace with a proper exported function once epic #83 stabilises.

#' @keywords internal
.shorten_project_local <- function(x) {
  if (is.null(x) || is.na(x)) return(NA_character_)
  # Special NHS/personal path prefix (double-dash form):
  # "-Users-johngavin-docs--pers-NHS-health-data-antigravity-<project>"
  x <- gsub("^-Users-johngavin-docs--pers-NHS-health-data-antigravity-", "", x)
  x <- gsub("^-Users-johngavin-docs[-_]gh-", "", x)
  x <- gsub("^docs[-_]gh[-_]",               "", x)
  x <- gsub("^llm-",                          "", x)
  x <- gsub("^proj-",                         "", x)
  # Convert dash separators to slash (dash-form hook output -> slash-form).
  x <- gsub("-", "/", x, fixed = TRUE)

  # Handle underscore-form nested paths emitted by some sanitiser variants
  # (e.g. "docs_gh_llmtelemetry", "-Users_johngavin_docs_gh_llm").
  # IMPORTANT: apply BEFORE dash-to-slash conversion has altered leading "-",
  # so we detect path-like prefixes on the ORIGINAL input.  Bare single-token
  # underscore names that appear in meta_only (e.g. "urban_planning") must NOT
  # be touched — the FIRST PATH SEGMENT check in the caller guards them later.
  # Detection: the raw value starts with "-" or "docs_gh_" (before any
  # conversion), which means we must re-test the pre-converted original here.
  # Since dash-to-slash already ran, we check the post-slash form for the
  # equivalent patterns: a leading "/" (was "-"), or literal "docs_gh_".
  is_path_like <- startsWith(x, "/") || startsWith(x, "docs_gh_")
  if (is_path_like) {
    x <- gsub("^/Users_johngavin_docs_gh_", "", x)
    x <- gsub("^/docs_gh_",                 "", x)
    x <- gsub("^docs_gh_",                  "", x)
    x <- gsub("^llm_",                       "", x)
    x <- gsub("^proj_",                      "", x)
    x <- gsub("_", "/", x, fixed = TRUE)
  }
  x
}

#' @keywords internal
.canonicalize_project_local_scalar <- function(name) {
  if (is.null(name) || is.na(name) || !nzchar(name)) return(NA_character_)
  # Convert dash-form project names emitted by the hook to slash-form first
  name <- .shorten_project_local(name)

  meta_only <- c(
    "sonnet", "roborev", "worktree",
    "antigravity", "crypto", "data", "github", "hello",
    "knowledge", "simulations", "sport", "subagents",
    "t", "io", "urban_planning", "notmineraft", "telemetry", "football"
  )
  if (name %in% meta_only) return(NA_character_)

  name <- sub("^[A-Za-z0-9_]{8,}/repo/?", "", name)
  if (!nzchar(name)) return(NA_character_)

  # Explicit prefix overrides — checked before container-prefix strip.
  overrides <- list(
    "buoy/network"                    = "irish_buoy_network",
    "irishbuoys"                      = "irish_buoy_network",
    # raw path proj-data-weather-irish-buoy-network -> data/weather/irish/buoy/network
    "data/weather/irish/buoy/network" = "irish_buoy_network",
    # underscore form used in tracked repo paths: data/weather/irish_buoy_network/...
    "data/weather/irish_buoy_network" = "irish_buoy_network",
    # after data/ strip: weather/irish/buoy/network (dash form)
    "weather/irish/buoy/network"      = "irish_buoy_network",
    # after data/ strip: weather/irish_buoy_network/... (underscore form)
    "weather/irish_buoy_network"      = "irish_buoy_network"
  )
  for (pat in names(overrides)) {
    if (startsWith(name, pat)) return(overrides[[pat]])
  }

  container_prefixes <- c(
    # Multi-segment container prefixes (must come before single-segment ones):
    "stats/simulations/", "stats/sport/", "finance/data/",
    # Single-segment container prefixes:
    "worktree/", "simulations/", "sport/", "data/", "crypto/",
    "subagents/", "knowledge/", "github/", "antigravity/", "hello/",
    "stats/", "pers/", "finance/"
  )
  for (pfx in container_prefixes) {
    if (startsWith(name, pfx)) {
      name <- sub(paste0("^", pfx), "", name)
      break
    }
  }
  if (!nzchar(name)) return(NA_character_)

  parts <- strsplit(name, "/", fixed = TRUE)[[1]]
  if (length(parts) == 0L || !nzchar(parts[1L])) return(NA_character_)
  first <- parts[1L]
  if (grepl("^[0-9]+$", first)) return(NA_character_)
  if (first %in% meta_only) return(NA_character_)
  first
}

#' @keywords internal
.canonicalize_project_local <- Vectorize(.canonicalize_project_local_scalar,
                                         USE.NAMES = FALSE)
