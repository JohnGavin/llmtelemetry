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

# Branch/worktree suffixes that, when appended to a project name with a dash,
# indicate the project itself and not a separate sub-entity.
# Pattern: <project>-<SUFFIX_BRANCH_RE>-<anything>
# Examples: llmtelemetry-feat-cc-20260524-102501 -> llmtelemetry
#           llm-wt-193 -> llm
#           llm-sonnet -> llm (bare suffix, no trailing tokens)
.BRANCH_SUFFIX_RE <- paste0(
  "-(feat|fix|chore|docs|refactor|test|ci|perf|style|build|revert|wt|",
  "sonnet|haiku|opus|worktree)(-.*)?$"
)

#' @keywords internal
.shorten_project_local <- function(x) {
  if (is.null(x) || is.na(x)) return(NA_character_)

  # Handle .claude/worktrees/agent-<hex> paths: these are ephemeral agent
  # checkout directories, not real projects.  Return a sentinel that the
  # canonicalize scalar will drop as NA.
  if (grepl("^[.]claude/worktrees/agent-", x) ||
      grepl("[-/]claude[-/]worktrees[-/]agent-", x)) {
    return(".claude/worktrees/agent")
  }

  # Special NHS/personal path prefix (double-dash form):
  # "-Users-johngavin-docs--pers-NHS-health-data-antigravity-<project>"
  x <- gsub("^-Users-johngavin-docs--pers-NHS-health-data-antigravity-", "", x)
  x <- gsub("^-Users-johngavin-docs[-_]gh-", "", x)
  x <- gsub("^docs[-_]gh[-_]",               "", x)

  # Strip branch/worktree suffixes BEFORE stripping the "llm-" prefix.
  # Without this, "llm-feat-cc-20260524-102501" becomes "feat-cc-..." which
  # yields "feat" as the first segment — the root cause of cc/feat/wt noise.
  # Apply to the dash-form string before any dash→slash conversion.
  x <- sub(.BRANCH_SUFFIX_RE, "", x, perl = TRUE, ignore.case = TRUE)

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

  # Noise/meta-only tokens that should never appear as canonical project names.
  # Split into two groups for clarity:
  #   meta_only_original — in the list before this fix
  #   meta_only_noise    — added by this fix (branch fragments, tool names,
  #                        single-char tokens, ephemeral names)
  meta_only <- c(
    # Original set:
    "sonnet", "roborev", "worktree",
    "antigravity", "crypto", "data", "github", "hello",
    "knowledge", "simulations", "sport", "subagents",
    "t", "io", "urban_planning", "notmineraft", "telemetry", "football",
    # Added: branch fragment tokens (appear when suffix-stripping fails or raw
    # branch-type fragments are recorded as project names by hooks)
    "cc", "feat", "fix", "chore", "ci", "perf", "style", "build", "revert",
    "wt", "scope", "worker", "network", "repo", "docs", "eval", "project",
    # Added: ephemeral/tool names not associated with any persistent project
    "ClaudeProbe",
    # Added: .claude/worktrees/agent sentinel returned by .shorten_project_local
    ".claude/worktrees/agent"
  )
  if (name %in% meta_only) return(NA_character_)

  # Reject bare hex strings of 12+ characters — these are agent worktree hashes
  # (e.g. "ab6f701adcaed79e9") recorded directly as project names by some hooks.
  if (grepl("^[0-9a-f]{12,}$", name)) return(NA_character_)

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
