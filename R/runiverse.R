# R-Universe API Helpers
# Query package + build-check metadata from an R-Universe instance
# (https://<universe>.r-universe.dev/api/packages) for CI-minute
# conservation dashboards (llm#761). Re-implemented against the live API —
# no code copied from third-party helper packages (external-code-zero-trust).

# Declare NSE variables used in dplyr pipelines
utils::globalVariables(c("config", "check"))

# ---------------------------------------------------------------------------
# Internal helpers (not exported)
# ---------------------------------------------------------------------------

#' Build an httr2 request against the R-Universe API
#'
#' Configures a shared user agent, an on-disk response cache, retries, and a
#' timeout so repeated calls (e.g. dashboard refreshes) reuse cached
#' responses instead of re-hitting the R-Universe CDN on every call — the
#' API advertises `Cache-Control: max-age=60`, so `req_cache()` respects
#' that window automatically.
#'
#' @param url Character scalar — full R-Universe API URL.
#' @return An `httr2_request`, not yet performed.
#' @keywords internal
.runiverse_req <- function(url) {
  httr2::request(url) |>
    httr2::req_user_agent(
      "runiverse (llmtelemetry; https://github.com/JohnGavin/llmtelemetry)"
    ) |>
    httr2::req_cache(tools::R_user_dir("runiverse", "cache"), use_on_error = TRUE) |>
    httr2::req_retry(max_tries = 3L) |>
    httr2::req_timeout(30)
}

#' Fetch and parse a JSON document from the R-Universe API
#'
#' @param url Character scalar — full R-Universe API URL.
#' @return The parsed JSON body as a base R list (`simplifyVector = FALSE`),
#'   so nested structures (`_jobs`, `_maintainer`, ...) stay list-shaped for
#'   explicit extraction downstream.
#' @keywords internal
.runiverse_fetch_json <- function(url) {
  resp <- tryCatch(
    httr2::req_perform(.runiverse_req(url)),
    error = function(e) {
      cli::cli_abort(
        c(
          "x" = "Failed to fetch R-Universe API data.",
          "i" = "URL: {.url {url}}",
          "i" = "Error: {conditionMessage(e)}"
        ),
        parent = e
      )
    }
  )
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

#' Extract a scalar field from a parsed R-Universe package list
#'
#' Thin wrapper over [purrr::pluck()] that coerces `NULL`/missing values to
#' the given NA-typed default, so a single absent field never aborts the
#' whole row.
#'
#' @param pkg A parsed package list (one element of `/api/packages`).
#' @param ... Path passed to [purrr::pluck()].
#' @param default NA-typed default returned when the path is absent or NULL.
#' @keywords internal
.runiverse_pluck <- function(pkg, ..., default = NA_character_) {
  val <- purrr::pluck(pkg, ..., .default = NULL)
  if (is.null(val) || length(val) == 0L) {
    return(default)
  }
  val
}

#' Derive an operating-system label from a `_jobs[].config` string
#'
#' R-Universe build configs look like `linux-devel-x86_64`,
#' `macos-release-arm64`, `windows-oldrel`, `source`, `pkgdown`, or
#' `wasm-release`. This collapses them to a coarse OS/target label for
#' grouping and filtering.
#'
#' @param config Character vector of `_jobs[].config` values.
#' @return Character vector, same length as `config`.
#' @keywords internal
.runiverse_config_os <- function(config) {
  dplyr::case_when(
    startsWith(config, "linux")   ~ "linux",
    startsWith(config, "macos")   ~ "macos",
    startsWith(config, "windows") ~ "windows",
    startsWith(config, "wasm")    ~ "wasm",
    config == "source"            ~ "source",
    config == "pkgdown"           ~ "pkgdown",
    TRUE                          ~ NA_character_
  )
}

#' Convert one parsed R-Universe package list into a one-row tibble
#'
#' @param pkg A parsed package list (one element of `/api/packages`).
#' @return A one-row tibble matching [runiverse_packages()]'s schema.
#' @keywords internal
.runiverse_package_row <- function(pkg) {
  downloads <- purrr::pluck(pkg, "_downloads", "count", .default = NA_integer_)
  upstream  <- purrr::pluck(pkg, "_upstream", .default = NA_character_)
  # `_upstream` is a bare URL string on GitHub-mirrored packages; on some
  # universes it may instead be a list with a `url` element — handle both.
  if (is.list(upstream)) {
    upstream <- purrr::pluck(upstream, "url", .default = NA_character_)
  }

  tibble::tibble(
    package           = .runiverse_pluck(pkg, "Package"),
    version            = .runiverse_pluck(pkg, "Version"),
    title              = .runiverse_pluck(pkg, "Title"),
    maintainer         = .runiverse_pluck(pkg, "_maintainer", "name"),
    maintainer_email   = .runiverse_pluck(pkg, "_maintainer", "email"),
    owner              = .runiverse_pluck(pkg, "_owner"),
    status             = .runiverse_pluck(pkg, "_status"),
    registered         = as.logical(.runiverse_pluck(pkg, "_registered", default = NA)),
    published          = .runiverse_as_time(.runiverse_pluck(pkg, "_published", default = NA)),
    stars              = as.integer(.runiverse_pluck(pkg, "_stars", default = NA_integer_)),
    downloads          = as.integer(downloads %||% NA_integer_),
    usedby             = as.integer(.runiverse_pluck(pkg, "_usedby", default = NA_integer_)),
    devurl             = .runiverse_pluck(pkg, "_devurl"),
    pkgdown            = .runiverse_pluck(pkg, "_pkgdown"),
    upstream           = as.character(upstream %||% NA_character_),
    jobs               = list(purrr::pluck(pkg, "_jobs", .default = list()))
  )
}

#' `%||%` — null-coalescing operator (local copy, no rlang import needed)
#' @keywords internal
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Parse an R-Universe ISO-8601 timestamp field
#'
#' `_published`/similar timestamp fields are ISO-8601 strings with a `Z`
#' suffix (e.g. `"2026-07-10T13:04:37.291Z"`); `_registered` is a boolean,
#' not a timestamp, and must NOT be passed here.
#'
#' @param x Character scalar or `NA`.
#' @return A `POSIXct` scalar (UTC), or `NA` if `x` is `NA`/unparseable.
#' @keywords internal
.runiverse_as_time <- function(x) {
  if (is.null(x) || (length(x) == 1L && is.na(x))) {
    return(as.POSIXct(NA_real_, tz = "UTC"))
  }
  as.POSIXct(as.character(x), format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
}

#' Empty [runiverse_packages()] tibble with the correct schema
#' @keywords internal
.empty_runiverse_packages_tibble <- function() {
  tibble::tibble(
    package           = character(),
    version            = character(),
    title              = character(),
    maintainer         = character(),
    maintainer_email   = character(),
    owner              = character(),
    status             = character(),
    registered         = logical(),
    published          = as.POSIXct(character(), tz = "UTC"),
    stars              = integer(),
    downloads          = integer(),
    usedby             = integer(),
    devurl             = character(),
    pkgdown            = character(),
    upstream           = character(),
    jobs               = list()
  )
}

#' Empty [runiverse_checks()] tibble with the correct schema
#' @keywords internal
.empty_runiverse_checks_tibble <- function() {
  tibble::tibble(
    package     = character(),
    version     = character(),
    config      = character(),
    os          = character(),
    r_version   = character(),
    check       = factor(levels = c("OK", "NOTE", "WARNING", "ERROR", "FAIL")),
    build_time  = integer(),
    artifact    = character(),
    status      = character()
  )
}

# ---------------------------------------------------------------------------
# Exported functions
# ---------------------------------------------------------------------------

#' List packages published on an R-Universe
#'
#' Queries the R-Universe API (`GET /api/packages`, or `GET
#' /api/packages/{package}` when `package` is given) and returns one row
#' per package summarising build status, ownership, and popularity
#' metadata. Responses are cached on disk via [httr2::req_cache()]
#' (`tools::R_user_dir("runiverse", "cache")`), matching the API's own
#' `Cache-Control: max-age=60` header — repeated calls within a short
#' window reuse the cached response instead of re-hitting the CDN
#' (CI-minute conservation, see llm#761).
#'
#' @param universe Character scalar — the R-Universe subdomain, e.g.
#'   `"johngavin"` for `https://johngavin.r-universe.dev`.
#' @param package Optional character scalar. When given, fetches only that
#'   package via `/api/packages/{package}` instead of the whole universe.
#' @return A tibble with one row per package:
#'   `package`, `version`, `title`, `maintainer`, `maintainer_email`,
#'   `owner`, `status`, `registered`, `published`, `stars`, `downloads`,
#'   `usedby`, `devurl`, `pkgdown`, `upstream`, and a list-column `jobs`
#'   holding each package's raw `_jobs` build-matrix entries — see
#'   [runiverse_checks()] to unnest them into one row per package x config.
#' @examples
#' \dontrun{
#' runiverse_packages("johngavin")
#' runiverse_packages("johngavin", package = "irishbuoys")
#' }
#' @export
runiverse_packages <- function(universe, package = NULL) {
  checkmate::assert_string(universe, min.chars = 1L)
  checkmate::assert_string(package, min.chars = 1L, null.ok = TRUE)

  url <- if (is.null(package)) {
    sprintf("https://%s.r-universe.dev/api/packages", universe)
  } else {
    sprintf("https://%s.r-universe.dev/api/packages/%s", universe, package)
  }

  body <- .runiverse_fetch_json(url)

  # A single-package request returns one JSON object (a named list keyed by
  # field names); a universe-wide request returns a JSON array (an unnamed
  # list of package objects). Normalise to a list-of-packages either way.
  pkgs <- if (!is.null(package)) list(body) else body

  if (length(pkgs) == 0L) {
    return(.empty_runiverse_packages_tibble())
  }

  dplyr::bind_rows(purrr::map(pkgs, .runiverse_package_row))
}

#' List per-config build checks for R-Universe packages
#'
#' Calls [runiverse_packages()] and unnests each package's `jobs` list-column
#' (the raw `_jobs` build matrix) into one row per package x build config,
#' with `check` as an ordered factor (`OK < NOTE < WARNING < ERROR < FAIL`)
#' so the worst result for a package sorts to the top of a descending sort.
#'
#' @param universe Character scalar — the R-Universe subdomain.
#' @param package Optional character scalar — restrict to a single package
#'   (passed through to [runiverse_packages()]).
#' @return A tibble with one row per package x build config: `package`,
#'   `version`, `config`, `os` (derived from `config`), `r_version`,
#'   `check` (ordered factor), `build_time` (seconds), `artifact`, `status`
#'   (the package-level `_status`).
#' @examples
#' \dontrun{
#' runiverse_checks("johngavin")
#' runiverse_checks("johngavin", package = "irishbuoys")
#' }
#' @export
runiverse_checks <- function(universe, package = NULL) {
  checkmate::assert_string(universe, min.chars = 1L)
  checkmate::assert_string(package, min.chars = 1L, null.ok = TRUE)

  pkgs_df <- runiverse_packages(universe, package = package)

  if (nrow(pkgs_df) == 0L) {
    return(.empty_runiverse_checks_tibble())
  }

  check_levels <- c("OK", "NOTE", "WARNING", "ERROR", "FAIL")

  rows <- purrr::pmap(
    list(
      package = pkgs_df$package,
      version = pkgs_df$version,
      status  = pkgs_df$status,
      jobs    = pkgs_df$jobs
    ),
    function(package, version, status, jobs) {
      if (length(jobs) == 0L) {
        return(NULL)
      }
      job_rows <- purrr::map(jobs, function(job) {
        tibble::tibble(
          config     = .runiverse_pluck(job, "config"),
          r_version  = .runiverse_pluck(job, "r"),
          check      = .runiverse_pluck(job, "check"),
          build_time = as.integer(.runiverse_pluck(job, "time", default = NA_integer_)),
          artifact   = .runiverse_pluck(job, "artifact")
        )
      })
      dplyr::bind_rows(job_rows) |>
        dplyr::mutate(package = package, version = version, status = status)
    }
  )

  out <- dplyr::bind_rows(purrr::keep(rows, Negate(is.null)))

  if (nrow(out) == 0L) {
    return(.empty_runiverse_checks_tibble())
  }

  out |>
    dplyr::mutate(
      os    = .runiverse_config_os(config),
      check = factor(
        dplyr::if_else(check %in% check_levels, check, NA_character_),
        levels = check_levels,
        ordered = TRUE
      )
    ) |>
    dplyr::select(
      package, version, config, os, r_version, check, build_time, artifact, status
    )
}
