#!/usr/bin/env Rscript
# roborev_loop_ack.R — CLI tool to acknowledge a loop entry in the roborev
# blocklist.  Suppresses further blocking for the given content_hash.
#
# Usage:
#   roborev_loop_ack.R --hash <hash> --reason "<text>" [--until YYYY-MM-DD]
#
# Arguments:
#   --hash    (required) content_hash from `read_blocklist()` output
#   --reason  (required) human-readable reason for acknowledgement
#   --until   (optional) date or datetime string; if omitted, acks indefinitely

suppressPackageStartupMessages({
  if (!requireNamespace("optparse", quietly = TRUE)) {
    stop("optparse is required: install.packages('optparse')")
  }
})

option_list <- list(
  optparse::make_option(
    c("--hash"),
    type    = "character",
    default = NULL,
    help    = "content_hash to acknowledge [required]",
    metavar = "HASH"
  ),
  optparse::make_option(
    c("--reason"),
    type    = "character",
    default = NULL,
    help    = "Reason for acknowledgement [required]",
    metavar = "TEXT"
  ),
  optparse::make_option(
    c("--until"),
    type    = "character",
    default = NULL,
    help    = "Suppress until this date/datetime (e.g. 2026-06-01) [optional]",
    metavar = "DATE"
  )
)

opt_parser <- optparse::OptionParser(
  option_list  = option_list,
  description  = paste(
    "Acknowledge a roborev loop entry to suppress blocking.",
    "The entry's ack_by/ack_at/ack_reason/ack_until fields are updated in",
    "~/.claude/state/roborev_blocklist.json."
  )
)

opt <- optparse::parse_args(opt_parser)

# --- validate required arguments ----------------------------------------------
if (is.null(opt$hash) || !nzchar(opt$hash)) {
  optparse::print_help(opt_parser)
  stop("--hash is required", call. = FALSE)
}
if (is.null(opt$reason) || !nzchar(opt$reason)) {
  optparse::print_help(opt_parser)
  stop("--reason is required", call. = FALSE)
}

until_val <- NULL
if (!is.null(opt$until) && nzchar(opt$until)) {
  until_val <- tryCatch(
    as.POSIXct(opt$until, tz = "UTC"),
    error = function(e) {
      stop(paste("--until value not parseable as date:", opt$until), call. = FALSE)
    }
  )
  if (is.na(until_val)) {
    stop(paste("--until value not parseable as date:", opt$until), call. = FALSE)
  }
}

# --- load package (dev or installed) -----------------------------------------
pkg_root <- Sys.getenv("PKG_ROOT", unset = "")
if (nzchar(pkg_root)) {
  pkgload::load_all(pkg_root, quiet = TRUE)
} else if (requireNamespace("llmtelemetry", quietly = TRUE)) {
  library(llmtelemetry)
} else {
  here_root <- tryCatch(here::here(), error = function(e) NULL)
  if (!is.null(here_root) && file.exists(file.path(here_root, "DESCRIPTION"))) {
    pkgload::load_all(here_root, quiet = TRUE)
  } else {
    stop("Cannot find llmtelemetry package. Set PKG_ROOT env var.", call. = FALSE)
  }
}

# --- call ack_loop ------------------------------------------------------------
ack_loop(
  content_hash = opt$hash,
  reason       = opt$reason,
  until        = until_val,
  by           = Sys.info()[["user"]]
)
