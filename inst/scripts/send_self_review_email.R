#!/usr/bin/env Rscript
# Emails the overnight self-review PASS/FAIL result. Dispatched by the local
# verify job (JohnGavin/llm#235) via workflow_dispatch. Reuses repo GMAIL_* secrets.
# Mirrors the blastula send pattern in send_daily_email.R.
suppressPackageStartupMessages(library(blastula))

result   <- Sys.getenv("SR_RESULT", "UNKNOWN")
count    <- Sys.getenv("SR_COUNT", "?")
run_date <- Sys.getenv("SR_RUN_DATE", "")
details  <- Sys.getenv("SR_DETAILS", "")
if (!nzchar(run_date)) run_date <- format(Sys.Date())

gmail_user <- Sys.getenv("GMAIL_USERNAME")
gmail_pass <- Sys.getenv("GMAIL_APP_PASSWORD")

emoji   <- if (identical(result, "PASS")) "✅" else "⚠️"
subject <- sprintf("%s Overnight self-review: %s (%s)", emoji, result, run_date)

details_line <- if (nzchar(details)) paste0("> ", details) else ""

email_body <- paste0(
  "## ", emoji, " Overnight self-review verification — ", run_date, "\n\n",
  "**Result: ", result, "** &nbsp;|&nbsp; findings rows in `unified.duckdb`: **", count, "**\n\n",
  details_line, "\n\n",
  "### What this means\n",
  "- **PASS** — last night's 02:30 self-review `--write` ran cleanly: wrote findings,",
  " completed (`Done.`), no `duckdb: command not found`,",
  " and `self_review_findings_stage1` has rows.\n",
  "- **FAIL** — a regression. The overnight job did not produce findings as expected;",
  " investigate.\n\n",
  "### Where to look (on your machine)\n",
  "- Stage-1 run log: `~/.claude/logs/self_review_stage1.log`\n",
  "- Verify log (PASS/FAIL history): `~/.claude/logs/self_review_verify.log`\n",
  "- Tracking issue: https://github.com/JohnGavin/llm/issues/235\n\n",
  "_Sent by the self-review verify job (llm#235) via llmtelemetry CI._"
)

if (nzchar(gmail_user) && nzchar(gmail_pass)) {
  email <- compose_email(body = md(email_body))
  smtp_creds <- creds_envvar(
    user = gmail_user,
    pass_envvar = "GMAIL_APP_PASSWORD",
    host = "smtp.gmail.com",
    port = 465,
    use_ssl = TRUE
  )
  tryCatch({
    smtp_send(
      email = email, to = gmail_user, from = gmail_user,
      subject = subject, credentials = smtp_creds
    )
    message("Self-review email sent: ", subject)
  }, error = function(e) {
    message("ERROR: SMTP send failed: ", e$message)
    cat(email_body)
  })
} else {
  message("GMAIL creds not set; printing body:")
  cat(email_body)
}
