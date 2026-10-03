# Deploy this Shiny app to shinyapps.io
#
# Prerequisites:
# 1. Free or paid account at https://www.shinyapps.io
# 2. A Supabase project (https://supabase.com)
# 3. .Renviron in the project root containing your SUPABASE_DB_URL
# 4. Configured rsconnect credentials from shinyapps.io -> Account -> Tokens:
#      rsconnect::setAccountInfo(name = '<ACCOUNT>', token = '<TOKEN>', secret = '<SECRET>')
#
# Usage (from project root):
#   Rscript deploy/shinyapps.R
# or from within R/RStudio:
#   source("deploy/shinyapps.R")

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  message("Installing rsconnect package...")
  install.packages("rsconnect", repos = "https://cloud.r-project.org")
}

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(file_arg), ".."), winslash = "/")
} else {
  getwd()
}
setwd(root)

message("\n========================================================")
message("  Deploying Expert Elicitation Platform to shinyapps.io  ")
message("========================================================\n")

# 1. Verify .Renviron exists and contains SUPABASE_DB_URL
renviron_path <- file.path(root, ".Renviron")
if (!file.exists(renviron_path)) {
  if (file.exists(file.path(root, ".Renviron.example"))) {
    message("WARNING: .Renviron not found in the project root.")
    message("Creating .Renviron from .Renviron.example...")
    file.copy(file.path(root, ".Renviron.example"), renviron_path)
    message("--> Please open .Renviron and configure your SUPABASE_DB_URL, then re-run this script.")
    stop(".Renviron needs configuration before deploying to shinyapps.io.", call. = FALSE)
  } else {
    stop(".Renviron file is missing. shinyapps.io requires .Renviron with SUPABASE_DB_URL.", call. = FALSE)
  }
}

readRenviron(renviron_path)
supabase_url <- Sys.getenv("SUPABASE_DB_URL", "")
if (!nzchar(supabase_url)) supabase_url <- Sys.getenv("DATABASE_URL", "")

if (nzchar(supabase_url)) {
  message("Database Engine: Supabase PostgreSQL (detected via SUPABASE_DB_URL)")
} else {
  message("WARNING: SUPABASE_DB_URL was not found in .Renviron.")
  message("shinyapps.io runs in an isolated cloud container and requires an external database.")
  message("--> Please set SUPABASE_DB_URL to your Supabase connection pooler string in .Renviron.")
}

# 2. Check for configured shinyapps.io accounts
accounts <- tryCatch(rsconnect::accounts(), error = function(e) data.frame())
shinyapps_accounts <- if (nrow(accounts) > 0 && "server" %in% names(accounts)) {
  accounts[accounts$server == "shinyapps.io", , drop = FALSE]
} else {
  data.frame()
}

if (nrow(shinyapps_accounts) == 0) {
  stop(
    "\nNo shinyapps.io account is configured in rsconnect.\n",
    "Please link your shinyapps.io account first by running:\n\n",
    "  rsconnect::setAccountInfo(\n",
    "    name   = '<YOUR_SHINYAPPS_ACCOUNT_NAME>',\n",
    "    token  = '<YOUR_TOKEN>',\n",
    "    secret = '<YOUR_SECRET>'\n",
    "  )\n\n",
    "You can get your token & secret at: https://www.shinyapps.io/admin/#/tokens\n",
    call. = FALSE
  )
}

account_name <- Sys.getenv("SHINYAPPS_ACCOUNT", shinyapps_accounts$name[1])

# Auto-detect existing live application on shinyapps.io
live_apps <- tryCatch(rsconnect::applications(account = account_name), error = function(e) data.frame())
detected_name <- if (nrow(live_apps) > 0 && "name" %in% names(live_apps)) live_apps$name[1] else "amchss"
app_name <- Sys.getenv("SHINYAPPS_APP_NAME", detected_name)

message(sprintf("Target Account: %s", account_name))
message(sprintf("App Name:       %s", app_name))
message(sprintf("Directory:      %s", root))
message("\nStarting deployment...\n")

rsconnect::deployApp(
  appDir = root,
  appName = app_name,
  appTitle = "Expert Elicitation & Statistical Platform",
  account = account_name,
  server = "shinyapps.io",
  appMode = "shiny",
  launch.browser = FALSE,
  forceUpdate = TRUE,
  logLevel = "verbose"
)

public_url <- sprintf("https://%s.shinyapps.io/%s/", account_name, app_name)
message("\n========================================================")
message("  Deployment Complete!")
message(sprintf("  Live URL: %s", public_url))
message("========================================================")
message("\nReminder:")
message("If SURVEY_PUBLIC_URL in .Renviron is not yet updated, update it to:")
message(sprintf("  SURVEY_PUBLIC_URL=%s", public_url))
message("and re-run deploy/shinyapps.R to ensure survey invite links use the live domain.\n")
