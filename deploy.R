# ==============================================================================
# deploy.R
# ------------------------------------------------------------------------------
# NOT run automatically by anything in this repository (no CI/deploy hook
# calls this file). It is a ready-to-uncomment starting point for deploying
# the app to shinyapps.io or Posit Connect via the `rsconnect` package, kept
# here rather than only in README.md so the exact file manifest is versioned
# alongside the app it deploys. See README.md's "Deployment" section for the
# one-time account-authorization step (rsconnect::setAccountInfo() /
# connectApiUser() / connectUser()) -- no credentials are configured here or
# anywhere else in this repository.
#
# Usage (once authorized): uncomment the block below and
#   Rscript deploy.R
# ==============================================================================

# --- 1. Make sure the data cache exists BEFORE deploying --------------------
# data/*.rds is git-ignored and rebuilt on demand (see R/app_helpers.R's
# load_app_data()), but rsconnect::deployApp() bundles whatever is on disk --
# not what .gitignore excludes -- so build it explicitly first, or the
# deployed app will pay the data_prep cost cold on its very first visitor.
#
# source("R/data_prep.R")
# run_data_prep(".")

# --- 2. Deploy ----------------------------------------------------------------
# if (!requireNamespace("rsconnect", quietly = TRUE)) install.packages("rsconnect")
#
# rsconnect::deployApp(
#   appDir    = ".",
#   appName   = "lottery-odds-simulator",
#   appFiles  = c(
#     "app.R", "install.R",
#     list.files("R", pattern = "\\.R$", full.names = TRUE),
#     list.files("data", pattern = "\\.rds$", full.names = TRUE)
#   ),
#   forceUpdate = TRUE
# )

# --- Posit Connect alternative ------------------------------------------------
# Same call, once the account is registered via rsconnect::connectApiUser()
# (API key) or rsconnect::connectUser() (interactive) instead of
# setAccountInfo() -- rsconnect::deployApp() itself is unchanged; the target
# account determines whether the bundle lands on shinyapps.io or Connect.
