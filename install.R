# ==============================================================================
# install.R
# ------------------------------------------------------------------------------
# Lightweight dependency installer for the Lottery Odds Simulator.
#
# WHY not renv: renv is deferred to the deployment phase (Phase 11) rather
# than adopted now. Trade-off: we give up renv's fully reproducible lockfile
# for the moment, in exchange for avoiding (a) disruption to the existing
# global R library already set up in this environment, and (b) network
# calls / renv bootstrap friction during early, fast-moving scaffolding work.
# This is a deliberate, temporary simplification -- revisit at deployment.
#
# Usage: Rscript install.R
# ==============================================================================

# Packages required across all layers of the app (see IMPLEMENTATION_PLAN.md
# section 4 and Phase 0). Kept as a flat vector so adding a dependency later
# is a one-line change.
required_packages <- c(
  "shiny",     # app framework
  "bslib",     # Bootstrap 5 theming/layout for Shiny
  "dplyr",     # data manipulation (data_prep/metrics/strategies)
  "readr",     # fast CSV ingestion (data_prep)
  "tidyr",     # tidy reshaping (data_prep)
  "stringr",   # title/game-number parsing (data_prep)
  "ggplot2",   # visualisations (viz)
  "scales",    # axis/label formatting for currency, percentages (viz)
  "matrixStats", # fast columnwise reductions in the engine hot path (Phase 9)
  "cachem",    # bounded LRU cache backend for repeat-run caching (Phase 9)
  "digest",    # content hashing for correct cache keys (Phase 9)
  "testthat",  # test suite
  "withr"      # scoped state management, used in tests and CRN/seed handling
)

# Install only what's missing -- avoids re-downloading/re-installing packages
# that are already present, which matters both for speed and because this
# environment may have limited/no network access.
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  } else {
    message(sprintf("'%s' already installed -- skipping.", pkg))
  }
}
