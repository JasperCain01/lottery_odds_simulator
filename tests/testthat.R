# ==============================================================================
# testthat.R
# ------------------------------------------------------------------------------
# Test-suite bootstrap for tests/testthat/.
#
# WHY not test_check(): this project is a standalone Shiny app (no
# DESCRIPTION/package structure), so we source R/*.R directly rather than
# relying on testthat's package-oriented `test_check()`. Run the suite with:
#   Rscript -e 'testthat::test_dir("tests/testthat")'
#
# TODO (Phase 10): expand tests/testthat/ with unit + statistical property
# tests as each layer (data_prep/simulate/metrics/strategies) is implemented.
# ==============================================================================

library(testthat)

# Source every layer file so its (currently stub) definitions are available
# to the test suite. Trade-off: simple sourcing over a package build -- fine
# while the app has no external consumers and no compiled code.
r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_files, source))

test_dir("tests/testthat")
