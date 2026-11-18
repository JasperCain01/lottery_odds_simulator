# ==============================================================================
# helper-data.R
# ------------------------------------------------------------------------------
# testthat sources helper-*.R before any test file. This one gives every test
# that needs the real data the same freshly-prepared copy, built once per run
# from the committed raw CSVs by prepare_data(), with `today` pinned so on_sale
# is reproducible.
#
# WHY not read data/*.rds: that cache is git-ignored and only rebuilt when it is
# missing, so tests reading it could run against data left over from an older
# data_prep.R, and a single file run on a fresh checkout had nothing to read.
# ==============================================================================

TEST_TODAY <- as.Date("2026-07-01")

test_repo_root <- function() {
  cands <- c(".", "..", "../..", "../../..")
  hit <- cands[file.exists(file.path(cands, "national_lottery_scratchcard_odds.csv"))][1]
  if (is.na(hit)) stop("Could not locate the repo root (raw CSVs not found).")
  normalizePath(hit)
}

.test_data_cache <- new.env(parent = emptyenv())

# list(outcomes, game_summary, tiers_raw, diag, ...) as returned by prepare_data().
# suppressWarnings: a clean snapshot can still warn about unmatched instant-win
# slugs; test-data-prep.R asserts on those diagnostics explicitly.
test_data <- function() {
  if (is.null(.test_data_cache$prep)) {
    root <- test_repo_root()
    if (!exists("prepare_data", mode = "function")) {
      source(file.path(root, "R", "data_prep.R"))
    }
    .test_data_cache$prep <- suppressWarnings(prepare_data(root = root, today = TEST_TODAY))
  }
  .test_data_cache$prep
}
