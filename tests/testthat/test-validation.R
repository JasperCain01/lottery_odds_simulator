# ==============================================================================
# test-validation.R  (Phase 10)
# ------------------------------------------------------------------------------
# Statistical property tests swept across EVERY REAL game in the prepared
# outcomes table (not just the hand fixtures in test-simulate.R). This is where
# the engine's correctness is proven against the actual data it will run on.
#
# For each of the ~123 games we assert two properties:
#   1. simulated mean(totals) ~ analytical N * netEV within k * MC-SE   [HEADLINE]
#   2. empirical cumulative percentiles are monotonically ordered
# The well-formedness of each game's distribution is checked in
# test-data-prep.R, and RNG/CRN reproducibility in test-simulate.R.
#
# THE HEADLINE PROPERTY (1): the mean of R iid session totals has standard error
#   SE = sd_total / sqrt(R),  sd_total = sqrt(N) * sd_play   (iid plays).
# We use the EXACT per-play SD the engine reports (sim$sd_play), so SE is the
# true sampling error, not a noisy estimate of it. A fixed seed makes the whole
# sweep deterministic; k = 5 clears the tightest observed margin (~2.7 SE on this
# data) with ~2x headroom, so the test is tight but non-flaky. Under the CLT a
# genuine 5-SE excursion has probability ~6e-7 per game.
#
# COST: N = 300, R = 1500 over all games runs in a few seconds (one seeded
# simulate per game via the cached wrapper). This is the deliberate
# runtime/precision trade-off: large enough R that the 5-SE band is meaningful,
# small enough that the full real-game sweep stays in the seconds range.
# ==============================================================================

# --- load the engine layers (mirror the guards in the other test files) -------
if (!exists("simulate_sessions_cached", mode = "function")) {
  .src <- function(rel) {
    cands <- c(rel, file.path("..", "..", rel), file.path("..", rel))
    hit <- cands[file.exists(cands)][1]
    if (is.na(hit)) stop(sprintf("Could not locate %s.", rel))
    source(hit)
  }
  .src("R/simulate.R")
  .src("R/app_helpers.R")
}

.val_data <- local({
  prep <- test_data()
  list(outcomes = prep$outcomes, gs = prep$game_summary)
})

# Sweep controls (documented above).
.SWEEP_N    <- 300L
.SWEEP_R    <- 1500L
.SWEEP_SEED <- 20260702L
.SWEEP_K    <- 5           # MC-SE band multiplier


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
test_that("every real game simulates without bias, with ordered quantiles", {
  outcomes <- .val_data$outcomes
  gids     <- unique(outcomes$game_id)

  worst_z <- 0; worst_g <- NA_character_
  t0 <- Sys.time()
  for (gid in gids) {
    d   <- game_distribution(gid, outcomes)
    sim <- simulate_sessions_cached(d, N = .SWEEP_N, R = .SWEEP_R,
                                    seed = .SWEEP_SEED)
    # Analytical reference mean and EXACT session SE.
    a       <- analytical_summary(d, N = .SWEEP_N)
    se      <- sqrt(.SWEEP_N) * sim$sd_play / sqrt(.SWEEP_R)
    mean_ok <- mean(sim$totals)

    # The fan-chart quantiles (p5 < p25 < ... < p95) must be ordered at the
    # final checkpoint and a midpoint, as must the quantiles of the totals.
    cq  <- sim$checkpoint_quantiles
    mid <- max(1L, floor(nrow(cq) / 2))
    expect_false(is.unsorted(cq[nrow(cq), ]), info = paste(gid, "final"))
    expect_false(is.unsorted(cq[mid, ]), info = paste(gid, "mid"))
    expect_false(is.unsorted(stats::quantile(sim$totals, c(.05, .25, .5, .75, .95),
                                             names = FALSE)), info = paste(gid, "totals"))

    if (se > 0) {
      z <- abs(mean_ok - a$mean) / se
      expect_lt(z, .SWEEP_K, label = sprintf("|mean-N*EV|/SE for %s", gid))
      if (z > worst_z) { worst_z <- z; worst_g <- gid }
    } else {
      # Degenerate (zero-variance) game: the mean must be exact.
      expect_equal(mean_ok, a$mean, tolerance = 1e-9, info = gid)
    }
  }
  el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  message(sprintf(
    "[validation] mean test swept %d games (N=%d,R=%d) in %.1fs; tightest margin %.2f of k=%d SE (%s)",
    length(gids), .SWEEP_N, .SWEEP_R, el, worst_z, .SWEEP_K, worst_g))
  expect_lt(worst_z, .SWEEP_K)
})


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
