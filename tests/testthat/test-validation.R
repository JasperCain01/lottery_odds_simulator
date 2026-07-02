# ==============================================================================
# test-validation.R  (Phase 10)
# ------------------------------------------------------------------------------
# Statistical property tests swept across EVERY REAL game in the prepared
# outcomes table (not just the hand fixtures in test-simulate.R). This is where
# the engine's correctness is proven against the actual data it will run on.
#
# For each of the ~123 games we assert five properties:
#   1. probabilities sum to 1 (fp tolerance)                    -- data invariant
#   2. net_value consistency: the losing row is EXACTLY -price, all values finite
#   3. simulated mean(totals) ~ analytical N * netEV within k * MC-SE   [HEADLINE]
#   4. empirical cumulative percentiles are monotonically ordered
#   5. (a couple of games) CRN reproducibility: same seed/crn -> identical totals
#
# THE HEADLINE PROPERTY (3): the mean of R iid session totals has standard error
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

if (!exists(".phase10_root", inherits = TRUE)) {
  .phase10_root <- local({
    cands <- c(".", "..", "../..", "../../..")
    hit <- cands[file.exists(file.path(cands, "data", "outcomes.rds"))][1]
    if (is.na(hit)) stop("Could not locate data/outcomes.rds; build the cache first.")
    hit
  })
}

.val_data <- local({
  root <- .phase10_root
  list(
    outcomes = readRDS(file.path(root, "data", "outcomes.rds")),
    gs       = readRDS(file.path(root, "data", "game_summary.rds"))
  )
})

# Sweep controls (documented above).
.SWEEP_N    <- 300L
.SWEEP_R    <- 1500L
.SWEEP_SEED <- 20260702L
.SWEEP_K    <- 5           # MC-SE band multiplier


# ------------------------------------------------------------------------------
test_that("every game's outcome distribution is well-formed (sums to 1, consistent net_value)", {
  outcomes <- .val_data$outcomes
  gs       <- .val_data$gs
  price    <- setNames(gs$price, gs$game_id)
  gids     <- unique(outcomes$game_id)

  for (gid in gids) {
    r <- outcomes[outcomes$game_id == gid, ]
    # (1) probabilities sum to 1.
    expect_equal(sum(r$probability), 1, tolerance = 1e-9, info = gid)
    expect_true(all(r$probability >= 0), info = gid)
    # (2) net_value finite; the losing row is exactly -price and is the minimum.
    expect_true(all(is.finite(r$net_value)), info = gid)
    p <- price[[gid]]
    expect_true(p > 0, info = gid)
    expect_equal(min(r$net_value), -p, tolerance = 1e-9, info = gid)
    # -price must actually be present as an outcome (the implicit loss row).
    expect_true(any(abs(r$net_value + p) < 1e-9), info = gid)
  }
})


# ------------------------------------------------------------------------------
test_that("simulated mean(totals) tracks analytical N*netEV within k*MC-SE across all games", {
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
test_that("empirical cumulative percentiles are monotonically ordered for every game", {
  outcomes <- .val_data$outcomes
  gids     <- unique(outcomes$game_id)

  for (gid in gids) {
    d   <- game_distribution(gid, outcomes)
    sim <- simulate_sessions_cached(d, N = .SWEEP_N, R = .SWEEP_R,
                                    seed = .SWEEP_SEED)
    # The fan-chart quantiles are p5<p25<p50<p75<p95 -- ordered along `probs`
    # at every checkpoint. Check the final checkpoint (full session) and a
    # midpoint; a broken quantile pipeline would unsort these.
    q_last <- sim$checkpoint_quantiles[nrow(sim$checkpoint_quantiles), ]
    expect_false(is.unsorted(q_last), info = paste(gid, "final"))
    mid <- max(1L, floor(nrow(sim$checkpoint_quantiles) / 2))
    expect_false(is.unsorted(sim$checkpoint_quantiles[mid, ]),
                 info = paste(gid, "mid"))
    # Direct empirical percentiles of the session totals are ordered too.
    qs <- stats::quantile(sim$totals, c(.05, .25, .5, .75, .95), names = FALSE)
    expect_false(is.unsorted(qs), info = paste(gid, "totals"))
  }
})


# ------------------------------------------------------------------------------
test_that("CRN gives byte-identical totals on repeat runs (representative + extreme games)", {
  outcomes <- .val_data$outcomes
  gs       <- .val_data$gs

  # A couple of games spanning the spectrum: the biggest-jackpot game (heaviest
  # skew) and the cheapest game, plus a mid game.
  big_jack <- {
    mx <- tapply(outcomes$net_value, outcomes$game_id, max)
    names(mx)[which.max(mx)]
  }
  cheapest <- gs$game_id[which.min(gs$price)]
  probe <- unique(c(big_jack, cheapest, gs$game_id[1]))

  for (gid in probe) {
    d  <- game_distribution(gid, outcomes)
    s1 <- simulate_sessions(d, N = 200, R = 500, crn = 99)
    s2 <- simulate_sessions(d, N = 200, R = 500, crn = 99)
    expect_identical(s1$totals, s2$totals, info = gid)
    # Seeded (non-CRN) path is likewise reproducible.
    s3 <- simulate_sessions(d, N = 200, R = 500, seed = 7)
    s4 <- simulate_sessions(d, N = 200, R = 500, seed = 7)
    expect_identical(s3$totals, s4$totals, info = gid)
  }
})
