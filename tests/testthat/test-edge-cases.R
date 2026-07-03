# ==============================================================================
# test-edge-cases.R  (Phase 10)
# ------------------------------------------------------------------------------
# Boundary / degenerate inputs the engine must handle without erroring and with
# sane output:
#   * N = 1  (a single play per session)
#   * R = 1  (a single session)
#   * a distribution containing a zero-probability tier (must never be drawn)
#   * an RTP-capped game (rtp_capped_applied == TRUE) simulated end to end
#   * the data extremes: biggest jackpot / worst RTP / cheapest / priciest
# These complement the full sweep in test-validation.R (which covers all games
# at fixed N,R) by hammering the corners of the parameter space.
# ==============================================================================

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

.edge_data <- local({
  root <- .phase10_root
  list(
    outcomes = readRDS(file.path(root, "data", "outcomes.rds")),
    gs       = readRDS(file.path(root, "data", "game_summary.rds"))
  )
})


# ------------------------------------------------------------------------------
test_that("N = 1 produces one play per session with sane summaries", {
  outcomes <- .edge_data$outcomes
  gid <- .edge_data$gs$game_id[1]
  d   <- game_distribution(gid, outcomes)

  sim <- simulate_sessions(d, N = 1, R = 200, seed = 1)
  expect_length(sim$totals, 200L)
  expect_true(all(is.finite(sim$totals)))
  # With N = 1 every session total is a single draw, hence one of the support
  # net_values exactly.
  expect_true(all(sim$totals %in% d$value))
  # Final checkpoint equals the total (the engine's own contract) and quantiles
  # are ordered.
  expect_equal(sim$checkpoint_mean[length(sim$checkpoint_mean)], mean(sim$totals),
               tolerance = 1e-9)
  a <- analytical_summary(d, N = 1)
  expect_equal(a$mean, sum(d$value * d$prob))
})


# ------------------------------------------------------------------------------
test_that("R = 1 produces a single well-formed session", {
  outcomes <- .edge_data$outcomes
  gid <- .edge_data$gs$game_id[1]
  d   <- game_distribution(gid, outcomes)

  sim <- simulate_sessions(d, N = 250, R = 1, seed = 3)
  expect_length(sim$totals, 1L)
  expect_true(is.finite(sim$totals))
  # One session: each "quantile" collapses to that session's cumulative value,
  # so the last-checkpoint quantiles all equal the single total.
  q_last <- sim$checkpoint_quantiles[nrow(sim$checkpoint_quantiles), ]
  expect_true(all(abs(q_last - sim$totals) < 1e-9))
})


# ------------------------------------------------------------------------------
test_that("a zero-probability tier is carried but never drawn", {
  outcomes <- .edge_data$outcomes
  gid <- .edge_data$gs$game_id[1]
  d   <- game_distribution(gid, outcomes)

  # Inject an impossible jackpot at probability 0. sum(prob) stays 1, so the
  # engine must NOT warn about renormalisation and must never select it.
  huge <- 1e6
  dz <- rbind(d, data.frame(value = huge, prob = 0))
  expect_silent(sim <- simulate_sessions(dz, N = 300, R = 1000, seed = 5))
  expect_false(any(sim$totals >= huge))            # the 0-prob value never fires
  expect_true(all(is.finite(sim$totals)))

  # The zero-prob outcome does not shift the mean vs the original distribution.
  a0 <- analytical_summary(d,  N = 300)
  az <- analytical_summary(dz, N = 300)
  expect_equal(a0$mean, az$mean, tolerance = 1e-9)
})


# ------------------------------------------------------------------------------
test_that("an RTP-capped game simulates end to end without error", {
  outcomes <- .edge_data$outcomes
  gs       <- .edge_data$gs

  capped <- gs$game_id[gs$rtp_capped_applied]
  skip_if(length(capped) == 0, "no rtp_capped_applied games in this snapshot")
  gid <- capped[1]
  d   <- game_distribution(gid, outcomes)

  # rtp_capped is an advisory column only -- it must not affect the outcomes
  # distribution, which should simulate exactly like any other game.
  sim <- simulate_sessions(d, N = 300, R = 1000, seed = 11)
  expect_length(sim$totals, 1000L)
  expect_true(all(is.finite(sim$totals)))
  expect_false(is.unsorted(sim$checkpoint_quantiles[nrow(sim$checkpoint_quantiles), ]))
  # Mean tracks the analytical mean within a generous MC band.
  a  <- analytical_summary(d, N = 300)
  se <- sqrt(300) * sim$sd_play / sqrt(1000)
  expect_lt(abs(mean(sim$totals) - a$mean), 5 * se)
})


# ------------------------------------------------------------------------------
test_that("the data extremes each simulate to sane output", {
  outcomes <- .edge_data$outcomes
  gs       <- .edge_data$gs

  mx <- tapply(outcomes$net_value, outcomes$game_id, max)
  extremes <- unique(c(
    biggest_jackpot = names(mx)[which.max(mx)],
    worst_rtp       = gs$game_id[which.min(gs$rtp)],
    cheapest        = gs$game_id[which.min(gs$price)],
    priciest        = gs$game_id[which.max(gs$price)]
  ))

  for (gid in extremes) {
    d   <- game_distribution(gid, outcomes)
    sim <- simulate_sessions(d, N = 200, R = 500, seed = 13)
    expect_equal(length(sim$totals), 500L, info = gid)
    expect_true(all(is.finite(sim$totals)), info = gid)
    expect_false(is.unsorted(sim$checkpoint_quantiles[nrow(sim$checkpoint_quantiles), ]),
                 info = gid)
  }
})
