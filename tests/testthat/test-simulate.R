# ==============================================================================
# test-simulate.R
# ------------------------------------------------------------------------------
# Correctness + statistical property tests for Layer 2 (R/simulate.R).
#
# The property tests are where correctness is actually proven: simulated moments
# must track the closed-form analytical moments within a PRINCIPLED Monte Carlo
# tolerance (derived from the sampling error, not hand-tuned); the RNG paths must
# be reproducible; CRN must give identical draws to the seeded path on the same
# distribution and an EXACT shift under a value shift; quantiles must be ordered;
# and the last checkpoint must equal the session total. Edge cases (N=1, R=1,
# degenerate single-value dist) must not error.
# ==============================================================================

# Ensure the layer under test is loaded (mirrors test-data-prep.R's guard).
if (!exists("simulate_sessions", mode = "function")) {
  .cands <- c("R/simulate.R", "../../R/simulate.R", "../R/simulate.R")
  .hit   <- .cands[file.exists(.cands)][1]
  if (is.na(.hit)) stop("Could not locate R/simulate.R.")
  source(.hit)
}

# ------------------------------------------------------------------------------
# Test fixtures: simple, hand-computable distributions so expectations are exact.
# ------------------------------------------------------------------------------
# A fair-ish 3-outcome distribution with a modest tail. Low kurtosis keeps the
# sample-variance sampling error small, so the strict variance test is tight.
dist_simple <- data.frame(
  value = c(-1, 0, 2, 10),
  prob  = c(0.5, 0.3, 0.15, 0.05)
)
# A degenerate single-value distribution (always the same net outcome).
dist_degen <- data.frame(value = -5, prob = 1)


# ------------------------------------------------------------------------------
test_that("analytical_summary matches hand-computed moments", {
  N <- 100
  ev  <- sum(dist_simple$value * dist_simple$prob)
  ex2 <- sum(dist_simple$value^2 * dist_simple$prob)
  v   <- ex2 - ev^2

  a <- analytical_summary(dist_simple, N = N, conf = 0.90)
  expect_equal(a$mean, N * ev)
  expect_equal(a$sd,   sqrt(N * v))
  expect_equal(a$ev_play, ev)
  expect_equal(a$var_play, v)

  # CI is symmetric about the mean and matches the requested z.
  z <- qnorm(0.95)
  expect_equal(a$ci_lo, N * ev - z * sqrt(N * v))
  expect_equal(a$ci_hi, N * ev + z * sqrt(N * v))
})


# ------------------------------------------------------------------------------
test_that("simulated mean(totals) matches analytical mean within 4x MC SE", {
  # Principled tolerance: the mean of R session-totals has standard error
  # sd_total / sqrt(R). A 4x band is ~exceeded with probability < 1e-4 under the
  # CLT, so a fixed seed makes this a stable, non-flaky assertion that is still
  # tight enough to catch a real bias.
  N <- 200; R <- 20000
  a   <- analytical_summary(dist_simple, N = N)
  sim <- simulate_sessions(dist_simple, N = N, R = R, seed = 2024)

  mc_se <- a$sd / sqrt(R)
  expect_lt(abs(mean(sim$totals) - a$mean), 4 * mc_se)
})


# ------------------------------------------------------------------------------
test_that("var(totals) matches N * per-play var within sampling error", {
  # Standard error of a sample variance is approximately
  #   SE(s^2) ~= var * sqrt((kappa - 1 + 2/(R-1)) / R),
  # where kappa is the (excess-free) kurtosis E[(X-mu)^4]/sigma^4 of the SESSION
  # total. We compute it analytically for the simple dist (low kurtosis) and
  # allow 4 SEs -- principled, not hand-tuned.
  N <- 200; R <- 40000
  a  <- analytical_summary(dist_simple, N = N)
  cd_v <- dist_simple$value; cd_p <- dist_simple$prob
  mu   <- sum(cd_v * cd_p)
  m2   <- sum((cd_v - mu)^2 * cd_p)         # per-play central moments
  m4   <- sum((cd_v - mu)^4 * cd_p)
  # For a sum of N i.i.d. draws: Var = N*m2; 4th central moment = N*m4 + 3N(N-1)m2^2.
  var_tot <- N * m2
  mu4_tot <- N * m4 + 3 * N * (N - 1) * m2^2
  kappa   <- mu4_tot / var_tot^2            # kurtosis of the session total
  se_s2   <- var_tot * sqrt((kappa - 1 + 2 / (R - 1)) / R)

  sim <- simulate_sessions(dist_simple, N = N, R = R, seed = 55)
  expect_lt(abs(var(sim$totals) - var_tot), 4 * se_s2)
  # Cross-check the analytical var equals N*per-play var (the contract).
  expect_equal(a$sd^2, var_tot)
})


# ------------------------------------------------------------------------------
test_that("same seed => identical totals (default path reproducibility)", {
  s1 <- simulate_sessions(dist_simple, N = 300, R = 1000, seed = 123)
  s2 <- simulate_sessions(dist_simple, N = 300, R = 1000, seed = 123)
  expect_identical(s1$totals, s2$totals)
  expect_identical(s1$checkpoint_quantiles, s2$checkpoint_quantiles)
  # Different seed should (almost surely) differ.
  s3 <- simulate_sessions(dist_simple, N = 300, R = 1000, seed = 124)
  expect_false(identical(s1$totals, s3$totals))
})


# ------------------------------------------------------------------------------
test_that("chunking is invariant to batch size (same seed => same result)", {
  # Force a tiny chunk by mocking .chunk_size, then compare against the default.
  # This proves the sequential-stream design makes results independent of how
  # many batches the run is split into.
  big   <- simulate_sessions(dist_simple, N = 500, R = 2000, seed = 9)

  local({
    # Temporarily shadow .chunk_size in the sourced namespace with a tiny value.
    orig <- .chunk_size
    assign(".chunk_size", function(N) 37L, envir = environment(simulate_sessions))
    on.exit(assign(".chunk_size", orig, envir = environment(simulate_sessions)),
            add = TRUE)
    small <- simulate_sessions(dist_simple, N = 500, R = 2000, seed = 9)
    expect_identical(big$totals, small$totals)
    expect_identical(big$checkpoint_quantiles, small$checkpoint_quantiles)
  })
})


# ------------------------------------------------------------------------------
test_that("CRN scalar-seed path reproduces the same uniforms deterministically", {
  # Two runs with the SAME crn seed on the SAME dist must be byte-identical, and
  # must not depend on the global RNG state.
  set.seed(1)
  a <- simulate_sessions(dist_simple, N = 100, R = 500, crn = 777)
  set.seed(99999)                       # perturb global state
  b <- simulate_sessions(dist_simple, N = 100, R = 500, crn = 777)
  expect_identical(a$totals, b$totals)
  expect_true(a$crn_used)
})


# ------------------------------------------------------------------------------
test_that("CRN: matrix and session-contiguous vector agree; both reproduce", {
  # Contract: a crn MATRIX has row i = session i's uniforms; a crn VECTOR is
  # session-contiguous (first N -> session 1, etc.). A matrix filled byrow and
  # its flat byrow vector therefore describe the SAME layout and must match.
  N <- 80; R <- 400
  set.seed(2026)
  flat <- runif(N * R)                              # session-contiguous stream
  U    <- matrix(flat, nrow = R, ncol = N, byrow = TRUE)  # row i = session i

  m1 <- simulate_sessions(dist_simple, N = N, R = R, crn = U)     # matrix path
  m2 <- simulate_sessions(dist_simple, N = N, R = R, crn = flat)  # vector path
  expect_identical(m1$totals, m2$totals)
  # Feeding the same matrix twice reproduces exactly.
  m3 <- simulate_sessions(dist_simple, N = N, R = R, crn = U)
  expect_identical(m1$totals, m3$totals)
})


# ------------------------------------------------------------------------------
test_that("CRN monotone shift: +£1 on every value => totals shift by exactly N", {
  # THE key CRN correctness property. Same uniforms fed to a dist and to a copy
  # with every value shifted by +1 must yield totals shifted by exactly N*1,
  # because the canonical (sorted) ordering -- and hence the inverse-CDF map --
  # is unchanged by a constant shift.
  N <- 150; R <- 600
  shift <- 1
  dist_shift <- data.frame(value = dist_simple$value + shift, prob = dist_simple$prob)

  base <- simulate_sessions(dist_simple, N = N, R = R, crn = 4242)
  up   <- simulate_sessions(dist_shift,  N = N, R = R, crn = 4242)
  expect_equal(up$totals - base$totals, rep(N * shift, R))
})


# ------------------------------------------------------------------------------
test_that("simulate_one_session: CRN seed matches the row of a CRN session run", {
  # Feeding the same uniforms to simulate_one_session and to session 1 of a
  # simulate_sessions CRN run must produce the same session total.
  N <- 120
  set.seed(11)
  u1 <- runif(N)                        # one session's worth of uniforms
  one <- simulate_one_session(dist_simple, N = N, crn = u1)
  # Build an R=1 session run with the same uniforms.
  many <- simulate_sessions(dist_simple, N = N, R = 1, crn = matrix(u1, nrow = 1))
  expect_equal(sum(one), many$totals[1])
})


# ------------------------------------------------------------------------------
test_that("quantiles are ordered and checkpoints equal session totals", {
  s <- simulate_sessions(dist_simple, N = 400, R = 3000, seed = 321,
                         probs = c(.05, .25, .5, .75, .95))
  cq <- s$checkpoint_quantiles
  # Row-wise monotonicity of the quantile columns (p05 <= p25 <= ... <= p95).
  for (k in seq_len(nrow(cq))) {
    expect_false(is.unsorted(cq[k, ]))
  }
  # Final checkpoint index is exactly N.
  expect_equal(tail(s$checkpoint_plays, 1), s$N)
  # Reconstruct the final-checkpoint quantiles directly from totals: they must
  # match the last row of checkpoint_quantiles (last cp cumulative == total).
  final_q <- quantile(s$totals, probs = s$probs, names = FALSE, type = 7)
  expect_equal(as.numeric(cq[nrow(cq), ]), final_q)
})


# ------------------------------------------------------------------------------
test_that("checkpoint_plays are increasing, unique, start >=1 and end at N", {
  s <- simulate_sessions(dist_simple, N = 1000, R = 200, seed = 1, checkpoints = 100)
  cp <- s$checkpoint_plays
  expect_false(is.unsorted(cp, strictly = TRUE))
  expect_gte(cp[1], 1L)
  expect_equal(tail(cp, 1), 1000L)
  expect_lte(length(cp), 100L)
})


# ------------------------------------------------------------------------------
test_that("edge case N=1 works and equals a single per-play draw", {
  # With N=1, session total is a single draw; checkpoints collapse to one point.
  s <- simulate_sessions(dist_simple, N = 1, R = 5000, seed = 8)
  expect_length(s$totals, 5000)
  expect_equal(s$checkpoint_plays, 1L)
  expect_equal(nrow(s$checkpoint_quantiles), 1L)
  # Every total must be one of the distribution's support values.
  expect_true(all(s$totals %in% dist_simple$value))
  # Mean is close to the per-play EV within MC error.
  a <- analytical_summary(dist_simple, N = 1)
  expect_lt(abs(mean(s$totals) - a$mean), 4 * a$sd / sqrt(5000))
})


# ------------------------------------------------------------------------------
test_that("edge case R=1 works (single session, full-length totals vector 1)", {
  s <- simulate_sessions(dist_simple, N = 250, R = 1, seed = 3)
  expect_length(s$totals, 1)
  # Quantiles of a single session are all equal to that session's cumulative.
  expect_equal(unname(s$checkpoint_quantiles[nrow(s$checkpoint_quantiles), 1]),
               s$totals[1])
})


# ------------------------------------------------------------------------------
test_that("degenerate single-value distribution is exact and error-free", {
  # Every play is -5, so any N-play session total is exactly -5N with zero var.
  N <- 137; R <- 50
  s <- simulate_sessions(dist_degen, N = N, R = R, seed = 1)
  expect_true(all(s$totals == -5 * N))
  expect_equal(var(s$totals), 0)
  a <- analytical_summary(dist_degen, N = N)
  expect_equal(a$mean, -5 * N)
  expect_equal(a$sd, 0)
  # CRN path on a degenerate dist must also be exact.
  sc <- simulate_sessions(dist_degen, N = N, R = R, crn = 5)
  expect_true(all(sc$totals == -5 * N))
})


# ------------------------------------------------------------------------------
test_that("extreme jackpot tail (tiny prob) simulates without error", {
  # A distribution with a 1-in-a-million huge payout and a near-certain small
  # loss -- mirrors the real jackpot games. Must run and stay finite.
  dist_jackpot <- data.frame(
    value = c(-2, 1e6),
    prob  = c(1 - 1e-6, 1e-6)
  )
  s <- simulate_sessions(dist_jackpot, N = 500, R = 2000, seed = 4)
  expect_length(s$totals, 2000)
  expect_true(all(is.finite(s$totals)))
  # Mean should sit near analytical within MC error.
  a <- analytical_summary(dist_jackpot, N = 500)
  expect_lt(abs(mean(s$totals) - a$mean), 5 * a$sd / sqrt(2000))
})


# ------------------------------------------------------------------------------
test_that("defensive renormalisation warns and rescales; bad probs error", {
  # Probabilities that don't sum to 1 (beyond fp tol) should warn and renormalise.
  d_bad <- data.frame(value = c(0, 1), prob = c(0.4, 0.4))   # sums to 0.8
  expect_warning(a <- analytical_summary(d_bad, N = 10), "renormalising")
  # After renormalising to (0.5, 0.5), per-play EV = 0.5.
  expect_equal(a$ev_play, 0.5)

  # Negative probability is a hard error.
  expect_error(analytical_summary(data.frame(value = c(0, 1), prob = c(-0.1, 1.1)),
                                  N = 10), "negative")
  # All-zero probability is a hard error.
  expect_error(analytical_summary(data.frame(value = c(0, 1), prob = c(0, 0)),
                                  N = 10), "sum to zero")
})


# ------------------------------------------------------------------------------
test_that("game_distribution extracts one game's (value, prob) generically", {
  outcomes <- data.frame(
    game_id     = c("g1", "g1", "g2"),
    source      = c("s", "s", "s"),
    net_value   = c(-1, 5, -2),
    probability = c(0.9, 0.1, 1.0)
  )
  d <- game_distribution("g1", outcomes)
  expect_named(d, c("value", "prob"))
  expect_equal(d$value, c(-1, 5))
  expect_equal(d$prob, c(0.9, 0.1))
  # Unknown game errors.
  expect_error(game_distribution("nope", outcomes), "not found")
})
