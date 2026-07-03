# ==============================================================================
# test-metrics.R
# ------------------------------------------------------------------------------
# Correctness + statistical property tests for Layer 3 (R/metrics.R).
#
# The risk metrics (VaR, expected shortfall), the dream-vs-reality conditioning,
# and the Monte Carlo quantile SE are the easy-to-flip / easy-to-fudge parts, so
# they are pinned against HAND-COMPUTABLE fixtures where possible. Where a metric
# is genuinely stochastic (p_profit from sim, longest streak, the MJ SE) the
# tolerances are derived from sampling theory (proportion SE sqrt(p(1-p)/R); the
# MJ estimator's own ~6% relative sampling error at the sample sizes used) with
# fixed seeds, NOT hand-tuned magic numbers.
# ==============================================================================

# Ensure the layers under test are loaded (mirrors test-simulate.R's guard).
# metrics.R depends on simulate.R (simulate_one_session), so load both.
if (!exists("simulate_sessions", mode = "function")) {
  .cands <- c("R/simulate.R", "../../R/simulate.R", "../R/simulate.R")
  .hit   <- .cands[file.exists(.cands)][1]
  if (is.na(.hit)) stop("Could not locate R/simulate.R.")
  source(.hit)
}
if (!exists("build_metrics", mode = "function")) {
  .cands <- c("R/metrics.R", "../../R/metrics.R", "../R/metrics.R")
  .hit   <- .cands[file.exists(.cands)][1]
  if (is.na(.hit)) stop("Could not locate R/metrics.R.")
  source(.hit)
}

# ------------------------------------------------------------------------------
# Fixtures: hand-computable distributions.
# ------------------------------------------------------------------------------
# £5 game. price = -min(value) = 5.
#   win     = value > -5           -> {0, 5, 45}: p_win = 0.40
#   ev_play = -3 + 0 + 0.75 + 2.25 = 0
dist_eng <- data.frame(
  value = c(-5, 0, 5, 45),
  prob  = c(0.60, 0.20, 0.15, 0.05)
)
# A degenerate all-loss dist (every play loses the £5 stake).
dist_allloss <- data.frame(value = -5, prob = 1)
# An all-win dist (no losing outcome); price must be supplied explicitly.
dist_allwin <- data.frame(value = c(1, 2), prob = c(0.5, 0.5))


# ==============================================================================
# Risk metrics
# ==============================================================================

test_that("risk_metrics probabilities match hand-counts", {
  totals <- c(-10, -5, -5, 0, 3, 3, 7, 20)   # R = 8
  rm <- risk_metrics(totals, alpha = 0.05,
                     loss_thresholds = c(4, 6))
  expect_equal(rm$p_profit,    mean(totals > 0))   # 4/8
  expect_equal(rm$p_breakeven, mean(totals >= 0))  # 5/8
  # loss > £4  => total < -4  => {-10, -5, -5} = 3/8
  expect_equal(unname(rm$p_loss_gt[1]), 3 / 8)
  # loss > £6  => total < -6  => {-10} = 1/8
  expect_equal(unname(rm$p_loss_gt[2]), 1 / 8)
  # A negative "loss magnitude" threshold is a caller error.
  expect_error(risk_metrics(totals, loss_thresholds = -1), "positive")
})


test_that("VaR is the signed alpha-quantile with a positive loss magnitude", {
  # n = 20, alpha*R = 1. type-7 5% quantile is hand-computable.
  totals <- c(-500, seq(-100, 100, length.out = 19))
  v <- value_at_risk(totals, 0.05)
  # h = (n-1)*p + 1 = 1.95 -> x1 + 0.95*(x2 - x1) = -500 + 0.95*400 = -120
  expect_equal(v$pnl, -120)
  expect_equal(v$loss, 120)          # positive magnitude = -pnl
  expect_equal(v$pnl, quantile(totals, 0.05, names = FALSE, type = 7))
})


test_that("expected_shortfall is the exact lower-alpha tail mean (Acerbi)", {
  # Case A: alpha*R = 1 exactly -> ES = the single worst P&L.
  tA <- c(-500, seq(-100, 100, length.out = 19))   # n = 20
  esA <- expected_shortfall(tA, 0.05)
  expect_equal(esA$pnl, -500)
  expect_equal(esA$loss, 500)

  # Case B: alpha*R = 2 exactly -> ES = mean of the 2 worst.
  tB <- c(-500, -300, seq(0, 100, length.out = 38))  # n = 40
  expect_equal(expected_shortfall(tB, 0.05)$pnl, mean(c(-500, -300)))

  # Case C: fractional tail (alpha*R = 2.5). Acerbi integral by hand:
  #   k = 2, frac = 0.5 -> (x1 + x2 + 0.5*x3) / 2.5
  tC <- c(-100, -80, -60, rep(50, 47))               # n = 50, alpha*R = 2.5
  xs <- sort(tC)
  expect_equal(expected_shortfall(tC, 0.05)$pnl,
               (xs[1] + xs[2] + 0.5 * xs[3]) / 2.5)
})


test_that("ES is at least as severe as VaR (coherence on the tail)", {
  # Expected shortfall averages the worst tail, so its loss >= VaR loss and its
  # signed P&L <= VaR signed P&L, for any sample.
  sim <- simulate_sessions(dist_eng, N = 50, R = 8000, seed = 7)
  v  <- value_at_risk(sim$totals, 0.05)
  es <- expected_shortfall(sim$totals, 0.05)
  expect_lte(es$pnl, v$pnl)
  expect_gte(es$loss, v$loss)
})


# ==============================================================================
# Dream-vs-reality decomposition
# ==============================================================================

test_that("dream_vs_reality removes exactly the top-tier mass and renormalises", {
  dv <- dream_vs_reality(dist_eng, N = 100)
  expect_equal(dv$top_value, 45)
  expect_equal(dv$p_top, 0.05)
  # Conditional probabilities sum to exactly 1.
  expect_equal(sum(dv$conditional_dist$prob), 1)
  # The conditional support excludes the top value and nothing else.
  expect_false(any(dv$conditional_dist$value == 45))
  expect_setequal(dv$conditional_dist$value, c(-5, 0, 5))
  # The removed mass is exactly p_top: conditional raw probs are the originals
  # rescaled by 1/(1 - p_top).
  expect_equal(dv$conditional_dist$prob,
               c(0.60, 0.20, 0.15) / 0.95)
})


test_that("dream_vs_reality obeys the law of total expectation", {
  dv <- dream_vs_reality(dist_eng, N = 250)
  # E[full] = (1 - p_top) * E[conditional] + p_top * top_value.
  reconstructed <- (1 - dv$p_top) * dv$conditional$ev_play +
    dv$p_top * dv$top_value
  expect_equal(dv$full$ev_play, reconstructed)
  expect_equal(dv$full$ev_play, 0)                 # hand value for dist_eng
  # Session means scale by N.
  expect_equal(dv$full$mean_session, 250 * dv$full$ev_play)
  expect_equal(dv$conditional$mean_session, 250 * dv$conditional$ev_play)
  # Stripping the jackpot makes the honest average worse (more negative) here.
  expect_lt(dv$conditional$ev_play, dv$full$ev_play)
})


test_that("dream_vs_reality guards the all-top degenerate case", {
  dv <- dream_vs_reality(data.frame(value = 10, prob = 1), N = 10)
  expect_true(is.na(dv$conditional$ev_play))
  expect_equal(nrow(dv$conditional_dist), 0L)
})


# ==============================================================================
# Engagement metrics (all analytical / exact)
# ==============================================================================

test_that("engagement_metrics computes exact analytical win rates", {
  N <- 200
  em <- engagement_metrics(dist_eng, N = N, k = c(1, 2, 10))
  expect_equal(em$price, 5)              # derived from -min(value)
  expect_equal(em$p_win, 0.40)           # {0, 5, 45}
  expect_equal(em$p_lose, 0.60)
  expect_equal(em$expected_wins, N * 0.40)
  # wins >= k*stake => net_value >= (k-1)*price:
  #   k=1: net >= 0  -> {0,5,45}  = 0.40
  #   k=2: net >= 5  -> {5,45}    = 0.20
  #   k=10: net >= 45 -> {45}     = 0.05
  expect_equal(unname(em$wins_ge_k), N * c(0.40, 0.20, 0.05))
  # k = 1 count equals the expected number of break-even-or-better plays.
  expect_equal(unname(em$wins_ge_k["k1"]), N * 0.40)
})


test_that("engagement_metrics honours an explicit price override", {
  # All-win dist: no losing row, so price cannot be derived -> must be supplied.
  expect_error(engagement_metrics(dist_allwin, N = 10), "derive a positive stake")
  em <- engagement_metrics(dist_allwin, N = 10, price = 5)
  # Every outcome (1, 2) is a win vs a £5 stake; none reaches 2x stake (£10).
  expect_equal(em$p_win, 1)
  expect_equal(unname(em$wins_ge_k["k2"]), 0)
})


# ==============================================================================
# Longest losing streak (the one simulated engagement metric)
# ==============================================================================

test_that("longest_losing_streak: degenerate all-loss dist => streak == N", {
  ls <- longest_losing_streak(dist_allloss, N = 137, R = 20, seed = 1)
  expect_equal(ls$p_lose, 1)
  expect_true(all(ls$streaks == 137))
  expect_equal(ls$max, 137L)
})


test_that("longest_losing_streak: no possible loss => streak == 0", {
  # All-win dist with a stake below the support -> p_lose = 0, so no losing run.
  ls <- longest_losing_streak(dist_allwin, N = 500, R = 10, seed = 1, price = 5)
  expect_equal(ls$p_lose, 0)
  expect_true(all(ls$streaks == 0))
})


test_that("longest_losing_streak: streaks lie in [0, N], reproducible, ~sane mean", {
  N <- 40
  ls1 <- longest_losing_streak(dist_eng, N = N, R = 1500, seed = 42)
  ls2 <- longest_losing_streak(dist_eng, N = N, R = 1500, seed = 42)
  expect_identical(ls1$streaks, ls2$streaks)          # reproducible
  expect_true(all(ls1$streaks >= 0 & ls1$streaks <= N))
  # A different seed almost surely differs.
  ls3 <- longest_losing_streak(dist_eng, N = N, R = 1500, seed = 43)
  expect_false(identical(ls1$streaks, ls3$streaks))
  # Sanity: with p_lose = 0.6 the expected longest run is roughly
  # log_{1/p}(N*(1-p)) ~ log(16)/log(1/0.6) ~ 5.4 -- assert a wide, principled
  # bracket rather than a point (this is only an asymptotic guide).
  expect_gt(ls1$mean, 2)
  expect_lt(ls1$mean, 15)
})


# ==============================================================================
# Monte Carlo standard error on percentiles (Maritz-Jarrett)
# ==============================================================================

test_that("quantile_se matches the theoretical median SE for a Normal sample", {
  # SE of the sample median of Normal(0,1) is sqrt(pi/(2n)). A single MJ estimate
  # has ~6% relative sampling error at n = 40000; averaging 10 independent
  # samples cuts that to ~2%, so a 10% band is a principled, non-flaky check.
  n <- 40000
  ses <- vapply(1:10, function(s) {
    set.seed(s); quantile_se(rnorm(n), 0.5)$se
  }, numeric(1))
  theory <- sqrt(pi / (2 * n))
  expect_lt(abs(mean(ses) / theory - 1), 0.10)
})


test_that("quantile_se shrinks like 1/sqrt(R) as the sample grows", {
  # Quadrupling n should halve the SE. Average over 10 seeds per size so the
  # ratio's own sampling error (~3%) is small; a 15% band is comfortably safe.
  avg <- function(n, seeds) mean(vapply(seeds, function(s) {
    set.seed(s); quantile_se(rnorm(n), 0.5)$se
  }, numeric(1)))
  s_small <- avg(20000, 101:110)
  s_big   <- avg(80000, 201:210)   # 4x the sample
  expect_lt(abs(s_small / s_big - 2), 0.15)
  expect_gt(s_small, s_big)        # strictly smaller SE at larger n
})


test_that("quantile_se returns a tidy frame with type-7 point estimates", {
  set.seed(5)
  x  <- rnorm(5000)
  qs <- quantile_se(x, probs = c(.05, .5, .95))
  expect_named(qs, c("prob", "quantile", "se"))
  expect_equal(qs$quantile, quantile(x, c(.05, .5, .95), names = FALSE, type = 7))
  expect_true(all(qs$se > 0))
  # p at exactly 0 or 1 has no finite-sample SE here -> NA (documented).
  expect_true(is.na(quantile_se(x, probs = c(0, 1))$se[1]))
})


# ==============================================================================
# Leaderboard-vs-N series
# ==============================================================================

test_that("leaderboard_series mean_pnl is exact analytical N * ev", {
  d   <- data.frame(value = c(-5, 15), prob = c(0.8, 0.2))
  evd <- sum(d$value * d$prob)                 # -4 + 3 = -1 per play
  ser <- leaderboard_series(d, N_grid = c(10, 100, 1000), metric = "mean_pnl")
  expect_equal(ser$value, c(10, 100, 1000) * evd)
  expect_equal(ser$metric, rep("mean_pnl", 3))
})


test_that("leaderboard_series p_profit is a valid, reproducible probability curve", {
  Ng  <- c(1, 5, 25)
  s1  <- leaderboard_series(dist_eng, N_grid = Ng, metric = "p_profit",
                            R = 4000, seed = 11)
  s2  <- leaderboard_series(dist_eng, N_grid = Ng, metric = "p_profit",
                            R = 4000, seed = 11)
  expect_equal(s1$value, s2$value)                 # reproducible
  expect_true(all(s1$value >= 0 & s1$value <= 1))  # probabilities
  expect_equal(nrow(s1), length(Ng))
})


test_that("leaderboard_series accepts a custom metric function", {
  ser <- leaderboard_series(dist_eng, N_grid = c(10, 50),
                            metric = function(sim, dist) mean(sim$totals),
                            R = 3000, seed = 3)
  # Custom metric = mean P&L; must track analytical N * ev (= 0) within MC SE.
  a10 <- analytical_summary(dist_eng, N = 10)
  expect_lt(abs(ser$value[1] - a10$mean), 4 * a10$sd / sqrt(3000))
})


# ==============================================================================
# Bundle builder integration
# ==============================================================================

test_that("build_metrics assembles a coherent bundle over one sim/dist", {
  N <- 100; R <- 6000
  sim    <- simulate_sessions(dist_eng, N = N, R = R, seed = 2024)
  bundle <- build_metrics(sim, dist_eng)

  # Meta: stake derived, total stake, alpha threaded through.
  expect_equal(bundle$meta$price, 5)
  expect_equal(bundle$meta$total_stake, N * 5)

  # Risk block is computed from the SAME totals the caller passed in.
  expect_equal(bundle$risk$p_profit, mean(sim$totals > 0))
  expect_equal(bundle$risk$var$pnl,
               quantile(sim$totals, 0.05, names = FALSE, type = 7))
  # Loss thresholds are fractions of total stake, relabelled by fraction.
  expect_equal(length(bundle$risk$p_loss_gt), 3L)
  expect_true(all(grepl("^frac_", names(bundle$risk$p_loss_gt))))

  # Engagement block matches the standalone analytical figures.
  em <- engagement_metrics(dist_eng, N = N, price = 5)
  expect_equal(bundle$engagement$expected_wins, em$expected_wins)

  # Dream block present with the hand-computed top tier.
  expect_equal(bundle$dream$p_top, 0.05)

  # Percentile SE frame covers the sim's fan-chart probs.
  expect_equal(nrow(bundle$percentile_se), length(sim$probs))
  expect_true(all(bundle$percentile_se$se >= 0))

  # Streak sub-sim ran and is bounded by N.
  expect_true(all(bundle$streak$streaks >= 0 & bundle$streak$streaks <= N))
})


test_that("build_metrics rejects a non-sim input", {
  expect_error(build_metrics(list(foo = 1), dist_eng), "simulate_sessions")
})
