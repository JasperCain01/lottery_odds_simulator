# ==============================================================================
# test-narrative.R
# ------------------------------------------------------------------------------
# Correctness + wording tests for Layer 5 (R/narrative.R), the plain-English
# narrative generator (Phase 7).
#
# This layer is prose, not arithmetic -- the numbers it narrates are already
# trusted (Phase 3's build_metrics()). So the tests here are NOT statistical
# property tests; they pin EXACT strings / regexes against small, hand-built
# metrics fixtures (mirroring build_metrics()'s output shape, see
# R/metrics.R's header) to lock down rounding, pluralisation, sign convention,
# and -- most importantly -- the app's honest, never-oversell-the-upside
# framing.
#
# Ensure the layer under test is loaded (mirrors test-metrics.R's guard).
# narrative.R has no dependency on simulate.R/metrics.R at the FUNCTION level
# (it only reads a build_metrics()-shaped list and, optionally, a
# simulate_sessions()-shaped list's $totals), so only itself needs sourcing.
# ==============================================================================

if (!exists("narrate_run", mode = "function")) {
  .cands <- c("R/narrative.R", "../../R/narrative.R", "../R/narrative.R")
  .hit   <- .cands[file.exists(.cands)][1]
  if (is.na(.hit)) stop("Could not locate R/narrative.R.")
  source(.hit)
}

# ------------------------------------------------------------------------------
# mk_metrics(...): a hand-built build_metrics()-shaped fixture.
# ------------------------------------------------------------------------------
# Mirrors the exact list shape build_metrics() returns (R/metrics.R, section
# 6) so narrate_run() sees the same contract it would from a real pipeline
# run, without needing to actually simulate anything. Every field narrate_run
# reads is overridable via named arguments; everything else is a plausible
# default for a small losing scratchcard game.
mk_metrics <- function(N = 100, price = 2, ev_play = -0.5,
                       p05 = -160, p25 = -90, p50 = -60, p75 = -10, p95 = 50,
                       p_profit = 0.18, p_top = 0.002, top_value = 500,
                       expected_wins = 30, p_win = 0.3, streak_median = 7) {
  total_stake <- N * price
  p_top_in_N  <- if (is.na(p_top) || p_top <= 0) 0 else 1 - (1 - p_top)^N

  list(
    meta = list(N = N, R = 1000L, seed = 42, price = price, alpha = 0.05,
               total_stake = total_stake, ev_play = ev_play, sd_play = 5),
    risk = list(
      alpha = 0.05, p_profit = p_profit, p_breakeven = p_profit + 0.02,
      loss_thresholds = c(50, 100),
      p_loss_gt = c(frac_0.25 = 0.6, frac_0.5 = 0.3),
      var = list(alpha = 0.05, pnl = p05, loss = -p05),
      es  = list(alpha = 0.05, pnl = p05 - 40, loss = -(p05 - 40))
    ),
    dream = list(
      N = N, top_value = top_value, p_top = p_top, p_top_in_N = p_top_in_N,
      full = list(ev_play = ev_play, sd_play = 5, var_play = 25,
                 mean_session = N * ev_play),
      conditional = list(ev_play = ev_play - 0.1, sd_play = 4, var_play = 16,
                        mean_session = N * (ev_play - 0.1))
    ),
    engagement = list(
      N = N, price = price, p_win = p_win, p_lose = 1 - p_win,
      expected_wins = expected_wins, k = c(1, 2, 5, 10),
      wins_ge_k = c(k1 = expected_wins, k2 = expected_wins / 3,
                    k5 = expected_wins / 10, k10 = expected_wins / 30)
    ),
    streak = list(
      N = N, R = 1000L, p_lose = 1 - p_win,
      mean = streak_median + 1, median = streak_median, max = streak_median * 3,
      probs = c(.5, .9, .95, .99),
      quantiles = c(p50 = streak_median, p90 = streak_median * 2,
                   p95 = streak_median * 2.5, p99 = streak_median * 3)
    ),
    percentile_se = data.frame(
      prob     = c(.05, .25, .5, .75, .95),
      quantile = c(p05, p25, p50, p75, p95),
      se       = c(5, 4, 3, 4, 6)
    )
  )
}


# ==============================================================================
# Small focused helpers
# ==============================================================================

test_that("pluralise: singular vs plural, including zero and comma grouping", {
  expect_equal(pluralise(1, "play"), "1 play")
  expect_equal(pluralise(2, "play"), "2 plays")
  expect_equal(pluralise(0, "play"), "0 plays")
  expect_equal(pluralise(1000, "play"), "1,000 plays")
})


test_that("fmt_gbp: whole pounds by default, pence for sub-£1, forced modes, comma grouping", {
  expect_equal(fmt_gbp(1234), "£1,234")
  expect_equal(fmt_gbp(1234.6), "£1,235")     # rounds to nearest whole pound
  expect_equal(fmt_gbp(0.4), "£0.40")          # sub-£1 auto-switches to pence
  expect_equal(fmt_gbp(2.5, cents = TRUE), "£2.50")
  expect_equal(fmt_gbp(5, cents = TRUE), "£5.00")
  expect_equal(fmt_gbp(5, cents = FALSE), "£5")
})


test_that("pnl_phrase: consistent sign convention -- 'lost' for losses, 'ahead' for profit", {
  expect_equal(pnl_phrase(-60), "£60 lost")
  expect_equal(pnl_phrase(40), "£40 ahead")
  expect_equal(pnl_phrase(0), "breaking even")
  expect_equal(pnl_phrase(0.001), "breaking even")   # near-zero collapses to break-even
})


test_that("fmt_pct rounds to the requested precision", {
  expect_equal(fmt_pct(0.1834, digits = 1), "18.3%")
  expect_equal(fmt_pct(0.5, digits = 0), "50%")
})


test_that("describe_chance: honest wording at the extremes, numeric in the middle", {
  expect_equal(describe_chance(0), "never")
  expect_equal(describe_chance(1), "always")
  expect_equal(describe_chance(0.001), "essentially never")   # not an awkward "0.1%"
  expect_equal(describe_chance(0.999), "almost always")
  expect_equal(describe_chance(0.18), "about 18.0% of the time")
  expect_equal(describe_chance(0.18, suffix = ""), "about 18.0%")
})


test_that("jackpot_odds: W = round(1/p) exactly, NA guard on p <= 0 / NA (no divide-by-zero)", {
  expect_equal(jackpot_odds(1 / 3), 3L)
  expect_equal(jackpot_odds(0.002), 500L)
  expect_equal(jackpot_odds(1 / 7692.3), round(7692.3))
  expect_true(is.na(jackpot_odds(0)))
  expect_true(is.na(jackpot_odds(NA_real_)))
})


# ==============================================================================
# narrate_run(): end-to-end wording on hand-built fixtures
# ==============================================================================

test_that("normal losing game: typical loss, interval, profit chance, jackpot framing", {
  m   <- mk_metrics()
  s   <- narrate_run(m, conf = 0.90)
  txt <- paste(s, collapse = " ")

  # Headline: N plays, price (pence-shown), stake, typical (median) loss.
  expect_true(grepl("100 plays", txt, fixed = TRUE))
  expect_true(grepl("£2.00 each", txt, fixed = TRUE))
  expect_true(grepl("£200.00 staked in total", txt, fixed = TRUE))
  expect_true(grepl("£60 lost", txt, fixed = TRUE))

  # Empirical interval: 90% of the time between the p05 and p95 rows.
  expect_true(grepl("90% of the time", txt, fixed = TRUE))
  expect_true(grepl("£160 lost", txt, fixed = TRUE))
  expect_true(grepl("£50 ahead", txt, fixed = TRUE))
  expect_true(grepl("not a theoretical approximation", txt, fixed = TRUE))

  # Profit chance, honestly worded.
  expect_true(grepl("about 18.0% of the time", txt, fixed = TRUE))

  # Jackpot: W = round(1/0.002) = 500, plausible 1-in-W.
  expect_true(grepl("1-in-500 chance per play", txt, fixed = TRUE))
  expect_true(grepl("£500", txt, fixed = TRUE))
})


test_that("essentially-never-profit game reads 'essentially never', not an awkward 0%", {
  m   <- mk_metrics(p_profit = 0.001)
  txt <- paste(narrate_run(m), collapse = " ")

  expect_true(grepl("essentially never", txt, fixed = TRUE))
  # Standalone "0.0%"/"0%" would be the awkward phrasing this guards against;
  # word-boundary the check so it does not false-positive on substrings like
  # the unrelated "30.0%" engagement figure elsewhere in the narrative.
  expect_false(grepl("\\b0\\.0%\\b", txt))
  expect_false(grepl("\\b0%\\b", txt))
})


test_that("N = 1 is singular: '1 play', not '1 plays'", {
  m <- mk_metrics(N = 1, price = 5, ev_play = -1,
                  p05 = -5, p25 = -5, p50 = -5, p75 = -5, p95 = -5,
                  expected_wins = 0.1, p_win = 0.1, streak_median = 1)
  txt <- paste(narrate_run(m), collapse = " ")

  expect_true(grepl("\\b1 play\\b", txt))
  expect_false(grepl("1 plays", txt, fixed = TRUE))
})


test_that("degenerate interval (lo == hi) reads plainly, not 'between £X and £X'", {
  # A conf-band that collapses onto one value (e.g. a cheap deterministic play).
  m <- mk_metrics(N = 1, price = 5, ev_play = -5,
                  p05 = -5, p25 = -5, p50 = -5, p75 = -5, p95 = -5)
  txt <- paste(narrate_run(m), collapse = " ")

  expect_true(grepl("Almost every session comes out the same: £5 lost", txt))
  # It must NOT produce the clumsy "between £5 lost and £5 lost".
  expect_false(grepl("between £5 lost and £5 lost", txt))
})


test_that("p_top = 0 (no top-prize tier) omits the jackpot clause gracefully, no divide-by-zero", {
  m <- mk_metrics(p_top = 0)
  expect_no_error(s <- narrate_run(m))
  txt <- paste(s, collapse = " ")

  expect_false(grepl("1-in-", txt, fixed = TRUE))
  expect_false(grepl("Inf", txt, fixed = TRUE))
  expect_false(grepl("NaN", txt, fixed = TRUE))
  expect_false(grepl("\\bNA\\b", txt))
})


test_that("p_top = NA (unknown/absent top tier) is also handled gracefully", {
  m <- mk_metrics(p_top = NA_real_)
  expect_no_error(s <- narrate_run(m))
  expect_false(any(grepl("1-in-", s, fixed = TRUE)))
})


test_that("jackpot 1-in-W matches round(1/p_top) exactly on a fixture", {
  m <- mk_metrics(p_top = 1 / 3, top_value = 999)
  W <- jackpot_odds(m$dream$p_top)

  expect_equal(W, 3L)
  expect_equal(W, round(1 / m$dream$p_top))

  txt <- paste(narrate_run(m), collapse = " ")
  expect_true(grepl("1-in-3 chance per play", txt, fixed = TRUE))
  expect_true(grepl("£999", txt, fixed = TRUE))
})


test_that("honest framing: never uses encouraging/overselling language on a losing game", {
  m   <- mk_metrics()   # a typical losing game (see header fixture defaults)
  txt <- paste(narrate_run(m), collapse = " ")

  bad_phrases <- c("good chance", "likely to win", "guaranteed", "great odds",
                   "you will win", "sure thing", "can't lose", "cannot lose",
                   "your best bet", "don't miss out")
  for (w in bad_phrases) {
    expect_false(grepl(w, txt, ignore.case = TRUE),
                info = sprintf("forbidden overselling phrase found: '%s'", w))
  }
})


test_that("mean-divergence caveat appears only when mean and median meaningfully diverge", {
  # Divergent: mean_session (N*ev_play = 0) is £60 above the median (-60).
  m_diverge <- mk_metrics(p50 = -60, ev_play = 0)
  expect_true(grepl("AVERAGE result is different",
                    paste(narrate_run(m_diverge), collapse = " "), fixed = TRUE))

  # Close: mean_session (N*-0.5 = -50) and median (-50) coincide -> omitted.
  m_close <- mk_metrics(p50 = -50, ev_play = -0.5)
  expect_false(grepl("AVERAGE result is different",
                     paste(narrate_run(m_close), collapse = " "), fixed = TRUE))
})


test_that("narrate_run prefers exact sim$totals over the percentile_se grid when supplied", {
  m      <- mk_metrics()   # percentile_se p50 = -60
  totals <- c(rep(-1000, 5), rep(1000, 5))   # type-7 median = 0 exactly
  sim    <- list(totals = totals)

  txt <- paste(narrate_run(m, sim = sim), collapse = " ")
  expect_true(grepl("breaking even", txt, fixed = TRUE))
  expect_false(grepl("£60 lost", txt, fixed = TRUE))
})


test_that("strategy label, when supplied, is prepended as its own sentence", {
  m <- mk_metrics()
  s <- narrate_run(m, strategy = list(label = "Always cheapest"))
  expect_equal(s[1], "Strategy: Always cheapest.")
})


test_that("engagement/streak flavour sentence is present, tight, and pluralised correctly", {
  m   <- mk_metrics(expected_wins = 30, p_win = 0.3, streak_median = 7)
  txt <- paste(narrate_run(m), collapse = " ")

  expect_true(grepl("win something on about 30.0 of your 100 plays", txt, fixed = TRUE))
  expect_true(grepl("30.0% per play", txt, fixed = TRUE))
  expect_true(grepl("streak of 7 plays in a row", txt, fixed = TRUE))
})


test_that("narrate_run errors clearly on a bundle missing $meta rather than failing obscurely", {
  expect_error(narrate_run(list(risk = list(p_profit = 0.1))), "meta")
})
