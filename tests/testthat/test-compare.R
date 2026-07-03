# ==============================================================================
# test-compare.R
# ------------------------------------------------------------------------------
# Correctness tests for Layer 8 (R/compare.R) -- the CRN-driven compare engine
# -- and the compare viz builders in R/viz.R.
#
# THE CORRECTNESS-CRITICAL PART is Common Random Numbers (CRN) fairness, so the
# property tests below are the heart of this file:
#   - SELF-COMPARISON IDENTITY: a distribution compared against a relabelled
#     copy of itself under a shared CRN gives byte-identical totals (paired
#     difference exactly 0) -- both are driven by the same uniforms.
#   - VARIANCE REDUCTION: for positively-correlated strategies, Var(A - B)
#     under shared CRN is strictly less than under independent seeds -- WHY CRN
#     makes the comparison fair (the shared noise cancels).
# Plus: comparison-table correctness against standalone build_metrics under the
# same CRN, viz builders return ggplot, the <2-strategy guard, and a 3+ compare.
# ==============================================================================

# Load the layers under test (mirrors the other test files' idempotent guard).
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
if (!exists("make_strategy", mode = "function")) {
  .cands <- c("R/strategies.R", "../../R/strategies.R", "../R/strategies.R")
  .hit   <- .cands[file.exists(.cands)][1]
  if (is.na(.hit)) stop("Could not locate R/strategies.R.")
  source(.hit)
}
if (!exists("viz_compare_dist", mode = "function")) {
  .cands <- c("R/viz.R", "../../R/viz.R", "../R/viz.R")
  .hit   <- .cands[file.exists(.cands)][1]
  if (is.na(.hit)) stop("Could not locate R/viz.R.")
  source(.hit)
}
if (!exists("run_compare", mode = "function")) {
  .cands <- c("R/compare.R", "../../R/compare.R", "../R/compare.R")
  .hit   <- .cands[file.exists(.cands)][1]
  if (is.na(.hit)) stop("Could not locate R/compare.R.")
  source(.hit)
}

# ------------------------------------------------------------------------------
# Fixtures: a tiny 3-game universe with distinct outcome laws (each prob sums 1).
# ------------------------------------------------------------------------------
outcomes_fix <- data.frame(
  game_id     = c("A", "A", "A",  "B", "B", "B",  "C", "C"),
  net_value   = c(-2, 3, 48,     -1, 1, 19,      -5, 45),
  probability = c(0.80, 0.18, 0.02,  0.70, 0.28, 0.02,  0.90, 0.10),
  stringsAsFactors = FALSE
)
gs_fix <- data.frame(
  game_id    = c("A", "B", "C"),
  name       = c("Game A", "Game B", "Game C"),
  price      = c(2, 1, 5),
  rtp        = c(0.70, 0.80, 0.60),
  on_sale    = TRUE, is_hidden = FALSE, prohibited = FALSE,
  category   = "scratch", source = "scratchcard",
  stringsAsFactors = FALSE
)

sA <- strategy_single("A", gs_fix)
sB <- strategy_single("B", gs_fix)
sC <- strategy_single("C", gs_fix)


# ==============================================================================
# 1. CRN FAIRNESS -- SELF-COMPARISON IDENTITY (the crux)
# ==============================================================================

test_that("self-comparison under shared CRN gives byte-identical totals", {
  # Two objects, SAME distribution, DIFFERENT id/label (so dedupe keeps both).
  sA_copy <- sA
  sA_copy$id    <- "single:A_copy"
  sA_copy$label <- "Game A (copy)"

  cr <- run_compare(list(sA, sA_copy), outcomes_fix, N = 60, R = 300, seed = 7)

  tA1 <- cr$per[[1]]$sim$totals
  tA2 <- cr$per[[2]]$sim$totals

  # Byte-identical: driven by the same uniforms on the same distribution.
  expect_identical(tA1, tA2)
  # Paired difference is exactly 0 for EVERY session.
  expect_true(all(tA1 - tA2 == 0))
})


# ==============================================================================
# 2. CRN FAIRNESS -- VARIANCE REDUCTION (why CRN is fair)
# ==============================================================================

test_that("shared CRN strictly reduces Var(totals_A - totals_B) vs independent seeds", {
  N <- 80L; R <- 4000L

  # Shared CRN: run_compare drives both strategies on the SAME uniform stream.
  cr <- run_compare(list(sA, sB), outcomes_fix, N = N, R = R, seed = 123)
  tA_crn <- cr$per[[1]]$sim$totals
  tB_crn <- cr$per[[2]]$sim$totals
  v_crn  <- stats::var(tA_crn - tB_crn)

  # Independent seeds: each strategy on its OWN, unrelated uniform stream.
  distA <- strategy_distribution(sA, outcomes_fix)
  distB <- strategy_distribution(sB, outcomes_fix)
  simA_ind <- simulate_sessions(distA, N = N, R = R, seed = 11)
  simB_ind <- simulate_sessions(distB, N = N, R = R, seed = 22)
  v_ind    <- stats::var(simA_ind$totals - simB_ind$totals)

  # CRN must reduce the paired-difference variance. Positively-correlated
  # outcomes mean Var(A-B) = VarA + VarB - 2*Cov with Cov > 0 under CRN, vs
  # Cov = 0 under independence -- so v_crn is strictly (and here substantially)
  # below v_ind. Principled margin: require at least a 20% reduction, well
  # inside the true gap for these rank-matched games.
  expect_lt(v_crn, v_ind)
  expect_lt(v_crn, 0.80 * v_ind)

  # Surface the measured variances in the test log for the reviewer.
  message(sprintf("[CRN variance reduction] Var(A-B) shared CRN = %.4f, independent = %.4f, ratio = %.3f",
                  v_crn, v_ind, v_crn / v_ind))
})


# ==============================================================================
# 3. COMPARISON-TABLE CORRECTNESS
# ==============================================================================

test_that("comparison table matches standalone build_metrics under the same CRN", {
  N <- 50L; R <- 500L; seed <- 42L; conf <- 0.90
  cr <- run_compare(list(sA, sB), outcomes_fix, N = N, R = R, seed = seed, conf = conf)

  for (s in list(sA, sB)) {
    dist  <- strategy_distribution(s, outcomes_fix)
    price <- strategy_expected_price(s)
    # Standalone run on the SAME scalar-crn stream (crn = seed) as run_compare.
    sim <- simulate_sessions(dist, N = N, R = R, crn = seed)
    m   <- build_metrics(sim, dist, price = price, alpha = 1 - conf)

    row <- cr$table[cr$table$strategy == s$label, , drop = FALSE]
    expect_equal(nrow(row), 1L)
    expect_equal(row$mean_pnl,       mean(sim$totals))
    expect_equal(row$median_pnl,     stats::median(sim$totals))
    expect_equal(row$p_profit,       m$risk$p_profit)
    expect_equal(row$p_breakeven,    m$risk$p_breakeven)
    expect_equal(row$var_loss,       m$risk$var$loss)
    expect_equal(row$es_loss,        m$risk$es$loss)
    expect_equal(row$expected_price, price)
    expect_equal(row$expected_spend, N * price)
  }
})

test_that("totals_long is tidy and complete (one row per session per strategy)", {
  cr <- run_compare(list(sA, sB), outcomes_fix, N = 20, R = 100, seed = 5)
  expect_true(all(c("strategy", "session_id", "pnl") %in% names(cr$totals_long)))
  expect_equal(nrow(cr$totals_long), 2L * 100L)
  expect_setequal(levels(cr$totals_long$strategy), cr$labels)
  # The long totals for each strategy match that strategy's sim totals exactly.
  la <- cr$totals_long$pnl[cr$totals_long$strategy == sA$label]
  expect_identical(la, cr$per[[1]]$sim$totals)
})


# ==============================================================================
# 4. GUARDS AND EDGE CASES
# ==============================================================================

test_that("run_compare errors on fewer than 2 strategies", {
  expect_error(run_compare(list(sA), outcomes_fix, N = 10, R = 10, seed = 1),
               "at least 2")
  # A bare strategy object (not a list) is rejected.
  expect_error(run_compare(sA, outcomes_fix, N = 10, R = 10, seed = 1),
               "list of lottery_strategy")
  # Duplicate ids collapse to one -> also < 2.
  expect_error(run_compare(list(sA, sA), outcomes_fix, N = 10, R = 10, seed = 1),
               "at least 2")
})

test_that("run_compare handles 3+ strategies", {
  cr <- run_compare(list(sA, sB, sC), outcomes_fix, N = 30, R = 200, seed = 9)
  expect_equal(nrow(cr$table), 3L)
  expect_equal(length(cr$per), 3L)
  expect_equal(nrow(cr$totals_long), 3L * 200L)
  # All three share the SAME uniforms: any two differ only by their outcome law,
  # never trivially identical unless the laws coincide (A, B, C differ here).
  expect_false(isTRUE(all(cr$per[[1]]$sim$totals == cr$per[[2]]$sim$totals)))
})

test_that("run_compare defaults a NULL seed so a comparison is always CRN-fair", {
  cr1 <- run_compare(list(sA, sB), outcomes_fix, N = 20, R = 50, seed = NULL)
  cr2 <- run_compare(list(sA, sB), outcomes_fix, N = 20, R = 50, seed = NULL)
  # Deterministic default seed -> two calls reproduce identical totals.
  expect_identical(cr1$per[[1]]$sim$totals, cr2$per[[1]]$sim$totals)
})


# ==============================================================================
# 5. COMPARE VIZ BUILDERS -- return ggplot, introspectable without a device
# ==============================================================================

test_that("compare viz builders return ggplot objects", {
  cr <- run_compare(list(sA, sB, sC), outcomes_fix, N = 40, R = 300, seed = 3)

  p_dist <- viz_compare_dist(cr)
  p_fan  <- viz_compare_fan(cr)
  expect_s3_class(p_dist, "ggplot")
  expect_s3_class(p_fan,  "ggplot")

  # ggplot_build introspection proves they assemble without a graphics device.
  expect_silent(ggplot2::ggplot_build(p_dist))
  expect_silent(ggplot2::ggplot_build(p_fan))

  # Transform is honoured on the distribution overlay.
  p_log <- viz_compare_dist(cr, transform = "signed_log")
  expect_s3_class(p_log, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p_log))

  # Alt text is a single descriptive string mentioning every strategy.
  alt <- viz_compare_dist_alt(cr)
  expect_type(alt, "character")
  expect_length(alt, 1L)
  for (lbl in cr$labels) expect_true(grepl(lbl, alt, fixed = TRUE))
})

test_that("compare fan chart draws one median line per strategy", {
  cr <- run_compare(list(sA, sB, sC), outcomes_fix, N = 40, R = 200, seed = 3)
  p  <- viz_compare_fan(cr)
  b  <- ggplot2::ggplot_build(p)
  # The line layer's grouping should carry exactly 3 strategies.
  line_idx <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomLine"), logical(1)))[1]
  n_groups <- length(unique(b$data[[line_idx]]$group))
  expect_equal(n_groups, 3L)
})

test_that("run_compare_leaderboard assembles a shared-seed crossover series", {
  series <- run_compare_leaderboard(list(sA, sB, sC), outcomes_fix,
                                    N_grid = c(10L, 100L, 500L),
                                    metric = "p_profit", R = 200, seed = 8)
  expect_true(all(c("strategy", "N", "metric", "value") %in% names(series)))
  expect_equal(length(unique(series$strategy)), 3L)
  expect_equal(sort(unique(series$N)), c(10L, 100L, 500L))
  # It feeds viz_leaderboard_vs_n() unchanged.
  expect_s3_class(viz_leaderboard_vs_n(series), "ggplot")
})

test_that("compare_table_display formats every strategy row for the UI", {
  cr <- run_compare(list(sA, sB), outcomes_fix, N = 20, R = 100, seed = 1)
  disp <- compare_table_display(cr)
  expect_equal(nrow(disp), 2L)
  expect_true("Strategy" %in% names(disp))
  expect_true(all(grepl("^\u00a3", disp$`Mean P&L`)))
  expect_true(all(grepl("%$", disp$`P(profit)`)))
})
