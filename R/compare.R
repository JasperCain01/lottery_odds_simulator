# ==============================================================================
# compare.R
# ------------------------------------------------------------------------------
# Layer 8 of the lottery odds simulator (see IMPLEMENTATION_PLAN.md, Phase 8).
#
# Purpose: put two or more strategies HEAD-TO-HEAD fairly. The engine glue that
# assembles a multi-strategy comparison from the finished backend layers
# (strategies.R -> simulate.R -> metrics.R) plus the tidy structures the Phase 8
# compare viz builders (R/viz.R) and app.R consume.
#
# ------------------------------------------------------------------------------
# THE ONE CORRECTNESS-CRITICAL PART: COMMON RANDOM NUMBERS (CRN) FAIRNESS
# ------------------------------------------------------------------------------
# A naive comparison runs each strategy on its OWN independent random draws, so
# any observed difference between two strategies is a mix of (a) the real
# difference in their outcome laws and (b) pure sampling noise -- two draws of
# the dice. To compare FAIRLY we drive every strategy with the SAME underlying
# randomness: Common Random Numbers.
#
# simulate.R implements CRN via a single inverse-CDF sampling path over the
# canonically value-sorted distribution (see its .draw_batch / .resolve_crn
# headers). Feeding the SAME uniform stream to two DIFFERENT strategy
# distributions therefore yields RANK-MATCHED, positively-correlated outcomes:
# session i of strategy A and session i of strategy B are driven by identical
# uniforms, so the shared "luck" cancels in the paired difference and what
# remains is the genuine difference between the strategies.
#
# HOW run_compare() wires it: we pass the SAME scalar `seed` as `crn = seed` to
# every strategy's simulate_sessions() call. A scalar crn is the memory-cheap
# option -- .resolve_crn() deterministically regenerates the identical R x N
# uniform matrix from that seed for each strategy, so no large matrix is held in
# memory and every strategy is provably evaluated on byte-identical uniforms.
# (Passing crn = seed leaves the `seed` argument NULL: the uniforms fully
# determine the draws, so the default seeded path is not used.)
#
# The two consequences the tests pin down:
#   - SELF-COMPARISON IDENTITY: the same distribution compared against itself
#     under a shared CRN gives byte-identical totals (paired difference 0 for
#     every session) -- both are driven by the same uniforms.
#   - VARIANCE REDUCTION: for positively-correlated strategies,
#     Var(totals_A - totals_B) under shared CRN is strictly less than under
#     independent seeds (the shared noise cancels) -- WHY CRN is fair.
#
# source()-ing this file is side-effect free: it defines functions only. It
# assumes simulate.R / metrics.R / strategies.R are already sourced.
# ==============================================================================


# ------------------------------------------------------------------------------
# .compare_strategy_list(strategies)
# ------------------------------------------------------------------------------
# Internal: validate the `strategies` argument and DEDUPE identical entries.
#
# Guard: must be a LIST of >= 2 lottery_strategy objects (a single strategy
# object, or a one-element list, has nothing to compare against).
#
# DEDUPE POLICY (documented, and deliberately by `id` not by distribution): two
# entries with the SAME strategy$id are the same chosen strategy selected twice
# (e.g. a UI multiselect that let a preset through twice) and the duplicate is
# dropped, keeping the first. We do NOT dedupe on distribution content: two
# objects with the same outcome law but different ids are a deliberate,
# meaningful pair (this is exactly the self-comparison identity the CRN tests
# rely on -- comparing a strategy against a relabelled copy of itself).
.compare_strategy_list <- function(strategies) {
  if (inherits(strategies, "lottery_strategy") || !is.list(strategies)) {
    stop("run_compare: `strategies` must be a list of lottery_strategy objects.",
         call. = FALSE)
  }
  ok <- vapply(strategies, function(s) inherits(s, "lottery_strategy"), logical(1))
  if (!all(ok)) {
    stop("run_compare: every element of `strategies` must be a lottery_strategy.",
         call. = FALSE)
  }
  ids  <- vapply(strategies, function(s) s$id, character(1))
  keep <- !duplicated(ids)
  strategies <- strategies[keep]
  if (length(strategies) < 2L) {
    stop("run_compare: need at least 2 distinct strategies to compare.",
         call. = FALSE)
  }
  strategies
}


# ------------------------------------------------------------------------------
# run_compare(strategies, outcomes, N, R, seed, conf, checkpoints, ...)
# ------------------------------------------------------------------------------
# Run a fair, CRN-driven head-to-head of >= 2 strategies over R sessions of N
# plays each, and assemble the tidy structures the compare viz + UI consume.
#
# CRN FAIRNESS GUARANTEE (the crux -- see the file header): the SAME scalar
# `seed` is passed as `crn = seed` to EVERY strategy's simulate_sessions() call,
# so all strategies are evaluated on byte-identical R x N uniforms. Differences
# between strategies are therefore real (their outcome laws differ), not
# sampling noise -- session i of one strategy and session i of another share the
# same underlying luck.
#
# Arguments:
#   strategies   list of >= 2 lottery_strategy objects (deduped by id).
#   outcomes     the Phase 1 outcomes table (bridge to each strategy's dist).
#   N, R         plays per session / number of sessions (shared by all).
#   seed         SHARED integer seed for the common uniform stream. Required for
#                CRN; if NULL it defaults to 1L (documented) so a comparison is
#                always reproducible and always fair.
#   conf         confidence level; risk metrics use alpha = 1 - conf.
#   checkpoints  fan-chart time points (forwarded to simulate_sessions()).
#   N_max/R_max/NR_max  optional UI ceilings; when all supplied, reuse
#                validate_run_bounds() as a secondary guard.
#
# Returns a list:
#   per          list, one element per (surviving) strategy, each a list of
#                (label, strategy, dist, sim, metrics, price).
#   labels       character vector of unique display labels (plot/table keys).
#   table        tidy comparison data.frame, ONE ROW PER STRATEGY, columns:
#                strategy, mean_pnl, median_pnl, p_profit, p_breakeven,
#                var_loss, es_loss, expected_price, expected_spend.
#   totals_long  tidy long data.frame (strategy, session_id, pnl) of every
#                session's final P&L, for the overlay plots.
#   N, R, seed, conf, probs, alpha   run metadata.
run_compare <- function(strategies, outcomes, N, R,
                        seed        = NULL,
                        conf        = 0.90,
                        checkpoints = 100,
                        N_max = NULL, R_max = NULL, NR_max = NULL,
                        ...) {
  strategies <- .compare_strategy_list(strategies)

  if (N < 1 || N != as.integer(N)) stop("N must be a positive integer.", call. = FALSE)
  if (R < 1 || R != as.integer(R)) stop("R must be a positive integer.", call. = FALSE)
  N <- as.integer(N); R <- as.integer(R)
  if (conf <= 0 || conf >= 1) stop("conf must be in (0, 1).", call. = FALSE)

  # Secondary UI-ceiling guard (reuse the single-run validator) when caps given.
  if (!is.null(N_max) && !is.null(R_max) && !is.null(NR_max)) {
    msg <- validate_run_bounds(N, R, N_max, R_max, NR_max)
    if (!is.null(msg)) stop(msg, call. = FALSE)
  }

  # The SHARED common-random-number seed. A scalar crn is memory-cheap: each
  # strategy regenerates the identical R x N uniforms from it (see header).
  if (is.null(seed) || !is.finite(seed)) seed <- 1L
  seed  <- as.integer(seed)
  alpha <- 1 - conf

  # Unique display labels so two strategies sharing a label do not merge in the
  # colour legend / table (id-level identity is already deduped upstream).
  labels <- make.unique(vapply(strategies, function(s) s$label, character(1)))

  per <- lapply(seq_along(strategies), function(i) {
    strat <- strategies[[i]]
    dist  <- strategy_distribution(strat, outcomes)
    price <- strategy_expected_price(strat)
    # THE CRN WIRING: identical uniforms for every strategy via crn = seed.
    sim   <- simulate_sessions(dist, N = N, R = R, crn = seed,
                               checkpoints = checkpoints, ...)
    metrics <- build_metrics(sim, dist, price = price, alpha = alpha)
    list(label = labels[i], strategy = strat, dist = dist,
         sim = sim, metrics = metrics, price = price)
  })
  names(per) <- labels

  # ---- comparison table: one row per strategy -------------------------------
  table <- do.call(rbind, lapply(per, function(p) {
    totals <- p$sim$totals
    data.frame(
      strategy       = p$label,
      mean_pnl       = mean(totals),
      median_pnl     = stats::median(totals),
      p_profit       = p$metrics$risk$p_profit,
      p_breakeven    = p$metrics$risk$p_breakeven,
      var_loss       = p$metrics$risk$var$loss,
      es_loss        = p$metrics$risk$es$loss,
      expected_price = p$price,
      expected_spend = p$metrics$meta$total_stake,
      stringsAsFactors = FALSE
    )
  }))
  rownames(table) <- NULL

  # ---- tidy long totals for overlay plots -----------------------------------
  totals_long <- do.call(rbind, lapply(per, function(p) {
    data.frame(
      strategy   = p$label,
      session_id = seq_along(p$sim$totals),
      pnl        = p$sim$totals,
      stringsAsFactors = FALSE
    )
  }))
  # Keep strategies in the supplied order for stable colour assignment.
  totals_long$strategy <- factor(totals_long$strategy, levels = labels)
  rownames(totals_long) <- NULL

  list(
    per         = per,
    labels      = labels,
    table       = table,
    totals_long = totals_long,
    N           = N,
    R           = R,
    seed        = seed,
    conf        = conf,
    alpha       = alpha,
    probs       = per[[1]]$sim$probs
  )
}


# ------------------------------------------------------------------------------
# run_compare_leaderboard(strategies, outcomes, N_grid, metric, R, seed)
# ------------------------------------------------------------------------------
# Assemble the low-N vs high-N CROSSOVER series across the compared strategies:
# a tidy data.frame(strategy, N, metric, value) ready for viz_leaderboard_vs_n().
#
# CRN across strategies here too: the SAME `seed` is handed to leaderboard_series
# for every strategy, so at each N every strategy's p_profit is estimated on the
# same seeded uniform stream (the default seeded path is byte-identical to a
# scalar-crn run for a given distribution -- see simulate.R). That makes the
# ranking crossover a real effect of N, not of differing sampling noise.
#
# This is deliberately its OWN (bounded) sweep, separate from run_compare()'s
# single-N run, because it costs a simulation per (strategy x N) grid point.
run_compare_leaderboard <- function(strategies, outcomes, N_grid,
                                    metric = "p_profit", R = 300, seed = 1) {
  strategies <- .compare_strategy_list(strategies)
  if (is.null(seed) || !is.finite(seed)) seed <- 1L
  seed   <- as.integer(seed)
  labels <- make.unique(vapply(strategies, function(s) s$label, character(1)))

  parts <- lapply(seq_along(strategies), function(i) {
    dist <- strategy_distribution(strategies[[i]], outcomes)
    d <- leaderboard_series(dist, N_grid = N_grid, metric = metric,
                            R = R, seed = seed)
    cbind(strategy = labels[i], d, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  out
}


# ------------------------------------------------------------------------------
# compare_table_display(compare_result)
# ------------------------------------------------------------------------------
# Turn run_compare()'s tidy `table` into a display-ready data.frame with
# human-readable, £-/%-formatted columns for a UI table (renderTable/DT). Kept
# separate from run_compare() so the raw numeric table stays available for CSV
# export and tests, and the formatting lives in one place.
compare_table_display <- function(compare_result) {
  tb <- compare_result$table
  # Use the £ escape (not a literal "£") so the string is a proper UTF-8
  # codepoint even when this file is source()'d under a non-UTF-8 (C) locale --
  # matches the convention in narrative.R and keeps grepl robust in tests.
  gbp <- function(x) paste0("\u00a3", formatC(x, format = "f", digits = 2, big.mark = ","))
  pct <- function(x) sprintf("%.1f%%", 100 * x)
  data.frame(
    Strategy              = tb$strategy,
    `Mean P&L`            = gbp(tb$mean_pnl),
    `Median P&L`          = gbp(tb$median_pnl),
    `P(profit)`           = pct(tb$p_profit),
    `P(break-even+)`      = pct(tb$p_breakeven),
    `VaR loss`            = gbp(tb$var_loss),
    `Exp. shortfall loss` = gbp(tb$es_loss),
    `Exp. price/play`     = gbp(tb$expected_price),
    `Exp. spend`          = gbp(tb$expected_spend),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}
