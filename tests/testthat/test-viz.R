# ==============================================================================
# test-viz.R
# ------------------------------------------------------------------------------
# Correctness tests for Layer 6 (R/viz.R) -- the chart builders.
#
# Every builder returns a ggplot object; we introspect it WITHOUT a graphics
# device via ggplot2::ggplot_build() wherever a chart's correctness can be
# pinned down numerically (fan-chart ribbon ordering/nesting, histogram marker
# positions, transform monotonicity). One dedicated test renders an actual PNG
# via ggsave() + a real (ragg) device, proving the render path end-to-end. Edge
# cases mirror test-simulate.R's: N = 1 (one checkpoint), a degenerate
# single-value distribution, and an all-loss distribution.
# ==============================================================================

# Ensure the layers under test are loaded (mirrors test-metrics.R's guard).
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
if (!exists("viz_fan_chart", mode = "function")) {
  .cands <- c("R/viz.R", "../../R/viz.R", "../R/viz.R")
  .hit   <- .cands[file.exists(.cands)][1]
  if (is.na(.hit)) stop("Could not locate R/viz.R.")
  source(.hit)
}

# ------------------------------------------------------------------------------
# Fixtures.
# ------------------------------------------------------------------------------
dist_simple  <- data.frame(value = c(-1, 0, 2, 10), prob = c(0.5, 0.3, 0.15, 0.05))
dist_degen   <- data.frame(value = -5, prob = 1)             # always the same outcome
dist_allloss <- data.frame(value = c(-5, 0), prob = c(1, 0)) # every play loses the stake
dist_jackpot <- data.frame(value = c(-5, -4.5, 9995), prob = c(0.90, 0.0985, 0.0015))

# helper: geom class name for each layer of a ggplot, for robust layer lookup
# (position-independent, so tests don't silently break on a harmless reorder).
.geom_classes <- function(p) vapply(p$layers, function(l) class(l$geom)[1], character(1))
.layer_data   <- function(p, geom_class) {
  built <- ggplot2::ggplot_build(p)
  idx   <- which(.geom_classes(p) == geom_class)
  if (length(idx) == 0L) return(NULL)
  built$data[[idx[1]]]
}


# ==============================================================================
# 1. viz_transform_pnl() / viz_untransform_pnl() -- monotone, sign-preserving,
#    invertible-in-sign.
# ==============================================================================

test_that("transform 'none' is the identity", {
  x <- c(-100, -1, 0, 1, 100)
  expect_equal(as.numeric(viz_transform_pnl(x, "none")), x)
})

test_that("transform 'signed_log' is strictly monotone and sign-preserving", {
  set.seed(1)
  x <- c(-50000, -100, -1, 0, 1, 5, 100, 99999, sort(runif(20, -1000, 1000)))
  y <- as.numeric(viz_transform_pnl(x, "signed_log"))

  # Monotone: the RANK ORDER of x is preserved exactly by y.
  expect_equal(order(x), order(y))
  # Strictly increasing when x is sorted and has no ties.
  xs <- sort(unique(x))
  ys <- as.numeric(viz_transform_pnl(xs, "signed_log"))
  expect_true(all(diff(ys) > 0))

  # Sign-preserving: f(x) has the same sign as x (f(0) == 0 exactly).
  expect_equal(sign(y), sign(x))
  expect_equal(viz_transform_pnl(0, "signed_log")[1], 0)
})

test_that("transform 'signed_log' round-trips through viz_untransform_pnl", {
  x <- c(-50000, -37.2, -1, 0, 1, 37.2, 99999)
  y <- viz_transform_pnl(x, "signed_log")
  back <- viz_untransform_pnl(y, "signed_log")
  expect_equal(back, x, tolerance = 1e-8)
})

test_that("transform 'winsorize' clips to bounds and is monotone (non-decreasing)", {
  x <- c(-1000, -5, -1, 0, 1, 5, 1000)
  y <- as.numeric(viz_transform_pnl(x, "winsorize", winsorize_probs = c(0.2, 0.8)))
  bounds <- stats::quantile(x, probs = c(0.2, 0.8), names = FALSE, type = 7)

  expect_true(all(y >= bounds[1] - 1e-9 & y <= bounds[2] + 1e-9))
  # Monotone non-decreasing under clipping (order never inverts, ties allowed).
  ord <- order(x)
  expect_true(all(diff(y[ord]) >= -1e-12))
})

test_that("winsorize honours explicit winsorize_bounds (so markers clip to the SAME bounds as the bulk data)", {
  x <- c(-100, -1, 0, 1, 100)
  y <- viz_transform_pnl(x, "winsorize", winsorize_bounds = c(-2, 2))
  expect_equal(as.numeric(y), c(-2, -1, 0, 1, 2))
})

test_that("viz_untransform_pnl on 'winsorize' warns and returns input unchanged (not invertible)", {
  expect_warning(out <- viz_untransform_pnl(c(1, 2, 3), "winsorize"), "not invertible")
  expect_equal(out, c(1, 2, 3))
})


# ==============================================================================
# 2. viz_fan_chart() -- the checkpoint-quantile -> ribbon mapping.
# ==============================================================================

test_that("viz_fan_chart returns a ggplot with a ribbon layer where ymin <= ymax everywhere", {
  sim <- simulate_sessions(dist_simple, N = 100, R = 800, seed = 1, checkpoints = 25)
  p <- viz_fan_chart(sim)
  expect_s3_class(p, "ggplot")

  rib <- .layer_data(p, "GeomRibbon")
  expect_false(is.null(rib))
  expect_true(all(rib$ymin <= rib$ymax + 1e-9))
})

test_that("viz_fan_chart nests bands outer-contains-inner (5-95 outer, 25-75 inner)", {
  # Default probs = .05 .25 .5 .75 .95 -> 2 ribbon bands (level 1 = outer 5-95,
  # level 2 = inner 25-75) plus a bare median line.
  sim <- simulate_sessions(dist_simple, N = 150, R = 1000, seed = 2, checkpoints = 20)
  p <- viz_fan_chart(sim)
  rib <- .layer_data(p, "GeomRibbon")

  # group is a factor-derived integer; level 1 (outer) is the first group, level
  # 2 (inner) the second, by construction of .fan_chart_bands().
  outer <- rib[rib$group == min(rib$group), ]
  inner <- rib[rib$group == max(rib$group), ]
  outer <- outer[order(outer$x), ]
  inner <- inner[order(inner$x), ]

  expect_equal(nrow(outer), nrow(inner))
  expect_true(all(outer$ymin <= inner$ymin + 1e-9))
  expect_true(all(outer$ymax >= inner$ymax - 1e-9))

  # A bare median line is drawn (odd count of probs -> exactly one leftover).
  expect_true("GeomLine" %in% .geom_classes(p))
})

test_that("viz_fan_chart omits the bare median line for an EVEN count of probs", {
  sim <- simulate_sessions(dist_simple, N = 100, R = 500, seed = 3, checkpoints = 15,
                           probs = c(.1, .3, .7, .9))
  p <- viz_fan_chart(sim)
  # Exactly ONE geom_line layer should remain: the mean reference line. If the
  # (nonexistent) bare median were drawn there would be two.
  expect_equal(sum(.geom_classes(p) == "GeomLine"), 1L)
})

test_that("viz_fan_chart with a single median prob draws a median line and no ribbon", {
  sim <- simulate_sessions(dist_simple, N = 100, R = 500, seed = 4, checkpoints = 15,
                           probs = c(.5))
  p <- viz_fan_chart(sim)
  expect_false("GeomRibbon" %in% .geom_classes(p))
  # Median line + mean line = 2 GeomLine layers.
  expect_equal(sum(.geom_classes(p) == "GeomLine"), 2L)

  # .layer_data() picks out the FIRST GeomLine layer, which is the median line
  # (added before the mean reference line): one row per checkpoint.
  med <- .layer_data(p, "GeomLine")
  expect_equal(nrow(med), length(sim$checkpoint_plays))
  expect_equal(med$y[order(med$x)], as.numeric(sim$checkpoint_quantiles[, 1]))
})

test_that("viz_fan_chart handles N = 1 (a single checkpoint) without error", {
  sim <- simulate_sessions(dist_simple, N = 1, R = 200, seed = 5)
  expect_equal(length(sim$checkpoint_plays), 1L)
  p <- viz_fan_chart(sim)
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)  # must not error with a single x point
  expect_true(is.list(built$data))
})

test_that("viz_fan_chart_alt returns a single non-empty description string", {
  sim <- simulate_sessions(dist_simple, N = 50, R = 300, seed = 6, checkpoints = 10)
  alt <- viz_fan_chart_alt(sim)
  expect_type(alt, "character")
  expect_length(alt, 1L)
  expect_true(nchar(alt) > 20)
})


# ==============================================================================
# 3. viz_pnl_hist() -- histogram + break-even/mean/median/percentile markers.
# ==============================================================================

test_that("viz_pnl_hist marker vlines sit exactly at 0, mean, median, and the requested percentiles", {
  sim <- simulate_sessions(dist_simple, N = 60, R = 900, seed = 7, checkpoints = 10)
  totals <- sim$totals
  p <- viz_pnl_hist(sim, transform = "none", marker_probs = c(0.1, 0.9))

  vl <- .layer_data(p, "GeomVline")
  expect_false(is.null(vl))

  expected <- sort(unique(round(c(
    0, mean(totals), stats::median(totals),
    stats::quantile(totals, probs = c(0.1, 0.9), names = FALSE, type = 7)
  ), 8)))
  observed <- sort(round(vl$xintercept, 8))
  expect_equal(observed, expected)
})

test_that("viz_pnl_hist under 'signed_log' places transformed markers exactly at f(raw value)", {
  sim <- simulate_sessions(dist_simple, N = 60, R = 900, seed = 7, checkpoints = 10)
  totals <- sim$totals
  p <- viz_pnl_hist(sim, transform = "signed_log", marker_probs = c(0.05, 0.95))
  vl <- .layer_data(p, "GeomVline")

  raw <- c(0, mean(totals), stats::median(totals),
          stats::quantile(totals, probs = c(0.05, 0.95), names = FALSE, type = 7))
  expected <- sort(unique(round(as.numeric(viz_transform_pnl(raw, "signed_log")), 8)))
  observed <- sort(round(vl$xintercept, 8))
  expect_equal(observed, expected)
})

test_that("viz_pnl_hist handles a degenerate single-value distribution", {
  sim <- simulate_sessions(dist_degen, N = 20, R = 100, seed = 8)
  expect_true(all(sim$totals == sim$totals[1]))  # every session totals the same
  p <- viz_pnl_hist(sim)
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  expect_true(is.list(built$data))
})

test_that("viz_pnl_hist handles an all-loss distribution", {
  sim <- simulate_sessions(dist_allloss, N = 30, R = 200, seed = 9)
  expect_true(all(sim$totals == -30 * 5))
  p <- viz_pnl_hist(sim, transform = "winsorize")
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  expect_true(is.list(built$data))
})

test_that("viz_pnl_hist_alt reports the RAW (untransformed) mean/median regardless of display transform", {
  sim <- simulate_sessions(dist_simple, N = 60, R = 900, seed = 7, checkpoints = 10)
  alt <- viz_pnl_hist_alt(sim)
  expect_type(alt, "character")
  expect_true(grepl(sprintf("%.2f", mean(sim$totals)), alt, fixed = TRUE))
})


# ==============================================================================
# 4. viz_dream_vs_reality() -- full vs conditional-on-no-jackpot.
# ==============================================================================

test_that("viz_dream_vs_reality bars equal metrics$dream's full/conditional mean_session", {
  sim <- simulate_sessions(dist_jackpot, N = 50, R = 500, seed = 10, checkpoints = 10)
  m   <- build_metrics(sim, dist_jackpot, price = 5)
  p   <- viz_dream_vs_reality(m)
  expect_s3_class(p, "ggplot")

  col <- .layer_data(p, "GeomCol")
  expect_false(is.null(col))
  # geom_col's built $y is the STACKING top (0 for a negative bar under
  # position_stack()); the actual signed bar height is whichever of
  # ymin/ymax is farther from zero.
  bar_value <- ifelse(abs(col$ymin) > abs(col$ymax), col$ymin, col$ymax)
  expect_equal(sort(bar_value), sort(c(m$dream$full$mean_session, m$dream$conditional$mean_session)))
})

test_that("viz_dream_vs_reality handles the degenerate all-mass-on-top-tier case", {
  # Every outcome IS the top tier: p_top == 1, conditional is NA/empty.
  dist_alltop <- data.frame(value = 100, prob = 1)
  sim <- simulate_sessions(dist_alltop, N = 5, R = 50, seed = 11, checkpoints = 5)
  m   <- build_metrics(sim, dist_alltop, price = 5)
  expect_true(is.na(m$dream$conditional$mean_session))
  p <- viz_dream_vs_reality(m)
  expect_s3_class(p, "ggplot")
  # NA bar height should not error ggplot_build (it's simply omitted from the plot).
  built <- ggplot2::ggplot_build(p)
  expect_true(is.list(built$data))
})

test_that("viz_dream_vs_reality_alt returns a non-empty description string", {
  sim <- simulate_sessions(dist_jackpot, N = 50, R = 500, seed = 10, checkpoints = 10)
  m   <- build_metrics(sim, dist_jackpot, price = 5)
  alt <- viz_dream_vs_reality_alt(m)
  expect_type(alt, "character")
  expect_true(nchar(alt) > 20)
})


# ==============================================================================
# 5. viz_leaderboard_vs_n() / viz_leaderboard_table() -- tidy series -> chart/table.
# ==============================================================================

test_that("viz_leaderboard_vs_n draws exactly the analytical mean_pnl values per strategy", {
  N_grid <- c(10L, 50L, 100L)
  dA <- data.frame(value = c(-1, 5), prob = c(0.8, 0.2))
  dB <- data.frame(value = c(-1, 2), prob = c(0.5, 0.5))
  series <- rbind(
    cbind(strategy = "A", leaderboard_series(dA, N_grid, metric = "mean_pnl")),
    cbind(strategy = "B", leaderboard_series(dB, N_grid, metric = "mean_pnl"))
  )
  p <- viz_leaderboard_vs_n(series)
  expect_s3_class(p, "ggplot")

  ln <- .layer_data(p, "GeomLine")
  expect_equal(sort(ln$y), sort(series$value))
})

test_that("viz_leaderboard_table pivots to one row per strategy, one column per N", {
  N_grid <- c(10L, 50L)
  d <- data.frame(value = c(-1, 5), prob = c(0.8, 0.2))
  series <- rbind(
    cbind(strategy = "A", leaderboard_series(d, N_grid, metric = "mean_pnl")),
    cbind(strategy = "B", leaderboard_series(d, N_grid, metric = "mean_pnl"))
  )
  tbl <- viz_leaderboard_table(series)
  expect_equal(nrow(tbl), 2L)
  expect_true(all(c("strategy", "N=10", "N=50") %in% names(tbl)))
})

test_that("viz_leaderboard_vs_n_alt names the leading strategy at min and max N", {
  N_grid <- c(10L, 1000L)
  dA <- data.frame(value = c(-1, 50), prob = c(0.9, 0.1))   # better at low N (fat tail)
  dB <- data.frame(value = c(-1, 0.5), prob = c(0.5, 0.5))  # better EV at high N
  series <- rbind(
    cbind(strategy = "A", leaderboard_series(dA, N_grid, metric = "p_profit", R = 500, seed = 1)),
    cbind(strategy = "B", leaderboard_series(dB, N_grid, metric = "p_profit", R = 500, seed = 1))
  )
  alt <- viz_leaderboard_vs_n_alt(series)
  expect_type(alt, "character")
  expect_true(grepl("N = 10", alt, fixed = TRUE))
})


# ==============================================================================
# 6. viz_play_by_play() -- single-session cumulative path + win/loss marking.
# ==============================================================================

test_that("viz_play_by_play's cumulative path matches cumsum() of the session exactly", {
  one <- simulate_one_session(dist_simple, N = 40, seed = 12)
  p <- viz_play_by_play(one, transform = "none")
  expect_s3_class(p, "ggplot")

  pts <- .layer_data(p, "GeomPoint")
  expect_equal(pts$y[order(pts$x)], cumsum(one))
})

test_that("viz_play_by_play classifies win/loss by price when supplied (matches engagement_metrics' definition)", {
  one <- simulate_one_session(dist_simple, N = 60, seed = 13)
  price <- 1  # -min(dist_simple$value) == 1
  p <- viz_play_by_play(one, price = price)
  pts <- .layer_data(p, "GeomPoint")
  # 2 colour groups (Win/Loss) -> 2 distinct 'colour' values used.
  expect_equal(length(unique(pts$colour)), length(unique(one > -price)))
})

test_that("viz_play_by_play handles an all-loss single session", {
  one <- simulate_one_session(dist_allloss, N = 25, seed = 14)
  expect_true(all(one == -5))
  p <- viz_play_by_play(one, price = 5)
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  expect_true(is.list(built$data))
})

test_that("viz_play_by_play_alt reports the correct win count and final balance", {
  one <- simulate_one_session(dist_simple, N = 40, seed = 12)
  alt <- viz_play_by_play_alt(one, price = 1)
  expect_type(alt, "character")
  expect_true(grepl(sprintf("%d of %d", sum(one > -1), 40), alt, fixed = TRUE))
})


# ==============================================================================
# 7. Render-to-PNG smoke test -- proves the render path end-to-end (ragg).
# ==============================================================================

test_that("charts render to an actual non-empty PNG file via ggsave()", {
  sim <- simulate_sessions(dist_simple, N = 80, R = 400, seed = 15, checkpoints = 20)

  f1 <- tempfile(fileext = ".png")
  ggplot2::ggsave(f1, viz_fan_chart(sim), width = 7, height = 5, dpi = 72)
  expect_true(file.exists(f1))
  expect_gt(file.info(f1)$size, 0)

  f2 <- tempfile(fileext = ".png")
  ggplot2::ggsave(f2, viz_pnl_hist(sim, transform = "signed_log"), width = 7, height = 5, dpi = 72)
  expect_true(file.exists(f2))
  expect_gt(file.info(f2)$size, 0)

  unlink(c(f1, f2))
})
