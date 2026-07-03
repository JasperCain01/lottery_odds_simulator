# ==============================================================================
# test-cache.R
# ------------------------------------------------------------------------------
# Phase 9 tests for the repeat-run cache (R/app_helpers.R) and the optimised
# engine path (R/simulate.R).
#
# The cache is correctness-critical: a HIT must return a result IDENTICAL to
# recomputing, and a DIFFERENT input must NEVER be served a stale (false) hit.
# These tests pin exactly that:
#   (a) a hit returns a result EQUAL to the uncached computation;
#   (b) a genuine cache hit is served WITHOUT recomputation (proven with the
#       unseeded path, where two fresh computations would differ but two cache
#       reads are identical);
#   (c) distinct inputs (seed / N / distribution) are NOT false hits;
#   (d) a row-permuted distribution IS a legitimate hit (same run to the engine,
#       which sorts by value), and equals the engine result -- so no correctness
#       is lost by treating it as a hit;
#   (e) the leaderboard-series cache round-trips and does not false-hit;
#   (f) the optimised engine path stays reproducible and equal to an independent
#       recomputation (leaning on the fuller property tests in test-simulate.R).
# ==============================================================================

# Load the layers under test (mirrors the guards in the other test files).
if (!exists("simulate_sessions_cached", mode = "function")) {
  .src <- function(rel) {
    cands <- c(rel, file.path("..", "..", rel), file.path("..", rel))
    hit <- cands[file.exists(cands)][1]
    if (is.na(hit)) stop(sprintf("Could not locate %s.", rel))
    source(hit)
  }
  .src("R/simulate.R")
  .src("R/metrics.R")
  .src("R/app_helpers.R")
}

# Hand distributions with exact, distinct support.
dist_a <- data.frame(value = c(-1, 0, 2, 10), prob = c(0.5, 0.3, 0.15, 0.05))
dist_b <- data.frame(value = c(-1, 0, 2, 20), prob = c(0.5, 0.3, 0.15, 0.05))

# A private cache per test so the process-level .pipeline_cache never leaks
# state between assertions.
fresh_cache <- function() cachem::cache_mem()


# ------------------------------------------------------------------------------
test_that("cache hit returns a result identical to the uncached computation", {
  cache <- fresh_cache()
  ref <- simulate_sessions(dist_a, N = 200, R = 500, seed = 7)          # uncached
  hit <- simulate_sessions_cached(dist_a, N = 200, R = 500, seed = 7,
                                  cache = cache)                        # cold: computes + stores
  again <- simulate_sessions_cached(dist_a, N = 200, R = 500, seed = 7,
                                    cache = cache)                      # warm: served from cache

  # Warm read is byte-identical to the cold compute AND to a plain engine call.
  expect_identical(hit$totals, again$totals)
  expect_identical(ref$totals, hit$totals)
  expect_identical(ref$checkpoint_quantiles, hit$checkpoint_quantiles)
  expect_equal(ref, again)                       # full object equality
})


# ------------------------------------------------------------------------------
test_that("a genuine cache HIT is served without recomputation (unseeded path)", {
  # With seed = NULL the engine consumes the ambient RNG stream, so two fresh
  # computations differ. If the second cached call is IDENTICAL to the first
  # despite the RNG having advanced, it can only have come from the cache.
  cache <- fresh_cache()
  set.seed(1)
  first  <- simulate_sessions_cached(dist_a, N = 50, R = 300, seed = NULL, cache = cache)
  set.seed(999)                                  # perturb global RNG state
  second <- simulate_sessions_cached(dist_a, N = 50, R = 300, seed = NULL, cache = cache)
  expect_identical(first$totals, second$totals)  # => served from cache

  # Control: WITHOUT the cache the same two unseeded calls genuinely differ,
  # proving the identity above is caused by caching, not by determinism.
  set.seed(1)
  u1 <- simulate_sessions_cached(dist_a, N = 50, R = 300, seed = NULL, cache = NULL)
  set.seed(999)
  u2 <- simulate_sessions_cached(dist_a, N = 50, R = 300, seed = NULL, cache = NULL)
  expect_false(identical(u1$totals, u2$totals))
})


# ------------------------------------------------------------------------------
test_that("different inputs are NOT false cache hits", {
  cache <- fresh_cache()

  s42 <- simulate_sessions_cached(dist_a, N = 100, R = 400, seed = 42, cache = cache)
  s43 <- simulate_sessions_cached(dist_a, N = 100, R = 400, seed = 43, cache = cache)
  expect_false(identical(s42$totals, s43$totals))
  # The seed=43 result is the CORRECT uncached one, not a stale seed=42 hit.
  expect_identical(s43$totals,
                   simulate_sessions(dist_a, N = 100, R = 400, seed = 43)$totals)

  # Different N with the same seed: distinct key, correct result.
  sN <- simulate_sessions_cached(dist_a, N = 101, R = 400, seed = 42, cache = cache)
  expect_equal(sN$N, 101L)
  expect_identical(sN$totals,
                   simulate_sessions(dist_a, N = 101, R = 400, seed = 42)$totals)

  # Different DISTRIBUTION, identical scalars: must not collide.
  rb <- simulate_sessions_cached(dist_b, N = 100, R = 400, seed = 42, cache = cache)
  expect_false(identical(s42$totals, rb$totals))
  expect_identical(rb$totals,
                   simulate_sessions(dist_b, N = 100, R = 400, seed = 42)$totals)

  # NULL seed vs seed = 0 must be distinct keys (guarded explicitly in the key).
  cache2 <- fresh_cache()
  set.seed(3)
  r_null <- simulate_sessions_cached(dist_a, N = 40, R = 200, seed = NULL, cache = cache2)
  r_zero <- simulate_sessions_cached(dist_a, N = 40, R = 200, seed = 0L, cache = cache2)
  expect_identical(r_zero$totals,
                   simulate_sessions(dist_a, N = 40, R = 200, seed = 0L)$totals)
  # (seed=0 result is deterministic; the NULL one used ambient RNG -- they must
  #  not be the same stored object.)
  expect_false(identical(r_null$totals, r_zero$totals))
})


# ------------------------------------------------------------------------------
test_that("a row-permuted distribution is a correct hit (engine sorts by value)", {
  # The engine canonicalises by sorting on value, so a row permutation is the
  # SAME run. The key canonicalises identically, so it HITS -- and that hit is
  # correct because the engine result is invariant to row order.
  cache <- fresh_cache()
  base <- simulate_sessions_cached(dist_a, N = 120, R = 500, seed = 11, cache = cache)

  dist_perm <- dist_a[c(4, 1, 3, 2), ]
  # Underlying justification: the engine itself is invariant to the permutation.
  expect_identical(simulate_sessions(dist_perm, N = 120, R = 500, seed = 11)$totals,
                   simulate_sessions(dist_a,    N = 120, R = 500, seed = 11)$totals)
  # Therefore serving the cached base result for the permuted dist is correct.
  perm <- simulate_sessions_cached(dist_perm, N = 120, R = 500, seed = 11, cache = cache)
  expect_identical(perm$totals, base$totals)
})


# ------------------------------------------------------------------------------
test_that("leaderboard_series_cached round-trips and does not false-hit", {
  cache <- fresh_cache()
  grid  <- c(10L, 50L, 100L)

  lb1 <- leaderboard_series_cached(dist_a, N_grid = grid, metric = "p_profit",
                                   R = 200, seed = 1, cache = cache)
  lb1b <- leaderboard_series_cached(dist_a, N_grid = grid, metric = "p_profit",
                                    R = 200, seed = 1, cache = cache)
  expect_identical(lb1, lb1b)                    # warm read identical
  # Equal to the uncached series (correctness of the hit).
  expect_equal(lb1,
               leaderboard_series(dist_a, N_grid = grid, metric = "p_profit",
                                  R = 200, seed = 1))

  # Different seed => different series, not a false hit.
  lb2 <- leaderboard_series_cached(dist_a, N_grid = grid, metric = "p_profit",
                                   R = 200, seed = 2, cache = cache)
  expect_false(isTRUE(all.equal(lb1$value, lb2$value)))

  # The analytical mean_pnl path needs no simulation and is exact.
  mp <- leaderboard_series_cached(dist_a, N_grid = grid, metric = "mean_pnl",
                                  R = 200, seed = 1, cache = cache)
  ev <- sum(dist_a$value * dist_a$prob)
  expect_equal(mp$value, grid * ev)
})


# ------------------------------------------------------------------------------
test_that("optimised engine path is reproducible and equals a recomputation", {
  # Leans on test-simulate.R's fuller property/chunk-invariance suite; here we
  # simply re-pin that the (Phase 9) natural-layout + colSums2 accumulation is
  # deterministic and that its last checkpoint equals the session total.
  s1 <- simulate_sessions(dist_a, N = 300, R = 800, seed = 2024)
  s2 <- simulate_sessions(dist_a, N = 300, R = 800, seed = 2024)
  expect_identical(s1$totals, s2$totals)
  ncp <- length(s1$checkpoint_plays)
  # cum_at_cp is not returned, but the final-checkpoint quantiles must equal the
  # quantiles of totals (the last checkpoint IS the session total).
  final_q <- stats::quantile(s1$totals, probs = s1$probs, names = FALSE, type = 7)
  expect_equal(as.numeric(s1$checkpoint_quantiles[ncp, ]), final_q)
})
