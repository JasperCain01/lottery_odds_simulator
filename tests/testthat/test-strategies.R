# ==============================================================================
# test-strategies.R
# ------------------------------------------------------------------------------
# Correctness + statistical property tests for Layer 4 (R/strategies.R).
#
# The crux of this layer is the claim that a weighted MIXTURE distribution is an
# EXACT model of multinomial allocation of N plays across games. That claim is
# proven two ways: (1) algebraically -- mixture probs sum to 1 and mixture EV =
# sum_g w_g EV_g, feeding the mixture to analytical_summary() gives mean N*EV;
# and (2) empirically -- simulating the built mixture matches an INDEPENDENT
# reference that explicitly draws per-game play counts ~ rmultinom then sums each
# game's own draws, within a principled Monte Carlo tolerance (proportion/mean
# SEs, fixed seeds -- no hand-tuned magic numbers). The rest (presets, filters,
# spend, budget->N) is pinned against hand-computable fixtures.
# ==============================================================================

# Ensure the layers under test are loaded (mirrors test-metrics.R's guard).
# strategies.R depends on simulate.R (game_distribution + the engine).
if (!exists("simulate_sessions", mode = "function")) {
  .cands <- c("R/simulate.R", "../../R/simulate.R", "../R/simulate.R")
  .hit   <- .cands[file.exists(.cands)][1]
  if (is.na(.hit)) stop("Could not locate R/simulate.R.")
  source(.hit)
}
if (!exists("make_strategy", mode = "function")) {
  .cands <- c("R/strategies.R", "../../R/strategies.R", "../R/strategies.R")
  .hit   <- .cands[file.exists(.cands)][1]
  if (is.na(.hit)) stop("Could not locate R/strategies.R.")
  source(.hit)
}

# ------------------------------------------------------------------------------
# Fixtures: a small, hand-computable universe.
# ------------------------------------------------------------------------------
# Purchasable games: gA, gB, gC. Non-purchasable: gH (hidden), gP (prohibited),
# gOff (off sale). Prices/RTP chosen so every extreme preset has a unique winner
# among the purchasable set:
#   cheapest = gA (£1)   priciest = gC (£5)
#   best RTP = gB (0.80) worst RTP = gC (0.50)
#   biggest jackpot = gB (top net £100)
game_summary <- data.frame(
  game_id  = c("gA", "gB", "gC", "gH", "gP", "gOff"),
  name     = c("Game A", "Game B", "Game C", "Hidden", "Prohibited", "OffSale"),
  price    = c(1, 2, 5, 0.5, 3, 2),
  category = c("Scratch", "Instant", "Scratch", "Scratch", "Instant", "Scratch"),
  source   = c("scratchcard", "instant_win", "scratchcard",
               "scratchcard", "instant_win", "scratchcard"),
  rtp      = c(0.60, 0.80, 0.50, 0.95, 0.99, 0.70),
  on_sale    = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
  is_hidden  = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
  prohibited = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE),
  stringsAsFactors = FALSE
)

# Outcomes: net_value = prize - price; the most-negative row is -price.
#   gA (£1): EV = -0.7 + 0 + 0.9 = 0.2 ;  top net 9
#   gB (£2): EV = -1.8 + 0.27 + 1 = -0.53 ; top net 100
#   gC (£5): EV = -4.5 + 4.5 = 0 ;  top net 45
outcomes <- rbind(
  data.frame(game_id = "gA", net_value = c(-1, 0, 9),  probability = c(0.7, 0.2, 0.1)),
  data.frame(game_id = "gB", net_value = c(-2, 3, 100), probability = c(0.90, 0.09, 0.01)),
  data.frame(game_id = "gC", net_value = c(-5, 45),    probability = c(0.9, 0.1)),
  data.frame(game_id = "gH", net_value = c(-0.5, 4),   probability = c(0.8, 0.2)),
  data.frame(game_id = "gP", net_value = c(-3, 60),    probability = c(0.95, 0.05)),
  data.frame(game_id = "gOff", net_value = c(-2, 8),   probability = c(0.85, 0.15))
)
outcomes$source <- "test"

# Hand EVs, reused throughout.
ev_g <- c(gA = 0.2, gB = -0.53, gC = 0.0)


# ==============================================================================
# Filters + purchasable predicate
# ==============================================================================

test_that("is_purchasable excludes hidden, prohibited, and off-sale games", {
  p <- is_purchasable(game_summary)
  names(p) <- game_summary$game_id
  expect_equal(unname(p), c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_false(p["gH"]);  expect_false(p["gP"]);  expect_false(p["gOff"])
})

test_that("filter_games composes filters and defaults to purchasable-only", {
  u <- filter_games(game_summary)                 # purchasable default
  expect_setequal(u$game_id, c("gA", "gB", "gC"))

  # Price band composes with the purchasable filter.
  u2 <- filter_games(game_summary, price_min = 2, price_max = 5)
  expect_setequal(u2$game_id, c("gB", "gC"))

  # Source and category filters.
  expect_setequal(filter_games(game_summary, source = "scratchcard")$game_id,
                  c("gA", "gC"))
  expect_setequal(filter_games(game_summary, category = "Instant")$game_id, "gB")

  # Turning off the purchasable filter lets non-purchasable games back in.
  u3 <- filter_games(game_summary, purchasable = FALSE)
  expect_true(all(c("gH", "gP", "gOff") %in% u3$game_id))
})

test_that("filter_games errors on an empty universe", {
  expect_error(filter_games(game_summary, price_min = 1000), "empty universe")
})


# ==============================================================================
# Custom mix builder: validation + normalisation
# ==============================================================================

test_that("make_strategy normalises weights and records aligned prices", {
  s <- make_strategy(c("gA", "gC"), weights = c(3, 1), game_summary = game_summary)
  expect_s3_class(s, "lottery_strategy")
  expect_equal(s$weights, c(0.75, 0.25))          # normalised to sum 1
  expect_equal(sum(s$weights), 1)
  expect_equal(s$prices, c(1, 5))                 # aligned to game_ids
  expect_equal(s$type, "mixture")
})

test_that("make_strategy defaults to uniform weights", {
  s <- make_strategy(c("gA", "gB", "gC"), game_summary = game_summary)
  expect_equal(s$weights, rep(1/3, 3))
})

test_that("make_strategy drops zero-weight games but keeps positives", {
  s <- make_strategy(c("gA", "gB", "gC"), weights = c(1, 0, 1),
                     game_summary = game_summary)
  expect_setequal(s$game_ids, c("gA", "gC"))
  expect_equal(s$weights, c(0.5, 0.5))
})

test_that("make_strategy guards bad weights and unknown/duplicate ids", {
  expect_error(make_strategy(c("gA", "gB"), c(1, -1), game_summary), "non-negative")
  expect_error(make_strategy(c("gA", "gB"), c(0, 0),  game_summary), "sum to zero")
  expect_error(make_strategy(c("gA"), c(1, 2),        game_summary), "same length")
  expect_error(make_strategy("nope",                  game_summary = game_summary), "unknown")
  expect_error(make_strategy(c("gA", "gA"),           game_summary = game_summary), "duplicate")
})


# ==============================================================================
# Mixture correctness (algebraic half of the equivalence claim)
# ==============================================================================

test_that("mixture probs sum to 1 and EV = sum_g w_g EV_g", {
  w <- c(gA = 0.5, gC = 0.5)
  s <- make_strategy(names(w), weights = unname(w), game_summary = game_summary)
  dist <- strategy_distribution(s, outcomes)

  expect_equal(sum(dist$prob), 1)                 # valid distribution
  mix_ev <- sum(dist$value * dist$prob)
  expect_equal(mix_ev, sum(w * ev_g[names(w)]))   # = 0.5*0.2 + 0.5*0 = 0.1
  expect_equal(mix_ev, 0.1)
})

test_that("aggregation merges shared net_values across games", {
  # gA and gC share NO net_values, but a mix of gA with itself-like values:
  # build a mix where two games contribute the SAME net_value (-? ) to confirm
  # the row is merged, not duplicated. gA has net 0; craft via gB? gB has 3.
  # Use gA (net -1,0,9) and a manual game overlapping at 0.
  gs2 <- rbind(game_summary,
               data.frame(game_id = "gZ", name = "Z", price = 1,
                          category = "Scratch", source = "scratchcard", rtp = 0.5,
                          on_sale = TRUE, is_hidden = FALSE, prohibited = FALSE))
  oc2 <- rbind(outcomes[, c("game_id","net_value","probability")],
               data.frame(game_id = "gZ", net_value = c(-1, 0), probability = c(0.5, 0.5)))
  s <- make_strategy(c("gA", "gZ"), weights = c(0.5, 0.5), game_summary = gs2)
  dist <- strategy_distribution(s, oc2)
  # Shared values -1 and 0 must appear once each (merged), plus gA's 9.
  expect_equal(sort(dist$value), c(-1, 0, 9))
  expect_equal(nrow(dist), 3L)
  # Merged prob at 0: 0.5*0.2 (gA) + 0.5*0.5 (gZ) = 0.35.
  expect_equal(dist$prob[dist$value == 0], 0.35)
  expect_equal(sum(dist$prob), 1)
})

test_that("analytical_summary on the mixture gives mean N * mixtureEV", {
  s    <- make_strategy(c("gA", "gC"), weights = c(0.5, 0.5), game_summary = game_summary)
  dist <- strategy_distribution(s, outcomes)
  N    <- 250
  a    <- analytical_summary(dist, N = N)
  expect_equal(a$ev_play, 0.1)
  expect_equal(a$mean, N * 0.1)
})


# ==============================================================================
# THE EQUIVALENCE PROPERTY (empirical half -- the important test)
# ==============================================================================

# Independent reference: explicit MULTINOMIAL allocation. Draw per-game play
# counts ~ rmultinom(1, N, w), draw each game's plays from its OWN distribution,
# sum to a session P&L. This shares NO code with .build_mixture(), so agreement
# is a genuine cross-check of the "mixture == multinomial allocation" claim.
.reference_multinomial_totals <- function(game_ids, weights, outcomes, N, R, seed) {
  set.seed(seed)
  dists  <- lapply(game_ids, function(g) game_distribution(g, outcomes))
  totals <- numeric(R)
  for (r in seq_len(R)) {
    counts <- as.integer(stats::rmultinom(1, size = N, prob = weights))
    s <- 0
    for (i in seq_along(game_ids)) {
      ni <- counts[i]
      if (ni > 0L) {
        d <- dists[[i]]
        s <- s + sum(sample(d$value, size = ni, replace = TRUE, prob = d$prob))
      }
    }
    totals[r] <- s
  }
  totals
}

test_that("simulated mixture matches explicit multinomial allocation (equivalence)", {
  # A genuine multi-price mix so allocation actually matters.
  gids <- c("gA", "gB", "gC")
  w    <- c(0.5, 0.3, 0.2)
  s    <- make_strategy(gids, weights = w, game_summary = game_summary)
  dist <- strategy_distribution(s, outcomes)

  N <- 40; R <- 30000
  # (1) The engine driven by the built mixture.
  sim_mix <- simulate_sessions(dist, N = N, R = R, seed = 2024)
  # (2) Independent explicit-allocation reference.
  ref     <- .reference_multinomial_totals(gids, w, outcomes, N = N, R = R, seed = 99)

  # Exact target moments (both must track these): per-play mixture moments * N.
  a <- analytical_summary(dist, N = N)
  se_mean <- a$sd / sqrt(R)                        # SE of a sample mean of totals

  # Each estimator within 4x its own mean-SE of the exact analytical mean
  # (~1e-4 exceedance under the CLT): a stable, principled, non-flaky band.
  expect_lt(abs(mean(sim_mix$totals) - a$mean), 4 * se_mean)
  expect_lt(abs(mean(ref)            - a$mean), 4 * se_mean)
  # The two independent estimators agree within their combined SE.
  expect_lt(abs(mean(sim_mix$totals) - mean(ref)), 4 * sqrt(2) * se_mean)

  # Distributions agree in spread too (sample sd within a generous relative band;
  # the sd's own sampling error is ~ sd/sqrt(2R), tiny at R=30000, so 5% is safe
  # and accounts for the heavy right tail from gB's £100 jackpot).
  expect_lt(abs(sd(sim_mix$totals) / sd(ref) - 1), 0.05)

  # And in the tails: 5th/95th percentiles match within a few percent.
  qm <- quantile(sim_mix$totals, c(.05, .95), names = FALSE, type = 7)
  qr <- quantile(ref,            c(.05, .95), names = FALSE, type = 7)
  expect_lt(max(abs(qm - qr)) / a$sd, 0.15)
})


# ==============================================================================
# Preset selection + tie-breaking
# ==============================================================================

test_that("extreme presets pick the right game over the purchasable universe", {
  u <- filter_games(game_summary)                 # gA, gB, gC
  expect_equal(strategy_cheapest(u)$game_ids, "gA")
  expect_equal(strategy_priciest(u)$game_ids, "gC")
  expect_equal(strategy_best_rtp(u)$game_ids,  "gB")
  expect_equal(strategy_worst_rtp(u)$game_ids, "gC")
  expect_equal(strategy_biggest_jackpot(u, outcomes)$game_ids, "gB")  # top net 100
})

test_that("presets ignore non-purchasable games once filtered out", {
  # Unfiltered, gH is cheapest (£0.5) and gP best RTP (0.99); the purchasable
  # filter must remove them so the presets pick from gA/gB/gC only.
  u <- filter_games(game_summary)
  expect_false("gH" %in% strategy_cheapest(u)$game_ids)
  expect_false("gP" %in% strategy_best_rtp(u)$game_ids)
})

test_that("extreme presets split ties with equal weight (documented rule)", {
  # Two games tied at the cheapest price -> equal weight across both.
  gs_tie <- data.frame(
    game_id = c("t1", "t2", "t3"),
    name    = c("T1", "T2", "T3"),
    price   = c(1, 1, 5),            # t1, t2 tied cheapest
    category = "X", source = "scratchcard",
    rtp     = c(0.7, 0.7, 0.9),      # t1, t2 tied on rtp too
    on_sale = TRUE, is_hidden = FALSE, prohibited = FALSE,
    stringsAsFactors = FALSE
  )
  s <- strategy_cheapest(gs_tie)
  expect_setequal(s$game_ids, c("t1", "t2"))
  expect_equal(s$weights, c(0.5, 0.5))
  expect_equal(s$type, "mixture")
  # A worst-RTP tie behaves the same way.
  sw <- strategy_worst_rtp(gs_tie)
  expect_setequal(sw$game_ids, c("t1", "t2"))
  expect_equal(sw$weights, c(0.5, 0.5))
})

test_that("random_each_play is uniform over the filtered universe", {
  u <- filter_games(game_summary)                 # gA, gB, gC
  s <- strategy_random_each_play(u)
  expect_setequal(s$game_ids, c("gA", "gB", "gC"))
  expect_equal(s$weights, rep(1/3, 3))
})

test_that("strategy_single is a weight-1 single-game strategy", {
  s <- strategy_single("gB", game_summary)
  expect_equal(s$game_ids, "gB")
  expect_equal(s$weights, 1)
  expect_equal(s$type, "single")
})


# ==============================================================================
# Realised spend + budget->N framing
# ==============================================================================

test_that("expected spend is N * sum_g w_g price_g; single game reduces to N*price", {
  # Mix gA(£1), gC(£5) equal weight -> E[price] = 3.
  s <- make_strategy(c("gA", "gC"), weights = c(0.5, 0.5), game_summary = game_summary)
  expect_equal(strategy_expected_price(s), 3)
  sp <- strategy_spend(s, N = 20)
  expect_equal(sp$expected_spend, 20 * 3)
  expect_true(sp$variable_spend)                  # mixed prices -> variable spend
  expect_gt(sp$spend_variance, 0)

  # Single game: deterministic spend, zero variance.
  ss <- strategy_single("gC", game_summary)       # £5
  expect_equal(strategy_expected_price(ss), 5)
  ssp <- strategy_spend(ss, N = 20)
  expect_equal(ssp$expected_spend, 100)
  expect_equal(ssp$price_variance, 0)
  expect_false(ssp$variable_spend)
})

test_that("price distribution aggregates weight by ticket price", {
  # gA(£1), gB(£2), gC(£5) with a shared price? make two £1 games to test merge.
  gs2 <- rbind(game_summary,
               data.frame(game_id = "gA2", name = "A2", price = 1,
                          category = "Scratch", source = "scratchcard", rtp = 0.6,
                          on_sale = TRUE, is_hidden = FALSE, prohibited = FALSE))
  s <- make_strategy(c("gA", "gA2", "gC"), weights = c(0.25, 0.25, 0.5),
                     game_summary = gs2)
  pd <- strategy_price_distribution(s)
  expect_equal(pd$price, c(1, 5))                 # £1 rows merged
  expect_equal(pd$prob,  c(0.5, 0.5))
  expect_equal(sum(pd$prob), 1)
})

test_that("budget_to_N uses the price for a single game", {
  s   <- strategy_single("gB", game_summary)      # £2
  fr  <- budget_to_N(s, budget_per_period = 10, periods = 52)   # £520
  expect_equal(fr$total_budget, 520)
  expect_equal(fr$spend_per_play, 2)
  expect_equal(fr$N, 260L)                        # 520 / 2
  expect_equal(fr$expected_spend, 520)
})

test_that("budget_to_N uses E[price] for a mixed-price strategy", {
  # gA(£1), gC(£5) equal weight -> E[price] = 3; £30 budget -> floor(30/3)=10.
  s  <- make_strategy(c("gA", "gC"), weights = c(0.5, 0.5), game_summary = game_summary)
  fr <- budget_to_N(s, budget_per_period = 30, periods = 1)
  expect_equal(fr$spend_per_play, 3)
  expect_equal(fr$N, 10L)
  expect_equal(fr$expected_spend, 30)
  # A budget that doesn't divide evenly floors N (expected spend <= budget).
  fr2 <- budget_to_N(s, budget_per_period = 20, periods = 1)  # 20/3 = 6.67
  expect_equal(fr2$N, 6L)
  expect_lte(fr2$expected_spend, 20)
})

test_that("budget_to_N guards non-positive budget / periods", {
  s <- strategy_single("gA", game_summary)
  expect_error(budget_to_N(s, 0),  "positive")
  expect_error(budget_to_N(s, 10, periods = 0), "positive")
})


# ==============================================================================
# End-to-end: a strategy flows through the engine AND the metrics layer
# ==============================================================================

test_that("a mixture strategy feeds build_metrics via the E[price] override", {
  # Load metrics.R if this test file is run in isolation.
  if (!exists("build_metrics", mode = "function")) {
    .cands <- c("R/metrics.R", "../../R/metrics.R", "../R/metrics.R")
    .hit   <- .cands[file.exists(.cands)][1]
    if (!is.na(.hit)) source(.hit)
  }
  skip_if_not(exists("build_metrics", mode = "function"))

  s    <- make_strategy(c("gA", "gC"), weights = c(0.5, 0.5), game_summary = game_summary)
  dist <- strategy_distribution(s, outcomes)
  N    <- 60; R <- 3000
  sim  <- simulate_sessions(dist, N = N, R = R, seed = 7)
  # Mixed stakes: pass E[price] as the stake override (a single -min(value) is
  # meaningless here). This is exactly the metrics.R price-override contract.
  bundle <- build_metrics(sim, dist, price = strategy_expected_price(s))
  expect_equal(bundle$meta$price, 3)
  expect_equal(bundle$meta$total_stake, N * 3)
  expect_true(all(bundle$streak$streaks >= 0 & bundle$streak$streaks <= N))
})
