# ==============================================================================
# strategies.R
# ------------------------------------------------------------------------------
# Layer 4 of the lottery odds simulator (see IMPLEMENTATION_PLAN.md, sections
# 4 and Phase 4).
#
# Purpose: define how plays are allocated across games, and present a uniform
# strategy interface that simulate.R / metrics.R can consume regardless of
# whether it is a single-game preset or a custom multi-game mix.
#   - presets: single game, always cheapest, always priciest, best RTP,
#     worst RTP, biggest jackpot, random-each-play
#   - custom mix builder: games + weights, multinomial allocation of plays
#     under a fixed N (mixed ticket prices mean variable realised spend --
#     documented and EXPOSED, not hidden)
#   - filters: on-sale, price, category, source, plus a "purchasable" predicate
#   - budget/time framing: translate "£X per week for T weeks" into an N
#
# ------------------------------------------------------------------------------
# THE CENTRAL EQUIVALENCE (why this layer is thin over the existing engine)
# ------------------------------------------------------------------------------
# A strategy that plays a weighted mix of games with MULTINOMIAL allocation of
# the N plays -- each play independently picks game g with probability w_g,
# sum(w_g) = 1 -- has a per-play net-outcome law that is EXACTLY the weighted
# mixture of the games' (value, prob) distributions:
#
#       mix_prob(v) = sum_g  w_g * P_g(v).
#
# Proof sketch: a single play's net outcome X is drawn by first choosing a game
# G (P(G=g) = w_g) then drawing X ~ P_G. By the law of total probability,
# P(X = v) = sum_g P(G = g) P(X = v | G = g) = sum_g w_g P_g(v). The N plays are
# i.i.d., so session P&L = sum of N i.i.d. draws from that ONE mixture law.
#
# Consequence: a mix strategy COLLAPSES to a single data.frame(value, prob) that
# the existing engine consumes unchanged -- no new simulation path is needed.
# We build the mixture by unioning the games' outcomes and summing weight-scaled
# probabilities (aggregating identical net_values so the distribution is tidy),
# then hand it to simulate_sessions() / analytical_summary() / build_metrics()
# exactly like a single game. test-strategies.R proves this equivalence against
# an independent explicit-multinomial-allocation reference.
#
# ------------------------------------------------------------------------------
# MIXED-PRICE SPEND (the documented risk, exposed not hidden)
# ------------------------------------------------------------------------------
# The mixture above is over NET outcomes (net_value = prize - price), so it is a
# complete and correct model of session P&L. What it does NOT pin down is the
# realised GROSS spend: with games of different ticket prices, a fixed number of
# plays N buys a RANDOM total spend, because each play's stake is itself drawn
# from the games' price mix. We expose this explicitly rather than pretend N*C:
#   - E[price]  = sum_g w_g * price_g          (expected per-play spend)
#   - Var[price]= sum_g w_g * price_g^2 - E[price]^2
#   - a companion per-play PRICE distribution (strategy_price_distribution())
#   - realised session spend: E = N*E[price], Var = N*Var[price] (i.i.d. plays)
# For metrics, a mixture's stake is passed via the `price` override as E[price];
# a single -min(value) is meaningless across mixed stakes (see metrics.R's
# .derive_price note, which anticipates exactly this).
#
# source()-ing this file is side-effect free: it defines functions only. It
# assumes simulate.R is already sourced (for game_distribution()).
# ==============================================================================


# ------------------------------------------------------------------------------
# .require_summary_cols(game_summary, cols)
# ------------------------------------------------------------------------------
# Internal: assert the game_summary carries the columns a routine needs, failing
# with a specific message rather than a downstream `$<NULL>` surprise.
.require_summary_cols <- function(game_summary, cols) {
  if (!is.data.frame(game_summary)) {
    stop("game_summary must be a data.frame.", call. = FALSE)
  }
  miss <- setdiff(cols, names(game_summary))
  if (length(miss)) {
    stop(sprintf("game_summary is missing column(s): %s.",
                 paste(miss, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}


# ------------------------------------------------------------------------------
# is_purchasable(game_summary)
# ------------------------------------------------------------------------------
# The documented "a player can actually buy this" predicate. A game is
# purchasable iff it is on sale AND not hidden AND not prohibited:
#
#       on_sale & !is_hidden & !prohibited
#
# WHY all three: `on_sale` alone leaks games that are technically listed but
# hidden from the operator's catalogue, or that have been prohibited/withdrawn
# (see IMPLEMENTATION_PLAN.md section 8 -- "prohibited/hidden/expired games
# leaking into presets"). Presets and the mix builder default to selecting only
# from purchasable games so a strategy can never allocate plays to a ticket a
# player cannot lawfully buy. Returns a logical vector aligned to the rows.
is_purchasable <- function(game_summary) {
  .require_summary_cols(game_summary, c("on_sale", "is_hidden", "prohibited"))
  as.logical(game_summary$on_sale) &
    !as.logical(game_summary$is_hidden) &
    !as.logical(game_summary$prohibited)
}


# ------------------------------------------------------------------------------
# filter_games(game_summary, purchasable, on_sale, price_min, price_max,
#              category, source)
# ------------------------------------------------------------------------------
# Compose filters over game_summary and return the surviving rows. Every filter
# is optional (NULL = "don't constrain on this axis") so they COMPOSE freely:
#   - purchasable  : if TRUE (default) keep only is_purchasable() rows.
#   - on_sale      : if non-NULL, keep rows whose on_sale equals it (a finer
#                    knob than `purchasable`, e.g. to inspect off-sale games).
#   - price_min/max: inclusive price band in £.
#   - category     : keep rows whose category is in this set.
#   - source       : keep rows whose source is in this set (scratchcard vs
#                    instant_win).
# Guard: an empty surviving universe is almost always a user/UI mistake (e.g. a
# price band that matches nothing), so we ERROR with a clear message rather than
# hand back a zero-row frame that would blow up later in a preset constructor.
filter_games <- function(game_summary,
                         purchasable = TRUE,
                         on_sale     = NULL,
                         price_min   = NULL,
                         price_max   = NULL,
                         category    = NULL,
                         source      = NULL) {
  .require_summary_cols(game_summary, c("game_id", "price", "category", "source"))
  keep <- rep(TRUE, nrow(game_summary))

  if (isTRUE(purchasable))      keep <- keep & is_purchasable(game_summary)
  if (!is.null(on_sale))        keep <- keep & (as.logical(game_summary$on_sale) == on_sale)
  if (!is.null(price_min))      keep <- keep & (game_summary$price >= price_min)
  if (!is.null(price_max))      keep <- keep & (game_summary$price <= price_max)
  if (!is.null(category))       keep <- keep & (game_summary$category %in% category)
  if (!is.null(source))         keep <- keep & (game_summary$source %in% source)

  out <- game_summary[keep, , drop = FALSE]
  if (nrow(out) == 0L) {
    stop("filter_games: no games satisfy the requested filters (empty universe).",
         call. = FALSE)
  }
  rownames(out) <- NULL
  out
}


# ------------------------------------------------------------------------------
# .new_strategy(id, label, description, game_ids, weights, prices)
# ------------------------------------------------------------------------------
# Internal: the single constructor for the uniform strategy object. Every preset
# and the custom builder funnel through here so the object shape is identical
# regardless of origin -- a single game is just a one-element mixture with
# weight 1. Carries everything needed to (a) build the engine distribution and
# (b) reason about realised spend, WITHOUT holding the outcomes table itself
# (the distribution is materialised on demand by strategy_distribution(), so a
# strategy object is small and serialisable).
#
# Fields:
#   id          machine id (stable, e.g. "preset:cheapest").
#   label       short human label for the UI.
#   description one-line explanation (incl. tie-breaking where relevant).
#   type        "single" (one game) or "mixture" (>1 game).
#   game_ids    character vector of selected games.
#   weights     numeric, normalised to sum 1, aligned to game_ids.
#   prices      numeric ticket prices (£), aligned to game_ids.
.new_strategy <- function(id, label, description, game_ids, weights, prices) {
  structure(
    list(
      id          = id,
      label       = label,
      description = description,
      type        = if (length(game_ids) == 1L) "single" else "mixture",
      game_ids    = game_ids,
      weights     = weights,
      prices      = prices
    ),
    class = "lottery_strategy"
  )
}

# A compact printer so a strategy is legible at the console / in test output.
print.lottery_strategy <- function(x, ...) {
  cat(sprintf("<lottery_strategy> %s [%s]\n", x$label, x$type))
  cat(sprintf("  id: %s\n", x$id))
  cat(sprintf("  %s\n", x$description))
  cat(sprintf("  E[price] per play: £%.4g   games: %d\n",
              strategy_expected_price(x), length(x$game_ids)))
  for (i in seq_along(x$game_ids)) {
    cat(sprintf("    - %-40s w=%.4f  £%s\n",
                x$game_ids[i], x$weights[i], format(x$prices[i])))
  }
  invisible(x)
}


# ------------------------------------------------------------------------------
# make_strategy(game_ids, weights, game_summary, id, label, description)
# ------------------------------------------------------------------------------
# The custom MIX BUILDER and the shared validation core for every preset. Takes
# a set of game_ids and (optional) weights, validates them against the supplied
# game_summary, normalises the weights to sum 1, and returns a strategy object.
#
# Weight rules (all guarded, with specific errors):
#   - weights default to UNIFORM if omitted.
#   - length must match game_ids.
#   - no NA / non-finite / NEGATIVE weights (a negative allocation is nonsense).
#   - at least one STRICTLY POSITIVE weight (all-zero has nothing to play).
#   - zero-weight games are DROPPED: they contribute nothing to the mixture and
#     would only pollute the spend distribution. (Documented so a caller who
#     passes a zero isn't surprised the game vanished from the object.)
#   - unknown game_ids (absent from game_summary) are rejected up front.
#   - duplicate game_ids are rejected (ambiguous weight; combine them instead).
make_strategy <- function(game_ids,
                          weights     = NULL,
                          game_summary,
                          id          = "custom:mix",
                          label       = "Custom mix",
                          description = "User-defined weighted mix of games.") {
  .require_summary_cols(game_summary, c("game_id", "price"))
  game_ids <- as.character(game_ids)
  if (length(game_ids) == 0L) stop("make_strategy: no game_ids supplied.", call. = FALSE)
  if (anyDuplicated(game_ids)) {
    stop("make_strategy: duplicate game_ids (combine their weights instead).",
         call. = FALSE)
  }

  # Unknown ids -> hard error (never silently drop a game the caller asked for).
  unknown <- setdiff(game_ids, game_summary$game_id)
  if (length(unknown)) {
    stop(sprintf("make_strategy: unknown game_id(s): %s.",
                 paste(unknown, collapse = ", ")), call. = FALSE)
  }

  # Weights: default uniform, then validate.
  if (is.null(weights)) weights <- rep(1, length(game_ids))
  weights <- as.numeric(weights)
  if (length(weights) != length(game_ids)) {
    stop("make_strategy: weights must be the same length as game_ids.", call. = FALSE)
  }
  if (any(!is.finite(weights))) stop("make_strategy: weights must be finite.", call. = FALSE)
  if (any(weights < 0))         stop("make_strategy: weights must be non-negative.", call. = FALSE)
  if (sum(weights) <= 0)        stop("make_strategy: weights sum to zero (nothing to play).", call. = FALSE)

  # Drop zero-weight games (they add nothing), then normalise to sum 1.
  keep     <- weights > 0
  game_ids <- game_ids[keep]
  weights  <- weights[keep]
  weights  <- weights / sum(weights)

  # Ticket prices aligned to the (surviving, ordered) game_ids.
  prices <- game_summary$price[match(game_ids, game_summary$game_id)]

  .new_strategy(id, label, description, game_ids, weights, prices)
}


# ------------------------------------------------------------------------------
# .build_mixture(game_ids, weights, outcomes)
# ------------------------------------------------------------------------------
# Internal: materialise the weighted mixture distribution (see THE CENTRAL
# EQUIVALENCE header). Union each game's (value, prob), scale probs by the game
# weight, then AGGREGATE identical net_values by summing their weighted probs so
# the returned distribution has one row per distinct net_value and is tidy.
#
# WHY aggregate on the numeric value directly (via match into sorted uniques)
# rather than coercing to character keys: it is exact for the doubles involved
# and avoids any as.character() round-trip precision worry. The result sums to 1
# because each game's probs sum to 1 and the weights sum to 1.
.build_mixture <- function(game_ids, weights, outcomes) {
  parts <- lapply(seq_along(game_ids), function(i) {
    d <- game_distribution(game_ids[i], outcomes)   # generic (value, prob)
    d$prob <- d$prob * weights[i]
    d
  })
  all <- do.call(rbind, parts)

  uv   <- sort(unique(all$value))
  idx  <- match(all$value, uv)                      # group key into sorted uniques
  prob <- as.numeric(tapply(all$prob, idx, sum))    # ordered by idx = uv order
  data.frame(value = uv, prob = prob)
}


# ------------------------------------------------------------------------------
# strategy_distribution(strategy, outcomes)
# ------------------------------------------------------------------------------
# THE bridge from a strategy object to the engine: returns the data.frame(value,
# prob) mixture to hand straight to simulate_sessions() / analytical_summary() /
# build_metrics(). For a single-game strategy this is just that game's
# distribution; for a mixture it is the weighted blend. Identical interface
# either way -- the whole point of the layer.
strategy_distribution <- function(strategy, outcomes) {
  stopifnot(inherits(strategy, "lottery_strategy"))
  .build_mixture(strategy$game_ids, strategy$weights, outcomes)
}


# ------------------------------------------------------------------------------
# strategy_expected_price(strategy)  /  strategy_price_variance(strategy)
# strategy_price_distribution(strategy)
# ------------------------------------------------------------------------------
# The realised-spend surface (see MIXED-PRICE SPEND header). Expected per-play
# spend is the weighted mean stake; its variance is the weighted stake variance
# (zero for a single game, or any mix of equal-priced games). The companion
# price distribution aggregates weight by distinct ticket price -- the per-play
# GROSS spend law, the mirror of the net-outcome mixture.
strategy_expected_price <- function(strategy) {
  stopifnot(inherits(strategy, "lottery_strategy"))
  sum(strategy$weights * strategy$prices)
}

strategy_price_variance <- function(strategy) {
  stopifnot(inherits(strategy, "lottery_strategy"))
  ep  <- sum(strategy$weights * strategy$prices)
  ex2 <- sum(strategy$weights * strategy$prices^2)
  v   <- ex2 - ep^2
  if (v < 0) v <- 0            # cancellation guard (as in simulate.R/metrics.R)
  v
}

strategy_price_distribution <- function(strategy) {
  stopifnot(inherits(strategy, "lottery_strategy"))
  up   <- sort(unique(strategy$prices))
  idx  <- match(strategy$prices, up)
  prob <- as.numeric(tapply(strategy$weights, idx, sum))
  data.frame(price = up, prob = prob)
}


# ------------------------------------------------------------------------------
# strategy_spend(strategy, N)
# ------------------------------------------------------------------------------
# Realised session spend framing for a fixed number of plays N. Because plays
# are i.i.d. draws from the per-play price mix, total spend has:
#   mean = N * E[price],  var = N * Var[price],  sd = sqrt(var).
# For a single game (or any equal-priced mix) Var[price] = 0, so spend is the
# deterministic N * price -- the mixed-price variability is surfaced ONLY when it
# genuinely exists. Returns both the expectation and its spread so the UI can
# show "you'll spend about £X, give or take £Y".
strategy_spend <- function(strategy, N) {
  stopifnot(inherits(strategy, "lottery_strategy"))
  if (N < 1 || N != as.integer(N)) stop("N must be a positive integer.", call. = FALSE)
  N  <- as.integer(N)
  ep <- strategy_expected_price(strategy)
  vp <- strategy_price_variance(strategy)
  list(
    N                     = N,
    expected_price        = ep,          # E[price] per play
    price_variance        = vp,          # Var[price] per play
    expected_spend        = N * ep,      # E[total spend over N plays]
    spend_variance        = N * vp,      # Var[total spend] (0 if equal-priced)
    spend_sd              = sqrt(N * vp),
    variable_spend        = vp > 0       # TRUE iff mixed ticket prices
  )
}


# ------------------------------------------------------------------------------
# budget_to_N(strategy, budget_per_period, periods)
# ------------------------------------------------------------------------------
# Translate a "£X per period for T periods" budget into a play count N:
#   total_budget   = budget_per_period * periods            (e.g. £10/wk * 52)
#   spend_per_play = E[price]  (== price for a single game) (see below)
#   N              = floor(total_budget / spend_per_play)
#
# WHY E[price] for a mixture (the documented modelling choice): under multinomial
# allocation the per-play stake is random, so there is no single ticket price to
# divide by. The EXPECTED spend per play is the natural, unbiased budget divisor
# -- it makes E[total spend] ~ budget. The realised spend then varies around the
# budget (quantified by strategy_spend()); floor() keeps the EXPECTED spend at or
# under budget. Returns the full realised framing so the UI can show the honest
# "your £X buys about N plays, expected spend £S" picture.
budget_to_N <- function(strategy, budget_per_period, periods = 1) {
  stopifnot(inherits(strategy, "lottery_strategy"))
  if (!is.finite(budget_per_period) || budget_per_period <= 0) {
    stop("budget_per_period must be a positive number.", call. = FALSE)
  }
  if (!is.finite(periods) || periods <= 0) {
    stop("periods must be a positive number.", call. = FALSE)
  }
  total_budget   <- budget_per_period * periods
  spend_per_play <- strategy_expected_price(strategy)
  N <- as.integer(floor(total_budget / spend_per_play))
  list(
    budget_per_period = budget_per_period,
    periods           = periods,
    total_budget      = total_budget,
    spend_per_play    = spend_per_play,   # E[price] (single game: the price)
    N                 = N,
    expected_spend    = N * spend_per_play
  )
}


# ==============================================================================
# PRESET STRATEGY CONSTRUCTORS
# ------------------------------------------------------------------------------
# Each preset selects over the universe it is HANDED (filter first with
# filter_games(), then build a preset over the survivors). Extreme presets
# (cheapest / priciest / best-RTP / worst-RTP / biggest-jackpot) use a single,
# documented TIE-BREAKING rule: EQUAL WEIGHT across every game tied at the
# extreme. WHY equal-weight-on-ties rather than an arbitrary "first row" pick:
# it is deterministic, order-independent (the answer does not depend on how the
# universe happened to be sorted), and faithful to the preset's intent ("play
# the cheapest" with three £1 games means play those three, not an arbitrary one
# of them). A lone extreme reduces to a single-game strategy automatically.
# ==============================================================================

# ------------------------------------------------------------------------------
# .extreme_ids(game_summary, values, which)
# ------------------------------------------------------------------------------
# Internal: the game_ids whose `values` attain the min/max, within a small fp
# tolerance so genuine ties are not split by rounding noise.
.extreme_ids <- function(game_summary, values, which = c("max", "min")) {
  which <- match.arg(which)
  target <- if (which == "max") max(values) else min(values)
  tol    <- 1e-9 * max(1, abs(target))
  game_summary$game_id[abs(values - target) <= tol]
}


# ------------------------------------------------------------------------------
# strategy_single(game_id, game_summary, ...)
# ------------------------------------------------------------------------------
# Play one named game every play. The degenerate (weight-1) strategy; still goes
# through make_strategy() so validation and the object shape are shared.
strategy_single <- function(game_id, game_summary,
                            label = NULL, description = NULL) {
  game_id <- as.character(game_id)
  if (length(game_id) != 1L) stop("strategy_single: supply exactly one game_id.", call. = FALSE)
  nm <- if ("name" %in% names(game_summary)) {
    game_summary$name[match(game_id, game_summary$game_id)]
  } else game_id
  make_strategy(
    game_ids     = game_id,
    weights      = 1,
    game_summary = game_summary,
    id           = paste0("single:", game_id),
    label        = if (is.null(label)) paste0("Single: ", nm) else label,
    description  = if (is.null(description))
      sprintf("Play '%s' on every play.", nm) else description
  )
}


# ------------------------------------------------------------------------------
# strategy_cheapest / strategy_priciest (by ticket price)
# ------------------------------------------------------------------------------
strategy_cheapest <- function(game_summary) {
  .require_summary_cols(game_summary, c("game_id", "price"))
  ids <- .extreme_ids(game_summary, game_summary$price, "min")
  make_strategy(ids, weights = rep(1, length(ids)), game_summary = game_summary,
                id = "preset:cheapest", label = "Always cheapest",
                description = sprintf(
                  "Play the cheapest ticket(s) (min price £%s); equal weight across %d tie(s).",
                  format(min(game_summary$price)), length(ids)))
}

strategy_priciest <- function(game_summary) {
  .require_summary_cols(game_summary, c("game_id", "price"))
  ids <- .extreme_ids(game_summary, game_summary$price, "max")
  make_strategy(ids, weights = rep(1, length(ids)), game_summary = game_summary,
                id = "preset:priciest", label = "Always priciest",
                description = sprintf(
                  "Play the priciest ticket(s) (max price £%s); equal weight across %d tie(s).",
                  format(max(game_summary$price)), length(ids)))
}


# ------------------------------------------------------------------------------
# strategy_best_rtp / strategy_worst_rtp (by return-to-player)
# ------------------------------------------------------------------------------
strategy_best_rtp <- function(game_summary) {
  .require_summary_cols(game_summary, c("game_id", "rtp"))
  ids <- .extreme_ids(game_summary, game_summary$rtp, "max")
  make_strategy(ids, weights = rep(1, length(ids)), game_summary = game_summary,
                id = "preset:best_rtp", label = "Best RTP",
                description = sprintf(
                  "Play the highest-RTP game(s) (max RTP %.4g); equal weight across %d tie(s).",
                  max(game_summary$rtp), length(ids)))
}

strategy_worst_rtp <- function(game_summary) {
  .require_summary_cols(game_summary, c("game_id", "rtp"))
  ids <- .extreme_ids(game_summary, game_summary$rtp, "min")
  make_strategy(ids, weights = rep(1, length(ids)), game_summary = game_summary,
                id = "preset:worst_rtp", label = "Worst RTP",
                description = sprintf(
                  "Play the lowest-RTP game(s) (min RTP %.4g); equal weight across %d tie(s).",
                  min(game_summary$rtp), length(ids)))
}


# ------------------------------------------------------------------------------
# strategy_biggest_jackpot(game_summary, outcomes)
# ------------------------------------------------------------------------------
# Play the game with the largest TOP PRIZE. There is no jackpot column in
# game_summary (see the data schema note), so "biggest jackpot" is DERIVED as
# the game with the maximum top net_value over the outcomes table. WHY top
# net_value (not gross prize): the engine and the whole app reason in net terms
# (prize - price), and the top net outcome is the headline "dream" a player is
# sold. Only games present in the supplied (already-filtered) universe are
# considered. Ties: equal weight, as for the other extremes.
strategy_biggest_jackpot <- function(game_summary, outcomes) {
  .require_summary_cols(game_summary, "game_id")
  if (!all(c("game_id", "net_value") %in% names(outcomes))) {
    stop("outcomes must have columns game_id, net_value.", call. = FALSE)
  }
  ids <- game_summary$game_id
  oc  <- outcomes[outcomes$game_id %in% ids, , drop = FALSE]
  # Top (max) net_value per game in the universe.
  top <- tapply(oc$net_value, factor(oc$game_id, levels = ids), max)
  top <- top[ids]                                  # align to universe order
  # Reuse .extreme_ids by treating `top` as the per-row value vector.
  ext_ids <- .extreme_ids(game_summary, as.numeric(top), "max")
  make_strategy(ext_ids, weights = rep(1, length(ext_ids)), game_summary = game_summary,
                id = "preset:biggest_jackpot", label = "Biggest jackpot",
                description = sprintf(
                  "Play the game(s) with the largest top prize (max net £%s); equal weight across %d tie(s).",
                  format(max(as.numeric(top))), length(ext_ids)))
}


# ------------------------------------------------------------------------------
# strategy_random_each_play(game_summary)
# ------------------------------------------------------------------------------
# Each play picks a game UNIFORMLY AT RANDOM from the (filtered) universe --
# i.e. equal weights across every game handed in. WHY uniform (a documented
# choice, not a derived optimum): "random each play" is the naive "I'll just buy
# whatever" baseline, and uniform is the unique maximally-uninformed allocation;
# any non-uniform weighting would smuggle in a preference this preset is meant to
# lack. It is the widest mixture and so the clearest demonstration that the
# mixture ~ multinomial-allocation equivalence holds across many games.
strategy_random_each_play <- function(game_summary) {
  .require_summary_cols(game_summary, "game_id")
  ids <- game_summary$game_id
  make_strategy(ids, weights = rep(1, length(ids)), game_summary = game_summary,
                id = "preset:random_each_play", label = "Random each play",
                description = sprintf(
                  "Each play picks uniformly at random among the %d games in the universe.",
                  length(ids)))
}
