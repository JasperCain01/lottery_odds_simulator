# ==============================================================================
# metrics.R
# ------------------------------------------------------------------------------
# Layer 3 of the lottery odds simulator (see IMPLEMENTATION_PLAN.md, sections
# 4 and Phase 3).
#
# Purpose: turn raw simulation output (from simulate.R) into the
# decision-relevant numbers a player would actually want to see:
#   - risk metrics: P(profit), P(lose > £X), 5% VaR / expected shortfall
#   - dream-vs-reality decomposition: full outcome distribution vs the
#     distribution conditional on NOT hitting the top prize tier
#   - engagement metrics: expected wins/session, count of wins >= k * stake,
#     longest losing streak
#   - Monte Carlo standard error on all reported percentiles, so the app can
#     be honest about sampling noise vs real signal
#   - leaderboard-vs-N series (how a scalar metric shifts as N grows)
#
# CONTRACTS this layer relies on (from simulate.R / data_prep.R):
#   - `sim` is the list returned by simulate_sessions(): $totals (length-R
#     session P&L), $N, $R, $probs, $seed, $ev_play, $sd_play.
#   - `dist` is the generic (value, prob) distribution consumed by the engine
#     (game_distribution() is the bridge from the outcomes table). value =
#     net_value = prize - price; the most-negative value IS -price (the losing
#     row). sum(prob) == 1.
#   - simulate_one_session() is the ONLY source of per-play sequence data; the
#     chunked simulate_sessions() does not retain per-play draws. The longest-
#     losing-streak metric therefore drives its own small simulation.
#
# SIGN CONVENTION (fixed everywhere in this file): P&L is signed, NEGATIVE = a
# loss to the player. `totals` are session P&Ls in that convention. Where a
# metric is more naturally read as a positive "how much did I lose" magnitude
# (VaR, expected shortfall, loss thresholds), we ALSO report the positive loss
# magnitude and name it explicitly (`*_loss`) alongside the signed value
# (`*_pnl`). This is the single easiest thing to flip, so both are surfaced.
#
# QUANTILE TYPE: we use R's default type 7 everywhere a sample percentile is
# taken, matching simulate.R's fan-chart checkpoints, so every percentile in
# the app is computed one consistent way.
#
# source()-ing this file is side-effect free: it defines functions only. It
# assumes simulate.R is already sourced (for simulate_one_session()).
# ==============================================================================


# ------------------------------------------------------------------------------
# .dist_vp(dist)
# ------------------------------------------------------------------------------
# Internal: pull (value, prob) out of the generic dist input and return it
# normalised to sum-1 probability, sorted ascending by value. Deliberately a
# small local mirror of simulate.R's .canonical_dist so the analytical metrics
# do not reach into that file's internals; validation is intentionally light
# because dist has already passed through the engine by the time we see it.
.dist_vp <- function(dist) {
  if (is.data.frame(dist) || is.list(dist)) {
    if (!all(c("value", "prob") %in% names(dist))) {
      stop("dist must have 'value' and 'prob'.", call. = FALSE)
    }
    value <- as.numeric(dist$value)
    prob  <- as.numeric(dist$prob)
  } else {
    stop("dist must be a data.frame/list with 'value' and 'prob'.", call. = FALSE)
  }
  if (length(value) == 0L) stop("dist has no outcomes.", call. = FALSE)
  s <- sum(prob)
  if (s <= 0) stop("dist probabilities sum to zero (or less).", call. = FALSE)
  prob <- prob / s
  ord <- order(value)
  list(value = value[ord], prob = prob[ord])
}


# ------------------------------------------------------------------------------
# .moments_of(value, prob)
# ------------------------------------------------------------------------------
# Internal: per-play mean / variance / sd of a (value, prob) distribution.
# var clamped at 0 to defend against catastrophic cancellation on near-
# degenerate distributions (same guard as simulate.R's .dist_moments).
.moments_of <- function(value, prob) {
  ev  <- sum(value * prob)
  ex2 <- sum(value^2 * prob)
  v   <- ex2 - ev^2
  if (v < 0) v <- 0
  list(ev = ev, var = v, sd = sqrt(v))
}


# ------------------------------------------------------------------------------
# .derive_price(dist, price)
# ------------------------------------------------------------------------------
# Internal: the per-game stake C. WHY derive it from the distribution: the
# losing row is net_value = -C and is (bar a zero-prize win colliding onto it)
# the MOST NEGATIVE outcome, so -min(value) recovers the stake exactly and for
# free -- no extra argument to thread through the app. We still accept an
# explicit `price` override for two reasons: (a) mixture distributions (Phase 4)
# blend games of different stakes so no single -min(value) is meaningful, and
# (b) it lets callers ask "wins vs a stake other than the min", which the
# engagement metrics need for the k*stake thresholds. Guard: a positive stake is
# required (a non-negative min value would imply a game you cannot lose, which
# for these products signals the caller must pass price explicitly).
.derive_price <- function(dist, price = NULL) {
  if (is.null(price)) {
    vp    <- .dist_vp(dist)
    price <- -min(vp$value)
    if (price <= 0) {
      stop("cannot derive a positive stake from dist (min value >= 0); ",
           "pass `price` explicitly.", call. = FALSE)
    }
  } else {
    if (length(price) != 1L || !is.finite(price) || price <= 0) {
      stop("`price` must be a single positive number.", call. = FALSE)
    }
  }
  price
}


# ==============================================================================
# 1. RISK METRICS  (computed from sim$totals, the length-R session P&L)
# ==============================================================================

# ------------------------------------------------------------------------------
# value_at_risk(totals, alpha = 0.05)
# ------------------------------------------------------------------------------
# The alpha-VaR of session P&L. Definition used: the alpha-quantile of the P&L
# distribution (type 7, matching the engine). At alpha = 0.05 this is the P&L
# you do worse than only 5% of the time -- a "reasonable worst case".
#
# Returns BOTH forms (see the file-header sign note):
#   $pnl  : the signed quantile (typically negative = a loss).
#   $loss : the positive loss magnitude, -pnl. This is the number a player
#           reads as "5% of the time you lose at least £loss".
value_at_risk <- function(totals, alpha = 0.05) {
  if (alpha <= 0 || alpha >= 1) stop("alpha must be in (0, 1).", call. = FALSE)
  q <- stats::quantile(totals, probs = alpha, names = FALSE, type = 7)
  list(alpha = alpha, pnl = q, loss = -q)
}


# ------------------------------------------------------------------------------
# expected_shortfall(totals, alpha = 0.05)
# ------------------------------------------------------------------------------
# Expected shortfall / CVaR at level alpha: the mean P&L conditional on landing
# in the worst-alpha tail. We use the Acerbi-Tasche empirical estimator -- the
# average of the quantile function over [0, alpha]:
#
#   ES = (1/(alpha*R)) * [ sum_{i=1}^{k} x_(i) + (alpha*R - k) * x_(k+1) ],
#        k = floor(alpha*R),   x_(1) <= ... <= x_(R) the sorted P&Ls.
#
# WHY this and not simply mean(totals[totals <= VaR]): the naive tail-mean
# includes however many points happen to fall at/under the empirical quantile,
# so the averaged mass is not exactly alpha and it double-counts ties at the
# VaR point. The Acerbi form integrates the quantile function over EXACTLY the
# lower alpha of probability mass -- the coherent, ties-safe definition -- and
# reduces to a plain mean of the worst m points when alpha*R = m is an integer.
# When alpha*R < 1 (fewer than one session in the tail) it correctly returns the
# single worst session. It is a deterministic function of the order statistics,
# so it is exactly hand-checkable in tests.
#
# Returns $pnl (signed, <= VaR$pnl) and $loss = -pnl (positive magnitude).
expected_shortfall <- function(totals, alpha = 0.05) {
  if (alpha <= 0 || alpha >= 1) stop("alpha must be in (0, 1).", call. = FALSE)
  x <- sort(as.numeric(totals))
  R <- length(x)
  aR <- alpha * R
  k  <- floor(aR)
  frac <- aR - k
  s <- if (k >= 1L) sum(x[seq_len(k)]) else 0
  # Fractional weight on the next order statistic completes the lower-alpha mass.
  if (frac > 0 && (k + 1L) <= R) s <- s + frac * x[k + 1L]
  es <- s / aR
  list(alpha = alpha, pnl = es, loss = -es)
}


# ------------------------------------------------------------------------------
# risk_metrics(totals, alpha = 0.05, loss_thresholds = numeric(0))
# ------------------------------------------------------------------------------
# The full risk bundle from a length-R vector of session P&Ls.
#   p_profit    = P(P&L > 0)            (strictly ahead)
#   p_breakeven = P(P&L >= 0)           (break even or better)
#   p_loss_gt   = P(loss exceeds X) = P(P&L < -X), for each X in loss_thresholds
#                 (X are POSITIVE loss magnitudes; a loss "exceeding £X" means
#                  the signed P&L is below -X).
#   var, es     = value_at_risk() / expected_shortfall() at alpha.
risk_metrics <- function(totals, alpha = 0.05, loss_thresholds = numeric(0)) {
  totals <- as.numeric(totals)
  if (length(totals) == 0L) stop("totals is empty.", call. = FALSE)

  p_profit    <- mean(totals > 0)
  p_breakeven <- mean(totals >= 0)

  # P(lose more than £X) for each requested positive threshold X.
  if (any(loss_thresholds < 0)) {
    stop("loss_thresholds are positive loss magnitudes; got a negative value.",
         call. = FALSE)
  }
  p_loss_gt <- vapply(loss_thresholds,
                      function(x) mean(totals < -x), numeric(1))
  names(p_loss_gt) <- if (length(loss_thresholds))
    paste0("gt_", format(loss_thresholds, trim = TRUE, scientific = FALSE)) else
      character(0)

  list(
    alpha           = alpha,
    p_profit        = p_profit,
    p_breakeven     = p_breakeven,
    loss_thresholds = loss_thresholds,
    p_loss_gt       = p_loss_gt,
    var             = value_at_risk(totals, alpha),
    es              = expected_shortfall(totals, alpha)
  )
}


# ==============================================================================
# 2. DREAM-VS-REALITY DECOMPOSITION
# ==============================================================================

# ------------------------------------------------------------------------------
# dream_vs_reality(dist, N)
# ------------------------------------------------------------------------------
# The full outcome distribution vs the distribution CONDITIONAL ON NOT hitting
# the top prize tier -- the app's "the advertised dream vs what almost always
# happens" contrast.
#
# DESIGN DECISION 1 -- what is the "top tier": the set of outcomes at the MAXIMUM
# net value, max(value). WHY the max value (not a top-k set, not a currency
# cutoff): the "dream" a player is sold is the single best possible result --
# the jackpot -- and the maximum net outcome is exactly that, with no arbitrary
# parameter to choose. We take the SET of outcomes tied at the max (not just one
# row) because after aggregation ties are the SAME prize reached different ways,
# so they are one tier and must be removed together. Trade-off (documented): a
# game with several large-but-distinct top prizes has only its single largest
# removed here; that is deliberate -- "the dream" is the headline jackpot, and
# defining the tier by a magnitude threshold would reintroduce an arbitrary knob.
# Phase 4 mixtures can still probe other cutoffs by editing `dist` upstream.
#
# DESIGN DECISION 2 -- conditioning = truncate-and-renormalise: drop the top-tier
# mass p_top and rescale the REMAINING probabilities by 1/(1 - p_top) so they sum
# to 1. This is the exact conditional law P(X = x | X < top), which is what
# "your outcome given you didn't hit the jackpot" means. Law of total
# expectation then holds exactly:
#     E[full] = (1 - p_top) * E[conditional] + p_top * top_value,
# which the tests assert.
#
# Returns per-play AND per-session (x N, i.i.d. so mean scales by N; the
# conditional session mean assumes EVERY play avoids the top tier, i.e. it is the
# "reality" a jackpot-free run experiences, not E[P&L | zero jackpots in N]) --
# see note below.
# (The stake `price` is deliberately NOT an argument: the top tier is defined by
# the maximum net value, which is independent of the stake.)
dream_vs_reality <- function(dist, N) {
  if (N < 1 || N != as.integer(N)) stop("N must be a positive integer.", call. = FALSE)
  N  <- as.integer(N)
  vp <- .dist_vp(dist)
  value <- vp$value; prob <- vp$prob

  top_value <- max(value)
  is_top    <- value == top_value
  p_top     <- sum(prob[is_top])

  full <- .moments_of(value, prob)

  # Guard: if the top tier is (essentially) all the mass there is no "reality"
  # left to condition on -- report NA rather than divide by ~0.
  if (p_top >= 1 - 1e-12 || all(is_top)) {
    cond <- list(ev = NA_real_, var = NA_real_, sd = NA_real_)
    cond_dist <- data.frame(value = numeric(0), prob = numeric(0))
  } else {
    cond_value <- value[!is_top]
    cond_prob  <- prob[!is_top] / (1 - p_top)   # renormalise the remainder
    cond       <- .moments_of(cond_value, cond_prob)
    cond_dist  <- data.frame(value = cond_value, prob = cond_prob)
  }

  # WHY N * ev for the "session" figures: plays are i.i.d., so the per-session
  # mean is N * per-play EV. The conditional session mean is the mean P&L of a
  # run in which no play hits the top tier (each play drawn from the conditional
  # law) -- the honest "what reality feels like" number that strips the rare
  # jackpot windfall out of the average. It is NOT E[P&L | 0 jackpots in N]; that
  # subtlety (selection on the whole run) is deferred and noted here.
  list(
    N            = N,
    top_value    = top_value,
    p_top        = p_top,               # per-play probability of the dream
    p_top_in_N   = 1 - (1 - p_top)^N,   # probability of >=1 top hit across N plays
    full = list(
      ev_play    = full$ev,  sd_play = full$sd, var_play = full$var,
      mean_session = N * full$ev
    ),
    conditional = list(
      ev_play    = cond$ev,  sd_play = cond$sd, var_play = cond$var,
      mean_session = N * cond$ev
    ),
    conditional_dist = cond_dist
  )
}


# ==============================================================================
# 3. ENGAGEMENT METRICS
# ==============================================================================

# ------------------------------------------------------------------------------
# engagement_metrics(dist, N, price = NULL, k = c(1, 2, 5, 10))
# ------------------------------------------------------------------------------
# How often the game "does something" over a session of N plays.
#
# A WIN = the play returned any prize, i.e. gross prize > 0, i.e.
#   net_value > -price. (net_value == -price is the losing row; net_value == 0
#   still counts as a win -- a prize equal to the stake.)
#
# WHY ANALYTICAL (N * P(win)) rather than simulating: the count of wins in N
# i.i.d. plays is Binomial(N, p_win); its expectation is EXACTLY N * p_win.
# Simulating would only add Monte Carlo noise to a number we can write down in
# closed form for free. Same argument for the k*stake thresholds. We reserve
# simulation for the one metric that genuinely needs it (longest streak, below).
#
#   expected_wins            = N * P(net_value > -price)
#   wins_ge_k[k]             = N * P(prize >= k*price) = N * P(net_value >= (k-1)*price)
#                              (k = 1 is "at least broke even on the ticket")
engagement_metrics <- function(dist, N, price = NULL, k = c(1, 2, 5, 10)) {
  if (N < 1 || N != as.integer(N)) stop("N must be a positive integer.", call. = FALSE)
  N     <- as.integer(N)
  vp    <- .dist_vp(dist)
  value <- vp$value; prob <- vp$prob
  price <- .derive_price(dist, price)

  # Small floating tolerance on the boundary so a net_value that should equal
  # -price (or (k-1)*price) is not missed by fp round-off from prize - price.
  tol    <- 1e-9 * max(1, price)
  p_win  <- sum(prob[value > -price + tol])
  p_lose <- 1 - p_win

  if (any(k <= 0)) stop("k multiples must be positive.", call. = FALSE)
  wins_ge_k <- vapply(k, function(kk) {
    thr <- (kk - 1) * price            # net_value threshold for prize >= kk*price
    N * sum(prob[value >= thr - tol])
  }, numeric(1))
  names(wins_ge_k) <- paste0("k", format(k, trim = TRUE, scientific = FALSE))

  list(
    N             = N,
    price         = price,
    p_win         = p_win,
    p_lose        = p_lose,
    expected_wins = N * p_win,
    k             = k,
    wins_ge_k     = wins_ge_k
  )
}


# ------------------------------------------------------------------------------
# .longest_run_true(flags)
# ------------------------------------------------------------------------------
# Internal: length of the longest run of TRUE in a logical vector (0 if none).
.longest_run_true <- function(flags) {
  if (!any(flags)) return(0L)
  r <- rle(flags)
  as.integer(max(r$lengths[r$values]))
}


# ------------------------------------------------------------------------------
# longest_losing_streak(dist, N, R = 1000, seed = 1, price = NULL, probs = ...)
# ------------------------------------------------------------------------------
# The longest run of consecutive LOSING plays (a play with no prize:
# net_value == -price) in a session of N plays. This is PATH-DEPENDENT, so it
# cannot be read off sim$totals -- the chunked engine keeps no per-play
# sequence. We therefore drive a small dedicated simulation via
# simulate_one_session() over R sessions and summarise the streak distribution.
#
# WHY simulate rather than approximate analytically: the exact distribution of
# the longest success run in N Bernoulli(p_lose) trials has a known but fiddly
# recurrence, and the classic closed form is only an ASYMPTOTIC approximation
# (mean ~ log_{1/p_lose}(N*(1-p_lose))). For a correctness-first tool we prefer
# the honest empirical distribution -- means, quantiles AND the observed max --
# from the same reproducible engine the rest of the app uses. Trade-off: it
# costs R*N draws and carries Monte Carlo noise (quantified: a streak-length
# quantile has the usual ~1/sqrt(R) sampling error); we bound the cost by
# defaulting R modestly and let callers raise it.
#
# DEGENERATE guards (exact, no simulation): p_lose == 0 -> every play is a win,
# so the longest losing streak is 0; p_lose == 1 -> every play loses, so it is N.
longest_losing_streak <- function(dist, N, R = 1000, seed = 1, price = NULL,
                                   probs = c(.5, .9, .95, .99)) {
  if (N < 1 || N != as.integer(N)) stop("N must be a positive integer.", call. = FALSE)
  if (R < 1 || R != as.integer(R)) stop("R must be a positive integer.", call. = FALSE)
  N <- as.integer(N); R <- as.integer(R)
  vp    <- .dist_vp(dist)
  price <- .derive_price(dist, price)
  loss_value <- -price
  tol   <- 1e-9 * max(1, price)
  p_lose <- sum(vp$prob[vp$value <= loss_value + tol])

  # ---- exact degenerate shortcuts -------------------------------------------
  if (p_lose <= 0) {
    streaks <- integer(R)                      # never lose -> streak 0
  } else if (p_lose >= 1) {
    streaks <- rep(N, R)                        # always lose -> streak N
  } else {
    # ---- small dedicated per-play simulation --------------------------------
    # Distinct per-session seeds derived from `seed` so the run is reproducible
    # yet the R sessions are independent draws. simulate_one_session() returns
    # the length-N per-play net_value vector; a play is a loss when it equals the
    # losing outcome (exact double match: draws come straight from dist$value).
    if (!is.null(seed)) set.seed(as.integer(seed))
    seeds   <- sample.int(.Machine$integer.max, R)
    streaks <- integer(R)
    for (i in seq_len(R)) {
      draws <- simulate_one_session(dist, N = N, seed = seeds[i])
      streaks[i] <- .longest_run_true(abs(draws - loss_value) <= tol)
    }
  }

  qs <- stats::quantile(streaks, probs = probs, names = FALSE, type = 7)
  names(qs) <- paste0("p", probs * 100)
  list(
    N       = N,
    R       = R,
    p_lose  = p_lose,
    mean    = mean(streaks),
    median  = stats::median(streaks),
    max     = max(streaks),
    probs   = probs,
    quantiles = qs,
    streaks = streaks          # raw per-session streak lengths (for viz)
  )
}


# ==============================================================================
# 4. MONTE CARLO STANDARD ERROR ON REPORTED PERCENTILES
# ==============================================================================

# ------------------------------------------------------------------------------
# quantile_se(x, probs, ...)
# ------------------------------------------------------------------------------
# Standard error of empirical percentiles of `x`, via the MARITZ-JARRETT
# estimator. WHY Maritz-Jarrett (not a bootstrap): it is a closed-form,
# deterministic estimate of the sampling SE of a sample quantile expressed as a
# linear combination of order statistics, so it adds NO extra Monte Carlo noise
# of its own and costs one sort -- ideal for an interactive app that must, per
# reported percentile, say "this is real vs this is sampling wobble".
#
# For the p-th quantile of an n-sample, the MJ weights come from the Beta CDF:
#   a = (n+1)*p,  b = (n+1)*(1-p),
#   W_i = pbeta(i/n, a, b) - pbeta((i-1)/n, a, b),
#   SE  = sqrt( sum W_i x_(i)^2  -  (sum W_i x_(i))^2 ).
# The W_i sum to 1 (telescoping Beta CDF), so this is the sd of an L-estimator of
# the quantile. It is consistent and, being ~ 1/(f(q) * sqrt(n)) in scale, shrink
# like 1/sqrt(n) as R grows -- the property the app relies on and the tests check.
#
# ASSUMPTIONS / limits (documented): MJ assumes a continuous underlying law, so
# on a HEAVILY discrete P&L (e.g. tiny N where totals take few distinct values)
# the SE is only indicative; it degrades gracefully (SE -> 0 as ties dominate).
# We return NA for p at exactly 0 or 1 (Beta params degenerate; an extreme
# order statistic has no finite-sample SE estimate here).
#
# Returns a data.frame(prob, quantile, se): the type-7 quantile (engine-
# consistent point estimate) with its MJ standard error.
quantile_se <- function(x, probs = c(.05, .25, .5, .75, .95)) {
  x <- sort(as.numeric(x))
  n <- length(x)
  if (n < 2L) stop("need at least 2 observations for a quantile SE.", call. = FALSE)

  se <- vapply(probs, function(p) {
    if (p <= 0 || p >= 1) return(NA_real_)
    a <- (n + 1) * p
    b <- (n + 1) * (1 - p)
    i <- seq_len(n)
    W <- stats::pbeta(i / n, a, b) - stats::pbeta((i - 1) / n, a, b)
    c1 <- sum(W * x)
    c2 <- sum(W * x * x)
    sqrt(max(c2 - c1 * c1, 0))
  }, numeric(1))

  q <- stats::quantile(x, probs = probs, names = FALSE, type = 7)
  data.frame(prob = probs, quantile = q, se = se)
}


# ==============================================================================
# 5. LEADERBOARD-VS-N SERIES
# ==============================================================================

# ------------------------------------------------------------------------------
# leaderboard_series(dist, N_grid, metric = "p_profit", R = 2000, seed = NULL, ...)
# ------------------------------------------------------------------------------
# A single distribution's chosen scalar metric across a vector of N values --
# the per-distribution row the Phase 6 leaderboard-vs-N small-multiple stacks up
# across strategies. No strategy layer here: this is just one dist swept over N.
#
# metric:
#   "mean_pnl" -> N * ev_play, computed ANALYTICALLY (exact, no simulation) --
#                 this is the "loses most over many plays" curve and needs no MC.
#   "p_profit" -> P(P&L > 0), which is path-independent in level but genuinely
#                 needs the empirical P&L distribution, so we simulate R sessions
#                 per N (shared `seed` for reproducibility). This is the curve
#                 that CROSSES between strategies as N grows -- the whole point of
#                 the small-multiple.
# `metric` may also be a function(sim, dist) -> scalar for custom leaderboards,
# in which case a simulation is run per N and handed to it.
#
# Returns a tidy data.frame(N, metric, value).
leaderboard_series <- function(dist, N_grid, metric = "p_profit",
                               R = 2000, seed = NULL, ...) {
  N_grid <- sort(unique(as.integer(N_grid)))
  if (any(N_grid < 1)) stop("N_grid must be positive integers.", call. = FALSE)

  # Analytical fast path: mean P&L needs no simulation at all.
  if (is.character(metric) && identical(metric, "mean_pnl")) {
    vp <- .dist_vp(dist)
    ev <- .moments_of(vp$value, vp$prob)$ev
    return(data.frame(N = N_grid, metric = "mean_pnl", value = N_grid * ev))
  }

  # Simulation path (p_profit or a custom function).
  fn <- if (is.function(metric)) {
    metric
  } else if (identical(metric, "p_profit")) {
    function(sim, dist) mean(sim$totals > 0)
  } else {
    stop("metric must be 'mean_pnl', 'p_profit', or a function(sim, dist).",
         call. = FALSE)
  }
  label <- if (is.function(metric)) "custom" else metric

  value <- vapply(N_grid, function(N) {
    sim <- simulate_sessions(dist, N = N, R = R, seed = seed, ...)
    as.numeric(fn(sim, dist))
  }, numeric(1))

  data.frame(N = N_grid, metric = label, value = value)
}


# ==============================================================================
# 6. BUNDLE BUILDER
# ==============================================================================

# ------------------------------------------------------------------------------
# build_metrics(sim, dist, price = NULL, alpha = 0.05, loss_fracs = ...,
#               k = c(1, 2, 5, 10), probs = sim$probs, streak_R = ...,
#               streak_seed = ...)
# ------------------------------------------------------------------------------
# Assemble the full Phase 3 metrics bundle for one simulation run -- the object
# narrative.R and viz.R consume. Ties every metric above together over one
# `sim` / `dist` pair with a single, consistently-derived stake.
#
# loss_fracs: the P(lose > £X) thresholds are expressed as FRACTIONS of the total
# amount staked (N * price), because "lose more than half of what you put in" is
# the framing a player understands; the absolute £ thresholds are reported too.
#
# streak_R: the longest-streak sub-simulation is capped (default min(sim$R, 500))
# because it is the only O(N*R) per-play pass here; raise it for a smoother tail.
build_metrics <- function(sim, dist,
                          price       = NULL,
                          alpha       = 0.05,
                          loss_fracs  = c(0.25, 0.5, 0.75),
                          k           = c(1, 2, 5, 10),
                          probs       = NULL,
                          streak_R    = NULL,
                          streak_seed = NULL) {
  if (is.null(sim$totals) || is.null(sim$N)) {
    stop("`sim` must be a simulate_sessions() result (needs $totals, $N).",
         call. = FALSE)
  }
  N <- sim$N
  price <- .derive_price(dist, price)
  if (is.null(probs))       probs       <- if (!is.null(sim$probs)) sim$probs else
    c(.05, .25, .5, .75, .95)
  if (is.null(streak_R))    streak_R    <- min(sim$R, 500L)
  if (is.null(streak_seed)) streak_seed <- if (!is.null(sim$seed)) sim$seed else 1L

  # Loss thresholds as fractions of total stake, plus their absolute £ values.
  total_stake     <- N * price
  loss_thresholds <- loss_fracs * total_stake

  risk <- risk_metrics(sim$totals, alpha = alpha,
                       loss_thresholds = loss_thresholds)
  # Re-label p_loss_gt by the fraction it represents (more legible than the £).
  names(risk$p_loss_gt) <- paste0("frac_", format(loss_fracs, trim = TRUE))

  dream      <- dream_vs_reality(dist, N = N)
  engagement <- engagement_metrics(dist, N = N, price = price, k = k)
  streak     <- longest_losing_streak(dist, N = N, R = streak_R,
                                      seed = streak_seed, price = price)
  pct_se     <- quantile_se(sim$totals, probs = probs)

  list(
    meta = list(N = N, R = sim$R, seed = sim$seed, price = price,
                alpha = alpha, total_stake = total_stake,
                ev_play = sim$ev_play, sd_play = sim$sd_play),
    risk          = risk,
    dream         = dream,
    engagement    = engagement,
    streak        = streak,
    percentile_se = pct_se
  )
}
