# ==============================================================================
# app_helpers.R
# ------------------------------------------------------------------------------
# Phase 5 support code for app.R -- plain (non-reactive) functions that glue
# the four finished backend layers (data_prep / simulate / metrics /
# strategies) together for the Shiny server.
#
# WHY a separate file: keeping the actual pipeline logic in ordinary functions
# (no reactive()/input$ references) means (a) the server code in app.R stays a
# thin wiring layer, and (b) tests/testthat/test-app.R can exercise the same
# logic both directly (fast, no Shiny needed) and via shiny::testServer()
# (checks the reactive wiring itself). Sourcing this file defines functions plus
# ONE module-level object -- the Phase 9 repeat-run cache (an empty in-memory
# cachem store); it performs no I/O and touches no global state beyond that.
#
# Phase 9 also parks the REPEAT-RUN CACHE here (see the caching section below):
# a bounded, content-keyed memory cache over the expensive engine call so an
# identical re-run (same distribution + N + R + seed + checkpoints + probs + crn)
# returns instantly instead of re-simulating. It lives in this glue layer, not
# in the pure engine (simulate.R stays side-effect free / cache-agnostic), and
# is wired into run_pipeline() and the leaderboard sweep.
# ==============================================================================


# ==============================================================================
# REPEAT-RUN CACHING (Phase 9)
# ==============================================================================
# WHY here and not in simulate.R: the engine must stay a pure, side-effect-free
# numerical core (its reproducibility/chunk-invariance contracts are pinned by
# tests). Caching is an app-layer concern, so the cache object and the cached
# wrappers live in this glue layer and simply call the pure engine on a miss.
#
# BACKEND: a cachem::cache_mem() LRU with a bounded byte ceiling, so a long
# interactive session cannot grow the cache without limit; least-recently-used
# results are evicted first. One process-level cache is shared across all Shiny
# sessions (defined once at source time), so a result computed for one user is
# reusable for another with identical inputs.
#
# KEY CORRECTNESS (the critical part): a cache HIT must be returned only when it
# is guaranteed identical to recomputing. The key is a digest() over EVERYTHING
# that affects the engine result -- the distribution's (value, prob) pairs and
# every scalar control (N, R, seed, checkpoints, probs, crn). The distribution
# is canonicalised to sorted (value, prob) numeric vectors first, so the key
# depends only on the numbers that drive the inverse-CDF engine, not on the
# container type (data.frame vs list) or the row order (the engine sorts by
# value internally, so a row permutation is the SAME run and SHOULD hit). A miss
# is always safe (we just recompute); only a false HIT would be a bug, so the key
# deliberately errs toward distinguishing inputs (e.g. NULL vs 0 seed).
.pipeline_cache <- cachem::cache_mem(max_size = 512 * 1024^2, evict = "lru")

# Canonical (value, prob) extraction mirroring the engine's own reading of dist,
# used purely to build a stable cache key (NOT to renormalise -- the engine does
# that; here we only need a deterministic, container-independent digest input).
.dist_key_part <- function(dist) {
  if (!(is.data.frame(dist) || is.list(dist)) ||
      !all(c("value", "prob") %in% names(dist))) {
    stop("dist must be a data.frame/list with 'value' and 'prob'.", call. = FALSE)
  }
  value <- as.numeric(dist$value)
  prob  <- as.numeric(dist$prob)
  ord   <- order(value)
  list(value = value[ord], prob = prob[ord])
}

# The full content key for one simulate_sessions() call.
.sim_cache_key <- function(dist, N, R, seed, checkpoints, probs, crn) {
  digest::digest(
    list(
      dist        = .dist_key_part(dist),
      N           = as.integer(N),
      R           = as.integer(R),
      # NULL and 0 must not collide: tag the no-seed case distinctly.
      seed        = if (is.null(seed)) "none" else as.integer(seed),
      checkpoints = checkpoints,
      probs       = probs,
      crn         = crn
    ),
    algo = "xxhash64"
  )
}

# ------------------------------------------------------------------------------
# simulate_sessions_cached(dist, N, R, ...) -- cached engine wrapper
# ------------------------------------------------------------------------------
# Behaviour-identical to simulate_sessions() but returns a cached result on an
# exact input match. Pass cache = NULL to bypass the cache entirely (used by
# tests to get an uncached reference to compare against). The result stored is
# the very object simulate_sessions() returns, so a hit is byte-identical to a
# fresh computation.
simulate_sessions_cached <- function(dist, N, R,
                                     seed        = NULL,
                                     checkpoints = 100,
                                     probs       = c(.05, .25, .5, .75, .95),
                                     crn         = NULL,
                                     cache       = .pipeline_cache) {
  if (is.null(cache)) {
    return(simulate_sessions(dist, N = N, R = R, seed = seed,
                             checkpoints = checkpoints, probs = probs, crn = crn))
  }
  key <- .sim_cache_key(dist, N, R, seed, checkpoints, probs, crn)
  hit <- cache$get(key)
  if (!cachem::is.key_missing(hit)) return(hit)
  res <- simulate_sessions(dist, N = N, R = R, seed = seed,
                           checkpoints = checkpoints, probs = probs, crn = crn)
  cache$set(key, res)
  res
}

# ------------------------------------------------------------------------------
# leaderboard_series_cached(dist, N_grid, metric, R, seed, ...)
# ------------------------------------------------------------------------------
# Cache the leaderboard-vs-N series (one simulation PER N on the p_profit path)
# by its inputs. The analytical "mean_pnl" metric needs no simulation and is
# already instant in leaderboard_series(); caching it too is harmless and keeps
# one code path. Key covers the distribution and every argument that changes the
# series. `metric` may be a string or a function; a function is folded into the
# key by its digest (two structurally identical closures hit, distinct ones do
# not) -- and on the (rare) event of a hash collision a hit is still only wrong
# if the closures differ AND digests collide, which xxhash64 makes negligible.
leaderboard_series_cached <- function(dist, N_grid, metric = "p_profit",
                                      R = 2000, seed = NULL,
                                      cache = .pipeline_cache, ...) {
  if (is.null(cache)) {
    return(leaderboard_series(dist, N_grid = N_grid, metric = metric,
                              R = R, seed = seed, ...))
  }
  key <- digest::digest(
    list(kind = "leaderboard_series",
         dist = .dist_key_part(dist),
         N_grid = sort(unique(as.integer(N_grid))),
         metric = metric,
         R = as.integer(R),
         seed = if (is.null(seed)) "none" else as.integer(seed),
         extra = list(...)),
    algo = "xxhash64")
  hit <- cache$get(key)
  if (!cachem::is.key_missing(hit)) return(hit)
  res <- leaderboard_series(dist, N_grid = N_grid, metric = metric,
                            R = R, seed = seed, ...)
  cache$set(key, res)
  res
}

# ------------------------------------------------------------------------------
# load_app_data(root = ".")
# ------------------------------------------------------------------------------
# Load the two cached Phase 1 artifacts the app needs, building them via
# run_data_prep() on first run if the cache is missing. Called once at app
# startup (outside the server function) so every session shares the same
# in-memory tables rather than re-reading/reparsing per session.
load_app_data <- function(root = ".") {
  outcomes_path <- file.path(root, "data", "outcomes.rds")
  summary_path  <- file.path(root, "data", "game_summary.rds")

  if (!file.exists(outcomes_path) || !file.exists(summary_path)) {
    run_data_prep(root)
  }

  list(
    outcomes     = readRDS(outcomes_path),
    game_summary = readRDS(summary_path)
  )
}


# ------------------------------------------------------------------------------
# weight_input_id(game_id)
# ------------------------------------------------------------------------------
# The custom-mix builder generates one numericInput per selected game whose
# Shiny input id is derived from the game_id. game_id values contain ":" and
# "-" (e.g. "instant_win:100x"), which are not safe/stable as literal Shiny
# input ids, so we deterministically sanitise them. Kept as a pure function so
# the UI generator and the reader (collect_custom_weights) always agree.
weight_input_id <- function(game_id) {
  paste0("cw__", gsub("[^A-Za-z0-9]", "_", game_id))
}


# ------------------------------------------------------------------------------
# collect_custom_weights(input, game_ids, default = 1)
# ------------------------------------------------------------------------------
# Read the dynamically generated per-game weight numericInputs back out of
# `input`, in the same order as `game_ids`. Any input not yet rendered (NULL,
# e.g. immediately after a game is added before the UI catches up) falls back
# to `default` so the pipeline never sees a missing weight.
collect_custom_weights <- function(input, game_ids, default = 1) {
  vapply(game_ids, function(gid) {
    val <- input[[weight_input_id(gid)]]
    if (is.null(val) || !is.finite(val)) default else val
  }, numeric(1))
}


# ------------------------------------------------------------------------------
# build_strategy(preset, gs, outcomes, single_game, custom_games, custom_weights)
# ------------------------------------------------------------------------------
# Map the sidebar's strategy choice onto the R/strategies.R constructors. `gs`
# is the ALREADY-FILTERED game_summary (universe filters applied), so every
# preset here operates over exactly the games the user asked to consider.
# Throws a plain, user-facing error (call. = FALSE) on invalid selections;
# the server wraps this in validate()/tryCatch so it surfaces as a friendly
# message instead of crashing the app.
build_strategy <- function(preset, gs, outcomes,
                           single_game    = NULL,
                           custom_games   = NULL,
                           custom_weights = NULL) {
  if (is.null(gs) || nrow(gs) == 0L) {
    stop("No games available in the current filtered universe.", call. = FALSE)
  }

  switch(preset,
    single = {
      if (is.null(single_game) || length(single_game) == 0L || !nzchar(single_game)) {
        stop("Pick a game for the 'Single game' strategy.", call. = FALSE)
      }
      if (!single_game %in% gs$game_id) {
        stop("The selected game is not in the current filtered universe; ",
             "adjust filters or pick another game.", call. = FALSE)
      }
      strategy_single(single_game, gs)
    },
    cheapest         = strategy_cheapest(gs),
    priciest         = strategy_priciest(gs),
    best_rtp         = strategy_best_rtp(gs),
    worst_rtp        = strategy_worst_rtp(gs),
    biggest_jackpot  = strategy_biggest_jackpot(gs, outcomes),
    random_each_play = strategy_random_each_play(gs),
    custom = {
      if (is.null(custom_games) || length(custom_games) == 0L) {
        stop("Pick at least one game for the custom mix.", call. = FALSE)
      }
      missing_games <- setdiff(custom_games, gs$game_id)
      if (length(missing_games) > 0L) {
        stop("Some selected custom-mix games are not in the current filtered ",
             "universe; adjust filters or reselect games.", call. = FALSE)
      }
      make_strategy(custom_games, weights = custom_weights, game_summary = gs)
    },
    stop(sprintf("Unknown strategy preset: '%s'.", preset), call. = FALSE)
  )
}


# ------------------------------------------------------------------------------
# run_pipeline(gs, outcomes, preset, ..., N, R, seed, conf, checkpoints)
# ------------------------------------------------------------------------------
# THE full backend pipeline for one "Run simulation" click, as a plain
# function: filtered universe + strategy choice -> strategy object ->
# mixture distribution -> simulate_sessions() -> build_metrics(). Kept
# separate from any reactive() so it can be unit-tested directly (no Shiny
# session needed) and so the eventReactive() in app.R's server is a one-line
# call plus input validation.
run_pipeline <- function(gs, outcomes, preset,
                         single_game    = NULL,
                         custom_games   = NULL,
                         custom_weights = NULL,
                         N, R,
                         seed        = NULL,
                         conf        = 0.90,
                         checkpoints = 100) {
  strat <- build_strategy(preset, gs, outcomes,
                          single_game    = single_game,
                          custom_games   = custom_games,
                          custom_weights = custom_weights)

  dist <- strategy_distribution(strat, outcomes)
  # Cached engine call (Phase 9): an identical re-run (same dist + N + R + seed +
  # checkpoints) returns the stored result instantly instead of re-simulating.
  sim  <- simulate_sessions_cached(dist, N = N, R = R, seed = seed,
                                   checkpoints = checkpoints)

  price      <- strategy_expected_price(strat)
  metrics    <- build_metrics(sim, dist, price = price, alpha = 1 - conf)
  spend      <- strategy_spend(strat, N)
  analytical <- analytical_summary(dist, N = N, conf = conf)

  list(
    strategy   = strat,
    dist       = dist,
    sim        = sim,
    metrics    = metrics,
    spend      = spend,
    analytical = analytical
  )
}


# ------------------------------------------------------------------------------
# validate_run_bounds(N, R, N_max, R_max, NR_max)
# ------------------------------------------------------------------------------
# Guard the N x R cost of a run before it reaches simulate_sessions(). The
# engine itself memory-chunks so it will not blow up, but an unbounded N x R
# from the UI can still take a long time and make the app feel unresponsive;
# this is a UI-level ceiling, documented and message-bearing rather than a
# silent clamp. Returns NULL if all bounds are satisfied, or a single
# human-readable message string describing the first violated bound.
validate_run_bounds <- function(N, R, N_max, R_max, NR_max) {
  if (is.null(N) || !is.finite(N) || N < 1) {
    return("N (tickets per session) must be a positive integer.")
  }
  if (is.null(R) || !is.finite(R) || R < 1) {
    return("R (number of sessions) must be a positive integer.")
  }
  if (N > N_max) {
    return(sprintf("N (tickets per session) is capped at %s for interactive use; lower it.",
                   format(N_max, big.mark = ",", scientific = FALSE)))
  }
  if (R > R_max) {
    return(sprintf("R (number of sessions) is capped at %s for interactive use; lower it.",
                   format(R_max, big.mark = ",", scientific = FALSE)))
  }
  if (N * R > NR_max) {
    return(sprintf(
      "N x R = %s exceeds the interactive cap of %s draws; lower N and/or R.",
      format(N * R, big.mark = ",", scientific = FALSE),
      format(NR_max, big.mark = ",", scientific = FALSE)))
  }
  NULL
}
