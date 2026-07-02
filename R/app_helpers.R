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
# (checks the reactive wiring itself). Sourcing this file is side-effect free.
# ==============================================================================

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
  sim  <- simulate_sessions(dist, N = N, R = R, seed = seed, checkpoints = checkpoints)

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
