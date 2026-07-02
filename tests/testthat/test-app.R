# ==============================================================================
# test-app.R
# ------------------------------------------------------------------------------
# Phase 5 smoke tests for app.R -- the Shiny app skeleton.
#
# Two layers:
#   1. Direct unit tests of the plain helper functions in R/app_helpers.R
#      (build_strategy, run_pipeline, validate_run_bounds) -- fast, no Shiny
#      session required, and pin the exact glue the server relies on.
#   2. A headless shiny::testServer() smoke test that drives app.R's actual
#      server() function: set inputs, click "Run simulation", and assert the
#      reactive graph produces a sane metrics bundle. testServer() evaluates
#      the server's reactive graph directly -- it never opens a browser or
#      blocks, so it is safe to run under testthat.
#
# app.R's own top-level code does `app_root <- getwd()` and sources the four
# backend layers relative to it, assuming it is sourced from the repo root.
# Under testthat::test_dir()/test_file() the working directory is
# tests/testthat, so we withr::with_dir() into the repo root while sourcing
# app.R (mirroring the ../../R/*.R relative-path guards the other test files
# use), then let testthat restore the working directory automatically.
# Sourcing app.R does NOT start a server or block: shinyApp() only
# *constructs* an app object -- it is runApp() that blocks, and app.R never
# calls it.
#
# TESTSERVER QUIRK (affects every block below): shiny::testServer() has a
# reproducible artifact where an eventReactive's ignoreInit = TRUE treats the
# FIRST observed change to its event expression (input$run: 0 -> 1) as the
# ignored "initial" evaluation, so the reactive body only actually executes
# from the SECOND change onward. This is testServer-specific -- ignoreInit =
# TRUE behaves correctly in a real browser session, which is exactly why
# app.R keeps it (it stops the pipeline from running once on page load).
# Every "click Run" below is therefore two setInputs() calls (run = 1, then
# run = 2), mirroring what a real user's second click looks like to the
# reactive graph.
# ==============================================================================

find_repo_root <- function() {
  cands <- c(".", "..", "../..")
  ok <- vapply(cands, function(d) {
    file.exists(file.path(d, "app.R")) && file.exists(file.path(d, "R", "app_helpers.R"))
  }, logical(1))
  hit <- cands[ok]
  if (length(hit) == 0L) stop("Could not locate the repo root (app.R + R/app_helpers.R).")
  normalizePath(hit[1])
}

repo_root <- find_repo_root()

# Sourcing app.R (guarded, mirrors the other test files' idempotent-source
# pattern) brings in the four backend layers, R/app_helpers.R, and defines
# `ui`/`server`/`game_summary_full`/`outcomes_full`/`N_MAX`/`R_MAX`/`NR_MAX`
# as ordinary objects in this environment.
if (!exists("server", mode = "function")) {
  withr::with_dir(repo_root, source("app.R"))
}

# ------------------------------------------------------------------------------
# 1. Direct unit tests of R/app_helpers.R (no Shiny session).
# ------------------------------------------------------------------------------

test_that("build_strategy: presets construct valid strategy objects over the filtered universe", {
  gs <- filter_games(game_summary_full, purchasable = TRUE)
  strat <- build_strategy("cheapest", gs, outcomes_full)
  expect_s3_class(strat, "lottery_strategy")
  expect_true(all(strat$game_ids %in% gs$game_id))

  strat2 <- build_strategy("random_each_play", gs, outcomes_full)
  expect_s3_class(strat2, "lottery_strategy")
})

test_that("build_strategy: 'single' with no game selected errors clearly", {
  gs <- filter_games(game_summary_full, purchasable = TRUE)
  expect_error(build_strategy("single", gs, outcomes_full, single_game = NULL),
               "Pick a game")
})

test_that("build_strategy: 'custom' with no games selected errors clearly", {
  gs <- filter_games(game_summary_full, purchasable = TRUE)
  expect_error(build_strategy("custom", gs, outcomes_full, custom_games = NULL),
               "Pick at least one game")
})

test_that("build_strategy: 'custom' with all-zero weights errors clearly (delegates to make_strategy)", {
  gs <- filter_games(game_summary_full, purchasable = TRUE)
  ids <- gs$game_id[1:2]
  expect_error(
    build_strategy("custom", gs, outcomes_full, custom_games = ids, custom_weights = c(0, 0)),
    "weights sum to zero"
  )
})

test_that("run_pipeline: end-to-end produces a metrics bundle with the expected shape", {
  gs  <- filter_games(game_summary_full, purchasable = TRUE)
  out <- run_pipeline(gs, outcomes_full, preset = "cheapest",
                      N = 20, R = 100, seed = 7, conf = 0.9)

  expect_true(is.list(out$metrics))
  expect_equal(out$metrics$meta$N, 20L)
  expect_true(out$metrics$risk$p_profit >= 0 && out$metrics$risk$p_profit <= 1)
  expect_length(out$sim$totals, 100L)
})

test_that("validate_run_bounds: flags an over-cap N x R and passes an in-bounds run", {
  msg_bad <- validate_run_bounds(N = 5000, R = 10000, N_max = 5000, R_max = 10000, NR_max = 5e6)
  expect_true(is.character(msg_bad))

  msg_ok <- validate_run_bounds(N = 10, R = 10, N_max = 5000, R_max = 10000, NR_max = 5e6)
  expect_null(msg_ok)
})

# ------------------------------------------------------------------------------
# 2. shiny::testServer() smoke tests -- the reactive graph itself.
# ------------------------------------------------------------------------------

test_that("testServer: a preset run populates the pipeline reactive with sane metrics", {
  shiny::testServer(server, {
    session$setInputs(
      purchasable_only = TRUE,
      on_sale_only     = FALSE,
      source_filter    = character(0),
      category_filter  = character(0),
      price_range      = c(1, 10),
      strategy_preset  = "cheapest",
      N    = 10,
      R    = 50,
      conf = 0.90,
      seed = 123
    )
    session$setInputs(run = 1)   # see TESTSERVER QUIRK note at top of file
    session$setInputs(run = 2)

    expect_equal(input$run, 2)

    res <- pipeline_result()
    expect_type(res, "list")
    expect_true(is.list(res$metrics))
    expect_equal(res$metrics$meta$N, 10L)
    expect_true(res$metrics$risk$p_profit >= 0 && res$metrics$risk$p_profit <= 1)
    expect_length(res$sim$totals, 50L)
  })
})

test_that("testServer: a custom-mix run with explicit weights populates the pipeline reactive", {
  shiny::testServer(server, {
    session$setInputs(
      purchasable_only = TRUE,
      price_range       = c(1, 10),
      strategy_preset   = "custom"
    )
    ids <- head(game_choices(filter_games(game_summary_full, purchasable = TRUE)), 2)
    session$setInputs(custom_games = unname(ids))
    # Dynamically-generated per-game weight inputs (renderUI in the app);
    # set them directly by their sanitised ids, as the real UI would once
    # rendered in a browser.
    w_inputs <- setNames(list(2, 1), vapply(ids, weight_input_id, character(1)))
    do.call(session$setInputs, w_inputs)
    session$setInputs(N = 5, R = 30, conf = 0.9, seed = 99)
    session$setInputs(run = 1)   # see TESTSERVER QUIRK note at top of file
    session$setInputs(run = 2)

    res <- pipeline_result()
    expect_equal(res$strategy$type, "mixture")
    expect_equal(length(res$strategy$game_ids), 2L)
    expect_equal(res$metrics$meta$N, 5L)
  })
})

test_that("testServer: an empty filter universe is a handled state, not a crash", {
  shiny::testServer(server, {
    session$setInputs(
      purchasable_only = TRUE,
      price_range       = c(1000, 1001),   # no game costs this much -> filter_games errors
      strategy_preset   = "cheapest",
      N = 10, R = 20, conf = 0.9, seed = 1
    )
    session$setInputs(run = 1)   # see TESTSERVER QUIRK note at top of file
    session$setInputs(run = 2)

    expect_equal(input$run, 2)
    expect_error(pipeline_result())   # validate()'s condition, not an unhandled crash
  })
})

test_that("testServer: a custom mix with no games selected is a handled state, not a crash", {
  shiny::testServer(server, {
    session$setInputs(
      purchasable_only = TRUE,
      price_range       = c(1, 10),
      strategy_preset   = "custom",
      custom_games      = character(0),
      N = 10, R = 20, conf = 0.9, seed = 1
    )
    session$setInputs(run = 1)   # see TESTSERVER QUIRK note at top of file
    session$setInputs(run = 2)

    expect_equal(input$run, 2)
    expect_error(pipeline_result())
  })
})

test_that("testServer: N x R over the interactive cap is a handled state, not a crash", {
  shiny::testServer(server, {
    session$setInputs(
      purchasable_only = TRUE,
      price_range       = c(1, 10),
      strategy_preset   = "cheapest",
      N = N_MAX, R = R_MAX,   # product far exceeds NR_MAX
      conf = 0.9, seed = 1
    )
    session$setInputs(run = 1)   # see TESTSERVER QUIRK note at top of file
    session$setInputs(run = 2)

    expect_equal(input$run, 2)
    expect_error(pipeline_result())
  })
})
