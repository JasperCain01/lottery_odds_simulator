# ==============================================================================
# app.R
# ------------------------------------------------------------------------------
# Phase 5 -- the Shiny app skeleton for the Lottery Odds Simulator.
#
# This wires the four finished backend layers together behind a bslib
# page_sidebar() UI:
#
#   filters (sidebar) --> filter_games()                       [strategies.R]
#                     --> chosen strategy object                [strategies.R]
#                     --> strategy_distribution()                [strategies.R]
#                     --> simulate_sessions()  (seeded)          [simulate.R]
#                     --> build_metrics(..., price = strategy_expected_price())
#                                                                 [metrics.R]
#
# The pipeline is deliberately gated behind an actionButton ("Run simulation")
# via eventReactive(), NOT recomputed on every slider tick -- N x R can be up
# to millions of draws, so re-running on every input change would make the app
# feel broken. See R/app_helpers.R for the plain (non-reactive) glue functions
# that do the actual work; the server below is thin wiring + input validation
# around them, which also makes them independently unit-testable
# (tests/testthat/test-app.R exercises both the helpers directly and the
# reactive graph via shiny::testServer()).
#
# Outputs here are DELIBERATELY placeholder-simple (a histogram, printed
# metrics, printed strategy detail) -- Phase 6 owns the real visualisations
# (fan chart, dream-vs-reality, leaderboard-vs-N, play-by-play).
# ==============================================================================

library(shiny)
library(bslib)
library(ggplot2)
library(scales)

# ------------------------------------------------------------------------------
# Startup (once, outside the server -- shared across all sessions):
#   1. source the four finished backend layers + the Phase 5 glue helpers.
#   2. load (or build, if the cache is absent) outcomes.rds / game_summary.rds.
#   3. compute the purchasable universe once, to seed the game-picker choices.
# ------------------------------------------------------------------------------
app_root <- getwd()

for (rel in c("R/data_prep.R", "R/simulate.R", "R/metrics.R",
              "R/strategies.R", "R/app_helpers.R")) {
  source(file.path(app_root, rel))
}

app_data          <- load_app_data(app_root)   # builds the cache on first run
outcomes_full     <- app_data$outcomes
game_summary_full <- app_data$game_summary

# Purchasable universe, computed once, to seed the initial game-picker choices
# (before any sidebar filter has been touched).
purchasable_default <- tryCatch(
  filter_games(game_summary_full, purchasable = TRUE),
  error = function(e) game_summary_full
)

# ------------------------------------------------------------------------------
# UI-level run-cost ceiling. simulate_sessions() memory-chunks internally so it
# will not blow up, but an unbounded N x R can still take a long time and make
# an interactive app feel unresponsive -- so we cap it here and message the
# user rather than silently clamping (see validate_run_bounds()).
# ------------------------------------------------------------------------------
N_MIN <- 1L;  N_MAX <- 5000L
R_MIN <- 1L;  R_MAX <- 10000L
NR_MAX <- 5e6

# Human-readable "Game (source, £price)" choices for the game-picker inputs.
game_choices <- function(gs) {
  if (is.null(gs) || nrow(gs) == 0L) return(character(0))
  labels <- sprintf("%s (%s, £%s)", gs$name, gs$source, format(gs$price))
  stats::setNames(gs$game_id, labels)
}

source_choices   <- sort(unique(game_summary_full$source))
category_choices <- sort(unique(game_summary_full$category))
price_bounds     <- range(game_summary_full$price, na.rm = TRUE)

strategy_choices <- c(
  "Single game"       = "single",
  "Always cheapest"   = "cheapest",
  "Always priciest"   = "priciest",
  "Best RTP"          = "best_rtp",
  "Worst RTP"         = "worst_rtp",
  "Biggest jackpot"   = "biggest_jackpot",
  "Random each play"  = "random_each_play",
  "Custom mix"        = "custom"
)

# ==============================================================================
# UI
# ==============================================================================
ui <- page_sidebar(
  title = "Lottery Odds Simulator",
  theme = bs_theme(version = 5, primary = "#2C3E50"),

  sidebar = sidebar(
    width = 360,

    h5("Game universe filters"),
    checkboxInput("purchasable_only",
                  "Purchasable only (exclude hidden / prohibited / off-sale)",
                  value = TRUE),
    checkboxInput("on_sale_only", "On sale only", value = FALSE),
    selectizeInput("source_filter", "Source", choices = source_choices,
                   selected = NULL, multiple = TRUE,
                   options = list(placeholder = "All sources")),
    selectizeInput("category_filter", "Category", choices = category_choices,
                   selected = NULL, multiple = TRUE,
                   options = list(placeholder = "All categories")),
    sliderInput("price_range", "Ticket price (£)",
                min = floor(price_bounds[1]), max = ceiling(price_bounds[2]),
                value = c(floor(price_bounds[1]), ceiling(price_bounds[2])),
                step = 0.5, pre = "£"),
    uiOutput("universe_status"),

    hr(),
    h5("Strategy"),
    selectInput("strategy_preset", "Strategy", choices = strategy_choices,
                selected = "cheapest"),
    conditionalPanel(
      condition = "input.strategy_preset == 'single'",
      selectizeInput("single_game", "Game",
                     choices = game_choices(purchasable_default))
    ),
    conditionalPanel(
      condition = "input.strategy_preset == 'custom'",
      selectizeInput("custom_games", "Games in the mix",
                     choices = game_choices(purchasable_default), multiple = TRUE),
      helpText("Set a relative weight for each selected game ",
               "(equal weight if left at the default)."),
      uiOutput("custom_weight_inputs")
    ),

    hr(),
    h5("Session design"),
    sliderInput("N", "N — tickets per session",
                min = N_MIN, max = N_MAX, value = 100, step = 1),
    sliderInput("R", "R — number of simulated sessions",
                min = R_MIN, max = R_MAX, value = 1000, step = 1),
    helpText(sprintf(
      "N x R is capped at %s draws per run to keep the app responsive.",
      format(NR_MAX, big.mark = ",", scientific = FALSE))),
    sliderInput("conf", "Confidence level", min = 0.5, max = 0.99,
                value = 0.90, step = 0.01),
    numericInput("seed", "Random seed", value = 42, min = 1, step = 1),

    hr(),
    actionButton("run", "Run simulation", class = "btn-primary w-100")
  ),

  navset_card_tab(
    nav_panel(
      "Distribution",
      plotOutput("dist_plot", height = "440px"),
      p(class = "text-muted small",
        "Placeholder histogram of simulated session P&L. Phase 6 replaces ",
        "this with the fan chart / dream-vs-reality visuals.")
    ),
    nav_panel(
      "Risk & engagement",
      verbatimTextOutput("headline_metrics")
    ),
    nav_panel(
      "Details",
      verbatimTextOutput("details_text")
    )
  )
)

# ==============================================================================
# Server
# ==============================================================================
server <- function(input, output, session) {

  is_error <- function(x) inherits(x, "error")

  # ---- Universe filters -> filtered game_summary (or a caught error) -------
  filtered_gs <- reactive({
    tryCatch(
      filter_games(
        game_summary_full,
        purchasable = isTRUE(input$purchasable_only),
        on_sale     = if (isTRUE(input$on_sale_only)) TRUE else NULL,
        price_min   = input$price_range[1],
        price_max   = input$price_range[2],
        category    = if (is.null(input$category_filter) ||
                          length(input$category_filter) == 0L) NULL
                      else input$category_filter,
        source      = if (is.null(input$source_filter) ||
                          length(input$source_filter) == 0L) NULL
                      else input$source_filter
      ),
      error = function(e) e
    )
  })

  # ---- Keep the game-picker choices in sync with the filtered universe -----
  observe({
    gs <- filtered_gs()
    choices <- if (is_error(gs)) character(0) else game_choices(gs)

    keep_single <- if (!is.null(input$single_game) && input$single_game %in% choices)
      input$single_game else if (length(choices)) choices[[1]] else character(0)
    updateSelectizeInput(session, "single_game", choices = choices, selected = keep_single)

    keep_custom <- intersect(input$custom_games, choices)
    updateSelectizeInput(session, "custom_games", choices = choices, selected = keep_custom)
  })

  output$universe_status <- renderUI({
    gs <- filtered_gs()
    if (is_error(gs)) {
      div(class = "text-danger small mt-1", conditionMessage(gs))
    } else {
      div(class = "text-muted small mt-1",
          sprintf("%d game(s) match the current filters.", nrow(gs)))
    }
  })

  # ---- Custom-mix: one weight numericInput per selected game ---------------
  output$custom_weight_inputs <- renderUI({
    ids <- input$custom_games
    if (is.null(ids) || length(ids) == 0L) {
      return(helpText("Select one or more games above."))
    }
    gs <- filtered_gs()
    tagList(lapply(ids, function(gid) {
      nm <- if (!is_error(gs)) gs$name[match(gid, gs$game_id)] else NA_character_
      lbl <- if (length(nm) == 0L || is.na(nm)) gid else nm
      numericInput(weight_input_id(gid), label = lbl, value = 1, min = 0, step = 0.5)
    }))
  })
  outputOptions(output, "custom_weight_inputs", suspendWhenHidden = FALSE)

  # ---- Gate the expensive pipeline behind "Run simulation" ------------------
  # WHY plain input$run (not a reactiveVal side-effect set inside the
  # eventReactive): a write to a reactiveVal made inside an eventReactive body
  # is not reliably observable afterwards on a path where that same body later
  # throws (validate() or a plain error) -- input$run itself has no such
  # wrinkle, so outputs gate on it directly.
  pipeline_result <- eventReactive(input$run, {
    gs <- filtered_gs()
    validate(need(!is_error(gs), if (is_error(gs)) conditionMessage(gs) else NULL))

    bound_msg <- validate_run_bounds(input$N, input$R, N_MAX, R_MAX, NR_MAX)
    validate(need(is.null(bound_msg), bound_msg))

    seed_val <- if (is.null(input$seed) || !is.finite(input$seed)) NULL else as.integer(input$seed)

    custom_ids <- input$custom_games
    custom_w   <- if (!is.null(custom_ids) && length(custom_ids) > 0L)
      collect_custom_weights(input, custom_ids) else NULL

    run_it <- function() {
      run_pipeline(
        gs = gs, outcomes = outcomes_full, preset = input$strategy_preset,
        single_game    = input$single_game,
        custom_games   = custom_ids,
        custom_weights = custom_w,
        N = input$N, R = input$R, seed = seed_val, conf = input$conf
      )
    }

    # Busy indicator while the run is in progress. WHY the inner tryCatch:
    # under shiny::testServer()'s MockShinySession, withProgress()/incProgress()
    # do not have real progress support and trip an empty "shiny.silent.error"
    # (the same condition class req() throws) purely as an artifact of the
    # mocked session -- NOT a validation failure from our own code (run_pipeline
    # only ever raises plain errors). Falling back to a plain call keeps the
    # pipeline testable headlessly while a real browser session still gets the
    # progress bar.
    result <- tryCatch(
      tryCatch(
        withProgress(message = "Running simulation", value = 0.1, {
          incProgress(0.2, detail = "Building strategy & distribution")
          out <- run_it()
          incProgress(0.7, detail = "Done")
          out
        }),
        shiny.silent.error = function(e) run_it()
      ),
      error = function(e) e
    )

    validate(need(!is_error(result), if (is_error(result)) conditionMessage(result) else NULL))
    # Best-effort toast; wrapped in try() because (like withProgress() above)
    # notifications are a noop-but-erroring artifact of testServer's
    # MockShinySession, and a failed toast should never fail the run.
    try(showNotification("Simulation complete.", type = "message", duration = 3),
        silent = TRUE)
    result
  }, ignoreInit = TRUE)

  not_run_msg <- "Configure your strategy and filters, then click \"Run simulation\"."

  # ---- Outputs (placeholder visuals -- Phase 6 owns the real charts) -------
  output$dist_plot <- renderPlot({
    validate(need(isTRUE(input$run > 0), not_run_msg))
    res    <- pipeline_result()
    totals <- res$sim$totals

    ggplot(data.frame(pnl = totals), aes(x = pnl)) +
      geom_histogram(bins = 40, fill = "#2C3E50", color = "white", alpha = 0.85) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
      geom_vline(xintercept = mean(totals), color = "firebrick") +
      scale_x_continuous(labels = scales::label_dollar(prefix = "£")) +
      labs(x = "Session P&L", y = "Sessions",
           title = sprintf("%s — N = %d, R = %d",
                           res$strategy$label, res$sim$N, res$sim$R)) +
      theme_minimal(base_size = 13)
  })

  output$headline_metrics <- renderPrint({
    validate(need(isTRUE(input$run > 0), not_run_msg))
    res    <- pipeline_result()
    m      <- res$metrics
    totals <- res$sim$totals

    cat("STRATEGY\n")
    cat(sprintf("  %s\n", res$strategy$label))
    cat(sprintf("  %s\n\n", res$strategy$description))

    cat("SESSION DESIGN\n")
    cat(sprintf("  N (tickets/session): %s   R (sessions): %s   seed: %s\n",
                format(m$meta$N, big.mark = ","), format(m$meta$R, big.mark = ","),
                if (is.null(m$meta$seed)) "none" else m$meta$seed))
    cat(sprintf("  Expected price/play: £%.2f   Expected stake/session: £%.2f\n\n",
                m$meta$price, m$meta$total_stake))

    cat("SESSION P&L\n")
    cat(sprintf("  Mean:   £%.2f\n", mean(totals)))
    cat(sprintf("  Median: £%.2f\n", stats::median(totals)))
    cat(sprintf("  P(profit):               %.1f%%\n", 100 * m$risk$p_profit))
    cat(sprintf("  P(break-even or better): %.1f%%\n", 100 * m$risk$p_breakeven))
    cat(sprintf("  %.0f%% VaR (loss):                £%.2f\n",
                100 * (1 - m$risk$alpha), m$risk$var$loss))
    cat(sprintf("  %.0f%% Expected shortfall (loss):  £%.2f\n\n",
                100 * (1 - m$risk$alpha), m$risk$es$loss))

    cat("ENGAGEMENT\n")
    cat(sprintf("  Expected wins/session: %.2f  (P(a given play wins) = %.1f%%)\n",
                m$engagement$expected_wins, 100 * m$engagement$p_win))
    cat(sprintf("  Longest losing streak — median: %d plays, 95th pct: %d plays\n",
                round(m$streak$median), round(m$streak$quantiles[["p95"]])))
  })

  output$details_text <- renderPrint({
    validate(need(isTRUE(input$run > 0), not_run_msg))
    res   <- pipeline_result()
    strat <- res$strategy

    cat(sprintf("<lottery_strategy> %s [%s]\n", strat$label, strat$type))
    cat(sprintf("id: %s\n", strat$id))
    cat(sprintf("%s\n\n", strat$description))

    cat("Games and weights:\n")
    for (i in seq_along(strat$game_ids)) {
      cat(sprintf("  %-45s w=%.4f  £%s\n",
                  strat$game_ids[i], strat$weights[i], format(strat$prices[i])))
    }

    sp <- res$spend
    cat("\nRealised spend framing (fixed N):\n")
    cat(sprintf("  Expected spend: £%.2f (sd £%.2f)%s\n",
                sp$expected_spend, sp$spend_sd,
                if (sp$variable_spend)
                  "  [mixed ticket prices — spend varies session to session]" else ""))

    a <- res$analytical
    cat("\nAnalytical reference (Normal/CLT approximation):\n")
    cat(sprintf("  Mean: £%.2f   %d%% CI: [£%.2f, £%.2f]\n",
                a$mean, round(a$conf * 100), a$ci_lo, a$ci_hi))
    cat("  (unreliable at small N for skewed/jackpot games — see simulate.R header)\n")
  })
}

shinyApp(ui = ui, server = server)
