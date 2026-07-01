# ==============================================================================
# app.R
# ------------------------------------------------------------------------------
# Phase 0 placeholder for the Lottery Odds Simulator Shiny app.
#
# This is intentionally minimal: it exists only to prove the project skeleton
# loads and runs under bslib/shiny before any real reactive logic (strategy
# selection, simulation engine, plots) is wired in. Full UI/server wiring
# happens in Phase 5 (Shiny app skeleton) onward -- see IMPLEMENTATION_PLAN.md.
#
# TODO (Phase 5): replace this placeholder with the real bslib page_sidebar()
# layout (strategy/N/R/confidence/filter/seed inputs) and reactive graph
# connecting inputs -> strategies.R -> simulate.R -> metrics.R -> viz.R.
# ==============================================================================

library(shiny)
library(bslib)

# Minimal page: a themed title bar and a single "under construction" message.
# Trade-off: no real inputs/outputs yet -- deliberately so, since Phase 0's
# only job is to confirm the app shell loads without error.
ui <- page_fillable(
  title = "Lottery Odds Simulator",
  theme = bs_theme(version = 5),
  card(
    card_header("Lottery Odds Simulator"),
    card_body(
      p("Under construction."),
      p("This placeholder confirms the project skeleton runs. See IMPLEMENTATION_PLAN.md for the full build plan.")
    )
  )
)

# No reactive logic yet -- server is a no-op stub until Phase 5.
server <- function(input, output, session) {
  # TODO (Phase 5): wire reactive inputs to the strategy/simulation/metrics pipeline.
}

shinyApp(ui = ui, server = server)
