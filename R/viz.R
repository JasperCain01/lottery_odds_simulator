# ==============================================================================
# viz.R
# ------------------------------------------------------------------------------
# Layer 6 of the lottery odds simulator (see IMPLEMENTATION_PLAN.md, sections
# 4 and Phase 6).
#
# Purpose: the charts that carry the app's story, built from simulate.R /
# metrics.R outputs and wired into app.R:
#   - fan chart: cumulative P&L over plays with a CI ribbon built from the
#     checkpointed quantiles (not every individual play)
#   - final-P&L histogram/density with break-even/mean/median/percentile
#     markers
#   - leaderboard-vs-N small multiple
#   - single-session play-by-play view
#   - log/winsorize toggle for heavily skewed distributions
#   - sortable leaderboard table
#
# TODO (Phase 6): implement ggplot2-based chart builders + accessible alt
# text for each.
# ==============================================================================
