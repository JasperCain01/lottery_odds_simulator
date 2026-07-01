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
#   - leaderboard-vs-N series (how strategy ranking shifts as N grows)
#
# TODO (Phase 3): implement the metrics bundle builder consumed by
# narrative.R and viz.R.
# ==============================================================================
