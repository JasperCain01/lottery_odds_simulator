# ==============================================================================
# data_prep.R
# ------------------------------------------------------------------------------
# Layer 1 of the lottery odds simulator (see IMPLEMENTATION_PLAN.md, sections
# 4 and Phase 1).
#
# Purpose: normalise the two raw scraped CSV sources (scratchcards and instant
# win) into a single tidy "outcomes" table of net prize values and implied
# probabilities per game, plus a per-game summary (RTP, EV, variance, outlier
# flags) and game-universe metadata (on-sale/hidden/prohibited/category/source).
# Cached results are written to data/*.rds so the Shiny app does not need to
# re-derive them on every reactive tick.
#
# Inputs (repo root):
#   - national_lottery_scratchcard_odds.csv
#   - national_lottery_scratchcard_games.csv
#   - national_lottery_instant_win_prize_tiers.csv
#   - national_lottery_instant_win_catalogue.csv
#   - national_lottery_instant_win_reconciliation.csv
#
# Outputs (data/):
#   - outcomes.rds       tidy per-game, per-tier net-value/probability rows
#   - game_summary.rds   per-game RTP / EV / SD / outlier flags / metadata
#
# TODO (Phase 1): implement CSV ingestion, instant-win -> catalogue join on
# `slug`, net_value = prize - price derivation, implicit losing-row
# construction (prob = 1 - sum(tier probs)), duplicate-tier aggregation by net
# value, RTP/EV/variance computation, RTP outlier flagging + optional top-tier
# cap, and the human-readable RTP/outlier report.
# ==============================================================================
