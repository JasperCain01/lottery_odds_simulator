# ==============================================================================
# simulate.R
# ------------------------------------------------------------------------------
# Layer 2 of the lottery odds simulator (see IMPLEMENTATION_PLAN.md, sections
# 4 and Phase 2).
#
# Purpose: the numerical core. Provides two complementary engines over a
# game's per-play net-outcome distribution (from data_prep.R's outcomes
# table):
#   - analytical(game, N, conf)  : closed-form mean (N * net EV), variance
#                                  (N * per-play Var), and CLT-based CI. Fast,
#                                  but a poor approximation at small N/skewed
#                                  tiers -- kept as a reference overlay only.
#   - monte_carlo(game, N, R, seed) : vectorised sampling of R independent
#                                  N-play sessions, rowSums for session P&L,
#                                  empirical percentiles. The primary engine
#                                  for anything user-facing, since it captures
#                                  skew that the analytical CI cannot.
#
# Also owns: memory chunking for large N x R (batched sampling, documented
# speed/memory trade-off), common random numbers (CRN) support for fair
# strategy comparisons, and fan-chart quantile checkpointing (~100 points
# along N rather than storing every intermediate play).
#
# TODO (Phase 2): implement analytical() and monte_carlo() engines, chunking,
# CRN wiring, checkpointing, and the tidy result-object contract consumed by
# metrics.R and viz.R.
# ==============================================================================
