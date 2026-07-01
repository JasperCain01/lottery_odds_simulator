# ==============================================================================
# narrative.R
# ------------------------------------------------------------------------------
# Layer 5 of the lottery odds simulator (see IMPLEMENTATION_PLAN.md, sections
# 4 and Phase 7).
#
# Purpose: auto-generate a plain-English, non-overstated summary of a
# simulation run from its metrics bundle (metrics.R), e.g. "typically lose
# £Y, 90% chance the result is between £A and £B, profit about Z% of the
# time, driven mostly by a 1-in-W jackpot." Responsible for correct rounding,
# pluralisation, and framing that never oversells the upside.
#
# TODO (Phase 7): implement the template engine mapping a metrics bundle to
# prose sentences.
# ==============================================================================
