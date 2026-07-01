# ==============================================================================
# strategies.R
# ------------------------------------------------------------------------------
# Layer 4 of the lottery odds simulator (see IMPLEMENTATION_PLAN.md, sections
# 4 and Phase 4).
#
# Purpose: define how plays are allocated across games, and present a uniform
# strategy interface that simulate.R can consume regardless of whether it is
# a single-game preset or a custom multi-game mix.
#   - presets: single game, always cheapest, always priciest, best RTP,
#     worst RTP, biggest jackpot, random-each-play
#   - custom mix builder: games + weights, multinomial allocation of plays
#     under a fixed N (mixed ticket prices mean variable realised spend --
#     documented, not hidden)
#   - filters: on-sale, price, category, source (scratchcard vs instant win)
#   - budget/time framing: translate "£X per week for a year" into an N
#
# TODO (Phase 4): implement preset strategy constructors, the custom mix
# builder with multinomial allocation, filters, and budget/time -> N framing.
# ==============================================================================
