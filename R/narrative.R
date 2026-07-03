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
# CONTRACT this layer relies on: `metrics` is a build_metrics() bundle
# (R/metrics.R) -- $meta, $risk, $dream, $engagement, $streak,
# $percentile_se. `sim`, when supplied, is a simulate_sessions() result
# (R/simulate.R) -- only $totals is used here, to compute EXACT empirical
# median/mean/quantiles instead of falling back to the metrics bundle's
# fan-chart checkpoint grid. `strategy`, when supplied, is a strategies.R
# object -- only $label is used, as an optional lead-in line.
#
# DESIGN
#   - Every sentence builder below is a small, independently testable pure
#     function of already-trusted numbers (Phases 1-3); nothing here recomputes
#     a statistic, it only formats one.
#   - HONEST-FRAMING RULES enforced throughout (never violate these):
#       1. Lead with the MEDIAN ("typical") session result, not the mean --
#          the mean is pulled up by the rare jackpot and is not what most
#          players experience. The mean is mentioned separately, labelled,
#          only when it diverges enough from the median to matter.
#       2. The reported interval is the EMPIRICAL percentile band from the
#          simulated sessions, never the Normal/CLT confidence interval
#          (analytical_summary() in simulate.R) -- that CI is a documented
#          poor approximation at the N these skewed games run at. If ever
#          surfaced elsewhere, it must be labelled "reference approximation".
#       3. The jackpot is always framed as a LONG-SHOT ("1-in-W", "do not
#          plan around it"), never as an expectation.
#       4. Probabilities near 0%/100% are worded ("essentially never" /
#          "almost always") instead of printing an awkward "0.0%"/"100.0%".
#       5. Losses and profits share one sign convention throughout: a loss is
#          always spoken as a positive "£X lost", a profit as "£X ahead" --
#          see pnl_phrase().
#
# source()-ing this file is side-effect free: it defines functions only, no
# reactive refs, no I/O.
# ==============================================================================


# ==============================================================================
# 1. FORMATTING / PLURALISATION HELPERS
# ==============================================================================

# ------------------------------------------------------------------------------
# pluralise(n, singular, plural = paste0(singular, "s"))
# ------------------------------------------------------------------------------
# "<n> <word>" with n comma-formatted and the correct singular/plural noun
# picked off |n| == 1 (so "1 play" vs "2 plays" vs "0 plays"; 0 is
# grammatically plural in English, which the |n| == 1 test gets right for
# free). `n` need not be an integer type, only integer-VALUED (counts of
# plays/wins are always whole numbers by the time they reach here).
pluralise <- function(n, singular, plural = paste0(singular, "s")) {
  word <- if (isTRUE(all.equal(abs(n), 1))) singular else plural
  sprintf("%s %s", format(n, big.mark = ",", trim = TRUE, scientific = FALSE), word)
}


# ------------------------------------------------------------------------------
# fmt_gbp(x, cents = NULL)
# ------------------------------------------------------------------------------
# Format a NON-NEGATIVE currency magnitude as "£...". Whole pounds
# (comma-grouped, rounded) by default; auto-switches to pence (2dp) for
# sub-£1 amounts so a 40p figure does not round away to "£0". Force either
# mode with `cents = TRUE/FALSE` (e.g. ticket PRICES always pass
# cents = TRUE, since £2.50 rounding to "£3" would misstate the stake).
#
# Callers pass MAGNITUDES (i.e. already abs()'d); this function never emits a
# minus sign -- the loss/profit sign is spoken in words by pnl_phrase(), per
# the file header's sign-convention rule.
fmt_gbp <- function(x, cents = NULL) {
  if (is.na(x)) return("\u00a3unknown")
  ax <- abs(x)
  use_cents <- if (is.null(cents)) (ax > 0 && ax < 1) else isTRUE(cents)
  if (use_cents) {
    paste0("\u00a3", formatC(ax, format = "f", digits = 2, big.mark = ","))
  } else {
    paste0("\u00a3", formatC(round(ax), format = "d", big.mark = ","))
  }
}


# ------------------------------------------------------------------------------
# pnl_phrase(x, cents = NULL)
# ------------------------------------------------------------------------------
# The app-wide sign convention rendered in prose: a loss (x < 0) reads
# "£X lost"; a profit (x > 0) reads "£X ahead"; (near-)zero reads "breaking
# even". Centralising this here means no sentence builder below ever has to
# hand-roll if (x < 0) sign logic.
pnl_phrase <- function(x, cents = NULL) {
  if (is.na(x)) return("an unknown amount")
  if (abs(x) < 0.005) return("breaking even")
  if (x < 0) sprintf("%s lost", fmt_gbp(-x, cents)) else sprintf("%s ahead", fmt_gbp(x, cents))
}


# ------------------------------------------------------------------------------
# fmt_pct(p, digits = 1)
# ------------------------------------------------------------------------------
# `p` (a 0-1 probability) as a percentage string, e.g. fmt_pct(0.034) ->
# "3.4%". Pure rounding/formatting; see describe_chance() for the
# honest-wording wrapper that avoids "0.0%"/"100.0%" at the extremes.
fmt_pct <- function(p, digits = 1) {
  sprintf("%.*f%%", digits, 100 * p)
}


# ------------------------------------------------------------------------------
# describe_chance(p, low = 0.005, high = 0.995, digits = 1, suffix = "of the time")
# ------------------------------------------------------------------------------
# Honest verbal wrapper around a probability. Below `low` (default 0.5%)
# reads "essentially never" rather than a misleadingly-precise "0.0%"; above
# `high` reads "almost always"; exactly 0/1 read "never"/"always"; otherwise
# a rounded percentage with `suffix` appended (pass suffix = "" to get a bare
# percentage, e.g. for use inside a longer clause).
describe_chance <- function(p, low = 0.005, high = 0.995, digits = 1,
                            suffix = "of the time") {
  if (is.na(p)) return("an uncertain amount")
  if (p <= 0) return("never")
  if (p >= 1) return("always")
  if (p < low)  return("essentially never")
  if (p > high) return("almost always")
  pct <- fmt_pct(p, digits)
  if (nzchar(suffix)) sprintf("about %s %s", pct, suffix) else sprintf("about %s", pct)
}


# ------------------------------------------------------------------------------
# jackpot_odds(p)
# ------------------------------------------------------------------------------
# 1-in-W odds for a per-outcome probability: W = round(1 / p). Returns
# NA_integer_ for p <= 0 or NA (no top-prize tier / never happens) instead of
# dividing by zero -- callers must check is.na(W) before narrating a jackpot
# clause (see .sentence_jackpot()), which is exactly the "no top-prize tier"
# guard the Phase 7 spec asks for.
jackpot_odds <- function(p) {
  if (is.na(p) || p <= 0) return(NA_integer_)
  as.integer(round(1 / p))
}


# ==============================================================================
# 2. INTERNAL EXTRACTORS -- prefer exact sim$totals, fall back to the metrics
#    bundle's percentile_se checkpoint grid.
# ==============================================================================

# ------------------------------------------------------------------------------
# .narrative_median(metrics, sim)
# ------------------------------------------------------------------------------
# The "typical" session P&L. Exact empirical median off raw sim$totals when a
# sim is supplied (matches the engine's own type-7 quantile exactly);
# otherwise the p50 row of metrics$percentile_se, which build_metrics()
# populates from the same probs grid simulate_sessions() checkpoints
# (default includes .5) -- so this is still the real simulated median, just
# read off the pre-computed bundle instead of raw totals.
.narrative_median <- function(metrics, sim = NULL) {
  if (!is.null(sim) && !is.null(sim$totals) && length(sim$totals) > 0L) {
    return(stats::median(sim$totals))
  }
  pse <- metrics$percentile_se
  if (is.null(pse) || nrow(pse) == 0L) return(NA_real_)
  row <- pse[abs(pse$prob - 0.5) < 1e-9, ]
  if (nrow(row) == 0L) return(NA_real_)
  row$quantile[1]
}


# ------------------------------------------------------------------------------
# .narrative_mean(metrics, sim)
# ------------------------------------------------------------------------------
# The mean session P&L: exact mean(sim$totals) when available, otherwise the
# analytical N * per-play EV already carried in metrics$dream$full$mean_session
# (build_metrics() computes this straight off `dist`, so it is exact, not an
# approximation -- only the simulated MEDIAN needs sim$totals for exactness).
.narrative_mean <- function(metrics, sim = NULL) {
  if (!is.null(sim) && !is.null(sim$totals) && length(sim$totals) > 0L) {
    return(mean(sim$totals))
  }
  if (!is.null(metrics$dream) && !is.null(metrics$dream$full)) {
    return(metrics$dream$full$mean_session)
  }
  NA_real_
}


# ------------------------------------------------------------------------------
# .narrative_interval(metrics, sim, conf)
# ------------------------------------------------------------------------------
# The EMPIRICAL (never Normal/CLT) interval a `conf` fraction of sessions
# fall within, e.g. conf = 0.90 -> the 5th/95th percentiles. This is the
# app's central teaching point (see simulate.R's header): at the N these
# games run at, the true session P&L distribution is far from Normal, so the
# analytical CI is only ever a labelled reference elsewhere in the app, never
# the headline interval narrated here.
#
# Prefers exact empirical quantiles from sim$totals; otherwise uses the
# CLOSEST available rows in metrics$percentile_se (the fan-chart checkpoint
# grid) and reports the ACTUAL coverage those rows represent (equal to the
# requested `conf` whenever the default .05/.25/.5/.75/.95 grid is used).
# Returns list(lo, hi, actual_conf), or NULL if there is nothing usable (too
# few distinct percentile rows to form a real interval).
.narrative_interval <- function(metrics, sim = NULL, conf = 0.90) {
  alpha <- 1 - conf
  lo_p  <- alpha / 2
  hi_p  <- 1 - alpha / 2

  if (!is.null(sim) && !is.null(sim$totals) && length(sim$totals) >= 2L) {
    q <- stats::quantile(sim$totals, probs = c(lo_p, hi_p), names = FALSE, type = 7)
    return(list(lo = q[1], hi = q[2], actual_conf = conf))
  }

  pse <- metrics$percentile_se
  if (is.null(pse) || nrow(pse) == 0L) return(NULL)
  lo_row <- pse[which.min(abs(pse$prob - lo_p)), ]
  hi_row <- pse[which.min(abs(pse$prob - hi_p)), ]
  if (isTRUE(all.equal(lo_row$prob, hi_row$prob))) return(NULL)  # nothing to span
  list(lo = lo_row$quantile, hi = hi_row$quantile,
       actual_conf = hi_row$prob - lo_row$prob)
}


# ==============================================================================
# 3. SENTENCE BUILDERS
# ==============================================================================

# ------------------------------------------------------------------------------
# .sentence_headline(metrics, sim)
# ------------------------------------------------------------------------------
# The lead sentence: N plays, stake, typical (median) result. Always present
# (requires only metrics$meta + a median, both guaranteed by build_metrics()).
.sentence_headline <- function(metrics, sim = NULL) {
  N     <- metrics$meta$N
  price <- metrics$meta$price
  stake <- metrics$meta$total_stake
  median_pnl <- .narrative_median(metrics, sim)

  sprintf(
    "Over %s at %s each (%s staked in total), you would typically end up %s.",
    pluralise(N, "play"), fmt_gbp(price, cents = TRUE), fmt_gbp(stake, cents = TRUE),
    pnl_phrase(median_pnl)
  )
}


# ------------------------------------------------------------------------------
# .sentence_mean_divergence(metrics, sim, threshold_frac = 0.05)
# ------------------------------------------------------------------------------
# Only emitted when the mean diverges from the median by more than
# `threshold_frac` of the total stake (floor £1) -- the honest reason NOT to
# quote the (jackpot-skewed) mean as "what happens to you". Returns NULL
# (silently omitted) when mean and median are close enough that mentioning
# both would just be noise.
.sentence_mean_divergence <- function(metrics, sim = NULL, threshold_frac = 0.05) {
  median_pnl <- .narrative_median(metrics, sim)
  mean_pnl   <- .narrative_mean(metrics, sim)
  if (is.na(median_pnl) || is.na(mean_pnl)) return(NULL)

  stake  <- metrics$meta$total_stake
  thresh <- max(1, threshold_frac * max(stake, 1, na.rm = TRUE))
  if (abs(mean_pnl - median_pnl) < thresh) return(NULL)

  sprintf(
    paste0("The AVERAGE result is different -- %s -- because a rare big prize ",
           "pulls the mean up; the typical (median) outcome above is the more ",
           "honest expectation for any one player."),
    pnl_phrase(mean_pnl)
  )
}


# ------------------------------------------------------------------------------
# .sentence_interval(metrics, sim, conf)
# ------------------------------------------------------------------------------
# The empirical interval sentence. Omitted (NULL) when .narrative_interval()
# has nothing usable to report -- never fabricates a range from one point.
.sentence_interval <- function(metrics, sim = NULL, conf = 0.90) {
  iv <- .narrative_interval(metrics, sim, conf)
  if (is.null(iv)) return(NULL)
  # Degenerate band: when the lower and upper percentiles coincide (e.g. a
  # single deterministic play, or a cheap game whose whole conf-band collapses
  # onto one loss value), "between £X and £X" reads clumsily -- state the
  # single value plainly instead.
  if (isTRUE(all.equal(iv$lo, iv$hi))) {
    return(sprintf(
      "Almost every session comes out the same: %s (from the simulated outcomes).",
      pnl_phrase(iv$lo)
    ))
  }
  sprintf(
    paste0("%s of the time, the result is between %s and %s ",
           "(from the simulated outcomes, not a theoretical approximation)."),
    fmt_pct(iv$actual_conf, digits = 0), pnl_phrase(iv$lo), pnl_phrase(iv$hi)
  )
}


# ------------------------------------------------------------------------------
# .sentence_profit_chance(metrics)
# ------------------------------------------------------------------------------
# How often the player ends up ahead at all (P&L > 0), honestly worded via
# describe_chance() so a near-zero chance reads "essentially never" instead
# of a spuriously precise "0.0%".
.sentence_profit_chance <- function(metrics) {
  p <- metrics$risk$p_profit
  sprintf("Chance of ending up ahead (any profit at all): %s.", describe_chance(p))
}


# ------------------------------------------------------------------------------
# .sentence_jackpot(metrics)
# ------------------------------------------------------------------------------
# Frames the top prize honestly as a long shot: "1-in-W chance per play"
# (the clearer, stake-independent framing -- labelled "per play"), plus,
# when it is not itself vanishingly small, the session-level chance of
# landing it at least once across all N plays (dream$p_top_in_N).
#
# Returns NULL (no jackpot clause at all) when there is no meaningful
# top-prize tier to narrate -- dream$p_top <= 0 or NA -- which also avoids
# the 1/p_top divide-by-zero.
.sentence_jackpot <- function(metrics) {
  dream <- metrics$dream
  if (is.null(dream)) return(NULL)
  W <- jackpot_odds(dream$p_top)
  if (is.na(W)) return(NULL)

  # top_value is a NET figure (prize minus the ticket cost): a £2,000,000
  # jackpot on a £5 ticket is £1,999,995 here. Say "net ... after the ticket
  # cost" or the number reads as a typo against the advertised prize.
  base <- sprintf(
    "The advertised big win is a 1-in-%s chance per play of the top prize (a net %s after the ticket cost) -- never plan around it.",
    format(W, big.mark = ",", scientific = FALSE, trim = TRUE), fmt_gbp(dream$top_value)
  )

  p_in_N <- dream$p_top_in_N
  if (!is.null(p_in_N) && !is.na(p_in_N) && p_in_N >= 0.0005) {
    base <- paste0(base, sprintf(
      " Across %s, that only adds up to %s of ever landing it.",
      pluralise(dream$N, "play"), describe_chance(p_in_N, suffix = "")
    ))
  }
  base
}


# ------------------------------------------------------------------------------
# .sentence_engagement(metrics)
# ------------------------------------------------------------------------------
# A tight, single-sentence flavour line: expected wins per session and the
# typical longest losing streak. Deliberately does not dump every engagement/
# streak field -- just the two that read naturally together. Omitted (NULL)
# when either bundle is missing.
.sentence_engagement <- function(metrics) {
  eng    <- metrics$engagement
  streak <- metrics$streak
  if (is.null(eng) || is.null(streak)) return(NULL)

  sprintf(
    paste0("Along the way, expect to win something on about %s of your %s (%s per play); ",
           "a losing streak of %s in a row with nothing is typical."),
    formatC(eng$expected_wins, format = "f", digits = 1),
    pluralise(eng$N, "play"), fmt_pct(eng$p_win),
    pluralise(round(streak$median), "play")
  )
}


# ==============================================================================
# 4. TOP-LEVEL GENERATOR
# ==============================================================================

# ------------------------------------------------------------------------------
# narrate_run(metrics, strategy = NULL, sim = NULL, conf = 0.90)
# ------------------------------------------------------------------------------
# Turn one build_metrics() bundle into a character vector of plain-English
# sentences: an optional strategy lead-in, the headline typical (median)
# result, an optional mean-divergence caveat, the empirical interval, the
# profit chance, the jackpot framing, and a tight engagement/streak flavour
# line. Pure function -- no reactive refs, no I/O -- so it is directly
# unit-testable and trivially callable from app.R's server on a
# pipeline_result() (metrics = res$metrics, strategy = res$strategy,
# sim = res$sim).
#
# `sim`, when supplied, upgrades the median/mean/interval from the metrics
# bundle's pre-computed checkpoint grid to EXACT figures off the raw
# sim$totals (see the .narrative_* extractors above); it is entirely optional
# -- narrate_run() works from `metrics` alone, which is what makes each
# sentence builder unit-testable against small hand-built fixtures without
# ever running a real simulation.
#
# Individual sentence builders return NULL when they have nothing honest to
# say (e.g. no top-prize tier, mean and median too close to bother
# distinguishing); c() silently drops NULLs, so the output length varies with
# what the bundle actually supports -- callers should not assume a fixed
# number of sentences, only a fixed ORDER for whichever are present.
narrate_run <- function(metrics, strategy = NULL, sim = NULL, conf = 0.90) {
  if (is.null(metrics) || is.null(metrics$meta)) {
    stop("narrate_run() needs a build_metrics() bundle (missing $meta).", call. = FALSE)
  }

  sentences <- c(
    .sentence_headline(metrics, sim),
    .sentence_mean_divergence(metrics, sim),
    .sentence_interval(metrics, sim, conf),
    .sentence_profit_chance(metrics),
    .sentence_jackpot(metrics),
    .sentence_engagement(metrics)
  )

  if (!is.null(strategy) && !is.null(strategy$label) && nzchar(strategy$label)) {
    sentences <- c(sprintf("Strategy: %s.", strategy$label), sentences)
  }

  sentences
}
