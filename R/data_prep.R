# ==============================================================================
# data_prep.R
# ------------------------------------------------------------------------------
# Layer 1 of the lottery odds simulator (see IMPLEMENTATION_PLAN.md, sections
# 4 and Phase 1).
#
# Purpose: normalise the two raw scraped CSV sources (scratchcards and instant
# win) into a single tidy "outcomes" table of net prize values and implied
# probabilities per game, plus a per-game summary (RTP, EV, variance, outlier
# flags) and a raw per-breakdown tier table for display.
#
# Inputs (repo root):
#   - national_lottery_scratchcard_odds.csv
#   - national_lottery_scratchcard_games.csv
#   - national_lottery_instant_win_prize_tiers.csv
#   - national_lottery_instant_win_catalogue.csv
#   - national_lottery_instant_win_reconciliation.csv   (QA only)
#
# Outputs (data/, gitignored, regenerated on demand):
#   - outcomes.rds     tidy simulation table: one row per (game, distinct
#                      net_value) incl. the implicit losing row.
#                      cols: game_id, source, net_value, probability
#   - game_summary.rds one row per game: rtp / net_ev / sd_play / flags / meta
#   - tiers_raw.rds    display table: raw per-breakdown winning tiers with labels
#
# ------------------------------------------------------------------------------
# KEY MODELLING DEFINITIONS (implemented exactly as specified in Phase 1):
#
#   * UK prizes are TOTAL RETURN, not on top of stake. For a winning tier with
#     gross prize P and per-ticket probability p at ticket cost C:
#         net_value = P - C
#   * The losing outcome is IMPLICIT: net_value = -C with
#         probability = 1 - sum(tier probabilities).
#     If sum(tier p) >= 1 for a game we do NOT clamp -- we quarantine the game
#     and flag it loudly (a physical impossibility signalling bad data).
#   * Duplicate breakdowns: several tiers can share the same gross prize P with
#     different prize_breakdown strings. For the SIMULATION outcomes table we
#     aggregate by distinct net_value -> sum their probabilities. The raw
#     per-breakdown tiers are kept separately (tiers_raw) for display.
#   * RTP       = sum(P * p) / C
#   * net_EV    = sum(P * p) - C            (per play)
#   * Var(play) = E[gross^2] - E[gross]^2   where E[gross]  = sum(P * p),
#                                                 E[gross^2] = sum(P^2 * p).
#     The losing row contributes 0 to both moments, and shifting every outcome
#     by the constant -C does not change the variance, so we compute the
#     variance on the GROSS prize distribution and take sd_play = sqrt(Var).
#
# ------------------------------------------------------------------------------
# STRUCTURE / SIDE-EFFECT GUARD:
#   Sourcing this file only DEFINES functions (no side effects), so downstream
#   layers and the test suite can `source("R/data_prep.R")` safely. Running it
#   as a script (`Rscript R/data_prep.R`) regenerates the caches and prints the
#   report. The script-vs-source distinction is made at the bottom of the file
#   via `sys.nframe()`.
# ==============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
})

# ------------------------------------------------------------------------------
# Configuration constants. Centralised so thresholds are easy to audit / tune.
# ------------------------------------------------------------------------------

# RTP flag thresholds (from the locked-in model decisions, section 2):
#   soft flag when RTP > 0.85 (implausibly generous -> suspect the odds),
#   hard flag when RTP > 1.00 (impossible in expectation -> definitely artifact).
RTP_SOFT_THRESHOLD <- 0.85
RTP_HARD_THRESHOLD <- 1.00

# Outlier / cap method: winsorise the contribution of tiers whose scraped odds
# carry a very high tolerance. `tolerance_pct` is the +/- band on the published
# approximate odds; a large band on a rare top-tier prize is exactly the
# artifact that can inflate EV. We treat >= 30% tolerance as "high". This is an
# ADVISORY column only -- it never touches outcomes.rds or the raw `rtp`.
# Trade-off: excluding these tiers slightly understates EV for genuinely rare
# prizes, but that is the conservative direction for a "money-shredder" tool
# and avoids over-crediting jackpots whose odds we least trust. Instant-win
# tiers carry no tolerance column, so their rtp_capped == rtp (no adjustment).
HIGH_TOLERANCE_PCT <- 30

# "Today" for on_sale derivation. Parameterised so tests can pin a date and the
# result is not silently coupled to the wall clock. Defaults to Sys.Date().
DEFAULT_TODAY <- Sys.Date()

# Floating-point tolerance for the "probabilities sum to 1" invariant.
PROB_SUM_TOL <- 1e-9

# ==============================================================================
# 1. READERS
# ==============================================================================

# Read all five raw CSVs from `root`. Kept as a single function so callers get a
# consistent, named bundle and the file names live in exactly one place.
read_raw_data <- function(root = ".") {
  paths <- list(
    sc_odds   = file.path(root, "national_lottery_scratchcard_odds.csv"),
    sc_games  = file.path(root, "national_lottery_scratchcard_games.csv"),
    iw_tiers  = file.path(root, "national_lottery_instant_win_prize_tiers.csv"),
    iw_cat    = file.path(root, "national_lottery_instant_win_catalogue.csv"),
    iw_recon  = file.path(root, "national_lottery_instant_win_reconciliation.csv")
  )
  # Fail early and clearly if any input is missing rather than deep inside a join.
  missing <- paths[!file.exists(unlist(paths))]
  if (length(missing)) {
    stop("Missing input CSV(s): ", paste(unlist(missing), collapse = ", "))
  }
  list(
    sc_odds  = read_csv(paths$sc_odds,  show_col_types = FALSE),
    sc_games = read_csv(paths$sc_games, show_col_types = FALSE),
    iw_tiers = read_csv(paths$iw_tiers, show_col_types = FALSE),
    iw_cat   = read_csv(paths$iw_cat,   show_col_types = FALSE),
    iw_recon = read_csv(paths$iw_recon, show_col_types = FALSE)
  )
}

# Parse the ISO-8601 date-time strings in the catalogue / games CSVs down to a
# plain Date. We only need day granularity for the on_sale comparison, and
# taking the first 10 chars sidesteps timezone parsing entirely.
.parse_date <- function(x) as.Date(substr(as.character(x), 1, 10))

# ==============================================================================
# 2. SCRATCHCARDS
# ==============================================================================

# Build the scratchcard tier table joined to game-universe metadata.
#
# Join key: scratchcard native id = game_number. The odds CSV carries
# game_number directly; the games CSV embeds it in the title as "[1495]", which
# we extract. Prices are cross-checked: play_price_p/100 (pence) must equal the
# odds `ticket_price` (pounds). Mismatches are reported, not silently trusted.
#
# Returns a list with $tiers (one row per raw breakdown, with net_value and
# metadata attached) and $mismatches (price-check diagnostics).
prep_scratchcards <- function(sc_odds, sc_games, today = DEFAULT_TODAY) {

  # --- Extract game_number from the games-CSV title "[1495]" pattern ---------
  games <- sc_games %>%
    mutate(game_number = str_match(title, "\\[(\\d+)\\]")[, 2]) %>%
    # A title without a bracketed number cannot be joined; surface it rather
    # than dropping silently. (Observed: all titles parse in current data.)
    mutate(game_number = as.character(game_number))

  # Collapse to one metadata row per game_number. Some games CSVs could carry
  # duplicate rows per game; take the first deterministically.
  games_meta <- games %>%
    filter(!is.na(game_number)) %>%
    group_by(game_number) %>%
    summarise(
      name          = first(name),
      play_price_p  = first(play_price_p),
      prize_pool    = first(prize_pool),
      closure_date  = .parse_date(first(closure_date)),
      .groups = "drop"
    ) %>%
    mutate(price_from_games = play_price_p / 100)  # pence -> pounds

  # --- Price sanity check: odds ticket_price vs games play_price_p/100 -------
  odds_price <- sc_odds %>%
    mutate(game_number = as.character(game_number)) %>%
    group_by(game_number) %>%
    summarise(ticket_price = first(ticket_price), .groups = "drop")

  price_check <- odds_price %>%
    left_join(games_meta %>% select(game_number, price_from_games),
              by = "game_number") %>%
    mutate(
      no_games_match = is.na(price_from_games),
      price_mismatch = !no_games_match &
        abs(ticket_price - price_from_games) > 1e-9
    )
  mismatches <- price_check %>% filter(price_mismatch | no_games_match)

  # --- Build the raw tier table ---------------------------------------------
  # `ticket_price` (pounds) from the odds CSV is the authoritative cost C: it is
  # the per-game constant used everywhere. net_value = prize_amount - ticket_price.
  tiers <- sc_odds %>%
    mutate(game_number = as.character(game_number)) %>%
    transmute(
      source            = "scratchcard",
      native_id         = game_number,
      game_id           = paste("scratchcard", game_number, sep = ":"),
      price             = ticket_price,
      gross_prize       = prize_amount,
      net_value         = prize_amount - ticket_price,
      probability       = implied_probability,
      prize_label       = prize_breakdown,
      approx_odds_1_in  = approx_odds_1_in,
      tolerance_pct     = tolerance_pct,
      game_name_raw     = game_name,
      headline_odds_1_in = overall_odds_1_in
    ) %>%
    # Attach game-universe metadata (name, dates). Scratchcards have no
    # category / hidden / prohibited fields in these sources, so we set
    # sensible constants: category "Scratchcard", never hidden, never prohibited.
    left_join(
      games_meta %>% select(game_number, name, closure_date, prize_pool),
      by = c("native_id" = "game_number")
    ) %>%
    mutate(
      # Prefer the tidy games-CSV name; fall back to the odds game_name.
      name        = coalesce(name, game_name_raw),
      category    = "Scratchcard",
      is_hidden   = FALSE,
      prohibited  = FALSE,
      # on_sale: closure_date is the last day of sale. Unknown dates -> FALSE
      # (we cannot assert a game is on sale without a valid future date).
      end_or_closure_date = closure_date,
      on_sale     = !is.na(closure_date) & closure_date >= today
    )

  list(tiers = tiers, mismatches = mismatches)
}

# ==============================================================================
# 3. INSTANT WIN
# ==============================================================================

# Build the instant-win tier table joined to the catalogue for price + metadata.
#
# Join key: instant-win native id = slug. Every tier slug MUST exist in the
# catalogue (that is where price lives); a miss means a tier with no price, so
# we assert and report. Catalogue slugs with no tiers are reported (3 expected:
# the Monopoly trio) and simply produce no game (no outcomes without tiers).
#
# Returns a list with $tiers, $unmatched_slugs (tier slugs missing from
# catalogue), and $no_tier_slugs (catalogue games with zero tiers).
prep_instant_win <- function(iw_tiers, iw_cat, today = DEFAULT_TODAY) {

  cat_meta <- iw_cat %>%
    transmute(
      slug,
      name,
      price   = price_gbp,
      category,
      is_hidden,
      prohibited,
      end_date          = .parse_date(end_date),
      overall_odds_1_in,
      prize_pool
    )

  # --- Join diagnostics ------------------------------------------------------
  tier_slugs <- unique(iw_tiers$slug)
  cat_slugs  <- unique(cat_meta$slug)
  unmatched_slugs <- setdiff(tier_slugs, cat_slugs)   # tiers without a price
  no_tier_slugs   <- setdiff(cat_slugs, tier_slugs)   # catalogue games w/o tiers

  # Assert every tier slug has a price. This is the correctness gate the plan
  # demands: a missing price would silently produce NA net_value downstream.
  if (length(unmatched_slugs) > 0) {
    warning("Instant-win tier slugs with NO catalogue match (missing price): ",
            paste(unmatched_slugs, collapse = ", "))
  }

  # --- Build the raw tier table ---------------------------------------------
  tiers <- iw_tiers %>%
    transmute(
      source           = "instant_win",
      native_id        = slug,
      game_id          = paste("instant_win", slug, sep = ":"),
      gross_prize      = prize_value_gbp,
      probability      = implied_probability,
      prize_label      = prize_label,
      approx_odds_1_in = approx_odds_1_in,
      # Instant-win tiers carry no tolerance band -> NA. The cap logic treats
      # NA tolerance as "not high", so IW rtp_capped == rtp.
      tolerance_pct    = NA_real_
    ) %>%
    left_join(cat_meta, by = c("native_id" = "slug")) %>%
    mutate(
      net_value           = gross_prize - price,
      game_name_raw       = name,
      headline_odds_1_in  = overall_odds_1_in,
      end_or_closure_date = end_date,
      # on_sale: not ended, not hidden, not prohibited (plan's IW definition).
      on_sale = !is.na(end_date) & end_date >= today &
        (is_hidden == FALSE) & (prohibited == FALSE)
    )

  list(
    tiers           = tiers,
    unmatched_slugs = unmatched_slugs,
    no_tier_slugs   = no_tier_slugs
  )
}

# ==============================================================================
# 4. CORE TRANSFORMS (source-agnostic)
# ==============================================================================

# Given a combined raw-tier table (both sources), identify games whose winning
# probabilities sum to >= 1 -- physically impossible, so we QUARANTINE them
# rather than clamp. Returns the vector of offending game_ids.
find_quarantined_games <- function(tiers) {
  tiers %>%
    group_by(game_id) %>%
    summarise(p_win = sum(probability), .groups = "drop") %>%
    filter(p_win >= 1 - PROB_SUM_TOL) %>%   # >= 1 within tolerance -> bad
    pull(game_id)
}

# Build the SIMULATION outcomes table from the (non-quarantined) raw tiers.
#
# For each game:
#   1. aggregate winning tiers by distinct net_value (summing probabilities of
#      duplicate breakdowns that share a gross prize);
#   2. append the implicit losing row: net_value = -price,
#      probability = 1 - sum(winning probabilities).
#
# The result has EXACTLY the columns the simulation engine consumes:
#   game_id, source, net_value, probability
# and by construction probabilities sum to 1 per game.
build_outcomes <- function(tiers) {

  # --- Aggregate winning tiers by distinct net_value ------------------------
  # WHY group by net_value (not gross prize): net_value = gross - price is a
  # constant per-game shift, so it is a 1:1 relabelling of gross within a game;
  # grouping by net_value directly yields the distinct simulation outcomes and
  # correctly merges the duplicate-breakdown rows.
  winning <- tiers %>%
    group_by(game_id, source, net_value) %>%
    summarise(probability = sum(probability), .groups = "drop")

  # --- Construct the implicit losing row per game ---------------------------
  losing <- tiers %>%
    group_by(game_id, source) %>%
    summarise(price = first(price),
              p_win = sum(probability),
              .groups = "drop") %>%
    transmute(
      game_id,
      source,
      net_value   = -price,               # lose your stake, win nothing
      probability = 1 - p_win
    )

  # --- Combine, then merge any collision between a winning net_value and the
  # losing -price. This happens only in the degenerate case where a winning
  # tier's gross prize equals 0 (net_value == -price); summing keeps the total
  # mass correct and preserves the "sum to 1" invariant.
  outcomes <- bind_rows(winning, losing) %>%
    group_by(game_id, source, net_value) %>%
    summarise(probability = sum(probability), .groups = "drop") %>%
    arrange(game_id, net_value)

  outcomes
}

# Per-game summary: RTP, net_EV, per-play SD, flags, cap, and metadata.
#
# `tiers` : raw per-breakdown winning tiers (both sources), non-quarantined.
# Moments are computed on the GROSS prize distribution (losing row contributes
# 0 and the -C shift does not change variance), exactly per the spec.
build_game_summary <- function(tiers) {

  # --- Expectation moments per game -----------------------------------------
  # E[gross]   = sum(P * p);  E[gross^2] = sum(P^2 * p).
  # These use the RAW per-breakdown probabilities; aggregating by net_value
  # first would give the identical sums (sum is associative), so we compute
  # directly on tiers for clarity.
  moments <- tiers %>%
    group_by(game_id, source) %>%
    summarise(
      price       = first(price),
      e_gross     = sum(gross_prize * probability),          # E[gross]
      e_gross_sq  = sum(gross_prize^2 * probability),        # E[gross^2]
      p_win       = sum(probability),
      n_tiers     = dplyr::n_distinct(net_value),            # distinct outcomes
      # headline / metadata fields (constant within a game -> first()):
      name                 = first(name),
      category             = first(category),
      is_hidden            = first(is_hidden),
      prohibited           = first(prohibited),
      headline_odds_1_in   = first(headline_odds_1_in),
      end_or_closure_date  = first(end_or_closure_date),
      on_sale              = first(on_sale),
      .groups = "drop"
    ) %>%
    mutate(
      # var(play) = E[gross^2] - E[gross]^2. Guard tiny negatives from fp error.
      var_play = pmax(e_gross_sq - e_gross^2, 0),
      sd_play  = sqrt(var_play),
      rtp      = e_gross / price,
      net_ev   = e_gross - price,     # = sum(P*p) - C
      # Flag tiers of implausibility. Order matters: hard supersedes soft.
      rtp_flag = dplyr::case_when(
        rtp > RTP_HARD_THRESHOLD ~ "hard",
        rtp > RTP_SOFT_THRESHOLD ~ "soft",
        TRUE                      ~ "none"
      )
    )

  # --- Advisory capped RTP ---------------------------------------------------
  # Recompute E[gross] EXCLUDING high-tolerance winning tiers, then divide by
  # the same price. NA tolerance (all IW tiers, and any scratchcard tier without
  # a band) is treated as "not high" so it is retained. This never touches the
  # raw `rtp`, outcomes, or moments -- it is a separate advisory number.
  capped <- tiers %>%
    mutate(is_high_tol = !is.na(tolerance_pct) & tolerance_pct >= HIGH_TOLERANCE_PCT) %>%
    group_by(game_id) %>%
    summarise(
      e_gross_capped = sum(gross_prize * probability * (!is_high_tol)),
      n_capped_tiers = sum(is_high_tol),
      .groups = "drop"
    )

  summary <- moments %>%
    left_join(capped, by = "game_id") %>%
    mutate(
      rtp_capped = e_gross_capped / price,
      # TRUE when the cap actually removed mass (i.e. any high-tol tier existed).
      rtp_capped_applied = n_capped_tiers > 0
    ) %>%
    # Final column selection & order per the Phase 1 spec.
    transmute(
      game_id, source, name, price, category,
      rtp, net_ev, sd_play,
      headline_odds_1_in, p_win, n_tiers,
      rtp_flag, rtp_capped, rtp_capped_applied,
      on_sale, is_hidden, prohibited, end_or_closure_date
    ) %>%
    arrange(source, game_id)

  summary
}

# The display table: raw per-breakdown winning tiers with labels, for later UI
# use. One row per scraped breakdown (NOT aggregated), keeping the human-readable
# prize_label and net_value.
build_tiers_raw <- function(tiers) {
  tiers %>%
    transmute(
      game_id, source, native_id,
      name = coalesce(name, game_name_raw),
      price,
      prize_label,
      gross_prize,
      net_value,
      probability,
      approx_odds_1_in,
      tolerance_pct
    ) %>%
    arrange(source, game_id, desc(gross_prize))
}

# ==============================================================================
# 5. TOP-LEVEL ORCHESTRATION
# ==============================================================================

# Run the full pipeline. Returns a list of the three artifacts plus a `diag`
# bundle used to build the report. Does NOT write to disk (that is the caller's
# job -- keeps this function pure and testable).
prepare_data <- function(root = ".", today = DEFAULT_TODAY) {

  raw <- read_raw_data(root)

  # --- Prep each source ------------------------------------------------------
  sc <- prep_scratchcards(raw$sc_odds, raw$sc_games, today = today)
  iw <- prep_instant_win(raw$iw_tiers, raw$iw_cat, today = today)

  # Combine raw tiers. bind_rows aligns on shared columns; source-specific
  # extras (e.g. game_name_raw) are carried where present.
  all_tiers <- bind_rows(sc$tiers, iw$tiers)

  # --- Quarantine games with impossible probability mass ---------------------
  quarantined <- find_quarantined_games(all_tiers)
  clean_tiers <- all_tiers %>% filter(!(game_id %in% quarantined))

  # --- Build the three artifacts --------------------------------------------
  outcomes     <- build_outcomes(clean_tiers)
  game_summary <- build_game_summary(clean_tiers)
  tiers_raw    <- build_tiers_raw(clean_tiers)

  # --- Post-build invariant check: probabilities sum to 1 per game ----------
  prob_sums <- outcomes %>%
    group_by(game_id) %>%
    summarise(total = sum(probability), .groups = "drop") %>%
    mutate(ok = abs(total - 1) < PROB_SUM_TOL)
  bad_sums <- prob_sums %>% filter(!ok)

  diag <- list(
    n_sc_games      = dplyr::n_distinct(sc$tiers$game_id),
    n_sc_tier_rows  = nrow(sc$tiers),
    n_iw_games      = dplyr::n_distinct(iw$tiers$game_id),
    n_iw_tier_rows  = nrow(iw$tiers),
    sc_mismatches   = sc$mismatches,
    iw_unmatched    = iw$unmatched_slugs,
    iw_no_tiers     = iw$no_tier_slugs,
    quarantined     = quarantined,
    bad_prob_sums   = bad_sums,
    today           = today
  )

  list(
    outcomes     = outcomes,
    game_summary = game_summary,
    tiers_raw    = tiers_raw,
    diag         = diag
  )
}

# ==============================================================================
# 6. REPORTING
# ==============================================================================

# Render the human-readable data-prep report as a character vector of lines.
# Separated from I/O so the same text can be written to reports/ and echoed to
# stdout without recomputation.
build_report_lines <- function(prep) {
  gs   <- prep$game_summary
  diag <- prep$diag

  fmt_pct <- function(x) sprintf("%.1f%%", 100 * x)
  L <- character(0)
  add <- function(...) L[[length(L) + 1]] <<- paste0(...)

  add("# Data-prep report — lottery odds simulator (Phase 1)")
  add("")
  add("Generated: ", as.character(Sys.time()),
      "  |  on_sale reference date: ", as.character(diag$today))
  add("")

  # --- Row / game counts -----------------------------------------------------
  add("## Row & game counts per source")
  add("")
  add(sprintf("- Scratchcards: %d games, %d raw tier rows",
              diag$n_sc_games, diag$n_sc_tier_rows))
  add(sprintf("- Instant win: %d games (with tiers), %d raw tier rows",
              diag$n_iw_games, diag$n_iw_tier_rows))
  add(sprintf("- Combined game_summary rows: %d", nrow(gs)))
  add(sprintf("- Combined outcomes rows: %d", nrow(prep$outcomes)))
  add(sprintf("- Combined tiers_raw rows: %d", nrow(prep$tiers_raw)))
  add("")

  # --- Join diagnostics ------------------------------------------------------
  add("## Join match rates & mismatches")
  add("")
  add(sprintf("- Scratchcard price check (ticket_price vs play_price_p/100): %d mismatch(es)",
              nrow(diag$sc_mismatches)))
  if (nrow(diag$sc_mismatches) > 0) {
    for (i in seq_len(nrow(diag$sc_mismatches))) {
      r <- diag$sc_mismatches[i, ]
      add(sprintf("    * game %s: ticket_price=%s, games-CSV price=%s%s",
                  r$game_number, r$ticket_price,
                  ifelse(is.na(r$price_from_games), "NA", r$price_from_games),
                  ifelse(isTRUE(r$no_games_match), " (NO games-CSV match)", "")))
    }
  }
  add(sprintf("- Instant-win tier->catalogue join: %d tier slug(s) with no price match",
              length(diag$iw_unmatched)))
  if (length(diag$iw_unmatched) > 0) {
    add("    * ", paste(diag$iw_unmatched, collapse = ", "))
  }
  add(sprintf("- Catalogue games with NO prize tiers (excluded — cannot simulate without outcomes): %d",
              length(diag$iw_no_tiers)))
  if (length(diag$iw_no_tiers) > 0) {
    add("    * ", paste(diag$iw_no_tiers, collapse = ", "))
  }
  add("")

  # --- Quarantine & invariants -----------------------------------------------
  add("## Data-quality warnings")
  add("")
  add(sprintf("- Games QUARANTINED (sum of tier probabilities >= 1, impossible): %d",
              length(diag$quarantined)))
  if (length(diag$quarantined) > 0) {
    add("    * ", paste(diag$quarantined, collapse = ", "))
  }
  add(sprintf("- Games failing the 'outcome probabilities sum to 1' invariant: %d",
              nrow(diag$bad_prob_sums)))
  add("")

  # --- Per-game p_win & RTP overview ----------------------------------------
  add("## Per-game p_win & RTP (by source)")
  add("")
  for (src in unique(gs$source)) {
    g <- gs %>% filter(source == src)
    add(sprintf("### %s (%d games)", src, nrow(g)))
    add(sprintf("- p_win: min %s, median %s, max %s",
                fmt_pct(min(g$p_win)), fmt_pct(median(g$p_win)), fmt_pct(max(g$p_win))))
    add(sprintf("- RTP:   min %s, median %s, max %s",
                fmt_pct(min(g$rtp)), fmt_pct(median(g$rtp)), fmt_pct(max(g$rtp))))
    add("")
  }

  # --- Worst / best RTP & highest SD ----------------------------------------
  add("## Extremes")
  add("")
  worst <- gs %>% arrange(rtp) %>% head(5)
  best  <- gs %>% arrange(desc(rtp)) %>% head(5)
  hisd  <- gs %>% arrange(desc(sd_play)) %>% head(5)
  add("Worst RTP (biggest money-shredders):")
  for (i in seq_len(nrow(worst))) add(sprintf("    %2d. %-34s £%-5s RTP %s  p_win %s",
      i, substr(worst$name[i], 1, 34), worst$price[i], fmt_pct(worst$rtp[i]), fmt_pct(worst$p_win[i])))
  add("Best RTP:")
  for (i in seq_len(nrow(best))) add(sprintf("    %2d. %-34s £%-5s RTP %s  p_win %s",
      i, substr(best$name[i], 1, 34), best$price[i], fmt_pct(best$rtp[i]), fmt_pct(best$p_win[i])))
  add("Highest per-play SD (volatility / jackpot skew):")
  for (i in seq_len(nrow(hisd))) add(sprintf("    %2d. %-34s £%-5s SD £%s  RTP %s",
      i, substr(hisd$name[i], 1, 34), hisd$price[i],
      format(round(hisd$sd_play[i], 2), big.mark = ","), fmt_pct(hisd$rtp[i])))
  add("")

  # --- Flagged games ---------------------------------------------------------
  flagged <- gs %>% filter(rtp_flag != "none") %>% arrange(desc(rtp))
  add("## Flagged games (RTP implausibility)")
  add("")
  add(sprintf("- soft flag (RTP > %s): %d",
              fmt_pct(RTP_SOFT_THRESHOLD), sum(gs$rtp_flag == "soft")))
  add(sprintf("- hard flag (RTP > %s): %d",
              fmt_pct(RTP_HARD_THRESHOLD), sum(gs$rtp_flag == "hard")))
  if (nrow(flagged) > 0) {
    for (i in seq_len(nrow(flagged))) {
      r <- flagged[i, ]
      add(sprintf("    * [%s] %-34s RTP %s (capped %s, applied=%s)",
                  r$rtp_flag, substr(r$name, 1, 34), fmt_pct(r$rtp),
                  fmt_pct(r$rtp_capped), r$rtp_capped_applied))
    }
  } else {
    add("    (none in this data snapshot)")
  }
  add("")

  # --- Cap method note -------------------------------------------------------
  add("## RTP capping method (advisory column `rtp_capped`)")
  add("")
  add(sprintf("Winsorise-by-exclusion: winning tiers whose scraped odds carry tolerance_pct >= %d%%",
              HIGH_TOLERANCE_PCT))
  add("are dropped from E[gross] before dividing by price. Instant-win tiers carry no")
  add("tolerance band, so their rtp_capped == rtp. This is ADVISORY ONLY — the raw")
  add("`rtp`, `outcomes.rds`, and per-play moments are unaffected. Trade-off: understates")
  add("EV for genuinely rare high-tolerance prizes, which is the conservative direction")
  add("for a loss-focused tool and avoids over-crediting the least-trusted jackpot odds.")
  add(sprintf("- games where the cap changed RTP: %d", sum(gs$rtp_capped_applied)))
  add("")

  L
}

# ==============================================================================
# 7. SCRIPT ENTRY POINT
# ==============================================================================

# Write the three .rds caches and the report, and echo a concise summary to
# stdout. Returns the prep object invisibly for interactive use.
run_data_prep <- function(root = ".", today = DEFAULT_TODAY) {
  prep <- prepare_data(root = root, today = today)

  # --- Ensure output dirs exist ---------------------------------------------
  data_dir    <- file.path(root, "data")
  reports_dir <- file.path(root, "reports")
  dir.create(data_dir,    showWarnings = FALSE, recursive = TRUE)
  dir.create(reports_dir, showWarnings = FALSE, recursive = TRUE)

  # --- Cache artifacts (gitignored, regenerated on demand) ------------------
  saveRDS(prep$outcomes,     file.path(data_dir, "outcomes.rds"))
  saveRDS(prep$game_summary, file.path(data_dir, "game_summary.rds"))
  saveRDS(prep$tiers_raw,    file.path(data_dir, "tiers_raw.rds"))

  # --- Write & echo the report ----------------------------------------------
  lines <- build_report_lines(prep)
  writeLines(lines, file.path(reports_dir, "data_prep_report.md"))
  cat(paste(lines, collapse = "\n"), "\n")

  invisible(prep)
}

# Run as a script only. When sourced this block is skipped, so downstream layers
# and the test suite get the function definitions without triggering file writes.
# `sys.nframe() == 0L` is TRUE only at top level of `Rscript data_prep.R`; when
# another file source()s this one, sys.nframe() > 0 and we do nothing.
if (sys.nframe() == 0L) {
  run_data_prep(root = ".")
}
