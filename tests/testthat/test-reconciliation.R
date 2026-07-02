# ==============================================================================
# test-reconciliation.R  (Phase 10)
# ------------------------------------------------------------------------------
# Validate the whole data-prep -> outcomes pipeline against an INDEPENDENT
# published figure: the per-game overall win odds in
# national_lottery_instant_win_reconciliation.csv. The CSV carries, per slug:
#   page_overall_1_in     -- the "1 in X to win" printed on the game page
#   implied_overall_1_in  -- 1 / sum(scraped per-tier probabilities)
#   catalogue_overall_1_in
#   diff_pct              -- |implied - page| / page * 100
#
# We recompute the implied overall win odds from OUR prepared artifacts and pin
# that the pipeline preserved the per-tier odds. Two complementary definitions
# of "win" are involved, and reconciling BOTH is the whole point:
#
#   (1) ANY TIER FIRED  = sum of all scraped tier probabilities. This is what the
#       CSV's implied_overall_1_in is (1 / that sum). Our prepared `tiers_raw`
#       lets us recompute it directly, and `game_summary$p_win` is the pipeline's
#       own copy of it. Both must equal the CSV to ~machine precision -- that is
#       the headline reconciliation: the pipeline did not corrupt the odds.
#
#   (2) NET MONEY GAIN  = sum(prob[net_value > -price]) over the `outcomes`
#       table, i.e. "won more than the ticket cost back" -- the definition
#       engagement_metrics() uses. This equals (1) for MOST games, but NOT for a
#       handful whose scraped data contains prize tiers LABELLED as small cash
#       (e.g. "£0.25", "£0.50") yet carrying gross_prize == 0. Those tiers have
#       net_value == -price, so build_outcomes() correctly folds them into the
#       losing row: they are wins by the published "any prize" count but not by
#       net money gain. This is a RAW-DATA scraping artifact (gross_prize parsed
#       as 0 for cash-labelled tiers), NOT a data_prep defect -- data_prep does
#       net_value = gross_prize - price exactly as specified.
#
# DOCUMENTED SYSTEMATIC OFFSET: for the small "phantom-zero" set, the money-gain
# implied odds are LARGER (rarer) than the published overall odds by exactly the
# gross_prize == 0 tier mass. We derive that set from the data (not a hardcoded
# list), assert it is small, and assert the offset is fully explained by the
# zero-value tier mass -- so a real regression (odds silently dropped/duplicated
# on a normal game) still fails loudly, while the known artifact does not mask it.
# ==============================================================================

# --- locate + load the prepared artifacts (tests run from tests/testthat) -----
if (!exists(".phase10_root", inherits = TRUE)) {
  .phase10_root <- local({
    cands <- c(".", "..", "../..", "../../..")
    hit <- cands[file.exists(file.path(cands, "data", "outcomes.rds"))][1]
    if (is.na(hit)) {
      stop("Could not locate data/outcomes.rds; build it with ",
           "Rscript -e 'source(\"R/data_prep.R\"); run_data_prep(\".\")'")
    }
    hit
  })
}

.recon_data <- local({
  root <- .phase10_root
  outcomes <- readRDS(file.path(root, "data", "outcomes.rds"))
  gs       <- readRDS(file.path(root, "data", "game_summary.rds"))
  tiers    <- readRDS(file.path(root, "data", "tiers_raw.rds"))
  recon    <- utils::read.csv(
    file.path(root, "national_lottery_instant_win_reconciliation.csv"),
    stringsAsFactors = FALSE)
  list(outcomes = outcomes, gs = gs, tiers = tiers, recon = recon)
})

# Slug = the part of an instant_win game_id after "instant_win:".
.iw_slug <- function(game_id) sub("^instant_win:", "", game_id)


# ------------------------------------------------------------------------------
test_that("reconciliation CSV joins cleanly onto our instant-win games", {
  gs    <- .recon_data$gs
  recon <- .recon_data$recon

  iw_slugs <- .iw_slug(gs$game_id[gs$source == "instant_win"])
  # Every reconciliation row must map to a prepared game, and vice versa: a
  # dropped or spuriously added game would break this and is worth catching.
  expect_setequal(recon$slug, iw_slugs)
  expect_equal(nrow(recon), 79L)                # snapshot size (documented)
  expect_false(any(duplicated(recon$slug)))
})


# ------------------------------------------------------------------------------
test_that("recomputed implied overall odds (any tier fired) match the CSV to ~machine precision", {
  # This is the HEADLINE reconciliation. Recompute 1 / sum(per-tier prob) from
  # BOTH prepared artifacts (the raw-tier table and game_summary's p_win) and
  # pin them against the independently-derived CSV implied_overall_1_in. They
  # must agree to floating-point precision -- the pipeline stored the odds
  # verbatim.
  tiers <- .recon_data$tiers
  gs    <- .recon_data$gs
  recon <- .recon_data$recon

  iw_tiers <- tiers[tiers$source == "instant_win", ]
  # Recompute "any tier fired" straight from the raw-tier artifact.
  p_anytier <- tapply(iw_tiers$probability,
                      .iw_slug(iw_tiers$game_id), sum)

  # game_summary's own p_win is the pipeline's copy of the same quantity.
  gs_iw <- gs[gs$source == "instant_win", ]
  p_gs  <- setNames(gs_iw$p_win, .iw_slug(gs_iw$game_id))

  m <- recon
  m$p_anytier <- p_anytier[m$slug]
  m$p_gs      <- p_gs[m$slug]
  m$impl_recompute <- 1 / m$p_anytier

  # Recompute vs CSV: tight relative tolerance.
  rel_csv <- abs(m$impl_recompute - m$implied_overall_1_in) / m$implied_overall_1_in
  expect_lt(max(rel_csv), 1e-6)

  # tiers_raw and game_summary must agree with each other exactly (same source).
  expect_equal(as.numeric(m$p_anytier), as.numeric(m$p_gs), tolerance = 1e-9)

  message(sprintf(
    "[reconciliation] any-tier implied vs CSV: max relative diff = %.2e over %d games",
    max(rel_csv), nrow(m)))
})


# ------------------------------------------------------------------------------
test_that("net-money-gain implied odds match the CSV for all non-artifact games", {
  # The engagement-metrics definition of a win (net_value > -price) recomputed
  # from the `outcomes` table. This equals the published "any tier" odds EXCEPT
  # for games carrying cash-labelled gross_prize == 0 tiers, which we derive from
  # the data rather than hardcode.
  outcomes <- .recon_data$outcomes
  gs       <- .recon_data$gs
  tiers    <- .recon_data$tiers
  recon    <- .recon_data$recon

  price <- setNames(gs$price, gs$game_id)
  iw <- outcomes[outcomes$source == "instant_win", ]

  # P(net money gain) per game, mirroring engagement_metrics' boundary tolerance.
  p_money <- tapply(seq_len(nrow(iw)), iw$game_id, function(ix) {
    r <- iw[ix, ]
    tol <- 1e-9 * max(1, price[[r$game_id[1]]])
    sum(r$probability[r$net_value > -price[[r$game_id[1]]] + tol])
  })

  # "Phantom-zero" games: those with any instant-win tier at gross_prize == 0.
  iw_tiers <- tiers[tiers$source == "instant_win", ]
  gid_zero_mass <- tapply(seq_len(nrow(iw_tiers)), iw_tiers$game_id, function(ix) {
    sum(iw_tiers$probability[ix][iw_tiers$gross_prize[ix] == 0])
  })
  phantom_gids  <- names(gid_zero_mass)[gid_zero_mass > 0]
  phantom_slugs <- .iw_slug(phantom_gids)

  m <- recon
  m$p_money <- p_money[paste0("instant_win:", m$slug)]
  m$impl_money <- 1 / m$p_money
  m$is_phantom <- m$slug %in% phantom_slugs
  rel <- abs(m$impl_money - m$implied_overall_1_in) / m$implied_overall_1_in

  # (a) Clean games: net-money-gain implied odds equal the CSV to fp precision.
  expect_lt(max(rel[!m$is_phantom]), 1e-6)

  # (b) The set that diverges is EXACTLY the data-derived phantom-zero set --
  #     nothing else drifts. (A regression on a normal game would break this.)
  diverging <- m$slug[rel > 1e-3]
  expect_setequal(diverging, phantom_slugs)

  # (c) The artifact set is small and documented (6 in this snapshot).
  expect_lte(length(phantom_slugs), 8L)

  message(sprintf(
    "[reconciliation] net-money-gain: %d clean games match to <1e-6; %d documented phantom-zero outliers: %s",
    sum(!m$is_phantom), length(phantom_slugs),
    paste(sort(phantom_slugs), collapse = ", ")))
})


# ------------------------------------------------------------------------------
test_that("the phantom-zero offset is fully explained by gross_prize==0 tier mass", {
  # Pin the SYSTEMATIC OFFSET precisely: for the artifact games,
  #   P(any tier)  ==  P(net money gain)  +  P(gross_prize == 0 tiers).
  # If this identity holds, the divergence is entirely the £0-labelled tiers and
  # not lost/duplicated odds -- the pipeline is sound.
  outcomes <- .recon_data$outcomes
  gs       <- .recon_data$gs
  tiers    <- .recon_data$tiers

  price <- setNames(gs$price, gs$game_id)
  iw    <- outcomes[outcomes$source == "instant_win", ]
  iwt   <- tiers[tiers$source == "instant_win", ]

  gids <- unique(iwt$game_id)
  for (gid in gids) {
    p_all  <- sum(iwt$probability[iwt$game_id == gid])
    p_zero <- sum(iwt$probability[iwt$game_id == gid & iwt$gross_prize == 0])
    r <- iw[iw$game_id == gid, ]
    tol <- 1e-9 * max(1, price[[gid]])
    p_money <- sum(r$probability[r$net_value > -price[[gid]] + tol])
    expect_equal(p_money + p_zero, p_all, tolerance = 1e-9, info = gid)
  }
})


# ------------------------------------------------------------------------------
test_that("implied-vs-published agreement (diff_pct) sits in a documented band", {
  # The implied odds (from scraped tiers) vs the page-printed odds will differ:
  # the scraped per-tier odds are rounded/approximate and a few games carry a
  # wide tolerance. We (1) re-derive diff_pct ourselves and confirm the CSV's
  # column, then (2) require the BULK inside a tight band while ALLOWING a small,
  # counted set of documented high-tolerance outliers -- rather than a blanket
  # loose tolerance that would hide a real regression.
  tiers <- .recon_data$tiers
  recon <- .recon_data$recon

  iw_tiers  <- tiers[tiers$source == "instant_win", ]
  p_anytier <- tapply(iw_tiers$probability, .iw_slug(iw_tiers$game_id), sum)

  m <- recon
  m$impl_recompute <- 1 / p_anytier[m$slug]
  # The CSV diff_pct is SIGNED (implied - page)/page*100 -- recompute it the same
  # way to confirm the column; use the magnitude for the band checks.
  m$diff_pct_recompute <- (as.numeric(m$impl_recompute) - m$page_overall_1_in) /
    m$page_overall_1_in * 100
  m$abs_diff <- abs(m$diff_pct_recompute)

  # (1) Our recomputed diff_pct reproduces the CSV's diff_pct column (rounded to
  #     0.1 in the CSV), confirming both derive from the same numbers.
  expect_lt(max(abs(m$diff_pct_recompute - m$diff_pct)), 0.06)

  # (2) Bulk band + counted outliers (this snapshot: median ~1.2%, max 12.2%).
  expect_lt(stats::median(m$abs_diff), 2)          # typical game agrees closely
  expect_gte(mean(m$abs_diff <= 5), 0.80)          # >= 80% within 5%
  expect_lte(sum(m$abs_diff > 8), 6L)              # only a few beyond 8%
  expect_lt(max(m$abs_diff), 13)                   # worst (word-spin) is 12.2%

  message(sprintf(
    "[reconciliation] diff_pct band: median=%.1f%%, within-5%%=%.0f%%, >8%%=%d game(s), max=%.1f%%",
    stats::median(m$abs_diff), 100 * mean(m$abs_diff <= 5),
    sum(m$abs_diff > 8), max(m$abs_diff)))
})
