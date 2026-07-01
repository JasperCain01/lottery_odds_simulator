# ==============================================================================
# test-data-prep.R
# ------------------------------------------------------------------------------
# Correctness tests for Layer 1 (R/data_prep.R). Covers the invariants the whole
# app trusts: per-game probabilities sum to 1, net_value = P - C, no NA prices
# after joins, a hand-checked RTP, rtp_flag thresholds, and losing-row
# probability correctness.
#
# The suite runs the real pipeline once against the repo's raw CSVs (pinned to a
# fixed `today` so on_sale is deterministic), then asserts against the result.
# WHY pin today: on_sale derivation compares dates to "now"; pinning keeps the
# tests reproducible regardless of when they run.
# ==============================================================================

# Locate repo root from the test working dir. testthat runs with wd at the
# tests/testthat directory (via test_dir) or at repo root (via test_check-style
# sourcing); handle both by walking up until we find the raw CSV.
.find_root <- function() {
  candidates <- c(".", "..", "../..", "../../..")
  for (p in candidates) {
    if (file.exists(file.path(p, "national_lottery_scratchcard_odds.csv"))) {
      return(normalizePath(p))
    }
  }
  stop("Could not locate repo root (scratchcard_odds.csv not found).")
}

ROOT   <- .find_root()
PINNED <- as.Date("2026-07-01")

# Ensure the layer under test is loaded. The project ships a testthat.R harness
# that sources R/*.R, but running via test_dir() alone does not, so source it
# here when the entry point is absent. WHY guard: avoid double-sourcing side
# effects when the harness already loaded it.
if (!exists("prepare_data", mode = "function")) {
  suppressWarnings(source(file.path(ROOT, "R", "data_prep.R")))
}

# Run the pipeline once for the whole file. suppressWarnings because a clean
# snapshot may still warn on unmatched IW slugs; those are asserted separately.
prep <- suppressWarnings(prepare_data(root = ROOT, today = PINNED))
outcomes <- prep$outcomes
gs       <- prep$game_summary
tiers    <- prep$tiers_raw

# ------------------------------------------------------------------------------
test_that("per-game outcome probabilities sum to exactly 1 (within fp tol)", {
  sums <- outcomes |>
    dplyr::group_by(game_id) |>
    dplyr::summarise(total = sum(probability), .groups = "drop")
  # Every game must be within floating-point tolerance of 1.
  expect_true(all(abs(sums$total - 1) < 1e-9))
  # And there must be no missing probabilities anywhere.
  expect_false(any(is.na(outcomes$probability)))
})

# ------------------------------------------------------------------------------
test_that("net_value = gross_prize - price for every raw tier", {
  # tiers_raw carries gross_prize, price and net_value directly.
  expect_equal(tiers$net_value, tiers$gross_prize - tiers$price)
})

# ------------------------------------------------------------------------------
test_that("no NA prices, net_values, or probabilities after joins", {
  expect_false(any(is.na(gs$price)))
  expect_false(any(is.na(tiers$price)))
  expect_false(any(is.na(tiers$net_value)))
  expect_false(any(is.na(outcomes$net_value)))
})

# ------------------------------------------------------------------------------
test_that("hand-checked RTP for prize-ball (instant win, £0.25) matches", {
  # Independently recompute E[gross] for prize-ball from the raw tier table and
  # divide by its catalogue price, then compare to the summary's rtp.
  pt   <- readr::read_csv(file.path(ROOT, "national_lottery_instant_win_prize_tiers.csv"),
                          show_col_types = FALSE)
  cat_ <- readr::read_csv(file.path(ROOT, "national_lottery_instant_win_catalogue.csv"),
                          show_col_types = FALSE)
  pb    <- pt[pt$slug == "prize-ball", ]
  price <- cat_$price_gbp[cat_$slug == "prize-ball"]
  rtp_hand <- sum(pb$prize_value_gbp * pb$implied_probability) / price

  rtp_out <- gs$rtp[gs$game_id == "instant_win:prize-ball"]
  expect_equal(rtp_out, rtp_hand, tolerance = 1e-12)
  # Sanity: this is the known worst-RTP game (~11.6%).
  expect_lt(rtp_out, 0.12)
})

# ------------------------------------------------------------------------------
test_that("hand-checked net_ev, sd_play, and moments for scratchcard 1512", {
  sc <- readr::read_csv(file.path(ROOT, "national_lottery_scratchcard_odds.csv"),
                        show_col_types = FALSE)
  sc <- sc[sc$game_number == 1512, ]
  C   <- sc$ticket_price[1]
  eg  <- sum(sc$prize_amount * sc$implied_probability)
  eg2 <- sum(sc$prize_amount^2 * sc$implied_probability)

  row <- gs[gs$game_id == "scratchcard:1512", ]
  expect_equal(row$rtp,    eg / C,          tolerance = 1e-12)
  expect_equal(row$net_ev, eg - C,          tolerance = 1e-12)
  expect_equal(row$sd_play, sqrt(eg2 - eg^2), tolerance = 1e-9)
})

# ------------------------------------------------------------------------------
test_that("rtp_flag thresholds map correctly (none / soft / hard)", {
  # Drive the flag logic with a tiny synthetic game so the thresholds are tested
  # independently of whatever the live data happens to contain.
  mk <- function(rtp_target, price = 1) {
    # single winning tier: prob p, gross g s.t. g*p/price = rtp_target, p small
    p <- 0.01
    g <- rtp_target * price / p
    tibble::tibble(
      game_id = "t:1", source = "scratchcard", price = price,
      gross_prize = g, net_value = g - price, probability = p,
      tolerance_pct = NA_real_, name = "t", category = "x",
      is_hidden = FALSE, prohibited = FALSE, headline_odds_1_in = 1,
      end_or_closure_date = as.Date("2030-01-01"), on_sale = TRUE
    )
  }
  expect_equal(build_game_summary(mk(0.50))$rtp_flag, "none")   # below soft
  expect_equal(build_game_summary(mk(0.90))$rtp_flag, "soft")   # >0.85, <=1.0
  expect_equal(build_game_summary(mk(1.20))$rtp_flag, "hard")   # >1.0
  # Boundary: exactly at threshold is NOT flagged (strict >).
  expect_equal(build_game_summary(mk(0.85))$rtp_flag, "none")
  expect_equal(build_game_summary(mk(1.00))$rtp_flag, "soft")
})

# ------------------------------------------------------------------------------
test_that("losing-row probability equals 1 - sum(winning tier probabilities)", {
  # For each game, the outcomes row at net_value == -price must carry the
  # complement of the summed winning probabilities. We reconstruct the expected
  # value from the raw tiers (which exclude the losing row).
  win_sum <- tiers |>
    dplyr::group_by(game_id, price) |>
    dplyr::summarise(p_win = sum(probability), .groups = "drop") |>
    dplyr::mutate(lose_net = -price, expected_lose_p = 1 - p_win)

  lose_rows <- outcomes |>
    dplyr::inner_join(win_sum, by = "game_id") |>
    dplyr::filter(abs(net_value - lose_net) < 1e-12)

  # Every game must have a losing row, and its probability must match. NOTE: for
  # the handful of games whose raw tiers include a £0 gross prize, that tier's
  # net_value coincides with -price and its mass is (correctly) folded into the
  # losing row, so the outcomes losing-row probability can EXCEED 1 - p_win by
  # exactly that £0-tier mass. We therefore assert >= and re-derive per game.
  expect_true(nrow(lose_rows) == dplyr::n_distinct(outcomes$game_id))

  # Precise per-game check that accounts for £0-gross collisions:
  zero_mass <- tiers |>
    dplyr::filter(gross_prize == 0) |>
    dplyr::group_by(game_id) |>
    dplyr::summarise(z = sum(probability), .groups = "drop")
  chk <- lose_rows |>
    dplyr::left_join(zero_mass, by = "game_id") |>
    dplyr::mutate(z = ifelse(is.na(z), 0, z),
                  expected = expected_lose_p + z)
  expect_equal(chk$probability, chk$expected, tolerance = 1e-12)
})

# ------------------------------------------------------------------------------
test_that("£0-gross winning tiers fold into the losing row (mass preserved)", {
  # 3-in-a-row is known to carry £0 'prize' tiers whose net == -price. The
  # outcomes table must still sum to 1 and must NOT contain a duplicate -price
  # row (aggregation collapsed it).
  g <- "instant_win:3-in-a-row"
  o <- outcomes[outcomes$game_id == g, ]
  expect_equal(sum(o$probability), 1, tolerance = 1e-9)
  price <- gs$price[gs$game_id == g]
  # Exactly one row at net == -price.
  expect_equal(sum(abs(o$net_value + price) < 1e-12), 1L)
})

# ------------------------------------------------------------------------------
test_that("cross-source game_id is unique and prefixed by source", {
  expect_equal(dplyr::n_distinct(gs$game_id), nrow(gs))
  expect_true(all(grepl("^(scratchcard|instant_win):", gs$game_id)))
})

# ------------------------------------------------------------------------------
test_that("rtp_capped never exceeds raw rtp and equals rtp for instant win", {
  # Capping only ever REMOVES tier mass, so capped RTP <= raw RTP always.
  expect_true(all(gs$rtp_capped <= gs$rtp + 1e-12))
  # Instant-win tiers carry no tolerance band, so no capping applies there.
  iw <- gs[gs$source == "instant_win", ]
  expect_equal(iw$rtp_capped, iw$rtp, tolerance = 1e-12)
})

# ------------------------------------------------------------------------------
test_that("expected game & row counts reconcile with the raw data snapshot", {
  # These are the counts for the CURRENT data snapshot (see the data-prep
  # report). They guard against a silent regression in the join / filter logic.
  expect_equal(sum(gs$source == "scratchcard"), 44L)
  expect_equal(sum(gs$source == "instant_win"), 79L)
  expect_equal(length(prep$diag$iw_no_tiers), 3L)      # the Monopoly trio
  expect_equal(nrow(prep$diag$sc_mismatches), 0L)      # prices all reconcile
  expect_equal(length(prep$diag$iw_unmatched), 0L)     # every tier slug priced
  expect_equal(length(prep$diag$quarantined), 0L)      # no impossible games
})
