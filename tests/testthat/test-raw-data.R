# ==============================================================================
# test-raw-data.R
# ------------------------------------------------------------------------------
# Schema checks on the five scraped CSVs, run BEFORE prepare_data() consumes
# them. The CSVs come from scraping the National Lottery site, so the realistic
# failure is a site or scraper change: a renamed column, prices captured in
# pence instead of pounds, odds parsed as text. Without these checks that shows
# up as an obscure error deep inside a join, or worse, as tiers silently
# dropped. Each check names the file and column so a refresh can be fixed fast.
# ==============================================================================

.raw <- local({
  root <- test_repo_root()
  read <- function(f) readr::read_csv(file.path(root, f), show_col_types = FALSE)
  list(
    sc_odds  = read("national_lottery_scratchcard_odds.csv"),
    sc_games = read("national_lottery_scratchcard_games.csv"),
    iw_tiers = read("national_lottery_instant_win_prize_tiers.csv"),
    iw_cat   = read("national_lottery_instant_win_catalogue.csv"),
    iw_recon = read("national_lottery_instant_win_reconciliation.csv")
  )
})

# The columns R/data_prep.R and the reconciliation tests actually read.
.required_columns <- list(
  sc_odds  = c("game_name", "game_number", "ticket_price", "overall_odds_1_in",
               "prize_amount", "prize_breakdown", "approx_odds_1_in",
               "implied_probability", "tolerance_pct"),
  sc_games = c("name", "title", "play_price_p", "prize_pool", "closure_date"),
  iw_tiers = c("slug", "prize_label", "prize_value_gbp", "approx_odds_1_in",
               "implied_probability"),
  iw_cat   = c("slug", "name", "game_id", "price_gbp", "overall_odds_1_in",
               "category", "end_date", "is_hidden", "prohibited"),
  iw_recon = c("slug", "page_overall_1_in", "implied_overall_1_in",
               "catalogue_overall_1_in", "diff_pct")
)

# ------------------------------------------------------------------------------
test_that("every raw CSV has the columns the pipeline reads", {
  for (f in names(.required_columns)) {
    missing <- setdiff(.required_columns[[f]], names(.raw[[f]]))
    expect_equal(missing, character(0), info = f)
  }
})

# ------------------------------------------------------------------------------
test_that("prices are positive pounds, and the scratchcard price cross-check columns agree in units", {
  expect_true(is.numeric(.raw$sc_odds$ticket_price))
  expect_true(all(.raw$sc_odds$ticket_price > 0))
  expect_true(is.numeric(.raw$iw_cat$price_gbp))
  expect_true(all(.raw$iw_cat$price_gbp > 0))
  # Pounds, not pence: no National Lottery ticket costs more than £30.
  expect_lte(max(.raw$sc_odds$ticket_price), 30, label = "sc_odds ticket_price (pounds)")
  expect_lte(max(.raw$iw_cat$price_gbp), 30, label = "iw_cat price_gbp (pounds)")
  # The games file is in pence, as data_prep.R's /100 conversion assumes.
  expect_true(is.numeric(.raw$sc_games$play_price_p))
  expect_gte(min(.raw$sc_games$play_price_p), 50, label = "sc_games play_price_p (pence)")
})

# ------------------------------------------------------------------------------
test_that("prize tiers carry numeric, non-negative prizes and probabilities in (0, 1]", {
  tiers <- list(sc_odds  = list(prize = "prize_amount"),
                iw_tiers = list(prize = "prize_value_gbp"))
  for (f in names(tiers)) {
    d <- .raw[[f]]
    prize <- d[[tiers[[f]]$prize]]
    expect_true(is.numeric(prize), info = f)
    expect_false(anyNA(prize), info = f)
    expect_true(all(prize >= 0), info = f)
    p <- d$implied_probability
    expect_true(is.numeric(p), info = f)
    expect_false(anyNA(p), info = f)
    expect_true(all(p > 0 & p <= 1), info = f)
  }
})

# ------------------------------------------------------------------------------
test_that("each tier's implied probability is the reciprocal of its quoted odds", {
  # implied_probability is derived from "1 in X" at scrape time; if the two
  # drift apart, one of them was mis-parsed.
  for (f in c("sc_odds", "iw_tiers")) {
    d <- .raw[[f]]
    expect_equal(d$implied_probability, 1 / d$approx_odds_1_in, tolerance = 1e-6,
                 info = f)
  }
})

# ------------------------------------------------------------------------------
test_that("join keys are present and parseable", {
  # Scratchcards join on game_number, which the games file embeds in its title.
  expect_false(anyNA(.raw$sc_odds$game_number))
  expect_true(all(grepl("\\[\\d+\\]", .raw$sc_games$title)))
  # Instant-win games join on slug, which must be unique in the catalogue and
  # present for every tier.
  expect_false(anyNA(.raw$iw_tiers$slug))
  expect_false(any(duplicated(.raw$iw_cat$slug)))
  # Dates that decide on_sale parse as dates where present.
  for (col in list(.raw$sc_games$closure_date, .raw$iw_cat$end_date)) {
    d <- as.Date(substr(as.character(col[!is.na(col)]), 1, 10))
    expect_false(anyNA(d))
  }
})
