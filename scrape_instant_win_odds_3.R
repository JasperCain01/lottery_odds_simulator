# scrape_instant_win_odds.R  (v3 - full catalogue via the iwgList-v2 endpoint)
#
# Scrapes ALL National Lottery online instant-win games: headline odds (from the
# catalogue JSON) + the full per-prize-tier "Approx odds (1 in x)" table (from
# each game page's Next.js RSC payload).
#
# ENDPOINTS (both confirmed live):
#  1. Catalogue + headline odds - the sturdy JSON list, same @jcr:uuid pattern as
#     your scratchcardList scraper:
#       GET {CATALOGUE_URL}
#     Returns ~82 game nodes, each with slug, oddsNumerator/oddsDenominator,
#     playPrice (pence), prizePool, gameId, category, launchDate, endDate,
#     isHidden, prohibited. The source UUID (cb64bf0e-...) is the sourceItems-v2
#     of the all-games grid; it was found by reading the grid's own list XHR off
#     the live page (DevTools Network / browser network read), not guessable -
#     the path is "iwgList-v2".
#  2. Per-game prize-tier table - escaped HTML inside the RSC payload on:
#       GET {WWW_BASE}/<slug>?noLayout=true
#     Pull the escaped \u003ctable\u003e fragment, JSON-unescape, parse with rvest.
#
# COMPLETENESS: the recursive extractor's game count is asserted (stopifnot)
# against an independent count of "oddsDenominator" keys in the raw JSON, so a
# silently partial parse fails loudly. Cross-check the total against the figure
# the all-games page states on screen.
#
# ACCESS: httr2 + a browser UA reaches both hosts; the AWS WAF doesn't block
# these GETs. No chromote needed.

library(httr2); library(rvest); library(jsonlite)
library(stringr); library(dplyr); library(purrr); library(tibble)

`%||%` <- function(x, y) if (is.null(x)) y else x   # base R >= 4.4 has this

UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"
WWW_BASE        <- "https://www.national-lottery.co.uk/games/instant-win-games"
IWG_SOURCE_UUID <- "cb64bf0e-89ca-4f8d-9f29-ec790c2e7c64"   # all-games grid sourceItems-v2
CATALOGUE_URL   <- sprintf(
  "https://api-dfe.national-lottery.co.uk/cms-proxy/iwgList-v2?@jcr:uuid=%s", IWG_SOURCE_UUID)
DELAY <- 1.5

# ============================================================================
# Part A: full catalogue + headline odds (one JSON call)
# ============================================================================

# Any nested node carrying slug + oddsDenominator is a game (recurse - resilient
# to the exact nesting).
extract_games <- function(node, acc = new.env(parent = emptyenv())) {
  if (is.list(node)) {
    if (!is.null(node$slug) && !is.null(node$oddsDenominator)) acc[[node$slug]] <- node
    for (child in node) extract_games(child, acc)
  }
  acc
}

flatten_category <- function(x) {
  if (is.list(x)) as.character(x$label %||% x$name %||% NA_character_)
  else            as.character(x %||% NA_character_)
}

get_catalogue <- function() {
  raw <- request(CATALOGUE_URL) |>
    req_user_agent(UA) |> req_headers(Accept = "application/json") |>
    req_retry(max_tries = 3) |> req_perform() |> resp_body_string()

  env   <- extract_games(fromJSON(raw, simplifyVector = FALSE))
  slugs <- ls(env)

  # completeness guard: walker count must equal an independent count of game nodes
  n_expected <- str_count(raw, '"oddsDenominator"')
  stopifnot("iwgList-v2 parse is incomplete - games were silently dropped" =
              length(slugs) == n_expected)

  map_dfr(slugs, function(s) {
    g <- env[[s]]
    tibble(
      slug              = s,
      name              = g$name %||% NA_character_,
      game_id           = as.character(g$gameId %||% NA_character_),
      price_gbp         = as.numeric(g$playPrice %||% NA) / 100,   # playPrice is pence
      prize_pool        = as.numeric(g$prizePool %||% NA),
      overall_odds_1_in = as.numeric(g$oddsDenominator %||% NA) /
                          as.numeric(g$oddsNumerator   %||% 1),
      category          = flatten_category(g$category),
      launch_date       = as.character(g$launchDate %||% NA_character_),
      end_date          = as.character(g$endDate %||% NA_character_),
      is_hidden         = isTRUE(g$isHidden),
      prohibited        = isTRUE(g$prohibited)
    )
  }) |> arrange(overall_odds_1_in)
}

# ============================================================================
# Part B: per-game prize-tier table (escaped HTML in the RSC payload)
# ============================================================================

fetch_game_page <- function(slug) {
  request(sprintf("%s/%s?noLayout=true", WWW_BASE, slug)) |>
    req_user_agent(UA) |> req_retry(max_tries = 3) |>
    req_perform() |> resp_body_string()
}

parse_tiers <- function(page, slug = NA_character_) {
  frags <- str_extract_all(page, "(?s)\\\\u003ctable\\\\u003e.*?\\\\u003c/table\\\\u003e")[[1]]
  frags <- frags[str_detect(frags, "Approx odds")]           # keep only odds tables
  if (length(frags) == 0) { warning("No prize/odds table for ", slug); return(NULL) }

  map_dfr(frags, function(frag) {
    clean <- fromJSON(paste0('"', frag, '"'))                # unescape \u003c, \n, \"
    tb <- read_html(clean) |> html_element("table") |> html_table()
    tibble(
      prize_label      = as.character(tb[[1]]),
      approx_odds_1_in = as.numeric(str_remove_all(as.character(tb[[2]]), "[^0-9.]"))
    )
  }) |>
    filter(!is.na(approx_odds_1_in)) |>
    mutate(
      slug            = slug,
      prize_value_gbp = as.numeric(str_remove_all(str_match(prize_label, "^\u00A3([0-9,]+)")[, 2], ",")),
      implied_probability = 1 / approx_odds_1_in
    ) |>
    select(slug, prize_label, prize_value_gbp, approx_odds_1_in, implied_probability)
}

overall_odds_from_page <- function(page) {
  as.numeric(str_match(page, "oddsDenominator[^0-9]+([0-9.]+)")[, 2])
}

scrape_tiers <- function(slug, delay = DELAY) {
  Sys.sleep(delay)
  page <- fetch_game_page(slug)
  list(slug = slug,
       stated_overall_1_in = overall_odds_from_page(page),
       tiers = parse_tiers(page, slug))
}

# ============================================================================
# Orchestrate
# ============================================================================

catalogue <- get_catalogue()
message("Catalogue: ", nrow(catalogue), " games (",
        sum(!catalogue$is_hidden & !catalogue$prohibited), " visible & permitted)")

# Scrape tier tables for the playable games (drop hidden/prohibited).
target  <- catalogue |> filter(!is_hidden, !prohibited)
results <- map(target$slug, safely(scrape_tiers)) |> map("result") |> compact()

tiers_all <- map(results, "tiers") |> compact() |> bind_rows()
message("Prize tiers scraped for ", n_distinct(tiers_all$slug), " of ",
        nrow(target), " games; ", nrow(tiers_all), " tier rows total")

# Reconcile: headline odds (catalogue) vs implied (1 / sum of tier probs).
# For a well-formed game these should agree within rounding.
recon <- map_dfr(results, function(r) {
  tibble(slug = r$slug,
         page_overall_1_in    = r$stated_overall_1_in,
         implied_overall_1_in = if (!is.null(r$tiers)) 1 / sum(r$tiers$implied_probability) else NA_real_)
}) |>
  left_join(catalogue |> select(slug, catalogue_overall_1_in = overall_odds_1_in), by = "slug") |>
  mutate(diff_pct = round(100 * (implied_overall_1_in - catalogue_overall_1_in) / catalogue_overall_1_in, 1))

print(recon, n = Inf)

write.csv(catalogue, "national_lottery_instant_win_catalogue.csv",   row.names = FALSE)
write.csv(tiers_all, "national_lottery_instant_win_prize_tiers.csv", row.names = FALSE)
write.csv(recon,     "national_lottery_instant_win_reconciliation.csv", row.names = FALSE)
