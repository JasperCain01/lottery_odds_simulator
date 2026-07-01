# scrape_lottery_odds.R  (v2 - using the real JSON API, not HTML scraping)
#
# Pulls the live scratchcard list from National Lottery's Magnolia CMS API,
# downloads each game's "Game Procedures" PDF via its DAM UUID, and parses
# the prize-tier odds table out of each one.
#
# HOW THIS WAS DERIVED (so future-you can re-derive it if it breaks):
# - The scratchcards page (https://www.national-lottery.co.uk/games/gamestore/scratchcards)
#   is a JS-rendered SPA - raw HTML has no game data, so httr2/rvest alone
#   can't see anything.
# - DevTools Network tab showed it calls:
#     https://api-dfe.national-lottery.co.uk/cms-proxy/scratchcardList?@jcr:uuid=<node id>
#   which returns the full live game list as JSON, including a
#   `gameProcedures` field per game (a bare Magnolia content UUID, no link).
# - Magnolia resolves DAM assets by UUID with a loose filename, so:
#     https://www.national-lottery.co.uk/dam/jcr:<uuid>/anything.pdf
#   returns the actual PDF regardless of what filename you put on the end.
#
# RISKS THAT STILL APPLY:
# - The `@jcr:uuid` value identifying the "All Scratchcards" listing node is
#   hardcoded below. If Allwyn restructures their CMS this will need
#   re-extracting from DevTools (same process as before).
# - PDF table parsing via regex on pdftools::pdf_text() output is still
#   fragile for games with unusual bonus mechanics - check the parse
#   failure warnings.
# - No network access was available to actually run this end-to-end while
#   writing it - the JSON shape and DAM URL pattern are confirmed against
#   real responses you pasted back, but the full loop over ~46 games is
#   untested in one go. Watch for rate limiting if it fails partway through.

library(httr2)
library(pdftools)
library(stringr)
library(dplyr)
library(purrr)
library(tibble)

# ---- Config -----------------------------------------------------------

LISTING_API <- "https://api-dfe.national-lottery.co.uk/cms-proxy/scratchcardList"
LISTING_NODE_UUID <- "1cbcc086-3710-47dd-ba65-7e58365efbfa"
OUTPUT_DIR  <- "lottery_pdfs"
DELAY_SECS  <- 1.5   # be polite - don't hammer the site

dir.create(OUTPUT_DIR, showWarnings = FALSE)

UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"

# ---- Step 1: get the live game list as a tidy tibble ---------------------

get_game_list <- function() {
  resp <- request(LISTING_API) |>
    req_url_query(`@jcr:uuid` = LISTING_NODE_UUID) |>
    req_user_agent(UA) |>
    req_headers(Accept = "application/json") |>
    req_perform()
  
  body <- resp_body_json(resp)
  cards <- body$results[[1]]$scratchcards
  
  map_dfr(cards, function(g) {
    tibble(
      name           = g$name,
      title          = g$title,
      play_price_p   = g$playPrice,         # pence
      prize_pool     = g$prizePool,
      all_top_prizes = g$allTopPrizes,
      left_top_prizes = g$leftTopPrizes,
      game_procedures_uuid = str_remove(g$gameProcedures, "^jcr:"),
      closure_date   = g$closureDate
    )
  })
}

# ---- Step 2: download a single procedures PDF by UUID --------------------

download_procedures_pdf <- function(uuid, game_name, dest_dir = OUTPUT_DIR) {
  url <- paste0("https://www.national-lottery.co.uk/dam/jcr:", uuid, "/procedures.pdf")
  safe_name <- str_replace_all(game_name, "[^A-Za-z0-9]+", "_")
  fname <- file.path(dest_dir, paste0(safe_name, ".pdf"))
  
  if (file.exists(fname)) return(fname)
  
  tryCatch({
    request(url) |>
      req_user_agent(UA) |>
      req_perform(path = fname)
    Sys.sleep(DELAY_SECS)
    fname
  }, error = function(e) {
    message("FAILED to download ", game_name, ": ", conditionMessage(e))
    NA_character_
  })
}

# ---- Step 3: parse the odds table out of one PDF --------------------------

parse_game_pdf <- function(pdf_path) {
  txt <- pdf_text(pdf_path) |> paste(collapse = "\n")
  lines <- str_split(txt, "\n")[[1]] |> str_trim()
  
  game_name    <- str_match(txt, 'Game Name:\\s*[“"]?([^”"\\n]+)')[,2]
  game_number  <- str_match(txt, "Game Number:\\s*[“\"]?Game (\\d+)")[,2]
  price        <- str_match(txt, "Retail Sales Price:\\s*£([0-9.]+)")[,2]
  overall_odds <- str_match(txt, "1 in ([0-9.]+) overall chance")[,2]
  
  start_idx <- str_which(lines, "Prize Amounts, Number of Prizes and Odds")
  if (length(start_idx) == 0) {
    warning("No prize table heading found in ", pdf_path)
    return(NULL)
  }
  end_idx <- str_which(lines, "^As Prizes are won")
  end_idx <- end_idx[end_idx > start_idx[1]][1]
  if (is.na(end_idx)) end_idx <- length(lines)
  
  table_lines <- lines[(start_idx[1] + 1):(end_idx - 1)]
  table_lines <- table_lines[nchar(table_lines) > 0]
  
  collapsed <- character(0)
  for (ln in table_lines) {
    is_new_row <- str_detect(ln, "^£[0-9]")
    is_boilerplate <- str_detect(ln, "^(GP GM|Allwyn Entertainment|The National Lottery Line)")
    if (is_boilerplate) next
    if (is_new_row) {
      collapsed <- c(collapsed, ln)
    } else if (length(collapsed) > 0) {
      collapsed[length(collapsed)] <- paste(collapsed[length(collapsed)], ln)
    }
  }
  
  row_pattern <- "^£([0-9,]+)\\s+(.+?)\\s+([0-9,]+)\\s+([0-9,]+)\\s+\\+/-\\s*([0-9.]+)%$"
  
  parsed <- collapsed |>
    str_match(row_pattern) |>
    as_tibble(.name_repair = "minimal")
  
  if (ncol(parsed) < 6) {
    warning("Table regex matched 0 columns for ", pdf_path, " - layout may differ")
    return(NULL)
  }
  
  names(parsed) <- c("raw", "prize_amount", "prize_breakdown",
                     "n_prizes", "approx_odds_1_in", "tolerance_pct")
  
  n_unmatched <- sum(is.na(parsed$prize_amount))
  if (n_unmatched > 0) {
    message(basename(pdf_path), ": ", n_unmatched, " table row(s) failed to parse - check manually")
  }
  
  parsed |>
    filter(!is.na(prize_amount)) |>
    mutate(
      game_name            = game_name,
      game_number          = game_number,
      ticket_price         = as.numeric(price),
      overall_odds_1_in    = as.numeric(overall_odds),
      prize_amount         = as.numeric(str_remove_all(prize_amount, ",")),
      n_prizes             = as.numeric(str_remove_all(n_prizes, ",")),
      approx_odds_1_in     = as.numeric(str_remove_all(approx_odds_1_in, ",")),
      tolerance_pct        = as.numeric(tolerance_pct),
      implied_probability  = 1 / approx_odds_1_in
    ) |>
    select(game_name, game_number, ticket_price, overall_odds_1_in,
           prize_amount, prize_breakdown, n_prizes,
           approx_odds_1_in, implied_probability, tolerance_pct, raw)
}

# ---- Step 4: orchestrate ---------------------------------------------------

games <- get_game_list()
message("Found ", nrow(games), " live scratchcard games")

games$pdf_path <- map2_chr(games$game_procedures_uuid, games$name, download_procedures_pdf)

n_failed_dl <- sum(is.na(games$pdf_path))
if (n_failed_dl > 0) message(n_failed_dl, " PDF(s) failed to download")

all_odds <- games |>
  filter(!is.na(pdf_path)) |>
  pull(pdf_path) |>
  map(safely(parse_game_pdf)) |>
  map("result") |>
  compact() |>
  bind_rows()

message("Parsed ", n_distinct(all_odds$game_number), " games, ",
        nrow(all_odds), " prize-tier rows total")

# Sanity check: sum of implied probabilities per game should be close to
# 1/overall_odds_1_in (the advertised aggregate odds) - worth eyeballing
# this before trusting the dataset.
check <- all_odds |>
  group_by(game_name, game_number, overall_odds_1_in) |>
  summarise(implied_overall_odds = 1 / sum(implied_probability), .groups = "drop") |>
  mutate(diff_pct = round(100 * (implied_overall_odds - overall_odds_1_in) / overall_odds_1_in, 1))

print(check)

write.csv(all_odds, "national_lottery_scratchcard_odds.csv", row.names = FALSE)
write.csv(games, "national_lottery_scratchcard_games.csv", row.names = FALSE)