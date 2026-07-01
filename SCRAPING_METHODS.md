# National Lottery Odds Scrapers — Technical Reference

Reference notes for the two scrapers in this project: one for physical
**scratchcards**, one for online **instant-win games**. They target the same
site but sit on completely different architectures, so they work in different
ways and fail in different ways. This document records *how* each works, *why*
it's built that way, and the maintenance steps to re-derive the moving parts if
the site changes.

> **Purpose of the project.** The advertised "overall odds" (e.g. *1 in 2.45*)
> for a game collapses a heavily skewed prize distribution into a single
> figure. These scrapers pull the full per-prize-tier odds so that skew can be
> analysed — most of the win-probability sits in low, near-stake-return tiers,
> while headline prizes are orders of magnitude rarer.

---

## 1. Shared context

| Thing | Detail |
|---|---|
| Operator | Allwyn (National Lottery licence holder) |
| CMS | Magnolia — content addressed by `jcr:uuid` node/asset identifiers |
| Front end | Next.js / React Server Components (RSC) on the consumer pages |
| API host | `api-dfe.national-lottery.co.uk` (the `cms-proxy` fronts Magnolia) |
| Consumer host | `www.national-lottery.co.uk` |
| Bot protection | AWS WAF (`*.edge.sdk.awswaf.com` challenge/telemetry) |
| Core tooling | R: `httr2`, `rvest`, `jsonlite`, `pdftools`, `stringr`, `dplyr`, `purrr` |

**Access behaviour (learned the hard way).** A plain `httr2` GET carrying a
browser `User-Agent` reaches the `cms-proxy` JSON endpoints and the DAM assets
fine. The WAF challenge fires on the interactive consumer pages / some fetchers
but does **not** block these GETs. A `404` from these hosts is a genuine
"not found", not a block — useful signal when probing for endpoints. Server-side
fetchers with a non-browser fingerprint *are* blocked, which is why endpoint
discovery has to happen in a real browser (DevTools / network read), not from a
headless service.

**Politeness.** Both scrapers use a ~1.5s delay between requests. This matters
more for the instant-win scraper, which loops ~80 game pages.

**Terms of use — read before scheduling.** The odds data is published for
regulatory transparency, so a one-off analytical pull is low-risk. A *scheduled*
full-catalogue sweep is exactly the kind of automated access a licensed
operator's terms tend to restrict. Check the site terms before putting either
scraper on a cron.

---

## 2. Scratchcard scraper

**Product nature.** Physical retail cards. Each game has a **regulatory "Game
Procedures" PDF** — a stable, structured document containing the full prize-tier
table. This makes scratchcards the *easier* of the two: the authoritative data
is a downloadable file.

### Pipeline

```
scratchcardList (JSON)  ──►  per-game gameProcedures UUID  ──►  DAM PDF  ──►  pdftools parse
```

1. **List the live games.**
   ```
   GET https://api-dfe.national-lottery.co.uk/cms-proxy/scratchcardList?@jcr:uuid=1cbcc086-3710-47dd-ba65-7e58365efbfa
   ```
   The `@jcr:uuid` is the "All Scratchcards" **listing node**. The response is
   JSON; the games sit at `body$results[[1]]$scratchcards`, each carrying
   `name`, `title`, `playPrice` (pence), `prizePool`, `allTopPrizes`,
   `leftTopPrizes`, `closureDate`, and — crucially — `gameProcedures`, a bare
   Magnolia content UUID (no link).

2. **Resolve the PDF from the UUID.** Magnolia serves DAM assets by UUID with a
   *loose* filename, so any filename on the end works:
   ```
   https://www.national-lottery.co.uk/dam/jcr:<uuid>/anything.pdf
   ```
   This is the key trick — you don't need a stored link, just the UUID from the
   list response.

3. **Parse each PDF** with `pdftools::pdf_text()`:
   - Regex out `Game Name`, `Game Number`, `Retail Sales Price`, and the
     `1 in X overall chance` figure.
   - Locate the prize table between the heading
     `"Prize Amounts, Number of Prizes and Odds"` and the line
     `"As Prizes are won"`.
   - Collapse multi-line rows (a row starts with `£`; continuation lines are
     appended), dropping boilerplate (`GP GM`, `Allwyn Entertainment`, …).
   - Match rows with a pattern capturing: prize amount, prize breakdown,
     number of prizes, approx odds (1 in x), tolerance (±%).

### Output shape
Full tier table per game: `prize_amount`, `prize_breakdown`, `n_prizes`,
`approx_odds_1_in`, `implied_probability`, `tolerance_pct`.

### Sanity check
Per game, `1 / sum(implied_probability)` across the tiers should land near the
advertised overall odds — a cheap integrity test before trusting the dataset.

### Fragility
- The **listing node UUID is hardcoded**. If Allwyn restructures the CMS it
  must be re-extracted (see §4).
- **Regex-on-PDF-text is brittle** for games with unusual bonus mechanics —
  watch the parse-failure warnings.

---

## 3. Instant-win game scraper

**Product nature.** Digital games, many from third-party suppliers (e.g. IWG).
There is **no regulatory PDF**; the "Game Procedures and How to Play" table is
shown in an in-page modal. The consumer section is a Next.js/RSC app, so the
useful data travels as **embedded JSON/HTML in the page payload**, not as clean
JSON API responses or DOM table markup. This is what made it hard, and the
false starts are worth recording so they aren't repeated.

### What *doesn't* work (and why)

| Attempt | Result |
|---|---|
| Fetch the game page / modal URL server-side | WAF-blocked |
| `.../instant-win-games/all-games` **JSON** endpoint | Returns only a grid **shell** — it references its game list by a source UUID (`sourceItems-v2 = cb64bf0e-…`) and embeds only promo tiles |
| `.../<slug>/game-procedures?noLayout=true` (the draw-game pattern) | `404` — instant-win games don't use that subpath |
| `read_html()` on the game page for a `<table>` | Finds none — the table is **escaped HTML inside the RSC string payload**, not DOM markup |
| Scraping slugs from the `all-games` page HTML | Empty — the grid fetches items **client-side after load**, so they aren't in the served payload |

### The architecture that *does* work

Two endpoints, both confirmed live, mirroring the scratchcard split of
"list" + "per-game detail":

**(a) Catalogue + headline odds — the sturdy JSON list.**
```
GET https://api-dfe.national-lottery.co.uk/cms-proxy/iwgList-v2?@jcr:uuid=cb64bf0e-89ca-4f8d-9f29-ec790c2e7c64
```
This is the direct analogue of `scratchcardList` — same `?@jcr:uuid=` pattern,
keyed on the `all-games` grid's `sourceItems-v2` UUID. Returns ~82 game nodes,
each with `slug`, `oddsNumerator`/`oddsDenominator` (headline odds), `playPrice`
(pence), `prizePool`, `gameId`, `category`, `launchDate`, `endDate`, and the
`isHidden` / `prohibited` flags. The path name `iwgList-v2` was **not guessable**
— it was found by reading the grid's own XHR off the live all-games page.

**(b) Per-game prize-tier table — escaped HTML in the RSC payload.**
```
GET https://www.national-lottery.co.uk/games/instant-win-games/<slug>?noLayout=true
```
`?noLayout=true` strips the layout chrome but the procedures table is
server-rendered into the HTML — as an **escaped** string inside the RSC
payload, e.g. `\u003ctable\u003e…\u003c/table\u003e`. So `rvest` sees no
`<table>` until you extract and unescape it.

### The critical idiom: unescaping the RSC table

Pull the escaped fragment, JSON-unescape it (wrapping in quotes and letting a
JSON parser handle `\u003c`, `\n`, `\"` is far more robust than hand-rolled
`gsub` chains), then parse the now-real HTML:

```r
frag  <- stringr::str_extract(page, "(?s)\\\\u003ctable\\\\u003e.*?\\\\u003c/table\\\\u003e")
clean <- jsonlite::fromJSON(paste0('"', frag, '"'))     # unescapes \u003c, \n, \"
tbl   <- rvest::read_html(clean) |> rvest::html_element("table") |> rvest::html_table()
```

The headline `oddsDenominator` also sits in the same payload and is recoverable
with a tolerant regex (`oddsDenominator[^0-9]+([0-9.]+)`), which reconciles with
the catalogue figure.

### Pipeline

1. `iwgList-v2` → catalogue of all games with headline odds (one call).
2. Recursive walker extracts every node carrying `slug` + `oddsDenominator`
   (resilient to exact nesting).
3. **Completeness guard** (see below).
4. For each playable game (`!isHidden`, `!prohibited`): fetch `<slug>?noLayout=true`,
   extract the escaped `…Approx odds…` table fragment(s), unescape, parse.
5. Reconcile `1 / sum(tier implied prob)` against the catalogue's overall odds.

### Output shape
- **Catalogue**: `slug`, `name`, `game_id`, `price_gbp`, `prize_pool`,
  `overall_odds_1_in`, `category`, `launch_date`, `end_date`, `is_hidden`,
  `prohibited`.
- **Tiers**: `slug`, `prize_label`, `prize_value_gbp`, `approx_odds_1_in`,
  `implied_probability`.

### Completeness guard (audit-style)
Completeness is the load-bearing risk here — a scraper that silently returns 40
of 82 games would bias the whole distribution analysis without erroring. The
script asserts the recursive walker's game count equals an **independent** count
of `"oddsDenominator"` keys in the raw JSON:

```r
stopifnot("iwgList-v2 parse is incomplete" = length(slugs) == str_count(raw, '"oddsDenominator"'))
```

This catches parser under-counting. It **cannot** catch server-side omission, so
the manual cross-check is: compare the catalogue total against the count the
`all-games` page states on screen.

### Fragility
- **RSC extraction is more fragile than the JSON endpoint.** It depends on the
  `\u003ctable\u003e` anchor and the current Next.js serialisation; a framework
  change could shift it. Well-commented so it can be re-derived.
- **Heterogeneous catalogue.** Alongside the reveal-style games there are bingo,
  cashword and mahjong-style games whose procedures pages may not present a
  single "Approx odds" table. Those return `NULL` from `parse_tiers` (handled by
  `safely()`); the run reports how many games yielded tiers.

---

## 4. Rediscovery / maintenance guide

Both scrapers depend on hardcoded UUIDs and endpoint paths that are **not**
reconstructable from code alone. If either breaks, re-derive via DevTools —
this is the method used to build both:

1. Open the relevant page in Chrome with **DevTools → Network** (type **All**,
   **Preserve log**, **Disable cache**).
2. For a **list endpoint**: reload and watch for the XHR that returns the game
   list (filter Fetch/XHR; look for a response containing `"slug"` or a known
   game name). Its URL gives the endpoint + `@jcr:uuid`. *(This is how
   `iwgList-v2?@jcr:uuid=cb64bf0e-…` was found — network tracking only starts on
   reload, so the pre-load request must be re-triggered.)*
3. For **scratchcard listing node / DAM UUIDs**: read them from the
   `scratchcardList` response.
4. To confirm a candidate endpoint without a browser, a **probe** works (your
   404-test pattern): fire the candidate URLs with `req_error(is_error = \(r) FALSE)`
   and inspect `status` / `content-type` / whether the body contains a known
   slug. Genuine `404`s rule paths out cleanly.

---

## 5. Method comparison

| | Scratchcards | Instant-win games |
|---|---|---|
| Product | Physical retail | Digital / online |
| Authoritative source | Regulatory PDF per game | RSC-embedded HTML per game |
| List endpoint | `scratchcardList?@jcr:uuid=` | `iwgList-v2?@jcr:uuid=` |
| Headline odds | In the PDF | In the catalogue JSON (`oddsNumerator`/`oddsDenominator`) |
| Tier table source | PDF text | Escaped `<table>` in RSC payload |
| Parser | `pdftools` + regex | extract + `jsonlite` unescape + `rvest` |
| Tier granularity | Full | Full (reveal games); varies for bingo/cashword |
| Sturdiness | Moderate (PDF regex) | List sturdy; tier extraction more fragile |
| Chief risk | Row parse failures | Silent incompleteness; RSC anchor drift |

---

## 6. Data outputs (instant-win)

- `national_lottery_instant_win_catalogue.csv` — all games + headline odds.
- `national_lottery_instant_win_prize_tiers.csv` — per-tier odds.
- `national_lottery_instant_win_reconciliation.csv` — headline vs tier-implied odds.

Scratchcard outputs: `national_lottery_scratchcard_games.csv`,
`national_lottery_scratchcard_odds.csv`.

---

*Sources for this document: the project's own reverse-engineering — the
scratchcard `httr2`/`pdftools` script, live endpoint probes, DevTools HAR
captures of the instant-win pages, and a browser network read of the all-games
grid XHR (which surfaced `iwgList-v2`). No external documentation exists for
these private CMS endpoints; they are undocumented and subject to change without
notice.*
