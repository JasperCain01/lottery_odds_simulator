# Lottery Odds Simulator

An R Shiny dashboard that simulates National Lottery scratchcard and
instant-win playing strategies, and shows -- honestly, with confidence
intervals rather than a single misleading average -- how much money a player
is likely to lose (or, rarely, win) over a chosen number of plays.

## The educational story

Every game's per-play outcome is a mixture of "lose the stake" (almost
always) and a ladder of prize tiers (rarely, with a vanishingly rare top
prize). Two things are true about that mixture at the same time, and the app
is built to make both visible instead of hiding one behind the other:

- **Over many plays (large N)**, the Law of Large Numbers takes over: the
  session result converges tightly around `N x` the game's expected value per
  play, which is set by its Return to Player (RTP). The house edge becomes
  near-deterministic and the outcome stops being interesting to look at.
- **Over few plays (small N)**, variance and the jackpot tail dominate. The
  *mean* outcome is pulled a long way from what almost every session actually
  experiences, because it is averaging in a vanishingly rare huge win. The
  *median*, and the spread between percentiles, tell the honest story; the
  mean alone does not.

That is why the app never reports a single number. It reports **confidence
intervals / percentile bands** (the fan chart, the histogram's marked
percentiles), a **dream-vs-reality decomposition** (the advertised full
distribution vs. what a session looks like conditional on not hitting the top
prize -- i.e. almost every session), and a **plain-English narrative** that is
written to never oversell the upside. Compare mode goes one step further:
when you put two strategies head-to-head, it drives them on the *same*
underlying random draws (Common Random Numbers) so an observed difference is
guaranteed to be real, not sampling noise -- which is exactly the kind of
crossover ("a worse-average, higher-variance game can have better break-even
odds at small N, but loses that edge as N grows") the small-N-vs-large-N story
predicts.

## Architecture

Backend layers under `R/`, each a `source()`-side-effect-free file of pure
functions (see `IMPLEMENTATION_PLAN.md` for the full phased design), plus the
Shiny app that wires them together:

| File | Layer | Purpose |
|---|---|---|
| `R/data_prep.R` | 1. Data prep | Normalises the raw scraped CSVs into a tidy `outcomes` table (net value + probability per game) and a per-game `game_summary` (RTP/EV/variance/flags); writes the `data/*.rds` cache. |
| `R/simulate.R` | 2. Simulation engine | The numerical core: `analytical_summary()` (closed-form Normal/CLT reference) and `simulate_sessions()` (memory-chunked Monte Carlo over R sessions of N plays, with checkpointed quantiles for the fan chart and Common-Random-Numbers support). |
| `R/metrics.R` | 3. Metrics | Turns raw simulation output into decision-relevant numbers: risk (P(profit), VaR, expected shortfall), the dream-vs-reality decomposition, engagement metrics (expected wins, losing-streak length), and Monte Carlo standard errors. |
| `R/strategies.R` | 4. Strategies | Defines how plays are allocated across games: single-game and preset strategies (cheapest/priciest/best-RTP/worst-RTP/biggest-jackpot/random-each-play), a custom weighted mix builder, universe filters, and budget-to-N framing. |
| `R/app_helpers.R` | 5. App glue | Plain (non-reactive) functions that wire data_prep/simulate/metrics/strategies together for the Shiny server, plus the Phase 9 repeat-run memory cache. |
| `R/viz.R` | 6. Visualisations | The chart builders (fan chart, P&L histogram, dream-vs-reality, leaderboard-vs-N, single-session play-by-play, compare-mode overlays) plus their `viz_*_alt()` alt-text companions and the log/winsorize skew transform. |
| `R/narrative.R` | 7. Narrative | Generates the plain-English, honestly-framed summary sentences shown in the app's "In plain English" card. |
| `R/compare.R` | 8. Compare mode | Drives two or more strategies head-to-head on Common Random Numbers for a fair comparison, and assembles the tidy structures the compare-mode charts and table consume. |
| `app.R` | UI + server | A `bslib::page_sidebar()` Shiny app: sidebar filters/strategy/session-design controls, an expensive pipeline gated behind a "Run simulation" button, and a tabbed chart/metric/compare panel. |
| `install.R` | — | Lightweight dependency installer (installs only what's missing; no `renv` -- see "Dependency management" below). |

Test coverage lives in `tests/testthat/`, one `test-<layer>.R` file per layer
above, plus `test-app.R` (Shiny wiring via `shiny::testServer()`),
`test-reconciliation.R` and `test-validation.R` (statistical/data-integrity
property tests across every real game), and `test-edge-cases.R`.

## Install

```sh
Rscript install.R
```

This installs only whatever packages from the required set (shiny, bslib,
dplyr, readr, tidyr, ggplot2, scales, matrixStats, cachem, digest, testthat,
withr) are missing from your existing R library. If you'd rather use your
system package manager, `HANDOVER.md` documents the apt path for a fresh
Ubuntu container.

### Building the data cache

The app reads pre-built `data/outcomes.rds` / `data/game_summary.rds` /
`data/tiers_raw.rds` (git-ignored, regenerated from the raw scraped CSVs at
the repo root). Build them explicitly with:

```sh
Rscript -e 'source("R/data_prep.R"); run_data_prep(".")'
```

or just launch the app / run the test suite -- both auto-build the cache on
first run if it's missing (`load_app_data()` in `R/app_helpers.R`).

### Locale note

Render charts and run the app under a UTF-8 locale so the `£` glyph draws
correctly (the default `C` locale renders it as `..`):

```sh
export LANG=C.UTF-8 LC_ALL=C.UTF-8
```

## Run

```sh
Rscript -e 'shiny::runApp()'
```

or open `app.R` in RStudio and click *Run App*.

## Run the tests

```sh
Rscript -e 'library(testthat); testthat::test_dir("tests/testthat", stop_on_failure=TRUE, reporter="summary")'
```

## Feature tour

- **Strategies.** Pick a single game, one of six presets (always cheapest,
  always priciest, best RTP, worst RTP, biggest jackpot, random-each-play), or
  build a custom weighted mix of several games. Sidebar filters restrict the
  game universe by purchasability, on-sale status, price range, category, and
  source.
- **How many plays.** A "play" is one ticket/scratchcard; this control is how
  many you play *over time* (e.g. one a week for a year is about 52) -- the
  variable behind the whole small-N-vs-large-N story. Set it directly, or let
  the app work it out from a budget over time (£X per period for N periods,
  divided by the strategy's expected price per play). Over many plays the house
  edge becomes near-certain; over few, luck dominates.
- **Simulation accuracy.** The number of simulation runs controls how precisely
  the odds are estimated (more runs = smoother, tighter estimates) -- it does
  *not* change the typical outcome, so above ~1,000 runs the charts change
  little. A confidence level and a random seed (fully reproducible per seed)
  round out the controls. Plays x runs is capped to keep the app responsive;
  the engine chunks internally so it never materialises the full matrix.
- **Charts.** A cumulative-P&L fan chart with a nested percentile ribbon; a
  final-P&L histogram with break-even/mean/median/percentile markers and an
  optional winsorize/signed-log transform for heavily skewed distributions; a
  dream-vs-reality bar chart; a single-session play-by-play path coloured by
  win/loss; and a leaderboard-vs-N small multiple showing how the ranking of
  strategies shifts as N grows (the low-N/high-N crossover). Every chart
  carries programmatically generated alt text for screen readers.
- **Compare mode**, with CRN. Tick several strategies (plus, optionally, the
  one currently configured in the sidebar) and run them head-to-head on the
  *same* underlying random draws (Common Random Numbers, via a shared seed) --
  so the differences the overlaid distribution and median-path charts show are
  real strategy differences, not sampling noise. Export the comparison table
  as CSV and the distribution chart as PNG.
- **Plain-English narrative.** The "In plain English" card above the charts
  auto-generates a small set of honestly-framed sentences from the run's
  metrics -- e.g. typical loss, the range a given confidence level covers,
  how often the session ends in profit, and what's driving the tail -- and
  never overstates the upside.
- **Risk & engagement / Details tabs.** Headline risk metrics (VaR, expected
  shortfall, P(profit)), engagement metrics (expected wins/session, losing
  streak length), and a full text dump of the strategy composition, realised
  spend framing, and the analytical Normal/CLT reference (flagged as
  unreliable at small N for skewed games -- that unreliability is itself part
  of the app's point, not a bug).

## Data quality note

Six instant-win games --
`3-in-a-row`, `lots-of-luck`, `lotto-hi-lo`, `lucky-stars`,
`pennies-and-pounds`, `prize-ball` -- carry prize tiers labelled with a small
cash amount (e.g. "£0.25", "£0.50") in the raw scraped data, but that tier's
`gross_prize` was scraped as `0`. Because `data_prep` correctly computes
`net_value = gross_prize - price`, those tiers fold into the losing row
(`net_value == -price`) rather than appearing as a (tiny) win. This is a
**raw-data scraping artifact** (see `SCRAPING_METHODS.md`), not a data-prep
defect: `data_prep` is verified correct against the tier data it was given.
It does mean that for these six games, the "any prize won" odds published on
the game page are slightly better than the "won more than you staked" odds
this app simulates. The offset is small, systematic, and fully explained by
the zero-value tier mass -- pinned by
`tests/testthat/test-reconciliation.R`, which reconciles every game's implied
overall odds against an independently published figure
(`national_lottery_instant_win_reconciliation.csv`) and asserts this exact
set is where (and only where) the two definitions of "win" diverge.

## Dependency management

Dependency management is intentionally lightweight: `install.R` installs only
whatever is missing from your existing R library (no `renv`). This is a
deliberate choice, not an oversight -- see the header comment in `install.R`
for the trade-off. `renv` would buy a fully pinned, reproducible lockfile at
the cost of an extra bootstrap step and a second source of truth for
dependencies alongside `install.R`'s flat vector; for a single-app project
with a short, stable dependency list, documenting versions here (and pinning
CRAN snapshot dates at deploy time, if desired) is simpler to keep correct.
If the dependency surface grows substantially or multiple contributors need
guaranteed-identical environments, revisit `renv::init()` at that point.

## Deployment

The app is a standard single-file (`app.R` + `R/*.R` + `data/`) Shiny app and
deploys as-is to shinyapps.io or Posit Connect. No credentials are configured
in this repository -- deploying requires your own shinyapps.io/Posit Connect
account.

1. Install the `rsconnect` package and authorize it with your account (once
   per machine): `rsconnect::setAccountInfo(name, token, secret)`
   (shinyapps.io) or `rsconnect::connectApiUser()` / `connectUser()` (Posit
   Connect) -- see the [rsconnect
   docs](https://docs.posit.co/shinyapps.io/getting-started.html).
2. **Build the data cache before deploying** (`Rscript -e
   'source("R/data_prep.R"); run_data_prep(".")'`) so `data/*.rds` exists to
   be bundled -- it is git-ignored and `rsconnect::deployApp()` bundles
   whatever is on disk, not what `.gitignore` excludes.
3. Deploy from the project root:

   ```r
   rsconnect::deployApp(
     appDir = ".",
     appFiles = c(
       "app.R", "install.R",
       list.files("R", pattern = "\\.R$", full.names = TRUE),
       list.files("data", pattern = "\\.rds$", full.names = TRUE)
     )
   )
   ```

   A commented-out version of this call lives in `deploy.R` at the repo root
   as a ready-to-uncomment starting point; it is not run automatically by
   anything in this repository.

`brand.yml` (bslib's declarative branding format) is not adopted here -- the
app's single `bs_theme(version = 5, primary = "#2C3E50")` call in `app.R` is
simple enough that a separate branding file would add indirection without
buying anything; revisit if the theme grows more elaborate.
