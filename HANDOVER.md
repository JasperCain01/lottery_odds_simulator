# HANDOVER — build status & resume protocol

Living status doc for the phased build of the Lottery Odds Simulator (an R
Shiny dashboard). Maintained by the orchestrating agent so any session can
resume from the git branch alone, without re-reading prior chat history.

**Branch:** `claude_lottery-simulator-build` (develop here; push here).
**Plan of record:** `IMPLEMENTATION_PLAN.md` (12 phases). This file tracks
progress against it.

## Orchestration model
Each phase is implemented by a dispatched worker (sub-agent), then the
orchestrator **independently verifies** before committing: runs the full test
suite, reads the diff, and — for numerical phases — re-derives key results;
for viz phases — renders charts to PNG and looks at them. Only then commit +
push. Model per phase follows the plan's table (Opus for engine/stats/CRN/perf;
Sonnet for UI/plumbing/docs).

## Environment setup (fresh container)
The container is ephemeral; a reclaimed container has **no R installed**.
- Install R + core CRAN binaries (fast apt path on Ubuntu 24.04):
  `sudo apt-get update -qq && sudo apt-get install -y -qq r-base-core r-cran-testthat r-cran-dplyr r-cran-readr r-cran-tidyr r-cran-withr r-cran-ggplot2 r-cran-scales r-cran-shiny r-cran-bslib r-cran-matrixstats r-cran-digest r-cran-cachem`
  (or `Rscript install.R` for the CRAN set once R is present).
- **Build the data cache first:** `Rscript -e 'source("R/data_prep.R"); run_data_prep(".")'` → writes `data/{outcomes,game_summary,tiers_raw}.rds` (git-ignored).
- **Run the suite:** `Rscript -e 'library(testthat); testthat::test_dir("tests/testthat", stop_on_failure=TRUE, reporter="summary")'`
- **Render charts / run app under UTF-8** so the `£` glyph draws:
  `export LANG=C.UTF-8 LC_ALL=C.UTF-8` (the default `C` locale shows `£` as `..`).

## Conventions (do not break)
- `£` in **executable** strings must use the `£` escape (literal `£`
  misencodes under a non-UTF-8/`C` locale and breaks `grepl`/`expect_equal`);
  literal `£` is fine in comments. See `R/narrative.R`, `R/compare.R`.
- Every `R/*.R` layer is `source()`-side-effect-free (functions only).
- Each phase adds `tests/testthat/test-<layer>.R`; keep the full suite green.
- Sign convention: P&L negative = loss throughout.

## Phase ledger
| Phase | Status | Commit | Notes |
|---|---|---|---|
| 0 Scaffolding | ✅ | 02dd56f | |
| 1 Data prep | ✅ | 4432a80 / b96d68e | `outcomes`, `game_summary`, RTP report |
| 2 Simulation engine | ✅ | cb914a2 | MC + analytical, CRN, chunking |
| 3 Metrics | ✅ | 77ca02b | VaR/ES, dream-vs-reality, MJ SE, streak |
| 4 Strategies | ✅ | 0537679 | presets, mix builder, filters, budget→N |
| 5 Shiny skeleton | ✅ | ee7c6d1 | bslib page_sidebar, reactive graph |
| 6 Visualisations | ✅ | 1bd012b | fan/hist/dream/leaderboard/play-by-play |
| 7 Narrative | ✅ | f9a042b | honest plain-English summary |
| 8 Compare mode | ✅ | c50c0c1 | CRN head-to-head, overlays, export |
| 9 Performance & caching | ✅ | 72a9281 | faster engine, tuned chunk, result cache |
| 10 Testing & validation | ✅ | (this commit) | reconciliation + property sweep (123 games) + edge cases |
| 11 Polish/docs/deploy | ✅ | (this commit) | README rewrite, `deploy.R`, chart text-wrap, `£` escapes, alt-text audit — see brief below |

**Build complete.** All 11 phases of `IMPLEMENTATION_PLAN.md` are done; Phase
12 (path-dependent / reinvestment modes) remains explicitly deferred/future
work, not part of this build.

## Phase 10 — brief (re-dispatch verbatim if lost)
Formalise testing (property tests + reconciliation) beyond the per-phase
fixtures. Model: **Opus · Medium**. Deliverables:
1. **Reconciliation test** (`tests/testthat/test-reconciliation.R`): using
   `national_lottery_instant_win_reconciliation.csv` (cols: `slug`,
   `page_overall_1_in` [published], `implied_overall_1_in` [from scraped odds],
   `catalogue_overall_1_in`, `diff_pct`). Recompute each game's implied overall
   odds `1 / P(win)` from the prepared data (P(win) = sum of winning-tier
   probabilities per game) and assert it matches `implied_overall_1_in` within a
   small tolerance, and that the implied-vs-published `diff_pct` is within a
   documented band (flag, don't hard-fail, the few known high-tolerance jackpot
   games). Validates the whole data-prep → outcomes pipeline against an
   independent published figure.
2. **Statistical property tests across ALL real games**
   (`tests/testthat/test-validation.R`): iterate every game in `outcomes` and
   assert: probabilities sum to 1; `net_value = prize − price` consistency;
   simulated `mean(totals)` ≈ analytical `N·netEV` within k·MC-SE (fixed seed,
   principled k, e.g. 4–5, so non-flaky); empirical percentiles are ordered;
   CRN reproducibility. Keep runtime reasonable (modest N/R per game, use the
   cached engine, maybe a representative subset + a few extremes if all-games is
   slow — document the choice).
3. **Edge cases**: N=1, R=1, zero-probability tiers, RTP-capped games.
Verify: `shiny::shinyAppFile("app.R")` constructs; FULL suite green
(`stop_on_failure=TRUE`). Do NOT commit (orchestrator commits after review).
Report: what was reconciled + the max diff observed; how many games the
property sweep covers + the tightest margin; the final `test_dir` line.

## Phase 11 — outline + accumulated polish items (done)
- ✅ README usage/deploy docs — full rewrite: educational story, architecture
  table (one line per `R/*.R` layer + `app.R`), install/data-cache/run/test
  commands, feature tour, data-quality note, dependency-management rationale,
  deployment section.
- ✅ Alt text already on figures (Phase 6) — re-audited: all 8 `renderPlot()`
  calls in `app.R` (fan/dist/dream/pbp/leaderboard/compare_dist/compare_fan/
  compare_crossover) pass `alt = function() ...` wired to the matching
  `viz_*_alt()`. No gaps found; no change needed.
- ✅ Deployment (shinyapps.io / Posit Connect) — documented in README +
  `deploy.R` (commented-out `rsconnect::deployApp()` call with the explicit
  file manifest incl. `data/*.rds`). No actual deploy attempted (no
  credentials).
- ✅ `renv` / `brand.yml` — deliberately **not** added; documented the
  reasoning in README's "Dependency management" section instead
  (`install.R`'s flat-vector approach stays simpler for a short, stable
  dependency list on a single-app project — revisit if that changes).
- ✅ **Chart text truncation** (dream-vs-reality caption, compare-distribution
  subtitle) — added `.viz_wrap()` in `R/viz.R` (base `strwrap()`, not
  `stringr` — not an existing dependency and base R suffices) and applied it
  to both. Re-rendered both charts to PNG under `LANG=C.UTF-8`: wraps to 2-3
  lines, fits within render width, no warnings.
- ✅ **`£` escape consistency** — replaced 21 literal `£` occurrences in
  EXECUTABLE strings in `R/viz.R` (axis labels, `sprintf()` templates, marker/
  caption text) with the `£` escape, matching `narrative.R`/`compare.R`.
  Literal `£` left as-is in comments (9 occurrences). Full suite reconfirmed
  green after the change (no test matched the old literal text).
- ✅ **Data-quality note** (from Phase 10 reconciliation): 6 instant-win games
  (`3-in-a-row, lots-of-luck, lotto-hi-lo, lucky-stars, pennies-and-pounds,
  prize-ball`) carry prize tiers labelled as small cash but scraped with
  `gross_prize == 0`, so they fold into the losing row (net_value = −price).
  `data_prep` is correct (verified); this is an upstream scrape artifact.
  Documented in README's "Data quality note" section. Pinned by
  `tests/testthat/test-reconciliation.R`.

## Resume protocol
1. Read this file + `git log --oneline`. The last ✅ commit is the last
   verified phase; anything after is unreviewed.
2. `git status`: if a phase's files are present uncommitted (worker WIP survived
   in the container), verify them (suite + read + re-derive) and commit.
3. If the working tree is clean but the ledger shows a phase 🔄, that phase's
   worker output was lost — re-dispatch it using the brief above (minimal
   repeat: only the one phase re-runs).
4. Continue the ledger.
