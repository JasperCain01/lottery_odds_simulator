# Project Review — Lottery Odds Simulator

Review date: 2026-07-03 · Branch: `fable_shiny-webr-review-7ooa9h`
Scope requested: (1) mathematical errors, (2) display errors, (3) feasibility
of converting the Shiny app to WebR for GitHub Pages hosting, (4) anything
else relevant. Progress/resume state lives in `REVIEW_PROGRESS.md`.

## Executive summary

- **Mathematics: no substantive errors found.** Every formula in the five
  numerical layers was read line-by-line and the key results independently
  re-derived from the raw CSVs (bypassing the project's own pipeline). The
  full 13-file test suite is green.
- **Display: eight issues found, all fixed on this branch** (F1–F8 below).
  The most user-visible were a jackpot figure that read as a typo
  ("top prize £1,999,995" for an advertised £2,000,000 prize), scientific
  notation in a chart caption ("1.73e-05%"), signed-log axis ticks that
  could be misread as pounds, and inconsistent strategy colours between
  adjacent charts on the Compare tab.
- **WebR conversion: feasible, and a good fit.** Every dependency has a
  WebAssembly build, the engine is comfortably fast enough (worst-case
  capped run is ~0.5 s natively, so a few seconds in wasm), and there is a
  well-trodden `shinylive::export()` → GitHub Actions → GitHub Pages path.
  Estimated effort: roughly a day including CI. Details and a concrete
  migration plan in section 3.
- **Other:** one real dependency bug fixed (`install.R` omitted `stringr`),
  plus recommendations on data staleness and deployment (section 4).

---

## 1. Mathematical review

### Method
- Line-by-line read of `R/data_prep.R`, `R/simulate.R`, `R/metrics.R`,
  `R/strategies.R`, `R/compare.R` (plus the display layers for the numbers
  they surface).
- Independent re-derivation, from the raw CSVs, without using the project's
  pipeline code: per-game RTP / net EV / per-play SD (matched to 1e-10);
  implied overall odds vs the reconciliation file (matched); the
  probabilities-sum-to-1 and losing-row = −price invariants across all
  123 games (hold everywhere).
- Hand-checks of the estimators: the Acerbi–Tasche expected-shortfall
  implementation reproduces a worked-by-hand fractional-tail example
  exactly; the Maritz–Jarrett quantile SE shrinks ~1/√n as claimed;
  a 20,000-session simulation mean sits 0.5 MC standard errors from the
  closed-form analytical mean.

### Findings
**No mathematical errors requiring correction.** The load-bearing design
decisions are correct and, notably, already documented in-code with their
trade-offs:

- The mixture-strategy equivalence (multinomial play allocation ≡ weighted
  mixture of per-game outcome laws) is mathematically right, and the CRN
  (common random numbers) fairness mechanism — one inverse-CDF sampling
  path over value-sorted distributions, all strategies fed identical
  uniforms — is correctly implemented, so compare-mode differences are
  genuinely differences between strategies rather than sampling noise.
- Variance computed on the gross-prize distribution (shift-invariance) is
  correct; moments guard against floating-point cancellation.
- Known approximations are labelled as such in the UI rather than passed
  off as exact: the Normal/CLT interval is called a "reference
  approximation"; the dream-vs-reality conditional session mean is
  documented as "every play avoids the jackpot", not E[P&L | 0 jackpots
  in N] (a fine distinction the code comments call out explicitly).
- One data-quality artifact (six instant-win games with £0-valued small
  prize tiers folding into the losing row) is upstream scrape behaviour,
  already documented in the README and pinned by a test — not a pipeline
  error.

## 2. Display review

### Method
All chart builders rendered to PNG and visually inspected in two N regimes
and all three axis transforms, plus compare mode; every narrative sentence
and alt text printed against real data; the app constructed headlessly.

### Findings (all fixed on this branch)

| # | Issue | Fix |
|---|---|---|
| F1 | Fan-chart alt text: "the 90% of outcomes fall between…" (missing word) | "the central 90% of outcomes" |
| F2 | Narrative and dream-vs-reality caption label the top NET prize as "the top prize (£1,999,995)" — reads as a typo against the advertised £2,000,000 | Now "a net £1,999,995 after the ticket cost" everywhere the figure appears |
| F3 | Budget mode: a budget below the cheapest ticket silently became N = 1 (simulating a play the user can't afford), with note "≈ 1 plays" | Run is blocked with an explanatory note; plays count properly pluralised |
| F4 | Negative currency in alt texts rendered "£-165" and width-padded "£ -12.00" | Shared signed-£ helper → "-£165.00" |
| F5 | Dream caption printed "hits with probability 1.73e-05% per play" (scientific notation) | "hits about 1 in 5,766,840 plays"; tiny percentages formatted non-scientifically |
| F6 | Signed-log axis ticks showed transform units (-3, 0, 3, 6) easily misread as £ (a tick at "-3" means −£19) | Ticks at powers of ten labelled with true £ values; linear axes gained £ tick labels |
| F7 | Leaderboard/crossover chart used a linear N axis: with grid 10–2000, the low-N crossover (the chart's entire story) was squashed into the left margin | Log-10 N axis |
| F8 | Compare tab: the crossover chart used ggplot default colours in alphabetical order while the two charts above used the Okabe-Ito palette in supplied order — the same strategy changed colour between adjacent charts | One palette, one ordering, across all three charts |

Charts confirmed clean after fixes; full test suite green throughout
(one alt-text assertion updated to the new formatting, and it now also pins
the median).

## 3. WebR / GitHub Pages feasibility

**Verdict: feasible with modest effort, and this app is a good candidate.**
The standard route is [Shinylive for R](https://github.com/posit-dev/shinylive):
`shinylive::export()` compiles nothing — it bundles the app source plus
WebAssembly binaries of every R package into a static site that runs the
full R interpreter (webR) in the browser. No server, so GitHub Pages works.

### Why this app fits well

1. **Dependencies.** Everything in `install.R` (shiny, bslib, ggplot2,
   scales, dplyr, readr, tidyr, stringr, matrixStats, cachem, digest) is a
   mainstream CRAN package with WebAssembly binaries in the webR
   repository; none has system-library dependencies (no libcurl/gdal-class
   problems, which are the usual blockers). Since Shinylive 0.8 the export
   step downloads and pins those wasm binaries into the bundle, so the
   deployed app is self-contained and immune to CRAN drift
   ([Shinylive 0.8.0 announcement](https://tidyverse.org/blog/2024/10/shinylive-0-8-0/)).
2. **No server-side requirements.** The app does no networking, no
   database, no file persistence beyond its own `data/` cache — all fine in
   the browser's virtual filesystem.
3. **Compute fits the budget.** Measured natively: the worst allowed run
   (N×R at the 5,000,000-draw UI cap) takes ~0.45 s; the default 52×1,000
   run takes ~0.03 s. WebAssembly typically runs this kind of vectorised R
   at ~2–5× native time, so even the worst case is a few seconds — and the
   app already has progress indicators and a run button (nothing recomputes
   on every slider tick). The existing caps can stay.
4. **Architecture is already right.** Pure function layers, one `app.R`,
   relative paths under one directory — exactly the shape `shinylive::export()`
   wants.

### Migration plan (concrete)

1. **Pre-build the data cache at export time** (recommended) — run
   `run_data_prep(".")` in CI before export so `data/*.rds` ship in the
   bundle, rather than parsing five CSVs through readr in the browser on
   first load. (`load_app_data()` already falls back to building in the
   browser, which works, but costs startup seconds and ships the CSVs.)
2. **Export**: `shinylive::export(appdir = ".", destdir = "_site")` with an
   app manifest trimmed to `app.R`, `R/`, `data/*.rds` (exclude tests,
   scrape scripts, PDFs, CSVs if pre-building).
3. **GitHub Actions workflow** (`.github/workflows/deploy.yml`): on push to
   main — setup-r → install shinylive + deps → build data cache → export →
   `actions/upload-pages-artifact` → `actions/deploy-pages`. This is the
   documented Posit pattern for Shinylive on Pages.
4. **Keep the test suite native.** testthat cannot run in the browser; CI
   keeps running it under normal R before the export step, which also
   guards the export.

### Caveats to plan around (none blocking)

- **First-load download.** The wasm bundle for this dependency set
  (ggplot2 + dplyr + shiny stacks) is roughly 70–100 MB on first visit
  (cached by the browser afterwards). Fine for an educational tool;worth a
  "loading R…" note on the page.
- **Downloads (CSV/PNG buttons).** `downloadHandler` works under recent
  Shinylive. The PNG export path (`ggsave`) needs a file-writing graphics
  device in webR — verify at conversion time; if it misbehaves, either add
  `svglite` (pure-C wasm build available) or drop the PNG button (the CSV
  one is the valuable export).
- **Snapshot semantics.** `on_sale` is computed against `Sys.Date()`. With
  a pre-built cache the date freezes at deploy time — acceptable (the
  scraped data is itself a snapshot) but worth a caption on the page; a
  scheduled re-deploy refreshes both.
- **Memory.** wasm32 has a ~2 GB heap ceiling. The engine's transient
  buffers (~80 MB) are fine, but consider lowering the repeat-run cache
  ceiling in `app_helpers.R` from 512 MB to ~128 MB for the browser build —
  a one-line change.
- **No secrets/analytics**: GitHub Pages is public static hosting; the app
  has nothing sensitive, so nothing to do here.
- **Everything else** (withProgress, showNotification, bslib theming,
  reactivity) is supported by Shinylive unchanged.

Sources: [posit-dev/shinylive](https://github.com/posit-dev/shinylive),
[Shinylive 0.8.0 — wasm binaries bundled](https://tidyverse.org/blog/2024/10/shinylive-0-8-0/),
[webR / Shiny without a server](https://georgestagg.github.io/shiny-without-a-server-2023/),
[R-universe wasm builds](https://ropensci.org/blog/2023/11/17/runiverse-wasm/).

**Effort estimate:** ~0.5–1 day: export script + workflow + the two small
config tweaks + verifying the download buttons in a real browser.

## 4. Anything else

- **Fixed — dependency bug:** `install.R` omitted `stringr`, which
  `R/data_prep.R` attaches directly. On a machine without a tidyverse
  meta-install, `Rscript install.R` followed by the data-prep step failed.
  Added to the list (also noted for apt installs in `REVIEW_PROGRESS.md`).
- **Data staleness:** the scraped catalogues are a mid-2025 snapshot;
  `on_sale`/purchasable status drifts as closure dates pass, so the
  purchasable universe quietly shrinks over time. Worth either re-running
  the scrapers periodically or displaying the snapshot date in the app so
  users know what "on sale" means. (The WebR deployment plan's scheduled
  re-deploy would solve both.)
- **Engineering quality is high** — worth saying explicitly: pure
  side-effect-free layers, an unusually strong test suite (reconciliation
  against independently published odds, property sweeps over all 123 games,
  CRN identity/variance-reduction tests), documented trade-offs at every
  decision point, and honest framing rules enforced in the narrative layer.
  Nothing in this review changes the architecture.
- **Deferred work** (matching `IMPLEMENTATION_PLAN.md` Phase 12):
  path-dependent play (stop-on-jackpot, reinvest-winnings) remains the one
  modelling extension users may ask for; the current i.i.d. fixed-N model
  is clearly documented as such.
