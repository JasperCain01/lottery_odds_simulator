# Lottery Odds Simulator — Implementation Plan

A planning document for an R Shiny dashboard that simulates National Lottery
scratchcard and instant-win playing strategies, and shows how much money a
player is likely to lose (or, rarely, win) over a chosen number of plays.

> Status: **planning only — no app code written yet.**
> This document is the agreed blueprint for build-out.

---

## 1. Goals

1. Let users run **pre-defined or custom playing strategies** across scratchcards,
   instant-win games, or a mix, and see the financial outcome.
2. Answer two headline questions:
   - **(a)** Which strategy **loses the most** over a large number of plays (e.g. 10,000)?
   - **(b)** Which strategy **loses the least / has the best shot at profit** over a
     smaller number of plays (e.g. 1,000)?
3. Present outcomes with **confidence intervals**, because fewer plays → wider,
   more skewed outcome distributions.

The deeper educational story the app is built around:

> Over **many** plays, the **mean** dominates (law of large numbers) → "loses most"
> is just *worst RTP × total stake*, and is nearly deterministic.
> Over **few** plays, **variance** dominates → a game with a worse average but a fat
> jackpot tail can have a better chance of breaking even. This is *why* confidence
> intervals matter and why the two questions have different answers.

---

## 2. Locked-in model decisions

| Decision | Choice |
|---|---|
| Session model | **Fixed *N* plays, fresh money each play** (winnings pocketed, not reinvested). Session P&L = sum of *N* i.i.d. net-outcome draws. |
| Controls | **Two independent sliders**: *N* = tickets per session; *R* = number of simulated sessions (drives CI tightness). |
| Odds handling | **Use raw scraped odds**, but **flag** games with implausible RTP (>85% soft, >100% hard) and offer an optional cap on top-tier contribution. |
| Strategy scope | **Preset strategies + custom mix builder**, spanning both sources. |

---

## 3. Data landscape (validated)

- **Scratchcards** (`national_lottery_scratchcard_odds.csv`): 41 games, 1,198 tier
  rows. `ticket_price` embedded (£1/£2/£3/£5). Duplicate prize values across
  breakdown rows → aggregate by net value for P&L, keep raw for display.
- **Instant win** (`national_lottery_instant_win_prize_tiers.csv`): 79 games, 4,769
  rows. **No price column** — join to `national_lottery_instant_win_catalogue.csv`
  on `slug` for `price_gbp` (£0.25–£10) and metadata.
- **Losing outcome** is implicit: `P(lose) = 1 − Σ implied_probability` (~0.65
  scratchcards, ~0.71 instant win).
- **RTP spread:** scratchcards 53%→65% median; instant win **11.6%→74%**. Low-price
  instant games are the biggest money-shredders.
- **Volatility spread:** a few jackpot games have per-play SD in the hundreds/thousands
  of £ vs a £5 stake — these drive the skew and the interesting small-*N* behaviour.
- **Data-quality flag:** raw odds push a few jackpot games to RTP >100% (rounded,
  high-tolerance top-tier odds). Handled via the flag/cap toggle, not silently.
- **Game-universe metadata** (catalogue): `is_hidden`, `prohibited`, `end_date` /
  `closure_date`, `category`. Used for filtering to purchasable games.

---

## 4. Architecture (with all agreed add-ons)

```
R/
  data_prep.R      # Layer 1: normalise both CSVs -> tidy outcomes table (.rds)
  simulate.R       # Layer 2: monte_carlo() + analytical() engines, CRN, chunking
  metrics.R        # Layer 3: risk/engagement/decomposition metrics, MC std error
  strategies.R     # Layer 4: preset + custom strategy definitions, filters
  narrative.R      # Layer 5: plain-English summary generator
  viz.R            # Layer 6: fan chart, histogram, leaderboard-vs-N, play-by-play
app.R              # Shiny UI + server (or ui.R / server.R split)
data/              # cached .rds artifacts from data_prep.R
tests/testthat/    # correctness + statistical property tests
```

### Feature set folded in from the review

- **Analytical outputs:** dream-vs-reality decomposition (distribution conditional on
  *not* hitting the top tier vs full); risk metrics (P(profit), P(lose > £X), 5%
  worst-case VaR / expected shortfall); engagement metrics (expected wins/session,
  count of wins ≥ k×stake, longest losing streak); Monte Carlo standard error on all
  reported percentiles; leaderboard-vs-*N* small-multiple.
- **Play options:** game-universe filters (on-sale, price, category, source);
  budget/time framing layer ("£X/week for a year" → *N*). Reinvestment and stop-win
  modes are **architected for but deferred** (path-dependent — see Phase 12).
- **Presentation:** plain-English auto-narrative; single-session play-by-play;
  common random numbers (CRN) + seed control for fair compare mode; log/winsorize
  toggle for skewed histograms; shareable/exportable scenarios.

---

## 5. Model & effort guidance — how to read the recommendations

Per-phase recommendations use this heuristic:

- **Opus 4.8** → correctness-critical, novel reasoning, numerical/statistical
  subtlety, subtle debugging. Being wrong here is costly and non-obvious.
- **Sonnet 5** → well-specified implementation, UI wiring, boilerplate, docs.
  Fast and capable when the spec is clear.
- **Effort** (maps to escalating extended-thinking): **Low** = mechanical /
  well-trodden; **Medium** = standard implementation with design choices;
  **High** = genuine reasoning / correctness-critical; **Max** = tricky algorithm
  or subtle debugging where mistakes are expensive.

These are defaults, not rules: **escalate mid-phase** if a task surprises you
(e.g. drop from Sonnet→Opus, or Medium→High, when an edge case turns hairy).
Overall pattern: the **engine and statistics run on Opus**; the **UI, plumbing,
and docs run on Sonnet** to save cost/time.

---

## 6. Phased implementation plan

Each phase lists: goal, key tasks, deliverables, dependencies, recommended
model + effort, and relevant skills. All phases follow the repo conventions:
`claude_`-prefixed branches, small logical commits, chunk-header comments, and
WHY/trade-off comments (per global CLAUDE.md).

---

### Phase 0 — Project scaffolding & environment
- **Goal:** reproducible project skeleton.
- **Tasks:** create `R/`, `data/`, `tests/` structure; set up `renv`; declare deps
  (shiny, bslib, dplyr, readr, tidyr, ggplot2, scales, testthat, withr); add
  `.gitignore` entries for `.rds` cache if large; stub files.
- **Deliverables:** buildable empty project, `renv.lock`.
- **Depends on:** —
- **Model / effort:** **Sonnet 5 · Low.**
- **Rationale:** pure mechanical setup, no reasoning.
- **Skills:** `shiny-bslib` (scaffolding conventions).

### Phase 1 — Data prep / normalisation layer
- **Goal:** one tidy `outcomes` table + per-game summary + game-universe metadata.
- **Tasks:** read both CSVs; join instant-win tiers → catalogue on `slug`; derive
  `net_value = prize − price` per winning row and the implicit `−price` losing row
  (`prob = 1 − Σ`); aggregate duplicate breakdown rows by net value (keep raw for
  display); compute per-game RTP, net-EV/play, per-play variance/SD, headline odds;
  compute **RTP outlier flags** + optional top-tier cap; attach catalogue metadata
  (hidden/prohibited/dates/category/source); cache to `.rds`; emit a
  human-readable **RTP/outlier report** for eyeballing before any simulation.
- **Deliverables:** `data/outcomes.rds`, `data/game_summary.rds`, printed report.
- **Depends on:** Phase 0.
- **Model / effort:** **Opus 4.8 · Medium–High.**
- **Rationale:** everything downstream trusts this. Join correctness, the implicit
  loss row, duplicate aggregation, and RTP maths must be exactly right; the outlier
  logic needs judgement. Not novel, but unforgiving.
- **Skills:** `critical-code-reviewer` (self-check the transforms).

### Phase 2 — Simulation engine
- **Goal:** the numerical core: Monte Carlo + analytical, correct and fast.
- **Tasks:** `analytical(game, N, conf)` → mean `N·netEV`, var `N·Var`, CLT CI;
  `monte_carlo(game, N, R, seed)` → vectorised `sample()` draws, `rowSums`,
  empirical percentiles; **memory chunking** for large N×R (cap + batch, documented
  trade-off); **CRN** support (shared uniform draws for fair comparisons);
  fan-chart **quantile checkpointing** (~100 points along *N*, not every play);
  return tidy result objects consumed by metrics/viz.
- **Deliverables:** `R/simulate.R` with both engines + result contract.
- **Depends on:** Phase 1.
- **Model / effort:** **Opus 4.8 · High (Max if debugging skew/memory).**
- **Rationale:** the crown jewel. Statistical correctness (skew, CI, CRN),
  numerical stability, and memory/perf trade-offs all live here; mistakes are
  subtle and propagate everywhere.
- **Skills:** `testing-r-packages` (design property tests alongside), `mirai`
  (only if parallelising R).

### Phase 3 — Metrics & analytical outputs
- **Goal:** turn raw simulation output into the decision-relevant numbers.
- **Tasks:** risk metrics (P(profit), P(lose > £X), 5% VaR / expected shortfall);
  **dream-vs-reality decomposition** (full vs conditional-on-no-jackpot
  distribution); engagement metrics (expected wins/session, count of wins ≥ k×stake,
  longest losing streak); **Monte Carlo standard error** on reported percentiles;
  leaderboard-vs-*N* series.
- **Deliverables:** `R/metrics.R` returning a tidy metrics bundle per run.
- **Depends on:** Phase 2.
- **Model / effort:** **Opus 4.8 · Medium–High.**
- **Rationale:** expected shortfall, the conditional decomposition, and MC standard
  error are easy to get subtly wrong; correctness matters for credibility.
- **Skills:** `critical-code-reviewer`.

### Phase 4 — Strategy layer
- **Goal:** define how plays are allocated across games.
- **Tasks:** preset strategies (single game, always cheapest, always priciest,
  best RTP, worst RTP, biggest jackpot, random-each-play); custom **mix builder**
  (games + weights, multinomial allocation under fixed *N*, documented); **filters**
  (on-sale, price, category, source); **budget/time framing** translation
  (budget × horizon → *N*).
- **Deliverables:** `R/strategies.R` with a uniform strategy interface.
- **Depends on:** Phases 1–2.
- **Model / effort:** **Sonnet 5 · Medium** (escalate to Opus for the multinomial
  allocation + mixed-price edge cases).
- **Rationale:** mostly well-specified plumbing over the engine; only the allocation
  maths needs care.

### Phase 5 — Shiny app skeleton
- **Goal:** working app shell with inputs and reactive wiring.
- **Tasks:** `bslib` page layout (sidebar inputs, main tabs); inputs for strategy,
  *N*, *R*, confidence level, filters, seed; reactive graph connecting inputs →
  strategy → engine → metrics; loading/lazy-eval handling for slow runs.
- **Deliverables:** runnable `app.R` rendering placeholder outputs.
- **Depends on:** Phases 2–4.
- **Model / effort:** **Sonnet 5 · Low–Medium.**
- **Rationale:** standard Shiny/bslib scaffolding; well-trodden.
- **Skills:** `shiny-bslib`, `brand-yml` (theming), `shiny-bslib-theming`.

### Phase 6 — Visualisations
- **Goal:** the charts that carry the story.
- **Tasks:** fan chart (cumulative P&L, CI ribbon from checkpoints); final-P&L
  histogram/density with break-even/mean/median/percentile markers;
  leaderboard-vs-*N* small multiple; single-session **play-by-play** view;
  **log/winsorize** toggle for skewed distributions; sortable leaderboard table.
- **Deliverables:** `R/viz.R`, wired into `app.R`.
- **Depends on:** Phases 3, 5.
- **Model / effort:** **Sonnet 5 · Medium** (Opus for the fan-chart checkpoint→ribbon
  mapping if it gets fiddly).
- **Rationale:** ggplot craft over well-defined inputs; mostly execution.
- **Skills:** `alt-text` (accessible figures).

### Phase 7 — Plain-English narrative generator
- **Goal:** auto-generated, non-overstated summary sentence(s) per run.
- **Tasks:** template engine mapping metrics → prose ("typically lose £Y, 90% chance
  £A–£B, profit ~Z% of the time, driven by a 1-in-W jackpot"); correct rounding,
  pluralisation, and honest framing (no overstated upside).
- **Deliverables:** `R/narrative.R`.
- **Depends on:** Phase 3.
- **Model / effort:** **Sonnet 5 · Medium.**
- **Rationale:** templating with light judgement on wording; not correctness-critical
  once metrics are trusted.
- **Skills:** `cli` (pluralisation/formatting patterns).

### Phase 8 — Compare mode
- **Goal:** put strategies head-to-head fairly.
- **Tasks:** multi-strategy overlay on distribution + fan chart; **CRN wiring** so
  strategies share random draws; **seed control** surfaced in UI; low-*N* vs high-*N*
  side-by-side to show the mean/variance crossover; scenario **export/share**
  (bookmarkable state, PNG/CSV download).
- **Deliverables:** compare tab + reproducible/exportable runs.
- **Depends on:** Phases 2, 6.
- **Model / effort:** **Opus 4.8 · Medium.**
- **Rationale:** CRN correctness (same draws → honest comparison) is the one subtle
  part; the rest is UI.

### Phase 9 — Performance & caching
- **Goal:** keep interactive runs snappy.
- **Tasks:** memoise/cache repeat runs; precompute leaderboard analytics; profile
  hot paths; tune chunk sizes; guard N×R memory ceiling with user-facing limits.
- **Deliverables:** measured speedups, documented limits.
- **Depends on:** Phases 2–6.
- **Model / effort:** **Opus 4.8 · Medium–High** for the profiling/optimisation;
  **Sonnet 5 · Low** for mechanical caching.
- **Rationale:** optimisation needs reasoning about where time/memory actually go.
- **Skills:** `mirai` (parallelism if needed).

### Phase 10 — Testing & validation
- **Goal:** trustworthy numbers.
- **Tasks:** unit tests for data-prep transforms; **statistical property tests**
  (simulated mean ≈ analytical `N·netEV` within MC error; probabilities sum to 1;
  net-value maths; CRN reproducibility; percentile ordering); reconcile implied vs
  published overall odds using `..._reconciliation.csv`; edge cases (N=1, R=1,
  zero-probability tiers, capped games).
- **Deliverables:** `tests/testthat/` suite, green.
- **Depends on:** Phases 1–4 (ongoing, but formalised here).
- **Model / effort:** **Opus 4.8 · Medium** for designing the statistical/property
  tests; **Sonnet 5 · Low–Medium** for routine coverage.
- **Rationale:** the property tests are where correctness is actually proven; they
  need statistical judgement.
- **Skills:** `testing-r-packages`.

### Phase 11 — Polish, accessibility, docs, deployment
- **Goal:** shippable.
- **Tasks:** README + usage docs; alt text on all figures; responsive/theme polish;
  deployment (shinyapps.io / Posit Connect); optional `brand.yml`.
- **Deliverables:** deployed app + docs.
- **Depends on:** all prior.
- **Model / effort:** **Sonnet 5 · Low.**
- **Rationale:** documentation and deployment plumbing.
- **Skills:** `alt-text`, `brand-yml`, `shiny-bslib-theming`.

### Phase 12 — Future extensions (deferred, architected-for)
- **Goal:** the path-dependent and higher-fidelity modes.
- **Tasks:** **reinvestment / play-till-broke** (gambler's ruin, sequential loop,
  Monte Carlo only); **stop-win rule** ("quit if up £X"); **odds-tolerance
  propagation** (widen CI using `tolerance_pct`); **Rcpp** for the sequential paths
  if R is too slow.
- **Model / effort:** **Opus 4.8 · High (Max for Rcpp / ruin correctness).**
- **Rationale:** path-dependence, ruin dynamics, and native-code correctness are the
  hardest reasoning in the whole project.

---

## 7. Model & effort summary table

| Phase | Focus | Model | Effort |
|---|---|---|---|
| 0 | Scaffolding | Sonnet 5 | Low |
| 1 | Data prep | **Opus 4.8** | Med–High |
| 2 | Simulation engine | **Opus 4.8** | **High** (Max if debugging) |
| 3 | Metrics/outputs | **Opus 4.8** | Med–High |
| 4 | Strategy layer | Sonnet 5 (→Opus for allocation) | Medium |
| 5 | Shiny skeleton | Sonnet 5 | Low–Med |
| 6 | Visualisations | Sonnet 5 (→Opus if fiddly) | Medium |
| 7 | Narrative | Sonnet 5 | Medium |
| 8 | Compare mode | **Opus 4.8** | Medium |
| 9 | Performance | Opus 4.8 / Sonnet 5 | Med–High / Low |
| 10 | Testing | **Opus 4.8** (design) / Sonnet 5 (fill) | Medium |
| 11 | Polish/docs/deploy | Sonnet 5 | Low |
| 12 | Future (path-dependent) | **Opus 4.8** | **High–Max** |

**Rule of thumb:** Opus for the engine + statistics + anything path-dependent;
Sonnet for UI, plumbing, docs. Escalate effort/model mid-phase on surprise.

---

## 8. Risk & edge-case register

- Raw odds → RTP >100% on jackpot games (handled: flag/cap toggle).
- N×R memory blow-up (handled: cap + chunking).
- Skew breaks Normal CI at small *N* (handled: report empirical percentiles, not
  mean±SD; keep analytical only as reference overlay).
- Mixed-price strategies → variable total spend under fixed *N* (documented; expose
  realised spend).
- Prohibited/hidden/expired games leaking into presets (handled: filters).
- MC sampling error mistaken for real signal in compare mode (handled: CRN + MC
  standard error display).

---

## 9. Build order (critical path)

`0 → 1 → 2 → 3 → 4 → 5 → 6 → 7 → 8 → 9 → 10 → 11`, with **10 (testing) run
continuously from Phase 1 onward**, formalised at Phase 10. Phase 12 is post-v1.
