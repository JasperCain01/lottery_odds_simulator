# What the tests protect

There are 147 `test_that()` blocks in `tests/testthat/`. This is a Shiny app rather than a package, so run them from the repository root:

```
Rscript tests/testthat.R
```

or, for one file: `testthat::test_file("tests/testthat/test-simulate.R")`. Each file loads the `R/` layers it needs.

**Data.** Tests that need the real catalogue call `test_data()` from `helper-data.R`. It runs `prepare_data()` once per test run, from the committed raw CSVs, with `today` pinned to 2026-07-01 so `on_sale` is reproducible. The tests never read the git-ignored `data/*.rds` cache. The exception is `test-app.R`, which sources `app.R` exactly as the app starts, and that builds the cache if it's missing. The app tests check wiring, not data correctness.

**Expected change-detectors.** The figures pinned in `test-data-prep.R` ("expected game & row counts") and `test-reconciliation.R` (79 games, the diff_pct band, up to 8 phantom-zero games) describe the current scrape. They'll fail after a data refresh. That's intended: check that the new figures are explained by the new data, then update them.

**Monte Carlo tolerances** are derived from sampling theory (standard errors), not hand-tuned, and every simulation uses a fixed seed. A failure is therefore a real bias, not bad luck. The header of each file states its tolerance reasoning.

No CI workflow runs this suite.

Severity describes what goes wrong for a user if the guarded behaviour breaks. The app tells people how much they're likely to lose gambling, so wrong numbers are the worst failure:

- **Critical**: the app doesn't run.
- **High**: a number shown to the user is wrong.
- **Medium**: a feature visibly misbehaves.
- **Low**: wording or a message.

## `test-raw-data.R`: the scraped CSVs, before the pipeline sees them

The CSVs come from scraping the National Lottery site, so the realistic failure is a site or scraper change.

| Test | Purpose | Severity |
|---|---|---|
| every raw CSV has the columns the pipeline reads | A renamed column is reported by file and name. Otherwise it surfaces as an obscure dplyr error inside a join. | High |
| prices are positive pounds, and the scratchcard price cross-check columns agree in units | Prices captured in pence instead of pounds would run without error and make every RTP 100× too small. | High |
| prize tiers carry numeric, non-negative prizes and probabilities in (0, 1] | Odds parsed as text, or blank prizes, are caught before they become NA tiers. | High |
| each tier's implied probability is the reciprocal of its quoted odds | The two scraped representations of the same odds agree; a mismatch means one was mis-parsed. | High |
| join keys are present and parseable | Scratchcard numbers, instant-win slugs and the on-sale dates can be joined and parsed, so no game silently drops out. | High |

## `test-data-prep.R`: the prepared catalogue

| Test | Purpose | Severity |
|---|---|---|
| every game's outcome distribution is well-formed | For every real game: probabilities present, non-negative, summing to 1; net values finite; −price is the minimum and appears exactly once (any £0-gross tiers fold into it). Everything downstream assumes this. | High |
| net_value = gross_prize - price for every raw tier | | High |
| no NA prices, net_values, or probabilities after joins | No game loses its price in a join. | High |
| hand-checked RTP for prize-ball (instant win, £0.25) matches | An instant-win RTP recomputed independently from the raw CSVs. | High |
| hand-checked net_ev, sd_play, and moments for scratchcard 1512 | The same for a scratchcard, including the SD that drives every simulation. | High |
| rtp_flag thresholds map correctly (none / soft / hard) | Including the exact boundaries (0.85 and 1.0 are not flagged). | Medium |
| losing-row probability equals 1 - sum(winning tier probabilities) | The implicit "lose" outcome gets exactly the leftover probability, accounting for £0-gross tiers. | High |
| cross-source game_id is unique and prefixed by source | Scratchcard and instant-win ids can't collide. | High |
| rtp_capped never exceeds raw rtp and equals rtp for instant win | | Medium |
| expected game & row counts reconcile with the raw data snapshot | Change detector (see above): 44 scratchcards, 79 instant-win games, and the expected diagnostics. | Medium |

## `test-reconciliation.R`: the pipeline against an independent published figure

This is the only check of the prepared data against a source of truth outside the code.

| Test | Purpose | Severity |
|---|---|---|
| reconciliation CSV joins cleanly onto our instant-win games | No game dropped or invented. | High |
| recomputed implied overall odds (any tier fired) match the CSV to ~machine precision | The pipeline stored every game's odds verbatim. | High |
| net-money-gain implied odds match the CSV for all non-artifact games | Only the data-derived "phantom-zero" games diverge. | High |
| the phantom-zero offset is fully explained by gross_prize==0 tier mass | The known scraping artefact accounts for all of the divergence, so it can't hide a real regression. | High |
| implied-vs-published agreement (diff_pct) sits in a documented band | Scraped tier odds agree with the published overall odds within a stated tolerance, allowing a counted set of outliers. | Medium |

## `test-validation.R`: the engine against every real game

| Test | Purpose | Severity |
|---|---|---|
| every real game simulates without bias, with ordered quantiles | For all ~123 games, the simulated mean is within 5 Monte Carlo standard errors of the exact N × EV, and the fan-chart quantiles are ordered. This is the headline proof that what users see is unbiased. | High |

## `test-simulate.R`: the Monte Carlo engine

| Test | Purpose | Severity |
|---|---|---|
| analytical_summary matches hand-computed moments | The exact mean, SD and interval every simulation is judged against. | High |
| simulated mean(totals) matches analytical mean within 4x MC SE | No bias. | High |
| var(totals) matches N * per-play var within sampling error | No variance error (the spread users see). | High |
| same seed => identical totals (default path reproducibility) | | Medium |
| chunking is invariant to batch size (same seed => same result) | Memory chunking can't change the answer. | High |
| CRN scalar-seed path reproduces the same uniforms deterministically | | Medium |
| CRN: matrix and session-contiguous vector agree; both reproduce | | Medium |
| CRN monotone shift: +£1 on every value => totals shift by exactly N | The property that makes strategy comparisons fair. | High |
| simulate_one_session: CRN seed matches the row of a CRN session run | The play-by-play view matches the session it claims to be. | Medium |
| quantiles are ordered and checkpoints equal session totals | | High |
| checkpoint_plays are increasing, unique, start >=1 and end at N | | Medium |
| edge case N=1 works and equals a single per-play draw | | Medium |
| edge case R=1 works (single session, full-length totals vector 1) | | Medium |
| degenerate single-value distribution is exact and error-free | | Medium |
| extreme jackpot tail (tiny prob) simulates without error | Mirrors the real jackpot games. | High |
| defensive renormalisation warns and rescales; bad probs error | | Medium |
| game_distribution extracts one game's (value, prob) generically | | Medium |
| a zero-probability outcome is carried but never drawn | | High |

## `test-metrics.R`: risk and engagement figures shown to the user

| Test | Purpose | Severity |
|---|---|---|
| risk_metrics probabilities match hand-counts | P(profit), P(break even), P(lose more than X). | High |
| VaR is the signed alpha-quantile with a positive loss magnitude | | High |
| expected_shortfall is the exact lower-alpha tail mean (Acerbi) | Including the fractional-observation case. | High |
| ES is at least as severe as VaR (coherence on the tail) | | High |
| dream_vs_reality removes exactly the top-tier mass and renormalises | | High |
| dream_vs_reality obeys the law of total expectation | | High |
| dream_vs_reality guards the all-top degenerate case | | Medium |
| engagement_metrics computes exact analytical win rates | | High |
| engagement_metrics honours an explicit price override | | Medium |
| longest_losing_streak: degenerate all-loss dist => streak == N | | Medium |
| longest_losing_streak: no possible loss => streak == 0 | | Medium |
| longest_losing_streak: streaks lie in [0, N], reproducible, ~sane mean | | Medium |
| quantile_se matches the theoretical median SE for a Normal sample | The uncertainty shown next to percentiles is correct. | Medium |
| quantile_se returns a tidy frame with type-7 point estimates | | Medium |
| leaderboard_series mean_pnl is exact analytical N * ev | | High |
| leaderboard_series p_profit is a valid, reproducible probability curve | | Medium |
| leaderboard_series accepts a custom metric function | | Low |
| build_metrics assembles a coherent bundle over one sim/dist | Every component agrees with its standalone function. | High |
| build_metrics rejects a non-sim input | | Low |

## `test-strategies.R`: which games a strategy plays, and at what cost

| Test | Purpose | Severity |
|---|---|---|
| is_purchasable excludes hidden, prohibited, and off-sale games | A user can't be shown a game they can't buy. | High |
| filter_games composes filters and defaults to purchasable-only | | High |
| filter_games errors on an empty universe | | Medium |
| make_strategy normalises weights and records aligned prices | | High |
| make_strategy and random_each_play default to uniform weights | | Medium |
| make_strategy drops zero-weight games but keeps positives | | Medium |
| make_strategy guards bad weights and unknown/duplicate ids | | Medium |
| mixture probs sum to 1 and EV = sum_g w_g EV_g | | High |
| aggregation merges shared net_values across games | | High |
| simulated mixture matches explicit multinomial allocation (equivalence) | Validates the modelling shortcut behind every mixed strategy against a brute-force simulation. | High |
| extreme presets pick the right game over the purchasable universe | Cheapest, priciest, best/worst RTP, biggest jackpot. | High |
| extreme presets split ties with equal weight (documented rule) | | Medium |
| strategy_single is a weight-1 single-game strategy | | Medium |
| expected spend is N * sum_g w_g price_g; single game reduces to N*price | | High |
| price distribution aggregates weight by ticket price | | Medium |
| budget_to_N uses the price for a single game | Budget → number of plays. | High |
| budget_to_N uses E[price] for a mixed-price strategy | Never overspends the budget. | High |
| budget_to_N guards non-positive budget / periods | | Low |
| a mixture strategy feeds build_metrics via the E[price] override | | Medium |

## `test-compare.R`: side-by-side strategy comparison

| Test | Purpose | Severity |
|---|---|---|
| self-comparison under shared CRN gives byte-identical totals | A strategy compared with itself shows zero difference. | High |
| shared CRN strictly reduces Var(totals_A - totals_B) vs independent seeds | The comparison is fair and less noisy than independent runs. | High |
| comparison table matches standalone build_metrics under the same CRN | | High |
| totals_long is tidy and complete (one row per session per strategy) | | Medium |
| run_compare errors on fewer than 2 strategies | | Low |
| run_compare handles 3+ strategies | | Medium |
| run_compare defaults a NULL seed so a comparison is always CRN-fair | | High |
| compare viz builders return ggplot objects | Smoke test; the alt text names every strategy. | Medium |
| compare fan chart draws one median line per strategy | | Medium |
| run_compare_leaderboard assembles a shared-seed crossover series | | Medium |
| compare_table_display formats every strategy row for the UI | | Low |

## `test-cache.R`: the repeat-run cache

A wrong cache hit would show one game's results for another, without any visible sign.

| Test | Purpose | Severity |
|---|---|---|
| cache hit returns a result identical to the uncached computation | | High |
| a genuine cache HIT is served without recomputation (unseeded path) | Uses a control to prove that caching, not determinism, explains the identity. | High |
| different inputs are NOT false cache hits | Seed, N and distribution are all part of the key; NULL ≠ 0. | High |
| a row-permuted distribution is a correct hit (engine sorts by value) | | Medium |
| leaderboard_series_cached round-trips and does not false-hit | | High |

## `test-narrative.R`: the plain-English summary

Exact wording is pinned, so copy edits will fail these tests. Update the expected text when you change the wording. What matters is that the sign and size of each figure in the sentence are right: "£50 ahead" for a £50 loss would be a serious misstatement.

| Test | Purpose | Severity |
|---|---|---|
| pluralise: singular vs plural, including zero and comma grouping | | Low |
| fmt_gbp: whole pounds by default, pence for sub-£1, forced modes, comma grouping | | Medium |
| pnl_phrase: consistent sign convention -- 'lost' for losses, 'ahead' for profit | | High |
| fmt_pct rounds to the requested precision | | Low |
| describe_chance: honest wording at the extremes, numeric in the middle | | Medium |
| jackpot_odds: W = round(1/p) exactly, NA guard on p <= 0 / NA (no divide-by-zero) | | Medium |
| normal losing game: typical loss, interval, profit chance, jackpot framing | Every figure in the main narrative. | High |
| essentially-never-profit game reads 'essentially never', not an awkward 0% | | Medium |
| N = 1 is singular: '1 play', not '1 plays' | | Low |
| degenerate interval (lo == hi) reads plainly, not 'between £X and £X' | | Low |
| p_top = 0 or NA omits the jackpot clause gracefully, no divide-by-zero | No `Inf`, `NaN` or `NA` in user-facing text. | Medium |
| honest framing: never uses encouraging/overselling language on a losing game | Catches only the listed phrases; a reminder, not a guarantee. | Medium |
| mean-divergence caveat appears only when mean and median meaningfully diverge | | Medium |
| narrate_run prefers exact sim$totals over the percentile_se grid when supplied | | Medium |
| strategy label, when supplied, is prepended as its own sentence | | Low |
| engagement/streak flavour sentence is present, tight, and pluralised correctly | | Low |
| narrate_run errors clearly on a bundle missing $meta rather than failing obscurely | | Low |

## `test-viz.R`: charts and their alt text

The tests check plotted *values* (marker positions, bar heights, line values, point colours), not just that a plot exists.

| Test | Purpose | Severity |
|---|---|---|
| transform 'none' is the identity | | Low |
| transform 'signed_log' is strictly monotone and sign-preserving | The display transform never reorders or flips outcomes. | High |
| transform 'signed_log' round-trips through viz_untransform_pnl | Axis labels show real £ values. | High |
| transform 'winsorize' clips to bounds and is monotone (non-decreasing) | | Medium |
| winsorize honours explicit winsorize_bounds | Markers clip to the same bounds as the data. | Medium |
| viz_untransform_pnl on 'winsorize' warns and returns input unchanged | | Low |
| viz_fan_chart returns a ggplot with a ribbon layer where ymin <= ymax everywhere | | Medium |
| viz_fan_chart nests bands outer-contains-inner (5-95 outer, 25-75 inner) | | Medium |
| viz_fan_chart omits the bare median line for an EVEN count of probs | | Low |
| viz_fan_chart with a single median prob draws a median line and no ribbon | | Low |
| viz_fan_chart handles N = 1 (a single checkpoint) without error | | Medium |
| viz_fan_chart_alt returns a single non-empty description string | Smoke test only. | Low |
| viz_pnl_hist marker vlines sit exactly at 0, mean, median, and the requested percentiles | | High |
| viz_pnl_hist under 'signed_log' places transformed markers exactly at f(raw value) | | High |
| viz_pnl_hist handles a degenerate single-value distribution | | Medium |
| viz_pnl_hist handles an all-loss distribution | | Medium |
| viz_pnl_hist_alt reports the RAW (untransformed) mean/median regardless of display transform | | Medium |
| viz_dream_vs_reality bars equal metrics$dream's full/conditional mean_session | | High |
| viz_dream_vs_reality handles the degenerate all-mass-on-top-tier case | | Medium |
| viz_dream_vs_reality_alt returns a non-empty description string | Smoke test only. | Low |
| viz_leaderboard_vs_n draws exactly the analytical mean_pnl values per strategy | | High |
| viz_leaderboard_table pivots to one row per strategy, one column per N | | Low |
| viz_leaderboard_vs_n_alt names the leading strategy at min and max N | On a hand-built series with known leaders. | Medium |
| viz_play_by_play's cumulative path matches cumsum() of the session exactly | | High |
| viz_play_by_play colours each play as a win or loss by price | Every point's colour is checked through the plot's own scale, so swapped colours fail. | Medium |
| viz_play_by_play handles an all-loss single session | | Medium |
| viz_play_by_play_alt reports the correct win count and final balance | | Medium |

## `test-app.R`: the Shiny server

| Test | Purpose | Severity |
|---|---|---|
| build_strategy: presets construct valid strategy objects over the filtered universe | | High |
| build_strategy: 'single' with no game selected errors clearly | | Low |
| build_strategy: 'custom' with no games selected errors clearly | | Low |
| run_pipeline: end-to-end produces a metrics bundle with the expected shape | | Critical |
| validate_run_bounds: flags an over-cap N x R and passes an in-bounds run | | Medium |
| testServer: a preset run populates the pipeline reactive with sane metrics | The real server, driven through its inputs. | Critical |
| testServer: a custom-mix run with explicit weights populates the pipeline reactive | | High |
| testServer: an empty filter universe is a handled state, not a crash | Raises Shiny's `validation` condition with a user-facing message, not an arbitrary error. | Medium |
| testServer: a custom mix with no games selected is a handled state, not a crash | As above. | Medium |
| testServer: N x R over the interactive cap is a handled state, not a crash | As above. | Medium |
