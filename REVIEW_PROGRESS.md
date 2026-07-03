# REVIEW_PROGRESS — fable_shiny-webr-review-7ooa9h

Living ledger for the project review requested 2026-07-03. Any future session
resumes from THIS FILE + `git log --oneline` on branch
`fable_shiny-webr-review-7ooa9h` — do not repeat completed stages.

## Scope of the review (user request, verbatim intent)
1. Mathematical errors that need to be addressed.
2. Display errors/problems that need to be fixed.
3. Feasibility of converting the Shiny app to run on WebR (host via GitHub Pages).
4. Anything else relevant.

Rules: work on this `fable_` branch; commit regularly; nothing needs user
approval except the final merge to main. Findings that need fixing get fixed
on this branch; the write-up lives in `REVIEW.md` (created at stage R4).

## Environment (fresh container)
Same as HANDOVER.md: apt-install R + CRAN binaries, then
`Rscript -e 'source("R/data_prep.R"); run_data_prep(".")'` to build
`data/*.rds`, then run the suite:
`Rscript -e 'library(testthat); testthat::test_dir("tests/testthat", stop_on_failure=TRUE, reporter="summary")'`
Use `LANG=C.UTF-8 LC_ALL=C.UTF-8` for anything rendering `£`.

## Stage ledger
| Stage | Status | Commit | Notes |
|---|---|---|---|
| R0 Setup: branch + this ledger | ✅ | (first commit on branch) | |
| R1 Environment: R installed, data cache built, baseline suite run | ✅ | — | Suite 100% green, 0 failures, 13 files. NOTE: two apt PPAs (deadsnakes, ondrej/php) are proxy-blocked in this container — `sudo rm /etc/apt/sources.list.d/*deadsnakes* *ondrej*` before `apt-get update`. Added r-cran-stringr to the install list (data_prep.R needs it; HANDOVER's list relied on it arriving transitively). |
| R2 Math review: data_prep, simulate, metrics, strategies, compare | ✅ | — | All 8 R/ files + app.R read line-by-line. Independent spot-checks (scratchpad script, not committed): RTP/EV/SD re-derived from raw CSVs match to 1e-10; ES matches hand calc; MJ SE shrinks ~1/sqrt(n); analytical vs simulated mean z=0.51; all 123 games sum-to-1 and losing-row identities hold. NO substantive math errors found. See findings log. |
| R3 Display review: viz, narrative, app_helpers, app.R (+ render charts to PNG, launch app headless) | ⬜ | | |
| R4 REVIEW.md written (findings 1,2,4 + WebR feasibility for 3) | ⬜ | | |
| R5 Fixes applied for confirmed math/display bugs, suite green | ⬜ | | one commit per logical fix |
| R6 Final: push, summary to user | ⬜ | | |

## Findings log (append as discovered; ✅ = fixed, 📝 = documented only)
- F1 (display, minor): `viz_fan_chart_alt()` alt text reads "the 90% of
  outcomes fall between" — missing word ("the central 90%"). R/viz.R.
- F2 (display/wording): narrative `.sentence_jackpot()` and
  `viz_dream_vs_reality()` caption label `dream$top_value` (a NET figure,
  prize − price) as "the top prize (£X)" — shows e.g. £999,995 for an
  advertised £1,000,000 jackpot. Not a math error (net convention is
  deliberate and consistent) but reads as a typo to users. Fix: label as
  net or add price back for display.
- F3 (display, minor): budget note in app.R says "≈ 1 plays" (no
  pluralisation) and forces `max(1L, bt$N)` even when the budget cannot
  afford a single ticket — overstates what £X buys. app.R.
- F4 (verify in R3): negative amounts in alt texts render "£-99.5"
  (format(round(x,2)) after a "£" prefix); check and normalise.

## Resume protocol
1. Read this file + `git log --oneline` on this branch.
2. The first ⬜/🔄 stage is where to resume. Findings marked neither ✅ nor 📝
   still need action.
3. Environment is ephemeral: re-run the Environment steps above if `Rscript`
   is missing; this costs ~3 min and is expected.
