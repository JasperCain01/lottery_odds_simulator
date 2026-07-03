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
| R0 Setup: branch + this ledger | 🔄 | | |
| R1 Environment: R installed, data cache built, baseline suite run | ⬜ | | record baseline pass/fail counts here |
| R2 Math review: data_prep, simulate, metrics, strategies, compare | ⬜ | | findings logged below as found |
| R3 Display review: viz, narrative, app_helpers, app.R (+ render charts to PNG, launch app headless) | ⬜ | | |
| R4 REVIEW.md written (findings 1,2,4 + WebR feasibility for 3) | ⬜ | | |
| R5 Fixes applied for confirmed math/display bugs, suite green | ⬜ | | one commit per logical fix |
| R6 Final: push, summary to user | ⬜ | | |

## Findings log (append as discovered; ✅ = fixed, 📝 = documented only)
(none yet)

## Resume protocol
1. Read this file + `git log --oneline` on this branch.
2. The first ⬜/🔄 stage is where to resume. Findings marked neither ✅ nor 📝
   still need action.
3. Environment is ephemeral: re-run the Environment steps above if `Rscript`
   is missing; this costs ~3 min and is expected.
