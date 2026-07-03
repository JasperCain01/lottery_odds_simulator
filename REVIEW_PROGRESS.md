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
| R3 Display review: viz, narrative, app_helpers, app.R (+ render charts to PNG, launch app headless) | ✅ | — | 11 PNGs rendered & visually inspected (jackpot game N=52 & N=2000, all transforms, compare mode); narrative + all alt texts printed & read; `shiny::shinyAppFile("app.R")` constructs. Findings F1–F8 below. |
| R4 REVIEW.md written (findings 1,2,4 + WebR feasibility for 3) | ✅ | — | Done AFTER R5 so it records the fixes. Includes benchmark numbers (max-cap run 0.45s native) grounding the WebR verdict. |
| R5 Fixes applied for confirmed math/display bugs, suite green | ✅ | 1bdd410 (viz/narrative F1,F2,F4-F8), d81ee94 (app budget F3), + install.R stringr fix | Full suite green after each commit; fixed charts re-rendered and inspected. |
| R6 Final: push, summary to user | ✅ | — | All findings ✅ fixed except data-staleness note (📝 documented, REVIEW.md §4). Review COMPLETE — nothing pending on resume. |

## WebR conversion stage ledger (added 2026-07-03, user-approved follow-on)
User approved: do the WebR conversion, then merge this branch into main.
| Stage | Status | Notes |
|---|---|---|
| W0 This ledger extension committed | ✅ | |
| W1 App tweaks: wasm-aware cache ceiling, data-snapshot note in sidebar | ✅ | commit 3934ab4 |
| W2 tools/export_webr.R export script (staging dir → shinylive::export → _site) | ✅ | commit 3934ab4 |
| W3 .github/workflows/deploy-pages.yml (tests → data cache → export → Pages) | ✅ | commit 3934ab4; temporarily triggers on this branch |
| W4 Local verification: suite green, export mechanics (proxy blocks repo.r-wasm.org AND CRAN locally, so the export itself is CI-verified) | ✅ | suite green; app note shown/omitted correctly; YAML valid; export guard fires |
| W5 CI verification on this branch | ✅ | Saga: run 1 failed at configure-pages (repo was private); user made repo public; run 2 failed the same way (GITHUB_TOKEN may not CREATE a Pages site); pushed a placeholder `gh-pages` branch which auto-enabled Pages (classic branch mode); rerun then passed the ENTIRE build job (deps, data, suite, export 99.4MB, configure-pages, artifact) and failed only the `deploy` job in 2s — the github-pages environment's default protection rule allows deployments from main only. That is resolved by the merge itself. |
| W6 Flip workflow trigger to main-only, merge branch → main, verify main run + live URL | 🔄 | Merged (7f18d67). Artifact-route deploys from main failed twice in the Pages backend ("Deployment failed, try again later" during syncing_files) — the site was created in BRANCH mode by the placeholder and artifact deploys conflict with it. Switched the workflow to classic branch publishing: force-push _site/ to gh-pages (single-commit history, .nojekyll). gh-pages is now the LIVE branch — do NOT delete it. Awaiting first green publish. |

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
- F4 (display, CONFIRMED): negative amounts in alt texts render "£-165"
  and even "£ -12.00" (width-padded vector format) — normalise to "-£165"
  via a shared helper in viz.R.
- F5 (display): viz_dream_vs_reality caption prints "hits with probability
  1.73e-05% per play" — raw scientific notation in user-facing text.
  Use 1-in-W framing / non-scientific formatting. Same in its alt text.
- F6 (display, misleading): signed-log histogram/density x-axis ticks show
  transform units (-3, 0, 3, 6) that users will read as £. Back-transform
  the tick labels to real £ at nice positions.
- F7 (display): leaderboard/crossover chart uses a linear N axis; with the
  grid 10..2000 all the low-N crossover action (the chart's entire story)
  is squashed into the left margin. Use a log10 N axis.
- F8 (display, inconsistent): on the Compare tab the crossover chart uses
  ggplot default hue colours and alphabetical strategy order, while the two
  charts above it use the Okabe-Ito .compare_palette in supplied order —
  the SAME strategy gets a different colour between adjacent charts. Align
  palette + factor ordering.

## Resume protocol
1. Read this file + `git log --oneline` on this branch.
2. The first ⬜/🔄 stage is where to resume. Findings marked neither ✅ nor 📝
   still need action.
3. Environment is ephemeral: re-run the Environment steps above if `Rscript`
   is missing; this costs ~3 min and is expected.
