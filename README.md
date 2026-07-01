# Lottery Odds Simulator

An R Shiny dashboard that simulates National Lottery scratchcard and
instant-win playing strategies, showing how much money a player is likely to
lose (or, rarely, win) over a chosen number of plays. It contrasts two
regimes: over many plays the average outcome (RTP) dominates and losses
become near-deterministic, while over few plays variance and jackpot tails
dominate, so confidence intervals -- not just averages -- are what matter for
an honest picture of realistic outcomes. See `IMPLEMENTATION_PLAN.md` for the
full build blueprint (architecture, phased plan, and modelling decisions).

Status: scaffolding only (Phase 0) -- no simulation logic is implemented yet.

## Dependencies

- shiny
- bslib
- dplyr
- readr
- tidyr
- ggplot2
- scales
- testthat
- withr

Dependency management is intentionally lightweight for now: `install.R`
installs only whatever is missing from your existing R library (no `renv`).
`renv` is deferred to the deployment phase (Phase 11) to avoid disrupting the
global library / network access while the project scaffolding and early
phases are still moving quickly.

## Install

```sh
Rscript install.R
```

## Run

```sh
R -e 'shiny::runApp()'
```
