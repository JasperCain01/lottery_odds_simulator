# ==============================================================================
# simulate.R
# ------------------------------------------------------------------------------
# Layer 2 of the lottery odds simulator (see IMPLEMENTATION_PLAN.md, sections
# 4 and Phase 2). THE NUMERICAL CORE.
#
# Two complementary engines over a *generic categorical distribution* of
# per-play net outcomes -- a data.frame/tibble of (value, prob) with
# sum(prob) == 1. The engine is deliberately decoupled from game_ids: the
# strategy layer (Phase 4) will build mixture distributions and feed them in
# through exactly the same interface. `game_distribution()` is the only bridge
# back to the outcomes table.
#
#   - analytical_summary(dist, N, conf)  : closed-form mean (N * net EV),
#         variance (N * per-play Var), and a Normal/CLT confidence interval.
#         Instant; used for the leaderboard and as a reference overlay. The CLT
#         CI is UNRELIABLE at small N for the skewed jackpot games -- that is
#         expected and is the app's central teaching point, not a bug.
#
#   - simulate_sessions(dist, N, R, ...) : the workhorse Monte Carlo engine.
#         R independent sessions of N i.i.d. plays each; returns the full length
#         -R vector of session P&Ls plus checkpointed cumulative quantiles for
#         the fan chart. Memory-chunked so the full R x N matrix is never
#         materialised. Captures skew the analytical CI cannot.
#
#   - simulate_one_session(dist, N, ...) : the full per-play net_value vector
#         for ONE session (Phase 6 play-by-play, streak metrics). Cheap.
#
# Also owns: Common Random Numbers (CRN) via inverse-CDF sampling, so different
# distributions can be evaluated on the SAME uniform stream for fair
# comparisons (Phase 8), and fan-chart quantile checkpointing.
#
# source()-ing this file is side-effect free: it defines functions only.
# ==============================================================================

# ------------------------------------------------------------------------------
# .canonical_dist(dist)
# ------------------------------------------------------------------------------
# Internal: validate and normalise any (value, prob) input into the canonical
# form the engine works on -- a list(value, prob, cdf) with values sorted
# ascending and prob summing to exactly 1.
#
# WHY sort by value: the CRN inverse-CDF map indexes outcomes by their position
# in cumsum(prob). Sorting by value gives a canonical, distribution-independent
# ordering so that (a) the same uniform maps to the same "rank" outcome across
# any two distributions, and (b) the monotone-shift property holds -- shifting
# every value by a constant leaves the ordering (and hence the CRN mapping)
# untouched, so totals shift by exactly N*constant.
#
# WHY carry cdf = cumsum(prob): computed once here and reused by every CRN
# batch, avoiding a recompute per chunk.
.canonical_dist <- function(dist) {
  # Accept a data.frame/tibble with (value, prob) or a bare named list; pull the
  # two columns out generically so the strategy layer isn't forced into a schema.
  if (is.data.frame(dist)) {
    if (!all(c("value", "prob") %in% names(dist))) {
      stop("dist must have columns 'value' and 'prob'.", call. = FALSE)
    }
    value <- dist$value
    prob  <- dist$prob
  } else if (is.list(dist) && all(c("value", "prob") %in% names(dist))) {
    value <- dist$value
    prob  <- dist$prob
  } else {
    stop("dist must be a data.frame/list with 'value' and 'prob'.", call. = FALSE)
  }

  # Basic structural validation.
  if (length(value) == 0L) stop("dist has no outcomes.", call. = FALSE)
  if (length(value) != length(prob)) {
    stop("'value' and 'prob' must have equal length.", call. = FALSE)
  }
  if (any(is.na(value)) || any(is.na(prob))) {
    stop("dist contains NA in 'value' or 'prob'.", call. = FALSE)
  }
  if (any(prob < 0)) stop("dist has negative probabilities.", call. = FALSE)
  s <- sum(prob)
  if (s <= 0) stop("dist probabilities sum to zero (or less).", call. = FALSE)

  # Defensive renormalisation. The Phase 1 contract guarantees sum(prob)==1, but
  # mixture distributions built downstream (Phase 4) may accumulate tiny
  # floating-point drift. Renormalise silently within fp tolerance; warn only if
  # the drift is large enough to signal a real upstream bug.
  if (abs(s - 1) > 1e-9) {
    warning(sprintf(
      "dist probabilities sum to %.12g, not 1; renormalising.", s),
      call. = FALSE)
  }
  prob <- prob / s

  # Sort ascending by value for a canonical CRN ordering (see WHY above).
  ord   <- order(value)
  value <- value[ord]
  prob  <- prob[ord]

  # cdf = cumsum(prob); force the final entry to exactly 1 so findInterval never
  # leaves a uniform very close to 1 unassigned due to rounding.
  cdf <- cumsum(prob)
  cdf[length(cdf)] <- 1

  list(value = value, prob = prob, cdf = cdf)
}


# ------------------------------------------------------------------------------
# .dist_moments(cd)
# ------------------------------------------------------------------------------
# Internal: per-play mean and variance of a canonical distribution.
# var = E[X^2] - E[X]^2, clamped at 0 to defend against tiny negative values
# from catastrophic cancellation on near-degenerate distributions.
.dist_moments <- function(cd) {
  ev  <- sum(cd$value * cd$prob)
  ex2 <- sum(cd$value^2 * cd$prob)
  v   <- ex2 - ev^2
  if (v < 0) v <- 0            # cancellation guard; a true variance is >= 0
  list(ev = ev, var = v, sd = sqrt(v))
}


# ------------------------------------------------------------------------------
# game_distribution(game_id, outcomes)
# ------------------------------------------------------------------------------
# The single bridge from the Phase 1 outcomes table to the generic engine.
# Extracts one game's (value, prob) as a two-column tibble/data.frame that any
# engine function accepts. Everything else in this file is game-agnostic.
game_distribution <- function(game_id, outcomes) {
  if (!all(c("game_id", "net_value", "probability") %in% names(outcomes))) {
    stop("outcomes must have columns game_id, net_value, probability.",
         call. = FALSE)
  }
  rows <- outcomes[outcomes$game_id == game_id, , drop = FALSE]
  if (nrow(rows) == 0L) {
    stop(sprintf("game_id '%s' not found in outcomes.", game_id), call. = FALSE)
  }
  # Return the generic (value, prob) contract; drop everything game-specific.
  data.frame(value = rows$net_value, prob = rows$probability)
}


# ------------------------------------------------------------------------------
# analytical_summary(dist, N, conf = 0.90)
# ------------------------------------------------------------------------------
# Closed-form summary of session P&L (= sum of N i.i.d. per-play draws) under
# the CLT. Instant; used for the leaderboard and as the reference overlay on the
# Monte Carlo fan chart.
#
#   mean   = N * E[X]
#   var    = N * Var[X]                (i.i.d. plays -> variances add)
#   CI     = mean +/- z * sqrt(var),   z = qnorm(1 - (1-conf)/2)
#
# CAVEAT (by design, not a defect): the Normal CI is a poor approximation at
# small N for the heavily right-skewed jackpot games -- the true session
# distribution is far from Gaussian until N is large. simulate_sessions() gives
# the honest empirical picture; this exists as the fast reference and to make
# the mean/variance crossover story explicit.
analytical_summary <- function(dist, N, conf = 0.90) {
  if (N < 1 || N != as.integer(N)) stop("N must be a positive integer.", call. = FALSE)
  if (conf <= 0 || conf >= 1) stop("conf must be in (0, 1).", call. = FALSE)

  cd <- .canonical_dist(dist)
  m  <- .dist_moments(cd)

  # Session moments: plays are i.i.d., so means and variances both scale by N.
  mean_total <- N * m$ev
  var_total  <- N * m$var
  sd_total   <- sqrt(var_total)

  # Two-sided Normal CI at the requested confidence level.
  z     <- stats::qnorm(1 - (1 - conf) / 2)
  ci_lo <- mean_total - z * sd_total
  ci_hi <- mean_total + z * sd_total

  list(
    N          = as.integer(N),
    conf       = conf,
    mean       = mean_total,   # expected session P&L
    sd         = sd_total,     # session standard deviation
    ci_lo      = ci_lo,
    ci_hi      = ci_hi,
    ev_play    = m$ev,         # per-play net EV (reference for metrics/leaderboard)
    sd_play    = m$sd,         # per-play SD
    var_play   = m$var,
    method     = "analytical (CLT Normal approximation)"
  )
}


# ------------------------------------------------------------------------------
# .checkpoint_indices(N, checkpoints)
# ------------------------------------------------------------------------------
# Internal: choose ~`checkpoints` play-indices along 1..N at which to snapshot
# cumulative P&L for the fan chart. Always includes N as the final checkpoint,
# which GUARANTEES the last checkpoint's cumulative value equals the session
# total (a contract the tests assert).
#
# WHY not every play: storing every intermediate cumulative value costs R x N
# memory and the fan chart only needs ~100 points to look smooth. Trade-off:
# a coarse time axis (fine for visualisation) for a large memory saving.
.checkpoint_indices <- function(N, checkpoints) {
  if (checkpoints >= N) {
    # Fewer plays than requested checkpoints: snapshot every play.
    return(seq_len(N))
  }
  # Evenly spaced indices on [1, N], de-duplicated, with N forced in.
  idx <- unique(round(seq(1, N, length.out = checkpoints)))
  idx[length(idx)] <- N          # ensure the final point is exactly N
  as.integer(idx)
}


# ------------------------------------------------------------------------------
# .draw_batch(cd, n_rows, n_cols, u)
# ------------------------------------------------------------------------------
# Internal: map a supplied Unif(0,1) vector `u` (length n_rows*n_cols,
# column-major) into an (n_rows x n_cols) matrix of per-play net_values via the
# INVERSE-CDF of the canonical distribution `cd`.
#
# findInterval(u, cdf[-last]) + 1 returns the index of the outcome whose
# cumulative-probability interval contains u. Interior breakpoints only (drop
# the final 1) so findInterval returns 0..(k-1); +1 shifts to the 1..k value
# index. u == 1 (exactly) lands in the top bin because the last cdf entry was
# forced to 1 and dropped.
#
# WHY a SINGLE inverse-CDF path (not sample.int for the default and inverse-CDF
# for CRN): unifying on inverse-CDF makes every code path reproducible AND
# chunk-invariant -- the draws are a deterministic function of the uniform
# stream, so slicing that stream into batches cannot change the result. It also
# means the default seeded path and a CRN run driven by the same uniforms are
# byte-identical, which is exactly the fair-compare guarantee Phase 8 needs.
# Because values are sorted canonically, feeding the SAME uniforms to two
# different distributions yields rank-matched (correlated) outcomes.
#
# Trade-off (documented): sample.int() with an alias table is O(1) per draw vs
# findInterval's O(log k); on these distributions (few distinct NET values -- the
# jackpot prizes collapse to a handful of net outcomes) the two are within ~20%,
# a price well worth the exact reproducibility and CRN unification. Phase 9 may
# revisit if a distribution with thousands of distinct net values appears.
#
# LAYOUT CONTRACT: `u` is laid out SESSION-CONTIGUOUS (row-major) -- the first
# n_cols entries are session 1's plays, the next n_cols session 2's, etc. The
# returned matrix is (n_rows x n_cols) filled byrow = TRUE, so row i is session
# i. WHY session-contiguous: it makes the mapping chunk-invariant -- session i
# always consumes the SAME contiguous N uniforms of the stream regardless of how
# the run is batched -- and lets simulate_one_session() reproduce any row of a
# sessions run by drawing that session's N uniforms alone.
.draw_batch <- function(cd, n_rows, n_cols, u) {
  breaks <- cd$cdf[-length(cd$cdf)]
  idx    <- findInterval(u, breaks) + 1L
  matrix(cd$value[idx], nrow = n_rows, ncol = n_cols, byrow = TRUE)
}


# ------------------------------------------------------------------------------
# .resolve_crn(crn, R, N)
# ------------------------------------------------------------------------------
# Internal: turn the user-facing `crn` argument into a concrete R x N uniform
# matrix (or NULL for the default path). Row i of the returned matrix is session
# i's N uniforms (SESSION-CONTIGUOUS layout; see .draw_batch's LAYOUT CONTRACT).
# Accepts:
#   - NULL   : no CRN; caller draws uniforms from the seeded stream instead.
#   - a numeric matrix (R x N): row i = session i's uniforms. Used as-is.
#   - a numeric vector (length R*N): interpreted SESSION-CONTIGUOUS (first N are
#     session 1, etc.) and reshaped byrow. This matches how a plain runif(R*N)
#     stream lays out, so a shared seed and a shared vector agree.
#   - a single integer/numeric scalar: treat as a SEED that deterministically
#     regenerates the same R*N uniforms via runif().
#
# WHY allow both a matrix and a seed: Phase 8 may either hold the shared uniforms
# in memory (matrix) or, to save memory across many strategies, pass a seed that
# each strategy expands identically. Both must yield byte-identical draws.
.resolve_crn <- function(crn, R, N) {
  if (is.null(crn)) return(NULL)

  need <- R * N
  if (length(crn) == 1L && is.numeric(crn) && is.null(dim(crn))) {
    # Scalar -> seed. Regenerate the shared uniform stream deterministically.
    # Use a local RNG scope so we don't disturb the caller's global seed state.
    old <- if (exists(".Random.seed", envir = .GlobalEnv))
             get(".Random.seed", envir = .GlobalEnv) else NULL
    set.seed(as.integer(crn))
    u <- stats::runif(need)
    if (!is.null(old)) assign(".Random.seed", old, envir = .GlobalEnv)
    # byrow: session-contiguous, so this agrees with a supplied vector/one-session.
    return(matrix(u, nrow = R, ncol = N, byrow = TRUE))
  }

  # A matrix is already in (session x play) shape; take it as given.
  if (is.matrix(crn)) {
    if (nrow(crn) != R || ncol(crn) != N) {
      stop(sprintf("crn matrix must be R x N = %d x %d; got %d x %d.",
                   R, N, nrow(crn), ncol(crn)), call. = FALSE)
    }
    u <- as.numeric(crn)
    if (any(u < 0) || any(u > 1)) stop("crn uniforms must lie in [0, 1].", call. = FALSE)
    return(crn)
  }

  # Otherwise a flat pool of uniforms in session-contiguous order.
  u <- as.numeric(crn)
  if (length(u) != need) {
    stop(sprintf("crn must supply exactly R*N = %d uniforms; got %d.",
                 need, length(u)), call. = FALSE)
  }
  if (any(u < 0) || any(u > 1)) stop("crn uniforms must lie in [0, 1].", call. = FALSE)
  matrix(u, nrow = R, ncol = N, byrow = TRUE)
}


# ------------------------------------------------------------------------------
# simulate_sessions(dist, N, R, seed, checkpoints, probs, crn)
# ------------------------------------------------------------------------------
# THE WORKHORSE. Simulates R independent sessions of N i.i.d. plays each and
# returns session P&Ls plus checkpointed cumulative quantiles for the fan chart.
#
# Arguments:
#   dist        (value, prob) distribution (generic; see game_distribution()).
#   N           plays per session (session P&L = sum of N draws).
#   R           number of simulated sessions (drives CI tightness).
#   seed        optional integer seed for reproducibility (default path only).
#   checkpoints target number of fan-chart time points along 1..N (~100).
#   probs       quantile probabilities for the fan chart ribbon.
#   crn         Common Random Numbers: NULL (default fast path), an R x N
#               uniform matrix/vector, or a scalar seed (see .resolve_crn()).
#
# Returns a list (the result-object contract consumed by Phases 3 & 6):
#   totals               numeric length R -- final net P&L per session. KEPT in
#                        full: downstream risk/engagement metrics need every
#                        session, not just summaries.
#   checkpoint_plays     integer play-indices at which cumulative P&L is snapped.
#   checkpoint_quantiles matrix (n_checkpoints x length(probs)) of cumulative-
#                        P&L quantiles across sessions -- the fan chart ribbon.
#   checkpoint_mean      numeric n_checkpoints -- mean cumulative P&L per point.
#   probs                the quantile probabilities used (column labels).
#   N, R, seed           run metadata.
#   ev_play, sd_play     effective per-play EV/SD of the distribution used.
#   crn_used             logical: was a shared uniform stream used?
#
# MEMORY CHUNKING (core design decision):
#   We NEVER materialise the full R x N matrix (10k x 10k doubles = 800 MB). We
#   batch over sessions: each batch draws (batch_size x N), computes that batch's
#   session totals and its cumulative values AT THE CHECKPOINTS ONLY, and writes
#   them into the length-R `totals` vector and the (R x n_checkpoints) cumulative
#   matrix. The peak transient allocation is (batch_size x N), controlled by
#   `.chunk_size()`; the persistent R x n_checkpoints matrix (e.g. 10000 x 100 =
#   8 MB) is small and is what quantiles are taken over.
#
#   Trade-off: smaller batches -> lower peak memory but more loop overhead and
#   more sample.int() calls; larger batches -> fewer calls but a bigger transient
#   matrix. We target a fixed transient cell budget (see .chunk_size()) so peak
#   memory is bounded regardless of N, R.
#
# REPRODUCIBILITY: with a fixed `seed` (and no CRN) the whole run is
# deterministic, INCLUDING the batching -- we set the seed once and consume the
# stream sequentially, so results are independent of chunk size. With CRN, the
# uniforms fully determine the draws (seed is ignored for sampling).
simulate_sessions <- function(dist, N, R,
                              seed        = NULL,
                              checkpoints = 100,
                              probs       = c(.05, .25, .5, .75, .95),
                              crn         = NULL) {
  # ---- validate controls -----------------------------------------------------
  if (N < 1 || N != as.integer(N)) stop("N must be a positive integer.", call. = FALSE)
  if (R < 1 || R != as.integer(R)) stop("R must be a positive integer.", call. = FALSE)
  N <- as.integer(N); R <- as.integer(R)
  if (any(probs < 0) || any(probs > 1)) stop("probs must lie in [0, 1].", call. = FALSE)

  cd <- .canonical_dist(dist)
  m  <- .dist_moments(cd)

  # ---- checkpoints -----------------------------------------------------------
  cp   <- .checkpoint_indices(N, checkpoints)
  ncp  <- length(cp)

  # ---- CRN vs default sampling setup ----------------------------------------
  crn_mat  <- .resolve_crn(crn, R, N)   # R x N uniform matrix, or NULL
  crn_used <- !is.null(crn_mat)

  # Seed handling. WHY set once here and draw uniforms sequentially per batch:
  # both paths map uniforms through the inverse CDF, so the run is a
  # deterministic function of the uniform stream. Drawing runif(bR*N) per batch
  # from one seeded stream is byte-identical to one big runif(R*N) call, which
  # makes the result INDEPENDENT OF CHUNK SIZE. CRN supplies the uniforms
  # directly (seed is then irrelevant to sampling).
  if (!is.null(seed) && !crn_used) set.seed(as.integer(seed))

  # ---- persistent accumulators (small) --------------------------------------
  totals   <- numeric(R)             # length R final P&L
  cum_at_cp <- matrix(0, nrow = R, ncol = ncp)  # R x n_checkpoints cumulative

  # ---- chunk over sessions ---------------------------------------------------
  batch  <- .chunk_size(N)           # sessions per batch (bounded cell budget)
  breaks <- cd$cdf[-length(cd$cdf)]  # interior inverse-CDF breakpoints (see .draw_batch)
  # Segment boundaries between consecutive checkpoints: segment k spans plays
  # seg_lo[k]..cp[k]. Precomputed once so the per-batch accumulation is a short
  # loop over the ~100 checkpoints, not over all N plays.
  seg_lo <- c(1L, utils::head(cp, -1L) + 1L)
  start  <- 1L
  while (start <= R) {
    end  <- min(start + batch - 1L, R)
    bR   <- end - start + 1L         # sessions in this batch

    # Obtain this batch's uniforms in SESSION-CONTIGUOUS order (see .draw_batch's
    # LAYOUT CONTRACT), then map through the inverse CDF. CRN: slice the shared
    # matrix's rows for these sessions and flatten row-major (t() then as.numeric)
    # so each session's N uniforms stay contiguous. Default: pull the next bR*N
    # uniforms from the seeded stream (already session-contiguous).
    if (crn_used) {
      u_batch <- as.numeric(t(crn_mat[start:end, , drop = FALSE]))
    } else {
      u_batch <- stats::runif(bR * N)
    }
    # PERF (Phase 9): lay the draws out in NATURAL column-major order -- (N x bR),
    # column i = session i -- so the session-contiguous uniform stream maps in
    # WITHOUT the byrow = TRUE transpose the old (bR x N) layout forced. Crucially
    # we attach the dimensions with `dim<-` IN PLACE rather than matrix(), which
    # would copy the freshly indexed vector a second time: profiling showed that
    # copy dominated engine time, and dropping it ~halves peak memory. A session's
    # N plays are now a contiguous COLUMN, so cumulative P&L is a columnwise sum.
    draws <- cd$value[findInterval(u_batch, breaks) + 1L]
    dim(draws) <- c(N, bR)

    # Cumulative P&L at the checkpoints. Accumulate the columnwise sum of each
    # inter-checkpoint play slice with matrixStats::colSums2(rows = ...), which
    # sums the requested rows IN PLACE (no slice copy), rather than one R-level
    # iteration per play. Chunk-invariant and per-session-independent: each
    # column is summed on its own, so a session's checkpoints/total do not depend
    # on how many sessions share its batch. Peak memory stays one N x bR matrix.
    running <- numeric(bR)
    for (k in seq_len(ncp)) {
      running <- running +
        matrixStats::colSums2(draws, rows = seg_lo[k]:cp[k])
      cum_at_cp[start:end, k] <- running
    }
    # Final cumulative == session total (cp always ends at N, so running holds it).
    totals[start:end] <- running

    start <- end + 1L
  }

  # ---- fan-chart quantiles across sessions ----------------------------------
  # Empirical percentiles per checkpoint (type 7, R's default). We report
  # percentiles rather than mean+/-SD precisely because the jackpot games are
  # heavily right-skewed and a Normal band would misrepresent the tails.
  checkpoint_quantiles <- matrix(NA_real_, nrow = ncp, ncol = length(probs),
                                 dimnames = list(NULL, paste0("p", probs * 100)))
  for (k in seq_len(ncp)) {
    checkpoint_quantiles[k, ] <- stats::quantile(cum_at_cp[, k], probs = probs,
                                                 names = FALSE, type = 7)
  }
  checkpoint_mean <- colMeans(cum_at_cp)

  # ---- consistency guarantee (cheap assertion) ------------------------------
  # The last checkpoint's cumulative value must equal the session total for
  # every session. This is guaranteed by construction (cp ends at N); assert it
  # so any future refactor that breaks it fails loudly.
  stopifnot(all(abs(cum_at_cp[, ncp] - totals) < 1e-9))

  list(
    totals               = totals,
    checkpoint_plays     = cp,
    checkpoint_quantiles = checkpoint_quantiles,
    checkpoint_mean      = checkpoint_mean,
    probs                = probs,
    N                    = N,
    R                    = R,
    seed                 = seed,
    ev_play              = m$ev,
    sd_play              = m$sd,
    crn_used             = crn_used
  )
}


# ------------------------------------------------------------------------------
# .chunk_size(N)
# ------------------------------------------------------------------------------
# Internal: choose the number of sessions per batch so the transient draw matrix
# (batch x N) stays within a fixed cell budget. This bounds PEAK memory
# independent of the total R.
#
# Budget rationale (TUNED in Phase 9): 1e7 doubles ~= 80 MB peak transient. A
# profiling sweep of 5e6 / 1e7 / 2e7 / 4e7 on representative large runs
# (N=2000xR=2000, N=5000xR=1000, N=1000xR=5000) found run time FLAT from 1e7
# upward (differences within measurement noise) and only a slight penalty at
# 5e6 from extra loop/RNG-call overhead. 1e7 sits at the knee: it matches the
# fastest observed times while HALVING the peak transient allocation vs the
# previous 2e7 (160 MB) -- the better memory/speed trade-off. At N = 10,000 this
# gives 1,000 sessions/batch; at small N the whole run fits in one batch.
#
# Trade-off (documented): a larger budget means fewer, larger runif()/findInterval()
# calls at the cost of a bigger transient N x batch matrix; a smaller budget is
# gentler on memory but adds loop overhead. It is deliberately a BOUNDED ceiling
# so peak memory is independent of the total R.
.chunk_size <- function(N) {
  cell_budget <- 1e7
  b <- max(1L, floor(cell_budget / N))
  as.integer(b)
}


# ------------------------------------------------------------------------------
# simulate_one_session(dist, N, seed = NULL, crn = NULL)
# ------------------------------------------------------------------------------
# Returns the full per-play net_value VECTOR for a single session (length N).
# For Phase 6 play-by-play and streak metrics (longest losing run etc.). Cheap:
# one session, no chunking, no checkpointing.
#
# Supports CRN too (a length-N uniform vector or a scalar seed) so a single
# session can be replayed on the same stream as a compare run.
simulate_one_session <- function(dist, N, seed = NULL, crn = NULL) {
  if (N < 1 || N != as.integer(N)) stop("N must be a positive integer.", call. = FALSE)
  N  <- as.integer(N)
  cd <- .canonical_dist(dist)

  if (!is.null(crn)) {
    # CRN path: resolve to a length-N uniform vector (scalar seed or supplied u).
    u <- as.numeric(.resolve_crn(crn, R = 1L, N = N))   # 1 x N matrix -> vector
  } else {
    if (!is.null(seed)) set.seed(as.integer(seed))
    u <- stats::runif(N)
  }
  draws <- .draw_batch(cd, 1L, N, u = u)
  as.numeric(draws)   # length-N per-play net_value vector
}
