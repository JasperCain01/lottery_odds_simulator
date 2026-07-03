# ==============================================================================
# viz.R
# ------------------------------------------------------------------------------
# Layer 6 of the lottery odds simulator (see IMPLEMENTATION_PLAN.md, sections
# 4 and Phase 6).
#
# Purpose: the charts that carry the app's story, built from simulate.R /
# metrics.R outputs and wired into app.R:
#   - fan chart: cumulative P&L over plays with a CI ribbon built from the
#     checkpointed quantiles (not every individual play)
#   - final-P&L histogram/density with break-even/mean/median/percentile
#     markers
#   - dream-vs-reality: the advertised jackpot vs what almost always happens
#   - leaderboard-vs-N small multiple
#   - single-session play-by-play view
#   - log/winsorize toggle for heavily skewed distributions
#   - sortable leaderboard table
#
# DESIGN CONTRACT: every viz_*() builder is a PURE function -- it takes result
# objects from simulate.R/metrics.R (or a caller-assembled tidy data.frame)
# and returns a ggplot object. No input$/reactive references, no side effects
# from source()-ing this file, so every chart renders headlessly (tests use
# ggplot2::ggplot_build()/ggsave() with no device) and app.R only has to wrap
# each builder in renderPlot(). Each builder has a matching viz_*_alt()
# function that returns a plain-text alt-text string describing the SAME
# chart from the underlying numbers (not the possibly-transformed axis), for
# accessibility (renderPlot(..., alt = ...)).
# ==============================================================================


# ------------------------------------------------------------------------------
# .viz_default(x, default)
# ------------------------------------------------------------------------------
# Internal: tiny "use x unless NULL" helper (no infix operator is defined
# elsewhere in this codebase, so we keep this as an ordinary function to
# match house style rather than introducing a `%||%`).
.viz_default <- function(x, default) if (is.null(x)) default else x


# ------------------------------------------------------------------------------
# .viz_wrap(x, width)
# ------------------------------------------------------------------------------
# Internal: wrap a long caption/subtitle string onto multiple lines so it fits
# within a fixed render width instead of running off the edge of the plot
# device. WHY base `strwrap()` and not `stringr::str_wrap()`: stringr is not
# already a project dependency (see install.R) and base R's strwrap() does the
# same word-wrapping job, so there is no need to add one just for this.
# Returns a single string with embedded "\n"s (what ggplot2's text grobs
# expect), not a character vector of lines.
.viz_wrap <- function(x, width = 80) {
  paste(strwrap(x, width = width), collapse = "\n")
}


# ------------------------------------------------------------------------------
# .viz_gbp(x, digits)
# ------------------------------------------------------------------------------
# Internal: signed currency formatting for user-facing text (captions, alt
# text). Puts the minus sign BEFORE the "£" ("-£165.00"), never inside it --
# the naive paste0("£", format(x)) yields "£-165" (and, on vectors, width-
# padded "£ -12.00"), which reads as a typo. Vectorised; NA -> "£?".
.viz_gbp <- function(x, digits = 2) {
  vapply(as.numeric(x), function(v) {
    if (is.na(v)) return("\u00a3?")
    paste0(if (v < 0) "-" else "", "\u00a3",
           formatC(abs(v), format = "f", digits = digits, big.mark = ","))
  }, character(1))
}


# ------------------------------------------------------------------------------
# .viz_pct_fg(p, digits)
# ------------------------------------------------------------------------------
# Internal: a 0-1 probability as a percentage string that NEVER falls back to
# scientific notation (sprintf/format render 1.73e-7 as "1.73e-05%" in
# user-facing captions). formatC(format = "fg") keeps significant digits in
# fixed notation for arbitrarily small values.
.viz_pct_fg <- function(p, digits = 3) {
  paste0(formatC(100 * as.numeric(p), format = "fg", digits = digits), "%")
}


# ------------------------------------------------------------------------------
# .viz_signed_log_axis(x_raw)
# ------------------------------------------------------------------------------
# Internal: axis breaks/labels for the "signed_log" transform. The transformed
# values are in sign(x)*log1p(|x|) units, which users would otherwise read as
# £ (a tick at "-3" looks like -£3 but means -£19). Place ticks at 0 and
# +/- powers of ten (at most ~5 per side, anchored to the data's magnitude)
# POSITIONED in transform units but LABELLED with the true £ value.
# Returns list(breaks, labels), or NULL when the data has no spread to label.
.viz_signed_log_axis <- function(x_raw) {
  m <- suppressWarnings(max(abs(as.numeric(x_raw)), na.rm = TRUE))
  if (!is.finite(m) || m <= 0) return(NULL)
  k_hi <- ceiling(log10(m))
  k_lo <- max(0, k_hi - 4)
  pows <- 10^(k_lo:k_hi)
  raw  <- sort(unique(c(-pows, 0, pows)))
  list(breaks = sign(raw) * log1p(abs(raw)),
       labels = .viz_gbp(raw, digits = 0))
}


# ------------------------------------------------------------------------------
# .viz_story_caption(N)
# ------------------------------------------------------------------------------
# One-line, N-aware restatement of the app's core lesson, shown as a chart
# caption so the "more plays -> losing becomes near-certain" story lands on the
# chart itself, not just in the docs. Two registers: at large N the house edge
# dominates and the loss is near-certain and barely varies; at small N variance
# still has a say, but the odds tilt to a loss and every extra play tilts it
# further. 500 plays is the rough crossover where the mean starts to dominate
# the spread for these games (a deliberately soft, documented threshold).
.viz_story_caption <- function(N) {
  if (N >= 500) {
    sprintf(paste0(
      "Over %s plays the house edge dominates: the loss is near-certain and ",
      "barely varies. Fewer plays leave more to luck; more plays make losing ",
      "a sure thing."), format(N, big.mark = ","))
  } else {
    sprintf(paste0(
      "Over just %s plays luck still has a say -- but the odds tilt to a loss, ",
      "and every extra play tilts it further toward a near-certain loss."),
      format(N, big.mark = ","))
  }
}


# ------------------------------------------------------------------------------
# .viz_band_colors(n)
# ------------------------------------------------------------------------------
# Internal: n shades from light to dark blue for nested fan-chart ribbons
# (outer band lightest, inner band darkest). WHY not scale_fill_brewer(): the
# Brewer sequential palettes assume a minimum of 3 levels and emit a warning
# (and silently substitute) when asked for fewer -- which happens routinely
# here (a single ribbon band is common), and the orchestrator explicitly
# checks charts render warning-free. A manual ramp works for any n >= 1.
.viz_band_colors <- function(n) {
  if (n <= 1L) return("#8FAEDB")
  grDevices::colorRampPalette(c("#D6E3F5", "#1B3A5C"))(n)
}


# ==============================================================================
# 0. LOG / WINSORIZE TRANSFORM FOR SKEWED P&L
# ==============================================================================

# ------------------------------------------------------------------------------
# viz_transform_pnl(x, mode, winsorize_probs, winsorize_bounds)
# ------------------------------------------------------------------------------
# Apply a documented transform to a (heavily right-skewed) vector of P&L
# values so a jackpot tail does not flatten the bulk of a histogram or
# play-by-play path into a single bar. Three modes:
#
#   "none"       identity -- the honest, undistorted axis. Default.
#   "winsorize"  clip x to [lo, hi], the winsorize_probs quantiles of x (or an
#                explicit winsorize_bounds = c(lo, hi) supplied by the caller
#                so markers/other series can be clipped to the SAME bounds as
#                the main data rather than recomputing quantiles on a tiny
#                vector of markers). Compresses the tails by DELETING their
#                true magnitude -- the clipped points all pile up at the
#                boundary, so "how far into the tail" information is lost.
#   "signed_log" sign(x) * log1p(abs(x)). Monotonic, continuous, and SIGN-
#                PRESERVING (f(x) has the same sign as x, f(0) = 0), which
#                keeps "loss" left-of-zero / "profit" right-of-zero legible.
#                Compresses the tails smoothly (no information thrown away)
#                but DISTORTS magnitudes: equal distances on a signed-log axis
#                do NOT correspond to equal £ differences, and small values
#                near zero are stretched relative to large ones.
#
# TRADE-OFF (documented, per the Phase 6 brief): any transformed axis
# distorts magnitudes relative to a linear £ scale. "none" is the honest
# default; "winsorize" and "signed_log" trade a truthful axis for a legible
# one when a jackpot tail would otherwise flatten the bulk of the
# distribution into a single bar. Every builder that accepts `transform`
# labels its axis via viz_transform_axis_label() so the distortion is never
# silently applied.
#
# Returns a plain numeric vector (no attributes -- see the WHY note at the end
# of the function body) -- deliberately NOT a ggplot2::scales trans object, so
# builders can transform vlines/markers with
# the exact same function used on the bulk data (and winsorize markers to the
# SAME clip bounds as the data they annotate).
viz_transform_pnl <- function(x,
                              mode = c("none", "winsorize", "signed_log"),
                              winsorize_probs  = c(0.01, 0.99),
                              winsorize_bounds = NULL) {
  mode <- match.arg(mode)
  x <- as.numeric(x)

  out <- switch(mode,
    none = x,
    winsorize = {
      bounds <- winsorize_bounds
      if (is.null(bounds)) {
        if (length(winsorize_probs) != 2L || any(winsorize_probs < 0) ||
            any(winsorize_probs > 1) || winsorize_probs[1] >= winsorize_probs[2]) {
          stop("winsorize_probs must be c(lo, hi) with 0 <= lo < hi <= 1.",
               call. = FALSE)
        }
        bounds <- stats::quantile(x, probs = winsorize_probs, names = FALSE,
                                  type = 7, na.rm = TRUE)
      }
      pmin(pmax(x, bounds[1]), bounds[2])
    },
    signed_log = sign(x) * log1p(abs(x))
  )

  # Deliberately return a bare numeric vector (no "mode" attribute): R's
  # arithmetic operators propagate arbitrary attributes from their LHS operand,
  # which would otherwise leak this transform's mode onto anything derived
  # from `out` by further arithmetic (e.g. viz_untransform_pnl()'s round trip).
  as.numeric(out)
}


# ------------------------------------------------------------------------------
# viz_untransform_pnl(x, mode)
# ------------------------------------------------------------------------------
# The inverse map for "none" and "signed_log" (both are bijections on the
# reals). "winsorize" is NOT invertible -- clipped points lose their true
# value by construction -- so it is returned unchanged with a warning; this
# is documented rather than silently wrong. Provided mainly so callers/tests
# can round-trip signed_log and confirm it is a true transform, not a lossy
# approximation.
viz_untransform_pnl <- function(x, mode = c("none", "winsorize", "signed_log")) {
  mode <- match.arg(mode)
  switch(mode,
    none       = as.numeric(x),
    signed_log = sign(x) * (expm1(abs(x))),
    winsorize  = {
      warning("winsorize is not invertible (clipped tail values are lost); ",
              "returning input unchanged.", call. = FALSE)
      as.numeric(x)
    }
  )
}


# ------------------------------------------------------------------------------
# viz_transform_axis_label(mode, winsorize_probs)
# ------------------------------------------------------------------------------
# The clear, honest axis label for each transform mode -- always shown so a
# distorted axis is never presented as if it were linear £.
viz_transform_axis_label <- function(mode = c("none", "winsorize", "signed_log"),
                                     winsorize_probs = c(0.01, 0.99)) {
  mode <- match.arg(mode)
  switch(mode,
    none = "Session P&L (\u00a3)",
    winsorize = sprintf(
      "Session P&L (\u00a3, winsorized to %.0f-%.0fth pct -- tails clipped, not to true scale)",
      100 * winsorize_probs[1], 100 * winsorize_probs[2]),
    signed_log = "Session P&L (signed log scale -- equal spacing != equal \u00a3)"
  )
}


# ==============================================================================
# 1. FAN CHART -- cumulative P&L with a checkpoint-quantile ribbon
# ==============================================================================

# ------------------------------------------------------------------------------
# .fan_chart_bands(checkpoint_quantiles, probs)
# ------------------------------------------------------------------------------
# Internal: the fiddly bit. Reshape the (n_checkpoints x length(probs)) matrix
# generically into nested ribbon bands, WITHOUT hard-coding five columns.
#
# PAIRING LOGIC: sort probs ascending and pair position i with position
# (n - i + 1) for i = 1 .. floor(n/2). Because probs are sorted, i = 1 pairs
# the smallest with the largest probability -- the WIDEST (outermost) band --
# and i = floor(n/2) pairs the two probabilities closest to the centre -- the
# NARROWEST (innermost) band. This generalises "p5-p95 outer, p25-p75 inner"
# to any symmetric-ish set of probs without assuming exactly five columns.
#
#   - ODD count (e.g. 5 probs: .05 .25 .5 .75 .95): floor(n/2) = 2 ribbon
#     bands, plus one leftover CENTRAL probability (index 3 = the median) that
#     gets its own bare line, not a ribbon.
#   - EVEN count (e.g. 4 probs: .05 .25 .75 .95): floor(n/2) = 2 ribbon bands,
#     no leftover index, so NO bare median line is drawn (there is no exact
#     central quantile to draw one from).
#
# Returns list(ribbons = long data.frame(play, ymin, ymax, level, label) or
# NULL, median = data.frame(play, value) or NULL).
.fan_chart_bands <- function(checkpoint_quantiles, probs, play) {
  ord    <- order(probs)
  probs_s <- probs[ord]
  cq_s   <- checkpoint_quantiles[, ord, drop = FALSE]
  n      <- length(probs_s)
  half   <- n %/% 2L

  ribbons <- NULL
  if (half >= 1L) {
    parts <- lapply(seq_len(half), function(i) {
      lo_j <- i
      hi_j <- n - i + 1L
      data.frame(
        play  = play,
        ymin  = cq_s[, lo_j],
        ymax  = cq_s[, hi_j],
        level = i,   # 1 = outermost (widest) band
        label = sprintf("%s%%–%s%%",
                        format(100 * probs_s[lo_j], trim = TRUE),
                        format(100 * probs_s[hi_j], trim = TRUE))
      )
    })
    ribbons <- do.call(rbind, parts)
    # Order outer-to-inner so the legend and draw order read outside-in;
    # factor levels fixed to creation order (NOT alphabetical).
    lbl_levels    <- unique(ribbons$label)
    ribbons$label <- factor(ribbons$label, levels = lbl_levels)
    ribbons$level <- factor(ribbons$level, levels = sort(unique(ribbons$level)))
  }

  median <- NULL
  if (n %% 2L == 1L) {
    mid    <- half + 1L
    median <- data.frame(play = play, value = cq_s[, mid])
  }

  list(ribbons = ribbons, median = median)
}


# ------------------------------------------------------------------------------
# viz_fan_chart(sim, show_mean = TRUE, title = NULL)
# ------------------------------------------------------------------------------
# Cumulative P&L over plays with a nested CI ribbon built from
# sim$checkpoint_quantiles (NOT per-play data -- the engine never retains
# per-play sequences for the chunked sessions run; see simulate.R). x =
# sim$checkpoint_plays; nested ribbons from symmetric quantile pairs (see
# .fan_chart_bands()); the median as its own line (odd prob count only); the
# checkpoint mean as a visually distinct dashed reference line (mean and
# median diverge exactly where the story lives -- a skewed jackpot game pulls
# the mean away from the median); a dashed break-even line at 0.
viz_fan_chart <- function(sim, show_mean = TRUE, title = NULL) {
  stopifnot(is.list(sim),
           !is.null(sim$checkpoint_quantiles), !is.null(sim$checkpoint_plays))

  cq   <- sim$checkpoint_quantiles
  play <- sim$checkpoint_plays
  probs <- sim$probs
  if (is.null(probs)) {
    # Defensive fallback for hand-built sim-like objects in tests: parse
    # "p5", "p25", ... column names back into fractions.
    nm <- colnames(cq)
    if (is.null(nm)) stop("sim$checkpoint_quantiles needs column names or sim$probs.",
                          call. = FALSE)
    probs <- as.numeric(sub("^p", "", nm)) / 100
  }
  if (length(probs) != ncol(cq)) {
    stop("length(sim$probs) must equal ncol(sim$checkpoint_quantiles).", call. = FALSE)
  }

  bands <- .fan_chart_bands(cq, probs, play)

  p <- ggplot2::ggplot()

  if (!is.null(bands$ribbons)) {
    n_bands <- length(levels(bands$ribbons$level))
    p <- p +
      ggplot2::geom_ribbon(
        data = bands$ribbons,
        ggplot2::aes(x = play, ymin = ymin, ymax = ymax,
                    group = level, fill = label),
        alpha = 0.55
      ) +
      ggplot2::scale_fill_manual(values = .viz_band_colors(n_bands),
                                 name = "Percentile band")
  }

  p <- p + ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey40")

  if (!is.null(bands$median)) {
    p <- p + ggplot2::geom_line(
      data = bands$median, ggplot2::aes(x = play, y = value),
      color = "#14213D", linewidth = 0.9
    )
  }

  if (isTRUE(show_mean)) {
    mean_df <- data.frame(play = play, value = sim$checkpoint_mean)
    p <- p + ggplot2::geom_line(
      data = mean_df, ggplot2::aes(x = play, y = value),
      color = "firebrick", linetype = "twodash", linewidth = 0.7
    )
  }

  p +
    ggplot2::scale_x_continuous(labels = scales::label_comma()) +
    ggplot2::scale_y_continuous(labels = scales::label_dollar(prefix = "\u00a3")) +
    ggplot2::labs(
      x = "Play #", y = "Cumulative session P&L",
      title = .viz_default(title, "Cumulative P&L over the session (fan chart)"),
      subtitle = sprintf(
        "N = %s, R = %s -- solid = median, dashed red = mean%s",
        format(sim$N, big.mark = ","), format(sim$R, big.mark = ","),
        if (is.null(bands$median)) " (no median: even # of percentiles)" else ""
      ),
      caption = .viz_wrap(.viz_story_caption(sim$N), 95)
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(plot.caption = ggplot2::element_text(
      hjust = 0, size = 9, face = "italic", color = "grey30"))
}


# ------------------------------------------------------------------------------
# viz_fan_chart_alt(sim)
# ------------------------------------------------------------------------------
viz_fan_chart_alt <- function(sim) {
  probs <- sim$probs
  cq    <- sim$checkpoint_quantiles
  ord   <- order(probs)
  lo_j  <- ord[1]; hi_j <- ord[length(ord)]
  final <- nrow(cq)

  sprintf(paste0(
    "Fan chart of cumulative session profit and loss over %s plays, across %s ",
    "simulated sessions. The band widens from the first play to the last; by ",
    "play %s the central %s%% of outcomes fall between %s and %s, the mean ",
    "path ends at %s, and the break-even line is at \u00a30."),
    format(sim$N, big.mark = ","), format(sim$R, big.mark = ","),
    format(tail(sim$checkpoint_plays, 1), big.mark = ","),
    format(100 * (probs[hi_j] - probs[lo_j]), trim = TRUE),
    .viz_gbp(cq[final, lo_j]),
    .viz_gbp(cq[final, hi_j]),
    .viz_gbp(tail(sim$checkpoint_mean, 1))
  )
}


# ==============================================================================
# 2. FINAL-P&L HISTOGRAM
# ==============================================================================

# ------------------------------------------------------------------------------
# viz_pnl_hist(sim, transform, bins, marker_probs, winsorize_probs, title)
# ------------------------------------------------------------------------------
# Distribution of sim$totals with vertical markers for break-even (£0), mean,
# median, and the requested percentiles (default: the outermost pair of
# sim$probs if present, else marker_probs). Markers are labelled directly on
# the plot rather than left to a legend, since 4-6 labelled vlines read more
# clearly inline than cross-referenced against a key.
#
# `transform` feeds viz_transform_pnl(): for "winsorize" the SAME clip bounds
# are applied to both the bulk histogram data and the markers (bounds computed
# once from sim$totals) so a marker that lies inside the data's natural range
# is never spuriously clipped by a marker-only quantile estimate.
viz_pnl_hist <- function(sim,
                         transform        = c("none", "winsorize", "signed_log"),
                         bins             = 40,
                         marker_probs     = NULL,
                         winsorize_probs  = c(0.01, 0.99),
                         title            = NULL) {
  transform <- match.arg(transform)
  totals <- as.numeric(sim$totals)
  if (length(totals) == 0L) stop("sim$totals is empty.", call. = FALSE)

  if (is.null(marker_probs)) {
    marker_probs <- if (!is.null(sim$probs) && length(sim$probs) >= 2L) {
      range(sim$probs)
    } else {
      c(0.05, 0.95)
    }
  }

  bounds <- NULL
  if (identical(transform, "winsorize")) {
    bounds <- stats::quantile(totals, probs = winsorize_probs, names = FALSE,
                              type = 7, na.rm = TRUE)
  }

  x_t <- viz_transform_pnl(totals, mode = transform,
                           winsorize_probs = winsorize_probs,
                           winsorize_bounds = bounds)

  mean_raw   <- mean(totals)
  median_raw <- stats::median(totals)
  pct_raw    <- stats::quantile(totals, probs = marker_probs, names = FALSE, type = 7)
  pct_names  <- paste0("p", format(100 * marker_probs, trim = TRUE))

  marker_raw <- c(0, mean_raw, median_raw, pct_raw)
  marker_lbl <- c("Break-even", "Mean", "Median", pct_names)
  marker_t   <- viz_transform_pnl(marker_raw, mode = transform,
                                  winsorize_probs = winsorize_probs,
                                  winsorize_bounds = bounds)

  marker_df <- data.frame(
    label = factor(marker_lbl, levels = marker_lbl),
    x_raw = marker_raw,
    x_t   = as.numeric(marker_t),
    kind  = c("breakeven", "mean", "median", rep("percentile", length(pct_raw)))
  )
  # De-duplicate exactly-coincident marker positions (e.g. mean == median on a
  # symmetric fixture) so overlapping vlines don't stack illegibly.
  marker_df <- marker_df[!duplicated(round(marker_df$x_t, 8)), , drop = FALSE]

  # TEXT labels get a coarser tolerance-based merge on top of that: when a
  # rare, huge jackpot pulls the mean far from a tight median/percentile
  # cluster (the app's central skew story), several markers can land within a
  # few pixels of each other and their text would otherwise overlap into
  # mush. Markers within 1.5% of the visible x-range are combined into one
  # "A / B / C" label positioned at their average x -- the vlines themselves
  # stay at their exact individual positions.
  rng <- diff(range(x_t))
  tol <- if (is.finite(rng) && rng > 0) 0.015 * rng else 0
  lbl_ord <- marker_df[order(marker_df$x_t), , drop = FALSE]
  grp_id  <- cumsum(c(TRUE, diff(lbl_ord$x_t) > tol))
  label_df <- do.call(rbind, lapply(split(lbl_ord, grp_id), function(g) {
    data.frame(
      label = paste(as.character(g$label), collapse = " / "),
      x_t   = mean(g$x_t),
      kind  = if (nrow(g) == 1L) as.character(g$kind[1]) else "percentile",
      stringsAsFactors = FALSE
    )
  }))

  marker_colors <- c(breakeven = "grey30", mean = "firebrick",
                     median = "#1B9E77", percentile = "#2C3E50")

  # Axis ticks: under "signed_log" the axis is in transform units, so place
  # ticks at powers of ten labelled with the TRUE £ value (see
  # .viz_signed_log_axis) -- otherwise a tick reading "-3" is misread as -£3.
  # On the linear/winsorized axes, plain £ labels.
  x_scale <- if (identical(transform, "signed_log")) {
    ax <- .viz_signed_log_axis(totals)
    if (!is.null(ax)) {
      ggplot2::scale_x_continuous(breaks = ax$breaks, labels = ax$labels)
    } else NULL
  } else {
    ggplot2::scale_x_continuous(labels = scales::label_dollar(prefix = "\u00a3"))
  }

  ggplot2::ggplot(data.frame(x = x_t), ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(bins = bins, fill = "#8FAEDB", color = "white", alpha = 0.9) +
    ggplot2::geom_vline(
      data = marker_df,
      ggplot2::aes(xintercept = x_t, color = kind,
                  linetype = ifelse(kind == "breakeven", "dashed", "solid")),
      linewidth = 0.7, show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = label_df,
      ggplot2::aes(x = x_t, y = Inf, label = label, color = kind),
      angle = 90, hjust = 1.1, vjust = -0.3, size = 3.2, show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(values = marker_colors) +
    ggplot2::scale_linetype_identity() +
    ggplot2::labs(
      x = viz_transform_axis_label(transform, winsorize_probs), y = "Sessions",
      title = .viz_default(title, sprintf(
        "Final session P&L -- N = %s, R = %s",
        format(sim$N, big.mark = ","), format(sim$R, big.mark = ","))),
      caption = .viz_wrap(.viz_story_caption(sim$N), 95)
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(plot.caption = ggplot2::element_text(
      hjust = 0, size = 9, face = "italic", color = "grey30")) +
    ggplot2::coord_cartesian(clip = "off") +
    x_scale
}


# ------------------------------------------------------------------------------
# viz_pnl_hist_alt(sim)
# ------------------------------------------------------------------------------
# Alt text always describes the RAW (untransformed) numbers -- a screen-reader
# user should get the honest £ figures regardless of which display transform
# is toggled on for the visible axis.
viz_pnl_hist_alt <- function(sim) {
  totals <- as.numeric(sim$totals)
  sprintf(paste0(
    "Histogram of final session profit and loss across %s simulated sessions ",
    "of %s plays each. Mean %s, median %s, break-even (\u00a30) marked; %.1f%% of ",
    "sessions ended in profit."),
    format(sim$R, big.mark = ","), format(sim$N, big.mark = ","),
    .viz_gbp(mean(totals)),
    .viz_gbp(stats::median(totals)),
    100 * mean(totals > 0)
  )
}


# ==============================================================================
# 3. DREAM VS REALITY
# ==============================================================================

# ------------------------------------------------------------------------------
# viz_dream_vs_reality(metrics, title = NULL)
# ------------------------------------------------------------------------------
# Contrasts metrics$dream$full$mean_session (the advertised full distribution,
# jackpot included) against metrics$dream$conditional$mean_session (what a
# session looks like when it does NOT hit the top prize tier -- i.e. almost
# always). Two bars, both labelled with their £ value; a caption states the
# per-play and per-N-plays jackpot probability HONESTLY (no overselling the
# upside -- the "reality" bar is deliberately the more prominent number, since
# it is the outcome almost every session actually experiences).
viz_dream_vs_reality <- function(metrics, title = NULL) {
  d <- metrics$dream
  if (is.null(d) || is.null(d$full) || is.null(d$conditional)) {
    stop("metrics$dream must be a dream_vs_reality() result.", call. = FALSE)
  }

  scenario_levels <- c("Advertised\n(full distribution)", "Reality\n(no jackpot hit)")
  df <- data.frame(
    scenario     = factor(scenario_levels, levels = scenario_levels),
    mean_session = c(d$full$mean_session, d$conditional$mean_session)
  )
  # Degenerate edge case: the top tier IS essentially the whole distribution,
  # so dream_vs_reality() reports the conditional mean as NA (see metrics.R).
  # Drop that row rather than handing geom_col() an NA bar (which only draws a
  # console warning and an empty bar) -- the caption below explains why.
  df <- df[!is.na(df$mean_session), , drop = FALSE]

  # NOTE the framing: top_value is a NET figure (prize minus the ticket cost),
  # so a \u00a32,000,000 jackpot on a \u00a35 ticket is \u00a31,999,995 here -- say so, or it
  # reads as a typo against the advertised prize. The per-play chance is given
  # as 1-in-W (a "1.73e-05%"-style scientific percentage is unreadable).
  odds_phrase <- if (!is.na(d$p_top) && d$p_top > 0) {
    sprintf("about 1 in %s plays",
            format(round(1 / d$p_top), big.mark = ",", scientific = FALSE))
  } else {
    "essentially never"
  }
  cap <- if (is.na(d$conditional$mean_session)) {
    sprintf(paste0(
      "The top prize (a net win of %s after the ticket cost) makes up ",
      "essentially all of the probability mass here -- there is no meaningful ",
      "'no jackpot' scenario to show."),
      .viz_gbp(d$top_value, digits = 0))
  } else {
    sprintf(paste0(
      "The top prize is a net win of %s (after the ticket cost) and hits %s ",
      "(~%s chance of at least one hit across N = %s plays). The 'Reality' bar ",
      "is what a session looks like on every play that does NOT hit it -- ",
      "almost every session."),
      .viz_gbp(d$top_value, digits = 0), odds_phrase,
      .viz_pct_fg(d$p_top_in_N),
      format(d$N, big.mark = ","))
  }
  # Wrapped so the caption fits within a fixed render width instead of
  # overflowing the plot device on one long line (see .viz_wrap()).
  cap <- .viz_wrap(cap, width = 80)

  ggplot2::ggplot(df, ggplot2::aes(x = scenario, y = mean_session, fill = scenario)) +
    ggplot2::geom_col(width = 0.55) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::label_dollar(prefix = "\u00a3")(mean_session),
                  vjust = ifelse(mean_session >= 0, -0.4, 1.3)),
      fontface = "bold", size = 4.2
    ) +
    ggplot2::scale_fill_manual(values = c("#7B8FA6", "#B0413E"), guide = "none") +
    ggplot2::scale_y_continuous(labels = scales::label_dollar(prefix = "\u00a3")) +
    ggplot2::labs(
      x = NULL, y = sprintf("Mean session P&L (N = %s plays)", format(d$N, big.mark = ",")),
      title = .viz_default(title, "The advertised dream vs. what almost always happens"),
      caption = cap
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0, size = 9))
}


# ------------------------------------------------------------------------------
# viz_dream_vs_reality_alt(metrics)
# ------------------------------------------------------------------------------
viz_dream_vs_reality_alt <- function(metrics) {
  d <- metrics$dream
  sprintf(paste0(
    "Bar chart contrasting mean session profit and loss across the full ",
    "distribution (%s, includes the jackpot chance) against the distribution ",
    "conditional on not hitting the top prize, a net win of %s after the ",
    "ticket cost (%s) -- the outcome in practically every session, since the ",
    "top prize hits with probability %s across %s plays."),
    .viz_gbp(d$full$mean_session),
    .viz_gbp(d$top_value, digits = 0),
    if (is.na(d$conditional$mean_session)) "n/a" else .viz_gbp(d$conditional$mean_session),
    .viz_pct_fg(d$p_top_in_N),
    format(d$N, big.mark = ",")
  )
}


# ==============================================================================
# 4. LEADERBOARD-VS-N SMALL MULTIPLE
# ==============================================================================

# ------------------------------------------------------------------------------
# viz_leaderboard_vs_n(series_df, group_col, x_col, y_col, title)
# ------------------------------------------------------------------------------
# `series_df` is a tidy data.frame the CALLER assembles across several
# strategies, e.g.:
#
#   do.call(rbind, lapply(strategies, function(s) {
#     d <- leaderboard_series(strategy_distribution(s, outcomes), N_grid,
#                             metric = "p_profit", R = 2000, seed = 42)
#     cbind(strategy = s$label, d)
#   }))
#
# i.e. columns identifying strategy + N + value (metrics.R's leaderboard_series()
# already returns N/metric/value; this just draws it). One line per strategy,
# so the crossover in ranking as N grows -- the app's central "loses most" vs
# "loses least" story -- is visible directly.
viz_leaderboard_vs_n <- function(series_df,
                                 group_col = "strategy",
                                 x_col     = "N",
                                 y_col     = "value",
                                 title     = NULL) {
  need <- c(group_col, x_col, y_col)
  if (!all(need %in% names(series_df))) {
    stop(sprintf("series_df must have columns: %s.", paste(need, collapse = ", ")),
         call. = FALSE)
  }

  metric_label <- if ("metric" %in% names(series_df)) unique(series_df$metric) else NA
  single_metric <- length(metric_label) == 1L && !is.na(metric_label)

  ylab <- if (single_metric) {
    switch(metric_label,
      p_profit = "P(profit)",
      mean_pnl = "Mean session P&L (\u00a3)",
      as.character(metric_label))
  } else "Value"

  # Stable colour + legend order: factor levels in order of FIRST APPEARANCE
  # (the caller-supplied strategy order) rather than ggplot's alphabetical
  # default, coloured with the same Okabe-Ito palette as the compare overlays
  # -- so on the Compare tab a strategy keeps one colour across the
  # distribution, fan, and crossover charts instead of being re-keyed.
  if (!is.factor(series_df[[group_col]])) {
    series_df[[group_col]] <- factor(series_df[[group_col]],
                                     levels = unique(series_df[[group_col]]))
  }
  n_groups <- nlevels(series_df[[group_col]])

  p <- ggplot2::ggplot(
    series_df,
    ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]], color = .data[[group_col]])
  ) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 1.6) +
    ggplot2::scale_color_manual(values = .compare_palette(n_groups)) +
    # Log-10 N axis: the grid spans 10..2000 and the ranking crossover -- the
    # chart's whole story -- happens at small N, which a linear axis squashes
    # into the left margin. N is validated positive upstream, so log is safe.
    ggplot2::scale_x_log10(labels = scales::label_comma()) +
    ggplot2::labs(
      x = "N (plays per session, log scale)", y = ylab, color = "Strategy",
      title = .viz_default(title, "How the leaderboard shifts as N grows")
    ) +
    ggplot2::theme_minimal(base_size = 13)

  if (single_metric && identical(metric_label, "p_profit")) {
    p <- p + ggplot2::scale_y_continuous(labels = scales::label_percent())
  } else if (single_metric && identical(metric_label, "mean_pnl")) {
    p <- p +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
      ggplot2::scale_y_continuous(labels = scales::label_dollar(prefix = "\u00a3"))
  }

  p
}


# ------------------------------------------------------------------------------
# viz_leaderboard_vs_n_alt(series_df, group_col, x_col, y_col)
# ------------------------------------------------------------------------------
viz_leaderboard_vs_n_alt <- function(series_df,
                                     group_col = "strategy",
                                     x_col     = "N",
                                     y_col     = "value") {
  n_min <- min(series_df[[x_col]]); n_max <- max(series_df[[x_col]])
  at_min <- series_df[series_df[[x_col]] == n_min, , drop = FALSE]
  at_max <- series_df[series_df[[x_col]] == n_max, , drop = FALSE]
  best_min <- at_min[[group_col]][which.max(at_min[[y_col]])]
  best_max <- at_max[[group_col]][which.max(at_max[[y_col]])]

  sprintf(paste0(
    "Multi-line chart of a metric vs. N (plays per session) across %d strategies. ",
    "At N = %s the leading strategy is '%s'; at N = %s it is '%s'%s."),
    length(unique(series_df[[group_col]])),
    format(n_min, big.mark = ","), best_min,
    format(n_max, big.mark = ","), best_max,
    if (identical(best_min, best_max)) " (no change in the leader)" else
      " (the ranking has flipped)"
  )
}


# ------------------------------------------------------------------------------
# viz_leaderboard_table(series_df, group_col, x_col, y_col)
# ------------------------------------------------------------------------------
# Tidy data.frame -> wide sortable table (one row per strategy, one column per
# N) for a UI table (e.g. DT::datatable() in app.R). Kept separate from the
# chart builder so app.R can render both a plot and a table from the same
# leaderboard_series() output.
viz_leaderboard_table <- function(series_df,
                                  group_col = "strategy",
                                  x_col     = "N",
                                  y_col     = "value") {
  need <- c(group_col, x_col, y_col)
  if (!all(need %in% names(series_df))) {
    stop(sprintf("series_df must have columns: %s.", paste(need, collapse = ", ")),
         call. = FALSE)
  }
  tidyr::pivot_wider(
    series_df[, need],
    id_cols     = dplyr::all_of(group_col),
    names_from  = dplyr::all_of(x_col),
    values_from = dplyr::all_of(y_col),
    names_prefix = "N="
  )
}


# ==============================================================================
# 5. SINGLE-SESSION PLAY-BY-PLAY
# ==============================================================================

# ------------------------------------------------------------------------------
# viz_play_by_play(one_session, price, transform, winsorize_probs, title)
# ------------------------------------------------------------------------------
# Cumulative P&L path for ONE session (the length-N per-play net-value vector
# from simulate_one_session()), with each play marked win/loss.
#
# WIN definition: if `price` is supplied, a play wins iff net_value > -price
# (matching engagement_metrics()'s definition, i.e. any prize at all,
# including one smaller than the stake). If `price` is NULL (the vector alone
# carries no stake information), a play is coloured a win iff net_value > 0
# (strictly ahead on that single play) -- a documented, coarser fallback.
viz_play_by_play <- function(one_session,
                             price            = NULL,
                             transform        = c("none", "winsorize", "signed_log"),
                             winsorize_probs  = c(0.01, 0.99),
                             title            = NULL) {
  transform <- match.arg(transform)
  values <- as.numeric(one_session)
  if (length(values) == 0L) stop("one_session is empty.", call. = FALSE)
  n <- length(values)

  is_win <- if (!is.null(price)) {
    tol <- 1e-9 * max(1, price)
    values > -price + tol
  } else {
    values > 0
  }

  cum   <- cumsum(values)
  cum_t <- viz_transform_pnl(cum, mode = transform, winsorize_probs = winsorize_probs)

  df <- data.frame(
    play    = seq_len(n),
    cum_t   = as.numeric(cum_t),
    outcome = factor(ifelse(is_win, "Win", "Loss"), levels = c("Win", "Loss"))
  )

  # Same axis-tick treatment as viz_pnl_hist, on the y axis here: real-£
  # labels on the signed-log axis, plain £ labels otherwise.
  y_scale <- if (identical(transform, "signed_log")) {
    ax <- .viz_signed_log_axis(cum)
    if (!is.null(ax)) {
      ggplot2::scale_y_continuous(breaks = ax$breaks, labels = ax$labels)
    } else NULL
  } else {
    ggplot2::scale_y_continuous(labels = scales::label_dollar(prefix = "\u00a3"))
  }

  ggplot2::ggplot(df, ggplot2::aes(x = play, y = cum_t)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    ggplot2::geom_line(color = "#2C3E50", linewidth = 0.5) +
    ggplot2::geom_point(ggplot2::aes(color = outcome), size = 1.7, alpha = 0.85) +
    ggplot2::scale_color_manual(values = c(Win = "#1B9E77", Loss = "#D95F02"),
                               name = NULL) +
    ggplot2::labs(
      x = "Play #", y = viz_transform_axis_label(transform, winsorize_probs),
      title = .viz_default(title, sprintf("Single-session play-by-play (N = %s)",
                                          format(n, big.mark = ",")))
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    y_scale
}


# ------------------------------------------------------------------------------
# viz_play_by_play_alt(one_session, price = NULL)
# ------------------------------------------------------------------------------
viz_play_by_play_alt <- function(one_session, price = NULL) {
  values <- as.numeric(one_session)
  n <- length(values)
  cum <- cumsum(values)
  is_win <- if (!is.null(price)) values > -price + 1e-9 * max(1, price) else values > 0

  sprintf(paste0(
    "Line chart of cumulative profit and loss over a single %s-play session. ",
    "%d of %d plays won; the session ends at %s, %s break-even."),
    format(n, big.mark = ","), sum(is_win), n,
    .viz_gbp(tail(cum, 1)),
    if (tail(cum, 1) >= 0) "at or above" else "below"
  )
}


# ==============================================================================
# 6. COMPARE MODE (Phase 8) -- multi-strategy overlays
# ------------------------------------------------------------------------------
# Builders that draw a run_compare() result (see R/compare.R): several
# strategies evaluated on Common Random Numbers (the SAME uniform stream), so
# the differences the charts show are real, not sampling noise. Each is a pure
# function returning a ggplot, with a matching viz_*_alt() companion, exactly
# like the single-run builders above. `transform` honours the same log/winsorize
# machinery (viz_transform_pnl()).
# ==============================================================================

# ------------------------------------------------------------------------------
# .compare_palette(n)
# ------------------------------------------------------------------------------
# Internal: a categorical, colour-blind-friendly palette for up to n strategies
# (Okabe-Ito, which reads in both light and dark and avoids the red/green trap).
# Cycles if n exceeds the palette length (rare -- compares are a handful).
.compare_palette <- function(n) {
  base <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7",
            "#E69F00", "#56B4E9", "#F0E442", "#000000")
  if (n <= length(base)) return(base[seq_len(n)])
  rep(base, length.out = n)
}


# ------------------------------------------------------------------------------
# viz_compare_dist(compare_result, transform, winsorize_probs, title)
# ------------------------------------------------------------------------------
# Overlaid final-P&L densities, one colour per strategy, with the break-even
# line at £0. `transform` feeds viz_transform_pnl(); for "winsorize" the SAME
# clip bounds (computed once over the POOLED totals of all strategies) are
# applied to every series and to the break-even marker, so the strategies stay
# on one comparable axis rather than each clipped to its own quantiles.
viz_compare_dist <- function(compare_result,
                             transform       = c("none", "winsorize", "signed_log"),
                             winsorize_probs = c(0.01, 0.99),
                             title           = NULL) {
  transform <- match.arg(transform)
  df <- compare_result$totals_long
  if (is.null(df) || nrow(df) == 0L) stop("compare_result has no totals.", call. = FALSE)

  bounds <- NULL
  if (identical(transform, "winsorize")) {
    bounds <- stats::quantile(df$pnl, probs = winsorize_probs, names = FALSE,
                              type = 7, na.rm = TRUE)
  }
  df$x <- viz_transform_pnl(df$pnl, mode = transform,
                            winsorize_probs = winsorize_probs,
                            winsorize_bounds = bounds)
  be <- viz_transform_pnl(0, mode = transform,
                          winsorize_probs = winsorize_probs,
                          winsorize_bounds = bounds)[1]

  n_strat <- length(levels(df$strategy))
  pal     <- .compare_palette(n_strat)

  # Wrapped so the subtitle fits within a fixed render width instead of
  # overflowing the plot device on one long line (see .viz_wrap()).
  subtitle <- .viz_wrap(sprintf(
    "N = %s, R = %s, seed = %s -- Common Random Numbers: differences are real, not noise",
    format(compare_result$N, big.mark = ","),
    format(compare_result$R, big.mark = ","),
    format(compare_result$seed)), width = 80)

  # Same axis-tick treatment as viz_pnl_hist: real-£ labels on the signed-log
  # axis (transform-unit ticks are misread as £), plain £ labels otherwise.
  x_scale <- if (identical(transform, "signed_log")) {
    ax <- .viz_signed_log_axis(df$pnl)
    if (!is.null(ax)) {
      ggplot2::scale_x_continuous(breaks = ax$breaks, labels = ax$labels)
    } else NULL
  } else {
    ggplot2::scale_x_continuous(labels = scales::label_dollar(prefix = "\u00a3"))
  }

  ggplot2::ggplot(df, ggplot2::aes(x = x, color = strategy, fill = strategy)) +
    ggplot2::geom_density(alpha = 0.12, linewidth = 0.9) +
    ggplot2::geom_vline(xintercept = be, linetype = "dashed", color = "grey40") +
    ggplot2::scale_color_manual(values = pal, name = "Strategy") +
    ggplot2::scale_fill_manual(values = pal, name = "Strategy") +
    ggplot2::labs(
      x = viz_transform_axis_label(transform, winsorize_probs), y = "Density",
      title = .viz_default(title, "Final session P&L -- strategies compared (shared random draws)"),
      subtitle = subtitle
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    x_scale
}


# ------------------------------------------------------------------------------
# viz_compare_dist_alt(compare_result)
# ------------------------------------------------------------------------------
# Alt text off the RAW (untransformed) totals -- honest £ figures per strategy.
viz_compare_dist_alt <- function(compare_result) {
  tb <- compare_result$table
  parts <- sprintf("'%s' (mean %s, %.1f%% in profit)",
                   tb$strategy,
                   .viz_gbp(tb$mean_pnl),
                   100 * tb$p_profit)
  sprintf(paste0(
    "Overlaid density plot of final session profit and loss for %d strategies ",
    "evaluated on the same random draws (Common Random Numbers), across %s ",
    "sessions of %s plays: %s. The break-even line is at \u00a30."),
    nrow(tb), format(compare_result$R, big.mark = ","),
    format(compare_result$N, big.mark = ","),
    paste(parts, collapse = "; ")
  )
}


# ------------------------------------------------------------------------------
# viz_compare_fan(compare_result, title)
# ------------------------------------------------------------------------------
# Overlaid fan-chart MEDIAN lines, one per strategy, from each sim's
# checkpoint_quantiles / checkpoint_plays, plus the break-even line at £0.
# Medians (not full ribbons) are the priority for legibility: several
# overlapping percentile ribbons quickly turn to mush, whereas one median line
# per strategy makes the "which one pulls ahead / falls behind as the session
# runs" story read at a glance.
viz_compare_fan <- function(compare_result, title = NULL) {
  per <- compare_result$per
  if (is.null(per) || length(per) == 0L) stop("compare_result has no per-strategy sims.", call. = FALSE)

  med_df <- do.call(rbind, lapply(per, function(p) {
    sim   <- p$sim
    probs <- sim$probs
    cq    <- sim$checkpoint_quantiles
    # Median column: the prob closest to 0.5 (default probs include it exactly).
    j <- which.min(abs(probs - 0.5))
    data.frame(play = sim$checkpoint_plays, median = cq[, j],
               strategy = p$label, stringsAsFactors = FALSE)
  }))
  med_df$strategy <- factor(med_df$strategy, levels = compare_result$labels)

  pal <- .compare_palette(length(compare_result$labels))

  ggplot2::ggplot(med_df, ggplot2::aes(x = play, y = median, color = strategy)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_color_manual(values = pal, name = "Strategy") +
    ggplot2::scale_x_continuous(labels = scales::label_comma()) +
    ggplot2::scale_y_continuous(labels = scales::label_dollar(prefix = "\u00a3")) +
    ggplot2::labs(
      x = "Play #", y = "Median cumulative P&L",
      title = .viz_default(title, "Median cumulative P&L over the session -- strategies compared"),
      subtitle = sprintf("N = %s, R = %s, seed = %s -- median path per strategy on shared random draws",
                         format(compare_result$N, big.mark = ","),
                         format(compare_result$R, big.mark = ","),
                         format(compare_result$seed))
    ) +
    ggplot2::theme_minimal(base_size = 13)
}


# ------------------------------------------------------------------------------
# viz_compare_fan_alt(compare_result)
# ------------------------------------------------------------------------------
viz_compare_fan_alt <- function(compare_result) {
  per <- compare_result$per
  finals <- vapply(per, function(p) {
    cq <- p$sim$checkpoint_quantiles
    j  <- which.min(abs(p$sim$probs - 0.5))
    cq[nrow(cq), j]
  }, numeric(1))
  parts <- sprintf("'%s' ends at a median of %s",
                   compare_result$labels,
                   .viz_gbp(finals))
  sprintf(paste0(
    "Line chart of the median cumulative profit and loss over %s plays for %d ",
    "strategies on shared random draws: %s. The break-even line is at \u00a30."),
    format(compare_result$N, big.mark = ","), length(per),
    paste(parts, collapse = "; ")
  )
}
