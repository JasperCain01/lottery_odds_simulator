# ==============================================================================
# tools/export_webr.R
# ------------------------------------------------------------------------------
# Build the static Shinylive (webR / WebAssembly) site for GitHub Pages.
#
#   Rscript tools/export_webr.R        # writes the site to _site/
#
# Run from the repo root, AFTER the data cache exists:
#   Rscript -e 'source("R/data_prep.R"); run_data_prep(".")'
#
# WHY a STAGING copy rather than exporting the repo root: shinylive::export()
# bundles the whole appdir and scans every .R file in it for dependencies.
# Exporting the root would (a) sweep deploy.R's rsconnect and the scrape
# scripts' dependencies into the wasm bundle, and (b) ship tests, PDFs and
# the raw CSVs to every visitor. The staging dir holds exactly what the app
# needs at runtime: app.R, R/, and the three prebuilt data/*.rds artifacts
# (prebuilt so the browser never pays the CSV-parse cost at startup).
#
# BUILD METADATA: data/build_info.rds records the build date. app.R shows it
# in the sidebar because the deployed app is a frozen snapshot -- `on_sale`
# was evaluated against THIS date, not the visitor's.
#
# WASM PACKAGE BUNDLING: by default shinylive downloads the WebAssembly
# binaries of every detected dependency from repo.r-wasm.org and pins them
# into the bundle (self-contained, immune to upstream drift). Set the
# standard SHINYLIVE_WASM_PACKAGES=0 env var to skip bundling (the visitor's
# browser then fetches packages from repo.r-wasm.org at load time) -- useful
# on networks where that host is unreachable at build time.
# ==============================================================================

main <- function() {
  if (!requireNamespace("shinylive", quietly = TRUE)) {
    stop("The 'shinylive' package is required: install.packages(\"shinylive\")",
         call. = FALSE)
  }

  root    <- normalizePath(".")
  destdir <- file.path(root, "_site")

  # --- Preconditions -----------------------------------------------------------
  if (!file.exists(file.path(root, "app.R"))) {
    stop("Run from the repo root (app.R not found).", call. = FALSE)
  }
  rds <- file.path("data", c("outcomes.rds", "game_summary.rds"))
  missing <- rds[!file.exists(file.path(root, rds))]
  if (length(missing)) {
    stop("Prebuilt data cache missing (", paste(missing, collapse = ", "),
         "). Run: Rscript -e 'source(\"R/data_prep.R\"); run_data_prep(\".\")'",
         call. = FALSE)
  }

  # --- Assemble the staging app dir ---------------------------------------------
  staging <- file.path(tempdir(), "lottery-odds-simulator-webr")
  unlink(staging, recursive = TRUE)
  dir.create(file.path(staging, "R"),    recursive = TRUE)
  dir.create(file.path(staging, "data"), recursive = TRUE)

  ok <- c(
    file.copy(file.path(root, "app.R"), staging),
    file.copy(list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE),
              file.path(staging, "R")),
    file.copy(file.path(root, rds), file.path(staging, "data"))
  )
  stopifnot(all(ok))

  saveRDS(list(built = Sys.Date()), file.path(staging, "data", "build_info.rds"))

  # --- Export ------------------------------------------------------------------
  unlink(destdir, recursive = TRUE)
  shinylive::export(appdir = staging, destdir = destdir)

  # --- Sanity check -------------------------------------------------------------
  index <- file.path(destdir, "index.html")
  if (!file.exists(index)) stop("Export finished but _site/index.html is missing.",
                                call. = FALSE)
  n <- length(list.files(destdir, recursive = TRUE))
  size_mb <- sum(file.info(list.files(destdir, recursive = TRUE,
                                      full.names = TRUE))$size) / 1024^2
  cat(sprintf("Shinylive site written to _site/ (%d files, %.1f MB).\n", n, size_mb))
}

main()
