if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]])) {
  validation_locale <- Sys.setlocale("LC_ALL", "Korean_Korea.utf8")
  if (is.na(validation_locale) || !isTRUE(l10n_info()[["UTF-8"]])) {
    stop("PLS fit benchmark requires a Windows UTF-8 locale; Korean_Korea.utf8 could not be activated.")
  }
}

source(file.path("R", "utils.R"), encoding = "UTF-8")
suppressPackageStartupMessages(library(shiny))
source(file.path("R", "setup_custom_model_canvas_structural_render_fit.R"), encoding = "UTF-8")

if (!requireNamespace("cSEM", quietly = TRUE)) {
  message("SKIP: cSEM is not installed; the package-level PLS fit benchmark was not run.")
  quit(status = 0L, save = "no")
}

observed <- matrix(c(
  1.00, 0.42, 0.23, 0.18,
  0.42, 1.00, 0.31, 0.21,
  0.23, 0.31, 1.00, 0.37,
  0.18, 0.21, 0.37, 1.00
), 4L, 4L, byrow = TRUE)
implied <- matrix(c(
  1.00, 0.38, 0.19, 0.16,
  0.38, 1.00, 0.27, 0.18,
  0.19, 0.27, 1.00, 0.33,
  0.16, 0.18, 0.33, 1.00
), 4L, 4L, byrow = TRUE)

statedu <- structural_canvas_pls_matrix_fit_indices(observed, implied)
csem_srmr <- getFromNamespace("calculateSRMR", "cSEM")(.matrix1 = observed, .matrix2 = implied)
csem_dg <- getFromNamespace("calculateDG", "cSEM")(.matrix1 = observed, .matrix2 = implied)
csem_duls <- getFromNamespace("calculateDL", "cSEM")(.matrix1 = observed, .matrix2 = implied)

stopifnot(
  isTRUE(all.equal(unname(statedu[["srmr"]]), unname(csem_srmr), tolerance = 1e-12)),
  isTRUE(all.equal(unname(statedu[["d_g"]]), unname(csem_dg), tolerance = 1e-12)),
  isTRUE(all.equal(unname(statedu[["d_uls"]]), unname(csem_duls), tolerance = 1e-12))
)

cat("PLS fit diagnostics match cSEM matrix functions.\n")
