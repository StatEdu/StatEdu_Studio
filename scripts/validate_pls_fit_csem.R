if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]])) {
  validation_locale <- Sys.setlocale("LC_ALL", "Korean_Korea.utf8")
  if (is.na(validation_locale) || !isTRUE(l10n_info()[["UTF-8"]])) {
    stop("PLS fit benchmark requires a Windows UTF-8 locale; Korean_Korea.utf8 could not be activated.")
  }
}

source(file.path("R", "utils.R"), encoding = "UTF-8")
suppressPackageStartupMessages(library(shiny))
source(file.path("R", "setup_custom_model_canvas_structural_render_fit.R"), encoding = "UTF-8")

validation_mode <- tolower(trimws(Sys.getenv("STATEDU_CSEM_VALIDATION_MODE", "required")))
if (!validation_mode %in% c("required", "optional")) {
  stop(
    "STATEDU_CSEM_VALIDATION_MODE must be explicitly 'required' or 'optional'. Release and installer gates must use 'required'.",
    call. = FALSE
  )
}
csem_required <- identical(validation_mode, "required")
expected_csem_version <- "0.6.1"

if (!requireNamespace("cSEM", quietly = TRUE)) {
  if (csem_required) {
    stop(
      paste0(
        "RELEASE BLOCKER: cSEM ", expected_csem_version,
        " is unavailable in the selected R library paths. The required package-level PLS numerical validation cannot be skipped. ",
        "Install cSEM in an isolated validation library and include that library in R_LIBS_USER, or run this non-release suite with STATEDU_CSEM_VALIDATION_MODE=optional."
      ),
      call. = FALSE
    )
  }
  message("SKIP (explicit optional contract): cSEM is not installed; no package-level PLS equivalence claim was made.")
  quit(status = 0L, save = "no")
}

actual_csem_version <- as.character(utils::packageVersion("cSEM"))
if (!identical(actual_csem_version, expected_csem_version)) {
  stop(
    sprintf(
      "cSEM validation version contract failed: expected %s, found %s. Revalidate and deliberately update the pinned contract before release.",
      expected_csem_version, actual_csem_version
    ),
    call. = FALSE
  )
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

cat(
  "PLS fit diagnostics match cSEM matrix functions (cSEM ",
  actual_csem_version, ", required=", csem_required, ", tolerance 1e-12).\n",
  sep = ""
)
