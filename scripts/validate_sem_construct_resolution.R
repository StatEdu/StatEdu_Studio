source(file.path("scripts", "validate_cfa_common.R"), encoding = "UTF-8")

snapshot_for <- function(construct_type, measurement_mode, weighting_mode = "auto") list(
  nodes = list(list(
    id = "latent_1", role = "latent", name = "eta",
    constructType = construct_type, measurementMode = measurement_mode,
    weightingMode = weighting_mode
  )),
  edges = list()
)

cfa_factor <- structural_canvas_resolve_construct_specification(snapshot_for("commonFactor", "reflective"), "cfa", "ML")
pls_factor <- structural_canvas_resolve_construct_specification(snapshot_for("commonFactor", "reflective"), "plssem", "PLS")
plsc_factor <- structural_canvas_resolve_construct_specification(snapshot_for("commonFactor", "reflective"), "plssem", "PLSc")
pls_reflective_composite <- structural_canvas_resolve_construct_specification(snapshot_for("composite", "reflective"), "plssem", "PLS")
pls_formative_composite <- structural_canvas_resolve_construct_specification(snapshot_for("composite", "formative"), "plssem", "PLS")
plsc_formative_composite <- structural_canvas_resolve_construct_specification(snapshot_for("composite", "formative"), "plssem", "PLSc")
factor_formative <- structural_canvas_resolve_construct_specification(snapshot_for("commonFactor", "formative"), "plssem", "PLS")
reflective_mode_b <- structural_canvas_resolve_construct_specification(snapshot_for("composite", "reflective", "modeB"), "plssem", "PLS")
unsupported_equal <- structural_canvas_resolve_construct_specification(snapshot_for("composite", "formative", "sum"), "plssem", "PLS")

stopifnot(
  isTRUE(cfa_factor$supported),
  identical(cfa_factor$effective_weighting, "Not applicable"),
  grepl("explicit measurement error", cfa_factor$estimand, fixed = TRUE),
  isTRUE(pls_factor$supported),
  identical(pls_factor$effective_weighting, "Mode A"),
  grepl("score proxy", pls_factor$estimand, fixed = TRUE),
  isTRUE(plsc_factor$supported),
  grepl("Consistency-corrected", plsc_factor$estimand, fixed = TRUE),
  isTRUE(pls_reflective_composite$supported),
  identical(pls_reflective_composite$estimand, "Reflective Mode A composite"),
  isTRUE(pls_formative_composite$supported),
  identical(pls_formative_composite$effective_weighting, "Mode B"),
  identical(pls_formative_composite$estimand, "Formative composite"),
  isTRUE(plsc_formative_composite$supported),
  identical(plsc_formative_composite$effective_weighting, "Mode B"),
  identical(plsc_formative_composite$engine_representation, "seminr Mode B composite"),
  identical(plsc_formative_composite$estimand, "Formative composite"),
  !isTRUE(factor_formative$supported),
  !isTRUE(reflective_mode_b$supported),
  !isTRUE(unsupported_equal$supported),
  grepl("not implemented", unsupported_equal$reason, fixed = TRUE)
)

cat("SEM construct-resolution validations passed.\n")
