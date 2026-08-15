`%||%` <- function(x, y) if (is.null(x)) y else x
structural_canvas_name <- function(node) as.character(node$name %||% node$id %||% "")
source(file.path("R", "setup_custom_model_canvas_structural_engine.R"), local = TRUE, encoding = "UTF-8")

factor_snapshot <- list(nodes = list(
  list(id = "f1", name = "Factor1", role = "latent", constructType = "commonFactor", measurementMode = "reflective"),
  list(id = "f2", name = "Factor2", role = "latent", constructType = "commonFactor", measurementMode = "reflective")
))
stopifnot(nrow(structural_canvas_validate_plsc_specification(factor_snapshot, "plssem", "PLSc")) == 2L)
stopifnot(is.null(structural_canvas_validate_plsc_specification(factor_snapshot, "plssem", "PLS")))

invalid_snapshots <- list(
  composite = list(nodes = list(list(id = "c", name = "Composite", role = "latent", constructType = "composite", measurementMode = "reflective"))),
  formative = list(nodes = list(list(id = "c", name = "Formative", role = "latent", constructType = "composite", measurementMode = "formative"))),
  unspecified = list(nodes = list(list(id = "u", name = "Unspecified", role = "latent", constructType = "unspecified", measurementMode = "reflective"))),
  mixed = list(nodes = c(factor_snapshot$nodes, list(list(id = "c", name = "Composite", role = "latent", constructType = "composite", measurementMode = "reflective"))))
)
for (snapshot in invalid_snapshots) {
  blocked <- tryCatch({
    structural_canvas_validate_plsc_specification(snapshot, "plssem", "PLSc")
    FALSE
  }, error = function(error) grepl("only when every construct", conditionMessage(error), fixed = TRUE))
  stopifnot(blocked)
}

message("SEM PLSc scope validation passed.")
