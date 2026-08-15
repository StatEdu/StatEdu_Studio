`%||%` <- function(x, y) if (is.null(x)) y else x
structural_canvas_name <- function(node) as.character(node$name %||% node$id %||% "")
source(file.path("R", "setup_custom_model_canvas_structural_engine.R"), local = TRUE, encoding = "UTF-8")

unspecified <- list(nodes = list(
  list(id = "u1", name = "Unspecified1", role = "latent", constructType = "unspecified", measurementMode = "reflective"),
  list(id = "u2", name = "Unspecified2", role = "latent", constructType = "unspecified", measurementMode = "formative")
))
for (analysis_type in c("cfa", "cbsem", "sem", "plssem")) {
  blocked <- tryCatch({
    structural_canvas_validate_construct_specification(unspecified, analysis_type)
    FALSE
  }, error = function(error) {
    grepl("Every latent construct must be specified", conditionMessage(error), fixed = TRUE) &&
      grepl("Unspecified1", conditionMessage(error), fixed = TRUE) &&
      grepl("Unspecified2", conditionMessage(error), fixed = TRUE)
  })
  stopifnot(blocked)
}

legacy <- list(nodes = list(
  list(id = "f", name = "LegacyFactor", role = "latent", measurementMode = "reflective"),
  list(id = "c", name = "LegacyComposite", role = "latent", measurementMode = "formative")
))
legacy_specification <- structural_canvas_construct_specification(legacy)
stopifnot(legacy_specification$construct_type[[1L]] == "commonFactor")
stopifnot(legacy_specification$construct_type[[2L]] == "composite")
stopifnot(nrow(structural_canvas_validate_construct_specification(legacy, "plssem")) == 2L)

message("SEM unspecified-construct blocking validation passed.")
