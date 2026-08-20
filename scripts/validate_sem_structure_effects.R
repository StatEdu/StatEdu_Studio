`%||%` <- function(x, y) if (is.null(x)) y else x
source("R/setup_custom_model_canvas_structural_engine.R", local = TRUE)

empty_snapshot <- list(nodes = list(), edges = list(), moderations = list())
cb_plan <- structural_canvas_structural_effect_plan(empty_snapshot, "cbsem", "MLR")
stopifnot(nrow(cb_plan) == 4L, cb_plan$Status[cb_plan$Effect == "Moderation"] == "Not requested")

moderated_snapshot <- empty_snapshot
moderated_snapshot$moderations <- list(list(from = "m1", toEdge = "e1"))
cb_moderated <- structural_canvas_structural_effect_plan(moderated_snapshot, "cbsem", "MLR")
stopifnot(cb_moderated$Status[cb_moderated$Effect == "Moderation"] == "Supported")

pls_plan <- structural_canvas_structural_effect_plan(moderated_snapshot, "plssem", "PLS")
stopifnot(all(pls_plan$Status[pls_plan$Effect %in% c("Moderation", "Moderated mediation")] == "Blocked"))
blocked <- tryCatch({
  structural_canvas_validate_structural_effects(moderated_snapshot, "plssem", "PLS")
  FALSE
}, error = function(error) grepl("does not estimate canvas moderation", conditionMessage(error), fixed = TRUE))
stopifnot(blocked)

message("SEM structure-effects validation passed.")
