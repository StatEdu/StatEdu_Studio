script_path <- tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE),
  error = function(e) "scripts/validate_custom_model_canvas.R"
)
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)

source(file.path(repo_root, "R", "utils.R"))
source(file.path(repo_root, "R", "setup_analysis_ui.R"))
source(file.path(repo_root, "R", "analysis_regression.R"))
source(file.path(repo_root, "R", "setup_mediation_moderation_ui.R"))
source(file.path(repo_root, "R", "setup_custom_model_canvas_ui.R"))

message("Checking custom model canvas snapshot-to-analysis maps...")

node <- function(id, variable, role, x, y) {
  list(id = id, variableId = variable, role = role, x = x, y = y)
}

edge <- function(id, from, to) {
  list(id = id, from = from, to = to)
}

moderation <- function(id, from, to_edge) {
  list(id = id, from = from, toEdge = to_edge)
}

snapshot <- list(
  nodes = list(
    node("x1", "X1", "independent", 80, 120),
    node("x2", "X2", "independent", 80, 220),
    node("m1", "M1", "mediator", 260, 120),
    node("m2", "M2", "mediator", 440, 120),
    node("y", "Y", "dependent", 620, 120),
    node("w", "W", "moderator", 260, 20),
    node("z", "Z", "moderator", 440, 20),
    node("unused_w", "UnusedW", "moderator", 620, 20)
  ),
  edges = list(
    edge("e_x1_m1", "x1", "m1"),
    edge("e_x2_m1", "x2", "m1"),
    edge("e_m1_m2", "m1", "m2"),
    edge("e_m2_y", "m2", "y"),
    edge("e_x1_y", "x1", "y")
  ),
  moderations = list(
    moderation("mod_xm", "w", "e_x1_m1"),
    moderation("mod_my", "z", "e_m2_y"),
    moderation("mod_xy", "w", "e_x1_y")
  ),
  covariates = c("C")
)

selected_names <- c("X1", "X2", "M1", "M2", "Y", "W", "Z", "UnusedW", "C")
spec <- custom_model_canvas_snapshot_spec(snapshot, selected_names)

stopifnot(identical(spec$roles$y, "Y"))
stopifnot(identical(spec$roles$x, c("X1", "X2")))
stopifnot(identical(spec$roles$mediators, c("M1", "M2")))
stopifnot(identical(spec$roles$w, c("W", "Z")))
stopifnot(identical(spec$roles$covariates, "C"))
stopifnot(identical(spec$mediator_arrangement, "serial"))

stopifnot(identical(spec$x_to_m$M1, c("X1", "X2")))
stopifnot(identical(spec$x_to_m$M2, character(0)))
stopifnot(identical(spec$m_to_m$M1, character(0)))
stopifnot(identical(spec$m_to_m$M2, "M1"))
stopifnot(identical(spec$m_to_y$Y, "M2"))
stopifnot(identical(spec$direct_x_to_y$Y, "X1"))

stopifnot(identical(spec$moderated_paths, c("xm", "my", "xy")))
stopifnot(identical(spec$moderated_x_to_m$M1, "X1"))
stopifnot(identical(spec$moderated_x_to_m$M2, character(0)))
stopifnot(identical(spec$moderated_m_to_y, "M2"))

expected_map <- data.frame(
  path_type = c("xm", "my", "xy"),
  moderator = c("W", "Z", "W"),
  x = c("X1", "", "X1"),
  mediator = c("M1", "M2", ""),
  y = c("", "Y", "Y"),
  stringsAsFactors = FALSE
)
stopifnot(identical(spec$moderation_map, expected_map))

message("Checking disconnected or invalid canvas records are ignored...")
invalid_snapshot <- snapshot
invalid_snapshot$edges <- c(
  invalid_snapshot$edges,
  list(edge("e_bad_xm", "x1", "missing_node"))
)
invalid_snapshot$moderations <- c(
  invalid_snapshot$moderations,
  list(moderation("mod_bad", "unused_w", "e_bad_xm"))
)
invalid_spec <- custom_model_canvas_snapshot_spec(invalid_snapshot, selected_names)
stopifnot(identical(invalid_spec$x_to_m, spec$x_to_m))
stopifnot(identical(invalid_spec$moderation_map, spec$moderation_map))
stopifnot(identical(invalid_spec$roles$w, c("W", "Z")))

message("All custom model canvas validations passed.")
