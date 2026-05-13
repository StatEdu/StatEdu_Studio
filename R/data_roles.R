# Data helpers for variable role assignment and ordering.

normalize_role_choices <- function(
  choices,
  dependent = character(0),
  independent = character(0),
  controls = character(0),
  continuous = character(0)
) {
  choices <- as.character(choices)
  dependent <- intersect(intersect(as.character(dependent), choices), as.character(continuous))
  independent <- setdiff(intersect(as.character(independent), choices), dependent)
  controls <- setdiff(intersect(as.character(controls), choices), c(dependent, independent))
  list(
    dependent = dependent,
    independent = independent,
    controls = controls
  )
}

active_role_assignment <- function(
  role,
  names,
  selected,
  dependent = character(0),
  independent = character(0),
  controls = character(0)
) {
  names <- intersect(as.character(names), as.character(selected))
  dependent <- as.character(dependent)
  independent <- as.character(independent)
  controls <- as.character(controls)

  switch(
    role,
    independent = list(
      dependent = setdiff(dependent, names),
      independent = names,
      controls = setdiff(controls, names)
    ),
    control = list(
      dependent = setdiff(dependent, names),
      independent = setdiff(independent, names),
      controls = names
    ),
    list(
      dependent = names,
      independent = setdiff(independent, names),
      controls = setdiff(controls, names)
    )
  )
}

active_role_names_for_role <- function(role, dependent = character(0), independent = character(0), controls = character(0)) {
  switch(
    role,
    independent = as.character(independent),
    control = as.character(controls),
    as.character(dependent)
  )
}

assigned_elsewhere_for_role <- function(role, dependent = character(0), independent = character(0), controls = character(0)) {
  switch(
    role,
    independent = unique(c(dependent, controls)),
    control = unique(c(dependent, independent)),
    unique(c(independent, controls))
  )
}

role_for_variable <- function(name, dependent = character(0), independent = character(0), controls = character(0)) {
  if (name %in% dependent) return("dependent")
  if (name %in% independent) return("independent")
  if (name %in% controls) return("covariate")
  "exclude"
}

valid_variable_role <- function(role) {
  as.character(role %||% "") %in% c("dependent", "independent", "control")
}

role_assignment_validation <- function(dependent = character(0), independent = character(0)) {
  if (length(dependent) == 0) {
    return("Select one dependent variable before applying roles.")
  }
  if (length(independent) == 0) {
    return("Select at least one independent variable before applying roles.")
  }
  NULL
}

variable_selection_state <- function(selected = character(0), available = character(0)) {
  selected <- as.character(selected %||% character(0))
  available <- as.character(available %||% character(0))
  if (length(selected) == 0) {
    return(list(ok = FALSE, selected = character(0), message = "Select at least one variable to keep."))
  }
  list(ok = TRUE, selected = selected[selected %in% available], message = NULL)
}

dependent_variable_candidates <- function(dependent = character(0), selected = character(0)) {
  intersect(as.character(dependent), as.character(selected))
}

predictor_variable_candidates <- function(independent = character(0), controls = character(0), selected = character(0)) {
  selected <- as.character(selected)
  unique(c(
    intersect(as.character(controls), selected),
    intersect(as.character(independent), selected)
  ))
}

ordered_existing_then_new <- function(current = character(0), candidates = character(0)) {
  candidates <- as.character(candidates)
  c(intersect(as.character(current), candidates), setdiff(candidates, as.character(current)))
}

reconcile_order_with_candidates <- function(current = character(0), candidates = character(0), append_missing = TRUE) {
  current <- as.character(current %||% character(0))
  candidates <- as.character(candidates %||% character(0))
  if (isTRUE(append_missing)) {
    return(ordered_existing_then_new(current, candidates))
  }
  intersect(current, candidates)
}

selected_order_item <- function(selected = NULL, order = character(0)) {
  order <- as.character(order %||% character(0))
  selected <- as.character(selected %||% utils::head(order, 1))
  selected <- intersect(selected, order)
  utils::head(selected, 1)
}

ordered_predictor_candidates <- function(
  current = character(0),
  candidates = character(0),
  initialize = FALSE
) {
  candidates <- as.character(candidates)
  ordered <- intersect(as.character(current), candidates)
  if (isTRUE(initialize)) {
    ordered <- c(ordered, setdiff(candidates, ordered))
  }
  ordered
}

