# Data helpers for regression setup variable lists and ordering.

regression_variable_table_data <- function(
  selected,
  info = NULL,
  label_overrides = character(0),
  dependent = character(0),
  independent = character(0),
  controls = character(0)
) {
  selected <- as.character(selected)
  selected <- selected[nzchar(selected)]
  if (length(selected) == 0) {
    return(NULL)
  }

  labels <- stats::setNames(rep("", length(selected)), selected)
  measurements <- stats::setNames(rep("", length(selected)), selected)

  if (!is.null(info) && all(c("name", "var_label", "measurement") %in% names(info))) {
    matched <- info$name %in% selected
    labels[info$name[matched]] <- as.character(info$var_label[matched])
    measurements[info$name[matched]] <- as.character(info$measurement[matched])
  }

  if (length(label_overrides) > 0 && !is.null(names(label_overrides))) {
    matched <- selected %in% names(label_overrides)
    labels[selected[matched]] <- as.character(label_overrides[selected[matched]])
  }

  output <- data.frame(
    name = selected,
    var_label = unname(labels[selected]),
    role = vapply(
      selected,
      role_for_variable,
      character(1),
      dependent = dependent,
      independent = independent,
      controls = controls
    ),
    measurement = unname(measurements[selected]),
    source_order = seq_along(selected),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  role_order <- c(dependent = 1, independent = 2, covariate = 3, exclude = 4)
  output$role_order <- unname(role_order[output$role])
  output$role_order[is.na(output$role_order)] <- 99
  output <- output[order(output$role_order, output$source_order), , drop = FALSE]

  role_labels <- c(
    dependent = "Dependent",
    independent = "Independent",
    covariate = "Covariate",
    exclude = "Unassigned"
  )
  output$role <- unname(role_labels[output$role])
  output$role[is.na(output$role)] <- "Unassigned"
  output[, c("name", "var_label", "role", "measurement"), drop = FALSE]
}

current_regression_variable_table <- function(
  selected,
  step4_info = NULL,
  fallback_info = NULL,
  label_overrides = character(0),
  dependent = character(0),
  independent = character(0),
  controls = character(0)
) {
  info <- step4_info
  if (is.null(info)) {
    info <- fallback_info
  }
  regression_variable_table_data(
    selected,
    info = info,
    label_overrides = label_overrides,
    dependent = dependent,
    independent = independent,
    controls = controls
  )
}

move_order_item <- function(order, selected, direction = c("up", "down")) {
  direction <- match.arg(direction)
  order <- as.character(order %||% character(0))
  selected <- utils::head(as.character(selected %||% character(0)), 1)
  index <- match(selected, order)

  if (length(selected) == 0 || is.na(index)) {
    return(list(order = order, selected = selected, changed = FALSE))
  }
  if (identical(direction, "up") && index <= 1) {
    return(list(order = order, selected = selected, changed = FALSE))
  }
  if (identical(direction, "down") && index >= length(order)) {
    return(list(order = order, selected = selected, changed = FALSE))
  }

  swap_index <- if (identical(direction, "up")) index - 1 else index + 1
  order[c(index, swap_index)] <- order[c(swap_index, index)]
  list(order = order, selected = selected, changed = TRUE)
}

append_order_items <- function(order, items) {
  order <- as.character(order %||% character(0))
  items <- as.character(items %||% character(0))
  items <- items[nzchar(items)]
  next_order <- c(order, setdiff(items, order))
  list(
    order = next_order,
    selected = utils::head(items, 1),
    changed = !identical(order, next_order)
  )
}

remove_order_items <- function(order, items) {
  order <- as.character(order %||% character(0))
  items <- as.character(items %||% character(0))
  items <- items[nzchar(items)]
  next_order <- setdiff(order, items)
  list(
    order = next_order,
    selected = utils::head(next_order, 1),
    changed = !identical(order, next_order)
  )
}
