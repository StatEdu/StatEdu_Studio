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
