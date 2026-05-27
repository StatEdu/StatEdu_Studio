# Settings helpers for easyflow_statistics.

settings_vector <- function(x) {
  if (is.null(x)) return(character(0))
  as.character(unlist(x, use.names = FALSE))
}

settings_scalar <- function(x) {
  values <- settings_vector(x)
  if (length(values) == 0) return("")
  values[[1]]
}

settings_named_vector <- function(x) {
  if (is.null(x)) {
    return(character(0))
  }

  clean_setting_names <- function(names) {
    names <- as.character(names %||% character(0))
    for (unused in seq_len(4)) {
      names <- sub("^(var_labels|values|var_label_overrides|measurements)\\.", "", names)
    }
    names
  }

  keep_named_values <- function(values) {
    names(values) <- trimws(clean_setting_names(names(values)))
    values[!is.na(names(values)) & nzchar(names(values))]
  }

  if (is.data.frame(x)) {
    if (all(c("name", "value") %in% names(x))) {
      values <- stats::setNames(as.character(x$value), as.character(x$name))
      return(keep_named_values(values))
    }
    if (ncol(x) == 1) {
      values <- as.character(x[[1]])
      names(values) <- rownames(x)
      return(keep_named_values(values))
    }
  }
  values <- unlist(x, use.names = TRUE)
  if (length(values) == 0) {
    return(character(0))
  }
  values <- as.character(values)
  keep_named_values(values)
}

settings_name_value_pairs <- function(x) {
  if (is.null(x)) {
    return(character(0))
  }

  if (is.data.frame(x) && all(c("name", "value") %in% names(x))) {
    values <- stats::setNames(as.character(x$value), as.character(x$name))
    return(values[!is.na(names(values)) & nzchar(names(values))])
  }

  if (is.list(x) && all(c("name", "value") %in% names(x))) {
    names_vec <- settings_vector(x$name)
    values_vec <- settings_vector(x$value)
    length_out <- min(length(names_vec), length(values_vec))
    if (length_out > 0) {
      values <- stats::setNames(as.character(values_vec[seq_len(length_out)]), as.character(names_vec[seq_len(length_out)]))
      return(values[!is.na(names(values)) & nzchar(names(values))])
    }
  }

  if (is.list(x) && length(x) > 0) {
    rows <- lapply(x, function(row) {
      if (!is.list(row)) {
        return(NULL)
      }
      name <- settings_scalar(row$name %||% "")
      value <- settings_scalar(row$value %||% "")
      if (!nzchar(name)) {
        return(NULL)
      }
      stats::setNames(value, name)
    })
    rows <- rows[!vapply(rows, is.null, logical(1))]
    if (length(rows) > 0) {
      values <- unlist(rows, use.names = TRUE)
      return(values[!is.na(names(values)) & nzchar(names(values))])
    }
  }

  character(0)
}

settings_state_measurements <- function(state) {
  if (is.null(state)) {
    return(character(0))
  }
  values <- settings_name_value_pairs(state$measurement_pairs %||% NULL)
  if (length(values) == 0) {
    values <- settings_named_vector(state$measurements %||% character(0))
  }
  values
}

settings_external_data_path <- function(settings, settings_path = NULL) {
  if (is.null(settings_path) || !nzchar(settings_path)) {
    return("")
  }

  file_name <- basename(settings_scalar(settings$data_file %||% settings$embedded_data_file$name))
  if (!nzchar(file_name) || identical(file_name, ".") || !supported_data_file_extension(file_name)) {
    return("")
  }

  candidate <- file.path(dirname(settings_path), file_name)
  if (!valid_data_file_path(candidate)) {
    return("")
  }

  normalizePath(candidate, winslash = "/", mustWork = TRUE)
}

settings_embedded_data_file <- function(settings) {
  embedded <- settings$data_file_content_base64 %||% settings$embedded_data_file$content_base64
  if (is.null(embedded) || !nzchar(settings_scalar(embedded))) {
    return(NULL)
  }

  file_name <- settings_scalar(settings$data_file %||% settings$embedded_data_file$name)
  if (!nzchar(file_name)) {
    file_name <- "EasyFlow_Statistics_data"
  }
  extension <- tools::file_ext(file_name)
  if (!supported_data_file_extension(file_name)) {
    return(NULL)
  }
  restored_path <- tempfile("easyflow_data_", fileext = if (nzchar(extension)) paste0(".", extension) else "")
  writeBin(jsonlite::base64_dec(settings_scalar(embedded)), restored_path)
  list(path = restored_path, name = file_name, restored = TRUE)
}

settings_external_data_file <- function(settings, settings_path = NULL) {
  path <- settings_external_data_path(settings, settings_path)
  if (!nzchar(path)) {
    return(NULL)
  }
  list(path = path, name = basename(path), restored = TRUE)
}

settings_external_data_switch <- function(settings, settings_path = NULL, current_data_file = NULL) {
  settings_data_path <- settings_external_data_path(settings, settings_path)
  current_path <- if (is.null(current_data_file)) {
    ""
  } else {
    normalizePath(current_data_file$path, winslash = "/", mustWork = FALSE)
  }

  if (!nzchar(settings_data_path) || identical(settings_data_path, current_path)) {
    return(NULL)
  }

  list(path = settings_data_path, name = basename(settings_data_path), restored = TRUE)
}

settings_restored_data_file <- function(settings, settings_path = NULL) {
  external <- settings_external_data_file(settings, settings_path)
  if (!is.null(external)) {
    return(external)
  }
  settings_embedded_data_file(settings)
}

settings_variable_info <- function(settings) {
  info <- settings$data_variable_info
  if (is.data.frame(info) && "name" %in% names(info)) {
    return(info)
  }

  variables <- settings_vector(settings$data_variables)
  if (length(variables) == 0) {
    return(NULL)
  }

  data.frame(
    source_order = seq_along(variables),
    name = variables,
    var_label = "",
    measurement = "",
    storage_type = "",
    n_unique = "",
    n_missing = "",
    min_value = "",
    max_value = "",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

settings_measurement_overrides <- function(settings) {
  info <- settings_variable_info(settings)
  saved <- character(0)
  if (is.data.frame(info) && all(c("name", "measurement") %in% names(info))) {
    saved <- stats::setNames(as.character(info$measurement), as.character(info$name))
    saved <- saved[nzchar(names(saved)) & saved %in% c("binary", "category", "ordered", "continuous")]
  }

  category_measurements <- settings_category_measurements(settings$category_value_labels %||% NULL)
  if (length(category_measurements) > 0) {
    saved[names(category_measurements)] <- category_measurements
  }

  overrides <- settings_named_vector(settings$measurement_overrides)
  overrides <- overrides[nzchar(names(overrides)) & overrides %in% c("binary", "category", "ordered", "continuous")]
  if (length(overrides) > 0) {
    names(overrides) <- sub("^.*\\.", "", names(overrides))
  }
  saved[names(overrides)] <- overrides
  if (length(category_measurements) > 0) {
    saved[names(category_measurements)] <- category_measurements
  }
  saved
}

settings_category_measurements <- function(table) {
  if (!is.data.frame(table) || !all(c("name", "measurement") %in% names(table))) {
    return(character(0))
  }
  measurements <- stats::setNames(as.character(table$measurement), as.character(table$name))
  measurements <- measurements[nzchar(names(measurements)) & measurements %in% c("binary", "category", "ordered")]
  measurements
}

settings_restore_state <- function(settings) {
  selected <- settings_vector(settings$selected_variables)
  dependent <- settings_vector(settings$dependent_variables %||% settings$dependent)
  independent <- settings_vector(settings$independent_variables %||% settings$independent)
  controls <- settings_vector(settings$control_variables %||% settings$covariates)

  labels <- settings_named_vector(settings$var_label_overrides)
  if (length(labels) == 0) {
    info <- settings_variable_info(settings)
    if (is.data.frame(info) && all(c("name", "var_label") %in% names(info))) {
      labels <- stats::setNames(as.character(info$var_label), as.character(info$name))
    }
  }
  labels <- labels[nzchar(names(labels)) & nzchar(trimws(as.character(labels)))]

  info <- settings_variable_info(settings)
  measurements <- settings_measurement_overrides(settings)
  if (is.data.frame(info) && "name" %in% names(info)) {
    info <- apply_measurement_overrides(info, measurements)
    info <- apply_var_label_overrides_to_info(info, labels)
  } else {
    info <- NULL
  }

  category_labels <- settings$category_value_labels %||% NULL
  if (!is.data.frame(category_labels)) {
    category_labels <- NULL
  }

  data_step <- settings$data_step %||% ""
  list(
    selected = selected,
    dependent = dependent,
    independent = independent,
    controls = controls,
    var_labels = labels,
    variable_info = info,
    measurement_overrides = measurements,
    category_labels = category_labels,
    selection_applied = isTRUE(settings$selection_applied) ||
      data_step %in% c("review_selected_variables", "category_labels"),
    roles_applied = isTRUE(settings$roles_applied) ||
      data_step %in% c("category_labels")
  )
}

settings_stage_info_state <- function(
  selected = character(0),
  saved_info = NULL,
  fallback_info = NULL,
  selection_applied = FALSE,
  roles_applied = FALSE,
  has_roles = FALSE
) {
  selected <- as.character(selected %||% character(0))
  applied <- isTRUE(selection_applied) && length(selected) > 0
  info <- if (is.null(saved_info)) fallback_info else saved_info
  step3_info <- NULL
  if (isTRUE(applied) && is.data.frame(info) && "name" %in% names(info)) {
    step3_info <- info[info$name %in% selected, , drop = FALSE]
  }

  list(
    selection_applied = applied,
    roles_applied = isTRUE(roles_applied) || isTRUE(has_roles),
    step3_info = step3_info
  )
}

settings_navigation_state <- function(settings) {
  step <- NULL
  view <- NULL

  if (!is.null(settings$active_step) && settings$active_step %in% c("step1", "step2", "step3")) {
    step <- settings$active_step
    view <- if (identical(settings$active_step, "step3")) "labels" else settings$data_view %||% "info"
  }

  if (!is.null(settings$data_view) && settings$data_view %in% c("info", "preview") && !identical(step, "step3")) {
    view <- settings$data_view
  }

  list(active_step = step, data_view = view)
}

prepare_settings_payload_data <- function(
  variable_info = NULL,
  measurement_overrides = character(0),
  direct_measurements = character(0),
  dependent_variables = character(0),
  var_label_overrides = character(0),
  direct_var_labels = character(0),
  category_table = NULL,
  category_labels = NULL
) {
  overrides <- measurement_overrides %||% character(0)
  if (length(direct_measurements) > 0) {
    overrides[names(direct_measurements)] <- direct_measurements
  }

  category_measurements <- settings_category_measurements(category_table)
  if (length(category_measurements) > 0) {
    overrides[names(category_measurements)] <- category_measurements
  }

  dependent <- as.character(dependent_variables %||% character(0))
  if (length(dependent) > 0) {
    overrides[dependent] <- "continuous"
  }

  if (!is.null(variable_info)) {
    variable_info <- apply_measurement_overrides(variable_info, overrides)
  }

  labels <- var_label_overrides %||% character(0)
  if (length(direct_var_labels) > 0) {
    labels[names(direct_var_labels)] <- direct_var_labels
  }

  category_labels_named <- category_var_label_lookup_static(category_table)
  if (length(category_labels_named) > 0) {
    labels[names(category_labels_named)] <- category_labels_named
  }

  labels <- labels[!is.na(names(labels)) & nzchar(names(labels)) & nzchar(trimws(as.character(labels)))]
  if (!is.null(variable_info) && length(labels) > 0 && "name" %in% names(variable_info)) {
    matched <- variable_info$name %in% names(labels)
    variable_info$var_label[matched] <- unname(labels[variable_info$name[matched]])
  }

  if (is.null(category_labels) || "Message" %in% names(category_labels)) {
    category_labels <- category_table
  }

  list(
    variable_info = variable_info,
    measurement_overrides = overrides,
    var_label_overrides = labels,
    category_labels = category_labels
  )
}

build_settings_object <- function(
  app_version,
  data_step,
  active_step,
  data_view,
  data_file,
  variable_info,
  measurement_overrides,
  var_label_overrides,
  selection_applied,
  roles_applied,
  filter_names,
  filter_condition,
  dependent_variables,
  independent_variables,
  control_variables,
  dependent_order,
  predictor_order,
  category_labels,
  selected_variables,
  bootstrap_resamples,
  seed
) {
  variable_names <- if (is.null(variable_info)) character(0) else as.character(variable_info$name)
  list(
    app = "easyflow_statistics",
    version = app_version,
    data_step = data_step,
    active_step = active_step,
    data_view = data_view,
    data_file = data_file,
    data_variables = I(variable_names),
    data_variable_info = I(if (is.null(variable_info)) list() else variable_info),
    measurement_overrides = as.list(measurement_overrides),
    var_label_overrides = as.list(var_label_overrides),
    selection_applied = isTRUE(selection_applied),
    roles_applied = isTRUE(roles_applied),
    id = I(character(0)),
    filter = I(as.character(filter_names)),
    filter_condition = filter_condition %||% "",
    dependent_variables = I(as.character(dependent_variables)),
    independent_variables = I(as.character(independent_variables)),
    control_variables = I(as.character(control_variables)),
    dependent = I(as.character(dependent_variables)),
    independent = I(as.character(independent_variables)),
    covariates = I(as.character(control_variables)),
    dependent_order = I(as.character(dependent_order)),
    predictor_order = I(as.character(predictor_order)),
    category_value_labels = I(if (is.null(category_labels)) list() else category_labels),
    selected_variables = I(as.character(selected_variables %||% character(0))),
    bootstrap_resamples = as.integer(bootstrap_resamples %||% 1000),
    seed = seed %||% default_seed()
  )
}

prepare_current_settings_object <- function(
  app_version,
  data_step,
  active_step,
  data_view,
  data_file,
  variable_info = NULL,
  measurement_overrides = character(0),
  direct_measurements = character(0),
  dependent_variables = character(0),
  var_label_overrides = character(0),
  direct_var_labels = character(0),
  category_table = NULL,
  category_labels = NULL,
  selection_applied = FALSE,
  roles_applied = FALSE,
  filter_names = character(0),
  filter_condition = "",
  independent_variables = character(0),
  control_variables = character(0),
  dependent_order = character(0),
  predictor_order = character(0),
  selected_variables = character(0),
  bootstrap_resamples = 1000,
  seed = NULL
) {
  payload <- prepare_settings_payload_data(
    variable_info = variable_info,
    measurement_overrides = measurement_overrides,
    direct_measurements = direct_measurements,
    dependent_variables = dependent_variables,
    var_label_overrides = var_label_overrides,
    direct_var_labels = direct_var_labels,
    category_table = category_table,
    category_labels = category_labels
  )

  list(
    settings = build_settings_object(
      app_version = app_version,
      data_step = data_step,
      active_step = active_step,
      data_view = data_view,
      data_file = data_file,
      variable_info = payload$variable_info,
      measurement_overrides = payload$measurement_overrides,
      var_label_overrides = payload$var_label_overrides,
      selection_applied = selection_applied,
      roles_applied = roles_applied,
      filter_names = filter_names,
      filter_condition = filter_condition,
      dependent_variables = dependent_order,
      independent_variables = independent_variables,
      control_variables = control_variables,
      dependent_order = dependent_order,
      predictor_order = predictor_order,
      category_labels = payload$category_labels,
      selected_variables = selected_variables,
      bootstrap_resamples = bootstrap_resamples,
      seed = seed
    ),
    payload = payload
  )
}

current_settings_variable_info <- function(
  step3_info = NULL,
  restored_info = NULL,
  current_file = NULL,
  dataset = NULL,
  input = NULL,
  raw_data = NULL
) {
  variable_info <- step3_info
  if (is.null(variable_info)) {
    variable_info <- if (is.null(current_file)) {
      restored_info
    } else {
      variable_summary_table(dataset, input, raw_data)
    }
  }
  variable_info
}

create_current_settings_fn <- function(
  app_version,
  input,
  current_data_file_fn,
  current_data_step_fn,
  active_step_fn,
  data_view_fn,
  step3_variable_info_fn,
  restored_variable_info_fn,
  restored_data_file_fn,
  dataset_fn,
  raw_dataset_fn,
  measurement_overrides,
  var_label_overrides,
  collect_measurement_inputs_fn,
  collect_var_label_inputs_fn,
  dependent_names_fn,
  independent_names_fn,
  control_names_fn,
  category_label_values_fn,
  category_label_table_data_fn,
  selection_applied_fn,
  roles_applied_fn,
  filter_names_fn,
  sync_dependent_order_fn,
  sync_predictor_order_fn,
  selected_names_fn
) {
  function() {
    file <- current_data_file_fn()
    variable_info <- current_settings_variable_info(
      step3_info = step3_variable_info_fn(),
      restored_info = restored_variable_info_fn(),
      current_file = file,
      dataset = if (is.null(file)) NULL else dataset_fn(),
      input = input,
      raw_data = if (is.null(file)) NULL else raw_dataset_fn()
    )

    prepared <- prepare_current_settings_object(
      app_version = app_version,
      data_step = current_data_step_fn(),
      active_step = active_step_fn(),
      data_view = data_view_fn(),
      data_file = if (is.null(file)) restored_data_file_fn() else file$name,
      variable_info = variable_info,
      measurement_overrides = measurement_overrides(),
      direct_measurements = character(0),
      dependent_variables = dependent_names_fn(),
      var_label_overrides = var_label_overrides(),
      direct_var_labels = character(0),
      category_table = category_label_values_fn(),
      category_labels = category_label_table_data_fn(),
      selection_applied = selection_applied_fn(),
      roles_applied = roles_applied_fn(),
      filter_names = filter_names_fn(),
      filter_condition = input$filter_condition %||% "",
      independent_variables = independent_names_fn(),
      control_variables = control_names_fn(),
      dependent_order = sync_dependent_order_fn(update_input = FALSE),
      predictor_order = sync_predictor_order_fn(update_input = FALSE),
      selected_variables = selected_names_fn(),
      bootstrap_resamples = input$boot_r %||% 1000,
      seed = input$seed %||% default_seed()
    )
    measurement_overrides(prepared$payload$measurement_overrides)
    var_label_overrides(prepared$payload$var_label_overrides)
    prepared$settings
  }
}

write_settings_json_file <- function(settings, path) {
  if (is.null(path) || !nzchar(path)) {
    stop("A settings file path is required.", call. = FALSE)
  }
  writeLines(
    as.character(jsonlite::toJSON(settings, pretty = TRUE, auto_unbox = TRUE)),
    con = path,
    useBytes = TRUE
  )
  invisible(list(
    path = path,
    var_label_count = length(settings$var_label_overrides %||% list())
  ))
}
