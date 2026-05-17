# Auto-extracted shared functions for easyflow_statistics.

prepare_data <- function(data) {
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = TRUE)
  data[] <- lapply(data, function(x) {
    if (inherits(x, "haven_labelled_spss")) x <- haven::zap_missing(x)
    if (inherits(x, "haven_labelled")) x <- haven::zap_labels(x)
    if (is.character(x)) return(factor(x))
    x
  })
  data
}

read_sav_robust <- function(path) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop(
      "SPSS SAV files require the CRAN package 'haven'. Install it with install.packages(\"haven\").",
      call. = FALSE
    )
  }

  source_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  read_path <- source_path

  # Some Windows/R setups fail when haven reads an uploaded file from a
  # non-ASCII or extensionless temporary path. Copy to an ASCII .sav path first.
  tmp_path <- tempfile(pattern = "easyflow_sav_", fileext = ".sav")
  copied <- tryCatch(
    file.copy(source_path, tmp_path, overwrite = TRUE),
    error = function(e) FALSE
  )
  if (isTRUE(copied) && file.exists(tmp_path)) {
    read_path <- tmp_path
    on.exit(unlink(tmp_path), add = TRUE)
  }

  supports_encoding <- "encoding" %in% names(formals(haven::read_sav))
  encodings <- if (supports_encoding) {
    c(NA_character_, "UTF-8", "CP949", "EUC-KR", "latin1")
  } else {
    NA_character_
  }
  errors <- character(0)

  for (encoding in encodings) {
    encoding_label <- if (is.na(encoding)) "default" else encoding
    result <- tryCatch(
      {
        args <- list(file = read_path, user_na = TRUE)
        if (!is.na(encoding) && supports_encoding) {
          args$encoding <- encoding
        }
        do.call(haven::read_sav, args)
      },
      error = function(e) {
        errors <<- c(errors, sprintf("%s: %s", encoding_label, conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(result)) {
      return(result)
    }
  }

  stop(
    paste0(
      "Could not read the SPSS SAV file. Tried encodings: ",
      paste(ifelse(is.na(encodings), "default", encodings), collapse = ", "),
      ". Last errors: ",
      paste(utils::tail(errors, 3), collapse = " | ")
    ),
    call. = FALSE
  )
}

read_input_data <- function(path, original_name, csv_header = TRUE, dat_delimiter = "whitespace", dat_has_names = FALSE) {
  ext <- tolower(tools::file_ext(original_name))

  if (identical(ext, "sav")) {
    return(read_sav_robust(path))
  }

  if (identical(ext, "csv")) {
    return(readr::read_csv(path, col_names = csv_header, show_col_types = FALSE, progress = FALSE))
  }

  if (identical(ext, "dat")) {
    if (identical(dat_delimiter, "comma")) {
      return(readr::read_delim(path, delim = ",", col_names = dat_has_names, show_col_types = FALSE, progress = FALSE))
    }
    if (identical(dat_delimiter, "tab")) {
      return(readr::read_tsv(path, col_names = dat_has_names, show_col_types = FALSE, progress = FALSE))
    }
    return(readr::read_table(path, col_names = dat_has_names, show_col_types = FALSE, progress = FALSE))
  }

  stop("Unsupported file type: .", ext, call. = FALSE)
}

current_data_file_value <- function(uploaded, active_file = NULL) {
  if (!is.null(uploaded)) {
    return(list(path = uploaded$datapath, name = uploaded$name, restored = FALSE))
  }
  active_file
}

read_current_data_file <- function(file, input) {
  read_input_data(
    file$path,
    file$name,
    csv_header = input$header,
    dat_delimiter = input$dat_delimiter %||% "whitespace",
    dat_has_names = isTRUE(input$dat_has_names)
  )
}

infer_measurement <- function(x) {
  values <- stats::na.omit(as.vector(x))
  unique_n <- length(unique(values))

  if (is.logical(x)) return("binary")
  if (is.ordered(x)) return("ordered")
  if (is.factor(x)) return(if (nlevels(x) <= 2) "binary" else "category")
  if (is.character(x)) return(if (unique_n <= 2) "binary" else "category")
  if ((is.numeric(x) || is.integer(x)) && unique_n <= 2) return("binary")
  if ((is.numeric(x) || is.integer(x)) && unique_n <= 12) return("category")
  "continuous"
}

variable_min <- function(x) {
  values <- stats::na.omit(as.vector(x))
  if (length(values) == 0 || !(is.numeric(values) || is.integer(values))) return("")
  as.character(min(values))
}

variable_max <- function(x) {
  values <- stats::na.omit(as.vector(x))
  if (length(values) == 0 || !(is.numeric(values) || is.integer(values))) return("")
  as.character(max(values))
}

variable_label <- function(x) {
  label <- attr(x, "label", exact = TRUE)
  if (is.null(label) || length(label) == 0 || is.na(label[[1]])) return("")
  as.character(label[[1]])
}

value_label_pairs <- function(x, prepared_x = x, max_pairs = 6) {
  labelled_values <- attr(x, "labels", exact = TRUE)
  if (!is.null(labelled_values) && length(labelled_values) > 0) {
    values <- as.character(unname(labelled_values))
    labels <- names(labelled_values)
  } else {
    values <- sort(unique(stats::na.omit(as.vector(prepared_x))))
    values <- as.character(utils::head(values, max_pairs))
    labels <- rep("", length(values))
  }

  output <- stats::setNames(rep("", max_pairs * 2), as.vector(rbind(paste0("value_", seq_len(max_pairs)), paste0("label_", seq_len(max_pairs)))))
  if (length(values) > 0) {
    for (i in seq_len(min(length(values), max_pairs))) {
      output[[paste0("value_", i)]] <- values[[i]]
      output[[paste0("label_", i)]] <- labels[[i]] %||% ""
    }
  }
  output
}

variable_summary_table <- function(data, input, raw_data = data) {
  raw_data <- as.data.frame(raw_data, stringsAsFactors = FALSE, check.names = TRUE)
  labels <- vapply(seq_along(data), function(i) variable_label(raw_data[[i]]), character(1))
  value_labels <- do.call(rbind, lapply(seq_along(data), function(i) {
    as.data.frame(as.list(value_label_pairs(raw_data[[i]], data[[i]])), stringsAsFactors = FALSE, check.names = FALSE)
  }))
  cbind(data.frame(
    source_order = seq_along(data),
    name = names(data),
    var_label = labels,
    measurement = vapply(data, infer_measurement, character(1)),
    storage_type = vapply(data, function(x) class(x)[1], character(1)),
    n_unique = vapply(data, function(x) length(unique(stats::na.omit(as.vector(x)))), integer(1)),
    n_missing = vapply(data, function(x) sum(is.na(x)), integer(1)),
    min_value = vapply(data, variable_min, character(1)),
    max_value = vapply(data, variable_max, character(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  ), value_labels)
}

apply_measurement_overrides <- function(table_data, overrides = character(0)) {
  if (length(overrides) == 0 || is.null(table_data) || nrow(table_data) == 0) {
    return(table_data)
  }

  matched <- table_data$name %in% names(overrides)
  table_data$measurement[matched] <- unname(overrides[table_data$name[matched]])
  table_data
}

apply_var_label_overrides_to_info <- function(info, labels = character(0)) {
  if (is.null(info) || nrow(info) == 0 || length(labels) == 0) {
    return(info)
  }
  matched <- info$name %in% names(labels)
  info$var_label[matched] <- unname(labels[info$name[matched]])
  info
}

apply_variable_overrides <- function(info, measurement_overrides = character(0), var_label_overrides = character(0)) {
  info <- apply_measurement_overrides(info, measurement_overrides)
  apply_var_label_overrides_to_info(info, var_label_overrides)
}

base_variable_info_value <- function(
  has_data_file = FALSE,
  data = NULL,
  input = NULL,
  raw_data = NULL,
  restored_info = NULL,
  measurement_overrides = character(0),
  var_label_overrides = character(0)
) {
  info <- if (isTRUE(has_data_file)) {
    variable_summary_table(data, input, raw_data)
  } else {
    restored_info
  }
  apply_variable_overrides(info, measurement_overrides, var_label_overrides)
}

ensure_variable_info_columns <- function(info) {
  if (is.null(info)) {
    return(NULL)
  }
  required <- c("source_order", "name", "var_label", "measurement", "storage_type", "n_unique", "n_missing", "min_value", "max_value")
  for (column in required) {
    if (!column %in% names(info)) {
      info[[column]] <- ""
    }
  }
  info
}

select_variable_info_source <- function(data_view = "info", selection_applied = FALSE, step3_info = NULL, step4_info = NULL, base_info = NULL) {
  if (identical(data_view, "labels") && !is.null(step4_info)) {
    return(step4_info)
  }
  if (isTRUE(selection_applied) && !is.null(step3_info)) {
    return(step3_info)
  }
  base_info
}

prepare_variable_info_table <- function(info, labels = character(0)) {
  if (is.null(info)) {
    return(NULL)
  }
  info <- ensure_variable_info_columns(info)
  apply_var_label_overrides_to_info(info, labels)
}

variable_info_table_value <- function(
  data_view = "info",
  selection_applied = FALSE,
  step3_info = NULL,
  step4_info = NULL,
  base_info = NULL,
  measurement_overrides = character(0),
  labels = character(0)
) {
  info <- select_variable_info_source(
    data_view = data_view,
    selection_applied = selection_applied,
    step3_info = step3_info,
    step4_info = step4_info,
    base_info = base_info
  )
  info <- apply_measurement_overrides(info, measurement_overrides)
  prepare_variable_info_table(info, labels)
}

data_step_value <- function(has_data_file = FALSE, has_restored_info = FALSE, active_step = "step1", selection_applied = FALSE) {
  if (!isTRUE(has_data_file) && !isTRUE(has_restored_info)) {
    return("load_data")
  }
  if (identical(active_step, "step3") || identical(active_step, "step4")) {
    return("category_labels")
  }
  if (isTRUE(selection_applied)) {
    return("assign_variable_roles")
  }
  "select_analysis_variables"
}

continuous_names_from_info <- function(info) {
  if (is.null(info) || nrow(info) == 0 || !all(c("name", "measurement") %in% names(info))) {
    return(character(0))
  }
  as.character(info$name[info$measurement == "continuous"])
}

available_names_from_data_state <- function(data = NULL, restored_info = NULL, has_data_file = FALSE) {
  if (isTRUE(has_data_file) && !is.null(data)) {
    return(names(data))
  }
  if (is.null(restored_info) || !"name" %in% names(restored_info)) {
    return(character(0))
  }
  as.character(restored_info$name)
}

clean_measurement_overrides <- function(values) {
  updates <- settings_named_vector(values)
  if (length(updates) == 0 || is.null(names(updates))) {
    return(character(0))
  }
  names(updates) <- trimws(names(updates))
  valid_names <- !is.na(names(updates)) & nzchar(names(updates))
  valid_values <- !is.na(updates) & updates %in% c("binary", "category", "ordered", "continuous")
  updates <- updates[valid_names & valid_values]
  if (length(updates) > 0) {
    names(updates) <- sub("^.*\\.", "", names(updates))
    names(updates) <- trimws(names(updates))
    updates <- updates[!is.na(names(updates)) & nzchar(names(updates))]
  }
  updates
}

clean_var_label_overrides <- function(values, allow_blank = TRUE) {
  labels <- settings_named_vector(values)
  if (length(labels) == 0 || is.null(names(labels))) {
    return(character(0))
  }
  labels <- labels[!is.na(names(labels)) & nzchar(names(labels))]
  if (!isTRUE(allow_blank)) {
    labels <- labels[nzchar(trimws(as.character(labels)))]
  }
  labels
}

merge_named_overrides <- function(current = character(0), updates = character(0), drop_blank_names = TRUE) {
  current <- current %||% character(0)
  updates <- updates %||% character(0)

  if (isTRUE(drop_blank_names) && length(current) > 0) {
    current <- current[!is.na(names(current)) & nzchar(trimws(names(current)))]
  }
  if (length(updates) == 0 || is.null(names(updates))) {
    return(list(values = current, changed = FALSE, updates = character(0)))
  }

  updates <- updates[!is.na(names(updates)) & nzchar(trimws(names(updates)))]
  if (length(updates) == 0) {
    return(list(values = current, changed = FALSE, updates = character(0)))
  }

  changed <- vapply(names(updates), function(name) {
    !identical(named_value(current, name, ""), as.character(updates[[name]] %||% ""))
  }, logical(1))
  if (!any(changed, na.rm = TRUE)) {
    return(list(values = current, changed = FALSE, updates = updates))
  }

  current[names(updates)] <- updates
  list(values = current, changed = TRUE, updates = updates)
}

variable_info_state_updates <- function(
  state = NULL,
  direct_measurements = character(0),
  direct_labels = character(0)
) {
  measurements <- settings_state_measurements(state)
  if (length(direct_measurements) > 0) {
    measurements[names(direct_measurements)] <- direct_measurements
  }
  measurements <- measurements[measurements %in% c("binary", "category", "ordered", "continuous")]

  labels <- settings_named_vector(state$var_labels %||% character(0))
  if (length(direct_labels) > 0) {
    labels[names(direct_labels)] <- direct_labels
  }

  list(measurements = measurements, labels = labels)
}

needs_direct_measurement_inputs <- function(state = NULL) {
  is.null(state) || length(settings_state_measurements(state)) == 0
}

merge_variable_info_state <- function(
  info,
  measurement_overrides = character(0),
  var_label_overrides = character(0),
  state = NULL,
  direct_measurements = character(0),
  direct_labels = character(0),
  selected_names = character(0),
  selected_only = FALSE
) {
  if (is.null(info) || nrow(info) == 0) {
    return(list(info = info, measurements = character(0), labels = character(0)))
  }

  info <- apply_variable_overrides(info, measurement_overrides, var_label_overrides)

  updates <- variable_info_state_updates(state, direct_measurements, direct_labels)
  if (length(updates$measurements) > 0) {
    info <- apply_measurement_overrides(info, updates$measurements)
  }
  if (length(updates$labels) > 0) {
    info <- apply_var_label_overrides_to_info(info, updates$labels)
  }

  if (isTRUE(selected_only)) {
    info <- info[info$name %in% selected_names, , drop = FALSE]
  }

  list(info = info, measurements = updates$measurements, labels = updates$labels)
}

measurement_select_html <- function(name, value, source_order) {
  choices <- unique(c("binary", "category", "ordered", "continuous", value))
  choice_labels <- ifelse(choices == "ordered", "ordinal", choices)
  sprintf(
    paste0(
      '<select id="measurement_input_%s" class="measurement-select" data-name="%s" ',
      'onchange="if(window.Shiny){Shiny.setInputValue(&quot;variable_measurement_update&quot;,',
      '{name:this.getAttribute(&quot;data-name&quot;),value:this.value,nonce:Date.now()+Math.random()},',
      '{priority:&quot;event&quot;});}">%s</select>'
    ),
    htmltools::htmlEscape(source_order),
    htmltools::htmlEscape(name),
    paste(
      sprintf(
        '<option value="%s" %s>%s</option>',
        choices,
        ifelse(choices == value, "selected", ""),
        choice_labels
      ),
      collapse = ""
    )
  )
}

variable_table_display_data <- function(
  info,
  checked_names = character(0),
  selected_names = character(0),
  assigned_elsewhere = character(0),
  dependent = character(0),
  independent = character(0),
  controls = character(0),
  selection_applied = FALSE,
  active_role = "dependent",
  measurement_overrides = character(0)
) {
  table_data <- apply_measurement_overrides(info, measurement_overrides)
  if (isTRUE(selection_applied)) {
    visible_names <- setdiff(as.character(selected_names), as.character(assigned_elsewhere))
    table_data <- table_data[table_data$name %in% unique(c(checked_names, visible_names)), , drop = FALSE]
  }
  table_data$role <- vapply(
    table_data$name,
    role_for_variable,
    character(1),
    dependent = dependent,
    independent = independent,
    controls = controls
  )
  table_data <- table_data[, c("source_order", "name", "var_label", "role", "measurement", "storage_type", "n_unique", "n_missing", "min_value", "max_value"), drop = FALSE]
  disabled_names <- if (isTRUE(selection_applied) && identical(active_role, "dependent")) {
    setdiff(table_data$name[table_data$measurement != "continuous"], checked_names)
  } else {
    character(0)
  }
  table_data$measurement <- mapply(
    measurement_select_html,
    table_data$name,
    table_data$measurement,
    table_data$source_order,
    USE.NAMES = FALSE
  )
  cbind(
    selected = sprintf(
      '<input type="checkbox" class="variable-select" data-name="%s" %s %s>',
      htmltools::htmlEscape(table_data$name),
      ifelse(table_data$name %in% checked_names, "checked", ""),
      ifelse(table_data$name %in% disabled_names, "disabled title=\"Dependent variable must be continuous\"", "")
    ),
    table_data,
    stringsAsFactors = FALSE
  )
}

variable_table_render_state <- function(
  info,
  checked_names = character(0),
  selected_names = character(0),
  assigned_elsewhere = character(0),
  dependent = character(0),
  independent = character(0),
  controls = character(0),
  selection_applied = FALSE,
  active_role = "dependent",
  measurement_overrides = character(0)
) {
  list(
    checked_names = checked_names,
    table_data = variable_table_display_data(
      info,
      checked_names = checked_names,
      selected_names = selected_names,
      assigned_elsewhere = assigned_elsewhere,
      dependent = dependent,
      independent = independent,
      controls = controls,
      selection_applied = selection_applied,
      active_role = active_role,
      measurement_overrides = measurement_overrides
    )
  )
}

collect_variable_input_values <- function(info, input, prefixes, allowed_values = NULL, keep_blank = FALSE) {
  if (is.null(info) || !all(c("source_order", "name") %in% names(info))) {
    return(character(0))
  }

  prefixes <- as.character(prefixes)
  collected <- character(0)
  for (row_index in seq_len(nrow(info))) {
    source_order <- as.character(info$source_order[[row_index]])
    name <- as.character(info$name[[row_index]])
    if (!nzchar(source_order) || !nzchar(name)) {
      next
    }
    value <- NULL
    for (prefix in prefixes) {
      value <- input[[paste0(prefix, source_order)]]
      if (!is.null(value)) {
        break
      }
    }
    if (is.null(value) || length(value) == 0) {
      next
    }
    value <- as.character(value[[1]])
    if (!isTRUE(keep_blank) && !nzchar(trimws(value))) {
      next
    }
    if (!is.null(allowed_values) && !value %in% allowed_values) {
      next
    }
    collected[name] <- value
  }
  collected
}

collect_variable_inputs_from_table <- function(variable_info_fn, input, prefixes, allowed_values = NULL, keep_blank = FALSE) {
  info <- tryCatch(variable_info_fn(reactive_labels = FALSE), error = function(e) NULL)
  collect_variable_input_values(
    info,
    input,
    prefixes = prefixes,
    allowed_values = allowed_values,
    keep_blank = keep_blank
  )
}

collect_var_label_inputs_from_table <- function(variable_info_fn, input) {
  collect_variable_inputs_from_table(
    variable_info_fn,
    input,
    prefixes = c("var_label_input_", "category_var_label_input_")
  )
}

collect_measurement_inputs_from_table <- function(variable_info_fn, input) {
  collect_variable_inputs_from_table(
    variable_info_fn,
    input,
    prefixes = "measurement_input_",
    allowed_values = c("binary", "category", "ordered", "continuous")
  )
}

