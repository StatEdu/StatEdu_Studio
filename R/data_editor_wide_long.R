# Wide-to-long reshape command for the Data Editor menu.

wide_long_empty_groups <- function() {
  data.frame(
    variable = character(0),
    measure = character(0),
    time = character(0),
    stringsAsFactors = FALSE
  )
}

wide_long_clean_name <- function(value, fallback = "value") {
  value <- trimws(as.character(value %||% ""))
  if (length(value) == 0 || is.na(value[[1]]) || !nzchar(value[[1]])) {
    value <- fallback
  }
  make.names(value[[1]], unique = FALSE)
}

wide_long_input_value <- function(input, name, default = NULL) {
  if (is.null(input)) {
    return(default)
  }
  value <- tryCatch(isolate(input[[name]]), error = function(e) NULL)
  value %||% default
}

wide_long_plain_choices <- function(variables) {
  variables <- as.character(variables %||% character(0))
  stats::setNames(variables, variables)
}

wide_long_parse_name <- function(name) {
  name <- as.character(name %||% "")
  patterns <- c(
    "^(.+?)[_\\.-]((?:t|T)?[0-9]+)$",
    "^(.+?)[_\\.-]([A-Za-z]+[0-9]+)$",
    "^(.+?)[_\\.-](baseline|base|pre|post|followup|follow_up|fu[0-9]*|wk[0-9]+|week[0-9]+|m[0-9]+|month[0-9]+|y[0-9]+|year[0-9]+)$"
  )
  for (pattern in patterns) {
    matched <- regexec(pattern, name, ignore.case = TRUE)
    parts <- regmatches(name, matched)[[1]]
    if (length(parts) == 3 && nzchar(parts[[2]]) && nzchar(parts[[3]])) {
      return(list(measure = parts[[2]], time = parts[[3]]))
    }
  }
  list(measure = sub("[_\\.-]+$", "", name), time = "")
}

wide_long_detect_groups <- function(variables) {
  variables <- as.character(variables %||% character(0))
  if (length(variables) == 0) {
    return(wide_long_empty_groups())
  }
  rows <- lapply(variables, function(variable) {
    parsed <- wide_long_parse_name(variable)
    data.frame(
      variable = variable,
      measure = wide_long_clean_name(parsed$measure, "value"),
      time = as.character(parsed$time %||% ""),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

wide_long_time_order <- function(values) {
  values <- unique(as.character(values %||% character(0)))
  if (length(values) == 0) {
    return(values)
  }
  numeric_part <- suppressWarnings(as.numeric(sub("^[A-Za-z_]+", "", values)))
  if (all(is.finite(numeric_part))) {
    values[order(numeric_part, values)]
  } else {
    values
  }
}

wide_long_manual_time_values <- function(text, variables) {
  variables <- as.character(variables %||% character(0))
  lines <- trimws(unlist(strsplit(as.character(text %||% ""), "\\r?\\n")))
  lines <- lines[nzchar(lines)]
  if (length(lines) == 0) {
    return(character(0))
  }
  values <- character(length(variables))
  names(values) <- variables
  for (index in seq_along(lines)) {
    line <- lines[[index]]
    if (grepl("=", line, fixed = TRUE)) {
      pieces <- strsplit(line, "=", fixed = TRUE)[[1]]
      key <- trimws(pieces[[1]])
      value <- trimws(paste(pieces[-1], collapse = "="))
      if (key %in% variables) {
        values[[key]] <- value
      }
    } else if (index <= length(variables)) {
      values[[index]] <- line
    }
  }
  values[nzchar(values)]
}

wide_long_group_table <- function(variables, time_text = "") {
  groups <- wide_long_detect_groups(variables)
  manual <- wide_long_manual_time_values(time_text, variables)
  if (length(manual) > 0) {
    matched <- match(names(manual), groups$variable)
    groups$time[matched[!is.na(matched)]] <- unname(manual[!is.na(matched)])
  }
  missing_time <- !nzchar(groups$time)
  if (any(missing_time)) {
    groups$time[missing_time] <- "1"
  }
  groups
}

wide_long_normalize_mapping <- function(mapping, variables) {
  variables <- as.character(variables %||% character(0))
  defaults <- wide_long_group_table(variables)
  if (!is.data.frame(mapping) || !all(c("variable", "measure", "time") %in% names(mapping))) {
    return(defaults)
  }
  mapping <- data.frame(
    variable = as.character(mapping$variable %||% character(0)),
    measure = as.character(mapping$measure %||% character(0)),
    time = as.character(mapping$time %||% character(0)),
    stringsAsFactors = FALSE
  )
  mapping <- mapping[mapping$variable %in% variables, , drop = FALSE]
  mapping <- mapping[match(intersect(variables, mapping$variable), mapping$variable), , drop = FALSE]
  missing_variables <- setdiff(variables, mapping$variable)
  if (length(missing_variables) > 0) {
    mapping <- rbind(mapping, defaults[defaults$variable %in% missing_variables, , drop = FALSE])
  }
  mapping <- mapping[match(variables, mapping$variable), , drop = FALSE]
  fallback <- wide_long_group_table(mapping$variable)
  blank_measure <- !nzchar(trimws(mapping$measure))
  blank_time <- !nzchar(trimws(mapping$time))
  mapping$measure[blank_measure] <- fallback$measure[blank_measure]
  mapping$time[blank_time] <- fallback$time[blank_time]
  mapping$measure <- vapply(mapping$measure, wide_long_clean_name, character(1), fallback = "value")
  mapping$time <- trimws(mapping$time)
  mapping
}

wide_long_mapping_from_input <- function(input, variables) {
  defaults <- wide_long_group_table(variables)
  if (nrow(defaults) == 0) {
    return(defaults)
  }
  rows <- lapply(seq_len(nrow(defaults)), function(index) {
    measure <- wide_long_input_value(input, paste0("wide_long_measure_", index), defaults$measure[[index]])
    time <- wide_long_input_value(input, paste0("wide_long_time_", index), defaults$time[[index]])
    data.frame(
      variable = defaults$variable[[index]],
      measure = wide_long_clean_name(measure, defaults$measure[[index]]),
      time = trimws(as.character(time %||% defaults$time[[index]])),
      stringsAsFactors = FALSE
    )
  })
  wide_long_normalize_mapping(do.call(rbind, rows), defaults$variable)
}

wide_long_mapping_table_ui <- function(variables, input = NULL, language = getOption("statedu.app_language", statedu_initial_language())) {
  language <- normalize_app_language(language)
  mapping <- wide_long_mapping_from_input(input, variables)
  if (nrow(mapping) == 0) {
    return(div(
      class = "wide-long-mapping-empty",
      statedu_text(
        language,
        "Move repeated-measure columns here.",
        statedu_utf8("ebb098ebb3b5ecb8a1eca09520ec97b4ec9d8420ec97aceab8b0ec979020eb8693ec9cbcec84b8ec9a942e")
      )
    ))
  }
  tags$table(
    class = "wide-long-mapping-table",
    tags$thead(
      tags$tr(
        tags$th(statedu_text(language, "Source column", statedu_utf8("ec9b90ebb3b820ec97b4"))),
        tags$th(statedu_text(language, "Long variable", statedu_utf8("6c6f6e6720ebb380ec8898"))),
        tags$th(statedu_text(language, "Time", statedu_utf8("ec8b9ceca090")))
      )
    ),
    tags$tbody(
      lapply(seq_len(nrow(mapping)), function(index) {
        tags$tr(
          tags$td(span(mapping$variable[[index]], class = "wide-long-source-name")),
          tags$td(textInput(paste0("wide_long_measure_", index), NULL, value = mapping$measure[[index]], width = "100%")),
          tags$td(textInput(paste0("wide_long_time_", index), NULL, value = mapping$time[[index]], width = "100%"))
        )
      })
    )
  )
}

wide_long_transform <- function(
  data,
  repeated_variables,
  id_variables = character(0),
  time_name = "time",
  value_name = "value",
  output_mode = "auto",
  keep_other_variables = TRUE,
  generated_id_name = "row_id",
  time_text = "",
  mapping = NULL
) {
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(data) == 0) {
    stop("The current data has no rows to reshape.", call. = FALSE)
  }
  repeated_variables <- intersect(as.character(repeated_variables %||% character(0)), names(data))
  if (length(repeated_variables) == 0) {
    stop("Select at least one repeated-measure variable.", call. = FALSE)
  }

  id_variables <- intersect(as.character(id_variables %||% character(0)), setdiff(names(data), repeated_variables))
  time_name <- wide_long_clean_name(time_name, "time")
  value_name <- wide_long_clean_name(value_name, "value")
  generated_id_name <- wide_long_clean_name(generated_id_name, "row_id")
  output_mode <- as.character(output_mode %||% "auto")[[1]]
  if (!output_mode %in% c("auto", "single")) {
    output_mode <- "auto"
  }

  source_data <- data
  if (length(id_variables) == 0) {
    generated_id_name <- make.unique(c(names(source_data), generated_id_name), sep = "_")
    generated_id_name <- generated_id_name[[length(generated_id_name)]]
    source_data[[generated_id_name]] <- seq_len(nrow(source_data))
    id_variables <- generated_id_name
  }

  group_table <- wide_long_normalize_mapping(mapping, repeated_variables)
  if (is.null(mapping) && nzchar(trimws(as.character(time_text %||% "")))) {
    group_table <- wide_long_group_table(repeated_variables, time_text)
  }
  if (identical(output_mode, "single")) {
    group_table$measure <- value_name
  }

  keep_variables <- if (isTRUE(keep_other_variables)) {
    setdiff(names(source_data), repeated_variables)
  } else {
    id_variables
  }
  keep_variables <- unique(c(id_variables, keep_variables))

  if (identical(output_mode, "single")) {
    value_output_name <- make.unique(c(keep_variables, time_name, value_name), sep = "_")
    value_output_name <- value_output_name[[length(value_output_name)]]
    long_rows <- unlist(
      lapply(seq_len(nrow(source_data)), function(row_index) {
        lapply(seq_len(nrow(group_table)), function(group_index) {
          row <- source_data[row_index, keep_variables, drop = FALSE]
          row[[time_name]] <- group_table$time[[group_index]]
          row[[value_output_name]] <- source_data[[group_table$variable[[group_index]]]][[row_index]]
          row
        })
      }),
      recursive = FALSE
    )
    long_data <- do.call(rbind, long_rows)
    rownames(long_data) <- NULL
    return(long_data)
  }

  times <- wide_long_time_order(group_table$time)
  reserved <- unique(c(keep_variables, time_name))
  measure_keys <- unique(group_table$measure)
  measure_names <- make.unique(c(reserved, measure_keys), sep = "_")
  measure_names <- utils::tail(measure_names, length(measure_keys))
  names(measure_names) <- measure_keys

  long_rows <- unlist(
    lapply(seq_len(nrow(source_data)), function(row_index) {
      lapply(times, function(time_value) {
        row <- source_data[row_index, keep_variables, drop = FALSE]
        row[[time_name]] <- time_value
        for (measure in unique(group_table$measure)) {
          output_name <- measure_names[[measure]]
          source_variable <- group_table$variable[group_table$time == time_value & group_table$measure == measure]
          if (length(source_variable) == 0) {
            row[[output_name]] <- NA
          } else {
            row[[output_name]] <- source_data[[source_variable[[1]]]][[row_index]]
          }
        }
        row
      })
    }),
    recursive = FALSE
  )
  long_data <- do.call(rbind, long_rows)
  rownames(long_data) <- NULL
  long_data
}

wide_long_group_summary <- function(variables, time_text = "") {
  groups <- wide_long_group_table(variables, time_text)
  if (nrow(groups) == 0) {
    return(data.frame(Message = "No repeated-measure variables are selected.", stringsAsFactors = FALSE))
  }
  groups[order(groups$measure, groups$time, groups$variable), , drop = FALSE]
}

wide_long_mapping_summary <- function(mapping, variables) {
  groups <- wide_long_normalize_mapping(mapping, variables)
  if (nrow(groups) == 0) {
    return(data.frame(Message = "No repeated-measure variables are selected.", stringsAsFactors = FALSE))
  }
  groups[order(groups$measure, groups$time, groups$variable), , drop = FALSE]
}

wide_long_default_value_name <- function(variables) {
  variables <- trimws(as.character(variables %||% character(0)))
  variables <- variables[nzchar(variables)]
  if (length(variables) == 0) {
    return("x")
  }
  stems <- sub("[_\\.-]?[0-9]+$", "", variables)
  stems <- ifelse(nzchar(stems), stems, variables)
  if (length(unique(stems)) == 1) {
    return(wide_long_clean_name(stems[[1]], "x"))
  }
  first <- sub("[_\\.-]?[0-9]+$", "", variables[[1]])
  wide_long_clean_name(if (nzchar(first)) first else variables[[1]], "x")
}

wide_long_parse_indicator_values <- function(text, count) {
  count <- max(0L, as.integer(count %||% 0L))
  if (count == 0L) {
    return(character(0))
  }
  values <- trimws(unlist(strsplit(as.character(text %||% ""), "[,\\r\\n]+")))
  values <- values[nzchar(values)]
  if (length(values) < count) {
    values <- c(values, as.character(seq.int(length(values) + 1L, count)))
  }
  values[seq_len(count)]
}

wide_long_make_spec <- function(
  variables,
  value_name,
  unit_type = "different",
  index_name = "time",
  index_values = character(0),
  group_name = "group",
  time_name = "time",
  group_count = 2L,
  time_count = NULL,
  id = NULL
) {
  variables <- trimws(as.character(variables %||% character(0)))
  variables <- variables[nzchar(variables)]
  if (length(variables) == 0) {
    stop("Move source columns into the repeated columns block first.", call. = FALSE)
  }
  value_name <- wide_long_clean_name(value_name, wide_long_default_value_name(variables))
  unit_type <- as.character(unit_type %||% "different")[[1]]
  if (!unit_type %in% c("different", "same")) {
    unit_type <- "different"
  }

  if (identical(unit_type, "same")) {
    group_count <- max(1L, as.integer(group_count %||% 2L))
    if (is.null(time_count) || !is.finite(suppressWarnings(as.numeric(time_count)))) {
      time_count <- length(variables) / group_count
    }
    time_count <- max(1L, as.integer(time_count))
    if (!identical(group_count * time_count, length(variables))) {
      stop("For same-object repeated measures, group count x time count must match the number of selected columns.", call. = FALSE)
    }
    group_name <- wide_long_clean_name(group_name, "group")
    time_name <- wide_long_clean_name(time_name, "time")
    if (identical(group_name, time_name)) {
      time_name <- make.unique(c(group_name, time_name), sep = "_")[[2]]
    }
    return(list(
      id = id %||% paste0("wide_long_spec_", as.integer(Sys.time()), "_", sample.int(1000000L, 1L)),
      variables = variables,
      value_name = value_name,
      unit_type = "same",
      group_name = group_name,
      time_name = time_name,
      group_count = group_count,
      time_count = time_count
    ))
  }

  index_name <- wide_long_clean_name(index_name, "time")
  index_values <- wide_long_parse_indicator_values(index_values, length(variables))
  list(
    id = id %||% paste0("wide_long_spec_", as.integer(Sys.time()), "_", sample.int(1000000L, 1L)),
    variables = variables,
    value_name = value_name,
    unit_type = "different",
    index_name = index_name,
    index_values = index_values
  )
}

wide_long_spec_sources <- function(specs) {
  unique(unlist(lapply(specs %||% list(), function(spec) as.character(spec$variables %||% character(0))), use.names = FALSE))
}

wide_long_spec_label <- function(spec) {
  value_name <- as.character(spec$value_name %||% "x")
  if (identical(spec$unit_type, "same")) {
    return(sprintf("%s (%s,%s)", value_name, spec$group_count %||% 1L, spec$time_count %||% length(spec$variables %||% character(0))))
  }
  sprintf("%s (%s)", value_name, length(spec$variables %||% character(0)))
}

wide_long_spec_items <- function(specs) {
  specs <- specs %||% list()
  if (length(specs) == 0) {
    return(list())
  }
  lapply(specs, function(spec) {
    list(
      value = as.character(spec$id),
      label = wide_long_spec_label(spec),
      measurement = "continuous"
    )
  })
}

wide_long_expand_spec <- function(spec) {
  variables <- as.character(spec$variables %||% character(0))
  if (identical(spec$unit_type, "same")) {
    group_values <- rep(seq_len(as.integer(spec$group_count)), each = as.integer(spec$time_count))
    time_values <- rep(seq_len(as.integer(spec$time_count)), times = as.integer(spec$group_count))
    return(data.frame(
      source = variables,
      value_name = as.character(spec$value_name),
      indicator_1_name = as.character(spec$group_name),
      indicator_1_value = as.character(group_values),
      indicator_2_name = as.character(spec$time_name),
      indicator_2_value = as.character(time_values),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    source = variables,
    value_name = as.character(spec$value_name),
    indicator_1_name = as.character(spec$index_name),
    indicator_1_value = wide_long_parse_indicator_values(spec$index_values, length(variables)),
    indicator_2_name = "",
    indicator_2_value = "",
    stringsAsFactors = FALSE
  )
}

wide_long_indicator_set_count <- function(specs) {
  specs <- specs %||% list()
  if (length(specs) == 0) {
    return(1L)
  }
  expanded <- do.call(rbind, lapply(specs, wide_long_expand_spec))
  if (!is.data.frame(expanded) || nrow(expanded) == 0) {
    return(1L)
  }
  keys <- apply(expanded[, c("indicator_1_name", "indicator_1_value", "indicator_2_name", "indicator_2_value"), drop = FALSE], 1, paste, collapse = "\r")
  max(1L, length(unique(keys)))
}

wide_long_preview_limit <- function(specs, sets = 2L) {
  max(1L, as.integer(sets %||% 2L)) * wide_long_indicator_set_count(specs)
}

wide_long_preview_columns <- function(
  result,
  specs,
  id_variables = character(0),
  fixed_variables = character(0),
  fixed_mode = "all",
  generated_id_name = "row_id"
) {
  result_names <- names(as.data.frame(result, stringsAsFactors = FALSE, check.names = FALSE))
  if (length(result_names) == 0) {
    return(character(0))
  }
  specs <- specs %||% list()
  expanded <- if (length(specs) > 0) {
    do.call(rbind, lapply(specs, wide_long_expand_spec))
  } else {
    NULL
  }
  indicator_names <- if (is.data.frame(expanded) && nrow(expanded) > 0) {
    unique(c(expanded$indicator_1_name, expanded$indicator_2_name))
  } else {
    character(0)
  }
  indicator_names <- intersect(indicator_names[nzchar(indicator_names)], result_names)

  value_count <- if (is.data.frame(expanded) && nrow(expanded) > 0) length(unique(expanded$value_name)) else 0L
  value_names <- if (value_count > 0) utils::tail(result_names, value_count) else character(0)

  id_columns <- intersect(as.character(id_variables %||% character(0)), result_names)
  if (length(id_columns) == 0) {
    generated_id_name <- wide_long_clean_name(generated_id_name, "row_id")
    id_columns <- intersect(generated_id_name, result_names)
  }
  if (length(id_columns) == 0) {
    id_columns <- utils::head(setdiff(result_names, c(indicator_names, value_names)), 1)
  }

  fixed_columns <- if (identical(as.character(fixed_mode %||% "all")[[1]], "selected")) {
    intersect(as.character(fixed_variables %||% character(0)), result_names)
  } else {
    character(0)
  }

  unique(c(id_columns, indicator_names, value_names, fixed_columns))
}

wide_long_preview_display <- function(
  result,
  specs,
  id_variables = character(0),
  fixed_variables = character(0),
  fixed_mode = "all",
  generated_id_name = "row_id"
) {
  if (is.null(result)) {
    return(NULL)
  }
  columns <- wide_long_preview_columns(
    result = result,
    specs = specs,
    id_variables = id_variables,
    fixed_variables = fixed_variables,
    fixed_mode = fixed_mode,
    generated_id_name = generated_id_name
  )
  if (length(columns) == 0) {
    return(result)
  }
  result[, columns, drop = FALSE]
}

save_wide_long_result_file <- function(data) {
  if (!exists("choose_data_csv_save_path", mode = "function")) {
    stop("Data save dialog is not available.", call. = FALSE)
  }
  path <- choose_data_csv_save_path()
  if (length(path) == 0 || !nzchar(path[[1]])) {
    return(list(saved = FALSE, path = ""))
  }
  path <- path[[1]]
  if (!grepl("\\.csv$", path, ignore.case = TRUE)) {
    path <- paste0(path, ".csv")
  }
  readr::write_excel_csv(as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE), path, na = "")
  list(saved = TRUE, path = path)
}

wide_long_transform_configured <- function(
  data,
  specs,
  id_variables = character(0),
  keep_other_variables = TRUE,
  fixed_variables = character(0),
  generated_id_name = "row_id"
) {
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(data) == 0) {
    stop("The current data has no rows to reshape.", call. = FALSE)
  }
  specs <- specs %||% list()
  if (length(specs) == 0) {
    stop("Set at least one wide-to-long variable group.", call. = FALSE)
  }
  repeated_variables <- intersect(wide_long_spec_sources(specs), names(data))
  if (length(repeated_variables) == 0) {
    stop("Configured source columns are not available in the current data.", call. = FALSE)
  }

  source_data <- data
  id_variables <- intersect(as.character(id_variables %||% character(0)), setdiff(names(source_data), repeated_variables))
  generated_id_name <- wide_long_clean_name(generated_id_name, "row_id")
  if (length(id_variables) == 0) {
    generated_id_name <- make.unique(c(names(source_data), generated_id_name), sep = "_")
    generated_id_name <- generated_id_name[[length(generated_id_name)]]
    source_data[[generated_id_name]] <- seq_len(nrow(source_data))
    id_variables <- generated_id_name
  }

  expanded <- do.call(rbind, lapply(specs, wide_long_expand_spec))
  expanded <- expanded[expanded$source %in% names(source_data), , drop = FALSE]
  indicator_names <- unique(c(expanded$indicator_1_name, expanded$indicator_2_name))
  indicator_names <- indicator_names[nzchar(indicator_names)]
  if (length(indicator_names) == 0) {
    stop("At least one indicator variable is required.", call. = FALSE)
  }
  expanded$indicator_key <- apply(expanded[, c("indicator_1_name", "indicator_1_value", "indicator_2_name", "indicator_2_value"), drop = FALSE], 1, paste, collapse = "\r")

  indicator_rows <- lapply(seq_len(nrow(expanded)), function(index) {
    row <- stats::setNames(as.list(rep(NA_character_, length(indicator_names))), indicator_names)
    row[[expanded$indicator_1_name[[index]]]] <- expanded$indicator_1_value[[index]]
    if (nzchar(expanded$indicator_2_name[[index]])) {
      row[[expanded$indicator_2_name[[index]]]] <- expanded$indicator_2_value[[index]]
    }
    as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)
  })
  indicator_table <- unique(do.call(rbind, indicator_rows))
  indicator_table <- indicator_table[do.call(order, indicator_table), , drop = FALSE]

  fixed_variables <- intersect(as.character(fixed_variables %||% character(0)), setdiff(names(source_data), repeated_variables))
  keep_variables <- if (isTRUE(keep_other_variables)) {
    setdiff(names(source_data), repeated_variables)
  } else {
    unique(c(id_variables, fixed_variables))
  }
  keep_variables <- unique(c(id_variables, keep_variables))
  value_keys <- unique(expanded$value_name)
  value_names <- make.unique(c(keep_variables, indicator_names, value_keys), sep = "_")
  value_names <- utils::tail(value_names, length(value_keys))
  names(value_names) <- value_keys

  long_rows <- unlist(
    lapply(seq_len(nrow(source_data)), function(row_index) {
      lapply(seq_len(nrow(indicator_table)), function(indicator_index) {
        row <- source_data[row_index, keep_variables, drop = FALSE]
        for (indicator_name in indicator_names) {
          row[[indicator_name]] <- indicator_table[[indicator_name]][[indicator_index]]
        }
        for (value_key in value_keys) {
          row[[value_names[[value_key]]]] <- NA
        }
        for (spec in specs) {
          spec_rows <- wide_long_expand_spec(spec)
          for (spec_row_index in seq_len(nrow(spec_rows))) {
            matched <- identical(as.character(indicator_table[[spec_rows$indicator_1_name[[spec_row_index]]]][[indicator_index]]), spec_rows$indicator_1_value[[spec_row_index]])
            if (matched && nzchar(spec_rows$indicator_2_name[[spec_row_index]])) {
              matched <- identical(as.character(indicator_table[[spec_rows$indicator_2_name[[spec_row_index]]]][[indicator_index]]), spec_rows$indicator_2_value[[spec_row_index]])
            }
            if (isTRUE(matched) && spec_rows$source[[spec_row_index]] %in% names(source_data)) {
              row[[value_names[[spec_rows$value_name[[spec_row_index]]]]]] <- source_data[[spec_rows$source[[spec_row_index]]]][[row_index]]
            }
          }
        }
        row
      })
    }),
    recursive = FALSE
  )
  long_data <- do.call(rbind, long_rows)
  rownames(long_data) <- NULL
  long_data
}

data_editor_wide_long_panel <- function(language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  options(statedu.app_language = language)
  div(
    class = "page-shell",
    div(
      class = "app-heading",
      h1(statedu_text(language, "Wide to Long", statedu_utf8("ec9980ec9db4eb939c2deba1b120ebb380ed9998"))),
      div(statedu_text(language, "Reshape repeated-measure columns into long format before longitudinal / panel analysis.", statedu_utf8("ebb098ebb3b5ecb8a1eca09520ec97b4ec9d84206c6f6e6720666f726d6174ec9cbceba19c20ebb380ed9998ed95a9eb8b88eb8ba42e")), class = "app-subtitle")
    ),
    div(
      class = "workspace-panel frequencies-workspace-panel data-editor-workspace",
      analysis_workspace_heading(statedu_text(language, "Wide to long", statedu_utf8("ec9980ec9db4eb939c2deba1b120ebb380ed9998")), "wide_long", language = language),
      analysis_workspace_body(
        "wide_long",
        uiOutput("wide_long_setup"),
        div(
          class = "analysis-action-row recode-same-action-row wide-long-action-row",
          actionButton("run_wide_long", analysis_ui_text("Run", language), class = "btn btn-primary"),
          actionButton("wide_long_remove_spec", analysis_ui_text("Remove", language), class = "btn btn-default wide-long-remove-button"),
          actionButton("preview_wide_long", analysis_ui_text("Preview", language), class = "btn btn-default wide-long-preview-button")
        ),
        uiOutput("wide_long_message"),
        div(class = "data-editor-result-output", DT::DTOutput("wide_long_preview"))
      )
    )
  )
}

wide_long_setup_panel <- function(
  file,
  data,
  variable_info,
  labels = character(0),
  selected_variables = character(0),
  configured_specs = list(),
  input = NULL,
  language = statedu_initial_language()
) {
  language <- normalize_app_language(language)
  options(statedu.app_language = language)
  if (is.null(file) || is.null(data)) {
    return(setup_empty_message(statedu_text(language, "Load a data file in the Data tab before reshaping data.", statedu_utf8("eb8db0ec9db4ed84b020ed83adec9790ec849c20eb8db0ec9db4ed84b020ed8c8cec9dbcec9d8420eba8bceca08020ebb688eb9facec98a820ed9b8420eb8db0ec9db4ed84b0eba5bc20ebb380ed9998ed9598ec84b8ec9a942e")), language = language))
  }
  variable_names <- names(data)
  if (length(variable_names) == 0) {
    return(setup_empty_message(statedu_text(language, "The current data file has no variables.", statedu_utf8("ed9884ec9eac20eb8db0ec9db4ed84b020ed8c8cec9dbcec979020ebb380ec8898eab08020ec9786ec8ab5eb8b88eb8ba42e")), language = language))
  }

  selected_variables <- intersect(as.character(selected_variables %||% character(0)), variable_names)
  configured_sources <- wide_long_spec_sources(configured_specs)
  available <- setdiff(variable_names, unique(c(selected_variables, configured_sources)))
  selected_available <- selected_order_items(wide_long_input_value(input, "wide_long_available", character(0)), available)
  selected_selected <- selected_order_items(wide_long_input_value(input, "wide_long_selected", character(0)), selected_variables)
  selected_configured <- selected_order_items(wide_long_input_value(input, "wide_long_configured", character(0)), vapply(configured_specs %||% list(), `[[`, character(1), "id"))
  id_choices <- setdiff(variable_names, unique(c(selected_variables, configured_sources)))
  current_ids <- intersect(as.character(wide_long_input_value(input, "wide_long_id_variables", character(0))), id_choices)
  if (length(current_ids) == 0) {
    likely_id <- id_choices[grepl("(^|[_ .-])(id|pid|person|subject|cluster)([_ .-]|$)", tolower(id_choices))]
    current_ids <- utils::head(likely_id, 1)
  }
  fixed_mode <- as.character(wide_long_input_value(input, "wide_long_fixed_mode", "all"))[[1]]
  if (!fixed_mode %in% c("all", "selected")) {
    fixed_mode <- "all"
  }
  fixed_choices <- id_choices
  current_fixed <- intersect(as.character(wide_long_input_value(input, "wide_long_fixed_variables", character(0))), fixed_choices)
  current_unit_type <- as.character(wide_long_input_value(input, "wide_long_unit_type", "different"))[[1]]
  if (!current_unit_type %in% c("different", "same")) {
    current_unit_type <- "different"
  }
  selected_count <- length(selected_variables)
  group_count <- suppressWarnings(as.integer(wide_long_input_value(input, "wide_long_group_count", 2L)))
  if (!is.finite(group_count) || group_count < 1L) {
    group_count <- 2L
  }
  if (selected_count > 0 && group_count > selected_count) {
    group_count <- selected_count
  }
  time_count <- if (selected_count > 0 && selected_count %% group_count == 0) selected_count / group_count else max(1L, selected_count)
  default_values <- if (selected_count > 0) paste(seq_len(selected_count), collapse = ", ") else ""
  default_value_name <- wide_long_default_value_name(selected_variables)

  div(
    class = "recode-same-setup-grid wide-long-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel wide-long-source-panel",
      analysis_field_label_tag("Variables", language = language),
      analysis_transfer_listbox_input(
        "wide_long_available",
        items = analysis_variable_items(available, variable_info, labels),
        selected = selected_available,
        size = 18
      )
    ),
    div(
      class = "analysis-transfer-controls recode-same-transfer-controls wide-long-transfer-controls",
      actionButton(
        "wide_long_move",
        ">",
        class = "btn btn-default analysis-move-button",
        onmousedown = "if(window.easyflowRememberAllTransferScrolls){window.easyflowRememberAllTransferScrolls();}",
        disabled = if (length(variable_names) == 0) "disabled" else NULL
      )
    ),
    div(
      class = "analysis-transfer-column analysis-transfer-panel wide-long-selected-panel",
      analysis_field_label_tag(statedu_text(language, "Repeated columns", statedu_utf8("ebb098ebb3b520ec97b4")), language = language),
      analysis_transfer_listbox_input(
        "wide_long_selected",
        items = analysis_variable_items(selected_variables, variable_info, labels),
        selected = selected_selected,
        size = 7
      ),
      div(
        class = "hierarchical-order-actions wide-long-order-actions",
        actionButton("wide_long_up", analysis_ui_text("Up", language), class = "btn-default btn-sm"),
        actionButton("wide_long_down", analysis_ui_text("Down", language), class = "btn-default btn-sm")
      ),
      div(
        class = "wide-long-configured-panel",
        div(class = "analysis-option-title wide-long-mapping-title", statedu_text(language, "Configured long variables", statedu_utf8("ec84a4eca095eb909c206c6f6e6720ebb380ec8898"))),
        analysis_transfer_listbox_input(
          "wide_long_configured",
          items = wide_long_spec_items(configured_specs),
          selected = selected_configured,
          size = 6
        ),
        div(
          class = "hierarchical-order-actions wide-long-configured-order-actions",
          actionButton("wide_long_configured_up", analysis_ui_text("Up", language), class = "btn-default btn-sm"),
          actionButton("wide_long_configured_down", analysis_ui_text("Down", language), class = "btn-default btn-sm")
        )
      )
    ),
    div(class = "wide-long-grid-spacer"),
    div(
      class = "analysis-options-panel wide-long-options analysis-tabbed-options",
      div(class = "analysis-option-title factor-options-title", analysis_ui_text("Options", language)),
      tabsetPanel(
        id = "wide_long_options_tab",
        type = "tabs",
        tabPanel(
          statedu_text(language, "Reshape", statedu_utf8("ebb380ed9998")),
          value = "Reshape",
          div(
            class = "factor-options-tab-content wide-long-options-tab-content",
            textInput("wide_long_value_name", statedu_text(language, "New long variable name", statedu_utf8("ec8388206c6f6e6720ebb380ec8898ebaa85")), value = wide_long_input_value(input, "wide_long_value_name", default_value_name), width = "100%"),
            div(
              class = "wide-long-structure-panel",
              radioButtons(
                "wide_long_unit_type",
                statedu_text(language, "Repeated columns are measured on", statedu_utf8("ebb098ebb3b520ec97b4ec9d9820ecb8a1eca09520eb8c80ec8381")),
                choices = stats::setNames(c("different", "same"), c(statedu_text(language, "Different objects", statedu_utf8("ec849ceba19c20eb8ba4eba5b820eb8c80ec8381")), statedu_text(language, "Same object", statedu_utf8("eab099ec9d8020eb8c80ec8381")))),
                selected = current_unit_type
              ),
              conditionalPanel(
                "input.wide_long_unit_type !== 'same'",
                textInput("wide_long_index_name", statedu_text(language, "Indicator variable", statedu_utf8("eca780ec8b9c20ebb380ec8898")), value = wide_long_input_value(input, "wide_long_index_name", "time"), width = "100%"),
                textAreaInput("wide_long_index_values", statedu_text(language, "Indicator values", statedu_utf8("eca780ec8b9c20eab092")), value = wide_long_input_value(input, "wide_long_index_values", default_values), rows = 2, width = "100%")
              ),
              conditionalPanel(
                "input.wide_long_unit_type === 'same'",
                div(
                  class = "wide-long-count-grid",
                  numericInput("wide_long_group_count", statedu_text(language, "Groups", statedu_utf8("eab7b8eba3b920ec8898")), value = group_count, min = 1, step = 1, width = "100%"),
                  numericInput("wide_long_time_count", statedu_text(language, "Repeats", statedu_utf8("ebb098ebb3b520ec8898")), value = time_count, min = 1, step = 1, width = "100%")
                ),
                div(
                  class = "wide-long-count-grid",
                  textInput("wide_long_group_name", statedu_text(language, "Group variable", statedu_utf8("eab7b8eba3b920ebb380ec8898")), value = wide_long_input_value(input, "wide_long_group_name", "group"), width = "100%"),
                  textInput("wide_long_time_name", statedu_text(language, "Time variable", statedu_utf8("ec8b9ceca09020ebb380ec8898")), value = wide_long_input_value(input, "wide_long_time_name", "time"), width = "100%")
                )
              )
            ),
            actionButton("wide_long_set_spec", statedu_text(language, "Set variable", statedu_utf8("ebb380ec889820ec84a4eca095")), class = "btn btn-primary wide-long-set-button")
          )
        ),
        tabPanel(
          statedu_text(language, "ID / Fixed", statedu_utf8("4944202f20eab3a0eca09520ebb380ec8898")),
          value = "ID / Fixed",
          div(
            class = "factor-options-tab-content wide-long-options-tab-content",
            div(
              class = "wide-long-id-section",
              selectInput(
                "wide_long_id_variables",
                "ID",
                choices = wide_long_plain_choices(id_choices),
                selected = current_ids,
                multiple = TRUE,
                width = "100%"
              ),
              conditionalPanel(
                "!input.wide_long_id_variables || input.wide_long_id_variables.length === 0",
                textInput("wide_long_generated_id", statedu_text(language, "Generated ID name", statedu_utf8("ec839dec84b1ed95a0204944ebaa85")), value = wide_long_input_value(input, "wide_long_generated_id", "row_id"), width = "100%")
              )
            ),
            div(
              class = "wide-long-fixed-section",
              radioButtons(
                "wide_long_fixed_mode",
                statedu_text(language, "Fixed variables", statedu_utf8("eab3a0eca09520ebb380ec8898")),
                choices = stats::setNames(c("all", "selected"), c(statedu_text(language, "Keep all unselected variables", statedu_utf8("ec84a0ed839ded9598eca78020ec958aec9d8020ebaaa8eb93a020ebb380ec889820ec9ca0eca780")), statedu_text(language, "Selected variables only", statedu_utf8("ec84a0ed839ded959c20ebb380ec8898eba78c20ec9ca0eca780")))),
                selected = fixed_mode
              ),
              conditionalPanel(
                "input.wide_long_fixed_mode === 'selected'",
                selectInput(
                  "wide_long_fixed_variables",
                  statedu_text(language, "Fixed variables", statedu_utf8("eab3a0eca09520ebb380ec8898")),
                  choices = wide_long_plain_choices(fixed_choices),
                  selected = current_fixed,
                  multiple = TRUE,
                  width = "100%"
                )
              )
            )
          )
        )
      )
    )
  )
}

register_wide_long_handlers <- function(
  input,
  output,
  session,
  dataset_fn,
  current_data_file_fn,
  variable_info_fn,
  labels_fn,
  replace_dataset_fn,
  mark_settings_dirty,
  language_fn = NULL
) {
  selected_variables <- reactiveVal(character(0))
  configured_specs <- reactiveVal(list())
  active_list <- reactiveVal("wide_long_available")
  last_message <- reactiveVal(NULL)
  preview_data <- reactiveVal(NULL)

  current_variable_names <- function() {
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    names(data %||% data.frame())
  }

  viewer_variables <- function() {
    fixed_variables <- if (identical(input$wide_long_fixed_mode, "selected")) {
      as.character(input$wide_long_fixed_variables %||% character(0))
    } else {
      character(0)
    }
    unique(c(
      selected_variables(),
      wide_long_spec_sources(configured_specs()),
      as.character(input$wide_long_id_variables %||% character(0)),
      fixed_variables
    ))
  }

  register_analysis_data_viewer_handlers(
    input = input,
    output = output,
    prefix = "wide_long",
    title = "Wide to Long Data Viewer",
    dataset_fn = dataset_fn,
    selected_names_fn = viewer_variables,
    variables_fn = viewer_variables,
    variable_table_fn = variable_info_fn,
    labels_fn = labels_fn,
    category_table_fn = function() NULL,
    language_fn = language_fn
  )

  output$wide_long_setup <- renderUI({
    language <- statedu_current_language(language_fn)
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    variable_info <- tryCatch(variable_info_fn(), error = function(e) NULL)
    wide_long_setup_panel(
      file = current_data_file_fn(),
      data = data,
      variable_info = variable_info,
      labels = labels_fn(),
      selected_variables = selected_variables(),
      configured_specs = configured_specs(),
      input = input,
      language = language
    )
  })

  observeEvent(input$wide_long_available_active, active_list("wide_long_available"), ignoreInit = TRUE)
  observeEvent(input$wide_long_selected_active, active_list("wide_long_selected"), ignoreInit = TRUE)

  observe({
    if (identical(active_list(), "wide_long_selected") && length(input$wide_long_selected %||% character(0)) > 0) {
      updateActionButton(session, "wide_long_move", label = "<")
    } else {
      updateActionButton(session, "wide_long_move", label = ">")
    }
  })

  move_available <- function(values) {
    blocked <- unique(c(selected_variables(), wide_long_spec_sources(configured_specs())))
    values <- intersect(as.character(values %||% character(0)), setdiff(current_variable_names(), blocked))
    if (length(values) == 0) {
      return(FALSE)
    }
    selected_variables(c(selected_variables(), values))
    active_list("wide_long_selected")
    TRUE
  }

  move_selected <- function(values) {
    values <- intersect(as.character(values %||% character(0)), selected_variables())
    if (length(values) == 0) {
      return(FALSE)
    }
    selected_variables(setdiff(selected_variables(), values))
    active_list("wide_long_available")
    TRUE
  }

  observeEvent(input$wide_long_move, {
    if (identical(active_list(), "wide_long_selected")) {
      move_selected(input$wide_long_selected)
    } else {
      move_available(input$wide_long_available)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$wide_long_move_direct, {
    payload <- input$wide_long_move_direct %||% list()
    source <- as.character(payload$source %||% "")
    values <- as.character(payload$values %||% character(0))
    if (identical(source, "selected")) {
      move_selected(values)
    } else {
      move_available(values)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$wide_long_available_doubleclick, {
    move_available(input$wide_long_available_doubleclick$value)
  }, ignoreInit = TRUE)

  observeEvent(input$wide_long_selected_doubleclick, {
    move_selected(input$wide_long_selected_doubleclick$value)
  }, ignoreInit = TRUE)

  observeEvent(input$wide_long_up, {
    updated <- move_order_item(selected_variables(), input$wide_long_selected, "up")
    if (isTRUE(updated$changed)) {
      selected_variables(updated$order)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$wide_long_down, {
    updated <- move_order_item(selected_variables(), input$wide_long_selected, "down")
    if (isTRUE(updated$changed)) {
      selected_variables(updated$order)
    }
  }, ignoreInit = TRUE)

  move_configured_specs <- function(direction) {
    specs <- configured_specs()
    if (length(specs) == 0) {
      return()
    }
    selected <- as.character(input$wide_long_configured %||% character(0))
    if (length(selected) == 0) {
      return()
    }
    ids <- vapply(specs, `[[`, character(1), "id")
    updated <- move_order_item(ids, selected, direction)
    if (!isTRUE(updated$changed)) {
      return()
    }
    configured_specs(specs[match(updated$order, ids)])
    preview_data(NULL)
    mark_settings_dirty()
  }

  observeEvent(input$wide_long_configured_up, {
    move_configured_specs("up")
  }, ignoreInit = TRUE)

  observeEvent(input$wide_long_configured_down, {
    move_configured_specs("down")
  }, ignoreInit = TRUE)

  observeEvent(selected_variables(), {
    default_name <- wide_long_default_value_name(selected_variables())
    current_name <- trimws(as.character(input$wide_long_value_name %||% ""))
    if (!nzchar(current_name) || current_name %in% c("x", "value")) {
      updateTextInput(session, "wide_long_value_name", value = default_name)
    }
    if (length(selected_variables()) > 0) {
      updateTextAreaInput(session, "wide_long_index_values", value = paste(seq_along(selected_variables()), collapse = ", "))
    }
  }, ignoreInit = TRUE)

  observeEvent(input$wide_long_group_count, {
    count <- length(selected_variables())
    group_count <- suppressWarnings(as.integer(input$wide_long_group_count %||% 2L))
    if (!is.finite(group_count) || group_count < 1L || count == 0L) {
      return()
    }
    if (count %% group_count == 0L) {
      updateNumericInput(session, "wide_long_time_count", value = count / group_count)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$wide_long_set_spec, {
    language <- statedu_current_language(language_fn)
    variables <- selected_variables()
    if (length(variables) == 0) {
      showNotification(statedu_text(language, "Move source columns into the repeated columns block first.", statedu_utf8("eba8bceca08020ec9b90ebb3b820ec97b4ec9d8420ebb098ebb3b520ec97b420ec9881ec97adec9cbceba19c20ec9db4eb8f99ed9598ec84b8ec9a942e")), type = "warning", duration = 5)
      return()
    }
    spec <- tryCatch(
      wide_long_make_spec(
        variables = variables,
        value_name = input$wide_long_value_name,
        unit_type = input$wide_long_unit_type,
        index_name = input$wide_long_index_name,
        index_values = input$wide_long_index_values,
        group_name = input$wide_long_group_name,
        time_name = input$wide_long_time_name,
        group_count = input$wide_long_group_count,
        time_count = input$wide_long_time_count
      ),
      error = function(e) {
        showNotification(conditionMessage(e), type = "warning", duration = 7)
        NULL
      }
    )
    if (is.null(spec)) {
      return()
    }
    specs <- configured_specs()
    existing_index <- which(vapply(specs, function(item) identical(item$value_name, spec$value_name), logical(1)))
    if (length(existing_index) > 0) {
      spec$id <- specs[[existing_index[[1]]]]$id
      specs[[existing_index[[1]]]] <- spec
      status <- sprintf(statedu_text(language, "Updated long variable group: %s", statedu_utf8("6c6f6e6720ebb380ec889820eab7b8eba3b9ec9d8420ec9785eb8db0ec9db4ed8ab8ed9688ec8ab5eb8b88eb8ba43a202573")), wide_long_spec_label(spec))
    } else {
      specs <- c(specs, list(spec))
      status <- sprintf(statedu_text(language, "Set long variable group: %s", statedu_utf8("6c6f6e6720ebb380ec889820eab7b8eba3b9ec9d8420ec84a4eca095ed9688ec8ab5eb8b88eb8ba43a202573")), wide_long_spec_label(spec))
    }
    configured_specs(specs)
    selected_variables(character(0))
    active_list("wide_long_available")
    preview_data(NULL)
    last_message(status)
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$wide_long_remove_spec, {
    language <- statedu_current_language(language_fn)
    selected <- as.character(input$wide_long_configured %||% character(0))
    if (length(selected) == 0) {
      return()
    }
    specs <- configured_specs()
    specs <- specs[!vapply(specs, function(spec) spec$id %in% selected, logical(1))]
    configured_specs(specs)
    preview_data(NULL)
    last_message(statedu_text(language, "Removed configured wide-to-long variable group.", statedu_utf8("ec84a4eca095eb909c20776964652d746f2d6c6f6e6720ebb380ec889820eab7b8eba3b9ec9d8420eca09ceab1b0ed9688ec8ab5eb8b88eb8ba42e")))
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  build_long_data <- function(show_errors = TRUE) {
    language <- statedu_current_language(language_fn)
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    if (is.null(data)) {
      if (isTRUE(show_errors)) showNotification(statedu_text(language, "Load a data file before reshaping data.", statedu_utf8("eb8db0ec9db4ed84b020ebb380ed999820eca084ec979020eb8db0ec9db4ed84b020ed8c8cec9dbcec9d8420ebb688eb9facec98a4ec84b8ec9a942e")), type = "warning", duration = 5)
      return(NULL)
    }
    tryCatch(
      wide_long_transform_configured(
        data = data,
        id_variables = input$wide_long_id_variables,
        keep_other_variables = !identical(input$wide_long_fixed_mode, "selected"),
        fixed_variables = input$wide_long_fixed_variables,
        generated_id_name = input$wide_long_generated_id,
        specs = configured_specs()
      ),
      error = function(e) {
        if (isTRUE(show_errors)) showNotification(conditionMessage(e), type = "warning", duration = 7)
        NULL
      }
    )
  }

  output$wide_long_preview <- DT::renderDT({
    preview <- preview_data()
    if (is.null(preview)) {
      return(NULL)
    }
    preview <- wide_long_preview_display(
      result = preview,
      specs = configured_specs(),
      id_variables = input$wide_long_id_variables,
      fixed_variables = input$wide_long_fixed_variables,
      fixed_mode = input$wide_long_fixed_mode,
      generated_id_name = input$wide_long_generated_id
    )
    limit <- wide_long_preview_limit(configured_specs(), sets = 2L)
    preview <- utils::head(preview, limit)
    DT::datatable(preview, rownames = FALSE, options = list(pageLength = limit, lengthChange = FALSE, paging = FALSE, scrollX = TRUE))
  })

  output$wide_long_message <- renderUI({
    statedu_current_language(language_fn)
    message <- last_message()
    if (is.null(message)) {
      return(NULL)
    }
    div(class = "recode-same-status", message)
  })

  observeEvent(input$preview_wide_long, {
    language <- statedu_current_language(language_fn)
    result <- build_long_data()
    if (is.null(result)) {
      return()
    }
    preview_data(result)
    display <- wide_long_preview_display(
      result = result,
      specs = configured_specs(),
      id_variables = input$wide_long_id_variables,
      fixed_variables = input$wide_long_fixed_variables,
      fixed_mode = input$wide_long_fixed_mode,
      generated_id_name = input$wide_long_generated_id
    )
    last_message(sprintf(statedu_text(language, "Previewed long data: %s row(s), %s displayed variable(s), %s configured long variable(s).", statedu_utf8("6c6f6e6720eb8db0ec9db4ed84b020ebafb8eba6acebb3b4eab8b03a202573ed96892c20ed919cec8b9c20ebb380ec8898202573eab09c2c20ec84a4eca095eb909c206c6f6e6720ebb380ec8898202573eab09c2e")), nrow(result), ncol(display), length(configured_specs())))
  }, ignoreInit = TRUE)

  observeEvent(input$run_wide_long, {
    language <- statedu_current_language(language_fn)
    result <- build_long_data()
    if (is.null(result)) {
      return()
    }
    if (!is.function(replace_dataset_fn)) {
      showNotification(statedu_text(language, "Wide-to-long reshape is not available in this session.", statedu_utf8("ec9db420ec84b8ec8598ec9790ec849ceb8a9420776964652d746f2d6c6f6e6720ebb380ed9998ec9d8420ec82acec9aa9ed95a020ec889820ec9786ec8ab5eb8b88eb8ba42e")), type = "warning", duration = 5)
      return()
    }
    save_result <- tryCatch(
      save_wide_long_result_file(result),
      error = function(e) {
        showNotification(paste(statedu_text(language, "Wide-to-long data was created, but saving failed:", statedu_utf8("776964652d746f2d6c6f6e6720eb8db0ec9db4ed84b0eb8a9420ec839dec84b1eb9090eca780eba78c20eca080ec9ea5ec979020ec8ba4ed8ca8ed9688ec8ab5eb8b88eb8ba43a")), conditionMessage(e)), type = "error", duration = 8)
        list(saved = FALSE, path = "")
      }
    )
    target_name <- if (isTRUE(save_result$saved)) basename(save_result$path) else "wide_to_long.csv"
    target_path <- if (isTRUE(save_result$saved)) save_result$path else NULL
    ok <- replace_dataset_fn(result, name = target_name, path = target_path, csv_header = TRUE)
    if (isTRUE(ok)) {
      preview_data(result)
      if (isTRUE(save_result$saved)) {
        last_message(sprintf(statedu_text(language, "Reshaped current data to long format: %s row(s), %s variable(s). Saved and connected: %s", statedu_utf8("ed9884ec9eac20eb8db0ec9db4ed84b0eba5bc206c6f6e6720666f726d6174ec9cbceba19c20ebb380ed9998ed9688ec8ab5eb8b88eb8ba43a202573ed96892c202573eab09c20ebb380ec88982e20eca080ec9ea520ebb08f20ec97b0eab2b03a202573")), nrow(result), ncol(result), save_result$path))
        showNotification(sprintf(statedu_text(language, "Wide-to-long data saved and connected: %s", statedu_utf8("776964652d746f2d6c6f6e6720eb8db0ec9db4ed84b0eba5bc20eca080ec9ea5ed9598eab3a020ec97b0eab2b0ed9688ec8ab5eb8b88eb8ba43a202573")), save_result$path), type = "message", duration = 6)
      } else {
        last_message(sprintf(statedu_text(language, "Reshaped current data to long format: %s row(s), %s variable(s). Save canceled; a temporary data file is connected for this session.", statedu_utf8("ed9884ec9eac20eb8db0ec9db4ed84b0eba5bc206c6f6e6720666f726d6174ec9cbceba19c20ebb380ed9998ed9688ec8ab5eb8b88eb8ba43a202573ed96892c202573eab09c20ebb380ec88982e20eca080ec9ea5ec9db420ecb7a8ec868ceb9098ec96b420ec9db420ec84b8ec8598ec9790ec849ceb8a9420ec9e84ec8b9c20eb8db0ec9db4ed84b020ed8c8cec9dbcec9db420ec97b0eab2b0eb90a9eb8b88eb8ba42e")), nrow(result), ncol(result)))
      }
      if (is.function(mark_settings_dirty)) {
        mark_settings_dirty()
      }
    }
  }, ignoreInit = TRUE)

  invisible(TRUE)
}
