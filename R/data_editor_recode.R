# Recoding tools for the Data Editor menu.

recode_same_rule_count <- function() 6L

empty_recode_same_rules <- function() {
  data.frame(old = character(0), new = character(0), stringsAsFactors = FALSE)
}

normalize_recode_same_rules <- function(rules) {
  if (is.null(rules) || !is.data.frame(rules) || !all(c("old", "new") %in% names(rules))) {
    return(empty_recode_same_rules())
  }
  rules <- data.frame(
    old = trimws(as.character(rules$old %||% character(0))),
    new = trimws(as.character(rules$new %||% character(0))),
    stringsAsFactors = FALSE
  )
  rules[nzchar(rules$old), , drop = FALSE]
}

recode_missing_marker <- function(value) {
  marker <- toupper(trimws(as.character(value %||% "")))
  marker %in% c("NA", "N/A", "MISSING")
}

recode_output_value <- function(value) {
  value <- trimws(as.character(value %||% ""))
  if (recode_missing_marker(value)) {
    return(NA_character_)
  }
  value
}

recode_same_values <- function(values, rules, keep_unmatched = TRUE) {
  rules <- normalize_recode_same_rules(rules)
  if (nrow(rules) == 0) {
    return(values)
  }

  source <- as.vector(values)
  output <- if (isTRUE(keep_unmatched)) source else rep(NA, length(source))
  source_text <- trimws(as.character(source))
  for (index in seq_len(nrow(rules))) {
    old_value <- trimws(as.character(rules$old[[index]] %||% ""))
    matched <- if (recode_missing_marker(old_value)) {
      is.na(source) | !nzchar(source_text)
    } else {
      !is.na(source) & nzchar(source_text) & source_text == old_value
    }
    output[matched] <- recode_output_value(rules$new[[index]])
  }

  numeric_candidate <- suppressWarnings(as.numeric(output))
  non_missing_text <- trimws(as.character(output[!is.na(output)]))
  if (length(non_missing_text) > 0 && all(nzchar(non_missing_text)) && all(!is.na(numeric_candidate[!is.na(output)]))) {
    return(numeric_candidate)
  }
  output
}

recode_numeric_values <- function(values) {
  if (is.numeric(values)) {
    return(as.numeric(values))
  }
  suppressWarnings(as.numeric(as.character(values)))
}

recode_scalar_numeric <- function(value) {
  value <- suppressWarnings(as.numeric(value %||% NA_real_))
  if (length(value) == 0 || !is.finite(value[[1]])) {
    return(NA_real_)
  }
  value[[1]]
}

recode_observed_values <- function(values, limit = 6L) {
  values <- values[!is.na(values)]
  text <- trimws(as.character(values))
  text <- text[nzchar(text)]
  unique_values <- unique(text)
  numeric_values <- suppressWarnings(as.numeric(unique_values))
  if (length(unique_values) > 0 && all(!is.na(numeric_values))) {
    unique_values <- unique_values[order(numeric_values)]
  } else {
    unique_values <- sort(unique_values)
  }
  utils::head(unique_values, limit)
}

reverse_score_values <- function(values, minimum = NULL, maximum = NULL) {
  numeric_values <- recode_numeric_values(values)
  observed <- numeric_values[!is.na(numeric_values)]
  if (length(observed) == 0) {
    return(numeric_values)
  }
  minimum <- suppressWarnings(as.numeric(minimum %||% min(observed, na.rm = TRUE)))
  maximum <- suppressWarnings(as.numeric(maximum %||% max(observed, na.rm = TRUE)))
  if (!is.finite(minimum) || !is.finite(maximum) || minimum >= maximum) {
    stop("Minimum and maximum must be valid numeric values, and minimum must be smaller than maximum.", call. = FALSE)
  }
  ifelse(is.na(numeric_values), NA_real_, minimum + maximum - numeric_values)
}

recode_same_reverse_summary <- function(data, variables) {
  variables <- intersect(as.character(variables %||% character(0)), names(data %||% data.frame()))
  if (is.null(data) || length(variables) == 0) {
    return(data.frame())
  }
  rows <- lapply(variables, function(name) {
    values <- recode_numeric_values(data[[name]])
    observed <- values[!is.na(values)]
    data.frame(
      Variable = name,
      N = length(observed),
      Min = if (length(observed) > 0) min(observed, na.rm = TRUE) else NA_real_,
      Max = if (length(observed) > 0) max(observed, na.rm = TRUE) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

recode_selected_range <- function(data, variables) {
  variables <- intersect(as.character(variables %||% character(0)), names(data %||% data.frame()))
  if (is.null(data) || length(variables) == 0) {
    return(c(minimum = NA_real_, maximum = NA_real_))
  }
  values <- unlist(lapply(variables, function(name) recode_numeric_values(data[[name]])), use.names = FALSE)
  values <- values[!is.na(values)]
  if (length(values) == 0) {
    return(c(minimum = NA_real_, maximum = NA_real_))
  }
  c(minimum = min(values, na.rm = TRUE), maximum = max(values, na.rm = TRUE))
}

recode_range_issues <- function(data, variables, minimum, maximum) {
  variables <- intersect(as.character(variables %||% character(0)), names(data %||% data.frame()))
  if (is.null(data) || length(variables) == 0) {
    return(data.frame())
  }
  minimum <- suppressWarnings(as.numeric(minimum))
  maximum <- suppressWarnings(as.numeric(maximum))
  if (!is.finite(minimum) || !is.finite(maximum) || minimum >= maximum) {
    stop("Minimum and maximum must be valid numeric values, and minimum must be smaller than maximum.", call. = FALSE)
  }

  rows <- lapply(variables, function(name) {
    raw_values <- data[[name]]
    text_values <- trimws(as.character(raw_values))
    numeric_values <- suppressWarnings(as.numeric(text_values))
    present <- !is.na(raw_values) & nzchar(text_values)
    non_numeric <- present & is.na(numeric_values)
    non_integer <- present & !is.na(numeric_values) & abs(numeric_values - round(numeric_values)) > sqrt(.Machine$double.eps)
    out_of_range <- present & !is.na(numeric_values) & (numeric_values < minimum | numeric_values > maximum)
    issue <- non_numeric | non_integer | out_of_range
    if (!any(issue)) {
      return(NULL)
    }
    reasons <- ifelse(
      non_numeric[issue] | non_integer[issue],
      "Non-integer",
      "Out of range"
    )
    both <- (non_numeric | non_integer) & out_of_range
    reasons[both[issue]] <- "Non-integer; Out of range"
    data.frame(
      Id = which(issue),
      Variable = name,
      Value = text_values[issue],
      Reason = reasons,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(data.frame())
  }
  do.call(rbind, rows)
}

coding_error_issues_display <- function(issues, prefix = "coding_error_fix", corrected_values = NULL) {
  if (is.null(issues) || nrow(issues) == 0) {
    return(data.frame())
  }
  issues <- as.data.frame(issues, stringsAsFactors = FALSE, check.names = FALSE)
  corrected_values <- as.character(corrected_values %||% issues$Value)
  if (length(corrected_values) < nrow(issues)) {
    corrected_values <- c(corrected_values, as.character(issues$Value)[seq.int(length(corrected_values) + 1L, nrow(issues))])
  }
  issues$`Corrected value` <- vapply(seq_len(nrow(issues)), function(index) {
    input_id <- sprintf("%s_%s", prefix, index)
    input_value <- as.character(corrected_values[[index]] %||% issues$Value[[index]] %||% "")
    sprintf(
      paste0(
        '<div class="coding-error-fix-control">',
        '<input id="%s" class="form-control input-sm coding-error-fix-input" type="text" value="%s" data-coding-error-index="%s" ',
        'oninput="window.easyflowCodingErrorFixValues=window.easyflowCodingErrorFixValues||{};window.easyflowCodingErrorFixValues[this.getAttribute(&quot;data-coding-error-index&quot;)]=this.value;">',
        '<button type="button" class="btn btn-default btn-sm coding-error-row-apply" tabindex="-1" ',
        'onclick="if(window.Shiny){var el=document.getElementById(&quot;%s&quot;);window.easyflowCodingErrorFixValues=window.easyflowCodingErrorFixValues||{};window.easyflowCodingErrorFixValues[%s]=el?el.value:&quot;&quot;;Shiny.setInputValue(&quot;coding_error_apply_one&quot;,{index:%s,value:el?el.value:&quot;&quot;,nonce:Date.now()+Math.random()},{priority:&quot;event&quot;});}">',
        'Apply</button>',
        '</div>'
      ),
      input_id,
      htmltools::htmlEscape(input_value),
      index,
      input_id,
      index,
      index
    )
  }, character(1))
  issues[, c("Id", "Variable", "Value", "Corrected value", "Reason"), drop = FALSE]
}

recode_new_variable_name <- function(pattern, variable, literal = TRUE) {
  pattern <- trimws(as.character(pattern %||% ""))
  variable <- as.character(variable %||% "")
  if (!nzchar(pattern)) {
    pattern <- "{variable}_R"
  }
  if (grepl("\\{variable\\}", pattern, fixed = FALSE)) {
    return(gsub("\\{variable\\}", variable, pattern))
  }
  if (identical(pattern, variable)) {
    return(paste0(variable, "_R"))
  }
  if (isTRUE(literal) && !grepl("_R$", pattern)) {
    return(pattern)
  }
  paste0(variable, pattern)
}

recode_source_measurement <- function(variable_info, variable, data = NULL) {
  variable <- as.character(variable %||% "")
  if (is.data.frame(variable_info) && all(c("name", "measurement") %in% names(variable_info))) {
    row_index <- match(variable, as.character(variable_info$name))
    if (!is.na(row_index)) {
      measurement <- as.character(variable_info$measurement[[row_index]] %||% "")
      if (nzchar(measurement)) {
        return(measurement)
      }
    }
  }
  if (!is.null(data) && variable %in% names(data)) {
    return(infer_measurement(data[[variable]]))
  }
  "continuous"
}

recode_default_rule_type <- function(variable, variable_info = NULL, data = NULL) {
  measurement <- tolower(recode_source_measurement(variable_info, variable, data))
  if (measurement %in% c("binary", "ordered", "category", "categorical")) {
    return("single")
  }
  if (identical(measurement, "continuous")) {
    return("category")
  }
  "single"
}

recode_infer_output_measurement <- function(entry, source_measurement = NULL) {
  source_measurement <- tolower(as.character(source_measurement %||% ""))
  values <- character(0)
  if (identical(as.character(entry$rule_type %||% "single"), "category")) {
    category_input <- entry$category_input %||% list()
    values <- c(
      category_input$recode_same_cat_lower_new %||% "",
      vapply(seq_len(max(1L, as.integer(entry$middle_count %||% 1L))), function(index) {
        category_input[[paste0("recode_same_cat_middle_new_", index)]] %||% ""
      }, character(1)),
      category_input$recode_same_cat_upper_new %||% ""
    )
  } else {
    rules <- normalize_recode_same_rules(entry$single_rules %||% empty_recode_same_rules())
    values <- rules$new
  }
  values <- unique(trimws(as.character(values)))
  values <- values[nzchar(values)]
  values <- values[!vapply(values, recode_missing_marker, logical(1))]
  if (length(values) == 0L) {
    return(NULL)
  }
  if (length(values) <= 2L) {
    return("binary")
  }
  if (source_measurement %in% c("category", "categorical")) {
    return("category")
  }
  if (source_measurement %in% c("ordered", "continuous")) {
    return("ordered")
  }
  NULL
}

variable_calculation_choices <- function() {
  c(
    "Mean" = "mean",
    "Sum" = "sum",
    "Standard deviation" = "sd",
    "Variance" = "var"
  )
}

variable_calculation_prefix <- function(operation) {
  switch(
    as.character(operation),
    mean = "M_",
    sum = "S_",
    sd = "SD_",
    var = "Var_",
    ""
  )
}

variable_calculation_label <- function(operation) {
  switch(
    as.character(operation),
    mean = "Mean",
    sum = "Sum",
    sd = "Standard deviation",
    var = "Variance",
    as.character(operation)
  )
}

numeric_matrix_for_variables <- function(data, variables) {
  variables <- intersect(as.character(variables %||% character(0)), names(data %||% data.frame()))
  if (is.null(data) || length(variables) == 0) {
    return(matrix(numeric(0), nrow = nrow(data %||% data.frame()), ncol = 0))
  }
  values <- lapply(variables, function(name) recode_numeric_values(data[[name]]))
  matrix(unlist(values, use.names = FALSE), nrow = nrow(data), ncol = length(values), byrow = FALSE)
}

row_mean_na <- function(values) {
  count <- rowSums(!is.na(values))
  result <- rowMeans(values, na.rm = TRUE)
  result[count == 0] <- NA_real_
  result
}

row_sum_na <- function(values) {
  count <- rowSums(!is.na(values))
  result <- rowSums(values, na.rm = TRUE)
  result[count == 0] <- NA_real_
  result
}

row_sd_na <- function(values) {
  apply(values, 1, function(row) {
    row <- row[!is.na(row)]
    if (length(row) < 2) NA_real_ else stats::sd(row)
  })
}

row_var_na <- function(values) {
  apply(values, 1, function(row) {
    row <- row[!is.na(row)]
    if (length(row) < 2) NA_real_ else stats::var(row)
  })
}

calculate_variable_outputs <- function(data, variables, operations, base_name) {
  variables <- intersect(as.character(variables %||% character(0)), names(data %||% data.frame()))
  operations <- intersect(as.character(operations %||% character(0)), unname(variable_calculation_choices()))
  base_name <- trimws(as.character(base_name %||% ""))
  if (is.null(data) || length(variables) == 0 || length(operations) == 0 || !nzchar(base_name)) {
    return(data.frame(check.names = FALSE))
  }

  values <- numeric_matrix_for_variables(data, variables)
  output <- data.frame(row_id = seq_len(nrow(data)), check.names = FALSE)
  output$row_id <- NULL
  for (operation in operations) {
    name <- paste0(variable_calculation_prefix(operation), base_name)
    output[[name]] <- switch(
      operation,
      mean = row_mean_na(values),
      sum = row_sum_na(values),
      sd = row_sd_na(values),
      var = row_var_na(values)
    )
  }
  output
}

recode_same_rules_from_input <- function(input, prefix = "recode_same") {
  count <- recode_same_rule_count()
  data.frame(
    old = vapply(seq_len(count), function(index) as.character(input[[paste0(prefix, "_old_", index)]] %||% ""), character(1)),
    new = vapply(seq_len(count), function(index) as.character(input[[paste0(prefix, "_new_", index)]] %||% ""), character(1)),
    stringsAsFactors = FALSE
  )
}

recode_rule_label <- function(input) {
  type <- as.character(input$recode_same_rule_type %||% "single")
  if (identical(type, "category")) {
    return("Categorize values")
  }
  "Single value recode"
}

recode_single_rules_from_input <- function(input, count) {
  count <- max(1L, as.integer(count %||% 1L))
  rules <- data.frame(
    old = vapply(seq_len(count), function(index) as.character(input[[paste0("recode_same_single_old_", index)]] %||% ""), character(1)),
    new = vapply(seq_len(count), function(index) as.character(input[[paste0("recode_same_single_new_", index)]] %||% ""), character(1)),
    stringsAsFactors = FALSE
  )
  normalize_recode_same_rules(rules)
}

recode_apply_single_rules <- function(values, rules, keep_unmatched = TRUE) {
  recode_same_values(values, rules, keep_unmatched = keep_unmatched)
}

recode_category_middle_rules_from_input <- function(input, count) {
  count <- max(1L, as.integer(count %||% 1L))
  lapply(seq_len(count), function(index) {
    list(
      from = recode_scalar_numeric(input[[paste0("recode_same_cat_middle_from_", index)]]),
      lower_op = as.character(input[[paste0("recode_same_cat_middle_lower_op_", index)]] %||% "ge"),
      to = recode_scalar_numeric(input[[paste0("recode_same_cat_middle_to_", index)]]),
      upper_op = as.character(input[[paste0("recode_same_cat_middle_upper_op_", index)]] %||% "lt"),
      new = trimws(as.character(input[[paste0("recode_same_cat_middle_new_", index)]] %||% ""))
    )
  })
}

recode_apply_category_rules <- function(values, input, middle_count = 1L, keep_unmatched = TRUE) {
  numeric_values <- recode_numeric_values(values)
  output <- if (isTRUE(keep_unmatched)) as.vector(values) else rep(NA, length(values))

  assign_matches <- function(matches, new_value) {
    new_value <- trimws(as.character(new_value %||% ""))
    if (!nzchar(new_value)) {
      return()
    }
    output[matches & !is.na(matches)] <<- recode_output_value(new_value)
  }

  lower_to <- recode_scalar_numeric(input$recode_same_cat_lower_to)
  if (is.finite(lower_to)) {
    upper_op <- as.character(input$recode_same_cat_lower_upper_op %||% "lt")
    matches <- if (identical(upper_op, "le")) numeric_values <= lower_to else numeric_values < lower_to
    assign_matches(matches, input$recode_same_cat_lower_new)
  }

  for (rule in recode_category_middle_rules_from_input(input, middle_count)) {
    if (is.finite(rule$from) && is.finite(rule$to)) {
      lower_match <- if (identical(rule$lower_op, "gt")) numeric_values > rule$from else numeric_values >= rule$from
      upper_match <- if (identical(rule$upper_op, "le")) numeric_values <= rule$to else numeric_values < rule$to
      assign_matches(lower_match & upper_match, rule$new)
    }
  }

  upper_from <- recode_scalar_numeric(input$recode_same_cat_upper_from)
  if (is.finite(upper_from)) {
    lower_op <- as.character(input$recode_same_cat_upper_lower_op %||% "ge")
    matches <- if (identical(lower_op, "gt")) numeric_values > upper_from else numeric_values >= upper_from
    assign_matches(matches, input$recode_same_cat_upper_new)
  }

  numeric_candidate <- suppressWarnings(as.numeric(output))
  non_missing_text <- trimws(as.character(output[!is.na(output)]))
  if (length(non_missing_text) > 0 && all(nzchar(non_missing_text)) && all(!is.na(numeric_candidate[!is.na(output)]))) {
    return(numeric_candidate)
  }
  output
}

recode_unmatched_values <- function(values, entry, limit = 20L) {
  values <- values[!is.na(values)]
  if (length(values) == 0) {
    return(character(0))
  }
  value_text <- trimws(as.character(values))
  present <- nzchar(value_text)

  if (identical(as.character(entry$rule_type %||% "single"), "category")) {
    numeric_values <- recode_numeric_values(values)
    input <- entry$category_input %||% list()
    matched <- rep(FALSE, length(values))

    lower_to <- recode_scalar_numeric(input$recode_same_cat_lower_to)
    lower_new <- trimws(as.character(input$recode_same_cat_lower_new %||% ""))
    if (is.finite(lower_to) && nzchar(lower_new)) {
      upper_op <- as.character(input$recode_same_cat_lower_upper_op %||% "lt")
      matched <- matched | if (identical(upper_op, "le")) numeric_values <= lower_to else numeric_values < lower_to
    }

    for (rule in recode_category_middle_rules_from_input(input, entry$middle_count %||% 1L)) {
      if (is.finite(rule$from) && is.finite(rule$to) && nzchar(rule$new)) {
        lower_match <- if (identical(rule$lower_op, "gt")) numeric_values > rule$from else numeric_values >= rule$from
        upper_match <- if (identical(rule$upper_op, "le")) numeric_values <= rule$to else numeric_values < rule$to
        matched <- matched | (lower_match & upper_match)
      }
    }

    upper_from <- recode_scalar_numeric(input$recode_same_cat_upper_from)
    upper_new <- trimws(as.character(input$recode_same_cat_upper_new %||% ""))
    if (is.finite(upper_from) && nzchar(upper_new)) {
      lower_op <- as.character(input$recode_same_cat_upper_lower_op %||% "ge")
      matched <- matched | if (identical(lower_op, "gt")) numeric_values > upper_from else numeric_values >= upper_from
    }

    matched[is.na(matched)] <- FALSE
    unmatched <- value_text[present & !matched]
  } else {
    rules <- normalize_recode_same_rules(entry$single_rules %||% empty_recode_same_rules())
    old_values <- trimws(as.character(rules$old))
    unmatched <- value_text[present & !value_text %in% old_values]
  }

  unique_values <- unique(unmatched)
  utils::head(unique_values, limit)
}

recode_unmatched_message <- function(values, entry) {
  if (!isTRUE(entry$keep_unmatched)) {
    return(NULL)
  }
  unmatched <- recode_unmatched_values(values, entry)
  if (length(unmatched) == 0) {
    return(NULL)
  }
  all_unmatched <- recode_unmatched_values(values, entry, limit = .Machine$integer.max)
  suffix <- if (length(all_unmatched) > length(unmatched)) ", ..." else ""
  sprintf("Unmatched values kept as-is: %s%s", paste(unmatched, collapse = ", "), suffix)
}

recode_entry_values <- function(values, entry) {
  if (identical(as.character(entry$rule_type %||% "single"), "category")) {
    return(recode_apply_category_rules(
      values,
      entry$category_input %||% list(),
      middle_count = entry$middle_count %||% 1L,
      keep_unmatched = isTRUE(entry$keep_unmatched)
    ))
  }
  recode_apply_single_rules(
    values,
    entry$single_rules %||% empty_recode_same_rules(),
    keep_unmatched = isTRUE(entry$keep_unmatched)
  )
}

recode_current_entry_from_input <- function(input, single_count, middle_count) {
  target <- as.character(input$recode_same_target %||% "same")
  if (!target %in% c("same", "new")) {
    target <- "same"
  }
  old_variable <- trimws(as.character(input$recode_same_old_variable %||% ""))
  new_variable <- if (identical(target, "same")) old_variable else trimws(as.character(input$recode_same_new_name %||% ""))
  rule_type <- as.character(input$recode_same_rule_type %||% "single")
  if (!rule_type %in% c("single", "category")) {
    rule_type <- "single"
  }
  measurement <- trimws(as.character(input$recode_same_measurement %||% ""))

  category_input <- list(
    recode_same_cat_lower_to = input$recode_same_cat_lower_to %||% "",
    recode_same_cat_lower_upper_op = input$recode_same_cat_lower_upper_op %||% "lt",
    recode_same_cat_lower_new = input$recode_same_cat_lower_new %||% "",
    recode_same_cat_upper_from = input$recode_same_cat_upper_from %||% "",
    recode_same_cat_upper_lower_op = input$recode_same_cat_upper_lower_op %||% "ge",
    recode_same_cat_upper_new = input$recode_same_cat_upper_new %||% ""
  )
  middle_count <- max(1L, as.integer(middle_count %||% 1L))
  for (index in seq_len(middle_count)) {
    category_input[[paste0("recode_same_cat_middle_from_", index)]] <- input[[paste0("recode_same_cat_middle_from_", index)]] %||% ""
    category_input[[paste0("recode_same_cat_middle_lower_op_", index)]] <- input[[paste0("recode_same_cat_middle_lower_op_", index)]] %||% "ge"
    category_input[[paste0("recode_same_cat_middle_to_", index)]] <- input[[paste0("recode_same_cat_middle_to_", index)]] %||% ""
    category_input[[paste0("recode_same_cat_middle_upper_op_", index)]] <- input[[paste0("recode_same_cat_middle_upper_op_", index)]] %||% "lt"
    category_input[[paste0("recode_same_cat_middle_new_", index)]] <- input[[paste0("recode_same_cat_middle_new_", index)]] %||% ""
  }

  list(
    old = old_variable,
    new = new_variable,
    target = target,
    rule_type = rule_type,
    rule = if (identical(rule_type, "category")) "Categorize values" else "Single value recode",
    measurement = if (nzchar(measurement)) measurement else NULL,
    keep_unmatched = isTRUE(input$recode_same_keep_unmatched),
    single_rules = recode_single_rules_from_input(input, single_count),
    category_input = category_input,
    middle_count = middle_count
  )
}

recode_applied_table_ui <- function(entries = list(), selected_index = NULL) {
  if (length(entries) == 0) {
    rows <- list(tags$tr(tags$td(colspan = 4, class = "recode-builder-empty-cell", "No variables queued yet.")))
  } else {
    rows <- lapply(seq_along(entries), function(index) {
      entry <- entries[[index]]
      select_call <- sprintf(
        "Shiny.setInputValue('recode_same_queue_select',{index:%s,nonce:Date.now()+Math.random()},{priority:'event'});",
        index
      )
      delete_call <- sprintf(
        "event.stopPropagation();Shiny.setInputValue('recode_same_queue_delete',{index:%s,nonce:Date.now()+Math.random()},{priority:'event'});",
        index
      )
      tags$tr(
        class = if (identical(as.integer(selected_index %||% NA_integer_), index)) "is-selected" else NULL,
        onclick = select_call,
        tags$td(as.character(entry$old %||% "")),
        tags$td(as.character(entry$new %||% "")),
        tags$td(as.character(entry$rule %||% "")),
        tags$td(tags$button(type = "button", class = "btn btn-default btn-xs recode-builder-delete-button", onclick = delete_call, "Delete"))
      )
    })
  }

  tags$table(
    class = "recode-builder-applied-table",
    tags$thead(tags$tr(tags$th("Old variable"), tags$th("New variable"), tags$th("Rule"), tags$th(""))),
    tags$tbody(rows)
  )
}

recode_single_rules_ui <- function(count, input = NULL) {
  count <- max(2L, as.integer(count %||% 2L))
  tagList(
    div(
      class = "recode-builder-rule-header recode-builder-single-header",
      span("Existing value"),
      span("New value")
    ),
    div(
      class = "recode-builder-single-rules",
      lapply(seq_len(count), function(index) {
        div(
          class = "recode-builder-single-row",
          textInput(
            paste0("recode_same_single_old_", index),
            NULL,
            value = isolate(input[[paste0("recode_same_single_old_", index)]] %||% ""),
            width = "100%"
          ),
          textInput(
            paste0("recode_same_single_new_", index),
            NULL,
            value = isolate(input[[paste0("recode_same_single_new_", index)]] %||% ""),
            width = "100%"
          )
        )
      })
    )
  )
}

recode_category_middle_rows_ui <- function(count, input = NULL) {
  count <- max(1L, as.integer(count %||% 1L))
  tagList(lapply(seq_len(count), function(index) {
    div(class = "recode-builder-category-row recode-builder-category-middle-row",
      textInput(paste0("recode_same_cat_middle_from_", index), NULL, value = isolate(input[[paste0("recode_same_cat_middle_from_", index)]] %||% ""), width = "100%"),
      selectInput(paste0("recode_same_cat_middle_lower_op_", index), NULL, choices = c("\u2265" = "ge", ">" = "gt"), selected = isolate(input[[paste0("recode_same_cat_middle_lower_op_", index)]] %||% "ge"), selectize = FALSE, width = "100%"),
      textInput(paste0("recode_same_cat_middle_to_", index), NULL, value = isolate(input[[paste0("recode_same_cat_middle_to_", index)]] %||% ""), width = "100%"),
      selectInput(paste0("recode_same_cat_middle_upper_op_", index), NULL, choices = c("<" = "lt", "\u2264" = "le"), selected = isolate(input[[paste0("recode_same_cat_middle_upper_op_", index)]] %||% "lt"), selectize = FALSE, width = "100%"),
      textInput(paste0("recode_same_cat_middle_new_", index), NULL, value = isolate(input[[paste0("recode_same_cat_middle_new_", index)]] %||% ""), width = "100%")
    )
  }))
}

coding_error_check_setup_panel <- function(file, data, variable_info, labels = character(0), selected_variables = character(0), input = NULL) {
  if (is.null(file) || is.null(data)) {
    return(setup_empty_message("Load a data file in the Data tab before checking coding errors."))
  }

  variables <- names(data)
  selected_variables <- intersect(as.character(selected_variables %||% character(0)), variables)
  available <- setdiff(variables, selected_variables)
  selected_available <- selected_order_items(isolate(input$coding_error_available) %||% character(0), available)
  selected_selected <- selected_order_items(isolate(input$coding_error_selected) %||% character(0), selected_variables)
  observed <- recode_selected_range(data, selected_variables)
  default_minimum <- if (is.finite(observed[["minimum"]])) observed[["minimum"]] else 1
  default_maximum <- if (is.finite(observed[["maximum"]])) observed[["maximum"]] else 5

  div(
    class = "recode-same-setup-grid recode-different-setup-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Variables"),
      analysis_transfer_listbox_input(
        "coding_error_available",
        items = analysis_variable_items(available, variable_info, labels),
        selected = selected_available,
        size = 18
      )
    ),
    div(
      class = "analysis-transfer-controls recode-same-transfer-controls",
      actionButton(
        "coding_error_move",
        ">",
        class = "btn btn-default analysis-move-button",
        disabled = if (length(available) == 0 && length(selected_variables) == 0) "disabled" else NULL
      )
    ),
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Variables to check", analysis_allowed_measurements_all()),
      analysis_transfer_listbox_input(
        "coding_error_selected",
        items = analysis_variable_items(selected_variables, variable_info, labels),
        selected = selected_selected,
        size = 18
      ),
      div(
        class = "dependent-order-actions",
        actionButton("coding_error_up", "Up", class = "btn-default btn-sm"),
        actionButton("coding_error_down", "Down", class = "btn-default btn-sm")
      )
    ),
    div(
      class = "analysis-options-column analysis-options-panel recode-different-options",
      div(class = "analysis-option-group recode-range-check-options",
        div(class = "analysis-option-title", "Auto coding error check"),
        div(class = "recode-help-text", "Run checks values outside the range and non-integer values."),
        div(class = "recode-observed-range",
          span("Observed range"),
          tags$strong(if (is.finite(observed[["minimum"]]) && is.finite(observed[["maximum"]])) {
            sprintf("%s to %s", observed[["minimum"]], observed[["maximum"]])
          } else {
            "No numeric values"
          })
        ),
        div(
          class = "recode-reverse-range",
          numericInput("coding_error_min", "Minimum", value = default_minimum, min = -Inf, step = 1, width = "100%"),
          numericInput("coding_error_max", "Maximum", value = default_maximum, min = -Inf, step = 1, width = "100%")
        )
      )
    )
  )
}

data_editor_coding_error_check_panel <- function() {
  div(
    class = "page-shell",
    div(
      class = "app-heading",
      h1("Auto Coding Error Check"),
      div("Check selected variables for out-of-range values and non-integer values.", class = "app-subtitle")
    ),
    div(
      class = "workspace-panel frequencies-workspace-panel data-editor-workspace",
      analysis_workspace_heading("Auto coding error check", "coding_error"),
      analysis_workspace_body(
        "coding_error",
        uiOutput("coding_error_setup"),
        div(
          class = "analysis-action-row recode-same-action-row",
          actionButton("apply_coding_error", "Run", class = "btn btn-primary"),
          uiOutput("coding_error_reset_control")
        ),
        uiOutput("coding_error_message"),
        div(
          class = "coding-error-output",
          div(
            class = "coding-error-output-actions",
            tags$button(
              id = "apply_coding_error_corrections",
              type = "button",
              class = "btn btn-default",
              onclick = paste0(
                "if(window.Shiny){",
                "window.easyflowCodingErrorFixValues=window.easyflowCodingErrorFixValues||{};",
                "document.querySelectorAll('.coding-error-fix-input').forEach(function(el){",
                "var idx=el.getAttribute('data-coding-error-index');",
                "if(idx){window.easyflowCodingErrorFixValues[idx]=el.value;}",
                "});",
                "Shiny.setInputValue('coding_error_apply_all_values',{values:window.easyflowCodingErrorFixValues,nonce:Date.now()+Math.random()},{priority:'event'});",
                "}"
              ),
              "Apply all corrections"
            )
          ),
          DT::DTOutput("coding_error_issues")
        )
      )
    )
  )
}

recode_different_setup_panel <- function(file, data, variable_info, labels = character(0), selected_variables = character(0), input = NULL) {
  if (is.null(file) || is.null(data)) {
    return(setup_empty_message("Load a data file in the Data tab before using auto reverse coding."))
  }

  variables <- names(data)
  selected_variables <- intersect(as.character(selected_variables %||% character(0)), variables)
  available <- setdiff(variables, selected_variables)
  selected_available <- selected_order_items(isolate(input$recode_different_available) %||% character(0), available)
  selected_selected <- selected_order_items(isolate(input$recode_different_selected) %||% character(0), selected_variables)
  observed <- recode_selected_range(data, selected_variables)
  default_minimum <- if (is.finite(observed[["minimum"]])) observed[["minimum"]] else 1
  default_maximum <- if (is.finite(observed[["maximum"]])) observed[["maximum"]] else 5
  current_pattern <- isolate(input$recode_different_new_name) %||% "{variable}_R"
  current_target <- isolate(input$recode_different_target) %||% "new"
  if (!current_target %in% c("new", "same")) {
    current_target <- "new"
  }

  div(
    class = "recode-same-setup-grid recode-different-setup-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Variables"),
      analysis_transfer_listbox_input(
        "recode_different_available",
        items = analysis_variable_items(available, variable_info, labels),
        selected = selected_available,
        size = 18
      )
    ),
    div(
      class = "analysis-transfer-controls recode-same-transfer-controls",
      actionButton(
        "recode_different_move",
        ">",
        class = "btn btn-default analysis-move-button",
        disabled = if (length(available) == 0 && length(selected_variables) == 0) "disabled" else NULL
      )
    ),
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Variables to reverse-code", analysis_allowed_measurements_all()),
      analysis_transfer_listbox_input(
        "recode_different_selected",
        items = analysis_variable_items(selected_variables, variable_info, labels),
        selected = selected_selected,
        size = 18
      ),
      div(
        class = "dependent-order-actions",
        actionButton("recode_different_up", "Up", class = "btn-default btn-sm"),
        actionButton("recode_different_down", "Down", class = "btn-default btn-sm")
      )
    ),
    div(
      class = "analysis-options-column analysis-options-panel recode-different-options",
      div(class = "analysis-option-group recode-auto-options",
        div(class = "analysis-option-title", "Save result to"),
        radioButtons(
          "recode_different_target",
          label = NULL,
          choices = c("New variables" = "new", "Same variables" = "same"),
          selected = current_target
        ),
        div(class = "recode-observed-range",
          span("Observed range"),
          tags$strong(if (is.finite(observed[["minimum"]]) && is.finite(observed[["maximum"]])) {
            sprintf("%s to %s", observed[["minimum"]], observed[["maximum"]])
          } else {
            "No numeric values"
          })
        ),
        div(
          class = "recode-reverse-range",
          numericInput("recode_different_min", "Minimum", value = default_minimum, min = -Inf, step = 1, width = "100%"),
          numericInput("recode_different_max", "Maximum", value = default_maximum, min = -Inf, step = 1, width = "100%")
        )
      ),
      conditionalPanel(
        condition = "input.recode_different_target == 'new'",
        div(class = "analysis-option-group recode-new-name-options",
          textInput("recode_different_new_name", "New variable name", value = current_pattern, width = "100%"),
          div(class = "recode-help-text", "Use {variable} for the original variable name, for example {variable}_R.")
        )
      )
    )
  )
}

data_editor_different_variable_panel <- function() {
  div(
    class = "page-shell",
    div(
      class = "app-heading",
      h1("Auto Reverse Coding"),
      div("Create new reverse-coded variables while preserving the original variables.", class = "app-subtitle")
    ),
    div(
      class = "workspace-panel frequencies-workspace-panel data-editor-workspace",
      analysis_workspace_heading("Auto reverse coding", "recode_different"),
      analysis_workspace_body(
        "recode_different",
        uiOutput("recode_different_setup"),
        div(
          class = "analysis-action-row recode-same-action-row",
          actionButton("apply_recode_different", "Run", class = "btn btn-primary"),
          uiOutput("recode_different_reset_control")
        ),
        uiOutput("recode_different_message"),
        div(class = "data-editor-result-output", DT::DTOutput("recode_different_issues"))
      )
    )
  )
}

variable_calculation_setup_panel <- function(file, data, variable_info, labels = character(0), selected_variables = character(0), input = NULL) {
  if (is.null(file) || is.null(data)) {
    return(setup_empty_message("Load a data file in the Data tab before calculating variables."))
  }

  variables <- names(data)
  selected_variables <- intersect(as.character(selected_variables %||% character(0)), variables)
  available <- setdiff(variables, selected_variables)
  selected_available <- selected_order_items(isolate(input$variable_calculation_available) %||% character(0), available)
  selected_selected <- selected_order_items(isolate(input$variable_calculation_selected) %||% character(0), selected_variables)
  current_base <- isolate(input$variable_calculation_base_name) %||% "score"
  current_operations <- intersect(
    as.character(isolate(input$variable_calculation_operations) %||% character(0)),
    unname(variable_calculation_choices())
  )
  current_reliability <- isTRUE(isolate(input$variable_calculation_reliability) %||% FALSE)

  div(
    class = "recode-same-setup-grid recode-different-setup-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Variables"),
      analysis_transfer_listbox_input(
        "variable_calculation_available",
        items = analysis_variable_items(available, variable_info, labels),
        selected = selected_available,
        size = 18
      )
    ),
    div(
      class = "analysis-transfer-controls recode-same-transfer-controls",
      actionButton(
        "variable_calculation_move",
        ">",
        class = "btn btn-default analysis-move-button",
        disabled = if (length(available) == 0 && length(selected_variables) == 0) "disabled" else NULL
      )
    ),
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Variables to calculate", analysis_allowed_measurements_all()),
      analysis_transfer_listbox_input(
        "variable_calculation_selected",
        items = analysis_variable_items(selected_variables, variable_info, labels),
        selected = selected_selected,
        size = 18
      ),
      div(
        class = "dependent-order-actions",
        actionButton("variable_calculation_up", "Up", class = "btn-default btn-sm"),
        actionButton("variable_calculation_down", "Down", class = "btn-default btn-sm")
      )
    ),
    div(
      class = "analysis-options-column analysis-options-panel recode-different-options variable-calculation-options",
      div(class = "analysis-option-group",
        div(class = "analysis-option-title", "Variable calculation"),
        checkboxGroupInput(
          "variable_calculation_operations",
          label = NULL,
          choices = variable_calculation_choices(),
          selected = current_operations
        ),
        textInput("variable_calculation_base_name", "Variable name", value = current_base, width = "100%"),
        div(
          class = "recode-help-text variable-calculation-help",
          "Created variables use M_, S_, SD_, and Var_ prefixes, for example M_score."
        )
      ),
      div(class = "analysis-option-group variable-calculation-reliability-options",
        div(class = "analysis-option-title", "Reliability"),
        checkboxInput("variable_calculation_reliability", "Reliability", value = current_reliability)
      )
    )
  )
}

data_editor_variable_calculation_panel <- function() {
  div(
    class = "page-shell",
    div(
      class = "app-heading",
      h1("Auto Variable Calculation"),
      div("Create row-wise summary variables from selected variables.", class = "app-subtitle")
    ),
    div(
      class = "workspace-panel frequencies-workspace-panel data-editor-workspace",
      analysis_workspace_heading("Auto variable calculation", "variable_calculation"),
      analysis_workspace_body(
        "variable_calculation",
        uiOutput("variable_calculation_setup"),
        div(
          class = "analysis-action-row recode-same-action-row",
          actionButton("apply_variable_calculation", "Run", class = "btn btn-primary"),
          uiOutput("variable_calculation_reset_control")
        ),
        uiOutput("variable_calculation_message"),
        div(class = "data-editor-result-output", DT::DTOutput("variable_calculation_preview")),
        div(class = "data-editor-result-output variable-calculation-reliability-output", uiOutput("variable_calculation_reliability_results"))
      )
    )
  )
}

recode_same_setup_panel <- function(file, data, variable_info, labels = character(0), selected_variables = character(0), input = NULL) {
  if (is.null(file) || is.null(data)) {
    return(setup_empty_message("Load a data file in the Data tab before using same-variable recoding."))
  }

  variables <- names(data)
  selected_available <- selected_order_items(isolate(input$recode_same_available) %||% character(0), variables)
  measurement_choices <- c(
    "Keep current type" = "",
    "Binary" = "binary",
    "Categorical" = "category",
    "Ordered" = "ordered",
    "Continuous" = "continuous"
  )
  current_target <- isolate(input$recode_same_target) %||% "same"
  if (!current_target %in% c("same", "new")) {
    current_target <- "same"
  }
  current_old <- isolate(input$recode_same_old_variable) %||% selected_available %||% ""
  current_new <- isolate(input$recode_same_new_name) %||% ""

  div(
    class = "recode-same-setup-grid recode-builder-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Variables"),
      analysis_transfer_listbox_input(
        "recode_same_available",
        items = analysis_variable_items(variables, variable_info, labels),
        selected = selected_available,
        size = 19
      )
    ),
    div(
      class = "analysis-transfer-column analysis-transfer-panel recode-builder-target-panel",
      div(class = "analysis-option-title", "Variable target"),
      div(
        class = "recode-builder-name-row",
        textInput("recode_same_old_variable", "Old variable", value = current_old, width = "100%"),
        textInput("recode_same_new_name", "New variable name", value = current_new, width = "100%")
      ),
      div(
        class = "analysis-option-group recode-auto-options recode-builder-target-choice",
        radioButtons(
          "recode_same_target",
          label = NULL,
          choices = c("Same name" = "same", "Different name" = "new"),
          selected = current_target,
          inline = TRUE
        )
      ),
      div(class = "variable-rename-queue-title", "Applied variables"),
      div(class = "recode-builder-applied-list", uiOutput("recode_same_applied_list")),
      tags$script(HTML(paste0(
        "(function(){\n",
        "  function toggleRecodeNewName(){\n",
        "    var same = $('input[name=\"recode_same_target\"]:checked').val() === 'same';\n",
        "    $('#recode_same_new_name').prop('disabled', same);\n",
        "  }\n",
        "  setTimeout(toggleRecodeNewName, 0);\n",
        "  $(document).off('change.easyflowRecodeSameTarget', 'input[name=\"recode_same_target\"]');\n",
        "  $(document).on('change.easyflowRecodeSameTarget', 'input[name=\"recode_same_target\"]', toggleRecodeNewName);\n",
        "})();"
      )))
    ),
    div(
      class = "analysis-options-column analysis-options-panel recode-same-options recode-builder-rule-panel",
      div(class = "analysis-option-group",
        div(class = "analysis-option-title", "Recoding rules"),
        radioButtons(
          "recode_same_rule_type",
          label = NULL,
          choices = c("Single value recode" = "single", "Categorize values" = "category"),
          selected = isolate(input$recode_same_rule_type) %||% "single"
        ),
        conditionalPanel(
          condition = "input.recode_same_rule_type == 'single'",
          uiOutput("recode_same_single_rules")
        ),
        conditionalPanel(
          condition = "input.recode_same_rule_type == 'category'",
          div(
            class = "recode-builder-category-rules",
            div(
              class = "recode-builder-rule-header recode-builder-category-header",
              span("From"),
              span("Op"),
              span("To"),
              span("Op"),
              span("New")
            ),
            div(class = "recode-builder-category-row",
              span(class = "recode-builder-range-anchor", "Min"),
              span(),
              textInput("recode_same_cat_lower_to", NULL, value = isolate(input$recode_same_cat_lower_to) %||% "", width = "100%"),
              selectInput("recode_same_cat_lower_upper_op", NULL, choices = c("<" = "lt", "\u2264" = "le"), selected = isolate(input$recode_same_cat_lower_upper_op) %||% "lt", selectize = FALSE, width = "100%"),
              textInput("recode_same_cat_lower_new", NULL, value = isolate(input$recode_same_cat_lower_new) %||% "", width = "100%")
            ),
            uiOutput("recode_same_category_middle_rules"),
            div(class = "recode-builder-category-row",
              textInput("recode_same_cat_upper_from", NULL, value = isolate(input$recode_same_cat_upper_from) %||% "", width = "100%"),
              selectInput("recode_same_cat_upper_lower_op", NULL, choices = c("\u2265" = "ge", ">" = "gt"), selected = isolate(input$recode_same_cat_upper_lower_op) %||% "ge", selectize = FALSE, width = "100%"),
              span(class = "recode-builder-range-anchor", "Max"),
              span(),
              textInput("recode_same_cat_upper_new", NULL, value = isolate(input$recode_same_cat_upper_new) %||% "", width = "100%")
            )
          )
        )
      ),
      checkboxInput("recode_same_keep_unmatched", "Keep unmatched values", value = TRUE),
      selectInput("recode_same_measurement", "Measurement after recoding", choices = measurement_choices, selected = "", selectize = FALSE),
      actionButton("add_recode_same", "Add", class = "btn btn-primary recode-builder-apply-button")
    )
  )
}

data_editor_same_variable_panel <- function() {
  div(
    class = "page-shell",
    div(
      class = "app-heading",
      h1("Recode Variable"),
      div("Change coding values in selected variables and save the result to the same variables or new variables.", class = "app-subtitle")
    ),
    div(
      class = "workspace-panel frequencies-workspace-panel data-editor-workspace",
      analysis_workspace_heading("Recode variable", "recode_same"),
      analysis_workspace_body(
        "recode_same",
        uiOutput("recode_same_setup"),
        div(
          class = "analysis-action-row recode-same-action-row recode-builder-action-row",
          actionButton("apply_recode_same", "Apply", class = "btn btn-primary"),
          uiOutput("recode_same_reset_control")
        ),
        uiOutput("recode_same_message"),
        div(class = "data-editor-result-output", DT::DTOutput("recode_same_preview"))
      )
    )
  )
}

register_recode_same_handlers <- function(
  input,
  output,
  session,
  dataset_fn,
  current_data_file_fn,
  selected_names_fn,
  variable_info_fn,
  labels_fn,
  category_table_fn,
  update_existing_variable_fn,
  add_calculated_variable_fn = NULL,
  mark_settings_dirty
) {
  selected_variables <- reactiveVal(character(0))
  active_list <- reactiveVal(NULL)
  last_message <- reactiveVal(NULL)
  applied_entries <- reactiveVal(list())
  selected_queue_index <- reactiveVal(NULL)
  single_rule_count <- reactiveVal(2L)
  single_rule_max <- reactiveVal(NULL)
  category_middle_count <- reactiveVal(1L)

  output$recode_same_setup <- renderUI({
    recode_same_setup_panel(
      file = current_data_file_fn(),
      data = tryCatch(dataset_fn(), error = function(e) NULL),
      variable_info = tryCatch(variable_info_fn(), error = function(e) NULL),
      labels = labels_fn(),
      selected_variables = selected_variables(),
      input = input
    )
  })

  register_analysis_data_viewer_handlers(
    input = input,
    output = output,
    prefix = "recode_same",
    title = "Same-variable Recoding Data Viewer",
    dataset_fn = dataset_fn,
    selected_names_fn = selected_names_fn,
    variables_fn = selected_variables,
    variable_table_fn = variable_info_fn,
    labels_fn = labels_fn,
    category_table_fn = category_table_fn
  )

  observe({
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    if (is.null(data)) {
      selected_variables(character(0))
      applied_entries(list())
      selected_queue_index(NULL)
      return()
    }
    current <- selected_variables()
    updated <- intersect(current, names(data))
    if (!identical(updated, current)) {
      selected_variables(updated)
    }
  })

  output$recode_same_single_rules <- renderUI({
    recode_single_rules_ui(single_rule_count(), input)
  })

  output$recode_same_category_middle_rules <- renderUI({
    recode_category_middle_rows_ui(category_middle_count(), input)
  })

  observe({
    count <- single_rule_count()
    last_new <- trimws(as.character(input[[paste0("recode_same_single_new_", count)]] %||% ""))
    max_count <- single_rule_max()
    can_grow <- is.null(max_count) || count < max_count
    if (nzchar(last_new) && isTRUE(can_grow)) {
      single_rule_count(count + 1L)
    }
  })

  observe({
    count <- category_middle_count()
    last_new <- trimws(as.character(input[[paste0("recode_same_cat_middle_new_", count)]] %||% ""))
    if (nzchar(last_new)) {
      category_middle_count(count + 1L)
    }
  })

  observe({
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    variable_info <- tryCatch(variable_info_fn(), error = function(e) NULL)
    entry <- recode_current_entry_from_input(input, single_rule_count(), category_middle_count())
    if (!nzchar(entry$old)) {
      return()
    }
    source_measurement <- recode_source_measurement(variable_info, entry$old, data)
    inferred <- recode_infer_output_measurement(entry, source_measurement)
    current <- as.character(input$recode_same_measurement %||% "")
    if (!is.null(inferred) && !identical(current, inferred)) {
      updateSelectInput(session, "recode_same_measurement", selected = inferred)
    }
  })

  observeEvent(input$recode_same_available_active, {
    active_list("recode_same_available")
    value <- as.character(input$recode_same_available %||% character(0))
    value <- value[nzchar(value)]
    if (length(value) > 0) {
      selected_name <- value[[1]]
      data <- tryCatch(dataset_fn(), error = function(e) NULL)
      variable_info <- tryCatch(variable_info_fn(), error = function(e) NULL)
      source_measurement <- tolower(recode_source_measurement(variable_info, selected_name, data))
      selected_queue_index(NULL)
      updateTextInput(session, "recode_same_old_variable", value = selected_name)
      updateRadioButtons(
        session,
        "recode_same_rule_type",
        selected = recode_default_rule_type(
          selected_name,
          variable_info = variable_info,
          data = data
        )
      )
      if (source_measurement %in% c("binary", "ordered", "category", "categorical") &&
          !is.null(data) && selected_name %in% names(data)) {
        values <- recode_observed_values(data[[selected_name]], limit = 20L)
        row_count <- if (identical(source_measurement, "binary")) {
          2L
        } else {
          max(2L, length(values) + 1L)
        }
        single_rule_count(row_count)
        single_rule_max(if (identical(source_measurement, "binary")) 2L else NULL)
        session$onFlushed(function() {
          for (row_index in seq_len(row_count)) {
            updateTextInput(
              session,
              paste0("recode_same_single_old_", row_index),
              value = if (length(values) >= row_index) values[[row_index]] else ""
            )
            updateTextInput(session, paste0("recode_same_single_new_", row_index), value = "")
          }
        }, once = TRUE)
      } else {
        single_rule_max(NULL)
      }
      if (identical(as.character(input$recode_same_target %||% "same"), "new") &&
          !nzchar(trimws(as.character(input$recode_same_new_name %||% "")))) {
        updateTextInput(session, "recode_same_new_name", value = paste0(selected_name, "_recode"))
      }
    }
  }, ignoreInit = TRUE)
  observeEvent(input$recode_same_selected_active, active_list("recode_same_selected"), ignoreInit = TRUE)

  observe({
    if (identical(active_list(), "recode_same_selected") && length(input$recode_same_selected %||% character(0)) > 0) {
      updateActionButton(session, "recode_same_move", label = "<")
    } else {
      updateActionButton(session, "recode_same_move", label = ">")
    }
  })

  observeEvent(input$recode_same_move, {
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    if (is.null(data)) return()
    variables <- names(data)
    current <- intersect(selected_variables(), variables)
    if (identical(active_list(), "recode_same_selected")) {
      chosen <- intersect(as.character(input$recode_same_selected %||% character(0)), current)
      if (length(chosen) == 0) return()
      selected_variables(setdiff(current, chosen))
      active_list("recode_same_available")
      mark_settings_dirty()
      return()
    }

    chosen <- intersect(as.character(input$recode_same_available %||% character(0)), setdiff(variables, current))
    if (length(chosen) == 0) return()
    selected_variables(c(current, chosen))
    active_list("recode_same_selected")
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$recode_same_selected_doubleclick, {
    current <- selected_variables()
    chosen <- intersect(as.character(input$recode_same_selected_doubleclick$value %||% ""), current)
    if (length(chosen) == 0) return()
    selected_variables(setdiff(current, chosen))
    active_list("recode_same_available")
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  register_dual_transfer_drop_observer(
    input = input,
    session = session,
    available_id = "recode_same_available",
    selected_id = "recode_same_selected",
    selected_values = selected_variables,
    all_values_fn = function() names(tryCatch(dataset_fn(), error = function(e) data.frame()) %||% data.frame()),
    active_list = active_list,
    mark_settings_dirty = mark_settings_dirty
  )

  observeEvent(input$recode_same_up, {
    updated <- move_order_item(selected_variables(), input$recode_same_selected, "up")
    if (isTRUE(updated$changed)) {
      selected_variables(updated$order)
      mark_settings_dirty()
    }
  })

  observeEvent(input$recode_same_down, {
    updated <- move_order_item(selected_variables(), input$recode_same_selected, "down")
    if (isTRUE(updated$changed)) {
      selected_variables(updated$order)
      mark_settings_dirty()
    }
  })

  output$recode_same_reset_control <- renderUI({
    analysis_reset_button("reset_recode_same", enabled = length(selected_variables()) > 0 || length(applied_entries()) > 0)
  })

  observeEvent(input$reset_recode_same, {
    if (length(selected_variables()) == 0 && length(applied_entries()) == 0) return()
    selected_variables(character(0))
    last_message(NULL)
    applied_entries(list())
    selected_queue_index(NULL)
    single_rule_count(2L)
    single_rule_max(NULL)
    category_middle_count(1L)
    session$sendCustomMessage(
      "easyflow-clear-transfer-selection",
      list(inputIds = c("recode_same_available", "recode_same_selected"))
    )
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  recoded_preview <- reactive({
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    variable <- as.character(input$recode_same_old_variable %||% "")
    variable <- variable[nzchar(variable)]
    variable <- if (length(variable) > 0) variable[[1]] else ""
    if (is.null(data) || !nzchar(variable) || !variable %in% names(data)) {
      return(data.frame())
    }
    rows <- seq_len(min(20L, nrow(data)))
    values <- data[[variable]][rows]
    if (identical(as.character(input$recode_same_rule_type %||% "single"), "category")) {
      recoded <- recode_apply_category_rules(values, input, middle_count = category_middle_count(), keep_unmatched = isTRUE(input$recode_same_keep_unmatched))
    } else {
      recoded <- recode_apply_single_rules(
        values,
        recode_single_rules_from_input(input, single_rule_count()),
        keep_unmatched = isTRUE(input$recode_same_keep_unmatched)
      )
    }
    preview <- data.frame(
      Row = rows,
      `Old value` = values,
      `New value` = recoded,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    if (identical(as.character(input$recode_same_target %||% "same"), "new")) {
      preview_name <- trimws(as.character(input$recode_same_new_name %||% ""))
      names(preview)[[3]] <- if (nzchar(preview_name)) preview_name else "New value"
    }
    preview
  })

  output$recode_same_preview <- DT::renderDT({
    preview <- recoded_preview()
    if (is.null(preview) || ncol(preview) == 0) {
      return(NULL)
    }
    DT::datatable(preview, rownames = FALSE, options = list(pageLength = 20, lengthChange = FALSE, scrollX = TRUE))
  })

  output$recode_same_applied_list <- renderUI({
    recode_applied_table_ui(applied_entries(), selected_queue_index())
  })

  load_recode_entry <- function(entry, index = NULL) {
    selected_queue_index(index)
    updateTextInput(session, "recode_same_old_variable", value = entry$old %||% "")
    updateTextInput(session, "recode_same_new_name", value = if (identical(entry$target, "new")) entry$new %||% "" else "")
    updateRadioButtons(session, "recode_same_target", selected = entry$target %||% "same")
    updateRadioButtons(session, "recode_same_rule_type", selected = entry$rule_type %||% "single")
    updateCheckboxInput(session, "recode_same_keep_unmatched", value = isTRUE(entry$keep_unmatched))
    updateSelectInput(session, "recode_same_measurement", selected = entry$measurement %||% "")

    rules <- normalize_recode_same_rules(entry$single_rules %||% empty_recode_same_rules())
    single_rule_count(max(2L, nrow(rules) + 1L))
    single_rule_max(NULL)
    middle_count <- max(1L, as.integer(entry$middle_count %||% 1L))
    category_middle_count(middle_count)
    single_count_snapshot <- max(2L, nrow(rules) + 1L)
    middle_count_snapshot <- middle_count
    session$onFlushed(function() {
      for (row_index in seq_len(single_count_snapshot)) {
        updateTextInput(session, paste0("recode_same_single_old_", row_index), value = if (row_index <= nrow(rules)) rules$old[[row_index]] else "")
        updateTextInput(session, paste0("recode_same_single_new_", row_index), value = if (row_index <= nrow(rules)) rules$new[[row_index]] else "")
      }
      category_input <- entry$category_input %||% list()
      updateTextInput(session, "recode_same_cat_lower_to", value = category_input$recode_same_cat_lower_to %||% "")
      updateSelectInput(session, "recode_same_cat_lower_upper_op", selected = category_input$recode_same_cat_lower_upper_op %||% "lt")
      updateTextInput(session, "recode_same_cat_lower_new", value = category_input$recode_same_cat_lower_new %||% "")
      for (row_index in seq_len(middle_count_snapshot)) {
        updateTextInput(session, paste0("recode_same_cat_middle_from_", row_index), value = category_input[[paste0("recode_same_cat_middle_from_", row_index)]] %||% "")
        updateSelectInput(session, paste0("recode_same_cat_middle_lower_op_", row_index), selected = category_input[[paste0("recode_same_cat_middle_lower_op_", row_index)]] %||% "ge")
        updateTextInput(session, paste0("recode_same_cat_middle_to_", row_index), value = category_input[[paste0("recode_same_cat_middle_to_", row_index)]] %||% "")
        updateSelectInput(session, paste0("recode_same_cat_middle_upper_op_", row_index), selected = category_input[[paste0("recode_same_cat_middle_upper_op_", row_index)]] %||% "lt")
        updateTextInput(session, paste0("recode_same_cat_middle_new_", row_index), value = category_input[[paste0("recode_same_cat_middle_new_", row_index)]] %||% "")
      }
      updateTextInput(session, "recode_same_cat_upper_from", value = category_input$recode_same_cat_upper_from %||% "")
      updateSelectInput(session, "recode_same_cat_upper_lower_op", selected = category_input$recode_same_cat_upper_lower_op %||% "ge")
      updateTextInput(session, "recode_same_cat_upper_new", value = category_input$recode_same_cat_upper_new %||% "")
    }, once = TRUE)
  }

  observeEvent(input$recode_same_queue_select, {
    index <- suppressWarnings(as.integer(input$recode_same_queue_select$index %||% NA_integer_))
    entries <- applied_entries()
    if (!is.finite(index) || index < 1L || index > length(entries)) {
      return()
    }
    load_recode_entry(entries[[index]], index)
  }, ignoreInit = TRUE)

  observeEvent(input$recode_same_queue_delete, {
    index <- suppressWarnings(as.integer(input$recode_same_queue_delete$index %||% NA_integer_))
    entries <- applied_entries()
    if (!is.finite(index) || index < 1L || index > length(entries)) {
      return()
    }
    entries <- entries[-index]
    applied_entries(entries)
    selected_queue_index(NULL)
    last_message("Removed queued recode rule.")
  }, ignoreInit = TRUE)

  output$recode_same_reverse_summary <- DT::renderDT({
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    summary <- recode_same_reverse_summary(data, selected_variables())
    if (is.null(summary) || nrow(summary) == 0) {
      return(DT::datatable(data.frame(Message = "Select scale items to inspect min/max."), rownames = FALSE, options = list(dom = "t")))
    }
    DT::datatable(summary, rownames = FALSE, options = list(dom = "t", ordering = FALSE, pageLength = 8))
  })

  output$recode_same_message <- renderUI({
    message <- last_message()
    if (is.null(message)) {
      return(NULL)
    }
    div(class = "recode-same-status", message)
  })

  observeEvent(input$add_recode_same, {
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    if (is.null(data)) {
      showNotification("Load a data file before adding a recode rule.", type = "warning", duration = 5)
      return()
    }
    variable_info <- tryCatch(variable_info_fn(), error = function(e) NULL)
    entry <- recode_current_entry_from_input(input, single_rule_count(), category_middle_count())
    if (!nzchar(entry$old) || !entry$old %in% names(data)) {
      showNotification("Select an old variable to add.", type = "warning", duration = 5)
      return()
    }
    if (identical(entry$target, "new") && !nzchar(entry$new)) {
      showNotification("Enter a new variable name.", type = "warning", duration = 5)
      return()
    }
    if (identical(entry$rule_type, "single") && nrow(entry$single_rules) == 0) {
        showNotification("Enter at least one existing value to recode.", type = "warning", duration = 5)
        return()
    }
    inferred_measurement <- recode_infer_output_measurement(entry, recode_source_measurement(variable_info, entry$old, data))
    if (!is.null(inferred_measurement)) {
      entry$measurement <- inferred_measurement
      updateSelectInput(session, "recode_same_measurement", selected = inferred_measurement)
    }
    unmatched_message <- recode_unmatched_message(data[[entry$old]], entry)

    entries <- applied_entries()
    index <- suppressWarnings(as.integer(selected_queue_index() %||% NA_integer_))
    if (is.finite(index) && index >= 1L && index <= length(entries)) {
      entries[[index]] <- entry
      status <- sprintf("Updated queued recode rule: %s -> %s", entry$old, entry$new)
    } else {
      entries <- c(entries, list(entry))
      status <- sprintf("Added queued recode rule: %s -> %s", entry$old, entry$new)
    }
    if (!is.null(unmatched_message)) {
      status <- paste(status, unmatched_message)
      showNotification(unmatched_message, type = "message", duration = 8)
    }
    last_message(status)
    applied_entries(entries)
    selected_queue_index(NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$apply_recode_same, {
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    if (is.null(data)) {
      showNotification("Load a data file before recoding.", type = "warning", duration = 5)
      return()
    }
    entries <- applied_entries()
    if (length(entries) == 0) {
      showNotification("Add at least one recode rule before applying.", type = "warning", duration = 5)
      return()
    }
    if (any(vapply(entries, function(entry) identical(entry$target, "new"), logical(1))) && !is.function(add_calculated_variable_fn)) {
      showNotification("New-variable recoding is not available in this session.", type = "warning", duration = 5)
      return()
    }

    working_data <- data
    changed <- character(0)
    for (entry in entries) {
      if (!nzchar(entry$old) || !entry$old %in% names(working_data)) {
        showNotification(sprintf("Queued variable is not available: %s", entry$old), type = "warning", duration = 5)
        return()
      }
      if (identical(entry$target, "new") && !nzchar(entry$new)) {
        showNotification(sprintf("Queued rule for %s has no new variable name.", entry$old), type = "warning", duration = 5)
        return()
      }
      values <- recode_entry_values(working_data[[entry$old]], entry)
      ok <- if (identical(entry$target, "new")) {
        add_calculated_variable_fn(entry$new, values, var_label = sprintf("%s recoded", entry$old), measurement = entry$measurement)
      } else {
        update_existing_variable_fn(entry$old, values, measurement = entry$measurement)
      }
      if (isTRUE(ok)) {
        output_name <- if (identical(entry$target, "new")) entry$new else entry$old
        working_data[[output_name]] <- values
        changed <- c(changed, output_name)
      }
    }
    if (length(changed) == 0) {
      last_message("No queued recode rules were applied.")
      return()
    }
    applied_entries(list())
    selected_queue_index(NULL)
    last_message(sprintf("Applied %s queued recode rule(s): %s", length(changed), paste(changed, collapse = ", ")))
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$apply_recode_same_reverse, {
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    if (is.null(data)) {
      showNotification("Load a data file before reverse scoring.", type = "warning", duration = 5)
      return()
    }
    variables <- intersect(selected_variables(), names(data))
    if (length(variables) == 0) {
      showNotification("Select at least one variable to reverse score.", type = "warning", duration = 5)
      return()
    }

    use_observed <- isTRUE(input$recode_same_reverse_use_observed)
    minimum <- input$recode_same_reverse_min
    maximum <- input$recode_same_reverse_max
    for (name in variables) {
      values <- if (isTRUE(use_observed)) {
        reverse_score_values(data[[name]])
      } else {
        reverse_score_values(data[[name]], minimum = minimum, maximum = maximum)
      }
      update_existing_variable_fn(name, values, measurement = "ordered")
    }
    last_message(sprintf("Reverse scored %s variable(s): %s", length(variables), paste(variables, collapse = ", ")))
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  invisible(TRUE)
}

register_coding_error_check_handlers <- function(
  input,
  output,
  session,
  dataset_fn,
  current_data_file_fn,
  selected_names_fn,
  variable_info_fn,
  labels_fn,
  category_table_fn,
  update_existing_variable_fn,
  mark_settings_dirty
) {
  selected_variables <- reactiveVal(character(0))
  active_list <- reactiveVal(NULL)
  last_message <- reactiveVal(NULL)
  last_issues <- reactiveVal(data.frame())
  correction_values <- reactiveVal(character(0))

  output$coding_error_setup <- renderUI({
    coding_error_check_setup_panel(
      file = current_data_file_fn(),
      data = tryCatch(dataset_fn(), error = function(e) NULL),
      variable_info = tryCatch(variable_info_fn(), error = function(e) NULL),
      labels = labels_fn(),
      selected_variables = selected_variables(),
      input = input
    )
  })

  register_analysis_data_viewer_handlers(
    input = input,
    output = output,
    prefix = "coding_error",
    title = "Auto Coding Error Check Data Viewer",
    dataset_fn = dataset_fn,
    selected_names_fn = selected_names_fn,
    variables_fn = selected_variables,
    variable_table_fn = variable_info_fn,
    labels_fn = labels_fn,
    category_table_fn = category_table_fn
  )

  observe({
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    if (is.null(data)) {
      selected_variables(character(0))
      last_issues(data.frame())
      correction_values(character(0))
      return()
    }
    current <- selected_variables()
    updated <- intersect(current, names(data))
    if (!identical(updated, current)) {
      selected_variables(updated)
      last_issues(data.frame())
      correction_values(character(0))
    }
  })

  observeEvent(input$coding_error_available_active, active_list("coding_error_available"), ignoreInit = TRUE)
  observeEvent(input$coding_error_selected_active, active_list("coding_error_selected"), ignoreInit = TRUE)

  observe({
    if (identical(active_list(), "coding_error_selected") && length(input$coding_error_selected %||% character(0)) > 0) {
      updateActionButton(session, "coding_error_move", label = "<")
    } else {
      updateActionButton(session, "coding_error_move", label = ">")
    }
  })

  observeEvent(input$coding_error_move, {
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    if (is.null(data)) return()
    variables <- names(data)
    current <- intersect(selected_variables(), variables)
    if (identical(active_list(), "coding_error_selected")) {
      chosen <- intersect(as.character(input$coding_error_selected %||% character(0)), current)
      if (length(chosen) == 0) return()
      selected_variables(setdiff(current, chosen))
      active_list("coding_error_available")
      last_issues(data.frame())
      correction_values(character(0))
      mark_settings_dirty()
      return()
    }

    chosen <- intersect(as.character(input$coding_error_available %||% character(0)), setdiff(variables, current))
    if (length(chosen) == 0) return()
    selected_variables(c(current, chosen))
    active_list("coding_error_selected")
    last_issues(data.frame())
    correction_values(character(0))
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$coding_error_move_direct, {
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    if (is.null(data)) return()
    variables <- names(data)
    current <- intersect(selected_variables(), variables)
    payload <- input$coding_error_move_direct %||% list()
    source <- as.character(payload$source %||% "")
    chosen <- intersect(as.character(payload$values %||% character(0)), variables)
    if (identical(source, "selected")) {
      chosen <- intersect(chosen, current)
      if (length(chosen) == 0) return()
      selected_variables(setdiff(current, chosen))
      active_list("coding_error_available")
    } else {
      chosen <- intersect(chosen, setdiff(variables, current))
      if (length(chosen) == 0) return()
      selected_variables(c(current, chosen))
      active_list("coding_error_selected")
    }
    last_issues(data.frame())
    correction_values(character(0))
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$coding_error_selected_doubleclick, {
    current <- selected_variables()
    chosen <- intersect(as.character(input$coding_error_selected_doubleclick$value %||% ""), current)
    if (length(chosen) == 0) return()
    selected_variables(setdiff(current, chosen))
    active_list("coding_error_available")
    last_issues(data.frame())
    correction_values(character(0))
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  register_dual_transfer_drop_observer(
    input = input,
    session = session,
    available_id = "coding_error_available",
    selected_id = "coding_error_selected",
    selected_values = selected_variables,
    all_values_fn = function() names(tryCatch(dataset_fn(), error = function(e) data.frame()) %||% data.frame()),
    active_list = active_list,
    mark_settings_dirty = mark_settings_dirty,
    after_change = function(target, chosen, next_values) {
      last_issues(data.frame())
      correction_values(character(0))
    }
  )

  observeEvent(input$coding_error_up, {
    updated <- move_order_item(selected_variables(), input$coding_error_selected, "up")
    if (isTRUE(updated$changed)) {
      selected_variables(updated$order)
      mark_settings_dirty()
    }
  })

  observeEvent(input$coding_error_down, {
    updated <- move_order_item(selected_variables(), input$coding_error_selected, "down")
    if (isTRUE(updated$changed)) {
      selected_variables(updated$order)
      mark_settings_dirty()
    }
  })

  output$coding_error_reset_control <- renderUI({
    analysis_reset_button("reset_coding_error", enabled = length(selected_variables()) > 0)
  })

  observeEvent(input$reset_coding_error, {
    if (length(selected_variables()) == 0) return()
    selected_variables(character(0))
    last_message(NULL)
    last_issues(data.frame())
    correction_values(character(0))
    session$sendCustomMessage(
      "easyflow-clear-transfer-selection",
      list(inputIds = c("coding_error_available", "coding_error_selected"))
    )
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  output$coding_error_message <- renderUI({
    message <- last_message()
    if (is.null(message)) {
      return(NULL)
    }
    div(class = "recode-same-status", message)
  })

  output$coding_error_issues <- DT::renderDT({
    issues <- last_issues()
    if (is.null(issues) || nrow(issues) == 0) {
      return(NULL)
    }
    display <- coding_error_issues_display(issues, corrected_values = correction_values())
    DT::datatable(
      display,
      rownames = FALSE,
      escape = FALSE,
      options = list(
        pageLength = 20,
        lengthChange = FALSE,
        scrollX = FALSE,
        autoWidth = FALSE,
        columnDefs = list(
          list(width = "70px", targets = 0),
          list(width = "180px", targets = 1),
          list(width = "100px", targets = 2),
          list(width = "160px", targets = 3),
          list(width = "220px", targets = 4),
          list(className = "dt-center", targets = c(0, 2))
        )
      ),
      callback = DT::JS(
        "table.on('draw.dt', function(){",
        "  if (window.Shiny) { try { Shiny.bindAll(table.table().container()); } catch(e) {} }",
        "  if (window.easyflowRestoreCodingErrorFixInputs) { window.easyflowRestoreCodingErrorFixInputs(table.table().container()); }",
        "});",
        "if (window.Shiny) { try { Shiny.bindAll(table.table().container()); } catch(e) {} }",
        "if (window.easyflowRestoreCodingErrorFixInputs) { window.easyflowRestoreCodingErrorFixInputs(table.table().container()); }"
      )
    )
  })

  merge_coding_error_corrections <- function(payload) {
    issues <- last_issues()
    if (is.null(issues) || nrow(issues) == 0 || is.null(payload)) {
      return(invisible(FALSE))
    }
    incoming <- payload$values %||% payload
    values <- as.character(correction_values() %||% character(0))
    if (length(values) < nrow(issues)) {
      values <- c(values, as.character(issues$Value)[seq.int(length(values) + 1L, nrow(issues))])
    }
    if (is.list(incoming)) {
      incoming_names <- names(incoming)
      if (is.null(incoming_names)) {
        incoming_names <- as.character(seq_along(incoming))
      }
      for (name in incoming_names) {
        index <- suppressWarnings(as.integer(name))
        if (is.finite(index) && index >= 1 && index <= nrow(issues)) {
          values[[index]] <- as.character(incoming[[name]] %||% "")
        }
      }
    }
    correction_values(values)
    invisible(TRUE)
  }

  apply_coding_error_corrections <- function(indices = NULL) {
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    if (is.null(data)) {
      showNotification("Load a data file before applying corrections.", type = "warning", duration = 5)
      return(invisible(FALSE))
    }
    issues <- last_issues()
    if (is.null(issues) || nrow(issues) == 0) {
      showNotification("Run coding error check before applying corrections.", type = "warning", duration = 5)
      return(invisible(FALSE))
    }

    minimum <- suppressWarnings(as.numeric(input$coding_error_min))
    maximum <- suppressWarnings(as.numeric(input$coding_error_max))
    if (!is.finite(minimum) || !is.finite(maximum) || minimum >= maximum) {
      showNotification("Minimum and maximum must be valid numeric values, and minimum must be smaller than maximum.", type = "warning", duration = 6)
      return(invisible(FALSE))
    }

    updates_by_variable <- list()
    changed <- 0L
    current_corrections <- as.character(correction_values() %||% character(0))
    if (length(current_corrections) < nrow(issues)) {
      current_corrections <- c(current_corrections, as.character(issues$Value)[seq.int(length(current_corrections) + 1L, nrow(issues))])
    }
    apply_all <- is.null(indices)
    if (isTRUE(apply_all)) {
      original_values <- trimws(as.character(issues$Value))
      correction_text <- trimws(as.character(current_corrections[seq_len(nrow(issues))]))
      indices <- which(nzchar(correction_text) & correction_text != original_values)
    } else {
      indices <- suppressWarnings(as.integer(indices))
      indices <- indices[is.finite(indices) & indices >= 1 & indices <= nrow(issues)]
    }
    if (length(indices) == 0) {
      showNotification("Enter at least one corrected value that differs from the original value.", type = "warning", duration = 5)
      return(invisible(FALSE))
    }

    for (index in indices) {
      corrected <- trimws(as.character(current_corrections[[index]] %||% ""))
      if (!nzchar(corrected)) {
        next()
      }
      numeric_value <- suppressWarnings(as.numeric(corrected))
      if (!is.finite(numeric_value) || abs(numeric_value - round(numeric_value)) > sqrt(.Machine$double.eps) || numeric_value < minimum || numeric_value > maximum) {
        showNotification(sprintf("Correction for row %s / %s must be an integer within the selected range.", issues$Id[[index]], issues$Variable[[index]]), type = "warning", duration = 6)
        return(invisible(FALSE))
      }
      variable <- as.character(issues$Variable[[index]])
      row_id <- suppressWarnings(as.integer(issues$Id[[index]]))
      if (!nzchar(variable) || !variable %in% names(data) || !is.finite(row_id) || row_id < 1 || row_id > nrow(data)) {
        next()
      }
      if (is.null(updates_by_variable[[variable]])) {
        updates_by_variable[[variable]] <- as.vector(data[[variable]])
      }
      updates_by_variable[[variable]][[row_id]] <- numeric_value
      changed <- changed + 1L
    }

    if (changed == 0L) {
      showNotification("Enter at least one corrected value.", type = "warning", duration = 5)
      return(invisible(FALSE))
    }

    for (variable in names(updates_by_variable)) {
      update_existing_variable_fn(variable, updates_by_variable[[variable]])
    }

    updated_data <- data
    for (variable in names(updates_by_variable)) {
      updated_data[[variable]] <- updates_by_variable[[variable]]
    }
    variables <- intersect(selected_variables(), names(updated_data))
    updated_issues <- recode_range_issues(updated_data, variables, minimum, maximum)
    last_issues(updated_issues)
    correction_values(as.character(updated_issues$Value %||% character(0)))
    session$sendCustomMessage("easyflow-clear-coding-error-fixes", list())
    if (nrow(updated_issues) > 0) {
      last_message(sprintf("Applied %s correction(s). Found %s coding issue(s) remaining.", changed, nrow(updated_issues)))
    } else {
      last_message(sprintf("Applied %s correction(s). No coding errors found.", changed))
    }
    mark_settings_dirty()
    invisible(TRUE)
  }

  observeEvent(input$apply_coding_error, {
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    if (is.null(data)) {
      showNotification("Load a data file before checking coding errors.", type = "warning", duration = 5)
      return()
    }
    variables <- intersect(selected_variables(), names(data))
    if (length(variables) == 0) {
      showNotification("Select at least one variable to check.", type = "warning", duration = 5)
      return()
    }

    issues <- tryCatch(
      recode_range_issues(data, variables, input$coding_error_min, input$coding_error_max),
      error = function(e) {
        showNotification(conditionMessage(e), type = "warning", duration = 6)
        NULL
      }
    )
    if (is.null(issues)) {
      return()
    }
    last_issues(issues)
    correction_values(as.character(issues$Value %||% character(0)))
    session$sendCustomMessage("easyflow-clear-coding-error-fixes", list())
    if (nrow(issues) > 0) {
      last_message(sprintf("Found %s coding issue(s).", nrow(issues)))
    } else {
      last_message("No coding errors found.")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$coding_error_apply_all_values, {
    merge_coding_error_corrections(input$coding_error_apply_all_values)
    apply_coding_error_corrections()
  }, ignoreInit = TRUE)

  observeEvent(input$coding_error_apply_one, {
    index <- input$coding_error_apply_one$index %||% NA_integer_
    issues <- last_issues()
    row_index <- suppressWarnings(as.integer(index))
    if (is.finite(row_index) && !is.null(issues) && row_index >= 1 && row_index <= nrow(issues)) {
      values <- as.character(correction_values() %||% character(0))
      if (length(values) < nrow(issues)) {
        values <- c(values, as.character(issues$Value)[seq.int(length(values) + 1L, nrow(issues))])
      }
      values[[row_index]] <- as.character(input$coding_error_apply_one$value %||% values[[row_index]] %||% "")
      correction_values(values)
    }
    apply_coding_error_corrections(indices = index)
  }, ignoreInit = TRUE)

  invisible(TRUE)
}

register_recode_different_handlers <- function(
  input,
  output,
  session,
  dataset_fn,
  current_data_file_fn,
  selected_names_fn,
  variable_info_fn,
  labels_fn,
  category_table_fn,
  add_calculated_variable_fn,
  update_existing_variable_fn,
  mark_settings_dirty
) {
  selected_variables <- reactiveVal(character(0))
  active_list <- reactiveVal(NULL)
  last_message <- reactiveVal(NULL)
  last_issues <- reactiveVal(data.frame())
  output_variables <- reactiveVal(character(0))

  output$recode_different_setup <- renderUI({
    recode_different_setup_panel(
      file = current_data_file_fn(),
      data = tryCatch(dataset_fn(), error = function(e) NULL),
      variable_info = tryCatch(variable_info_fn(), error = function(e) NULL),
      labels = labels_fn(),
      selected_variables = selected_variables(),
      input = input
    )
  })

  register_analysis_data_viewer_handlers(
    input = input,
    output = output,
    prefix = "recode_different",
    title = "Different-variable Recoding Data Viewer",
    dataset_fn = dataset_fn,
    selected_names_fn = selected_names_fn,
    variables_fn = selected_variables,
    extra_variables_fn = output_variables,
    variable_table_fn = variable_info_fn,
    labels_fn = labels_fn,
    category_table_fn = category_table_fn
  )

  observe({
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    if (is.null(data)) {
      selected_variables(character(0))
      last_issues(data.frame())
      return()
    }
    current <- selected_variables()
    updated <- intersect(current, names(data))
    if (!identical(updated, current)) {
      selected_variables(updated)
    }
  })

  observeEvent(input$recode_different_available_active, active_list("recode_different_available"), ignoreInit = TRUE)
  observeEvent(input$recode_different_selected_active, active_list("recode_different_selected"), ignoreInit = TRUE)

  observe({
    if (identical(active_list(), "recode_different_selected") && length(input$recode_different_selected %||% character(0)) > 0) {
      updateActionButton(session, "recode_different_move", label = "<")
    } else {
      updateActionButton(session, "recode_different_move", label = ">")
    }
  })

  observeEvent(input$recode_different_move, {
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    if (is.null(data)) return()
    variables <- names(data)
    current <- intersect(selected_variables(), variables)
    if (identical(active_list(), "recode_different_selected")) {
      chosen <- intersect(as.character(input$recode_different_selected %||% character(0)), current)
      if (length(chosen) == 0) return()
      selected_variables(setdiff(current, chosen))
      active_list("recode_different_available")
      last_issues(data.frame())
      output_variables(character(0))
      mark_settings_dirty()
      return()
    }

    chosen <- intersect(as.character(input$recode_different_available %||% character(0)), setdiff(variables, current))
    if (length(chosen) == 0) return()
    selected_variables(c(current, chosen))
    active_list("recode_different_selected")
    last_issues(data.frame())
    output_variables(character(0))
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$recode_different_move_direct, {
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    if (is.null(data)) return()
    variables <- names(data)
    current <- intersect(selected_variables(), variables)
    payload <- input$recode_different_move_direct %||% list()
    source <- as.character(payload$source %||% "")
    chosen <- intersect(as.character(payload$values %||% character(0)), variables)
    if (identical(source, "selected")) {
      chosen <- intersect(chosen, current)
      if (length(chosen) == 0) return()
      selected_variables(setdiff(current, chosen))
      active_list("recode_different_available")
    } else {
      chosen <- intersect(chosen, setdiff(variables, current))
      if (length(chosen) == 0) return()
      selected_variables(c(current, chosen))
      active_list("recode_different_selected")
    }
    last_issues(data.frame())
    output_variables(character(0))
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$recode_different_selected_doubleclick, {
    current <- selected_variables()
    chosen <- intersect(as.character(input$recode_different_selected_doubleclick$value %||% ""), current)
    if (length(chosen) == 0) return()
    selected_variables(setdiff(current, chosen))
    active_list("recode_different_available")
    last_issues(data.frame())
    output_variables(character(0))
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  register_dual_transfer_drop_observer(
    input = input,
    session = session,
    available_id = "recode_different_available",
    selected_id = "recode_different_selected",
    selected_values = selected_variables,
    all_values_fn = function() names(tryCatch(dataset_fn(), error = function(e) data.frame()) %||% data.frame()),
    active_list = active_list,
    mark_settings_dirty = mark_settings_dirty,
    after_change = function(target, chosen, next_values) {
      last_issues(data.frame())
      output_variables(character(0))
    }
  )

  observeEvent(input$recode_different_up, {
    updated <- move_order_item(selected_variables(), input$recode_different_selected, "up")
    if (isTRUE(updated$changed)) {
      selected_variables(updated$order)
      mark_settings_dirty()
    }
  })

  observeEvent(input$recode_different_down, {
    updated <- move_order_item(selected_variables(), input$recode_different_selected, "down")
    if (isTRUE(updated$changed)) {
      selected_variables(updated$order)
      mark_settings_dirty()
    }
  })

  output$recode_different_reset_control <- renderUI({
    analysis_reset_button("reset_recode_different", enabled = length(selected_variables()) > 0)
  })

  observeEvent(input$reset_recode_different, {
    if (length(selected_variables()) == 0) return()
    selected_variables(character(0))
    last_message(NULL)
    last_issues(data.frame())
    output_variables(character(0))
    session$sendCustomMessage(
      "easyflow-clear-transfer-selection",
      list(inputIds = c("recode_different_available", "recode_different_selected"))
    )
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  output$recode_different_message <- renderUI({
    message <- last_message()
    if (is.null(message)) {
      return(NULL)
    }
    div(class = "recode-same-status", message)
  })

  output$recode_different_issues <- DT::renderDT({
    issues <- last_issues()
    if (is.null(issues) || nrow(issues) == 0) {
      return(NULL)
    }
    DT::datatable(issues, rownames = FALSE, options = list(pageLength = 20, lengthChange = FALSE, scrollX = TRUE))
  })

  observeEvent(input$apply_recode_different, {
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    if (is.null(data)) {
      showNotification("Load a data file before recoding.", type = "warning", duration = 5)
      return()
    }
    variables <- intersect(selected_variables(), names(data))
    if (length(variables) == 0) {
      showNotification("Select at least one variable to inspect or recode.", type = "warning", duration = 5)
      return()
    }

    minimum <- suppressWarnings(as.numeric(input$recode_different_min))
    maximum <- suppressWarnings(as.numeric(input$recode_different_max))
    if (!is.finite(minimum) || !is.finite(maximum) || minimum >= maximum) {
      showNotification("Minimum and maximum must be valid numeric values, and minimum must be smaller than maximum.", type = "warning", duration = 6)
      return()
    }
    last_issues(data.frame())
    output_variables(character(0))

    target <- as.character(input$recode_different_target %||% "new")
    if (!target %in% c("new", "same")) {
      target <- "new"
    }
    created <- character(0)
    variable_info <- tryCatch(variable_info_fn(), error = function(e) NULL)
    for (name in variables) {
      values <- reverse_score_values(data[[name]], minimum = minimum, maximum = maximum)
      source_measurement <- recode_source_measurement(variable_info, name, data)
      if (identical(target, "same")) {
        ok <- update_existing_variable_fn(name, values, measurement = source_measurement)
        if (isTRUE(ok)) {
          created <- c(created, name)
        }
        next()
      }
      pattern <- input$recode_different_new_name %||% "{variable}_R"
      new_name <- recode_new_variable_name(pattern, name, literal = length(variables) == 1)
      ok <- add_calculated_variable_fn(new_name, values, var_label = sprintf("%s reverse-coded", name), measurement = source_measurement)
      if (isTRUE(ok)) {
        created <- c(created, new_name)
      }
    }
    if (length(created) == 0) {
      last_message("No coding errors found, but no variables were updated.")
    } else if (identical(target, "same")) {
      last_message(sprintf("No coding errors found. Updated %s existing variable(s): %s", length(created), paste(created, collapse = ", ")))
    } else {
      last_message(sprintf("No coding errors found. Created %s variable(s): %s", length(created), paste(created, collapse = ", ")))
    }
    output_variables(created)
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  invisible(TRUE)
}

register_variable_calculation_handlers <- function(
  input,
  output,
  session,
  dataset_fn,
  current_data_file_fn,
  selected_names_fn,
  variable_info_fn,
  labels_fn,
  category_table_fn,
  add_calculated_variable_fn,
  mark_settings_dirty
) {
  selected_variables <- reactiveVal(character(0))
  active_list <- reactiveVal(NULL)
  last_message <- reactiveVal(NULL)
  preview_data <- reactiveVal(data.frame(check.names = FALSE))
  output_variables <- reactiveVal(character(0))
  reliability_result <- reactiveVal(NULL)

  output$variable_calculation_setup <- renderUI({
    variable_calculation_setup_panel(
      file = current_data_file_fn(),
      data = tryCatch(dataset_fn(), error = function(e) NULL),
      variable_info = tryCatch(variable_info_fn(), error = function(e) NULL),
      labels = labels_fn(),
      selected_variables = selected_variables(),
      input = input
    )
  })

  register_analysis_data_viewer_handlers(
    input = input,
    output = output,
    prefix = "variable_calculation",
    title = "Auto Variable Calculation Data Viewer",
    dataset_fn = dataset_fn,
    selected_names_fn = selected_names_fn,
    variables_fn = selected_variables,
    extra_variables_fn = output_variables,
    variable_table_fn = variable_info_fn,
    labels_fn = labels_fn,
    category_table_fn = category_table_fn
  )

  observe({
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    if (is.null(data)) {
      selected_variables(character(0))
      preview_data(data.frame(check.names = FALSE))
      output_variables(character(0))
      reliability_result(NULL)
      return()
    }
    current <- selected_variables()
    updated <- intersect(current, names(data))
    if (!identical(updated, current)) {
      selected_variables(updated)
      preview_data(data.frame(check.names = FALSE))
      output_variables(character(0))
      reliability_result(NULL)
    }
  })

  observeEvent(input$variable_calculation_available_active, active_list("variable_calculation_available"), ignoreInit = TRUE)
  observeEvent(input$variable_calculation_selected_active, active_list("variable_calculation_selected"), ignoreInit = TRUE)

  observe({
    if (identical(active_list(), "variable_calculation_selected") && length(input$variable_calculation_selected %||% character(0)) > 0) {
      updateActionButton(session, "variable_calculation_move", label = "<")
    } else {
      updateActionButton(session, "variable_calculation_move", label = ">")
    }
  })

  observeEvent(input$variable_calculation_move, {
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    if (is.null(data)) return()
    variables <- names(data)
    current <- intersect(selected_variables(), variables)
    if (identical(active_list(), "variable_calculation_selected")) {
      chosen <- intersect(as.character(input$variable_calculation_selected %||% character(0)), current)
      if (length(chosen) == 0) return()
      selected_variables(setdiff(current, chosen))
      active_list("variable_calculation_available")
      preview_data(data.frame(check.names = FALSE))
      output_variables(character(0))
      reliability_result(NULL)
      mark_settings_dirty()
      return()
    }

    chosen <- intersect(as.character(input$variable_calculation_available %||% character(0)), setdiff(variables, current))
    if (length(chosen) == 0) return()
    selected_variables(c(current, chosen))
    active_list("variable_calculation_selected")
    preview_data(data.frame(check.names = FALSE))
    output_variables(character(0))
    reliability_result(NULL)
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$variable_calculation_move_direct, {
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    if (is.null(data)) return()
    variables <- names(data)
    current <- intersect(selected_variables(), variables)
    payload <- input$variable_calculation_move_direct %||% list()
    source <- as.character(payload$source %||% "")
    chosen <- intersect(as.character(payload$values %||% character(0)), variables)
    if (identical(source, "selected")) {
      chosen <- intersect(chosen, current)
      if (length(chosen) == 0) return()
      selected_variables(setdiff(current, chosen))
      active_list("variable_calculation_available")
    } else {
      chosen <- intersect(chosen, setdiff(variables, current))
      if (length(chosen) == 0) return()
      selected_variables(c(current, chosen))
      active_list("variable_calculation_selected")
    }
    preview_data(data.frame(check.names = FALSE))
    output_variables(character(0))
    reliability_result(NULL)
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$variable_calculation_selected_doubleclick, {
    current <- selected_variables()
    chosen <- intersect(as.character(input$variable_calculation_selected_doubleclick$value %||% ""), current)
    if (length(chosen) == 0) return()
    selected_variables(setdiff(current, chosen))
    active_list("variable_calculation_available")
    preview_data(data.frame(check.names = FALSE))
    output_variables(character(0))
    reliability_result(NULL)
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  register_dual_transfer_drop_observer(
    input = input,
    session = session,
    available_id = "variable_calculation_available",
    selected_id = "variable_calculation_selected",
    selected_values = selected_variables,
    all_values_fn = function() names(tryCatch(dataset_fn(), error = function(e) data.frame()) %||% data.frame()),
    active_list = active_list,
    mark_settings_dirty = mark_settings_dirty,
    after_change = function(target, chosen, next_values) {
      preview_data(data.frame(check.names = FALSE))
      output_variables(character(0))
      reliability_result(NULL)
    }
  )

  observeEvent(input$variable_calculation_up, {
    updated <- move_order_item(selected_variables(), input$variable_calculation_selected, "up")
    if (isTRUE(updated$changed)) {
      selected_variables(updated$order)
      preview_data(data.frame(check.names = FALSE))
      output_variables(character(0))
      reliability_result(NULL)
      mark_settings_dirty()
    }
  })

  observeEvent(input$variable_calculation_down, {
    updated <- move_order_item(selected_variables(), input$variable_calculation_selected, "down")
    if (isTRUE(updated$changed)) {
      selected_variables(updated$order)
      preview_data(data.frame(check.names = FALSE))
      output_variables(character(0))
      reliability_result(NULL)
      mark_settings_dirty()
    }
  })

  output$variable_calculation_reset_control <- renderUI({
    analysis_reset_button("reset_variable_calculation", enabled = length(selected_variables()) > 0)
  })

  observeEvent(input$reset_variable_calculation, {
    if (length(selected_variables()) == 0) return()
    selected_variables(character(0))
    last_message(NULL)
    preview_data(data.frame(check.names = FALSE))
    output_variables(character(0))
    reliability_result(NULL)
    session$sendCustomMessage(
      "easyflow-clear-transfer-selection",
      list(inputIds = c("variable_calculation_available", "variable_calculation_selected"))
    )
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  output$variable_calculation_message <- renderUI({
    message <- last_message()
    if (is.null(message)) {
      return(NULL)
    }
    div(class = "recode-same-status", message)
  })

  output$variable_calculation_preview <- DT::renderDT({
    preview <- preview_data()
    if (is.null(preview) || ncol(preview) == 0) {
      return(NULL)
    }
    DT::datatable(utils::head(preview, 50), rownames = FALSE, options = list(pageLength = 10, lengthChange = FALSE, scrollX = TRUE))
  })

  output$variable_calculation_reliability_results <- renderUI({
    reliability_results_ui(reliability_result())
  })

  observeEvent(input$apply_variable_calculation, {
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    if (is.null(data)) {
      showNotification("Load a data file before calculating variables.", type = "warning", duration = 5)
      return()
    }
    variables <- intersect(selected_variables(), names(data))
    if (length(variables) == 0) {
      showNotification("Select at least one variable to calculate.", type = "warning", duration = 5)
      return()
    }
    operations <- intersect(as.character(input$variable_calculation_operations %||% character(0)), unname(variable_calculation_choices()))
    run_reliability <- isTRUE(input$variable_calculation_reliability)
    if (length(operations) == 0 && !isTRUE(run_reliability)) {
      showNotification("Select at least one calculation command or Reliability.", type = "warning", duration = 5)
      return()
    }
    base_name <- trimws(as.character(input$variable_calculation_base_name %||% ""))
    if (length(operations) > 0 && !nzchar(base_name)) {
      showNotification("Enter a variable name.", type = "warning", duration = 5)
      return()
    }

    reliability_output <- NULL
    if (isTRUE(run_reliability)) {
      reliability_output <- tryCatch(
        prepare_reliability_results(
          data = data,
          variables = variables,
          variable_info = variable_info_fn(),
          labels = labels_fn(),
          category_table = category_table_fn(),
          options = list(
            normality = TRUE,
            ordinal = FALSE,
            reliability_if_deleted = TRUE,
            item_total_correlation = TRUE
          )
        ),
        error = function(error) {
          showNotification(conditionMessage(error), type = "warning", duration = 6)
          NULL
        }
      )
      reliability_result(reliability_output)
      if (is.null(reliability_output) && length(operations) == 0) {
        return()
      }
    } else {
      reliability_result(NULL)
    }

    created <- character(0)
    calculated <- data.frame(check.names = FALSE)
    if (length(operations) > 0) {
      calculated <- calculate_variable_outputs(data, variables, operations, base_name)
      if (ncol(calculated) == 0) {
        showNotification("No variables were calculated.", type = "warning", duration = 5)
        if (is.null(reliability_output)) {
          return()
        }
      }

      for (name in names(calculated)) {
        operation <- operations[[match(name, paste0(vapply(operations, variable_calculation_prefix, character(1)), base_name))]]
        label <- sprintf("%s of %s", variable_calculation_label(operation), paste(variables, collapse = ", "))
        ok <- add_calculated_variable_fn(name, calculated[[name]], var_label = label, measurement = "continuous")
        if (isTRUE(ok)) {
          created <- c(created, name)
        }
      }
    }

    if (length(operations) > 0 && length(created) == 0 && is.null(reliability_output)) {
      last_message("No variables were created.")
      preview_data(data.frame(check.names = FALSE))
      output_variables(character(0))
      return()
    }
    preview_data(if (length(created) > 0) calculated[, created, drop = FALSE] else data.frame(check.names = FALSE))
    output_variables(created)
    messages <- character(0)
    if (length(created) > 0) {
      messages <- c(messages, sprintf("Created %s variable(s): %s", length(created), paste(created, collapse = ", ")))
    }
    if (!is.null(reliability_output)) {
      messages <- c(messages, "Reliability analysis completed.")
    }
    last_message(paste(messages, collapse = " "))
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  invisible(TRUE)
}
