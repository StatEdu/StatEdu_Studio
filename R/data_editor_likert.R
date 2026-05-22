# Automatic Likert label detection and batch conversion.

likert_dictionary <- function() {
  list(
    agreement_ko_5 = c("전혀 아니다", "아니다", "보통이다", "그렇다", "매우 그렇다"),
    agreement_ko_5_alt = c("전혀 그렇지 않다", "그렇지 않다", "보통이다", "그렇다", "매우 그렇다"),
    agreement_ko_5_formal = c("매우 그렇지 않다", "그렇지 않다", "보통", "그렇다", "매우 그렇다"),
    satisfaction_ko_5 = c("매우 불만족", "불만족", "보통", "만족", "매우 만족"),
    importance_ko_5 = c("전혀 중요하지 않다", "중요하지 않다", "보통", "중요하다", "매우 중요하다"),
    frequency_ko_5 = c("전혀 없음", "드물게", "가끔", "자주", "항상"),
    agreement_en_5 = c("strongly disagree", "disagree", "neutral", "agree", "strongly agree"),
    satisfaction_en_5 = c("very dissatisfied", "dissatisfied", "neutral", "satisfied", "very satisfied"),
    frequency_en_5 = c("never", "rarely", "sometimes", "often", "always")
  )
}

likert_normalize_label <- function(x) {
  x <- tolower(trimws(as.character(x %||% "")))
  x <- gsub("[[:space:][:punct:]]+", "", x)
  x
}

likert_present_values <- function(values) {
  values <- trimws(as.character(as.vector(values)))
  values <- values[!is.na(values) & nzchar(values)]
  unique(values)
}

likert_numeric_text_mapping <- function(unique_values) {
  text <- trimws(as.character(unique_values))
  numeric <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", text)))
  valid <- !is.na(numeric) & grepl("[0-9]", text)
  if (length(text) < 3 || length(text) > 11 || !all(valid)) {
    return(NULL)
  }
  if (length(unique(numeric)) != length(numeric)) {
    return(NULL)
  }
  ordered_index <- order(numeric)
  data.frame(
    label = text[ordered_index],
    value = numeric[ordered_index],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

likert_mapping_signature <- function(mapping) {
  mapping <- as.data.frame(mapping, stringsAsFactors = FALSE, check.names = FALSE)
  paste(sprintf("%s=%s", mapping$label, mapping$value), collapse = "|")
}

likert_mapping_values <- function(mapping) {
  suppressWarnings(as.numeric(as.data.frame(mapping, stringsAsFactors = FALSE)$value))
}

likert_mapping_is_superset <- function(candidate, observed) {
  candidate <- as.data.frame(candidate, stringsAsFactors = FALSE, check.names = FALSE)
  observed <- as.data.frame(observed, stringsAsFactors = FALSE, check.names = FALSE)
  candidate_values <- likert_mapping_values(candidate)
  observed_values <- likert_mapping_values(observed)
  if (length(candidate_values) <= length(observed_values)) {
    return(FALSE)
  }
  if (!all(observed_values %in% candidate_values)) {
    return(FALSE)
  }
  for (value in observed_values) {
    candidate_label <- candidate$label[match(value, candidate_values)]
    observed_label <- observed$label[match(value, observed_values)]
    if (!identical(likert_normalize_label(candidate_label), likert_normalize_label(observed_label))) {
      return(FALSE)
    }
  }
  TRUE
}

likert_best_group_signature <- function(index, mappings, dictionaries, signatures) {
  if (!identical(as.character(dictionaries[[index]]), "numeric_text")) {
    return(signatures[[index]])
  }
  candidates <- which(as.character(dictionaries) == "numeric_text")
  candidates <- candidates[candidates != index]
  candidates <- candidates[vapply(candidates, function(candidate) {
    likert_mapping_is_superset(mappings[[candidate]], mappings[[index]])
  }, logical(1))]
  if (length(candidates) == 0) {
    return(signatures[[index]])
  }
  candidate_sizes <- vapply(candidates, function(candidate) nrow(as.data.frame(mappings[[candidate]])), integer(1))
  signatures[[candidates[[which.max(candidate_sizes)]]]]
}

likert_group_representative_mapping <- function(group) {
  if (is.null(group) || !is.data.frame(group) || nrow(group) == 0 || !"mapping" %in% names(group)) {
    return(NULL)
  }
  sizes <- vapply(group$mapping, function(mapping) nrow(as.data.frame(mapping)), integer(1))
  group$mapping[[which.max(sizes)]]
}

likert_variable_choice_labels <- function(group, representative_mapping = NULL) {
  if (is.null(group) || !is.data.frame(group) || nrow(group) == 0) {
    return(character(0))
  }
  variables <- as.character(group$variable)
  representative_levels <- if (!is.null(representative_mapping)) {
    nrow(as.data.frame(representative_mapping, stringsAsFactors = FALSE, check.names = FALSE))
  } else if ("n_levels" %in% names(group)) {
    max(group$n_levels, na.rm = TRUE)
  } else {
    NA_integer_
  }
  labels <- htmltools::htmlEscape(variables)
  if (is.finite(representative_levels) && "n_levels" %in% names(group)) {
    partial <- group$n_levels < representative_levels
    labels[partial] <- sprintf(
      "%s <span class=\"likert-partial-level-note\">[observed %s of %s levels]</span>",
      labels[partial],
      group$n_levels[partial],
      representative_levels
    )
  }
  stats::setNames(variables, labels)
}

likert_variables_checkbox_group <- function(input_id, choices, selected = unname(choices)) {
  choices <- choices %||% character(0)
  values <- unname(as.character(choices))
  labels <- names(choices)
  if (is.null(labels) || length(labels) != length(values)) {
    labels <- htmltools::htmlEscape(values)
  }
  selected <- as.character(selected %||% character(0))
  div(
    id = input_id,
    class = "form-group shiny-input-checkboxgroup shiny-input-container",
    div(
      class = "shiny-options-group",
      lapply(seq_along(values), function(index) {
        div(
          class = "checkbox",
          tags$label(
            tags$input(
              type = "checkbox",
              name = input_id,
              value = values[[index]],
              checked = if (values[[index]] %in% selected) "checked" else NULL
            ),
            tags$span(HTML(labels[[index]]))
          )
        )
      })
    )
  )
}

detect_likert_mapping <- function(values) {
  unique_values <- likert_present_values(values)
  if (length(unique_values) < 3 || length(unique_values) > 11) {
    return(NULL)
  }

  numeric_mapping <- likert_numeric_text_mapping(unique_values)
  if (!is.null(numeric_mapping)) {
    return(list(mapping = numeric_mapping, dictionary = "numeric_text", score = 0.82))
  }

  normalized_values <- likert_normalize_label(unique_values)
  for (dict_name in names(likert_dictionary())) {
    labels <- likert_dictionary()[[dict_name]]
    normalized_labels <- likert_normalize_label(labels)
    matched <- match(normalized_values, normalized_labels)
    if (all(!is.na(matched))) {
      ordered <- order(matched)
      mapping <- data.frame(
        label = unique_values[ordered],
        value = matched[ordered],
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      return(list(mapping = mapping, dictionary = dict_name, score = 0.95))
    }
  }

  NULL
}

detect_likert_variables <- function(data) {
  if (is.null(data) || !is.data.frame(data) || ncol(data) == 0) {
    return(data.frame(check.names = FALSE))
  }
  rows <- lapply(names(data), function(name) {
    values <- data[[name]]
    if (!(is.character(values) || is.factor(values) || is.ordered(values))) {
      return(NULL)
    }
    detected <- detect_likert_mapping(values)
    if (is.null(detected)) {
      return(NULL)
    }
    mapping <- detected$mapping
    signature <- likert_mapping_signature(mapping)
    data.frame(
      variable = name,
      group_id = paste0("likert_", sprintf("%03d", match(signature, signature))),
      source_index = match(name, names(data)),
      signature = signature,
      dictionary = detected$dictionary,
      n_levels = nrow(mapping),
      mapping = I(list(mapping)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(data.frame(check.names = FALSE))
  }
  detected <- do.call(rbind, rows)
  exact_signatures <- as.character(detected$signature)
  grouped_signatures <- vapply(seq_len(nrow(detected)), function(index) {
    likert_best_group_signature(index, detected$mapping, detected$dictionary, exact_signatures)
  }, character(1))
  signatures <- unique(grouped_signatures)
  detected$signature <- grouped_signatures
  detected$group_id <- paste0("likert_", match(grouped_signatures, signatures))
  detected
}

likert_group_summary <- function(detected) {
  if (is.null(detected) || !is.data.frame(detected) || nrow(detected) == 0) {
    return(data.frame(check.names = FALSE))
  }
  group_ids <- unique(as.character(detected$group_id))
  group_order <- order(suppressWarnings(as.integer(sub("^likert_", "", group_ids))))
  group_ids <- group_ids[group_order]
  rows <- lapply(group_ids, function(group_id) {
    group <- detected[detected$group_id == group_id, , drop = FALSE]
    if ("source_index" %in% names(group)) {
      group <- group[order(group$source_index), , drop = FALSE]
    }
    data.frame(
      group_id = group$group_id[[1]],
      variables = paste(group$variable, collapse = "\n"),
      variable_count = nrow(group),
      levels = max(group$n_levels, na.rm = TRUE),
      dictionary = group$dictionary[[1]],
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  do.call(rbind, rows)
}

likert_mapping_from_input <- function(input, mapping, prefix = "likert_map") {
  if (is.null(mapping) || !is.data.frame(mapping) || nrow(mapping) == 0) {
    return(mapping)
  }
  mapping$value <- vapply(seq_len(nrow(mapping)), function(index) {
    value <- suppressWarnings(as.numeric(input[[paste0(prefix, "_", index)]] %||% mapping$value[[index]]))
    if (is.na(value)) mapping$value[[index]] else value
  }, numeric(1))
  mapping
}

recode_likert_values <- function(values, mapping, reverse = FALSE) {
  mapping <- as.data.frame(mapping, stringsAsFactors = FALSE, check.names = FALSE)
  text <- trimws(as.character(as.vector(values)))
  output <- rep(NA_real_, length(text))
  for (index in seq_len(nrow(mapping))) {
    matched <- !is.na(text) & text == as.character(mapping$label[[index]])
    output[matched] <- as.numeric(mapping$value[[index]])
  }
  if (isTRUE(reverse)) {
    present <- sort(unique(stats::na.omit(as.numeric(mapping$value))))
    if (length(present) > 1) {
      output <- ifelse(is.na(output), NA_real_, min(present) + max(present) - output)
    }
  }
  output
}

likert_category_payload <- function(variables, mapping, reverse = FALSE) {
  mapping <- as.data.frame(mapping, stringsAsFactors = FALSE, check.names = FALSE)
  values <- as.numeric(mapping$value)
  labels <- as.character(mapping$label)
  if (isTRUE(reverse)) {
    values <- min(values, na.rm = TRUE) + max(values, na.rm = TRUE) - values
  }
  order_index <- order(values)
  values <- values[order_index]
  labels <- labels[order_index]
  rows <- list()
  for (variable in variables) {
    row <- list(reference = "", reference_label = "")
    for (index in seq_along(values)) {
      row[[paste0("value_", index)]] <- as.character(values[[index]])
      row[[paste0("label_", index)]] <- labels[[index]]
    }
    rows[[variable]] <- row
  }
  rows
}

likert_detection_table_display <- function(summary) {
  if (is.null(summary) || nrow(summary) == 0) {
    return(data.frame(Message = "No Likert-style text variables were detected.", check.names = FALSE))
  }
  variables <- vapply(strsplit(as.character(summary$variables), "\n", fixed = TRUE), function(items) {
    items <- items[nzchar(items)]
    display_items <- if (length(items) <= 2) {
      items
    } else {
      c(items[[1]], sprintf("... %s item(s) hidden ...", length(items) - 2L), items[[length(items)]])
    }
    classes <- if (length(items) <= 2) {
      rep("likert-variable-line", length(display_items))
    } else {
      c("likert-variable-line", "likert-variable-line likert-variable-hidden-count", "likert-variable-line")
    }
    paste(sprintf("<div class=\"%s\">%s</div>", classes, htmltools::htmlEscape(display_items)), collapse = "")
  }, character(1))
  output <- data.frame(
    Select = sprintf(
      '<input type="radio" name="likert_group_select" class="likert-group-select" value="%s">',
      htmltools::htmlEscape(summary$group_id)
    ),
    Group = summary$group_id,
    Items = variables,
    Count = summary$variable_count,
    Levels = summary$levels,
    Detection = summary$dictionary,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  output
}

likert_review_panel_ui <- function(choices, mapping) {
  choices <- choices %||% character(0)
  values <- unname(as.character(choices))
  if (length(values) == 0 || is.null(mapping) || !is.data.frame(mapping) || nrow(mapping) == 0) {
    return(div(class = "empty-message", div("Select a detected Likert group.")))
  }
  div(
    class = "likert-review-grid",
    div(
      class = "likert-review-block likert-items-block",
      div(class = "likert-review-title", "Item text"),
      likert_variables_checkbox_group(
        "likert_variables",
        choices = choices,
        selected = values
      )
    ),
    div(
      class = "likert-review-block likert-labels-block",
      div(class = "likert-review-title", "Original label"),
      div(
        class = "likert-label-list",
        lapply(seq_len(nrow(mapping)), function(index) {
          div(class = "likert-label-row", as.character(mapping$label[[index]] %||% ""))
        })
      )
    ),
    div(
      class = "likert-review-block likert-values-block",
      div(class = "likert-review-title", "Numeric value"),
      div(
        class = "likert-value-list",
        lapply(seq_len(nrow(mapping)), function(index) {
          numericInput(paste0("likert_map_", index), NULL, value = mapping$value[[index]], step = 1, width = "92px")
        })
      )
    )
  )
}

data_editor_likert_panel <- function() {
  div(
    class = "page-shell",
    div(
      class = "app-heading",
      h1("Auto Likert Conversion"),
      div("Detect text Likert items, map labels to numbers, and apply the same rule to grouped variables.", class = "app-subtitle")
    ),
    div(
      class = "workspace-panel frequencies-workspace-panel data-editor-workspace",
      analysis_workspace_heading("Likert label conversion", "likert"),
      analysis_workspace_body(
        "likert",
        uiOutput("likert_status"),
        div(class = "data-editor-result-output", DT::DTOutput("likert_groups")),
        uiOutput("likert_review_panel"),
        div(
          class = "likert-action-row",
          checkboxInput("likert_apply_same_pattern", "Apply this rule to every variable in the same detected group", value = TRUE),
          checkboxInput("likert_reverse", "Reverse items after conversion", value = FALSE),
          div(
            class = "likert-measurement-control",
            selectInput(
              "likert_measurement",
              "Variable type after conversion",
              choices = c("Continuous" = "continuous", "Ordinal" = "ordered", "Categorical" = "category", "Binary" = "binary"),
              selected = "continuous",
              width = "220px",
              selectize = FALSE
            )
          ),
          actionButton("apply_likert_conversion", "Convert selected group", class = "btn btn-primary")
        ),
        uiOutput("likert_message")
      )
    )
  )
}

register_likert_conversion_handlers <- function(
  input,
  output,
  session,
  dataset_fn,
  raw_dataset_fn,
  current_data_file_fn,
  selected_names_fn,
  update_existing_variable_fn,
  apply_category_label_snapshot_fn,
  mark_settings_dirty
) {
  dismissed_file <- reactiveVal("")
  last_message <- reactiveVal(NULL)

  detected <- reactive({
    file <- current_data_file_fn()
    if (is.null(file)) {
      return(data.frame(check.names = FALSE))
    }
    data <- tryCatch(raw_dataset_fn(), error = function(e) tryCatch(dataset_fn(), error = function(e) NULL))
    detect_likert_variables(data)
  })

  summary <- reactive({
    likert_group_summary(detected())
  })

  output$likert_status <- renderUI({
    groups <- summary()
    if (is.null(groups) || nrow(groups) == 0) {
      return(div(class = "empty-message", div("No Likert-style text variables were detected in the current data.")))
    }
    div(class = "recode-same-status", sprintf("%s Likert group(s), %s variable(s) detected.", nrow(groups), sum(groups$variable_count)))
  })

  output$likert_groups <- DT::renderDT({
    table <- likert_detection_table_display(summary())
    if ("Message" %in% names(table)) {
      return(DT::datatable(table, rownames = FALSE, options = list(dom = "t", ordering = FALSE)))
    }
    DT::datatable(
      table,
      rownames = FALSE,
      escape = FALSE,
      selection = list(mode = "single", selected = 1, target = "row"),
      class = "compact stripe hover likert-group-table",
      options = list(
        pageLength = 8,
        lengthChange = FALSE,
        scrollX = FALSE,
        autoWidth = FALSE,
        ordering = FALSE,
        columnDefs = list(
          list(width = "34px", targets = 0, orderable = FALSE, searchable = FALSE, className = "dt-center likert-select-col"),
          list(width = "76px", targets = 1, className = "likert-group-col"),
          list(width = "680px", targets = 2, className = "likert-variables-col"),
          list(width = "56px", targets = 3, className = "dt-center likert-count-col"),
          list(width = "56px", targets = 4, className = "dt-center likert-levels-col"),
          list(width = "112px", targets = 5, className = "likert-dictionary-col")
        ),
        drawCallback = DT::JS(
          "function(settings){",
          "  var api = this.api();",
          "  api.rows({selected:true}).every(function(){",
          "    $(this.node()).find('input.likert-group-select').prop('checked', true);",
          "  });",
          "}"
        )
      ),
      callback = DT::JS(
        "table.on('click', 'input.likert-group-select', function(e){",
        "  e.stopPropagation();",
        "  var row = table.row($(this).closest('tr'));",
        "  table.rows().deselect();",
        "  row.select();",
        "});",
        "table.on('select', function(e, dt, type, indexes){",
        "  if(type === 'row'){",
        "    $(table.rows().nodes()).find('input.likert-group-select').prop('checked', false);",
        "    $(table.rows(indexes).nodes()).find('input.likert-group-select').prop('checked', true);",
        "  }",
        "});"
      )
    )
  })

  selected_group_id <- reactive({
    groups <- summary()
    if (is.null(groups) || nrow(groups) == 0) {
      return("")
    }
    row <- input$likert_groups_rows_selected %||% 1L
    row <- row[[1]] %||% 1L
    if (!is.finite(row) || row < 1 || row > nrow(groups)) {
      row <- 1L
    }
    groups$group_id[[row]]
  })

  selected_group <- reactive({
    group_id <- selected_group_id()
    data <- detected()
    if (!nzchar(group_id) || is.null(data) || nrow(data) == 0) {
      return(NULL)
    }
    data[data$group_id == group_id, , drop = FALSE]
  })

  output$likert_review_panel <- renderUI({
    group <- selected_group()
    if (is.null(group) || nrow(group) == 0) {
      return(likert_review_panel_ui(character(0), NULL))
    }
    representative_mapping <- likert_group_representative_mapping(group)
    likert_review_panel_ui(likert_variable_choice_labels(group, representative_mapping), representative_mapping)
  })

  output$likert_message <- renderUI({
    message <- last_message()
    if (is.null(message)) {
      return(NULL)
    }
    div(class = "recode-same-status", message)
  })

  observeEvent(current_data_file_fn(), {
    file <- current_data_file_fn()
    groups <- summary()
    file_key <- as.character(file$name %||% file$path %||% "")
    if (is.null(file) || !nzchar(file_key) || identical(dismissed_file(), file_key) || is.null(groups) || nrow(groups) == 0) {
      return()
    }
    showModal(modalDialog(
      title = "Likert text variables detected",
      sprintf("%s Likert group(s), %s variable(s) were detected. Review and convert them before analysis?", nrow(groups), sum(groups$variable_count)),
      footer = tagList(
        modalButton("Later"),
        actionButton("open_likert_conversion", "Review and convert", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
    dismissed_file(file_key)
  }, ignoreInit = TRUE)

  observeEvent(input$open_likert_conversion, {
    removeModal()
    updateNavbarPage(session, "main_menu", selected = "data_editor_likert")
  }, ignoreInit = TRUE)

  observeEvent(input$apply_likert_conversion, {
    group <- selected_group()
    data <- tryCatch(raw_dataset_fn(), error = function(e) tryCatch(dataset_fn(), error = function(e) NULL))
    if (is.null(group) || nrow(group) == 0 || is.null(data)) {
      showNotification("No detected Likert group is available.", type = "warning", duration = 5)
      return()
    }
    variables <- if (isTRUE(input$likert_apply_same_pattern)) {
      group$variable
    } else {
      intersect(as.character(input$likert_variables %||% character(0)), group$variable)
    }
    variables <- intersect(variables, names(data))
    if (length(variables) == 0) {
      showNotification("Select at least one variable to convert.", type = "warning", duration = 5)
      return()
    }
    mapping <- likert_mapping_from_input(input, likert_group_representative_mapping(group))
    if (any(is.na(mapping$value)) || length(unique(mapping$value)) != nrow(mapping)) {
      showNotification("Numeric mapping values must be complete and unique.", type = "warning", duration = 6)
      return()
    }
    measurement <- as.character(input$likert_measurement %||% "continuous")
    if (!measurement %in% c("continuous", "ordered", "category", "binary")) {
      measurement <- "continuous"
    }

    converted <- character(0)
    for (variable in variables) {
      values <- recode_likert_values(data[[variable]], mapping, reverse = isTRUE(input$likert_reverse))
      ok <- update_existing_variable_fn(variable, values, measurement = measurement)
      if (isTRUE(ok)) {
        converted <- c(converted, variable)
      }
    }
    if (length(converted) == 0) {
      last_message("No variables were converted.")
      return()
    }

    if (is.function(apply_category_label_snapshot_fn)) {
      apply_category_label_snapshot_fn(list(
        category_labels = likert_category_payload(converted, mapping, reverse = isTRUE(input$likert_reverse)),
        measurements = stats::setNames(rep(measurement, length(converted)), converted),
        var_labels = character(0)
      ))
    }
    if (is.function(mark_settings_dirty)) {
      mark_settings_dirty()
    }
    last_message(sprintf("Converted %s Likert variable(s): %s", length(converted), paste(converted, collapse = ", ")))
  }, ignoreInit = TRUE)

  invisible(TRUE)
}
