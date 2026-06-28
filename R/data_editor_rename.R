# Variable rename command for the Data Editor menu.

empty_variable_rename_queue <- function() {
  data.frame(old = character(0), new = character(0), label = character(0), stringsAsFactors = FALSE)
}

variable_rename_scalar <- function(value, default = "") {
  value <- as.character(value %||% default)
  if (length(value) == 0 || is.na(value[[1]])) {
    return(default)
  }
  value[[1]]
}

normalize_variable_rename_queue <- function(queue) {
  if (!is.data.frame(queue) || !all(c("old", "new", "label") %in% names(queue))) {
    return(empty_variable_rename_queue())
  }
  queue <- data.frame(
    old = trimws(as.character(queue$old %||% character(0))),
    new = trimws(as.character(queue$new %||% character(0))),
    label = as.character(queue$label %||% character(0)),
    stringsAsFactors = FALSE
  )
  queue[nzchar(queue$old), , drop = FALSE]
}

variable_rename_label_for <- function(name, variable_info = NULL, labels = character(0)) {
  name <- as.character(name %||% "")
  label <- named_value(labels, name, "")
  if (!nzchar(label) && is.data.frame(variable_info) && all(c("name", "var_label") %in% names(variable_info))) {
    row_index <- match(name, as.character(variable_info$name))
    if (!is.na(row_index)) {
      label <- as.character(variable_info$var_label[[row_index]] %||% "")
    }
  }
  label
}

variable_rename_queue_text <- function(queue) {
  queue <- normalize_variable_rename_queue(queue)
  if (nrow(queue) == 0) {
    return("")
  }
  paste(sprintf("%s - %s", queue$old, queue$new), collapse = "\n")
}

variable_rename_queue_listbox <- function(queue, variable_info = NULL, labels = character(0), selected = character(0)) {
  queue <- normalize_variable_rename_queue(queue)
  old_items <- analysis_variable_items(queue$old, variable_info, labels)
  measurements <- stats::setNames(
    vapply(old_items, function(item) as.character(item$measurement %||% ""), character(1)),
    vapply(old_items, function(item) as.character(item$value %||% ""), character(1))
  )
  queue_items <- lapply(seq_len(nrow(queue)), function(index) {
    old_name <- queue$old[[index]]
    list(
      value = old_name,
      label = sprintf("%s - %s", old_name, queue$new[[index]]),
      measurement = named_value(measurements, old_name, "")
    )
  })
  div(
    class = "variable-rename-queue-listbox-wrap",
    analysis_transfer_listbox_input(
      "variable_rename_queue",
      queue_items,
      selected = selected,
      size = 8
    )
  )
}

remove_variable_rename_targets <- function(queue, queue_selected = character(0), target_selected = character(0)) {
  queue <- normalize_variable_rename_queue(queue)
  if (nrow(queue) == 0) {
    return(character(0))
  }
  queue_selected <- intersect(as.character(queue_selected %||% character(0)), queue$old)
  if (length(queue_selected) > 0) {
    return(queue_selected)
  }
  target_selected <- intersect(as.character(target_selected %||% character(0)), queue$old)
  if (length(target_selected) > 0) {
    return(target_selected)
  }
  if (nrow(queue) == 1) {
    return(queue$old[[1]])
  }
  character(0)
}

data_editor_variable_rename_panel <- function(language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  options(statedu.app_language = language)
  div(
    class = "page-shell",
    div(
      class = "app-heading",
      h1(statedu_text(language, "Rename Variable", statedu_utf8("ebb380ec889820ec9db4eba68420ebb380eab2bd"))),
      div(
        statedu_text(
          language,
          "Rename variables and labels in the current data.",
          statedu_utf8("ed9884ec9eac20eb8db0ec9db4ed84b0ec9d9820ebb380ec8898ebaa85eab3bc20eb9dbcebb2a8ec9d8420ebb380eab2bded95a9eb8b88eb8ba42e")
        ),
        class = "app-subtitle"
      )
    ),
    div(
      class = "workspace-panel frequencies-workspace-panel data-editor-workspace",
      analysis_workspace_heading("Variable rename", "variable_rename", language = language),
      analysis_workspace_body(
        "variable_rename",
        uiOutput("variable_rename_setup"),
        div(
          class = "analysis-action-row recode-same-action-row variable-rename-action-row",
          actionButton("run_variable_rename", analysis_ui_text("Run", language), class = "btn btn-primary"),
          actionButton("remove_variable_rename", analysis_ui_text("Remove", language), class = "btn btn-default variable-rename-remove-button")
        ),
        uiOutput("variable_rename_message")
      )
    )
  )
}

variable_rename_setup_panel <- function(
  file,
  data,
  variable_info,
  labels = character(0),
  selected_variables = character(0),
  selected_target = character(0),
  queue = empty_variable_rename_queue(),
  input = NULL,
  language = statedu_initial_language()
) {
  language <- normalize_app_language(language)
  options(statedu.app_language = language)
  if (is.null(file) || is.null(data)) {
    return(setup_empty_message(statedu_text(
      language,
      "Load a data file in the Data tab before renaming variables.",
      statedu_utf8("eb8db0ec9db4ed84b020ed83adec9790ec849c20eb8db0ec9db4ed84b020ed8c8cec9dbcec9d8420eba8bceca08020ebb688eb9facec98a820ed9b8420ebb380ec8898ebaa85ec9d8420ebb380eab2bded9598ec84b8ec9a942e")
    ), language = language))
  }
  variable_names <- names(data)
  if (length(variable_names) == 0) {
    return(setup_empty_message(statedu_text(
      language,
      "The current data file has no variables.",
      statedu_utf8("ed9884ec9eac20eb8db0ec9db4ed84b020ed8c8cec9dbcec979020ebb380ec8898eab08020ec9786ec8ab5eb8b88eb8ba42e")
    ), language = language))
  }

  queue <- normalize_variable_rename_queue(queue)
  selected_variables <- intersect(as.character(selected_variables %||% character(0)), variable_names)
  available <- setdiff(variable_names, selected_variables)
  selected_available <- selected_order_items(isolate(input$variable_rename_available) %||% character(0), available)
  selected_selected <- selected_order_items(selected_target, selected_variables)
  if (length(selected_selected) == 0) {
    selected_selected <- selected_order_items(isolate(input$variable_rename_selected) %||% character(0), selected_variables)
  }
  active_variable <- selected_order_item(selected_selected, selected_variables)
  if ((length(active_variable) == 0 || !nzchar(active_variable)) && length(selected_variables) > 0) {
    active_variable <- selected_variables[[1]]
  }
  active_variable <- variable_rename_scalar(active_variable)
  selected_queue <- selected_order_items(isolate(input$variable_rename_queue) %||% character(0), queue$old)

  queued_index <- if (nzchar(active_variable)) match(active_variable, queue$old) else NA_integer_
  active_new_name <- if (!is.na(queued_index)) queue$new[[queued_index]] else active_variable
  active_label <- if (!is.na(queued_index)) queue$label[[queued_index]] else variable_rename_label_for(active_variable, variable_info, labels)

  div(
    class = "recode-same-setup-grid variable-rename-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel variable-rename-source-panel",
      analysis_field_label_tag("Variables", language = language),
      analysis_transfer_listbox_input(
        "variable_rename_available",
        items = analysis_variable_items(available, variable_info, labels),
        selected = selected_available,
        size = 19
      )
    ),
    div(
      class = "analysis-transfer-controls recode-same-transfer-controls variable-rename-transfer-controls",
      actionButton(
        "variable_rename_move",
        ">",
        class = "btn btn-default analysis-move-button",
        onmousedown = "if(window.easyflowRememberAllTransferScrolls){window.easyflowRememberAllTransferScrolls();}",
        disabled = if (length(available) == 0 && length(selected_variables) == 0) "disabled" else NULL
      )
    ),
    div(
      class = "analysis-transfer-column analysis-transfer-panel variable-rename-target-panel",
      analysis_field_label_tag("Old variable", language = language),
      analysis_transfer_listbox_input(
        "variable_rename_selected",
        items = analysis_variable_items(selected_variables, variable_info, labels),
        selected = selected_selected,
        size = 8
      ),
      div(
        class = "analysis-option-title variable-rename-queue-title",
        statedu_text(language, "Rename variables", statedu_utf8("ec9db4eba68420ebb380eab2bded95a020ebb380ec8898"))
      ),
      variable_rename_queue_listbox(queue, variable_info, labels, selected = selected_queue)
    ),
    div(class = "variable-rename-grid-spacer"),
    div(
      class = "analysis-options-column analysis-options-panel variable-rename-options",
      div(
        class = "analysis-option-group",
        div(class = "analysis-option-title", analysis_ui_text("New name", language)),
        textInput("variable_rename_new_name", analysis_ui_text("Variable name", language), value = active_new_name, width = "100%"),
        textAreaInput("variable_rename_label", analysis_ui_text("Label", language), value = active_label, width = "100%", height = "92px"),
        actionButton("queue_variable_rename", analysis_ui_text("Apply", language), class = "btn btn-primary variable-rename-apply-button")
      )
    )
  )
}

register_variable_rename_handlers <- function(
  input,
  output,
  session,
  dataset_fn,
  current_data_file_fn,
  variable_info_fn,
  labels_fn,
  rename_variable_fn,
  mark_settings_dirty,
  language_fn = NULL
) {
  selected_variables <- reactiveVal(character(0))
  target_selection <- reactiveVal(character(0))
  rename_queue <- reactiveVal(empty_variable_rename_queue())
  last_message <- reactiveVal(NULL)
  active_rename_list <- reactiveVal(NULL)

  current_variable_names <- function() {
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    names(data %||% data.frame())
  }

  output$variable_rename_setup <- renderUI({
    language <- statedu_current_language(language_fn)
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    variable_info <- tryCatch(variable_info_fn(), error = function(e) NULL)
    variable_rename_setup_panel(
      file = current_data_file_fn(),
      data = data,
      variable_info = variable_info,
      labels = labels_fn(),
      selected_variables = selected_variables(),
      selected_target = isolate(target_selection()),
      queue = rename_queue(),
      input = input,
      language = language
    )
  })

  observeEvent(input$variable_rename_available_active, {
    active_rename_list("variable_rename_available")
  }, ignoreInit = TRUE)

  observeEvent(input$variable_rename_selected_active, {
    active_rename_list("variable_rename_selected")
  }, ignoreInit = TRUE)

  observeEvent(input$variable_rename_queue_active, {
    active_rename_list("variable_rename_queue")
  }, ignoreInit = TRUE)

  observe({
    if (identical(active_rename_list(), "variable_rename_selected") && length(input$variable_rename_selected %||% character(0)) > 0) {
      updateActionButton(session, "variable_rename_move", label = "<")
    } else {
      updateActionButton(session, "variable_rename_move", label = ">")
    }
  })

  observeEvent(input$variable_rename_move, {
    values <- intersect(as.character(input$variable_rename_available %||% character(0)), current_variable_names())
    current <- intersect(as.character(selected_variables() %||% character(0)), current_variable_names())
    selected_values <- intersect(as.character(input$variable_rename_selected %||% character(0)), current)

    if (identical(active_rename_list(), "variable_rename_selected") && length(selected_values) > 0) {
      selected_variables(setdiff(current, selected_values))
      target_selection(setdiff(target_selection(), selected_values))
      queue <- normalize_variable_rename_queue(rename_queue())
      rename_queue(queue[!queue$old %in% selected_values, , drop = FALSE])
      active_rename_list("variable_rename_available")
      return()
    }

    if (length(values) == 0) {
      return()
    }
    moved <- setdiff(values, selected_variables())
    selected_variables(c(selected_variables(), moved))
    target_selection(values)
    active_rename_list("variable_rename_selected")
  }, ignoreInit = TRUE)

  observeEvent(input$variable_rename_available_doubleclick, {
    value <- variable_rename_scalar(input$variable_rename_available_doubleclick$value)
    if (!nzchar(value) || !value %in% current_variable_names()) {
      return()
    }
    selected_variables(c(selected_variables(), setdiff(value, selected_variables())))
    target_selection(value)
    active_rename_list("variable_rename_selected")
  }, ignoreInit = TRUE)

  observeEvent(input$variable_rename_selected_doubleclick, {
    value <- variable_rename_scalar(input$variable_rename_selected_doubleclick$value)
    if (!nzchar(value)) {
      return()
    }
    selected_variables(setdiff(selected_variables(), value))
    target_selection(setdiff(target_selection(), value))
    queue <- normalize_variable_rename_queue(rename_queue())
    rename_queue(queue[queue$old != value, , drop = FALSE])
    active_rename_list("variable_rename_available")
  }, ignoreInit = TRUE)

  observeEvent(input$variable_rename_selected, {
    selected <- selected_order_items(input$variable_rename_selected, selected_variables())
    active <- selected_order_item(selected, selected_variables())
    active <- variable_rename_scalar(active)
    if (!nzchar(active)) {
      return()
    }
    queue <- normalize_variable_rename_queue(rename_queue())
    target_selection(selected)
    active_rename_list("variable_rename_selected")
    queued_index <- match(active, queue$old)
    variable_info <- tryCatch(variable_info_fn(), error = function(e) NULL)
    updateTextInput(
      session,
      "variable_rename_new_name",
      value = if (!is.na(queued_index)) queue$new[[queued_index]] else active
    )
    updateTextAreaInput(
      session,
      "variable_rename_label",
      value = if (!is.na(queued_index)) queue$label[[queued_index]] else variable_rename_label_for(active, variable_info, labels_fn())
    )
  }, ignoreInit = TRUE)

  observeEvent(input$variable_rename_queue, {
    queue <- normalize_variable_rename_queue(rename_queue())
    selected <- selected_order_items(input$variable_rename_queue, queue$old)
    active <- variable_rename_scalar(selected_order_item(selected, queue$old))
    if (!nzchar(active)) {
      return()
    }
    active_rename_list("variable_rename_queue")
    target_selection(active)
    queued_index <- match(active, queue$old)
    if (is.na(queued_index)) {
      return()
    }
    updateTextInput(session, "variable_rename_new_name", value = queue$new[[queued_index]])
    updateTextAreaInput(session, "variable_rename_label", value = queue$label[[queued_index]])
  }, ignoreInit = TRUE)

  output$variable_rename_message <- renderUI({
    statedu_current_language(language_fn)
    message <- last_message()
    if (is.null(message)) {
      return(NULL)
    }
    div(class = "recode-same-status", message)
  })

  observeEvent(input$queue_variable_rename, {
    language <- statedu_current_language(language_fn)
    old_name <- selected_order_item(input$variable_rename_selected, selected_variables())
    old_name <- variable_rename_scalar(old_name)
    new_name <- trimws(variable_rename_scalar(input$variable_rename_new_name))
    new_label <- variable_rename_scalar(input$variable_rename_label)
    if (!nzchar(old_name)) {
      showNotification(
        statedu_text(
          language,
          "Move a variable to the second block first.",
          statedu_utf8("eba8bceca08020ebb380ec8898eba5bc20eb919020ebb288eca7b820ec9881ec97adec9cbceba19c20ec9db4eb8f99ed9598ec84b8ec9a942e")
        ),
        type = "warning",
        duration = 5
      )
      return()
    }
    if (!nzchar(new_name)) {
      showNotification(
        statedu_text(
          language,
          "Enter the new variable name.",
          statedu_utf8("ec838820ebb380ec8898ebaa85ec9d8420ec9e85eba0a5ed9598ec84b8ec9a942e")
        ),
        type = "warning",
        duration = 5
      )
      return()
    }
    data_names <- current_variable_names()
    queue <- normalize_variable_rename_queue(rename_queue())
    reserved_names <- setdiff(queue$new, queue$new[match(old_name, queue$old)])
    if (new_name %in% setdiff(data_names, old_name) || new_name %in% reserved_names) {
      showNotification(
        sprintf(
          statedu_text(
            language,
            "Variable already exists or is already queued: %s",
            statedu_utf8("ec9db4ebafb820eca1b4ec9eaced9598eab1b0eb829820eb8c80eab8b0ec97b4ec979020ec9e88eb8a9420ebb380ec8898ec9e85eb8b88eb8ba43a202573")
          ),
          new_name
        ),
        type = "warning",
        duration = 5
      )
      return()
    }

    queue <- queue[queue$old != old_name, , drop = FALSE]
    queue <- rbind(
      queue,
      data.frame(old = old_name, new = new_name, label = new_label, stringsAsFactors = FALSE)
    )
    rename_queue(queue)
    last_message(sprintf(
      statedu_text(
        language,
        "Queued rename: %s -> %s",
        statedu_utf8("ec9db4eba68420ebb380eab2bd20eb8c80eab8b03a202573202d3e202573")
      ),
      old_name,
      new_name
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$remove_variable_rename, {
    language <- statedu_current_language(language_fn)
    queue <- normalize_variable_rename_queue(rename_queue())
    targets <- remove_variable_rename_targets(
      queue,
      queue_selected = input$variable_rename_queue %||% character(0),
      target_selected = input$variable_rename_selected %||% character(0)
    )
    if (length(targets) == 0) {
      showNotification(
        statedu_text(
          language,
          "Select a queued rename to remove.",
          statedu_utf8("eca09ceab1b0ed95a020ec9db4eba68420ebb380eab2bd20ed95adebaaa9ec9d8420ec84a0ed839ded9598ec84b8ec9a942e")
        ),
        type = "warning",
        duration = 5
      )
      return()
    }
    rename_queue(queue[!queue$old %in% targets, , drop = FALSE])
    target_selection(setdiff(target_selection(), targets))
    last_message(sprintf(
      statedu_text(
        language,
        "Removed queued rename: %s",
        statedu_utf8("ec9db4eba68420ebb380eab2bd20eb8c80eab8b020eca09ceab1b03a202573")
      ),
      paste(targets, collapse = ", ")
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$run_variable_rename, {
    language <- statedu_current_language(language_fn)
    queue <- normalize_variable_rename_queue(rename_queue())
    if (nrow(queue) == 0) {
      showNotification(
        statedu_text(
          language,
          "Add at least one rename to the queue before running.",
          statedu_utf8("ec8ba4ed9689ed9598eab8b020eca084ec979020ed9598eb829820ec9db4ec8381ec9d9820ec9db4eba68420ebb380eab2bd20ed95adebaaa9ec9d8420eb8c80eab8b0ec97b4ec979020ecb694eab080ed9598ec84b8ec9a942e")
        ),
        type = "warning",
        duration = 5
      )
      return()
    }
    if (!is.function(rename_variable_fn)) {
      showNotification(
        statedu_text(
          language,
          "Variable rename is not available in this session.",
          statedu_utf8("ec9db420ec84b8ec8598ec9790ec849ceb8a9420ebb380ec889820ec9db4eba68420ebb380eab2bdec9d8420ec82acec9aa9ed95a020ec889820ec9786ec8ab5eb8b88eb8ba42e")
        ),
        type = "warning",
        duration = 5
      )
      return()
    }

    completed <- character(0)
    for (row_index in seq_len(nrow(queue))) {
      ok <- rename_variable_fn(queue$old[[row_index]], queue$new[[row_index]], var_label = queue$label[[row_index]])
      if (isTRUE(ok)) {
        completed <- c(completed, sprintf("%s -> %s", queue$old[[row_index]], queue$new[[row_index]]))
      }
    }
    if (length(completed) > 0) {
      selected_variables(character(0))
      target_selection(character(0))
      rename_queue(empty_variable_rename_queue())
      last_message(sprintf(
        statedu_text(
          language,
          "Renamed %s variable(s): %s",
          statedu_utf8("2573eab09c20ebb380ec8898ec9d9820ec9db4eba684ec9d8420ebb380eab2bded9688ec8ab5eb8b88eb8ba43a202573")
        ),
        length(completed),
        paste(completed, collapse = ", ")
      ))
      if (is.function(mark_settings_dirty)) {
        mark_settings_dirty()
      }
    }
  }, ignoreInit = TRUE)

  invisible(TRUE)
}
