# Server handlers for reliability analysis.

register_reliability_handlers <- function(
  input,
  output,
  session,
  dataset_fn,
  selected_names_fn,
  variable_table_fn,
  labels_fn,
  category_table_fn,
  reliability_variables,
  mark_settings_dirty
) {
  active_reliability_list <- reactiveVal(NULL)
  reliability_factor_blocks <- reactiveVal(NULL)
  active_reliability_factor <- reactiveVal(1L)

  normalize_reliability_blocks <- function(blocks, selected = selected_names_fn()) {
    selected <- as.character(selected %||% character(0))
    blocks <- blocks %||% list(as.character(reliability_variables() %||% character(0)))
    blocks <- lapply(blocks, function(block) intersect(as.character(block %||% character(0)), selected))
    if (length(blocks) == 0) blocks <- list(character(0))
    blocks
  }

  current_reliability_blocks <- reactive({
    blocks <- reliability_factor_blocks()
    if (is.null(blocks)) {
      blocks <- list(as.character(reliability_variables() %||% character(0)))
    }
    normalize_reliability_blocks(blocks)
  })

  set_reliability_blocks <- function(blocks) {
    blocks <- normalize_reliability_blocks(blocks)
    reliability_factor_blocks(blocks)
    index <- min(max(1L, as.integer(active_reliability_factor() %||% 1L)), length(blocks))
    active_reliability_factor(index)
    reliability_variables(blocks[[index]])
    mark_settings_dirty()
    invisible(blocks)
  }

  active_reliability_variables <- function() {
    blocks <- current_reliability_blocks()
    index <- min(max(1L, as.integer(active_reliability_factor() %||% 1L)), length(blocks))
    blocks[[index]]
  }

  reliability_state <- reactive({
    reliability_setup_state(
      selected_names = selected_names_fn(),
      variable_table = variable_table_fn(),
      labels = labels_fn(),
      selected_variables = active_reliability_variables(),
      factor_blocks = current_reliability_blocks(),
      active_factor = active_reliability_factor(),
      selected_available = isolate(input$reliability_available),
      selected_selected = isolate(input$reliability_selected),
      normality = input$reliability_normality,
      ordinal = input$reliability_ordinal,
      subfactor_enabled = input$reliability_subfactor_enabled,
      reliability_if_deleted = input$reliability_if_deleted,
      item_total_correlation = input$reliability_item_total_correlation
    )
  })

  output$reliability_setup <- renderUI({
    selected <- as.character(selected_names_fn() %||% character(0))
    if (length(selected) == 0) {
      return(setup_empty_message("Complete Step 2 in the Data tab before setting up reliability analysis."))
    }
    reliability_setup_panel(reliability_state())
  })

  register_analysis_data_viewer_handlers(
    input = input,
    output = output,
    prefix = "reliability",
    title = "Reliability Data Viewer",
    dataset_fn = dataset_fn,
    selected_names_fn = selected_names_fn,
    variables_fn = function() unique(unlist(current_reliability_blocks(), use.names = FALSE)),
    variable_table_fn = variable_table_fn,
    labels_fn = labels_fn,
    category_table_fn = category_table_fn
  )

  observe({
    selected <- as.character(selected_names_fn() %||% character(0))
    blocks <- current_reliability_blocks()
    updated <- normalize_reliability_blocks(blocks, selected)
    if (!identical(updated, blocks)) {
      set_reliability_blocks(updated)
    }
  })

  observeEvent(input$reliability_available_active, {
    active_reliability_list("reliability_available")
  }, ignoreInit = TRUE)

  observeEvent(input$reliability_selected_active, {
    active_reliability_list("reliability_selected")
  }, ignoreInit = TRUE)

  observe({
    if (identical(active_reliability_list(), "reliability_selected") && length(input$reliability_selected %||% character(0)) > 0) {
      updateActionButton(session, "reliability_move", label = "<")
    } else {
      updateActionButton(session, "reliability_move", label = ">")
    }
  })

  observeEvent(input$reliability_subfactor_enabled, {
    if (!isTRUE(input$reliability_subfactor_enabled)) {
      blocks <- current_reliability_blocks()
      active_reliability_factor(1L)
      reliability_variables(blocks[[1L]])
      active_reliability_list("reliability_selected")
    }
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$reliability_factor_next, {
    blocks <- current_reliability_blocks()
    index <- as.integer(active_reliability_factor() %||% 1L)
    if (index >= length(blocks)) {
      blocks <- c(blocks, list(character(0)))
      reliability_factor_blocks(blocks)
    }
    active_reliability_factor(index + 1L)
    reliability_variables(blocks[[index + 1L]])
    active_reliability_list("reliability_selected")
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$reliability_factor_prev, {
    blocks <- current_reliability_blocks()
    index <- max(1L, as.integer(active_reliability_factor() %||% 1L) - 1L)
    active_reliability_factor(index)
    reliability_variables(blocks[[index]])
    active_reliability_list("reliability_selected")
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  reliability_selection_measurements <- function(names) {
    reliability_measurements_for(names, variable_table_fn())
  }

  reliability_selection_allowed <- function(names) {
    measurements <- reliability_selection_measurements(names)
    allowed <- c("binary", "ordered", "continuous")
    length(names) > 0 &&
      all(measurements %in% allowed) &&
      length(unique(unname(measurements))) == 1
  }

  observeEvent(input$reliability_move, {
    selected <- as.character(selected_names_fn() %||% character(0))
    available_selected <- intersect(as.character(input$reliability_available %||% character(0)), selected)
    blocks <- current_reliability_blocks()
    factor_index <- min(max(1L, as.integer(active_reliability_factor() %||% 1L)), length(blocks))
    current <- intersect(as.character(blocks[[factor_index]] %||% character(0)), selected)
    selected_selected <- intersect(as.character(input$reliability_selected %||% character(0)), current)

    if (identical(active_reliability_list(), "reliability_selected") && length(selected_selected) > 0) {
      blocks[[factor_index]] <- setdiff(current, selected_selected)
      set_reliability_blocks(blocks)
      active_reliability_list("reliability_available")
      return()
    }
    if (length(available_selected) > 0) {
      next_variables <- c(current, setdiff(available_selected, current))
      if (!isTRUE(reliability_selection_allowed(next_variables))) {
        showNotification(
          "Reliability analysis accepts only one measurement level at a time. Select binary, ordinal, or continuous items only.",
          type = "warning",
          duration = 6
        )
        return()
      }
      blocks[[factor_index]] <- next_variables
      set_reliability_blocks(blocks)
      active_reliability_list("reliability_selected")
    }
  })

  observeEvent(input$reliability_selected_doubleclick, {
    selected <- as.character(selected_names_fn() %||% character(0))
    blocks <- current_reliability_blocks()
    factor_index <- min(max(1L, as.integer(active_reliability_factor() %||% 1L)), length(blocks))
    current <- intersect(as.character(blocks[[factor_index]] %||% character(0)), selected)
    chosen <- intersect(as.character(input$reliability_selected_doubleclick$value %||% ""), current)
    if (length(chosen) == 0) return()
    blocks[[factor_index]] <- setdiff(current, chosen)
    set_reliability_blocks(blocks)
    active_reliability_list("reliability_available")
  }, ignoreInit = TRUE)

  observeEvent(input$reliability_move_up, {
    blocks <- current_reliability_blocks()
    factor_index <- min(max(1L, as.integer(active_reliability_factor() %||% 1L)), length(blocks))
    updated <- move_order_item(blocks[[factor_index]], input$reliability_selected, "up")
    if (isTRUE(updated$changed)) {
      blocks[[factor_index]] <- updated$order
      set_reliability_blocks(blocks)
      active_reliability_list("reliability_selected")
    }
  })

  observeEvent(input$reliability_move_down, {
    blocks <- current_reliability_blocks()
    factor_index <- min(max(1L, as.integer(active_reliability_factor() %||% 1L)), length(blocks))
    updated <- move_order_item(blocks[[factor_index]], input$reliability_selected, "down")
    if (isTRUE(updated$changed)) {
      blocks[[factor_index]] <- updated$order
      set_reliability_blocks(blocks)
      active_reliability_list("reliability_selected")
    }
  })

  reliability_result <- reactiveVal(NULL)

  observeEvent(input$run_reliability, {
    valid_blocks <- if (isTRUE(input$reliability_subfactor_enabled)) {
      Filter(function(block) length(block) >= 2, current_reliability_blocks())
    } else {
      list(active_reliability_variables())
    }
    if (length(valid_blocks) == 0) {
      showNotification("Select at least two items for reliability analysis.", type = "warning", duration = 5)
      return()
    }
    result <- tryCatch(
      {
        options <- list(
          normality = isTRUE(input$reliability_normality),
          ordinal = isTRUE(input$reliability_ordinal),
          reliability_if_deleted = isTRUE(input$reliability_if_deleted),
          item_total_correlation = isTRUE(input$reliability_item_total_correlation)
        )
        results <- lapply(seq_along(valid_blocks), function(index) {
          result <- prepare_reliability_results(
            data = dataset_fn(),
            variables = valid_blocks[[index]],
            variable_info = variable_table_fn(),
            labels = labels_fn(),
            category_table = category_table_fn(),
            options = options
          )
          result$subfactor <- paste0("Subfactor ", index)
          result
        })
        if (!isTRUE(input$reliability_subfactor_enabled)) {
          results[[1]]
        } else {
          total_variables <- unique(unlist(valid_blocks, use.names = FALSE))
          total <- prepare_reliability_results(
            data = dataset_fn(),
            variables = total_variables,
            variable_info = variable_table_fn(),
            labels = labels_fn(),
            category_table = category_table_fn(),
            options = options
          )
          total$subfactor <- "Total"
          list(type = "reliability_factors", total = total, factors = results, options = options)
        }
      },
      error = function(e) {
        showNotification(conditionMessage(e), type = "warning", duration = 7)
        NULL
      }
    )
    if (!is.null(result)) {
      reliability_result(result)
    }
  })

  output$reliability_results <- renderUI({
    reliability_results_ui(reliability_result())
  })

  output$reliability_reset_control <- renderUI({
    analysis_reset_button(
      "reset_reliability_selection",
      enabled = length(unique(unlist(current_reliability_blocks(), use.names = FALSE))) > 0
    )
  })

  observeEvent(input$reset_reliability_selection, {
    if (length(unique(unlist(current_reliability_blocks(), use.names = FALSE))) == 0) return()
    reliability_factor_blocks(list(character(0)))
    active_reliability_factor(1L)
    reliability_variables(character(0))
    reliability_result(NULL)
    active_reliability_list("reliability_available")
    session$sendCustomMessage(
      "easyflow-clear-transfer-selection",
      list(inputIds = c("reliability_available", "reliability_selected"))
    )
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  output$reliability_save_control <- renderUI({
    result <- reliability_result()
    if (is.null(result)) {
      return(NULL)
    }
    analysis_save_buttons(
      html_button_id = "save_reliability_html_dialog",
      figure_button_id = NULL,
      excel_button_id = "save_reliability_excel_dialog",
      add_result_button_id = "add_reliability_result",
      has_figures = FALSE
    )
  })

  observeEvent(input$save_reliability_excel_dialog, {
    result <- reliability_result()
    shiny::req(!is.null(result))
    path <- choose_excel_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) {
      showNotification("Save dialog was not available or was canceled.", type = "warning", duration = 5)
      return(invisible(NULL))
    }
    if (!grepl("\\.xlsx$", path, ignore.case = TRUE)) {
      path <- paste0(path, ".xlsx")
    }
    tryCatch(
      {
        save_reliability_excel_file(result, path)
        showNotification(sprintf("Analysis results saved: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save analysis results:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  observeEvent(input$save_reliability_html_dialog, {
    result <- reliability_result()
    shiny::req(!is.null(result))
    path <- choose_html_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) {
      showNotification("Save dialog was not available or was canceled.", type = "warning", duration = 5)
      return(invisible(NULL))
    }
    if (!grepl("\\.html?$", path, ignore.case = TRUE)) {
      path <- paste0(path, ".html")
    }
    tryCatch(
      {
        write_reliability_results_html(result, path)
        showNotification(sprintf("HTML results saved: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save HTML results:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  register_add_result_placeholder(input, "add_reliability_result")

  invisible(TRUE)
}
