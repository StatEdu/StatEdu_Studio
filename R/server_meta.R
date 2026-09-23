# Server handlers for study-level meta-analysis effect input.

meta_values_from_input <- function(input, family) {
  field_names <- c(
    "m1", "sd1", "n1", "m0", "sd0", "n0", "n", "g", "d_value", "r", "r_pb",
    "fisher_z", "t_value", "k_controls", "cell_a", "cell_b", "cell_c", "cell_d",
    "or_value", "log_or", "logit_b", "se", "ci_lower", "ci_upper"
  )
  values <- list(
    included = isTRUE(input$meta_field_included),
    study_id = input$meta_field_study_id,
    study_name = input$meta_field_study_name,
    publication_year = input$meta_field_publication_year,
    outcome = input$meta_field_outcome,
    predictor = input$meta_field_predictor,
    moderator_categorical = input$meta_field_moderator_categorical,
    moderator_continuous = input$meta_field_moderator_continuous,
    family = family,
    input_type = input$meta_input_type,
    direction = input$meta_field_direction
  )
  input_type <- as.character(input$meta_input_type %||% "")
  active_fields <- meta_input_field_map(family)[[input_type]]
  if (is.null(active_fields)) active_fields <- character(0)
  for (field in field_names) {
    values[[field]] <- if (field %in% active_fields) input[[paste0("meta_field_", field)]] else NULL
  }
  values
}

meta_status_label <- function(status, language = statedu_initial_language()) {
  if (!status %in% c("valid", "warning", "error")) return(status)
  statedu_t(paste0("meta.input_status.", status), language)
}

meta_excel_template_sheet <- function(family) {
  switch(tolower(as.character(family[[1]])), g = "Hedges_g", r = "Correlation_r", or = "Odds_Ratio", "Hedges_g")
}

meta_excel_type_sheets <- function(family) {
  switch(
    tolower(as.character(family[[1]])),
    g = c(MEANS = "means", G_SE = "g_se", G_CI = "g_ci", COHENS_D = "d", T_TEST = "t", R_PB = "r_pb"),
    r = c(R = "r", Z_SE = "z_se", T_CORR = "t", PARTIAL_R = "partial_r"),
    or = c(TABLE_2X2 = "2x2", OR_CI = "or_ci", LOGOR_SE = "logor_se", LOGISTIC_B = "logistic_b"),
    character(0)
  )
}

meta_bind_import_frames <- function(frames) {
  frames <- Filter(function(frame) is.data.frame(frame) && nrow(frame) > 0L, frames)
  if (length(frames) == 0L) return(data.frame())
  columns <- unique(unlist(lapply(frames, names), use.names = FALSE))
  frames <- lapply(frames, function(frame) {
    for (column in setdiff(columns, names(frame))) frame[[column]] <- NA_character_
    frame[, columns, drop = FALSE]
  })
  rownames(frames) <- NULL
  do.call(rbind, frames)
}

meta_excel_workbook_settings <- function(path, sheets) {
  if (!"StatEdu" %in% sheets) return(list(mode = "", family = ""))
  settings <- suppressMessages(as.data.frame(
    readxl::read_excel(path, sheet = "StatEdu", range = "A12:B13", col_names = FALSE, col_types = "text"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  ))
  if (ncol(settings) < 2L || nrow(settings) == 0L) return(list(mode = "", family = ""))
  keys <- tolower(trimws(as.character(settings[[1]])))
  values <- trimws(as.character(settings[[2]]))
  lookup <- stats::setNames(values, keys)
  setting_value <- function(key) {
    value <- unname(lookup[key])
    if (length(value) == 0L || is.na(value[[1]])) "" else as.character(value[[1]])
  }
  mode <- toupper(setting_value("entry_mode"))
  mode <- gsub("[^A-Z0-9]+", "_", mode)
  mode <- gsub("^_+|_+$", "", mode)
  list(
    mode = mode,
    family = tolower(setting_value("target_family"))
  )
}

meta_import_stop <- function(key, ...) {
  values <- list(...)
  message <- do.call(sprintf, c(list(statedu_t(paste0("meta.import_error.", key), "en")), values))
  stop(structure(list(message = message, call = NULL, key = key, values = values),
    class = c("statedu_meta_import_error", "error", "condition")))
}

meta_import_error_text <- function(error, language) {
  if (!inherits(error, "statedu_meta_import_error")) return(conditionMessage(error))
  do.call(sprintf, c(list(statedu_t(paste0("meta.import_error.", error$key), language)), error$values))
}

meta_read_effect_input_file <- function(file, family) {
  extension <- tolower(tools::file_ext(as.character(file$name %||% file$datapath %||% "")))
  if (identical(extension, "csv")) return(read_csv_robust(file$datapath, csv_header = TRUE))
  if (!extension %in% c("xlsx", "xls")) meta_import_stop("extension")
  if (!requireNamespace("readxl", quietly = TRUE)) meta_import_stop("readxl")
  sheets <- readxl::excel_sheets(file$datapath)
  settings <- meta_excel_workbook_settings(file$datapath, sheets)
  selected_family <- tolower(as.character(family[[1]]))
  if (nzchar(settings$family) && !identical(settings$family, selected_family)) {
    meta_import_stop("family", settings$family, selected_family)
  }
  if (identical(settings$mode, "ALL")) {
    if (!"All_Input" %in% sheets) meta_import_stop("all_sheet")
    return(as.data.frame(
      readxl::read_excel(file$datapath, sheet = "All_Input", col_types = "text", .name_repair = "minimal"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }
  if (settings$mode %in% c("ENTRY_MODE", "TYPE_SHEETS")) {
    mapping <- meta_excel_type_sheets(selected_family)
    available <- intersect(names(mapping), sheets)
    if (length(available) == 0L) meta_import_stop("type_sheets")
    frames <- lapply(available, function(sheet_name) {
      frame <- as.data.frame(
        readxl::read_excel(file$datapath, sheet = sheet_name, col_types = "text", .name_repair = "minimal"),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      frame$input_type <- unname(mapping[[sheet_name]])
      frame
    })
    return(meta_bind_import_frames(frames))
  }
  if ("StatEdu" %in% sheets) {
    shown_mode <- if (nzchar(settings$mode)) settings$mode else "(blank)"
    meta_import_stop("mode", shown_mode)
  }
  requested <- meta_excel_template_sheet(family)
  selected <- if (requested %in% sheets) requested else sheets[[1]]
  as.data.frame(
    readxl::read_excel(file$datapath, sheet = selected, col_types = "text", .name_repair = "minimal"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

meta_excel_template_filename <- function(family) {
  switch(
    tolower(as.character(family[[1]])),
    g = "statedu_meta_g_template.xlsx",
    r = "statedu_meta_r_template.xlsx",
    or = "statedu_meta_or_template.xlsx",
    "statedu_meta_g_template.xlsx"
  )
}

meta_excel_template_asset <- function(family = "g") {
  file.path(getwd(), "www", "templates", meta_excel_template_filename(family))
}

meta_input_detail_text <- function(message, language) {
  keys <- paste0("meta.input_error.", c("pairs", "unique", "numeric", "conflict",
    "g_sample", "g_means", "g_pooled", "g_se", "g_ci", "g_d", "g_t", "g_rpb", "g_type", "g_rpb_assumption",
    "r_range", "r_sample", "r_controls", "r_df", "r_z", "r_t", "r_type", "r_assumption",
    "or_nonnegative", "or_integer", "or_ci", "or_log", "or_b", "or_type", "or_correction", "or_correction_assumption", "or_coding",
    "effect_variance", "family", "format", "study_id"))
  english <- vapply(keys, function(key) statedu_t(key, "en"), character(1))
  index <- match(message, english)
  if (!is.na(index)) return(statedu_t(keys[[index]], language))
  # Translate only a complete sequence of owned messages; never replace fragments in external text.
  remaining <- message
  matched <- character(0)
  while (nzchar(remaining)) {
    year <- regmatches(remaining, regexec("^Publication year must be an integer from 1800 to ([0-9]{4})[.]( |$)", remaining))[[1]]
    if (length(year)) {
      matched <- c(matched, sprintf(statedu_t("meta.input_error.year", language), year[[2]]))
      remaining <- substring(remaining, nchar(year[[1]]) + 1L)
      next
    }
    candidates <- which(remaining == english | startsWith(remaining, paste0(english, " ")))
    if (!length(candidates)) return(message)
    next_index <- candidates[[which.max(nchar(english[candidates]))]]
    matched <- c(matched, statedu_t(keys[[next_index]], language))
    remaining <- substring(remaining, nchar(english[[next_index]]) + 1L)
    if (startsWith(remaining, " ")) remaining <- substring(remaining, 2L)
  }
  if (!length(matched)) return(message)
  paste(matched, collapse = " ")
}

meta_model_error_text <- function(error, language) {
  message <- conditionMessage(error)
  keys <- paste0("meta.model_error.", c("confidence", "estimator", "input_errors", "two_effects", "finite_effects",
    "moderator_select", "moderator_rank", "moderator_df", "moderator_studies", "moderator_levels", "moderator_values"))
  index <- match(message, vapply(keys, function(key) statedu_t(key, "en"), character(1)))
  if (is.na(index)) meta_input_detail_text(message, language) else statedu_t(keys[[index]], language)
}

meta_input_moderator_display <- function(categorical = "", continuous = "", language = statedu_initial_language()) {
  categorical <- meta_text_value(categorical)
  continuous <- meta_text_value(continuous)
  paste(c(
    if (nzchar(categorical)) paste0(statedu_t("meta.input_review.categorical_prefix", language), categorical),
    if (nzchar(continuous)) paste0(statedu_t("meta.input_review.continuous_prefix", language), continuous)
  ), collapse = " | ")
}

meta_effect_display_table <- function(effects, family, language = statedu_initial_language()) {
  rows <- effects[effects$family == family, , drop = FALSE]
  if (nrow(rows) == 0L) return(stats::setNames(data.frame(meta_ui_text("no_rows", language), check.names = FALSE), statedu_t("meta.input_column.message", language)))
  type_choices <- meta_input_type_choices(family, language)
  type_labels <- stats::setNames(names(type_choices), unname(type_choices))
  effect_label <- switch(family, g = "Hedges' g", r = "r", or = "OR")
  moderator_labels <- mapply(
    meta_input_moderator_display,
    rows$moderator_categorical,
    rows$moderator_continuous,
    MoreArgs = list(language = language),
    USE.NAMES = FALSE
  )
  result <- data.frame(
    Include = ifelse(rows$included, "✓", ""),
    ID = rows$study_id,
    Study = rows$study_name,
    Year = ifelse(is.finite(rows$publication_year), as.character(round(rows$publication_year)), ""),
    Outcome = rows$outcome,
    Predictor = rows$predictor,
    Moderators = moderator_labels,
    Format = unname(type_labels[rows$input_type]),
    `Reported values` = rows$source_summary,
    Effect = ifelse(is.finite(rows$display_effect), vapply(rows$display_effect, meta_format_number, character(1)), ""),
    `Analysis-scale SE` = ifelse(is.finite(rows$analysis_se), vapply(rows$analysis_se, meta_format_number, character(1)), ""),
    Status = vapply(rows$status, meta_status_label, character(1), language = language),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  columns <- c("include", "study_id", "study_name", "year", "outcome", "predictor", "moderators", "format", "values", "effect", "se", "status")
  names(result) <- vapply(columns, function(key) {
    if (key == "effect") effect_label else statedu_t(paste0("meta.input_column.", key), language)
  }, character(1))
  attr(result, "meta_row_ids") <- rows$row_id
  result
}

register_meta_server <- function(input, output, session, app_language_fn = NULL) {
  if (statedu_public_release()) return(invisible(NULL))
  language <- function() statedu_current_language(app_language_fn)
  effects <- reactiveVal(meta_empty_effects())
  next_id <- reactiveVal(1L)
  editing_id <- reactiveVal(NA_integer_)
  effect_modal_generation <- reactiveVal(0L)
  effect_fields_context <- NULL
  analysis_result <- analysis_scope_result_val(NULL)

  current_family <- reactive({
    family <- tolower(as.character(input$meta_target_family %||% "g"))
    if (!family %in% names(meta_effect_families())) "g" else family
  })

  current_family_effects <- reactive({
    rows <- effects()
    rows[rows$family == current_family(), , drop = FALSE]
  })

  selected_effect <- reactive({
    selected <- input$meta_effects_table_rows_selected
    rows <- current_family_effects()
    if (length(selected) != 1L || selected[[1]] < 1L || selected[[1]] > nrow(rows)) return(NULL)
    rows[selected[[1]], , drop = FALSE]
  })

  output$meta_supported_formats <- renderUI({
    statedu_current_language(app_language_fn)
    labels <- names(meta_input_type_choices(current_family(), language()))
    tags$ul(class = "sample-size-method-note", lapply(labels, tags$li))
  })

  output$meta_effects_table <- renderDT({
    statedu_current_language(app_language_fn)
    table <- meta_effect_display_table(effects(), current_family(), language())
    DT::datatable(
      table,
      rownames = FALSE,
      selection = "single",
      escape = TRUE,
      options = with_datatable_language(list(
        dom = "tip",
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = FALSE,
        ordering = FALSE
      ), language())
    )
  }, server = FALSE)

  output$meta_validation_summary <- renderUI({
    statedu_current_language(app_language_fn)
    summary <- meta_effect_summary(effects(), current_family())
    div(
      class = "sample-size-result-list meta-validation-summary",
      tags$p(strong(statedu_t("meta.input_summary.total", language())), summary$total),
      tags$p(strong(statedu_t("meta.input_summary.ready", language())), summary$valid),
      tags$p(strong(statedu_t("meta.input_summary.warnings", language())), summary$warnings),
      tags$p(strong(statedu_t("meta.input_summary.errors", language())), summary$errors)
    )
  })

  output$meta_moderator_selector <- renderUI({
    statedu_current_language(app_language_fn)
    catalog <- meta_moderator_catalog(effects(), current_family())
    if (nrow(catalog) == 0L) {
      return(div(
        class = "analysis-option-group meta-moderator-selector meta-moderator-selector-empty",
        div(class = "analysis-option-title", meta_ui_text("moderator_analysis", language())),
        div(class = "sample-size-method-note", meta_ui_text("moderator_unavailable", language()))
      ))
    }
    labels <- vapply(seq_len(nrow(catalog)), function(index) {
      name <- catalog$name[[index]]
      if (identical(name, "publication_year")) name <- statedu_t("meta.input_column.year", language())
      type <- statedu_t(paste0("meta.input_type.", if (identical(catalog$type[[index]], "categorical")) "categorical" else "continuous"), language())
      paste0(name, " [", type, ", k=", catalog$available[[index]], "]")
    }, character(1))
    choices <- c(stats::setNames("", meta_ui_text("no_moderator", language())), stats::setNames(catalog$key, labels))
    selected <- isolate(as.character(input$meta_moderator_selection %||% ""))
    if (!selected %in% unname(choices)) selected <- ""
    div(
      class = "analysis-option-group meta-moderator-selector",
      selectInput("meta_moderator_selection", meta_ui_text("moderator_analysis", language()), choices = choices, selected = selected)
    )
  })

  output$meta_validation_details <- renderUI({
    statedu_current_language(app_language_fn)
    rows <- current_family_effects()
    flagged <- rows[rows$status != "valid" | nzchar(rows$assumption), , drop = FALSE]
    if (nrow(flagged) == 0L) return(NULL)
    items <- lapply(seq_len(nrow(flagged)), function(index) {
      row <- flagged[index, , drop = FALSE]
      pieces <- c(row$message[[1]], row$assumption[[1]])
      detail <- paste(vapply(pieces[nzchar(pieces)], meta_input_detail_text, character(1), language = language()), collapse = " ")
      tags$li(
        strong(paste0(if (nzchar(row$study_id[[1]])) row$study_id[[1]] else statedu_t("meta.input_review.missing_id", language()), ": ")),
        detail
      )
    })
    div(class = "analysis-warning-panel", h4(statedu_t("meta.input_review.title", language())), tags$ul(items))
  })

  output$meta_analysis_results <- renderUI({
    statedu_current_language(app_language_fn)
    result <- analysis_result()
    if (is.null(result)) return(NULL)
    meta_analysis_results_ui(result, language())
  })

  output$meta_forest_plot <- renderPlot({
    result <- analysis_result()
    req(!is.null(result))
    draw_meta_forest_plot(result, "en")
  }, res = 110)

  output$meta_funnel_plot <- renderPlot({
    result <- analysis_result()
    req(!is.null(result), isTRUE(result$show_funnel))
    draw_meta_funnel_plot(result, "en")
  }, res = 110)

  output$meta_effect_modal_title <- renderText({
    meta_ui_text(if (is.finite(editing_id())) "edit_title" else "add_title", language())
  })
  output$meta_effect_modal_help <- renderText(meta_ui_text("moderator_help", language()))
  output$meta_effect_modal_cancel <- renderText(meta_ui_text("cancel", language()))
  observeEvent(language(), {
    lang <- language()
    for (field in c("study_id", "study_name", "outcome", "predictor",
                    "moderator_categorical", "moderator_continuous")) {
      placeholder <- switch(field,
        moderator_categorical = statedu_t("meta.dialog.categorical_example", lang),
        moderator_continuous = statedu_t("meta.dialog.continuous_example", lang), NULL)
      updateTextInput(session, paste0("meta_field_", field),
        label = meta_ui_text(field, lang), placeholder = placeholder)
    }
    updateNumericInput(session, "meta_field_publication_year", label = meta_ui_text("publication_year", lang))
    updateCheckboxInput(session, "meta_field_included", label = meta_ui_text("included", lang))
    updateSelectInput(session, "meta_input_type", label = meta_ui_text("format", lang),
      choices = meta_input_type_choices(current_family(), lang), selected = input$meta_input_type)
    updateSelectInput(session, "meta_field_direction", label = meta_ui_text("direction", lang),
      choices = stats::setNames(c("positive", "negative"), c(meta_ui_text("positive", lang), meta_ui_text("reverse", lang))),
      selected = input$meta_field_direction)
    updateActionButton(session, "meta_save_effect", label = meta_ui_text("save", lang))
  }, ignoreInit = TRUE)

  show_effect_modal <- function(record = NULL) {
    effect_modal_generation(isolate(effect_modal_generation()) + 1L)
    editing_id(if (!is.null(record) && nrow(record) > 0L) as.integer(record$row_id[[1]]) else NA_integer_)
    showModal(meta_effect_modal(current_family(), record, language()))
  }

  observeEvent(input$meta_add_effect, {
    show_effect_modal(NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$meta_edit_effect, {
    row <- selected_effect()
    if (is.null(row)) {
      showNotification(meta_ui_text("select_row", language()), type = "warning", duration = 4)
      return()
    }
    show_effect_modal(row)
  }, ignoreInit = TRUE)

  output$meta_effect_fields <- renderUI({
    req(input$meta_input_type)
    id <- editing_id()
    rows <- effects()
    record <- if (is.finite(id) && any(rows$row_id == id)) rows[rows$row_id == id, , drop = FALSE] else NULL
    if (!is.null(record) && !identical(as.character(record$input_type[[1]]), as.character(input$meta_input_type))) record <- NULL
    context <- list(generation = effect_modal_generation(), family = current_family(),
      type = as.character(input$meta_input_type), id = id)
    if (identical(context, effect_fields_context)) {
      # A language-only redraw must retain unsaved values, including cleared fields.
      fields <- meta_input_field_map(current_family())[[as.character(input$meta_input_type)]]
      draft <- isolate(lapply(fields, function(field) input[[paste0("meta_field_", field)]]))
      if (is.null(record)) record <- list()
      for (index in seq_along(fields)) {
        value <- draft[[index]]
        if (!is.null(value)) record[[fields[[index]]]] <- value
      }
    }
    effect_fields_context <<- context
    meta_effect_fields_ui(current_family(), as.character(input$meta_input_type), record, language())
  })

  observeEvent(input$meta_save_effect, {
    family <- current_family()
    id <- editing_id()
    is_edit <- is.finite(id)
    if (!is_edit) id <- next_id()
    record <- meta_normalize_effect(meta_values_from_input(input, family), row_id = id)
    rows <- effects()
    if (is_edit && any(rows$row_id == id)) {
      rows[rows$row_id == id, ] <- record[1, names(rows), drop = FALSE]
    } else {
      rows <- rbind(rows, record)
      next_id(as.integer(id) + 1L)
    }
    effects(rows)
    analysis_result(NULL)
    removeModal()
    if (identical(record$status[[1]], "error")) {
      showNotification(meta_input_detail_text(record$message[[1]], language()), type = "error", duration = 7)
    } else if (identical(record$status[[1]], "warning")) {
      showNotification(paste(meta_input_detail_text(record$message[[1]], language()), meta_input_detail_text(record$assumption[[1]], language())), type = "warning", duration = 7)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$meta_remove_effect, {
    row <- selected_effect()
    if (is.null(row)) {
      showNotification(meta_ui_text("select_row", language()), type = "warning", duration = 4)
      return()
    }
    rows <- effects()
    effects(rows[rows$row_id != row$row_id[[1]], , drop = FALSE])
    analysis_result(NULL)
  }, ignoreInit = TRUE)

  output$meta_reset_title <- renderText(statedu_t("meta.reset.title", language()))
  output$meta_reset_message <- renderText(statedu_t("meta.reset.confirm", language()))
  output$meta_reset_cancel <- renderText(meta_ui_text("cancel", language()))
  observeEvent(language(), {
    updateActionButton(session, "meta_confirm_reset", label = meta_ui_text("reset", language()))
  }, ignoreInit = TRUE)

  observeEvent(input$meta_reset_effects, {
    showModal(modalDialog(
      title = span(id = "meta_reset_title", class = "shiny-text-output", statedu_t("meta.reset.title", language())),
      span(id = "meta_reset_message", class = "shiny-text-output", statedu_t("meta.reset.confirm", language())),
      footer = tagList(
        modalButton(span(id = "meta_reset_cancel", class = "shiny-text-output", meta_ui_text("cancel", language()))),
        actionButton("meta_confirm_reset", meta_ui_text("reset", language()), class = "btn-danger")
      )
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$meta_confirm_reset, {
    effects(meta_empty_effects())
    next_id(1L)
    editing_id(NA_integer_)
    analysis_result(NULL)
    removeModal()
    showNotification(meta_ui_text("reset_done", language()), type = "message", duration = 4)
  }, ignoreInit = TRUE)

  observeEvent(input$meta_validate_effects, {
    effects(meta_revalidate_effects(effects(), current_family()))
    analysis_result(NULL)
    summary <- meta_effect_summary(effects(), current_family())
    type <- if (summary$errors > 0L) "error" else if (summary$warnings > 0L) "warning" else "message"
    message <- sprintf(statedu_t("meta.notice.validation_complete", language()),
      summary$valid, summary$warnings, summary$errors)
    showNotification(message, type = type, duration = 6)
  }, ignoreInit = TRUE)

  register_analysis_command_handler(
    "meta_run_analysis", input, output, session,
    states = list(effects = effects, next_id = next_id),
    dataset_fn = function() NULL, context_fn = function() NULL,
    run_fn = function() {
    result <- tryCatch(
      meta_fit_model(
        effects = effects(),
        family = current_family(),
        model = as.character(input$meta_model %||% "random"),
        tau_method = as.character(input$meta_tau_method %||% "REML"),
        conf_level = as.numeric(input$meta_conf_level %||% 0.95),
        prediction_interval = isTRUE(input$meta_prediction_interval)
      ),
      error = function(error) error
    )
    if (inherits(result, "error")) {
      analysis_result(NULL)
      showNotification(meta_model_error_text(result, language()), type = "error", duration = 8)
      return()
    }
    moderator_key <- as.character(input$meta_moderator_selection %||% "")
    group_mode <- as.character(input$meta_group_mode %||% "overall")
    dependency_method <- as.character(input$meta_dependency_method %||% "auto")
    rve_compare <- isTRUE(input$meta_rve_compare)
    result$grouped <- NULL
    if (!identical(group_mode, "overall")) {
      result$grouped <- meta_fit_grouped_models(
        effects(), current_family(), group_mode,
        model = result$model,
        tau_method = if (identical(result$model, "random")) result$tau_method else "REML",
        conf_level = result$conf_level,
        dependency_method = dependency_method,
        rve_compare = rve_compare
      )
    }
    dependency <- meta_dependency_summary(result$rows)
    result$dependency_summary <- dependency
    result$dependency_models <- list()
    result$dependency_sensitivity <- NULL
    result$leave_one_study_out <- NULL
    if (isTRUE(dependency$dependent)) {
      result$notes <- unique(c(
        result$notes,
        paste0(dependency$multi_effect_studies, " study/studies contribute multiple effects. A dependency-aware model or study-level sensitivity analysis is required.")
      ))
      requested_methods <- switch(
        dependency_method,
        auto = c("three_level", "rve"),
        both = c("three_level", "rve"),
        three_level = "three_level",
        rve = "rve",
        independent = character(0),
        c("three_level", "rve")
      )
      if ("three_level" %in% requested_methods) result$dependency_models$three_level <- tryCatch(meta_fit_three_level(result), error = function(error) error)
      if ("rve" %in% requested_methods) result$dependency_models <- c(result$dependency_models, meta_fit_rve_models(result, compare = rve_compare))
      sensitivity <- tryCatch(meta_dependency_sensitivity(result), error = function(error) error)
      if (inherits(sensitivity, "error")) {
        result$notes <- unique(c(result$notes, paste0("Dependency sensitivity analysis was unavailable: ", conditionMessage(sensitivity))))
      } else {
        result$dependency_sensitivity <- sensitivity
      }
      leave_one_out <- tryCatch(meta_leave_one_study_out(result, rho = 0.5), error = function(error) error)
      if (inherits(leave_one_out, "error")) {
        result$notes <- unique(c(result$notes, paste0("Leave-one-study-out analysis was unavailable: ", conditionMessage(leave_one_out))))
      } else {
        result$leave_one_study_out <- leave_one_out
      }
      if (identical(dependency_method, "independent")) {
        result$notes <- unique(c(result$notes, "Multiple effects were treated as independent by user selection; standard errors may be too small."))
      }
    }
    result$moderator <- NULL
    if (nzchar(moderator_key)) {
      moderator <- tryCatch(meta_fit_moderator(result, moderator_key, rve_compare = rve_compare), error = function(error) error)
      if (inherits(moderator, "error")) {
        analysis_result(NULL)
        showNotification(meta_model_error_text(moderator, language()), type = "error", duration = 8)
        return()
      }
      result$moderator <- moderator
      if (inherits(moderator$cr2, "error")) {
        result$notes <- unique(c(result$notes, paste0("Moderator CR2 inference was unavailable: ", conditionMessage(moderator$cr2))))
      } else if (!is.null(moderator$cr2) && isTRUE(moderator$cr2$small_sample_warning)) {
        result$notes <- unique(c(result$notes, "Moderator CR2 inference has few independent studies or low Satterthwaite degrees of freedom; interpret it cautiously."))
      }
      if (isTRUE(rve_compare) && length(moderator$robust_models) > 1L) {
        result$notes <- unique(c(result$notes, "CR1 and CR3 are sensitivity comparisons; CR2 remains the primary cluster-robust result."))
      }
      if (moderator$omitted > 0L) {
        result$notes <- unique(c(
          result$notes,
          paste0(moderator$omitted, " included study row(s) were omitted from moderator analysis because the selected moderator was missing.")
        ))
      }
    }
    result$show_funnel <- isTRUE(input$meta_funnel_plot_enabled)
    result$show_egger <- isTRUE(input$meta_egger_test_enabled)
    result$egger <- if (isTRUE(result$show_egger)) meta_egger_test(result) else NULL
    result$trimfill <- NULL
    if (isTRUE(input$meta_trimfill_enabled)) {
      trimfill <- tryCatch(meta_trimfill(result, rho = 0.5), error = function(error) error)
      if (inherits(trimfill, "error")) {
        result$notes <- unique(c(result$notes, paste0("Trim-and-fill was unavailable: ", conditionMessage(trimfill))))
      } else {
        result$trimfill <- trimfill
        result$notes <- unique(c(result$notes, "Trim-and-fill is a sensitivity analysis for one assumed selection mechanism, not a definitive correction for publication bias."))
      }
    }
    if ((isTRUE(result$show_funnel) || isTRUE(result$show_egger)) && result$k < 10L) {
      result$notes <- unique(c(
        result$notes,
        "Funnel-plot asymmetry diagnostics generally require at least 10 studies. Interpret these results cautiously."
      ))
    }
    if (isTRUE(result$show_egger) && !isTRUE(result$egger$available) && nzchar(result$egger$message)) {
      result$notes <- unique(c(result$notes, result$egger$message))
    }
    analysis_result(result)
    showNotification(statedu_t("meta.notice.analysis_complete", language()), type = "message", duration = 5)
  }, ignoreInit = TRUE)

  observeEvent(
    list(
      input$meta_target_family, input$meta_model, input$meta_tau_method,
      input$meta_conf_level, input$meta_prediction_interval,
      input$meta_funnel_plot_enabled, input$meta_egger_test_enabled,
      input$meta_moderator_selection, input$meta_group_mode,
      input$meta_dependency_method, input$meta_rve_compare, input$meta_trimfill_enabled
    ),
    analysis_result(NULL),
    ignoreInit = TRUE
  )

  observeEvent(input$meta_import_file, {
    file <- input$meta_import_file
    if (is.null(file) || !nzchar(file$datapath %||% "")) return()
    imported <- tryCatch({
      data <- meta_read_effect_input_file(file, current_family())
      meta_import_effects(data, current_family(), start_id = next_id())
    }, error = function(error) error)
    if (inherits(imported, "error")) {
      showNotification(paste(meta_ui_text("invalid_file", language()), meta_import_error_text(imported, language())), type = "error", duration = 8)
      return()
    }
    if (nrow(imported) == 0L) {
      showNotification(meta_ui_text("invalid_file", language()), type = "warning", duration = 5)
      return()
    }
    effects(rbind(effects(), imported))
    analysis_result(NULL)
    next_id(max(imported$row_id, na.rm = TRUE) + 1L)
    showNotification(paste(nrow(imported), meta_ui_text("imported", language())), type = "message", duration = 5)
  }, ignoreInit = TRUE)

  output$meta_download_template <- downloadHandler(
    filename = function() meta_excel_template_filename(current_family()),
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content = function(file) {
      template <- meta_excel_template_asset(current_family())
      if (!file.exists(template)) stop("The Excel template asset is missing.", call. = FALSE)
      if (!file.copy(template, file, overwrite = TRUE)) stop("The Excel template could not be copied.", call. = FALSE)
    }
  )

  output$meta_download_effects <- downloadHandler(
    filename = function() paste0("statedu_meta_", current_family(), "_entered_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) {
      utils::write.csv(
        meta_export_effects(effects(), current_family()),
        file,
        row.names = FALSE,
        na = "",
        fileEncoding = "UTF-8"
      )
    }
  )

  invisible(list(effects = effects, current_family = current_family, analysis_result = analysis_result))
}
