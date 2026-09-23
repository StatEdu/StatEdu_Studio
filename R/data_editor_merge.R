# Data merge command for the Data Editor menu.

merge_ko <- function(hex) statedu_utf8(hex)

merge_text <- function(language, en, ko = en) {
  statedu_localized_text(language, en, ko)
}

merge_abort <- function(key, ...) {
  values <- list(...)
  message <- do.call(sprintf, c(list(statedu_t(paste0("merge.error.", key), "en")), values))
  stop(structure(list(message = message, call = NULL, key = key, values = values),
                 class = c("statedu_merge_error", "error", "condition")))
}

merge_error_text <- function(error, language) {
  if (!inherits(error, "statedu_merge_error")) return(conditionMessage(error))
  do.call(sprintf, c(list(statedu_t(paste0("merge.error.", error$key), language)), error$values))
}

merge_clean_name <- function(value, fallback = "merged_data") {
  value <- trimws(as.character(value %||% ""))
  if (length(value) == 0 || !nzchar(value[[1]])) value <- fallback
  make.names(value[[1]], unique = FALSE)
}

merge_parse_indicator_values <- function(text, count) {
  count <- max(0L, as.integer(count %||% 0L))
  if (count == 0L) return(character(0))
  values <- trimws(unlist(strsplit(as.character(text %||% ""), "[,\r\n]+")))
  values <- values[nzchar(values)]
  if (length(values) < count) {
    values <- c(values, as.character(seq.int(length(values) + 1L, count)))
  }
  values[seq_len(count)]
}

merge_uploaded_files <- function(uploaded, input, require_at_least = 1L) {
  if (is.null(uploaded) || nrow(uploaded) < require_at_least) {
    merge_abort("files", require_at_least)
  }
  rows <- lapply(seq_len(nrow(uploaded)), function(index) {
    file <- uploaded[index, , drop = FALSE]
    data <- read_input_data(
      path = file$datapath[[1]],
      original_name = file$name[[1]],
      csv_header = isTRUE(input$merge_csv_header %||% TRUE),
      dat_delimiter = input$merge_dat_delimiter %||% "whitespace",
      dat_has_names = isTRUE(input$merge_dat_has_names)
    )
    as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  })
  names(rows) <- as.character(uploaded$name)
  rows
}

merge_add_variables <- function(files, key, join_type = "left") {
  if (length(files) < 2L) merge_abort("variable_files")
  key <- trimws(as.character(key %||% ""))
  if (!nzchar(key)) merge_abort("enter_id")
  join_type <- as.character(join_type %||% "left")[[1]]
  if (!join_type %in% c("left", "inner", "full")) join_type <- "left"

  for (index in seq_along(files)) {
    if (!key %in% names(files[[index]])) {
      merge_abort("missing_id", index, key)
    }
    ids <- as.character(files[[index]][[key]])
    ids <- ids[!is.na(ids) & nzchar(ids)]
    if (any(duplicated(ids))) {
      merge_abort("duplicate_id", index)
    }
  }

  result <- files[[1]]
  for (index in seq.int(2L, length(files))) {
    next_data <- files[[index]]
    overlap <- setdiff(intersect(names(result), names(next_data)), key)
    if (length(overlap) > 0) {
      replacement <- make.unique(c(names(result), overlap), sep = "_")
      names(next_data)[match(overlap, names(next_data))] <- utils::tail(replacement, length(overlap))
    }
    result <- merge(
      result,
      next_data,
      by = key,
      all.x = join_type %in% c("left", "full"),
      all.y = identical(join_type, "full"),
      sort = FALSE
    )
  }
  rownames(result) <- NULL
  result
}

merge_common_columns <- function(files) {
  Reduce(intersect, lapply(files, names))
}

merge_add_cases <- function(files, selected_variables, indicator_name = "time", indicator_values = "") {
  if (length(files) < 2L) merge_abort("case_files")
  common <- merge_common_columns(files)
  selected_variables <- intersect(as.character(selected_variables %||% character(0)), common)
  if (length(selected_variables) == 0) selected_variables <- common
  if (length(selected_variables) == 0) {
    merge_abort("no_common")
  }
  indicator_name <- merge_clean_name(indicator_name, "time")
  if (indicator_name %in% selected_variables) {
    indicator_name <- make.unique(c(selected_variables, indicator_name), sep = "_")[[length(selected_variables) + 1L]]
  }
  values <- merge_parse_indicator_values(indicator_values, length(files))
  rows <- lapply(seq_along(files), function(index) {
    data <- files[[index]][, selected_variables, drop = FALSE]
    data[[indicator_name]] <- values[[index]]
    data
  })
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

merge_summary_table <- function(files, language = statedu_initial_language()) {
  if (length(files) == 0) return(setNames(data.frame(statedu_t("merge.ui.no_files", language), check.names = FALSE), statedu_t("merge.ui.message", language)))
  result <- data.frame(
    File = names(files),
    Rows = vapply(files, nrow, integer(1)),
    Columns = vapply(files, ncol, integer(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(result) <- vapply(c("file", "rows_header", "columns"), function(key) statedu_t(paste0("merge.ui.", key), language), character(1))
  result
}

data_editor_merge_panel <- function(language = statedu_initial_language(), values = list()) {
  language <- normalize_app_language(language)
  div(
    class = "page-shell",
    div(
      class = "app-heading",
      h1(statedu_t("merge.ui.title", language)),
      div(statedu_t("merge.ui.subtitle", language), class = "app-subtitle")
    ),
    div(
      class = "workspace-panel frequencies-workspace-panel data-editor-workspace",
      analysis_workspace_heading(statedu_t("merge.ui.heading", language), "merge", language = language),
      analysis_workspace_body(
        "merge",
        div(
          class = "data-editor-simple-setup merge-setup-grid",
          div(
            class = "analysis-options-panel merge-file-panel",
            fileInput(
              "merge_files",
              statedu_t("merge.ui.files", language),
              multiple = TRUE,
              buttonLabel = statedu_t("merge.ui.choose", language),
              placeholder = if (!is.null(values$merge_files)) paste(values$merge_files$name, collapse = ", ") else statedu_t("merge.ui.no_files", language),
              accept = c(".sav", ".sas7bdat", ".xpt", ".dta", ".csv", ".dat", ".xlsx", ".xls"),
              width = "100%"
            ),
            div(
              class = "merge-checkbox-stack",
              checkboxInput("merge_csv_header", statedu_t("merge.ui.csv_header", language), value = values$merge_csv_header %||% TRUE),
              checkboxInput("merge_dat_has_names", statedu_t("merge.ui.dat_header", language), value = values$merge_dat_has_names %||% FALSE)
            ),
            selectInput(
              "merge_dat_delimiter",
              statedu_t("merge.ui.delimiter", language),
              choices = stats::setNames(c("whitespace", "tab", "comma"), vapply(c("whitespace", "tab", "comma"), function(key) statedu_t(paste0("merge.ui.", key), language), character(1))),
              selected = values$merge_dat_delimiter %||% "whitespace",
              selectize = FALSE,
              width = "100%"
            )
          ),
          div(
            class = "analysis-options-panel merge-mode-panel",
            tabsetPanel(
              id = "merge_mode",
              type = "tabs",
              selected = values$merge_mode %||% "variables",
              tabPanel(
                statedu_t("merge.ui.variables", language),
                value = "variables",
                div(
                  class = "factor-options-tab-content merge-options-tab-content",
                  textInput("merge_id_variable", statedu_t("merge.ui.id", language), value = values$merge_id_variable %||% "id", width = "100%"),
                  radioButtons(
                    "merge_join_type",
                    statedu_t("merge.ui.rows", language),
                    choices = stats::setNames(
                      c("left", "inner", "full"),
                      c(
                        statedu_t("merge.ui.left", language),
                        statedu_t("merge.ui.inner", language),
                        statedu_t("merge.ui.full", language)
                      )
                    ),
                    selected = values$merge_join_type %||% "left"
                  )
                )
              ),
              tabPanel(
                statedu_t("merge.ui.cases", language),
                value = "cases",
                div(
                  class = "factor-options-tab-content merge-options-tab-content",
                  uiOutput("merge_case_variables_ui"),
                  div(
                    class = "merge-two-column",
                    textInput("merge_indicator_name", statedu_t("merge.ui.indicator_name", language), value = values$merge_indicator_name %||% "time", width = "100%"),
                    textInput("merge_indicator_values", statedu_t("merge.ui.indicator_values", language), value = values$merge_indicator_values %||% "", width = "100%", placeholder = "1, 2, 3 / 2, 4, 6, 10")
                  )
                )
              )
            )
          )
        ),
        div(
          class = "analysis-action-row data-editor-simple-action-row merge-action-row",
          actionButton("preview_merge_data", analysis_ui_text("Preview", language), class = "btn btn-default"),
          actionButton("run_merge_data", analysis_ui_text("Run", language), class = "btn btn-primary")
        ),
        uiOutput("merge_data_message"),
        div(class = "data-editor-result-output", DT::DTOutput("merge_data_preview"))
      )
    )
  )
}

register_merge_handlers <- function(input, output, session, replace_dataset_fn, mark_settings_dirty, language_fn = NULL) {
  saved <- reactiveValues()
  for (field in c("merge_files", "merge_csv_header", "merge_dat_has_names", "merge_dat_delimiter", "merge_mode", "merge_id_variable", "merge_join_type", "merge_indicator_name", "merge_indicator_values", "merge_case_variables")) local({
    field_name <- field
    observeEvent(input[[field_name]], {
      saved[[field_name]] <- input[[field_name]]
      session$userData$merge_ui_values <- reactiveValuesToList(saved)
    }, ignoreNULL = TRUE, priority = 100)
  })
  loaded_files <- reactive({
    merge_uploaded_files(saved$merge_files, list(merge_csv_header = saved$merge_csv_header, merge_dat_has_names = saved$merge_dat_has_names, merge_dat_delimiter = saved$merge_dat_delimiter), require_at_least = 1L)
  })
  preview_data <- reactiveVal(NULL)
  last_message <- reactiveVal(NULL)

  # A preview belongs to its input files and settings, not to the UI language.
  observeEvent(reactiveValuesToList(saved), {
    preview_data(NULL)
    last_message(NULL)
  }, priority = 90)

  output$merge_case_variables_ui <- renderUI({
    language <- statedu_current_language(language_fn)
    files <- tryCatch(loaded_files(), error = function(e) list())
    common <- if (length(files) > 0) merge_common_columns(files) else character(0)
    if (length(common) == 0) {
      return(div(class = "empty-message", statedu_t("merge.ui.common", language)))
    }
    selectInput(
      "merge_case_variables",
      statedu_t("merge.ui.keep", language),
      choices = stats::setNames(common, common),
      selected = intersect(saved$merge_case_variables %||% common, common),
      multiple = TRUE,
      width = "100%"
    )
  })

  build_merge_result <- function() {
    files <- loaded_files()
    mode <- as.character(input$merge_mode %||% "variables")[[1]]
    if (identical(mode, "cases")) {
      merge_add_cases(
        files = files,
        selected_variables = input$merge_case_variables %||% character(0),
        indicator_name = input$merge_indicator_name %||% "time",
        indicator_values = input$merge_indicator_values %||% ""
      )
    } else {
      merge_add_variables(
        files = files,
        key = input$merge_id_variable %||% "id",
        join_type = input$merge_join_type %||% "left"
      )
    }
  }

  observeEvent(input$preview_merge_data, {
    language <- statedu_current_language(language_fn)
    result <- tryCatch(build_merge_result(), error = function(e) {
      showNotification(merge_error_text(e, language), type = "warning", duration = 7)
      NULL
    })
    if (is.null(result)) return()
    preview_data(result)
    last_message(list(key = "merge.ui.preview", rows = nrow(result), columns = ncol(result)))
  }, ignoreInit = TRUE)

  observeEvent(input$run_merge_data, {
    language <- statedu_current_language(language_fn)
    if (!is.function(replace_dataset_fn)) {
      showNotification(statedu_t("merge.error.replacement", language), type = "warning", duration = 5)
      return()
    }
    result <- tryCatch(build_merge_result(), error = function(e) {
      showNotification(merge_error_text(e, language), type = "warning", duration = 7)
      NULL
    })
    if (is.null(result)) return()
    target_name <- if (identical(as.character(input$merge_mode %||% "variables")[[1]], "cases")) "merged_cases.csv" else "merged_variables.csv"
    ok <- replace_dataset_fn(result, name = target_name, path = NULL, csv_header = TRUE)
    if (isTRUE(ok)) {
      preview_data(result)
      last_message(list(key = "merge.ui.loaded", rows = nrow(result), columns = ncol(result)))
      if (is.function(mark_settings_dirty)) mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  output$merge_data_message <- renderUI({
    message <- last_message()
    if (is.null(message)) return(NULL)
    div(class = "recode-same-status", sprintf(statedu_t(message$key, statedu_current_language(language_fn)), message$rows, message$columns))
  })

  output$merge_data_preview <- DT::renderDT({
    language <- statedu_current_language(language_fn)
    options <- with_datatable_language(list(pageLength = 10, lengthChange = FALSE, scrollX = TRUE), language)
    result <- preview_data()
    if (is.null(result)) {
      files <- tryCatch(loaded_files(), error = function(e) list())
      return(DT::datatable(merge_summary_table(files, language), rownames = FALSE, options = options))
    }
    DT::datatable(utils::head(result, 50), rownames = FALSE, options = options)
  })

  invisible(TRUE)
}
