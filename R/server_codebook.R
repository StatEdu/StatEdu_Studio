# Shiny server integration for applying an Excel coding book after data import.

codebook_preview_datatable <- function(match_result, language = statedu_initial_language()) {
  variables <- match_result$variables
  table <- data.frame(
    Variable = variables$변수명,
    Label = variables$변수라벨,
    Type = variables$변수유형,
    `Value labels` = variables$value_label_count,
    Status = variables$status,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(table) <- if (identical(normalize_app_language(language), "ko")) {
    c("변수명", "변수라벨", "변수유형", "값라벨 수", "연결 상태")
  } else {
    names(table)
  }
  DT::datatable(
    table,
    rownames = FALSE,
    selection = "none",
    options = with_datatable_language(list(dom = "tip", pageLength = 10, scrollX = TRUE), language)
  )
}

register_codebook_handlers <- function(
  input,
  output,
  session,
  current_data_file_fn,
  variable_info_table_fn,
  measurement_overrides,
  var_label_overrides,
  category_label_values,
  step3_variable_info,
  selected_names_fn,
  selection_applied_fn,
  go_data_step_fn,
  mark_settings_dirty,
  app_language_fn = NULL
) {
  preview <- reactiveVal(NULL)
  applied <- reactiveVal(NULL)
  undo_snapshot <- reactiveVal(NULL)

  language_value <- function() {
    if (is.function(app_language_fn)) app_language_fn() else statedu_initial_language()
  }

  output$codebook_status <- renderUI({
    status <- applied()
    if (is.null(status)) return(NULL)
    language <- language_value()
    div(
      class = "codebook-applied-status",
      div(
        sprintf(statedu_localized_text(language, "Applied: %s (%s variables)", "적용됨: %s (변수 %s개)"), status$file_name, status$matched_count),
        class = "step-summary-detail"
      ),
      actionButton(
        "undo_codebook_apply",
        statedu_localized_text(language, "Undo coding book", "코딩북 적용 취소"),
        class = "btn btn-default btn-sm"
      )
    )
  })

  output$codebook_preview_table <- DT::renderDT({
    req(preview())
    codebook_preview_datatable(preview(), language_value())
  })

  output$codebook_preview_warnings <- renderUI({
    result <- preview()
    if (is.null(result) || length(result$warnings) == 0) return(NULL)
    tags$div(
      class = "codebook-warning-list",
      tags$strong(statedu_localized_text(language_value(), "Warnings", "확인 사항")),
      tags$ul(lapply(result$warnings, tags$li))
    )
  })

  observeEvent(input$codebook_file, {
    req(!is.null(current_data_file_fn()))
    file <- input$codebook_file
    req(!is.null(file$datapath), nzchar(file$datapath))
    language <- language_value()
    result <- tryCatch(
      {
        info <- variable_info_table_fn(reactive_labels = FALSE)
        match_codebook_to_variables(
          read_codebook_excel(file$datapath, file$name),
          info
        )
      },
      error = function(error) error
    )
    if (inherits(result, "error")) {
      showNotification(conditionMessage(result), type = "error", duration = 10)
      return()
    }
    preview(result)
    showModal(modalDialog(
      title = statedu_localized_text(language, "Apply coding book", "코딩북 적용"),
      tags$p(sprintf(statedu_localized_text(language,
        "%s variables matched; %s coding-book variables were not found in the data.",
        "변수 %s개가 연결되었고, 코딩북 변수 %s개는 데이터에서 찾지 못했습니다."),
        length(result$matched_names), length(result$codebook_only_names))),
      DTOutput("codebook_preview_table"),
      uiOutput("codebook_preview_warnings"),
      radioButtons(
        "codebook_conflict_policy",
        statedu_localized_text(language, "When existing labels conflict", "기존 라벨과 충돌할 때"),
        choices = stats::setNames(
          c("codebook", "existing"),
          c(
            statedu_localized_text(language, "Use the coding book", "코딩북 우선"),
            statedu_localized_text(language, "Keep existing metadata", "기존 메타데이터 유지")
          )
        ),
        selected = "codebook",
        inline = TRUE
      ),
      footer = tagList(
        modalButton(statedu_localized_text(language, "Cancel", "취소")),
        actionButton(
          "apply_codebook",
          statedu_localized_text(language, "Apply", "적용"),
          class = "btn btn-primary",
          disabled = if (length(result$matched_names) == 0) "disabled" else NULL
        )
      ),
      size = "l",
      easyClose = FALSE
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$apply_codebook, {
    result <- preview()
    req(!is.null(result), length(result$matched_names) > 0)
    overwrite <- identical(input$codebook_conflict_policy %||% "codebook", "codebook")
    info <- variable_info_table_fn(reactive_labels = FALSE)

    undo_snapshot(list(
      measurements = measurement_overrides(),
      var_labels = var_label_overrides(),
      categories = category_label_values(),
      step3_info = step3_variable_info()
    ))

    variables <- result$variables[nzchar(result$variables$matched_name), , drop = FALSE]
    measurements <- stats::setNames(as.character(variables$measurement), variables$matched_name)
    labels <- stats::setNames(as.character(variables$변수라벨), variables$matched_name)
    labels <- labels[nzchar(trimws(labels))]

    current_measurements <- measurement_overrides()
    current_labels <- var_label_overrides()
    if (isTRUE(overwrite)) {
      current_measurements[names(measurements)] <- measurements
      current_labels[names(labels)] <- labels
    } else {
      info_measurements <- stats::setNames(as.character(info$measurement), as.character(info$name))
      info_labels <- stats::setNames(as.character(info$var_label), as.character(info$name))
      missing_measurement <- !names(measurements) %in% names(info_measurements) | !nzchar(info_measurements[names(measurements)])
      missing_label <- !names(labels) %in% names(info_labels) | !nzchar(trimws(info_labels[names(labels)]))
      current_measurements[names(measurements)[missing_measurement]] <- measurements[missing_measurement]
      current_labels[names(labels)[missing_label]] <- labels[missing_label]
    }
    measurement_overrides(clean_measurement_overrides(current_measurements))
    var_label_overrides(clean_var_label_overrides(current_labels))
    category_label_values(codebook_category_table(
      category_label_values(),
      info,
      result,
      overwrite = overwrite
    ))

    updated_info <- apply_variable_overrides(info, measurement_overrides(), var_label_overrides())
    if (!is.null(step3_variable_info())) {
      selected <- selected_names_fn()
      step3_variable_info(updated_info[updated_info$name %in% selected, , drop = FALSE])
    }
    applied(list(
      file_name = input$codebook_file$name %||% "coding book",
      matched_count = length(result$matched_names)
    ))
    mark_settings_dirty()
    removeModal()
    if (isTRUE(selection_applied_fn())) {
      go_data_step_fn("step3", "labels")
    } else {
      go_data_step_fn("step2", "info")
    }
    showNotification(
      sprintf(statedu_localized_text(language_value(), "Coding book applied to %s variables.", "변수 %s개에 코딩북을 적용했습니다."), length(result$matched_names)),
      type = "message",
      duration = 5
    )
  }, ignoreInit = TRUE)

  observeEvent(input$undo_codebook_apply, {
    snapshot <- undo_snapshot()
    if (is.null(snapshot)) return()
    measurement_overrides(snapshot$measurements)
    var_label_overrides(snapshot$var_labels)
    category_label_values(snapshot$categories)
    step3_variable_info(snapshot$step3_info)
    undo_snapshot(NULL)
    applied(NULL)
    preview(NULL)
    mark_settings_dirty()
    showNotification(
      statedu_localized_text(language_value(), "Coding-book changes were undone.", "코딩북 적용을 취소했습니다."),
      type = "message",
      duration = 4
    )
  }, ignoreInit = TRUE)

  observeEvent(current_data_file_fn(), {
    preview(NULL)
    applied(NULL)
    undo_snapshot(NULL)
  }, ignoreInit = TRUE)

  invisible(list(preview = preview, applied = applied))
}
