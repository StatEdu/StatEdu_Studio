# Non-destructive row selection and sequential, snapshot-based split analyses.
analysis_scope_text <- function(language, ko, en) {
  if (identical(normalize_app_language(language), "ko")) return(ko)
  key <- gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", tolower(trimws(en))))
  statedu_t(paste0("analysis.ui.", key), language, en)
}

# The row-scope columns remain in the source data, but are not analysis terms.
# Carry this contract on the data so asynchronous workers receive it as well.
analysis_scope_excluded <- function(data) attr(data, "statedu_scope_excluded", exact=TRUE) %||% character()

analysis_scope_without <- function(value, excluded) {
  if (is.list(value)) return(lapply(value, analysis_scope_without, excluded=excluded))
  if (is.character(value)) return(value[!value %in% excluded])
  value
}

analysis_scope_prepare_variables <- function(data, frame, arguments) {
  excluded <- analysis_scope_excluded(data)
  if (!length(excluded)) return(invisible(NULL))
  for (argument in arguments) {
    value <- get(argument, envir=frame, inherits=FALSE)
    assign(argument, analysis_scope_without(value, excluded), envir=frame)
  }
  invisible(NULL)
}

analysis_scope_model_snapshot <- function(snapshot, data) {
  excluded <- analysis_scope_excluded(data)
  if (!length(excluded)) return(snapshot)
  nodes <- custom_model_canvas_records(snapshot$nodes)
  removed <- vapply(Filter(function(node) !identical(node$role,"latent") &&
    custom_model_canvas_record_value(node,"variableId",custom_model_canvas_record_value(node,"name","")) %in% excluded,
    nodes), custom_model_canvas_record_value, character(1), key="id")
  # Remove the residual nodes belonging to omitted indicators as well.
  edges <- custom_model_canvas_records(snapshot$edges)
  residual_ids <- vapply(Filter(function(node) node$role %in% c("error","disturbance"),nodes),custom_model_canvas_record_value,character(1),key="id")
  for (edge in edges) {
    if (edge$to %in% removed && edge$from %in% residual_ids) removed <- union(removed,edge$from)
  }
  snapshot$nodes <- Filter(function(node) !node$id %in% removed, nodes)
  # Canvas collections must remain JSON arrays, including a single covariate.
  snapshot$covariates <- as.list(setdiff(as.character(snapshot$covariates %||% character()),excluded))
  snapshot$edges <- Filter(function(edge) !any(c(edge$from,edge$to) %in% removed),custom_model_canvas_records(snapshot$edges))
  edge_ids <- vapply(snapshot$edges,custom_model_canvas_record_value,character(1),key="id")
  snapshot$moderations <- Filter(function(link) !link$from %in% removed && link$toEdge %in% edge_ids,custom_model_canvas_records(snapshot$moderations))
  snapshot
}

analysis_scope_panel <- function(kind, language) {
  cases <- identical(kind, "cases")
  title <- analysis_scope_text(language, if (cases) "케이스 선택" else "파일 분할", if (cases) "Select cases" else "Split file")
  div(class = "page-shell scope-page", h2(title), div(class = "workspace-panel scope-workspace",
    p(analysis_scope_text(language,
      if (cases) "조건에 맞는 케이스만 이후 분석에 사용합니다. 원본 데이터는 유지됩니다." else "선택한 변수의 범주·범위별로 동일한 분석을 각각 실행합니다. 케이스 선택 조건을 먼저 적용합니다.",
      if (cases) "Use cases matching the conditions in subsequent analyses. The source data is preserved." else "Run the same analysis for each category or range, after applying case selection.")),
    uiOutput(paste0("scope_", kind, "_controls"))))
}

analysis_scope_filter <- function(data, selection) {
  if (!length(selection$variable)) return(data)
  if (!selection$variable %in% names(data)) stop("케이스 선택 변수가 없습니다. 조건을 다시 설정하세요. / Case-selection variable is missing.")
  value <- data[[selection$variable]]
  keep <- if(length(selection$rules)) Reduce(`|`,lapply(selection$rules,function(rule)analysis_scope_rule_mask(value,rule))) else !is.na(value) & as.character(value) %in% selection$values
  data[keep, , drop = FALSE]
}

analysis_scope_groups <- function(data, variable, label_fn = identity) {
  if (!length(variable)) return(list(list(label = "", data = data)))
  if (!variable %in% names(data)) stop("파일 분할 변수가 없습니다. / Split variable is missing.")
  values <- unique(as.character(data[[variable]][!is.na(data[[variable]])]))
  lapply(values, function(value) list(label = paste0(variable, ": ", label_fn(value)),
    data = data[!is.na(data[[variable]]) & as.character(data[[variable]]) == value, , drop = FALSE]))
}

analysis_scope_result_val <- function(value = NULL, job = FALSE) {
  val <- shiny::reactiveVal(value)
  session <- shiny::getDefaultReactiveDomain()
  if (job && !is.null(session)) {
    session$userData$scope_jobs <- c(session$userData$scope_jobs %||% list(), list(val))
  }
  function(value) {
    if (missing(value)) return(val())
    # A new run must replace a previously mounted split snapshot even when its
    # numerical result happens to be identical to the last group's result.
    if (!job && !is.null(value) && !is.null(session) && isTRUE(session$userData$scope_force_result)) {
      val(NULL); session$userData$scope_force_result <- FALSE
    }
    val(value)
    if (!job && !is.null(value) && !is.null(session) && is.function(session$userData$scope_commit)) session$userData$scope_commit(value)
    invisible(value)
  }
}

analysis_scope_output_id <- function(run_id) {
  map <- c(run = "regression", run_hierarchical = "hierarchical", run_factor_analysis = "factor_analysis",
    run_one_group_rm_anova = "one_group_rm_anova", meta_run_analysis = "meta_analysis",
    complex_freq_run = "complex_freq", complex_crosstab_run = "complex_crosstab",
    complex_ttest_run = "complex_ttest", complex_correlation_run = "complex_correlation",
    complex_regression_run = "complex_regression", complex_logistic_run = "complex_logistic")
  prefix <- if (run_id %in% names(map)) unname(map[[run_id]]) else sub("^run_", "", run_id)
  paste0(prefix, "_results")
}

analysis_scope_run <- function(session, run_id, run_fn, output_id = analysis_scope_output_id(run_id), canvas_root_id = NULL) {
  if (is.null(session$userData$scope_run)) return(run_fn())
  session$userData$scope_run(run_fn, output_id, canvas_root_id)
}

# R/Shiny evaluates one handler at a time. Rebind its input frames only for
# synchronous preparation; restore them before returning to the event loop.
# Background workers receive the arguments prepared under these frozen inputs.
analysis_scope_with_inputs <- function(run_fn, values, app_frame = NULL) {
  frozen <- do.call(shiny::reactiveValues, values)
  frames <- list(); frame <- environment(run_fn)
  while (!is.null(frame) && !identical(frame, globalenv()) && !identical(frame, emptyenv())) {
    if (exists("input", frame, inherits = FALSE) && !bindingIsLocked("input", frame)) frames <- c(frames, list(frame))
    frame <- parent.env(frame)
  }
  if (!is.null(app_frame) && !any(vapply(frames, identical, logical(1), app_frame))) frames <- c(frames, list(app_frame))
  original <- lapply(frames, function(frame) get("input", frame, inherits = FALSE))
  on.exit(for (index in seq_along(frames)) assign("input", original[[index]], frames[[index]]), add = TRUE)
  for (frame in frames) assign("input", frozen, frame)
  run_fn()
}

analysis_scope_localize_one_group_snapshot <- function(html, result, language) {
  if (!isTRUE(result$type %in% c("one_group_rm_anova", "scope_regression", "scope_renderer")) || !is.null(result$error)) return(html)
  previous <- options(statedu.app_language = language)
  on.exit(options(previous), add = TRUE)
  original <- xml2::read_html(html, encoding = "UTF-8")
  rendered <- if (is.function(result$render)) result$render() else one_group_rm_anova_results_ui(result)
  translated <- xml2::read_html(as.character(rendered), encoding = "UTF-8")
  sections <- function(doc) xml2::xml_find_all(doc, "//table[@data-result-table-role='appendix']/ancestor::div[contains(concat(' ',normalize-space(@class),' '),' result-section ')][1]")
  old <- sections(original); new <- sections(translated)
  if (!length(old) || length(old) != length(new)) return(html)
  for (i in seq_along(old)) {
    if (length(xml2::xml_find_all(old[[i]], ".//table[@data-result-table-role='main']"))) return(html)
  }
  for (i in seq_along(old)) xml2::xml_replace(old[[i]], new[[i]])
  paste(vapply(xml2::xml_children(xml2::xml_find_first(original, "//body")), as.character, character(1)), collapse = "\n")
}

register_analysis_scope <- function(input, output, session, dataset_fn, language_fn, file_fn,
                                    labels_fn = function() character(), category_table_fn = function() NULL,
                                    variable_info_fn = function() NULL) {
  selection <- shiny::reactiveVal(list(variable = character(), values = character()))
  split <- shiny::reactiveVal(character())
  split_rules <- shiny::reactiveVal(list())
  configured_groups <- function(data, variable, label_fn=identity) analysis_scope_condition_groups(data,variable,split_rules(),label_fn)
  active_data <- shiny::reactiveVal(NULL)
  status <- shiny::reactiveVal("")
  batch <- NULL
  saved <- new.env(parent = emptyenv())
  saved_models <- new.env(parent = emptyenv())
  sequence <- 0L
  committed <- FALSE
  session$userData$scope_commit <- function(value = NULL) {
    committed <<- TRUE
    if (!is.null(batch) && is.list(value) && identical(value$type, "one_group_rm_anova") && is.null(value$error)) batch$models[[batch$index]] <<- value
    if (!is.null(batch)) {
      factory <- session$userData$scope_localization_factories[[batch$output_id]]
      if (is.function(factory)) batch$models[[batch$index]] <<- list(type = "scope_renderer", render = factory(value))
    }
  }
  filtered <- shiny::reactive(analysis_scope_filter(dataset_fn(), selection()))
  excluded <- shiny::reactive(unique(c(selection()$variable, split())))
  analysis_data <- shiny::reactive({
    active <- active_data()
    data <- if (is.null(active)) filtered() else active
    attr(data,"statedu_scope_excluded") <- excluded()
    data
  })
  label_values <- function(data, variable) {
    values <- unique(as.character(data[[variable]][!is.na(data[[variable]])]))
    labels <- attr(data[[variable]], "labels", exact = TRUE)
    display <- values
    if (length(labels)) { match <- match(values, as.character(labels)); display[!is.na(match)] <- names(labels)[match[!is.na(match)]] }
    category_labels <- category_value_label_lookup_static(category_table_fn())[[variable]]
    if (length(category_labels)) {
      overrides <- unname(category_labels[values]); use <- !is.na(overrides) & nzchar(overrides)
      display[use] <- overrides[use]
    }
    stats::setNames(values, display)
  }
  variable_choices <- function(data) {
    labels <- labels_fn(); display <- names(data)
    overrides <- unname(labels[display]); use <- !is.na(overrides) & nzchar(overrides)
    display[use] <- paste0(overrides[use], " (", names(data)[use], ")")
    c(stats::setNames("", "—"), stats::setNames(names(data), display))
  }
  controls <- analysis_scope_register_controls(input,output,session,dataset_fn,variable_info_fn,labels_fn,label_values,language_fn,function()editable())
  scope_status <- shiny::reactive({
    data <- dataset_fn(); used <- filtered(); variable <- split()
    paste0(analysis_scope_text(language_fn(), "분석 대상: ", "Selected cases: "), nrow(used), "/", nrow(data),
      if (length(selection()$variable)) paste0(" · ", selection()$variable, " = ", if(length(selection()$rules))paste(vapply(selection()$rules,function(rule)rule$label,character(1)),collapse=" | ")else paste(selection()$values,collapse=", ")) else "",
      if (length(variable)) paste0(" · ", sprintf(statedu_t("analysis.scope.split_status", language_fn(),
        "Split variable: %s · Total groups: %s"), variable, length(configured_groups(used, variable)))) else "", " ", status())
  })
  output$scope_cases_status <- shiny::renderUI(p(scope_status()))
  output$scope_split_status <- shiny::renderUI(p(scope_status()))
  shiny::observe({ session$sendCustomMessage("statedu-analysis-scope-status", list(text = scope_status(), active = length(selection()$variable) > 0L || length(split()) > 0L)) })
  editable <- function() {
    if (!is.null(batch)) { shiny::showNotification("분할 분석이 진행 중입니다. / Split analysis is running.", type = "warning"); return(FALSE) }
    TRUE
  }
  shiny::observeEvent(input$scope_cases_apply, {
    if (!editable()) return()
    tryCatch({
      configured <- controls$read("cases")
      selection(list(variable=configured$variable,values=character(),rules=configured$rules))
    },error=function(e)shiny::showNotification(conditionMessage(e),type="warning"))
  })
  shiny::observeEvent(input$scope_cases_clear, { if (editable()) {selection(list(variable=character(),values=character()));controls$reset("cases")} })
  shiny::observeEvent(input$scope_split_apply, {
    if (!editable()) return()
    tryCatch({
      configured <- controls$read("split")
      analysis_scope_condition_groups(filtered(),configured$variable,configured$rules)
      split_rules(configured$rules);split(configured$variable)
    },error=function(e)shiny::showNotification(conditionMessage(e),type="warning"))
  })
  shiny::observeEvent(input$scope_split_clear, { if (editable()) {split(character());split_rules(list());controls$reset("split")} })
  shiny::observeEvent(file_fn(), {
    selection(list(variable=character(),values=character()));split(character());split_rules(list())
    controls$reset("cases");controls$reset("split")
  }, ignoreInit=TRUE)
  finish <- function() {
    html <- paste(vapply(batch$sections, as.character, character(1)), collapse = "\n")
    saved[[batch$output_id]] <- html
    saved_models[[batch$output_id]] <- if (length(batch$models) == length(batch$sections) && all(vapply(batch$models, function(x) isTRUE(x$type %in% c("one_group_rm_anova", "scope_regression", "scope_renderer")), logical(1)))) list(sections = batch$sections, models = batch$models) else NULL
    session$sendCustomMessage("statedu-analysis-scope-finish", list(outputId = batch$output_id, html = html, canvasRootId = batch$canvas_root_id))
    batch <<- NULL; active_data(NULL); status("")
  }
  shiny::observeEvent(language_fn(), {
    if (!is.null(batch)) return()
    for (id in ls(saved_models)) {
      entry <- saved_models[[id]]
      if (is.null(entry)) next
      html <- paste(vapply(seq_along(entry$sections), function(i) analysis_scope_localize_one_group_snapshot(as.character(entry$sections[[i]]), entry$models[[i]], language_fn()), character(1)), collapse = "\n")
      saved[[id]] <- html
      session$sendCustomMessage("statedu-analysis-scope-finish", list(outputId = id, html = html))
    }
  }, ignoreInit = TRUE)
  next_group <- function() {
    batch$index <<- batch$index + 1L
    if (isTRUE(batch$cancel) || batch$index > length(batch$groups)) return(finish())
    group <- batch$groups[[batch$index]]
    session$sendCustomMessage("statedu-analysis-scope-group", list(canvasRootId = batch$canvas_root_id,
      key = paste0("scope-", batch$token, "-", batch$index), label = group$label))
    active_data(group$data); committed <<- FALSE
    session$userData$scope_force_result <- TRUE
    batch$waiting <<- FALSE; batch$error <<- ""; batch$started <<- Sys.time()
    status(paste0(" · ", batch$index, "/", length(batch$groups), " · ", group$label))
    tryCatch(shiny::isolate(analysis_scope_with_inputs(batch$run, batch$inputs, session$userData$scope_app_frame)),
      error = function(e) batch$error <<- conditionMessage(e))
  }
  session$userData$scope_run <- function(run_fn, output_id, canvas_root_id) {
    if (!editable()) return(invisible(NULL))
    omitted <- shiny::isolate(excluded())
    if (length(omitted)) shiny::showNotification(paste0(analysis_scope_text(language_fn(),
      "케이스 선택·파일 분할 변수는 분석 변수에서 자동 제외합니다: ",
      "Case-selection / split variables are automatically excluded from analysis terms: "),paste(omitted,collapse=", ")),type="message",duration=8)
    if (!length(shiny::isolate(split()))) {
      saved[[output_id]] <- NULL
      saved_models[[output_id]] <- NULL
      session$userData$scope_force_result <- TRUE
      session$sendCustomMessage("statedu-analysis-scope-start", list(outputId = output_id, running = FALSE,
        canvasRootId = canvas_root_id, label = if (length(shiny::isolate(selection()$variable))) shiny::isolate(scope_status()) else NULL))
      return(run_fn())
    }
    if (any(vapply(session$userData$scope_jobs %||% list(), function(job) !is.null(shiny::isolate(job())), logical(1)))) {
      shiny::showNotification("진행 중인 분석이 끝난 후 파일 분할 분석을 실행하세요. / Wait for the active analysis to finish.", type = "warning")
      return(invisible(NULL))
    }
    groups <- configured_groups(shiny::isolate(filtered()), shiny::isolate(split()), function(value) {
      choices <- label_values(shiny::isolate(filtered()), shiny::isolate(split()))
      names(choices)[match(value, choices)]
    })
    variable <- shiny::isolate(split())
    variable_label <- unname(shiny::isolate(labels_fn())[variable])
    if (length(variable_label) && !is.na(variable_label) && nzchar(variable_label)) {
      groups <- lapply(groups, function(group) { group$label <- paste0(variable_label, substring(group$label, nchar(variable) + 1L)); group })
    }
    if (!length(groups)) { shiny::showNotification("분석할 케이스가 없습니다. / No cases to analyze.", type = "error"); return(invisible(NULL)) }
    sequence <<- sequence + 1L
    saved[[output_id]] <- NULL
    saved_models[[output_id]] <- NULL
    batch <<- list(groups = groups, index = 0L, sections = list(), models = list(), run = run_fn, output_id = output_id, canvas_root_id = canvas_root_id, token = sequence,
      inputs = shiny::isolate(shiny::reactiveValuesToList(input)))
    session$sendCustomMessage("statedu-analysis-scope-start", list(outputId = output_id, running = TRUE, canvasRootId = canvas_root_id))
    next_group()
  }
  shiny::observe({
    shiny::invalidateLater(200, session)
    if (is.null(batch) || isTRUE(batch$waiting) || difftime(Sys.time(), batch$started, units = "secs") < .15) return()
    jobs <- session$userData$scope_jobs %||% list()
    if (any(vapply(jobs, function(job) !is.null(shiny::isolate(job())), logical(1)))) return()
    batch$waiting <<- TRUE
    if (!committed || nzchar(batch$error)) {
      message <- if (nzchar(batch$error)) batch$error else "분석 결과를 생성하지 못했습니다. 변수 설정과 케이스 수를 확인하세요. / No result was produced; check variables and case counts."
      batch$sections[[batch$index]] <<- div(class = "result-section", h3(batch$groups[[batch$index]]$label), p(message))
      next_group(); return()
    }
    session$onFlushed(function() {
      if (is.null(batch)) return()
      session$sendCustomMessage("easyflow-capture-result-snapshot", list(outputId = batch$output_id,
        inputId = "scope_group_snapshot", canvasRootId = batch$canvas_root_id, token = batch$token, groupIndex = batch$index))
    }, once = TRUE)
  })
  shiny::observeEvent(input$scope_group_snapshot, {
    payload <- input$scope_group_snapshot
    if (is.null(batch) || !identical(as.integer(payload$token), batch$token) || !identical(as.integer(payload$groupIndex), batch$index)) return()
    body <- if (nzchar(payload$error %||% "")) p(payload$error) else htmltools::HTML(payload$html)
    batch$sections[[batch$index]] <<- div(class = "result-section statedu-split-group", h3(batch$groups[[batch$index]]$label), body)
    next_group()
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$scope_cancel, {
    if (!is.null(batch)) {
      batch$cancel <<- TRUE
      status(analysis_scope_text(language_fn(), "현재 집단 완료 후 중단합니다.", "Stopping after the current group."))
    }
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$scope_save, {
    tryCatch({
      payload <- input$scope_save
      html <- saved[[as.character(payload$outputId)]]
      shiny::req(!is.null(html), nzchar(html))
      format <- as.character(payload$format)
      title <- analysis_scope_text(language_fn(), "파일 분할 분석 결과", "Split-file analysis results")
      if (identical(format, "figures")) {
        images <- xml2::xml_find_all(xml2::read_html(html), "//img[starts-with(@src,'data:image/png')]")
        if (!length(images)) stop("저장할 그림이 없습니다. / No figures to save.")
        directory <- choose_figure_save_dir()
        if (!length(directory) || !nzchar(directory[[1]])) return()
        files <- lapply(seq_along(images), function(index) list(name = paste0("figure_", index, ".png"), data = xml2::xml_attr(images[[index]], "src")))
        paths <- save_canvas_figure_snapshots(files, directory[[1]], "split_analysis")
        shiny::showNotification(dirname(paths[[1]]), type = "message"); return()
      }
      path <- switch(format, html = choose_html_save_path(), pdf = choose_pdf_save_path(), excel = choose_excel_save_path(),
        word = choose_word_save_path(), hwpx = choose_hwpx_save_path(), stop("Unsupported format"))
      if (!length(path) || !nzchar(path[[1]])) return()
      path <- path[[1]]
      extension <- switch(format, excel = "xlsx", word = "docx", format)
      if (!grepl(paste0("[.]", extension, "$"), path, ignore.case = TRUE)) path <- paste0(path, ".", extension)
      entries <- list(list(id = "split", title = title, html = html))
      switch(format,
        html = write_result_html_document(result_snapshot_document_html(title, html), path, useBytes = TRUE),
        pdf = write_pdf_from_html(saved_result_sheet_document(title, htmltools::HTML(html)), path),
        excel = save_screen_excel_file(result_snapshot_document_html(title, html), path),
        word = write_result_collection_docx(entries, path),
        hwpx = write_result_collection_hwpx(entries, path))
      shiny::showNotification(path, type = "message")
    }, error = function(e) shiny::showNotification(result_export_error_text(e, language_fn()), type = "error", duration = 10))
  }, ignoreInit = TRUE)
  list(dataset = analysis_data, selection = selection, split = split, filtered = filtered, excluded = excluded)
}
