# Data-only commands for analysis menus. Execution stays in the menu's handler.
analysis_command_run_ids <- function() c(
  "run_frequencies", "run_reliability", "run_crosstab", "run_correlation", "run_ipa",
  "run_factor_analysis", "run_pca", "run_interrater", "run_ttest_anova",
  "run_penalized_regularized",
  "run_ancova", "run_paired", "run_nonparametric_paired", "run_paired_rm",
  "run_nonparametric", "run_mixed_rm_anova", "run_one_group_rm_anova",
  "run_mediation_moderation", "run_logistic", "run_generalized", "run_longitudinal",
  "complex_freq_run", "complex_crosstab_run", "complex_ttest_run",
  "complex_correlation_run", "complex_regression_run", "complex_logistic_run",
  "run_survival_km", "run_survival_cox", "run_survival_competing", "meta_run_analysis"
)

analysis_command_json <- function(x) as.character(jsonlite::toJSON(
  x, auto_unbox = FALSE, null = "null", na = "null", digits = NA, dataframe = "columns"
))

analysis_command_text <- function(spec) {
  base <- spec[setdiff(names(spec), c("STATE", "OPTIONS"))]
  state <- if (length(spec$STATE)) stats::setNames(spec$STATE, paste0("STATE_", toupper(names(spec$STATE)))) else list()
  options <- if (length(spec$OPTIONS)) stats::setNames(spec$OPTIONS, paste0("OPTION_", toupper(names(spec$OPTIONS)))) else list()
  spec <- c(base, state, options)
  width <- max(nchar(names(spec)))
  lines <- vapply(names(spec), function(key) paste0(
    "  ", sprintf("%-*s", width, key), " = ",
    if (startsWith(key, "STATE_")) analysis_command_json(spec[[key]]) else
      as.character(jsonlite::toJSON(spec[[key]], auto_unbox = TRUE, digits = NA))
  ), character(1))
  paste(c("ANALYSIS", lines, "END"), collapse = "\n")
}

parse_analysis_command <- function(text, run_id, state_names, option_names) {
  if (!is.character(text) || length(text) != 1L || is.na(text) ||
      nchar(text, type = "bytes") > 1048576L) stop("명령어는 1 MB 이하의 텍스트여야 합니다.")
  lines <- trimws(strsplit(sub("^\ufeff", "", text), "\n", fixed = TRUE)[[1]])
  lines <- lines[nzchar(lines) & !startsWith(lines, "#")]
  if (length(lines) < 3L || lines[[1]] != "ANALYSIS" || tail(lines, 1) != "END")
    stop("ANALYSIS로 시작하고 END로 끝나는 명령어를 입력하세요.")
  fields <- c("VERSION", "STUDIO_VERSION", "ANALYSIS", "DATA_HASH", "CONTEXT_HASH", "STATE", "OPTIONS")
  state_keys <- if (length(state_names)) setNames(state_names, paste0("STATE_", toupper(state_names))) else character(0)
  option_keys <- if (length(option_names)) setNames(option_names, paste0("OPTION_", toupper(option_names))) else character(0)
  wire_fields <- c(setdiff(fields, c("STATE", "OPTIONS")), names(state_keys), names(option_keys))
  spec <- list()
  for (line in lines[2:(length(lines) - 1L)]) {
    match <- regmatches(line, regexec("^([A-Z][A-Z0-9_]*)\\s*=\\s*(.+)$", line))[[1]]
    if (length(match) != 3L || !match[[2]] %in% wire_fields || match[[2]] %in% names(spec))
      stop("잘못되었거나 중복된 명령어 항목입니다.")
    spec[match[[2]]] <- list(jsonlite::fromJSON(match[[3]], simplifyVector = FALSE))
  }
  state <- spec[intersect(names(state_keys), names(spec))]
  names(state) <- unname(state_keys[names(state)])
  options <- spec[intersect(names(option_keys), names(spec))]
  names(options) <- unname(option_keys[names(options)])
  spec <- c(spec[setdiff(names(spec), c(names(state_keys), names(option_keys)))], list(STATE = state, OPTIONS = options))
  if (!setequal(names(spec), fields)) stop("필수 명령어 항목이 누락되었습니다.")
  if (!identical(spec$VERSION, 1L) || !identical(spec$ANALYSIS, run_id))
    stop("명령어 버전 또는 분석 메뉴가 다릅니다. 해당 분석 메뉴에서 불러오세요.")
  for (key in c("STUDIO_VERSION", "DATA_HASH", "CONTEXT_HASH")) {
    if (!is.character(spec[[key]]) || length(spec[[key]]) != 1L || is.na(spec[[key]]))
      stop(paste("문자열이 필요합니다:", key))
  }
  valid_object <- function(x, allowed, required = FALSE) {
    is.list(x) && (!length(x) || (!is.null(names(x)) && !anyDuplicated(names(x)))) &&
      all(names(x) %in% allowed) && (!required || setequal(names(x), allowed))
  }
  if (!valid_object(spec$STATE, state_names, TRUE) || !valid_object(spec$OPTIONS, option_names))
    stop("지원하지 않는 설정 항목입니다. 현재 메뉴에서 명령어를 다시 생성하세요.")
  spec
}

analysis_command_restore_value <- function(value, prototype) {
  if (is.null(value)) return(NULL)
  if (is.data.frame(prototype)) {
    if (!setequal(names(value), names(prototype))) stop("명령어 데이터의 열이 일치하지 않습니다.")
    columns <- lapply(names(prototype), function(key) analysis_command_restore_value(value[[key]], prototype[[key]]))
    names(columns) <- names(prototype)
    return(as.data.frame(columns, stringsAsFactors = FALSE, check.names = FALSE))
  }
  if (is.atomic(prototype) && !is.null(prototype)) {
    if (is.list(value) && any(vapply(value, is.list, logical(1)))) stop("단순한 값의 배열이 필요합니다. / A flat array is required.")
    flat <- unlist(lapply(value, function(x) if (is.null(x)) NA else x), use.names = FALSE)
    if (is.null(flat)) return(prototype[FALSE])
    expected <- typeof(prototype)
    if (expected %in% c("integer", "double")) {
      if ((!is.numeric(flat) && !all(is.na(flat))) || any(is.infinite(flat))) stop("유한한 숫자가 필요합니다.")
      if (expected == "integer" && any(flat != trunc(flat), na.rm = TRUE)) stop("정수가 필요합니다.")
      return(if (expected == "integer") as.integer(flat) else as.numeric(flat))
    }
    if (all(is.na(flat))) return(switch(expected, character = as.character(flat), logical = as.logical(flat), flat))
    if (!identical(typeof(flat), expected)) stop("설정 값의 자료형이 일치하지 않습니다.")
    return(flat)
  }
  # Preserve nested arrays (for example repeated-measure groups), including empty ones.
  decode <- function(item) {
    if (!is.list(item)) return(item)
    if (is.null(names(item)) && length(item) && all(vapply(item, function(x) !is.list(x), logical(1))))
      return(unlist(lapply(item, function(x) if (is.null(x)) NA else x), use.names = FALSE))
    lapply(item, decode)
  }
  if (is.list(value)) return(lapply(value, decode))
  stop("잘못된 설정 값입니다.")
}

register_analysis_command_handler <- function(run_id, input, output, session, states,
                                              dataset_fn, context_fn, run_fn, ignoreInit = FALSE,
                                              after_apply = function() NULL) {
  option_names <- character(0)
  option_prototypes <- list()
  pending <- NULL
  sequence <- 0L
  original_run <- run_fn
  run_fn <- function() {
    # Meta-analysis has its own manually entered study-level data, not this file.
    if (identical(run_id, "meta_run_analysis")) return(original_run())
    if (grepl("_canvas_command_run$", run_id) || grepl("_canvas_state_command_run$", run_id)) return(original_run())
    # Restored variable metadata alone is not an analyzable dataset. Shiny's
    # req() otherwise aborts silently and makes the Run button appear inert.
    data <- tryCatch(dataset_fn(), error = function(e) e)
    if (!is.data.frame(data)) {
      message <- if (inherits(data, "error") && !inherits(data, "shiny.silent.error")) conditionMessage(data) else
        analysis_scope_text(statedu_current_language(),
          "실제 데이터가 연결되지 않았습니다. 데이터 탭에서 원본 파일을 다시 여세요. 복원한 설정은 유지됩니다.",
          "No data is connected. Reopen the original file in the Data tab. Restored settings are retained.")
      showNotification(message, type = "error", duration = 15)
      return(invisible(NULL))
    }
    analysis_scope_run(session, run_id, original_run)
  }
  report <- function(expr) tryCatch(expr, error = function(e) {
    pending <<- NULL
    showNotification(conditionMessage(e), type = "error", duration = 12)
  })
  # Only ordinary bound controls collected from this menu can be restored.
  observeEvent(input[[paste0(run_id, "_open_regression_syntax")]], {
    descriptor <- input[[paste0(run_id, "_open_regression_syntax")]]
    option_names <<- as.character(unlist(descriptor$options, use.names = FALSE))
    option_names <<- option_names[grepl("^[A-Za-z][A-Za-z0-9_]*$", option_names)]
    option_prototypes <<- setNames(lapply(option_names, function(key) isolate(input[[key]])), option_names)
  }, priority = 100)
  capture <- function() {
    list(VERSION = 1L, STUDIO_VERSION = as.character(readLines("VERSION", warn = FALSE)[1]),
         ANALYSIS = run_id, DATA_HASH = regression_syntax_data_hash(dataset_fn()),
         CONTEXT_HASH = regression_syntax_data_hash(context_fn()),
         STATE = lapply(states, function(get) get()),
         OPTIONS = setNames(lapply(option_names, function(key) input[[key]]), option_names))
  }
  parse <- function(text) parse_analysis_command(text, run_id, names(states), option_names)
  send_options <- function() {
    if (is.null(pending)) return(invisible(NULL))
    session$sendCustomMessage("statedu-command-options", list(
      id = run_id, token = pending$token, options = pending$options))
  }
  register_regression_syntax(
    input, output, session, capture_fn = capture,
    validate_fn = function(spec, allow) {
      if (!allow && spec$DATA_HASH != regression_syntax_data_hash(dataset_fn()))
        stop("저장 당시 데이터와 다릅니다. 현재 데이터 적용 여부를 확인하세요.")
      if (spec$CONTEXT_HASH != regression_syntax_data_hash(context_fn()))
        stop("변수 선택·측정수준·값 레이블 설정이 다릅니다. 데이터 화면 설정을 확인하세요.")
      # Validate every value before applying any of them.
      for (key in names(states)) analysis_command_restore_value(spec$STATE[[key]], states[[key]]())
      for (key in names(spec$OPTIONS)) analysis_command_restore_value(spec$OPTIONS[[key]], option_prototypes[[key]])
    },
    prepare_fn = identity,
    apply_fn = function(spec) {
      restored <- setNames(lapply(names(states), function(key)
        analysis_command_restore_value(spec$STATE[[key]], states[[key]]())), names(states))
      options <- setNames(lapply(names(spec$OPTIONS), function(key)
        analysis_command_restore_value(spec$OPTIONS[[key]], option_prototypes[[key]])), names(spec$OPTIONS))
      sequence <<- sequence + 1L
      pending <<- list(token = sequence, options = options, state = restored, run = FALSE, attempts = 0L)
      for (key in names(states)) states[[key]](restored[[key]])
      after_apply()
      session$onFlushed(send_options, once = TRUE)
    },
    run_fn = function(spec) pending$run <<- TRUE,
    version = readLines("VERSION", warn = FALSE)[1], prefix = paste0(run_id, "_"),
    parse_fn = parse, text_fn = analysis_command_text, filename = paste0(run_id, ".stcmd"),
    notify_apply = FALSE,
    details = if (identical(run_id, "meta_run_analysis"))
      "메타분석 명령어에는 입력한 연구별 효과값이 포함됩니다. STATE_EFFECTS에서 연구 데이터를 확인하세요." else
      "변수 목록은 [\"변수1\", \"변수2\"]로 입력하세요. 원자료는 명령어 파일에 포함하지 않습니다.",
    help = "이 메뉴의 변수 선택과 옵션을 명령어로 저장하고 다시 실행합니다. 먼저 데이터를 불러오고 변수 선택을 적용하세요."
  )
  observeEvent(input[[paste0(run_id, "_command_ack")]], report({
    ack <- input[[paste0(run_id, "_command_ack")]]
    if (is.null(pending) || !identical(as.integer(ack$token), pending$token)) return()
    matches <- vapply(names(pending$options), function(key)
      identical(analysis_command_json(input[[key]]), analysis_command_json(pending$options[[key]])), logical(1))
    if (!all(matches)) {
      pending$attempts <<- pending$attempts + 1L
      if (pending$attempts >= 5L) stop(paste("메뉴에 적용할 수 없는 옵션:", paste(names(matches)[!matches], collapse = ", ")))
      session$onFlushed(send_options, once = TRUE)
      return()
    }
    # Reject choices pruned by the menu's measurement/selection constraints.
    if (!all(vapply(names(states), function(key)
      identical(analysis_command_json(states[[key]]()), analysis_command_json(pending$state[[key]])), logical(1))))
      stop("선택한 변수를 현재 분석 메뉴에 적용할 수 없습니다. 변수와 측정수준을 확인하세요.")
    execute <- pending$run
    pending <<- NULL
    if (execute) run_fn() else showNotification("분석 명령어를 메뉴에 반영했습니다. / Applied to menu.")
  }), priority = -100)
  observeEvent(input[[run_id]], run_fn(), ignoreInit = ignoreInit)
}

analysis_canvas_command_button <- function(language) tags$button(
  type = "button", class = "custom-model-toolbar-button",
  onclick = "window.stateduOpenCommand(this.closest('.custom-model-canvas-root').getAttribute('data-input-prefix') + '_command_run', this)",
  if (identical(normalize_app_language(language), "ko")) "분석 명령어" else analysis_ui_text("Analysis Commands", language)
)

register_canvas_analysis_commands <- function(input, output, session, canvas_input, root_id,
                                               dataset_fn, context_fn, run_fn, extra_states = list()) {
  model <- reactiveVal(NULL)
  observeEvent(input[[canvas_input]], {
    snapshot <- input[[canvas_input]]
    snapshot$nonce <- NULL
    model(snapshot)
  })
  register_analysis_command_handler(
    paste0(sub("_state$", "", canvas_input), "_command_run"), input, output, session,
    states = c(list(model = model), extra_states), dataset_fn = dataset_fn, context_fn = context_fn,
    run_fn = function() {
      if (is.null(model()) || !length(model()$nodes)) stop("명령어에 분석할 모형이 없습니다.")
      run_fn(model())
    },
    after_apply = function() {
      snapshot <- model()
      session$onFlushed(function() session$sendCustomMessage("statedu-command-canvas", list(
        rootId = root_id, snapshot = snapshot)), once = TRUE)
    }
  )
}

# Keep command/run controls inside the canvas root so existing event bindings and
# command state capture continue to use the same analysis instance.
analysis_canvas_sidebar <- function(root, language, save_control = NULL) {
  actions <- list()
  extract <- function(node) {
    if (inherits(node, "shiny.tag")) {
      classes <- strsplit(node$attribs$class %||% "", " ")[[1]]
      command <- grepl("stateduOpenCommand", node$attribs$onclick %||% "", fixed = TRUE)
      run <- identical(node$attribs[["data-action"]], "run")
      popover <- "custom-model-run-options-popover" %in% classes
      if (command || run || popover) {
        if (command || run) {
          node$attribs$class <- paste(node$attribs$class, "btn", if (run) "btn-primary" else "btn-default")
          if (run) node$children <- list(if (identical(normalize_app_language(language), "ko")) "분석 실행" else analysis_ui_text("Run analysis", language))
        }
        actions[[if (command) "command" else if (run) "run" else "options"]] <<- node
        return(NULL)
      }
      node$children <- lapply(node$children, extract)
    } else if (is.list(node)) node <- lapply(node, extract)
    node
  }
  root$attribs[["data-export-dpi"]] <- as.character(analysis_figure_dpi())
  root <- extract(root)
  if (!is.null(actions$options)) {
    children <- actions$options$children
    is_footer <- vapply(children, function(child) {
      inherits(child, "shiny.tag") && grepl("custom-model-run-options-actions", child$attribs$class %||% "", fixed = TRUE)
    }, logical(1))
    actions$options$children <- list(
      div(class = "custom-model-sidebar-options-body", children[!is_footer]),
      children[is_footer])
  }
  root$children[[1]] <- div(class = "custom-model-sidebar",
    root$children[[1]],
    div(class = "custom-model-sidebar-actions custom-model-toolbar-panel is-active",
      div(class = "analysis-primary-actions", actions$command, actions$run),
      actions$options),
    div(class = "custom-model-sidebar-save", save_control))
  root
}

# Capture displayed tables and the displayed canvas together before opening Save.
register_canvas_report_exports <- function(input, session, html_id, pdf_id, output_id,
                                           root_id, title_fn, result_fn, app_language_fn = NULL, excel_id = NULL, hwpx_id = NULL, canvas_input_prefix = NULL) {
  figure_prefix <- sub("_results$", "", output_id)
  figure_input <- paste0(canvas_input_prefix %||% figure_prefix, "_figures_snapshot")
  observeEvent(input[[figure_input]], {
    tryCatch({
      files <- input[[figure_input]]$files
      shiny::req(length(files) > 0L)
      directory <- choose_figure_save_dir()
      if (!length(directory) || !nzchar(directory[[1]])) return(invisible(NULL))
      saved <- save_canvas_figure_snapshots(files, directory[[1]], figure_prefix)
      showNotification(sprintf(statedu_t("result.figures_saved", statedu_current_language(app_language_fn)),
        length(saved), dirname(saved[[1]])), type = "message")
    }, error = function(error) showNotification(result_export_error_text(error, statedu_current_language(app_language_fn)), type = "error", duration = 8))
  }, ignoreInit = TRUE)
  for (format in c("html", "pdf", if (!is.null(excel_id)) "excel", if (!is.null(hwpx_id)) "hwpx")) local({
    export_format <- format
    button_id <- switch(format, html = html_id, pdf = pdf_id, excel = excel_id, hwpx = hwpx_id)
    capture_id <- paste0(button_id, "_snapshot")
    observeEvent(input[[button_id]], {
      shiny::req(!is.null(result_fn()))
      session$sendCustomMessage("easyflow-capture-result-snapshot", list(
        outputId = output_id, inputId = capture_id, canvasRootId = root_id))
    }, ignoreInit = TRUE)
    observeEvent(input[[capture_id]], {
      tryCatch({
        payload <- input[[capture_id]]
        if (nzchar(payload$error %||% "")) stop(payload$error)
        shiny::req(nzchar(payload$html %||% ""))
        path <- switch(export_format, pdf = choose_pdf_save_path(), html = choose_html_save_path(), excel = choose_excel_save_path(), hwpx = choose_hwpx_save_path())
        if (!length(path) || !nzchar(path[[1]])) return(invisible(NULL))
        path <- path[[1]]
        extension <- if (export_format == "excel") "xlsx" else export_format
        if (!grepl(paste0("\\.", extension, "$"), path, ignore.case = TRUE)) path <- paste0(path, ".", extension)
        if (export_format == "hwpx") {
          write_result_collection_hwpx(list(list(id = "current", title = title_fn(), html = payload$html)), path)
        } else if (export_format == "excel") {
          save_screen_excel_file(result_snapshot_document_html(title_fn(), payload$html), path)
        } else if (export_format == "pdf") {
          write_pdf_from_html(saved_result_sheet_document(title_fn(), htmltools::HTML(payload$html)), path)
        } else {
          write_result_html_document(result_snapshot_document_html(title_fn(), payload$html), path, useBytes = TRUE)
        }
        if (export_format == "hwpx") showNotification(paste("HWPX 저장 완료:", path)) else
          showNotification(sprintf(statedu_t(paste0("result.", export_format, "_saved"), statedu_current_language(app_language_fn)), path))
      }, error = function(e) showNotification(result_export_error_text(e, statedu_current_language(app_language_fn)), type = "error", duration = 8))
    }, ignoreInit = TRUE)
  })
}
