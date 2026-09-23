# Versioned, data-only regression commands. Never evaluate imported R code.
regression_syntax_fields <- function() c(
  "VERSION", "STUDIO_VERSION", "DATA", "DATA_HASH", "DEPENDENTS", "PREDICTORS",
  "MEASUREMENTS", "REFERENCES", "MISSING", "CI_METHOD", "BOOTSTRAP", "SEED",
  "RESIDUAL_DIAGNOSTICS", "AUTO_METHOD", "SHOW_SR2", "SHOW_F2", "SHOW_VIF", "OUTPUT_STYLE"
)

regression_syntax_text <- function(spec) {
  key_width <- max(nchar(names(spec)), 0L)
  lines <- vapply(names(spec), function(key) {
    vector <- key %in% c("DEPENDENTS", "PREDICTORS", "BLOCK1", "BLOCK2", "BLOCK3", "BLOCK4")
    paste0("  ", sprintf("%-*s", key_width, key), " = ", jsonlite::toJSON(spec[[key]], auto_unbox = !vector,
                                          null = "null", digits = NA))
  }, character(1))
  paste(c("REGRESSION", lines, "END"), collapse = "\n")
}

parse_regression_syntax <- function(text) {
  if (length(text) != 1L || is.na(text) || nchar(text, type = "bytes") > 1048576L)
    stop("명령어는 1 MB 이하의 텍스트여야 합니다. / Commands must be text under 1 MB.")
  lines <- trimws(strsplit(sub("^\ufeff", "", text), "\n", fixed = TRUE)[[1]])
  lines <- lines[nzchar(lines) & !startsWith(lines, "#")]
  if (length(lines) < 3L || lines[[1]] != "REGRESSION" || tail(lines, 1) != "END")
    stop("REGRESSION으로 시작하고 END로 끝나는 분석 명령어 하나를 입력하세요.")
  spec <- list()
  for (line in lines[2:(length(lines) - 1L)]) {
    match <- regmatches(line, regexec("^([A-Z][A-Z0-9_]*)\\s*=\\s*(.+)$", line))[[1]]
    if (length(match) != 3L) stop(paste("잘못된 명령어 / Invalid line:", line))
    key <- match[[2]]
    if (!key %in% c(regression_syntax_fields(), "BLOCK1", "BLOCK2", "BLOCK3", "BLOCK4") || key %in% names(spec))
      stop(paste("알 수 없거나 중복된 항목 / Unknown or duplicate field:", key))
    value <- tryCatch(jsonlite::fromJSON(match[[3]], simplifyVector = FALSE),
                      error = function(e) stop(paste("JSON 값 오류 / Invalid JSON:", key)))
    if (is.null(value)) stop(paste("빈 값은 허용하지 않습니다 / Null value:", key))
    spec[key] <- list(value)
  }
  if (!all(regression_syntax_fields() %in% names(spec)))
    stop(paste("필수 항목 누락 / Missing fields:", paste(setdiff(regression_syntax_fields(), names(spec)), collapse = ", ")))
  scalar <- function(x, type) length(x) == 1L && !is.list(x) && !is.na(x) && typeof(x) %in% type
  for (key in c("VERSION", "BOOTSTRAP", "SEED")) {
    x <- spec[[key]]
    if (!scalar(x, c("integer", "double")) || !is.finite(x) || x != floor(x) || x < 1 || x > .Machine$integer.max)
      stop(paste("양의 정수가 필요합니다 / Positive integer required:", key))
    spec[[key]] <- as.integer(x)
  }
  if (spec$VERSION != 1L) stop("지원하지 않는 명령어 버전입니다. / Unsupported syntax version.")
  if (!spec$BOOTSTRAP %in% c(1000L, 5000L, 10000L, 20000L, 50000L))
    stop("BOOTSTRAP: 1000, 5000, 10000, 20000, 50000 중 선택하세요.")
  for (key in c("STUDIO_VERSION", "DATA", "DATA_HASH", "MISSING", "CI_METHOD", "OUTPUT_STYLE"))
    if (!scalar(spec[[key]], "character")) stop(paste("문자열이 필요합니다 / String required:", key))
  if (spec$MISSING != "LISTWISE" || spec$CI_METHOD != "bias_corrected")
    stop("현재 지원하는 옵션: MISSING = LISTWISE, CI_METHOD = bias_corrected")
  if (!spec$OUTPUT_STYLE %in% c("standard", "wide", "compact", "compact_xm")) stop("OUTPUT_STYLE 값이 잘못되었습니다.")
  if (!grepl("^[a-f0-9]{32}$", spec$DATA_HASH)) stop("DATA_HASH 값이 잘못되었습니다.")
  for (key in c("RESIDUAL_DIAGNOSTICS", "AUTO_METHOD", "SHOW_SR2", "SHOW_F2", "SHOW_VIF"))
    if (!scalar(spec[[key]], "logical")) stop(paste("true 또는 false가 필요합니다:", key))
  if (spec$AUTO_METHOD && !spec$RESIDUAL_DIAGNOSTICS)
    stop("AUTO_METHOD를 사용하려면 RESIDUAL_DIAGNOSTICS도 true여야 합니다.")
  for (key in c("DEPENDENTS", "PREDICTORS")) {
    x <- spec[[key]]
    if (!is.list(x) || !is.null(names(x)) || !length(x) ||
        !all(vapply(x, function(v) scalar(v, "character") && nzchar(v), logical(1))))
      stop(paste("변수명 배열이 필요합니다 / Variable array required:", key))
    spec[[key]] <- unlist(x, use.names = FALSE)
    if (anyDuplicated(spec[[key]])) stop(paste("중복 변수 / Duplicate variable:", key))
  }
  if (length(intersect(spec$DEPENDENTS, spec$PREDICTORS))) stop("종속변수와 독립변수는 겹칠 수 없습니다.")
  blocks <- intersect(c("BLOCK1", "BLOCK2", "BLOCK3", "BLOCK4"), names(spec))
  if (length(blocks)) {
    if (!all(c("BLOCK1", "BLOCK2", "BLOCK3") %in% blocks)) stop("BLOCK1, BLOCK2, BLOCK3을 모두 지정하세요.")
    for (key in blocks) {
      x <- spec[[key]]
      if (!is.list(x) || !is.null(names(x)) ||
          !all(vapply(x, function(v) scalar(v, "character") && nzchar(v), logical(1))))
        stop(paste("변수명 배열이 필요합니다:", key))
      spec[[key]] <- as.character(unlist(x, use.names = FALSE))
    }
    if (!length(spec$BLOCK1) || (!length(spec$BLOCK2) && length(spec$BLOCK3)) || (length(spec$BLOCK4) && (!length(spec$BLOCK2) || !length(spec$BLOCK3)))) stop("블록은 1부터 순서대로 채우세요.")
    all_predictors <- unlist(spec[blocks], use.names = FALSE)
    if (anyDuplicated(all_predictors) || !identical(all_predictors, spec$PREDICTORS))
      stop("PREDICTORS는 지정된 BLOCK의 변수를 순서대로 합친 목록이어야 합니다.")
    if (spec$OUTPUT_STYLE == "compact_xm") stop("블록 회귀분석은 compact_xm 표 스타일을 지원하지 않습니다.")
  }
  for (key in c("MEASUREMENTS", "REFERENCES")) {
    x <- spec[[key]]
    if (!is.list(x) || (length(x) && (is.null(names(x)) || any(!nzchar(names(x))) || anyDuplicated(names(x)))) ||
        !all(vapply(x, function(v) scalar(v, "character"), logical(1))))
      stop(paste("변수명과 문자열 값의 객체가 필요합니다:", key))
  }
  spec[c(regression_syntax_fields(), blocks)]
}

regression_syntax_data_hash <- function(data) {
  # Hash the effective data, including user-missing rules and calculated columns.
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  saveRDS(data, path, version = 2, compress = FALSE)
  unname(tools::md5sum(path))
}

regression_syntax_measurements <- function(info) {
  if (!is.data.frame(info) || !all(c("name", "measurement") %in% names(info)))
    stop("먼저 데이터의 변수 정보를 설정하세요. / Set up variable information first.")
  as.list(stats::setNames(as.character(info$measurement), as.character(info$name)))
}

validate_regression_syntax_context <- function(spec, data, info, references, allow_changed_data = FALSE) {
  vars <- c(spec$DEPENDENTS, spec$PREDICTORS)
  missing <- setdiff(vars, names(data))
  if (length(missing)) stop(paste("데이터에 없는 변수 / Missing variables:", paste(missing, collapse = ", ")))
  if (!allow_changed_data && spec$DATA_HASH != regression_syntax_data_hash(data))
    stop("저장 당시와 데이터가 다릅니다. 데이터를 확인하거나 '현재 데이터에 적용'을 선택하세요. / Data has changed.")
  measurements <- regression_syntax_measurements(info)
  for (name in vars) {
    if (is.null(spec$MEASUREMENTS[[name]]) || !identical(spec$MEASUREMENTS[[name]], measurements[[name]]))
      stop(paste("변수 측정수준이 저장 당시와 다릅니다. 변수 설정을 확인하세요:", name))
    saved <- spec$REFERENCES[[name]] %||% ""
    current <- if (name %in% names(references)) unname(references[[name]]) else ""
    if (!identical(saved, current)) stop(paste("범주 기준값이 다릅니다. 변수 설정을 확인하세요:", name))
  }
  invisible(spec)
}

prepare_regression_syntax <- function(spec, data, info) {
  if ("BLOCK1" %in% names(spec)) return(prepare_hierarchical_analysis_results(
    data = data, dependents = spec$DEPENDENTS, block1 = spec$BLOCK1, block2 = spec$BLOCK2, block3 = spec$BLOCK3, block4 = spec$BLOCK4 %||% character(0),
    variable_info = info, reference_values = unlist(spec$REFERENCES, use.names = TRUE),
    boot_r = spec$BOOTSTRAP, seed = spec$SEED,
    residual_diagnostics = spec$RESIDUAL_DIAGNOSTICS, auto_method = spec$AUTO_METHOD, ci_method = spec$CI_METHOD
  ))
  prepare_regression_analysis_results(
    data = data, dependents = spec$DEPENDENTS, predictors = spec$PREDICTORS,
    variable_info = info, reference_values = unlist(spec$REFERENCES, use.names = TRUE),
    boot_r = spec$BOOTSTRAP, seed = spec$SEED,
    residual_diagnostics = spec$RESIDUAL_DIAGNOSTICS, auto_method = spec$AUTO_METHOD,
    ci_method = spec$CI_METHOD
  )
}

register_regression_syntax <- function(input, output, session, capture_fn, validate_fn, apply_fn,
                                       prepare_fn, run_fn, version, prefix = "", scope_run_id = NULL,
                                       parse_fn = parse_regression_syntax, text_fn = regression_syntax_text,
                                       filename = "regression.stcmd",
                                       notify_apply = TRUE,
                                       details = "변수명은 따옴표로, 변수 목록은 [\"변수1\", \"변수2\"]로 입력하세요. 데이터 자체는 명령어 파일에 포함하지 않습니다.",
                                       help = "회귀분석 설정을 파일로 저장하고 수정하여 다시 실행합니다. 먼저 데이터를 불러오고 변수를 선택하세요.") {
  id <- function(name) paste0(prefix, name)
  value <- function(name) input[[id(name)]]
  submit <- function(action) sprintf(
    "Shiny.setInputValue('%s', {text: document.getElementById('%s').value, allow: document.getElementById('%s').checked, action: '%s'}, {priority: 'event'});",
    id("regression_syntax_request"), id("regression_syntax_text"), id("regression_syntax_current_data"), action)
  draft <- reactiveVal("")
  download_text <- reactiveVal("")
  observeEvent(value("regression_syntax_text"), draft(value("regression_syntax_text")), ignoreNULL = TRUE)
  report <- function(expr) tryCatch(expr, error = function(e) {
    showNotification(conditionMessage(e), type = "error", duration = 12)
    invisible(NULL)
  })
  observeEvent(value("open_regression_syntax"), {
    showModal(modalDialog(
      title = "분석 명령어 / Analysis Commands", size = "l", easyClose = FALSE,
      tags$style(HTML(paste0(
        "#shiny-modal:has(.statedu-syntax-help){z-index:11010;}",
        "body:has(.statedu-syntax-help) .modal-backdrop{z-index:11000;}",
        "#shiny-modal:has(.statedu-syntax-help) .modal-dialog{margin:20px auto;}",
        "#shiny-modal:has(.statedu-syntax-help) .modal-content{max-height:calc(100vh - 40px);display:flex;flex-direction:column;}",
        "#shiny-modal:has(.statedu-syntax-help) .modal-body{overflow-y:auto;min-height:0;}",
        "#shiny-modal:has(.statedu-syntax-help) .modal-header,#shiny-modal:has(.statedu-syntax-help) .modal-footer{flex-shrink:0;}",
        "#shiny-modal:has(.statedu-syntax-help) textarea{font-family:Consolas,monospace;}"
      ))),
      tags$p(class = "statedu-syntax-help", help),
      actionButton(id("generate_regression_syntax"), "메뉴에서 명령어 생성 / From menu"),
      actionButton(id("save_regression_syntax"), "명령어 저장 / Save", onclick = submit("save")),
      tags$div(style = "display:none", downloadLink(id("download_regression_syntax"), "Download")),
      tags$script(HTML("Shiny.addCustomMessageHandler('statedu-syntax-download',function(m){var a=document.getElementById(m.id);if(a)a.click();});")),
      fileInput(id("load_regression_syntax"), "명령어 불러오기 / Open", accept = c(".stcmd", ".txt")),
      textAreaInput(id("regression_syntax_text"), "분석 명령어 / Commands", value = isolate(draft()), width = "100%", rows = 19),
      checkboxInput(id("regression_syntax_current_data"), "현재 데이터에 적용 (저장 당시 데이터와 달라도 실행) / Allow changed data", FALSE),
      tags$p(class = "help-block", details),
      footer = tagList(modalButton("닫기 / Close"),
                      actionButton(id("apply_regression_syntax"), "메뉴에 반영 / Apply", onclick = submit("apply")),
                      actionButton(id("run_regression_syntax"), "명령어 실행 / Run", class = "btn-primary", onclick = submit("run")))
    ))
  })
  observeEvent(value("generate_regression_syntax"), report({
    text <- text_fn(capture_fn())
    parse_fn(text)
    draft(text)
    updateTextAreaInput(session, id("regression_syntax_text"), value = text)
  }))
  observeEvent(value("load_regression_syntax"), report({
    file <- value("load_regression_syntax")
    if (file$size > 1048576) stop("명령어 파일은 1 MB 이하여야 합니다.")
    text <- paste(readLines(file$datapath, encoding = "UTF-8", warn = FALSE), collapse = "\n")
    parse_fn(text)
    draft(text)
    updateTextAreaInput(session, id("regression_syntax_text"), value = text)
  }))
  output[[id("download_regression_syntax")]] <- downloadHandler(
    filename = function() filename,
    content = function(file) writeLines(enc2utf8(isolate(download_text())), file, useBytes = TRUE),
    contentType = "text/plain; charset=utf-8"
  )
  outputOptions(output, id("download_regression_syntax"), suspendWhenHidden = FALSE)
  read_spec <- function(request) {
    spec <- parse_fn(request$text)
    validate_fn(spec, isTRUE(request$allow))
    if (!identical(spec$STUDIO_VERSION, as.character(version)))
      showNotification("저장 당시와 Studio 버전이 다릅니다. / Studio version differs.", type = "warning", duration = 8)
    spec
  }
  observeEvent(value("regression_syntax_request"), report({
    request <- value("regression_syntax_request")
    if (identical(request$action, "save")) {
      if (!is.character(request$text) || length(request$text) != 1L || is.na(request$text) ||
          nchar(request$text, type = "bytes") > 1048576L) stop("명령어는 1 MB 이하의 텍스트여야 합니다.")
      draft(request$text)
      download_text(request$text)
      session$sendCustomMessage("statedu-syntax-download", list(id = id("download_regression_syntax")))
      return(invisible(NULL))
    }
    if (!request$action %in% c("apply", "run")) stop("Unknown command action.")
    spec <- read_spec(request)
    draft(request$text)
    if (request$action == "run" && !is.null(scope_run_id)) {
      apply_fn(spec)
      analysis_scope_run(session, scope_run_id, function() run_fn(prepare_fn(spec)))
    } else {
      if (request$action == "run") prepared <- prepare_fn(spec)
      apply_fn(spec)
      if (request$action == "run") run_fn(prepared)
    }
    removeModal()
    if (request$action == "apply" && isTRUE(notify_apply)) showNotification("분석 명령어를 메뉴에 반영했습니다. / Applied to menu.")
  }))
  invisible(draft)
}
