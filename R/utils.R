# Auto-extracted shared functions for StatEdu Studio.

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

normalize_app_language <- function(language) {
  value <- tolower(as.character(language %||% "ko")[[1]])
  aliases <- unlist(unname(lapply(statedu_language_registry(), function(spec) {
    stats::setNames(rep(spec$code, length(spec$aliases %||% character(0))), spec$aliases %||% character(0))
  })), use.names = TRUE)
  if (value %in% names(aliases)) {
    value <- unname(aliases[[value]])
  }
  if (value %in% statedu_supported_languages()) {
    return(value)
  }
  "ko"
}

statedu_builtin_language_registry <- function() {
  list(
    ko = list(
      code = "ko",
      names = list(en = "Korean", ko = statedu_utf8("ed959ceab5adec96b4")),
      aliases = c("korean", "korea", "kr", statedu_utf8("ed959ceab5adec96b4"))
    ),
    en = list(
      code = "en",
      names = list(en = "English", ko = statedu_utf8("ec9881ec96b4")),
      aliases = c("english", "eng")
    )
  )
}

statedu_i18n_dir <- function() {
  custom_dir <- Sys.getenv("STATEDU_I18N_DIR", "")
  if (nzchar(custom_dir)) {
    return(normalizePath(custom_dir, winslash = "/", mustWork = FALSE))
  }
  app_dir <- Sys.getenv("STATEDU_APP_DIR", "")
  if (nzchar(app_dir)) {
    candidate <- file.path(app_dir, "i18n")
    if (dir.exists(candidate)) {
      return(normalizePath(candidate, winslash = "/", mustWork = FALSE))
    }
  }
  repo_dir <- file.path(getwd(), "i18n")
  if (dir.exists(repo_dir)) {
    return(normalizePath(repo_dir, winslash = "/", mustWork = FALSE))
  }
  file.path(getwd(), "i18n")
}

statedu_read_json_file <- function(path, fallback = list()) {
  if (!nzchar(path) || !file.exists(path) || !requireNamespace("jsonlite", quietly = TRUE)) {
    return(fallback)
  }
  value <- tryCatch({
    bytes <- readBin(path, what = "raw", n = file.info(path)$size)
    text <- rawToChar(bytes)
    Encoding(text) <- "UTF-8"
    jsonlite::fromJSON(text, simplifyVector = FALSE)
  }, error = function(e) fallback)
  if (is.list(value)) value else fallback
}

statedu_language_registry <- local({
  cache <- NULL
  function(refresh = FALSE) {
    if (!isTRUE(refresh) && !is.null(cache)) {
      return(cache)
    }
    registry <- statedu_builtin_language_registry()
    path <- file.path(statedu_i18n_dir(), "languages.json")
    external <- statedu_read_json_file(path, list())
    languages <- external$languages %||% list()
    for (spec in languages) {
      if (!is.list(spec)) next
      code <- tolower(as.character(spec$code %||% "")[[1]])
      if (!nzchar(code) || !grepl("^[a-z][a-z0-9_-]*$", code)) next
      names <- spec$names %||% list(en = code)
      aliases <- unique(tolower(as.character(spec$aliases %||% character(0))))
      aliases <- aliases[nzchar(aliases)]
      registry[[code]] <- list(code = code, names = names, aliases = aliases)
    }
    cache <<- registry
    registry
  }
})

statedu_supported_languages <- function() {
  names(statedu_language_registry())
}

statedu_default_language <- function() {
  "ko"
}

statedu_language_display_name <- function(code, language = statedu_initial_language()) {
  code <- normalize_app_language(code)
  language <- normalize_app_language(language)
  spec <- statedu_language_registry()[[code]]
  if (is.null(spec)) {
    return(code)
  }
  names <- spec$names %||% list(en = code)
  value <- if (language %in% names(names)) {
    names[[language]]
  } else if ("en" %in% names(names)) {
    names[["en"]]
  } else {
    code
  }
  as.character(value)
}

statedu_locale_overlay <- function(code) {
  code <- normalize_app_language(code)
  path <- file.path(statedu_i18n_dir(), paste0(code, ".json"))
  values <- statedu_read_json_file(path, list())
  translations <- values$translations %||% values
  if (!is.list(translations)) {
    return(list())
  }
  translations
}

statedu_locale_overlays <- function() {
  languages <- statedu_supported_languages()
  stats::setNames(lapply(languages, statedu_locale_overlay), languages)
}

statedu_query_value <- function(query_string, key) {
  if (is.null(query_string) || !nzchar(query_string)) {
    return("")
  }
  query_string <- as.character(query_string)[[1]]
  query_string <- sub("^\\?", "", query_string)
  query_string <- sub("#.*$", "", query_string)
  parts <- strsplit(query_string, "&", fixed = TRUE)[[1]]
  for (part in parts) {
    pair <- strsplit(part, "=", fixed = TRUE)[[1]]
    if (length(pair) >= 1 && identical(utils::URLdecode(pair[[1]]), key)) {
      return(utils::URLdecode(paste(pair[-1], collapse = "=")))
    }
  }
  ""
}

statedu_url_query <- function(value) {
  if (is.null(value) || !length(value)) {
    return("")
  }
  value <- as.character(value)[[1]]
  if (!nzchar(value)) {
    return("")
  }
  value <- sub("#.*$", "", value)
  if (grepl("\\?", value)) {
    value <- sub("^[^?]*\\?", "", value)
  }
  sub("^\\?", "", value)
}

statedu_query_language <- function(query_string) {
  language <- statedu_query_value(query_string, "lang")
  if (!nzchar(language)) {
    language <- statedu_query_value(query_string, "language")
  }
  language
}

statedu_request_language <- function(request = NULL) {
  if (is.null(request)) {
    return("")
  }
  request_value <- function(key) request[[key]] %||% ""
  query_candidates <- c(
    request_value("QUERY_STRING"),
    statedu_url_query(request_value("REQUEST_URI")),
    statedu_url_query(request_value("HTTP_REFERER")),
    statedu_url_query(request_value("HTTP_ORIGIN"))
  )
  for (query_string in query_candidates) {
    language <- statedu_query_language(query_string)
    if (nzchar(language)) {
      return(language)
    }
  }
  ""
}

statedu_user_settings_dir <- function() {
  path <- Sys.getenv("STATEDU_USER_SETTINGS_DIR", "")
  if (!nzchar(path)) {
    local_app_data <- Sys.getenv("LOCALAPPDATA", "")
    path <- if (nzchar(local_app_data)) {
      file.path(local_app_data, "StatEdu Studio", "settings")
    } else {
      file.path(path.expand("~"), ".statedu-studio", "settings")
    }
  }
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

statedu_setting_file_path <- function(env_var, filename) {
  path <- Sys.getenv(env_var, "")
  if (!nzchar(path)) {
    path <- file.path(statedu_user_settings_dir(), filename)
  }
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

statedu_language_file_path <- function() {
  statedu_setting_file_path("STATEDU_APP_LANGUAGE_FILE", "app-language.txt")
}

statedu_read_persisted_language <- function() {
  path <- statedu_language_file_path()
  if (!nzchar(path) || !file.exists(path)) {
    return("")
  }
  value <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8", n = 1), error = function(e) "")
  if (!length(value) || !nzchar(value[[1]])) {
    return("")
  }
  normalize_app_language(value[[1]])
}

statedu_write_persisted_language <- function(language) {
  path <- statedu_language_file_path()
  if (!nzchar(path)) {
    return(invisible(FALSE))
  }
  language <- normalize_app_language(language)
  result <- tryCatch({
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines(language, path, useBytes = TRUE)
    TRUE
  }, error = function(e) FALSE)
  invisible(result)
}

statedu_result_zoom_default <- function() {
  150L
}

normalize_result_zoom_percent <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(statedu_result_zoom_default())
  }
  number <- suppressWarnings(as.numeric(value[[1]]))
  if (!is.finite(number)) {
    return(statedu_result_zoom_default())
  }
  as.integer(max(80, min(200, round(number))))
}

statedu_result_zoom_file_path <- function() {
  statedu_setting_file_path("STATEDU_RESULT_ZOOM_FILE", "result-zoom-percent.txt")
}

statedu_read_persisted_result_zoom <- function() {
  path <- statedu_result_zoom_file_path()
  if (!nzchar(path) || !file.exists(path)) {
    return(NA_integer_)
  }
  value <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8", n = 1), error = function(e) "")
  if (!length(value) || !nzchar(value[[1]])) {
    return(NA_integer_)
  }
  normalize_result_zoom_percent(value[[1]])
}

statedu_write_persisted_result_zoom <- function(value) {
  path <- statedu_result_zoom_file_path()
  if (!nzchar(path)) {
    return(invisible(FALSE))
  }
  zoom <- normalize_result_zoom_percent(value)
  result <- tryCatch({
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines(as.character(zoom), path, useBytes = TRUE)
    TRUE
  }, error = function(e) FALSE)
  invisible(result)
}

statedu_initial_result_zoom <- function() {
  zoom <- getOption("statedu.result_zoom_percent", NA_integer_)
  zoom_value <- if (!is.null(zoom) && length(zoom) > 0) {
    suppressWarnings(as.numeric(zoom[[1]]))
  } else {
    NA_real_
  }
  if (!is.na(zoom_value)) {
    return(normalize_result_zoom_percent(zoom))
  }
  persisted_preferences <- statedu_read_persisted_preferences()
  if (!is.null(persisted_preferences$result_zoom_percent)) {
    return(normalize_result_zoom_percent(persisted_preferences$result_zoom_percent))
  }
  zoom <- statedu_read_persisted_result_zoom()
  if (!is.na(zoom)) {
    return(normalize_result_zoom_percent(zoom))
  }
  env_zoom <- Sys.getenv("STATEDU_RESULT_ZOOM_PERCENT", "")
  if (nzchar(env_zoom)) {
    return(normalize_result_zoom_percent(env_zoom))
  }
  statedu_result_zoom_default()
}

statedu_preferences_file_path <- function() {
  statedu_setting_file_path("STATEDU_APP_PREFERENCES_FILE", "app-preferences.json")
}

statedu_default_preferences <- function() {
  list(
    result_zoom_percent = statedu_result_zoom_default(),
    output_decimal_digits = 3L,
    p_value_format = "apa",
    multiple_correction_default = "holm",
    selected_variables_only_default = TRUE,
    default_save_dir = ""
  )
}

statedu_read_persisted_preferences <- function() {
  path <- statedu_preferences_file_path()
  if (!nzchar(path) || !file.exists(path)) {
    return(list())
  }
  preferences <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) list())
  if (!is.list(preferences)) {
    return(list())
  }
  preferences
}

statedu_write_persisted_preferences <- function(preferences) {
  path <- statedu_preferences_file_path()
  if (!nzchar(path)) {
    return(invisible(FALSE))
  }
  preferences <- preferences %||% list()
  result <- tryCatch({
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines(jsonlite::toJSON(preferences, auto_unbox = TRUE, pretty = TRUE), path, useBytes = TRUE)
    TRUE
  }, error = function(e) FALSE)
  invisible(result)
}

normalize_output_decimal_digits <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(statedu_default_preferences()$output_decimal_digits)
  }
  number <- suppressWarnings(as.numeric(value[[1]]))
  if (!is.finite(number)) {
    return(statedu_default_preferences()$output_decimal_digits)
  }
  as.integer(max(0, min(5, round(number))))
}

normalize_p_value_format <- function(value) {
  value <- tolower(as.character(value %||% "apa")[[1]])
  if (value %in% c("leading_zero", "leading-zero", "zero", "0")) {
    return("leading_zero")
  }
  "apa"
}

normalize_multiple_correction_default <- function(value) {
  value <- tolower(as.character(value %||% "holm")[[1]])
  if (value %in% c("bonferroni", "holm")) {
    return(value)
  }
  "holm"
}

normalize_selected_variables_only_default <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(TRUE)
  }
  value <- value[[1]]
  if (is.logical(value)) {
    return(isTRUE(value))
  }
  !tolower(as.character(value)) %in% c("0", "false", "no", "off")
}

normalize_default_save_dir <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return("")
  }
  value <- trimws(as.character(value[[1]] %||% ""))
  if (!nzchar(value)) {
    return("")
  }
  normalizePath(path.expand(value), winslash = "/", mustWork = FALSE)
}

statedu_initial_preferences <- function() {
  defaults <- statedu_default_preferences()
  persisted <- statedu_read_persisted_preferences()
  preferences <- utils::modifyList(defaults, persisted)
  preferences$result_zoom_percent <- normalize_result_zoom_percent(
    getOption("statedu.result_zoom_percent", preferences$result_zoom_percent)
  )
  preferences$output_decimal_digits <- normalize_output_decimal_digits(
    getOption("statedu.output_decimal_digits", preferences$output_decimal_digits)
  )
  preferences$p_value_format <- normalize_p_value_format(
    getOption("statedu.p_value_format", preferences$p_value_format)
  )
  preferences$multiple_correction_default <- normalize_multiple_correction_default(
    getOption("statedu.multiple_correction_default", preferences$multiple_correction_default)
  )
  preferences$selected_variables_only_default <- normalize_selected_variables_only_default(
    getOption("statedu.selected_variables_only_default", preferences$selected_variables_only_default)
  )
  preferences$default_save_dir <- normalize_default_save_dir(
    getOption("statedu.default_save_dir", preferences$default_save_dir)
  )
  preferences
}

statedu_apply_preferences <- function(preferences = statedu_initial_preferences()) {
  preferences <- preferences %||% statedu_default_preferences()
  options(
    statedu.result_zoom_percent = normalize_result_zoom_percent(preferences$result_zoom_percent),
    statedu.output_decimal_digits = normalize_output_decimal_digits(preferences$output_decimal_digits),
    statedu.p_value_format = normalize_p_value_format(preferences$p_value_format),
    statedu.multiple_correction_default = normalize_multiple_correction_default(preferences$multiple_correction_default),
    statedu.selected_variables_only_default = normalize_selected_variables_only_default(preferences$selected_variables_only_default),
    statedu.default_save_dir = normalize_default_save_dir(preferences$default_save_dir)
  )
  invisible(preferences)
}

statedu_save_preferences <- function(preferences) {
  preferences <- statedu_apply_preferences(preferences)
  statedu_write_persisted_preferences(list(
    result_zoom_percent = getOption("statedu.result_zoom_percent", statedu_result_zoom_default()),
    output_decimal_digits = getOption("statedu.output_decimal_digits", 3L),
    p_value_format = getOption("statedu.p_value_format", "apa"),
    multiple_correction_default = getOption("statedu.multiple_correction_default", "holm"),
    selected_variables_only_default = getOption("statedu.selected_variables_only_default", TRUE),
    default_save_dir = getOption("statedu.default_save_dir", "")
  ))
}

statedu_output_decimal_digits <- function() {
  normalize_output_decimal_digits(getOption("statedu.output_decimal_digits", statedu_initial_preferences()$output_decimal_digits))
}

statedu_p_value_format <- function() {
  normalize_p_value_format(getOption("statedu.p_value_format", statedu_initial_preferences()$p_value_format))
}

statedu_multiple_correction_default <- function() {
  normalize_multiple_correction_default(getOption("statedu.multiple_correction_default", statedu_initial_preferences()$multiple_correction_default))
}

statedu_selected_variables_only_default <- function() {
  normalize_selected_variables_only_default(getOption("statedu.selected_variables_only_default", statedu_initial_preferences()$selected_variables_only_default))
}

statedu_default_save_dir <- function() {
  normalize_default_save_dir(getOption("statedu.default_save_dir", statedu_initial_preferences()$default_save_dir))
}

statedu_initial_language <- function(request = NULL) {
  language <- if (is.null(request)) "" else statedu_request_language(request)
  if (!nzchar(language)) {
    language <- getOption("statedu.app_language", "")
  }
  if (!nzchar(language)) {
    language <- statedu_read_persisted_language()
  }
  if (!nzchar(language)) {
    language <- Sys.getenv("STATEDU_APP_LANGUAGE", "ko")
  }
  normalize_app_language(language)
}

statedu_current_language <- function(language_fn = NULL, request = NULL) {
  language <- if (is.function(language_fn)) {
    tryCatch(language_fn(), error = function(e) "")
  } else {
    ""
  }
  if (!nzchar(as.character(language %||% ""))) {
    language <- statedu_initial_language(request)
  }
  language <- normalize_app_language(language)
  options(statedu.app_language = language)
  language
}

statedu_text <- function(language, en, ko = en) {
  if (identical(normalize_app_language(language), "ko")) ko else en
}

statedu_t <- function(key, language = statedu_initial_language(), fallback = NULL) {
  if (exists("statedu_translate", mode = "function")) {
    return(statedu_translate(key, language, fallback))
  }
  fallback %||% as.character(key)
}

statedu_measurement_choices <- function(language = statedu_initial_language()) {
  if (identical(normalize_app_language(language), "ko")) {
    return(stats::setNames(
      c("binary", "category", "ordered", "continuous"),
      c(
        statedu_utf8("ec9db4ebb684ed9895"),
        statedu_utf8("ebb294eca3bced9895"),
        statedu_utf8("ec889cec849ced9895"),
        statedu_utf8("ec97b0ec868ded9895")
      )
    ))
  }
  c("binary" = "binary", "category" = "category", "ordinal" = "ordered", "continuous" = "continuous")
}

statedu_measurement_label <- function(value, language = statedu_initial_language()) {
  value <- tolower(as.character(value %||% ""))
  if (identical(value, "ordinal")) value <- "ordered"
  if (identical(value, "nominal")) value <- "category"
  choices <- statedu_measurement_choices(language)
  match_index <- match(value, unname(choices))
  if (is.na(match_index)) {
    return(if (identical(value, "ordered")) "ordinal" else value)
  }
  names(choices)[[match_index]]
}

statedu_utf8 <- function(hex) {
  pairs <- substring(hex, seq(1, nchar(hex), 2), seq(2, nchar(hex), 2))
  value <- rawToChar(as.raw(strtoi(pairs, 16L)))
  Encoding(value) <- "UTF-8"
  value
}

statedu_ui_label <- function(key, language = statedu_initial_language()) {
  statedu_t(paste0("ui.", key), language, as.character(key))
}
statedu_timing_enabled <- function() {
  !identical(tolower(Sys.getenv("STATEDU_TIMING", "1")), "0")
}

statedu_log_timing <- function(label, start, detail = "") {
  if (!isTRUE(statedu_timing_enabled())) {
    return(invisible(FALSE))
  }
  elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  suffix <- if (nzchar(as.character(detail %||% ""))) paste0(" ", detail) else ""
  message(sprintf("[StatEdu timing] %s: %.3fs%s", label, elapsed, suffix))
  invisible(TRUE)
}

statedu_time_expr <- function(label, expr, detail = "") {
  start <- Sys.time()
  on.exit(statedu_log_timing(label, start, detail), add = TRUE)
  force(expr)
}

named_value <- function(x, name, default = "") {
  name <- as.character(name %||% "")
  name <- if (length(name) == 0 || is.na(name[[1]])) "" else name[[1]]
  if (is.null(x) || !nzchar(name) || is.null(names(x))) {
    return(default)
  }
  index <- match(name, names(x))
  if (is.na(index) || index < 1 || index > length(x)) {
    return(default)
  }
  value <- x[[index]]
  if (is.null(value) || length(value) == 0 || is.na(value[[1]])) {
    return(default)
  }
  as.character(value[[1]])
}

named_override_log_text <- function(values = character(0)) {
  values <- values %||% character(0)
  paste(sprintf("%s=%s", names(values), unname(values)), collapse = ", ")
}

format_p <- function(p) {
  if (length(p) == 0 || is.null(p[[1]]) || is.na(p[[1]])) {
    return(NA_character_)
  }
  leading_zero <- identical(statedu_p_value_format(), "leading_zero")
  if (is.character(p[[1]])) {
    text <- trimws(p[[1]])
    if (!nzchar(text)) return("")
    less_than <- startsWith(text, "<")
    value_text <- sub("^<", "", text)
    value <- suppressWarnings(as.numeric(sub("^\\.", "0.", value_text)))
    if (is.na(value)) return(text)
    if (isTRUE(less_than) && value <= .001) {
      return(if (isTRUE(leading_zero)) "<0.001" else "<.001")
    }
  } else {
    value <- suppressWarnings(as.numeric(p[[1]]))
  }
  if (is.na(value)) return(NA_character_)
  if (value < .001) return(if (isTRUE(leading_zero)) "<0.001" else "<.001")
  text <- sprintf("%.3f", value)
  if (isTRUE(leading_zero)) text else sub("^0\\.", ".", text)
}

format_decimal3 <- function(x) {
  if (is.na(x)) return("")
  digits <- statedu_output_decimal_digits()
  text <- sprintf(paste0("%.", digits, "f"), x)
  text <- sub("^-0\\.", "-.", text)
  sub("^0\\.", ".", text)
}

format_decimal2 <- function(x) {
  if (is.na(x)) return("")
  text <- sprintf("%.2f", x)
  text <- sub("^-0\\.", "-.", text)
  sub("^0\\.", ".", text)
}

format_effect_size <- function(x) {
  format_decimal3(x)
}

analysis_bind_rows <- function(rows) {
  rows <- Filter(function(row) is.data.frame(row) && nrow(row) > 0, rows %||% list())
  if (length(rows) == 0) {
    return(data.frame(stringsAsFactors = FALSE))
  }
  columns <- unique(unlist(lapply(rows, names), use.names = FALSE))
  rows <- lapply(rows, function(row) {
    missing_columns <- setdiff(columns, names(row))
    for (column in missing_columns) {
      row[[column]] <- ""
    }
    row <- row[, columns, drop = FALSE]
    rownames(row) <- NULL
    row
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

analysis_message_table <- function(messages, column = "Warning") {
  messages <- unique(as.character(messages %||% character(0)))
  messages <- messages[!is.na(messages) & nzchar(messages)]
  if (length(messages) == 0) {
    return(data.frame(stringsAsFactors = FALSE))
  }
  stats::setNames(data.frame(messages, stringsAsFactors = FALSE, check.names = FALSE), column)
}

analysis_warning_table <- function(messages) {
  analysis_message_table(messages, "Warning")
}

analysis_guard_row <- function(type, target, predictors = "", n = NA_integer_, message = "") {
  data.frame(
    Type = as.character(type %||% ""),
    Target = as.character(target %||% ""),
    `Independent variables` = as.character(predictors %||% ""),
    N = if (is.na(n)) "" else as.character(n),
    Message = as.character(message %||% ""),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

analysis_has_rows <- function(table) {
  is.data.frame(table) && nrow(table) > 0
}

register_dual_transfer_drop_observer <- function(
  input,
  session,
  available_id,
  selected_id,
  selected_values,
  all_values_fn,
  active_list = NULL,
  mark_settings_dirty = NULL,
  validate_next = NULL,
  after_change = NULL
) {
  observeEvent(input$analysis_transfer_drop, {
    drop <- input$analysis_transfer_drop
    ids <- c(available_id, selected_id)
    source <- as.character(drop$source %||% "")
    target <- as.character(drop$target %||% "")
    values <- unique(as.character(drop$values %||% character(0)))
    values <- values[nzchar(values)]
    if (!source %in% ids || !target %in% ids || identical(source, target) || length(values) == 0) {
      return()
    }

    all_values <- as.character(all_values_fn() %||% character(0))
    current <- intersect(as.character(selected_values() %||% character(0)), all_values)
    if (identical(target, selected_id)) {
      chosen <- intersect(values, all_values)
      next_values <- c(current, setdiff(chosen, current))
    } else {
      chosen <- intersect(values, current)
      next_values <- setdiff(current, chosen)
    }
    if (length(chosen) == 0 || identical(next_values, current)) {
      return()
    }
    if (!is.null(validate_next) && !isTRUE(validate_next(next_values, target, chosen))) {
      return()
    }

    selected_values(next_values)
    if (!is.null(active_list)) active_list(target)
    if (!is.null(after_change)) after_change(target, chosen, next_values)
    if (!is.null(mark_settings_dirty)) mark_settings_dirty()
    if (!is.null(session)) {
      session$sendCustomMessage(
        "easyflow-clear-transfer-selection",
        list(inputIds = ids)
      )
    }
  }, ignoreInit = TRUE)

  invisible(TRUE)
}

register_dual_transfer_doubleclick_observers <- function(
  input,
  available_id,
  selected_id,
  selected_values,
  all_values_fn,
  active_list = NULL,
  mark_settings_dirty = NULL,
  validate_next = NULL
) {
  observeEvent(input[[paste0(available_id, "_doubleclick")]], {
    event <- input[[paste0(available_id, "_doubleclick")]]
    value <- as.character(event$value %||% "")
    all_values <- as.character(all_values_fn() %||% character(0))
    chosen <- intersect(value, all_values)
    if (length(chosen) == 0) {
      return()
    }
    current <- intersect(as.character(selected_values() %||% character(0)), all_values)
    next_values <- c(current, setdiff(chosen, current))
    if (identical(next_values, current)) {
      return()
    }
    if (!is.null(validate_next) && !isTRUE(validate_next(next_values, selected_id, chosen))) {
      return()
    }
    selected_values(next_values)
    if (!is.null(active_list)) active_list(selected_id)
    if (!is.null(mark_settings_dirty)) mark_settings_dirty()
  }, ignoreInit = TRUE)

  invisible(TRUE)
}

stat_chisq_label <- function(with_p = FALSE) {
  if (isTRUE(with_p)) "x\u00B2(p)" else "x\u00B2"
}

default_seed <- function() {
  as.integer(format(Sys.Date(), "%Y%m%d"))
}

bootstrap_percentile_ci <- function(values, probs = c(.025, .975)) {
  values <- as.numeric(values)
  values <- values[is.finite(values)]
  if (length(values) == 0L) {
    return(rep(NA_real_, length(probs)))
  }
  stats::quantile(values, probs = probs, na.rm = TRUE, names = FALSE, type = 6)
}

bootstrap_bias_corrected_ci <- function(point, values, conf = .95) {
  point <- suppressWarnings(as.numeric(point)[[1]])
  values <- as.numeric(values)
  values <- values[is.finite(values)]
  if (length(values) == 0L || !is.finite(point)) {
    return(rep(NA_real_, 2L))
  }
  alpha <- (1 - conf) / 2
  prop_less <- mean(values < point)
  prop_less <- min(max(prop_less, 0.5 / length(values)), 1 - 0.5 / length(values))
  z0 <- stats::qnorm(prop_less)
  probs <- stats::pnorm(2 * z0 + stats::qnorm(c(alpha, 1 - alpha)))
  bootstrap_percentile_ci(values, probs)
}

bootstrap_ci <- function(point, values, conf = .95, method = "bias_corrected") {
  method <- as.character(method %||% "bias_corrected")[[1]]
  if (identical(method, "percentile")) {
    alpha <- (1 - conf) / 2
    return(bootstrap_percentile_ci(values, c(alpha, 1 - alpha)))
  }
  bootstrap_bias_corrected_ci(point, values, conf = conf)
}

bootstrap_ci_method_label <- function(method = "bias_corrected") {
  method <- as.character(method %||% "bias_corrected")[[1]]
  if (identical(method, "percentile")) "Percentile" else "Bias-corrected"
}

has_request_nonce <- function(request) {
  !is.null(request) && !is.null(request$nonce)
}

setup_option_checked <- function(value, default = FALSE) {
  if (is.null(value)) {
    return(isTRUE(default))
  }
  isTRUE(value)
}

data_editor_variable_names <- function(data, variable_info = NULL) {
  variables <- names(data %||% data.frame())
  if (!is.data.frame(variable_info) || !"name" %in% names(variable_info)) {
    return(variables)
  }
  scoped <- intersect(variables, as.character(variable_info$name %||% character(0)))
  if (length(scoped) == 0L && length(variables) > 0L && nrow(variable_info) == 0L) {
    return(character(0))
  }
  scoped
}
