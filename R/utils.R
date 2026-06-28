# Auto-extracted shared functions for StatEdu Studio.

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

normalize_app_language <- function(language) {
  value <- tolower(as.character(language %||% "ko")[[1]])
  if (value %in% c("en", "english")) {
    return("en")
  }
  "ko"
}

statedu_query_value <- function(query_string, key) {
  if (is.null(query_string) || !nzchar(query_string)) {
    return("")
  }
  query_string <- sub("^\\?", "", query_string)
  parts <- strsplit(query_string, "&", fixed = TRUE)[[1]]
  for (part in parts) {
    pair <- strsplit(part, "=", fixed = TRUE)[[1]]
    if (length(pair) >= 1 && identical(utils::URLdecode(pair[[1]]), key)) {
      return(utils::URLdecode(paste(pair[-1], collapse = "=")))
    }
  }
  ""
}

statedu_request_language <- function(request = NULL) {
  query_string <- request$QUERY_STRING %||% ""
  statedu_query_value(query_string, "lang")
}

statedu_initial_language <- function(request = NULL) {
  language <- if (is.null(request)) "" else statedu_request_language(request)
  if (!nzchar(language)) {
    language <- getOption("statedu.app_language", "")
  }
  if (!nzchar(language)) {
    language <- Sys.getenv("STATEDU_APP_LANGUAGE", Sys.getenv("EASYFLOW_APP_LANGUAGE", "ko"))
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
  h <- statedu_utf8
  labels <- c(
    data = paste0("Data|", h("eb8db0ec9db4ed84b0")),
    data_editor = paste0("Data Editor|", h("eb8db0ec9db4ed84b020ed8eb8eca791")),
    calculator = paste0("Calculator|", h("eab384ec82b0eab8b0")),
    analysis = paste0("Analysis|", h("ebb684ec849d")),
    sample_size = paste0("Sample Size|", h("ed919cebb3b8ec8898")),
    effect_size = paste0("Effect Size|", h("ed9aa8eab3bced81aceab8b0")),
    result = paste0("Result|", h("eab2b0eab3bc")),
    about = paste0("About|", h("eca095ebb3b4")),
    preferences = paste0("Preferences|", h("ed9998eab2bdec84a4eca095")),
    frequencies = paste0("Frequencies / Descriptives|", h("ebb988eb8f84ebb684ec849d202f20eab8b0ec88a0ed86b5eab384")),
    crosstabs = paste0("Cross-tabulation Analysis|", h("eab590ecb0a8ebb684ec849d")),
    ttest_anova = "t-test / ANOVA|t-test / ANOVA",
    paired = paste0("Paired test|", h("eb8c80ec9d91ed919cebb3b820eab280eca095")),
    ancova = "ANCOVA|ANCOVA",
    nonparametric = paste0("Nonparametric Tests|", h("ebb984ebaaa8ec889820eab280eca095")),
    nonparametric_paired = paste0("Nonparametric Paired|", h("eb8c80ec9d9120ebb984ebaaa8ec889820eab280eca095")),
    correlation = paste0("Correlation|", h("ec8381eab480ebb684ec849d")),
    reliability = paste0("Reliability|", h("ec8ba0eba2b0eb8f84")),
    factor_analysis = paste0("Factor Analysis|", h("ec9a94ec9db8ebb684ec849d")),
    pca = paste0("Principal Components|", h("eca3bcec84b1ebb684ebb684ec849d")),
    regression = paste0("Regression|", h("ed9a8ceab780ebb684ec849d")),
    glm = paste0("Generalized Linear Model (GLM)|", h("ec9dbcebb098ed9994ec84a0ed9895ebaaa8ed989528474c4d29")),
    logistic = paste0("Logistic Regression|", h("eba19ceca780ec8aa4ed8bb120ed9a8ceab780")),
    longitudinal = paste0("Longitudinal / Panel Models|", h("eca285eb8ba8202f20ed8ca8eb849020ebaaa8ed9895")),
    open_data_file = paste0("Open data file|", h("eb8db0ec9db4ed84b020ed8c8cec9dbc20ec97b4eab8b0")),
    load_settings = paste0("Load settings|", h("ec84a4eca09520ebb688eb9facec98a4eab8b0")),
    save_settings = paste0("Save settings|", h("ec84a4eca09520eca080ec9ea5")),
    reset_settings = paste0("Reset settings|", h("ec84a4eca09520ecb488eab8b0ed9994")),
    save_html = paste0("Save HTML|", h("48544d4c20eca080ec9ea5")),
    save_pdf = paste0("Save PDF|", h("50444620eca080ec9ea5")),
    save_fig = paste0("Save fig|", h("eab7b8eba6bc20eca080ec9ea5")),
    save_excel = paste0("Save Excel|", h("457863656c20eca080ec9ea5")),
    save_word = paste0("Save Word|", h("576f726420eca080ec9ea5")),
    add_result = paste0("Add result|", h("eab2b0eab3bc20ecb694eab080")),
    run_analysis = paste0("Run analysis|", h("ebb684ec849d20ec8ba4ed9689")),
    run_logistic = paste0("Run logistic|", h("eba19ceca780ec8aa4ed8bb120ec8ba4ed9689")),
    calculate = paste0("Calculate|", h("eab384ec82b0")),
    download_csv = paste0("Download CSV|", h("43535620eb8ba4ec9ab4eba19ceb939c")),
    select_variable = paste0("Select variable|", h("ebb380ec889820ec84a0ed839d")),
    data_preview = paste0("Data Preview|", h("eb8db0ec9db4ed84b020ebafb8eba6acebb3b4eab8b0")),
    view_selected_data = paste0("View selected data|", h("ec84a0ed839d20eb8db0ec9db4ed84b020ebb3b4eab8b0")),
    reference = paste0("Reference|", h("eab8b0eca480")),
    exclusion_rules = paste0("Exclusion rules|", h("eca09cec99b820eab79cecb999")),
    output = paste0("Output|", h("ecb69ceba0a5")),
    outputs = paste0("Outputs|", h("ecb69ceba0a520ebb380ec8898")),
    initial_values = paste0("Initial values|", h("ecb488eab8b0eab092")),
    initial_score = paste0("Initial score|", h("ecb488eab8b020eca090ec8898")),
    value_set = paste0("Value set|", h("eab09220ec84b8ed8ab8")),
    type = paste0("Type|", h("ebb684eba598")),
    units = paste0("Units|", h("eb8ba8ec9c84")),
    coding = paste0("Coding|", h("ecbd94eb94a9")),
    formula = paste0("Formula|", h("eab3b5ec8b9d")),
    criteria_unit = paste0("Criteria / Unit|", h("eab8b0eca480202f20eb8ba8ec9c84")),
    criteria_population = paste0("Criteria / population|", h("eab8b0eca480202f20eca791eb8ba8")),
    reference_cutoffs = paste0("Reference cutoffs|", h("eab8b0eca480eab092")),
    waist_unit = paste0("Waist unit|", h("ed9788eba6aceb9198eba08820eb8ba8ec9c84")),
    glucose_unit = paste0("Glucose unit|", h("ed9888eb8bb920eb8ba8ec9c84")),
    lipid_unit = paste0("Lipid unit|", h("eca780eca78820eb8ba8ec9c84")),
    overview = paste0("Overview|", h("eab09cec9a94")),
    user_guide = paste0("User Guide|", h("ec82acec9aa9ec9e9020ec9588eb82b4ec849c")),
    analyses = paste0("Analyses|", h("ebb684ec849d")),
    method_notes = paste0("Method Notes|", h("ebb0a9ebb295eba1a020eb85b8ed8ab8")),
    validation = paste0("Validation|", h("eab280eca69d")),
    version_history = paste0("Version History|", h("ebb284eca08420ec9db4eba0a5")),
    source_license = paste0("Source & License|", h("ec868cec8aa420ebb08f20eb9dbcec9db4ec84a0ec8aa4")),
    open_source_licenses = paste0("Open Source Licenses|", h("ec98a4ed9488ec868cec8aa420eb9dbcec9db4ec84a0ec8aa4"))
  )
  value <- labels[[key]] %||% as.character(key)
  parts <- strsplit(value, "\\|", fixed = FALSE)[[1]]
  statedu_text(language, parts[[1]], parts[[length(parts)]])
}
easyflow_timing_enabled <- function() {
  !identical(tolower(Sys.getenv("EASYFLOW_TIMING", "1")), "0")
}

easyflow_log_timing <- function(label, start, detail = "") {
  if (!isTRUE(easyflow_timing_enabled())) {
    return(invisible(FALSE))
  }
  elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  suffix <- if (nzchar(as.character(detail %||% ""))) paste0(" ", detail) else ""
  message(sprintf("[StatEdu timing] %s: %.3fs%s", label, elapsed, suffix))
  invisible(TRUE)
}

easyflow_time_expr <- function(label, expr, detail = "") {
  start <- Sys.time()
  on.exit(easyflow_log_timing(label, start, detail), add = TRUE)
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
  if (is.character(p[[1]])) {
    text <- trimws(p[[1]])
    if (!nzchar(text)) return("")
    less_than <- startsWith(text, "<")
    value_text <- sub("^<", "", text)
    value <- suppressWarnings(as.numeric(sub("^\\.", "0.", value_text)))
    if (is.na(value)) return(text)
    if (isTRUE(less_than) && value <= .001) {
      return("<.001")
    }
  } else {
    value <- suppressWarnings(as.numeric(p[[1]]))
  }
  if (is.na(value)) return(NA_character_)
  if (value < .001) return("<.001")
  sub("^0\\.", ".", sprintf("%.3f", value))
}

format_decimal3 <- function(x) {
  if (is.na(x)) return("")
  text <- sprintf("%.3f", x)
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

has_request_nonce <- function(request) {
  !is.null(request) && !is.null(request$nonce)
}

setup_option_checked <- function(value, default = FALSE) {
  if (is.null(value)) {
    return(isTRUE(default))
  }
  isTRUE(value)
}
