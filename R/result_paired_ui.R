# Paired test result UI.

paired_main_table <- function(table) {
  if (is.data.frame(table)) {
    attr(table, "result_table_role") <- "main"
    attr(table, "result_table_language") <- result_main_table_language()
  }
  table
}

paired_appendix_text <- function(text, language = NULL) {
  language <- result_appendix_table_language(language)
  text <- as.character(text %||% "")
  if (!nzchar(text)) return(text)
  if (!language %in% c("en", "ko")) {
    normality_parts <- regmatches(text, regexec("^skew=(.*), kurtosis=(.*)$", text))[[1]]
    if (length(normality_parts) == 3L) return(sprintf(statedu_localized_text(language, "skew=%s, kurtosis=%s"), normality_parts[2], normality_parts[3]))
    outlier_parts <- regmatches(text, regexec("^([0-9]+) detected \\(IDs: (.*)\\)$", text, perl = TRUE))[[1]]
    if (length(outlier_parts) == 3L) return(sprintf(statedu_localized_text(language, "%s detected (IDs: %s)"), outlier_parts[2], outlier_parts[3]))
    if (grepl("^[0-9]+ detected$", text)) return(sprintf(statedu_localized_text(language, "%s detected"), sub(" detected$", "", text)))
    templates <- c(
      "Skipped because variable(s) were not found in the active data: " = "Skipped because variable(s) were not found in the active data: %s.",
      "Skipped because paired variables have different measurement levels (" = "Skipped because paired variables have different measurement levels (%s).",
      "Variable(s) were not found in the active data: " = "Variable(s) were not found in the active data: %s.",
      "Repeated-measures variables have different measurement levels: " = "Repeated-measures variables have different measurement levels: %s."
    )
    for (prefix in names(templates)) {
      ending <- if (endsWith(prefix, "(")) ")." else "."
      if (startsWith(text, prefix) && endsWith(text, ending)) {
        detail <- substr(text, nchar(prefix) + 1L, nchar(text) - nchar(ending))
        return(sprintf(statedu_localized_text(language, templates[[prefix]]), detail))
      }
    }
    parts <- regmatches(text, regexec("^([0-9]+) zero difference(?:\\(s\\)|s)? (?:was|were) omitted from the Wilcoxon signed-rank calculation\\.(.*)$", text, perl = TRUE))[[1]]
    if (length(parts) == 3L) {
      suffix <- trimws(parts[3])
      tied <- "Tied absolute differences were present; the large-sample Wilcoxon approximation was used."
      if (!suffix %in% c("", tied)) return(text)
      sentence <- sprintf(statedu_localized_text(language, "%s zero differences were omitted from the Wilcoxon signed-rank calculation."), parts[2])
      return(paste(c(sentence, if (nzchar(suffix)) result_appendix_ui_text(tied, language)), collapse = " "))
    }
  }
  if (!identical(language, "ko")) return(result_appendix_ui_text(text, language))

  exact <- c(
    "Pair" = "쌍",
    "Repeated variables" = "반복측정 변수",
    "Analysis method" = "분석 방법",
    "Normality method" = "정규성 검토 방법",
    "Sphericity" = "구형성",
    "Sphericity p" = "구형성 p",
    "Outliers" = "이상치",
    "Package" = "패키지",
    "Warnings / skipped pairs" = "경고 / 제외된 쌍",
    "Warnings / skipped repeated-measures rows" = "경고 / 제외된 반복측정 행",
    "Skipped pairs" = "제외된 쌍",
    "Skipped repeated-measures rows" = "제외된 반복측정 행",
    "paired t" = "대응표본 t 검정",
    "Paired t-test" = "대응표본 t 검정",
    "Paired test" = "대응표본 검정",
    "Paired categorical test" = "대응 범주형 검정",
    "Nonparametric paired test" = "비모수 대응표본 검정",
    "Wilcoxon" = "Wilcoxon 부호순위 검정",
    "Wilcoxon signed-rank test" = "Wilcoxon 부호순위 검정",
    "McNemar" = "McNemar 검정",
    "McNemar test" = "McNemar 검정",
    "Exact McNemar" = "정확 McNemar 검정",
    "Exact McNemar test" = "정확 McNemar 검정",
    "Stuart-Maxwell" = "Stuart-Maxwell 검정",
    "Stuart-Maxwell test" = "Stuart-Maxwell 검정",
    "Bowker" = "Bowker 대칭성 검정",
    "Bowker symmetry test" = "Bowker 대칭성 검정",
    "RM ANOVA" = "반복측정 분산분석",
    "Standard RM ANOVA" = "반복측정 분산분석",
    "RM ANOVA + GG" = "반복측정 분산분석 + Greenhouse-Geisser 보정",
    "RM ANOVA + Greenhouse-Geisser correction" = "반복측정 분산분석 + Greenhouse-Geisser 보정",
    "RM ANOVA + Wilks" = "반복측정 분산분석 + Wilks' lambda",
    "RM ANOVA + Wilks' lambda" = "반복측정 분산분석 + Wilks' lambda",
    "Friedman" = "Friedman 검정",
    "Friedman test" = "Friedman 검정",
    "Cochran Q" = "Cochran Q 검정",
    "Cochran's Q test" = "Cochran Q 검정",
    "Continuous" = "연속형",
    "Ordinal" = "순서형",
    "Binary" = "이분형",
    "Categorical" = "범주형",
    "Unknown" = "알 수 없음",
    "Normality met" = "정규성 충족",
    "Normality not met" = "정규성 미충족",
    "Sphericity met" = "구형성 충족",
    "Sphericity not met" = "구형성 미충족",
    "Post-hoc included" = "사후분석 포함",
    "None detected" = "발견되지 않음",
    "not checked" = "검토하지 않음",
    "Skewness/Kurtosis" = "왜도/첨도",
    "Skewness and kurtosis" = "왜도와 첨도",
    "Nonparametric method" = "비모수 방법",
    "Continuous nonparametric repeated-measures test" = "연속형 비모수 반복측정 검정",
    "Ordinal nonparametric repeated-measures test" = "순서형 비모수 반복측정 검정",
    "Binary nonparametric repeated-measures test" = "이분형 비모수 반복측정 검정",
    "Nonparametric repeated-measures test" = "비모수 반복측정 검정",
    "At least two complete paired cases are required." = "완전한 대응 사례가 최소 2개 필요합니다.",
    "The paired differences are all zero; no paired test was performed." = "모든 대응 차이가 0이므로 대응표본 검정을 실행하지 않았습니다.",
    "The paired differences have zero variance; paired t-test was not performed." = "대응 차이의 분산이 0이므로 대응표본 t 검정을 실행하지 않았습니다.",
    "At least two complete paired cases with at least two observed categories are required." = "관측 범주가 2개 이상인 완전한 대응 사례가 최소 2개 필요합니다.",
    "Tied absolute differences were present; the large-sample Wilcoxon approximation was used." = "절대 차이의 동률이 있어 큰 표본 Wilcoxon 근사를 사용했습니다.",
    "At least two complete repeated-measures cases are required." = "완전한 반복측정 사례가 최소 2개 필요합니다.",
    "All repeated measurements are identical within subjects; no repeated-measures test was performed." = "각 대상자 내 반복측정값이 모두 같아 반복측정 검정을 실행하지 않았습니다."
  )
  if (text %in% names(exact)) return(unname(exact[[text]]))

  # `paired_wilcoxon_note()` can combine the zero-difference count and the
  # tied-rank warning in one cell.  Translate the parameterized first sentence
  # before translating the optional second sentence so that the observed count
  # is preserved.  Accept the legacy "difference(s)" form as well as ordinary
  # singular/plural English emitted by saved or imported results.
  zero_difference_pattern <- paste0(
    "^([0-9]+) zero difference(?:\\(s\\)|s)? (?:was|were) omitted from ",
    "the Wilcoxon signed-rank calculation\\."
  )
  if (grepl(zero_difference_pattern, text, perl = TRUE)) {
    text <- sub(
      zero_difference_pattern,
      "차이가 0인 사례 \\1개를 Wilcoxon 부호순위 계산에서 제외했습니다.",
      text,
      perl = TRUE
    )
    text <- gsub(
      "Tied absolute differences were present; the large-sample Wilcoxon approximation was used.",
      exact[["Tied absolute differences were present; the large-sample Wilcoxon approximation was used."]],
      text,
      fixed = TRUE
    )
    return(trimws(text))
  }

  if (grepl("^Skipped because variable\\(s\\) were not found in the active data: ", text)) {
    return(sub(
      "^Skipped because variable\\(s\\) were not found in the active data: (.*)\\.$",
      "활성 자료에서 다음 변수를 찾을 수 없어 제외했습니다: \\1.",
      text
    ))
  }
  if (grepl("^Skipped because paired variables have different measurement levels ", text)) {
    return(sub(
      "^Skipped because paired variables have different measurement levels \\((.*)\\)\\.$",
      "대응 변수의 측정수준이 서로 달라 제외했습니다 (\\1).",
      text
    ))
  }
  if (grepl("^Variable\\(s\\) were not found in the active data: ", text)) {
    return(sub(
      "^Variable\\(s\\) were not found in the active data: (.*)\\.$",
      "활성 자료에서 다음 변수를 찾을 수 없습니다: \\1.",
      text
    ))
  }
  if (grepl("^Repeated-measures variables have different measurement levels: ", text)) {
    return(sub(
      "^Repeated-measures variables have different measurement levels: (.*)\\.$",
      "반복측정 변수의 측정수준이 서로 다릅니다: \\1.",
      text
    ))
  }
  if (grepl("^([0-9]+) detected \\(IDs: ", text)) {
    return(sub("^([0-9]+) detected \\(IDs: (.*)\\)$", "\\1개 발견 (ID: \\2)", text))
  }
  if (grepl("^[0-9]+ detected$", text)) {
    return(sub("^([0-9]+) detected$", "\\1개 발견", text))
  }
  if (grepl("^skew=", text)) {
    text <- sub("^skew=", "왜도=", text)
    return(gsub("kurtosis=", "첨도=", text, fixed = TRUE))
  }
  text
}

paired_appendix_localize_values <- function(table, language = NULL) {
  source_table <- table
  if (!is.data.frame(table)) return(table)
  language <- result_appendix_table_language(language)
  if (!identical(language, "ko")) {
    localized <- result_appendix_localize_table(table, language)
    # Keep source field names for diagnostic normalization; translate headers at rendering.
    names(localized) <- names(table)
    if ("Outliers" %in% names(table)) localized$Outliers <- vapply(as.character(table$Outliers), paired_appendix_text, character(1), language = language)
    if (all(c("Item", "Result") %in% names(table))) {
      outlier_rows <- which(table$Item == "Outliers")
      localized$Result[outlier_rows] <- vapply(as.character(table$Result[outlier_rows]), paired_appendix_text, character(1), language = language)
      normality_rows <- which(table$Item == "Normality" & !is.na(table$Result) & startsWith(as.character(table$Result), "skew="))
      localized$Result[normality_rows] <- vapply(as.character(table$Result[normality_rows]), paired_appendix_text, character(1), language = language)
    }
    for (column in intersect(c("Warning", "Reason", "Message"), names(table))) {
      values <- as.character(table[[column]])
      matched <- which(!is.na(values) & grepl("(^|\n)([0-9]+ zero difference|Skipped because variable\\(s\\) were not found|Skipped because paired variables have different measurement levels|Variable\\(s\\) were not found|Repeated-measures variables have different measurement levels)", values, perl = TRUE))
      if (!length(matched)) next
      localized[[column]][matched] <- vapply(values[matched], function(value) {
        paste(vapply(strsplit(value, "\n", fixed = TRUE)[[1]], paired_appendix_text, character(1), language = language), collapse = "\n")
      }, character(1))
    }
    return(localized)
  }
  fallback <- result_appendix_localize_table(source_table, language)
  names(fallback) <- names(source_table)
  for (column in names(table)) {
    if (!is.character(table[[column]]) && !is.factor(table[[column]])) next
    values <- as.character(table[[column]])
    table[[column]] <- vapply(seq_along(values), function(index) {
      value <- values[index]
      if (is.na(value)) return(NA_character_)
      lines <- strsplit(value, "\n", fixed = TRUE)[[1]]
      translated <- paste(vapply(lines, paired_appendix_text, character(1), language = language), collapse = "\n")
      if (identical(translated, value)) as.character(fallback[[column]][index]) else translated
    }, character(1))
  }
  result_appendix_preserve_data(table, source_table)
}

paired_appendix_table <- function(table) {
  source_table <- table
  language <- result_appendix_table_language()
  table <- paired_appendix_localize_values(table, language)
  if (is.data.frame(table)) {
    # Values have already been localized. A second pass can reinterpret a
    # translated warning containing a method name as just that method label.
    names(table) <- names(result_appendix_localize_table(source_table, language))
    if (identical(language, "ko")) names(table) <- vapply(names(table), paired_appendix_text, character(1), language = language)
    attr(table, "result_table_role") <- "appendix"
    attr(table, "result_table_language") <- language
  }
  result_appendix_preserve_data(table, source_table)
}

paired_main_note <- function(text, category = "estimation") {
  values <- list()
  values[[category]] <- text
  do.call(result_sci_note_text, values)
}

paired_scale_statistic_label <- function(table) {
  labels <- unique(as.character(table$StatisticLabel %||% ""))
  labels <- labels[nzchar(labels)]
  if (length(labels) == 1L) labels[[1]] else "Statistic"
}

paired_effect_header_label <- function(table) {
  labels <- unique(as.character(table$EffectLabel %||% ""))
  labels <- labels[nzchar(labels)]
  if (length(labels) == 1L) labels[[1]] else "ES"
}

paired_effect_labels <- function(table) {
  if (!is.data.frame(table) || !"EffectLabel" %in% names(table)) return(character(0))
  labels <- unique(unlist(strsplit(as.character(table$EffectLabel %||% ""), ";", fixed = TRUE), use.names = FALSE))
  labels <- trimws(labels)
  labels[nzchar(labels)]
}

paired_effect_header_text <- function(label, label_count = 1L) {
  label <- trimws(as.character(label %||% ""))
  switch(
    label,
    "Hedges' g" = "g",
    "Cohen's d" = "d",
    r = "r",
    label
  )
}

paired_effect_note_text <- function(label) {
  label <- trimws(as.character(label %||% ""))
  switch(
    label,
    r = "r = Wilcoxon signed-rank effect size",
    "Hedges' g" = "g = Hedges' g",
    "Cohen's d" = "d = Cohen's d",
    label
  )
}

paired_header_label_content <- function(label) {
  parts <- strsplit(as.character(label %||% ""), "\n", fixed = TRUE)[[1]]
  if (length(parts) <= 1L) {
    return(label)
  }
  tags$span(
    lapply(seq_along(parts), function(index) {
      tagList(
        if (index > 1L) tags$br(),
        parts[[index]]
      )
    })
  )
}

paired_count_statistic_label <- function(table) {
  labels <- unique(as.character(table$StatisticLabel %||% ""))
  labels <- labels[nzchar(labels)]
  if (length(labels) == 1L) labels[[1]] else "Statistic"
}

paired_has_statistic <- function(table) {
  if (!is.data.frame(table) || !"Statistic" %in% names(table)) return(FALSE)
  any(nzchar(as.character(table$Statistic %||% "")))
}

paired_has_effect <- function(table) {
  if (!is.data.frame(table) || !"Effect" %in% names(table)) return(FALSE)
  any(nzchar(as.character(table$Effect %||% "")))
}

paired_effect_note <- function(table, show_effect_size = TRUE) {
  if (!isTRUE(show_effect_size)) return("")
  labels <- paired_effect_labels(table)
  if (length(labels) == 0) return("")
  if (length(labels) == 1L) {
    paste0("ES = effect size (", paired_effect_note_text(labels[[1]]), ").")
  } else {
    paste0("ES = effect size (", paste(vapply(labels, paired_effect_note_text, character(1)), collapse = ", "), ").")
  }
}

paired_method_note <- function(table, show_effect_size = TRUE) {
  if (!is.data.frame(table) || nrow(table) == 0 || !"Method" %in% names(table)) return("")
  methods <- unique(as.character(table$Method))
  methods <- methods[nzchar(methods)]
  if (length(methods) == 0) return("")
  marker_map <- paired_method_marker_map(table)
  method_note <- if (length(marker_map) > 1) {
    marker_notes <- paste(sprintf("%s %s", unname(marker_map), names(marker_map)), collapse = "; ")
    paste0("Analysis method: ", marker_notes, ".")
  } else {
    paste0("Analysis method: ", methods[[1]], ".")
  }
  paste(c(method_note, paired_effect_note(table, show_effect_size)), collapse = " ")
}

paired_count_method_note <- function(result, show_effect_size = TRUE) {
  paired_method_note(result$count_table, show_effect_size = show_effect_size)
}

paired_method_marker_map <- function(table) {
  if (!is.data.frame(table) || !"Method" %in% names(table)) return(character(0))
  methods <- unique(as.character(table$Method))
  methods <- methods[nzchar(methods)]
  if (length(methods) <= 1L) return(character(0))
  stats::setNames(letters[seq_along(methods)], methods)
}

paired_method_marker_for_row <- function(table, row_index) {
  marker_map <- paired_method_marker_map(table)
  if (length(marker_map) == 0 || !"Method" %in% names(table)) return("")
  method <- as.character(table$Method[[row_index]] %||% "")
  named_value(marker_map, method, "")
}

paired_p_value_cell <- function(value, marker) {
  if (!nzchar(marker %||% "")) return(value %||% "")
  tags$span(
    style = "white-space:nowrap;",
    value %||% "",
    tags$sup(style = "margin-left:2px;font-size:75%;vertical-align:super;", marker)
  )
}

paired_short_method <- function(value) {
  value <- as.character(value %||% "")
  switch(
    value,
    "Paired t-test" = "paired t",
    "Wilcoxon signed-rank test" = "Wilcoxon",
    "McNemar test" = "McNemar",
    "Exact McNemar test" = "Exact McNemar",
    "Stuart-Maxwell test" = "Stuart-Maxwell",
    "Bowker symmetry test" = "Bowker",
    value
  )
}

paired_check_summary <- function(check_row) {
  if (!is.data.frame(check_row) || nrow(check_row) == 0) return("")
  method <- as.character(check_row$`Check Result`[[1]] %||% "")
  if (identical(method, "Skewness/Kurtosis")) {
    skewness <- as.character(check_row$Skewness[[1]] %||% "")
    kurtosis <- as.character(check_row$Kurtosis[[1]] %||% "")
    return(sprintf("skew=%s, kurtosis=%s", skewness, kurtosis))
  }
  if (identical(method, "Shapiro-Wilk")) {
    w <- as.character(check_row$`Shapiro-Wilk W`[[1]] %||% "")
    p <- as.character(check_row$`Shapiro-Wilk p`[[1]] %||% "")
    if (nzchar(w)) return(sprintf("S-W W=%s(%s)", w, p))
    return(sprintf("S-W p=%s", p))
  }
  method
}

paired_check_satisfied_label <- function(check_row) {
  if (!is.data.frame(check_row) || nrow(check_row) == 0) return("")
  method <- as.character(check_row$`Check Result`[[1]] %||% "")
  if (identical(method, "Skewness/Kurtosis")) {
    skewness <- suppressWarnings(as.numeric(check_row$Skewness[[1]]))
    kurtosis <- suppressWarnings(as.numeric(check_row$Kurtosis[[1]]))
    if (is.na(skewness) || is.na(kurtosis)) return("")
    return(if (abs(skewness) <= 2 && abs(kurtosis) <= 7) "Normality met" else "Normality not met")
  }
  if (identical(method, "Shapiro-Wilk")) {
    p_text <- as.character(check_row$`Shapiro-Wilk p`[[1]] %||% "")
    if (!nzchar(p_text)) return("")
    p_value <- suppressWarnings(as.numeric(sub("^<", "", p_text)))
    if (is.na(p_value)) return("")
    return(if (p_value >= .05) "Normality met" else "Normality not met")
  }
  ""
}

paired_model_overview_table <- function(result) {
  if (!is.list(result) || !is.data.frame(result$table) || nrow(result$table) == 0) {
    return(NULL)
  }
  rows <- list()
  for (index in seq_len(nrow(result$table))) {
    item <- result$table[index, , drop = FALSE]
    pair <- as.character(item$Pair[[1]] %||% "")
    check_row <- if (is.data.frame(result$checks) && "Pair" %in% names(result$checks)) {
      result$checks[as.character(result$checks$Pair) == pair, , drop = FALSE]
    } else {
      NULL
    }
    reason_parts <- c(
      paired_check_satisfied_label(check_row),
      if ("Level" %in% names(item) && as.character(item$Level[[1]] %||% "") %in% c("Binary", "Categorical")) as.character(item$Level[[1]]) else ""
    )
    reason_parts <- reason_parts[nzchar(reason_parts)]
    row <- data.frame(
      Pair = pair,
      N = as.character(item$N[[1]] %||% ""),
      Analysis = paired_short_method(item$Method[[1]]),
      Reason = paste(reason_parts, collapse = "\n"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    names(row) <- c("Pair", "N", "Analysis method", "Reason")
    rows[[length(rows) + 1L]] <- row
  }
  if (length(rows) == 0) NULL else do.call(rbind, rows)
}

paired_assumption_review_table <- function(result) {
  if (!is.list(result) || !is.data.frame(result$table) || nrow(result$table) == 0) {
    return(NULL)
  }
  rows <- list()
  for (index in seq_len(nrow(result$table))) {
    item <- result$table[index, , drop = FALSE]
    pair <- as.character(item$Pair[[1]] %||% "")
    check_row <- if (is.data.frame(result$checks) && "Pair" %in% names(result$checks)) {
      result$checks[as.character(result$checks$Pair) == pair, , drop = FALSE]
    } else {
      NULL
    }
    values <- stats::setNames(
      list(
        paired_check_summary(check_row),
        if (is.data.frame(check_row) && nrow(check_row) > 0) as.character(check_row$Outliers[[1]] %||% "") else "",
        "stats"
      ),
      c("Normality", "Outliers", "Package")
    )
    metric_index <- 0L
    for (metric in names(values)) {
      metric_index <- metric_index + 1L
      rows[[length(rows) + 1L]] <- data.frame(
        Pair = if (metric_index == 1L) pair else "",
        Item = metric,
        Result = values[[metric]],
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
  }
  if (length(rows) == 0) NULL else do.call(rbind, rows)
}

paired_combined_review_table <- function(paired_table, paired_rm_table) {
  normalize <- function(table, first_name) {
    if (!is.data.frame(table) || nrow(table) == 0) {
      return(NULL)
    }
    names(table)[[1]] <- first_name
    table
  }
  rows <- Filter(Negate(is.null), list(
    normalize(paired_table, "Pair"),
    normalize(paired_rm_table, "Pair")
  ))
  if (length(rows) == 0) {
    return(NULL)
  }
  do.call(rbind, rows)
}

paired_combined_diagnostics_table <- function(result, field) {
  rows <- Filter(function(table) is.data.frame(table) && nrow(table) > 0, list(
    result$paired[[field]],
    result$paired_rm[[field]]
  ))
  if (length(rows) == 0) {
    return(NULL)
  }
  columns <- unique(unlist(lapply(rows, names), use.names = FALSE))
  rows <- lapply(rows, function(table) {
    missing <- setdiff(columns, names(table))
    for (column in missing) {
      table[[column]] <- ""
    }
    table[, columns, drop = FALSE]
  })
  do.call(rbind, rows)
}

paired_effect_value_for_label <- function(table, row_index, effect_label) {
  value <- as.character(table$Effect[[row_index]] %||% "")
  if (!nzchar(value)) return("")
  labels <- trimws(strsplit(as.character(table$EffectLabel[[row_index]] %||% ""), ";", fixed = TRUE)[[1]])
  values <- trimws(strsplit(value, ";", fixed = TRUE)[[1]])
  index <- match(effect_label, labels)
  if (is.na(index) || index < 1L || index > length(values)) return("")
  values[[index]]
}

paired_grouped_column_class <- function(column) {
  if (identical(column, "Variable")) return("paired-two-col-variable")
  if (identical(column, "post-hoc")) return("paired-two-col-posthoc")
  if (column %in% c("Pre_M", "Pre_SD", "Post_M", "Post_SD", "Pre_MS", "Post_MS")) return("paired-two-col-summary")
  if (column %in% c("Statistic", "p")) return("paired-two-col-stat")
  if (startsWith(column, "Effect:")) return("paired-two-col-effect")
  "paired-two-col-default"
}

paired_grouped_column_widths <- function(body_columns, table = NULL) {
  median_summary <- is.data.frame(table) &&
    "SummaryCenter" %in% names(table) &&
    any(as.character(table$SummaryCenter %||% "") == "Median", na.rm = TRUE)
  mean_sd <- isTRUE(attr(table, "mean_sd", exact = TRUE))
  effect_count <- sum(startsWith(body_columns, "Effect:"))
  variable_width <- if (isTRUE(median_summary)) 17 else 22
  statistic_width <- if ("Statistic" %in% body_columns) if (isTRUE(median_summary)) 15 else 16 else 0
  p_width <- if ("p" %in% body_columns) if (isTRUE(median_summary)) 6 else 8 else 0
  effect_single_width <- if (isTRUE(median_summary)) 6 else 8
  effect_width <- effect_single_width * effect_count
  fixed_width <- variable_width + statistic_width + p_width + effect_width
  summary_columns <- c("Pre_M", "Pre_SD", "Post_M", "Post_SD", "Pre_MS", "Post_MS")
  summary_count <- sum(body_columns %in% summary_columns)
  minimum_summary_width <- if (isTRUE(median_summary) && isTRUE(mean_sd)) 18 else if (isTRUE(median_summary)) 10.5 else 9
  summary_width <- if (summary_count > 0) max(minimum_summary_width, (100 - fixed_width) / summary_count) else 10
  widths <- rep(summary_width, length(body_columns))
  widths[body_columns == "Variable"] <- variable_width
  widths[body_columns == "Statistic"] <- statistic_width
  widths[body_columns == "p"] <- p_width
  widths[startsWith(body_columns, "Effect:")] <- effect_single_width
  stats::setNames(widths, body_columns)
}

paired_summary_header_labels <- function(table) {
  if (!"SummaryCenter" %in% names(table) && isTRUE(attr(table, "median_iqr", exact = TRUE))) {
    return(list(center = "Median", spread = "Q1~Q3", combined = "Median(Q1~Q3)"))
  }
  centers <- unique(as.character(table$SummaryCenter %||% "M"))
  spreads <- unique(as.character(table$SummarySpread %||% "SD"))
  centers <- centers[nzchar(centers)]
  spreads <- spreads[nzchar(spreads)]
  mixed_median <- length(centers) > 1L || any(centers == "Median")
  list(
    center = if (length(centers) == 1L) centers[[1]] else "M/Median",
    spread = if (length(spreads) == 1L) spreads[[1]] else "SD/\nQ1~Q3",
    combined = if (length(centers) == 1L && identical(centers[[1]], "Median")) {
      "Median(Q1~Q3)"
    } else if (isTRUE(mixed_median)) {
      "M \u00B1 SD /\nMedian(Q1~Q3)"
    } else {
      "M \u00B1 SD"
    }
  )
}

paired_scale_display_table <- function(result) {
  table <- result$scale_table
  if (!is.data.frame(table) || nrow(table) == 0) return(table)
  attr(table, "mean_sd") <- isTRUE(result$options$mean_sd)
  table
}

paired_grouped_table <- function(table, type = c("scale", "count"), show_effect_size = FALSE, table_role = "main") {
  type <- match.arg(type)
  if (!is.data.frame(table) || nrow(table) == 0) return(NULL)
  show_effect_size <- isTRUE(show_effect_size) && paired_has_effect(table)
  effect_labels <- if (show_effect_size) paired_effect_labels(table) else character(0)
  effect_columns <- if (length(effect_labels)) paste0("Effect:", effect_labels) else character(0)
  summary_labels <- paired_summary_header_labels(table)
  mean_sd <- isTRUE(attr(table, "mean_sd", exact = TRUE))
  if (identical(type, "scale")) {
    summary_header_style <- paste0(
      result_header_cell_style(FALSE),
      "white-space:normal;line-height:1.22;",
      if (is.data.frame(table) && "SummaryCenter" %in% names(table) && any(as.character(table$SummaryCenter %||% "") == "Median", na.rm = TRUE)) {
        "font-size:13px;"
      } else {
        ""
      }
    )
    body_columns <- if (isTRUE(mean_sd)) {
      c("Variable", "Pre_MS", "Post_MS", "Statistic", "p")
    } else {
      c("Variable", "Pre_M", "Pre_SD", "Post_M", "Post_SD", "Statistic", "p")
    }
    body_columns <- c(body_columns, effect_columns)
    statistic_label <- paired_scale_statistic_label(table)
    headers <- list(
      tags$tr(
        tags$th(rowspan = 2, style = result_header_cell_style(TRUE), "Variable"),
        tags$th(colspan = if (isTRUE(mean_sd)) 1 else 2, style = paste0(result_header_cell_style(FALSE), "text-align:center;white-space:nowrap;"), "Pre"),
        tags$th(colspan = if (isTRUE(mean_sd)) 1 else 2, style = paste0(result_header_cell_style(FALSE), "text-align:center;white-space:nowrap;"), "Post"),
        tags$th(rowspan = 2, style = paste0(result_header_cell_style(FALSE), "white-space:nowrap;"), statistic_label),
        tags$th(rowspan = 2, style = result_header_cell_style(FALSE), "p"),
        if (length(effect_labels) == 1L) {
          tags$th(rowspan = 2, style = paste0(result_header_cell_style(FALSE), "white-space:nowrap;"), paired_effect_header_text(effect_labels[[1]], 1L))
        } else if (length(effect_labels) > 1L) {
          tags$th(colspan = length(effect_labels), style = paste0(result_header_cell_style(FALSE), "text-align:center;white-space:nowrap;"), "ES")
        }
      ),
      tags$tr(
        if (isTRUE(mean_sd)) {
          tagList(
            tags$th(style = summary_header_style, paired_header_label_content(summary_labels$combined)),
            tags$th(style = summary_header_style, paired_header_label_content(summary_labels$combined))
          )
        } else {
          tagList(
            tags$th(style = summary_header_style, paired_header_label_content(summary_labels$center)),
            tags$th(style = summary_header_style, paired_header_label_content(summary_labels$spread)),
            tags$th(style = summary_header_style, paired_header_label_content(summary_labels$center)),
            tags$th(style = summary_header_style, paired_header_label_content(summary_labels$spread))
          )
        },
        if (length(effect_labels) > 1L) {
          lapply(effect_labels, function(label) tags$th(style = paste0(result_header_cell_style(FALSE), "white-space:nowrap;"), paired_effect_header_text(label, length(effect_labels))))
        }
      )
    )
  } else {
    post_columns <- grep("^Post_", names(table), value = TRUE)
    post_labels <- sub("^Post_", "", post_columns)
    include_statistic <- paired_has_statistic(table)
    body_columns <- c("Variable", "Pre", post_columns, if (include_statistic) "Statistic", "p", effect_columns)
    statistic_label <- paired_count_statistic_label(table)
    headers <- list(
      tags$tr(
        tags$th(rowspan = 2, style = result_header_cell_style(TRUE), "Variable"),
        tags$th(rowspan = 2, style = paste0(result_header_cell_style(FALSE), "text-align:left;"), "Pre"),
        tags$th(colspan = length(post_columns), style = paste0(result_header_cell_style(FALSE), "text-align:center;"), "Post"),
        if (include_statistic) tags$th(rowspan = 2, style = result_header_cell_style(FALSE), statistic_label),
        tags$th(rowspan = 2, style = result_header_cell_style(FALSE), "p"),
        if (length(effect_labels) == 1L) {
          tags$th(rowspan = 2, style = paste0(result_header_cell_style(FALSE), "white-space:nowrap;"), paired_effect_header_text(effect_labels[[1]], 1L))
        } else if (length(effect_labels) > 1L) {
          tags$th(colspan = length(effect_labels), style = paste0(result_header_cell_style(FALSE), "text-align:center;"), "ES")
        }
      ),
      tags$tr(
        lapply(post_labels, function(label) tags$th(style = result_header_cell_style(FALSE), label)),
        if (length(effect_labels) > 1L) {
          lapply(effect_labels, function(label) tags$th(style = paste0(result_header_cell_style(FALSE), "white-space:nowrap;"), paired_effect_header_text(label, length(effect_labels))))
        }
      )
    )
  }
  column_widths <- paired_grouped_column_widths(body_columns, table)
  median_summary <- isTRUE(identical(type, "scale")) &&
    is.data.frame(table) &&
    "SummaryCenter" %in% names(table) &&
    any(as.character(table$SummaryCenter %||% "") == "Median", na.rm = TRUE)
  table_width <- if (isTRUE(median_summary)) {
    if (isTRUE(mean_sd)) 840L else 900L
  } else {
    max(640L, min(900L, 150L + (length(body_columns) - 1L) * 86L))
  }
  if (!isTRUE(median_summary) && length(body_columns) <= 9L) table_width <- 590L
  body_style <- function(first = FALSE, last = FALSE) {
    paste0(
      result_body_cell_style(first, last),
      if (isTRUE(first)) {
        "white-space:normal;"
      } else {
        "white-space:nowrap !important;overflow-wrap:normal !important;word-break:normal !important;"
      }
    )
  }
  table_tag <- tags$table(
    class = "coefficient-table paired-grouped-table paired-two-grouped-table",
    style = paste0(
      result_table_style(font_size = 12, min_width = table_width),
      sprintf("table-layout:fixed;width:%dpx;max-width:100%%;", table_width)
    ),
    tags$colgroup(lapply(body_columns, function(column) {
      tags$col(
        class = paired_grouped_column_class(column),
        style = sprintf("width:%.3f%% !important;", column_widths[[column]])
      )
    })),
    tags$thead(headers),
    tags$tbody(
      lapply(seq_len(nrow(table)), function(row_index) {
        tags$tr(lapply(seq_along(body_columns), function(column_index) {
          column <- body_columns[[column_index]]
          marker <- if (identical(column, "p")) paired_method_marker_for_row(table, row_index) else ""
          tags$td(
            style = paste0(
              body_style(column_index == 1, row_index == nrow(table)),
              if (identical(type, "count") && identical(column, "Pre")) "text-align:left;" else ""
            ),
            if (identical(column, "p")) {
              paired_p_value_cell(table[[column]][[row_index]], marker)
            } else if (startsWith(column, "Effect:")) {
              paired_effect_value_for_label(table, row_index, sub("^Effect:", "", column))
            } else {
              result_cell_content(table[[column]][[row_index]] %||% "", column = column)
            }
          )
        }))
      })
    )
  )
  result_table_apply_contract(
    table_tag,
    result_table_contract(
      table,
      role = table_role,
      language = if (identical(result_table_role(table_role), "main")) result_main_table_language() else result_appendix_table_language(),
      intrinsic_width = table_width
    )
  )
}

paired_results_ui <- function(result) {
  if (is.null(result)) {
    return(NULL)
  }
  if (!is.null(result$error)) {
    return(empty_message(result$error))
  }
  if (identical(result$type, "paired_rm")) {
    return(paired_rm_results_ui(result))
  }
  if (identical(result$type, "paired_combined")) {
    overview_table <- paired_combined_review_table(
      paired_model_overview_table(result$paired),
      paired_rm_model_overview_table(result$paired_rm)
    )
    assumption_table <- paired_combined_review_table(
      paired_assumption_review_table(result$paired),
      paired_rm_assumption_review_table(result$paired_rm)
    )
    return(tags$div(
      class = "regression-results paired-results paired-combined-results",
      if (is.data.frame(overview_table) && nrow(overview_table) > 0) {
        tags$div(
          class = "result-section paired-result-section regression-result-panel",
          tags$h3(result_appendix_ui_text("Model overview")),
          model_overview_html_table(paired_appendix_table(overview_table))
        )
      },
      if (is.data.frame(result$paired$scale_table) && nrow(result$paired$scale_table) > 0) {
        tags$div(
          class = "result-section paired-result-section regression-result-panel",
          tags$h3("Paired test: continuous / ordinal"),
          result_table_with_notes(
            paired_grouped_table(paired_main_table(paired_scale_display_table(result$paired)), "scale", show_effect_size = isTRUE(result$paired$options$effect_size)),
            result_note_tag(paired_main_note(paired_method_note(paired_scale_display_table(result$paired), show_effect_size = isTRUE(result$paired$options$effect_size)))),
            class = "result-table-with-note paired-fit-table-wrap"
          )
        )
      },
      if (is.data.frame(result$paired$count_table) && nrow(result$paired$count_table) > 0) {
        tags$div(
          class = "result-section paired-result-section regression-result-panel",
          tags$h3("Paired test: binary / categorical"),
          result_table_with_notes(
            paired_grouped_table(paired_main_table(result$paired$count_table), "count", show_effect_size = isTRUE(result$paired$options$effect_size)),
            result_note_tag(paired_main_note(paired_count_method_note(result$paired, show_effect_size = isTRUE(result$paired$options$effect_size)))),
            class = "result-table-with-note paired-fit-table-wrap"
          )
        )
      },
      if (is.data.frame(result$paired_rm$display_table) && nrow(result$paired_rm$display_table) > 0) {
        tags$div(
          class = "result-section paired-result-section regression-result-panel landscape-table-panel",
          tags$h3("Repeated-measures test: continuous / ordinal"),
          result_table_with_notes(
            paired_rm_grouped_table(paired_main_table(paired_rm_table_with_options(result$paired_rm$display_table, result$paired_rm$options)), "scale", table_role = "main"),
            result_note_tag(paired_main_note(paired_rm_table_method_note(result$paired_rm$display_table))),
            class = "result-table-with-note paired-fit-table-wrap"
          )
        )
      },
      if (is.data.frame(result$paired_rm$count_table) && nrow(result$paired_rm$count_table) > 0) {
        tags$div(
          class = "result-section paired-result-section regression-result-panel landscape-table-panel",
          tags$h3("Repeated-measures test: binary"),
          result_table_with_notes(
            paired_rm_grouped_table(paired_main_table(result$paired_rm$count_table), "count", table_role = "main"),
            result_note_tag(paired_main_note(paired_rm_table_method_note(result$paired_rm$count_table))),
            class = "result-table-with-note paired-fit-table-wrap"
          )
        )
      },
      if (
        (!is.data.frame(result$paired_rm$display_table) || nrow(result$paired_rm$display_table) == 0) &&
          (!is.data.frame(result$paired_rm$count_table) || nrow(result$paired_rm$count_table) == 0) &&
          is.data.frame(result$paired_rm$table) &&
          nrow(result$paired_rm$table) > 0
      ) {
        tags$div(
          class = "result-section paired-result-section regression-result-panel",
          tags$h3("Repeated-measures test"),
          coefficient_html_table(
            paired_main_table(result$paired_rm$table),
            note_line = paired_main_note(paired_rm_method_note(result$paired_rm)),
            table_role = "main"
          )
        )
      },
      if (is.data.frame(result$paired_rm$posthoc) && nrow(result$paired_rm$posthoc) > 0) {
        tags$div(
          class = "result-section paired-result-section regression-result-panel",
          tags$h3("Post-hoc pairwise comparisons"),
          coefficient_html_table(
            paired_main_table(result$paired_rm$posthoc),
            note_line = paired_main_note(paired_rm_posthoc_note(result$paired_rm), "multiplicity"),
            table_role = "main"
          )
        )
      },
      if (is.data.frame(assumption_table) && nrow(assumption_table) > 0) {
        tags$div(
          class = "result-section paired-result-section regression-result-panel",
          tags$h3(result_appendix_ui_text("Assumption review")),
          model_overview_html_table(paired_appendix_table(assumption_table))
        )
      },
      analysis_diagnostics_section(
        paired_appendix_localize_values(paired_combined_diagnostics_table(result, "warnings")),
        paired_appendix_localize_values(paired_combined_diagnostics_table(result, "skipped")),
        title = paired_appendix_text("Warnings / skipped repeated-measures rows"),
        class = "result-section paired-result-section regression-result-panel paired-diagnostics-panel"
      )
    ))
  }
  tags$div(
    class = "regression-results paired-results",
    if (is.data.frame(paired_model_overview_table(result)) && nrow(paired_model_overview_table(result)) > 0) {
      tags$div(
        class = "result-section paired-result-section regression-result-panel",
        tags$h3(result_appendix_ui_text("Model overview")),
        model_overview_html_table(paired_appendix_table(paired_model_overview_table(result)))
      )
    },
    if (is.data.frame(result$scale_table) && nrow(result$scale_table) > 0) {
      tags$div(
        class = "result-section paired-result-section regression-result-panel",
        tags$h3("Paired test: continuous / ordinal"),
        result_table_with_notes(
          paired_grouped_table(paired_main_table(paired_scale_display_table(result)), "scale", show_effect_size = isTRUE(result$options$effect_size)),
          result_note_tag(paired_main_note(paired_method_note(paired_scale_display_table(result), show_effect_size = isTRUE(result$options$effect_size)))),
          class = "result-table-with-note paired-fit-table-wrap"
        )
      )
    },
    if (is.data.frame(result$count_table) && nrow(result$count_table) > 0) {
      tags$div(
        class = "result-section paired-result-section regression-result-panel",
        tags$h3("Paired test: binary / categorical"),
        result_table_with_notes(
          paired_grouped_table(paired_main_table(result$count_table), "count", show_effect_size = isTRUE(result$options$effect_size)),
          result_note_tag(paired_main_note(paired_count_method_note(result, show_effect_size = isTRUE(result$options$effect_size)))),
          class = "result-table-with-note paired-fit-table-wrap"
        )
      )
    },
    if (is.data.frame(paired_assumption_review_table(result)) && nrow(paired_assumption_review_table(result)) > 0) {
      tags$div(
        class = "result-section paired-result-section regression-result-panel",
        tags$h3(result_appendix_ui_text("Assumption review")),
        model_overview_html_table(paired_appendix_table(paired_assumption_review_table(result)))
      )
    },
    analysis_diagnostics_section(
      paired_appendix_localize_values(result$warnings),
      paired_appendix_localize_values(result$skipped),
      title = paired_appendix_text("Warnings / skipped pairs"),
      class = "result-section paired-result-section regression-result-panel paired-diagnostics-panel"
    )
  )
}
