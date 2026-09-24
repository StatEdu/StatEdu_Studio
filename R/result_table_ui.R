# HTML table builders for result output.

# Shared screen-table contract -------------------------------------------------
#
# Result tables are displayed as independent B5 sheets.  The role and language
# helpers deliberately keep journal tables separate from diagnostic/appendix
# output: journal tables are always English, while appendix tables follow the
# normalized UI language.

result_table_role <- function(role = NULL, table = NULL, default = "main") {
  if (is.null(role) && !is.null(table)) {
    for (attribute_name in c("result_table_role", "table_role", "output_role")) {
      candidate <- attr(table, attribute_name, exact = TRUE)
      if (!is.null(candidate) && length(candidate) > 0L && nzchar(as.character(candidate[[1]] %||% ""))) {
        role <- candidate
        break
      }
    }
  }
  value <- tolower(trimws(as.character(role %||% default)[[1]]))
  if (value %in% c("appendix", "diagnostic", "diagnostics", "supplement", "supplementary", "auxiliary")) {
    return("appendix")
  }
  "main"
}

result_main_table_language <- function(language = NULL) {
  "en"
}

result_appendix_table_language <- function(language = NULL) {
  selected <- language %||% getOption("statedu.app_language", NULL)
  if (is.null(selected) || length(selected) == 0L || !nzchar(as.character(selected[[1]] %||% ""))) {
    selected <- if (exists("statedu_initial_language", mode = "function")) statedu_initial_language() else "ko"
  }
  if (exists("normalize_app_language", mode = "function")) {
    return(normalize_app_language(selected))
  }
  value <- tolower(trimws(as.character(selected[[1]] %||% "ko")))
  if (value %in% c("en", "english", "eng")) "en" else "ko"
}

result_table_language <- function(role = "main", language = NULL) {
  if (identical(result_table_role(role), "appendix")) {
    result_appendix_table_language(language)
  } else {
    result_main_table_language(language)
  }
}

result_appendix_ui_text <- function(text, language = NULL) {
  language <- result_appendix_table_language(language)
  text <- as.character(text %||% "")
  if (identical(language, "en") || is.na(text) || !nzchar(text)) {
    return(text)
  }
  if (!identical(language, "ko") && identical(text, "Valid %")) {
    return(statedu_t("analysis.ui.valid_percent", language, text))
  }
  # `analysis_ui_text()` uses `|` as its internal bilingual separator.  Result
  # prose may legitimately contain statistical notation such as |skewness|;
  # passing that prose through the separator parser would truncate the note.
  translated <- if (grepl("|", text, fixed = TRUE)) {
    text
  } else if (exists("analysis_ui_text", mode = "function")) {
    tryCatch(analysis_ui_text(text, language), error = function(error) text)
  } else {
    text
  }
  translated <- enc2utf8(as.character(translated))
  if (!identical(translated, text) && !grepl("<U\\+[0-9A-Fa-f]+>", translated, perl = TRUE)) {
    return(as.character(translated))
  }
    if (!identical(language, "ko")) {
      if (identical(text, "not checked Satisfied")) return(paste(
        statedu_localized_text(language, "not checked"), result_appendix_ui_text("Satisfied", language)))
      # Only recognize complete generated diagnostic formats. Values are inserted
      # after translation; identity columns are restored by preserve_data().
      formats <- list(
        list(pattern = "^Crossing survival curves: (.+)$", template = "Crossing survival curves: %s"),
        list(pattern = "^At risk near follow-up tail = ([0-9]+)$", template = "At risk near follow-up tail = %s"),
        list(pattern = "^Acceptable \\(max VIF=([0-9.]+)\\)$", template = "Acceptable (max VIF=%s)"),
        list(pattern = "^High collinearity \\(max VIF=([0-9.]+)\\)$", template = "High collinearity (max VIF=%s)"),
        list(pattern = "^Moderate collinearity \\(max VIF=([0-9.]+)\\)$", template = "Moderate collinearity (max VIF=%s)"),
        list(pattern = "^Flagged cases=([0-9]+); max Cook's D=([0-9.]+)$", template = "Flagged cases=%s; max Cook's D=%s")
      )
      for (format in formats) {
        captures <- regmatches(text, regexec(format$pattern, text, perl = TRUE))[[1L]]
        if (length(captures)) return(do.call(sprintf,
          c(list(statedu_localized_text(language, format$template)), as.list(captures[-1L]))))
      }
      # These two generated reason variants differ only in initial capitalization.
      if (text %in% c("no clear slope heterogeneity", "no clear nonlinearity")) {
        canonical <- paste0(toupper(substr(text, 1L, 1L)), substring(text, 2L))
        return(statedu_localized_text(language, canonical))
      }
      # Repeated-measures diagnostic cells join complete system messages.
      sentences <- c(
        "Cell-level Shapiro-Wilk checks did not flag p < .05.",
        "The repeated outcome is treated as continuous Gaussian; mixed modeling handles unbalanced repeated records.")
      if (identical(text, paste(sentences, collapse = " "))) {
        return(paste(vapply(sentences, function(x) statedu_localized_text(language, x), character(1)), collapse = " "))
      }
    # Diagnostic reasons can combine fixed application phrases with test values.
    # Match complete diagnostic phrases, leaving variable labels and values intact.
    fragments <- c("Normality not met", "Homogeneity not met", "OLS selected manually",
      "Residual diagnostics not run", "Normality met", "Homogeneity met",
        "Autocorrelation likely", "OLS selected", "Bootstrap used", "HC3 used",
        "Sphericity not met", "Sphericity met", "not checked")
    for (phrase in fragments) text <- gsub(phrase, statedu_localized_text(language, phrase), text, fixed = TRUE)
    return(text)
  }
  korean <- c(
    "Variable" = "변수", "Variables" = "변수", "Message" = "메시지",
    "Status" = "상태", "Reason" = "사유", "Method" = "방법",
    "Result" = "결과", "Type" = "유형", "N" = "N", "Item" = "항목",
    "Value" = "값", "Check" = "검토 항목", "Selected" = "선택",
    "Selection" = "선택", "Factor" = "요인", "Component" = "성분",
    "Eigenvalue" = "고유값", "Eigenvalues" = "고유값",
    "Analysis" = "분석", "Model" = "모형", "Term" = "항", "Measure" = "측정값",
    "Statistic" = "통계량", "Estimate" = "추정치", "SE" = "표준오차",
    "Skewness" = "왜도", "Kurtosis" = "첨도",
    "Cumulative %" = "누적 %", "Requested" = "요청", "Valid" = "유효",
    "Valid %" = "유효 %", "Adequate" = "충분",
    "Lower" = "하한", "Upper" = "상한", "Reference" = "기준",
    "Criterion" = "기준", "Decision" = "판정", "Recommendation" = "권고",
    "Details" = "세부내용", "Package" = "패키지", "Test" = "검정",
    "Normality" = "정규성", "Homogeneity" = "등분산성", "Post-hoc" = "사후분석",
    "Bartlett's test of sphericity" = "Bartlett 구형성 검정",
    "Model overview" = "모형 개요", "Assumption review" = "가정 검토",
    "Normality review" = "정규성 검토", "Recommended interpretation" = "권고 해석",
    "Item analysis" = "문항 분석", "Auxiliary agreement indices" = "보조 일치도 지수",
    "Suitability" = "적합성", "Measurement level" = "측정수준", "Subfactor" = "하위요인",
    "Omitted variables" = "제외된 변수", "Expected counts" = "기대도수",
    "Data structure" = "자료 구조", "Missing data" = "결측 자료",
    "Dependent variable" = "종속변수", "Independent variable" = "독립변수",
    "Outcome" = "결과변수", "Predictor" = "예측변수",
    "Dependent" = "종속변수", "Independent" = "독립",
    "Residual normality" = "잔차 정규성", "Residual homogeneity" = "잔차 등분산성",
    "Autocorrelation" = "자기상관", "Autocorrelation likely" = "자기상관 가능성 높음",
    "Inconclusive" = "판정 불가", "Not run" = "실행하지 않음",
    "Normality met" = "정규성 충족", "Normality not met" = "정규성 미충족",
    "Homogeneity met" = "등분산성 충족", "Homogeneity not met" = "등분산성 미충족",
    "Residual diagnostics not run" = "잔차 진단을 실행하지 않음",
    "OLS selected" = "OLS 선택", "OLS selected manually" = "OLS 수동 선택",
    "Bootstrap used" = "부트스트랩 사용", "HC3 used" = "HC3 사용",
    "Bootstrap Regression" = "부트스트랩 회귀분석",
    "Bootstrap regression" = "부트스트랩 회귀분석",
    "Bootstrap + HC3 Regression" = "부트스트랩 + HC3 회귀분석",
    "HC3 Regression" = "HC3 회귀분석", "OLS Regression" = "OLS 회귀분석",
    "Bootstrap diagnostics" = "부트스트랩 진단",
    "Effect Size Guidelines" = "효과크기 해석 기준",
    "Reference" = "기준", "Small" = "작음", "Medium" = "중간", "Large" = "큼",
    "Warning" = "경고", "Warnings" = "경고",
    "Skipped" = "제외됨", "Skipped analyses" = "제외된 분석",
    "Warnings / skipped analyses" = "경고 / 제외된 분석",
    "Pass" = "통과", "Passed" = "통과", "Fail" = "실패", "Failed" = "실패",
    "Complete" = "완료", "Incomplete" = "불완전", "Error" = "오류",
    "Caution" = "주의", "Available" = "사용 가능", "Not available" = "사용 불가",
    "Assessed" = "평가됨", "Not assessed" = "평가하지 않음",
    "Satisfied" = "충족", "satisfied" = "충족",
    "Not satisfied" = "미충족", "not satisfied" = "미충족",
    "Not required" = "필요하지 않음", "Not testable" = "평가 불가",
    "Potential violation" = "잠재적 위반",
    "Yes" = "예", "No" = "아니요",
    "KMO values of .60 or higher and a significant Bartlett test are commonly treated as evidence that factor analysis is appropriate." = "KMO가 .60 이상이고 Bartlett 검정이 유의하면 일반적으로 요인분석에 적합한 것으로 판단합니다.",
    "KMO and Bartlett's test are reported as descriptive diagnostics for whether the variable set has enough shared association for dimension reduction." = "KMO와 Bartlett 검정은 변수 집합이 차원 축소에 충분한 공통 연관성을 갖는지 판단하는 기술적 진단입니다.",
    "Mardia normality is treated as satisfied when both skewness and kurtosis tests have p >= .05." = "Mardia 정규성은 왜도와 첨도 검정이 모두 p >= .05일 때 충족으로 판단합니다.",
    "Normality is treated as satisfied when each variable has |skewness| < 2 and |kurtosis| < 7." = "각 변수의 |왜도| < 2 및 |첨도| < 7이면 정규성을 충족한 것으로 판단합니다.",
    "Eigenvalue >= 1.0 and the scree plot are screening aids; consider parallel analysis or theory when deciding the final number of factors." = "고유값 >= 1.0과 스크리 도표는 선별 기준이며, 최종 요인 수는 평행분석과 이론을 함께 고려해 결정합니다.",
    "The fixed factor count should be checked against the scree plot, interpretability, and theory; parallel analysis can be useful as an additional check." = "고정한 요인 수는 스크리 도표, 해석 가능성 및 이론과 대조하고 평행분석을 추가로 참고합니다."
  )
  if (text %in% names(korean)) unname(korean[[text]]) else text
}

# User data can equal an application phrase (for example a variable named
# "Normality" or a category named "Yes"). Preserve by table position, not by
# looking at the cell's spelling or script.
result_appendix_preserve_data <- function(localized, source) {
  if (!is.data.frame(source) || !is.data.frame(localized) || !identical(dim(source), dim(localized))) return(localized)
  keys <- tolower(trimws(names(source)))
  identity_keys <- c("id", "name", "variable", "variables", "variable name", "variable label", "value label", "label", "labels",
    "dependent variable", "independent variable", "dependent", "independent", "dv", "iv", "outcome", "predictor", "predictors",
    "covariate", "covariates", "term", "terms", "group", "group 1", "group 2", "group variable", "level", "levels", "category",
    "raw value", "code", "rater", "rater 1", "rater 2", "variable 1", "variable 2", "pair", "contrast", "comparison",
    "path", "stratum", "subject", "subject id", "study", "study id", "study name", "study name / citation",
    "x", "y", "mediator", "moderator", "factor", "factor1", "factor2", "factor 1", "factor 2",
    "latent", "latent factor", "indicator", "indicator 1", "indicator 2", "construct", "lhs", "rhs", "element")
  protected <- which(keys %in% identity_keys)
  explicit <- attr(source, "result_user_columns", exact = TRUE)
  protected <- unique(c(protected, if (is.numeric(explicit)) explicit else match(explicit, names(source))))
  protected <- protected[!is.na(protected) & protected >= 1L & protected <= ncol(source)]
  for (index in seq_along(source)) {
    if (index %in% protected || is.numeric(source[[index]]) || is.logical(source[[index]])) localized[[index]] <- source[[index]]
  }
  # Overview tables store variable assignments vertically, not in a Variable column.
  item <- match("item", keys)
  value <- match("value", keys)
  if (!is.na(item) && !is.na(value)) {
    rows <- tolower(trimws(as.character(source[[item]]))) %in% c(identity_keys,
      "focal x analyses", "direct x -> y paths", "mediators", "moderator", "time variable", "event variable",
      "repeated-measures variables", "time labels", "cluster", "weight", "offset")
    if (any(rows)) localized[[value]][rows] <- source[[value]][rows]
  }
  user_headers <- attr(source, "result_user_headers", exact = TRUE)
  if (!is.numeric(user_headers)) user_headers <- match(user_headers, names(source))
  # Matrix axes are variable labels, including labels equal to a diagnostic term.
  if (nrow(source) > 0L && ncol(source) == nrow(source) + 1L && identical(as.character(source[[1L]]), names(source)[-1L])) {
    localized[[1L]] <- source[[1L]]
    user_headers <- union(user_headers, seq.int(2L, ncol(source)))
  }
  user_headers <- user_headers[!is.na(user_headers) & user_headers >= 1L & user_headers <= ncol(source)]
  names(localized)[user_headers] <- names(source)[user_headers]
  user_cells <- attr(source, "result_user_cells", exact = TRUE)
  if (is.matrix(user_cells) && ncol(user_cells) == 2L && is.numeric(user_cells)) {
    for (index in seq_len(nrow(user_cells))) {
      row <- user_cells[index, 1L]; column <- user_cells[index, 2L]
      if (is.finite(row) && is.finite(column) && row == as.integer(row) && column == as.integer(column) &&
          row >= 1L && row <= nrow(source) && column >= 1L && column <= ncol(source)) {
        localized[[column]][row] <- source[[column]][row]
      }
    }
  }
  localized
}

result_appendix_localize_table <- function(table, language = NULL) {
  if (!is.data.frame(table)) {
    return(table)
  }
  source_table <- table
  language <- result_appendix_table_language(language)
  if (identical(language, "en")) {
    attr(table, "result_table_role") <- "appendix"
    attr(table, "result_table_language") <- "en"
    return(table)
  }
  original_names <- names(table)
  diagnostic_columns <- original_names[
    tolower(original_names) %in% c(
      "type", "status", "result", "item", "metric", "assessment",
      "decision", "check", "analysis", "reason", "recommendation",
      "selected", "selection", "normality"
    )
  ]
  for (column in diagnostic_columns) {
    table[[column]] <- vapply(as.character(table[[column]]), result_appendix_ui_text, character(1), language = language)
  }
  translate_cell <- function(value) {
    value <- as.character(value %||% "")
    if (is.na(value)) return(NA_character_)
    if (!nzchar(value)) return("")
    lines <- strsplit(value, "\n", fixed = TRUE)[[1]]
    paste(vapply(lines, result_appendix_ui_text, character(1), language = language), collapse = "\n")
  }
  for (column in original_names) {
    if (!is.character(table[[column]]) && !is.factor(table[[column]])) next
    table[[column]] <- vapply(as.character(table[[column]]), translate_cell, character(1))
  }
  names(table) <- vapply(original_names, result_appendix_ui_text, character(1), language = language)
  attr(table, "result_table_role") <- "appendix"
  attr(table, "result_table_language") <- language
  result_appendix_preserve_data(table, source_table)
}

result_sci_note_categories <- function() {
  c("format", "abbreviations", "estimation", "reference", "multiplicity", "symbol")
}

result_sci_note_category <- function(category) {
  key <- gsub("[^[:alnum:]]+", "", tolower(as.character(category %||% "symbol")[[1]]))
  aliases <- c(
    format = "format", presentation = "format", scale = "format",
    abbreviation = "abbreviations", abbreviations = "abbreviations", terms = "abbreviations",
    estimation = "estimation", estimate = "estimation", se = "estimation", ci = "estimation", inference = "estimation",
    reference = "reference", coding = "reference", referencecoding = "reference",
    multiplicity = "multiplicity", significance = "multiplicity", adjustment = "multiplicity", alpha = "multiplicity",
    symbol = "symbol", symbols = "symbol", warning = "symbol", tablespecific = "symbol"
  )
  value <- if (key %in% names(aliases)) unname(aliases[[key]]) else "symbol"
  if (value %in% result_sci_note_categories()) value else "symbol"
}

result_sci_note_values <- function(value) {
  if (is.null(value)) {
    return(character(0))
  }
  values <- trimws(as.character(unlist(value, recursive = TRUE, use.names = FALSE)))
  values[!is.na(values) & nzchar(values)]
}

result_sci_notes <- function(
  format = NULL,
  abbreviations = NULL,
  estimation = NULL,
  reference = NULL,
  multiplicity = NULL,
  symbol = NULL,
  notes = NULL
) {
  grouped <- stats::setNames(
    list(format, abbreviations, estimation, reference, multiplicity, symbol),
    result_sci_note_categories()
  )
  if (!is.null(notes)) {
    note_list <- if (is.list(notes)) notes else as.list(notes)
    note_names <- names(note_list)
    if (is.null(note_names)) {
      grouped$symbol <- c(grouped$symbol, note_list)
    } else {
      for (index in seq_along(note_list)) {
        category <- result_sci_note_category(note_names[[index]])
        grouped[[category]] <- c(grouped[[category]], note_list[[index]])
      }
    }
  }
  ordered <- unlist(lapply(result_sci_note_categories(), function(category) {
    result_sci_note_values(grouped[[category]])
  }), use.names = FALSE)
  if (length(ordered) == 0L) {
    return(character(0))
  }
  normalized <- tolower(gsub("\\s+", " ", trimws(ordered), perl = TRUE))
  ordered[!duplicated(normalized)]
}

# Regression's abbreviation order is the common publication-note contract.
# Other analysis-specific definitions retain their own order after these keys.
result_note_clause_rank <- function(part) {
  key <- part
  if (grepl("<[^>]+>|&[A-Za-z#]", key, perl = TRUE)) {
    key <- xml2::xml_text(xml2::read_html(paste0("<div>", key, "</div>")))
  }
  key <- sub("^\\s*[0-9]+(?:[.]\\s*|\\s+)", "", trimws(key), perl = TRUE)
  patterns <- c(
    "^M(?:\\s*[±+]|\\s*=)|^SD\\s*=",
    "^(?:(?:HC[0-5]|Boot(?:strap)?|Robust)\\s+)?SE\\s*=",
    "^(?:95%\\s*)?CI\\s*=", "^LLCI\\s*=", "^ULCI\\s*=", "^(Tol|Tolerance)\\s*=", "^VIF\\s*=",
    "^(d(?:\\([^)]*\\))?|DW)\\s*=", "^z\\s*\\(p\\)\\s*=",
    "^(χ[²2]|chi[- ]square)\\s*\\(p\\)\\s*=",
    "^f[²2]\\s*=", "^sr[²2]\\s*=")
  hits <- which(vapply(patterns, grepl, logical(1), x = key, perl = TRUE, ignore.case = TRUE))
  if (length(hits)) return(hits[[1]])
  if (grepl("^[^.;=]{1,80}\\s=\\s", key, perl = TRUE)) return(length(patterns) + 1L)
  length(patterns) + 2L
}

result_publication_note <- function(text) {
  values <- result_sci_note_values(text)
  # Keep markup on definitions and numeric footnote markers; remove only the
  # redundant prefix, including the emphasized prefix used by SEM tables.
  values <- gsub("(?i)^(?:<(?:em|i|b|strong)>\\s*)?(?:Notes?|주석|주)[.:]\\s*(?:</(?:em|i|b|strong)>\\s*)?", "", values, perl = TRUE)
  text <- trimws(paste(values, collapse = " "))
  if (!nzchar(text)) return("")
  definition <- "(?:(?:<[^>]+>)*(?:95%\\s*CI|[A-Za-zα-ωΑ-Ωχ²±])[^=.;]{0,100}\\s*=)"
  method <- "(?:<[^>]+>)*[A-Z가-힣]|Analysis method:|Post-hoc:|(?:Welch|Greenhouse-Geisser|Huynh-Feldt)\\b"
  starts <- paste0("(?:", definition, "|", method, ")")
  parts <- strsplit(text, paste0("(?<![0-9]\\.)(?<=[.;])\\s+(?=(?:(?:<sup>)?[0-9]+(?:</sup>|\\.)?\\s*)?", starts, ")"), perl = TRUE)[[1]]
  if (!length(parts)) return("")
  parts <- parts[order(vapply(parts, result_note_clause_rank, integer(1)), seq_along(parts))]
  parts <- sub("[.;]$", "", trimws(parts))
  combined <- paste(unique(parts[nzchar(parts)]), collapse = "; ")
  if (grepl("[.!?。！？]$", combined)) combined else paste0(combined, ".")
}

# Use these wrappers for rich result notes that do not use coefficient_html_table.
# Their ... signature preserves the original p/div attributes and inline markup.
result_normalize_note_node <- function(node) {
  content <- paste(vapply(node$children, function(child) {
    if (is.character(child) && !inherits(child, "html")) {
      as.character(htmltools::htmlEscape(child))
    } else as.character(htmltools::renderTags(child)$html)
  }, character(1)), collapse = " ")
  node$children <- list(htmltools::HTML(result_publication_note(content)))
  node$attribs[["data-result-note-contract"]] <- "regression-v1"
  node
}

result_note_paragraph <- function(...) result_normalize_note_node(tags$p(...))
result_note_div <- function(...) result_normalize_note_node(tags$div(...))

result_sci_note_text <- function(..., prefix = "") {
  notes <- result_sci_notes(...)
  if (length(notes) == 0L) {
    return("")
  }
  complete_sentence <- function(value) {
    value <- trimws(value)
    if (!grepl("[.!?]$", value, perl = TRUE)) paste0(value, ".") else value
  }
  result_publication_note(paste(c(prefix, vapply(notes, complete_sentence, character(1))), collapse = " "))
}

result_table_portrait_capacity <- function() 590L

result_table_landscape_capacity <- function() 890L

result_table_intrinsic_width <- function(
  table,
  columns = NULL,
  first_width = 118,
  default_width = 62,
  min_width = 0,
  explicit_widths = NULL
) {
  if (!is.data.frame(table) || ncol(table) == 0L) {
    return(max(0, as.numeric(min_width %||% 0)))
  }
  columns <- as.character(columns %||% names(table))
  columns <- columns[columns %in% names(table)]
  if (length(columns) == 0L) {
    columns <- names(table)
  }
  if (is.numeric(explicit_widths) && length(explicit_widths) == length(columns) && all(is.finite(explicit_widths))) {
    total <- sum(pmax(24, explicit_widths))
    return(as.integer(ceiling(max(total, as.numeric(min_width %||% 0)))))
  }
  numeric_like <- function(values) {
    values <- trimws(as.character(values))
    values <- values[nzchar(values)]
    length(values) == 0L || mean(grepl("^(?:[-+]?\\d*(?:\\.\\d+)?|<\\.?\\d+|NA|Inf|-Inf)(?:\\s*\\([^)]*\\))?$", values, perl = TRUE)) >= 0.8
  }
  widths <- vapply(seq_along(columns), function(index) {
    column <- columns[[index]]
    values <- c(column, as.character(table[[column]] %||% ""))
    longest_line <- max(nchar(unlist(strsplit(values, "\n", fixed = TRUE)), type = "width"), na.rm = TRUE)
    base <- if (index == 1L) first_width else default_width
    if (index == 1L || !numeric_like(table[[column]])) {
      base <- max(base, min(220, 18 + longest_line * 6.2))
    } else {
      base <- max(base, min(92, 18 + longest_line * 6.2))
    }
    base
  }, numeric(1))
  as.integer(ceiling(max(sum(widths), as.numeric(min_width %||% 0))))
}

result_table_orientation <- function(intrinsic_width, orientation = "auto") {
  requested <- tolower(trimws(as.character(orientation %||% "auto")[[1]]))
  if (requested %in% c("portrait", "landscape")) {
    return(requested)
  }
  if (is.finite(as.numeric(intrinsic_width)) && as.numeric(intrinsic_width) > result_table_portrait_capacity()) {
    "landscape"
  } else {
    "portrait"
  }
}

result_table_contract <- function(table = NULL, role = NULL, language = NULL, orientation = "auto", intrinsic_width = NULL) {
  if (identical(orientation, "auto") && is.data.frame(table) && nrow(table) > 0L &&
      ncol(table) == nrow(table) + 1L && identical(as.character(table[[1L]]), names(table)[-1L])) {
    orientation <- if (nrow(table) <= 9L) "portrait" else "landscape"
  }
  role <- result_table_role(role, table = table)
  if (is.null(language) && !is.null(table)) {
    language <- attr(table, "result_table_language", exact = TRUE)
  }
  if (is.null(intrinsic_width) || !is.finite(as.numeric(intrinsic_width))) {
    intrinsic_width <- if (is.data.frame(table)) result_table_intrinsic_width(table) else 0L
  }
  orientation <- result_table_orientation(intrinsic_width, orientation)
  list(
    role = role,
    language = result_table_language(role, language),
    orientation = orientation,
    intrinsic_width = as.integer(ceiling(as.numeric(intrinsic_width))),
    classes = paste(
      "result-table-sheet",
      "result-table-sheet--b5",
      paste0("result-table-sheet--", role),
      paste0("result-table-sheet--", orientation)
    )
  )
}

# Group confidence limits in the actual table markup so every snapshot export
# receives the same row/column spans as the displayed result.
result_ci_expand_columns <- function(table, force = FALSE) {
  if (!is.data.frame(table) || !nrow(table)) return(table)
  if (!force && !isTRUE(attr(table, "bootstrap_regression")) &&
      !any(grepl("boot|부트스트랩", names(table), ignore.case=TRUE))) return(table)
  candidates <- which(grepl("95%\\s*(CI|신뢰구간)$", names(table), ignore.case=TRUE))
  if (!length(candidates)) return(table)
  number <- "[-+−]?(?:[0-9]+(?:[.][0-9]*)?|[.][0-9]+)(?:[eE][-+]?[0-9]+)?|[-+]?Inf"
  pattern <- paste0("^\\s*(?:\\[|\\()?\\s*(",number,")\\s*(?:,|~|–|—|\\bto\\b)\\s*(",number,")\\s*(?:\\]|\\))?\\s*$")
  attrs <- attributes(table)
  columns <- list(); widths <- numeric(); original_widths <- attr(table,"compact_column_widths")
  for (i in seq_along(table)) {
    values <- as.character(table[[i]])
    matches <- regmatches(values, regexec(pattern, values, perl=TRUE))
    valid <- lengths(matches) == 3L
    empty <- is.na(values) | trimws(values) %in% c("", "NA", "N/A", "—", "-")
    split <- i %in% candidates && all(valid | empty)
    if (split) {
      prefix <- trimws(sub("95%\\s*(CI|신뢰구간)$", "", names(table)[i], ignore.case=TRUE))
      prefix <- sub("^(?:Bootstrap|Boot|부트스트랩)\\b\\s*", "", prefix, perl=TRUE)
      for (j in 1:2) columns[[trimws(paste(prefix,c("LLCI","ULCI")[j]))]] <- vapply(seq_along(matches),function(k)if(valid[k])matches[[k]][j+1L] else if(is.na(values[k]))"" else values[k],character(1))
      if (length(original_widths)==ncol(table)) widths <- c(widths,rep(original_widths[i]/2,2))
    } else {
      columns[[names(table)[i]]] <- table[[i]]
      if (length(original_widths)==ncol(table)) widths <- c(widths,original_widths[i])
    }
  }
  expanded <- as.data.frame(columns,check.names=FALSE,stringsAsFactors=FALSE)
  for (name in setdiff(names(attrs),c("names","row.names","class","compact_column_widths"))) attr(expanded,name)<-attrs[[name]]
  if(length(widths))attr(expanded,"compact_column_widths")<-widths
  expanded
}

result_ci_header <- function(content) {
  flatten <- function(items) {
    out <- list()
    for (item in items) {
      if (inherits(item, "shiny.tag")) out <- c(out, list(item))
      else if (is.list(item)) out <- c(out, flatten(item))
    }
    out
  }
  label <- function(cell) {
    value <- trimws(gsub("<[^>]+>", " ", gsub("<sup[^>]*>.*?</sup>", "", as.character(cell), perl=TRUE)))
    value <- sub("^2[.]5%\\s*CI$", "95% CI lower", value)
    sub("^97[.]5%\\s*CI$", "95% CI upper", value)
  }
  limit_cell <- function(cell, text) {
    superscripts <- list()
    collect <- function(node) {
      if (inherits(node,"shiny.tag")) {
        if (node$name == "sup") superscripts[[length(superscripts)+1L]] <<- node
        else lapply(node$children,collect)
      } else if(is.list(node))lapply(node,collect)
      invisible(NULL)
    }
    collect(cell); cell$children <- c(list(text),superscripts); cell
  }
  changed <- FALSE
  visit <- function(node) {
    if (!inherits(node, "shiny.tag")) {
      if (is.list(node)) return(lapply(node, visit))
      return(node)
    }
    if (identical(node$name, "thead")) {
      rows <- flatten(node$children)
      if (!length(rows)) return(node)
      cells <- flatten(rows[[length(rows)]]$children)
      keys <- vapply(cells, function(cell) tolower(gsub("[^[:alnum:]]", "", gsub("상한", "upper", gsub("하한", "lower", label(cell))))), character(1))
      pairs <- integer()
      for (i in seq_along(keys)) {
        if (i == length(keys)) next
        lower <- keys[i]; upper <- keys[i+1L]
        if ((grepl("llci$", lower) && identical(sub("llci$", "", lower), sub("ulci$", "", upper)) && grepl("ulci$", upper)) ||
            (grepl("ci.*lower$", lower) && identical(sub("lower$", "", lower), sub("upper$", "", upper)) && grepl("upper$", upper)) ||
            (lower == "lower" && upper == "upper" && grepl("95%", as.character(node), fixed = TRUE))) pairs <- c(pairs, i)
      }
      if (!length(pairs)) return(node)
      # Existing CI groups already have the required two-tier geometry.
      # RMSEA can use 90% (or another explicit level); never add a 95% tier
      # or a 95% definition to a complete, explicitly labelled interval.
      explicit_group <- if (length(rows) > 1L) as.character(rows[[length(rows)-1L]]) else ""
      if (length(rows) > 1L && all(keys %in% c("lower", "upper", "llci", "ulci")) &&
          grepl("[0-9]+(?:[.][0-9]+)?%[[:space:]]*CI", explicit_group, perl = TRUE) &&
          !grepl("95%", explicit_group, fixed = TRUE)) return(node)
      existing <- length(rows) > 1L && all(keys[seq_along(keys)] %in% c("lower", "upper", "llci", "ulci")) &&
        grepl("95%", as.character(rows[[length(rows)-1L]]), fixed = TRUE)
      if (existing) {
        for (i in pairs) { cells[[i]] <- limit_cell(cells[[i]],"LLCI"); cells[[i+1L]] <- limit_cell(cells[[i+1L]],"ULCI") }
        rows[[length(rows)]]$children <- cells
      } else {
        top <- list(); bottom <- list(); i <- 1L
        while (i <= length(cells)) {
          cell <- cells[[i]]
          if (i %in% pairs) {
            group <- cell
            original_label <- label(cell)
            prefix <- trimws(sub("(?:LLCI|95%.*|lower).*$", "", original_label, ignore.case=TRUE, perl=TRUE))
            prefix <- sub("^(?:Bootstrap|Boot|부트스트랩)\\b\\s*", "", prefix, perl=TRUE)
            group$children <- list(if (grepl("^exp", prefix, ignore.case=TRUE)) "exp(B) 95% CI" else trimws(paste(prefix, "95% CI")))
            group$attribs$colspan <- 2L; group$attribs$rowspan <- NULL
            group$attribs$class <- paste(group$attribs$class %||% "", "result-ci-group")
            group$attribs$style <- paste0(group$attribs$style %||% "", ";text-align:center!important;border-bottom:1px solid #1f2937!important;")
            top <- c(top, list(group))
            cell <- limit_cell(cell,"LLCI"); cells[[i+1L]] <- limit_cell(cells[[i+1L]],"ULCI")
            bottom <- c(bottom, list(cell, cells[[i+1L]])); i <- i + 2L
          } else {
            cell$attribs$rowspan <- 2L; top <- c(top, list(cell)); i <- i + 1L
          }
        }
        if (length(rows) > 1L) for (r in seq_len(length(rows)-1L)) {
          prior <- flatten(rows[[r]]$children)
          for (j in seq_along(prior)) {
            span <- as.integer(prior[[j]]$attribs$rowspan %||% 1L)
            if (r + span - 1L >= length(rows)) prior[[j]]$attribs$rowspan <- span + 1L
          }
          rows[[r]]$children <- prior
        }
        rows[[length(rows)]] <- do.call(tags$tr, top)
        rows <- c(rows, list(do.call(tags$tr, bottom)))
      }
      node$children <- rows; changed <<- TRUE
      return(node)
    }
    node$children <- lapply(node$children, visit)
    node
  }
  content <- visit(content)
  if (changed) attr(content, "result_ci_header") <- TRUE
  content
}

result_ci_note <- function(notes) {
  notes <- Filter(Negate(is.null), notes)
  text <- paste(vapply(notes, as.character, character(1)), collapse = " ")
  definitions <- c("95% CI" = "95% CI = 95% confidence interval", LLCI = "LLCI = lower confidence limit", ULCI = "ULCI = upper confidence limit")
  definitions <- definitions[!vapply(names(definitions),function(key)grepl(paste0(key,"\\s*="),text),logical(1))]
  if (!length(definitions)) return(notes)
  definition <- paste(definitions, collapse="; ")
  added <- FALSE
  visit <- function(node) {
    if (added) return(node)
    if (inherits(node, "shiny.tag")) {
      classes <- node$attribs$class %||% ""
      if (grepl("(coefficient-note|structural.*note)", classes) && !grepl("model-notes", classes)) {
        node$children <- list(HTML(result_publication_note(paste(definition, ";", paste(vapply(node$children, as.character, character(1)), collapse=" ")))))
        added <<- TRUE
      } else node$children <- lapply(node$children, visit)
    } else if (is.list(node)) node <- lapply(node, visit)
    node
  }
  notes <- lapply(notes, visit)
  if (!added) notes <- c(list(result_note_tag(paste0(definition, "."))), notes)
  notes
}

result_table_apply_contract <- function(table_tag, contract) {
  if (is.null(table_tag)) {
    return(NULL)
  }
  center_headers <- function(node) {
    if (!inherits(node, "shiny.tag")) return(node)
    if (identical(node$name, "th")) node$attribs$style <- paste0(node$attribs$style %||% "", ";text-align:center !important;")
    if (identical(contract$orientation, "portrait") && node$name %in% c("th", "td")) {
      node$attribs$style <- paste0(node$attribs$style %||% "",
        ";width:auto !important;min-width:0 !important;white-space:normal !important;overflow-wrap:anywhere !important;",
        if (grepl("paired-grouped-table", table_tag$attribs$class %||% "")) "padding:5px 3px !important;" else "")
    }
    if (identical(contract$orientation, "portrait") && identical(node$name, "col") &&
        !grepl("crosstab-", node$attribs$class %||% "")) {
      style <- node$attribs$style %||% ""
      if (!grepl("%", style, fixed = TRUE)) {
        pixels <- regmatches(style, regexec("(?:^|;)\\s*width:\\s*([0-9.]+)px", style, perl = TRUE))[[1]]
        width <- if (length(pixels) > 1L && contract$intrinsic_width > 0)
          sprintf("%.4f%%", 100 * as.numeric(pixels[2]) / contract$intrinsic_width) else "auto"
        node$attribs$style <- paste0(style, ";width:", width, " !important;min-width:0 !important;")
      }
    }
    node$children <- lapply(node$children, function(child) {
      if (inherits(child, "shiny.tag")) center_headers(child) else if (is.list(child)) lapply(child, center_headers) else child
    })
    node
  }
  table_tag <- result_ci_header(center_headers(table_tag))
  if (identical(contract$orientation, "portrait")) table_tag$attribs$style <- paste0(
    table_tag$attribs$style %||% "", ";width:100% !important;min-width:0 !important;max-width:100% !important;table-layout:fixed !important;")
  table_tag$attribs$class <- paste(table_tag$attribs$class %||% "", "result-table-contract-table")
  table_tag$attribs$`data-result-table-role` <- contract$role
  table_tag$attribs$`data-result-table-language` <- contract$language
  table_tag$attribs$`data-result-table-orientation` <- contract$orientation
  table_tag$attribs$`data-result-table-intrinsic-width` <- as.character(contract$intrinsic_width)
  table_tag$attribs$style <- paste0(
    table_tag$attribs$style %||% "",
    "--result-table-intrinsic-width:",
    max(0L, as.integer(contract$intrinsic_width)),
    "px;"
  )
  attr(table_tag, "result_table_contract") <- contract
  table_tag
}

analysis_result_table_section <- function(title, table, class = "result-section regression-result-panel", table_fn = coefficient_html_table) {
  if (!analysis_has_rows(table)) {
    return(NULL)
  }
  contract <- result_table_contract(table)
  tags$div(
    class = paste(
      class,
      "result-table-sheet-section",
      paste0("result-table-sheet-section--", contract$role),
      paste0("result-table-sheet-section--", contract$orientation)
    ),
    lang = contract$language,
    tags$h3(title),
    table_fn(table)
  )
}

analysis_warning_section <- function(table, class = "result-section regression-result-panel") {
  table <- result_appendix_localize_table(table)
  analysis_result_table_section(result_appendix_ui_text("Warnings"), table, class = class)
}

analysis_skipped_section <- function(table, title = "Skipped analyses", class = "result-section regression-result-panel") {
  table <- result_appendix_localize_table(table)
  analysis_result_table_section(result_appendix_ui_text(title), table, class = class)
}

analysis_diagnostics_row <- function(table, type) {
  if (!analysis_has_rows(table)) {
    return(NULL)
  }
  pick <- function(candidates) {
    matched <- intersect(candidates, names(table))
    if (length(matched) == 0) {
      return(rep("", nrow(table)))
    }
    as.character(table[[matched[[1]]]])
  }
  data.frame(
    Type = rep(type, nrow(table)),
    `Dependent variable` = pick(c("Dependent variable", "Dependent", "Outcome")),
    `Independent variable` = pick(c("Independent variable", "Independent", "Predictor", "Variable")),
    N = pick(c("N", "n")),
    Message = pick(c("Warning", "Reason", "Message")),
    check.names = FALSE
  )
}

analysis_diagnostics_section <- function(warnings, skipped, title = "Warnings / skipped analyses", class = "result-section regression-result-panel", messages_localized = FALSE) {
  rows <- Filter(Negate(is.null), list(
    analysis_diagnostics_row(warnings, "Warning"),
    analysis_diagnostics_row(skipped, "Skipped")
  ))
  if (length(rows) == 0) {
    return(NULL)
  }
  table <- do.call(rbind, rows)
  if (all(!nzchar(table$N))) {
    table$N <- NULL
  }
  attr(table, "result_table_role") <- "appendix"
  attr(table, "result_table_language") <- result_appendix_table_language()
  if (isTRUE(messages_localized)) attr(table, "result_user_columns") <- "Message"
  analysis_result_table_section(result_appendix_ui_text(title), table, class = class, table_fn = analysis_diagnostics_html_table)
}

analysis_diagnostics_html_table <- function(table) {
  if (!analysis_has_rows(table)) {
    return(NULL)
  }
  localized_table <- result_appendix_localize_table(
    table,
    attr(table, "result_table_language", exact = TRUE)
  )
  columns <- names(table)
  localized_columns <- names(localized_table)
  message_index <- match("Message", columns)
  widths <- rep(14, length(columns))
  names(widths) <- columns
  widths[columns == "Type"] <- 10
  widths[columns %in% c("Dependent variable", "Independent variable")] <- 13
  widths[columns == "N"] <- 6
  widths[columns == "Message"] <- max(50, 100 - sum(widths[columns != "Message"]))
  if (!is.finite(message_index)) {
    widths <- rep(100 / length(columns), length(columns))
    names(widths) <- columns
  }
  body_style <- function(column, last = FALSE) {
    paste0(
      "padding:5px 7px;line-height:1.35;border-left:0;border-right:0;border-top:0;border-bottom:",
      if (isTRUE(last)) "0" else "1px solid #d7dde5",
      ";vertical-align:middle;background:transparent;white-space:normal;",
      "font-variant-numeric:tabular-nums lining-nums;font-feature-settings:'tnum' 1,'lnum' 1;",
      "text-align:", if (identical(column, "Message")) "left" else "left", ";"
    )
  }
  table_tag <- tags$table(
    class = "coefficient-table diagnostics-message-table",
    style = paste0(result_table_style(font_size = 12, min_width = 640), "table-layout:fixed;width:100%;max-width:100%;"),
    tags$colgroup(lapply(columns, function(column) {
      tags$col(style = sprintf("width:%.3f%% !important;", widths[[column]]))
    })),
    tags$thead(tags$tr(lapply(seq_along(columns), function(index) {
      column <- columns[[index]]
      tags$th(
        style = paste0(
          result_header_cell_style(identical(column, columns[[1]])),
          if (identical(column, "Message")) "text-align:left;" else ""
        ),
        localized_columns[[index]]
      )
    }))),
    tags$tbody(lapply(seq_len(nrow(table)), function(row_index) {
      tags$tr(lapply(seq_along(columns), function(index) {
        column <- columns[[index]]
        tags$td(style = body_style(column, row_index == nrow(table)), as.character(localized_table[[index]][[row_index]] %||% ""))
      }))
    }))
  )
  contract <- result_table_contract(
    localized_table,
    role = "appendix",
    intrinsic_width = max(640, result_table_intrinsic_width(table))
  )
  result_table_with_notes(result_table_apply_contract(table_tag, contract))
}

result_table_style <- function(font_size = 12, min_width = 480) {
  paste(
    sprintf("width:auto;min-width:%dpx;border-collapse:collapse;border-spacing:0;", min_width),
    "border-top:2px solid #1f2937;border-bottom:2px solid #1f2937;",
    sprintf("color:#2f3a46;font-size:%spx;background:transparent;", as.character(font_size))
  )
}

result_header_cell_style <- function(first = FALSE, compact = FALSE, compact_font_size = 12, compact_width = 62, compact_first_width = 118) {
  padding <- if (isTRUE(compact)) "5px 7px" else "5px 7px"
  header_font_size <- max(8, as.numeric(compact_font_size %||% 12) - 1)
  width <- if (isTRUE(first)) {
    if (isTRUE(compact)) paste0(compact_first_width, "px") else "90px"
  } else if (isTRUE(compact)) {
    paste0(compact_width, "px")
  } else {
    "58px"
  }
  paste0(
    "padding:", padding, ";line-height:1.35;border-left:0;border-right:0;",
    "border-top:0;border-bottom:2px solid #1f2937;vertical-align:middle;",
    "font-weight:700;font-size:", header_font_size, "px;background:transparent;white-space:nowrap;",
    "min-width:", width, ";",
    "text-align:center !important;"
  )
}

result_body_cell_style <- function(first = FALSE, last = FALSE, compact = FALSE, compact_font_size = 12, compact_width = 62, compact_first_width = 118) {
  padding <- if (isTRUE(compact)) "5px 7px" else "5px 7px"
  width <- if (isTRUE(first)) {
    if (isTRUE(compact)) paste0(compact_first_width, "px") else "90px"
  } else if (isTRUE(compact)) {
    paste0(compact_width, "px")
  } else {
    "58px"
  }
  paste0(
    "padding:", padding, ";line-height:1.35;border-left:0;border-right:0;",
    "border-top:0;border-bottom:", if (isTRUE(last)) "0" else "1px solid #d7dde5", ";",
    "vertical-align:middle;background:transparent;white-space:normal;",
    "font-variant-numeric:tabular-nums lining-nums;font-feature-settings:'tnum' 1,'lnum' 1;",
    "min-width:", width, ";",
    "white-space:pre-line;",
    "text-align:", if (isTRUE(first)) "left" else "right", " !important;"
  )
}

result_note_tag <- function(text, class = "coefficient-note") {
  text <- result_sci_note_values(text)
  if (length(text) == 0L) {
    return(NULL)
  }
  tags$div(class = class, style = "white-space:pre-line;", result_publication_note(text))
}

result_table_with_notes <- function(table_tag, ..., class = "result-table-with-note") {
  notes <- list(...)
  notes <- Filter(Negate(is.null), notes)
  if (is.null(table_tag)) {
    return(NULL)
  }
  contract <- attr(table_tag, "result_table_contract", exact = TRUE)
  if (is.null(contract) || !is.list(contract)) {
    contract <- result_table_contract()
    table_tag <- result_table_apply_contract(table_tag, contract)
  }
  if (isTRUE(attr(table_tag, "result_ci_header")) && !isTRUE(attr(table_tag,"result_ci_note_deferred"))) notes <- result_ci_note(notes)
  do.call(
    tags$div,
    c(
      list(
        class = paste(class, contract$classes),
        lang = contract$language,
        `data-result-table-sheet` = "true",
        `data-result-table-role` = contract$role,
        `data-result-table-language` = contract$language,
        `data-result-table-orientation` = contract$orientation,
        `data-result-table-intrinsic-width` = as.character(contract$intrinsic_width)
      ),
      list(table_tag),
      notes
    )
  )
}

result_cell_note_marker <- function(table, row_index, column) {
  markers <- attr(table, "note_markers", exact = TRUE)
  if (!is.data.frame(markers) || nrow(markers) == 0) {
    return("")
  }
  selected <- markers$row == row_index & markers$column == column
  if (identical(class(markers), "data.frame") && is.logical(selected) &&
      is.null(dim(selected)) && !is.object(selected) && length(selected) == nrow(markers) &&
      !anyNA(selected) && !is.object(markers$marker)) {
    index <- match(TRUE, selected, nomatch = 0L)
    return(if (index == 0L) "" else as.character(markers$marker[[index]]))
  }
  matched <- markers[selected, , drop = FALSE]
  if (nrow(matched) == 0) "" else as.character(matched$marker[[1]])
}

result_note_marker_column <- function(column) {
  key <- result_column_key(column)
  key %in% c("p", "pfortrend", "effectsize") ||
    grepl("effect|hedges|cohen|eta|omega|epsilon|cliff|bootp", key)
}

result_split_inline_marker <- function(value, marker = "", column = "") {
  value <- as.character(value %||% "")
  marker <- as.character(marker %||% "")
  if (!nzchar(value)) {
    return(c(value = value, marker = ""))
  }
  if (nzchar(marker)) {
    spaced_pattern <- paste0("\\s+", marker, "$")
    if (grepl(spaced_pattern, value, perl = TRUE)) {
      return(c(value = trimws(sub(spaced_pattern, "", value, perl = TRUE)), marker = marker))
    }
    compact_pattern <- paste0(marker, "$")
    compact_value <- sub(compact_pattern, "", value, perl = TRUE)
    compact_marker_appended <- !identical(compact_value, value) && (
      grepl("^-?\\.[0-9]{4,}$", value, perl = TRUE) ||
        grepl("^<\\.001[1-9][0-9]?$", value, perl = TRUE)
    )
    if (isTRUE(compact_marker_appended)) {
      return(c(value = compact_value, marker = marker))
    }
    return(c(value = value, marker = marker))
  }
  if (!result_note_marker_column(column)) {
    return(c(value = value, marker = ""))
  }
  spaced <- regexec("^(.+?)\\s+([1-9][0-9]?)$", value, perl = TRUE)
  spaced_match <- regmatches(value, spaced)[[1]]
  if (length(spaced_match) == 3L) {
    return(c(value = trimws(spaced_match[[2]]), marker = spaced_match[[3]]))
  }
  compact <- regexec("^((?:<\\.001)|(?:-?(?:0)?\\.[0-9]{3,}))([1-9][0-9]?)$", value, perl = TRUE)
  compact_match <- regmatches(value, compact)[[1]]
  if (length(compact_match) == 3L) {
    return(c(value = compact_match[[2]], marker = compact_match[[3]]))
  }
  c(value = value, marker = "")
}

result_cell_bold <- function(table, row_index, column) {
  bold_cells <- attr(table, "bold_cells", exact = TRUE)
  if (!is.data.frame(bold_cells) || nrow(bold_cells) == 0) {
    return(FALSE)
  }
  any(bold_cells$row == row_index & bold_cells$column == column)
}

result_cell_style_extra <- function(table, row_index, column) {
  cell_styles <- attr(table, "cell_styles", exact = TRUE)
  if (!is.data.frame(cell_styles) || nrow(cell_styles) == 0) {
    return("")
  }
  selected <- cell_styles$row == row_index & cell_styles$column == column
  # Read ordinary style vectors directly, preserving duplicate-match order.
  if (identical(class(cell_styles), "data.frame") && is.logical(selected) &&
      is.null(dim(selected)) && !is.object(selected) && length(selected) == nrow(cell_styles) &&
      is.character(cell_styles$style) && !is.object(cell_styles$style) && is.null(dim(cell_styles$style))) {
    return(paste(as.character(cell_styles$style[selected]), collapse = ""))
  }
  matched <- cell_styles[selected, , drop = FALSE]
  if (nrow(matched) == 0 || !"style" %in% names(matched)) {
    return("")
  }
  paste(as.character(matched$style), collapse = "")
}

result_cell_span_start <- function(table, row_index, column) {
  spans <- attr(table, "spanning_cells", exact = TRUE)
  if (!is.data.frame(spans) || nrow(spans) == 0) {
    return(NULL)
  }
  selected <- spans$row == row_index & spans$start_column == column
  if (identical(class(spans), "data.frame") && is.logical(selected) &&
      !is.object(selected) && is.null(dim(selected)) && length(selected) == nrow(spans) &&
      !anyNA(selected) && !any(vapply(spans, is.object, logical(1)))) {
    index <- match(TRUE, selected, nomatch = 0L)
    return(if (index == 0L) NULL else spans[index, , drop = FALSE])
  }
  matched <- spans[selected, , drop = FALSE]
  if (nrow(matched) == 0) NULL else matched[1, , drop = FALSE]
}

result_cell_covered_by_span <- function(table, row_index, column, columns) {
  spans <- attr(table, "spanning_cells", exact = TRUE)
  if (!is.data.frame(spans) || nrow(spans) == 0) {
    return(FALSE)
  }
  column_index <- match(column, columns)
  if (!is.finite(column_index)) {
    return(FALSE)
  }
  indices <- seq_len(nrow(spans))
  # Other rows cannot cover this cell; keep unusual row types on the original path.
  if (identical(class(spans), "data.frame") && is.numeric(spans$row) &&
      !is.object(spans$row) && is.null(dim(spans$row)) && !anyNA(spans$row) &&
      is.numeric(row_index) && !is.object(row_index) && is.null(dim(row_index)) && length(row_index) == 1L && !is.na(row_index)) {
    indices <- which(spans$row == row_index)
  }
  any(vapply(indices, function(index) {
    if (spans$row[[index]] != row_index || spans$start_column[[index]] == column) {
      return(FALSE)
    }
    start_index <- match(spans$start_column[[index]], columns)
    end_index <- match(spans$end_column[[index]], columns)
    is.finite(start_index) && is.finite(end_index) && column_index >= start_index && column_index <= end_index
  }, logical(1)))
}

result_cell_multiline_content <- function(value, marker = "", column = "") {
  parts <- strsplit(as.character(value %||% ""), "\n", fixed = TRUE)[[1]]
  if (length(parts) == 0L) {
    parts <- ""
  }
  line_tags <- lapply(seq_along(parts), function(index) {
    if (index == length(parts) && nzchar(marker)) {
      return(tags$span(
        parts[[index]],
        tags$sup(class = "coefficient-footnote-marker", marker)
      ))
    }
    tags$span(parts[[index]])
  })
  tags$span(
    class = "coefficient-cell-break",
    line_tags
  )
}

result_format_df <- function(value) {
  text <- as.character(value)
  number <- suppressWarnings(as.numeric(text))
  whole <- is.finite(number) & abs(number - round(number)) < 1e-8
  text[whole] <- format(round(number[whole]), scientific = FALSE, trim = TRUE)
  text
}

result_cell_content <- function(value, marker = "", column = "") {
  if (grepl("^(df|df1|df2|num[ ._]df|den[ ._]df)$", column, ignore.case = TRUE)) value <- result_format_df(value)
  split <- result_split_inline_marker(value, marker, column)
  value <- split[["value"]]
  marker <- split[["marker"]]
  suffix <- if (nzchar(marker)) tags$sup(class = "coefficient-footnote-marker", marker) else NULL
  if (grepl("^[[:space:]]*[-+.0-9]+[[:space:]]*±[[:space:]]*[-+.0-9]+$", value)) {
    return(tags$span(style = "white-space:nowrap!important;", value, suffix))
  }
  if (grepl("^[[:space:]]*[-+.0-9]+[[:space:]]*\\([-+.0-9]+[[:space:]]*[,~][[:space:]]*[-+.0-9]+\\)$", value)) {
    bracket <- regexpr("(", value, fixed = TRUE)[[1L]]
    return(tags$span(tags$span(style = "white-space:nowrap!important;", trimws(substr(value, 1L, bracket - 1L))),
      " ", tags$wbr(), tags$span(style = "white-space:nowrap!important;", substr(value, bracket, nchar(value))), suffix))
  }
  if (nzchar(value) && grepl("\n", value, fixed = TRUE)) {
    return(result_cell_multiline_content(value, marker, column))
  }
  if (!nzchar(value) || !nzchar(marker)) {
    return(value)
  }
  tags$span(
    class = "coefficient-footnote-value",
    value,
    tags$sup(class = "coefficient-footnote-marker", marker)
  )
}

result_cell_value_without_marker <- function(value, marker = "") {
  value <- as.character(value %||% "")
  marker <- as.character(marker %||% "")
  if (!nzchar(value) || !nzchar(marker)) {
    return(value)
  }
  sub(paste0(marker, "$"), "", value)
}

result_column_header_marker <- function(table, column) {
  markers <- attr(table, "column_header_markers", exact = TRUE)
  if (!is.data.frame(markers) || nrow(markers) == 0 || !"column" %in% names(markers) || !"marker" %in% names(markers)) {
    return("")
  }
  matched <- markers[as.character(markers$column) == column, , drop = FALSE]
  if (nrow(matched) == 0) "" else as.character(matched$marker[[1]])
}

coefficient_display_columns <- function(table) {
  columns <- names(table)
  labels <- columns
  labels[result_column_key(labels) == "term"] <- "Variable"
  labels[result_column_key(labels) == "beta"] <- "\u03B2"
  labels[result_column_key(labels) == "effectsize"] <- "ES"
  labels[result_column_key(labels) == "posthoc"] <- "post\n-hoc"
  labels[result_column_key(labels) == "bootse"] <- "Boot\nSE"
  labels[result_column_key(labels) == "hc3se"] <- "HC3\nSE"
  labels[result_column_key(labels) == "bootp"] <- "Boot\np"
  labels[result_column_key(labels) == "tolerance"] <- "Tol"
  custom_labels <- attr(table, "column_display_labels", exact = TRUE)
  if (!is.null(custom_labels) && length(custom_labels) > 0) {
    custom_labels <- unlist(custom_labels, use.names = TRUE)
    if (length(custom_labels) == length(columns) && is.null(names(custom_labels))) {
      labels <- as.character(custom_labels)
    } else if (!is.null(names(custom_labels))) {
      matched <- match(columns, names(custom_labels))
      labels[is.finite(matched)] <- as.character(custom_labels[matched[is.finite(matched)]])
    }
  }
  data.frame(
    source = columns,
    label = labels,
    header_marker = vapply(columns, function(column) result_column_header_marker(table, column), character(1)),
    marker = FALSE,
    stringsAsFactors = FALSE
  )
}

result_header_content <- function(label, marker = "") {
  label <- as.character(label %||% "")
  marker <- as.character(marker %||% "")
  content <- if (!grepl("\n", label, fixed = TRUE)) {
    label
  } else {
    parts <- strsplit(label, "\n", fixed = TRUE)[[1]]
    tags$span(
      class = "coefficient-header-break",
      lapply(parts, function(part) {
        tags$span(part)
      })
    )
  }
  if (!nzchar(marker)) {
    return(content)
  }
  tags$span(
    style = "white-space:nowrap;",
    content,
    tags$sup(style = "margin-left:2px;font-size:75%;vertical-align:super;", marker)
  )
}

coefficient_show_df_column_width <- function(table, column) {
  column_key <- result_column_key(column)
  if (isTRUE(attr(table, "bootstrap_regression", exact = TRUE))) {
    if (column_key == "term") return(330L)
    if (column_key == "b") return(56L)
    if (column_key %in% c("bootse", "hc3se")) return(78L)
    if (column_key %in% c("llci", "ulci")) return(68L)
    if (column_key == "bootp") return(72L)
    if (column_key %in% c("sr2", "f2")) return(48L)
    if (column_key == "tolerance") return(92L)
    if (column_key == "vif") return(60L)
  }
  if (!isTRUE(attr(table, "show_df", exact = TRUE))) {
    return(NA_integer_)
  }
  mean_sd <- isTRUE(attr(table, "mean_sd", exact = TRUE))
  trend_analysis <- isTRUE(attr(table, "trend_analysis", exact = TRUE))
  if (isTRUE(attr(table, "complex_sample_group_table", exact = TRUE))) {
    if (column_key == "variable") return(88L)
    if (column_key == "value") return(116L)
    if (column_key %in% c("msd", "mse")) return(134L)
    if (column_key == "95ci") return(124L)
    if (column_key == "effectsize") return(52L)
    if (column_key %in% c("tdf", "fdf", "tfdf", "statistic", "t", "f", "tf")) return(78L)
    if (column_key == "p") return(46L)
  }
  if (isTRUE(trend_analysis)) {
    if (column_key == "variable") {
      return(82L)
    }
    if (column_key == "value") {
      return(if (isTRUE(mean_sd)) 112L else 104L)
    }
    if (column_key %in% c("msd", "mse", "rankmse")) {
      return(116L)
    }
    if (column_key %in% c("p", "effectsize")) {
      return(50L)
    }
    if (column_key == "95ci") {
      return(92L)
    }
    if (column_key == "pfortrend") {
      return(if (isTRUE(mean_sd)) 82L else 86L)
    }
    if (column_key == "posthoc") {
      return(68L)
    }
  }
  if (column_key %in% c("m", "sd") && !isTRUE(mean_sd)) {
    return(if (isTRUE(trend_analysis)) 46L else 44L)
  }
  if (column_key == "95ci") {
    return(92L)
  }
  if (column_key %in% c("statistic", "t", "f", "tf", "tdf", "fdf", "tfdf", "fstatistic")) {
    if (isTRUE(trend_analysis)) {
      return(if (isTRUE(mean_sd)) 144L else 158L)
    }
    return(if (isTRUE(mean_sd)) 112L else 144L)
  }
  NA_integer_
}

coefficient_show_df_width_style <- function(table, column) {
  has_explicit_column_widths <- is.numeric(attr(table, "compact_column_widths", exact = TRUE))
  if ((isTRUE(attr(table, "bootstrap_regression", exact = TRUE)) ||
       isTRUE(attr(table, "complex_sample_group_table", exact = TRUE))) &&
      isTRUE(has_explicit_column_widths)) {
    return("")
  }
  width <- coefficient_show_df_column_width(table, column)
  if (!is.finite(width)) {
    return("")
  }
  paste0("width:", width, "px !important;min-width:", width, "px !important;max-width:", width, "px !important;")
}

coefficient_display_cell_style <- function(table, row_index, column, display_index, display_meta, compact, compact_font_size, compact_width, compact_first_width) {
  marker_column <- isTRUE(display_meta$marker[[display_index]])
  source_columns <- display_meta$source[!display_meta$marker]
  source_index <- match(column, source_columns)
  column_key <- result_column_key(column)
  style <- result_body_cell_style(
    display_index == 1,
    row_index == nrow(table),
    compact = compact,
    compact_font_size = compact_font_size,
    compact_width = compact_width,
    compact_first_width = compact_first_width
  )
  if (isTRUE(marker_column)) {
    return(paste0(
      style,
      "padding-left:2px;padding-right:8px;padding-top:0;min-width:16px;width:16px;text-align:left;vertical-align:top;line-height:1;"
    ))
  }
  next_is_marker <- display_index < nrow(display_meta) &&
    isTRUE(display_meta$marker[[display_index + 1L]]) &&
    identical(display_meta$source[[display_index + 1L]], column)
  if (isTRUE(next_is_marker)) {
    style <- paste0(style, "padding-right:2px;")
  }
  if (is.finite(source_index) && source_index == 1 && display_index != 1) {
    style <- paste0(style, "text-align:left;")
  }
  if (result_note_marker_column(column) || column_key %in% c("msd", "mse", "rankmse")) {
    style <- paste0(style, "white-space:nowrap;overflow-wrap:normal;word-break:normal;")
  }
  if (column_key == "95ci") {
    style <- paste0(style, "white-space:nowrap;overflow-wrap:normal;word-break:normal;")
  }
  if (isTRUE(attr(table, "complex_sample_group_table", exact = TRUE)) && column_key %in% c("es", "effectsize", "tdf", "fdf", "tfdf", "p")) {
    style <- paste0(style, "padding-left:6px !important;padding-right:6px !important;")
  }
  if (nzchar(result_cell_note_marker(table, row_index, column))) {
    style <- paste0(style, "white-space:nowrap;overflow-wrap:normal;word-break:normal;")
  }
  style <- paste0(style, coefficient_show_df_width_style(table, column))
  if (isTRUE(attr(table, "show_df", exact = TRUE)) && column_key %in% c("statistic", "t", "f", "tf", "tdf", "fdf", "tfdf", "fstatistic")) {
    style <- paste0(style, "padding-right:12px !important;")
  }
  if (isTRUE(attr(table, "show_df", exact = TRUE)) && column_key == "p") {
    style <- paste0(style, "padding-left:12px !important;")
  }
  if (isTRUE(attr(table, "trend_analysis", exact = TRUE)) && column_key == "pfortrend") {
    style <- paste0(style, "white-space:nowrap;overflow-wrap:normal;word-break:normal;")
  }
  if (column_key == "level") {
    style <- paste0(style, "text-align:center !important;white-space:nowrap;overflow-wrap:normal;word-break:normal;")
  }
  if (column_key %in% c("effect", "path")) {
    style <- paste0(style, "text-align:left !important;white-space:pre-line;overflow-wrap:anywhere;word-break:normal;")
  }
  right_align_columns <- as.character(attr(table, "right_align_columns", exact = TRUE) %||% character(0))
  if (column %in% right_align_columns) {
    style <- paste0(style, "text-align:right !important;white-space:nowrap;overflow-wrap:normal;word-break:normal;")
  }
  if (column_key %in% c("statistic", "t", "f", "tf", "tdf", "fdf", "tfdf", "fstatistic")) {
    style <- paste0(style, "text-align:right !important;white-space:nowrap;overflow-wrap:normal;word-break:normal;")
  }
  style
}

coefficient_column_class <- function(name) {
  normalized <- gsub("[^[:alnum:]]+", "", tolower(as.character(name %||% "")))
  switch(
    normalized,
    term = "coefficient-col-term",
    variable = "coefficient-col-term",
    value = "coefficient-col-value",
    effect = "coefficient-col-effect",
    path = "coefficient-col-path",
    level = "coefficient-col-level",
    label = "coefficient-col-reference",
    statistic = "coefficient-col-statistic",
    es = "coefficient-col-effect-size",
    tf = "coefficient-col-statistic",
    tdf = "coefficient-col-statistic",
    fdf = "coefficient-col-statistic",
    tfdf = "coefficient-col-statistic",
    `95ci` = "coefficient-col-ci",
    f = "coefficient-col-f",
    msd = "coefficient-col-mse",
    mse = "coefficient-col-mse",
    rankmse = "coefficient-col-mse",
    b = "coefficient-col-b",
    bootse = "coefficient-col-boot-se",
    hc3se = "coefficient-col-boot-se",
    llci = "coefficient-col-ci",
    ulci = "coefficient-col-ci",
    bootp = "coefficient-col-boot-p",
    reference = "coefficient-col-reference",
    t = "coefficient-col-compact",
    p = "coefficient-col-p",
    pfortrend = "coefficient-col-p-trend",
    effectsize = "coefficient-col-effect-size",
    posthoc = "coefficient-col-posthoc",
    sr2 = "coefficient-col-compact",
    f2 = "coefficient-col-compact",
    vif = "coefficient-col-vif",
    tolerance = "coefficient-col-tolerance",
    "coefficient-col-stat"
  )
}

result_column_key <- function(name) {
  name <- gsub("\u00B2", "2", as.character(name %||% ""), fixed = TRUE)
  gsub("[^[:alnum:]]+", "", tolower(name))
}

hierarchical_compact_stat_column <- function(name) {
  result_column_key(name) %in% c("llci", "ulci", "p", "bootp", "sr2", "f2", "tolerance", "vif")
}

hierarchical_stat_column_class <- function(name) {
  if (isTRUE(hierarchical_compact_stat_column(name))) {
    return("hierarchical-stat-col hierarchical-stat-col-narrow")
  }
  "hierarchical-stat-col"
}

hierarchical_stat_column_width <- function(name) {
  key <- result_column_key(name)
  if (key %in% c("bootp")) {
    return(54L)
  }
  if (key %in% c("llci", "ulci", "p", "sr2", "f2", "vif")) {
    return(48L)
  }
  70L
}

hierarchical_stat_cell_style <- function(column, last = FALSE, header = FALSE) {
  padding <- if (isTRUE(hierarchical_compact_stat_column(column))) "9px 4px" else "9px 7px"
  paste0(
    "padding:", padding, ";line-height:1.45;border-left:0;border-right:0;",
    "border-top:0;border-bottom:",
    if (isTRUE(header)) "2px solid #1f2937" else if (isTRUE(last)) "2px solid #1f2937 !important" else "1px solid #d7dde5",
    ";vertical-align:middle;background:transparent;",
    "width:auto;min-width:0;max-width:none;",
    "text-align:", if (isTRUE(header)) "center" else "right", " !important;",
    "white-space:", if (isTRUE(header)) "normal" else "nowrap", ";",
    "overflow-wrap:normal;"
  )
}

coefficient_html_table <- function(
  table,
  fit_line = NULL,
  stat_lines = character(0),
  warning_line = NULL,
  note_line = NULL,
  compact = FALSE,
  compact_font_size = 12,
  compact_width = 62,
  compact_first_width = 118,
  compact_min_width = 330,
  output_table_style = "standard",
  table_role = NULL,
  table_language = NULL,
  sheet_orientation = "auto",
  ci_note_deferred = FALSE
) {
  table <- result_ci_expand_columns(table, force = any(grepl("bootstrap|부트스트랩", note_line %||% "", ignore.case=TRUE)))
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(NULL)
  }
  output_table_style <- analysis_output_table_style(output_table_style)
  style_params <- analysis_output_table_style_params(output_table_style)
  if (!isTRUE(compact) && isTRUE(style_params$compact)) {
    compact <- TRUE
    compact_font_size <- style_params$font_size
    compact_width <- style_params$compact_width
    compact_first_width <- style_params$compact_first_width
    compact_min_width <- style_params$min_width
  }
  columns <- names(table)
  display_meta <- coefficient_display_columns(table)
  compact_column_widths <- attr(table, "compact_column_widths", exact = TRUE)
  compact_column_width_style <- function(index) {
    if (!is.numeric(compact_column_widths) || index > length(compact_column_widths)) {
      return("")
    }
    width <- compact_column_widths[[index]]
    if (!is.finite(width) || width <= 0) {
      return("")
    }
    sprintf("width:%.4f%% !important;min-width:0 !important;max-width:none !important;", width)
  }
  table_class <- paste(
    "coefficient-table",
    if (isTRUE(attr(table, "conditional_effect_table", exact = TRUE))) "conditional-effects-table" else "",
    if (isTRUE(attr(table, "regression_publication_style", exact = TRUE))) "regression-publication-table" else "",
    paste0("output-table-style-", output_table_style),
    if (isTRUE(attr(table, "show_df", exact = TRUE))) "coefficient-table-show-df" else "",
    if (isTRUE(attr(table, "mean_sd", exact = TRUE))) "coefficient-table-mean-sd" else "",
    if (isTRUE(attr(table, "trend_analysis", exact = TRUE))) "coefficient-table-trend-analysis" else "",
    if (isTRUE(attr(table, "complex_sample_group_table", exact = TRUE))) "coefficient-table-complex-sample-group" else "",
    if (isTRUE(attr(table, "bootstrap_regression", exact = TRUE))) "coefficient-table-bootstrap-regression" else ""
  )
  table_style <- result_table_style(
    font_size = if (isTRUE(compact)) compact_font_size else 12,
    min_width = if (isTRUE(compact)) compact_min_width else if (identical(output_table_style, "wide")) style_params$min_width else 480
  )
  if (is.numeric(compact_column_widths) && length(compact_column_widths) == nrow(display_meta)) {
    table_style <- paste0(table_style, "width:100% !important;min-width:0 !important;max-width:100% !important;table-layout:fixed;")
  }
  if (isTRUE(attr(table, "trend_analysis", exact = TRUE))) {
    table_style <- paste0(table_style, "width:100% !important;min-width:0 !important;max-width:100% !important;table-layout:fixed;")
  }
  table_tag <- tags$table(
      class = table_class,
      style = table_style,
      `data-result-column-widths` = if (is.numeric(compact_column_widths) && length(compact_column_widths) == nrow(display_meta)) jsonlite::toJSON(unname(compact_column_widths / sum(compact_column_widths)), auto_unbox = FALSE) else NULL,
      tags$colgroup(lapply(seq_len(nrow(display_meta)), function(index) {
        tags$col(
          class = if (isTRUE(display_meta$marker[[index]])) "coefficient-col-note-marker" else coefficient_column_class(display_meta$source[[index]]),
          style = paste0(
            compact_column_width_style(index),
            if (isTRUE(display_meta$marker[[index]])) "" else coefficient_show_df_width_style(table, display_meta$source[[index]])
          )
        )
      })),
      tags$thead(
        tags$tr(lapply(seq_len(nrow(display_meta)), function(index) {
          tags$th(
            class = if (isTRUE(display_meta$marker[[index]])) {
              "coefficient-note-marker-cell"
            } else {
              coefficient_column_class(display_meta$source[[index]])
            },
            style = paste0(
              result_header_cell_style(
                index == 1,
                compact = compact,
                compact_font_size = compact_font_size,
                compact_width = compact_width,
                compact_first_width = compact_first_width
              ),
              compact_column_width_style(index),
              if (!is.null(attr(table, "compact_cell_padding", exact = TRUE))) paste0("padding:", attr(table, "compact_cell_padding", exact = TRUE), " !important;"),
              if (isTRUE(display_meta$marker[[index]])) "" else coefficient_show_df_width_style(table, display_meta$source[[index]]),
              if (isTRUE(display_meta$marker[[index]])) "padding-left:2px;padding-right:8px;min-width:16px;width:16px;text-align:left;" else "",
              if (!isTRUE(display_meta$marker[[index]]) && index < nrow(display_meta) && isTRUE(display_meta$marker[[index + 1L]])) "padding-right:2px;" else ""
            ),
            result_header_content(display_meta$label[[index]], display_meta$header_marker[[index]] %||% "")
          )
        }))
      ),
      tags$tbody(
        lapply(seq_len(nrow(table)), function(row_index) {
          tags$tr(lapply(seq_len(nrow(display_meta)), function(column_index) {
            column <- display_meta$source[[column_index]]
            marker_column <- isTRUE(display_meta$marker[[column_index]])
            if (isTRUE(result_cell_covered_by_span(table, row_index, column, columns))) {
              return(NULL)
            }
            span <- result_cell_span_start(table, row_index, column)
            marker <- result_cell_note_marker(table, row_index, column)
            value <- if (!is.null(span) && "value" %in% names(span)) span$value[[1]] else table[[column]][[row_index]] %||% ""
            content <- if (isTRUE(marker_column)) {
              if (nzchar(marker)) tags$sup(class = "coefficient-note-cell-marker", marker) else ""
            } else {
              result_cell_content(value, marker, column)
            }
            if (!isTRUE(marker_column) && column %in% attr(table, "nowrap_columns", exact = TRUE)) {
              content <- tags$span(style = "white-space:nowrap!important;", content)
            }
            bold_style <- if (isTRUE(result_cell_bold(table, row_index, column)) && nzchar(as.character(table[[column]][[row_index]] %||% ""))) "font-weight:700;" else ""
            colspan <- if (!is.null(span)) {
              start_index <- match(span$start_column[[1]], columns)
              end_index <- match(span$end_column[[1]], columns)
              max(1L, end_index - start_index + 1L)
            } else {
              NULL
            }
            span_style <- if (!is.null(span) && "style" %in% names(span)) as.character(span$style[[1]] %||% "") else ""
            tags$td(
              class = if (isTRUE(marker_column)) "coefficient-note-marker-cell" else coefficient_column_class(column),
              colspan = colspan,
              style = paste0(
                coefficient_display_cell_style(
                  table,
                  row_index,
                  column,
                  column_index,
                  display_meta,
                  compact = compact,
                  compact_font_size = compact_font_size,
                  compact_width = compact_width,
                  compact_first_width = compact_first_width
                ),
                compact_column_width_style(column_index),
                if (!is.null(attr(table, "compact_cell_padding", exact = TRUE))) paste0("padding:", attr(table, "compact_cell_padding", exact = TRUE), " !important;"),
                bold_style,
                result_cell_style_extra(table, row_index, column),
                span_style
              ),
              content
            )
          }))
        })
      ),
      if (!is.null(fit_line) && nzchar(fit_line)) {
        tags$tfoot(
          tags$tr(
            class = "coefficient-fit-row",
            tags$td(
              colspan = nrow(display_meta),
              style = "padding:9px 18px;line-height:1.45;border-left:0;border-right:0;border-top:2px solid #1f2937;border-bottom:0;text-align:center;font-weight:500;",
              fit_line
            )
          ),
          lapply(as.character(stat_lines), function(line) {
            if (!nzchar(line)) return(NULL)
            tags$tr(
              class = "coefficient-fit-row coefficient-dw-row",
              tags$td(
                colspan = nrow(display_meta),
                style = "padding:9px 18px;line-height:1.45;border-left:0;border-right:0;border-top:1px solid #d7dde5;border-bottom:0;text-align:center;font-weight:500;",
                line
              )
            )
          })
        )
      } else if (length(stat_lines) > 0) {
        tags$tfoot(
          lapply(as.character(stat_lines), function(line) {
            if (!nzchar(line)) return(NULL)
            tags$tr(
              class = "coefficient-fit-row coefficient-dw-row",
              tags$td(
                colspan = nrow(display_meta),
                style = "padding:9px 18px;line-height:1.45;border-left:0;border-right:0;border-top:1px solid #d7dde5;border-bottom:0;text-align:center;font-weight:500;",
                line
              )
            )
          })
        )
      }
  )
  width_table <- table[, unique(display_meta$source[!display_meta$marker]), drop = FALSE]
  width_labels <- display_meta$label[match(names(width_table), display_meta$source)]
  valid_labels <- !is.na(width_labels) & nzchar(as.character(width_labels))
  names(width_table)[valid_labels] <- as.character(width_labels[valid_labels])
  requested_orientation <- attr(table, "result_table_orientation", exact = TRUE) %||%
    attr(table, "sheet_orientation", exact = TRUE) %||%
    sheet_orientation
  intrinsic_width <- result_table_intrinsic_width(
    width_table,
    first_width = if (isTRUE(compact)) compact_first_width else 118,
    default_width = if (isTRUE(compact)) compact_width else 62,
    min_width = if (isTRUE(compact)) compact_min_width else if (identical(output_table_style, "wide")) style_params$min_width else 480
  )
  # An explicit portrait layout must also constrain the shared export width;
  # otherwise saved HTML's intrinsic-width rule expands it back to landscape size.
  if (identical(requested_orientation, "portrait")) {
    intrinsic_width <- min(intrinsic_width, result_table_portrait_capacity())
  }
  contract <- result_table_contract(
    table,
    role = table_role,
    language = table_language,
    orientation = requested_orientation,
    intrinsic_width = intrinsic_width
  )
  table_tag <- result_table_apply_contract(table_tag, contract)
  attr(table_tag,"result_ci_note_deferred") <- ci_note_deferred
  result_table_with_notes(
    table_tag,
    result_note_tag(note_line),
    result_note_tag(warning_line, class = "coefficient-warning")
  )
}


model_overview_html_table <- function(table) {
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(NULL)
  }
  if (isTRUE(attr(table, "regression_overview_rows"))) {
    widths <- c(25, 8, 34, 33)
    table_tag <- tags$table(
      class = "table shiny-table combined-model-overview-table",
      style = "width:100%;table-layout:fixed;border-collapse:collapse;font-size:12px;border-top:2px solid #1f2937;border-bottom:2px solid #1f2937;",
      tags$colgroup(lapply(widths, function(width) tags$col(style = sprintf("width:%s%%", width)))),
      tags$thead(tags$tr(lapply(names(table), function(label) tags$th(style = "padding:6px;border-bottom:2px solid #1f2937;text-align:left;white-space:normal;", label)))),
      tags$tbody(lapply(seq_len(nrow(table)), function(index) tags$tr(lapply(table[index, , drop = FALSE], function(value) {
        tags$td(style = "padding:6px;vertical-align:top;text-align:left;white-space:pre-line;overflow-wrap:anywhere;border-bottom:1px solid #d7dde5;", as.character(value))
      }))))
    )
    contract <- result_table_contract(table, orientation = "portrait", intrinsic_width = 590)
    return(result_table_with_notes(result_table_apply_contract(table_tag, contract)))
  }
  left_columns <- if ("Item" %in% names(table)) {
    if (identical(names(table)[[1]], "Item")) 1L else 2L
  } else {
    1L
  }
  compact_overview <- "Item" %in% names(table)
  if (isTRUE(compact_overview)) {
    n_cols <- ncol(table)
    value_cols <- max(1L, n_cols - left_columns)
    left_width <- if (left_columns == 1L) 96L else 160L
    target_width <- if (value_cols >= 5L) 890L else 590L
    first_width_pct <- 96 / target_width * 100
    item_width_pct <- if (left_columns >= 2L) 64 / target_width * 100 else 0
    value_width_pct <- max(6, (100 - first_width_pct - item_width_pct) / value_cols)
    value_names <- names(table)[seq.int(left_columns + 1L, n_cols)]
    model_header_match <- regexec("^(.*)\\s+(Model\\s+[0-9]+)$", value_names)
    model_header_parts <- regmatches(value_names, model_header_match)
    use_model_header <- length(value_names) > 1L && all(vapply(model_header_parts, length, integer(1)) == 3L)
    header_tag <- if (isTRUE(use_model_header)) {
      group_names <- vapply(model_header_parts, `[[`, character(1), 2L)
      model_names <- vapply(model_header_parts, `[[`, character(1), 3L)
      grouped_headers <- list()
      for (left_index in seq_len(left_columns)) {
        grouped_headers <- c(grouped_headers, list(tags$th(
          rowspan = 2,
          style = paste(
            "padding:6px 8px;line-height:1.3;border-left:0;border-right:0;border-bottom:2px solid #1f2937;",
            "vertical-align:middle;font-weight:700;background:transparent;white-space:normal;overflow-wrap:anywhere;",
            "text-align:left;"
          ),
          names(table)[[left_index]]
        )))
      }
      for (group in unique(group_names)) {
        grouped_headers <- c(grouped_headers, list(tags$th(
          colspan = sum(group_names == group),
          style = paste(
            "padding:6px 8px;line-height:1.3;border-left:0;border-right:0;border-bottom:2px solid #1f2937;",
            "vertical-align:middle;font-weight:700;background:transparent;white-space:normal;overflow-wrap:anywhere;",
            "text-align:center;"
          ),
          group
        )))
      }
      tags$thead(
        do.call(tags$tr, grouped_headers),
        tags$tr(lapply(model_names, function(name) {
          tags$th(
            style = paste(
              "padding:6px 8px;line-height:1.3;border-left:0;border-right:0;border-bottom:2px solid #1f2937;",
              "vertical-align:middle;font-weight:700;background:transparent;white-space:normal;overflow-wrap:anywhere;",
              "text-align:center;"
            ),
            name
          )
        }))
      )
    } else {
      tags$thead(tags$tr(lapply(seq_along(names(table)), function(index) {
        tags$th(
          style = paste(
            "padding:6px 8px;line-height:1.3;border-left:0;border-right:0;border-bottom:2px solid #1f2937;",
            "vertical-align:middle;font-weight:700;background:transparent;white-space:normal;overflow-wrap:anywhere;",
            "text-align:", if (index <= left_columns) "left" else "center", ";"
          ),
          names(table)[[index]]
        )
      })))
    }
    table_tag <- tags$table(
      class = "table shiny-table combined-model-overview-table compact-model-overview-table",
      style = paste(
        "width:100%;max-width:100%;min-width:0;table-layout:fixed;",
        "border-collapse:collapse;border-spacing:0;border-top:2px solid #1f2937;border-bottom:2px solid #1f2937;",
        "color:#2f3a46;font-size:12px;background:transparent;"
      ),
      tags$colgroup(
        tags$col(style = sprintf("width:%.4f%%;", first_width_pct)),
        if (left_columns >= 2L) tags$col(style = sprintf("width:%.4f%%;", item_width_pct)),
        lapply(seq_len(value_cols), function(unused) {
          tags$col(style = sprintf("width:%.4f%%;", value_width_pct))
        })
      ),
      header_tag,
      tags$tbody(lapply(seq_len(nrow(table)), function(row_index) {
        values <- table[row_index, , drop = TRUE]
        tags$tr(lapply(seq_along(values), function(index) {
          tags$td(
            style = paste(
              "padding:6px 8px;line-height:1.35;border-left:0;border-right:0;",
              "border-bottom:", if (row_index == nrow(table)) "0" else "1px solid #d7dde5", ";",
              "vertical-align:top;background:transparent;white-space:pre-line;overflow-wrap:anywhere;word-break:normal;",
              "font-variant-numeric:tabular-nums lining-nums;font-feature-settings:'tnum' 1,'lnum' 1;",
              "text-align:", if (index <= left_columns) "left" else "center", ";"
            ),
            values[[index]]
          )
        }))
      }))
    )
    contract <- result_table_contract(table, intrinsic_width = target_width)
    return(result_table_with_notes(result_table_apply_contract(table_tag, contract)))
  }
  overview_header_style <- function(index) {
    style <- result_header_cell_style(index <= left_columns, compact = compact_overview, compact_font_size = 12, compact_width = 88, compact_first_width = 118)
    if (isTRUE(compact_overview)) {
      style <- paste0(style, "min-width:0;white-space:normal;overflow-wrap:anywhere;")
    }
    style
  }
  overview_body_style <- function(index, last) {
    style <- result_body_cell_style(index <= left_columns, last, compact = compact_overview, compact_font_size = 12, compact_width = 88, compact_first_width = 118)
    if (isTRUE(compact_overview)) {
      style <- paste0(style, "min-width:0;white-space:pre-line;overflow-wrap:anywhere;word-break:normal;")
    }
    style
  }
  table_tag <- tags$table(
    class = "table shiny-table combined-model-overview-table",
    style = if (isTRUE(compact_overview)) {
      paste0(result_table_style(font_size = 12, min_width = 360), "width:100%;min-width:0;max-width:100%;table-layout:fixed;")
    } else {
      result_table_style()
    },
    tags$thead(tags$tr(lapply(seq_along(names(table)), function(index) {
      tags$th(style = overview_header_style(index), names(table)[[index]])
    }))),
    tags$tbody(lapply(seq_len(nrow(table)), function(row_index) {
      values <- table[row_index, , drop = TRUE]
      tags$tr(lapply(seq_along(values), function(index) {
        tags$td(style = overview_body_style(index, row_index == nrow(table)), result_cell_content(values[[index]], column = names(table)[index]))
      }))
    }))
  )
  contract <- result_table_contract(table, intrinsic_width = result_table_intrinsic_width(table, min_width = 360))
  result_table_with_notes(result_table_apply_contract(table_tag, contract))
}
combined_dw_html_table <- function(table) {
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(NULL)
  }
  table_tag <- tags$table(
    class = "table shiny-table combined-dw-table",
    style = result_table_style(),
    tags$thead(tags$tr(lapply(seq_along(names(table)), function(index) {
      tags$th(style = result_header_cell_style(index == 1), names(table)[[index]])
    }))),
    tags$tbody(lapply(seq_len(nrow(table)), function(row_index) {
      values <- table[row_index, , drop = TRUE]
      tags$tr(lapply(seq_along(values), function(index) {
        tags$td(style = result_body_cell_style(index == 1, row_index == nrow(table)), values[[index]])
      }))
    }))
  )
  contract <- result_table_contract(table, intrinsic_width = result_table_intrinsic_width(table, min_width = 480))
  result_table_with_notes(result_table_apply_contract(table_tag, contract))
}
