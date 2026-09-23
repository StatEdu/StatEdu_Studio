# Mixed repeated-measures ANOVA result UI.

mixed_rm_main_table <- function(table) {
  if (is.data.frame(table)) {
    attr(table, "result_table_role") <- "main"
    attr(table, "result_table_language") <- result_main_table_language()
  }
  table
}

# Keep the estimation table intact; derive the same publication view for every export.
mixed_rm_publication_tables <- function(result) {
  source <- result$anova
  if (!is.data.frame(source) || !nrow(source)) return(list(main = source, diagnostics = NULL, note = ""))
  covariates <- unique(c(result$covariates, result$covariate_labels))
  is_covariate <- vapply(as.character(source$Effect), function(effect) {
    any(vapply(covariates, function(label) identical(effect, label) || startsWith(effect, paste0(label, " x ")), logical(1)))
  }, logical(1))
  row_order <- order(is_covariate, seq_len(nrow(source)))
  appendix_effects <- attr(source, "result_appendix_effects", exact = TRUE)
  old_markers <- attr(source, "note_markers", exact = TRUE)
  if (is.data.frame(old_markers)) old_markers$row <- match(old_markers$row, row_order)
  source <- source[row_order, , drop = FALSE]
  diagnostic_names <- intersect(c("Effect", "Mauchly W", "p_sphericity", "epsilon(GG)", "epsilon(HF)", "Correction"), names(source))
  diagnostics <- source[nzchar(as.character(source$Correction)), diagnostic_names, drop = FALSE]
  if (length(appendix_effects) == nrow(source)) {
    diagnostics$Effect <- appendix_effects[row_order][nzchar(as.character(source$Correction))]
    attr(diagnostics, "result_user_columns") <- "Effect"
  }
  main <- source[, setdiff(names(source), c(setdiff(diagnostic_names, "Effect"), "df1", "df2")), drop = FALSE]
  if (all(c("df1", "df2", "F") %in% names(source))) {
    main$F <- paste0(source$F, " (", result_format_df(source$df1), ", ", result_format_df(source$df2), ")")
    names(main)[names(main) == "F"] <- "F(df1,df2)"
  }
  notes <- character(); markers <- list()
  for (i in seq_len(nrow(source))) {
    method <- as.character(source$Correction[i])
    if (!length(method) || is.na(method) || !nzchar(method)) next
    detail <- method
    if (method %in% c("Greenhouse-Geisser", "Huynh-Feldt", "Sphericity assumed")) {
      p <- as.character(source$p_sphericity[i]); gg <- as.character(source[["epsilon(GG)"]][i])
      epsilon <- suppressWarnings(as.numeric(gg))
      relation <- if (is.finite(epsilon)) if (epsilon < .75) " < .75" else " >= .75" else ""
      epsilon_note <- if (nzchar(gg)) paste0(", GG epsilon = ", gg, relation) else ""
      if (method == "Greenhouse-Geisser" && is.finite(epsilon) && epsilon >= .75) {
        epsilon_note <- paste0(", GG epsilon < .75 (rounded display = ", gg, ")")
      }
      detail <- paste0(method, " (Mauchly W's p", if (startsWith(p, "<")) " " else " = ", p,
                       epsilon_note, ")")
    }
    if (identical(method, "Not required")) detail <- "Sphericity correction not required (two repeated measurements)"
    if (!detail %in% notes) notes <- c(notes, detail)
    markers[[length(markers) + 1L]] <- data.frame(row = i, column = "p", marker = letters[match(detail, notes)])
  }
  # Effect-size annotations are supplied by the existing table metadata.
  if (length(markers)) attr(main, "note_markers") <- rbind(old_markers, do.call(rbind, markers))
  list(main = main, diagnostics = diagnostics,
       note = paste(paste0(letters[seq_along(notes)], ". ", notes), collapse = "; "))
}

mixed_rm_appendix_korean_text <- function(text) {
  if (length(text) == 0L) return("")
  text <- as.character(text)[[1]]
  if (is.na(text)) return(NA_character_)
  text <- enc2utf8(text)
  if (!nzchar(text)) return(text)

  exact <- c(
    "Consider ordinal mixed model as the main or sensitivity analysis." = "주 분석 또는 민감도 분석으로 순서형 혼합모형을 검토합니다.",
    "Consider count GLMM as the main or sensitivity analysis." = "주 분석 또는 민감도 분석으로 계수형 GLMM을 검토합니다.",
    "Consider Gamma GLMM as the main or sensitivity analysis." = "주 분석 또는 민감도 분석으로 Gamma GLMM을 검토합니다.",
    "Use the fitted Gamma GLMM as the ITT mixed-model result." = "적합된 Gamma GLMM을 ITT 혼합모형 결과로 사용합니다.",
    "Use the fitted count GLMM as the ITT mixed-model result." = "적합된 계수형 GLMM을 ITT 혼합모형 결과로 사용합니다.",
    "Use the fitted count GLMM as the ITT result." = "적합된 계수형 GLMM을 ITT 결과로 사용합니다.",
    "ordinal mixed model" = "순서형 혼합모형",
    "Use an ordinal mixed model path for ITT; automatic fitting was not available." = "ITT에는 순서형 혼합모형을 사용합니다. 자동 적합은 사용할 수 없습니다.",
    "Use the fitted LMM as the ITT mixed-model result." = "적합된 LMM을 ITT 혼합모형 결과로 사용합니다.",
    "ITT keeps available repeated records through a mixed-model path; complete-case RM ANOVA remains a PP reference." = "ITT는 혼합모형을 통해 이용 가능한 반복측정 기록을 유지하며, 완전 사례 반복측정 분산분석은 PP 참고 결과로 남습니다.",
    "At least three complete cases are required for mixed repeated-measures ANOVA." = "혼합 반복측정 분산분석에는 완전 사례가 최소 3개 필요합니다.",
    "Total cases" = "전체 사례",
    "Excluded cases" = "제외 사례",
    "Complete cases" = "완전 사례",
    "Groups" = "집단",
    "Time points" = "시점 수",
    "Sphericity" = "구형성",
    "GG epsilon" = "GG 엡실론",
    "Covariates" = "공변량",
    "Levene homogeneity" = "Levene 등분산성",
    "Analysis population" = "분석 대상",
    "Primary effect" = "핵심 효과",
    "Repeated-measures variables" = "반복측정 변수",
    "Independent variables" = "독립변수",
    "Time labels" = "시점 라벨",
    "Complete N" = "완전 사례 N",
    "Available subjects" = "이용 가능 대상자",
    "Available repeated records" = "이용 가능 반복측정 기록",
    "Complete-case RM ANOVA" = "완전 사례 반복측정 ANOVA",
    "Recommended model" = "권장 모형",
    "Recommended alternative" = "권장 대안",
    "PP RM ANOVA status" = "PP 반복측정 ANOVA 상태",
    "Mixed-model decision" = "혼합모형 결정",
    "Follow-up decision" = "후속 분석 결정",
    "Assumption decision" = "가정 검토 결정",
    "Data condition" = "자료 처리",
    "Normality decision" = "정규성 결정",
    "Family" = "분포군",
    "Reference levels" = "기준 범주",
    "Analyzed rows" = "분석 행",
    "Subjects" = "대상자",
    "Formula" = "모형식",
    "Covariate x Time" = "공변량 × 시점",
    "Satisfied" = "충족",
    "Not satisfied" = "불충족",
    "Not required" = "불필요",
    "Not testable" = "검정 불가",
    "Potential violation" = "위반 가능성",
    "Not available" = "사용 불가",
    "Not fitted" = "적합하지 않음",
    "None" = "없음",
    "Mixed repeated-measures ANOVA" = "혼합 반복측정 분산분석",
    "Factorial mixed repeated-measures ANOVA" = "요인 혼합 반복측정 분산분석",
    "Covariate-adjusted mixed repeated-measures ANOVA" = "공변량 보정 혼합 반복측정 분산분석",
    "Covariate-adjusted factorial mixed repeated-measures ANOVA" = "공변량 보정 요인 혼합 반복측정 분산분석",
    "PP / complete-case repeated-measures ANOVA" = "PP / 완전 사례 반복측정 분산분석",
    "ITT / available repeated-measures mixed model" = "ITT / 이용 가능 반복측정 혼합모형",
    "Available-record repeated-measures mixed model" = "이용 가능 기록 반복측정 혼합모형",
    "Rows with missing values in selected variables are excluded listwise." = "선택한 변수에 결측값이 있는 행은 목록별로 제외합니다.",
    "Used for corrected within-subject p values." = "개체 내 효과의 보정 p값 계산에 사용합니다.",
    "Only two repeated measurements were selected." = "반복측정 시점이 두 개이므로 구형성 검정이 필요하지 않습니다.",
    "Subjects with complete group/covariate values and at least one observed repeated outcome." = "집단·공변량 값이 완전하고 반복 결과가 하나 이상 관측된 대상자입니다.",
    "Long-format observed outcome rows used by the ITT mixed-model path." = "ITT 혼합모형 분석에 사용한 장형 관측 결과 행입니다.",
    "Complete-case RM ANOVA was not produced." = "완전 사례 반복측정 분산분석 결과를 생성하지 못했습니다.",
    "Complete-case RM ANOVA could not be computed for the selected variables." = "선택한 변수로 완전 사례 반복측정 분산분석을 계산할 수 없습니다.",
    "ITT keeps available repeated records through a mixed-model path." = "ITT는 혼합모형을 통해 이용 가능한 반복측정 기록을 유지합니다.",
    "More than one independent variable was selected." = "독립변수가 둘 이상 선택되었습니다.",
    "One independent variable was selected." = "독립변수가 하나 선택되었습니다.",
    "A long-format subject-random-intercept model was fitted from the RM selections." = "선택한 반복측정 변수로 장형 대상자 임의절편 모형을 적합했습니다.",
    "Use a covariate-adjusted available-record mixed model." = "공변량을 보정한 이용 가능 기록 혼합모형을 사용합니다.",
    "Use an available-record mixed model." = "이용 가능 기록 혼합모형을 사용합니다.",
    "Use a covariate-adjusted factorial repeated-measures model." = "공변량 보정 요인 반복측정 모형을 사용합니다.",
    "Use a covariate-adjusted mixed repeated-measures model." = "공변량 보정 혼합 반복측정 모형을 사용합니다.",
    "Use factorial mixed repeated-measures ANOVA." = "요인 혼합 반복측정 분산분석을 사용합니다.",
    "Use mixed repeated-measures ANOVA." = "혼합 반복측정 분산분석을 사용합니다.",
    "Prioritize follow-up comparisons for this interaction." = "이 상호작용에 대한 후속 비교를 우선합니다.",
    "Do not treat the group-specific change pattern as supported; review Time and between-subject effects as secondary." = "집단별 변화 양상이 지지된 것으로 해석하지 말고 시점 및 개체 간 효과를 이차적으로 검토합니다.",
    "Review the returned ANOVA table and assumptions before interpretation." = "해석 전에 분산분석표와 가정을 검토합니다.",
    "Assumption review was not requested." = "가정 검토를 요청하지 않았습니다.",
    "Assumption review was not available." = "가정 검토 결과를 사용할 수 없습니다.",
    "Sphericity correction is not required." = "구형성 보정이 필요하지 않습니다.",
    "Use the correction decision shown in the ANOVA table." = "분산분석표에 제시된 보정 결정을 따릅니다.",
    "Between-group homogeneity was flagged; interpret between-subject effects with caution." = "집단 간 등분산성 문제가 표시되었으므로 개체 간 효과를 주의해서 해석합니다.",
    "Listwise deletion was applied." = "목록별 제외를 적용했습니다.",
    "No selected rows were excluded." = "선택한 행 중 제외된 행이 없습니다.",
    "Mixed-model alternative is not required by the current checks." = "현재 점검 결과에서는 혼합모형 대안이 필요하지 않습니다.",
    "At least one Shapiro-Wilk p < .05; treat this as a sensitivity-analysis cue." = "하나 이상의 Shapiro-Wilk p값이 .05 미만이므로 민감도 분석 필요성을 검토합니다.",
    "Cell-level Shapiro-Wilk checks did not flag p < .05." = "셀 수준 Shapiro-Wilk 검정에서 p < .05가 나타나지 않았습니다.",
    "No matching time interaction was returned by the model." = "모형에서 일치하는 시점 상호작용이 산출되지 않았습니다.",
    "No covariate-by-time effect was flagged in the fitted RM ANCOVA table." = "적합된 반복측정 공분산분석표에서 공변량×시점 효과가 표시되지 않았습니다.",
    "Covariate effects vary over time; interpret adjusted means and Time effects with caution." = "공변량 효과가 시점에 따라 달라지므로 보정 평균과 시점 효과를 주의해서 해석합니다."
  )
  if (text %in% names(exact)) return(unname(exact[[text]]))

  replacements <- c(
    "Sphericity assumed" = "구형성 가정",
    "Normality was flagged in at least one RM cell." = "하나 이상의 반복측정 셀에서 정규성 문제가 표시되었습니다.",
    "Covariate-by-time terms were included because covariates were selected." = "공변량을 선택했으므로 공변량×시점 항을 포함했습니다.",
    "Use the correction decision shown in the ANOVA table." = "분산분석표에 제시된 보정 결정을 따릅니다.",
    "Sphericity correction is not required." = "구형성 보정이 필요하지 않습니다.",
    "Between-group homogeneity was flagged; interpret between-subject effects with caution." = "집단 간 등분산성 문제가 표시되었으므로 개체 간 효과를 주의해서 해석합니다.",
    "At least one Shapiro-Wilk p < .05; treat this as a sensitivity-analysis cue." = "하나 이상의 Shapiro-Wilk p값이 .05 미만이므로 민감도 분석 필요성을 검토합니다.",
    "Cell-level Shapiro-Wilk checks did not flag p < .05." = "셀 수준 Shapiro-Wilk 검정에서 p < .05가 나타나지 않았습니다.",
    "Normality flagged; review LMM/robust sensitivity if distributional mismatch is meaningful." = "정규성 문제가 표시되었습니다. 분포 불일치가 실질적이면 LMM 또는 강건 민감도 분석을 검토합니다.",
    "Consider LMM with robust/bootstrap inference as a sensitivity analysis." = "민감도 분석으로 강건·부트스트랩 추론을 적용한 LMM을 검토합니다.",
    "The repeated outcome is treated as continuous Gaussian; mixed modeling handles unbalanced repeated records." = "반복 결과를 연속형 가우시안 변수로 처리하며, 혼합모형은 불균형 반복측정 기록을 다룹니다.",
    "At least one repeated-measures variable is ordinal." = "하나 이상의 반복측정 변수가 순서형입니다.",
    "The stacked repeated outcome is non-negative integer-like count data." = "누적한 반복 결과가 음이 아닌 정수형 계수 자료입니다.",
    "The stacked repeated outcome is positive and strongly right-skewed." = "누적한 반복 결과가 양수이고 오른쪽으로 강하게 치우쳐 있습니다.",
    "More than one independent variable was selected, so between-subject main effects and their interactions are modeled." = "독립변수가 둘 이상이므로 개체 간 주효과와 상호작용을 함께 모형화합니다.",
    "PP uses subjects with complete selected repeated-measures and model variables." = "PP는 선택한 반복측정 변수와 모형 변수가 모두 완전한 대상자를 사용합니다.",
    "Primary p was not available." = "핵심 효과의 p값을 사용할 수 없습니다.",
    "Not satisfied" = "불충족",
    "Satisfied" = "충족",
    "raw values by group." = "집단별 원자료 값.",
    "adjusted residuals (group + covariates)." = "보정 잔차(집단 + 공변량).",
    "not testable" = "검정 불가",
    "Sphericity: " = "구형성: ",
    "p column: " = "p 열: ",
    "Levene: potential violation" = "Levene: 위반 가능성",
    "Levene: satisfied" = "Levene: 충족",
    "Levene: not testable" = "Levene: 검정 불가",
    "Primary p=" = "핵심 효과 p=",
    "Excluded cases: " = "제외 사례: "
  )
  for (source in names(replacements)) {
    text <- gsub(source, replacements[[source]], text, fixed = TRUE)
  }
  text <- sub(
    "^Use the fitted (.+) as the ITT result\\.$",
    "적합된 \\1을 ITT 결과로 사용합니다.",
    text,
    perl = TRUE
  )
  text <- sub(
    "^Use an? (.+) path for ITT; automatic fitting was not available\\.$",
    "ITT에는 \\1 분석 경로를 사용합니다. 자동 적합은 사용할 수 없습니다.",
    text,
    perl = TRUE
  )
  text
}

mixed_rm_appendix_table <- function(table) {
  if (!is.data.frame(table)) return(table)
  source_table <- table
  language <- result_appendix_table_language()
  if (identical(language, "ko")) {
    text_columns <- intersect(
      names(table),
      c("Item", "Value", "Result", "Detail", "Recommendation", "Reason", "Status", "Analysis", "Assessment", "Decision")
    )
    for (column in text_columns) {
      if (is.character(table[[column]]) || is.factor(table[[column]])) {
        table[[column]] <- vapply(as.character(table[[column]]), mixed_rm_appendix_korean_text, character(1))
      }
    }
  } else if (!identical(language, "en")) {
    fragments <- c(
      "Levene: satisfied", "Sphericity assumed",
      "Normality was flagged in at least one RM cell.",
      "The repeated outcome is treated as continuous Gaussian; mixed modeling handles unbalanced repeated records.",
      "Covariate-by-time terms were included because covariates were selected.",
      "At least one repeated-measures variable is ordinal.",
      "The stacked repeated outcome is non-negative integer-like count data.",
      "The stacked repeated outcome is positive and strongly right-skewed.",
      "At least one Shapiro-Wilk p < .05; treat this as a sensitivity-analysis cue.",
      "Use the correction decision shown in the ANOVA table.",
      "Sphericity correction is not required.",
      "Between-group homogeneity was flagged; interpret between-subject effects with caution.",
      "PP uses subjects with complete selected repeated-measures and model variables.",
      "adjusted residuals (group + covariates).", "raw values by group.",
      "Not satisfied", "Satisfied", "not testable", "potential violation",
      "Sphericity", "p column", "Primary p", "Excluded cases")
    for (column in intersect(names(table), c("Item", "Value", "Result", "Detail", "Recommendation", "Reason", "Status", "Analysis", "Assessment", "Decision"))) {
      if (!is.character(table[[column]]) && !is.factor(table[[column]])) next
      table[[column]] <- vapply(as.character(table[[column]]), function(value) {
        exact <- result_appendix_ui_text(value, language)
        if (!identical(exact, value)) return(exact)
        for (phrase in fragments) value <- gsub(phrase, result_appendix_ui_text(phrase, language), value, fixed = TRUE)
        value
      }, character(1))
    }
  }
  result_appendix_preserve_data(result_appendix_localize_table(table, language = language), source_table)
}

mixed_rm_main_note <- function(text, category = "estimation") {
  values <- list()
  values[[category]] <- text
  do.call(result_sci_note_text, values)
}

mixed_rm_appendix_normality_method <- function(method, language = result_appendix_table_language()) {
  method <- trimws(as.character(method %||% "Shapiro-Wilk")[[1]])
  if (!nzchar(method)) return(method)
  if (!identical(language, "ko")) return(result_appendix_ui_text(method, language))

  methods <- c(
    "Shapiro-Wilk" = "Shapiro-Wilk 검정",
    "Shapiro-Wilk by group" = "집단별 Shapiro-Wilk 검정",
    "Lilliefors (K-S)" = "Lilliefors(K-S) 검정",
    "Kolmogorov-Smirnov" = "Kolmogorov-Smirnov 검정",
    "Kolmogorov-Smirnov (Lilliefors)" = "Kolmogorov-Smirnov(Lilliefors) 검정",
    "Skewness and kurtosis" = "왜도·첨도 검토",
    "Skewness/kurtosis" = "왜도·첨도 검토"
  )
  if (method %in% names(methods)) unname(methods[[method]]) else method
}

mixed_rm_appendix_normality_note <- function(normality) {
  language <- result_appendix_table_language()
  method <- attr(normality, "normality_method", exact = TRUE) %||% "Shapiro-Wilk"
  label <- if (identical(language, "ko")) "정규성 검정 방법" else result_appendix_ui_text("Normality method", language)
  paste0(label, ": ", mixed_rm_appendix_normality_method(method, language), ".")
}

mixed_rm_anova_results_ui <- function(result) {
  if (is.null(result)) return(NULL)
  if (is.list(result) && !is.null(result$error)) return(empty_message(result$error))
  publication <- mixed_rm_publication_tables(result)
  summary_orientation <- if (length(result$repeated_variables) <= 3L) "portrait" else "auto"
  tags$div(
    class = "regression-results mixed-rm-anova-results",
    if (is.data.frame(result$overview) && nrow(result$overview) > 0) {
      tags$div(
        class = "result-section regression-result-panel",
        tags$h3(result_appendix_ui_text("Model overview")),
        model_overview_html_table(mixed_rm_appendix_table(result$overview))
      )
    },
    if (is.data.frame(result$recommendation) && nrow(result$recommendation) > 0) {
      tags$div(
        class = "result-section regression-result-panel",
        tags$h3(result_appendix_ui_text("Recommended interpretation")),
        coefficient_html_table(mixed_rm_appendix_table(result$recommendation), table_role = "appendix")
      )
    },
    if (is.data.frame(result$observed_descriptives) && nrow(result$observed_descriptives) > 0 && is.data.frame(result$adjusted_descriptives) && nrow(result$adjusted_descriptives) > 0) {
      tags$div(
        class = "result-section regression-result-panel",
        tags$h3("Observed mean summary"),
        coefficient_html_table(
          mixed_rm_main_table(result$observed_descriptives),
          sheet_orientation = summary_orientation,
          note_line = mixed_rm_main_note(result$observed_descriptives_note %||% "", "format"),
          table_role = "main"
        )
      )
    },
    if (is.data.frame(result$adjusted_descriptives) && nrow(result$adjusted_descriptives) > 0) {
      tags$div(
        class = "result-section regression-result-panel",
        tags$h3("Adjusted mean summary"),
        coefficient_html_table(
          mixed_rm_main_table(result$adjusted_descriptives),
          sheet_orientation = summary_orientation,
          note_line = mixed_rm_main_note(result$adjusted_descriptives_note %||% "", "format"),
          table_role = "main"
        )
      )
    },
    if ((!is.data.frame(result$adjusted_descriptives) || nrow(result$adjusted_descriptives) == 0) && is.data.frame(result$descriptives) && nrow(result$descriptives) > 0) {
      tags$div(
        class = "result-section regression-result-panel",
        tags$h3("Group x time summary"),
        coefficient_html_table(
          mixed_rm_main_table(result$descriptives),
          sheet_orientation = summary_orientation,
          note_line = mixed_rm_main_note(result$descriptives_note %||% "", "format"),
          table_role = "main"
        )
      )
    },
    if (is.data.frame(result$anova) && nrow(result$anova) > 0) {
      tags$div(
        class = "result-section regression-result-panel",
        tags$h3("Repeated-measures ANOVA"),
        coefficient_html_table(
          mixed_rm_main_table(publication$main),
          sheet_orientation = "portrait",
          note_line = mixed_rm_main_note(paste("df1 = numerator degrees of freedom; df2 = denominator degrees of freedom;", result$method_note %||% "", publication$note), "estimation"),
          table_role = "main"
        )
      )
    },
    if (is.data.frame(publication$diagnostics) && nrow(publication$diagnostics) > 0) {
      tags$div(class = "result-section regression-result-panel",
        tags$h3(result_appendix_ui_text("Sphericity tests and corrections")),
        coefficient_html_table(mixed_rm_appendix_table(publication$diagnostics), table_role = "appendix", sheet_orientation = "portrait"))
    },
    if (is.data.frame(result$mixed_model_overview) && nrow(result$mixed_model_overview) > 0) {
      tags$div(
        class = "result-section regression-result-panel",
        tags$h3(result_appendix_ui_text("Mixed-model alternative")),
        model_overview_html_table(mixed_rm_appendix_table(result$mixed_model_overview))
      )
    },
    if (is.data.frame(result$mixed_model_coefficients) && nrow(result$mixed_model_coefficients) > 0) {
      tags$div(
        class = "result-section regression-result-panel",
        tags$h3("Mixed-model coefficients"),
        coefficient_html_table(
          mixed_rm_main_table(result$mixed_model_coefficients),
          note_line = mixed_rm_main_note(result$mixed_model_note %||% "", "estimation"),
          table_role = "main"
        )
      )
    },
    if (is.data.frame(result$posthoc) && nrow(result$posthoc) > 0) {
      tags$div(
        class = "result-section regression-result-panel",
        tags$h3("Post-hoc comparisons"),
        coefficient_html_table(
          mixed_rm_main_table(result$posthoc),
          note_line = mixed_rm_main_note(result$posthoc_note %||% "", "multiplicity"),
          table_role = "main"
        )
      )
    },
    if (is.data.frame(result$assumption) && nrow(result$assumption) > 0) {
      tags$div(
        class = "result-section regression-result-panel",
        tags$h3(result_appendix_ui_text("Assumption review")),
        model_overview_html_table(mixed_rm_appendix_table(result$assumption))
      )
    },
    if (is.data.frame(result$normality) && nrow(result$normality) > 0) {
      tags$div(
        class = "result-section regression-result-panel",
        tags$h3(`data-title-en` = "Normality review", result_appendix_ui_text("Normality review")),
        coefficient_html_table(
          mixed_rm_appendix_table(result$normality),
          note_line = mixed_rm_appendix_normality_note(result$normality),
          table_role = "appendix"
        )
      )
    }
  )
}

saved_mixed_rm_anova_results_html <- function(result, css_path = file.path("www", "style.css"), report_mode = FALSE) {
  if (isTRUE(report_mode)) return(saved_result_sheet_document(
    "StatEdu Studio Repeated-Measures ANOVA",
    tags$div(class = "regression-results", mixed_rm_anova_results_ui(result)), css_path))
  html <- tags$html(
    tags$head(
      tags$meta(charset = "UTF-8"),
      tags$title("StatEdu Studio Repeated-Measures ANOVA"),
      tags$link(rel = "stylesheet", type = "text/css", href = css_path)
    ),
    tags$body(
      class = if (isTRUE(report_mode)) "print-report" else NULL,
      tags$div(class = "regression-results", mixed_rm_anova_results_ui(result))
    )
  )
  paste("<!DOCTYPE html>", tags_to_html(html), sep = "\n")
}

write_mixed_rm_anova_results_html <- function(result, file) {
  write_result_html_document(saved_mixed_rm_anova_results_html(result), file, useBytes = TRUE)
  invisible(file)
}

write_mixed_rm_anova_results_pdf <- function(result, file) {
  write_pdf_from_html(saved_mixed_rm_anova_results_html(result, report_mode = TRUE), file)
  invisible(file)
}

save_mixed_rm_anova_excel_file <- function (result, file)
{
    save_screen_excel_file(saved_mixed_rm_anova_results_html(result = result), file)
}
