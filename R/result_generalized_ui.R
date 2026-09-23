# Generalized linear model result UI.

generalized_main_table <- function(table) {
  if (is.data.frame(table)) {
    attr(table, "result_table_role") <- "main"
    attr(table, "result_table_language") <- result_main_table_language()
  }
  table
}

generalized_appendix_text <- function(text, language = NULL) {
  language <- result_appendix_table_language(language)
  text <- as.character(text %||% "")
  if (identical(language, "en") || is.na(text) || !nzchar(text)) return(text)
  if (!identical(language, "ko")) return(generalized_appendix_value_text(text, language))
  korean <- c(
    "Model overview" = "\ubaa8\ud615 \uac1c\uc694",
    "Multiple-imputation pooling diagnostics" = "\ub2e4\uc911\ub300\uce58 \ud480\ub9c1 \uc9c4\ub2e8",
    "Imputation-specific model checks" = "\ub300\uce58\ubcc4 \ubaa8\ud615 \uc810\uac80",
    "Count-family / overdispersion screening" = "\uce74\uc6b4\ud2b8 \ubd84\ud3ec / \uacfc\uc0b0\ud3ec \uc120\ubcc4",
    "Assumption checks" = "\uac00\uc815 \uac80\ud1a0",
    "SCI reporting checklist" = "SCI \ubcf4\uace0 \uc810\uac80\ud45c",
    "Variable coding" = "\ubcc0\uc218 \ucf54\ub529",
    "Collinearity diagnostics" = "\ub2e4\uc911\uacf5\uc120\uc131 \uc9c4\ub2e8",
    "Missing-data handling" = "\uacb0\uce21\uc790\ub8cc \ucc98\ub9ac",
    "Missing-data details" = "\uacb0\uce21\uc790\ub8cc \ucc98\ub9ac \uc138\ubd80\ub0b4\uc6a9",
    "Missing-data pattern" = "\uacb0\uce21\uc790\ub8cc \ud328\ud134",
    "Missing values" = "\uacb0\uce21\uac12",
    "Suggested manuscript text" = "\uc6d0\uace0 \ubb38\uc7a5 \uc81c\uc548",
    "Software versions" = "\uc18c\ud504\ud2b8\uc6e8\uc5b4 \ubc84\uc804",
    "Model diagnostics" = "\ubaa8\ud615 \uc9c4\ub2e8",
    "Notes" = "\ucc38\uace0\uc0ac\ud56d",
    "RIV = relative increase in variance; FMI = fraction of missing information. Degrees of freedom use the Barnard-Rubin adjustment." =
      "RIV\ub294 \ubd84\uc0b0\uc758 \uc0c1\ub300\uc801 \uc99d\uac00\ub7c9, FMI\ub294 \uacb0\uce21\uc815\ubcf4\ube44\uc728\uc774\uba70 \uc790\uc720\ub3c4\ub294 Barnard-Rubin \ubcf4\uc815\uc744 \uc801\uc6a9\ud569\ub2c8\ub2e4.",
    "All imputed datasets must use the same family, link, term signature, residual df, and standard-error method; otherwise pooling is stopped." =
      "\ubaa8\ub4e0 \ub300\uce58 \uc790\ub8cc\ub294 \ub3d9\uc77c\ud55c \ubd84\ud3ec, \ub9c1\ud06c, \ud56d \uad6c\uc131, \uc794\ucc28 \uc790\uc720\ub3c4, \ud45c\uc900\uc624\ucc28 \ubc29\ubc95\uc744 \uc0ac\uc6a9\ud574\uc57c \ud558\uba70 \ubd88\uc77c\uce58\ud558\uba74 \ud480\ub9c1\uc744 \uc911\ub2e8\ud569\ub2c8\ub2e4."
  )
  if (text %in% names(korean)) unname(korean[[text]]) else generalized_appendix_value_text(result_appendix_ui_text(text, language), language)
}

generalized_appendix_value_text <- function(text, language = NULL) {
  language <- result_appendix_table_language(language)
  text <- as.character(text %||% "")
  if (identical(language, "en") || is.na(text) || !nzchar(text)) return(text)
  if (!identical(language, "ko")) {
    localize_model_term <- function(value) {
      terms <- c(gaussian = "Gaussian distribution", binomial = "Binomial distribution",
        gamma = "Gamma distribution", poisson = "Poisson distribution",
        negative_binomial = "Negative binomial distribution", `negative binomial` = "Negative binomial distribution",
        identity = "Identity link", logit = "Logit link", log = "Log link", inverse = "Inverse link")
      key <- tolower(trimws(value))
      if (key %in% names(terms)) statedu_localized_text(language, unname(terms[[key]])) else value
    }
    formats <- c(
      "^(.+); analyzed ([0-9]+) of ([0-9]+) (?:rows|row\\(s\\))\\.$" = "%s; analyzed %s of %s rows.",
      "^(.+); analyzed ([0-9]+) of ([0-9]+) rows; complete-case rows before missing-data engine: ([0-9]+)\\.$" = "%s; analyzed %s of %s rows; complete-case rows before missing-data engine: %s.",
      "^(Linear Gaussian / identity|Binary logistic / logit|Gamma / log|Count: Poisson or negative binomial / log|Negative binomial / log|Poisson / log) with (identity|logit|log|inverse) link\\.$" = "%s with %s link.",
      "^User selected (.+); fitted as (.+)\\.$" = "User selected %s; fitted as %s.",
      "^Coefficient standard errors use (.+) sandwich robust covariance\\.$" = "Coefficient standard errors use %s sandwich robust covariance.",
      "^Robust standard errors \\((.+)\\) were requested, but robust covariance could not be computed; model-based covariance is shown\\.$" = "Robust standard errors (%s) were requested, but robust covariance could not be computed; model-based covariance is shown.",
      "^Exposure offset applied as log\\((.+)\\)\\.$" = "Exposure offset applied as log(%s).",
      "^([0-9]+) coding row\\(s\\) generated\\.$" = "%s coding row(s) generated.",
      "^Complete-case GLM excluded ([0-9]+) of ([0-9]+) rows with missing values in selected analysis variables\\.$" = "Complete-case GLM excluded %s of %s rows with missing values in selected analysis variables.",
      "^Complete-case rows before missing-data engine: ([0-9]+) of ([0-9]+)\\.$" = "Complete-case rows before missing-data engine: %s of %s.",
      "^Observation model: (.+); IPW clipped at the 99th percentile and normalized to mean 1\\. Report the observation model and review positivity/weight stability\\.$" = "Observation model: %s; IPW clipped at the 99th percentile and normalized to mean 1. Report the observation model and review positivity/weight stability.",
      "^Observation model failed \\((.+)\\); intercept-only IPW was used\\. Treat this as a weak IPW sensitivity analysis\\.$" = "Observation model failed (%s); intercept-only IPW was used. Treat this as a weak IPW sensitivity analysis.",
      "^Assumption screening flagged (.+); recommended reporting cautions or sensitivity analyses were generated accordingly\\.$" = "Assumption screening flagged %s; recommended reporting cautions or sensitivity analyses were generated accordingly.",
      "^([0-9]+) item\\(s\\) flagged/review: (.+)$" = "%s item(s) flagged/review: %s",
      "^([0-9]+) check\\(s\\) OK$" = "%s check(s) OK",
      "^([0-9]+) assumption check item\\(s\\) reported\\.$" = "%s assumption check item(s) reported.",
      "^Analyses were performed using (.+)\\.$" = "Analyses were performed using %s.",
      "^Regression estimates were reported for (.+) with standard errors, p-values, and 95% confidence intervals\\.$" = "Regression estimates were reported for %s with standard errors, p-values, and 95%% confidence intervals.",
      "^Standard mice-based multiple imputation used ([0-9]+) fitted dataset\\(s\\); coefficients were pooled using Rubin total variance, Barnard-Rubin degrees of freedom, and t-based confidence intervals\\. (.+)$" = "Standard mice-based multiple imputation used %s fitted dataset(s); coefficients were pooled using Rubin total variance, Barnard-Rubin degrees of freedom, and t-based confidence intervals. %s",
      "^The (.+) family with (.+) link was held fixed across all imputed datasets\\.$" = "The %s family with %s link was held fixed across all imputed datasets.",
      "^The count family was selected once before pooling: median Poisson dispersion across ([0-9]+) imputations = (.+); threshold = (.+); locked family = (.+)\\.$" = "The count family was selected once before pooling: median Poisson dispersion across %s imputations = %s; threshold = %s; locked family = %s.",
      "^Multiple imputation \\(MI\\); analyzed rows after imputation: ([^ ]+) of ([^;]+); complete-case rows before MI: ([^ ]+) of ([^.]+)\\.$" = "Multiple imputation (MI); analyzed rows after imputation: %s of %s; complete-case rows before MI: %s of %s.",
      "^Inverse probability weighting \\(IPW\\); weighted complete-case rows: ([^ ]+) of ([^.]+)\\.$" = "Inverse probability weighting (IPW); weighted complete-case rows: %s of %s.",
      "^Complete-case: row-wise; complete cases used: ([^ ]+) of ([^.]+)\\.$" = "Complete-case: row-wise; complete cases used: %s of %s.",
      "^Fitted family: ([^,]+), link: ([^.]+)\\.$" = "Fitted family: %s, link: %s.",
      "^Outcome modeled using (.+) family\\.$" = "Outcome modeled using %s family.",
      "^Continuous outcome; (.+) link\\.$" = "Continuous outcome; %s link.",
      "^Strictly positive continuous outcome; (.+) link\\.$" = "Strictly positive continuous outcome; %s link.",
      "^Non-negative integer count outcome; negative-binomial model with (.+) link\\.$" = "Non-negative integer count outcome; negative-binomial model with %s link.",
      "^Non-negative integer count outcome; (.+) link\\.$" = "Non-negative integer count outcome; %s link.",
      "^Ordered factor; levels = (.+)\\. R ordered-factor contrasts are used unless the variable is recoded as numeric or nominal\\.$" = "Ordered factor; levels = %s. R ordered-factor contrasts are used unless the variable is recoded as numeric or nominal.",
      "^Pearson dispersion ratio = (.+)\\.$" = "Pearson dispersion ratio = %s.",
      "^Maximum model-matrix VIF = (.+)\\.$" = "Maximum model-matrix VIF = %s.",
      "^EPV = (.+) using the smaller outcome class and ([0-9]+) non-intercept coefficient\\(s\\)\\.$" = "EPV = %s using the smaller outcome class and %s non-intercept coefficient(s).",
      "^Max Cook's D = ([^;]+); high Cook's D count = ([^;]+); max leverage = ([^;]+); high leverage count = ([^.]+)\\.$" = "Max Cook's D = %s; high Cook's D count = %s; max leverage = %s; high leverage count = %s.",
      "^Binary outcome coded as event = (.+), reference = (.+)\\.$" = "Binary outcome coded as event = %s, reference = %s.",
      "^Factor with treatment contrasts; reference = (.+)\\.$" = "Factor with treatment contrasts; reference = %s.",
      "^Standard mice-based multiple imputation was selected for GLM missing-data handling \\(m = ([0-9]+), iterations = ([0-9]+)\\)\\.$" = "Standard mice-based multiple imputation was selected for GLM missing-data handling (m = %s, iterations = %s)."
    )
    translate_line <- function(value) {
      for (pattern in names(formats)) {
        matched <- regmatches(value, regexec(pattern, value, perl = TRUE))[[1L]]
        if (length(matched)) {
          args <- as.list(matched[-1L])
          if (grepl("; analyzed [0-9]+ of [0-9]+", value))
            args[[1L]] <- generalized_appendix_value_text(args[[1L]], language)
          if (identical(unname(formats[[pattern]]), "%s with %s link.")) {
            args[[1L]] <- generalized_appendix_value_text(args[[1L]], language)
            args[[2L]] <- localize_model_term(args[[2L]])
          }
          if (startsWith(value, "User selected "))
            args <- lapply(args, generalized_appendix_value_text, language = language)
          # These lists contain program-defined check names, not user variables.
          if (startsWith(value, "Assumption screening flagged ") || grepl("^[0-9]+ item\\(s\\) flagged/review:", value)) {
            index <- if (startsWith(value, "Assumption screening flagged ")) 1L else 2L
            separator <- if (index == 1L) ", " else "; "
            checks <- strsplit(args[[index]], separator, fixed = TRUE)[[1L]]
            args[[index]] <- paste(vapply(checks, generalized_appendix_value_text,
              character(1), language = language), collapse = separator)
          }
          if (startsWith(value, "Standard mice-based multiple imputation used "))
            args[[2L]] <- generalized_appendix_value_text(args[[2L]], language)
          if (startsWith(value, "The ") && grepl("family with ", value, fixed = TRUE))
            args <- lapply(args, localize_model_term)
          if (startsWith(value, "The count family was selected once before pooling:"))
            args[[4L]] <- localize_model_term(args[[4L]])
          if (grepl("^(Fitted family:|Outcome modeled using |Continuous outcome;|Strictly positive continuous outcome;|Non-negative integer count outcome;)", value))
            args <- lapply(args, localize_model_term)
          return(do.call(sprintf, c(
            list(statedu_localized_text(language, unname(formats[[pattern]]))), args)))
        }
      }
      result_appendix_ui_text(value, language)
    }
    return(paste(vapply(strsplit(text, "\n", fixed = TRUE)[[1L]], translate_line,
      character(1)), collapse = "\n"))
  }
  exact <- c(
    "Primary analysis" = "주 분석", "Family selection" = "분포 선택", "Link" = "링크",
    "Standard errors" = "표준오차", "Missing-data handling" = "결측자료 처리",
    "Assumption check summary" = "가정 검토 요약", "Recommended reporting" = "권고 보고 방식",
    "Selected strategy" = "선택한 방법", "MI datasets" = "MI 자료 수", "MI iterations" = "MI 반복 수",
    "Complete-case rows before MI" = "MI 전 완전사례 행", "Dependent-variable handling" = "종속변수 처리",
    "Complete-case rows" = "완전사례 행", "Selected auxiliary variables" = "선택한 보조변수",
    "IPW summary" = "IPW 요약", "Observation model variables" = "관측모형 변수",
    "Predicted observation probability: min" = "예측 관측확률: 최솟값",
    "Predicted observation probability: median" = "예측 관측확률: 중앙값",
    "Predicted observation probability: max" = "예측 관측확률: 최댓값",
    "Probability clipping count" = "확률 절단 건수", "Final weight summary" = "최종 가중치 요약",
    "Effective sample size" = "유효표본크기", "Weight clipping count" = "가중치 절단 건수",
    "Diagnostic note" = "진단 참고", "Intercept only" = "절편만 사용", "None selected" = "선택 없음",
    "Complete-case: row-wise" = "완전사례: 행 단위", "Multiple imputation (MI)" = "다중대치(MI)",
    "Inverse probability weighting (IPW)" = "역확률가중(IPW)",
    "General linear model" = "일반선형모형", "Binary logistic GLM" = "이분형 로지스틱 GLM",
    "Gamma GLM" = "감마 GLM", "Poisson GLM" = "포아송 GLM",
    "Negative binomial GLM" = "음이항 GLM", "Generalized linear model" = "일반화선형모형",
    "Linear Gaussian / identity" = "선형 가우시안 / 항등", "Binary logistic / logit" = "이분형 로지스틱 / 로짓",
    "Gamma / log" = "감마 / 로그", "Count: Poisson or negative binomial / log" = "카운트: 포아송 또는 음이항 / 로그",
    "Negative binomial / log" = "음이항 / 로그", "Poisson / log" = "포아송 / 로그",
    "Model-based" = "모형기반", "Robust sandwich HC0" = "강건 샌드위치 HC0",
    "Robust sandwich HC1" = "강건 샌드위치 HC1", "Robust sandwich HC2" = "강건 샌드위치 HC2",
    "Robust sandwich HC3" = "강건 샌드위치 HC3",
    "Model rationale" = "모형 선택 근거", "Family and link reported" = "분포와 링크 보고",
    "Missing data described" = "결측자료 기술", "Variable coding described" = "변수 코딩 기술",
    "Effect estimate and 95% CI reported" = "효과 추정치와 95% CI 보고",
    "Standard error method reported" = "표준오차 방법 보고", "Assumptions checked" = "가정 검토",
    "Count screening reported" = "카운트 선별 보고", "Publication table notes generated" = "출판표 주석 생성",
    "Software/package version reported" = "소프트웨어/패키지 버전 보고",
    "Manuscript-ready text generated" = "원고용 문장 생성",
    "Methods" = "방법", "Results" = "결과", "Assumptions" = "가정", "Software" = "소프트웨어",
    "Ready" = "준비됨", "Needs review" = "검토 필요", "Not selected" = "선택하지 않음",
    "Not applicable" = "해당 없음", "OK" = "양호", "Review" = "검토", "Flag" = "주의",
    "continuous" = "연속형", "binary" = "이분형", "category" = "범주형", "ordered" = "순서형",
    "Family / link" = "분포 / 링크", "Independent observations" = "관측치 독립성",
    "Poisson dispersion screening" = "포아송 산포 선별", "Residual diagnostics" = "잔차 진단",
    "Events per variable" = "변수당 사건 수", "Separation risk" = "분리 위험",
    "Sparse categorical cells" = "희소 범주 셀", "Influential observations" = "영향 관측치",
    "Collinearity" = "다중공선성", "Not run" = "실행하지 않음",
    "Raw rows" = "원자료 행", "Complete model rows" = "완전 모형 행",
    "Rows excluded by complete-case screen" = "완전사례 선별에서 제외된 행",
    "Rows with missing dependent variable" = "종속변수가 결측인 행",
    "Rows with any missing independent variable" = "독립변수 중 하나라도 결측인 행",
    "Rows with missing exposure / offset" = "노출량 / 오프셋이 결측인 행",
    "Distinct missingness patterns" = "서로 다른 결측 패턴 수",
    "Most common missingness pattern" = "가장 흔한 결측 패턴",
    "Standard GLM assumes independent observations after conditioning on predictors." =
      "표준 GLM은 예측변수를 조건화한 뒤 관측치가 서로 독립이라고 가정합니다.",
    "Confirm that the outcome scale and scientific estimand match the selected GLM family." =
      "결과변수의 척도와 과학적 추정대상이 선택한 GLM 분포에 부합하는지 확인하십시오.",
    "If observations are repeated, clustered, matched, or panel data, use GEE, LMM/GLMM, or cluster-robust/design-based analysis instead of ordinary GLM." =
      "반복, 군집, 대응 또는 패널 자료라면 일반 GLM 대신 GEE, LMM/GLMM 또는 군집강건/설계기반 분석을 사용하십시오.",
    "Shapiro-Wilk screening was applied to deviance residuals; this is a diagnostic screen, not a normality requirement for non-Gaussian GLMs." =
      "편차 잔차에 Shapiro-Wilk 선별검정을 적용했습니다. 이는 진단용 선별이며 비가우시안 GLM의 정규성 요건이 아닙니다.",
    "Inspect residual plots and consider robust inference or a more appropriate family/link." =
      "잔차 그림을 검토하고 강건 추론 또는 더 적절한 분포/링크를 고려하십시오.",
    "Residual diagnostic screening did not flag a major issue." =
      "잔차 진단 선별에서 주요 문제가 발견되지 않았습니다.",
    "No fitted-probability separation flag was detected." = "적합확률에서 분리 징후가 발견되지 않았습니다.",
    "Continue to inspect sparse categories and confidence interval width." =
      "희소 범주와 신뢰구간 너비를 계속 검토하십시오.",
    "Fitted probabilities or coefficients suggest possible complete/quasi-complete separation." =
      "적합확률 또는 계수에서 완전/준완전 분리 가능성이 나타났습니다.",
    "Collapse sparse categories, reduce predictors, or use Firth/penalized logistic regression." =
      "희소 범주를 통합하거나 예측변수를 줄이고 Firth/규제 로지스틱 회귀를 사용하십시오.",
    "No zero/sparse categorical predictor cell flag was detected." =
      "범주형 예측변수 셀에서 빈도 0 또는 희소 셀 징후가 발견되지 않았습니다.",
    "No sparse-cell action is suggested by this screen." = "이 선별 결과에서는 희소 셀 조치가 필요하지 않습니다.",
    "Collapse sparse levels or reduce categorical predictors before interpreting logistic coefficients." =
      "로지스틱 계수를 해석하기 전에 희소 수준을 통합하거나 범주형 예측변수를 줄이십시오.",
    "Inspect influential records and report sensitivity analysis if conclusions change." =
      "영향력이 큰 사례를 검토하고 결론이 달라지면 민감도 분석을 보고하십시오.",
    "No major influence flag by simple Cook's D and leverage screening rules." =
      "단순 Cook's D 및 레버리지 선별 규칙에서 주요 영향력 징후가 발견되지 않았습니다.",
    "Review correlated predictors or consider penalized regression for prediction-focused models." =
      "상관된 예측변수를 검토하거나 예측 중심 모형에서는 규제 회귀를 고려하십시오.",
    "No major VIF signal by the screening threshold." = "선별 기준에서 주요 VIF 징후가 발견되지 않았습니다.",
    "VIF could not be estimated." = "VIF를 추정할 수 없습니다.",
    "Dispersion ratio could not be estimated." = "산포비를 추정할 수 없습니다.",
    "No major overdispersion signal by the screening threshold." = "선별 기준에서 주요 과산포 징후가 발견되지 않았습니다.",
    "Use negative binomial for count outcomes or robust standard errors; investigate model misspecification." =
      "카운트 결과에는 음이항 모형 또는 강건 표준오차를 사용하고 모형 오지정을 점검하십시오.",
    "Event count appears adequate by the common EPV screening rule." = "일반적인 EPV 선별 규칙에서 사건 수가 충분합니다.",
    "Interpret coefficients cautiously and consider sensitivity analysis." = "계수를 주의해서 해석하고 민감도 분석을 고려하십시오.",
    "Estimates may be unstable; reduce predictors, collapse categories, or use penalized/Firth logistic regression." =
      "추정치가 불안정할 수 있습니다. 예측변수를 줄이거나 범주를 통합하고 규제/Firth 로지스틱 회귀를 사용하십시오.",
    "Factor with treatment contrasts; reference level could not be determined." =
      "처리 대비를 사용한 요인이며 기준 수준을 확인할 수 없습니다.",
    "Ordered factor; R ordered-factor contrasts are used unless the variable is recoded as numeric or nominal." =
      "순서형 요인이며 변수를 수치형 또는 명목형으로 재코딩하지 않으면 R의 순서형 요인 대비를 사용합니다.",
    "Binary outcome coded as event = second observed level and reference = first observed level." =
      "이분형 결과변수는 두 번째 관측 수준을 사건, 첫 번째 관측 수준을 기준으로 코딩했습니다.",
    "Rows with originally missing dependent-variable values are excluded from each fitted imputed GLM." =
      "원래 종속변수가 결측인 행은 각 대치 GLM 적합에서 제외합니다.",
    "Rows with imputed dependent-variable values are included as a sensitivity analysis; report this explicitly." =
      "종속변수 대치값이 있는 행은 민감도 분석에 포함하며 이를 명시해서 보고해야 합니다.",
    "The dependent variable has no missing values in the selected GLM variables." =
      "선택한 GLM 변수에서 종속변수의 결측값이 없습니다.",
    "MI was selected, but no missing values were present in selected GLM variables; complete data were fitted." =
      "MI를 선택했지만 선택한 GLM 변수에 결측값이 없어 완전자료를 적합했습니다.",
    "IPW was selected, but no complete-case exclusion occurred; unweighted complete data were fitted." =
      "IPW를 선택했지만 완전사례 제외가 없어 비가중 완전자료를 적합했습니다.",
    "Inverse-probability weighting was selected for GLM missing-data handling." =
      "GLM 결측자료 처리에 역확률가중을 선택했습니다.",
    "No fully observed predictors were available for the observation model; intercept-only IPW was used. Treat this as a weak IPW sensitivity analysis." =
      "관측모형에 사용할 완전 관측 예측변수가 없어 절편만 포함한 IPW를 사용했습니다. 제한적인 IPW 민감도 분석으로 해석하십시오.",
    "Observation status had no variation; unit weights were used." = "관측 상태에 변이가 없어 단위 가중치를 사용했습니다.",
    "Review positivity and weight stability; report the observation model and any clipping." =
      "양성성과 가중치 안정성을 검토하고 관측모형 및 절단 여부를 보고하십시오.",
    "No rows were excluded for missing values in selected analysis variables." =
      "선택한 분석변수의 결측값으로 제외된 행이 없습니다.",
    "Coefficient standard errors use model-based covariance." = "계수의 표준오차는 모형기반 공분산을 사용합니다.",
    "Poisson and negative binomial were treated as one count-family workflow; overdispersion screening selected the final fitted model." =
      "포아송과 음이항을 하나의 카운트 분포 절차로 다루고 과산포 선별로 최종 모형을 선택했습니다.",
    "No coding summary was generated." = "코딩 요약을 생성하지 못했습니다.",
    "Coefficient table includes B, SE, p-value, and 95% CI; exp(B) is added for logit/log models when selected." =
      "계수표에는 B, SE, p값과 95% CI가 포함되며 선택한 로짓/로그 모형에는 exp(B)를 추가합니다.",
    "Poisson dispersion, zero screen, and supplementary AIC/BIC are reported." =
      "포아송 산포, 영점 선별 및 보조 AIC/BIC를 보고합니다.",
    "The selected outcome was not fitted through the Count workflow." = "선택한 결과변수는 카운트 절차로 적합하지 않았습니다.",
    "Footnotes for family/link, standard errors, missing data, offset, and count screening are provided." =
      "분포/링크, 표준오차, 결측자료, 오프셋 및 카운트 선별에 대한 주석을 제공합니다.",
    "Suggested Methods, Results, Assumptions, and Software text is provided for manuscript drafting." =
      "원고 작성을 위한 방법, 결과, 가정 및 소프트웨어 문장을 제공합니다.",
    "Regression estimates were reported with standard errors, p-values, and 95% confidence intervals." =
      "회귀 추정치는 표준오차, p값 및 95% 신뢰구간과 함께 보고했습니다.",
    "Exponentiated coefficients were additionally reported for logit/log-link effects." =
      "로짓/로그 링크 효과에는 지수화 계수를 추가로 보고했습니다.",
    "Assumption screening did not flag a major issue among the selected GLM checks." =
      "선택한 GLM 검토 항목에서 주요 가정 문제가 발견되지 않았습니다.",
    "Assumption screening was not requested or no checks were selected." =
      "가정 선별을 요청하지 않았거나 선택한 검토 항목이 없습니다.",
    "For count outcomes, Poisson versus negative binomial selection was based on the prespecified dispersion-threshold screening rule, with AIC/BIC reported as supplementary diagnostics." =
      "카운트 결과에서는 사전 지정 산포 임계값 선별 규칙으로 포아송과 음이항을 선택했으며 AIC/BIC는 보조 진단값으로 보고했습니다.",
    "Software and package versions should be reported." = "소프트웨어와 패키지 버전을 보고해야 합니다.",
    "Continuous predictor; coefficient is per one-unit increase." = "연속형 예측변수이며 계수는 1단위 증가에 대한 값입니다.",
    "Offset applied as log(exposure); exposure values must be strictly positive." =
      "오프셋은 log(노출량)으로 적용하며 노출량은 양수여야 합니다.",
    "Auto selected binary logistic GLM because the dependent variable is marked binary or has two observed outcome levels." =
      "종속변수가 이분형으로 지정되었거나 관측 수준이 두 개이므로 이분형 로지스틱 GLM을 자동 선택했습니다.",
    "Auto selected the count-family workflow; Poisson overdispersion screening selected negative binomial as the final model." =
      "카운트 분포 절차를 자동 선택했으며 포아송 과산포 선별 결과 음이항을 최종 모형으로 선택했습니다.",
    "Auto selected the count-family workflow because the dependent variable is non-negative integer; Poisson was retained unless screening indicated otherwise." =
      "종속변수가 음이 아닌 정수이므로 카운트 분포 절차를 자동 선택했으며 선별 결과가 달라지지 않으면 포아송을 유지했습니다.",
    "Auto selected Gamma GLM because the dependent variable is strictly positive and right-skewed." =
      "종속변수가 양수이고 오른쪽으로 치우쳐 감마 GLM을 자동 선택했습니다.",
    "Auto selected Gaussian GLM because the dependent variable is continuous without count/gamma/binary screening flags." =
      "종속변수가 연속형이며 카운트/감마/이분형 선별 표시가 없어 가우시안 GLM을 자동 선택했습니다.",
    "Use the negative-binomial GLM as the primary count model and report the Poisson overdispersion screening." =
      "음이항 GLM을 주 카운트 모형으로 사용하고 포아송 과산포 선별 결과를 보고하십시오.",
    "Report Poisson GLM with caution and consider zero-inflated or hurdle sensitivity analysis." =
      "포아송 GLM을 주의해서 보고하고 영과잉 또는 허들 민감도 분석을 검토하십시오.",
    "Use Poisson GLM if count screening remains acceptable; report dispersion and zero-screen diagnostics." =
      "카운트 선별 결과가 허용 범위이면 포아송 GLM을 사용하고 산포 및 영점 선별 진단을 보고하십시오.",
    "Use the fitted GLM with caution and report flagged diagnostics; consider sensitivity analysis if conclusions depend on flagged items." =
      "적합한 GLM을 주의해서 사용하고 표시된 진단을 보고하며 결론이 해당 항목에 의존하면 민감도 분석을 검토하십시오.",
    "Use the fitted GLM as the primary analysis and report family, link, standard errors, missing-data strategy, and diagnostics." =
      "적합한 GLM을 주 분석으로 사용하고 분포, 링크, 표준오차, 결측자료 처리 방법 및 진단을 보고하십시오.",
    "Rows with originally missing dependent-variable values were excluded from each fitted imputed GLM." =
      "원래 종속변수가 결측인 행은 각 대치 GLM 적합에서 제외했습니다.",
    "Rows with imputed dependent-variable values were included as a sensitivity analysis." =
      "종속변수 대치값이 있는 행은 민감도 분석에 포함했습니다.",
    "Model-fit and assumption diagnostics use the first completed dataset as a transparent representative; coefficient inference uses all imputations." =
      "모형 적합도와 가정 진단은 첫 번째 완성 자료를 대표 자료로 사용하고 계수 추론은 모든 대치 자료를 사용합니다."
  )
  if (text %in% names(exact)) return(unname(exact[[text]]))

  capture <- function(pattern) {
    matched <- regmatches(text, regexec(pattern, text, perl = TRUE))[[1L]]
    if (length(matched) > 0L) matched else character(0)
  }
  localize_family <- function(value) {
    key <- tolower(trimws(as.character(value %||% "")))
    switch(
      key,
      gaussian = "가우시안", binomial = "이항", gamma = "감마", count = "카운트",
      poisson = "포아송", negative_binomial = "음이항", `negative binomial` = "음이항",
      value
    )
  }
  localize_link <- function(value) {
    key <- tolower(trimws(as.character(value %||% "")))
    switch(key, identity = "항등", logit = "로짓", log = "로그", inverse = "역수", value)
  }
  matched <- capture("^([0-9]+) item\\(s\\) flagged/review: (.+)$")
  if (length(matched)) {
    checks <- strsplit(matched[[3]], "; ", fixed = TRUE)[[1L]]
    checks <- paste(vapply(checks, generalized_appendix_value_text, character(1), language = language), collapse = "; ")
    return(sprintf("%s개 항목 주의/검토 필요: %s", matched[[2]], checks))
  }
  matched <- capture("^([0-9]+) check\\(s\\) OK$")
  if (length(matched)) return(sprintf("%s개 검토 항목 양호", matched[[2]]))
  matched <- capture("^Fitted family: ([^,]+), link: ([^.]+)\\.$")
  if (length(matched)) return(sprintf("적합 분포: %s, 링크: %s.", localize_family(matched[[2]]), localize_link(matched[[3]])))
  matched <- capture("^User selected (.+); fitted as (.+)\\.$")
  if (length(matched)) return(sprintf("사용자 선택: %s; 실제 적합: %s.", generalized_appendix_value_text(matched[[2]], language), generalized_appendix_value_text(matched[[3]], language)))
  matched <- capture("^Pearson dispersion ratio = (.+)\\.$")
  if (length(matched)) return(sprintf("Pearson 산포비 = %s.", matched[[2]]))
  matched <- capture("^EPV = (.+) using the smaller outcome class and ([0-9]+) non-intercept coefficient\\(s\\)\\.$")
  if (length(matched)) return(sprintf("더 작은 결과 범주와 절편 제외 계수 %s개를 사용한 EPV = %s.", matched[[3]], matched[[2]]))
  matched <- capture("^Max Cook's D = ([^;]+); high Cook's D count = ([^;]+); max leverage = ([^;]+); high leverage count = ([^.]+)\\.$")
  if (length(matched)) return(sprintf("최대 Cook's D = %s; 높은 Cook's D 사례 수 = %s; 최대 레버리지 = %s; 높은 레버리지 사례 수 = %s.", matched[[2]], matched[[3]], matched[[4]], matched[[5]]))
  matched <- capture("^Maximum model-matrix VIF = (.+)\\.$")
  if (length(matched)) return(sprintf("모형행렬 최대 VIF = %s.", matched[[2]]))
  matched <- capture("^Continuous outcome; (.+) link\\.$")
  if (length(matched)) return(sprintf("연속형 결과변수; %s 링크.", localize_link(matched[[2]])))
  matched <- capture("^Strictly positive continuous outcome; (.+) link\\.$")
  if (length(matched)) return(sprintf("양수인 연속형 결과변수; %s 링크.", localize_link(matched[[2]])))
  matched <- capture("^Non-negative integer count outcome; (?!negative-binomial model with )(.+) link\\.$")
  if (length(matched)) return(sprintf("음이 아닌 정수 카운트 결과변수; %s 링크.", localize_link(matched[[2]])))
  matched <- capture("^Non-negative integer count outcome; negative-binomial model with (.+) link\\.$")
  if (length(matched)) return(sprintf("음이 아닌 정수 카운트 결과변수; %s 링크 음이항 모형.", localize_link(matched[[2]])))
  matched <- capture("^Binary outcome coded as event = (.+), reference = (.+)\\.$")
  if (length(matched)) return(sprintf("이분형 결과변수 코딩: 사건 = %s, 기준 = %s.", matched[[2]], matched[[3]]))
  matched <- capture("^Factor with treatment contrasts; reference = (.+)\\.$")
  if (length(matched)) return(sprintf("처리 대비를 사용한 요인; 기준 = %s.", matched[[2]]))
  matched <- capture("^Ordered factor; levels = (.+)\\. R ordered-factor contrasts are used unless the variable is recoded as numeric or nominal\\.$")
  if (length(matched)) return(sprintf("순서형 요인; 수준 = %s. 변수를 수치형 또는 명목형으로 재코딩하지 않으면 R의 순서형 요인 대비를 사용합니다.", matched[[2]]))
  matched <- capture("^Outcome modeled using (.+) family\\.$")
  if (length(matched)) return(sprintf("결과변수에 %s 분포를 사용했습니다.", localize_family(matched[[2]])))
  matched <- capture("^([0-9]+) of ([0-9]+)$")
  if (length(matched)) return(sprintf("%s/%s", matched[[2]], matched[[3]]))
  matched <- capture("^Complete \\(n=([0-9]+)\\)$")
  if (length(matched)) return(sprintf("완전사례 (n=%s)", matched[[2]]))
  matched <- capture("^Standard mice-based multiple imputation was selected for GLM missing-data handling \\(m = ([0-9]+), iterations = ([0-9]+)\\)\\.$")
  if (length(matched)) return(sprintf("GLM 결측자료 처리에 표준 mice 기반 다중대치를 선택했습니다(m = %s, 반복 = %s).", matched[[2]], matched[[3]]))
  matched <- capture("^Standard mice-based multiple imputation used ([0-9]+) fitted dataset\\(s\\); coefficients were pooled using Rubin total variance, Barnard-Rubin degrees of freedom, and t-based confidence intervals\\. (.+)$")
  if (length(matched)) return(sprintf("표준 mice 기반 다중대치로 %s개 자료를 적합했으며 Rubin 총분산, Barnard-Rubin 자유도 및 t 기반 신뢰구간으로 계수를 통합했습니다. %s", matched[[2]], generalized_appendix_value_text(matched[[3]], language)))
  matched <- capture("^The (.+) family with (.+) link was held fixed across all imputed datasets\\.$")
  if (length(matched)) return(sprintf("모든 대치 자료에서 %s 분포와 %s 링크를 동일하게 고정했습니다.", matched[[2]], matched[[3]]))
  matched <- capture("^The count family was selected once before pooling: median Poisson dispersion across ([0-9]+) imputations = (.+); threshold = (.+); locked family = (.+)\\.$")
  if (length(matched)) return(sprintf("통합 전에 카운트 분포를 한 번 선택했습니다: %s회 대치의 포아송 산포 중앙값 = %s; 임계값 = %s; 고정 분포 = %s.", matched[[2]], matched[[3]], matched[[4]], matched[[5]]))
  matched <- capture("^Multiple imputation \\(MI\\); analyzed rows after imputation: ([^ ]+) of ([^;]+); complete-case rows before MI: ([^ ]+) of ([^.]+)\\.$")
  if (length(matched)) return(sprintf("다중대치(MI); 대치 후 분석 행: %s/%s; MI 전 완전사례 행: %s/%s.", matched[[2]], matched[[3]], matched[[4]], matched[[5]]))
  matched <- capture("^Inverse probability weighting \\(IPW\\); weighted complete-case rows: ([^ ]+) of ([^.]+)\\.$")
  if (length(matched)) return(sprintf("역확률가중(IPW); 가중 완전사례 행: %s/%s.", matched[[2]], matched[[3]]))
  matched <- capture("^Complete-case: row-wise; complete cases used: ([^ ]+) of ([^.]+)\\.$")
  if (length(matched)) return(sprintf("완전사례(행 단위); 사용한 완전사례: %s/%s.", matched[[2]], matched[[3]]))
  matched <- capture("^(.+); analyzed ([^ ]+) of ([^ ]+) rows(?:; complete-case rows before missing-data engine: ([^ ]+))?\\.$")
  if (length(matched)) {
    method <- generalized_appendix_value_text(matched[[2]], language)
    suffix <- if (length(matched) >= 5L && nzchar(matched[[5]])) sprintf("; 결측자료 처리 전 완전사례 행: %s", matched[[5]]) else ""
    return(sprintf("%s; %s/%s개 행 분석%s.", method, matched[[3]], matched[[4]], suffix))
  }
  matched <- capture("^Complete-case GLM excluded ([0-9]+) of ([0-9]+) rows with missing values in selected analysis variables\\.$")
  if (length(matched)) return(sprintf("완전사례 GLM에서 선택한 분석변수의 결측값으로 %s/%s개 행을 제외했습니다.", matched[[2]], matched[[3]]))
  matched <- capture("^Complete-case rows before missing-data engine: ([0-9]+) of ([0-9]+)\\.$")
  if (length(matched)) return(sprintf("결측자료 처리 전 완전사례 행: %s/%s.", matched[[2]], matched[[3]]))
  matched <- capture("^Observation model: (.+); IPW clipped at the 99th percentile and normalized to mean 1\\. Report the observation model and review positivity/weight stability\\.$")
  if (length(matched)) return(sprintf("관측모형: %s; IPW를 99백분위수에서 절단하고 평균 1로 정규화했습니다. 관측모형을 보고하고 양성성/가중치 안정성을 검토하십시오.", matched[[2]]))
  matched <- capture("^Observation model failed \\((.+)\\); intercept-only IPW was used\\. Treat this as a weak IPW sensitivity analysis\\.$")
  if (length(matched)) return(sprintf("관측모형 적합에 실패하여(%s) 절편만 포함한 IPW를 사용했습니다. 제한적인 IPW 민감도 분석으로 해석하십시오.", matched[[2]]))
  matched <- capture("^Coefficient standard errors use (.+) sandwich robust covariance\\.$")
  if (length(matched)) return(sprintf("계수의 표준오차는 %s 샌드위치 강건 공분산을 사용합니다.", matched[[2]]))
  matched <- capture("^Robust standard errors \\((.+)\\) were requested, but robust covariance could not be computed; model-based covariance is shown\\.$")
  if (length(matched)) return(sprintf("강건 표준오차(%s)를 요청했지만 강건 공분산을 계산할 수 없어 모형기반 공분산을 제시합니다.", matched[[2]]))
  matched <- capture("^Exposure offset applied as log\\((.+)\\)\\.$")
  if (length(matched)) return(sprintf("노출량 오프셋을 log(%s)로 적용했습니다.", matched[[2]]))
  matched <- capture("^([0-9]+) coding row\\(s\\) generated\\.$")
  if (length(matched)) return(sprintf("코딩 정보 %s개 행을 생성했습니다.", matched[[2]]))
  matched <- capture("^([0-9]+) assumption check item\\(s\\) reported\\.$")
  if (length(matched)) return(sprintf("가정 검토 항목 %s개를 보고했습니다.", matched[[2]]))
  matched <- capture("^Analyses were performed using (.+)\\.$")
  if (length(matched)) return(sprintf("분석에는 %s를 사용했습니다.", matched[[2]]))
  matched <- capture("^Regression estimates were reported for (.+) with standard errors, p-values, and 95% confidence intervals\\.$")
  if (length(matched)) return(sprintf("%s의 회귀 추정치를 표준오차, p값 및 95%% 신뢰구간과 함께 보고했습니다.", matched[[2]]))
  matched <- capture("^Assumption screening flagged (.+); recommended reporting cautions or sensitivity analyses were generated accordingly\\.$")
  if (length(matched)) {
    checks <- paste(vapply(strsplit(matched[[2]], ", ", fixed = TRUE)[[1L]], generalized_appendix_value_text, character(1), language = language), collapse = ", ")
    return(sprintf("가정 선별에서 %s 항목이 표시되어 이에 따른 보고상 주의사항 또는 민감도 분석을 제시했습니다.", checks))
  }
  matched <- capture("^(.+) Missing data were handled using (.+), analyzing ([^ ]+) of ([^ ]+) row\\(s\\)\\. Standard errors were reported as (.+)\\.$")
  if (length(matched)) return(sprintf("%s 결측자료는 %s으로 처리하여 %s/%s개 행을 분석했습니다. 표준오차는 %s으로 보고했습니다.", generalized_appendix_value_text(matched[[2]], language), generalized_appendix_value_text(matched[[3]], language), matched[[4]], matched[[5]], generalized_appendix_value_text(matched[[6]], language)))
  matched <- capture("^(.+) was fitted for dependent variable (.+) with independent variables (.+)\\. Requested family: (.+); fitted family: (.+)\\.$")
  if (length(matched)) return(sprintf("종속변수 %s와 독립변수 %s에 %s을(를) 적합했습니다. 요청 분포: %s; 적합 분포: %s.", matched[[3]], matched[[4]], generalized_appendix_value_text(matched[[2]], language), generalized_appendix_value_text(matched[[5]], language), localize_family(matched[[6]])))
  matched <- capture("^(.+) with (.+) link\\.$")
  if (length(matched)) return(sprintf("%s, %s 링크.", generalized_appendix_value_text(matched[[2]], language), localize_link(matched[[3]])))
  matched <- capture("^(.+); analyzed ([0-9]+) of ([0-9]+) row\\(s\\)\\.$")
  if (length(matched)) return(sprintf("%s; %s/%s개 행 분석.", generalized_appendix_value_text(matched[[2]], language), matched[[3]], matched[[4]]))
  text
}

generalized_localize_manuscript_results <- function(localized, table, language) {
  if (all(c("Imputation", "Family", "Link", "Term signature") %in% names(table))) {
    families <- c(gaussian="Gaussian distribution",binomial="Binomial distribution",gamma="Gamma distribution",
      count="Poisson distribution",poisson="Poisson distribution",negative_binomial="Negative binomial distribution")
    families_ko <- c(gaussian="가우시안",binomial="이항",gamma="감마",count="포아송",poisson="포아송",negative_binomial="음이항")
    links <- c(identity="Identity link",logit="Logit link",log="Log link",inverse="Inverse link")
    links_ko <- c(identity="항등",logit="로짓",log="로그",inverse="역수")
    for(column in c("Family", "Link")) {
      labels <- if(column=="Family") families else links
      korean <- if(column=="Family") families_ko else links_ko
      localized[[match(column,names(table))]] <- vapply(as.character(table[[column]]),function(value) {
        if(is.na(value) || !value %in% names(labels)) return(value)
        statedu_localized_text(language,unname(labels[[value]]),unname(korean[[value]]))
      },character(1))
    }
    localized[[match("Term signature",names(table))]] <- table[["Term signature"]]
  }
  user_rows <- attr(table, "generalized_user_value_rows", exact = TRUE)
  if (length(user_rows) && "Value" %in% names(table)) {
    rows <- user_rows[user_rows >= 1L & user_rows <= nrow(table)]
    localized[[match("Value", names(table))]][rows] <- table$Value[rows]
  }
  rp <- attr(table, "generalized_checklist_rationale", exact = TRUE)
  if (!is.null(rp) && all(c("Item", "Details") %in% names(table))) {
    row <- which(as.character(table$Item) == "Model rationale")
    if (length(row) == 1L && identical(as.character(table$Details[[row]]), rp$source)) {
      localized[[match("Details", names(table))]][[row]] <- sprintf(statedu_localized_text(language,
        "%s was fitted for dependent variable %s with independent variables %s. Requested family: %s; fitted family: %s.",
        "%s을(를) 종속변수 %s와 독립변수 %s에 적합했습니다. 요청 분포: %s; 적합 분포: %s."),
        generalized_appendix_value_text(rp$method, language), rp$outcome, rp$predictors,
        generalized_appendix_value_text(rp$requested, language), generalized_appendix_value_text(rp$fitted, language))
    }
  }
  methods <- attr(table, "generalized_manuscript_methods", exact = TRUE)
  if (!is.null(methods) && all(c("Section", "SuggestedText") %in% names(table))) {
    row <- which(as.character(table$Section) == "Methods")
    if (length(row) == 1L && identical(as.character(table$SuggestedText[[row]]), methods$source)) {
      rationale <- generalized_appendix_value_text(as.character(methods$rationale), language)
      rp <- methods$rationale_parts
      if (!is.null(rp) && identical(as.character(methods$rationale), rp$source)) {
        rationale <- sprintf(statedu_localized_text(language,
          "%s was fitted for dependent variable %s with independent variables %s. Requested family: %s; fitted family: %s.",
          "%s을(를) 종속변수 %s와 독립변수 %s에 적합했습니다. 요청 분포: %s; 적합 분포: %s."),
          generalized_appendix_value_text(rp$method, language), rp$outcome, rp$predictors,
          generalized_appendix_value_text(rp$requested, language), generalized_appendix_value_text(rp$fitted, language))
      }
      tail <- sprintf(statedu_localized_text(language,
        "Missing data were handled using %s, analyzing %s of %s row(s). Standard errors were reported as %s.",
        "결측자료는 %s으로 처리하여 %s/%s개 행을 분석했습니다. 표준오차는 %s으로 보고했습니다."),
        generalized_appendix_value_text(methods$missing, language), methods$n, methods$raw_n,
        generalized_appendix_value_text(methods$se, language))
      localized[[match("SuggestedText", names(table))]][[row]] <- trimws(paste(rationale, tail))
    }
  }
  parts <- attr(table, "generalized_manuscript_results", exact = TRUE)
  if (is.null(parts) || !all(c("Section", "SuggestedText") %in% names(table))) return(localized)
  row <- which(as.character(table$Section) == "Results")
  if (length(row) != 1L || !identical(as.character(table$SuggestedText[[row]]), parts$source)) return(localized)
  effect <- if (length(parts$terms)) {
    template <- if (isTRUE(parts$more_terms))
      "Regression estimates were reported for %s, among other terms, with standard errors, p-values, and 95%% confidence intervals." else
      "Regression estimates were reported for %s with standard errors, p-values, and 95%% confidence intervals."
    korean <- if (isTRUE(parts$more_terms))
      "%s 등의 회귀 추정치를 표준오차, p값 및 95%% 신뢰구간과 함께 보고했습니다." else
      "%s의 회귀 추정치를 표준오차, p값 및 95%% 신뢰구간과 함께 보고했습니다."
    sprintf(statedu_localized_text(language, template, korean), paste(parts$terms, collapse = ", "))
  } else generalized_appendix_value_text("Regression estimates were reported with standard errors, p-values, and 95% confidence intervals.", language)
  extra <- parts$extra[nzchar(parts$extra)]
  localized[[match("SuggestedText", names(table))]][[row]] <- paste(c(effect,
    vapply(extra, generalized_appendix_value_text, character(1), language = language)), collapse = " ")
  localized
}

generalized_appendix_table <- function(table, language = NULL) {
  if (!is.data.frame(table)) return(table)
  language <- result_appendix_table_language(language)
  localized <- result_appendix_localize_table(table, language)
  if (identical(language, "en")) return(localized)
  if (!identical(language, "ko")) {
    # Work from original text so generic localization cannot partially rewrite
    # a template or the user-supplied values embedded inside it.
    for (index in seq_along(table)) {
      if (is.character(table[[index]]) || is.factor(table[[index]]))
        localized[[index]] <- vapply(as.character(table[[index]]),
          generalized_appendix_value_text, character(1), language = language)
    }
    localized <- generalized_localize_manuscript_results(localized, table, language)
    return(result_appendix_preserve_data(localized, table))
  }
  header_map <- c(
    "Item" = "항목", "Variable" = "변수", "Value" = "\uac12", "Check" = "\uac80\ud1a0",
    "Result" = "결과", "Statistic" = "통계량", "Interpretation" = "\ud574\uc11d", "Recommendation" = "권고",
    "Missing" = "\uacb0\uce21", "Missing %" = "\uacb0\uce21 %", "Role" = "\uc5ed\ud560",
    "Measurement" = "\uce21\uc815\uc218\uc900", "Coding" = "\ucf54\ub529", "Tolerance" = "\uacf5\ucc28\ud55c\uacc4",
    "Within variance" = "\ub300\uce58 \ub0b4 \ubd84\uc0b0", "Between variance" = "\ub300\uce58 \uac04 \ubd84\uc0b0",
    "Total variance" = "\ucd1d\ubd84\uc0b0", "Family" = "\ubd84\ud3ec", "Link" = "\ub9c1\ud06c",
    "Residual df" = "\uc794\ucc28 \uc790\uc720\ub3c4", "Term signature" = "\ud56d \uad6c\uc131",
    "Poisson dispersion" = "Poisson \uc0b0\ud3ec", "Software" = "\uc18c\ud504\ud2b8\uc6e8\uc5b4",
    "Version" = "\ubc84\uc804", "Section" = "\uad6c\ubd84", "Text" = "\ubb38\uc7a5",
    "SuggestedText" = "\uc81c\uc548 \ubb38\uc7a5", "Details" = "\uc138\ubd80\ub0b4\uc6a9", "Message" = "\uba54\uc2dc\uc9c0"
  )
  original_names <- names(table)
  current_names <- names(localized)
  names(localized) <- vapply(seq_along(current_names), function(index) {
    key <- original_names[[index]]
    if (key %in% names(header_map)) unname(header_map[[key]]) else current_names[[index]]
  }, character(1))
  value_map <- c(
    "Ready" = "\uc900\ube44\ub428", "Needs review" = "\uac80\ud1a0 \ud544\uc694",
    "Not selected" = "\uc120\ud0dd\ud558\uc9c0 \uc54a\uc74c", "Not applicable" = "\ud574\ub2f9 \uc5c6\uc74c",
    "Flag" = "\uc8fc\uc758", "Review" = "\uac80\ud1a0", "Pass" = "\ud1b5\uacfc",
    "Dependent variable" = "\uc885\uc18d\ubcc0\uc218", "Independent variable" = "\ub3c5\ub9bd\ubcc0\uc218",
    "Exposure / offset" = "\ub178\ucd9c / \uc624\ud504\uc14b", "Summary" = "\uc694\uc57d"
  )
  for (index in seq_along(localized)) {
    values <- as.character(localized[[index]])
    matched <- values %in% names(value_map)
    values[matched] <- unname(value_map[values[matched]])
    localized[[index]] <- vapply(values, function(value) {
      if (is.na(value)) return(NA_character_)
      lines <- strsplit(value, "\n", fixed = TRUE)[[1L]]
      paste(vapply(lines, generalized_appendix_value_text, character(1), language = language), collapse = "\n")
    }, character(1))
  }
  if (all(c("Item", "Value") %in% original_names)) {
    item_index <- match("Item", original_names)
    value_index <- match("Value", original_names)
    link_rows <- as.character(table[[item_index]]) == "Link"
    if (any(link_rows, na.rm = TRUE)) {
      raw_links <- tolower(trimws(as.character(table[[value_index]][link_rows])))
      localized[[value_index]][link_rows] <- vapply(raw_links, function(value) {
        switch(value, identity = "항등", logit = "로짓", log = "로그", inverse = "역수", value)
      }, character(1))
    }
  }
  attr(localized, "result_table_role") <- "appendix"
  attr(localized, "result_table_language") <- language
  localized <- generalized_localize_manuscript_results(localized, table, language)
  result_appendix_preserve_data(localized, table)
}

generalized_table_panel <- function(title, table_tag, class = "") {
  if (is.null(table_tag)) return(NULL)
  div(
    class = paste("result-section regression-result-panel generalized-result-panel", class),
    h3(title),
    table_tag
  )
}

generalized_exponentiated_estimate_label <- function(result) {
  switch(
    as.character(result$family %||% "")[[1]],
    binomial = "OR",
    gamma = "Mean ratio",
    count = "RR",
    negative_binomial = "RR",
    "exp(B)"
  )
}

generalized_coefficient_display_input <- function(result) {
  table <- result$coef_table
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  exp_columns <- c("exp(B)", "exp(LLCI)", "exp(ULCI)")
  if (isTRUE(result$exponentiate) && all(exp_columns %in% names(table))) {
    estimate_label <- generalized_exponentiated_estimate_label(result)
    display <- data.frame(Term = as.character(table$Term), stringsAsFactors = FALSE, check.names = FALSE)
    display[[estimate_label]] <- table[["exp(B)"]]
    display$LLCI <- table[["exp(LLCI)"]]
    display$ULCI <- table[["exp(ULCI)"]]
    if ("p" %in% names(table)) display$p <- table$p
    return(display)
  }
  raw_columns <- if (as.character(result$family %||% "")[[1]] %in% c("binomial", "gamma", "count", "negative_binomial")) {
    c("Term", "B", "SE", "Statistic", "df", "p")
  } else {
    c("Term", "B", "SE", "Statistic", "df", "p", "LLCI", "ULCI")
  }
  keep <- intersect(raw_columns, names(table))
  table[, keep, drop = FALSE]
}

generalized_display_coef_table <- function(result, variable_table = NULL, labels = character(0), category_table = NULL) {
  table <- generalized_coefficient_display_input(result)
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  coefficient_output_table_with_context(
    table,
    predictors = as.character(result$predictors %||% character(0)),
    include_references = TRUE,
    variable_info = variable_table,
    refs = regression_reference_values_static(category_table),
    value_labels = category_value_label_lookup_static(category_table),
    labels = labels,
    category_table = category_table
  )
}

generalized_display_assumption_table <- function(result) {
  table <- result$assumption_checks
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  data.frame(
    Check = as.character(table$Check),
    Result = as.character(table$Result),
    Statistic = vapply(table$Statistic, longitudinal_format_number, character(1)),
    p = vapply(table$p, format_p, character(1)),
    Interpretation = as.character(table$Interpretation),
    Recommendation = as.character(table$Recommendation),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

generalized_display_missing_table <- function(result) {
  table <- result$missing_table
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  data.frame(
    Variable = as.character(table$Variable),
    Missing = as.integer(table$Missing),
    `Missing %` = vapply(table$`Missing %`, longitudinal_format_number, character(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

generalized_display_count_details <- function(result) {
  table <- result$count_details
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  data.frame(
    Item = as.character(table$Item),
    Value = as.character(table$Value),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

generalized_display_missing_details <- function(result) {
  table <- result$missing_details
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  data.frame(
    Item = as.character(table$Item),
    Value = as.character(table$Value),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

generalized_display_mi_pooling_diagnostics <- function(result) {
  table <- result$mi_pooling_diagnostics
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  data.frame(
    Term = as.character(table$Term),
    m = as.integer(table$m),
    df = vapply(table$df, longitudinal_format_number, character(1)),
    RIV = vapply(table$RIV, longitudinal_format_number, character(1)),
    FMI = vapply(table$FMI, longitudinal_format_number, character(1)),
    `Within variance` = vapply(table$`Within variance`, longitudinal_format_number, character(1)),
    `Between variance` = vapply(table$`Between variance`, longitudinal_format_number, character(1)),
    `Total variance` = vapply(table$`Total variance`, longitudinal_format_number, character(1)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

generalized_display_mi_fit_diagnostics <- function(result) {
  table <- result$mi_fit_diagnostics
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  out <- table
  if ("Poisson dispersion" %in% names(out)) {
    out[["Poisson dispersion"]] <- vapply(out[["Poisson dispersion"]], longitudinal_format_number, character(1))
  }
  out
}

generalized_display_missing_pattern <- function(result) {
  table <- result$missing_pattern
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  data.frame(
    Item = as.character(table$Item),
    Value = as.character(table$Value),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

generalized_display_decision_summary <- function(result) {
  table <- result$decision_summary
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  data.frame(
    Item = as.character(table$Item),
    Value = as.character(table$Value),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

generalized_display_coding_summary <- function(result) {
  table <- result$coding_summary
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  data.frame(
    Variable = as.character(table$Variable),
    Role = as.character(table$Role),
    Measurement = as.character(table$Measurement),
    Coding = as.character(table$Coding),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

generalized_display_vif_table <- function(result) {
  table <- result$vif_table
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  data.frame(
    Term = as.character(table$Term),
    VIF = vapply(table$VIF, longitudinal_format_number, character(1)),
    Tolerance = vapply(table$Tolerance, longitudinal_format_number, character(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

generalized_fit_summary_lines <- function(result) {
  table <- result$fit_stats
  if (!is.data.frame(table) || nrow(table) == 0 || !all(c("Item", "Value") %in% names(table))) {
    return(character(0))
  }
  values <- stats::setNames(as.character(table$Value), as.character(table$Item))
  get_value <- function(name) {
    value <- trimws(named_value(values, name, ""))
    if (nzchar(value)) value else NA_character_
  }
  has_value <- function(value) {
    isTRUE(!is.na(value) && nzchar(value))
  }
  line <- function(...) {
    parts <- Filter(function(value) !is.na(value) && nzchar(value), c(...))
    if (length(parts) == 0) "" else paste(parts, collapse = "; ")
  }
  count_values <- if (is.data.frame(result$count_details) && nrow(result$count_details) > 0 && all(c("Item", "Value") %in% names(result$count_details))) {
    stats::setNames(as.character(result$count_details$Value), as.character(result$count_details$Item))
  } else {
    character(0)
  }
  count_value <- function(name) {
    value <- trimws(named_value(count_values, name, ""))
    if (nzchar(value)) value else NA_character_
  }
  aic_value <- get_value("AIC")
  bic_value <- get_value("BIC")
  loglik_value <- get_value("Log likelihood")
  dispersion_value <- get_value("Dispersion")
  residual_df_value <- get_value("Residual df")
  r2_value <- get_value("R\u00B2")
  adjusted_r2_value <- get_value("Adjusted R\u00B2")
  mcfadden_value <- get_value("McFadden pseudo R\u00B2")
  r2_parts <- if (has_value(r2_value)) {
    c(
      sprintf("R%s=%s", "\u00b2", r2_value),
      if (has_value(adjusted_r2_value)) sprintf("adjusted R%s=%s", "\u00b2", adjusted_r2_value) else NA_character_
    )
  } else if (has_value(mcfadden_value)) {
    sprintf("McFadden pseudo R%s=%s", "\u00b2", mcfadden_value)
  } else {
    character(0)
  }
  lines <- c(
    line(
      if (has_value(loglik_value)) sprintf("LogLik=%s", loglik_value) else NA_character_,
      if (has_value(aic_value)) sprintf("AIC=%s", aic_value) else NA_character_,
      if (has_value(bic_value)) sprintf("BIC=%s", bic_value) else NA_character_
    ),
    line(
      r2_parts,
      if (has_value(dispersion_value)) sprintf("dispersion=%s", dispersion_value) else NA_character_,
      if (has_value(residual_df_value)) sprintf("residual df=%s", residual_df_value) else NA_character_
    ),
    line(
      if (has_value(count_value("Poisson dispersion ratio"))) {
        sprintf("Poisson overdispersion screen: ratio=%s", count_value("Poisson dispersion ratio"))
      } else {
        NA_character_
      },
      if (has_value(count_value("Overdispersion threshold"))) {
        sprintf("threshold=%s", count_value("Overdispersion threshold"))
      } else {
        NA_character_
      },
      if (has_value(count_value("Selected family"))) {
        sprintf("selected=%s", count_value("Selected family"))
      } else {
        NA_character_
      }
    )
  )
  lines[nzchar(lines)]
}

generalized_coefficient_note <- function(result) {
  se_label <- generalized_se_type_label(result$se_type_used %||% "model")
  family <- as.character(result$family %||% "")[[1]]
  abbreviation_note <- c("B = unstandardized coefficient; CI = confidence interval; SE = standard error")
  if (identical(family, "binomial")) abbreviation_note <- c(abbreviation_note, "OR = odds ratio")
  if (family %in% c("count", "negative_binomial")) abbreviation_note <- c(abbreviation_note, "RR = rate ratio")
  family_note <- switch(
    family,
    gaussian = "A Gaussian identity-link model was fitted",
    binomial = "A binomial logit-link model was fitted",
    gamma = "A gamma log-link model was fitted",
    count = "A Poisson log-link model was fitted",
    negative_binomial = "A negative-binomial log-link model was fitted",
    sprintf("A %s model was fitted", result$method %||% "generalized linear")
  )
  missing_note <- sprintf(
    "Missing data were handled using %s (%s of %s rows analyzed)",
    result$missing_method %||% "complete-case analysis",
    as.character(result$n %||% ""),
    as.character(result$raw_n %||% "")
  )
  result_sci_note_text(
    abbreviations = abbreviation_note,
    estimation = c(family_note, sprintf("Standard errors were computed using %s", se_label), missing_note)
  )
}

generalized_display_coef_table_with_fit_rows <- function(result, variable_table = NULL, labels = character(0), category_table = NULL) {
  table <- generalized_display_coef_table(result, variable_table, labels, category_table)
  if (!is.data.frame(table) || nrow(table) == 0) return(table)
  footer_lines <- generalized_fit_summary_lines(result)
  if (length(footer_lines) == 0) return(table)
  footer <- as.data.frame(stats::setNames(as.list(rep("", ncol(table))), names(table)), stringsAsFactors = FALSE, check.names = FALSE)
  footer <- footer[rep(1L, length(footer_lines)), , drop = FALSE]
  footer$Term <- footer_lines
  rbind(table, footer)
}

generalized_coefficient_html_table <- function(table, result) {
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(div(class = "empty-message", "No coefficient table was returned."))
  }
  footer_lines <- generalized_fit_summary_lines(result)
  table <- generalized_main_table(table)
  table_tag <- tags$table(
    class = "coefficient-table generalized-coefficient-table",
    style = paste0(result_table_style(font_size = 12, min_width = 0), "width:100% !important;min-width:0 !important;max-width:100% !important;table-layout:fixed;"),
    tags$thead(tags$tr(lapply(seq_along(names(table)), function(index) {
      tags$th(style = result_header_cell_style(first = index == 1L), names(table)[[index]])
    }))),
    tags$tbody(lapply(seq_len(nrow(table)), function(row_index) {
      tags$tr(lapply(seq_along(table), function(col_index) {
        tags$td(style = result_body_cell_style(first = col_index == 1L, last = length(footer_lines) == 0 && row_index == nrow(table)), as.character(table[[col_index]][[row_index]] %||% ""))
      }))
    })),
    if (length(footer_lines) > 0) {
      tags$tfoot(lapply(seq_along(footer_lines), function(index) {
        tags$tr(
          class = "coefficient-fit-row generalized-fit-row",
          tags$td(
            colspan = ncol(table),
            style = paste0(
              "padding:5px 10px;line-height:1.35;border-left:0;border-right:0;",
              "border-top:", if (index == 1L) "2px solid #1f2937" else "1px solid #d7dde5", ";",
              "border-bottom:", if (index == length(footer_lines)) "0" else "1px solid #d7dde5", ";",
              "text-align:center !important;white-space:normal;font-size:12px;"
            ),
            footer_lines[[index]]
          )
        )
      }))
    }
  )
  intrinsic_width <- result_table_intrinsic_width(table, first_width = 118, default_width = 62, min_width = 480)
  contract <- result_table_contract(table, role = "main", language = "en", intrinsic_width = intrinsic_width)
  result_table_with_notes(
    result_table_apply_contract(table_tag, contract),
    result_note_tag(generalized_coefficient_note(result))
  )
}

generalized_html_table <- function(
  table,
  widths = NULL,
  align = NULL,
  font_size = 12,
  table_role = "appendix",
  table_language = NULL,
  note_line = NULL
) {
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(div(class = "empty-message", "No table was returned."))
  }
  table_role <- result_table_role(table_role, table)
  table_language <- result_table_language(table_role, table_language)
  display_table <- if (identical(table_role, "appendix")) generalized_appendix_table(table, table_language) else generalized_main_table(table)
  columns <- names(display_table)
  column_count <- length(columns)
  if (is.null(widths) || length(widths) != column_count) {
    widths <- rep(sprintf("%.3f%%", 100 / column_count), column_count)
  }
  if (is.null(align) || length(align) != column_count) {
    align <- c("left", rep("right", max(0, column_count - 1L)))
  }
  cell_style <- function(index, last = FALSE, header = FALSE) {
    paste0(
      "padding:5px 10px;line-height:1.35;border-left:0;border-right:0;",
      "border-top:0;border-bottom:", if (isTRUE(header)) "2px solid #1f2937" else if (isTRUE(last)) "0" else "1px solid #d7dde5", ";",
      "vertical-align:middle;background:transparent;white-space:normal;",
      "font-variant-numeric:tabular-nums lining-nums;font-feature-settings:'tnum' 1,'lnum' 1;",
      "overflow-wrap:break-word;word-break:normal;",
      "font-weight:", if (isTRUE(header)) "700" else "400", ";",
      "font-size:", if (isTRUE(header)) max(8, font_size - 1) else font_size, "px;",
      "width:", widths[[index]], " !important;",
      "min-width:0 !important;max-width:none !important;",
      "text-align:", align[[index]], " !important;"
    )
  }
  table_tag <- tags$table(
    class = "coefficient-table generalized-result-table",
    style = paste0(result_table_style(font_size = font_size, min_width = 0), "width:100% !important;min-width:0 !important;table-layout:fixed;"),
    tags$colgroup(lapply(widths, function(width) tags$col(style = paste0("width:", width, ";")))),
    tags$thead(tags$tr(lapply(seq_along(columns), function(index) {
      tags$th(style = cell_style(index, header = TRUE), columns[[index]])
    }))),
    tags$tbody(lapply(seq_len(nrow(display_table)), function(row_index) {
      tags$tr(lapply(seq_along(display_table), function(col_index) {
        tags$td(style = cell_style(col_index, last = row_index == nrow(display_table)), as.character(display_table[[col_index]][[row_index]] %||% ""))
      }))
    }))
  )
  intrinsic_width <- result_table_intrinsic_width(display_table, first_width = 118, default_width = 62, min_width = 480)
  contract <- result_table_contract(
    display_table,
    role = table_role,
    language = table_language,
    intrinsic_width = intrinsic_width
  )
  result_table_with_notes(
    result_table_apply_contract(table_tag, contract),
    result_note_tag(note_line)
  )
}

generalized_result_block <- function(result, variable_table = NULL, labels = character(0), category_table = NULL) {
  coef_table <- generalized_display_coef_table(result, variable_table, labels, category_table)
  missing_table <- generalized_display_missing_table(result)
  missing_details <- generalized_display_missing_details(result)
  mi_pooling_diagnostics <- generalized_display_mi_pooling_diagnostics(result)
  mi_fit_diagnostics <- generalized_display_mi_fit_diagnostics(result)
  missing_pattern <- generalized_display_missing_pattern(result)
  decision_summary <- generalized_display_decision_summary(result)
  coding_summary <- generalized_display_coding_summary(result)
  count_details <- generalized_display_count_details(result)
  assumption_table <- generalized_display_assumption_table(result)
  vif_table <- generalized_display_vif_table(result)
  software_versions <- result$software_versions
  if (!is.data.frame(software_versions)) software_versions <- data.frame()
  reporting_checklist <- result$reporting_checklist
  if (!is.data.frame(reporting_checklist)) reporting_checklist <- data.frame()
  manuscript_text <- result$manuscript_text
  if (!is.data.frame(manuscript_text)) manuscript_text <- data.frame()
  notes <- as.character(result$notes %||% character(0))
  notes <- notes[nzchar(notes)]
  outcome <- display_variable_name_static(result$outcome, variable_table, labels, label_only = TRUE)
  appendix_language <- result_appendix_table_language()
  missing_summary <- switch(
    as.character(result$missing_strategy %||% "complete")[[1]],
    mi = sprintf(
      "%s; analyzed rows after imputation: %s of %s; complete-case rows before MI: %s of %s.",
      result$missing_method %||% "Multiple imputation",
      as.character(result$n %||% ""),
      as.character(result$raw_n %||% ""),
      as.character(result$complete_case_n %||% ""),
      as.character(result$raw_n %||% "")
    ),
    ipw = sprintf(
      "%s; weighted complete-case rows: %s of %s.",
      result$missing_method %||% "Inverse probability weighting",
      as.character(result$n %||% result$complete_case_n %||% ""),
      as.character(result$raw_n %||% "")
    ),
    sprintf(
      "%s; complete cases used: %s of %s.",
      result$missing_method %||% "Complete-case",
      as.character(result$complete_case_n %||% result$n %||% ""),
      as.character(result$raw_n %||% result$n %||% "")
    )
  )

  sections <- list()
  add_section <- function(title, table_tag, class = "") {
    section <- generalized_table_panel(title, table_tag, class)
    if (!is.null(section)) sections[[length(sections) + 1L]] <<- section
  }

  if (nrow(coef_table) > 0) {
    add_section(
      sprintf("Coefficients - %s: %s", result$method, outcome),
      generalized_coefficient_html_table(coef_table, result),
      "generalized-coefficient-panel"
    )
  }
  if (nrow(decision_summary) > 0) {
    add_section(
      generalized_appendix_text("Model overview", appendix_language),
      generalized_html_table(
        decision_summary,
        widths = c("34%", "66%"),
        align = c("left", "right"),
        table_role = "appendix",
        table_language = appendix_language
      ),
      "generalized-overview-panel"
    )
  }
  if (nrow(mi_pooling_diagnostics) > 0) {
    add_section(
      generalized_appendix_text("Multiple-imputation pooling diagnostics", appendix_language),
      generalized_html_table(
        mi_pooling_diagnostics,
        table_role = "appendix",
        table_language = appendix_language,
        note_line = generalized_appendix_text(
          "RIV = relative increase in variance; FMI = fraction of missing information. Degrees of freedom use the Barnard-Rubin adjustment.",
          appendix_language
        )
      ),
      "generalized-mi-pooling-panel"
    )
  }
  if (nrow(mi_fit_diagnostics) > 0) {
    add_section(
      generalized_appendix_text("Imputation-specific model checks", appendix_language),
      generalized_html_table(
        mi_fit_diagnostics,
        table_role = "appendix",
        table_language = appendix_language,
        note_line = generalized_appendix_text(
          "All imputed datasets must use the same family, link, term signature, residual df, and standard-error method; otherwise pooling is stopped.",
          appendix_language
        )
      ),
      "generalized-mi-fit-panel"
    )
  }
  if (nrow(count_details) > 0) {
    add_section(
      generalized_appendix_text("Count-family / overdispersion screening", appendix_language),
      generalized_html_table(
        count_details,
        widths = c("36%", "64%"),
        align = c("left", "right"),
        table_role = "appendix",
        table_language = appendix_language
      ),
      "generalized-count-panel"
    )
  }
  if (nrow(assumption_table) > 0) {
    add_section(
      generalized_appendix_text("Assumption checks", appendix_language),
      generalized_html_table(
        assumption_table,
        widths = c("17%", "9%", "9%", "7%", "29%", "29%"),
        align = c("left", "center", "right", "right", "left", "left"),
        table_role = "appendix",
        table_language = appendix_language
      ),
      "generalized-assumption-panel"
    )
  }
  if (nrow(reporting_checklist) > 0) {
    add_section(
      generalized_appendix_text("SCI reporting checklist", appendix_language),
      generalized_html_table(
        reporting_checklist,
        widths = c("24%", "12%", "64%"),
        align = c("left", "center", "left"),
        table_role = "appendix",
        table_language = appendix_language
      ),
      "generalized-checklist-panel"
    )
  }
  if (nrow(coding_summary) > 0) {
    add_section(
      generalized_appendix_text("Variable coding", appendix_language),
      generalized_html_table(
        coding_summary,
        widths = c("18%", "18%", "14%", "50%"),
        align = c("left", "center", "center", "left"),
        table_role = "appendix",
        table_language = appendix_language
      ),
      "generalized-coding-panel"
    )
  }
  if (nrow(vif_table) > 0) {
    add_section(
      generalized_appendix_text("Collinearity diagnostics", appendix_language),
      generalized_html_table(
        vif_table,
        widths = c("50%", "25%", "25%"),
        align = c("left", "right", "right"),
        table_role = "appendix",
        table_language = appendix_language
      ),
      "generalized-collinearity-panel"
    )
  }

  missing_overview <- data.frame(Item = "Summary", Value = missing_summary, stringsAsFactors = FALSE, check.names = FALSE)
  add_section(
    generalized_appendix_text("Missing-data handling", appendix_language),
    generalized_html_table(
      missing_overview,
      widths = c("24%", "76%"),
      align = c("left", "left"),
      table_role = "appendix",
      table_language = appendix_language
    ),
    "generalized-missing-overview-panel"
  )
  if (nrow(missing_details) > 0) {
    add_section(
      generalized_appendix_text("Missing-data details", appendix_language),
      generalized_html_table(
        missing_details,
        widths = c("36%", "64%"),
        align = c("left", "right"),
        table_role = "appendix",
        table_language = appendix_language
      ),
      "generalized-missing-details-panel"
    )
  }
  if (nrow(missing_pattern) > 0) {
    add_section(
      generalized_appendix_text("Missing-data pattern", appendix_language),
      generalized_html_table(
        missing_pattern,
        widths = c("42%", "58%"),
        align = c("left", "right"),
        table_role = "appendix",
        table_language = appendix_language
      ),
      "generalized-missing-pattern-panel"
    )
  }
  if (nrow(missing_table) > 0) {
    add_section(
      generalized_appendix_text("Missing values", appendix_language),
      generalized_html_table(
        missing_table,
        widths = c("50%", "25%", "25%"),
        align = c("left", "right", "right"),
        table_role = "appendix",
        table_language = appendix_language
      ),
      "generalized-missing-values-panel"
    )
  }
  if (nrow(manuscript_text) > 0) {
    add_section(
      generalized_appendix_text("Suggested manuscript text", appendix_language),
      generalized_html_table(
        manuscript_text,
        widths = c("22%", "78%"),
        align = c("left", "left"),
        table_role = "appendix",
        table_language = appendix_language
      ),
      "generalized-manuscript-panel"
    )
  }
  if (nrow(software_versions) > 0) {
    add_section(
      generalized_appendix_text("Software versions", appendix_language),
      generalized_html_table(
        software_versions,
        widths = c("55%", "45%"),
        align = c("left", "left"),
        table_role = "appendix",
        table_language = appendix_language
      ),
      "generalized-software-panel"
    )
  }
  if (length(notes) > 0) {
    add_section(
      generalized_appendix_text("Model diagnostics", appendix_language),
      generalized_html_table(
        data.frame(Message = notes, stringsAsFactors = FALSE, check.names = FALSE),
        widths = "100%",
        align = "left",
        table_role = "appendix",
        table_language = appendix_language
      ),
      "generalized-notes-panel"
    )
  }
  do.call(tagList, sections)
}

generalized_results_panel <- function(result, variable_table = NULL, labels = character(0), category_table = NULL) {
  if (is.null(result)) return(NULL)
  div(
    class = "regression-results generalized-results",
    generalized_result_block(result, variable_table, labels, category_table)
  )
}

saved_generalized_results_html <- function(
  result,
  variable_table = NULL,
  labels = character(0),
  category_table = NULL,
  css_path = file.path("www", "style.css"),
  report_mode = FALSE
) {
  content <- div(
    class = "page-shell",
    div(
      class = "app-heading",
      h1("Generalized Linear Model (GLM)"),
      div("Gaussian, logistic, gamma, Poisson, and negative-binomial generalized linear model results.", class = "app-subtitle")
    ),
    generalized_results_panel(result, variable_table, labels, category_table)
  )
  saved_results_document(
    title = "Generalized Linear Model (GLM)",
    content = content,
    max_width = 688,
    css_path = css_path,
    report_mode = report_mode
  )
}

write_generalized_results_html <- function(
  result,
  file,
  variable_table = NULL,
  labels = character(0),
  category_table = NULL
) {
  write_result_html_document(
    saved_generalized_results_html(result, variable_table, labels, category_table),
    file,
    useBytes = TRUE
  )
  invisible(file)
}

write_generalized_results_pdf <- function(
  result,
  file,
  variable_table = NULL,
  labels = character(0),
  category_table = NULL
) {
  write_pdf_from_html(
    saved_generalized_results_html(result, variable_table, labels, category_table, report_mode = TRUE),
    file
  )
}

save_generalized_excel_file <- function (result, file, variable_table = NULL, labels = character(0), category_table = NULL)
{
    save_screen_excel_file(saved_generalized_results_html(result = result, variable_table = variable_table, labels = labels,
        category_table = category_table), file)
}
