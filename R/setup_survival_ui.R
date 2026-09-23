# Survival analysis setup UI.

survival_restore_ui_values <- function(ui, values = list()) {
  walk <- function(tag) {
    if (!inherits(tag, "shiny.tag")) {
      if (is.list(tag)) tag[] <- lapply(tag, walk)
      return(tag)
    }
    id <- tag$attribs$id
    if (!is.null(id) && id %in% names(values)) {
      value <- values[[id]]
      if (identical(tag$name, "select")) {
        select_option <- function(option) {
          if (inherits(option,"html")) {
            document <- xml2::read_html(paste0("<select>",as.character(option),"</select>"),encoding="UTF-8")
            nodes <- xml2::xml_find_all(document,"//option")
            for (i in seq_along(nodes)) xml2::xml_attr(nodes[[i]],"selected") <- if(xml2::xml_attr(nodes[[i]],"value") %in% as.character(value)) "selected" else NULL
            return(htmltools::HTML(paste(vapply(xml2::xml_children(xml2::xml_find_first(document,"//select")),as.character,character(1)),collapse="\n")))
          }
          if (inherits(option,"shiny.tag") && identical(option$name,"option")) option$attribs$selected <- if (option$attribs$value %in% as.character(value)) "selected" else NULL
          else if (is.list(option) && !inherits(option,"shiny.tag")) option[] <- lapply(option,select_option)
          return(option)
        }
        tag$children <- lapply(tag$children,select_option)
      } else if (identical(tag$name,"input")) {
        if (identical(tag$attribs$type,"checkbox")) tag$attribs$checked <- if(isTRUE(value)) "checked" else NULL
        else tag$attribs$value <- as.character(value)
      }
    }
    tag$children <- lapply(tag$children,walk)
    tag
  }
  walk(ui)
}

survival_setup_tab_panel <- function(language = statedu_initial_language(), values = list()) {
  language <- normalize_app_language(language)
  ko <- identical(language, "ko")
  objective_values <- c("group_comparison", "association", "prediction", "recurrent", "competing", "state_transition")
  objective_labels <- statedu_localized_text(language, c("Group comparison", "Association", "Prediction", "Recurrent events", "Competing risks", "State transition"), c("집단 비교", "연관성 추정", "예측", "반복 사건", "경쟁위험", "상태 전이"))
  shape_values <- c("single_record", "entry_exit", "start_stop", "interval_censored")
  shape_labels <- statedu_localized_text(language, c("One row per subject", "Entry–exit", "Start–stop", "Interval-censored"), c("대상자당 한 행", "Entry–exit", "Start–stop", "구간 검열"))
  event_values <- c("single", "competing", "recurrent", "multistate")
  event_labels <- statedu_localized_text(language, c("Single event", "Competing events", "Recurrent events", "Multi-state"), c("단일 사건", "경쟁 사건", "반복 사건", "다상태"))
  estimand_values <- c("", "cumulative_incidence", "cause_specific", "both")
  estimand_labels <- statedu_localized_text(language, c("Choose...", "Cumulative incidence", "Cause-specific hazard", "Both"), c("선택 필요", "누적발생", "원인별 위험", "둘 다"))
  survival_restore_ui_values(tabPanel(
    survival_ui_text("Analysis Setup", language), value = "analysis_survival_setup",
    div(class = "page-shell",
      div(class = "app-heading",
        h1(survival_ui_text("Analysis Setup", language)),
        div(statedu_localized_text(language, "Choose the analysis that matches the research question and data structure.", "연구 질문과 자료 구조를 확인하여 적절한 생존분석을 추천합니다."), class = "app-subtitle")
      ),
      div(class = "workspace-panel frequencies-workspace-panel survival-workspace-panel analysis-three-block-workspace survival-design-workspace",
        div(class = "analysis-workspace-heading survival-design-workspace-heading",
          h3(statedu_localized_text(language, "Choose a survival analysis", "생존분석 선택"))
        ),
        div(class = "survival-design-setup-grid analysis-three-block-setup-grid",
          div(class = "analysis-transfer-column analysis-transfer-panel survival-design-question-panel",
            h4(statedu_localized_text(language, "1. Study design", "1. 연구 설계")),
            selectInput("survival_design_objective", statedu_localized_text(language, "Objective", "연구 목적"), choices = stats::setNames(objective_values, objective_labels)),
            selectInput("survival_design_shape", statedu_localized_text(language, "Data shape", "자료 형태"), choices = stats::setNames(shape_values, shape_labels)),
            selectInput("survival_design_events", statedu_localized_text(language, "Event structure", "사건 구조"), choices = stats::setNames(event_values, event_labels)),
            conditionalPanel("input.survival_design_events == 'competing' || input.survival_design_objective == 'competing'",
              selectInput("survival_design_estimand", statedu_localized_text(language, "Competing-risk estimand", "경쟁위험 목표량"), choices = stats::setNames(estimand_values, estimand_labels))
            ),
            checkboxInput("survival_design_time_dependent", statedu_localized_text(language, "Time-dependent covariates", "시간의존 공변량이 있음"), FALSE)
          ),
          uiOutput("survival_contract_setup")
        ),
        analysis_three_block_action_row(
          class = "survival-design-action-row",
          run_button = actionButton("run_survival_design", statedu_localized_text(language, "Recommend analysis", "분석 추천"), class = "btn btn-primary")
        ),
        div(class = "survival-design-results", uiOutput("survival_design_recommendation"))
      )
    )
  ), values)
}

survival_contract_setup_panel <- function(selected_names, shape = "single_record", event_structure = "single", language = statedu_initial_language(), saved_values = list()) {
  ko <- identical(normalize_app_language(language), "ko")
  variables <- survival_selected_names(selected_names)
  choices <- c(stats::setNames("", statedu_localized_text(language, "Choose...", "선택")), stats::setNames(variables, variables))
  time_roles <- switch(shape,
    entry_exit = tagList(selectInput("survival_contract_entry", statedu_localized_text(language, "Entry time", "진입 시간"), choices), selectInput("survival_contract_time", statedu_localized_text(language, "Exit time", "종료 시간"), choices)),
    start_stop = tagList(selectInput("survival_contract_subject_id", statedu_localized_text(language, "Subject ID", "대상자 ID"), choices), selectInput("survival_contract_start", statedu_localized_text(language, "Interval start", "구간 시작"), choices), selectInput("survival_contract_stop", statedu_localized_text(language, "Interval stop", "구간 종료"), choices)),
    interval_censored = div(class = "result-note", statedu_localized_text(language, "Interval-censored data are not supported by the v1 standard analyses.", "구간 검열 자료는 v1 표준 분석에서 지원하지 않습니다.")),
    tagList(selectInput("survival_contract_time", statedu_localized_text(language, "Observed time", "관찰 시간"), choices), selectInput("survival_contract_subject_id", statedu_localized_text(language, "Subject ID (optional)", "대상자 ID (선택)"), choices))
  )
  survival_restore_ui_values(div(class = "survival-contract-grid-fragment",
    div(class = "analysis-transfer-column analysis-transfer-panel survival-design-time-panel",
      h4(statedu_localized_text(language, "2. Time definition", "2. 시간 정의")),
      textInput("survival_contract_origin", statedu_localized_text(language, "Time origin", "시간 원점"), placeholder = statedu_localized_text(language, "e.g., date of surgery", "예: 수술일")),
      selectInput("survival_contract_unit", statedu_localized_text(language, "Time unit", "시간 단위"), choices = stats::setNames(c("", "day", "week", "month", "year", "other"), statedu_localized_text(language, c("Choose...", "Day", "Week", "Month", "Year", "Other"), c("선택", "일", "주", "월", "년", "기타")))),
      conditionalPanel("input.survival_contract_unit == 'other'", textInput("survival_contract_custom_unit", statedu_localized_text(language, "Custom unit", "사용자 단위"))),
      time_roles
    ),
    div(class = "analysis-options-column analysis-options-panel survival-design-variable-panel",
      h4(statedu_localized_text(language, "3. Event & analysis variables", "3. 사건·분석 변수")),
      selectInput("survival_contract_event", statedu_localized_text(language, "Event variable", "사건 변수"), choices),
      selectInput("survival_contract_group", statedu_localized_text(language, "Group variable (optional)", "집단 변수 (선택)"), choices),
      selectInput("survival_contract_covariates", statedu_localized_text(language, "Covariates (optional)", "공변량 (선택)"), choices = stats::setNames(variables, variables), multiple = TRUE),
      uiOutput("survival_event_map_setup")
    )
  ), saved_values)
}

survival_event_map_panel <- function(values, language = statedu_initial_language(), saved_values = list()) {
  ko <- identical(normalize_app_language(language), "ko")
  text <- function(en, kr) statedu_localized_text(language, en, kr)
  values <- unique(trimws(as.character(values[!is.na(values)])))
  values <- values[nzchar(values)]
  if (!length(values)) return(div(class = "result-note", text("Select an event variable to display its observed-value map.", "사건 변수를 선택하면 관측값 매핑표가 표시됩니다.")))
  role_values <- c("unknown", "censored", "event_of_interest", "competing_event", "exclude")
  role_labels <- if (ko) c("미확인", "검열", "관심 사건", "경쟁사건", "제외") else c("Unknown", "Censored", "Event of interest", "Competing event", "Exclude")
  if (!normalize_app_language(language) %in% c("en", "ko")) role_labels <- vapply(role_values, function(x) statedu_localized_text(language, paste("Survival event role", x)), character(1))
  survival_restore_ui_values(div(class = "survival-event-map-panel",
    h4(text("Observed event-code mapping", "관측된 사건 코드 매핑")),
    lapply(seq_along(values), function(index) {
      proposed <- if (values[[index]] == "0") "censored" else if (values[[index]] == "1") "event_of_interest" else "unknown"
      div(class = "survival-event-map-row",
        div(class = "survival-event-map-raw", tags$label(sprintf(text("Raw value: %s", "원자료값: %s"), values[[index]]))),
        div(class = "survival-event-map-role", selectInput(
          paste0("survival_event_role_", index),
          NULL,
          choices = stats::setNames(role_values, role_labels),
          selected = proposed,
          selectize = FALSE
        )),
        div(class = "survival-event-map-label", textInput(paste0("survival_event_label_", index), NULL, value = values[[index]], placeholder = text("Event label", "사건 라벨")))
      )
    }),
    checkboxInput("survival_event_map_confirmed", text("I confirmed the meaning of every event code.", "모든 사건 코드의 의미를 확인했습니다."), FALSE),
    div(class = "result-note", text("The 0/1 roles are initial suggestions only; analysis is blocked until confirmed.", "0/1 역할은 초기 제안일 뿐이며 확인 전에는 분석할 수 없습니다."))
  ), saved_values)
}

survival_recommendation_text <- function(value, language = statedu_initial_language()) {
  value <- as.character(value %||% "")
  language <- normalize_app_language(language)
  if (identical(language, "en") || !length(value)) return(value)
  translations <- c(
    "Complete the survival data contract" = "생존분석 데이터 계약을 완료하세요",
    "Kaplan-Meier, log-rank test, and RMST" = "Kaplan–Meier 생존곡선, log-rank 검정 및 RMST",
    "Cox proportional hazards regression" = "Cox 비례위험 회귀분석",
    "Cumulative incidence function and Gray test" = "누적발생함수(CIF)와 Gray 검정",
    "Competing-risk regression" = "경쟁위험 회귀분석",
    "Cause-specific Cox regression (HR)" = "원인별 Cox 회귀분석(HR)",
    "Fine-Gray regression (sHR)" = "Fine–Gray 회귀분석(sHR)",
    "Cause-specific Cox and Fine-Gray regression" = "원인별 Cox 및 Fine–Gray 회귀분석",
    "Time-dependent Cox regression" = "시간의존 Cox 회귀분석",
    "Time-dependent Cox model" = "시간의존 Cox 모형",
    "Interval-censored survival model" = "구간 검열 생존모형",
    "Survival prediction model" = "생존 예측모형",
    "Recurrent-event model" = "반복 사건 모형",
    "Multi-state model" = "다상태 모형",
    "Delayed-entry competing-risk model" = "지연 진입 경쟁위험 모형",
    "Cause-specific summaries" = "원인별 사건 요약",
    "Kaplan-Meier descriptive curves" = "Kaplan–Meier 기술 생존곡선",
    "Cox regression for adjusted association" = "보정 연관성 추정을 위한 Cox 회귀분석",
    "Subject-level cluster-robust standard errors will be used." = "대상자 수준 군집 강건 표준오차를 사용합니다.",
    "Confirm that the start-stop rows represent time-dependent covariate intervals." = "Start–stop 행이 시간의존 공변량 구간을 나타내는지 확인하세요.",
    "Choose whether the target is cumulative incidence, cause-specific hazard, or both." = "분석 목표를 누적발생, 원인별 위험 또는 둘 다 중에서 선택하세요.",
    "Start-stop and time-dependent covariates require a later survival module." = "Start–stop 자료와 시간의존 공변량은 후속 생존분석 모듈이 필요합니다."
    ,"Interval-censored data are not supported in survival v1." = "생존분석 v1에서는 구간 검열 자료를 지원하지 않습니다."
    ,"Prediction modelling and validation are not supported in survival v1." = "생존분석 v1에서는 예측 모형 개발과 검증을 지원하지 않습니다."
    ,"Recurrent-event models are not supported in survival v1." = "생존분석 v1에서는 반복 사건 모형을 지원하지 않습니다."
    ,"Multi-state models are not supported in survival v1." = "생존분석 v1에서는 다상태 모형을 지원하지 않습니다."
    ,"The current CIF/Fine-Gray engine does not support delayed entry." = "현재 CIF/Fine–Gray 분석 엔진은 지연 진입을 지원하지 않습니다."
  )
  translated <- unname(translations[value])
  if (!identical(language, "ko")) {
    known <- !is.na(translated)
    translated[known] <- vapply(value[known], function(x) statedu_localized_text(language, x), character(1), USE.NAMES = FALSE)
  }
  translated[is.na(translated)] <- value[is.na(translated)]
  translated
}

survival_issue_text <- function(code, fallback = "", language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  if (identical(language, "en")) return(as.character(fallback))
  if (length(code) > 1L) return(vapply(seq_along(code), function(i) survival_issue_text(code[[i]], rep_len(as.character(fallback), length(code))[[i]], language), character(1)))
  extra <- c(
    invalid_time_encoding = "시간 역할 값은 숫자형 기간이어야 합니다. 날짜·시간 필드는 분석 전에 경과 기간으로 변환하세요.",
    competing_event_requires_competing_risk = "경쟁사건 코드는 일반 Kaplan–Meier 또는 Cox 분석에서 통상적인 검열로 처리할 수 없습니다.",
    unsupported_other_state = "other_state로 지정된 사건 코드는 다상태 생존모형이 필요하며 현재 생존분석 버전에서는 지원하지 않습니다."
  )
  if (length(code) == 1L && !is.na(code) && code == "missing_columns") {
    prefix <- "Variables not found: "
    if (!startsWith(fallback, prefix)) return(as.character(fallback))
    return(paste0(statedu_t("survival.setup.missing_columns_prefix", language, if (language == "ko") "변수를 찾을 수 없음: " else prefix), substring(fallback, nchar(prefix) + 1L)))
  }
  if (length(code) == 1L && !is.na(code) && code %in% names(extra)) {
    return(if (language == "ko") unname(extra[code]) else statedu_t(paste0("survival.setup.extra.", code), language, fallback))
  }
  translations <- c(
    missing_time_origin = "시간 원점을 입력하세요.",
    missing_time_unit = "시간 단위를 선택하거나 입력하세요.",
    event_map_not_confirmed = "관측된 모든 사건 코드의 의미를 확인하세요.",
    event_of_interest_count = "관심 사건 코드를 정확히 하나 지정하세요.",
    missing_competing_event_code = "경쟁사건 코드를 하나 이상 지정하세요.",
    missing_time_role = "시간 변수를 선택하세요.",
    missing_event_role = "사건 변수를 선택하세요.",
    missing_entry_role = "진입 시간 변수를 선택하세요.",
    missing_subject_id = "Start–stop 자료에는 대상자 ID가 필요합니다.",
    missing_interval_role = "구간 시작 및 종료 변수를 선택하세요.",
    conflicting_roles = "하나의 변수가 서로 충돌하는 필수 역할에 중복 지정되었습니다.",
    invalid_time = "시간값은 유한한 0 이상의 값이어야 합니다.",
    entry_not_before_exit = "진입 시간은 종료 시간보다 빨라야 합니다.",
    invalid_interval_time = "구간 시간값은 유한한 0 이상의 값이어야 합니다.",
    start_not_before_stop = "구간 시작 시간은 종료 시간보다 빨라야 합니다.",
    missing_subject_id_value = "모든 Start–stop 행에 대상자 ID가 필요합니다.",
    invalid_event_map = "모든 관측 사건 코드에 유효한 역할을 하나씩 지정하세요.",
    overlapping_intervals = "대상자 내 위험 구간이 서로 겹칩니다.",
    interval_after_event = "사건 발생 후에도 위험 구간이 남아 있습니다.",
    multiple_subject_events = "표준 시간의존 Cox 설정에서 한 대상자에게 사건 행이 여러 개 있습니다.",
    intervals_not_source_order = "일부 대상자 구간이 시간순으로 저장되지 않아 시작 시간순으로 분석합니다.",
    no_analysis_rows = "생존분석 변수를 적용한 뒤 분석 가능한 완전한 행이 없습니다.",
    no_events = "선택한 관심 사건이 자료에서 발견되지 않았습니다.",
    potential_competing_events = "여러 비관심 사건 코드가 기존 설정에서 검열로 처리되었습니다."
  )
  translated <- unname(translations[as.character(code)])
  if (identical(language, "ko")) return(ifelse(is.na(translated), as.character(fallback), translated))
  fallback <- rep_len(as.character(fallback), length(code))
  vapply(seq_along(code), function(i) {
    if (is.na(translated[[i]])) return(fallback[[i]])
    statedu_t(paste0("survival.setup.issue.", code[[i]]), language, fallback[[i]])
  }, character(1))
}

survival_recommendation_explanation <- function(result, language = statedu_initial_language()) {
  ko <- identical(normalize_app_language(language), "ko")
  rule_ids <- as.character(result$rule_ids %||% character(0))
  key <- if (length(rule_ids)) rule_ids[[1]] else ""
  explanations <- list(
    G01 = list(
      reason = if (ko) "하나의 관심 사건이 있는 자료에서 두 집단 이상의 생존 경험을 비교하는 연구 목적에 적합합니다." else "This matches a group-comparison question with one event of interest.",
      outputs = if (ko) c("집단별 Kaplan–Meier 생존곡선과 위험대상자 수", "중앙 생존시간과 지정 시점 생존율", "log-rank 검정 및 제한평균생존시간(RMST)") else c("Kaplan-Meier curves and numbers at risk by group", "Median survival and survival rates at requested times", "Log-rank test and restricted mean survival time (RMST)"),
      alternative = if (ko) "연령 등 공변량을 보정한 집단 효과나 위험비(HR)가 필요하면 Cox 회귀분석을 선택합니다." else "Choose Cox regression when adjusted group effects or hazard ratios are required."
    ),
    A01 = list(
      reason = if (ko) "생존시간과 하나 이상의 설명변수 사이의 보정된 연관성을 추정하려는 목적에 적합합니다." else "This matches an adjusted association question between survival time and one or more predictors.",
      outputs = if (ko) c("각 공변량의 위험비(HR), 95% 신뢰구간 및 유의확률", "비례위험 가정 검정", "선택한 집단의 보정 생존곡선") else c("Hazard ratios, 95% confidence intervals, and p-values", "Proportional-hazards assumption checks", "Adjusted survival curves for a selected group"),
      alternative = if (ko) "보정 없이 집단별 생존곡선과 전체적인 차이만 비교하려면 Kaplan–Meier 분석을 선택합니다." else "Choose Kaplan-Meier when only unadjusted group curves and an overall comparison are needed."
    ),
    G02 = list(
      reason = if (ko) "관심 사건 외의 사건이 관심 사건의 발생을 막을 수 있는 경쟁사건 자료의 집단 비교에 적합합니다." else "This matches a group comparison where competing events can prevent the event of interest.",
      outputs = if (ko) c("사건 유형별 누적발생함수(CIF)", "집단 간 Gray 검정", "지정 시점의 누적발생률") else c("Cumulative incidence functions by event type", "Gray test between groups", "Cumulative incidence at requested times"),
      alternative = if (ko) "순간적인 원인별 위험의 연관성을 추정하려면 원인별 Cox 회귀분석을 함께 고려합니다." else "Consider cause-specific Cox regression for associations with the instantaneous cause-specific hazard."
    ),
    A03 = list(
      reason = if (ko) "경쟁사건을 별도의 종료로 처리하면서 관심 사건의 원인별 위험과 공변량의 연관성을 추정합니다." else "This estimates covariate associations with the cause-specific hazard while treating competing events as separate endpoints.",
      outputs = if (ko) c("원인별 위험비(HR)와 95% 신뢰구간", "공변량별 회귀계수 및 유의확률", "비례위험 가정 점검") else c("Cause-specific hazard ratios and 95% confidence intervals", "Coefficients and p-values by covariate", "Proportional-hazards checks"),
      alternative = if (ko) "공변량이 실제 누적발생확률에 미치는 효과가 연구 목적이면 Fine–Gray 회귀분석을 선택합니다." else "Choose Fine-Gray regression when the effect on cumulative incidence probability is the target."
    ),
    A04 = list(
      reason = if (ko) "경쟁사건이 있는 상태에서 공변량이 관심 사건의 누적발생확률에 미치는 효과를 추정합니다." else "This estimates covariate effects on cumulative incidence in the presence of competing events.",
      outputs = if (ko) c("부분분포 위험비(sHR)와 95% 신뢰구간", "공변량별 회귀계수 및 유의확률", "관심 사건의 누적발생 해석") else c("Subdistribution hazard ratios and 95% confidence intervals", "Coefficients and p-values by covariate", "Interpretation in terms of cumulative incidence"),
      alternative = if (ko) "병인적 연관성이나 순간적인 원인별 위험이 목적이면 원인별 Cox 회귀분석을 선택합니다." else "Choose cause-specific Cox regression for etiologic or instantaneous-risk questions."
    ),
    A05 = list(
      reason = if (ko) "원인별 위험과 누적발생확률이라는 두 경쟁위험 추정 목표를 모두 보고하려는 설정입니다." else "This addresses both cause-specific hazard and cumulative-incidence estimands.",
      outputs = if (ko) c("원인별 Cox 위험비(HR)", "Fine–Gray 부분분포 위험비(sHR)", "두 추정량의 목적과 해석 차이") else c("Cause-specific Cox hazard ratios", "Fine-Gray subdistribution hazard ratios", "A side-by-side interpretation of both estimands"),
      alternative = if (ko) "주요 연구 질문이 하나로 명확하면 해당 추정량만 선택하여 결과를 단순화할 수 있습니다." else "If one estimand clearly matches the primary question, select it alone for a simpler report."
    ),
    S03 = list(
      reason = if (ko) "대상자별 Start–stop 구간에 따라 공변량 값이 변하는 자료의 연관성 분석에 적합합니다." else "This matches start-stop data in which covariates vary across subject intervals.",
      outputs = if (ko) c("시간의존 공변량의 위험비(HR)", "대상자 군집을 고려한 강건 표준오차", "비례위험 가정 점검") else c("Hazard ratios for time-dependent covariates", "Subject-clustered robust standard errors", "Proportional-hazards checks"),
      alternative = if (ko) "공변량이 시간에 따라 변하지 않는다면 일반 Cox 회귀분석으로 단순화할 수 있습니다." else "Use ordinary Cox regression when covariates do not vary over time."
    )
  )
  explanation <- explanations[[key]] %||% list(
    reason = if (ko) "선택한 연구 목적, 자료 형태 및 사건 구조에 가장 잘 맞는 분석입니다." else "This best matches the selected objective, data shape, and event structure.",
    outputs = character(0),
    alternative = ""
  )
  if (!normalize_app_language(language) %in% c("en", "ko")) {
    explanation <- lapply(explanation, function(values) vapply(values, function(x) {
      if (!nzchar(x)) x else statedu_localized_text(language, x)
    }, character(1), USE.NAMES = FALSE))
  }
  explanation
}

survival_design_recommendation_panel <- function(result, language = statedu_initial_language()) {
  if (is.null(result)) return(NULL)
  ko <- identical(normalize_app_language(language), "ko")
  text <- function(en, kr) statedu_localized_text(language, en, kr)
  audit <- result$preflight %||% NULL
  audit_codes <- if (!is.null(audit) && nrow(audit$issues)) as.character(audit$issues$code) else character(0)
  status_label <- switch(result$status, ready = text("Ready", "실행 가능"), needs_confirmation = text("Confirmation needed", "확인 필요"), blocked = text("Blocked", "실행 전 확인 필요"), unsupported = text("Not supported", "현재 미지원"), result$status)
  details <- c(result$confirmations, result$warnings, setdiff(result$blocked_by, audit_codes))
  details <- survival_recommendation_text(details, language)
  blocked <- identical(result$status, "blocked")
  explanation <- if (identical(result$status, "ready")) survival_recommendation_explanation(result, language) else NULL
  excluded_rows <- if (!is.null(audit)) audit$counts$source_rows - audit$counts$analysis_rows else 0L
  div(class = "result-card survival-design-result-card",
    h3(if (blocked) text("Input check", "입력 확인 결과") else text("Recommendation", "추천 결과")),
    tags$p(tags$strong(paste0(status_label, ": ")), survival_recommendation_text(result$primary, language)),
    if (!is.null(explanation)) div(class = "survival-recommendation-explanation",
      div(class = "survival-recommendation-section", h4(text("Why this analysis", "추천 이유")), tags$p(explanation$reason)),
      if (length(explanation$outputs)) div(class = "survival-recommendation-section", h4(text("Main results", "주요 결과")), tags$ul(lapply(explanation$outputs, tags$li))),
      if (nzchar(explanation$alternative)) div(class = "survival-recommendation-section", h4(text("When to choose the alternative", "대안 선택 기준")), tags$p(explanation$alternative))
    ),
    if (!is.null(audit)) div(class = "survival-recommendation-section survival-recommendation-data",
      h4(text("Data handling summary", "자료 처리 요약")),
      tags$p(if (ko) sprintf("원본 %d행 중 %d행을 분석에 사용하고 %d행을 제외했습니다. 관심 사건은 %d건입니다.", audit$counts$source_rows, audit$counts$analysis_rows, excluded_rows, audit$counts$events) else sprintf(text("Used %d of %d source rows, excluded %d rows, and observed %d events of interest.", ""), audit$counts$analysis_rows, audit$counts$source_rows, excluded_rows, audit$counts$events))
    ),
    if (!is.null(audit) && nrow(audit$issues)) tags$ul(lapply(seq_len(nrow(audit$issues)), function(i) tags$li(
      if (ko) survival_issue_text(audit$issues$code[[i]], audit$issues$message[[i]], language)
      else sprintf("[%s] %s", audit$issues$code[[i]], survival_issue_text(audit$issues$code[[i]], audit$issues$message[[i]], language))
    ))),
    if (length(details)) tags$ul(lapply(details, tags$li)),
    if (length(result$rule_ids) && !blocked) tags$p(class = "survival-recommendation-rule", sprintf(text("Recommendation rule: %s", "추천 규칙: %s"), paste(result$rule_ids, collapse = ", "))),
    if (identical(result$status, "ready")) actionButton("open_recommended_survival_analysis", text("Open recommended analysis", "추천 분석 열기"), class = "btn btn-default")
  )
}

survival_km_tab_panel <- function(language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  tabPanel(
    survival_ui_text("Kaplan-Meier", language),
    value = "analysis_survival_km",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(survival_ui_text("Kaplan-Meier", language)),
        div("Time-to-event curve, log-rank test, median survival, and number at risk.", class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel survival-workspace-panel analysis-three-block-workspace",
        style = "min-width:980px;overflow-x:auto;",
        analysis_workspace_heading(survival_ui_text("Kaplan-Meier", language), "survival_km", language),
        analysis_workspace_body(
          "survival_km",
          uiOutput("survival_km_setup"),
          analysis_three_block_action_row(
            class = "survival-action-row",
            run_button = actionButton("run_survival_km", survival_ui_text("Run Kaplan-Meier", language), class = "btn btn-primary"),
            reset_control = uiOutput("survival_km_reset_control"),
            save_control = uiOutput("survival_km_save_control")
          ),
          uiOutput("survival_km_results")
        )
      )
    )
  )
}

survival_cox_tab_panel <- function(language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  tabPanel(
    survival_ui_text("Cox Regression", language),
    value = "analysis_survival_cox",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(survival_ui_text("Cox Regression", language)),
        div("Hazard ratios with 95% CI and proportional hazards assumption checks.", class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel survival-workspace-panel analysis-three-block-workspace",
        style = "min-width:980px;overflow-x:auto;",
        analysis_workspace_heading(survival_ui_text("Cox Regression", language), "survival_cox", language),
        analysis_workspace_body(
          "survival_cox",
          uiOutput("survival_cox_setup"),
          analysis_three_block_action_row(
            class = "survival-action-row",
            run_button = actionButton("run_survival_cox", survival_ui_text("Run Cox regression", language), class = "btn btn-primary"),
            reset_control = uiOutput("survival_cox_reset_control"),
            save_control = uiOutput("survival_cox_save_control")
          ),
          uiOutput("survival_cox_results")
        )
      )
    )
  )
}

survival_competing_tab_panel <- function(language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  tabPanel(
    survival_ui_text("Competing Risks", language),
    value = "analysis_survival_competing",
    div(
      class = "page-shell",
      div(class = "app-heading",
        h1(survival_ui_text("Competing Risks", language)),
        div("Cumulative incidence functions, Gray tests, and explicit competing-event mapping.", class = "app-subtitle")
      ),
      div(class = "workspace-panel frequencies-workspace-panel survival-workspace-panel analysis-three-block-workspace",
        analysis_workspace_heading(survival_ui_text("Competing Risks", language), "survival_competing", language),
        analysis_workspace_body(
          "survival_competing",
          uiOutput("survival_competing_setup"),
          analysis_three_block_action_row(
            class = "survival-action-row",
            run_button = actionButton("run_survival_competing", survival_ui_text("Run competing risks", language), class = "btn btn-primary"),
            reset_control = uiOutput("survival_competing_reset_control"),
            save_control = uiOutput("survival_competing_save_control")
          ),
          uiOutput("survival_competing_results")
        )
      )
    )
  )
}

survival_competing_setup_panel <- function(selected_names, values = list(), language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  text <- function(en, kr) statedu_localized_text(language, en, kr)
  selected <- survival_selected_names(selected_names)
  choices <- stats::setNames(selected, selected)
  optional_choices <- c(stats::setNames("", survival_ui_text("No group", language)), choices)
  censoring_choices <- c(stats::setNames("", text("None", "사용 안 함")), choices)
  div(
    class = "survival-competing-setup-grid ttest-anova-setup-grid survival-setup-grid analysis-three-block-setup-grid",
    div(class = "analysis-transfer-column analysis-transfer-panel survival-competing-variable-panel",
      h4(text("1. Variables", "1. 변수 지정")),
      selectInput("survival_competing_time", survival_ui_text("Time variable", language), choices = choices, selected = values$time %||% ""),
      selectInput("survival_competing_event", survival_ui_text("Event variable", language), choices = choices, selected = values$event %||% ""),
      selectInput("survival_competing_group", survival_ui_text("Group variable", language), choices = optional_choices, selected = values$group %||% "")
    ),
    div(class = "analysis-transfer-column analysis-transfer-panel survival-competing-event-panel",
      h4(text("2. Event codes & times", "2. 사건코드·시점")),
      textInput("survival_competing_censored_value", survival_ui_text("Censored value", language), value = values$censored_value %||% "0"),
      textInput("survival_competing_interest_value", survival_ui_text("Event of interest", language), value = values$interest_value %||% "1"),
      textInput("survival_competing_event_values", survival_ui_text("Competing event values", language), value = values$competing_values %||% "2", placeholder = "2, 3"),
      textInput("survival_competing_rate_times", survival_ui_text("Time-point rates", language), value = values$rate_times %||% "", placeholder = "12, 36, 60")
    ),
    div(class = "analysis-options-column analysis-options-panel ttest-anova-options-column survival-competing-options-panel",
      h4(text("3. Analysis options", "3. 분석 옵션")),
      selectInput(
        "survival_competing_regression",
        text("Regression estimand", "회귀 추정량"),
        choices = stats::setNames(
          c("none", "cause_specific", "fine_gray", "both"),
          text(c("None", "Cause-specific Cox (HR)", "Fine-Gray (sHR)", "Both estimands"), c("사용 안 함", "원인별 Cox (HR)", "Fine–Gray (sHR)", "두 추정량 모두"))
        ),
        selected = values$regression %||% "none"
      ),
      selectInput("survival_competing_covariates", survival_ui_text("Covariates", language), choices = choices, selected = values$covariates %||% character(0), multiple = TRUE),
      conditionalPanel(
        "input.survival_competing_regression == 'fine_gray' || input.survival_competing_regression == 'both'",
        selectInput(
          "survival_competing_censoring_group",
          text("Censoring-distribution strata (optional)", "검열분포 층화 변수 (선택)"),
          choices = censoring_choices,
          selected = values$censoring_group %||% ""
        ),
        div(class = "result-note", text("Specify only when the design supports different censoring distributions across strata; do not select it from observed p-values.", "집단별 검열분포가 다르다는 설계 근거가 있을 때만 지정합니다. p값에 따라 자동 선택하지 않습니다."))
      ),
      div(class = "result-note", text("Every observed event code must be assigned explicitly. Separate multiple competing-event values with commas.", "모든 사건코드를 명시적으로 지정하세요. 경쟁사건값이 여러 개면 쉼표로 구분합니다."))
    )
  )
}

survival_target_panel <- function(
  title,
  input_id,
  items,
  selected,
  size = 3,
  class = "",
  language = statedu_initial_language(),
  allowed_measurements = analysis_allowed_measurements_all(),
  extra_content = NULL
) {
  language <- normalize_app_language(language)
  div(
    class = paste("analysis-transfer-column analysis-transfer-panel survival-target-panel", class),
    analysis_field_label_tag(title, allowed_measurements, language = language),
    analysis_transfer_listbox_input(input_id, items = items, selected = selected, size = size, important_height = TRUE, min_size = size),
    extra_content
  )
}

survival_event_variable_note <- function(language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  text <- function(en, kr) statedu_localized_text(language, en, kr)
  div(
    class = "result-note survival-event-variable-note",
    text("Binary or categorical event-code variable; mapped to event/non-event for analysis", "이분형 또는 범주형 사건코드 변수 · 분석 시 사건/비사건으로 매핑")
  )
}

survival_km_setup_panel <- function(
  selected_names,
  time = "",
  entry = "",
  event = "",
  group = "",
  event_value = "1",
  rate_times = "",
  rmst_tau = "",
  data_shape = "single_record",
  analysis_method = "km",
  test_method = "logrank",
  output_tables = c("survival_table", "survival_time"),
  plot_types = c("survival", "event", "cumhaz", "log_survival"),
  plot_versions = "color",
  show_ci = TRUE,
  show_censor = TRUE,
  variable_table = NULL,
  labels = character(0),
  selected_available = NULL,
  selected_time = NULL,
  selected_event = NULL,
  selected_group = NULL,
  option_tab = "analysis",
  language = statedu_initial_language()
) {
  language <- normalize_app_language(language)
  text <- function(en, kr) statedu_localized_text(language, en, kr)
  data_shape <- as.character(data_shape %||% "single_record")[[1]]
  if (!data_shape %in% c("single_record", "entry_exit")) data_shape <- "single_record"
  option_tab <- as.character(option_tab %||% "analysis")[[1]]
  if (!option_tab %in% c("analysis", "tables", "plots")) option_tab <- "analysis"
  selected <- survival_selected_names(selected_names)
  assigned <- unique(c(time, entry, event, group))
  assigned <- assigned[nzchar(assigned)]
  available <- setdiff(selected, assigned)
  method_choices <- stats::setNames(c("km", "life_table"), c(
    survival_ui_text("Kaplan-Meier", language),
    survival_ui_text("Life table", language)
  ))
  test_choices <- c("Log-rank" = "logrank", "Breslow" = "breslow", "Tarone-Ware" = "tarone_ware")
  output_choices <- stats::setNames(c("survival_table", "survival_time"), c(
    survival_ui_text("Survival table", language),
    survival_ui_text("Mean / median / quartiles", language)
  ))
  plot_choices <- stats::setNames(c("survival", "event", "cumhaz", "log_survival"), c(
    survival_ui_text("Survival function", language),
    survival_ui_text("1 - Survival function", language),
    survival_ui_text("Cumulative hazard", language),
    survival_ui_text("Log survival", language)
  ))
  plot_version_choices <- stats::setNames(c("color", "bw"), c(
    survival_ui_text("Color", language),
    survival_ui_text("Black and white", language)
  ))
  km_shape_choices <- stats::setNames(
    c("single_record", "entry_exit"),
    text(c("Standard Kaplan-Meier", "Delayed-entry Kaplan-Meier"), c("일반 Kaplan–Meier", "지연 진입 Kaplan–Meier"))
  )
  optional_entry_choices <- c(stats::setNames("", text("None", "선택 안 함")), stats::setNames(selected, selected))
  selected_entry <- survival_selected_names(entry)
  selected_entry <- if (identical(data_shape, "entry_exit") && length(selected_entry)) selected_entry[[1]] else ""
  div(
    class = "ttest-anova-setup-grid survival-setup-grid analysis-three-block-setup-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel survival-available-panel",
      analysis_field_label_tag("Variables", language = language),
      analysis_transfer_listbox_input(
        "survival_km_available",
        analysis_variable_items(available, variable_table, labels),
        selected = selected_order_items(selected_available, available),
        size = 17
      )
    ),
    div(
      class = "analysis-transfer-controls ttest-anova-transfer-controls survival-transfer-controls",
      actionButton("survival_km_time_move", ">", class = "btn btn-default analysis-move-button"),
      actionButton("survival_km_event_move", ">", class = "btn btn-default analysis-move-button"),
      actionButton("survival_km_group_move", ">", class = "btn btn-default analysis-move-button")
    ),
    div(
      class = "ttest-anova-target-column survival-target-column",
      survival_target_panel(
        survival_ui_text("Time variable", language),
        "survival_km_time",
        analysis_variable_items(time, variable_table, labels),
        selected_order_items(selected_time, time),
        size = 1,
        class = "survival-time-panel",
        language = language,
        allowed_measurements = c("ordered", "continuous"),
        extra_content = div(
          class = "survival-target-setting",
          textInput("survival_km_rate_times", survival_ui_text("Time-point rates", language), value = rate_times, placeholder = "12, 36, 60")
        )
      ),
      survival_target_panel(
        survival_ui_text("Event variable", language),
        "survival_km_event",
        analysis_variable_items(event, variable_table, labels),
        selected_order_items(selected_event, event),
        size = 1,
        class = "survival-event-panel",
        language = language,
        allowed_measurements = c("binary", "category"),
        extra_content = div(
          class = "survival-target-setting",
          textInput("survival_km_event_value", survival_ui_text("Event value", language), value = event_value, placeholder = survival_ui_text("Event value placeholder", language)),
          survival_event_variable_note(language)
        )
      ),
      survival_target_panel(
        survival_ui_text("Group variable", language),
        "survival_km_group",
        analysis_variable_items(group, variable_table, labels),
        selected_order_items(selected_group, group),
        size = 8,
        class = "survival-group-panel",
        language = language,
        allowed_measurements = c("binary", "category", "ordered")
      )
    ),
    div(
      class = "ttest-anova-options-column analysis-options-panel survival-options-column",
      tabsetPanel(
        id = "survival_km_option_tabs",
        type = "tabs",
        selected = option_tab,
        tabPanel(
          survival_ui_text("Analysis", language),
          value = "analysis",
          div(
            class = "survival-options-tab-content",
            div(class = "analysis-option-group",
              div(class = "analysis-option-title", text("Data structure", "자료 구조")),
              selectInput("survival_km_data_shape", NULL, choices = km_shape_choices, selected = data_shape),
              conditionalPanel(
                "input.survival_km_data_shape == 'entry_exit'",
                selectInput("survival_km_entry", text("Entry time", "진입 시간"), choices = optional_entry_choices, selected = selected_entry),
                div(class = "result-note", text("Entry time is when each subject joins the risk set and must be earlier than exit time.", "진입 시간은 각 대상자가 위험집단에 들어온 시점이며 종료 시간보다 빨라야 합니다."))
              )
            ),
            div(class = "analysis-option-group",
              div(class = "analysis-option-title", survival_ui_text("Analysis method", language)),
              selectInput("survival_km_analysis_method", NULL, choices = method_choices, selected = analysis_method)
            ),
            div(class = "analysis-option-group",
              div(class = "analysis-option-title", survival_ui_text("Group comparison", language)),
              selectInput("survival_km_test_method", NULL, choices = test_choices, selected = test_method),
              textInput("survival_km_rmst_tau", survival_ui_text("RMST tau", language), value = rmst_tau, placeholder = text("e.g., 365", "예: 365"))
            )
          )
        ),
        tabPanel(
          survival_ui_text("Tables", language),
          value = "tables",
          div(
            class = "survival-options-tab-content",
            div(class = "analysis-option-group",
              div(class = "analysis-option-title", analysis_ui_text("Output", language)),
              checkboxGroupInput("survival_km_output_tables", NULL, choices = output_choices, selected = output_tables)
            )
          )
        ),
        tabPanel(
          survival_ui_text("Plots", language),
          value = "plots",
          div(
            class = "survival-options-tab-content",
            div(class = "analysis-option-group",
              div(class = "analysis-option-title", survival_ui_text("Plot type", language)),
              checkboxGroupInput("survival_km_plot_types", NULL, choices = plot_choices, selected = plot_types)
            ),
            div(class = "analysis-option-group",
              div(class = "analysis-option-title", survival_ui_text("Plot version", language)),
              checkboxGroupInput("survival_km_plot_versions", NULL, choices = plot_version_choices, selected = plot_versions)
            ),
            div(class = "analysis-option-group",
              div(class = "analysis-option-title", survival_ui_text("Display", language)),
              checkboxInput("survival_km_show_ci", survival_ui_text("Show confidence interval", language), value = isTRUE(show_ci)),
              checkboxInput("survival_km_show_censor", survival_ui_text("Show censor marks", language), value = isTRUE(show_censor))
            )
          )
        )
      )
    )
  )
}

survival_cox_setup_panel <- function(
  selected_names,
  time = "",
  entry = "",
  start = "",
  stop = "",
  subject_id = "",
  strata = "",
  cluster = "",
  spline_covariate = "",
  spline_df = 4L,
  time_varying_covariate = "",
  time_varying_times = "",
  ties_method = "efron",
  event = "",
  covariates = character(0),
  event_value = "1",
  adjusted_group = "",
  adjusted_bootstrap_reps = 2000L,
  adjusted_times = "",
  data_shape = "single_record",
  variable_table = NULL,
  labels = character(0),
  selected_available = NULL,
  selected_time = NULL,
  selected_event = NULL,
  selected_covariates = NULL,
  language = statedu_initial_language()
) {
  language <- normalize_app_language(language)
  text <- function(en, kr) statedu_localized_text(language, en, kr)
  data_shape <- as.character(data_shape %||% "single_record")[[1]]
  if (!data_shape %in% c("single_record", "entry_exit", "start_stop")) data_shape <- "single_record"
  selected <- survival_selected_names(selected_names)
  assigned <- unique(c(time, entry, start, stop, subject_id, strata, cluster, event, covariates))
  assigned <- assigned[nzchar(assigned)]
  available <- setdiff(selected, assigned)
  adjusted_choices <- c(stats::setNames("", text("None", "선택 안 함")), stats::setNames(covariates, covariates))
  spline_choices <- c(stats::setNames("", text("None", "선택 안 함")), stats::setNames(covariates, covariates))
  time_varying_choices <- c(stats::setNames("", text("None", "선택 안 함")), stats::setNames(covariates, covariates))
  optional_choices <- c(stats::setNames("", text("None", "선택 안 함")), stats::setNames(selected, selected))
  optional_selected <- function(value) {
    value <- survival_selected_names(value)
    if (length(value)) value[[1]] else ""
  }
  shape_choices <- stats::setNames(
    c("single_record", "entry_exit", "start_stop"),
    text(c("Standard Cox", "Delayed-entry Cox", "Time-dependent Cox"), c("일반 Cox", "지연 진입 Cox", "시간의존 Cox"))
  )
  div(
    class = "ttest-anova-setup-grid survival-setup-grid analysis-three-block-setup-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel survival-available-panel",
      analysis_field_label_tag("Variables", language = language),
      analysis_transfer_listbox_input(
        "survival_cox_available",
        analysis_variable_items(available, variable_table, labels),
        selected = selected_order_items(selected_available, available),
        size = 17
      )
    ),
    div(
      class = "analysis-transfer-controls ttest-anova-transfer-controls survival-transfer-controls survival-cox-transfer-controls",
      actionButton("survival_cox_time_move", ">", class = "btn btn-default analysis-move-button"),
      actionButton("survival_cox_event_move", ">", class = "btn btn-default analysis-move-button"),
      actionButton("survival_cox_covariates_move", ">", class = "btn btn-default analysis-move-button")
    ),
    div(
      class = "ttest-anova-target-column survival-target-column survival-cox-target-column",
      survival_target_panel(
        survival_ui_text("Time variable", language),
        "survival_cox_time",
        analysis_variable_items(time, variable_table, labels),
        selected_order_items(selected_time, time),
        size = 1,
        class = "survival-time-panel",
        language = language,
        allowed_measurements = c("ordered", "continuous")
      ),
      survival_target_panel(
        survival_ui_text("Event variable", language),
        "survival_cox_event",
        analysis_variable_items(event, variable_table, labels),
        selected_order_items(selected_event, event),
        size = 1,
        class = "survival-event-panel",
        language = language,
        allowed_measurements = c("binary", "category"),
        extra_content = div(
          class = "survival-target-setting",
          textInput("survival_cox_event_value", survival_ui_text("Event value", language), value = event_value, placeholder = survival_ui_text("Event value placeholder", language)),
          survival_event_variable_note(language)
        )
      ),
      survival_target_panel(
        survival_ui_text("Covariates", language),
        "survival_cox_covariates",
        analysis_variable_items(covariates, variable_table, labels),
        selected_order_items(selected_covariates, covariates),
        size = 8,
        class = "survival-covariates-panel",
        language = language,
        allowed_measurements = analysis_allowed_measurements_all()
      )
    ),
    div(
      class = "ttest-anova-options-column analysis-options-panel survival-options-column",
      tabsetPanel(
        id = "survival_cox_option_tabs",
        type = "tabs",
        tabPanel(
          text("Data structure", "자료 구조"),
          value = "structure",
          div(class = "survival-options-tab-content",
            selectInput("survival_cox_data_shape", text("Cox data structure", "Cox 자료 구조"), choices = shape_choices, selected = data_shape),
            conditionalPanel(
              "input.survival_cox_data_shape == 'entry_exit'",
              selectInput("survival_cox_entry", text("Entry time", "진입 시간"), choices = optional_choices, selected = optional_selected(entry))
            ),
            conditionalPanel(
              "input.survival_cox_data_shape == 'start_stop'",
              selectInput("survival_cox_start", text("Interval start", "구간 시작"), choices = optional_choices, selected = optional_selected(start)),
              selectInput("survival_cox_stop", text("Interval stop", "구간 종료"), choices = optional_choices, selected = optional_selected(stop)),
              selectInput("survival_cox_subject_id", text("Subject ID", "대상자 ID"), choices = optional_choices, selected = optional_selected(subject_id))
            ),
            div(class = "result-note", text("Use standard Cox for one row per subject, delayed-entry Cox for entry/exit times, and time-dependent Cox for start-stop intervals.", "일반 Cox는 대상자당 한 행, 지연 진입 Cox는 진입·종료 시간, 시간의존 Cox는 Start–stop 구간 자료에 사용합니다."))
          )
        ),
        tabPanel(
          text("Analysis options", "분석 옵션"),
          value = "analysis",
          div(class = "survival-options-tab-content",
            div(class = "analysis-option-group",
              div(class = "analysis-option-title", survival_ui_text("PH assumption", language)),
              div(class = "analysis-option-subtitle", text("Schoenfeld residual test", "Schoenfeld 잔차 검정"))
            ),
            div(class = "analysis-option-group",
              div(class = "analysis-option-title", text("Tied-event handling", "동률 사건 처리")),
              selectInput(
                "survival_cox_ties_method",
                text("Partial-likelihood method", "부분우도 방식"),
                choices = stats::setNames(c("efron", "breslow", "exact"), c(text("Efron (default)", "Efron (기본값)"), "Breslow", text("Exact", "정확법"))),
                selected = if (ties_method %in% c("efron", "breslow", "exact")) ties_method else "efron"
              ),
              div(class = "result-note", text("Efron is the default. Use exact partial likelihood cautiously for small discrete-time datasets with substantial ties.", "Efron이 기본값입니다. Exact는 소규모 이산시간 자료의 많은 동률에만 신중히 사용하세요."))
            ),
            div(class = "analysis-option-group",
              div(class = "analysis-option-title", text("Stratified Cox", "층화 Cox")),
              selectInput(
                "survival_cox_strata",
                text("Stratification variable", "층화 변수"),
                choices = optional_choices,
                selected = optional_selected(strata)
              ),
              div(class = "result-note", text("Allows a separate baseline hazard in each stratum; no hazard ratio is estimated for the stratification variable itself.", "층마다 서로 다른 기저위험을 허용하며, 층화 변수 자체의 HR은 추정하지 않습니다."))
            ),
            conditionalPanel(
              "input.survival_cox_data_shape != 'start_stop'",
              div(class = "analysis-option-group",
                div(class = "analysis-option-title", text("Cluster-robust variance", "군집-강건 분산")),
                selectInput(
                  "survival_cox_cluster",
                  text("Cluster ID variable", "군집 ID 변수"),
                  choices = optional_choices,
                  selected = optional_selected(cluster)
                ),
                div(class = "result-note", text("Uses sandwich standard errors for within-cluster correlation (for example, hospitals, schools, or households). Few clusters require additional review.", "병원·학교·가구 등 군집 내 상관을 sandwich 표준오차로 반영합니다. 적은 군집 수에서는 별도 검토가 필요합니다."))
              )
            ),
            div(class = "analysis-option-group",
              div(class = "analysis-option-title", text("Continuous nonlinear effect", "연속형 비선형 효과")),
              selectInput(
                "survival_cox_spline_covariate",
                text("Natural cubic spline covariate", "Natural cubic spline 공변량"),
                choices = spline_choices,
                selected = if (spline_covariate %in% covariates) spline_covariate else ""
              ),
              numericInput(
                "survival_cox_spline_df",
                text("Spline degrees of freedom", "Spline 자유도"),
                value = as.integer(spline_df %||% 4L),
                min = 3,
                max = 5,
                step = 1
              ),
              div(class = "result-note", text("Compares a linear term with a natural cubic spline and reports an HR curve relative to the median. Use for a prespecified covariate.", "선형항과 natural cubic spline 모형을 비교하고 중앙값 대비 HR 곡선을 제공합니다. 사전에 계획한 공변량에 사용하세요."))
            ),
            div(class = "analysis-option-group",
              div(class = "analysis-option-title", text("Time-varying coefficient", "시간가변 계수")),
              selectInput(
                "survival_cox_time_varying_covariate",
                text("Covariate with time-varying HR", "시간에 따라 HR이 변하는 공변량"),
                choices = time_varying_choices,
                selected = if (time_varying_covariate %in% covariates) time_varying_covariate else ""
              ),
              textInput(
                "survival_cox_time_varying_times",
                text("HR reporting times", "HR 보고 시점"),
                value = time_varying_times,
                placeholder = text("e.g., 1, 5, 10 (blank uses event-time quartiles)", "예: 1, 5, 10 (공란이면 사건시점 사분위수)")
              ),
              div(class = "result-note", text("Fits covariate × log(1 + time) through coxph tt() and reports time-specific HRs instead of one constant HR.", "coxph tt()로 공변량 × log(1 + time)을 적합하며 단일 HR 대신 시점별 HR을 보고합니다."))
            ),
            div(class = "analysis-option-group",
              div(class = "analysis-option-title", survival_ui_text("Adjusted survival group", language)),
              selectInput("survival_cox_adjusted_group", NULL, choices = adjusted_choices, selected = adjusted_group),
              textInput(
                "survival_cox_adjusted_times",
                text("Adjusted-survival reporting times", "보정 생존확률 보고 시점"),
                value = adjusted_times,
                placeholder = text("e.g., 12, 36, 60", "예: 12, 36, 60")
              ),
              numericInput("survival_cox_adjusted_bootstrap_reps", survival_ui_text("Bootstrap repetitions", language), value = adjusted_bootstrap_reps, min = 200, max = 5000, step = 200),
              div(class = "result-note", text("Reports group-specific adjusted survival and pairwise differences/ratios at the specified times with pointwise percentile-bootstrap 95% confidence intervals.", "지정 시점의 집단별 보정 생존확률과 집단 간 차이·비율을 점별 percentile bootstrap 95% 신뢰구간과 함께 보고합니다."))
            )
          )
        )
      )
    )
  )
}
