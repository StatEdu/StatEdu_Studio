# Survival analysis core functions.

survival_ui_text <- function(text, language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  key <- tolower(trimws(as.character(text %||% "")))
  labels <- list(
    "survival analysis" = c(en = "Survival Analysis", ko = "생존분석"),
    "kaplan-meier" = c(en = "Kaplan-Meier", ko = "Kaplan-Meier"),
    "cox regression" = c(en = "Cox Regression", ko = "Cox 회귀분석"),
    "time variable" = c(en = "Time variable", ko = "생존시간 변수"),
    "event variable" = c(en = "Event variable", ko = "사건 변수"),
    "event value" = c(en = "Event value", ko = "사건값"),
    "group variable" = c(en = "Group variable", ko = "집단 변수"),
    "no group" = c(en = "No group", ko = "집단 없음"),
    "covariates" = c(en = "Covariates", ko = "공변량"),
    "time-point rates" = c(en = "Time-point rates", ko = "시점별 생존율"),
    "show confidence interval" = c(en = "Show confidence interval", ko = "신뢰구간 표시"),
    "show censor marks" = c(en = "Show censor marks", ko = "검열 표시"),
    "run kaplan-meier" = c(en = "Run Kaplan-Meier", ko = "Kaplan-Meier 실행"),
    "run cox regression" = c(en = "Run Cox regression", ko = "Cox 회귀분석 실행"),
    "ph assumption" = c(en = "PH assumption", ko = "PH 가정"),
    "number at risk" = c(en = "Number at risk", ko = "Number at risk"),
    "median survival" = c(en = "Median survival", ko = "중앙 생존시간"),
    "log-rank test" = c(en = "Log-rank test", ko = "Log-rank 검정"),
    "model overview" = c(en = "Model overview", ko = "모형 요약"),
    "analysis" = c(en = "Analysis", ko = "분석"),
    "tables" = c(en = "Tables", ko = "표"),
    "plots" = c(en = "Plots", ko = "도표"),
    "analysis method" = c(en = "Analysis method", ko = "분석 방법"),
    "life table" = c(en = "Life table", ko = "생명표분석"),
    "group comparison" = c(en = "Group comparison", ko = "집단 비교"),
    "survival table" = c(en = "Survival table", ko = "생존표"),
    "mean / median / quartiles" = c(en = "Mean / median / quartiles", ko = "평균 / 중위수 / 사분위수"),
    "plot type" = c(en = "Plot type", ko = "도표 종류"),
    "plot version" = c(en = "Plot version", ko = "도표 버전"),
    "display" = c(en = "Display", ko = "표시"),
    "color" = c(en = "Color", ko = "컬러"),
    "black and white" = c(en = "Black and white", ko = "흑백"),
    "survival function" = c(en = "Survival function", ko = "생존함수"),
    "1 - survival function" = c(en = "1 - Survival function", ko = "1 - 생존함수"),
    "cumulative hazard" = c(en = "Cumulative hazard", ko = "누적위험함수"),
    "log survival" = c(en = "Log survival", ko = "로그 생존함수"),
    "kaplan-meier survival time summary" = c(en = "Kaplan-Meier survival time summary", ko = "Kaplan-Meier 생존시간 요약"),
    "group comparison test" = c(en = "Group comparison test", ko = "집단 비교 검정"),
    "post-hoc pairwise comparison" = c(en = "Post-hoc pairwise comparison", ko = "사후 쌍별 비교"),
    "complete step 2 in the data tab before setting up survival analysis." = c(
      en = "Complete Step 2 in the Data tab before setting up survival analysis.",
      ko = "생존분석 설정 전에 데이터 탭의 Step 2를 완료하세요."
    )
  )
  value <- labels[[key]]
  if (is.null(value)) return(as.character(text))
  value[[language]] %||% value[["en"]]
}

survival_selected_names <- function(selected_names) {
  values <- as.character(selected_names %||% character(0))
  values[!is.na(values) & nzchar(values)]
}

survival_variable_choices <- function(selected_names, include_none = FALSE, language = statedu_initial_language()) {
  names <- survival_selected_names(selected_names)
  choices <- stats::setNames(names, names)
  if (isTRUE(include_none)) {
    choices <- c(stats::setNames("", survival_ui_text("No group", language)), choices)
  }
  choices
}

survival_parse_event <- function(values, event_value = "1") {
  values_chr <- trimws(as.character(values))
  event_chr <- trimws(as.character(event_value %||% "1")[[1]])
  if (!nzchar(event_chr)) {
    numeric_values <- suppressWarnings(as.numeric(values_chr))
    return(!is.na(numeric_values) & numeric_values > 0)
  }
  values_chr == event_chr
}

survival_analysis_data <- function(data, time, event, event_value = "1", group = "", covariates = character(0), variable_info = NULL) {
  if (!is.data.frame(data)) stop("No data is loaded.")
  time <- as.character(time %||% character(0))
  event <- as.character(event %||% character(0))
  group <- as.character(group %||% character(0))
  covariates <- as.character(covariates %||% character(0))
  time <- time[!is.na(time) & nzchar(time)]
  event <- event[!is.na(event) & nzchar(event)]
  group <- group[!is.na(group) & nzchar(group)]
  covariates <- covariates[!is.na(covariates) & nzchar(covariates)]
  if (length(time) == 0) stop("Select a time variable.")
  if (length(event) == 0) stop("Select an event variable.")
  time <- time[[1]]
  event <- event[[1]]
  group <- if (length(group) > 0) group[[1]] else ""
  variables <- unique(c(time, event, group, covariates))
  variables <- variables[nzchar(as.character(variables))]
  missing <- setdiff(variables, names(data))
  if (length(missing) > 0) {
    stop("Variables not found: ", paste(missing, collapse = ", "))
  }
  model_data <- data[, variables, drop = FALSE]
  model_data[[time]] <- suppressWarnings(as.numeric(model_data[[time]]))
  model_data[[event]] <- survival_parse_event(model_data[[event]], event_value)
  if (nzchar(as.character(group %||% ""))) {
    model_data[[group]] <- as.factor(model_data[[group]])
  }
  measurements <- character(0)
  if (is.data.frame(variable_info) && all(c("name", "measurement") %in% names(variable_info))) {
    measurements <- stats::setNames(tolower(as.character(variable_info$measurement)), as.character(variable_info$name))
  }
  for (covariate in covariates) {
    measurement <- named_value(measurements, covariate, "")
    if (measurement %in% c("binary", "category", "ordered", "ordinal", "nominal")) {
      model_data[[covariate]] <- as.factor(model_data[[covariate]])
    }
  }
  complete <- stats::complete.cases(model_data)
  model_data <- model_data[complete, , drop = FALSE]
  model_data <- model_data[!is.na(model_data[[time]]) & model_data[[time]] >= 0, , drop = FALSE]
  if (nrow(model_data) == 0) stop("No complete rows remain after applying survival variables.")
  if (!any(model_data[[event]], na.rm = TRUE)) stop("No events were found for the selected event value.")
  model_data
}

survival_display_name <- function(name, variable_table = NULL, labels = character(0)) {
  display_variable_name_static(name, variable_table, labels, label_only = TRUE)
}

survival_format_number <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0) return("")
  x <- x[[1]]
  if (is.na(x)) return("")
  if (!is.finite(x)) return(as.character(x))
  format_decimal3(x)
}

survival_format_ci <- function(lower, upper) {
  lower <- suppressWarnings(as.numeric(lower))
  upper <- suppressWarnings(as.numeric(upper))
  if ((length(lower) == 0 || is.na(lower)) && (length(upper) == 0 || is.na(upper))) return("")
  lower_text <- if (length(lower) == 0 || is.na(lower)) "NE" else survival_format_number(lower)
  upper_text <- if (length(upper) == 0 || is.na(upper)) "NE" else survival_format_number(upper)
  sprintf("(%s-%s)", lower_text, upper_text)
}

survival_p <- function(p) {
  if (length(p) == 0 || is.na(p)) "" else format_p(p)
}

survival_parse_times <- function(value) {
  raw <- unlist(strsplit(as.character(value %||% ""), "[,;[:space:]]+"))
  times <- suppressWarnings(as.numeric(raw))
  sort(unique(times[!is.na(times) & times >= 0]))
}

survival_default_risk_times <- function(time) {
  max_time <- suppressWarnings(max(as.numeric(time), na.rm = TRUE))
  if (!is.finite(max_time) || max_time <= 0) return(numeric(0))
  pretty(c(0, max_time), n = 5)
}

survival_fit_label <- function(fit) {
  paste(capture.output(print(fit$call)), collapse = " ")
}

survival_km_test_label <- function(method) {
  method <- as.character(method %||% "logrank")[[1]]
  switch(method,
    breslow = "Breslow",
    tarone_ware = "Tarone-Ware",
    "Log-rank"
  )
}

survival_weighted_rank_test <- function(data, time, event, group, method = "logrank") {
  if (!nzchar(as.character(group %||% ""))) return(NULL)
  group_values <- droplevels(as.factor(data[[group]]))
  levels <- levels(group_values)
  if (length(levels) < 2L) return(NULL)
  times <- sort(unique(data[[time]][data[[event]]]))
  if (length(times) == 0) return(NULL)
  k <- length(levels)
  observed_minus_expected <- numeric(k)
  variance <- matrix(0, nrow = k, ncol = k)
  for (event_time in times) {
    risk <- data[[time]] >= event_time
    event_at_time <- data[[time]] == event_time & data[[event]]
    n_total <- sum(risk)
    d_total <- sum(event_at_time)
    if (n_total <= 1 || d_total <= 0) next
    n_group <- as.numeric(table(factor(group_values[risk], levels = levels)))
    d_group <- as.numeric(table(factor(group_values[event_at_time], levels = levels)))
    expected <- d_total * n_group / n_total
    weight <- switch(as.character(method %||% "logrank")[[1]],
      breslow = n_total,
      tarone_ware = sqrt(n_total),
      1
    )
    observed_minus_expected <- observed_minus_expected + weight * (d_group - expected)
    for (i in seq_len(k)) {
      for (j in seq_len(k)) {
        value <- if (i == j) {
          d_total * (n_total - d_total) * n_group[[i]] * (n_total - n_group[[i]]) / (n_total^2 * (n_total - 1))
        } else {
          -d_total * (n_total - d_total) * n_group[[i]] * n_group[[j]] / (n_total^2 * (n_total - 1))
        }
        variance[i, j] <- variance[i, j] + weight^2 * value
      }
    }
  }
  keep <- seq_len(k - 1L)
  chisq <- tryCatch(
    as.numeric(t(observed_minus_expected[keep]) %*% qr.solve(variance[keep, keep, drop = FALSE], observed_minus_expected[keep])),
    error = function(e) NA_real_
  )
  if (!is.finite(chisq)) return(NULL)
  list(
    method = as.character(method %||% "logrank")[[1]],
    label = survival_km_test_label(method),
    chisq = chisq,
    df = k - 1L,
    p = stats::pchisq(chisq, df = k - 1L, lower.tail = FALSE)
  )
}

survival_life_table <- function(data, time, event, group = "", breaks = numeric(0)) {
  max_time <- suppressWarnings(max(data[[time]], na.rm = TRUE))
  if (!is.finite(max_time) || max_time <= 0) return(data.frame())
  breaks <- sort(unique(suppressWarnings(as.numeric(breaks))))
  breaks <- breaks[is.finite(breaks) & breaks > 0 & breaks < max_time]
  breaks <- unique(c(0, breaks, max_time))
  if (length(breaks) < 2L) breaks <- pretty(c(0, max_time), n = 5)
  groups <- if (nzchar(as.character(group %||% ""))) levels(droplevels(as.factor(data[[group]]))) else "All"
  rows <- list()
  for (group_value in groups) {
    group_data <- if (identical(group_value, "All")) data else data[as.factor(data[[group]]) == group_value, , drop = FALSE]
    survival_value <- 1
    for (index in seq_len(length(breaks) - 1L)) {
      start <- breaks[[index]]
      end <- breaks[[index + 1L]]
      in_interval <- group_data[[time]] > start & group_data[[time]] <= end
      at_risk <- sum(group_data[[time]] > start)
      events <- sum(in_interval & group_data[[event]], na.rm = TRUE)
      censored <- sum(in_interval & !group_data[[event]], na.rm = TRUE)
      effective <- at_risk - censored / 2
      q <- if (effective > 0) events / effective else NA_real_
      p <- if (is.finite(q)) max(0, 1 - q) else NA_real_
      if (is.finite(p)) survival_value <- survival_value * p
      rows[[length(rows) + 1L]] <- data.frame(
        Strata = group_value,
        Interval = sprintf("%s-%s", survival_format_number(start), survival_format_number(end)),
        `At risk` = at_risk,
        Events = events,
        Censored = censored,
        Survival = survival_value,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

survival_km_plot_transform <- function(values, plot_type = "survival", lower = NULL, upper = NULL) {
  plot_type <- as.character(plot_type %||% "survival")[[1]]
  eps <- .Machine$double.eps
  values <- pmin(pmax(as.numeric(values), eps), 1)
  if (identical(plot_type, "event")) {
    return(1 - values)
  }
  if (identical(plot_type, "cumhaz")) {
    return(-log(values))
  }
  if (identical(plot_type, "log_survival")) {
    return(log(values))
  }
  values
}

survival_km_plot_ci <- function(lower, upper, plot_type = "survival") {
  lower <- pmin(pmax(as.numeric(lower), .Machine$double.eps), 1)
  upper <- pmin(pmax(as.numeric(upper), .Machine$double.eps), 1)
  if (identical(plot_type, "event")) {
    return(list(lower = 1 - upper, upper = 1 - lower))
  }
  if (identical(plot_type, "cumhaz")) {
    return(list(lower = -log(upper), upper = -log(lower)))
  }
  if (identical(plot_type, "log_survival")) {
    return(list(lower = log(lower), upper = log(upper)))
  }
  list(lower = lower, upper = upper)
}

survival_km_plot_data <- function(fit, plot_type = "survival") {
  curve_summary <- summary(fit)
  strata <- as.character(curve_summary$strata %||% "All")
  if (length(strata) == 0) strata <- rep("All", length(curve_summary$time))
  curve <- data.frame(
    Time = as.numeric(curve_summary$time),
    Survival = as.numeric(curve_summary$surv),
    Lower = as.numeric(curve_summary$lower),
    Upper = as.numeric(curve_summary$upper),
    Strata = strata,
    stringsAsFactors = FALSE
  )
  if (nrow(curve) > 0) {
    zero_rows <- do.call(rbind, lapply(unique(curve$Strata), function(stratum) {
      data.frame(Time = 0, Survival = 1, Lower = 1, Upper = 1, Strata = stratum, stringsAsFactors = FALSE)
    }))
    curve <- rbind(zero_rows, curve)
  }
  ci <- survival_km_plot_ci(curve$Lower, curve$Upper, plot_type)
  curve$Value <- survival_km_plot_transform(curve$Survival, plot_type)
  curve$LowerValue <- ci$lower
  curve$UpperValue <- ci$upper

  censor_summary <- summary(fit, censored = TRUE)
  censor_strata <- as.character(censor_summary$strata %||% "All")
  if (length(censor_strata) == 0) censor_strata <- rep("All", length(censor_summary$time))
  censor <- data.frame(
    Time = as.numeric(censor_summary$time),
    Survival = as.numeric(censor_summary$surv),
    Censored = as.numeric(censor_summary$n.censor %||% rep(0, length(censor_summary$time))),
    Strata = censor_strata,
    stringsAsFactors = FALSE
  )
  censor <- censor[censor$Censored > 0, , drop = FALSE]
  if (nrow(censor) > 0) {
    censor$Value <- survival_km_plot_transform(censor$Survival, plot_type)
  }
  list(curve = curve, censor = censor)
}

survival_publication_palette <- function(n) {
  base <- c("#1F77B4", "#D62728", "#2CA02C", "#9467BD", "#FF7F0E", "#17BECF", "#8C564B", "#111111")
  rep(base, length.out = max(1L, n))
}

survival_bw_palette <- function(n) {
  if (n <= 1L) return("#111111")
  grDevices::gray.colors(n, start = 0.05, end = 0.65)
}

survival_bw_linetypes <- function(n) {
  base <- c("solid", "22", "42", "44", "13", "1343", "73", "2262")
  rep(base, length.out = max(1L, n))
}

survival_km_ggplot <- function(result, plot_type = "survival", figure_version = "color") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for survival plots.")
  }
  plot_type <- as.character(plot_type %||% "survival")[[1]]
  figure_version <- as.character(figure_version %||% "color")[[1]]
  if (!figure_version %in% c("color", "bw")) figure_version <- "color"
  plot_data <- survival_km_plot_data(result$fit, plot_type)
  curve <- plot_data$curve
  censor <- plot_data$censor
  strata_count <- length(unique(curve$Strata))
  palette <- if (identical(figure_version, "bw")) survival_bw_palette(strata_count) else survival_publication_palette(strata_count)
  y_label <- switch(plot_type,
    event = "1 - Survival probability",
    cumhaz = "Cumulative hazard",
    log_survival = "Log survival probability",
    "Survival probability"
  )
  p <- ggplot2::ggplot(curve, ggplot2::aes(x = Time, y = Value, color = Strata, group = Strata))
  if (identical(figure_version, "bw")) {
    p <- p +
      ggplot2::geom_step(ggplot2::aes(linetype = Strata), linewidth = 0.95) +
      ggplot2::scale_linetype_manual(values = survival_bw_linetypes(strata_count), drop = FALSE)
  } else {
    p <- p + ggplot2::geom_step(linewidth = 0.95)
  }
  p <- p +
    ggplot2::labs(x = result$time, y = y_label, color = NULL, linetype = NULL, shape = NULL) +
    ggplot2::scale_color_manual(values = palette, drop = FALSE) +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(
      axis.line = ggplot2::element_line(linewidth = 0.45, colour = "#111111"),
      axis.ticks = ggplot2::element_line(linewidth = 0.45, colour = "#111111"),
      axis.ticks.length = grid::unit(3, "pt"),
      axis.title = ggplot2::element_text(size = 12, colour = "#111111"),
      axis.text = ggplot2::element_text(size = 10.5, colour = "#111111"),
      legend.position = if (strata_count > 1L) "bottom" else "none",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.key.width = grid::unit(22, "pt"),
      legend.key.height = grid::unit(12, "pt"),
      legend.text = ggplot2::element_text(size = 10.5),
      legend.margin = ggplot2::margin(2, 0, 0, 0),
      plot.margin = ggplot2::margin(8, 10, 8, 8)
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(override.aes = list(linewidth = 1.2))
    )
  if (isTRUE(result$show_ci) && all(c("LowerValue", "UpperValue") %in% names(curve))) {
    p <- p +
      ggplot2::geom_step(ggplot2::aes(y = LowerValue), linewidth = 0.28, linetype = "22", alpha = 0.45) +
      ggplot2::geom_step(ggplot2::aes(y = UpperValue), linewidth = 0.28, linetype = "22", alpha = 0.45)
  }
  if (isTRUE(result$show_censor) && nrow(censor) > 0) {
    if (identical(figure_version, "bw")) {
      p <- p +
        ggplot2::geom_point(
          data = censor,
          ggplot2::aes(x = Time, y = Value, color = Strata, shape = Strata),
          size = 1.45,
          stroke = 0.55,
          inherit.aes = FALSE
        ) +
        ggplot2::scale_shape_manual(values = rep(c(3, 4, 8, 1, 2, 5, 6, 7), length.out = strata_count), drop = FALSE)
    } else {
      p <- p +
        ggplot2::geom_point(
          data = censor,
          ggplot2::aes(x = Time, y = Value, color = Strata),
          shape = 3,
          size = 1.45,
          stroke = 0.55,
          inherit.aes = FALSE
        )
    }
  }
  if (plot_type %in% c("survival", "event")) {
    p <- p + ggplot2::coord_cartesian(ylim = c(0, 1))
  }
  p
}

survival_km_posthoc_table <- function(data, time, event, group, formula, test_method = "logrank") {
  if (!nzchar(as.character(group %||% ""))) return(data.frame())
  levels <- levels(droplevels(as.factor(data[[group]])))
  if (length(levels) < 3L) return(data.frame())
  pairs <- utils::combn(levels, 2, simplify = FALSE)
  rows <- lapply(pairs, function(pair) {
    subset_data <- data[data[[group]] %in% pair, , drop = FALSE]
    subset_data[[group]] <- droplevels(as.factor(subset_data[[group]]))
    tryCatch(
      {
        test <- survival_weighted_rank_test(subset_data, time, event, group, test_method)
        if (is.null(test)) return(NULL)
        data.frame(
          Comparison = paste(pair, collapse = " vs "),
          Chisq = unname(test$chisq),
          df = test$df,
          p = test$p,
          stringsAsFactors = FALSE
        )
      },
      error = function(e) NULL
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(data.frame())
  table <- do.call(rbind, rows)
  table$p_adjusted <- stats::p.adjust(table$p, method = "holm")
  table
}

prepare_km_single_analysis_result <- function(
  data,
  time,
  event,
  group = "",
  event_value = "1",
  rate_times = NULL,
  analysis_method = "km",
  test_method = "logrank",
  output_tables = c("survival_table", "survival_time"),
  plot_types = c("survival", "event", "cumhaz", "log_survival"),
  plot_versions = "color",
  show_ci = TRUE,
  show_censor = TRUE
) {
  time <- as.character(time %||% character(0))
  event <- as.character(event %||% character(0))
  group <- as.character(group %||% character(0))
  time <- time[!is.na(time) & nzchar(time)]
  event <- event[!is.na(event) & nzchar(event)]
  group <- group[!is.na(group) & nzchar(group)]
  if (length(time) == 0) stop("Select a time variable.")
  if (length(event) == 0) stop("Select an event variable.")
  time <- time[[1]]
  event <- event[[1]]
  group <- if (length(group) > 0) group[[1]] else ""
  model_data <- survival_analysis_data(data, time, event, event_value, group)
  formula <- if (nzchar(as.character(group %||% ""))) {
    stats::as.formula(sprintf("survival::Surv(`%s`, `%s`) ~ `%s`", time, event, group))
  } else {
    stats::as.formula(sprintf("survival::Surv(`%s`, `%s`) ~ 1", time, event))
  }
  fit <- survival::survfit(formula, data = model_data)
  logrank <- NULL
  if (nzchar(as.character(group %||% "")) && length(unique(model_data[[group]])) > 1L) {
    logrank <- survival_weighted_rank_test(model_data, time, event, group, test_method)
  }
  times <- survival_parse_times(rate_times)
  if (length(times) == 0) times <- survival_default_risk_times(model_data[[time]])
  summary_at <- if (length(times) > 0) summary(fit, times = times) else NULL
  output_tables <- intersect(as.character(output_tables %||% character(0)), c("survival_table", "survival_time"))
  plot_types <- intersect(as.character(plot_types %||% character(0)), c("survival", "event", "cumhaz", "log_survival"))
  plot_versions <- intersect(as.character(plot_versions %||% character(0)), c("color", "bw"))
  if (length(plot_versions) == 0) plot_versions <- "color"
  list(
    type = "km",
    analysis_method = as.character(analysis_method %||% "km")[[1]],
    test_method = as.character(test_method %||% "logrank")[[1]],
    test_label = survival_km_test_label(test_method),
    fit = fit,
    data = model_data,
    time = time,
    event = event,
    group = group,
    event_value = event_value,
    n = nrow(model_data),
    events = sum(model_data[[event]], na.rm = TRUE),
    censored = sum(!model_data[[event]], na.rm = TRUE),
    show_ci = isTRUE(show_ci),
    show_censor = isTRUE(show_censor),
    rate_times = times,
    summary_at = summary_at,
    median_table = survival_km_median_table(fit),
    life_table = survival_life_table(model_data, time, event, group, times),
    logrank = logrank,
    posthoc = survival_km_posthoc_table(model_data, time, event, group, formula, test_method),
    output_tables = output_tables,
    plot_types = plot_types,
    plot_versions = plot_versions,
    packages = package_version_label("survival")
  )
}

prepare_km_analysis_result <- function(
  data,
  time,
  event,
  group = "",
  event_value = "1",
  rate_times = NULL,
  analysis_method = "km",
  test_method = "logrank",
  output_tables = c("survival_table", "survival_time"),
  plot_types = c("survival", "event", "cumhaz", "log_survival"),
  plot_versions = "color",
  show_ci = TRUE,
  show_censor = TRUE
) {
  groups <- survival_selected_names(group)
  if (length(groups) <= 1L) {
    return(prepare_km_single_analysis_result(data, time, event, groups, event_value, rate_times, analysis_method, test_method, output_tables, plot_types, plot_versions, show_ci, show_censor))
  }
  analyses <- lapply(groups, function(group_name) {
    prepare_km_single_analysis_result(data, time, event, group_name, event_value, rate_times, analysis_method, test_method, output_tables, plot_types, plot_versions, show_ci, show_censor)
  })
  names(analyses) <- groups
  n_values <- vapply(analyses, function(item) item$n, numeric(1))
  event_values <- vapply(analyses, function(item) item$events, numeric(1))
  censored_values <- vapply(analyses, function(item) item$censored, numeric(1))
  list(
    type = "km_multi",
    analyses = analyses,
    time = analyses[[1]]$time,
    event = analyses[[1]]$event,
    group = groups,
    event_value = event_value,
    analysis_method = as.character(analysis_method %||% "km")[[1]],
    test_method = as.character(test_method %||% "logrank")[[1]],
    test_label = survival_km_test_label(test_method),
    n = max(n_values),
    events = max(event_values),
    censored = max(censored_values),
    show_ci = isTRUE(show_ci),
    show_censor = isTRUE(show_censor),
    output_tables = analyses[[1]]$output_tables,
    plot_types = analyses[[1]]$plot_types,
    plot_versions = analyses[[1]]$plot_versions,
    packages = package_version_label("survival")
  )
}

survival_km_median_table <- function(fit) {
  summary_table <- summary(fit)$table
  table <- as.data.frame(summary_table, stringsAsFactors = FALSE)
  single_stratum <- is.null(dim(summary_table))
  if (isTRUE(single_stratum)) {
    table <- as.data.frame(t(summary_table), stringsAsFactors = FALSE)
  }
  table$Strata <- if (isTRUE(single_stratum)) rep("All", nrow(table)) else rownames(table)
  rownames(table) <- NULL
  quantiles <- tryCatch(quantile(fit, probs = c(0.25, 0.5, 0.75)), error = function(e) NULL)
  add_quantile <- function(prob_label, column_name) {
    if (is.null(quantiles) || is.null(quantiles$quantile)) return(rep(NA_real_, nrow(table)))
    quantile_values <- quantiles$quantile
    if (is.null(dim(quantile_values))) {
      value <- unname(quantile_values[[prob_label]] %||% NA_real_)
      return(rep(value, nrow(table)))
    }
    values <- quantile_values[, prob_label]
    names(values) <- rownames(quantile_values)
    unname(values[table$Strata])
  }
  result <- data.frame(
    Strata = table$Strata,
    Records = table[["records"]] %||% table[["n.max"]] %||% NA,
    Events = table[["events"]] %||% NA,
    Mean = table[["rmean"]] %||% NA,
    `Mean SE` = table[["se(rmean)"]] %||% NA,
    Q1 = add_quantile("25", "Q1"),
    Median = table[["median"]] %||% add_quantile("50", "Median"),
    Q3 = add_quantile("75", "Q3"),
    `Median 95% LCL` = table[["0.95LCL"]] %||% NA,
    `Median 95% UCL` = table[["0.95UCL"]] %||% NA,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  result
}

survival_cox_formula <- function(time, event, covariates) {
  rhs <- paste(sprintf("`%s`", covariates), collapse = " + ")
  stats::as.formula(sprintf("survival::Surv(`%s`, `%s`) ~ %s", time, event, rhs))
}

prepare_cox_analysis_result <- function(data, time, event, covariates, event_value = "1", variable_info = NULL) {
  time <- as.character(time %||% character(0))
  event <- as.character(event %||% character(0))
  time <- time[!is.na(time) & nzchar(time)]
  event <- event[!is.na(event) & nzchar(event)]
  if (length(time) == 0) stop("Select a time variable.")
  if (length(event) == 0) stop("Select an event variable.")
  time <- time[[1]]
  event <- event[[1]]
  covariates <- survival_selected_names(covariates)
  if (length(covariates) == 0) stop("Select at least one covariate.")
  model_data <- survival_analysis_data(data, time, event, event_value, covariates = covariates, variable_info = variable_info)
  formula <- survival_cox_formula(time, event, covariates)
  fit <- survival::coxph(formula, data = model_data, x = TRUE)
  coef_summary <- summary(fit)$coefficients
  ci <- summary(fit)$conf.int
  coef_table <- data.frame(
    Term = rownames(coef_summary),
    B = coef_summary[, "coef"],
    SE = coef_summary[, "se(coef)"],
    HR = ci[, "exp(coef)"],
    LLCI = ci[, "lower .95"],
    ULCI = ci[, "upper .95"],
    z = coef_summary[, "z"],
    p = coef_summary[, "Pr(>|z|)"],
    row.names = NULL,
    check.names = FALSE
  )
  ph <- tryCatch(survival::cox.zph(fit), error = function(e) NULL)
  ph_table <- if (!is.null(ph)) {
    table <- as.data.frame(ph$table, stringsAsFactors = FALSE)
    table$Term <- rownames(table)
    rownames(table) <- NULL
    table[, c("Term", setdiff(names(table), "Term")), drop = FALSE]
  } else {
    data.frame()
  }
  concordance <- summary(fit)$concordance
  list(
    type = "cox",
    fit = fit,
    formula = formula,
    data = model_data,
    time = time,
    event = event,
    covariates = covariates,
    event_value = event_value,
    n = nrow(model_data),
    events = sum(model_data[[event]], na.rm = TRUE),
    coef_table = coef_table,
    ph = ph,
    ph_table = ph_table,
    concordance = concordance,
    packages = package_version_label("survival")
  )
}
