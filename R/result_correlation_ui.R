# Correlation result UI and plots.

correlation_normality_display_table <- function(result) {
  table <- result$normality_table
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(NULL)
  }
  table[, intersect(c("Variable", "N", "Skewness", "Kurtosis", "Normality"), names(table)), drop = FALSE]
}

correlation_appendix_localize_table <- function(table, language = result_appendix_table_language()) {
  source_table <- table
  table <- result_appendix_localize_table(table, language)
  if (!is.data.frame(table) || nrow(table) == 0 || !identical(language, "ko")) {
    return(table)
  }
  translations <- c(
    "satisfied" = "충족",
    "not satisfied" = "미충족",
    "unavailable" = "평가 불가",
    "Omitted because fewer than three valid values were available." = "유효값이 3개 미만이어서 제외했습니다.",
    "Omitted because fewer than two unique values were available." = "서로 다른 값이 2개 미만이어서 제외했습니다."
  )
  for (column in names(table)) {
    if (!is.character(table[[column]]) && !is.factor(table[[column]])) next
    values <- as.character(table[[column]])
    matched <- match(values, names(translations))
    replace <- !is.na(matched)
    values[replace] <- unname(translations[matched[replace]])
    table[[column]] <- values
  }
  result_appendix_preserve_data(table, source_table)
}

correlation_lower_matrix_display_table <- function(
  matrix,
  formatter = format_decimal3,
  p_matrix = NULL,
  significance_levels = FALSE
) {
  if (!is.matrix(matrix) || nrow(matrix) == 0) {
    return(NULL)
  }
  out <- matrix("", nrow = nrow(matrix), ncol = ncol(matrix))
  for (row in seq_len(nrow(matrix))) {
    for (col in seq_len(ncol(matrix))) {
      if (row > col && is.finite(matrix[row, col])) {
        stars <- if (
          isTRUE(significance_levels) &&
            is.matrix(p_matrix) &&
            row <= nrow(p_matrix) &&
            col <= ncol(p_matrix)
        ) {
          correlation_sig(p_matrix[row, col])
        } else {
          ""
        }
        out[row, col] <- paste0(formatter(matrix[row, col]), stars)
      }
    }
  }
  table <- as.data.frame(out, check.names = FALSE)
  names(table) <- colnames(matrix)
  data.frame(Variable = rownames(matrix), table, check.names = FALSE)
}

correlation_matrix_display_table <- function(result, source = NULL) {
  source <- source %||% result
  options <- result$options %||% list()
  correlation_lower_matrix_display_table(
    source$correlation_matrix,
    format_decimal3,
    p_matrix = source$p_matrix,
    significance_levels = isTRUE(options$significance_levels)
  )
}

correlation_matrix_variable_note <- function(source = NULL) {
  matrix <- source$correlation_matrix %||% NULL
  if (!is.matrix(matrix) || nrow(matrix) < 5L) return("")
  labels <- rownames(matrix)
  if (is.null(labels) || length(labels) == 0) labels <- colnames(matrix)
  if (is.null(labels) || length(labels) == 0) return("")
  paste(sprintf("x%d = %s", seq_along(labels), labels), collapse = "; ")
}

correlation_compact_matrix_labels <- function(table, source = NULL, label_start_column = 2L) {
  matrix <- source$correlation_matrix %||% NULL
  if (!is.data.frame(table) || !is.matrix(matrix) || nrow(matrix) < 5L) return(table)
  labels <- paste0("x", seq_len(nrow(matrix)))
  table$Variable <- labels[seq_len(nrow(table))]
  column_indices <- seq.int(label_start_column, min(ncol(table), label_start_column + length(labels) - 1L))
  names(table)[column_indices] <- labels[seq_along(column_indices)]
  if (ncol(table) > 1L) {
    first_width <- 6
    other_width <- (100 - first_width) / (ncol(table) - 1L)
    attr(table, "compact_column_widths") <- c(first_width, rep(other_width, ncol(table) - 1L))
  }
  table
}

correlation_p_matrix_display_table <- function(result, source = NULL) {
  source <- source %||% result
  matrix <- source$p_matrix
  if (!is.matrix(matrix) || nrow(matrix) == 0) {
    return(NULL)
  }
  ci_matrix <- source$ci_matrix
  out <- matrix("", nrow = nrow(matrix), ncol = ncol(matrix))
  for (row in seq_len(nrow(matrix))) {
    for (col in seq_len(ncol(matrix))) {
      if (row > col) {
        ci_text <- if (is.matrix(ci_matrix)) ci_matrix[row, col] %||% "" else ""
        p_text <- if (is.finite(matrix[row, col])) format_p(matrix[row, col]) else ""
        out[row, col] <- paste(ci_text, p_text, sep = "\n")
      }
    }
  }
  table <- as.data.frame(out, check.names = FALSE)
  names(table) <- colnames(matrix)
  data.frame(Variable = rownames(matrix), table, check.names = FALSE)
}

correlation_model_overview_reason <- function(reason) {
  reason <- trimws(as.character(reason %||% ""))
  if (!nzchar(reason)) return("")
  reason <- sub("^Auto selected [^ ]+ because ", "", reason)
  reason <- sub("^[^\\.]+ was selected because ", "", reason)
  reason <- sub("^[^\\.]+ was selected for ", "", reason)
  reason <- sub("^[^\\.]+ was retained because ", "", reason)
  reason <- sub("^[^\\.]+ was retained for ", "", reason)
  reason <- sub("^[^\\.]+ was retained as ", "", reason)
  reason <- sub("\\.$", "", reason)
  reason <- switch(
    reason,
    "both continuous variables satisfied normality" = "normality satisfied",
    "at least one continuous variable did not satisfy normality" = "normality not satisfied",
    "one variable is ordinal; polyserial can be added as an advanced option" = "ordinal variable; polyserial optional",
    "two ordinal variables; polychoric can be added as an advanced option" = "ordinal variables; polychoric optional",
    "binary-ordinal variables; polychoric can be added as an advanced option" = "binary-ordinal; polychoric optional",
    "a continuous variable and a binary variable" = "continuous-binary",
    "a nominal and continuous variable; ANOVA is recommended for detailed group comparison" = "nominal-continuous; ANOVA for group comparison",
    "categorical variables" = "categorical variables",
    reason
  )
  reason
}

correlation_model_overview_wrap <- function(text, width = 24L) {
  text <- trimws(as.character(text %||% ""))
  if (!nzchar(text)) return("")
  wrapped <- strwrap(text, width = width, simplify = TRUE)
  paste(wrapped, collapse = "\n")
}

correlation_method_abbreviation <- function(label) {
  label <- trimws(as.character(label %||% ""))
  normalized <- tolower(label)
  switch(
    normalized,
    pearson = "r",
    spearman = "rho",
    kendall = "tau",
    "point-biserial" = "r_pb",
    phi = "phi",
    "cramer's v" = "V",
    eta = "eta",
    polyserial = "polyser",
    polychoric = "polychor",
    tetrachoric = "tetra",
    label
  )
}

correlation_method_abbreviation_note <- function(
  table,
  language = result_appendix_table_language()
) {
  if (!is.data.frame(table) || nrow(table) == 0) return("")
  values <- unique(unlist(table[-1L], use.names = FALSE))
  values <- values[nzchar(values %||% "")]
  if (length(values) == 0) return("")
  descriptions <- c(
    r = "Pearson correlation",
    rho = "Spearman correlation",
    tau = "Kendall's tau",
    r_pb = "point-biserial correlation",
    phi = "phi coefficient",
    V = "Cramer's V",
    eta = "eta coefficient",
    polyser = "polyserial correlation",
    polychor = "polychoric correlation",
    tetra = "tetrachoric correlation"
  )
  if (identical(result_appendix_table_language(language), "ko")) {
    descriptions <- c(
      r = "Pearson 상관",
      rho = "Spearman 상관",
      tau = "Kendall 순위상관",
      r_pb = "점이연 상관",
      phi = "phi 계수",
      V = "Cramer's V",
      eta = "eta 계수",
      polyser = "다분 상관",
      polychor = "다항 상관",
      tetra = "사분 상관"
    )
  }
  used <- unique(values[values %in% names(descriptions)])
  if (length(used) == 0) return("")
  if (!identical(result_appendix_table_language(language), "ko")) {
    descriptions <- vapply(descriptions, result_appendix_ui_text, character(1), language = language)
  }
  paste(sprintf("%s = %s", used, descriptions[used]), collapse = "; ")
}

correlation_model_overview_matrix_display_table <- function(result, source = NULL) {
  source <- source %||% result
  matrix <- source$method_matrix
  if (!is.matrix(matrix) || nrow(matrix) == 0) {
    return(NULL)
  }
  out <- matrix("", nrow = nrow(matrix), ncol = ncol(matrix))
  for (row in seq_len(nrow(matrix))) {
    for (col in seq_len(ncol(matrix))) {
      if (row > col) {
        out[row, col] <- correlation_method_abbreviation(matrix[row, col] %||% "")
      }
    }
  }
  table <- as.data.frame(out, check.names = FALSE)
  names(table) <- colnames(matrix)
  out_table <- data.frame(Variable = rownames(matrix), table, check.names = FALSE)
  styled_cells <- list()
  for (row in seq_len(nrow(out_table))) {
    for (column in names(out_table)[-1L]) {
      if (nzchar(as.character(out_table[[column]][[row]] %||% ""))) {
        styled_cells[[length(styled_cells) + 1L]] <- list(
          row = row,
          column = column,
          style = "text-align:center;white-space:nowrap;overflow-wrap:normal;word-break:normal;min-width:42px;max-width:70px;width:56px;"
        )
      }
    }
  }
  if (length(styled_cells) > 0) {
    attr(out_table, "cell_styles") <- data.frame(
      row = vapply(styled_cells, `[[`, integer(1), "row"),
      column = vapply(styled_cells, `[[`, character(1), "column"),
      style = vapply(styled_cells, `[[`, character(1), "style"),
      stringsAsFactors = FALSE
    )
  }
  out_table
}

correlation_omitted_display_table <- function(result) {
  table <- result$omitted_table
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(NULL)
  }
  table
}

correlation_matrix_set_ui <- function(result, source = NULL, title_prefix = "") {
  source <- source %||% result
  options <- result$options %||% list()
  main_table <- correlation_matrix_display_table(result, source)
  p_table <- correlation_p_matrix_display_table(result, source)
  overview_table <- correlation_model_overview_matrix_display_table(result, source)
  main_table <- correlation_compact_matrix_labels(main_table, source, label_start_column = 2L)
  p_table <- correlation_compact_matrix_labels(p_table, source, label_start_column = 2L)
  overview_table <- correlation_compact_matrix_labels(overview_table, source, label_start_column = 2L)
  variable_note <- correlation_matrix_variable_note(source)
  compact_label_mode <- nzchar(variable_note)
  variable_count <- length(result$variables %||% character(0))
  appendix_language <- result_appendix_table_language()
  overview_note <- c(
    correlation_method_abbreviation_note(overview_table, appendix_language),
    variable_note
  )
  overview_note <- paste(overview_note[nzchar(overview_note)], collapse = "\n")
  coefficient_note <- result_sci_note_text(
    reference = variable_note,
    symbol = if (!nzchar(title_prefix) && isTRUE(options$significance_levels)) {
      "* p < .05; ** p < .01; *** p < .001"
    } else {
      ""
    }
  )
  latent_response_methods <- c("Polyserial", "Polychoric", "Tetrachoric")
  has_latent_response_inference <- is.matrix(source$method_matrix %||% NULL) &&
    any(source$method_matrix %in% latent_response_methods, na.rm = TRUE)
  appendix_text <- function(en, ko) statedu_localized_text(appendix_language, en, ko)
  appendix_prefix <- if (identical(title_prefix, "Latent-variable ")) {
    paste0(statedu_t("analysis.correlation.latent_variable_prefix", appendix_language), " ")
  } else title_prefix
  p_ci_note <- c(
    appendix_text("Values are 95% CIs and p values.", "값은 95% 신뢰구간과 p값입니다."),
    if (isTRUE(has_latent_response_inference)) {
      appendix_text(
        "Polyserial, polychoric, and tetrachoric inference uses two-step asymptotic standard errors with fixed thresholds and Fisher-z Wald inference.",
        "다분·다항·사분 상관의 추론은 고정 임계값을 둔 2단계 점근 표준오차와 Fisher-z Wald 검정을 사용합니다."
      )
    } else {
      ""
    },
    variable_note
  )
  p_ci_note <- paste(p_ci_note[nzchar(p_ci_note)], collapse = "\n")
  overview_table <- result_appendix_localize_table(overview_table, appendix_language)
  p_table <- result_appendix_localize_table(p_table, appendix_language)
  matrix_landscape_class <- if (variable_count >= 10L) " landscape-table-panel" else ""
  pci_landscape_class <- if (variable_count >= 10L) " landscape-table-panel" else ""
  tagList(
    if (is.data.frame(overview_table) && nrow(overview_table) > 0) {
      div(
        class = paste0("result-section correlation-result-section regression-result-panel", matrix_landscape_class),
        h3(paste0(appendix_prefix, appendix_text("Model overview", "모형 개요"))),
        coefficient_html_table(
          overview_table,
          compact = TRUE,
          compact_font_size = 11,
          compact_width = if (isTRUE(compact_label_mode)) 58 else 88,
          compact_first_width = if (isTRUE(compact_label_mode)) 42 else 82,
          compact_min_width = 280,
          note_line = if (nzchar(overview_note)) overview_note else NULL,
          table_role = "appendix",
          table_language = appendix_language
        )
      )
    },
    div(
      class = paste0("result-section correlation-result-section regression-result-panel", matrix_landscape_class),
      h3(paste0(title_prefix, "Correlation / association coefficients")),
      coefficient_html_table(
        main_table,
        sheet_orientation = if (variable_count <= 9L) "portrait" else "landscape",
        compact = TRUE,
        compact_font_size = 13,
        compact_width = if (isTRUE(compact_label_mode)) 66 else 62,
        compact_first_width = if (isTRUE(compact_label_mode)) 42 else 118,
        note_line = if (nzchar(coefficient_note)) coefficient_note else NULL
      )
    ),
    if (isTRUE(options$p_ci) && is.data.frame(p_table) && nrow(p_table) > 0) {
      div(
        class = paste0("result-section correlation-result-section regression-result-panel", pci_landscape_class),
        h3(paste0(appendix_prefix, appendix_text("p value and 95% CI", "p값 및 95% 신뢰구간"))),
        coefficient_html_table(
          p_table,
          sheet_orientation = if (variable_count <= 9L) "portrait" else "landscape",
          compact = TRUE,
          compact_font_size = 12,
          compact_width = if (isTRUE(compact_label_mode)) 54 else 50,
          compact_first_width = if (isTRUE(compact_label_mode)) 42 else 104,
          compact_min_width = 280,
          note_line = p_ci_note,
          table_role = "appendix",
          table_language = appendix_language
        )
      )
    }
  )
}

correlation_scatter_plot_id <- function() {
  "correlation_scatter_plot_output"
}

correlation_heatmap_plot_id <- function() {
  "correlation_heatmap_plot_output"
}

correlation_plot_height <- function(result, base = 560, per_variable = 28, max_height = 900) {
  count <- length(result$variables %||% character(0))
  paste0(min(max_height, max(base, 180 + count * per_variable)), "px")
}

correlation_square_plot_size <- function(result, base = 500, per_variable = 14, max_size = 620) {
  count <- length(result$variables %||% character(0))
  paste0(min(max_size, max(base, 180 + count * per_variable)), "px")
}

correlation_plot_note <- function(result) {
  matrix <- result$correlation_matrix %||% NULL
  if (!is.matrix(matrix) || nrow(matrix) < 6L) return(NULL)
  result_note_tag(correlation_matrix_variable_note(result))
}

correlation_results_ui <- function(result) {
  if (is.null(result)) {
    return(NULL)
  }
  main_table <- correlation_matrix_display_table(result)
  if (!is.data.frame(main_table) || nrow(main_table) == 0) {
    return(empty_message(statedu_t("analysis.correlation.no_results", result_appendix_table_language())))
  }
  options <- result$options %||% list()
  omitted_table <- correlation_omitted_display_table(result)
  appendix_language <- result_appendix_table_language()
  appendix_text <- function(en, ko) statedu_localized_text(appendix_language, en, ko)
  normality_table <- correlation_appendix_localize_table(correlation_normality_display_table(result), appendix_language)
  omitted_table <- correlation_appendix_localize_table(omitted_table, appendix_language)
  tagList(
    div(
      class = "correlation-results regression-results",
      correlation_matrix_set_ui(result),
      if (is.list(result$latent)) {
        correlation_matrix_set_ui(result, source = result$latent, title_prefix = "Latent-variable ")
      },
      if (isTRUE(options$normality)) {
        div(
          class = "result-section correlation-result-section regression-result-panel",
          h3(appendix_text("Normality", "정규성")),
          coefficient_html_table(normality_table, table_role = "appendix", table_language = appendix_language)
        )
      },
      if (is.data.frame(omitted_table) && nrow(omitted_table) > 0) {
        div(
          class = "result-section correlation-result-section regression-result-panel",
          h3(appendix_text("Omitted variables", "제외된 변수")),
          coefficient_html_table(omitted_table, table_role = "appendix", table_language = appendix_language)
        )
      },
      if (isTRUE(options$scatter_plot)) {
        div(
          class = "result-section correlation-result-section correlation-plot-section regression-result-panel",
          h3("Scatter plot matrix"),
          plotOutput(
            correlation_scatter_plot_id(),
            width = correlation_square_plot_size(result),
            height = correlation_square_plot_size(result)
          ),
          correlation_plot_note(result)
        )
      },
      if (isTRUE(options$matrix_plot)) {
        div(
          class = "result-section correlation-result-section correlation-plot-section regression-result-panel",
          h3("Correlation matrix heatmap"),
          plotOutput(
            correlation_heatmap_plot_id(),
            width = correlation_square_plot_size(result),
            height = correlation_square_plot_size(result)
          ),
          correlation_plot_note(result)
        )
      }
    )
  )
}

correlation_export_image_cache <- function(render = plot_data_uri, max_bytes = 16 * 1024^2,
                                         context = function() list(
                                           dpi = analysis_figure_dpi(), options = options(),
                                           locale = Sys.getlocale(), cwd = getwd(),
                                           windows = if (.Platform$OS.type == "windows") grDevices::windows.options() else NULL,
                                           fonts = if (.Platform$OS.type == "windows") grDevices::windowsFonts() else NULL),
                                         max_entries = 2L) {
  entries <- list()
  list(
    clear = function() { entries <<- list(); invisible(NULL) },
    render = function(plot_function, result, width = 420, height = 420, res = 96) {
      key <- list(plot_function = plot_function, result = result, width = width,
                  height = height, res = res, context = context())
      for (entry in entries) if (identical(entry$key, key, num.eq = FALSE)) return(entry$value)
      quiet <- TRUE
      seed <- get0(".Random.seed", .GlobalEnv, inherits = FALSE)
      value <- withCallingHandlers(render(plot_function, result, width, height, res),
        warning = function(w) { quiet <<- FALSE }, message = function(m) { quiet <<- FALSE })
      # Preserve conditions and RNG effects by only retaining quiet, deterministic draws.
      if (quiet && identical(seed, get0(".Random.seed", .GlobalEnv, inherits = FALSE), num.eq = FALSE)) {
        entry <- list(key = key, value = value)
        if (as.numeric(object.size(entry)) <= max_bytes) {
          entries <<- c(entries, list(entry))
          while (length(entries) > max_entries || as.numeric(object.size(entries)) > max_bytes) {
            entries <<- entries[-1L]
          }
        }
      }
      value
    }
  )
}

draw_correlation_scatter_plot <- function(result) {
  data <- result$data
  measurements <- result$measurements %||% character(0)
  plot_vars <- names(measurements)[measurements %in% c("continuous")]
  plot_vars <- intersect(plot_vars, names(data))
  if (!is.data.frame(data) || length(plot_vars) < 2) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, statedu_t("analysis.correlation.scatter_requires_continuous", result_appendix_table_language()), cex = 0.9)
    return(invisible(NULL))
  }
  original_count <- length(plot_vars)
  plot_vars <- head(plot_vars, 12)
  plot_data <- data.frame(lapply(plot_vars, function(name) {
    correlation_analysis_vector(data[[name]], measurements[[name]])
  }), check.names = FALSE)
  plot_labels <- unname(result$labels[plot_vars])
  if (length(plot_labels) >= 6L) {
    plot_labels <- paste0("x", seq_along(plot_labels))
  }
  names(plot_data) <- plot_labels
  plot_data <- plot_data[, vapply(plot_data, function(values) sum(!is.na(values)) >= 3 && stats::sd(values, na.rm = TRUE) > 0, logical(1)), drop = FALSE]
  if (ncol(plot_data) < 2) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, statedu_t("analysis.correlation.scatter_requires_varying", result_appendix_table_language()), cex = 0.9)
    return(invisible(NULL))
  }
  n <- ncol(plot_data)
  ranges <- lapply(plot_data, function(values) {
    range(values, na.rm = TRUE)
  })
  pad_range <- function(range_value) {
    span <- diff(range_value)
    if (!is.finite(span) || span == 0) {
      span <- 1
    }
    range_value + c(-1, 1) * span * 0.06
  }
  ranges <- lapply(ranges, pad_range)
  layout_matrix <- matrix(seq_len(n * n), nrow = n, byrow = TRUE)
  graphics::layout(layout_matrix)
  graphics::par(
    oma = c(3, 3, if (original_count > length(plot_vars)) 2.5 else 1, 1),
    mar = c(0.35, 0.35, 0.35, 0.35),
    mgp = c(1.4, 0.35, 0),
    tck = -0.02,
    cex = if (n >= 6L) 0.9 else 1.05
  )
  for (row in seq_len(n)) {
    for (col in seq_len(n)) {
      x <- plot_data[[col]]
      y <- plot_data[[row]]
      if (row < col) {
        graphics::plot.new()
        next
      }
      if (row == col) {
        h <- graphics::hist(x, plot = FALSE)
        graphics::plot(
          NA,
          xlim = ranges[[col]],
          ylim = c(0, max(h$counts, 1)),
          axes = FALSE,
          xlab = "",
          ylab = "",
          frame.plot = TRUE
        )
        graphics::rect(h$breaks[-length(h$breaks)], 0, h$breaks[-1], h$counts, col = "#dbeafe", border = "#8aa4c2")
        graphics::text(mean(ranges[[col]]), max(h$counts, 1) * 0.84, names(plot_data)[[col]], cex = if (n >= 6L) 1.05 else 1.16, font = 2, col = "#15233a")
      } else {
        graphics::plot(
          x,
          y,
          xlim = ranges[[col]],
          ylim = ranges[[row]],
          axes = FALSE,
          xlab = "",
          ylab = "",
          pch = 16,
          cex = 0.86,
          col = grDevices::adjustcolor("#1f6fa8", alpha.f = 0.62),
          frame.plot = TRUE
        )
        ok <- stats::complete.cases(x, y)
        if (sum(ok) >= 6 && length(unique(x[ok])) > 2 && length(unique(y[ok])) > 2) {
          fit <- stats::lowess(x[ok], y[ok], f = 0.8)
          graphics::lines(fit, col = "#c2410c", lwd = 1.25)
        }
      }
      if (row == n) {
        graphics::axis(1, labels = FALSE)
      }
      if (col == 1 && row > 1) {
        graphics::axis(2, labels = FALSE)
      }
    }
  }
  if (original_count > length(plot_vars)) {
    graphics::mtext(sprintf(statedu_t("analysis.correlation.scatter_display_limit", result_appendix_table_language()), length(plot_vars), original_count), outer = TRUE, side = 3, cex = 1.18, col = "#52606d")
  }
  invisible(NULL)
}

draw_correlation_heatmap <- function(result) {
  matrix <- result$correlation_matrix
  if (!is.matrix(matrix) || nrow(matrix) == 0) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, statedu_t("analysis.correlation.no_matrix_data", result_appendix_table_language()))
    return(invisible(NULL))
  }
  values <- matrix
  diag(values) <- NA_real_
  values <- values[nrow(values):1, , drop = FALSE]
  labels_x <- colnames(matrix)
  labels_y <- rev(rownames(matrix))
  n <- ncol(values)
  if (n >= 6L) {
    labels_x <- paste0("x", seq_len(n))
    labels_y <- rev(labels_x)
  }
  graphics::par(
    mar = if (n >= 6L) c(4.5, 4.5, 2, 5.5) else c(11, 12, 3, 7),
    xpd = NA,
    cex = if (n >= 6L) 0.95 else 1.12
  )
  palette <- grDevices::colorRampPalette(c("#2b6cb0", "#f7fafc", "#c2410c"))(121)
  graphics::image(
    x = seq_len(ncol(values)),
    y = seq_len(nrow(values)),
    z = t(values),
    zlim = c(-1, 1),
    col = palette,
    axes = FALSE,
    xlab = "",
    ylab = "",
    asp = 1
  )
  graphics::axis(1, at = seq_len(ncol(values)), labels = labels_x, las = 2, cex.axis = if (n > 12) 0.92 else 1.08, tick = FALSE)
  graphics::axis(2, at = seq_len(nrow(values)), labels = labels_y, las = 1, cex.axis = if (n > 12) 0.92 else 1.08, tick = FALSE)
  graphics::box(col = "#1f2937")
  show_values <- n <= 10
  if (show_values) {
    for (row in seq_len(nrow(values))) {
      for (col in seq_len(ncol(values))) {
        if (is.finite(values[row, col])) {
          graphics::text(col, row, format_decimal3(values[row, col]), cex = 0.96, col = "#111827")
        }
      }
    }
  }
  legend_y <- seq(1, nrow(values), length.out = length(palette))
  plot_limits <- graphics::par("usr")
  plot_width <- plot_limits[[2]] - plot_limits[[1]]
  legend_x <- plot_limits[[2]] + plot_width * 0.04
  graphics::rect(legend_x, legend_y[-length(legend_y)], legend_x + plot_width * 0.045, legend_y[-1], col = palette[-length(palette)], border = NA)
  graphics::text(legend_x + plot_width * 0.10, c(1, (1 + nrow(values)) / 2, nrow(values)), c("-1", "0", "1"), cex = 0.98, adj = 0)
  invisible(NULL)
}
