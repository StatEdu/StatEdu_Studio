# ANCOVA result UI.

ancova_model_overview_table <- function(result, variable_table = NULL, labels = character(0)) {
  rows <- lapply(result$results %||% list(), function(item) {
    data.frame(
      DV = display_variable_name_static(item$dependent, variable_table, labels, label_only = TRUE),
      Group = display_variable_name_static(item$factor, variable_table, labels, label_only = TRUE),
      Covariates = paste(vapply(item$covariates, display_variable_name_static, character(1), table = variable_table, labels = labels, label_only = TRUE), collapse = " + "),
      N = item$n,
      Analysis = item$method,
      `Sum of squares` = ancova_sum_of_squares_label(item$sum_of_squares %||% "type2"),
      Reason = item$reason,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  ttest_bind_result_rows(rows)
}

ancova_assumption_review_table <- function(result, variable_table = NULL, labels = character(0)) {
  rows <- lapply(result$results %||% list(), function(item) {
    data.frame(
      DV = display_variable_name_static(item$dependent, variable_table, labels, label_only = TRUE),
      Group = display_variable_name_static(item$factor, variable_table, labels, label_only = TRUE),
      `Residual normality p` = format_p(item$assumptions$normality_p),
      `Homogeneity p` = format_p(item$assumptions$homogeneity_p),
      `Slope p` = format_p(item$assumptions$slope_p),
      Linearity = item$assumptions$linearity_summary %||% "",
      Collinearity = item$collinearity$summary %||% "",
      Influence = item$influence$summary %||% "",
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  ttest_bind_result_rows(rows)
}

ancova_normality_review_table <- function(result, variable_table = NULL, labels = character(0)) {
  rows <- lapply(result$results %||% list(), function(item) {
    data.frame(
      DV = display_variable_name_static(item$dependent, variable_table, labels, label_only = TRUE),
      `Outcome method` = item$assumptions$outcome_normality_method %||% "",
      `Outcome p` = format_p(item$assumptions$outcome_normality_p),
      `Residual method` = item$assumptions$normality_method %||% "",
      `Residual p` = format_p(item$assumptions$normality_p),
      Note = "Outcome normality is descriptive; residual normality drives ANCOVA method selection.",
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  ttest_bind_result_rows(rows)
}

ancova_linearity_review_table <- function(result, variable_table = NULL, labels = character(0)) {
  rows <- list()
  for (item in result$results %||% list()) {
    table <- item$assumptions$linearity_table
    if (!is.data.frame(table) || nrow(table) == 0) next
    rows[[length(rows) + 1L]] <- data.frame(
      DV = display_variable_name_static(item$dependent, variable_table, labels, label_only = TRUE),
      Covariate = vapply(table$Covariate, display_variable_name_static, character(1), table = variable_table, labels = labels, label_only = TRUE),
      `Quadratic p` = format_p(table$p),
      Status = table$Status,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
  ttest_bind_result_rows(rows)
}

ancova_interaction_terms_review_table <- function(result, variable_table = NULL, labels = character(0)) {
  rows <- list()
  for (item in result$results %||% list()) {
    table <- item$interaction_terms
    if (!is.data.frame(table) || nrow(table) == 0) next
    rows[[length(rows) + 1L]] <- data.frame(
      DV = display_variable_name_static(item$dependent, variable_table, labels, label_only = TRUE),
      table,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
  ttest_bind_result_rows(rows)
}

ancova_simple_effects_review_table <- function(result, variable_table = NULL, labels = character(0)) {
  rows <- list()
  for (item in result$results %||% list()) {
    table <- item$simple_effects
    if (!is.data.frame(table) || nrow(table) == 0) next
    table$Covariate <- vapply(table$Covariate, display_variable_name_static, character(1), table = variable_table, labels = labels, label_only = TRUE)
    rows[[length(rows) + 1L]] <- data.frame(
      DV = display_variable_name_static(item$dependent, variable_table, labels, label_only = TRUE),
      table,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
  ttest_bind_result_rows(rows)
}

ancova_collinearity_review_table <- function(result, variable_table = NULL, labels = character(0)) {
  rows <- list()
  for (item in result$results %||% list()) {
    table <- item$collinearity$table
    if (!is.data.frame(table) || nrow(table) == 0) next
    rows[[length(rows) + 1L]] <- data.frame(
      DV = display_variable_name_static(item$dependent, variable_table, labels, label_only = TRUE),
      Term = table$Term,
      Tolerance = ancova_format_decimal3_vec(table$Tolerance),
      VIF = ancova_format_decimal3_vec(table$VIF),
      Status = table$Status,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
  ttest_bind_result_rows(rows)
}

ancova_influence_review_table <- function(result, variable_table = NULL, labels = character(0)) {
  rows <- list()
  for (item in result$results %||% list()) {
    table <- item$influence$table
    if (!is.data.frame(table) || nrow(table) == 0 || !"Flag" %in% names(table)) next
    table <- table[nzchar(as.character(table$Flag)), , drop = FALSE]
    if (nrow(table) == 0) next
    rows[[length(rows) + 1L]] <- data.frame(
      DV = display_variable_name_static(item$dependent, variable_table, labels, label_only = TRUE),
      Case = table$Case,
      `Studentized residual` = ancova_format_decimal3_vec(table$`Studentized residual`),
      Leverage = ancova_format_decimal3_vec(table$Leverage),
      `Cook's D` = ancova_format_decimal3_vec(table$`Cook's D`),
      Flag = table$Flag,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
  ttest_bind_result_rows(rows)
}

ancova_combined_result_table <- function(result, variable_table = NULL, labels = character(0)) {
  items <- result$results %||% list()
  note_spec <- ancova_combined_note_spec(result, variable_table, labels)
  marker_rows <- list()
  row_offset <- 0L
  add_marker <- function(row, column, marker) {
    marker <- as.character(marker %||% "")
    if (!nzchar(marker)) return()
    marker <- paste(unique(trimws(unlist(strsplit(marker, ",")))), collapse = ",")
    marker_rows[[length(marker_rows) + 1L]] <<- data.frame(
      row = row,
      column = column,
      marker = marker,
      stringsAsFactors = FALSE
    )
  }
  rows <- lapply(items, function(item) {
    table <- item$table
    if (!is.data.frame(table) || nrow(table) == 0) {
      return(NULL)
    }
    dv <- display_variable_name_static(item$dependent, variable_table, labels, label_only = TRUE)
    table <- cbind(DV = rep("", nrow(table)), table, stringsAsFactors = FALSE)
    table$DV[[1]] <- dv
    first_row <- row_offset + 1L
    effect_rows <- which(nzchar(as.character(table[["Effect size"]] %||% "")))
    if (isTRUE(note_spec$effect_specific_markers)) {
      for (row in effect_rows) add_marker(row_offset + row, "Effect size", note_spec$markers$effect)
    }
    if (isTRUE(note_spec$posthoc_specific_markers) && "post-hoc" %in% names(table)) {
      posthoc_marker <- ancova_posthoc_note_marker(item, note_spec)
      posthoc_rows <- which(nzchar(as.character(table[["post-hoc"]] %||% "")))
      if (length(posthoc_rows) > 0L) add_marker(row_offset + posthoc_rows[[1]], "post-hoc", posthoc_marker)
    }
    item_markers <- character(0)
    method_specific_markers <- isTRUE(note_spec$method_specific_markers)
    if (isTRUE(method_specific_markers)) {
      method_marker <- note_spec$method_markers[[item$method %||% ""]]
      item_markers <- c(item_markers, method_marker)
    }
    add_marker(first_row, "p", paste(item_markers, collapse = ","))
    row_offset <<- row_offset + nrow(table)
    table
  })
  out <- ttest_bind_result_rows(rows)
  markers <- ttest_bind_result_rows(marker_rows)
  if (is.data.frame(markers) && nrow(markers) > 0) {
    markers <- markers[nzchar(markers$marker), , drop = FALSE]
    marker_keys <- paste(markers$row, markers$column, sep = "\r")
    markers <- ttest_bind_result_rows(lapply(split(markers, marker_keys), function(group) {
      data.frame(
        row = group$row[[1]],
        column = group$column[[1]],
        marker = paste(unique(trimws(unlist(strsplit(paste(group$marker, collapse = ","), ",")))), collapse = ","),
        stringsAsFactors = FALSE
      )
    }))
    attr(out, "note_markers") <- markers
  }
  out
}

ancova_combined_note_spec <- function(result, variable_table = NULL, labels = character(0)) {
  items <- result$results %||% list()
  if (length(items) == 0) {
    return(list(lines = character(0), markers = list()))
  }
  unique_text <- function(values) {
    values <- trimws(as.character(values %||% character(0)))
    unique(values[nzchar(values)])
  }

  marker_index <- 0L
  marker_for <- function() {
    marker_index <<- marker_index + 1L
    as.character(marker_index)
  }
  markers <- list()

  methods <- unique_text(vapply(items, function(item) item$method %||% "", character(1)))
  item_methods <- vapply(items, function(item) item$method %||% "", character(1))
  method_specific_markers <- length(unique_text(item_methods)) > 1L
  method_lines <- character(0)
  ss_lines <- character(0)
  posthoc_lines <- character(0)
  effect_lines <- character(0)
  display_lines <- character(0)

  method_note_text <- function(method) {
    if (identical(method, "Robust ANCOVA (HC3)")) {
      return("Robust ANCOVA (HC3): group effect F, p, and post-hoc contrasts use HC3 robust covariance; Type II/III robust tests use the selected sum-of-squares type.")
    }
    if (identical(method, "Ranked ANCOVA")) {
      return("Ranked ANCOVA: rank-transformed dependent variable and continuous covariates are used; adjusted means are on the rank scale, not the original outcome scale.")
    }
    if (identical(method, "Interaction ANCOVA")) {
      return("Interaction ANCOVA: group x covariate interaction was detected; interpret group differences conditionally across covariate values rather than as a single adjusted mean difference.")
    }
    sprintf("Analysis method: %s.", method)
  }
  method_markers <- list()
  if (!isTRUE(method_specific_markers)) {
    method_lines <- c(method_lines, if (length(methods) > 0L) method_note_text(methods[[1]]) else "Analysis method: ANCOVA.")
  } else {
    for (method in methods) {
      marker <- marker_for()
      method_markers[[method]] <- marker
      method_lines <- c(method_lines, sprintf("%s. %s", marker, method_note_text(method)))
    }
  }

  ss_types <- unique_text(vapply(items, function(item) item$sum_of_squares %||% "type2", character(1)))
  if (length(ss_types) <= 1L) {
    ss_lines <- c(ss_lines, ancova_sum_of_squares_note(if (length(ss_types) > 0L) ss_types[[1]] else "type2"))
  } else {
    ss_lines <- c(ss_lines, paste(vapply(ss_types, ancova_sum_of_squares_note, character(1)), collapse = " "))
  }
  if (any(vapply(items, function(item) identical(item$method, "Interaction ANCOVA") && !identical(item$sum_of_squares %||% "type2", "type3"), logical(1)))) {
    ss_lines <- c(ss_lines, "Type III SS is recommended for interaction models.")
  }

  posthoc_items <- items[vapply(items, function(item) {
    fit_data <- item$fit_data
    factor_name <- item$factor
    if (!is.data.frame(fit_data) || !factor_name %in% names(fit_data)) {
      return(FALSE)
    }
    nlevels(fit_data[[factor_name]]) >= 3L
  }, logical(1))]
  posthoc_key <- function(item) {
    posthoc_method <- as.character(item$options$posthoc_method %||% "bonferroni")
    adjustment <- if (identical(posthoc_method, "holm")) "Holm-Bonferroni-adjusted" else "Bonferroni-corrected"
    display <- if (isTRUE(item$options$ordered_significance)) "ordered significance notation" else "compact letter notation"
    paste(adjustment, display, sep = "\r")
  }
  posthoc_note_text <- function(item) {
    parts <- strsplit(posthoc_key(item), "\r", fixed = TRUE)[[1]]
    target <- if (identical(item$method, "Ranked ANCOVA")) "adjusted rank means" else "adjusted means"
    sprintf(
      "Post-hoc: %s pairwise model contrasts of %s; displayed with %s.",
      parts[[1]],
      target,
      parts[[2]]
    )
  }
  posthoc_specific_markers <- FALSE
  posthoc_markers <- list()
  if (length(posthoc_items) > 0) {
    posthoc_keys <- unique_text(vapply(posthoc_items, posthoc_key, character(1)))
    posthoc_specific_markers <- length(posthoc_keys) > 1L
    if (isTRUE(posthoc_specific_markers)) {
      for (key in posthoc_keys) {
        marker <- marker_for()
        posthoc_markers[[key]] <- marker
        template_item <- posthoc_items[[match(key, vapply(posthoc_items, posthoc_key, character(1)))]]
        posthoc_lines <- c(posthoc_lines, sprintf("%s. %s", marker, posthoc_note_text(template_item)))
      }
    } else {
      posthoc_lines <- c(posthoc_lines, posthoc_note_text(posthoc_items[[1]]))
    }
  }

  effect_labels <- unique_text(vapply(items, function(item) item$effect_size_label %||% "partial eta squared", character(1)))
  effect_specific_markers <- length(effect_labels) > 1L
  if (isTRUE(effect_specific_markers)) {
    markers$effect <- marker_for()
    effect_lines <- c(effect_lines, sprintf("%s. Effect size = %s.", markers$effect, paste(effect_labels, collapse = "; ")))
  } else {
    effect_lines <- c(effect_lines, sprintf("Effect size = %s.", if (length(effect_labels) > 0) effect_labels[[1]] else "partial eta squared"))
  }
  if (any(vapply(items, function(item) isTRUE(item$options$mean_se), logical(1)))) {
    if (any(vapply(items, function(item) identical(item$method, "Ranked ANCOVA"), logical(1)))) {
      display_lines <- c(display_lines, "Rank M \u00B1 SE = adjusted rank mean \u00B1 standard error.")
    }
    if (any(vapply(items, function(item) !identical(item$method, "Ranked ANCOVA"), logical(1)))) {
      display_lines <- c(display_lines, "M \u00B1 SE = adjusted mean \u00B1 standard error.")
    }
  }
  list(
    lines = c(method_lines, ss_lines, posthoc_lines, effect_lines, display_lines),
    markers = markers,
    method_markers = method_markers,
    method_specific_markers = method_specific_markers,
    effect_specific_markers = effect_specific_markers,
    posthoc_markers = posthoc_markers,
    posthoc_specific_markers = posthoc_specific_markers
  )
}

ancova_posthoc_note_marker <- function(item, note_spec) {
  if (!isTRUE(note_spec$posthoc_specific_markers)) {
    return("")
  }
  posthoc_method <- as.character(item$options$posthoc_method %||% "bonferroni")
  adjustment <- if (identical(posthoc_method, "holm")) "Holm-Bonferroni-adjusted" else "Bonferroni-corrected"
  display <- if (isTRUE(item$options$ordered_significance)) "ordered significance notation" else "compact letter notation"
  key <- paste(adjustment, display, sep = "\r")
  note_spec$posthoc_markers[[key]] %||% ""
}

ancova_combined_note <- function(result, variable_table = NULL, labels = character(0)) {
  spec <- ancova_combined_note_spec(result, variable_table, labels)
  lines <- spec$lines %||% character(0)
  paste(lines, collapse = "\n")
}

ancova_result_has_plots <- function(item) {
  options <- item$options %||% list()
  isTRUE(options$plot_adjusted_means) ||
    isTRUE(options$plot_raw_overlay) ||
    isTRUE(options$plot_regression_lines)
}

ancova_plot_palette <- function(n) {
  grDevices::hcl.colors(max(1L, n), "Dark 3")
}

ancova_plot_adjusted_data <- function(item) {
  ancova_adjusted_means(item$model, item$fit_data, item$factor, item$covariates)
}

ancova_plot_y_label <- function(item) {
  if (identical(item$method, "Ranked ANCOVA")) "Ranked outcome" else item$dependent
}

draw_ancova_adjusted_mean_plot <- function(item) {
  adjusted <- ancova_plot_adjusted_data(item)
  if (!is.data.frame(adjusted) || nrow(adjusted) == 0) {
    graphics::plot.new()
    graphics::text(.5, .5, "No adjusted means available")
    return(invisible(NULL))
  }
  x <- seq_len(nrow(adjusted))
  ci <- stats::qt(.975, df = stats::df.residual(item$model)) * adjusted$SE
  y_min <- min(0, adjusted$Estimate - ci, na.rm = TRUE)
  y_max <- max(0, adjusted$Estimate + ci, na.rm = TRUE)
  y_padding <- diff(range(c(y_min, y_max))) * .06
  if (!is.finite(y_padding) || y_padding <= 0) y_padding <- max(abs(y_max), 1) * .06
  y_limits <- c(y_min - y_padding, y_max + y_padding)
  bar_width <- .62
  colors <- ancova_plot_palette(nrow(adjusted))
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mar = c(5, 4.2, 2, 1), cex.axis = .85)
  adjusted_label <- if (identical(item$method, "Ranked ANCOVA")) "Adjusted rank mean" else "Adjusted mean"
  graphics::plot(
    NA_real_,
    NA_real_,
    type = "n",
    xaxt = "n",
    xlim = c(.45, nrow(adjusted) + .55),
    xlab = item$factor,
    ylab = adjusted_label,
    ylim = y_limits,
    main = ""
  )
  graphics::abline(h = pretty(y_limits), col = "#e5e7eb", lwd = .8)
  graphics::abline(h = 0, col = "#475569", lwd = .9)
  graphics::rect(
    xleft = x - bar_width / 2,
    ybottom = 0,
    xright = x + bar_width / 2,
    ytop = adjusted$Estimate,
    col = grDevices::adjustcolor(colors, alpha.f = .72),
    border = "#334155",
    lwd = 1
  )
  graphics::segments(x, adjusted$Estimate - ci, x, adjusted$Estimate + ci, col = "#1f2937", lwd = 1.2)
  graphics::segments(x - .08, adjusted$Estimate - ci, x + .08, adjusted$Estimate - ci, col = "#1f2937", lwd = 1.2)
  graphics::segments(x - .08, adjusted$Estimate + ci, x + .08, adjusted$Estimate + ci, col = "#1f2937", lwd = 1.2)
  graphics::axis(1, at = x, labels = adjusted$Level)
  graphics::box()
}

draw_ancova_raw_overlay_plot <- function(item) {
  data <- item$fit_data
  factor_values <- factor(data[[item$factor]], levels = levels(item$fit_data[[item$factor]]))
  y <- suppressWarnings(as.numeric(data[[item$dependent]]))
  adjusted <- ancova_plot_adjusted_data(item)
  levels <- as.character(levels(factor_values))
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mar = c(5, 4.2, 2, 1), cex.axis = .85)
  graphics::boxplot(
    y ~ factor_values,
    outline = FALSE,
    col = grDevices::adjustcolor("#c7d2fe", alpha.f = .45),
    border = "#64748b",
    xlab = item$factor,
    ylab = ancova_plot_y_label(item),
    main = ""
  )
  for (index in seq_along(levels)) {
    values <- y[as.character(factor_values) == levels[[index]]]
    graphics::points(
      jitter(rep(index, length(values)), amount = .08),
      values,
      pch = 16,
      cex = .45,
      col = grDevices::adjustcolor("#334155", alpha.f = .35)
    )
  }
  match_index <- match(adjusted$Level, levels)
  graphics::points(match_index, adjusted$Estimate, pch = 18, cex = 1.35, col = "#b91c1c")
  graphics::lines(match_index, adjusted$Estimate, col = "#b91c1c", lwd = 1.1)
  graphics::legend("topright", legend = if (identical(item$method, "Ranked ANCOVA")) "Adjusted rank mean" else "Adjusted mean", pch = 18, col = "#b91c1c", bty = "n", cex = .8)
}

draw_ancova_regression_lines_plot <- function(item) {
  numeric_covariates <- item$covariates[vapply(item$covariates, function(name) is.numeric(item$fit_data[[name]]), logical(1))]
  covariate <- if (length(numeric_covariates) > 0) numeric_covariates[[1]] else ""
  if (is.null(covariate) || !nzchar(covariate)) {
    graphics::plot.new()
    graphics::text(.5, .5, "No continuous covariate available")
    return(invisible(NULL))
  }
  data <- item$fit_data
  factor_levels <- levels(data[[item$factor]])
  x <- suppressWarnings(as.numeric(data[[covariate]]))
  y <- suppressWarnings(as.numeric(data[[item$dependent]]))
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mar = c(5, 4.2, 2, 1), cex.axis = .85)
  colors <- ancova_plot_palette(length(factor_levels))
  graphics::plot(
    x,
    y,
    type = "n",
    xlab = covariate,
    ylab = ancova_plot_y_label(item),
    main = ""
  )
  for (index in seq_along(factor_levels)) {
    level <- factor_levels[[index]]
    group_rows <- as.character(data[[item$factor]]) == level
    graphics::points(x[group_rows], y[group_rows], pch = 16, cex = .45, col = grDevices::adjustcolor(colors[[index]], alpha.f = .45))
    x_grid <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = 60)
    newdata <- data.frame(stats::setNames(list(factor(rep(level, length(x_grid)), levels = factor_levels)), item$factor), check.names = FALSE)
    for (name in item$covariates) {
      if (identical(name, covariate)) {
        newdata[[name]] <- x_grid
      } else if (is.numeric(data[[name]])) {
        newdata[[name]] <- mean(data[[name]], na.rm = TRUE)
      } else {
        value_levels <- if (is.factor(data[[name]])) levels(data[[name]]) else unique(as.character(stats::na.omit(data[[name]])))
        reference <- if (length(value_levels) > 0) value_levels[[1]] else NA_character_
        newdata[[name]] <- factor(rep(reference, length(x_grid)), levels = value_levels)
      }
    }
    prediction <- stats::predict(item$model, newdata = newdata)
    graphics::lines(x_grid, prediction, col = colors[[index]], lwd = 1.5)
  }
  graphics::legend("topleft", legend = factor_levels, col = colors, lwd = 1.5, pch = 16, bty = "n", cex = .8)
}

ancova_plot_sections <- function(item) {
  options <- item$options %||% list()
  sections <- list()
  add_plot <- function(title, plot_function) {
    sections[[length(sections) + 1L]] <<- tags$div(
      class = "ancova-plot-card",
      tags$h4(title),
      tags$img(
        class = "analysis-plot-image",
        style = "display:block;width:100%;max-width:640px;height:auto;",
        src = plot_data_uri(plot_function, item, width = 1700, height = 1250, res = 300),
        alt = title
      )
    )
  }
  adjusted_title <- if (identical(item$method, "Ranked ANCOVA")) "Adjusted rank mean error bar plot (95% CI)" else "Adjusted mean error bar plot (95% CI)"
  overlay_title <- if (identical(item$method, "Ranked ANCOVA")) "Ranked data + adjusted rank mean overlay" else "Raw data + adjusted mean overlay"
  if (isTRUE(options$plot_adjusted_means)) add_plot(adjusted_title, draw_ancova_adjusted_mean_plot)
  if (isTRUE(options$plot_raw_overlay)) add_plot(overlay_title, draw_ancova_raw_overlay_plot)
  if (isTRUE(options$plot_regression_lines)) add_plot("Covariate-adjusted regression lines", draw_ancova_regression_lines_plot)
  sections
}

ancova_model_overview_html_table <- function(table) {
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(NULL)
  }
  column_widths <- c(
    DV = "52px",
    Group = "72px",
    Covariates = "88px",
    N = "44px",
    Analysis = "96px",
    `Sum of squares` = "88px",
    Reason = "250px"
  )
  width_for <- function(name) column_widths[[name]] %||% "90px"
  header_style <- function(index) {
    paste(
      "padding:6px 8px;line-height:1.3;border-left:0;border-right:0;border-bottom:2px solid #1f2937;",
      "vertical-align:middle;font-weight:700;background:transparent;white-space:normal;overflow-wrap:anywhere;",
      "text-align:", if (names(table)[[index]] %in% c("DV", "Group", "Covariates", "Reason")) "left" else "center", ";"
    )
  }
  body_style <- function(index, last) {
    paste(
      "padding:6px 8px;line-height:1.35;border-left:0;border-right:0;",
      "border-bottom:", if (last) "0" else "1px solid #d7dde5", ";",
      "vertical-align:top;background:transparent;white-space:pre-line;overflow-wrap:anywhere;word-break:normal;",
      "font-variant-numeric:tabular-nums lining-nums;font-feature-settings:'tnum' 1,'lnum' 1;",
      "text-align:", if (names(table)[[index]] %in% c("DV", "Group", "Covariates", "Reason")) "left" else "center", ";"
    )
  }
  table_tag <- tags$table(
    class = "table shiny-table combined-model-overview-table ancova-model-overview-table",
    style = paste(
      "width:100%;max-width:100%;min-width:0;table-layout:fixed;",
      "border-collapse:collapse;border-spacing:0;border-top:2px solid #1f2937;border-bottom:2px solid #1f2937;",
      "color:#2f3a46;font-size:12px;background:transparent;"
    ),
    tags$colgroup(lapply(names(table), function(name) {
      tags$col(style = sprintf("width:%s;", width_for(name)))
    })),
    tags$thead(tags$tr(lapply(seq_along(names(table)), function(index) {
      tags$th(style = header_style(index), names(table)[[index]])
    }))),
    tags$tbody(lapply(seq_len(nrow(table)), function(row_index) {
      values <- table[row_index, , drop = TRUE]
      tags$tr(lapply(seq_along(values), function(index) {
        tags$td(style = body_style(index, row_index == nrow(table)), values[[index]])
      }))
    }))
  )
  result_table_with_notes(table_tag)
}

ancova_results_ui <- function(result, variable_table = NULL, labels = character(0)) {
  if (is.null(result)) {
    return(NULL)
  }
  if (!is.null(result$error)) {
    return(tags$div(class = "analysis-error", result$error))
  }
  sections <- list(
    tags$div(
      class = "result-section regression-result-panel ancova-model-overview-panel",
      tags$h3("Model overview"),
      ancova_model_overview_html_table(ancova_model_overview_table(result, variable_table, labels))
    )
  )
  combined_table <- ancova_combined_result_table(result, variable_table, labels)
  if (is.data.frame(combined_table) && nrow(combined_table) > 0) {
    sections[[length(sections) + 1L]] <- tags$div(
      class = "result-section regression-result-panel ancova-result-panel",
      tags$h3("ANCOVA table"),
      coefficient_html_table(
        combined_table,
        note_line = ancova_combined_note(result, variable_table, labels),
        compact = TRUE,
        compact_font_size = 13,
        compact_width = 82,
        compact_first_width = 72,
        compact_min_width = 720
      )
    )
  }
  assumption <- ancova_assumption_review_table(result, variable_table, labels)
  if (is.data.frame(assumption) && nrow(assumption) > 0) {
    sections[[length(sections) + 1L]] <- tags$div(
      class = "result-section regression-result-panel ancova-assumption-panel",
      tags$h3("Assumption summary"),
      model_overview_html_table(assumption)
    )
  }
  interaction_terms <- ancova_interaction_terms_review_table(result, variable_table, labels)
  if (is.data.frame(interaction_terms) && nrow(interaction_terms) > 0) {
    sections[[length(sections) + 1L]] <- tags$div(
      class = "result-section regression-result-panel ancova-interaction-terms-panel",
      tags$h3("Interaction terms"),
      model_overview_html_table(interaction_terms)
    )
  }
  simple_effects <- ancova_simple_effects_review_table(result, variable_table, labels)
  if (is.data.frame(simple_effects) && nrow(simple_effects) > 0) {
    sections[[length(sections) + 1L]] <- tags$div(
      class = "result-section regression-result-panel ancova-simple-effects-panel",
      tags$h3("Simple group effects"),
      model_overview_html_table(simple_effects)
    )
  }
  normality <- ancova_normality_review_table(result, variable_table, labels)
  if (is.data.frame(normality) && nrow(normality) > 0) {
    sections[[length(sections) + 1L]] <- tags$div(
      class = "result-section regression-result-panel ancova-normality-panel",
      tags$h3("Normality diagnostics"),
      model_overview_html_table(normality)
    )
  }
  linearity <- ancova_linearity_review_table(result, variable_table, labels)
  if (is.data.frame(linearity) && nrow(linearity) > 0) {
    sections[[length(sections) + 1L]] <- tags$div(
      class = "result-section regression-result-panel ancova-linearity-panel",
      tags$h3("Covariate linearity check"),
      model_overview_html_table(linearity)
    )
  }
  collinearity <- ancova_collinearity_review_table(result, variable_table, labels)
  if (is.data.frame(collinearity) && nrow(collinearity) > 0) {
    sections[[length(sections) + 1L]] <- tags$div(
      class = "result-section regression-result-panel ancova-collinearity-panel",
      tags$h3("Collinearity diagnostics"),
      model_overview_html_table(collinearity)
    )
  }
  influence <- ancova_influence_review_table(result, variable_table, labels)
  if (is.data.frame(influence) && nrow(influence) > 0) {
    sections[[length(sections) + 1L]] <- tags$div(
      class = "result-section regression-result-panel ancova-influence-panel",
      tags$h3("Influence diagnostics"),
      model_overview_html_table(influence)
    )
  }
  for (item in result$results %||% list()) {
    plot_sections <- ancova_plot_sections(item)
    if (length(plot_sections) > 0) {
      sections[[length(sections) + 1L]] <- tags$div(
        class = "result-section regression-result-panel ancova-plots-panel",
        tags$h3(sprintf("ANCOVA plots(%s)", display_variable_name_static(item$dependent, variable_table, labels, label_only = TRUE))),
        do.call(tagList, plot_sections)
      )
    }
  }
  diagnostics <- analysis_diagnostics_section(NULL, result$skipped, title = "Warnings / skipped models")
  if (!is.null(diagnostics)) {
    sections[[length(sections) + 1L]] <- diagnostics
  }
  do.call(tagList, sections)
}
