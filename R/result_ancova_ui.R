# ANCOVA result UI.

ancova_main_table <- function(table) {
  if (is.data.frame(table)) {
    attr(table, "result_table_role") <- "main"
    attr(table, "result_table_language") <- result_main_table_language()
  }
  table
}

ancova_appendix_text <- function(text, language = NULL) {
  language <- result_appendix_table_language(language)
  text <- as.character(text %||% "")
  if (identical(language, "en") || !nzchar(text)) {
    return(text)
  }
  if (!identical(language, "ko")) return(result_appendix_ui_text(text, language))
  korean <- c(
    "Model overview" = "\ubaa8\ud615 \uac1c\uc694",
    "Original-scale descriptive estimates" = "\uc6d0\ucc99\ub3c4 \uae30\uc220\ud1b5\uacc4 \ucd94\uc815\uce58",
    "Assumption summary" = "\uac00\uc815 \uc694\uc57d",
    "Regression slope homogeneity" = "\ud68c\uadc0\uae30\uc6b8\uae30 \ub3d9\uc9c8\uc131",
    "Normality diagnostics" = "\uc815\uaddc\uc131 \uc9c4\ub2e8",
    "Covariate linearity check" = "\uacf5\ubcc0\ub7c9 \uc120\ud615\uc131 \uac80\ud1a0",
    "Collinearity diagnostics" = "\ub2e4\uc911\uacf5\uc120\uc131 \uc9c4\ub2e8",
    "Influence diagnostics" = "\uc601\ud5a5\uce58 \uc9c4\ub2e8",
    "Influence sensitivity analysis" = "\uc601\ud5a5\uce58 \ubbfc\uac10\ub3c4 \ubd84\uc11d",
    "Warnings / skipped models" = "\uacbd\uace0 / \uc81c\uc678\ub41c \ubaa8\ud615",
    "ANCOVA plots" = "ANCOVA \uadf8\ub9bc",
    "Descriptive only. Estimates come from a separate unranked linear model and do not determine ranked-model inference." =
      "\uae30\uc220\ud1b5\uacc4 \uc6a9\ub3c4\uc785\ub2c8\ub2e4. \ucd94\uc815\uce58\ub294 \ubcc4\ub3c4\uc758 \ube44\uc21c\uc704 \uc120\ud615\ubaa8\ud615\uc5d0\uc11c \uacc4\uc0b0\ub418\uba70 \uc21c\uc704 \ubaa8\ud615\uc758 \ucd94\ub860\uc5d0 \uc0ac\uc6a9\ub418\uc9c0 \uc54a\uc2b5\ub2c8\ub2e4."
  )
  if (text %in% names(korean)) unname(korean[[text]]) else result_appendix_ui_text(text, language)
}

ancova_appendix_table <- function(table, language = NULL) {
  if (!is.data.frame(table)) return(table)
  language <- result_appendix_table_language(language)
  localized <- result_appendix_localize_table(table, language)
  if (!identical(language, "ko")) return(localized)
  header_map <- c(
    "DV" = "\uc885\uc18d\ubcc0\uc218", "Group" = "\uc9d1\ub2e8", "Covariates" = "\uacf5\ubcc0\ub7c9",
    "Raw N" = "\uc6d0\uc790\ub8cc N", "Complete N" = "\uc644\uc804\uc0ac\ub840 N", "Excluded N" = "\uc81c\uc678 N",
    "Sum of squares" = "\uc81c\uacf1\ud569", "Decision mode" = "\ud310\uc815 \ubc29\uc2dd",
    "Decision alpha" = "\ud310\uc815 \uc720\uc758\uc218\uc900", "Residual normality p" = "\uc794\ucc28 \uc815\uaddc\uc131 p",
    "Homogeneity test" = "\ub4f1\ubd84\uc0b0\uc131 \uac80\uc815", "Homogeneity p" = "\ub4f1\ubd84\uc0b0\uc131 p",
    "Slope p" = "\uae30\uc6b8\uae30 p", "Slope check" = "\uae30\uc6b8\uae30 \uac80\ud1a0",
    "Linearity" = "\uc120\ud615\uc131", "Collinearity" = "\ub2e4\uc911\uacf5\uc120\uc131", "Influence" = "\uc601\ud5a5\uce58",
    "Outcome method" = "\uacb0\uacfc\ubcc0\uc218 \ubc29\ubc95", "Outcome p" = "\uacb0\uacfc\ubcc0\uc218 p",
    "Residual method" = "\uc794\ucc28 \ubc29\ubc95", "Residual p" = "\uc794\ucc28 p", "Note" = "\ube44\uace0",
    "Covariate" = "\uacf5\ubcc0\ub7c9", "Quadratic p" = "\uc774\ucc28\ud56d p", "Status" = "\uc0c1\ud0dc",
    "Case" = "\uc0ac\ub840", "Studentized residual" = "\uc2a4\ud29c\ub358\ud2b8\ud654 \uc794\ucc28",
    "Leverage" = "\ub808\ubc84\ub9ac\uc9c0", "Cook's D" = "Cook's D", "Flag" = "\ud45c\uc2dc",
    "Excluded flagged cases" = "\uc81c\uc678\ub41c \ud45c\uc2dc \uc0ac\ub840", "partial eta2" = "\ubd80\ubd84 eta2"
  )
  current_names <- names(localized)
  english_names <- names(table)
  mapped <- vapply(seq_along(current_names), function(index) {
    key <- english_names[[index]]
    if (key %in% names(header_map)) unname(header_map[[key]]) else current_names[[index]]
  }, character(1))
  names(localized) <- mapped
  value_map <- c(
    "Auto switch" = "\uc790\ub3d9 \uc804\ud658", "Warn only" = "\uacbd\uace0\ub9cc",
    "Pass" = "\ud1b5\uacfc", "Review" = "\uac80\ud1a0", "Flag" = "\uc8fc\uc758",
    "Type I SS" = "\uc81cI\ud615 \uc81c\uacf1\ud569", "Type II SS" = "\uc81cII\ud615 \uc81c\uacf1\ud569", "Type III SS" = "\uc81cIII\ud615 \uc81c\uacf1\ud569",
    "No predictors" = "\uc608\uce21\ubcc0\uc218 \uc5c6\uc74c", "Rank deficient" = "\uc644\uc804\uc120\ud615\uc885\uc18d",
    "Not testable" = "\uac80\ud1a0 \ubd88\uac00", "severe collinearity" = "\uc2ec\uac01\ud55c \ub2e4\uc911\uacf5\uc120\uc131",
    "acceptable" = "\ud5c8\uc6a9 \ubc94\uc704", "possible nonlinearity" = "\ube44\uc120\ud615\uc131 \uac00\ub2a5\uc131",
    "flagged" = "\ud45c\uc2dc\ub428",
    "Outcome normality is descriptive; residual normality drives ANCOVA method selection." =
      "\uacb0\uacfc\ubcc0\uc218\uc758 \uc815\uaddc\uc131\uc740 \uae30\uc220\uc801\uc73c\ub85c \uc81c\uc2dc\ud558\uba70 ANCOVA \ubc29\ubc95 \uc120\ud0dd\uc740 \uc794\ucc28 \uc815\uaddc\uc131\uc744 \uae30\uc900\uc73c\ub85c \ud569\ub2c8\ub2e4."
  )
  for (index in seq_along(localized)) {
    values <- as.character(localized[[index]])
    matched <- values %in% names(value_map)
    values[matched] <- unname(value_map[values[matched]])
    values <- vapply(values, ancova_method_ui_label, character(1), language = language)
    values <- vapply(values, ancova_method_reason_ui_text, character(1), language = language)
    values <- vapply(values, function(value) {
      if (grepl("^High collinearity \\(max VIF=.+\\)$", value, perl = TRUE)) {
        return(sub("^High collinearity \\(max VIF=(.+)\\)$", "높은 다중공선성(최대 VIF=\\1)", value, perl = TRUE))
      }
      if (grepl("^Moderate collinearity \\(max VIF=.+\\)$", value, perl = TRUE)) {
        return(sub("^Moderate collinearity \\(max VIF=(.+)\\)$", "중간 수준 다중공선성(최대 VIF=\\1)", value, perl = TRUE))
      }
      if (grepl("^Acceptable \\(max VIF=.+\\)$", value, perl = TRUE)) {
        return(sub("^Acceptable \\(max VIF=(.+)\\)$", "허용 범위(최대 VIF=\\1)", value, perl = TRUE))
      }
      if (grepl("^Flagged cases=[0-9]+; max Cook's D=.+$", value, perl = TRUE)) {
        return(sub("^Flagged cases=([0-9]+); max Cook's D=(.+)$", "표시 사례=\\1; 최대 Cook's D=\\2", value, perl = TRUE))
      }
      value
    }, character(1))
    localized[[index]] <- values
  }
  attr(localized, "result_table_role") <- "appendix"
  attr(localized, "result_table_language") <- language
  result_appendix_preserve_data(localized, table)
}

ancova_model_overview_table <- function(result, variable_table = NULL, labels = character(0)) {
  rows <- lapply(result$results %||% list(), function(item) {
    data.frame(
      DV = display_variable_name_static(item$dependent, variable_table, labels, label_only = TRUE),
      Group = display_variable_name_static(item$factor, variable_table, labels, label_only = TRUE),
      Covariates = paste(vapply(item$covariates, display_variable_name_static, character(1), table = variable_table, labels = labels, label_only = TRUE), collapse = " + "),
      `Raw N` = item$raw_n %||% item$n,
      `Complete N` = item$complete_n %||% item$n,
      `Excluded N` = item$excluded_n %||% 0L,
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
      `Decision mode` = if (identical(as.character(item$options$auto_method %||% "auto"), "warn")) "Warn only" else "Auto switch",
      `Decision alpha` = format_decimal3(ancova_decision_alpha(item$options %||% list())),
      `Residual normality p` = format_p(item$assumptions$normality_p),
      `Homogeneity test` = item$assumptions$homogeneity_method %||% "Levene",
      `Homogeneity p` = format_p(item$assumptions$homogeneity_p),
      `Slope p` = format_p(item$assumptions$slope_p),
      `Slope check` = item$assumptions$slope_summary %||% "",
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

ancova_slope_homogeneity_review_table <- function(result, variable_table = NULL, labels = character(0)) {
  rows <- list()
  for (item in result$results %||% list()) {
    table <- item$assumptions$slope_table
    if (!is.data.frame(table) || nrow(table) == 0) next
    rows[[length(rows) + 1L]] <- data.frame(
      DV = display_variable_name_static(item$dependent, variable_table, labels, label_only = TRUE),
      Term = table$Term,
      df = result_format_df(ancova_format_decimal3_vec(table$df)),
      F = ancova_format_decimal3_vec(table$F),
      p = ancova_format_p_vec(table$p),
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
    if ("Covariate value" %in% names(table)) {
      covariate_value <- as.character(table$`Covariate value`)
      covariate_value[covariate_value == "Mean - 1 SD"] <- "M-SD"
      covariate_value[covariate_value == "Mean + 1 SD"] <- "M+SD"
      table$`Covariate value` <- covariate_value
    }
    display <- data.frame(
      DV = display_variable_name_static(item$dependent, variable_table, labels, label_only = TRUE),
      table,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    original_display <- display
    if ("DV" %in% names(display)) {
      display$DV[duplicated(original_display$DV)] <- ""
    }
    if (all(c("DV", "Covariate") %in% names(display))) {
      covariate_key <- paste(original_display$DV, original_display$Covariate, sep = "\r")
      display$Covariate[duplicated(covariate_key)] <- ""
    }
    if (all(c("DV", "Covariate", "Covariate value", "Value") %in% names(display))) {
      value_key <- paste(original_display$DV, original_display$Covariate, original_display$`Covariate value`, original_display$Value, sep = "\r")
      repeated_value <- duplicated(value_key)
      display$`Covariate value`[repeated_value] <- ""
      display$Value[repeated_value] <- ""
    }
    rows[[length(rows) + 1L]] <- display
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
    table$`Influence score` <- pmax(
      abs(suppressWarnings(as.numeric(table$`Studentized residual`))) / 3,
      suppressWarnings(as.numeric(table$Leverage)) / (item$influence$leverage_cutoff %||% NA_real_),
      suppressWarnings(as.numeric(table$`Cook's D`)) / (item$influence$cooks_cutoff %||% NA_real_),
      na.rm = TRUE
    )
    table$`Influence score`[!is.finite(table$`Influence score`)] <- 0
    table <- table[order(table$`Influence score`, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
    flagged_n <- nrow(table)
    display_limit <- 10L
    if (flagged_n > display_limit) {
      table <- table[seq_len(display_limit), , drop = FALSE]
    }
    rows[[length(rows) + 1L]] <- data.frame(
      DV = display_variable_name_static(item$dependent, variable_table, labels, label_only = TRUE),
      Case = table$Case,
      `Studentized residual` = ancova_format_decimal3_vec(table$`Studentized residual`),
      Leverage = ancova_format_decimal3_vec(table$Leverage),
      `Cook's D` = ancova_format_decimal3_vec(table$`Cook's D`),
      Flag = if (flagged_n > display_limit) {
        paste0("top ", display_limit, " of ", flagged_n, " flagged")
      } else {
        table$Flag
      },
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
  ttest_bind_result_rows(rows)
}

ancova_influence_sensitivity_review_table <- function(result, variable_table = NULL, labels = character(0)) {
  rows <- list()
  format_df <- function(values) {
    vapply(values, function(value) {
      value <- suppressWarnings(as.numeric(value))
      if (!is.finite(value)) return("")
      if (abs(value - round(value)) < 1e-8) return(as.character(as.integer(round(value))))
      ancova_format_decimal3_vec(value)
    }, character(1))
  }
  for (item in result$results %||% list()) {
    table <- item$influence_sensitivity
    if (!is.data.frame(table) || nrow(table) == 0) next
    rows[[length(rows) + 1L]] <- data.frame(
      DV = display_variable_name_static(item$dependent, variable_table, labels, label_only = TRUE),
      Model = table$Model,
      N = table$N,
      `Excluded flagged cases` = table$`Excluded flagged cases`,
      F = ancova_format_decimal3_vec(table$F),
      df1 = format_df(table$df1),
      df2 = format_df(table$df2),
      p = ancova_format_p_vec(table$p),
      `partial eta2` = ancova_format_decimal3_vec(table$`partial eta2`),
      Note = table$Note,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
  ttest_bind_result_rows(rows)
}

ancova_items_for_table_type <- function(items, table_type = "display") {
  table_type <- as.character(table_type %||% "display")
  is_ranked <- vapply(items, function(item) identical(item$method, "Ranked ANCOVA"), logical(1))
  if (identical(table_type, "rank") || identical(table_type, "original_scale")) {
    return(items[is_ranked])
  }
  items[!is_ranked]
}

ancova_combined_result_table <- function(result, variable_table = NULL, labels = character(0), table_type = "display") {
  items <- result$results %||% list()
  table_type <- as.character(table_type %||% "display")
  items <- ancova_items_for_table_type(items, table_type)
  note_spec <- ancova_combined_note_spec(result, variable_table, labels, table_type = table_type)
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
    table <- if (identical(table_type, "rank")) {
      item$table
    } else if (identical(table_type, "original_scale")) {
      item$original_scale_table %||% data.frame()
    } else {
      item$display_table %||% item$table
    }
    if (identical(table_type, "rank") && "Adjusted rank mean" %in% names(table)) {
      names(table)[names(table) == "Adjusted rank mean"] <- "Rank M"
    }
    if (!is.data.frame(table) || nrow(table) == 0) {
      return(NULL)
    }
    existing_markers <- attr(table, "note_markers", exact = TRUE)
    if (is.data.frame(existing_markers) && nrow(existing_markers) > 0) {
      for (marker_index in seq_len(nrow(existing_markers))) {
        add_marker(
          row_offset + as.integer(existing_markers$row[[marker_index]]),
          as.character(existing_markers$column[[marker_index]]),
          as.character(existing_markers$marker[[marker_index]])
        )
      }
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
    if ("p" %in% names(table)) add_marker(first_row, "p", paste(item_markers, collapse = ","))
    row_offset <<- row_offset + nrow(table)
    table
  })
  out <- ttest_bind_result_rows(rows)
  if (!identical(table_type, "rank") && "Adjusted mean" %in% names(out)) {
    names(out)[names(out) == "Adjusted mean"] <- "M"
  }
  if (!identical(table_type, "rank") && "Adjusted rank mean" %in% names(out)) {
    names(out)[names(out) == "Adjusted rank mean"] <- "Rank M"
  }
  if (identical(table_type, "rank") && "Rank mean" %in% names(out)) {
    names(out)[names(out) == "Rank mean"] <- "Rank M"
  }
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

ancova_combined_note_spec <- function(result, variable_table = NULL, labels = character(0), table_type = "display") {
  items <- ancova_items_for_table_type(result$results %||% list(), table_type)
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
  data_lines <- character(0)
  decision_lines <- character(0)
  posthoc_lines <- character(0)
  effect_lines <- character(0)
  display_lines <- character(0)

  method_note_text <- function(method) {
    if (identical(method, "Robust ANCOVA (HC3)")) {
      return("Robust ANCOVA (HC3): group effect F, p, and post-hoc contrasts use HC3 robust covariance; Type II/III robust tests use the selected sum-of-squares type.")
    }
    if (identical(method, "Ranked ANCOVA")) {
      return("Ranked ANCOVA: the dependent variable and continuous covariates are rank-transformed; adjusted means and post-hoc contrasts are interpreted on the rank scale, not the original outcome scale.")
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

  complete_keys <- unique_text(vapply(items, function(item) {
    ancova_complete_case_note(item$raw_n %||% item$n, item$complete_n %||% item$n, item$excluded_n %||% 0L)
  }, character(1)))
  if (length(complete_keys) == 1L) {
    data_lines <- c(data_lines, complete_keys[[1]])
  } else if (length(complete_keys) > 1L) {
    data_lines <- c(data_lines, paste(complete_keys, collapse = " "))
  }

  decision_keys <- unique_text(vapply(items, function(item) {
    ancova_decision_rule_note(item$assumptions, item$options %||% list())
  }, character(1)))
  if (length(decision_keys) == 1L) {
    decision_lines <- c(decision_lines, decision_keys[[1]])
  } else if (length(decision_keys) > 1L) {
    decision_lines <- c(decision_lines, paste(decision_keys, collapse = " "))
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
    effect_lines <- c(effect_lines, sprintf("%s. ES = effect size (%s).", markers$effect, paste(effect_labels, collapse = "; ")))
  } else {
    effect_lines <- c(effect_lines, sprintf("ES = effect size (%s).", if (length(effect_labels) > 0) effect_labels[[1]] else "partial eta squared"))
  }
  if (any(vapply(items, function(item) isTRUE(item$options$mean_se), logical(1)))) {
    if (any(vapply(items, function(item) identical(item$method, "Ranked ANCOVA"), logical(1)))) {
      display_lines <- c(display_lines, "Rank M \u00B1 SE = adjusted rank mean \u00B1 standard error.")
    }
    if (any(vapply(items, function(item) !identical(item$method, "Ranked ANCOVA"), logical(1)))) {
      display_lines <- c(display_lines, "M \u00B1 SE = adjusted mean \u00B1 standard error.")
    }
  } else {
    if (any(vapply(items, function(item) identical(item$method, "Ranked ANCOVA"), logical(1)))) {
      display_lines <- c(display_lines, "Rank M = adjusted rank mean; SE = standard error.")
    }
    if (any(vapply(items, function(item) !identical(item$method, "Ranked ANCOVA"), logical(1)))) {
      display_lines <- c(display_lines, "M = adjusted mean; SE = standard error.")
    }
  }
  list(
    lines = c(method_lines, effect_lines, posthoc_lines, display_lines, ss_lines, data_lines, decision_lines),
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

ancova_combined_note <- function(result, variable_table = NULL, labels = character(0), table_type = "display") {
  spec <- ancova_combined_note_spec(result, variable_table, labels, table_type = table_type)
  lines <- spec$lines %||% character(0)
  pick <- function(pattern) lines[grepl(pattern, lines, ignore.case = TRUE, perl = TRUE)]
  result_sci_note_text(
    format = pick("^(?:[0-9]+\\.\\s*)?(?:M|Rank M).*SE"),
    abbreviations = pick("ES = effect size"),
    estimation = pick("Analysis method:|Robust ANCOVA|Ranked ANCOVA|Interaction ANCOVA|Sum of squares: Type|Type I{1,3} SS"),
    multiplicity = pick("Post-hoc:")
  )
}

ancova_result_has_plots <- function(item) {
  options <- item$options %||% list()
  isTRUE(options$plot_adjusted_means) ||
    isTRUE(options$plot_raw_overlay) ||
    isTRUE(options$plot_regression_lines) ||
    isTRUE(options$plot_linearity_diagnostics)
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
  graphics::par(mar = c(5, 4.2, 3.5, 1), cex.axis = .85)
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
  graphics::par(mar = c(5, 4.2, 3.5, 1), cex.axis = .85)
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
      # Fixed display-only offsets: repeated screen/export renders must agree
      # without consuming the analysis random-number stream.
      index + .16 * ((seq_along(values) * ((sqrt(5) - 1) / 2)) %% 1 - .5),
      values,
      pch = 16,
      cex = .45,
      col = grDevices::adjustcolor("#334155", alpha.f = .35)
    )
  }
  match_index <- match(adjusted$Level, levels)
  graphics::points(match_index, adjusted$Estimate, pch = 18, cex = 1.35, col = "#b91c1c")
  graphics::lines(match_index, adjusted$Estimate, col = "#b91c1c", lwd = 1.1)
  graphics::legend("topright", inset = c(0, -.05), xpd = NA, legend = if (identical(item$method, "Ranked ANCOVA")) "Adjusted rank mean" else "Adjusted mean", pch = 18, col = "#b91c1c", bty = "n", cex = .8)
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

ancova_numeric_covariates <- function(item) {
  item$covariates[vapply(item$covariates, function(name) is.numeric(item$fit_data[[name]]), logical(1))]
}

draw_ancova_linearity_diagnostic_plot <- function(item) {
  covariate <- as.character(item$linearity_covariate %||% "")
  if (!nzchar(covariate) || !covariate %in% names(item$fit_data) || !is.numeric(item$fit_data[[covariate]])) {
    graphics::plot.new()
    graphics::text(.5, .5, "No continuous covariate available")
    return(invisible(NULL))
  }
  x <- suppressWarnings(as.numeric(item$fit_data[[covariate]]))
  residuals <- suppressWarnings(as.numeric(stats::residuals(item$model)))
  keep <- is.finite(x) & is.finite(residuals)
  if (sum(keep) < 4L) {
    graphics::plot.new()
    graphics::text(.5, .5, "Not enough data for linearity diagnostic")
    return(invisible(NULL))
  }
  group_values <- factor(item$fit_data[[item$factor]], levels = levels(item$fit_data[[item$factor]]))
  colors <- ancova_plot_palette(length(levels(group_values)))
  point_colors <- colors[pmax(1L, match(as.character(group_values), levels(group_values)))]
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mar = c(5, 4.2, 2, 1), cex.axis = .85)
  graphics::plot(
    x[keep],
    residuals[keep],
    pch = 16,
    cex = .48,
    col = grDevices::adjustcolor(point_colors[keep], alpha.f = .48),
    xlab = covariate,
    ylab = if (identical(item$method, "Ranked ANCOVA")) "Model residuals (rank scale)" else "Model residuals",
    main = ""
  )
  graphics::abline(h = 0, col = "#475569", lwd = .9)
  if (sum(keep) >= 8L && length(unique(x[keep])) >= 6L) {
    smooth_data <- data.frame(x = x[keep], residual = residuals[keep])
    smooth <- tryCatch(stats::loess(residual ~ x, data = smooth_data, span = .75), error = function(e) NULL)
    if (!is.null(smooth)) {
      x_grid <- seq(min(x[keep], na.rm = TRUE), max(x[keep], na.rm = TRUE), length.out = 100)
      y_grid <- tryCatch(stats::predict(smooth, newdata = data.frame(x = x_grid)), error = function(e) rep(NA_real_, length(x_grid)))
      if (any(is.finite(y_grid))) {
        graphics::lines(x_grid, y_grid, col = "#b91c1c", lwd = 1.8)
      }
    }
  }
  graphics::legend("topright", legend = c(levels(group_values), "loess"), col = c(colors, "#b91c1c"), pch = c(rep(16, length(colors)), NA), lty = c(rep(NA, length(colors)), 1), lwd = c(rep(NA, length(colors)), 1.8), bty = "n", cex = .75)
}

ancova_plot_sections <- function(item, plot_renderer = plot_data_uri) {
  options <- item$options %||% list()
  sections <- list()
  add_plot <- function(title, plot_function, plot_item = item) {
    sections[[length(sections) + 1L]] <<- tags$div(
      class = "ancova-plot-card",
      tags$h4(title),
      tags$img(
        class = "analysis-plot-image",
        style = "display:block;width:100%;max-width:640px;height:auto;",
        src = plot_renderer(plot_function, plot_item, width = 1700, height = 1250, res = 300),
        width = 1700,
        height = 1250,
        alt = title
      )
    )
  }
  adjusted_title <- if (identical(item$method, "Ranked ANCOVA")) "Adjusted rank mean error bar plot (95% CI)" else "Adjusted mean error bar plot (95% CI)"
  overlay_title <- if (identical(item$method, "Ranked ANCOVA")) "Ranked data + adjusted rank mean overlay" else "Raw data + adjusted mean overlay"
  if (isTRUE(options$plot_adjusted_means)) add_plot(adjusted_title, draw_ancova_adjusted_mean_plot)
  if (isTRUE(options$plot_raw_overlay)) add_plot(overlay_title, draw_ancova_raw_overlay_plot)
  if (isTRUE(options$plot_regression_lines)) add_plot("Covariate-adjusted regression lines", draw_ancova_regression_lines_plot)
  if (isTRUE(options$plot_linearity_diagnostics)) {
    for (covariate in ancova_numeric_covariates(item)) {
      plot_item <- item
      plot_item$linearity_covariate <- covariate
      add_plot(sprintf("Linearity diagnostic: residuals vs %s", covariate),
               draw_ancova_linearity_diagnostic_plot, plot_item)
    }
  }
  sections
}

ancova_model_overview_html_table <- function(
  table,
  extra_class = "ancova-model-overview-transposed",
  table_role = "appendix",
  table_language = NULL,
  note_line = NULL
) {
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(NULL)
  }
  table_role <- result_table_role(table_role, table)
  table_language <- result_table_language(table_role, table_language)
  display_table <- if (identical(table_role, "appendix")) ancova_appendix_table(table, table_language) else ancova_main_table(table)
  dv_index <- match("DV", names(table))
  dependent_headers <- if (is.finite(dv_index)) {
    as.character(display_table[[dv_index]])
  } else {
    paste0("Model ", seq_len(nrow(display_table)))
  }
  dependent_headers[!nzchar(dependent_headers)] <- paste0("Model ", which(!nzchar(dependent_headers)))
  field_indices <- if (is.finite(dv_index)) setdiff(seq_along(display_table), dv_index) else seq_along(display_table)
  field_names <- names(display_table)[field_indices]
  transposed <- data.frame(
    Item = field_names,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  if (identical(table_role, "appendix")) names(transposed)[[1L]] <- result_appendix_ui_text("Item", table_language)
  for (row_index in seq_len(nrow(display_table))) {
    column_name <- dependent_headers[[row_index]]
    if (column_name %in% names(transposed)) {
      column_name <- paste0(column_name, " ", row_index)
    }
    transposed[[column_name]] <- vapply(field_indices, function(field_index) {
      as.character(display_table[[field_index]][[row_index]] %||% "")
    }, character(1))
  }

  header_style <- function(index) {
    paste(
      "padding:6px 8px;line-height:1.3;border-left:0;border-right:0;border-bottom:2px solid #1f2937;",
      "vertical-align:middle;font-weight:700;background:transparent;white-space:normal;overflow-wrap:anywhere;",
      "text-align:", if (index == 1L) "left" else "center", ";"
    )
  }
  body_style <- function(index, last) {
    paste(
      "padding:6px 8px;line-height:1.35;border-left:0;border-right:0;",
      "border-bottom:", if (last) "0" else "1px solid #d7dde5", ";",
      "vertical-align:top;background:transparent;white-space:pre-line;overflow-wrap:anywhere;word-break:normal;",
      "font-variant-numeric:tabular-nums lining-nums;font-feature-settings:'tnum' 1,'lnum' 1;",
      "font-weight:", if (index == 1L) "700" else "400", ";",
      "text-align:", if (index == 1L) "left" else "center", ";"
    )
  }
  table_tag <- tags$table(
    class = paste("table shiny-table combined-model-overview-table ancova-model-overview-table", extra_class),
    style = paste(
      "width:100%;max-width:100%;min-width:0;table-layout:fixed;",
      "border-collapse:collapse;border-spacing:0;border-top:2px solid #1f2937;border-bottom:2px solid #1f2937;",
      "color:#2f3a46;font-size:12px;background:transparent;"
    ),
    tags$colgroup(c(
      list(tags$col(style = "width:132px;")),
      lapply(seq_len(ncol(transposed) - 1L), function(unused) tags$col(style = "width:auto;"))
    )),
    tags$thead(tags$tr(lapply(seq_along(names(transposed)), function(index) {
      tags$th(style = header_style(index), names(transposed)[[index]])
    }))),
    tags$tbody(lapply(seq_len(nrow(transposed)), function(row_index) {
      values <- transposed[row_index, , drop = TRUE]
      tags$tr(lapply(seq_along(values), function(index) {
        tags$td(style = body_style(index, row_index == nrow(transposed)), values[[index]])
      }))
    }))
  )
  contract <- result_table_contract(
    transposed,
    role = table_role,
    language = table_language,
    intrinsic_width = result_table_intrinsic_width(transposed, first_width = 132, default_width = 118, min_width = 480)
  )
  result_table_with_notes(
    result_table_apply_contract(table_tag, contract),
    result_note_tag(note_line)
  )
}

ancova_normality_html_table <- function(table, table_role = "appendix", table_language = NULL, note_line = NULL) {
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(NULL)
  }
  table_role <- result_table_role(table_role, table)
  table_language <- result_table_language(table_role, table_language)
  display_table <- if (identical(table_role, "appendix")) ancova_appendix_table(table, table_language) else ancova_main_table(table)
  original_names <- names(table)
  display_names <- names(display_table)
  header_label <- function(name, label) {
    switch(
      name,
      `Outcome method` = HTML(gsub(" ", "<br>", label, fixed = TRUE)),
      `Residual method` = HTML(gsub(" ", "<br>", label, fixed = TRUE)),
      label
    )
  }
  column_width <- function(name) {
    switch(
      name,
      DV = "48px",
      `Outcome method` = "82px",
      `Outcome p` = "64px",
      `Residual method` = "82px",
      `Residual p` = "64px",
      Note = "auto",
      "80px"
    )
  }
  header_style <- function(name) {
    paste(
      "padding:6px 8px;line-height:1.2;border-left:0;border-right:0;border-bottom:2px solid #1f2937;",
      "vertical-align:middle;font-weight:700;background:transparent;white-space:normal;overflow-wrap:normal;word-break:normal;",
      "text-align:", if (identical(name, "Note")) "left" else "center", ";"
    )
  }
  body_style <- function(name, last) {
    paste(
      "padding:6px 8px;line-height:1.3;border-left:0;border-right:0;",
      "border-bottom:", if (last) "0" else "1px solid #d7dde5", ";",
      "vertical-align:middle;background:transparent;white-space:normal;overflow-wrap:normal;word-break:normal;",
      "font-variant-numeric:tabular-nums lining-nums;font-feature-settings:'tnum' 1,'lnum' 1;",
      "text-align:", if (identical(name, "Note")) "left" else "center", ";"
    )
  }
  body_content <- function(name, value) {
    value <- as.character(value %||% "")
    if (name %in% c("Outcome method", "Residual method") && identical(value, "Lilliefors (K-S)")) {
      return(HTML("Lilliefors<br>(K-S)"))
    }
    value
  }
  table_tag <- tags$table(
    class = "table shiny-table combined-model-overview-table ancova-normality-diagnostics-table",
    style = paste(
      "width:100%;max-width:100%;min-width:0;table-layout:fixed;",
      "border-collapse:collapse;border-spacing:0;border-top:2px solid #1f2937;border-bottom:2px solid #1f2937;",
      "color:#2f3a46;font-size:12px;background:transparent;"
    ),
    tags$colgroup(lapply(original_names, function(name) {
      tags$col(style = sprintf("width:%s;", column_width(name)))
    })),
    tags$thead(tags$tr(lapply(seq_along(original_names), function(index) {
      name <- original_names[[index]]
      tags$th(style = header_style(name), header_label(name, display_names[[index]]))
    }))),
    tags$tbody(lapply(seq_len(nrow(display_table)), function(row_index) {
      values <- display_table[row_index, , drop = TRUE]
      tags$tr(lapply(seq_along(values), function(index) {
        name <- original_names[[index]]
        tags$td(style = body_style(name, row_index == nrow(display_table)), body_content(name, values[[index]]))
      }))
    }))
  )
  explicit_widths <- vapply(original_names, function(name) {
    value <- sub("px$", "", column_width(name))
    suppressWarnings(as.numeric(value))
  }, numeric(1))
  intrinsic_width <- if (all(is.finite(explicit_widths))) sum(explicit_widths) else result_table_intrinsic_width(display_table, min_width = 480)
  contract <- result_table_contract(display_table, role = table_role, language = table_language, intrinsic_width = max(480, intrinsic_width))
  result_table_with_notes(result_table_apply_contract(table_tag, contract), result_note_tag(note_line))
}

ancova_diagnostics_html_table <- function(
  table,
  extra_class,
  widths,
  wrap_headers = list(),
  sortable = character(0),
  left_columns = character(0),
  table_role = "appendix",
  table_language = NULL,
  note_line = NULL
) {
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(NULL)
  }
  table_role <- result_table_role(table_role, table)
  table_language <- result_table_language(table_role, table_language)
  display_table <- if (identical(table_role, "appendix")) ancova_appendix_table(table, table_language) else ancova_main_table(table)
  names_table <- names(table)
  display_names <- names(display_table)
  header_label <- function(name, label) {
    if (name %in% names(wrap_headers)) {
      english_label <- as.character(wrap_headers[[name]])
      if (identical(table_language, "en")) return(HTML(english_label))
      return(HTML(gsub(" ", "<br>", label, fixed = TRUE)))
    }
    label
  }
  column_width <- function(name) {
    widths[[name]] %||% "auto"
  }
  is_left <- function(name) {
    name %in% left_columns
  }
  is_numeric_like <- function(name) {
    name %in% c("Case", "df", "F", "p", "Quadratic p", "Value", "Estimate", "SE", "t", "Leverage", "Cook's D", "Studentized residual")
  }
  cell_align <- function(name) {
    if (is_left(name)) return("left")
    if (is_numeric_like(name)) return("right")
    "center"
  }
  sort_type <- function(name) {
    if (name %in% c("Case", "df", "F", "p", "Quadratic p", "Leverage", "Cook's D", "Studentized residual")) {
      "numeric"
    } else {
      "text"
    }
  }
  sort_default <- function(name) {
    if (identical(name, "Case")) "asc" else "desc"
  }
  header_style <- function(name) {
    paste(
      "padding:6px 8px;line-height:1.2;border-left:0;border-right:0;border-bottom:2px solid #1f2937;",
      "vertical-align:middle;font-weight:700;background:transparent;white-space:normal;overflow-wrap:normal;word-break:normal;",
      "text-align:", cell_align(name), ";"
    )
  }
  body_style <- function(name, last) {
    wrap_style <- if (is_numeric_like(name)) {
      "white-space:nowrap;overflow-wrap:normal;word-break:normal;"
    } else {
      "white-space:normal;overflow-wrap:anywhere;word-break:normal;"
    }
    paste(
      "padding:6px 8px;line-height:1.3;border-left:0;border-right:0;",
      "border-bottom:", if (last) "0" else "1px solid #d7dde5", ";",
      "vertical-align:middle;background:transparent;", wrap_style,
      "font-variant-numeric:tabular-nums lining-nums;font-feature-settings:'tnum' 1,'lnum' 1;",
      "text-align:", cell_align(name), ";"
    )
  }
  table_tag <- tags$table(
    class = paste("table shiny-table combined-model-overview-table ancova-diagnostics-table", extra_class),
    style = paste(
      "width:100%;max-width:100%;min-width:0;table-layout:fixed;",
      "border-collapse:collapse;border-spacing:0;border-top:2px solid #1f2937;border-bottom:2px solid #1f2937;",
      "color:#2f3a46;font-size:12px;background:transparent;"
    ),
    tags$colgroup(lapply(names_table, function(name) {
      tags$col(style = sprintf("width:%s;", column_width(name)))
    })),
    tags$thead(tags$tr(lapply(seq_along(names_table), function(index) {
      name <- names_table[[index]]
      label <- header_label(name, display_names[[index]])
      content <- if (name %in% sortable) {
        tags$button(
          type = "button",
          class = "ancova-sort-button",
          `data-sort-column` = index,
          `data-sort-type` = sort_type(name),
          `data-sort-default` = sort_default(name),
          label,
          tags$span(class = "ancova-sort-indicator", "\u25be")
        )
      } else {
        label
      }
      tags$th(style = header_style(name), content)
    }))),
    tags$tbody(lapply(seq_len(nrow(display_table)), function(row_index) {
      values <- display_table[row_index, , drop = TRUE]
      tags$tr(lapply(seq_along(values), function(index) {
        name <- names_table[[index]]
        tags$td(style = body_style(name, row_index == nrow(display_table)), values[[index]])
      }))
    }))
  )
  explicit_widths <- vapply(names_table, function(name) {
    value <- sub("px$", "", column_width(name))
    suppressWarnings(as.numeric(value))
  }, numeric(1))
  intrinsic_width <- if (all(is.finite(explicit_widths))) sum(explicit_widths) else result_table_intrinsic_width(display_table, min_width = 480)
  contract <- result_table_contract(display_table, role = table_role, language = table_language, intrinsic_width = max(480, intrinsic_width))
  result_table_with_notes(result_table_apply_contract(table_tag, contract), result_note_tag(note_line))
}

ancova_observed_descriptives <- function(result, variable_table = NULL, labels = character(0)) {
  rows <- lapply(result$results %||% list(), function(item) {
    data <- item$clean_data
    if (!is.data.frame(data) || !nrow(data)) return(NULL)
    groups <- split(data[[item$dependent]], data[[item$factor]], drop = TRUE)
    data.frame(DV = display_variable_name_static(item$dependent, variable_table, labels, label_only = TRUE),
      Variable = display_variable_name_static(item$factor, variable_table, labels, label_only = TRUE),
      Group = names(groups), N = lengths(groups),
      `M ± SD` = vapply(groups, function(x) paste(format_decimal3(mean(x)), "±", format_decimal3(stats::sd(x))), character(1)),
      check.names = FALSE, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

ancova_error_ui_text <- function(message, language = result_appendix_table_language()) {
  labels <- c(`ANCOVA requires at least four complete cases.` = "ANCOVA에는 완전 사례가 최소 4개 필요합니다.",
    `Grouping variable must have at least two observed levels.` = "집단변수에는 관측된 수준이 최소 2개 필요합니다.",
    `Select at least one covariate.` = "공변량을 하나 이상 선택하세요.",
    `No data frame is available for ANCOVA.` = "ANCOVA에 사용할 데이터가 없습니다.")
  vapply(as.character(message), function(x) {
    if (!is.na(x) && x %in% names(labels)) statedu_localized_text(language, x, unname(labels[[x]])) else x
  }, character(1), USE.NAMES = FALSE)
}

ancova_results_ui <- function(result, variable_table = NULL, labels = character(0), plot_renderer = plot_data_uri) {
  if (is.null(result)) {
    return(NULL)
  }
  if (!is.null(result$error)) {
    return(tags$div(class = "analysis-error", ancova_error_ui_text(result$error)))
  }
  appendix_language <- result_appendix_table_language()
  sections <- list(
    tags$div(
      class = "result-section regression-result-panel ancova-model-overview-panel",
      tags$h3(ancova_appendix_text("Model overview", appendix_language)),
      ancova_model_overview_html_table(
        ancova_model_overview_table(result, variable_table, labels),
        table_role = "appendix",
        table_language = appendix_language
      )
    )
  )
  combined_table <- ancova_main_table(ancova_combined_result_table(result, variable_table, labels))
  if (is.data.frame(combined_table) && nrow(combined_table) > 0) {
    sections[[length(sections) + 1L]] <- tags$div(
      class = "result-section regression-result-panel ancova-result-panel",
      tags$h3("ANCOVA table"),
      coefficient_html_table(
        combined_table,
        sheet_orientation = "portrait",
        note_line = ancova_combined_note(result, variable_table, labels, table_type = "display"),
        compact = TRUE,
        compact_font_size = 12,
        compact_width = 58,
        compact_first_width = 92,
        compact_min_width = 480,
        table_role = "main"
      )
    )
  }
  ranked_table <- ancova_main_table(ancova_combined_result_table(result, variable_table, labels, table_type = "rank"))
  if (is.data.frame(ranked_table) && nrow(ranked_table) > 0) {
    sections[[length(sections) + 1L]] <- tags$div(
      class = "result-section regression-result-panel ancova-result-panel ancova-ranked-result-panel",
      tags$h3("Ranked ANCOVA table"),
      coefficient_html_table(
        ranked_table,
        sheet_orientation = "portrait",
        note_line = ancova_combined_note(result, variable_table, labels, table_type = "rank"),
        compact = TRUE,
        compact_font_size = 12,
        compact_width = 58,
        compact_first_width = 92,
        compact_min_width = 480,
        table_role = "main"
      )
    )
  }
  original_scale_table <- ancova_combined_result_table(result, variable_table, labels, table_type = "original_scale")
  if (is.data.frame(original_scale_table) && nrow(original_scale_table) > 0) {
    sections[[length(sections) + 1L]] <- tags$div(
      class = "result-section regression-result-panel ancova-result-panel ancova-original-scale-descriptive-panel",
      tags$h3(ancova_appendix_text("Original-scale descriptive estimates", appendix_language)),
      coefficient_html_table(
        ancova_appendix_table(original_scale_table, appendix_language),
        note_line = ancova_appendix_text(
          "Descriptive only. Estimates come from a separate unranked linear model and do not determine ranked-model inference.",
          appendix_language
        ),
        compact = TRUE,
        compact_font_size = 12,
        compact_width = 58,
        compact_first_width = 92,
        compact_min_width = 480,
        table_role = "appendix",
        table_language = appendix_language
      )
    )
  }
  observed <- ancova_observed_descriptives(result, variable_table, labels)
  if (is.data.frame(observed) && nrow(observed)) sections[[length(sections) + 1L]] <- tags$div(
    class = "result-section regression-result-panel ancova-observed-descriptive-panel",
    tags$h3(statedu_localized_text(appendix_language, "Appendix: observed descriptive statistics", "부록: 관측값 기술통계")),
    coefficient_html_table(ancova_appendix_table(observed, appendix_language), table_role = "appendix", sheet_orientation = "portrait",
      note_line = statedu_localized_text(appendix_language, "Unadjusted observed mean ± SD in the complete-case analysis sample.", "분석에 사용한 완전 사례의 보정 전 관측 평균 ± 표준편차입니다.")))
  assumption <- ancova_assumption_review_table(result, variable_table, labels)
  if (is.data.frame(assumption) && nrow(assumption) > 0) {
    sections[[length(sections) + 1L]] <- tags$div(
      class = "result-section regression-result-panel ancova-assumption-panel",
      tags$h3(ancova_appendix_text("Assumption summary", appendix_language)),
      ancova_model_overview_html_table(
        assumption,
        "ancova-assumption-summary-transposed",
        table_role = "appendix",
        table_language = appendix_language
      )
    )
  }
  interaction_terms <- ancova_interaction_terms_review_table(result, variable_table, labels)
  if (is.data.frame(interaction_terms) && nrow(interaction_terms) > 0) {
    sections[[length(sections) + 1L]] <- tags$div(
      class = "result-section regression-result-panel ancova-interaction-terms-panel",
      tags$h3("Interaction terms"),
      coefficient_html_table(
        ancova_main_table(interaction_terms),
        note_line = result_sci_note_text(
          estimation = "Interaction terms are group-by-covariate effects",
          multiplicity = "p values are two-sided"
        ),
        compact = TRUE,
        compact_font_size = 12,
        compact_width = 58,
        compact_first_width = 92,
        compact_min_width = 480,
        table_role = "main"
      )
    )
  }
  slope_homogeneity <- ancova_slope_homogeneity_review_table(result, variable_table, labels)
  if (is.data.frame(slope_homogeneity) && nrow(slope_homogeneity) > 0) {
    sections[[length(sections) + 1L]] <- tags$div(
      class = "result-section regression-result-panel ancova-slope-homogeneity-panel",
      tags$h3(ancova_appendix_text("Regression slope homogeneity", appendix_language)),
      ancova_diagnostics_html_table(
        slope_homogeneity,
        extra_class = "ancova-slope-homogeneity-table",
        widths = list(DV = "48px", Term = "110px", df = "56px", F = "66px", p = "58px", Status = "190px"),
        left_columns = c("Term", "Status"),
        table_role = "appendix",
        table_language = appendix_language
      )
    )
  }
  simple_effects <- ancova_simple_effects_review_table(result, variable_table, labels)
  if (is.data.frame(simple_effects) && nrow(simple_effects) > 0) {
    sections[[length(sections) + 1L]] <- tags$div(
      class = "result-section regression-result-panel ancova-simple-effects-panel",
      tags$h3("Simple group effects"),
      ancova_diagnostics_html_table(
        simple_effects,
        extra_class = "ancova-simple-effects-table",
        widths = list(DV = "44px", Covariate = "76px", `Covariate value` = "78px", Value = "62px", Contrast = "64px", Estimate = "72px", SE = "54px", t = "62px", p = "62px"),
        wrap_headers = list(`Covariate value` = "Covariate<br>value"),
        left_columns = c("Covariate", "Covariate value"),
        table_role = "main",
        table_language = "en",
        note_line = result_sci_note_text(
          abbreviations = "M = mean; SD = standard deviation; SE = standard error",
          estimation = "Estimates are covariate-adjusted simple group contrasts",
          multiplicity = "p values are two-sided"
        )
      )
    )
  }
  normality <- ancova_normality_review_table(result, variable_table, labels)
  if (is.data.frame(normality) && nrow(normality) > 0) {
    sections[[length(sections) + 1L]] <- tags$div(
      class = "result-section regression-result-panel ancova-normality-panel",
      tags$h3(ancova_appendix_text("Normality diagnostics", appendix_language)),
      ancova_normality_html_table(normality, table_role = "appendix", table_language = appendix_language)
    )
  }
  linearity <- ancova_linearity_review_table(result, variable_table, labels)
  if (is.data.frame(linearity) && nrow(linearity) > 0) {
    sections[[length(sections) + 1L]] <- tags$div(
      class = "result-section regression-result-panel ancova-linearity-panel",
      tags$h3(ancova_appendix_text("Covariate linearity check", appendix_language)),
      ancova_diagnostics_html_table(
        linearity,
        extra_class = "ancova-linearity-table",
        widths = list(DV = "48px", Covariate = "96px", `Quadratic p` = "92px", Status = "170px"),
        left_columns = c("Covariate", "Status"),
        table_role = "appendix",
        table_language = appendix_language
      )
    )
  }
  collinearity <- ancova_collinearity_review_table(result, variable_table, labels)
  if (is.data.frame(collinearity) && nrow(collinearity) > 0) {
    sections[[length(sections) + 1L]] <- tags$div(
      class = "result-section regression-result-panel ancova-collinearity-panel",
      tags$h3(ancova_appendix_text("Collinearity diagnostics", appendix_language)),
      model_overview_html_table(ancova_appendix_table(collinearity, appendix_language))
    )
  }
  influence <- ancova_influence_review_table(result, variable_table, labels)
  if (is.data.frame(influence) && nrow(influence) > 0) {
    sections[[length(sections) + 1L]] <- tags$div(
      class = "result-section regression-result-panel ancova-influence-panel",
      tags$h3(ancova_appendix_text("Influence diagnostics", appendix_language)),
      ancova_diagnostics_html_table(
        influence,
        extra_class = "ancova-influence-table",
        widths = list(DV = "48px", Case = "64px", `Studentized residual` = "96px", Leverage = "76px", `Cook's D` = "82px", Flag = "142px"),
        wrap_headers = list(`Studentized residual` = "Studentized<br>residual"),
        sortable = c("Case", "Studentized residual", "Leverage", "Cook's D"),
        left_columns = c("Flag"),
        table_role = "appendix",
        table_language = appendix_language
      )
    )
  }
  influence_sensitivity <- ancova_influence_sensitivity_review_table(result, variable_table, labels)
  if (is.data.frame(influence_sensitivity) && nrow(influence_sensitivity) > 0) {
    sections[[length(sections) + 1L]] <- tags$div(
      class = "result-section regression-result-panel ancova-influence-sensitivity-panel",
      tags$h3(ancova_appendix_text("Influence sensitivity analysis", appendix_language)),
      ancova_diagnostics_html_table(
        influence_sensitivity,
        extra_class = "ancova-influence-sensitivity-table",
        widths = list(
          DV = "30px",
          Model = "82px",
          N = "44px",
          `Excluded flagged cases` = "58px",
          F = "50px",
          df1 = "36px",
          df2 = "44px",
          p = "42px",
          `partial eta2` = "52px",
          Note = "132px"
        ),
        wrap_headers = list(
          `Excluded flagged cases` = "Excluded<br>flagged<br>cases",
          `partial eta2` = "partial<br>eta2"
        ),
        left_columns = c("Model", "Note"),
        table_role = "appendix",
        table_language = appendix_language
      )
    )
  }
  for (item in result$results %||% list()) {
    plot_sections <- ancova_plot_sections(item, plot_renderer)
    if (length(plot_sections) > 0) {
      sections[[length(sections) + 1L]] <- tags$div(
        class = "result-section regression-result-panel ancova-plots-panel",
        tags$h3(sprintf("%s (%s)", ancova_appendix_text("ANCOVA plots", appendix_language), display_variable_name_static(item$dependent, variable_table, labels, label_only = TRUE))),
        do.call(tagList, plot_sections)
      )
    }
  }
  skipped <- result$skipped
  if (is.data.frame(skipped) && "Message" %in% names(skipped)) skipped$Message <- ancova_error_ui_text(skipped$Message, appendix_language)
  diagnostics <- analysis_diagnostics_section(NULL, skipped, title = ancova_appendix_text("Warnings / skipped models", appendix_language), messages_localized = TRUE)
  if (!is.null(diagnostics)) {
    sections[[length(sections) + 1L]] <- diagnostics
  }
  do.call(tagList, sections)
}
