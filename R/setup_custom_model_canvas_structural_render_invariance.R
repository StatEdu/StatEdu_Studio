# Structural measurement invariance result rendering.

structural_canvas_structural_path_group_comparison_ui <- function(result, ko = FALSE, display_name = identity,
                                                                  table_number_fn = NULL,
                                                                  bootstrap_execution = list()) {
  if (exists("structural_canvas_enforce_product_factor_joint_gate", mode = "function")) {
    result <- structural_canvas_enforce_product_factor_joint_gate(result)
  }
  result_frame <- function(value) if (is.data.frame(value)) value else data.frame()
  table <- result_frame(result$table)
  group_table <- result_frame(result$group_diagnostics)
  path_estimates <- result_frame(result$path_estimates)
  formal_path_tests <- result_frame(result$formal_path_tests)
  path_differences <- result_frame(result$path_differences)
  interaction_group_estimates <- result_frame(result$interaction_group_estimates)
  interaction_omnibus_tests <- result_frame(result$interaction_omnibus_tests)
  interaction_pairwise_differences <- result_frame(result$interaction_pairwise_differences)
  moderated_mediation_group_indices <- result_frame(result$moderated_mediation_group_indices)
  moderated_mediation_delta_tests <- result_frame(result$moderated_mediation_delta_tests)
  moderated_mediation_pairwise_differences <- result_frame(result$moderated_mediation_pairwise_differences)
  moderated_mediation_bootstrap_diagnostics <- result_frame(result$moderated_mediation_bootstrap_diagnostics)
  unsupported_moderated_mediation_paths <- as.character(
    result$moderated_mediation_unsupported_paths %||% character(0)
  )
  path_scope <- tolower(trimws(as.character(result$path_scope %||% "all")))
  path_scope <- if (length(path_scope) == 1L && path_scope %in% c("all", "selected")) path_scope else "all"
  selected_path_registry <- result_frame(result$selected_path_registry)
  selected_path_labels <- if (identical(path_scope, "selected") && nrow(selected_path_registry) && "path" %in% names(selected_path_registry)) {
    structural_canvas_display_path(as.character(selected_path_registry$path), display_name)
  } else {
    character(0)
  }
  combine_interval <- function(lower, upper) {
    lower <- as.character(lower)
    upper <- as.character(upper)
    valid <- !is.na(lower) & !is.na(upper) & nzchar(trimws(lower)) & nzchar(trimws(upper))
    interval <- rep("", max(length(lower), length(upper)))
    interval[valid] <- paste0("[", lower[valid], ", ", upper[valid], "]")
    interval
  }
  format_p_display <- function(value) {
    formatted <- vapply(value, format_p, character(1))
    formatted[is.na(formatted)] <- ""
    formatted
  }
  collapse_first_interval <- function(value, pairs, output_name) {
    if (!is.data.frame(value) || !nrow(value)) return(value)
    for (pair in pairs) {
      if (!all(pair %in% names(value))) next
      value[[output_name]] <- combine_interval(value[[pair[[1L]]]], value[[pair[[2L]]]])
      value <- value[, setdiff(names(value), pair), drop = FALSE]
      break
    }
    value
  }
  format_result_columns <- function(value, decimal = character(0), p = character(0), integer = character(0), percent = character(0)) {
    if (!is.data.frame(value) || !nrow(value)) return(value)
    for (name in intersect(decimal, names(value))) value[[name]] <- vapply(value[[name]], format_decimal3, character(1))
    for (name in intersect(p, names(value))) value[[name]] <- format_p_display(value[[name]])
    for (name in intersect(integer, names(value))) {
      raw <- suppressWarnings(as.numeric(value[[name]]))
      value[[name]] <- ifelse(is.finite(raw), formatC(raw, format = "f", digits = 0), "")
    }
    for (name in intersect(percent, names(value))) {
      raw <- suppressWarnings(as.numeric(value[[name]]))
      value[[name]] <- ifelse(is.finite(raw), paste0(vapply(raw, format_decimal3, character(1)), "%"), "")
    }
    value
  }
  select_result_columns <- function(value, preferred) {
    if (!is.data.frame(value) || !nrow(value)) return(value)
    value[, intersect(preferred, names(value)), drop = FALSE]
  }
  display_extended_paths <- function(value) {
    if (!is.data.frame(value) || !nrow(value)) return(value)
    value <- structural_canvas_display_identifier_table(value, display_name)
    for (column in intersect(c("Interaction path", "Indirect path", "Moderated path"), names(value))) {
      path <- as.character(value[[column]])
      nonmissing <- !is.na(path)
      path[nonmissing] <- structural_canvas_display_path(path[nonmissing], display_name)
      value[[column]] <- path
    }
    value
  }
  result_table_heading <- function(kind, ko_title, en_title) {
    title <- if (ko) ko_title else en_title
    number <- if (is.function(table_number_fn)) table_number_fn(kind) else NA_character_
    if (!length(number) || is.na(number) || !nzchar(as.character(number))) return(title)
    if (ko) paste0("표 ", number, ". ", title) else paste0("Table ", number, ". ", title)
  }
  result_supplement_heading <- function(parent_kind, ko_title, en_title) {
    title <- if (ko) ko_title else en_title
    number <- if (is.function(table_number_fn)) table_number_fn(parent_kind) else NA_character_
    if (!length(number) || is.na(number) || !nzchar(as.character(number))) return(title)
    if (ko) paste0("표 ", number, " 보조: ", title) else paste0("Supplement to Table ", number, ": ", title)
  }
  compact_invariance_table <- function(value) {
    if (!is.data.frame(value) || !nrow(value)) return(value)
    columns <- intersect(c(
      "Model", "Chisq", "df", "p", "CFI", "RMSEA", "SRMR",
      "DeltaCFI", "DeltaRMSEA", "DeltaSRMR", "DeltaChisq", "DeltaDf", "DeltaP",
      "Converged", "Admissible", "Admissibility reasons"
    ), names(value))
    value <- value[, columns, drop = FALSE]
    for (name in intersect(c("Chisq", "df", "CFI", "RMSEA", "SRMR", "DeltaCFI", "DeltaRMSEA", "DeltaSRMR", "DeltaChisq", "DeltaDf"), names(value))) {
      value[[name]] <- vapply(value[[name]], format_decimal3, character(1))
    }
    for (name in intersect(c("p", "DeltaP"), names(value))) value[[name]] <- format_p_display(value[[name]])
    value
  }
  if (nrow(path_estimates) && all(c("Predictor", "Outcome") %in% names(path_estimates))) {
    path_estimates$Path <- paste(path_estimates$Predictor, path_estimates$Outcome, sep = " → ")
    path_estimates <- path_estimates[c(
      intersect("Group", names(path_estimates)), "Path",
      setdiff(names(path_estimates), c("Group", "Path", "Predictor", "Outcome"))
    )]
  }
  if (nrow(path_differences) && all(c("Predictor", "Outcome") %in% names(path_differences))) {
    path_differences$Path <- paste(path_differences$Predictor, path_differences$Outcome, sep = " → ")
    path_differences <- path_differences[c(
      "Path", setdiff(names(path_differences), c("Path", "Predictor", "Outcome"))
    )]
  }
  path_estimates <- structural_canvas_display_identifier_table(path_estimates, display_name)
  formal_path_tests <- structural_canvas_display_identifier_table(formal_path_tests, display_name)
  path_differences <- structural_canvas_display_identifier_table(path_differences, display_name)
  interaction_group_estimates <- display_extended_paths(interaction_group_estimates)
  interaction_omnibus_tests <- display_extended_paths(interaction_omnibus_tests)
  interaction_pairwise_differences <- display_extended_paths(interaction_pairwise_differences)
  moderated_mediation_group_indices <- display_extended_paths(moderated_mediation_group_indices)
  moderated_mediation_delta_tests <- display_extended_paths(moderated_mediation_delta_tests)
  moderated_mediation_pairwise_differences <- display_extended_paths(moderated_mediation_pairwise_differences)
  moderated_mediation_bootstrap_diagnostics <- display_extended_paths(moderated_mediation_bootstrap_diagnostics)
  bootstrap_table_state <- function(value) {
    if (!is.data.frame(value) || !nrow(value)) return(list(recorded = FALSE, usable = FALSE))
    bootstrap_columns <- intersect(c(
      "Bootstrap SE", "Bootstrap CI lower", "Bootstrap CI upper", "Bootstrap p",
      "Bootstrap BH-adjusted p", "Valid replicates", "Bootstrap inference source"
    ), names(value))
    recorded <- length(bootstrap_columns) > 0L
    required_columns <- c("Bootstrap CI lower", "Bootstrap CI upper", "Bootstrap p")
    usable <- all(required_columns %in% names(value)) && any(Reduce(`&`, lapply(
      required_columns,
      function(name) is.finite(suppressWarnings(as.numeric(value[[name]])))
    )))
    list(recorded = recorded, usable = usable)
  }
  group_bootstrap_state <- bootstrap_table_state(moderated_mediation_group_indices)
  pairwise_bootstrap_state <- bootstrap_table_state(moderated_mediation_pairwise_differences)
  interaction_group_bootstrap_state <- bootstrap_table_state(interaction_group_estimates)
  interaction_pairwise_bootstrap_state <- bootstrap_table_state(interaction_pairwise_differences)
  diagnostic_recorded <- nrow(moderated_mediation_bootstrap_diagnostics) > 0L
  diagnostic_values <- if (diagnostic_recorded &&
      "Inference usable" %in% names(moderated_mediation_bootstrap_diagnostics)) {
    as.logical(moderated_mediation_bootstrap_diagnostics[["Inference usable"]])
  } else {
    logical(0)
  }
  diagnostic_usable <- length(diagnostic_values) > 0L &&
    any(diagnostic_values %in% TRUE, na.rm = TRUE) &&
    !any(diagnostic_values %in% FALSE, na.rm = TRUE)
  has_group_index_bootstrap <- isTRUE(group_bootstrap_state$usable) && diagnostic_usable
  has_pairwise_index_bootstrap <- isTRUE(pairwise_bootstrap_state$usable) && diagnostic_usable
  has_modmed_bootstrap_recorded <- isTRUE(group_bootstrap_state$recorded) ||
    isTRUE(pairwise_bootstrap_state$recorded) || diagnostic_recorded
  has_modmed_bootstrap_primary <- has_group_index_bootstrap || has_pairwise_index_bootstrap
  has_interaction_group_bootstrap <- isTRUE(interaction_group_bootstrap_state$usable) &&
    diagnostic_usable
  has_interaction_pairwise_bootstrap <- isTRUE(interaction_pairwise_bootstrap_state$usable) &&
    diagnostic_usable
  has_interaction_bootstrap_recorded <- isTRUE(interaction_group_bootstrap_state$recorded) ||
    isTRUE(interaction_pairwise_bootstrap_state$recorded)
  has_any_mg_bootstrap_primary <- has_modmed_bootstrap_primary ||
    has_interaction_group_bootstrap || has_interaction_pairwise_bootstrap
  has_any_mg_bootstrap_recorded <- has_modmed_bootstrap_recorded ||
    has_interaction_bootstrap_recorded || diagnostic_recorded
  has_moderated_mediation_results <- nrow(moderated_mediation_group_indices) > 0L ||
    nrow(moderated_mediation_pairwise_differences) > 0L
  bootstrap_requested <- isTRUE(bootstrap_execution$requested)
  bootstrap_pending <- isTRUE(bootstrap_execution$pending)
  bootstrap_canceled <- isTRUE(bootstrap_execution$canceled)
  bootstrap_error <- trimws(paste(as.character(bootstrap_execution$error %||% ""), collapse = " "))
  bootstrap_blocked_reason <- trimws(paste(as.character(bootstrap_execution$blocked_reason %||% ""), collapse = " "))
  if (nrow(group_table) && "Indicator missing %" %in% names(group_table)) {
    group_table[["Indicator missing %"]] <- paste0(vapply(group_table[["Indicator missing %"]], format_decimal3, character(1)), "%")
  }
  if (nrow(table)) {
    numeric_columns <- c("Chisq", "df", "CFI", "RMSEA", "SRMR", "DeltaCFI", "DeltaRMSEA", "DeltaSRMR", "DeltaChisq", "DeltaDf")
    for (name in intersect(numeric_columns, names(table))) table[[name]] <- vapply(table[[name]], format_decimal3, character(1))
    if ("p" %in% names(table)) table$p <- format_p_display(table$p)
    if ("DeltaP" %in% names(table)) table$DeltaP <- format_p_display(table$DeltaP)
    if (!ko && "Difference test method" %in% names(table)) {
      method <- as.character(table[["Difference test method"]])
      robust <- !is.na(method) & grepl("^MLR robust/scaled likelihood-ratio", method)
      ml <- !is.na(method) & grepl("^ML likelihood-ratio", method)
      correction <- rep("", length(method))
      correction[robust & grepl("satorra.bentler.2001", method, fixed = TRUE)] <- " (Satorra-Bentler 2001)"
      correction[robust & grepl("satorra.bentler.2010", method, fixed = TRUE)] <- " (Satorra-Bentler 2010)"
      correction[robust & grepl("yuan.bentler", method, fixed = TRUE)] <- " (Yuan-Bentler)"
      method[robust] <- paste0("MLR scaled likelihood-ratio chi-square difference test", correction[robust])
      method[ml] <- "ML likelihood-ratio chi-square difference test"
      table[["Difference test method"]] <- method
    }
    names(table)[names(table) == "DeltaCFI"] <- "ΔCFI"
    names(table)[names(table) == "DeltaRMSEA"] <- "ΔRMSEA"
    names(table)[names(table) == "DeltaSRMR"] <- "ΔSRMR"
    names(table)[names(table) == "DeltaChisq"] <- "Δχ²"
    names(table)[names(table) == "DeltaDf"] <- "Δdf"
    names(table)[names(table) == "DeltaP"] <- "Δp"
  }
  if (nrow(path_estimates)) {
    for (name in intersect(c("B", "SE", "z", "B 95% CI lower", "B 95% CI upper", "beta", "beta 95% CI lower", "beta 95% CI upper"), names(path_estimates))) {
      path_estimates[[name]] <- vapply(path_estimates[[name]], format_decimal3, character(1))
    }
    if ("p" %in% names(path_estimates)) path_estimates$p <- format_p_display(path_estimates$p)
    if (all(c("B 95% CI lower", "B 95% CI upper") %in% names(path_estimates))) {
      path_estimates[["B 95% CI"]] <- combine_interval(
        path_estimates[["B 95% CI lower"]], path_estimates[["B 95% CI upper"]]
      )
      path_estimates <- path_estimates[, setdiff(names(path_estimates), c("B 95% CI lower", "B 95% CI upper")), drop = FALSE]
    }
    if (all(c("beta 95% CI lower", "beta 95% CI upper") %in% names(path_estimates))) {
      path_estimates[["beta 95% CI"]] <- combine_interval(
        path_estimates[["beta 95% CI lower"]], path_estimates[["beta 95% CI upper"]]
      )
      path_estimates <- path_estimates[, setdiff(names(path_estimates), c("beta 95% CI lower", "beta 95% CI upper")), drop = FALSE]
    }
    path_estimates <- path_estimates[, intersect(
      c("Group", "Path", "B", "SE", "B 95% CI", "beta", "beta 95% CI", "z", "p", "Inference status"),
      names(path_estimates)
    ), drop = FALSE]
  }
  if (nrow(formal_path_tests)) {
    for (name in intersect(c("Wald chi-square", "df"), names(formal_path_tests))) {
      formal_path_tests[[name]] <- vapply(formal_path_tests[[name]], format_decimal3, character(1))
    }
    for (name in intersect(c("p", "BH-adjusted p"), names(formal_path_tests))) {
      formal_path_tests[[name]] <- format_p_display(formal_path_tests[[name]])
    }
    formal_path_tests <- formal_path_tests[, intersect(
      c("Path", "Wald chi-square", "df", "p", "BH-adjusted p", "Test method", "Status"),
      names(formal_path_tests)
    ), drop = FALSE]
  }
  if (nrow(path_differences)) {
    for (name in intersect(c("B difference", "SE", "B difference 95% CI lower", "B difference 95% CI upper", "z"), names(path_differences))) {
      path_differences[[name]] <- vapply(path_differences[[name]], format_decimal3, character(1))
    }
    for (name in intersect(c("p", "BH-adjusted p"), names(path_differences))) {
      path_differences[[name]] <- format_p_display(path_differences[[name]])
    }
    if (all(c("B difference 95% CI lower", "B difference 95% CI upper") %in% names(path_differences))) {
      path_differences[["B difference 95% CI"]] <- combine_interval(
        path_differences[["B difference 95% CI lower"]], path_differences[["B difference 95% CI upper"]]
      )
    }
    path_differences <- path_differences[, intersect(
      c("Path", "Group 1", "Group 2", "B difference", "SE", "B difference 95% CI", "z", "p", "BH-adjusted p", "Test method", "Status"),
      names(path_differences)
    ), drop = FALSE]
  }
  if (nrow(interaction_group_estimates)) {
    interaction_group_estimates <- format_result_columns(
      interaction_group_estimates,
      decimal = c(
        "B", "SE", "z", "B 95% CI lower", "B 95% CI upper",
        "Bootstrap SE", "Bootstrap CI lower", "Bootstrap CI upper"
      ),
      p = c("p", "Bootstrap p", "Bootstrap BH-adjusted p"),
      integer = c("Valid replicates", "Requested replicates"),
      percent = "Valid %"
    )
    interaction_group_estimates <- collapse_first_interval(
      interaction_group_estimates,
      if (has_interaction_group_bootstrap) {
        list(c("Bootstrap CI lower", "Bootstrap CI upper"))
      } else {
        list(c("B 95% CI lower", "B 95% CI upper"), c("CI lower", "CI upper"))
      },
      if (has_interaction_group_bootstrap) "Bootstrap 95% CI" else "B 95% CI"
    )
    interaction_group_inference_columns <- if (has_interaction_group_bootstrap) {
      c(
        "B", "Bootstrap SE", "Bootstrap 95% CI", "Bootstrap p",
        "Bootstrap BH-adjusted p", "Valid replicates", "Requested replicates",
        "Valid %", "CI method", "Quantile type", "Bootstrap inference source",
        "Bootstrap status", "Inference status"
      )
    } else {
      c(
        "B", "SE", "B 95% CI", "z", "p", "Inference status", "Status",
        "Bootstrap status", "Bootstrap inference source"
      )
    }
    interaction_group_estimates <- select_result_columns(
      interaction_group_estimates,
      c("Group", "Interaction path", "Predictor", "Moderator", "Outcome", interaction_group_inference_columns)
    )
  }
  if (nrow(interaction_omnibus_tests)) {
    interaction_omnibus_tests <- format_result_columns(
      interaction_omnibus_tests,
      decimal = c("Wald chi-square", "df"),
      p = c("p", "BH-adjusted p"),
      integer = "Multiplicity family size"
    )
    interaction_omnibus_tests <- select_result_columns(interaction_omnibus_tests, c(
      "Interaction path", "Predictor", "Moderator", "Outcome", "Wald chi-square", "df",
      "p", "BH-adjusted p", "Test method", "Estimand", "Groups constrained",
      "Multiplicity family", "Multiplicity family size", "Status"
    ))
  }
  if (nrow(interaction_pairwise_differences)) {
    interaction_pairwise_differences <- format_result_columns(
      interaction_pairwise_differences,
      decimal = c(
        "B group 1", "B group 2", "B difference", "SE", "z",
        "B difference 95% CI lower", "B difference 95% CI upper", "CI lower", "CI upper",
        "Bootstrap B difference", "Bootstrap SE", "Bootstrap CI lower", "Bootstrap CI upper"
      ),
      p = c("p", "BH-adjusted p", "Bootstrap p", "Bootstrap BH-adjusted p"),
      integer = c("Multiplicity family size", "Valid replicates", "Requested replicates"),
      percent = "Valid %"
    )
    interaction_pairwise_differences <- collapse_first_interval(
      interaction_pairwise_differences,
      if (has_interaction_pairwise_bootstrap) {
        list(c("Bootstrap CI lower", "Bootstrap CI upper"))
      } else {
        list(c("B difference 95% CI lower", "B difference 95% CI upper"), c("CI lower", "CI upper"))
      },
      if (has_interaction_pairwise_bootstrap) "Bootstrap 95% CI" else "B difference 95% CI"
    )
    interaction_pairwise_inference_columns <- if (has_interaction_pairwise_bootstrap) {
      c(
        "B group 1", "B group 2", "Bootstrap B difference", "Bootstrap SE",
        "Bootstrap 95% CI", "Bootstrap p", "Bootstrap BH-adjusted p",
        "Valid replicates", "Requested replicates", "Valid %", "CI method",
        "Quantile type", "Bootstrap inference source", "Bootstrap status", "Status"
      )
    } else {
      c(
        "B group 1", "B group 2", "B difference", "SE", "B difference 95% CI",
        "z", "p", "BH-adjusted p", "Test method", "Estimand",
        "Multiplicity family", "Multiplicity family size", "Status",
        "Bootstrap status", "Bootstrap inference source"
      )
    }
    interaction_pairwise_differences <- select_result_columns(
      interaction_pairwise_differences,
      c(
        "Interaction path", "Predictor", "Moderator", "Outcome", "Group 1", "Group 2",
        interaction_pairwise_inference_columns
      )
    )
  }
  if (nrow(moderated_mediation_group_indices)) {
    moderated_mediation_group_indices <- format_result_columns(
      moderated_mediation_group_indices,
      decimal = c(
        "Index", "Bootstrap SE", "SE", "z", "Index 95% CI lower", "Index 95% CI upper",
        "Bootstrap CI lower", "Bootstrap CI upper", "CI lower", "CI upper"
      ),
      p = c("Bootstrap p", "Bootstrap BH-adjusted p", "p", "BH-adjusted p"),
      integer = c("Valid replicates", "Requested replicates", "Valid bootstrap", "Requested bootstrap"),
      percent = c("Valid %", "Valid percent")
    )
    moderated_mediation_group_indices <- collapse_first_interval(
      moderated_mediation_group_indices,
      if (has_group_index_bootstrap) {
        list(c("Bootstrap CI lower", "Bootstrap CI upper"))
      } else {
        list(c("Index 95% CI lower", "Index 95% CI upper"), c("CI lower", "CI upper"))
      },
      if (has_group_index_bootstrap) "Bootstrap 95% CI" else "Index 95% CI"
    )
    group_index_inference_columns <- if (has_group_index_bootstrap) {
      c(
        "Index", "Bootstrap SE", "Bootstrap 95% CI", "Bootstrap p", "Bootstrap BH-adjusted p",
        "Valid replicates", "Requested replicates", "Valid bootstrap", "Requested bootstrap",
        "Valid %", "Valid percent", "CI method", "Quantile type",
        "Bootstrap inference source", "Bootstrap status", "Inference status"
      )
    } else {
      c("Index", "SE", "Index 95% CI", "z", "p", "Inference method", "Inference status", "Status")
    }
    moderated_mediation_group_indices <- select_result_columns(
      moderated_mediation_group_indices,
      c(
        "Group", "Indirect path", "Moderated path", "Predictor", "Moderator", "Outcome",
        group_index_inference_columns
      )
    )
  }
  if (nrow(moderated_mediation_delta_tests)) {
    moderated_mediation_delta_tests <- format_result_columns(
      moderated_mediation_delta_tests,
      decimal = c("Wald chi-square", "df"),
      p = c("p", "BH-adjusted p"),
      integer = "Multiplicity family size"
    )
    moderated_mediation_delta_tests <- select_result_columns(moderated_mediation_delta_tests, c(
      "Indirect path", "Moderated path", "Predictor", "Moderator", "Outcome",
      "Wald chi-square", "df", "p", "BH-adjusted p", "Test method", "Groups constrained",
      "Estimand", "Multiplicity family", "Multiplicity family size", "Status"
    ))
  }
  if (nrow(moderated_mediation_pairwise_differences)) {
    moderated_mediation_pairwise_differences <- format_result_columns(
      moderated_mediation_pairwise_differences,
      decimal = c(
        "Index group 1", "Index group 2", "Index difference", "Bootstrap SE", "SE",
        "Index difference 95% CI lower", "Index difference 95% CI upper",
        "Bootstrap CI lower", "Bootstrap CI upper", "CI lower", "CI upper", "z"
      ),
      p = c("Bootstrap p", "Bootstrap BH-adjusted p", "p", "BH-adjusted p"),
      integer = c("Valid replicates", "Requested replicates", "Valid bootstrap", "Requested bootstrap"),
      percent = c("Valid %", "Valid percent")
    )
    moderated_mediation_pairwise_differences <- collapse_first_interval(
      moderated_mediation_pairwise_differences,
      if (has_pairwise_index_bootstrap) {
        list(c("Bootstrap CI lower", "Bootstrap CI upper"))
      } else {
        list(c("Index difference 95% CI lower", "Index difference 95% CI upper"), c("CI lower", "CI upper"))
      },
      if (has_pairwise_index_bootstrap) "Bootstrap 95% CI" else "Index difference 95% CI"
    )
    pairwise_index_inference_columns <- if (has_pairwise_index_bootstrap) {
      c(
        "Index group 1", "Index group 2", "Index difference", "Bootstrap SE",
        "Bootstrap 95% CI", "Bootstrap p", "Bootstrap BH-adjusted p",
        "Valid replicates", "Requested replicates", "Valid bootstrap", "Requested bootstrap",
        "Valid %", "Valid percent", "CI method", "Quantile type",
        "Bootstrap inference source", "Bootstrap status", "Status"
      )
    } else {
      c(
        "Index difference", "SE", "Index difference 95% CI", "z", "p", "BH-adjusted p",
        "Test method", "Estimand", "Status"
      )
    }
    moderated_mediation_pairwise_differences <- select_result_columns(
      moderated_mediation_pairwise_differences,
      c(
        "Indirect path", "Moderated path", "Predictor", "Moderator", "Outcome",
        "Group 1", "Group 2", pairwise_index_inference_columns
      )
    )
  }
  if (nrow(moderated_mediation_bootstrap_diagnostics)) {
    moderated_mediation_bootstrap_diagnostics <- format_result_columns(
      moderated_mediation_bootstrap_diagnostics,
      p = c("p", "BH-adjusted p"),
      integer = c(
        "Requested", "Fit-valid", "Joint-valid", "Valid replicates", "Requested replicates",
        "Valid bootstrap", "Requested bootstrap", "Seed"
      ),
      percent = c("Fit-valid %", "Joint-valid %", "Valid %", "Valid percent")
    )
  }
  measurement <- result$measurement_invariance %||% NULL
  measurement_table <- compact_invariance_table(measurement$table %||% data.frame())
  gate <- result$measurement_gate %||% list(
    passed = FALSE,
    reason = if (ko) "측정불변성 기준이 기록되지 않았습니다." else "Measurement-invariance gate was not recorded."
  )
  gate <- structural_canvas_normalize_metric_invariance_gate(gate, measurement)
  gate_reason <- structural_canvas_metric_invariance_gate_reason(gate, if (ko) "ko" else "en")
  if (ko) {
    model_labels <- c(
      "Configural" = "형태 동일",
      "Metric" = "측정단위 동일",
      "Thresholds" = "임계값 동일",
      "Scalar (thresholds + loadings)" = "임계값·요인부하량 동일",
      "Scalar" = "절편 동일",
      "Strict" = "엄격 동일",
      "Free structural paths" = "자유 구조경로",
      "Equal structural paths" = "동일 구조경로"
    )
    localize_models <- function(value) {
      if (!is.data.frame(value) || !nrow(value) || !"Model" %in% names(value)) return(value)
      matched <- match(as.character(value$Model), names(model_labels))
      replace <- !is.na(matched)
      value$Model[replace] <- unname(model_labels[matched[replace]])
      value
    }
    measurement_table <- localize_models(measurement_table)
    table <- localize_models(table)
  }
  rename_headers <- function(value) {
    if (!is.data.frame(value)) return(value)
    replacements <- c(
      "Model" = if (ko) "모형" else "Model",
      "Group" = if (ko) "집단" else "Group",
      "Path" = if (ko) "경로" else "Path",
      "Interaction path" = if (ko) "상호작용 경로" else "Interaction path",
      "Indirect path" = if (ko) "간접경로" else "Indirect path",
      "Moderated path" = if (ko) "조절경로" else "Moderated path",
      "Predictor" = if (ko) "예측변수" else "Predictor",
      "Moderator" = if (ko) "조절변수" else "Moderator",
      "Outcome" = if (ko) "결과변수" else "Outcome",
      "Chisq" = "χ²", "DeltaCFI" = "ΔCFI", "DeltaRMSEA" = "ΔRMSEA",
      "DeltaSRMR" = "ΔSRMR", "DeltaChisq" = "Δχ²", "DeltaDf" = "Δdf", "DeltaP" = "Δp",
      "Admissibility reasons" = if (ko) "허용성\n사유" else "Admissibility\nreasons",
      "Converged" = if (ko) "수렴" else "Converged",
      "Admissible" = if (ko) "허용" else "Admissible",
      "B 95% CI lower" = "B 95% CI\n하한", "B 95% CI upper" = "B 95% CI\n상한",
      "beta 95% CI lower" = "beta 95% CI\n하한", "beta 95% CI upper" = "beta 95% CI\n상한",
      "B 95% CI" = "B 95% CI", "beta 95% CI" = "beta 95% CI",
      "Wald chi-square" = "Wald χ²",
      "B group 1" = if (ko) "집단 1 B" else "Group 1 B",
      "B group 2" = if (ko) "집단 2 B" else "Group 2 B",
      "B difference" = if (ko) "ΔB" else "B difference",
      "B difference 95% CI" = if (ko) "ΔB 95% CI" else "B difference 95% CI",
      "Bootstrap B difference" = if (ko) "부트스트랩 ΔB" else "Bootstrap B difference",
      "Index" = if (ko) "조절된 매개 지수" else "Index",
      "Index group 1" = if (ko) "집단 1 지수" else "Group 1 index",
      "Index group 2" = if (ko) "집단 2 지수" else "Group 2 index",
      "Index difference" = if (ko) "지수 차이" else "Index difference",
      "Index 95% CI" = if (ko) "지수 95% CI" else "Index 95% CI",
      "Index difference 95% CI" = if (ko) "지수 차이 95% CI" else "Index difference 95% CI",
      "Bootstrap 95% CI" = if (ko) "부트스트랩 95% CI" else "Bootstrap 95% CI",
      "Bootstrap SE" = if (ko) "부트스트랩 SE" else "Bootstrap SE",
      "Bootstrap p" = if (ko) "부트스트랩 p" else "Bootstrap p",
      "Bootstrap BH-adjusted p" = if (ko) "부트스트랩 BH 보정 p" else "Bootstrap BH-adjusted p",
      "Bootstrap status" = if (ko) "부트스트랩 상태" else "Bootstrap status",
      "Bootstrap inference source" = if (ko) "부트스트랩 추론 근거" else "Bootstrap inference source",
      "Test method" = if (ko) "검정\n방법" else "Test\nmethod",
      "Difference test method" = if (ko) "차이검정\n방법" else "Difference test\nmethod",
      "Comparison status" = if (ko) "비교\n상태" else "Comparison\nstatus",
      "Comparison reason" = if (ko) "비교\n사유" else "Comparison\nreason",
      "Estimand" = if (ko) "검정대상" else "Estimand",
      "Groups constrained" = if (ko) "동일성 제약 집단" else "Groups constrained",
      "Multiplicity family" = if (ko) "다중검정군" else "Multiplicity family",
      "Multiplicity family size" = if (ko) "다중검정군 크기" else "Multiplicity family size",
      "Inference status" = if (ko) "추론 상태" else "Inference status",
      "Inference method" = if (ko) "추론 방법" else "Inference method",
      "Inference source" = if (ko) "추론 근거" else "Inference source",
      "Status" = if (ko) "상태" else "Status",
      "Group 1" = if (ko) "집단 1" else "Group 1",
      "Group 2" = if (ko) "집단 2" else "Group 2",
      "Complete indicator cases" = if (ko) "지표 완전사례" else "Complete indicator cases",
      "Indicator missing %" = if (ko) "지표 결측률" else "Indicator missing %",
      "Minimum category count" = if (ko) "최소 범주 빈도" else "Minimum category count",
      "Absent ordered categories" = if (ko) "누락된 순서형 범주" else "Absent ordered categories",
      "BH-adjusted p" = if (ko) "BH 보정 p" else "BH-adjusted p",
      "Valid replicates" = if (ko) "유효 반복" else "Valid replicates",
      "Requested replicates" = if (ko) "요청 반복" else "Requested replicates",
      "Valid bootstrap" = if (ko) "유효 부트스트랩" else "Valid bootstrap",
      "Requested bootstrap" = if (ko) "요청 부트스트랩" else "Requested bootstrap",
      "Valid %" = if (ko) "유효율" else "Valid %",
      "Valid percent" = if (ko) "유효율" else "Valid percent",
      "CI method" = if (ko) "CI 방법" else "CI method",
      "Quantile type" = if (ko) "분위수 유형" else "Quantile type",
      "Requested" = if (ko) "요청 반복" else "Requested",
      "Fit-valid" = if (ko) "적합 유효 반복" else "Fit-valid",
      "Fit-valid %" = if (ko) "적합 유효율" else "Fit-valid %",
      "Joint-valid" = if (ko) "공동 유효 반복" else "Joint-valid",
      "Joint-valid %" = if (ko) "공동 유효율" else "Joint-valid %",
      "Inference usable" = if (ko) "추론 사용 가능" else "Inference usable",
      "Seed" = if (ko) "시드" else "Seed",
      "RNG" = if (ko) "난수 생성기" else "RNG",
      "R version" = if (ko) "R 버전" else "R version",
      "Centering scope" = if (ko) "중심화 범위" else "Centering scope",
      "Method" = if (ko) "방법" else "Method",
      "Failure counts" = if (ko) "실패 횟수" else "Failure counts"
    )
    matched <- match(names(value), names(replacements))
    replace <- !is.na(matched)
    names(value)[replace] <- unname(replacements[matched[replace]])
    value
  }
  measurement_table <- rename_headers(measurement_table)
  table <- rename_headers(table)
  path_estimates <- rename_headers(path_estimates)
  formal_path_tests <- rename_headers(formal_path_tests)
  path_differences <- rename_headers(path_differences)
  interaction_group_estimates <- rename_headers(interaction_group_estimates)
  interaction_omnibus_tests <- rename_headers(interaction_omnibus_tests)
  interaction_pairwise_differences <- rename_headers(interaction_pairwise_differences)
  moderated_mediation_group_indices <- rename_headers(moderated_mediation_group_indices)
  moderated_mediation_delta_tests <- rename_headers(moderated_mediation_delta_tests)
  moderated_mediation_pairwise_differences <- rename_headers(moderated_mediation_pairwise_differences)
  moderated_mediation_bootstrap_diagnostics <- rename_headers(moderated_mediation_bootstrap_diagnostics)
  if (ko) {
    localize_status <- function(value) {
      value <- as.character(value)
      translations <- c(
        "Estimated" = "추정됨",
        "Completed" = "완료",
        "Available" = "산출 가능",
        "Auxiliary" = "보조 검정",
        "Adequate" = "적정",
        "Caution" = "주의",
        "Unreliable" = "신뢰 불가",
        "Not requested" = "요청하지 않음",
        "Pending" = "대기 중",
        "Failed" = "실패",
        "Canceled" = "중단됨",
        "Bootstrap requested - inference suppressed" = "부트스트랩 요청됨 - 추론 산출 억제",
        "Not available - insufficient valid bootstrap replicates" = "산출 불가 - 유효 부트스트랩 반복 부족",
        "Not reported: product-indicator index is scale-dependent" = "보고하지 않음: 곱지표 지수는 척도 의존적임",
        "Fixed-zero component - no inferential test" = "0으로 고정된 구성요소 - 추론검정 없음",
        "Fixed effect - no inferential test" = "고정효과 - 추론검정 없음",
        "Fixed parameter - no inferential test" = "고정모수 - 추론검정 없음",
        "Unidentified parameter - no inferential test" = "식별되지 않은 모수 - 추론검정 없음",
        "Suppressed: free structural-path model was not admissible." = "산출 억제: 자유 구조경로 모형이 허용 가능한 해가 아닙니다.",
        "Suppressed: the free structural-path model did not converge." = "산출 억제: 자유 구조경로 모형이 수렴하지 않았습니다.",
        "Suppressed: the free structural-path model was not admissible." = "산출 억제: 자유 구조경로 모형이 허용 가능한 해가 아닙니다.",
        "Suppressed: the joint parameter covariance matrix was unavailable." = "산출 억제: 공동 모수 공분산행렬을 사용할 수 없습니다.",
        "Suppressed: exactly one comparable path parameter was not available in every group." = "산출 억제: 모든 집단에서 비교 가능한 경로모수가 하나씩 확인되지 않았습니다.",
        "Suppressed: one or more group-specific path estimates were non-finite." = "산출 억제: 하나 이상의 집단별 경로 추정값이 유한하지 않습니다.",
        "Suppressed: one or more group-specific path parameters were fixed or unidentified." = "산출 억제: 하나 이상의 집단별 경로모수가 고정되었거나 식별되지 않았습니다.",
        "Suppressed: a path parameter was not represented in the joint covariance matrix." = "산출 억제: 경로모수가 공동 공분산행렬에 포함되지 않았습니다.",
        "Suppressed: the relevant joint covariance block contained non-finite values." = "산출 억제: 관련 공동 공분산행렬 블록에 유한하지 않은 값이 있습니다.",
        "Suppressed: unique lavaan parameter labels were unavailable for the equality constraints." = "산출 억제: 동일성 제약에 필요한 고유 lavaan 모수 라벨을 사용할 수 없습니다.",
        "Suppressed: the joint equality-constraint Wald test was singular or unavailable." = "산출 억제: 공동 동일성 제약 Wald 검정이 특이행렬 문제로 산출되지 않았습니다.",
        "Suppressed: the pairwise contrast variance was non-positive or unavailable." = "산출 억제: 집단 쌍 대비분산이 양수가 아니거나 산출되지 않았습니다."
      )
      matched <- match(value, names(translations))
      replace <- !is.na(matched)
      value[replace] <- unname(translations[matched[replace]])
      value[!replace] <- sub("^Suppressed: ", "산출 억제: ", value[!replace])
      value
    }
    localize_test_method <- function(value) {
      translations <- c(
        "Robust Wald chi-square (joint multi-group robust vcov)" = "강건 Wald χ²(공동 다집단 강건 공분산행렬)",
        "Wald chi-square (joint multi-group model vcov)" = "Wald χ²(공동 다집단 모형기반 공분산행렬)",
        "Pairwise Wald contrast (joint multi-group robust vcov)" = "집단 쌍 Wald 대비(공동 다집단 강건 공분산행렬)",
        "Pairwise Wald contrast (joint multi-group model vcov)" = "집단 쌍 Wald 대비(공동 다집단 모형기반 공분산행렬)",
        "Delta-method Wald chi-square (joint multi-group robust vcov)" = "Delta-method Wald χ²(공동 다집단 강건 공분산행렬)",
        "Delta-method Wald chi-square (joint multi-group model vcov)" = "Delta-method Wald χ²(공동 다집단 모형기반 공분산행렬)",
        "Robust Wald chi-square for equality of moderated-mediation indices" = "조절된 매개 지수 동일성 강건 Wald χ²",
        "Wald chi-square for equality of moderated-mediation indices" = "조절된 매개 지수 동일성 Wald χ²",
        "Delta method using the joint multi-group robust covariance matrix" = "공동 다집단 강건 공분산행렬 Delta 방법",
        "Delta method using the joint multi-group model covariance matrix" = "공동 다집단 모형기반 공분산행렬 Delta 방법",
        "Stratified case-resampling bootstrap" = "집단별 층화 사례 재표집 부트스트랩",
        "Stratified case-resampling bootstrap difference" = "집단별 층화 사례 재표집 부트스트랩 차이검정"
      )
      value <- as.character(value)
      matched <- match(value, names(translations))
      replace <- !is.na(matched)
      value[replace] <- unname(translations[matched[replace]])
      value
    }
    localize_difference_method <- function(value) {
      value <- as.character(value)
      robust <- !is.na(value) & grepl("^MLR robust/scaled likelihood-ratio", value)
      ml <- !is.na(value) & grepl("^ML likelihood-ratio", value)
      correction <- rep("", length(value))
      correction[robust & grepl("satorra.bentler.2001", value, fixed = TRUE)] <- " (Satorra–Bentler 2001)"
      correction[robust & grepl("satorra.bentler.2010", value, fixed = TRUE)] <- " (Satorra–Bentler 2010)"
      correction[robust & grepl("yuan.bentler", value, fixed = TRUE)] <- " (Yuan–Bentler)"
      value[robust] <- paste0("MLR 강건 척도보정 우도비 χ² 차이검정", correction[robust])
      value[ml] <- "ML 우도비 χ² 차이검정"
      value
    }
    localize_comparison_status <- function(value) {
      translations <- c(
        "Reference model" = "기준모형",
        "Estimated" = "추정됨",
        "Difference test unavailable" = "차이검정 산출 불가",
        "Suppressed" = "산출 억제"
      )
      value <- as.character(value)
      matched <- match(value, names(translations))
      replace <- !is.na(matched)
      value[replace] <- unname(translations[matched[replace]])
      value
    }
    localize_comparison_reason <- function(value) {
      translations <- c(
        "Reference model for the nested structural-path comparison." = "중첩 구조경로 비교의 기준모형입니다.",
        "Both structural comparison models converged and were admissible." = "두 구조 비교모형이 모두 수렴했고 허용 가능한 해였습니다.",
        "Suppressed because one or both structural comparison models did not converge." = "하나 이상의 구조 비교모형이 수렴하지 않아 비교 통계량을 산출하지 않았습니다.",
        "Suppressed because one or both structural comparison models were inadmissible." = "하나 이상의 구조 비교모형이 허용 가능한 해가 아니어서 비교 통계량을 산출하지 않았습니다.",
        "Suppressed because one or both structural comparison models did not converge or were inadmissible." = "하나 이상의 구조 비교모형이 수렴하지 않았거나 허용 가능한 해가 아니어서 비교 통계량을 산출하지 않았습니다.",
        "Both models converged and were admissible, but lavaan did not return a usable likelihood-ratio difference test." = "두 모형은 수렴했고 허용 가능했지만 lavaan이 사용 가능한 우도비 차이검정을 반환하지 않았습니다."
      )
      value <- as.character(value)
      matched <- match(value, names(translations))
      replace <- !is.na(matched)
      value[replace] <- unname(translations[matched[replace]])
      value
    }
    for (name in intersect(c("추론 상태", "상태"), names(path_estimates))) path_estimates[[name]] <- localize_status(path_estimates[[name]])
    for (name in intersect(c("추론 상태", "상태"), names(formal_path_tests))) formal_path_tests[[name]] <- localize_status(formal_path_tests[[name]])
    for (name in intersect(c("추론 상태", "상태"), names(path_differences))) path_differences[[name]] <- localize_status(path_differences[[name]])
    for (name in intersect("검정\n방법", names(formal_path_tests))) formal_path_tests[[name]] <- localize_test_method(formal_path_tests[[name]])
    for (name in intersect("검정\n방법", names(path_differences))) path_differences[[name]] <- localize_test_method(path_differences[[name]])
    for (value_name in c(
      "interaction_group_estimates", "interaction_omnibus_tests", "interaction_pairwise_differences",
      "moderated_mediation_group_indices", "moderated_mediation_delta_tests",
      "moderated_mediation_pairwise_differences", "moderated_mediation_bootstrap_diagnostics"
    )) {
      value <- get(value_name)
      if (!is.data.frame(value) || !nrow(value)) next
      for (name in intersect(c("추론 상태", "상태", "부트스트랩 상태"), names(value))) value[[name]] <- localize_status(value[[name]])
      for (name in intersect("검정\n방법", names(value))) value[[name]] <- localize_test_method(value[[name]])
      for (name in intersect("추론 방법", names(value))) value[[name]] <- localize_test_method(value[[name]])
      if ("CI 방법" %in% names(value)) {
        ci_method <- as.character(value[["CI 방법"]])
        ci_method[ci_method == "bias_corrected"] <- "편향보정"
        ci_method[ci_method == "percentile"] <- "백분위"
        value[["CI 방법"]] <- ci_method
      }
      for (source_name in intersect(c("추론 근거", "부트스트랩 추론 근거"), names(value))) {
        inference_source <- as.character(value[[source_name]])
        inference_source[inference_source == "Stratified case-resampling bootstrap"] <- "집단별 층화 사례 재표집 부트스트랩"
        inference_source[inference_source == "Bootstrap (empirical two-sided p)"] <- "부트스트랩(경험적 양측 p)"
        inference_source[inference_source == "Bootstrap requested - inference suppressed"] <- "부트스트랩 요청됨 - 추론 산출 억제"
        inference_source[inference_source == "Fixed effect - no inferential test"] <- "고정효과 - 추론검정 없음"
        inference_source[inference_source == "Delta-method Wald sensitivity test"] <- "Delta-method Wald 민감도 검정"
        value[[source_name]] <- inference_source
      }
      assign(value_name, value)
    }
    if ("차이검정\n방법" %in% names(table)) table[["차이검정\n방법"]] <- localize_difference_method(table[["차이검정\n방법"]])
    if ("비교\n상태" %in% names(table)) table[["비교\n상태"]] <- localize_comparison_status(table[["비교\n상태"]])
    if ("비교\n사유" %in% names(table)) table[["비교\n사유"]] <- localize_comparison_reason(table[["비교\n사유"]])
    if ("Absent ordered categories" %in% names(group_table)) group_table[["Absent ordered categories"]][group_table[["Absent ordered categories"]] == "None"] <- "없음"
    if ("Status" %in% names(group_table)) {
      group_status <- c(
        "Ordered category absent" = "집단 내 순서형 범주 누락",
        "No group-level flag" = "집단 수준 경고 없음"
      )
      matched <- match(group_table$Status, names(group_status))
      replace <- !is.na(matched)
      group_table$Status[replace] <- unname(group_status[matched[replace]])
      group_table$Status <- sub("^Very small group \\(N < 30\\); invariance estimates may be unstable$", "매우 작은 집단(N < 30): 불변성 추정이 불안정할 수 있음", group_table$Status)
      group_table$Status <- sub("^Small group; review power/stability$", "작은 집단: 검정력과 안정성 검토 필요", group_table$Status)
      group_table$Status <- sub("^Severely unbalanced smallest group; review power/stability$", "심하게 불균형한 최소 집단: 검정력과 안정성 검토 필요", group_table$Status)
    }
  }
  # Continuous-variable diagnostics legitimately leave category fields missing.
  # Render those cells as blank instead of exposing R's literal `NA` in the UI.
  for (name in names(group_table)) {
    missing <- is.na(group_table[[name]])
    if (any(missing)) {
      group_table[[name]] <- as.character(group_table[[name]])
      group_table[[name]][missing] <- ""
    }
  }
  for (value_name in c(
    "measurement_table", "table", "path_estimates", "formal_path_tests", "path_differences",
    "interaction_group_estimates", "interaction_omnibus_tests", "interaction_pairwise_differences",
    "moderated_mediation_group_indices", "moderated_mediation_delta_tests",
    "moderated_mediation_pairwise_differences", "moderated_mediation_bootstrap_diagnostics"
  )) {
    value <- get(value_name)
    if (!is.data.frame(value)) next
    for (name in names(value)) {
      missing <- is.na(value[[name]])
      if (any(missing)) {
        value[[name]] <- as.character(value[[name]])
        value[[name]][missing] <- ""
      }
    }
    assign(value_name, value)
  }
  group_table <- rename_headers(group_table)
  has_latent_moderation_comparison <- identical(as.character(result$subtype %||% ""), "latent_product_indicator") ||
    any(vapply(list(
      interaction_group_estimates, interaction_omnibus_tests, interaction_pairwise_differences,
      moderated_mediation_group_indices, moderated_mediation_delta_tests,
      moderated_mediation_pairwise_differences, moderated_mediation_bootstrap_diagnostics
    ), function(value) is.data.frame(value) && nrow(value) > 0L, logical(1)))
  product_indicator_policy <- result$product_indicator_policy %||% NULL
  product_indicator_audit <- result$product_indicator_audit %||% NULL
  product_factor_joint_gate <- result$product_factor_joint_gate %||% NULL
  policy_scalar <- function(value, names) {
    if (!is.list(value)) return("")
    for (name in names) {
      candidate <- as.character(value[[name]] %||% "")
      candidate <- candidate[!is.na(candidate) & nzchar(trimws(candidate))]
      if (length(candidate)) return(candidate[[1L]])
    }
    ""
  }
  product_policy_statement <- if (is.character(product_indicator_policy)) {
    paste(product_indicator_policy[!is.na(product_indicator_policy) & nzchar(trimws(product_indicator_policy))], collapse = " ")
  } else {
    policy_scalar(product_indicator_policy, c("statement", "policy", "description", "method"))
  }
  product_audit_status <- policy_scalar(product_indicator_audit, c("status", "summary", "message"))
  div(class = "result-section regression-result-panel structural-main-result-panel structural-invariance-result structural-path-group-result",
    h4(if (ko) paste0("구조경로 집단비교: ", display_name(result$group)) else paste0("Structural path group comparison by ", display_name(result$group))),
    if (has_latent_moderation_comparison) tags$p(
      class = paste("structural-result-note structural-section-status-note", if (!is.list(product_factor_joint_gate) || !isTRUE(product_factor_joint_gate$passed)) "structural-result-warning" else ""),
      if (has_latent_moderation_comparison &&
          (!is.list(product_factor_joint_gate) || !isTRUE(product_factor_joint_gate$passed))) {
        "The joint product-factor gate failed; latent-moderation and moderated-mediation inference is suppressed."
      } else if (has_any_mg_bootstrap_primary) {
        "Latent-interaction group differences use unstandardized B and stratified-bootstrap inference; moderated-mediation indices are unstandardized."
      } else if (has_any_mg_bootstrap_recorded) {
        "Stratified-bootstrap inference was recorded but unusable; displayed Wald/Delta results are auxiliary."
      } else {
        "Latent-interaction group differences use unstandardized B; moderated-mediation Wald/Delta results are auxiliary until bootstrap inference is available."
      }
    ),
    if (bootstrap_requested && (bootstrap_pending || bootstrap_canceled || nzchar(bootstrap_error) || nzchar(bootstrap_blocked_reason))) result_note_paragraph(
      class = "structural-result-note structural-section-status-note structural-result-warning",
      if (bootstrap_pending) {
        if (ko) "다집단 잠재조절 층화 부트스트랩을 계산하고 있습니다. 완료되면 이 결과가 자동으로 갱신됩니다." else "The stratified multi-group latent-moderation bootstrap is running. This result will update automatically when it finishes."
      } else if (bootstrap_canceled) {
        if (ko) "다집단 잠재조절 층화 부트스트랩이 사용자 요청으로 중단되었습니다. 모형기반 결과는 유지되지만 부트스트랩 추론은 사용할 수 없습니다." else "The stratified multi-group latent-moderation bootstrap was canceled by the user. Model-based results remain available, but bootstrap inference is unavailable."
      } else if (nzchar(bootstrap_error)) {
        if (ko) paste0("다집단 잠재조절 층화 부트스트랩 실패: ", bootstrap_error) else paste0("Multi-group latent-moderation stratified bootstrap failed: ", bootstrap_error)
      } else {
        if (ko) paste0("다집단 잠재조절 층화 부트스트랩을 실행하지 않았습니다: ", bootstrap_blocked_reason) else paste0("The multi-group latent-moderation stratified bootstrap was not run: ", bootstrap_blocked_reason)
      }
    ),
    if (length(unsupported_moderated_mediation_paths)) result_note_paragraph(
      class = "structural-result-note structural-section-status-note text-warning",
      paste0("Indices were not reported for paths with multiple moderated stages: ", paste(structural_canvas_display_path(unsupported_moderated_mediation_paths, display_name), collapse = "; "), ".")
    ),
    if (is.list(product_factor_joint_gate)) tags$p(
      class = paste(
        "structural-result-note structural-section-status-note",
        if (!isTRUE(product_factor_joint_gate$passed)) "structural-result-warning" else ""
      ),
      paste0("Joint product-factor gate: ", if (isTRUE(product_factor_joint_gate$passed)) "passed" else "failed", ".")
    ),
    tags$h5(result_table_heading("mg_measurement_gate", "구조경로 비교 선행 측정불변성", "Measurement-invariance gate before structural comparison")),
    if (nrow(measurement_table)) structural_canvas_basic_html_table(
      measurement_table,
      class = "table table-striped table-bordered structural-group-gate-table",
      role = "main",
      orientation = "auto",
      note = result_sci_note_text(estimation = paste0("Metric-invariance gate: ", if (isTRUE(gate$passed)) "passed" else "failed", ". ", gate_reason))
    ),
    tags$h5(result_table_heading(
      "mg_structural_models",
      if (identical(path_scope, "selected")) "자유 구조경로 모형과 선택 경로 동일화 모형 비교" else "자유 구조경로 모형과 동일 구조경로 모형 비교",
      if (identical(path_scope, "selected")) "Free versus selected-path equality models" else "Free versus equal structural-path models"
    )),
    structural_canvas_basic_html_table(
      table,
      class = "table table-striped table-bordered structural-group-model-table",
      role = "main",
      orientation = "landscape",
      note = result_sci_note_text(estimation = paste0(
        "Both models retain metric invariance; equality tests target unstandardized B, whereas standardized beta is descriptive; ",
        if (identical(path_scope, "selected")) {
          "the equality model constrains the selected unstandardized paths, and delta chi-square is the omnibus test of that path set"
        } else {
          "the equality model constrains all unstandardized structural paths, and delta chi-square is the omnibus test of path equality"
        },
        if (identical(path_scope, "selected")) paste0("; selected paths: ", paste(selected_path_labels, collapse = ", ")) else ""
      ))
    ),
    if (nrow(path_estimates)) tagList(
      tags$h5(result_table_heading("mg_group_paths", if (identical(path_scope, "selected")) "선택 경로의 집단별 구조경로 계수" else "집단별 구조경로 계수", if (identical(path_scope, "selected")) "Group-specific estimates for selected paths" else "Group-specific structural path estimates")),
      structural_canvas_basic_html_table(
        path_estimates,
        class = "table table-striped table-bordered structural-multigroup-path-table structural-multigroup-path-estimates",
        role = "main",
        orientation = "landscape",
        note = result_sci_note_text(abbreviations = "B = unstandardized coefficient; beta = standardized coefficient", estimation = "B is tested for group equality; beta is descriptive")
      )
    ),
    if (nrow(formal_path_tests)) tagList(
      tags$h5(result_table_heading("mg_formal_tests", if (identical(path_scope, "selected")) "선택 경로의 공식 집단 동일성 Wald 검정" else "경로별 공식 집단 동일성 Wald 검정", if (identical(path_scope, "selected")) "Formal group-equality Wald tests for selected paths" else "Formal path-level group-equality Wald tests")),
      structural_canvas_basic_html_table(
        formal_path_tests,
        class = "table table-striped table-bordered structural-multigroup-path-table structural-multigroup-formal-path-tests",
        role = "main",
        orientation = "auto",
        note = result_sci_note_text(
          estimation = "Each row is an omnibus Wald chi-square test of B equality across groups",
          multiplicity = paste0("BH adjustment covers the ", if (identical(path_scope, "selected")) "selected" else "reported", " path family")
        )
      )
    ),
    if (nrow(path_differences)) tagList(
      tags$h5(result_table_heading("mg_pairwise", if (identical(path_scope, "selected")) "선택 경로의 집단 쌍 차이" else "경로별 집단 쌍 차이", if (identical(path_scope, "selected")) "Pairwise group differences for selected paths" else "Pairwise group differences by path")),
      structural_canvas_basic_html_table(
        path_differences,
        class = "table table-striped table-bordered structural-multigroup-path-table structural-multigroup-path-differences",
        role = "main",
        orientation = "landscape",
        note = result_sci_note_text(
          abbreviations = "SE = standard error; CI = confidence interval",
          estimation = "Delta B is group 1 minus group 2; SEs and 95% CIs are joint-model Wald contrasts",
          multiplicity = paste0("BH adjustment covers the ", if (identical(path_scope, "selected")) "selected" else "reported", " path-by-pair family")
        )
      )
    ) else result_note_paragraph(class = "structural-result-note", if (ko) "비교 가능한 구조경로가 충분하지 않아 경로별 집단 차이 표를 만들지 않았습니다." else "No path-level group-difference table was produced because comparable structural paths were unavailable."),
    if (nrow(interaction_group_estimates)) tagList(
      tags$h5(result_table_heading("mg_interaction_estimates", "집단별 잠재 조절효과", "Group-specific latent interaction effects")),
      structural_canvas_basic_html_table(
        interaction_group_estimates,
        class = "table table-striped table-bordered structural-multigroup-path-table structural-multigroup-interaction-estimates",
        role = "main",
        orientation = "landscape",
        note = result_sci_note_text(
          abbreviations = "B = unstandardized latent interaction coefficient; SE = standard error; CI = confidence interval",
          estimation = if (has_interaction_group_bootstrap) "SEs, CIs, and p values use within-group stratified bootstrap" else NULL,
          reference = "Within-group significance does not establish a between-group difference"
        )
      )
    ),
    if (nrow(interaction_omnibus_tests)) tagList(
      tags$h5(result_table_heading("mg_interaction_omnibus", "잠재 조절효과 집단 동일성 검정", "Omnibus group-equality tests of latent interaction effects")),
      structural_canvas_basic_html_table(
        interaction_omnibus_tests,
        class = "table table-striped table-bordered structural-multigroup-path-table structural-multigroup-interaction-omnibus",
        role = "main",
        orientation = "auto",
        note = result_sci_note_text(
          estimation = "Each row is a Wald chi-square test of unstandardized interaction-coefficient equality across groups",
          multiplicity = "BH adjustment covers the estimable interaction omnibus family"
        )
      )
    ),
    if (nrow(interaction_pairwise_differences)) tagList(
      tags$h5(result_table_heading("mg_interaction_pairwise", "잠재 조절효과 집단 쌍 차이", "Pairwise group differences in latent interaction effects")),
      structural_canvas_basic_html_table(
        interaction_pairwise_differences,
        class = "table table-striped table-bordered structural-multigroup-path-table structural-multigroup-interaction-pairwise",
        role = "main",
        orientation = "landscape",
        note = result_sci_note_text(
          abbreviations = "Delta B = group 1 minus group 2; CI = confidence interval",
          estimation = if (has_interaction_pairwise_bootstrap) "Bootstrap contrasts use draws jointly valid across all requested targets and groups" else NULL,
          multiplicity = "Use pairwise contrasts after the omnibus test when three or more groups are compared"
        )
      )
    ),
    if (nrow(moderated_mediation_group_indices)) tagList(
      tags$h5(result_table_heading("mg_modmed_indices", "집단별 조절된 매개효과 지수", "Group-specific indices of moderated mediation")),
      structural_canvas_basic_html_table(
        moderated_mediation_group_indices,
        class = "table table-striped table-bordered structural-multigroup-path-table structural-multigroup-modmed-indices",
        role = "main",
        orientation = "landscape",
        note = result_sci_note_text(
          abbreviations = "CI = confidence interval",
          estimation = if (has_group_index_bootstrap) {
            "Unstandardized indices and within-group stratified-bootstrap CIs are primary"
          } else if (has_modmed_bootstrap_recorded) {
            "Bootstrap inference was suppressed for insufficient valid replicates; Delta results are auxiliary"
          } else {
            "Delta-method results are auxiliary until stratified-bootstrap inference is available"
          },
          reference = "The product-indicator index is scale-dependent; no standardized index is reported"
        )
      )
    ),
    if (nrow(moderated_mediation_delta_tests)) tagList(
      tags$h5(result_table_heading("mg_modmed_delta", "조절된 매개효과 지수 집단 동일성 검정(Delta/Wald 보조)", "Group-equality tests of moderated-mediation indices (auxiliary Delta/Wald)")),
      structural_canvas_basic_html_table(
        moderated_mediation_delta_tests,
        class = "table table-striped table-bordered structural-multigroup-path-table structural-multigroup-modmed-delta",
        role = "main",
        orientation = "landscape",
        note = result_sci_note_text(
          estimation = if (has_pairwise_index_bootstrap) "Delta/Wald tests are auxiliary; stratified-bootstrap index contrasts are primary" else "Delta/Wald tests are auxiliary while usable stratified-bootstrap inference is unavailable"
        )
      )
    ),
    if (nrow(moderated_mediation_pairwise_differences)) tagList(
      tags$h5(result_table_heading("mg_modmed_pairwise", "조절된 매개효과 지수 집단 쌍 차이", "Pairwise group differences in indices of moderated mediation")),
      structural_canvas_basic_html_table(
        moderated_mediation_pairwise_differences,
        class = "table table-striped table-bordered structural-multigroup-path-table structural-multigroup-modmed-pairwise",
        role = "main",
        orientation = "landscape",
        note = result_sci_note_text(
          abbreviations = "CI = confidence interval",
          estimation = if (has_pairwise_index_bootstrap) {
            "Index differences are group 1 minus group 2; jointly valid stratified-bootstrap contrasts are primary"
          } else if (has_modmed_bootstrap_recorded) {
            "Bootstrap inference was suppressed for insufficient jointly valid replicates; Delta/Wald contrasts are auxiliary"
          } else {
            "Delta/Wald contrasts are auxiliary until stratified-bootstrap CIs and p values are available"
          },
          multiplicity = if (has_pairwise_index_bootstrap) "BH adjustment covers the reported pairwise-index family" else NULL
        )
      )
    ),
    NULL
  )
}

structural_canvas_invariance_result_ui <- function(bundle, language = statedu_initial_language(),
                                                   variable_table = NULL, labels = character(0),
                                                   table_number_fn = NULL) {
  result <- bundle$invariance_result %||% NULL
  if (is.null(result)) return(NULL)
  ko <- identical(normalize_app_language(language), "ko")
  if (identical(result$type %||% "", "structural_path_comparison")) {
    display_name <- structural_canvas_display_name_resolver(
      snapshot = bundle$snapshot %||% list(),
      variable_table = variable_table,
      labels = labels,
      moderation_definitions = bundle$diagnostics$moderation_definitions %||% bundle$moderation_definitions %||% list(),
      language = language
    )
    return(structural_canvas_structural_path_group_comparison_ui(
      result, ko, display_name, table_number_fn = table_number_fn,
      bootstrap_execution = list(
        requested = isTRUE(bundle$multigroup_moderation_bootstrap_requested),
        pending = isTRUE(bundle$multigroup_moderation_bootstrap_pending),
        canceled = isTRUE(bundle$multigroup_moderation_bootstrap_canceled),
        error = bundle$multigroup_moderation_bootstrap_error %||% "",
        blocked_reason = bundle$multigroup_moderation_bootstrap_blocked_reason %||% ""
      )
    ))
  }
  if (identical(result$type %||% "", "pls_micom")) {
    # Journal-facing MICOM/PLS-MGA tables have a fixed English publication
    # contract. UI-language localization belongs exclusively to
    # structural_canvas_invariance_appendix_ui().
    main_ko <- FALSE
    gate <- result$measurement_gate %||% list(passed = FALSE, reason = "MICOM gate was not recorded.")
    table <- result$table %||% data.frame()
    pairwise_gate <- result$pairwise_gate %||% data.frame()
    configural_audit <- result$configural_audit %||% data.frame()
    mga <- result$pls_mga %||% list()
    group_diagnostics <- result$group_diagnostics %||% mga$group_diagnostics %||% data.frame()
    group_effects <- mga$group_effects %||% result$group_effects %||% data.frame()
    pairwise_effects <- mga$pairwise_differences %||% result$pairwise_effect_differences %||% data.frame()
    group_validity <- mga$validity_gate$groups %||% data.frame()
    pair_validity <- mga$validity_gate$pairs %||% data.frame()
    sensitivity <- result$permutation_path_sensitivity %||% data.frame()
    modmed_mga <- result$pls_modmed_mga %||% mga$pls_modmed_mga %||% list()
    bind_group_modmed <- function(field) {
      rows <- lapply(names(modmed_mga$group_results %||% list()), function(group_name) {
        value <- modmed_mga$group_results[[group_name]][[field]] %||% data.frame()
        if (!is.data.frame(value) || !nrow(value)) return(NULL)
        data.frame(Group = group_name, value, check.names = FALSE, stringsAsFactors = FALSE)
      })
      rows <- Filter(Negate(is.null), rows)
      if (length(rows)) do.call(rbind, rows) else data.frame()
    }
    group_interactions <- bind_group_modmed("interaction_effects")
    group_simple_slopes <- bind_group_modmed("simple_slopes")
    group_modmed_indices <- bind_group_modmed("moderated_mediation")
    group_conditional_indirect <- bind_group_modmed("conditional_indirect")
    modmed_pairwise <- modmed_mga$pairwise_differences %||% data.frame()
    modmed_pair_validity <- modmed_mga$pairwise_validity %||% data.frame()
    display_name <- structural_canvas_display_name_resolver(
      snapshot = bundle$snapshot %||% list(),
      variable_table = variable_table, labels = labels,
      moderation_definitions = bundle$diagnostics$moderation_definitions %||%
        bundle$moderation_definitions %||% list(),
      language = "en"
    )
    pls_path_scope <- tolower(trimws(as.character(result$path_scope %||% mga$path_scope %||% "all")))
    pls_path_scope <- if (length(pls_path_scope) == 1L && pls_path_scope %in% c("all", "selected")) pls_path_scope else "all"
    pls_selected_registry <- result$selected_path_registry %||% mga$selected_path_registry %||% data.frame()
    pls_selected_labels <- if (identical(pls_path_scope, "selected") && is.data.frame(pls_selected_registry) &&
        nrow(pls_selected_registry) && "path" %in% names(pls_selected_registry)) {
      structural_canvas_display_path(as.character(pls_selected_registry$path), display_name)
    } else {
      character(0)
    }
    display_table <- function(value) {
      if (!is.data.frame(value) || !nrow(value)) return(value)
      structural_canvas_display_identifier_table(value, display_name)
    }
    format_numeric <- function(value, columns) {
      if (!is.data.frame(value) || !nrow(value)) return(value)
      for (name in intersect(columns, names(value))) {
        value[[name]] <- vapply(suppressWarnings(as.numeric(value[[name]])), format_decimal3, character(1))
      }
      value
    }
    format_probability <- function(value, columns) {
      if (!is.data.frame(value) || !nrow(value)) return(value)
      for (name in intersect(columns, names(value))) {
        value[[name]] <- vapply(suppressWarnings(as.numeric(value[[name]])), format_p, character(1))
      }
      value
    }
    combine_interval <- function(value, lower, upper, output) {
      if (!is.data.frame(value) || !nrow(value) || !all(c(lower, upper) %in% names(value))) return(value)
      lo <- suppressWarnings(as.numeric(value[[lower]]))
      hi <- suppressWarnings(as.numeric(value[[upper]]))
      value[[output]] <- ifelse(
        is.finite(lo) & is.finite(hi),
        paste0("[", vapply(lo, format_decimal3, character(1)), ", ", vapply(hi, format_decimal3, character(1)), "]"),
        ""
      )
      value[, c(setdiff(names(value), c(lower, upper, output)), output), drop = FALSE]
    }
    translate_values <- function(value, columns, translations, na_label = NULL) {
      if (!main_ko || !is.data.frame(value) || !nrow(value)) return(value)
      for (name in intersect(columns, names(value))) {
        source <- value[[name]]
        output <- as.character(source)
        matched <- match(output, names(translations))
        replace <- !is.na(matched)
        output[replace] <- unname(translations[matched[replace]])
        if (!is.null(na_label)) output[is.na(source)] <- na_label
        value[[name]] <- output
      }
      value
    }
    translate_logical <- function(value, columns, true_label = "통과", false_label = "실패") {
      if (!main_ko || !is.data.frame(value) || !nrow(value)) return(value)
      for (name in intersect(columns, names(value))) {
        source <- suppressWarnings(as.logical(value[[name]]))
        value[[name]] <- ifelse(is.na(source), "산출 불가", ifelse(source, true_label, false_label))
      }
      value
    }
    rename_korean_columns <- function(value) {
      if (!main_ko || !is.data.frame(value)) return(value)
      translations <- c(
        Criterion = "점검 기준", Passed = "통과", Evidence = "근거",
        Group = "집단", `Group 1` = "집단 1", `Group 2` = "집단 2",
        `N 1` = "집단 1 N", `N 2` = "집단 2 N", Construct = "구성개념",
        `Complete indicator cases` = "지표 완전 사례 수",
        `Indicator missing %` = "지표 결측률(%)", `N warning` = "표본수 경고",
        `Group Seed` = "집단별 난수 시드", `Missing-data handling` = "결측 처리",
        `Small-group warning` = "소집단 경고", Status = "상태",
        `Pair permutation adequate` = "집단쌍 순열 충분",
        `Observed c` = "관측 c", `5% permutation c` = "순열 c 5% 분위수",
        `Compositional permutation p` = "합성불변성 순열 p",
        `Compositional Holm p` = "합성불변성 Holm p",
        `Compositional invariance` = "합성불변성",
        `Mean difference` = "평균 차이", `Mean permutation 95% interval` = "평균 차이 순열 95% 구간",
        `Mean permutation p` = "평균 차이 순열 p", `Mean Holm p` = "평균 차이 Holm p",
        `Mean equality` = "평균 동등성", `Variance difference` = "분산 차이",
        `Log variance ratio` = "로그 분산비",
        `Log-variance permutation 95% interval` = "로그 분산비 순열 95% 구간",
        `Variance permutation p` = "분산 차이 순열 p", `Variance Holm p` = "분산 차이 Holm p",
        `Variance equality` = "분산 동등성", `Invariance level` = "불변성 수준",
        `Small-N warning` = "소표본 경고",
        `Composite-score invariance gate` = "합성점수 불변성 허용",
        `Constructs passed` = "통과 구성개념 수", `Constructs tested` = "검정 구성개념 수",
        `Valid permutations` = "유효 순열 수", `Requested permutations` = "요청 순열 수",
        `Valid ratio` = "유효 비율", `Pair seed` = "집단쌍 난수 시드", Reason = "사유",
        `Effect Family Label` = "효과 구분", Path = "경로",
        Estimate = "추정값", `Estimate Group 1` = "집단 1 추정값",
        `Estimate Group 2` = "집단 2 추정값", Difference = "차이(집단 1-집단 2)",
        `Bootstrap SE` = "부트스트랩 SE", `T Stat.` = "t",
        `Bootstrap 95% CI` = "부트스트랩 95% CI",
        `Bootstrap difference 95% CI` = "차이의 부트스트랩 95% CI",
        `Bootstrap P Val` = "부트스트랩 p", `BH-adjusted p` = "BH 보정 p",
        `Holm-adjusted p` = "Holm 보정 p", `Bootstrap Status` = "부트스트랩 상태",
        `Valid N` = "유효 재표집 수", `Requested N` = "요청 재표집 수",
        `Minimum Valid N` = "최소 유효 재표집 수", `MICOM admitted` = "MICOM 비교 허용",
        `MICOM reason` = "MICOM 사유", Predictor = "예측변수", Moderator = "조절변수",
        Outcome = "결과변수", `Interaction Factor` = "상호작용항",
        `Downstream Path` = "후속 경로", `Moderator Level` = "조절변수 수준",
        `Moderator Position` = "조절변수 위치", `Effect Family` = "효과 구분",
        `Bootstrap Mean` = "부트스트랩 평균",
        `Moderator level` = "조절변수 수준", `Moderator value` = "조절변수 값",
        `Direct effect` = "직접효과", `Interaction effect` = "상호작용효과",
        `Simple slope` = "단순기울기", `Bootstrap mean` = "부트스트랩 평균",
        `95% CI lower` = "95% CI 하한", `95% CI upper` = "95% CI 상한",
        p = "부트스트랩 p", `Valid replicates` = "유효 재표집 수",
        `Requested replicates` = "요청 재표집 수", `Valid ratio` = "유효 비율",
        `Inference available` = "추론 가능",
        `Bootstrap Mean Difference` = "부트스트랩 평균 차이",
        `Inference Source` = "추론 근거",
        `Path difference` = "경로 차이", `Permutation p` = "순열 p",
        `MGA permutation adequate` = "MGA 순열 충분"
      )
      positions <- match(names(value), names(translations))
      replace <- !is.na(positions)
      names(value)[replace] <- unname(translations[positions[replace]])
      value
    }
    category_translations <- c(
      "Direct effect / structural path" = "직접효과/구조경로",
      "Specific indirect effect" = "특정 간접효과",
      "Total indirect effect" = "총 간접효과", "Total effect" = "총효과",
      moderation = "상호작용효과", moderated_mediation_index = "조절된 매개효과 지수",
      conditional_indirect = "조건부 간접효과",
      `-1 SD` = "평균 - 1 SD", Mean = "평균", `+1 SD` = "평균 + 1 SD",
      Adequate = "충분", Insufficient = "불충분",
      `Blocked by MICOM` = "MICOM으로 차단", `Partially available` = "일부 집단쌍만 추론 가능",
      Passed = "통과", Failed = "실패",
      Unavailable = "산출 불가", None = "없음", Partial = "부분", Full = "완전",
      `Within-group mean replacement` = "집단별 평균 대체",
      `No group-size flag` = "집단 크기 경고 없음",
      `Small group (N < 30); permutation estimates may be unstable` = "소집단(N < 30): 순열 추정이 불안정할 수 있음",
      `At least one group has N < 30; review stability` = "하나 이상의 집단이 N < 30이므로 안정성을 확인하십시오",
      `Every construct passed compositional invariance after the global Holm MICOM adjustment.` = "모든 구성개념이 전체 Holm MICOM 보정 후 합성불변성을 통과했습니다.",
      `One or more constructs failed compositional invariance after the global Holm MICOM adjustment.` = "하나 이상의 구성개념이 전체 Holm MICOM 보정 후 합성불변성을 통과하지 못했습니다.",
      `Pairwise permutation-validity gate failed.` = "집단쌍 순열 유효성 기준을 통과하지 못했습니다."
    )
    configural_criterion_translations <- c(
      `Identical indicators and model specification` = "동일한 지표와 모형 명세",
      `Identical missing-data and standardization treatment` = "동일한 결측 처리와 표준화 방식",
      `Identical PLS algorithm and settings` = "동일한 PLS 알고리즘과 설정"
    )
    configural_evidence_translations <- c(
      `Every pair uses seminr::mean_replacement in group and pooled fits; the pair-pooled mean and SD define the common Step 2/3 score scale.` = "모든 집단쌍의 집단별·통합 적합에 seminr::mean_replacement를 사용하며, 집단쌍 통합 평균과 표준편차로 2·3단계의 공통 점수 척도를 정의합니다.",
      `Every fit uses run_structural_canvas_analysis(..., analysis_type='plssem', estimator='PLS') with the shared production settings.` = "모든 적합은 동일한 운영 설정으로 run_structural_canvas_analysis(..., analysis_type='plssem', estimator='PLS')를 사용합니다."
    )
    table <- combine_interval(table, "2.5% permutation mean difference", "97.5% permutation mean difference", "Mean permutation 95% interval")
    table <- combine_interval(table, "2.5% permutation log variance ratio", "97.5% permutation log variance ratio", "Log-variance permutation 95% interval")
    table <- format_numeric(table, c("Observed c", "5% permutation c", "Mean difference", "Variance difference", "Log variance ratio", "Permutation valid ratio"))
    table <- format_probability(table, c(
      "Compositional permutation p", "Compositional Holm p", "Mean permutation p",
      "Mean Holm p", "Variance permutation p", "Variance Holm p"
    ))
    table <- display_table(table)
    group_effects <- combine_interval(group_effects, "2.5% CI", "97.5% CI", "Bootstrap 95% CI")
    group_effects <- format_numeric(group_effects, c("Estimate", "Bootstrap Mean", "Bootstrap SE", "T Stat.", "Valid Ratio"))
    group_effects <- format_probability(group_effects, "Bootstrap P Val")
    group_effects <- display_table(group_effects)
    if (nrow(group_effects)) group_effects <- group_effects[, intersect(c(
      "Effect Family Label", "Path", "Group", "Estimate", "Bootstrap SE", "T Stat.",
      "Bootstrap 95% CI", "Bootstrap P Val", "Bootstrap Status", "Valid N", "Requested N"
    ), names(group_effects)), drop = FALSE]
    pairwise_effects <- combine_interval(pairwise_effects, "2.5% CI", "97.5% CI", "Bootstrap difference 95% CI")
    pairwise_effects <- format_numeric(pairwise_effects, c(
      "Estimate Group 1", "Estimate Group 2", "Difference", "Bootstrap Mean Difference",
      "Bootstrap SE", "T Stat.", "Valid Ratio"
    ))
    pairwise_effects <- format_probability(pairwise_effects, c("Bootstrap P Val", "BH-adjusted p", "Holm-adjusted p"))
    pairwise_effects <- display_table(pairwise_effects)
    if (nrow(pairwise_effects)) pairwise_effects <- pairwise_effects[, intersect(c(
      "Effect Family Label", "Path", "Group 1", "Group 2", "Estimate Group 1",
      "Estimate Group 2", "Difference", "Bootstrap SE", "Bootstrap difference 95% CI",
      "Bootstrap P Val", "BH-adjusted p", "Holm-adjusted p", "Bootstrap Status", "Valid N", "Requested N"
    ), names(pairwise_effects)), drop = FALSE]
    sensitivity <- format_numeric(display_table(sensitivity), "Path difference")
    sensitivity <- format_probability(sensitivity, c("Permutation p", "BH-adjusted p"))
    pairwise_gate <- format_numeric(pairwise_gate, "Valid ratio")
    group_validity <- format_numeric(group_validity, "Valid Ratio")
    pair_validity <- format_numeric(pair_validity, "Valid Ratio")
    display_modmed <- function(value) {
      if (!is.data.frame(value) || !nrow(value)) return(value)
      value <- display_table(value)
      if ("Downstream Path" %in% names(value)) {
        path <- as.character(value[["Downstream Path"]])
        present <- !is.na(path) & nzchar(trimws(path))
        path[present] <- structural_canvas_display_path(path[present], display_name)
        value[["Downstream Path"]] <- path
      }
      if (all(c("Predictor", "Moderator", "Interaction Factor") %in% names(value))) {
        value[["Interaction Factor"]] <- paste(value$Predictor, "x", value$Moderator)
      }
      value
    }
    prepare_group_modmed <- function(value, conditional = FALSE) {
      value <- combine_interval(value, "2.5% CI", "97.5% CI", "Bootstrap 95% CI")
      value <- format_numeric(value, c("Estimate", "Bootstrap Mean", "Bootstrap SE", "Valid Ratio", "Moderator Position"))
      value <- format_probability(value, c("Bootstrap P Val", "BH-adjusted p"))
      value <- display_modmed(value)
      preferred <- c(
        "Group", "Effect Family", "Path", "Predictor", "Moderator", "Outcome",
        "Downstream Path", if (conditional) c("Moderator Level", "Moderator Position") else character(0),
        "Estimate", "Bootstrap SE", "Bootstrap 95% CI", "Bootstrap P Val",
        "BH-adjusted p", "Bootstrap Status", "Valid N", "Requested N"
      )
      value[, intersect(preferred, names(value)), drop = FALSE]
    }
    group_interactions <- prepare_group_modmed(group_interactions)
    prepare_group_simple_slopes <- function(value) {
      value <- combine_interval(value, "95% CI lower", "95% CI upper", "Bootstrap 95% CI")
      value <- format_numeric(value, c(
        "Moderator value", "Direct effect", "Interaction effect", "Simple slope",
        "Bootstrap mean", "Bootstrap SE", "Valid ratio"
      ))
      value <- format_probability(value, c("p", "BH-adjusted p"))
      value <- display_modmed(value)
      preferred <- c(
        "Group", "Predictor", "Moderator", "Outcome", "Moderator level",
        "Moderator value", "Simple slope", "Bootstrap SE", "Bootstrap 95% CI",
        "p", "BH-adjusted p", "Bootstrap Status", "Inference Source",
        "Valid replicates", "Requested replicates", "Valid ratio", "Inference available"
      )
      value[, intersect(preferred, names(value)), drop = FALSE]
    }
    group_simple_slopes <- prepare_group_simple_slopes(group_simple_slopes)
    group_modmed_indices <- prepare_group_modmed(group_modmed_indices)
    group_conditional_indirect <- prepare_group_modmed(group_conditional_indirect, conditional = TRUE)
    prepare_pair_modmed <- function(value) {
      value <- combine_interval(value, "2.5% CI", "97.5% CI", "Bootstrap difference 95% CI")
      value <- format_numeric(value, c(
        "Estimate Group 1", "Estimate Group 2", "Difference", "Bootstrap Mean Difference",
        "Bootstrap SE", "Valid Ratio"
      ))
      value <- format_probability(value, c("Bootstrap P Val", "BH-adjusted p", "Holm-adjusted p"))
      value <- display_modmed(value)
      if (nrow(value)) value <- value[, intersect(c(
        "Effect Family", "Path", "Predictor", "Moderator", "Outcome", "Downstream Path",
        "Group 1", "Group 2", "Estimate Group 1", "Estimate Group 2", "Difference",
        "Bootstrap SE", "Bootstrap difference 95% CI", "Bootstrap P Val",
        "BH-adjusted p", "Holm-adjusted p", "Bootstrap Status", "MICOM admitted",
        "Valid N", "Requested N"
      ), names(value)), drop = FALSE]
      value
    }
    modmed_pairwise_moderation <- if (is.data.frame(modmed_pairwise) && nrow(modmed_pairwise)) {
      modmed_pairwise[as.character(modmed_pairwise[["Effect Family"]]) == "moderation", , drop = FALSE]
    } else data.frame()
    modmed_pairwise_indices <- if (is.data.frame(modmed_pairwise) && nrow(modmed_pairwise)) {
      modmed_pairwise[as.character(modmed_pairwise[["Effect Family"]]) == "moderated_mediation_index", , drop = FALSE]
    } else data.frame()
    modmed_pairwise_moderation <- prepare_pair_modmed(modmed_pairwise_moderation)
    modmed_pairwise_indices <- prepare_pair_modmed(modmed_pairwise_indices)
    modmed_pair_validity <- format_numeric(modmed_pair_validity, "Valid Ratio")
    pair_gate_passed <- if (
      is.data.frame(pairwise_gate) && nrow(pairwise_gate) &&
        "Composite-score invariance gate" %in% names(pairwise_gate)
    ) {
      suppressWarnings(as.logical(pairwise_gate[["Composite-score invariance gate"]]))
    } else logical(0)
    any_pair_passed <- any(pair_gate_passed, na.rm = TRUE)
    if (main_ko) {
      configural_audit <- translate_values(
        configural_audit, "Criterion", configural_criterion_translations
      )
      configural_audit <- translate_values(
        configural_audit, "Evidence", configural_evidence_translations
      )
      if (is.data.frame(configural_audit) && nrow(configural_audit) && "Evidence" %in% names(configural_audit)) {
        configural_audit$Evidence <- sub(
          "^One shared canvas snapshot supplies ([0-9]+) constructs and ([0-9]+) indicators to every group fit\\.$",
          "하나의 공통 캔버스 스냅샷이 모든 집단 적합에 \\1개 구성개념과 \\2개 지표를 제공합니다.",
          as.character(configural_audit$Evidence)
        )
      }
      configural_audit <- translate_logical(configural_audit, "Passed")
      group_diagnostics <- translate_values(
        group_diagnostics, names(group_diagnostics), category_translations
      )
      group_diagnostics <- translate_logical(
        group_diagnostics, "Small-group warning", true_label = "있음", false_label = "없음"
      )
      table <- translate_values(table, names(table), category_translations)
      table <- translate_logical(table, c(
        "Pair permutation adequate", "Compositional invariance", "Mean equality", "Variance equality"
      ))
      pairwise_gate <- translate_values(pairwise_gate, names(pairwise_gate), category_translations)
      pairwise_gate <- translate_logical(pairwise_gate, "Composite-score invariance gate")
      group_effects <- translate_values(group_effects, names(group_effects), category_translations)
      pairwise_effects <- translate_values(pairwise_effects, names(pairwise_effects), category_translations)
      group_validity <- translate_values(group_validity, names(group_validity), category_translations)
      pair_validity <- translate_values(pair_validity, names(pair_validity), category_translations)
      pair_validity <- translate_logical(pair_validity, "MICOM admitted")
      group_interactions <- translate_values(group_interactions, names(group_interactions), category_translations)
      group_simple_slopes <- translate_values(group_simple_slopes, names(group_simple_slopes), category_translations)
      group_modmed_indices <- translate_values(group_modmed_indices, names(group_modmed_indices), category_translations)
      group_conditional_indirect <- translate_values(group_conditional_indirect, names(group_conditional_indirect), category_translations)
      modmed_pairwise_moderation <- translate_values(modmed_pairwise_moderation, names(modmed_pairwise_moderation), category_translations)
      modmed_pairwise_indices <- translate_values(modmed_pairwise_indices, names(modmed_pairwise_indices), category_translations)
      modmed_pairwise_moderation <- translate_logical(modmed_pairwise_moderation, "MICOM admitted")
      modmed_pairwise_indices <- translate_logical(modmed_pairwise_indices, "MICOM admitted")
      modmed_pair_validity <- translate_values(modmed_pair_validity, names(modmed_pair_validity), category_translations)
      modmed_pair_validity <- translate_logical(modmed_pair_validity, "MICOM admitted")
      sensitivity <- translate_values(sensitivity, names(sensitivity), category_translations)
      sensitivity <- translate_logical(sensitivity, "MGA permutation adequate")
      if (is.data.frame(group_diagnostics) && nrow(group_diagnostics) && "Status" %in% names(group_diagnostics)) {
        group_diagnostics$Status <- sub(
          "^Small group \\(N < ([0-9]+)\\); review bootstrap stability and power$",
          "소집단(N < \\1): 부트스트랩 안정성과 검정력을 확인하십시오",
          as.character(group_diagnostics$Status)
        )
      }
      configural_audit <- rename_korean_columns(configural_audit)
      group_diagnostics <- rename_korean_columns(group_diagnostics)
      table <- rename_korean_columns(table)
      pairwise_gate <- rename_korean_columns(pairwise_gate)
      group_effects <- rename_korean_columns(group_effects)
      pairwise_effects <- rename_korean_columns(pairwise_effects)
      group_validity <- rename_korean_columns(group_validity)
      pair_validity <- rename_korean_columns(pair_validity)
      group_interactions <- rename_korean_columns(group_interactions)
      group_simple_slopes <- rename_korean_columns(group_simple_slopes)
      group_modmed_indices <- rename_korean_columns(group_modmed_indices)
      group_conditional_indirect <- rename_korean_columns(group_conditional_indirect)
      modmed_pairwise_moderation <- rename_korean_columns(modmed_pairwise_moderation)
      modmed_pairwise_indices <- rename_korean_columns(modmed_pairwise_indices)
      modmed_pair_validity <- rename_korean_columns(modmed_pair_validity)
      sensitivity <- rename_korean_columns(sensitivity)
    }
    first_text <- function(value, fallback = "") {
      output <- as.character(value %||% character(0))
      if (length(output) && !is.na(output[[1L]]) && nzchar(output[[1L]])) output[[1L]] else fallback
    }
    mga_status_raw <- first_text(mga$status, "Not recorded")
    mga_reason_raw <- first_text(mga$reason)
    mga_status_label <- if (main_ko) {
      translated <- unname(category_translations[mga_status_raw])
      if (length(translated) && !is.na(translated)) translated else "기록되지 않음"
    } else mga_status_raw
    mga_reason_label <- if (main_ko) {
      reason_translations <- c(
        `Inference is available for adequate MICOM-admitted pairs; one or more other pairs were blocked or failed the 80% common-position bootstrap gate.` = "MICOM을 통과하고 부트스트랩 기준을 충족한 집단쌍에는 추론을 제공하며, 나머지 집단쌍은 MICOM 또는 공통 유효 재표집 80% 기준으로 차단했습니다.",
        `Every MICOM-admitted pair failed the 80% common-position bootstrap gate, or a group bootstrap was inadequate.` = "MICOM을 통과한 모든 집단쌍이 공통 유효 재표집 80% 기준을 충족하지 못했거나 집단별 부트스트랩이 불충분합니다.",
        `No group pair passed the MICOM composite-invariance gate. Group-specific effects remain descriptive.` = "MICOM 합성불변성 기준을 통과한 집단쌍이 없습니다. 집단별 효과는 기술적으로만 제시합니다."
      )
      translated <- unname(reason_translations[mga_reason_raw])
      if (length(translated) && !is.na(translated)) translated else if (nzchar(mga_reason_raw)) "세부 사유는 감사 기록을 확인하십시오." else ""
    } else mga_reason_raw
    format_count <- function(value) {
      numeric_value <- suppressWarnings(as.numeric(value %||% NA_real_))
      if (length(numeric_value) && is.finite(numeric_value[[1L]])) {
        format(round(numeric_value[[1L]]), big.mark = ",", scientific = FALSE, trim = TRUE)
      } else ""
    }
    group_seeds <- unlist(mga$group_seeds %||% integer(0), use.names = TRUE)
    if (length(group_seeds) && (is.null(names(group_seeds)) || any(!nzchar(names(group_seeds))))) {
      names(group_seeds) <- as.character(mga$groups %||% seq_along(group_seeds))
    }
    group_seed_text <- if (length(group_seeds)) {
      paste0(names(group_seeds), "=", vapply(group_seeds, format_count, character(1)), collapse = ", ")
    } else ""
    bootstrap_count <- format_count(mga$bootstrap_reps_requested %||% NA_integer_)
    master_seed <- format_count(mga$seed %||% NA_integer_)
    reproducibility_parts <- c(
      if (main_ko) paste0("PLS-MGA 상태: ", mga_status_label) else paste0("PLS-MGA status: ", mga_status_label),
      if (nzchar(bootstrap_count)) {
        if (main_ko) paste0("요청 부트스트랩: ", bootstrap_count, "회") else paste0("requested bootstrap replicates: ", bootstrap_count)
      },
      if (nzchar(master_seed)) {
        if (main_ko) paste0("기준 난수 시드: ", master_seed) else paste0("master seed: ", master_seed)
      },
      if (nzchar(group_seed_text)) {
        if (main_ko) paste0("집단별 난수 시드: ", group_seed_text) else paste0("group seeds: ", group_seed_text)
      }
    )
    mga_reproducibility <- paste0(
      paste(reproducibility_parts, collapse = "; "), ".",
      if (nzchar(mga_reason_label)) paste0(" ", mga_reason_label) else ""
    )
    empty_pairwise_message <- if (main_ko) {
      paste0("집단쌍 차이 추론을 계산하지 않았습니다. 상태: ", mga_status_label, ".", if (nzchar(mga_reason_label)) paste0(" ", mga_reason_label) else "")
    } else {
      paste0("Pairwise effect inference was not computed. Status: ", mga_status_label, ".", if (nzchar(mga_reason_label)) paste0(" ", mga_reason_label) else "")
    }
    empty_group_message <- if (main_ko) {
      paste0("집단별 구조효과를 계산하지 않았습니다. 상태: ", mga_status_label, ".", if (nzchar(mga_reason_label)) paste0(" ", mga_reason_label) else "")
    } else {
      paste0("Group-specific structural effects were not computed. Status: ", mga_status_label, ".", if (nzchar(mga_reason_label)) paste0(" ", mga_reason_label) else "")
    }
    gate_reason <- if (main_ko) {
      if (isTRUE(gate$passed)) {
        "모든 구성개념과 집단쌍에서 Holm 보정 합성불변성 기준을 통과했습니다."
      } else if (any_pair_passed) {
        "일부 집단쌍만 Holm 보정 합성불변성 기준을 통과했습니다. 통과한 집단쌍에만 구조효과 차이 추론을 제공합니다."
      } else {
        "Holm 보정 합성불변성 기준을 통과한 집단쌍이 없어 집단쌍 구조효과 차이 추론을 차단했습니다."
      }
    } else as.character(gate$reason %||% "MICOM gate was not recorded.")
    gate_label <- if (isTRUE(gate$passed)) {
      if (main_ko) "통과" else "passed"
    } else if (any_pair_passed) {
      if (main_ko) "부분 통과" else "partially passed"
    } else {
      if (main_ko) "실패" else "failed"
    }
    return(div(class = "result-section regression-result-panel structural-main-result-panel structural-invariance-result structural-micom-result",
      h4(if (main_ko) paste0("PLS 합성점수 측정불변성(MICOM): ", result$group) else paste0("PLS composite-score measurement invariance (MICOM) by ", result$group)),
      if (identical(pls_path_scope, "selected")) result_note_paragraph(
        class = "structural-result-note",
        if (main_ko) {
          paste0("선택 직접경로 비교: ", paste(pls_selected_labels, collapse = ", "), ". 직접효과 및 직접경로 순열 민감도 검정군만 이 선택에 맞춰 제한하며, 특정·총 간접효과, 총효과, 잠재 조절효과와 조절된 매개효과 검정군은 유지합니다.")
        } else {
          paste0("Selected direct-path comparison: ", paste(pls_selected_labels, collapse = ", "), ". Only the direct-effect and direct-path permutation-sensitivity families are restricted; specific/total indirect, total, latent-moderation, and moderated-mediation families remain available.")
        }
      ),
      tags$h5(if (main_ko) "MICOM 2·3단계 결과" else "MICOM Steps 2 and 3"),
      structural_canvas_basic_html_table(
        table,
        role = "main",
        orientation = "landscape",
        note = result_sci_note_text(
          estimation = paste0("MICOM gate: ", gate_label, "; Step 2 gates pairwise structural comparisons and Step 3 classifies full invariance"),
          reference = paste0("Requested permutations: ", result$permutations_requested %||% "", "; seed=", result$seed %||% "", "; ", mga_reproducibility),
          symbol = "MICOM does not establish PLSc common-factor invariance"
        ),
        note_class = paste("structural-result-note structural-main-note", if (!isTRUE(gate$passed) || !identical(mga_status_raw, "Adequate")) "structural-result-warning" else "")
      ),
      tags$h5(if (main_ko) "집단별 PLS 구조효과" else "Group-specific PLS structural effects"),
      if (nrow(group_effects)) structural_canvas_basic_html_table(
        group_effects,
        class = "table table-striped table-bordered structural-pls-mga-table",
        role = "main",
        orientation = "landscape",
        note = result_sci_note_text(estimation = "Effects use a common path registry and within-group case bootstrap; the complete PLS model is re-estimated in every replicate")
      ) else result_note_paragraph(class = "structural-result-note", empty_group_message),
      tags$h5(if (main_ko) "PLS-MGA 집단쌍 구조효과 차이" else "PLS-MGA pairwise structural-effect differences"),
      if (nrow(pairwise_effects)) structural_canvas_basic_html_table(
        pairwise_effects,
        class = "table table-striped table-bordered structural-pls-mga-table",
        role = "main",
        orientation = "landscape",
        note = result_sci_note_text(
          estimation = "Each contrast is group 1 minus group 2 and is reported only for MICOM-admitted pairs meeting the common-valid-replicate gate",
          multiplicity = "Raw, BH-adjusted, and Holm-adjusted p values are reported within each effect family"
        )
      ) else result_note_paragraph(class = "structural-result-note", empty_pairwise_message),
      if (nrow(group_interactions)) tagList(
        tags$h5(if (main_ko) "집단별 PLS 잠재 조절효과" else "Group-specific PLS latent moderation effects"),
        structural_canvas_basic_html_table(
          group_interactions,
          class = "table table-striped table-bordered structural-pls-mga-table structural-pls-mga-moderation",
          role = "main",
          orientation = "landscape",
          note = result_sci_note_text(
            abbreviations = "CI = confidence interval",
            estimation = "Interaction effects are unstandardized PLS construct-score paths; bootstrap inference uses whole-draw valid replicates",
            symbol = "Interaction terms are not PLSc consistency-corrected"
          )
        )
      ),
      if (nrow(group_simple_slopes)) tagList(
        tags$h5(if (main_ko) "집단별 PLS 단순기울기" else "Group-specific PLS simple slopes"),
        structural_canvas_basic_html_table(
          group_simple_slopes,
          class = "table table-striped table-bordered structural-pls-mga-table structural-pls-mga-simple-slopes",
          role = "main",
          orientation = "landscape",
          note = result_sci_note_text(
            abbreviations = "SD = standard deviation; CI = confidence interval",
            estimation = "Simple slopes are evaluated at the moderator mean and mean plus or minus 1 SD with bootstrap 95% CIs",
            multiplicity = "BH-adjusted p values accompany raw p values",
            symbol = "This table does not test between-group simple-slope differences"
          )
        )
      ),
      if (nrow(group_modmed_indices)) tagList(
        tags$h5(if (main_ko) "집단별 PLS 조절된 매개효과 지수" else "Group-specific PLS indices of moderated mediation"),
        structural_canvas_basic_html_table(
          group_modmed_indices,
          class = "table table-striped table-bordered structural-pls-mga-table structural-pls-mga-modmed",
          role = "main",
          orientation = "landscape",
          note = result_sci_note_text(
            abbreviations = "CI = confidence interval",
            estimation = "The index is the interaction-path coefficient times the downstream indirect-path coefficients; use the bootstrap 95% CI for inference",
            symbol = "The index is unstandardized on the PLS construct-score scale"
          )
        )
      ),
      if (nrow(group_conditional_indirect)) tagList(
        tags$h5(if (main_ko) "집단별 PLS 조건부 간접효과" else "Group-specific PLS conditional indirect effects"),
        structural_canvas_basic_html_table(
          group_conditional_indirect,
          class = "table table-striped table-bordered structural-pls-mga-table structural-pls-mga-conditional-indirect",
          role = "main",
          orientation = "landscape",
          note = result_sci_note_text(estimation = "Conditional indirect effects use the within-group bootstrap", symbol = "Effects are unstandardized on the PLS construct-score scale")
        )
      ),
      if (nrow(modmed_pairwise_moderation)) tagList(
        tags$h5(if (main_ko) "PLS-MGA 집단쌍 잠재 조절효과 차이" else "PLS-MGA pairwise differences in latent moderation effects"),
        structural_canvas_basic_html_table(
          modmed_pairwise_moderation,
          class = "table table-striped table-bordered structural-pls-mga-table structural-pls-mga-moderation-difference",
          role = "main",
          orientation = "landscape",
          note = result_sci_note_text(
            estimation = "Each difference is group 1 minus group 2 and is reported only for MICOM-admitted pairs meeting the 80% common-valid-replicate gate",
            multiplicity = "BH- and Holm-adjusted p values accompany raw p values; no omnibus test is provided"
          )
        )
      ),
      if (nrow(modmed_pairwise_indices)) tagList(
        tags$h5(if (main_ko) "PLS-MGA 집단쌍 조절된 매개효과 지수 차이" else "PLS-MGA pairwise differences in indices of moderated mediation"),
        structural_canvas_basic_html_table(
          modmed_pairwise_indices,
          class = "table table-striped table-bordered structural-pls-mga-table structural-pls-mga-modmed-difference",
          role = "main",
          orientation = "landscape",
          note = result_sci_note_text(
            estimation = "Each difference is group 1 minus group 2 and is reported only for MICOM-admitted pairs meeting the 80% common-valid-replicate gate",
            multiplicity = "BH- and Holm-adjusted p values accompany raw p values; no omnibus test is provided"
          )
        )
      ),
      NULL
    ))
  }
  table <- result$table
  group_table <- result$group_diagnostics
  group_reliability <- result$group_reliability %||% data.frame()
  group_htmt <- result$group_htmt %||% data.frame()
  group_residuals <- result$group_residuals %||% list()
  residual_summary <- if (isTRUE(group_residuals$available)) group_residuals$group_summary %||% data.frame() else data.frame()
  residual_largest <- if (isTRUE(group_residuals$available)) group_residuals$group_largest %||% data.frame() else data.frame()
  partial_invariance <- result$partial_invariance %||% structural_canvas_partial_invariance_status()
  group_table[["Indicator missing %"]] <- paste0(vapply(group_table[["Indicator missing %"]], format_decimal3, character(1)), "%")
  if (nrow(group_reliability)) {
    for (name in intersect(c("AVE", "CR", "Cronbach's alpha", "Omega total"), names(group_reliability))) {
      group_reliability[[name]] <- vapply(group_reliability[[name]], format_decimal3, character(1))
    }
  }
  if (nrow(group_htmt)) {
    for (name in intersect(c("HTMT"), names(group_htmt))) {
      group_htmt[[name]] <- vapply(group_htmt[[name]], format_decimal3, character(1))
    }
  }
  if (nrow(residual_summary)) {
    residual_summary[["Max |standardized residual|"]] <- vapply(residual_summary[["Max |standardized residual|"]], format_decimal3, character(1))
  }
  if (nrow(residual_largest)) {
    residual_largest[["Standardized residual"]] <- vapply(residual_largest[["Standardized residual"]], format_decimal3, character(1))
    residual_largest[["Correlation residual"]] <- vapply(residual_largest[["Correlation residual"]], format_decimal3, character(1))
  }
  srmr_limit <- ifelse(table$Model == "Metric", .030, .010)
  table$Decision <- ifelse(
    !table$Admissible, "Inadmissible stage",
    ifelse(table$Model == "Configural", "Baseline stage",
    ifelse(table$Admissible & table$DeltaCFI >= -.010 & table$DeltaRMSEA <= .015 & table$DeltaSRMR <= srmr_limit, "Descriptive change guidance met", "Review descriptive change")
  ))
  reviewed_stages <- table$Model[table$Decision == "Review descriptive change"]
  score_tables <- (result$score_diagnostics %||% list())[intersect(reviewed_stages, names(result$score_diagnostics %||% list()))]
  score_tables <- Filter(function(value) nrow(value), score_tables)
  if (ko) {
    decision_labels <- c(
      "Inadmissible stage" = "허용 불가 단계",
      "Baseline stage" = "기준 단계",
      "Descriptive change guidance met" = "기술적 변화 기준 충족",
      "Review descriptive change" = "기술적 변화 검토"
    )
    table$Decision <- ifelse(table$Decision %in% names(decision_labels), decision_labels[table$Decision], table$Decision)
  }
  numeric_columns <- c("Chisq", "df", "CFI", "RMSEA", "SRMR", "DeltaCFI", "DeltaRMSEA", "DeltaSRMR", "DeltaChisq", "DeltaDf", "Residual min eigenvalue", "Latent min eigenvalue", "Parameter min eigenvalue")
  for (name in numeric_columns) table[[name]] <- vapply(table[[name]], format_decimal3, character(1))
  for (name in c("Residual condition number", "Latent condition number", "Parameter condition number")) table[[name]] <- vapply(table[[name]], function(value) if (is.finite(value)) format(value, scientific = TRUE, digits = 3) else "Inf", character(1))
  table$p <- vapply(table$p, format_p, character(1))
  table$DeltaP <- vapply(table$DeltaP, format_p, character(1))
  names(table)[names(table) == "DeltaCFI"] <- "ΔCFI"
  names(table)[names(table) == "DeltaRMSEA"] <- "ΔRMSEA"
  names(table)[names(table) == "DeltaSRMR"] <- "ΔSRMR"
  names(table)[names(table) == "DeltaChisq"] <- "Δχ²"
  names(table)[names(table) == "DeltaDf"] <- "Δdf"
  names(table)[names(table) == "DeltaP"] <- "Δp"
  div(class = "result-section regression-result-panel structural-main-result-panel structural-invariance-result",
    h4(if (ko) paste0("집단별 측정불변성: ", result$group) else paste0("Measurement invariance by ", result$group)),
    structural_canvas_basic_html_table(
      table,
      role = "main",
      orientation = "landscape",
      note = tagList(
        result_note_paragraph(
          class = "structural-result-note structural-main-note structural-main-note-1",
          if (isTRUE(result$ordinal)) {
            "Note. Ordinal stages constrain thresholds, thresholds plus loadings, and residual variances; Δ values compare each stage with its predecessor."
          } else {
            "Note. Continuous-indicator stages constrain loadings, intercepts, and residual variances; Δ values compare each stage with its predecessor."
          }
        ),
        result_note_paragraph(
          class = "structural-result-note structural-main-note structural-main-note-2",
          "Descriptive review criteria are ΔCFI < −.010, ΔRMSEA > .015, or ΔSRMR > .030 (metric) and > .010 (scalar/strict); Δχ² is sample-size sensitive."
        ),
        if (any(!result$table$Admissible)) result_note_paragraph(
          class = "structural-result-note structural-main-note structural-main-note-status structural-result-warning",
          "One or more stages were inadmissible; their change tests were suppressed."
        )
      )
    )
  )

}

# Decision-support diagnostics are rendered separately from the journal-facing
# invariance tables.  This keeps the main tables in English while allowing the
# appendix to follow the current UI language without changing any estimates.
structural_canvas_invariance_appendix_ui <- function(bundle, language = statedu_initial_language(),
                                                      variable_table = NULL, labels = character(0)) {
  result <- bundle$invariance_result %||% NULL
  if (is.null(result)) return(NULL)
  ko <- identical(normalize_app_language(language), "ko")
  display_name <- structural_canvas_display_name_resolver(
    snapshot = bundle$snapshot %||% list(),
    variable_table = variable_table,
    labels = labels,
    moderation_definitions = bundle$diagnostics$moderation_definitions %||%
      bundle$moderation_definitions %||% list(),
    language = language
  )
  localize_table <- function(value) {
    if (!is.data.frame(value) || !nrow(value)) return(data.frame())
    value <- structural_canvas_display_identifier_table(value, display_name)
    for (column in intersect(c("Path", "Interaction path", "Indirect path", "Moderated path"), names(value))) {
      path <- as.character(value[[column]])
      present <- !is.na(path) & nzchar(trimws(path))
      path[present] <- structural_canvas_display_path(path[present], display_name)
      value[[column]] <- path
    }
    p_columns <- names(value)[grepl("(^p$| p$|p value|p-value|adjusted p|permutation p)", names(value), ignore.case = TRUE)]
    percent_columns <- names(value)[grepl("(%|percent|ratio$)", names(value), ignore.case = TRUE)]
    integer_columns <- names(value)[grepl("(^N$| N$|replicates|permutations|cases|count|dimensions|seed$)", names(value), ignore.case = TRUE)]
    integer_columns <- union(integer_columns, intersect(c("Flagged residuals", "k", "Requested", "Fit-valid", "Joint-valid"), names(value)))
    for (column in names(value)) {
      raw <- value[[column]]
      if (is.logical(raw)) {
        value[[column]] <- ifelse(is.na(raw), statedu_localized_text(language, "Not available", "산출 불가"), ifelse(raw, statedu_localized_text(language, "Yes", "예"), statedu_localized_text(language, "No", "아니요")))
      } else if (is.numeric(raw)) {
        if (column %in% p_columns) {
          value[[column]] <- vapply(raw, format_p, character(1))
        } else if (column %in% percent_columns) {
          value[[column]] <- ifelse(is.finite(raw), paste0(vapply(raw, format_decimal3, character(1)), if (grepl("%|percent", column, ignore.case = TRUE)) "%" else ""), "")
        } else if (column %in% integer_columns) {
          value[[column]] <- ifelse(is.finite(raw), formatC(raw, format = "f", digits = 0), "")
        } else {
          value[[column]] <- vapply(raw, format_decimal3, character(1))
        }
      }
    }
    # Preserve identifiers across this formatter and the shared table localizer,
    # including when their values happen to equal application status words.
    attr(value, "result_user_columns") <- unique(c(attr(value, "result_user_columns", exact = TRUE), which(tolower(names(value)) %in% c(
      "group", "group 1", "group 2", "path", "interaction path", "indirect path", "moderated path",
      "construct", "variable", "factor", "factor1", "factor2", "indicator", "indicator1", "indicator2", "pair", "constraint"))))
    if ("Residual scale" %in% names(value)) {
      scale_labels <- c(Standardized = "표준화", `Correlation residual fallback` = "상관잔차 대체")
      value[["Residual scale"]] <- vapply(as.character(value[["Residual scale"]]), function(x) {
        if (!is.na(x) && x %in% names(scale_labels)) statedu_localized_text(language, x, unname(scale_labels[[x]])) else x
      }, character(1), USE.NAMES = FALSE)
      attr(value, "result_user_columns") <- union(attr(value, "result_user_columns"), match("Residual scale", names(value)))
    }
    source_value <- value
    residual_headers <- c(`Fit-valid` = "적합 유효 수", `Joint-valid` = "공동 유효 수", Requested = "요청 수",
      `Inference usable` = "추론 사용 가능", `R version` = "R 버전", `CI method` = "CI 방법", `Quantile type` = "분위수 유형",
      `Centering scope` = "중심화 범위", `Failure counts` = "실패 횟수",
      `Cronbach's alpha` = "크론바흐 알파", `Omega total` = "총 오메가", Factor1 = "요인 1", Factor2 = "요인 2",
      `Max |standardized residual|` = "최대 |표준화 잔차|", `Flagged residuals` = "기준 초과 잔차 수",
      Indicator1 = "지표 1", Indicator2 = "지표 2", `Standardized residual` = "표준화 잔차",
      `Correlation residual` = "상관잔차", `Residual scale` = "잔차 척도", `Screening p` = "선별 p",
      `BH-adjusted screening p` = "BH 보정 선별 p", `Exceeds descriptive cutoff` = "기술적 절단값 초과")
    for (index in which(names(value) %in% names(residual_headers))) {
      header <- names(value)[index]
      names(value)[index] <- statedu_localized_text(language, header, unname(residual_headers[[header]]))
    }
    for (header in c("Fit-valid %", "Joint-valid %")) if (header %in% names(value)) {
      base <- sub(" %$", "", header)
      names(value)[names(value) == header] <- paste0(statedu_localized_text(language, base, unname(residual_headers[[base]])), " %")
    }
    if (ko) {
      value_map <- c(
        "Adequate" = "충분", "Caution" = "주의", "Unreliable" = "신뢰 불가",
        "Passed" = "통과", "Failed" = "실패", "Review" = "검토",
        "Not assessed" = "평가하지 않음", "Not available" = "산출 불가"
      )
      for (column in names(value)) if (is.character(value[[column]])) {
        matched <- match(value[[column]], names(value_map))
        replace <- !is.na(matched)
        value[[column]][replace] <- unname(value_map[matched[replace]])
      }
      header_map <- c(
        Constraint = "동등성 제약", `Score χ²` = "스코어 χ²", `Max |standardized EPC|` = "최대 |표준화 EPC|",
        `Raw χ²` = "원시 χ²", `Raw p` = "원시 p", `Raw BH-adjusted p` = "원시 BH 보정 p",
        Predictor = "예측변수", Outcome = "결과변수", `Path difference` = "경로 차이",
        `Permutation p` = "순열 p", `MGA permutation adequate` = "MGA 순열 유효성 충족",
        `Small-N warning` = "소표본 경고", `Composite-score invariance gate` = "합성점수 불변성 기준",
        `Constructs passed` = "통과한 구성개념 수", `Constructs tested` = "점검한 구성개념 수",
        `Pair seed` = "집단쌍 시드", `MICOM admitted` = "MICOM 비교 허용", `MICOM reason` = "MICOM 사유",
        `Valid N` = "유효 재표집 수", `Requested N` = "요청 재표집 수",
        `Valid Ratio` = "유효 비율", `Valid ratio` = "유효 비율", `Minimum Valid N` = "최소 유효 재표집 수",
        `N warning` = "표본 수 경고", `Minimum category count` = "최소 범주 빈도",
        `Absent ordered categories` = "누락된 순서형 범주",
        Group = "집단", `Group 1` = "집단 1", `Group 2` = "집단 2",
        Path = "경로", Construct = "구성개념", Criterion = "점검 기준",
        Passed = "통과", Evidence = "근거", Status = "상태", Reason = "사유",
        `Indicator missing %` = "지표 결측률", `Complete indicator cases` = "지표 완전 사례 수",
        `Valid replicates` = "유효 재표집 수", `Requested replicates` = "요청 재표집 수",
        `Valid permutations` = "유효 순열 수", `Requested permutations` = "요청 순열 수",
        `Inference available` = "추론 가능", `Bootstrap Status` = "부트스트랩 상태",
        `BH-adjusted p` = "BH 보정 p", `Holm-adjusted p` = "Holm 보정 p",
        `Comparison status` = "비교 상태", Model = "모형", Variable = "변수",
        Factor = "요인", Indicator = "지표", Pair = "변수쌍"
      )
      matched <- match(names(value), names(header_map))
      replace <- !is.na(matched)
      names(value)[replace] <- unname(header_map[matched[replace]])
    }
    score_headers <- c("Constraint", "Score χ²", "Max |standardized EPC|", "Raw χ²", "Raw p", "Raw BH-adjusted p")
    if (!ko) for (index in which(names(value) %in% score_headers)) {
      names(value)[index] <- statedu_localized_text(language, names(value)[index])
    }
    result_appendix_preserve_data(value, source_value)
  }
  appendix_table <- function(title_ko, title_en, value, class = "table table-striped table-bordered") {
    value <- localize_table(value)
    if (!nrow(value)) return(NULL)
    tagList(
      tags$h5(statedu_localized_text(language, title_en, title_ko)),
      structural_canvas_basic_html_table(value, class = class, role = "appendix", orientation = "auto", language = language)
    )
  }
  sections <- list()
  type <- as.character(result$type %||% "")
  if (identical(type, "structural_path_comparison")) {
    sections <- list(
      appendix_table("집단별 데이터 진단", "Group-level data diagnostics", structural_canvas_group_diagnostics_display(result$group_diagnostics %||% data.frame(), language)),
      appendix_table(
        "다집단 조절된 매개효과 부트스트랩 진단",
        "Multi-group moderated-mediation bootstrap diagnostics",
        structural_canvas_multigroup_bootstrap_diagnostics_display(result$moderated_mediation_bootstrap_diagnostics %||% data.frame(), language)
      )
    )
  } else if (identical(type, "pls_micom")) {
    mga <- result$pls_mga %||% list()
    modmed <- result$pls_modmed_mga %||% mga$pls_modmed_mga %||% list()
    configural_audit <- result$configural_audit %||% data.frame()
    if (is.data.frame(configural_audit) && nrow(configural_audit)) {
      for (column in intersect(c("Criterion", "Evidence"), names(configural_audit))) {
        configural_audit[[column]] <- structural_canvas_micom_audit_text(configural_audit[[column]], language)
      }
      attr(configural_audit, "result_user_columns") <- match(c("Criterion", "Evidence"), names(configural_audit))
    }
    sections <- list(
      appendix_table("MICOM 1단계 구성불변성 점검", "MICOM Step 1 configural-invariance audit", configural_audit),
      appendix_table("집단별 데이터 및 재표집 진단", "Group-level data and resampling diagnostics", structural_canvas_group_diagnostics_display(result$group_diagnostics %||% mga$group_diagnostics %||% data.frame(), language)),
      appendix_table("집단쌍별 MICOM 비교 허용 여부", "Pair-specific MICOM comparison gate", structural_canvas_micom_pair_display(result$pairwise_gate %||% data.frame(), language)),
      appendix_table("PLS-MGA 집단별 부트스트랩 유효성", "PLS-MGA group bootstrap validity", structural_canvas_micom_pair_display(mga$validity_gate$groups %||% data.frame(), language)),
      appendix_table("PLS-MGA 집단쌍 부트스트랩 유효성", "PLS-MGA pairwise bootstrap validity", structural_canvas_micom_pair_display(mga$validity_gate$pairs %||% data.frame(), language)),
      appendix_table("잠재 조절·조절된 매개효과 집단쌍 유효성", "Moderation and moderated-mediation pairwise validity", structural_canvas_micom_pair_display(modmed$pairwise_validity %||% data.frame(), language)),
      appendix_table("집단표지 순열 경로차이 민감도", "Group-label permutation sensitivity for direct paths", result$permutation_path_sensitivity %||% data.frame())
    )
  } else {
    residuals <- result$group_residuals %||% list()
    score_tables <- result$score_diagnostics %||% list()
    group_htmt <- result$group_htmt %||% data.frame()
    if (is.data.frame(group_htmt) && nrow(group_htmt)) {
      htmt_labels <- c(`Below reference` = "참고값 미만", `Review needed` = "검토 필요", `Not assessed` = "평가하지 않음",
        `At least two indicators per factor are required` = "요인마다 지표가 최소 2개 필요합니다",
        `Cross-loaded indicators prevent standard HTMT calculation` = "교차부하 지표로 인해 표준 HTMT를 계산할 수 없습니다",
        `Indicator correlations are unavailable` = "지표 상관을 사용할 수 없습니다",
        `Within-factor correlations are insufficient` = "요인 내 상관이 불충분합니다")
      for (column in intersect(c("Criterion", "Reason"), names(group_htmt))) {
        group_htmt[[column]] <- vapply(as.character(group_htmt[[column]]), function(x) {
          if (!is.na(x) && x %in% names(htmt_labels)) statedu_localized_text(language, x, unname(htmt_labels[[x]])) else x
        }, character(1), USE.NAMES = FALSE)
      }
      attr(group_htmt, "result_user_columns") <- union(attr(group_htmt, "result_user_columns"), which(names(group_htmt) %in% c("Criterion", "Reason")))
    }
    stage_title <- function(stage) {
      stages <- c(Configural = "Configural invariance", Metric = "Metric invariance",
                  Thresholds = "Threshold invariance", Scalar = "Scalar invariance",
                  `Scalar (thresholds + loadings)` = "Scalar invariance (thresholds + loadings)",
                  Strict = "Strict invariance")
      korean <- c("형태 불변성", "측정단위 불변성", "임계값 불변성", "절편 불변성",
                  "절편 불변성 (임계값 + 요인부하량)", "엄격 불변성")
      index <- match(stage, names(stages))
      if (is.na(index)) return(stage)
      statedu_localized_text(language, unname(stages[index]), korean[index])
    }
    sections <- c(list(
      appendix_table("집단별 데이터 진단", "Group-level data diagnostics", structural_canvas_group_diagnostics_display(result$group_diagnostics %||% data.frame(), language)),
      appendix_table("집단별 형태모형 잔차 요약", "Group-specific configural residual summary", residuals$group_summary %||% data.frame()),
      appendix_table("큰 집단별 형태모형 잔차", "Largest group-specific configural residuals", residuals$group_largest %||% data.frame()),
      appendix_table("집단별 신뢰도 및 수렴타당도", "Group-specific reliability and convergent validity", result$group_reliability %||% data.frame()),
      appendix_table("집단별 HTMT", "Group-specific HTMT", group_htmt)
    ), lapply(names(score_tables), function(stage) appendix_table(
        sprintf(statedu_localized_text(language, "%s: equality-constraint score tests", "%s: 동등성 제약 score 검정"), stage_title(stage)),
        sprintf(statedu_localized_text(language, "%s: equality-constraint score tests", "%s: 동등성 제약 score 검정"), stage_title(stage)),
        utils::head(score_tables[[stage]], 10L)
      )))
  }
  sections <- Filter(Negate(is.null), sections)
  if (!length(sections)) return(NULL)
  div(
    class = "result-section regression-result-panel structural-appendix-result-panel structural-invariance-appendix-result",
    tags$h4(statedu_localized_text(language, "Multi-group analysis supplementary tables and diagnostics", "다집단 분석 보조표 및 진단")),
    tagList(sections)
  )
}

structural_canvas_multigroup_bootstrap_diagnostics_display <- function(value, language) {
  if (!is.data.frame(value) || !nrow(value)) return(value)
  tr <- function(en, ko) statedu_localized_text(language, en, ko)
  labels <- c(Adequate = "충분", Caution = "주의", Unreliable = "신뢰 불가", None = "없음",
    percentile = "백분위", bias_corrected = "편향 보정",
    `Within group; product indicators regenerated after every stratified resample` = "집단 내 중심화; 매 층화 재표집 후 곱지표 재생성")
  for (column in intersect(c("Status", "CI method", "Centering scope"), names(value))) {
    value[[column]] <- vapply(as.character(value[[column]]), function(x) {
      if (!is.na(x) && x %in% names(labels)) tr(x, unname(labels[[x]])) else x
    }, character(1), USE.NAMES = FALSE)
  }
  if ("Failure counts" %in% names(value)) {
    failures <- c(product_preparation = "곱지표 준비 실패", fit_error = "모형 적합 오류", nonconverged = "미수렴",
      inadmissible = "부적절한 해", target_extraction = "대상 추정치 추출 실패")
    value[["Failure counts"]] <- vapply(as.character(value[["Failure counts"]]), function(x) {
      if (is.na(x)) return(x)
      if (x == "None") return(tr("None", "없음"))
      parts <- strsplit(x, "; ", fixed = TRUE)[[1]]
      paste(vapply(parts, function(part) {
        key <- sub("=[0-9]+$", "", part)
        if (key %in% names(failures) && grepl("=[0-9]+$", part)) paste0(tr(key, unname(failures[[key]])), substring(part, nchar(key)+1L)) else part
      }, character(1)), collapse = "; ")
    }, character(1), USE.NAMES = FALSE)
  }
  # Only known diagnostic strings are translated; preserve runtime identifiers and custom text.
  attr(value, "result_user_columns") <- union(attr(value, "result_user_columns"), which(vapply(value, is.character, logical(1))))
  value
}
