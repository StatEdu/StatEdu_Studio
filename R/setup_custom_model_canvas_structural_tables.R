structural_canvas_pls_number <- function(value) {
  value <- suppressWarnings(as.numeric(value))
  if (length(value) < 1L || !is.finite(value[[1L]])) "" else format_decimal3(value[[1L]])
}

structural_canvas_pls_matrix_cell <- function(matrix_value, row_name, column_name) {
  if (is.null(matrix_value)) return(NA_real_)
  matrix_value <- as.matrix(matrix_value)
  if (!row_name %in% rownames(matrix_value) || !column_name %in% colnames(matrix_value)) return(NA_real_)
  suppressWarnings(as.numeric(matrix_value[row_name, column_name]))
}

structural_canvas_pls_bootstrap_row <- function(table, from, to) {
  if (is.null(table)) return(NULL)
  table <- as.data.frame(table, check.names = FALSE)
  if (!nrow(table)) return(NULL)
  parts <- strsplit(rownames(table), "->", fixed = TRUE)
  keys <- vapply(parts, function(item) {
    if (length(item) != 2L) return("")
    paste(trimws(item[[1L]]), trimws(item[[2L]]), sep = "\r")
  }, character(1))
  match_index <- match(paste(from, to, sep = "\r"), keys)
  if (is.na(match_index)) NULL else table[match_index, , drop = FALSE]
}

structural_canvas_pls_bootstrap_pair_row <- function(table, first, second) {
  row <- structural_canvas_pls_bootstrap_row(table, first, second)
  if (is.null(row)) structural_canvas_pls_bootstrap_row(table, second, first) else row
}

structural_canvas_pls_bootstrap_value <- function(row, column) {
  if (is.null(row) || !column %in% names(row)) return("")
  structural_canvas_pls_number(row[[column]][[1L]])
}

structural_canvas_pls_bootstrap_p <- function(row) {
  if (is.null(row) || !"Bootstrap P Val" %in% names(row)) return("")
  format_p(row[["Bootstrap P Val"]][[1L]])
}

structural_canvas_pls_effect_size_label <- function(value) {
  value <- suppressWarnings(as.numeric(value))
  if (length(value) < 1L || !is.finite(value[[1L]])) return("")
  value <- value[[1L]]
  if (value < .02) "Below small"
  else if (value < .15) "Small"
  else if (value < .35) "Medium"
  else "Large"
}

structural_canvas_pls_indicator_vifs <- function(summary_fit) {
  vif_items <- summary_fit$validity$vif_items %||% list()
  values <- numeric(0)
  for (construct in names(vif_items)) {
    construct_values <- suppressWarnings(as.numeric(vif_items[[construct]]))
    names(construct_values) <- names(vif_items[[construct]])
    values <- c(values, construct_values)
  }
  values
}

structural_canvas_pls_inner_vif <- function(summary_fit, predictor, outcome) {
  antecedents <- summary_fit$vif_antecedents %||% list()
  values <- antecedents[[outcome]] %||% numeric(0)
  values <- suppressWarnings(as.numeric(values))
  names(values) <- names(antecedents[[outcome]] %||% numeric(0))
  if (!predictor %in% names(values)) return(NA_real_)
  values[[predictor]]
}

structural_canvas_pls_assigned_indicators <- function(snapshot, latent) {
  edges <- snapshot$edges %||% list()
  vapply(Filter(function(edge) {
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    !is.null(from) && !is.null(to) &&
      ((identical(from$id, latent$id) && identical(to$role, "indicator")) ||
       (identical(to$id, latent$id) && identical(from$role, "indicator")))
  }, edges), function(edge) {
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    structural_canvas_name(if (identical(from$role, "indicator")) from else to)
  }, character(1))
}

structural_canvas_pls_path_specs <- function(diagnostics) {
  path_specs <- as.character(diagnostics$structural_paths %||% character(0))
  rows <- lapply(path_specs, function(spec) {
    parts <- strsplit(spec, "~", fixed = TRUE)[[1L]]
    if (length(parts) != 2L) return(NULL)
    data.frame(
      outcome = trimws(parts[[1L]]),
      predictor = trimws(parts[[2L]]),
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame(outcome = character(0), predictor = character(0)))
  do.call(rbind, rows)
}

structural_canvas_pls_effect_value <- function(total_effects, paths, predictor, outcome, effect) {
  total <- structural_canvas_pls_matrix_cell(total_effects, predictor, outcome)
  direct <- structural_canvas_pls_matrix_cell(paths, predictor, outcome)
  if (identical(effect, "Direct")) {
    indirect <- if (is.finite(total) && is.finite(direct)) total - direct else NA_real_
    if (is.finite(indirect) && abs(indirect) < 1e-12) indirect <- NA_real_
    return(list(coefficient = direct, indirect = indirect, total = total))
  }
  direct_value <- if (is.finite(direct)) direct else 0
  indirect <- if (is.finite(total)) total - direct_value else NA_real_
  list(coefficient = NA_real_, indirect = indirect, total = total)
}

structural_canvas_pls_q2_value <- function(diagnostics, outcome) {
  q2_table <- as.data.frame(diagnostics$q2 %||% data.frame(), check.names = FALSE)
  if (!nrow(q2_table) || !"Outcome" %in% names(q2_table) || !"Q2" %in% names(q2_table)) return(NA_real_)
  row <- q2_table[q2_table$Outcome == outcome, , drop = FALSE]
  if (!nrow(row)) return(NA_real_)
  suppressWarnings(as.numeric(row$Q2[[1L]]))
}

structural_canvas_pls_q2_effect_value <- function(diagnostics, predictor, outcome) {
  q2_effects <- as.data.frame(diagnostics$q2_effects %||% data.frame(), check.names = FALSE)
  if (!nrow(q2_effects) || !all(c("Outcome", "Predictor", "q2") %in% names(q2_effects))) return(NA_real_)
  row <- q2_effects[q2_effects$Outcome == outcome & q2_effects$Predictor == predictor, , drop = FALSE]
  if (!nrow(row)) return(NA_real_)
  suppressWarnings(as.numeric(row$q2[[1L]]))
}

structural_canvas_pls_indirect_specs <- function(path_specs) {
  if (!nrow(path_specs)) return(data.frame(outcome = character(0), predictor = character(0)))
  nodes <- sort(unique(c(path_specs$predictor, path_specs$outcome)))
  adjacency <- setNames(vector("list", length(nodes)), nodes)
  for (index in seq_len(nrow(path_specs))) {
    adjacency[[path_specs$predictor[[index]]]] <- unique(c(adjacency[[path_specs$predictor[[index]]]] %||% character(0), path_specs$outcome[[index]]))
  }
  if (structural_canvas_structural_has_cycle(adjacency)) return(data.frame(outcome = character(0), predictor = character(0)))
  rows <- list()
  for (predictor in nodes) {
    for (outcome in setdiff(nodes, predictor)) {
      paths <- structural_canvas_find_structural_paths(adjacency, predictor, outcome)
      indirect_paths <- Filter(function(path) length(path) > 2L, paths)
      if (length(indirect_paths)) {
        rows[[length(rows) + 1L]] <- data.frame(outcome = outcome, predictor = predictor, stringsAsFactors = FALSE)
      }
    }
  }
  if (!length(rows)) return(data.frame(outcome = character(0), predictor = character(0)))
  unique(do.call(rbind, rows))
}

structural_canvas_pls_fit_row <- function(effect, outcome, predictor, summary_fit, diagnostics, paths, f_square, total_effects, bootstrap_paths, bootstrap_total_paths, bootstrap_indirect_paths, display_name) {
  values <- structural_canvas_pls_effect_value(total_effects, paths, predictor, outcome, effect)
  boot_row <- if (identical(effect, "Direct")) structural_canvas_pls_bootstrap_row(bootstrap_paths, predictor, outcome) else NULL
  total_boot_row <- structural_canvas_pls_bootstrap_row(bootstrap_total_paths, predictor, outcome)
  indirect_boot_row <- structural_canvas_pls_bootstrap_row(bootstrap_indirect_paths, predictor, outcome)
  data.frame(
    Effect = effect,
    Outcome = display_name(outcome),
    Predictor = display_name(predictor),
    Coefficient = structural_canvas_pls_number(values$coefficient),
    R2 = structural_canvas_pls_number(structural_canvas_pls_matrix_cell(paths, "R^2", outcome)),
    AdjR2 = structural_canvas_pls_number(structural_canvas_pls_matrix_cell(paths, "AdjR^2", outcome)),
    f2 = if (identical(effect, "Direct")) structural_canvas_pls_number(structural_canvas_pls_matrix_cell(f_square, predictor, outcome)) else "",
    `f2 size` = if (identical(effect, "Direct")) structural_canvas_pls_effect_size_label(structural_canvas_pls_matrix_cell(f_square, predictor, outcome)) else "",
    Q2 = structural_canvas_pls_number(structural_canvas_pls_q2_value(diagnostics, outcome)),
    q2 = if (identical(effect, "Direct")) structural_canvas_pls_number(structural_canvas_pls_q2_effect_value(diagnostics, predictor, outcome)) else "",
    `q2 size` = if (identical(effect, "Direct")) structural_canvas_pls_effect_size_label(structural_canvas_pls_q2_effect_value(diagnostics, predictor, outcome)) else "",
    `Inner VIF` = if (identical(effect, "Direct")) structural_canvas_pls_number(structural_canvas_pls_inner_vif(summary_fit, predictor, outcome)) else "",
    `Indirect effect` = structural_canvas_pls_number(values$indirect),
    `Indirect effect CI lower` = structural_canvas_pls_bootstrap_value(indirect_boot_row, "2.5% CI"),
    `Indirect effect CI upper` = structural_canvas_pls_bootstrap_value(indirect_boot_row, "97.5% CI"),
    `Indirect effect t` = structural_canvas_pls_bootstrap_value(indirect_boot_row, "T Stat."),
    `Indirect effect p` = structural_canvas_pls_bootstrap_p(indirect_boot_row),
    `Total effect` = structural_canvas_pls_number(values$total),
    `Total effect CI lower` = structural_canvas_pls_bootstrap_value(total_boot_row, "2.5% CI"),
    `Total effect CI upper` = structural_canvas_pls_bootstrap_value(total_boot_row, "97.5% CI"),
    `Total effect t` = structural_canvas_pls_bootstrap_value(total_boot_row, "T Stat."),
    `Total effect p` = structural_canvas_pls_bootstrap_p(total_boot_row),
    `Boot CI lower` = structural_canvas_pls_bootstrap_value(boot_row, "2.5% CI"),
    `Boot CI upper` = structural_canvas_pls_bootstrap_value(boot_row, "97.5% CI"),
    `Boot t` = structural_canvas_pls_bootstrap_value(boot_row, "T Stat."),
    `Boot p` = structural_canvas_pls_bootstrap_p(boot_row),
    check.names = FALSE
  )
}

structural_canvas_pls_fit_result_table <- function(summary_fit, diagnostics, display_name, bootstrap = NULL) {
  paths <- as.matrix(summary_fit$paths %||% matrix(numeric(0), 0L, 0L))
  f_square <- as.matrix(summary_fit$fSquare %||% matrix(numeric(0), 0L, 0L))
  total_effects <- as.matrix(summary_fit$total_effects %||% matrix(numeric(0), 0L, 0L))
  bootstrap_paths <- bootstrap$bootstrapped_paths %||% NULL
  bootstrap_total_paths <- bootstrap$bootstrapped_total_paths %||% NULL
  bootstrap_indirect_paths <- bootstrap$bootstrapped_total_indirect_paths %||% NULL
  path_specs <- structural_canvas_pls_path_specs(diagnostics)
  rows <- lapply(seq_len(nrow(path_specs)), function(index) {
    structural_canvas_pls_fit_row("Direct", path_specs$outcome[[index]], path_specs$predictor[[index]], summary_fit, diagnostics, paths, f_square, total_effects, bootstrap_paths, bootstrap_total_paths, bootstrap_indirect_paths, display_name)
  })
  indirect_specs <- structural_canvas_pls_indirect_specs(path_specs)
  rows <- c(rows, lapply(seq_len(nrow(indirect_specs)), function(index) {
    structural_canvas_pls_fit_row("Indirect", indirect_specs$outcome[[index]], indirect_specs$predictor[[index]], summary_fit, diagnostics, paths, f_square, total_effects, bootstrap_paths, bootstrap_total_paths, bootstrap_indirect_paths, display_name)
  }))
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}

structural_canvas_subset_columns <- function(table, columns) {
  if (!is.data.frame(table) || !nrow(table)) return(data.frame())
  columns <- columns[columns %in% names(table)]
  if (!length(columns)) return(data.frame())
  table[, columns, drop = FALSE]
}

structural_canvas_pls_fit_main_table <- function(table) {
  if (!is.data.frame(table) || !nrow(table)) return(data.frame())
  display <- table
  if ("Coefficient" %in% names(display)) names(display)[names(display) == "Coefficient"] <- "Path beta"
  structural_canvas_subset_columns(display, c("Effect", "Outcome", "Predictor", "Path beta", "Indirect effect", "Total effect", "R2", "AdjR2", "Q2"))
}

structural_canvas_pls_fit_guide_table <- function(table) {
  structural_canvas_subset_columns(table, c("Effect", "Outcome", "Predictor", "f2", "f2 size", "q2", "q2 size", "Inner VIF"))
}

structural_canvas_pls_fit_bootstrap_table <- function(table) {
  structural_canvas_subset_columns(table, c(
    "Effect", "Outcome", "Predictor",
    "Boot CI lower", "Boot CI upper", "Boot t", "Boot p",
    "Indirect effect CI lower", "Indirect effect CI upper", "Indirect effect t", "Indirect effect p",
    "Total effect CI lower", "Total effect CI upper", "Total effect t", "Total effect p"
  ))
}

structural_canvas_pls_construct_modes <- function(snapshot) {
  latents <- Filter(function(node) identical(node$role, "latent"), snapshot$nodes %||% list())
  stats::setNames(
    vapply(latents, function(latent) {
      if (identical(latent$measurementMode %||% "reflective", "formative")) "Formative" else "Reflective"
    }, character(1)),
    vapply(latents, structural_canvas_name, character(1))
  )
}

structural_canvas_pls_validity_result_table <- function(summary_fit, display_name, bootstrap = NULL, snapshot = NULL) {
  reliability <- as.data.frame(summary_fit$reliability %||% data.frame(), check.names = FALSE)
  if (!nrow(reliability)) return(data.frame())
  constructs <- rownames(reliability)
  modes <- structural_canvas_pls_construct_modes(snapshot %||% list())
  specification <- structural_canvas_construct_specification(snapshot %||% list())
  types <- stats::setNames(ifelse(specification$construct_type == "commonFactor", "Common factor", ifelse(specification$construct_type == "composite", "Composite", "Unspecified")), specification$name)
  construct_modes <- modes[constructs]
  construct_modes[is.na(construct_modes) | !nzchar(construct_modes)] <- "Reflective"
  construct_types <- types[constructs]
  construct_types[is.na(construct_types) | !nzchar(construct_types)] <- "Unspecified"
  reflective <- construct_modes == "Reflective"
  htmt <- suppressWarnings(as.matrix(summary_fit$validity$htmt %||% matrix(numeric(0), 0L, 0L)))
  fl <- suppressWarnings(as.matrix(summary_fit$validity$fl_criteria %||% matrix(numeric(0), 0L, 0L)))
  max_from_matrix <- function(matrix_value, construct) {
    if (!length(matrix_value) || !construct %in% rownames(matrix_value) || !construct %in% colnames(matrix_value)) return(NA_real_)
    values <- c(matrix_value[construct, setdiff(colnames(matrix_value), construct)], matrix_value[setdiff(rownames(matrix_value), construct), construct])
    values <- suppressWarnings(as.numeric(values))
    if (!any(is.finite(values))) NA_real_ else max(abs(values), na.rm = TRUE)
  }
  max_pair_from_matrix <- function(matrix_value, construct) {
    if (!length(matrix_value) || !construct %in% rownames(matrix_value) || !construct %in% colnames(matrix_value)) {
      return(list(value = NA_real_, partner = ""))
    }
    row_partners <- setdiff(colnames(matrix_value), construct)
    column_partners <- setdiff(rownames(matrix_value), construct)
    values <- c(matrix_value[construct, row_partners], matrix_value[column_partners, construct])
    partners <- c(row_partners, column_partners)
    values <- suppressWarnings(as.numeric(values))
    finite <- is.finite(values)
    if (!any(finite)) return(list(value = NA_real_, partner = ""))
    values <- values[finite]
    partners <- partners[finite]
    index <- which.max(abs(values))
    list(value = values[[index]], partner = partners[[index]])
  }
  sqrt_ave <- vapply(constructs, function(construct) {
    fl_value <- structural_canvas_pls_matrix_cell(fl, construct, construct)
    if (is.finite(fl_value)) fl_value else sqrt(suppressWarnings(as.numeric(reliability[construct, "AVE"])))
  }, numeric(1))
  max_correlation <- vapply(constructs, function(construct) max_from_matrix(fl, construct), numeric(1))
  htmt_pairs <- lapply(constructs, function(construct) max_pair_from_matrix(htmt, construct))
  names(htmt_pairs) <- constructs
  max_htmt <- vapply(htmt_pairs, function(pair) pair$value, numeric(1))
  bootstrap_htmt <- bootstrap$bootstrapped_HTMT %||% NULL
  htmt_boot_rows <- lapply(constructs, function(construct) {
    partner <- htmt_pairs[[construct]]$partner
    if (!nzchar(partner)) NULL else structural_canvas_pls_bootstrap_pair_row(bootstrap_htmt, construct, partner)
  })
  data.frame(
    Construct = vapply(constructs, display_name, character(1)),
    `Construct type` = unname(construct_types),
    Mode = unname(construct_modes),
    alpha = ifelse(reflective, vapply(reliability$alpha, structural_canvas_pls_number, character(1)), "N/A"),
    rhoA = ifelse(reflective, vapply(reliability$rhoA, structural_canvas_pls_number, character(1)), "N/A"),
    rhoC = ifelse(reflective, vapply(reliability$rhoC, structural_canvas_pls_number, character(1)), "N/A"),
    AVE = ifelse(reflective, vapply(reliability$AVE, structural_canvas_pls_number, character(1)), "N/A"),
    `sqrt(AVE)` = ifelse(reflective, vapply(sqrt_ave, structural_canvas_pls_number, character(1)), "N/A"),
    `Max HTMT` = ifelse(reflective, vapply(max_htmt, structural_canvas_pls_number, character(1)), "N/A"),
    `Max HTMT CI lower` = ifelse(reflective, vapply(htmt_boot_rows, structural_canvas_pls_bootstrap_value, character(1), column = "2.5% CI"), "N/A"),
    `Max HTMT CI upper` = ifelse(reflective, vapply(htmt_boot_rows, structural_canvas_pls_bootstrap_value, character(1), column = "97.5% CI"), "N/A"),
    `Max HTMT p` = ifelse(reflective, vapply(htmt_boot_rows, function(row) if (is.null(row) || !"Bootstrap P Val" %in% names(row)) "" else format_p(row[["Bootstrap P Val"]][[1L]]), character(1)), "N/A"),
    `Fornell-Larcker` = ifelse(reflective, ifelse(is.finite(max_correlation) & is.finite(sqrt_ave) & sqrt_ave > max_correlation, "Criterion met", "Review needed"), "N/A - formative"),
    check.names = FALSE
  )
}

structural_canvas_pls_validity_main_table <- function(table) {
  structural_canvas_subset_columns(table, c("Construct", "Construct type", "Mode", "alpha", "rhoA", "rhoC", "AVE", "sqrt(AVE)", "Max HTMT"))
}

structural_canvas_pls_validity_guide_table <- function(table) {
  structural_canvas_subset_columns(table, c("Construct", "Construct type", "Mode", "Max HTMT CI lower", "Max HTMT CI upper", "Max HTMT p", "Fornell-Larcker"))
}

structural_canvas_pls_measurement_result_table <- function(summary_fit, snapshot, display_name, bootstrap = NULL) {
  loadings <- as.matrix(summary_fit$loadings %||% matrix(numeric(0), 0L, 0L))
  weights <- as.matrix(summary_fit$weights %||% matrix(numeric(0), 0L, 0L))
  cross_loadings <- as.matrix(summary_fit$validity$cross_loadings %||% matrix(numeric(0), 0L, 0L))
  bootstrap_loadings <- bootstrap$bootstrapped_loadings %||% NULL
  bootstrap_weights <- bootstrap$bootstrapped_weights %||% NULL
  item_vifs <- structural_canvas_pls_indicator_vifs(summary_fit)
  latents <- Filter(function(node) identical(node$role, "latent"), snapshot$nodes %||% list())
  rows <- list()
  for (latent in latents) {
    construct <- structural_canvas_name(latent)
    construct_type <- latent$constructType %||% if (identical(latent$measurementMode %||% "reflective", "formative")) "composite" else "commonFactor"
    construct_type_label <- if (identical(construct_type, "commonFactor")) "Common factor" else if (identical(construct_type, "composite")) "Composite" else "Unspecified"
    for (indicator in structural_canvas_pls_assigned_indicators(snapshot, latent)) {
      cross_values <- if (indicator %in% rownames(cross_loadings)) suppressWarnings(as.numeric(cross_loadings[indicator, setdiff(colnames(cross_loadings), construct)])) else NA_real_
      cross_max <- if (any(is.finite(cross_values))) max(abs(cross_values), na.rm = TRUE) else NA_real_
      loading_boot <- structural_canvas_pls_bootstrap_row(bootstrap_loadings, indicator, construct)
      weight_boot <- structural_canvas_pls_bootstrap_row(bootstrap_weights, indicator, construct)
      rows[[length(rows) + 1L]] <- data.frame(
        Construct = display_name(construct),
        `Construct type` = construct_type_label,
        Indicator = display_name(indicator),
        Loading = structural_canvas_pls_number(structural_canvas_pls_matrix_cell(loadings, indicator, construct)),
        `Loading Boot SE` = structural_canvas_pls_bootstrap_value(loading_boot, "Bootstrap SD"),
        `Loading CI lower` = structural_canvas_pls_bootstrap_value(loading_boot, "2.5% CI"),
        `Loading CI upper` = structural_canvas_pls_bootstrap_value(loading_boot, "97.5% CI"),
        `Loading t` = structural_canvas_pls_bootstrap_value(loading_boot, "T Stat."),
        `Loading p` = structural_canvas_pls_bootstrap_p(loading_boot),
        Weight = structural_canvas_pls_number(structural_canvas_pls_matrix_cell(weights, indicator, construct)),
        `Weight Boot SE` = structural_canvas_pls_bootstrap_value(weight_boot, "Bootstrap SD"),
        `Weight CI lower` = structural_canvas_pls_bootstrap_value(weight_boot, "2.5% CI"),
        `Weight CI upper` = structural_canvas_pls_bootstrap_value(weight_boot, "97.5% CI"),
        `Weight t` = structural_canvas_pls_bootstrap_value(weight_boot, "T Stat."),
        `Weight p` = structural_canvas_pls_bootstrap_p(weight_boot),
        `Item VIF` = structural_canvas_pls_number(if (indicator %in% names(item_vifs)) item_vifs[[indicator]] else NA_real_),
        `Max cross-loading` = structural_canvas_pls_number(cross_max),
        Mode = if (identical(latent$measurementMode %||% "reflective", "formative")) "Formative" else "Reflective",
        check.names = FALSE
      )
    }
  }
  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}

structural_canvas_pls_measurement_main_table <- function(table) {
  if (!is.data.frame(table) || !nrow(table)) return(data.frame())
  if (!all(c("Mode", "Loading", "Weight") %in% names(table))) return(data.frame())
  display <- table
  display[["loading/weight"]] <- ifelse(display$Mode == "Formative", display$Weight, display$Loading)
  display[["Boot SE"]] <- ifelse(display$Mode == "Formative", display$`Weight Boot SE`, display$`Loading Boot SE`)
  display[["Boot 95% CI lower"]] <- ifelse(display$Mode == "Formative", display$`Weight CI lower`, display$`Loading CI lower`)
  display[["Boot 95% CI upper"]] <- ifelse(display$Mode == "Formative", display$`Weight CI upper`, display$`Loading CI upper`)
  display[["Boot t"]] <- ifelse(display$Mode == "Formative", display$`Weight t`, display$`Loading t`)
  display[["Boot p"]] <- ifelse(display$Mode == "Formative", display$`Weight p`, display$`Loading p`)
  structural_canvas_subset_columns(display, c("Construct", "Construct type", "Indicator", "loading/weight", "Boot SE", "Boot 95% CI lower", "Boot 95% CI upper", "Boot t", "Boot p", "Mode"))
}

structural_canvas_pls_measurement_guide_table <- function(table) {
  structural_canvas_subset_columns(table, c("Construct", "Construct type", "Indicator", "Loading", "Weight", "Item VIF", "Max cross-loading", "Mode"))
}

structural_canvas_pls_measurement_bootstrap_table <- function(table) {
  structural_canvas_subset_columns(table, c(
    "Construct", "Construct type", "Indicator", "Mode",
    "Loading Boot SE", "Loading CI lower", "Loading CI upper", "Loading t", "Loading p",
    "Weight Boot SE", "Weight CI lower", "Weight CI upper", "Weight t", "Weight p"
  ))
}

structural_canvas_pls_htmt_matrix_table <- function(summary_fit, display_name) {
  htmt <- suppressWarnings(as.matrix(summary_fit$validity$htmt %||% matrix(numeric(0), 0L, 0L)))
  if (!length(htmt) || nrow(htmt) < 2L || ncol(htmt) < 2L) return(data.frame())
  constructs <- rownames(htmt)
  values <- matrix("", nrow = length(constructs), ncol = length(constructs) + 1L)
  colnames(values) <- c("Construct", vapply(constructs, display_name, character(1)))
  for (row in seq_along(constructs)) {
    values[row, 1L] <- display_name(constructs[[row]])
    for (column in seq_along(constructs)) {
      if (row == column) {
        values[row, column + 1L] <- "-"
      } else if (row > column) {
        values[row, column + 1L] <- structural_canvas_pls_number(htmt[row, column])
      }
    }
  }
  as.data.frame(values, check.names = FALSE)
}

structural_canvas_pls_predict_tables <- function(prediction) {
  if (is.null(prediction)) return(list(items = data.frame(), constructs = data.frame()))
  summary_value <- prediction$summary %||% prediction
  pls_oos <- as.matrix(summary_value$PLS_out_of_sample %||% matrix(numeric(0), 0L, 0L))
  lm_oos <- as.matrix(summary_value$LM_out_of_sample %||% matrix(numeric(0), 0L, 0L))
  item_rows <- list()
  for (indicator in intersect(colnames(pls_oos), colnames(lm_oos))) {
    for (metric in intersect(rownames(pls_oos), rownames(lm_oos))) {
      pls_value <- suppressWarnings(as.numeric(pls_oos[metric, indicator]))
      lm_value <- suppressWarnings(as.numeric(lm_oos[metric, indicator]))
      item_rows[[length(item_rows) + 1L]] <- data.frame(
        Indicator = indicator,
        Metric = metric,
        `PLS out-of-sample` = pls_value,
        `LM benchmark` = lm_value,
        `PLS - LM` = pls_value - lm_value,
        Assessment = if (!is.finite(pls_value) || !is.finite(lm_value)) "Not available" else if (pls_value < lm_value) "PLS lower error" else if (pls_value > lm_value) "LM lower error" else "Tie",
        check.names = FALSE
      )
    }
  }
  item_table <- if (length(item_rows)) do.call(rbind, item_rows) else data.frame()
  construct_error <- as.matrix(summary_value$construct_error %||% matrix(numeric(0), 0L, 0L))
  construct_table <- if (length(construct_error) && nrow(construct_error) && ncol(construct_error)) {
    values <- as.data.frame(t(construct_error), check.names = FALSE)
    data.frame(Construct = rownames(values), values, check.names = FALSE)
  } else {
    data.frame()
  }
  list(items = item_table, constructs = construct_table)
}

structural_canvas_lavaan_standardized_effect <- function(fit, effect) {
  standardized <- lavaan::standardizedSolution(fit, ci = FALSE)
  standardized <- standardized[standardized$op == "~", c("lhs", "rhs", "est.std"), drop = FALSE]
  coefficient <- function(predictor, outcome) {
    row <- standardized[standardized$lhs == outcome & standardized$rhs == predictor, , drop = FALSE]
    if (!nrow(row)) return(NA_real_)
    suppressWarnings(as.numeric(row$est.std[[1L]]))
  }
  path_value <- function(path) {
    if (length(path) < 2L) return(NA_real_)
    values <- vapply(seq_len(length(path) - 1L), function(index) coefficient(path[[index]], path[[index + 1L]]), numeric(1))
    if (any(!is.finite(values))) NA_real_ else prod(values)
  }
  values <- vapply(effect$paths %||% list(), path_value, numeric(1))
  if (!any(is.finite(values))) NA_real_ else sum(values[is.finite(values)])
}

structural_canvas_lavaan_structural_effect_rows <- function(fit, effect_definitions, fmt, display_name) {
  if (!length(effect_definitions)) return(data.frame())
  raw <- lavaan::parameterEstimates(fit, ci = TRUE)
  raw <- raw[raw$op == ":=", c("lhs", "est", "se", "z", "pvalue", "ci.lower", "ci.upper"), drop = FALSE]
  standardized <- tryCatch(lavaan::standardizedSolution(fit, ci = TRUE, level = .95), error = function(error) data.frame())
  if (all(c("lhs", "op", "est.std", "ci.lower", "ci.upper") %in% names(standardized))) {
    standardized <- standardized[standardized$op == ":=", c("lhs", "est.std", "ci.lower", "ci.upper"), drop = FALSE]
  } else {
    standardized <- data.frame(lhs = character(0), est.std = numeric(0), ci.lower = numeric(0), ci.upper = numeric(0))
  }
  rows <- lapply(effect_definitions, function(effect) {
    row <- raw[raw$lhs == effect$label, , drop = FALSE]
    if (!nrow(row)) return(NULL)
    standardized_row <- standardized[standardized$lhs == effect$label, , drop = FALSE]
    beta <- if (nrow(standardized_row)) suppressWarnings(as.numeric(standardized_row$est.std[[1L]])) else structural_canvas_lavaan_standardized_effect(fit, effect)
    beta_ci_lower <- if (nrow(standardized_row)) suppressWarnings(as.numeric(standardized_row$ci.lower[[1L]])) else NA_real_
    beta_ci_upper <- if (nrow(standardized_row)) suppressWarnings(as.numeric(standardized_row$ci.upper[[1L]])) else NA_real_
    data.frame(
      Effect = as.character(effect$type %||% ""),
      Outcome = vapply(as.character(effect$outcome %||% ""), display_name, character(1)),
      Predictor = vapply(as.character(effect$predictor %||% ""), display_name, character(1)),
      B = fmt(row$est),
      `B 95% CI lower` = fmt(row$ci.lower),
      `B 95% CI upper` = fmt(row$ci.upper),
      SE = fmt(row$se),
      beta = fmt(beta),
      `beta 95% CI lower` = fmt(beta_ci_lower),
      `beta 95% CI upper` = fmt(beta_ci_upper),
      R2 = "",
      z = fmt(row$z),
      p = vapply(row$pvalue, format_p, character(1)),
      check.names = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame())
  table <- do.call(rbind, rows)
  names(table)[names(table) == "R2"] <- "R2"
  table
}

structural_canvas_lavaan_structural_result_table <- function(kind, fit, ko, fmt, display_name, effect_definitions = list()) {
  if (!kind %in% c("structural", "structural_ci", "structural_effects", "structural_effect_ci")) return(NULL)
  raw <- lavaan::parameterEstimates(fit, ci = TRUE)
  raw <- raw[raw$op == "~", c("lhs", "rhs", "est", "se", "z", "pvalue", "ci.lower", "ci.upper"), drop = FALSE]
  if (!nrow(raw)) return(data.frame())
  standardized <- lavaan::standardizedSolution(fit, ci = TRUE, level = .95)
  standardized <- standardized[standardized$op == "~", c("lhs", "rhs", "est.std", "ci.lower", "ci.upper"), drop = FALSE]
  raw_key <- paste(raw$lhs, raw$rhs, sep = "\r")
  standardized_key <- paste(standardized$lhs, standardized$rhs, sep = "\r")
  standardized_match <- match(raw_key, standardized_key)
  beta <- standardized$est.std[standardized_match]
  beta_ci_lower <- standardized$ci.lower[standardized_match]
  beta_ci_upper <- standardized$ci.upper[standardized_match]
  r2_values <- tryCatch(lavaan::lavInspect(fit, "r2"), error = function(error) numeric(0))
  r2 <- suppressWarnings(as.numeric(r2_values[raw$lhs]))
  table <- data.frame(
    Effect = "Direct",
    vapply(raw$lhs, display_name, character(1)),
    vapply(raw$rhs, display_name, character(1)),
    B = fmt(raw$est),
    `B 95% CI lower` = fmt(raw$ci.lower),
    `B 95% CI upper` = fmt(raw$ci.upper),
    SE = fmt(raw$se),
    beta = fmt(beta),
    `beta 95% CI lower` = fmt(beta_ci_lower),
    `beta 95% CI upper` = fmt(beta_ci_upper),
    R2 = fmt(r2),
    z = fmt(raw$z),
    p = vapply(raw$pvalue, format_p, character(1)),
    check.names = FALSE
  )
  names(table)[1:3] <- c("Effect", if (ko) c("결과변수", "예측변수") else c("Outcome", "Predictor"))
  names(table)[names(table) == "R2"] <- "R²"
  table
}

structural_canvas_effect_summary_table <- function(structural_table, ci = FALSE) {
  if (!is.data.frame(structural_table) || !nrow(structural_table) ||
      !all(c("Effect", "Outcome", "Predictor") %in% names(structural_table))) {
    return(data.frame())
  }
  if (!any(structural_table$Effect %in% c("Indirect", "Total"))) return(data.frame())
  pairs <- unique(structural_table[, c("Outcome", "Predictor"), drop = FALSE])
  value_for <- function(outcome, predictor, effect, column) {
    rows <- structural_table$Outcome == outcome & structural_table$Predictor == predictor & structural_table$Effect == effect
    if (!any(rows) || !column %in% names(structural_table)) return("")
    as.character(structural_table[[column]][which(rows)[[1L]]])
  }
  ci_for <- function(outcome, predictor, effect) {
    lower <- value_for(outcome, predictor, effect, "beta 95% CI lower")
    upper <- value_for(outcome, predictor, effect, "beta 95% CI upper")
    if (!nzchar(lower) && !nzchar(upper)) return("")
    paste0(lower, " ~ ", upper)
  }
  rows <- lapply(seq_len(nrow(pairs)), function(index) {
    outcome <- pairs$Outcome[[index]]
    predictor <- pairs$Predictor[[index]]
    if (isTRUE(ci)) {
      data.frame(
        Outcome = outcome,
        Predictor = predictor,
        `Direct beta 95% CI` = ci_for(outcome, predictor, "Direct"),
        `Indirect beta 95% CI` = ci_for(outcome, predictor, "Indirect"),
        `Total beta 95% CI` = ci_for(outcome, predictor, "Total"),
        check.names = FALSE
      )
    } else {
      data.frame(
        Outcome = outcome,
        Predictor = predictor,
        `Direct beta` = value_for(outcome, predictor, "Direct", "beta"),
        `Direct p` = value_for(outcome, predictor, "Direct", "p"),
        `Indirect beta` = value_for(outcome, predictor, "Indirect", "beta"),
        `Indirect p` = value_for(outcome, predictor, "Indirect", "p"),
        `Total beta` = value_for(outcome, predictor, "Total", "beta"),
        `Total p` = value_for(outcome, predictor, "Total", "p"),
        check.names = FALSE
      )
    }
  })
  do.call(rbind, rows)
}

structural_canvas_result_table <- function(kind, fit_result, analysis_type, labels_fn, app_language_fn = NULL) {
  bundle <- fit_result()
  shiny::req(!is.null(bundle))
  fit <- bundle$fit
  snapshot <- bundle$snapshot %||% list()
  labels <- labels_fn() %||% character(0)
  ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
  moderation_definitions <- bundle$diagnostics$moderation_definitions %||% bundle$moderation_definitions %||% list()
  interaction_display_names <- vapply(moderation_definitions, function(item) {
    predictor <- as.character(item$predictor %||% "")
    moderator <- as.character(item$moderator %||% "")
    if (nzchar(predictor) && nzchar(moderator)) paste0(predictor, "*", moderator) else ""
  }, character(1))
  names(interaction_display_names) <- vapply(
    moderation_definitions,
    function(item) as.character(item$interaction_factor %||% ""),
    character(1)
  )
  interaction_display_names <- interaction_display_names[
    nzchar(names(interaction_display_names)) & nzchar(interaction_display_names)
  ]
  display_name <- function(name) {
    name <- as.character(name %||% "")
    if (name %in% names(interaction_display_names)) return(unname(interaction_display_names[[name]]))
    node <- Filter(function(item) identical(structural_canvas_name(item), name), snapshot$nodes %||% list())
    label <- if (length(node)) as.character(node[[1]]$canvasLabel %||% "") else ""
    if (!nzchar(label) && !is.null(names(labels)) && name %in% names(labels)) label <- as.character(labels[[name]] %||% "")
    if (!nzchar(label) && length(node)) label <- as.character(node[[1]]$dataLabel %||% "")
    if (!ko && grepl("^잠재변수\\s*[0-9]+$", label)) {
      label <- sub("^잠재변수\\s*", "Latent variable ", label)
    }
    if (nzchar(label)) label else name
  }
  residual_name <- function(name) {
    target <- Filter(function(item) identical(structural_canvas_name(item), as.character(name)), snapshot$nodes %||% list())
    if (!length(target)) return(display_name(name))
    target_id <- as.character(target[[1]]$id %||% "")
    residual_edge <- Filter(function(edge) {
      if (identical(edge$kind, "covariance") || !identical(as.character(edge$to), target_id)) return(FALSE)
      source <- structural_canvas_node(snapshot, edge$from)
      !is.null(source) && source$role %in% c("error", "disturbance")
    }, snapshot$edges %||% list())
    if (!length(residual_edge)) return(display_name(name))
    residual <- structural_canvas_node(snapshot, residual_edge[[1]]$from)
    candidates <- as.character(c(residual$canvasLabel, residual$dataLabel, residual$name))
    candidates <- candidates[nzchar(candidates)]
    label <- if (length(candidates)) candidates[[1L]] else ""
    if (nzchar(label)) label else display_name(name)
  }
  fmt <- function(value) vapply(as.numeric(value), format_decimal3, character(1))
  if (analysis_type %in% c("cfa", "cbsem", "sem")) {
    if (identical(kind, "common_method")) {
      return(structural_canvas_common_method_display_table(
        bundle$common_method_result,
        format_values = TRUE,
        language = statedu_current_language(app_language_fn)
      ))
    }
    summary_table <- structural_canvas_summary_result_table(kind, bundle, fit, analysis_type, ko, fmt)
    if (!is.null(summary_table)) return(summary_table)
    validity_table <- structural_canvas_validity_result_table(kind, bundle, snapshot, fit, ko, fmt, display_name)
    if (!is.null(validity_table)) return(validity_table)
    measurement_table <- structural_canvas_measurement_result_table(kind, fit, ko, fmt, display_name)
    if (!is.null(measurement_table)) {
      moderation_factors <- vapply(moderation_definitions, function(item) as.character(item$interaction_factor %||% ""), character(1))
      moderation_factors <- moderation_factors[nzchar(moderation_factors)]
      if (length(moderation_factors) && "Latent" %in% names(measurement_table)) {
        measurement_table <- measurement_table[!measurement_table$Latent %in% moderation_factors, , drop = FALSE]
      }
      return(measurement_table)
    }
    structural_table <- structural_canvas_lavaan_structural_result_table(kind, fit, ko, fmt, display_name)
    if (!is.null(structural_table)) {
      if (kind %in% c("structural", "structural_ci", "structural_effects", "structural_effect_ci") && ncol(structural_table) >= 3L) {
        names(structural_table)[1:3] <- c("Effect", "Outcome", "Predictor")
        effect_rows <- structural_canvas_lavaan_structural_effect_rows(
          fit, bundle$diagnostics$effect_definitions %||% bundle$effect_definitions %||% list(), fmt, display_name
        )
        if (nrow(effect_rows)) {
          names(effect_rows) <- names(structural_table)
          structural_table <- rbind(structural_table, effect_rows)
        }
      }
      if (identical(kind, "structural")) {
        direct <- structural_table[structural_table$Effect == "Direct", , drop = FALSE]
        return(direct[, c("Outcome", "Predictor", "B", "SE", "beta", "z", "p", "R²"), drop = FALSE])
      }
      if (identical(kind, "structural_ci")) {
        direct <- structural_table[structural_table$Effect == "Direct", , drop = FALSE]
        return(direct[, c(
          "Outcome", "Predictor",
          "B 95% CI lower", "B 95% CI upper",
          "beta 95% CI lower", "beta 95% CI upper"
        ), drop = FALSE])
      }
      if (identical(kind, "structural_effects")) {
        return(structural_canvas_effect_summary_table(structural_table, ci = FALSE))
      }
      if (identical(kind, "structural_effect_ci")) {
        return(structural_canvas_effect_summary_table(structural_table, ci = TRUE))
      }
      return(structural_table)
    }
    return(structural_canvas_mi_result_table(bundle, snapshot, fit, ko, fmt, display_name, residual_name))
  }
  summary_fit <- summary(fit)
  if (identical(kind, "overview")) {
    diagnostics <- bundle$diagnostics %||% list()
    estimator_label <- diagnostics$estimator %||% bundle$estimator %||% "PLS"
    overview_df <- data.frame(
      Item = if (ko) c("분석", "추정 방법", "표본 크기(N)", "구성개념", "지표", "구조 경로", "수렴 여부") else c("Analysis", "Estimator", "N", "Constructs", "Indicators", "Structural paths", "Converged"),
      Value = c(
        structural_analysis_title(analysis_type, "en"),
        estimator_label,
        as.character(diagnostics$n %||% NA_integer_),
        length(diagnostics$constructs %||% character(0)),
        length(diagnostics$observed %||% character(0)),
        length(diagnostics$structural_paths %||% character(0)),
        if (isTRUE(diagnostics$converged %||% TRUE)) "Yes" else "No"
      ),
      check.names = FALSE
    )
    ignored_covariances <- diagnostics$ignored_covariances %||% character(0)
    if (length(ignored_covariances)) {
      overview_df <- rbind(
        overview_df,
        data.frame(
          Item = if (ko) "제외된 공분산 경로" else "Ignored covariance paths",
          Value = paste(ignored_covariances, collapse = ", "),
          check.names = FALSE
        )
      )
    }
    names(overview_df)[[1]] <- if (ko) "항목" else "Item"
    names(overview_df)[[2]] <- if (ko) "값" else "Value"
    return(overview_df)
  }
  if (kind %in% c("fit", "fit_guide", "fit_bootstrap")) {
    full_fit <- structural_canvas_pls_fit_result_table(summary_fit, bundle$diagnostics %||% list(), display_name, bundle$pls_bootstrap_result %||% NULL)
    if (identical(kind, "fit")) return(structural_canvas_pls_fit_main_table(full_fit))
    if (identical(kind, "fit_guide")) return(structural_canvas_pls_fit_guide_table(full_fit))
    return(structural_canvas_pls_fit_bootstrap_table(full_fit))
  }
  if (kind %in% c("validity", "validity_guide")) {
    full_validity <- structural_canvas_pls_validity_result_table(summary_fit, display_name, bundle$pls_bootstrap_result %||% NULL, bundle$snapshot %||% NULL)
    if (identical(kind, "validity")) return(structural_canvas_pls_validity_main_table(full_validity))
    return(structural_canvas_pls_validity_guide_table(full_validity))
  }
  if (identical(kind, "pls_htmt")) {
    return(structural_canvas_pls_htmt_matrix_table(summary_fit, display_name))
  }
  if (kind %in% c("measurement", "measurement_guide", "measurement_bootstrap")) {
    full_measurement <- structural_canvas_pls_measurement_result_table(summary_fit, snapshot, display_name, bundle$pls_bootstrap_result %||% NULL)
    if (identical(kind, "measurement")) return(structural_canvas_pls_measurement_main_table(full_measurement))
    if (identical(kind, "measurement_guide")) return(structural_canvas_pls_measurement_guide_table(full_measurement))
    return(structural_canvas_pls_measurement_bootstrap_table(full_measurement))
  }
  matrix_value <- switch(kind, mi = NULL)
  if (is.null(matrix_value)) return(data.frame())
  table <- as.data.frame(matrix_value, check.names = FALSE)
  row_labels <- rownames(table)
  row_labels <- vapply(row_labels, function(name) if (name %in% c("R^2", "AdjR^2")) name else display_name(name), character(1))
  names(table) <- vapply(names(table), display_name, character(1))
  result <- data.frame(row_labels, table, check.names = FALSE)
  names(result)[[1]] <- if (ko) "항목" else "Item"
  result
}
