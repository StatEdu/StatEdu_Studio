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

structural_canvas_pls_bootstrap_p_numeric <- function(row) {
  if (is.null(row) || !"Bootstrap P Val" %in% names(row)) return(NA_real_)
  value <- suppressWarnings(as.numeric(row[["Bootstrap P Val"]][[1L]]))
  if (length(value) && is.finite(value[[1L]])) value[[1L]] else NA_real_
}

structural_canvas_add_bh_column <- function(table, numeric_column, output_column) {
  values <- suppressWarnings(as.numeric(table[[numeric_column]]))
  adjusted <- rep("", length(values))
  finite <- is.finite(values)
  if (any(finite)) adjusted[finite] <- vapply(stats::p.adjust(values[finite], method = "BH"), format_p, character(1))
  table[[output_column]] <- adjusted
  table
}

structural_canvas_pls_effect_size_label <- function(value) {
  value <- suppressWarnings(as.numeric(value))
  if (length(value) < 1L || !is.finite(value[[1L]])) return("")
  value <- value[[1L]]
  if (value < .02) "Descriptive: below .02"
  else if (value < .15) "Descriptive: .02-.15"
  else if (value < .35) "Descriptive: .15-.35"
  else "Descriptive: >= .35"
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
    indirect_p_numeric = structural_canvas_pls_bootstrap_p_numeric(indirect_boot_row),
    `Total effect` = structural_canvas_pls_number(values$total),
    `Total effect CI lower` = structural_canvas_pls_bootstrap_value(total_boot_row, "2.5% CI"),
    `Total effect CI upper` = structural_canvas_pls_bootstrap_value(total_boot_row, "97.5% CI"),
    `Total effect t` = structural_canvas_pls_bootstrap_value(total_boot_row, "T Stat."),
    `Total effect p` = structural_canvas_pls_bootstrap_p(total_boot_row),
    total_p_numeric = structural_canvas_pls_bootstrap_p_numeric(total_boot_row),
    `Boot CI lower` = structural_canvas_pls_bootstrap_value(boot_row, "2.5% CI"),
    `Boot CI upper` = structural_canvas_pls_bootstrap_value(boot_row, "97.5% CI"),
    `Boot SE` = structural_canvas_pls_bootstrap_value(boot_row, "Bootstrap SD"),
    `Boot t` = structural_canvas_pls_bootstrap_value(boot_row, "T Stat."),
    `Boot p` = structural_canvas_pls_bootstrap_p(boot_row),
    direct_p_numeric = structural_canvas_pls_bootstrap_p_numeric(boot_row),
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
  table <- do.call(rbind, rows)
  table <- structural_canvas_add_bh_column(table, "direct_p_numeric", "Boot BH-adjusted p")
  table <- structural_canvas_add_bh_column(table, "indirect_p_numeric", "Indirect effect BH-adjusted p")
  structural_canvas_add_bh_column(table, "total_p_numeric", "Total effect BH-adjusted p")
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
  if ("Coefficient" %in% names(display)) names(display)[names(display) == "Coefficient"] <- "B"
  if ("Effect" %in% names(display)) display <- display[display$Effect == "Direct", , drop = FALSE]
  if (all(c("Outcome", "Predictor") %in% names(display))) {
    display$Path <- paste(display$Predictor, "→", display$Outcome)
    display <- display[order(display$Path, method = "radix"), , drop = FALSE]
  }
  if ("Boot t" %in% names(display)) names(display)[names(display) == "Boot t"] <- "z"
  if ("Boot p" %in% names(display)) names(display)[names(display) == "Boot p"] <- "p"
  if (all(c("R2", "AdjR2") %in% names(display))) {
    r2 <- as.character(display$R2 %||% "")
    adj_r2 <- as.character(display$AdjR2 %||% "")
    display$R2AdjR2 <- ifelse(nzchar(r2) & nzchar(adj_r2), paste0(r2, " (", adj_r2, ")"), ifelse(nzchar(r2), r2, adj_r2))
  }
  structural_canvas_subset_columns(display, c("Path", "B", "Boot SE", "z", "p", "f2", "R2AdjR2", "Q2", "Inner VIF"))
}

structural_canvas_pls_fit_guide_table <- function(table) {
  structural_canvas_subset_columns(table, c("Effect", "Outcome", "Predictor", "f2", "f2 size", "q2", "q2 size", "Inner VIF"))
}

structural_canvas_pls_fit_bootstrap_table <- function(table) {
  structural_canvas_subset_columns(table, c(
    "Effect", "Outcome", "Predictor",
    "Boot CI lower", "Boot CI upper", "Boot t", "Boot p", "Boot BH-adjusted p",
    "Indirect effect CI lower", "Indirect effect CI upper", "Indirect effect t", "Indirect effect p", "Indirect effect BH-adjusted p",
    "Total effect CI lower", "Total effect CI upper", "Total effect t", "Total effect p", "Total effect BH-adjusted p"
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

structural_canvas_pls_validity_result_table <- function(summary_fit, display_name, bootstrap = NULL, snapshot = NULL, estimator = "PLS") {
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
  reflective_constructs <- constructs[reflective]
  htmt <- suppressWarnings(as.matrix(summary_fit$validity$htmt %||% matrix(numeric(0), 0L, 0L)))
  fl <- suppressWarnings(as.matrix(summary_fit$validity$fl_criteria %||% matrix(numeric(0), 0L, 0L)))
  max_from_matrix <- function(matrix_value, construct, eligible = constructs) {
    if (!length(matrix_value) || !construct %in% rownames(matrix_value) || !construct %in% colnames(matrix_value)) return(NA_real_)
    partners <- setdiff(intersect(eligible, intersect(rownames(matrix_value), colnames(matrix_value))), construct)
    if (!length(partners)) return(NA_real_)
    values <- c(matrix_value[construct, partners], matrix_value[partners, construct])
    values <- suppressWarnings(as.numeric(values))
    if (!any(is.finite(values))) NA_real_ else max(abs(values), na.rm = TRUE)
  }
  max_pair_from_matrix <- function(matrix_value, construct, eligible = constructs) {
    if (!length(matrix_value) || !construct %in% rownames(matrix_value) || !construct %in% colnames(matrix_value)) {
      return(list(value = NA_real_, partner = ""))
    }
    row_partners <- setdiff(intersect(colnames(matrix_value), eligible), construct)
    column_partners <- setdiff(intersect(rownames(matrix_value), eligible), construct)
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
  max_correlation <- vapply(constructs, function(construct) max_from_matrix(fl, construct, reflective_constructs), numeric(1))
  htmt_pairs <- lapply(constructs, function(construct) max_pair_from_matrix(htmt, construct, reflective_constructs))
  names(htmt_pairs) <- constructs
  max_htmt <- vapply(htmt_pairs, function(pair) pair$value, numeric(1))
  bootstrap_htmt <- bootstrap$bootstrapped_HTMT %||% NULL
  htmt_boot_rows <- lapply(constructs, function(construct) {
    partner <- htmt_pairs[[construct]]$partner
    if (!nzchar(partner)) NULL else structural_canvas_pls_bootstrap_pair_row(bootstrap_htmt, construct, partner)
  })
  evidence_role <- ifelse(
    construct_modes == "Formative",
    "Weights, collinearity, content coverage, and redundancy; internal consistency/AVE/HTMT not applicable",
    ifelse(
      construct_types == "Common factor",
      if (identical(toupper(as.character(estimator %||% "PLS")), "PLSC")) "PLSc common-factor diagnostics; interpretation depends on consistency-correction assumptions" else "Mode A score-proxy diagnostics; not covariance-based factor-model evidence",
      "Reflective-composite diagnostics; do not infer a latent common cause or explicit measurement-error separation"
    )
  )
  data.frame(
    Construct = vapply(constructs, display_name, character(1)),
    `Construct type` = unname(construct_types),
    Mode = unname(construct_modes),
    `Evidence role` = unname(evidence_role),
    alpha = ifelse(reflective, vapply(reliability$alpha, structural_canvas_pls_number, character(1)), "N/A"),
    rhoA = ifelse(reflective, vapply(reliability$rhoA, structural_canvas_pls_number, character(1)), "N/A"),
    rhoC = ifelse(reflective, vapply(reliability$rhoC, structural_canvas_pls_number, character(1)), "N/A"),
    AVE = ifelse(reflective, vapply(reliability$AVE, structural_canvas_pls_number, character(1)), "N/A"),
    `sqrt(AVE)` = ifelse(reflective, vapply(sqrt_ave, structural_canvas_pls_number, character(1)), "N/A"),
    `Max HTMT` = ifelse(reflective, vapply(max_htmt, structural_canvas_pls_number, character(1)), "N/A"),
    `Max HTMT CI lower` = ifelse(reflective, vapply(htmt_boot_rows, structural_canvas_pls_bootstrap_value, character(1), column = "2.5% CI"), "N/A"),
    `Max HTMT CI upper` = ifelse(reflective, vapply(htmt_boot_rows, structural_canvas_pls_bootstrap_value, character(1), column = "97.5% CI"), "N/A"),
    `Max HTMT p` = ifelse(reflective, vapply(htmt_boot_rows, function(row) if (is.null(row) || !"Bootstrap P Val" %in% names(row)) "" else format_p(row[["Bootstrap P Val"]][[1L]]), character(1)), "N/A"),
    `Fornell-Larcker` = ifelse(reflective, ifelse(is.finite(max_correlation) & is.finite(sqrt_ave) & sqrt_ave > max_correlation, "Below reference", "Review needed"), "N/A - formative"),
    check.names = FALSE
  )
}

structural_canvas_pls_validity_main_table <- function(table) {
  structural_canvas_subset_columns(table, c("Construct", "alpha", "rhoA", "rhoC", "AVE", "sqrt(AVE)", "Max HTMT"))
}

structural_canvas_pls_validity_guide_table <- function(table) {
  structural_canvas_subset_columns(table, c("Construct", "Construct type", "Mode", "Evidence role", "Max HTMT CI lower", "Max HTMT CI upper", "Max HTMT p", "Fornell-Larcker"))
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
        loading_p_numeric = structural_canvas_pls_bootstrap_p_numeric(loading_boot),
        Weight = structural_canvas_pls_number(structural_canvas_pls_matrix_cell(weights, indicator, construct)),
        `Weight Boot SE` = structural_canvas_pls_bootstrap_value(weight_boot, "Bootstrap SD"),
        `Weight CI lower` = structural_canvas_pls_bootstrap_value(weight_boot, "2.5% CI"),
        `Weight CI upper` = structural_canvas_pls_bootstrap_value(weight_boot, "97.5% CI"),
        `Weight t` = structural_canvas_pls_bootstrap_value(weight_boot, "T Stat."),
        `Weight p` = structural_canvas_pls_bootstrap_p(weight_boot),
        weight_p_numeric = structural_canvas_pls_bootstrap_p_numeric(weight_boot),
        `Item VIF` = structural_canvas_pls_number(if (indicator %in% names(item_vifs)) item_vifs[[indicator]] else NA_real_),
        `Max cross-loading` = structural_canvas_pls_number(cross_max),
        Mode = if (identical(latent$measurementMode %||% "reflective", "formative")) "Formative" else "Reflective",
        check.names = FALSE
      )
    }
  }
  if (!length(rows)) return(data.frame())
  table <- do.call(rbind, rows)
  table <- structural_canvas_add_bh_column(table, "loading_p_numeric", "Loading BH-adjusted p")
  structural_canvas_add_bh_column(table, "weight_p_numeric", "Weight BH-adjusted p")
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
  display[["Boot BH-adjusted p"]] <- ifelse(display$Mode == "Formative", display$`Weight BH-adjusted p`, display$`Loading BH-adjusted p`)
  structural_canvas_subset_columns(display, c("Construct", "Construct type", "Indicator", "loading/weight", "Boot SE", "Boot 95% CI lower", "Boot 95% CI upper", "Boot t", "Boot p", "Boot BH-adjusted p", "Item VIF", "Mode"))
}

structural_canvas_pls_measurement_guide_table <- function(table) {
  structural_canvas_subset_columns(table, c("Construct", "Construct type", "Indicator", "Loading", "Weight", "Item VIF", "Max cross-loading", "Mode"))
}

structural_canvas_pls_measurement_bootstrap_table <- function(table) {
  structural_canvas_subset_columns(table, c(
    "Construct", "Construct type", "Indicator", "Mode",
    "Loading Boot SE", "Loading CI lower", "Loading CI upper", "Loading t", "Loading p", "Loading BH-adjusted p",
    "Weight Boot SE", "Weight CI lower", "Weight CI upper", "Weight t", "Weight p", "Weight BH-adjusted p"
  ))
}

structural_canvas_pls_htmt_matrix_table <- function(summary_fit, display_name, snapshot = NULL) {
  htmt <- suppressWarnings(as.matrix(summary_fit$validity$htmt %||% matrix(numeric(0), 0L, 0L)))
  if (!length(htmt) || nrow(htmt) < 2L || ncol(htmt) < 2L) return(data.frame())
  constructs <- rownames(htmt)
  modes <- structural_canvas_pls_construct_modes(snapshot %||% list())
  reflective <- constructs[modes[constructs] != "Formative" | is.na(modes[constructs])]
  values <- matrix("", nrow = length(constructs), ncol = length(constructs) + 1L)
  colnames(values) <- c("Construct", vapply(constructs, display_name, character(1)))
  for (row in seq_along(constructs)) {
    values[row, 1L] <- display_name(constructs[[row]])
    for (column in seq_along(constructs)) {
      if (row == column) {
        values[row, column + 1L] <- "-"
      } else if (row > column && constructs[[row]] %in% reflective && constructs[[column]] %in% reflective) {
        values[row, column + 1L] <- structural_canvas_pls_number(htmt[row, column])
      } else if (row > column) {
        values[row, column + 1L] <- "N/A"
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
  repetition_summaries <- prediction$repetition_summaries %||% list(summary_value)
  for (indicator in intersect(colnames(pls_oos), colnames(lm_oos))) {
    for (metric in intersect(rownames(pls_oos), rownames(lm_oos))) {
      pls_value <- suppressWarnings(as.numeric(pls_oos[metric, indicator]))
      lm_value <- suppressWarnings(as.numeric(lm_oos[metric, indicator]))
      repetition_differences <- vapply(repetition_summaries, function(repetition) {
        repetition_pls <- as.matrix(repetition$PLS_out_of_sample %||% matrix(numeric(0), 0L, 0L))
        repetition_lm <- as.matrix(repetition$LM_out_of_sample %||% matrix(numeric(0), 0L, 0L))
        if (!metric %in% rownames(repetition_pls) || !indicator %in% colnames(repetition_pls) || !metric %in% rownames(repetition_lm) || !indicator %in% colnames(repetition_lm)) return(NA_real_)
        as.numeric(repetition_pls[metric, indicator]) - as.numeric(repetition_lm[metric, indicator])
      }, numeric(1))
      finite_differences <- repetition_differences[is.finite(repetition_differences)]
      difference_sd <- if (length(finite_differences) > 1L) stats::sd(finite_differences) else NA_real_
      pls_win_rate <- if (length(finite_differences)) mean(finite_differences < 0) else NA_real_
      item_rows[[length(item_rows) + 1L]] <- data.frame(
        Indicator = indicator,
        Metric = metric,
        `PLS out-of-sample` = pls_value,
        `LM benchmark` = lm_value,
        `PLS - LM` = pls_value - lm_value,
        `PLS - LM SD` = difference_sd,
        `PLS lower %` = 100 * pls_win_rate,
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

structural_canvas_lavaan_structural_effect_rows <- function(fit, effect_definitions, fmt, display_name, bootstrap = NULL) {
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
    boot <- if (is.data.frame(bootstrap)) bootstrap[bootstrap$lhs == effect$label & bootstrap$op == ":=", , drop = FALSE] else data.frame()
    if (nrow(boot)) {
      if ("se" %in% names(boot) && is.finite(boot$se[[1L]])) row$se <- boot$se[[1L]]
      row$ci.lower <- boot$lower[[1L]]
      row$ci.upper <- boot$upper[[1L]]
      row$pvalue <- boot$p[[1L]]
    }
    boot_ci_label <- if (nrow(boot) && "ci_method" %in% names(boot) && identical(as.character(boot$ci_method[[1L]]), "bias_corrected")) "Bootstrap bias-corrected (BC) 95% CI" else "Bootstrap percentile 95% CI"
    b_ci_source <- if (!nrow(boot)) "Model-based 95% CI" else if (all(is.finite(c(row$ci.lower[[1L]], row$ci.upper[[1L]])))) boot_ci_label else "Not estimated - insufficient valid bootstrap replicates"
    standardized_row <- standardized[standardized$lhs == effect$label, , drop = FALSE]
    beta <- if (nrow(standardized_row)) suppressWarnings(as.numeric(standardized_row$est.std[[1L]])) else structural_canvas_lavaan_standardized_effect(fit, effect)
    beta_ci_lower <- if (nrow(standardized_row)) suppressWarnings(as.numeric(standardized_row$ci.lower[[1L]])) else NA_real_
    beta_ci_upper <- if (nrow(standardized_row)) suppressWarnings(as.numeric(standardized_row$ci.upper[[1L]])) else NA_real_
    beta_ci_source <- "Model-based 95% CI"
    if (nrow(boot) && all(c("beta_lower", "beta_upper") %in% names(boot))) {
      beta_ci_lower <- boot$beta_lower[[1L]]
      beta_ci_upper <- boot$beta_upper[[1L]]
      beta_ci_source <- if (all(is.finite(c(beta_ci_lower, beta_ci_upper)))) boot_ci_label else "Not estimated - insufficient valid standardized bootstrap replicates"
    }
    data.frame(
      Effect = as.character(effect$type %||% ""),
      Outcome = vapply(as.character(effect$outcome %||% ""), display_name, character(1)),
      Predictor = vapply(as.character(effect$predictor %||% ""), display_name, character(1)),
      `Path detail` = if (identical(as.character(effect$type %||% ""), "Specific indirect")) {
        path <- as.character(effect$path %||% character(0))
        if (length(path)) paste(vapply(path, display_name, character(1)), collapse = " → ") else ""
      } else "",
      B = fmt(row$est),
      `B 95% CI lower` = fmt(row$ci.lower),
      `B 95% CI upper` = fmt(row$ci.upper),
      `B CI source` = b_ci_source,
      SE = fmt(row$se),
      beta = fmt(beta),
      `beta 95% CI lower` = fmt(beta_ci_lower),
      `beta 95% CI upper` = fmt(beta_ci_upper),
      `beta CI source` = beta_ci_source,
      R2 = "",
      z = fmt(row$z),
      p = vapply(row$pvalue, format_p, character(1)),
      p_numeric = as.numeric(row$pvalue),
      `BH-adjusted p` = "",
      check.names = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame())
  table <- do.call(rbind, rows)
  names(table)[names(table) == "R2"] <- "R2"
  table
}

structural_canvas_lavaan_structural_result_table <- function(kind, fit, ko, fmt, display_name, effect_definitions = list(), bootstrap = NULL) {
  if (!kind %in% c("structural", "structural_ci", "structural_effects", "structural_effect_ci", "structural_specific_indirect")) return(NULL)
  raw <- lavaan::parameterEstimates(fit, ci = TRUE)
  raw <- raw[raw$op == "~", c("lhs", "rhs", "est", "se", "z", "pvalue", "ci.lower", "ci.upper"), drop = FALSE]
  if (!nrow(raw)) return(data.frame())
  if (is.data.frame(bootstrap) && nrow(bootstrap)) {
    bootstrap_paths <- bootstrap[bootstrap$op == "~", , drop = FALSE]
    raw_keys <- paste(raw$lhs, raw$op, raw$rhs, sep = "\r")
    bootstrap_keys <- paste(bootstrap_paths$lhs, bootstrap_paths$op, bootstrap_paths$rhs, sep = "\r")
    bootstrap_match <- match(raw_keys, bootstrap_keys)
    available <- !is.na(bootstrap_match)
    raw$ci.lower[available] <- bootstrap_paths$lower[bootstrap_match[available]]
    raw$ci.upper[available] <- bootstrap_paths$upper[bootstrap_match[available]]
    raw$pvalue[available] <- bootstrap_paths$p[bootstrap_match[available]]
  }
  b_ci_source <- rep("Model-based 95% CI", nrow(raw))
  if (is.data.frame(bootstrap) && nrow(bootstrap)) {
    bootstrap_ci_label <- if ("ci_method" %in% names(bootstrap) && any(bootstrap$ci_method == "bias_corrected", na.rm = TRUE)) "Bootstrap bias-corrected (BC) 95% CI" else "Bootstrap percentile 95% CI"
    b_ci_source[available] <- ifelse(is.finite(raw$ci.lower[available]) & is.finite(raw$ci.upper[available]), bootstrap_ci_label, "Not estimated - insufficient valid bootstrap replicates")
  }
  standardized <- lavaan::standardizedSolution(fit, ci = TRUE, level = .95)
  standardized <- standardized[standardized$op == "~", c("lhs", "rhs", "est.std", "ci.lower", "ci.upper"), drop = FALSE]
  raw_key <- paste(raw$lhs, raw$rhs, sep = "\r")
  standardized_key <- paste(standardized$lhs, standardized$rhs, sep = "\r")
  standardized_match <- match(raw_key, standardized_key)
  beta <- standardized$est.std[standardized_match]
  beta_ci_lower <- standardized$ci.lower[standardized_match]
  beta_ci_upper <- standardized$ci.upper[standardized_match]
  beta_ci_source <- rep("Model-based 95% CI", nrow(raw))
  if (is.data.frame(bootstrap) && nrow(bootstrap) && all(c("beta_lower", "beta_upper") %in% names(bootstrap))) {
    bootstrap_paths <- bootstrap[bootstrap$op == "~", , drop = FALSE]
    bootstrap_keys <- paste(bootstrap_paths$lhs, bootstrap_paths$rhs, sep = "\r")
    beta_bootstrap_match <- match(raw_key, bootstrap_keys)
    matched_beta <- !is.na(beta_bootstrap_match)
    beta_ci_lower[matched_beta] <- bootstrap_paths$beta_lower[beta_bootstrap_match[matched_beta]]
    beta_ci_upper[matched_beta] <- bootstrap_paths$beta_upper[beta_bootstrap_match[matched_beta]]
    bootstrap_ci_label <- if ("ci_method" %in% names(bootstrap) && any(bootstrap$ci_method == "bias_corrected", na.rm = TRUE)) "Bootstrap bias-corrected (BC) 95% CI" else "Bootstrap percentile 95% CI"
    beta_ci_source[matched_beta] <- ifelse(is.finite(beta_ci_lower[matched_beta]) & is.finite(beta_ci_upper[matched_beta]), bootstrap_ci_label, "Not estimated - insufficient valid standardized bootstrap replicates")
  }
  r2_values <- tryCatch(lavaan::lavInspect(fit, "r2"), error = function(error) numeric(0))
  r2 <- suppressWarnings(as.numeric(r2_values[raw$lhs]))
  table <- data.frame(
    Effect = "Direct",
    vapply(raw$lhs, display_name, character(1)),
    vapply(raw$rhs, display_name, character(1)),
    `Path detail` = "",
    B = fmt(raw$est),
    `B 95% CI lower` = fmt(raw$ci.lower),
    `B 95% CI upper` = fmt(raw$ci.upper),
    `B CI source` = b_ci_source,
    SE = fmt(raw$se),
    beta = fmt(beta),
    `beta 95% CI lower` = fmt(beta_ci_lower),
    `beta 95% CI upper` = fmt(beta_ci_upper),
    `beta CI source` = beta_ci_source,
    R2 = fmt(r2),
    z = fmt(raw$z),
    p = vapply(raw$pvalue, format_p, character(1)),
    p_numeric = as.numeric(raw$pvalue),
    `BH-adjusted p` = "",
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
        `Direct CI source` = value_for(outcome, predictor, "Direct", "beta CI source"),
        `Indirect beta 95% CI` = ci_for(outcome, predictor, "Indirect"),
        `Indirect CI source` = value_for(outcome, predictor, "Indirect", "beta CI source"),
        `Total beta 95% CI` = ci_for(outcome, predictor, "Total"),
        `Total CI source` = value_for(outcome, predictor, "Total", "beta CI source"),
        check.names = FALSE
      )
    } else {
      data.frame(
        Outcome = outcome,
        Predictor = predictor,
        `Direct beta` = value_for(outcome, predictor, "Direct", "beta"),
        `Direct p` = value_for(outcome, predictor, "Direct", "p"),
        `Direct BH-adjusted p` = value_for(outcome, predictor, "Direct", "BH-adjusted p"),
        `Indirect beta` = value_for(outcome, predictor, "Indirect", "beta"),
        `Indirect p` = value_for(outcome, predictor, "Indirect", "p"),
        `Indirect BH-adjusted p` = value_for(outcome, predictor, "Indirect", "BH-adjusted p"),
        `Total beta` = value_for(outcome, predictor, "Total", "beta"),
        `Total p` = value_for(outcome, predictor, "Total", "p"),
        `Total BH-adjusted p` = value_for(outcome, predictor, "Total", "BH-adjusted p"),
        check.names = FALSE
      )
    }
  })
  do.call(rbind, rows)
}

structural_canvas_specific_indirect_table <- function(structural_table) {
  if (!is.data.frame(structural_table) || !nrow(structural_table) ||
      !all(c("Effect", "Outcome", "Predictor", "Path detail") %in% names(structural_table))) {
    return(data.frame())
  }
  rows <- structural_table[structural_table$Effect == "Specific indirect", , drop = FALSE]
  if (!nrow(rows)) return(data.frame())
  path <- trimws(as.character(rows[["Path detail"]] %||% ""))
  fallback <- paste(rows$Predictor, "→", rows$Outcome)
  path[!nzchar(path)] <- fallback[!nzchar(path)]
  data.frame(
    Path = path,
    B = rows$B,
    `Boot SE` = rows$SE,
    `Boot 95% CI lower` = rows[["B 95% CI lower"]],
    `Boot 95% CI upper` = rows[["B 95% CI upper"]],
    z = rows$z,
    p = rows$p,
    `BH-adjusted p` = rows[["BH-adjusted p"]],
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
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
    structural_table <- structural_canvas_lavaan_structural_result_table(kind, fit, ko, fmt, display_name, bootstrap = bundle$effect_bootstrap_result %||% NULL)
    if (!is.null(structural_table)) {
      if (kind %in% c("structural", "structural_ci", "structural_effects", "structural_effect_ci", "structural_specific_indirect") && ncol(structural_table) >= 3L) {
        names(structural_table)[1:3] <- c("Effect", "Outcome", "Predictor")
        effect_rows <- structural_canvas_lavaan_structural_effect_rows(
          fit, bundle$diagnostics$effect_definitions %||% bundle$effect_definitions %||% list(), fmt, display_name, bundle$effect_bootstrap_result %||% NULL
        )
        if (nrow(effect_rows)) {
          names(effect_rows) <- names(structural_table)
          structural_table <- rbind(structural_table, effect_rows)
        }
        finite_p <- is.finite(structural_table$p_numeric)
        structural_table$`BH-adjusted p`[finite_p] <- vapply(stats::p.adjust(structural_table$p_numeric[finite_p], method = "BH"), format_p, character(1))
      }
      if (identical(kind, "structural")) {
        direct <- structural_table[structural_table$Effect == "Direct", , drop = FALSE]
        return(direct[, c("Outcome", "Predictor", "B", "SE", "beta", "z", "p", "BH-adjusted p", "R²"), drop = FALSE])
      }
      if (identical(kind, "structural_ci")) {
        direct <- structural_table[structural_table$Effect == "Direct", , drop = FALSE]
        return(direct[, c(
          "Outcome", "Predictor",
          "B 95% CI lower", "B 95% CI upper",
          "B CI source", "beta 95% CI lower", "beta 95% CI upper", "beta CI source"
        ), drop = FALSE])
      }
      if (identical(kind, "structural_effects")) {
        return(structural_canvas_effect_summary_table(structural_table, ci = FALSE))
      }
      if (identical(kind, "structural_effect_ci")) {
        return(structural_canvas_effect_summary_table(structural_table, ci = TRUE))
      }
      if (identical(kind, "structural_specific_indirect")) {
        return(structural_canvas_specific_indirect_table(structural_table))
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
    full_validity <- structural_canvas_pls_validity_result_table(summary_fit, display_name, bundle$pls_bootstrap_result %||% NULL, bundle$snapshot %||% NULL, bundle$estimator %||% "PLS")
    if (identical(kind, "validity")) return(structural_canvas_pls_validity_main_table(full_validity))
    return(structural_canvas_pls_validity_guide_table(full_validity))
  }
  if (identical(kind, "pls_htmt")) {
    return(structural_canvas_pls_htmt_matrix_table(summary_fit, display_name, bundle$snapshot %||% NULL))
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
