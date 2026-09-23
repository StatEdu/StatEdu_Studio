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
  value <- structural_canvas_pls_bootstrap_p_numeric(row)
  if (!is.finite(value)) "" else format_p(value)
}

structural_canvas_pls_bootstrap_p_numeric <- function(row) {
  if (is.null(row)) return(NA_real_)
  column <- intersect(
    c("Bootstrap P Val", "p", "P", "p value", "P value", "p-value", "Bootstrap p"),
    names(row) %||% character(0)
  )
  if (!length(column)) return(NA_real_)
  raw <- row[[column[[1L]]]][[1L]]
  text <- trimws(as.character(raw %||% ""))
  value <- suppressWarnings(as.numeric(sub("^[<>=~]\\s*", "", text, perl = TRUE)))
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

# seminr::summary.pls_model() computes f-squared by fitting every reduced
# model with estimate_pls(). That is correct for an ordinary PLS fit, but it
# silently falls back to uncorrected PLS when the full StatEdu fit has been
# corrected with PLSc. Keep the reporting contract estimator-consistent by
# removing each structural path from the canvas snapshot and using the same
# StatEdu estimator entry point for the reduced model.
if (!exists(".structural_canvas_pls_f_square_cache", inherits = FALSE)) {
  .structural_canvas_pls_f_square_cache <- new.env(parent = emptyenv())
}

structural_canvas_pls_effect_points_for_summary <- function(fit) {
  direct <- fit$statedu_direct_paths %||% NULL
  specific <- fit$statedu_specific_indirect_paths %||% NULL
  total_indirect <- fit$statedu_total_indirect_paths %||% NULL
  total <- fit$statedu_total_paths %||% NULL
  if (any(vapply(list(direct, specific, total_indirect, total), is.null, logical(1))) &&
      exists("structural_canvas_pls_effect_point_tables", mode = "function", inherits = TRUE)) {
    calculated <- structural_canvas_pls_effect_point_tables(fit$path_coef, fit$smMatrix %||% NULL)
    direct <- direct %||% calculated$direct
    specific <- specific %||% calculated$specific
    total_indirect <- total_indirect %||% calculated$total_indirect
    total <- total %||% calculated$total
  }
  list(direct = direct, specific = specific, total_indirect = total_indirect, total = total)
}

structural_canvas_pls_effect_registry_matrix <- function(path_coef, primary, direct = NULL) {
  path_coef <- as.matrix(path_coef %||% matrix(numeric(0), 0L, 0L))
  constructs <- unique(c(rownames(path_coef) %||% character(0), colnames(path_coef) %||% character(0)))
  result <- matrix(0, nrow = length(constructs), ncol = length(constructs), dimnames = list(constructs, constructs))
  assign_registry <- function(registry) {
    if (is.null(registry)) return(invisible(NULL))
    registry <- as.data.frame(registry, check.names = FALSE, stringsAsFactors = FALSE)
    if (!nrow(registry) || !all(c("Predictor", "Outcome", "Original Est.") %in% names(registry))) return(invisible(NULL))
    for (index in seq_len(nrow(registry))) {
      predictor <- as.character(registry$Predictor[[index]] %||% "")
      outcome <- as.character(registry$Outcome[[index]] %||% "")
      estimate <- suppressWarnings(as.numeric(registry[["Original Est."]][[index]] %||% NA_real_))
      if (predictor %in% rownames(result) && outcome %in% colnames(result) && length(estimate) && is.finite(estimate[[1L]])) {
        result[predictor, outcome] <<- estimate[[1L]]
      }
    }
    invisible(NULL)
  }
  assign_registry(direct)
  assign_registry(primary)
  seminr:::convert_to_table_output(result)
}

structural_canvas_pls_summary <- function(fit, f_square_result = NULL) {
  stopifnot(inherits(fit, "seminr_model"))
  path_reports <- seminr:::report_paths(fit)
  metrics <- seminr:::evaluate_model(fit)
  effect_points <- structural_canvas_pls_effect_points_for_summary(fit)
  total_effect_matrix <- structural_canvas_pls_effect_registry_matrix(fit$path_coef, effect_points$total, effect_points$direct)
  total_indirect_matrix <- structural_canvas_pls_effect_registry_matrix(fit$path_coef, effect_points$total_indirect)
  values <- f_square_result$values %||% NULL
  if (is.null(values)) {
    values <- as.matrix(fit$path_coef %||% matrix(numeric(0), 0L, 0L))
    if (length(values)) values[] <- 0
    values <- seminr:::convert_to_table_output(values)
  }
  model_summary <- list(
    meta = list(seminr = seminr:::seminr_info()),
    iterations = fit$iterations,
    paths = path_reports,
    total_effects = total_effect_matrix,
    total_indirect_effects = total_indirect_matrix,
    statedu_direct_paths = effect_points$direct,
    statedu_specific_indirect_paths = effect_points$specific,
    statedu_total_indirect_paths = effect_points$total_indirect,
    statedu_total_paths = effect_points$total,
    loadings = seminr:::convert_to_table_output(fit$outer_loadings),
    weights = seminr:::convert_to_table_output(fit$outer_weights),
    validity = list(
      vif_items = metrics$validity$item_vifs,
      htmt = t(metrics$validity$htmt),
      fl_criteria = metrics$validity$fl_criteria,
      cross_loadings = metrics$validity$cross_loadings
    ),
    reliability = metrics$reliability,
    composite_scores = seminr:::return_only_composite_scores(fit),
    vif_antecedents = metrics$validity$antecedent_vifs,
    fSquare = values,
    descriptives = seminr:::descriptives(fit),
    it_criteria = seminr:::calculate_itcriteria(fit),
    missing_data = seminr:::report_missing(fit)
  )
  class(model_summary) <- "summary.seminr_model"
  model_summary
}

structural_canvas_pls_f_square_estimator <- function(bundle) {
  value <- bundle$diagnostics$estimator %||% bundle$estimator %||% "PLS"
  value <- toupper(gsub("[^A-Za-z]", "", as.character(value)[[1L]]))
  if (identical(value, "PLSC")) "PLSC" else "PLS"
}

structural_canvas_pls_f_square_r2 <- function(fit, outcome) {
  r_squared <- suppressWarnings(as.matrix(fit$rSquared %||% matrix(numeric(0), 0L, 0L)))
  if (length(r_squared) && "Rsq" %in% rownames(r_squared) && outcome %in% colnames(r_squared)) {
    value <- suppressWarnings(as.numeric(r_squared["Rsq", outcome]))
    if (length(value) && is.finite(value[[1L]])) return(value[[1L]])
  }
  NA_real_
}

structural_canvas_pls_f_square_edge_rows <- function(snapshot) {
  edges <- snapshot$edges %||% list()
  rows <- lapply(seq_along(edges), function(index) {
    edge <- edges[[index]]
    if (identical(edge$kind, "covariance")) return(NULL)
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    if (is.null(from) || is.null(to) || !identical(from$role, "latent") || !identical(to$role, "latent")) return(NULL)
    data.frame(
      edge_index = as.integer(index),
      predictor = structural_canvas_name(from),
      outcome = structural_canvas_name(to),
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame(edge_index = integer(0), predictor = character(0), outcome = character(0)))
  do.call(rbind, rows)
}

structural_canvas_pls_f_square_empty_matrix <- function(summary_fit, snapshot) {
  native <- suppressWarnings(as.matrix(summary_fit$fSquare %||% matrix(numeric(0), 0L, 0L)))
  if (length(native) && !is.null(rownames(native)) && !is.null(colnames(native))) {
    native[] <- 0
    return(native)
  }
  constructs <- vapply(
    Filter(function(node) identical(node$role, "latent"), snapshot$nodes %||% list()),
    structural_canvas_name,
    character(1)
  )
  matrix(0, nrow = length(constructs), ncol = length(constructs), dimnames = list(constructs, constructs))
}

structural_canvas_pls_f_square_native_result <- function(summary_fit, snapshot, estimator = "PLS") {
  values <- structural_canvas_pls_f_square_empty_matrix(summary_fit, snapshot)
  status <- matrix("Not applicable", nrow(values), ncol(values), dimnames = dimnames(values))
  path_rows <- structural_canvas_pls_f_square_edge_rows(snapshot)
  for (index in seq_len(nrow(path_rows))) {
    predictor <- path_rows$predictor[[index]]
    outcome <- path_rows$outcome[[index]]
    if (predictor %in% rownames(status) && outcome %in% colnames(status)) {
      status[predictor, outcome] <- paste0(estimator, " estimator-consistent reduced model")
    }
  }
  list(
    values = values,
    status = status,
    complete = TRUE,
    failures = data.frame(Predictor = character(0), Outcome = character(0), Reason = character(0)),
    estimator = estimator,
    source = if (identical(estimator, "PLSC")) "StatEdu estimator-consistent reduced models" else "seminr PLS reduced models"
  )
}

structural_canvas_pls_refit_f_square <- function(bundle, summary_fit = NULL, runner = NULL) {
  fit <- bundle$fit
  snapshot <- bundle$snapshot %||% list(nodes = list(), edges = list())
  if (is.null(summary_fit)) summary_fit <- structural_canvas_pls_summary(fit)
  estimator <- structural_canvas_pls_f_square_estimator(bundle)
  values <- structural_canvas_pls_f_square_empty_matrix(summary_fit, snapshot)
  status <- matrix("Not applicable", nrow(values), ncol(values), dimnames = dimnames(values))
  path_rows <- structural_canvas_pls_f_square_edge_rows(snapshot)
  if (!nrow(path_rows)) {
    return(list(
      values = values, status = status, complete = TRUE,
      failures = data.frame(Predictor = character(0), Outcome = character(0), Reason = character(0)),
      estimator = estimator, source = "StatEdu estimator-consistent reduced models"
    ))
  }
  duplicate_paths <- duplicated(paste(path_rows$predictor, path_rows$outcome, sep = "\r"))
  if (any(duplicate_paths)) {
    stop("PLS f-squared cannot be calculated because the structural model contains duplicate directed paths.", call. = FALSE)
  }
  runner <- runner %||% get("structural_canvas_run_pls_analysis", mode = "function", inherits = TRUE)
  full_r2 <- vapply(unique(path_rows$outcome), function(outcome) structural_canvas_pls_f_square_r2(fit, outcome), numeric(1))
  expected_common_factors <- as.character(
    bundle$diagnostics$plsc_corrected_constructs %||%
      bundle$plsc_corrected_constructs %||%
      fit$statedu_common_factor_constructs %||% character(0)
  )
  failures <- list()
  record_failure <- function(predictor, outcome, reason) {
    failures[[length(failures) + 1L]] <<- data.frame(
      Predictor = predictor, Outcome = outcome, Reason = as.character(reason), stringsAsFactors = FALSE
    )
    if (predictor %in% rownames(values) && outcome %in% colnames(values)) values[predictor, outcome] <<- NA_real_
    if (predictor %in% rownames(status) && outcome %in% colnames(status)) status[predictor, outcome] <<- paste0("Not reported: ", reason)
  }
  for (index in seq_len(nrow(path_rows))) {
    predictor <- path_rows$predictor[[index]]
    outcome <- path_rows$outcome[[index]]
    included_r2 <- full_r2[[outcome]]
    if (!is.finite(included_r2) || included_r2 < -sqrt(.Machine$double.eps) || included_r2 >= 1) {
      record_failure(predictor, outcome, "the full-model R-squared is unavailable or outside [0, 1)")
      next
    }
    reduced_edges <- snapshot$edges %||% list()
    reduced_edges <- reduced_edges[-path_rows$edge_index[[index]]]
    reduced_snapshot <- snapshot
    reduced_snapshot$edges <- reduced_edges
    reduced_rows <- structural_canvas_pls_f_square_edge_rows(reduced_snapshot)
    has_remaining_predictor <- any(reduced_rows$outcome == outcome)
    excluded_r2 <- 0
    if (has_remaining_predictor) {
      latents <- Filter(function(node) identical(node$role, "latent"), reduced_snapshot$nodes %||% list())
      data <- fit$rawdata %||% bundle$analysis_data %||% data.frame()
      reduced <- tryCatch(
        runner(reduced_snapshot, data, latents, reduced_edges, estimator = estimator),
        error = function(error) error
      )
      if (inherits(reduced, "error")) {
        record_failure(predictor, outcome, paste0("reduced-model estimation failed: ", conditionMessage(reduced)))
        next
      }
      if (!isTRUE(reduced$converged)) {
        record_failure(predictor, outcome, "the reduced model did not converge")
        next
      }
      if (!isTRUE(reduced$admissible)) {
        record_failure(predictor, outcome, "the reduced model was numerically inadmissible")
        next
      }
      reduced_estimator <- toupper(gsub("[^A-Za-z]", "", as.character(reduced$estimator %||% "")[[1L]]))
      if (!identical(reduced_estimator, estimator)) {
        record_failure(predictor, outcome, "the reduced model used a different estimator")
        next
      }
      if (identical(estimator, "PLSC") && length(expected_common_factors)) {
        reduced_common_factors <- as.character(reduced$plsc_corrected_constructs %||% character(0))
        if (!setequal(expected_common_factors, reduced_common_factors)) {
          record_failure(predictor, outcome, "the reduced model used a different PLSc common-factor specification")
          next
        }
      }
      excluded_r2 <- structural_canvas_pls_f_square_r2(reduced$fit, outcome)
      if (!is.finite(excluded_r2) || excluded_r2 < -sqrt(.Machine$double.eps) || excluded_r2 >= 1) {
        record_failure(predictor, outcome, "the reduced-model R-squared is unavailable or outside [0, 1)")
        next
      }
    }
    f_square <- (included_r2 - excluded_r2) / (1 - included_r2)
    if (!is.finite(f_square)) {
      record_failure(predictor, outcome, "the f-squared formula produced a non-finite value")
      next
    }
    if (predictor %in% rownames(values) && outcome %in% colnames(values)) values[predictor, outcome] <- f_square
    if (predictor %in% rownames(status) && outcome %in% colnames(status)) {
      status[predictor, outcome] <- paste0(if (identical(estimator, "PLSC")) "PLSc" else "PLS", " estimator-consistent reduced model")
    }
  }
  failure_table <- if (length(failures)) do.call(rbind, failures) else data.frame(Predictor = character(0), Outcome = character(0), Reason = character(0))
  list(
    values = values,
    status = status,
    complete = !nrow(failure_table),
    failures = failure_table,
    estimator = estimator,
    source = "StatEdu estimator-consistent reduced models"
  )
}

structural_canvas_pls_f_square_for_reporting <- function(bundle, summary_fit = NULL, runner = NULL,
                                                          force_refit = FALSE, use_cache = TRUE) {
  fit <- bundle$fit
  snapshot <- bundle$snapshot %||% list(nodes = list(), edges = list())
  if (is.null(summary_fit)) summary_fit <- structural_canvas_pls_summary(fit)
  estimator <- structural_canvas_pls_f_square_estimator(bundle)
  precomputed <- fit$statedu_f_square_result %||% bundle$statedu_f_square_result %||% NULL
  precomputed_values <- fit$statedu_fSquare %||% bundle$statedu_fSquare %||% NULL
  if (is.null(precomputed) && !is.null(precomputed_values)) {
    precomputed <- structural_canvas_pls_f_square_native_result(summary_fit, snapshot, estimator)
    precomputed$values <- as.matrix(precomputed_values)
    precomputed$source <- "Precomputed StatEdu estimator-consistent reduced models"
  }
  if (is.list(precomputed) && !is.null(precomputed$values)) return(precomputed)
  cache_key <- as.character(bundle$analysis_run_id %||% "")
  can_cache <- isTRUE(use_cache) && is.null(runner) && nzchar(cache_key)
  cache_key <- paste0(cache_key, "::", estimator, "::fSquare-v1")
  if (can_cache && exists(cache_key, envir = .structural_canvas_pls_f_square_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .structural_canvas_pls_f_square_cache, inherits = FALSE))
  }
  result <- structural_canvas_pls_refit_f_square(bundle, summary_fit, runner = runner)
  if (can_cache) {
    assign(cache_key, result, envir = .structural_canvas_pls_f_square_cache)
    cache_names <- ls(.structural_canvas_pls_f_square_cache, all.names = TRUE)
    if (length(cache_names) > 20L) rm(list = head(cache_names, length(cache_names) - 20L), envir = .structural_canvas_pls_f_square_cache)
  }
  result
}

structural_canvas_pls_matrix_cell_character <- function(matrix_value, row_name, column_name) {
  if (is.null(matrix_value)) return("")
  matrix_value <- as.matrix(matrix_value)
  if (is.null(rownames(matrix_value)) || is.null(colnames(matrix_value))) return("")
  if (!row_name %in% rownames(matrix_value) || !column_name %in% colnames(matrix_value)) return("")
  as.character(matrix_value[row_name, column_name])
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

structural_canvas_pls_path_tokens <- function(path) {
  path <- trimws(as.character(path %||% ""))
  if (!length(path) || !nzchar(path[[1L]])) return(character(0))
  tokens <- trimws(strsplit(path[[1L]], "\\s*(?:->|→)\\s*", perl = TRUE)[[1L]])
  tokens[nzchar(tokens)]
}

structural_canvas_pls_path_text <- function(nodes, display_name = identity) {
  nodes <- as.character(nodes %||% character(0))
  if (!length(nodes)) return("")
  paste(vapply(nodes, function(value) as.character(display_name(value))[[1L]], character(1)), collapse = " → ")
}

structural_canvas_pls_estimand_registry <- function(value, effect = "") {
  if (is.null(value)) return(data.frame())
  table <- as.data.frame(value, check.names = FALSE, stringsAsFactors = FALSE)
  if (!nrow(table)) return(data.frame())
  if (!"Path" %in% names(table)) table$Path <- rownames(table)
  if (!"Estimand Key" %in% names(table)) table[["Estimand Key"]] <- ""
  rows <- lapply(seq_len(nrow(table)), function(index) {
    path <- as.character(table$Path[[index]] %||% "")
    nodes <- structural_canvas_pls_path_tokens(path)
    predictor <- as.character(table$Predictor[[index]] %||% if (length(nodes)) nodes[[1L]] else "")
    outcome <- as.character(table$Outcome[[index]] %||% if (length(nodes)) nodes[[length(nodes)]] else "")
    mediators <- as.character(table$Mediators[[index]] %||% if (length(nodes) > 2L) paste(nodes[2L:(length(nodes) - 1L)], collapse = " -> ") else "")
    key <- as.character(table[["Estimand Key"]][[index]] %||% "")
    if (!nzchar(key)) {
      key_parts <- if (identical(effect, "Specific indirect")) nodes else c(predictor, outcome)
      key <- paste(c(tolower(gsub(" ", "_", effect, fixed = TRUE)), key_parts), collapse = "|")
    }
    estimate <- suppressWarnings(as.numeric(table[["Original Est."]][[index]] %||% NA_real_))
    data.frame(
      Effect = effect, `Estimand Key` = key, Path = path,
      Predictor = predictor, Outcome = outcome, Mediators = mediators,
      estimate_numeric = if (length(estimate) && is.finite(estimate[[1L]])) estimate[[1L]] else NA_real_,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

structural_canvas_pls_specific_indirect_registry <- function(path_specs, paths) {
  if (!nrow(path_specs)) return(data.frame())
  nodes <- unique(c(path_specs$predictor, path_specs$outcome))
  adjacency <- stats::setNames(vector("list", length(nodes)), nodes)
  for (index in seq_len(nrow(path_specs))) {
    predictor <- path_specs$predictor[[index]]
    adjacency[[predictor]] <- unique(c(adjacency[[predictor]] %||% character(0), path_specs$outcome[[index]]))
  }
  if (structural_canvas_structural_has_cycle(adjacency)) return(data.frame())
  rows <- list()
  for (predictor in nodes) {
    for (outcome in setdiff(nodes, predictor)) {
      indirect_paths <- Filter(
        function(path) length(path) > 2L,
        structural_canvas_find_structural_paths(adjacency, predictor, outcome)
      )
      for (path in indirect_paths) {
        edge_values <- vapply(seq_len(length(path) - 1L), function(index) {
          structural_canvas_pls_matrix_cell(paths, path[[index]], path[[index + 1L]])
        }, numeric(1))
        estimate <- if (length(edge_values) && all(is.finite(edge_values))) prod(edge_values) else NA_real_
        rows[[length(rows) + 1L]] <- data.frame(
          Effect = "Specific indirect",
          `Estimand Key` = paste(c("specific", path), collapse = "|"),
          Path = paste(path, collapse = " -> "),
          Predictor = predictor,
          Outcome = outcome,
          Mediators = paste(path[2L:(length(path) - 1L)], collapse = " -> "),
          estimate_numeric = estimate,
          check.names = FALSE, stringsAsFactors = FALSE
        )
      }
    }
  }
  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}

structural_canvas_pls_pair_registry <- function(effect, specs, matrix_value, paths = NULL) {
  if (!nrow(specs)) return(data.frame())
  rows <- lapply(seq_len(nrow(specs)), function(index) {
    predictor <- specs$predictor[[index]]
    outcome <- specs$outcome[[index]]
    estimate <- structural_canvas_pls_matrix_cell(matrix_value, predictor, outcome)
    data.frame(
      Effect = effect,
      `Estimand Key` = paste(tolower(gsub(" ", "_", effect, fixed = TRUE)), predictor, outcome, sep = "|"),
      Path = paste(predictor, outcome, sep = " -> "),
      Predictor = predictor, Outcome = outcome, Mediators = "",
      estimate_numeric = estimate,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

structural_canvas_pls_pair_registry_from_specific <- function(effect, specific, paths) {
  if (!is.data.frame(specific) || !nrow(specific)) return(data.frame())
  pairs <- unique(specific[, c("Predictor", "Outcome"), drop = FALSE])
  rows <- lapply(seq_len(nrow(pairs)), function(index) {
    predictor <- as.character(pairs$Predictor[[index]])
    outcome <- as.character(pairs$Outcome[[index]])
    selected <- specific$Predictor == predictor & specific$Outcome == outcome
    component_values <- suppressWarnings(as.numeric(specific$estimate_numeric[selected]))
    indirect <- if (length(component_values) && all(is.finite(component_values))) sum(component_values) else NA_real_
    estimate <- indirect
    if (identical(effect, "Total")) {
      direct <- structural_canvas_pls_matrix_cell(paths, predictor, outcome)
      estimate <- indirect + if (is.finite(direct)) direct else 0
    }
    data.frame(
      Effect = effect,
      `Estimand Key` = paste(tolower(gsub(" ", "_", effect, fixed = TRUE)), predictor, outcome, sep = "|"),
      Path = paste(predictor, outcome, sep = " -> "),
      Predictor = predictor, Outcome = outcome, Mediators = "",
      estimate_numeric = estimate,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

structural_canvas_pls_bootstrap_estimand_row <- function(table, key = "", path = "", predictor = "", outcome = "", pair_fallback = TRUE) {
  if (is.null(table)) return(NULL)
  value <- as.data.frame(table, check.names = FALSE, stringsAsFactors = FALSE)
  if (!nrow(value)) return(NULL)
  if (nzchar(key) && "Estimand Key" %in% names(value)) {
    matched <- match(key, as.character(value[["Estimand Key"]]))
    if (!is.na(matched)) return(value[matched, , drop = FALSE])
  }
  normalize_path <- function(item) paste(structural_canvas_pls_path_tokens(item), collapse = "|")
  expected_path <- normalize_path(path)
  available_paths <- if ("Path" %in% names(value)) as.character(value$Path) else rownames(value)
  if (nzchar(expected_path) && length(available_paths)) {
    matched <- match(expected_path, vapply(available_paths, normalize_path, character(1)))
    if (!is.na(matched)) return(value[matched, , drop = FALSE])
  }
  if (isTRUE(pair_fallback) && nzchar(predictor) && nzchar(outcome)) {
    return(structural_canvas_pls_bootstrap_row(value, predictor, outcome))
  }
  NULL
}

structural_canvas_pls_bootstrap_numeric <- function(row, column) {
  if (is.null(row) || !column %in% names(row)) return(NA_real_)
  value <- suppressWarnings(as.numeric(row[[column]][[1L]]))
  if (length(value) && is.finite(value[[1L]])) value[[1L]] else NA_real_
}

structural_canvas_pls_bootstrap_context <- function(bootstrap, boot_row = NULL) {
  bootstrap <- bootstrap %||% list()
  row_valid <- structural_canvas_pls_bootstrap_numeric(boot_row, "Valid N")
  row_requested <- structural_canvas_pls_bootstrap_numeric(boot_row, "Requested N")
  valid <- if (is.finite(row_valid)) row_valid else suppressWarnings(as.numeric(bootstrap$nboot %||% NA_real_))
  requested <- if (is.finite(row_requested)) row_requested else suppressWarnings(as.numeric(bootstrap$requested_nboot %||% 0L))
  if (!is.finite(requested) || requested < 0) requested <- 0
  row_status <- if (!is.null(boot_row) && "Bootstrap Status" %in% names(boot_row)) as.character(boot_row[["Bootstrap Status"]][[1L]] %||% "") else ""
  row_source <- if (!is.null(boot_row) && "Inference Source" %in% names(boot_row)) as.character(boot_row[["Inference Source"]][[1L]] %||% "") else ""
  status <- as.character(bootstrap$bootstrap_status %||% if (nzchar(row_status)) row_status else if (requested > 0) "Not recorded" else "Not requested")
  status <- if (length(status) && nzchar(trimws(status[[1L]]))) trimws(status[[1L]]) else if (requested > 0) "Not recorded" else "Not requested"
  has_row <- !is.null(boot_row)
  available <- if (requested > 0 && !is.null(bootstrap$inference_available)) {
    isTRUE(bootstrap$inference_available)
  } else {
    has_row
  }
  source <- if (requested <= 0 && !has_row) {
    "Point estimate only - bootstrap not requested"
  } else if (available && has_row) {
    if (nzchar(row_source)) row_source else "Percentile 95% CI and plus-one two-sided empirical sign p"
  } else if (identical(tolower(status), "pending")) {
    "Point estimate retained - bootstrap pending"
  } else if (identical(tolower(status), "canceled")) {
    "Point estimate retained - bootstrap canceled"
  } else if (identical(tolower(status), "failed")) {
    "Point estimate retained - bootstrap failed"
  } else {
    "Point estimate retained - bootstrap inference suppressed"
  }
  list(
    available = isTRUE(available) && has_row,
    source = source,
    status = status,
    valid = if (is.finite(valid)) format(as.integer(valid), trim = TRUE) else "",
    requested = if (requested > 0) format(as.integer(requested), trim = TRUE) else ""
  )
}

structural_canvas_pls_effect_row <- function(descriptor, summary_fit, paths, f_square, f_square_status, bootstrap_table, bootstrap, display_name) {
  effect <- as.character(descriptor$Effect[[1L]])
  predictor <- as.character(descriptor$Predictor[[1L]])
  outcome <- as.character(descriptor$Outcome[[1L]])
  path_nodes <- structural_canvas_pls_path_tokens(descriptor$Path[[1L]])
  if (!length(path_nodes)) path_nodes <- c(predictor, outcome)
  boot_row <- structural_canvas_pls_bootstrap_estimand_row(
    bootstrap_table,
    key = as.character(descriptor[["Estimand Key"]][[1L]] %||% ""),
    path = as.character(descriptor$Path[[1L]] %||% ""),
    predictor = predictor, outcome = outcome,
    pair_fallback = !identical(effect, "Specific indirect")
  )
  context <- structural_canvas_pls_bootstrap_context(bootstrap, boot_row)
  inference_number <- function(column) {
    if (isTRUE(context$available)) structural_canvas_pls_bootstrap_numeric(boot_row, column) else NA_real_
  }
  p_numeric <- inference_number("Bootstrap P Val")
  data.frame(
    Effect = effect,
    `Estimand Key` = as.character(descriptor[["Estimand Key"]][[1L]] %||% ""),
    Path = structural_canvas_pls_path_text(path_nodes, display_name),
    Outcome = display_name(outcome),
    Predictor = display_name(predictor),
    Mediators = if (length(path_nodes) > 2L) structural_canvas_pls_path_text(path_nodes[2L:(length(path_nodes) - 1L)], display_name) else "",
    beta = structural_canvas_pls_number(descriptor$estimate_numeric[[1L]]),
    `Boot SE` = structural_canvas_pls_number(inference_number("Bootstrap SD")),
    `Boot 95% CI lower` = structural_canvas_pls_number(inference_number("2.5% CI")),
    `Boot 95% CI upper` = structural_canvas_pls_number(inference_number("97.5% CI")),
    t = structural_canvas_pls_number(inference_number("T Stat.")),
    p = if (is.finite(p_numeric)) format_p(p_numeric) else "",
    p_numeric = p_numeric,
    `BH-adjusted p` = "",
    `BH family` = switch(effect,
      Direct = "Direct structural paths",
      `Specific indirect` = "Specific indirect effects",
      `Total indirect` = "Total indirect effects",
      Total = "Total effects",
      effect
    ),
    `Inference source` = context$source,
    `Bootstrap status` = context$status,
    `Valid N` = context$valid,
    `Requested N` = context$requested,
    R2 = structural_canvas_pls_number(structural_canvas_pls_matrix_cell(paths, "R^2", outcome)),
    AdjR2 = structural_canvas_pls_number(structural_canvas_pls_matrix_cell(paths, "AdjR^2", outcome)),
    f2 = if (identical(effect, "Direct")) structural_canvas_pls_number(structural_canvas_pls_matrix_cell(f_square, predictor, outcome)) else "",
    `f2 size` = if (identical(effect, "Direct")) structural_canvas_pls_effect_size_label(structural_canvas_pls_matrix_cell(f_square, predictor, outcome)) else "",
    `f2 status` = if (identical(effect, "Direct")) structural_canvas_pls_matrix_cell_character(f_square_status, predictor, outcome) else "",
    `Inner VIF` = if (identical(effect, "Direct")) structural_canvas_pls_number(structural_canvas_pls_inner_vif(summary_fit, predictor, outcome)) else "",
    check.names = FALSE
  )
}

structural_canvas_pls_fit_result_table <- function(summary_fit, diagnostics, display_name, bootstrap = NULL, f_square_result = NULL, construct_order = character(0)) {
  paths <- as.matrix(summary_fit$paths %||% matrix(numeric(0), 0L, 0L))
  f_square <- as.matrix(f_square_result$values %||% summary_fit$fSquare %||% matrix(numeric(0), 0L, 0L))
  f_square_status <- as.matrix(f_square_result$status %||% matrix("", nrow(f_square), ncol(f_square), dimnames = dimnames(f_square)))
  total_effects <- as.matrix(summary_fit$total_effects %||% matrix(numeric(0), 0L, 0L))
  total_indirect_effects <- as.matrix(summary_fit$total_indirect_effects %||% matrix(numeric(0), 0L, 0L))
  path_specs <- structural_canvas_pls_path_specs(diagnostics)
  direct <- structural_canvas_pls_estimand_registry(summary_fit$statedu_direct_paths, "Direct")
  if (!nrow(direct)) direct <- structural_canvas_pls_pair_registry("Direct", path_specs, paths)
  direct[["Estimand Key"]] <- if (nrow(direct)) paste("direct", direct$Predictor, direct$Outcome, sep = "|") else character(0)
  specific <- structural_canvas_pls_estimand_registry(summary_fit$statedu_specific_indirect_paths, "Specific indirect")
  if (!nrow(specific)) specific <- structural_canvas_pls_specific_indirect_registry(path_specs, paths)
  indirect_specs <- structural_canvas_pls_indirect_specs(path_specs)
  total_indirect <- structural_canvas_pls_estimand_registry(summary_fit$statedu_total_indirect_paths, "Total indirect")
  if (!nrow(total_indirect)) total_indirect <- structural_canvas_pls_pair_registry_from_specific("Total indirect", specific, paths)
  # A total effect is reported only for predictor/outcome pairs connected by
  # at least one indirect route. Pure direct paths remain in the direct table.
  total_specs <- indirect_specs
  total <- structural_canvas_pls_estimand_registry(summary_fit$statedu_total_paths, "Total")
  if (nrow(total)) {
    allowed_total <- paste(total_specs$predictor, total_specs$outcome, sep = "\r")
    total <- total[paste(total$Predictor, total$Outcome, sep = "\r") %in% allowed_total, , drop = FALSE]
  }
  if (!nrow(total)) total <- structural_canvas_pls_pair_registry_from_specific("Total", specific, paths)
  descriptors <- Filter(function(value) is.data.frame(value) && nrow(value), list(direct, specific, total_indirect, total))
  if (!length(descriptors)) return(data.frame())
  descriptors <- do.call(rbind, descriptors)
  keep <- !duplicated(paste(descriptors$Effect, descriptors[["Estimand Key"]], sep = "\r"))
  descriptors <- descriptors[keep, , drop = FALSE]
  bootstrap_tables <- list(
    Direct = bootstrap$bootstrapped_paths %||% NULL,
    `Specific indirect` = bootstrap$bootstrapped_specific_indirect_paths %||% NULL,
    `Total indirect` = bootstrap$bootstrapped_total_indirect_paths %||% NULL,
    Total = bootstrap$bootstrapped_total_paths %||% NULL
  )
  rows <- lapply(seq_len(nrow(descriptors)), function(index) {
    descriptor <- descriptors[index, , drop = FALSE]
    structural_canvas_pls_effect_row(
      descriptor, summary_fit, paths, f_square, f_square_status,
      bootstrap_tables[[as.character(descriptor$Effect[[1L]])]], bootstrap, display_name
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame())
  table <- do.call(rbind, rows)
  for (effect in c("Direct", "Specific indirect", "Total indirect", "Total")) {
    family <- which(table$Effect == effect & is.finite(table$p_numeric))
    if (length(family)) table[["BH-adjusted p"]][family] <- vapply(stats::p.adjust(table$p_numeric[family], method = "BH"), format_p, character(1))
  }
  table[["Path detail"]] <- table$Path
  table <- structural_canvas_order_structural_paths(table, construct_order)
  table[["Path detail"]] <- NULL
  table
}

structural_canvas_subset_columns <- function(table, columns) {
  if (!is.data.frame(table) || !nrow(table)) return(data.frame())
  columns <- columns[columns %in% names(table)]
  if (!length(columns)) return(data.frame())
  table[, columns, drop = FALSE]
}

structural_canvas_drop_empty_display_columns <- function(table, columns) {
  if (!is.data.frame(table) || !nrow(table)) return(table)
  candidates <- intersect(as.character(columns), names(table))
  empty <- candidates[vapply(candidates, function(column) {
    !any(nzchar(trimws(as.character(table[[column]]))))
  }, logical(1))]
  if (length(empty)) table[empty] <- NULL
  table
}

structural_canvas_compact_common_display_columns <- function(table, key_columns = character(0)) {
  if (!is.data.frame(table) || !nrow(table)) return(list(table = table, common = character(0)))
  key_columns <- intersect(as.character(key_columns), names(table))
  candidates <- setdiff(names(table), key_columns)
  table <- structural_canvas_drop_empty_display_columns(table, candidates)
  candidates <- setdiff(names(table), key_columns)
  common_columns <- candidates[vapply(candidates, function(column) {
    values <- trimws(as.character(table[[column]]))
    length(unique(values)) == 1L && nzchar(values[[1L]])
  }, logical(1))]
  common <- if (length(common_columns)) {
    stats::setNames(vapply(common_columns, function(column) as.character(table[[column]][[1L]]), character(1)), common_columns)
  } else character(0)
  keep <- c(key_columns, setdiff(names(table), c(key_columns, common_columns)))
  list(table = table[, keep, drop = FALSE], common = common)
}

structural_canvas_pls_fit_main_table <- function(table) {
  if (!is.data.frame(table) || !nrow(table)) return(data.frame())
  display <- table
  if ("Effect" %in% names(display)) display <- display[display$Effect == "Direct", , drop = FALSE]
  if (all(c("R2", "AdjR2") %in% names(display))) {
    r2 <- as.character(display$R2 %||% "")
    adj_r2 <- as.character(display$AdjR2 %||% "")
    display$R2AdjR2 <- ifelse(nzchar(r2) & nzchar(adj_r2), paste0(r2, " (", adj_r2, ")"), ifelse(nzchar(r2), r2, adj_r2))
  }
  display <- structural_canvas_subset_columns(display, c(
    "Path", "beta", "Boot SE", "Boot 95% CI lower", "Boot 95% CI upper", "t", "p", "BH-adjusted p",
    "f2", "R2AdjR2", "Inner VIF"
  ))
  structural_canvas_drop_empty_display_columns(
    display,
    c("Boot SE", "Boot 95% CI lower", "Boot 95% CI upper", "t", "p", "BH-adjusted p")
  )
}

structural_canvas_pls_fit_guide_table <- function(table) {
  if (!is.data.frame(table) || !nrow(table)) return(data.frame())
  display <- table
  if ("Effect" %in% names(display)) display <- display[display$Effect == "Direct", , drop = FALSE]
  if (!nrow(display)) return(data.frame())
  value_columns <- intersect(c("f2", "Inner VIF"), names(display))
  if (length(value_columns)) {
    has_value <- apply(display[, value_columns, drop = FALSE], 1L, function(row) {
      any(nzchar(trimws(as.character(row))))
    })
    display <- display[has_value, , drop = FALSE]
  }
  display <- structural_canvas_subset_columns(display, c("Outcome", "Predictor", "f2", "Inner VIF"))
  if ("f2" %in% names(display)) names(display)[names(display) == "f2"] <- "f²"
  display
}

structural_canvas_pls_fit_bootstrap_table <- function(table) {
  structural_canvas_subset_columns(table, c(
    "Effect", "Path", "Outcome", "Predictor", "Mediators", "beta",
    "Boot SE", "Boot 95% CI lower", "Boot 95% CI upper", "t", "p", "BH-adjusted p", "BH family",
    "Inference source", "Bootstrap status", "Valid N", "Requested N"
  ))
}

structural_canvas_pls_effect_table <- function(table, effect) {
  if (!is.data.frame(table) || !nrow(table) || !"Effect" %in% names(table)) return(data.frame())
  rows <- table[table$Effect == effect, , drop = FALSE]
  display <- structural_canvas_subset_columns(rows, c(
    "Path", "Outcome", "Predictor", "Mediators", "beta",
    "Boot SE", "Boot 95% CI lower", "Boot 95% CI upper", "t", "p", "BH-adjusted p"
  ))
  structural_canvas_drop_empty_display_columns(
    display,
    c("Boot SE", "Boot 95% CI lower", "Boot 95% CI upper", "t", "p", "BH-adjusted p")
  )
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
    assigned_indicators <- structural_canvas_pls_assigned_indicators(snapshot, latent)
    fixed_single_indicator <- length(assigned_indicators) == 1L
    for (indicator in assigned_indicators) {
      cross_values <- if (indicator %in% rownames(cross_loadings)) suppressWarnings(as.numeric(cross_loadings[indicator, setdiff(colnames(cross_loadings), construct)])) else NA_real_
      cross_max <- if (any(is.finite(cross_values))) max(abs(cross_values), na.rm = TRUE) else NA_real_
      loading_boot <- structural_canvas_pls_bootstrap_row(bootstrap_loadings, indicator, construct)
      weight_boot <- structural_canvas_pls_bootstrap_row(bootstrap_weights, indicator, construct)
      rows[[length(rows) + 1L]] <- data.frame(
        Construct = display_name(construct),
        `Construct type` = construct_type_label,
        Indicator = display_name(indicator),
        Loading = structural_canvas_pls_number(structural_canvas_pls_matrix_cell(loadings, indicator, construct)),
        `Loading Boot SE` = if (fixed_single_indicator) "—" else structural_canvas_pls_bootstrap_value(loading_boot, "Bootstrap SD"),
        `Loading CI lower` = if (fixed_single_indicator) "—" else structural_canvas_pls_bootstrap_value(loading_boot, "2.5% CI"),
        `Loading CI upper` = if (fixed_single_indicator) "—" else structural_canvas_pls_bootstrap_value(loading_boot, "97.5% CI"),
        `Loading t` = if (fixed_single_indicator) "—" else structural_canvas_pls_bootstrap_value(loading_boot, "T Stat."),
        `Loading p` = if (fixed_single_indicator) "—" else structural_canvas_pls_bootstrap_p(loading_boot),
        loading_p_numeric = if (fixed_single_indicator) NA_real_ else structural_canvas_pls_bootstrap_p_numeric(loading_boot),
        Weight = structural_canvas_pls_number(structural_canvas_pls_matrix_cell(weights, indicator, construct)),
        `Weight Boot SE` = if (fixed_single_indicator) "—" else structural_canvas_pls_bootstrap_value(weight_boot, "Bootstrap SD"),
        `Weight CI lower` = if (fixed_single_indicator) "—" else structural_canvas_pls_bootstrap_value(weight_boot, "2.5% CI"),
        `Weight CI upper` = if (fixed_single_indicator) "—" else structural_canvas_pls_bootstrap_value(weight_boot, "97.5% CI"),
        `Weight t` = if (fixed_single_indicator) "—" else structural_canvas_pls_bootstrap_value(weight_boot, "T Stat."),
        `Weight p` = if (fixed_single_indicator) "—" else structural_canvas_pls_bootstrap_p(weight_boot),
        weight_p_numeric = if (fixed_single_indicator) NA_real_ else structural_canvas_pls_bootstrap_p_numeric(weight_boot),
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
  table <- structural_canvas_add_bh_column(table, "weight_p_numeric", "Weight BH-adjusted p")
  fixed_loading <- table[["Loading p"]] == "—"
  fixed_weight <- table[["Weight p"]] == "—"
  table[["Loading BH-adjusted p"]][fixed_loading] <- "—"
  table[["Weight BH-adjusted p"]][fixed_weight] <- "—"
  table
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
  display <- structural_canvas_subset_columns(display, c("Construct", "Construct type", "Indicator", "loading/weight", "Boot SE", "Boot 95% CI lower", "Boot 95% CI upper", "Boot t", "Boot p", "Boot BH-adjusted p", "Item VIF", "Mode"))
  bootstrap_columns <- intersect(
    c("Boot SE", "Boot 95% CI lower", "Boot 95% CI upper", "Boot t", "Boot p", "Boot BH-adjusted p"),
    names(display)
  )
  structural_canvas_drop_empty_display_columns(display, bootstrap_columns)
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

structural_canvas_structural_construct_order <- function(snapshot, display_name = function(value) value) {
  latent_nodes <- Filter(function(node) identical(as.character(node$role %||% ""), "latent"), snapshot$nodes %||% list())
  if (!length(latent_nodes)) return(character(0))
  node_ids <- vapply(latent_nodes, function(node) as.character(node$id %||% ""), character(1))
  node_names <- vapply(latent_nodes, structural_canvas_name, character(1))
  valid <- nzchar(node_ids) & nzchar(node_names)
  node_ids <- node_ids[valid]
  node_names <- node_names[valid]
  if (!length(node_names)) return(character(0))
  keep <- !duplicated(node_ids) & !duplicated(node_names)
  node_ids <- node_ids[keep]
  node_names <- node_names[keep]
  id_to_name <- stats::setNames(node_names, node_ids)
  latent_name <- function(id) {
    value <- unname(id_to_name[as.character(id %||% "")])
    if (!length(value) || is.na(value[[1L]])) "" else as.character(value[[1L]])
  }

  adjacency <- stats::setNames(vector("list", length(node_names)), node_names)
  indegree <- stats::setNames(integer(length(node_names)), node_names)
  for (edge in snapshot$edges %||% list()) {
    if (identical(as.character(edge$kind %||% ""), "covariance") ||
        identical(as.character(edge$pathType %||% ""), "higherOrder")) next
    from <- latent_name(edge$from)
    to <- latent_name(edge$to)
    if (!nzchar(from) || !nzchar(to) || identical(from, to) || to %in% (adjacency[[from]] %||% character(0))) next
    adjacency[[from]] <- c(adjacency[[from]] %||% character(0), to)
    indegree[[to]] <- indegree[[to]] + 1L
  }

  base_rank <- stats::setNames(seq_along(node_names), node_names)
  available <- node_names[indegree[node_names] == 0L]
  ordered <- character(0)
  while (length(available)) {
    available <- available[order(base_rank[available], method = "radix")]
    current <- available[[1L]]
    available <- available[-1L]
    ordered <- c(ordered, current)
    for (next_node in adjacency[[current]] %||% character(0)) {
      indegree[[next_node]] <- indegree[[next_node]] - 1L
      if (indegree[[next_node]] == 0L && !next_node %in% c(ordered, available)) available <- c(available, next_node)
    }
  }
  if (length(ordered) < length(node_names)) ordered <- c(ordered, setdiff(node_names, ordered))
  unique(vapply(ordered, display_name, character(1)))
}

structural_canvas_order_structural_paths <- function(table, construct_order) {
  if (!is.data.frame(table) || !nrow(table) || !all(c("Outcome", "Predictor") %in% names(table))) return(table)
  outcome <- as.character(table$Outcome)
  predictor <- as.character(table$Predictor)
  unknown <- unique(c(outcome, predictor))
  rank_names <- unique(c(as.character(construct_order %||% character(0)), unknown))
  rank_value <- function(values) {
    ranks <- match(as.character(values), rank_names)
    if (anyNA(ranks)) {
      missing_names <- unique(as.character(values)[is.na(ranks)])
      missing_rank <- stats::setNames(length(rank_names) + seq_along(missing_names), missing_names)
      ranks[is.na(ranks)] <- unname(missing_rank[as.character(values)[is.na(ranks)]])
    }
    ranks
  }
  outcome_rank <- rank_value(outcome)
  predictor_rank <- rank_value(predictor)
  effect <- as.character(table$Effect %||% rep("", nrow(table)))
  effect_rank <- match(effect, c("Direct", "Specific indirect", "Total indirect", "Indirect", "Total"))
  effect_rank[is.na(effect_rank)] <- 5L
  path_detail <- as.character(table[["Path detail"]] %||% rep("", nrow(table)))
  path_key <- vapply(path_detail, function(path) {
    nodes <- trimws(strsplit(path, "→", fixed = TRUE)[[1L]])
    nodes <- nodes[nzchar(nodes)]
    if (!length(nodes)) return("")
    paste(sprintf("%06d", rank_value(nodes)), collapse = "-")
  }, character(1))
  table[order(outcome_rank, predictor_rank, effect_rank, path_key, seq_len(nrow(table)), method = "radix"), , drop = FALSE]
}

structural_canvas_effect_bootstrap_ci_label <- function(bootstrap) {
  ci_method <- if (is.data.frame(bootstrap) && nrow(bootstrap) && "ci_method" %in% names(bootstrap)) {
    structural_canvas_bootstrap_ci_method(bootstrap$ci_method[[1L]])
  } else {
    "percentile"
  }
  method <- switch(
    ci_method,
    bca = "bias-corrected and accelerated (BCa)",
    bias_corrected = "bias-corrected (BC)",
    "percentile"
  )
  quantile_type <- if (is.data.frame(bootstrap) && nrow(bootstrap) && "quantile_type" %in% names(bootstrap)) {
    suppressWarnings(as.integer(bootstrap$quantile_type[[1L]]))
  } else {
    structural_canvas_bootstrap_quantile_type(method, "structural_effects")
  }
  paste0("Bootstrap ", method, " 95% CI (R quantile type ", quantile_type, ")")
}

structural_canvas_effect_bootstrap_metadata <- function(row = NULL, bootstrap = NULL, valid_column = "valid") {
  requested <- if (is.data.frame(row) && nrow(row) && "requested" %in% names(row)) {
    suppressWarnings(as.integer(row$requested[[1L]]))
  } else if (is.data.frame(bootstrap) && nrow(bootstrap) && "requested" %in% names(bootstrap)) {
    candidates <- suppressWarnings(as.integer(bootstrap$requested))
    candidates <- candidates[is.finite(candidates) & candidates > 0L]
    if (length(candidates)) candidates[[1L]] else NA_integer_
  } else {
    NA_integer_
  }
  was_requested <- is.data.frame(bootstrap) && nrow(bootstrap) && is.finite(requested) && requested > 0L
  if (!was_requested) {
    return(list(requested = FALSE, valid = NA_integer_, requested_n = NA_integer_, percent = NA_real_,
                usable = FALSE, valid_label = "", status = "Not requested"))
  }
  valid <- if (is.data.frame(row) && nrow(row) && valid_column %in% names(row)) {
    suppressWarnings(as.integer(row[[valid_column]][[1L]]))
  } else {
    0L
  }
  if (!is.finite(valid) || valid < 0L) valid <- 0L
  percent <- 100 * valid / requested
  usable <- structural_canvas_bootstrap_inference_usable(valid, requested)
  list(
    requested = TRUE,
    valid = valid,
    requested_n = requested,
    percent = percent,
    usable = usable,
    valid_label = sprintf("%d/%d (%.1f%%)", valid, requested, percent),
    status = as.character(structural_canvas_bootstrap_status(valid, requested)[[1L]])
  )
}

structural_canvas_effect_inference_source <- function(metadata) {
  if (!isTRUE(metadata$requested)) return("Model-based normal-theory")
  if (isTRUE(metadata$usable)) "Bootstrap (empirical two-sided p)" else "Bootstrap requested - inference suppressed"
}

structural_canvas_effect_bootstrap_reporting_state <- function(
  requested = 0L, result = NULL, pending = FALSE, canceled = FALSE, error = "",
  blocked_reason = ""
) {
  requested <- suppressWarnings(as.integer(requested %||% 0L))
  requested <- length(requested) && is.finite(requested[[1L]]) && requested[[1L]] > 0L
  if (!isTRUE(requested)) {
    return(list(
      state = "not_requested", reason = "", requested = FALSE, complete = FALSE,
      source = "", note = "Bootstrap was not requested."
    ))
  }
  blocked_reason <- trimws(as.character(blocked_reason %||% "")[[1L]])
  if (nzchar(blocked_reason)) {
    return(list(
      state = "blocked", reason = blocked_reason,
      requested = TRUE, complete = FALSE,
      source = "Bootstrap blocked - original model ineligible",
      note = blocked_reason
    ))
  }
  error <- trimws(as.character(error %||% "")[[1L]])
  if (isTRUE(canceled)) {
    return(list(
      state = "canceled", reason = "Canceled by user",
      requested = TRUE, complete = FALSE,
      source = "Bootstrap canceled - inference suppressed",
      note = "Structural-effect bootstrap was canceled; model-based inferential values were not substituted."
    ))
  }
  if (nzchar(error)) {
    return(list(
      state = "failed", reason = error,
      requested = TRUE, complete = FALSE,
      source = "Bootstrap failed - inference suppressed",
      note = paste0("Structural-effect bootstrap failed; model-based inferential values were not substituted. Error: ", error)
    ))
  }
  if (isTRUE(pending)) {
    return(list(
      state = "pending", reason = "",
      requested = TRUE, complete = FALSE,
      source = "Bootstrap pending - inference suppressed",
      note = "Structural-effect bootstrap is pending; model-based inferential values were not substituted."
    ))
  }
  if (is.data.frame(result) && nrow(result)) {
    return(list(
      state = "complete", reason = "",
      requested = TRUE, complete = TRUE, source = "",
      note = "Structural-effect bootstrap completed; inferential values use the recorded bootstrap result and valid-replicate gate."
    ))
  }
  list(
    state = "unavailable", reason = "Structural-effect bootstrap returned no result.",
    requested = TRUE, complete = FALSE,
    source = "Bootstrap unavailable - inference suppressed",
    note = "Structural-effect bootstrap returned no result; model-based inferential values were not substituted."
  )
}

structural_canvas_effect_bootstrap_bundle_state <- function(bundle) {
  structural_canvas_effect_bootstrap_reporting_state(
    requested = bundle$effect_bootstrap %||% 0L,
    result = bundle$effect_bootstrap_result %||% NULL,
    pending = isTRUE(bundle$effect_bootstrap_pending),
    canceled = isTRUE(bundle$effect_bootstrap_canceled),
    error = bundle$effect_bootstrap_error %||% "",
    blocked_reason = bundle$effect_bootstrap_blocked_reason %||% ""
  )
}

structural_canvas_effect_bh_family <- function(effect) {
  effect <- as.character(effect %||% "")
  unname(c(
    "Direct" = "Direct structural paths",
    "Specific indirect" = "Specific indirect effects",
    "Indirect" = "Other indirect and total effects",
    "Total" = "Other indirect and total effects"
  )[effect])
}

structural_canvas_apply_effect_bh_families <- function(table) {
  if (!is.data.frame(table) || !nrow(table)) return(table)
  table[["BH family"]] <- vapply(as.character(table$Effect %||% ""), function(effect) {
    family <- structural_canvas_effect_bh_family(effect)
    if (!length(family) || is.na(family)) "Other effects" else family
  }, character(1))
  inference_source <- as.character(table[["Inference source"]] %||% rep("", nrow(table)))
  fixed_parameter <- grepl("^Fixed parameter", inference_source)
  fixed_effect <- grepl("^Fixed effect", inference_source)
  table[["BH family"]][fixed_parameter] <- "Fixed parameter (not tested)"
  table[["BH family"]][fixed_effect] <- "Fixed effect (not tested)"
  table[["BH-adjusted p"]] <- ""
  p_values <- suppressWarnings(as.numeric(table$p_numeric %||% NA_real_))
  for (family in unique(table[["BH family"]])) {
    indices <- which(table[["BH family"]] == family & is.finite(p_values))
    if (!length(indices)) next
    table[["BH-adjusted p"]][indices] <- vapply(stats::p.adjust(p_values[indices], method = "BH"), format_p, character(1))
  }
  table
}

structural_canvas_effect_ci_text <- function(lower, upper) {
  lower <- trimws(as.character(lower %||% ""))
  upper <- trimws(as.character(upper %||% ""))
  if (!nzchar(lower) && !nzchar(upper)) "" else paste0(lower, " ~ ", upper)
}

structural_canvas_effect_p_text <- function(value) {
  value <- suppressWarnings(as.numeric(value))
  if (length(value) != 1L || !is.finite(value)) "" else format_p(value)
}

structural_canvas_lavaan_structural_effect_rows <- function(fit, effect_definitions, fmt, display_name, bootstrap = NULL, bootstrap_state = NULL) {
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
    fixed_effect <- structural_canvas_lavaan_effect_is_constant(fit, effect)
    bootstrap_suppression_source <- if (
      is.list(bootstrap_state) && isTRUE(bootstrap_state$requested) && !isTRUE(bootstrap_state$complete)
    ) as.character(bootstrap_state$source %||% "") else ""
    boot <- if (is.data.frame(bootstrap)) bootstrap[bootstrap$lhs == effect$label & bootstrap$op == ":=", , drop = FALSE] else data.frame()
    metadata <- structural_canvas_effect_bootstrap_metadata(boot, bootstrap)
    estimate <- suppressWarnings(as.numeric(row$est[[1L]]))
    se <- suppressWarnings(as.numeric(row$se[[1L]]))
    ci_lower <- suppressWarnings(as.numeric(row$ci.lower[[1L]]))
    ci_upper <- suppressWarnings(as.numeric(row$ci.upper[[1L]]))
    z_value <- suppressWarnings(as.numeric(row$z[[1L]]))
    p_value <- suppressWarnings(as.numeric(row$pvalue[[1L]]))
    if (isTRUE(metadata$requested)) {
      se <- if (isTRUE(metadata$usable) && "se" %in% names(boot)) suppressWarnings(as.numeric(boot$se[[1L]])) else NA_real_
      ci_lower <- if (isTRUE(metadata$usable) && "lower" %in% names(boot)) suppressWarnings(as.numeric(boot$lower[[1L]])) else NA_real_
      ci_upper <- if (isTRUE(metadata$usable) && "upper" %in% names(boot)) suppressWarnings(as.numeric(boot$upper[[1L]])) else NA_real_
      p_value <- if (isTRUE(metadata$usable) && "p" %in% names(boot)) suppressWarnings(as.numeric(boot$p[[1L]])) else NA_real_
      # Report the Wald ratio separately from the empirical bootstrap p value.
      z_value <- if (is.finite(se) && se > 0) estimate / se else NA_real_
    }
    b_ci_source <- if (isTRUE(metadata$requested)) {
      if (isTRUE(metadata$usable) && all(is.finite(c(ci_lower, ci_upper)))) structural_canvas_effect_bootstrap_ci_label(boot) else "Not estimated - insufficient valid bootstrap replicates"
    } else {
      "Model-based 95% CI"
    }
    standardized_row <- standardized[standardized$lhs == effect$label, , drop = FALSE]
    beta_p <- if (nrow(standardized_row)) suppressWarnings(as.numeric(standardized_row$pvalue[[1L]])) else NA_real_
    if (isTRUE(metadata$requested)) beta_p <- if (isTRUE(structural_canvas_effect_bootstrap_metadata(boot, bootstrap, "beta_valid")$usable) && "beta_p" %in% names(boot)) as.numeric(boot$beta_p[[1L]]) else NA_real_
    beta <- if (nrow(standardized_row)) suppressWarnings(as.numeric(standardized_row$est.std[[1L]])) else structural_canvas_lavaan_standardized_effect(fit, effect)
    beta_ci_lower <- if (nrow(standardized_row)) suppressWarnings(as.numeric(standardized_row$ci.lower[[1L]])) else NA_real_
    beta_ci_upper <- if (nrow(standardized_row)) suppressWarnings(as.numeric(standardized_row$ci.upper[[1L]])) else NA_real_
    beta_ci_source <- "Model-based 95% CI"
    if (isTRUE(metadata$requested)) {
      beta_metadata <- structural_canvas_effect_bootstrap_metadata(boot, bootstrap, "beta_valid")
      if (isTRUE(beta_metadata$usable) && all(c("beta_lower", "beta_upper") %in% names(boot))) {
        beta_ci_lower <- suppressWarnings(as.numeric(boot$beta_lower[[1L]]))
        beta_ci_upper <- suppressWarnings(as.numeric(boot$beta_upper[[1L]]))
      } else {
        beta_ci_lower <- beta_ci_upper <- NA_real_
      }
      beta_ci_source <- if (isTRUE(beta_metadata$usable) && all(is.finite(c(beta_ci_lower, beta_ci_upper)))) {
        paste0(
          structural_canvas_effect_bootstrap_ci_label(boot),
          "; valid standardized bootstrap ", beta_metadata$valid_label,
          "; status ", beta_metadata$status
        )
      } else {
        paste0(
          "Not estimated - insufficient valid standardized bootstrap replicates",
          if (nzchar(beta_metadata$valid_label)) paste0("; valid ", beta_metadata$valid_label, "; status ", beta_metadata$status) else ""
        )
      }
    }
    if (nzchar(bootstrap_suppression_source)) {
      se <- ci_lower <- ci_upper <- z_value <- p_value <- NA_real_
      beta_ci_lower <- beta_ci_upper <- NA_real_
      b_ci_source <- beta_ci_source <- bootstrap_suppression_source
    }
    if (isTRUE(fixed_effect)) {
      se <- ci_lower <- ci_upper <- z_value <- p_value <- NA_real_
      beta_ci_lower <- beta_ci_upper <- NA_real_
      b_ci_source <- beta_ci_source <- "Fixed effect - no inferential test"
    }
    data.frame(
      Effect = as.character(effect$type %||% ""),
      Outcome = vapply(as.character(effect$outcome %||% ""), display_name, character(1)),
      Predictor = vapply(as.character(effect$predictor %||% ""), display_name, character(1)),
      `Path detail` = if (identical(as.character(effect$type %||% ""), "Specific indirect")) {
        path <- as.character(effect$path %||% character(0))
        if (length(path)) paste(vapply(path, display_name, character(1)), collapse = " → ") else ""
      } else "",
      B = fmt(estimate),
      `B 95% CI lower` = fmt(ci_lower),
      `B 95% CI upper` = fmt(ci_upper),
      `B CI source` = b_ci_source,
      SE = fmt(se),
      beta = fmt(beta),
      `beta 95% CI lower` = fmt(beta_ci_lower),
      `beta 95% CI upper` = fmt(beta_ci_upper),
      `beta CI source` = beta_ci_source,
      R2 = "",
      `beta p` = structural_canvas_effect_p_text(if (isTRUE(fixed_effect) || nzchar(bootstrap_suppression_source)) NA_real_ else beta_p),
      z = fmt(z_value),
      p = structural_canvas_effect_p_text(p_value),
      p_numeric = p_value,
      `BH-adjusted p` = "",
      `Inference source` = if (isTRUE(fixed_effect)) "Fixed effect - no inferential test" else if (nzchar(bootstrap_suppression_source)) bootstrap_suppression_source else structural_canvas_effect_inference_source(metadata),
      `Valid bootstrap` = if (isTRUE(fixed_effect) || nzchar(bootstrap_suppression_source)) "" else metadata$valid_label,
      `Bootstrap status` = if (isTRUE(fixed_effect)) "Fixed effect - no inferential test" else if (nzchar(bootstrap_suppression_source)) bootstrap_suppression_source else metadata$status,
      `BH family` = if (isTRUE(fixed_effect)) "Fixed effect (not tested)" else structural_canvas_effect_bh_family(as.character(effect$type %||% "")),
      check.names = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame())
  table <- do.call(rbind, rows)
  names(table)[names(table) == "R2"] <- "R2"
  table
}

structural_canvas_lavaan_structural_result_table <- function(kind, fit, ko, fmt, display_name, effect_definitions = list(), bootstrap = NULL, bootstrap_state = NULL) {
  if (!kind %in% c("structural", "structural_ci", "structural_effects", "structural_effect_ci", "structural_specific_indirect")) return(NULL)
  raw <- lavaan::parameterEstimates(fit, ci = TRUE)
  raw <- raw[raw$op == "~", c("lhs", "rhs", "est", "se", "z", "pvalue", "ci.lower", "ci.upper"), drop = FALSE]
  if (!nrow(raw)) return(data.frame())
  parameter_table <- lavaan::parameterTable(fit)
  path_parameters <- parameter_table[parameter_table$op == "~", c("lhs", "rhs", "free"), drop = FALSE]
  raw_parameter_key <- paste(raw$lhs, raw$rhs, sep = "\r")
  path_parameter_key <- paste(path_parameters$lhs, path_parameters$rhs, sep = "\r")
  parameter_match <- match(raw_parameter_key, path_parameter_key)
  fixed <- path_parameters$free[parameter_match] == 0L
  fixed[is.na(fixed)] <- raw$se[is.na(fixed)] == 0 & is.na(raw$z[is.na(fixed)]) & is.na(raw$pvalue[is.na(fixed)])
  bootstrap_suppression_source <- if (
    is.list(bootstrap_state) && isTRUE(bootstrap_state$requested) && !isTRUE(bootstrap_state$complete)
  ) as.character(bootstrap_state$source %||% "") else ""
  bootstrap_requested <- is.data.frame(bootstrap) && nrow(bootstrap)
  bootstrap_paths <- if (bootstrap_requested) bootstrap[bootstrap$op == "~", , drop = FALSE] else data.frame()
  raw_keys <- paste(raw$lhs, "~", raw$rhs, sep = "\r")
  bootstrap_keys <- if (nrow(bootstrap_paths)) paste(bootstrap_paths$lhs, "~", bootstrap_paths$rhs, sep = "\r") else character(0)
  bootstrap_match <- match(raw_keys, bootstrap_keys)
  bootstrap_rows <- lapply(seq_len(nrow(raw)), function(index) {
    match_index <- bootstrap_match[[index]]
    if (is.na(match_index)) data.frame() else bootstrap_paths[match_index, , drop = FALSE]
  })
  metadata <- lapply(bootstrap_rows, structural_canvas_effect_bootstrap_metadata, bootstrap = bootstrap)

  estimate <- suppressWarnings(as.numeric(raw$est))
  se <- suppressWarnings(as.numeric(raw$se))
  z_value <- suppressWarnings(as.numeric(raw$z))
  p_value <- suppressWarnings(as.numeric(raw$pvalue))
  ci_lower <- suppressWarnings(as.numeric(raw$ci.lower))
  ci_upper <- suppressWarnings(as.numeric(raw$ci.upper))
  b_ci_source <- rep("Model-based 95% CI", nrow(raw))
  if (bootstrap_requested) {
    for (index in seq_len(nrow(raw))) {
      boot <- bootstrap_rows[[index]]
      meta <- metadata[[index]]
      se[[index]] <- if (isTRUE(meta$usable) && "se" %in% names(boot)) suppressWarnings(as.numeric(boot$se[[1L]])) else NA_real_
      ci_lower[[index]] <- if (isTRUE(meta$usable) && "lower" %in% names(boot)) suppressWarnings(as.numeric(boot$lower[[1L]])) else NA_real_
      ci_upper[[index]] <- if (isTRUE(meta$usable) && "upper" %in% names(boot)) suppressWarnings(as.numeric(boot$upper[[1L]])) else NA_real_
      p_value[[index]] <- if (isTRUE(meta$usable) && "p" %in% names(boot)) suppressWarnings(as.numeric(boot$p[[1L]])) else NA_real_
      # Wald ratio; the p column retains the empirical bootstrap test.
      z_value[[index]] <- if (is.finite(se[[index]]) && se[[index]] > 0) estimate[[index]] / se[[index]] else NA_real_
      b_ci_source[[index]] <- if (isTRUE(meta$usable) && all(is.finite(c(ci_lower[[index]], ci_upper[[index]])))) {
        structural_canvas_effect_bootstrap_ci_label(boot)
      } else {
        "Not estimated - insufficient valid bootstrap replicates"
      }
    }
  }
  if (nzchar(bootstrap_suppression_source)) {
    se[!fixed] <- z_value[!fixed] <- p_value[!fixed] <- NA_real_
    ci_lower[!fixed] <- ci_upper[!fixed] <- NA_real_
    b_ci_source[!fixed] <- bootstrap_suppression_source
  }
  if (any(fixed)) {
    se[fixed] <- z_value[fixed] <- p_value[fixed] <- NA_real_
    ci_lower[fixed] <- ci_upper[fixed] <- NA_real_
    b_ci_source[fixed] <- "Not applicable - fixed parameter"
  }
  standardized <- lavaan::standardizedSolution(fit, ci = TRUE, level = .95)
  standardized <- standardized[standardized$op == "~", c("lhs", "rhs", "est.std", "ci.lower", "ci.upper", "pvalue"), drop = FALSE]
  raw_key <- paste(raw$lhs, raw$rhs, sep = "\r")
  standardized_key <- paste(standardized$lhs, standardized$rhs, sep = "\r")
  standardized_match <- match(raw_key, standardized_key)
  beta <- standardized$est.std[standardized_match]
  beta_p <- standardized$pvalue[standardized_match]
  beta_ci_lower <- standardized$ci.lower[standardized_match]
  beta_ci_upper <- standardized$ci.upper[standardized_match]
  beta_ci_source <- rep("Model-based 95% CI", nrow(raw))
  if (bootstrap_requested) {
    for (index in seq_len(nrow(raw))) {
      boot <- bootstrap_rows[[index]]
      beta_metadata <- structural_canvas_effect_bootstrap_metadata(boot, bootstrap, "beta_valid")
      beta_p[[index]] <- if (isTRUE(beta_metadata$usable) && "beta_p" %in% names(boot)) as.numeric(boot$beta_p[[1L]]) else NA_real_
      if (isTRUE(beta_metadata$usable) && all(c("beta_lower", "beta_upper") %in% names(boot))) {
        beta_ci_lower[[index]] <- suppressWarnings(as.numeric(boot$beta_lower[[1L]]))
        beta_ci_upper[[index]] <- suppressWarnings(as.numeric(boot$beta_upper[[1L]]))
      } else {
        beta_ci_lower[[index]] <- beta_ci_upper[[index]] <- NA_real_
      }
      beta_ci_source[[index]] <- if (isTRUE(beta_metadata$usable) && all(is.finite(c(beta_ci_lower[[index]], beta_ci_upper[[index]])))) {
        paste0(
          structural_canvas_effect_bootstrap_ci_label(boot),
          "; valid standardized bootstrap ", beta_metadata$valid_label,
          "; status ", beta_metadata$status
        )
      } else {
        paste0(
          "Not estimated - insufficient valid standardized bootstrap replicates",
          if (nzchar(beta_metadata$valid_label)) paste0("; valid ", beta_metadata$valid_label, "; status ", beta_metadata$status) else ""
        )
      }
    }
  }
  if (nzchar(bootstrap_suppression_source)) {
    beta_ci_lower[!fixed] <- beta_ci_upper[!fixed] <- NA_real_
    beta_ci_source[!fixed] <- bootstrap_suppression_source
  }
  if (any(fixed)) {
    beta_ci_lower[fixed] <- beta_ci_upper[fixed] <- NA_real_
    beta_ci_source[fixed] <- "Not applicable - fixed parameter"
  }
  r2_values <- tryCatch(lavaan::lavInspect(fit, "r2"), error = function(error) numeric(0))
  r2 <- suppressWarnings(as.numeric(r2_values[raw$lhs]))
  inference_source <- vapply(metadata, structural_canvas_effect_inference_source, character(1))
  valid_bootstrap <- vapply(metadata, function(value) value$valid_label, character(1))
  bootstrap_status <- vapply(metadata, function(value) value$status, character(1))
  if (nzchar(bootstrap_suppression_source)) {
    inference_source[!fixed] <- bootstrap_suppression_source
    valid_bootstrap[!fixed] <- ""
    bootstrap_status[!fixed] <- bootstrap_suppression_source
  }
  inference_source[fixed] <- "Fixed parameter - no inferential test"
  valid_bootstrap[fixed] <- ""
  bootstrap_status[fixed] <- "Not applicable - fixed parameter"
  table <- data.frame(
    Effect = "Direct",
    vapply(raw$lhs, display_name, character(1)),
    vapply(raw$rhs, display_name, character(1)),
    `Path detail` = "",
    B = fmt(estimate),
    `B 95% CI lower` = fmt(ci_lower),
    `B 95% CI upper` = fmt(ci_upper),
    `B CI source` = b_ci_source,
    SE = fmt(se),
    beta = fmt(beta),
    `beta 95% CI lower` = fmt(beta_ci_lower),
    `beta 95% CI upper` = fmt(beta_ci_upper),
    `beta CI source` = beta_ci_source,
    R2 = fmt(r2),
    `beta p` = vapply(ifelse(fixed | nzchar(bootstrap_suppression_source), NA_real_, beta_p), structural_canvas_effect_p_text, character(1)),
    z = fmt(z_value),
    p = vapply(p_value, structural_canvas_effect_p_text, character(1)),
    p_numeric = p_value,
    `BH-adjusted p` = "",
    `Inference source` = inference_source,
    `Valid bootstrap` = valid_bootstrap,
    `Bootstrap status` = bootstrap_status,
    `BH family` = ifelse(fixed, "Fixed parameter (not tested)", "Direct structural paths"),
    check.names = FALSE
  )
  names(table)[1:3] <- c("Effect", if (ko) c("결과변수", "예측변수") else c("Outcome", "Predictor"))
  names(table)[names(table) == "R2"] <- "R²"
  table
}

structural_canvas_table_uses_bootstrap_inference <- function(table) {
  if (!is.data.frame(table) || !nrow(table)) return(FALSE)
  inference_source <- as.character(table[["Inference source"]] %||% rep("", nrow(table)))
  valid_bootstrap <- as.character(table[["Valid bootstrap"]] %||% rep("", nrow(table)))
  any(grepl("^Bootstrap", inference_source) | nzchar(trimws(valid_bootstrap)))
}

structural_canvas_effect_summary_table <- function(structural_table, ci = FALSE) {
  if (!is.data.frame(structural_table) || !nrow(structural_table) ||
      !all(c("Effect", "Outcome", "Predictor") %in% names(structural_table))) {
    return(data.frame())
  }
  if (!any(structural_table$Effect %in% c("Indirect", "Total"))) return(data.frame())
  if (!isTRUE(ci)) {
    rows <- structural_table[structural_table$Effect %in% c("Direct", "Indirect", "Total"), , drop = FALSE]
    if (!nrow(rows)) return(data.frame())
    keep <- !duplicated(paste(rows$Effect, rows$Outcome, rows$Predictor, sep = "\r"))
    rows <- rows[keep, , drop = FALSE]
    result <- data.frame(
      Outcome = rows$Outcome,
      Predictor = rows$Predictor,
      Effect = rows$Effect,
      B = rows$B,
      SE = rows$SE,
      `B 95% CI` = mapply(structural_canvas_effect_ci_text, rows[["B 95% CI lower"]], rows[["B 95% CI upper"]], USE.NAMES = FALSE),
      beta = rows$beta,
      `beta p` = rows[["beta p"]],
      `beta 95% CI` = mapply(structural_canvas_effect_ci_text, rows[["beta 95% CI lower"]], rows[["beta 95% CI upper"]], USE.NAMES = FALSE),
      `beta CI source` = rows[["beta CI source"]],
      p = rows$p,
      `BH-adjusted p` = rows[["BH-adjusted p"]],
      `CI source` = rows[["B CI source"]],
      `Inference source` = rows[["Inference source"]],
      `Valid bootstrap` = rows[["Valid bootstrap"]],
      `Bootstrap status` = rows[["Bootstrap status"]],
      `BH family` = rows[["BH family"]],
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    if (structural_canvas_table_uses_bootstrap_inference(rows)) names(result)[names(result) == "SE"] <- "Boot SE"
    return(result)
  }
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
  result <- data.frame(
    Path = path,
    B = rows$B,
    SE = rows$SE,
    `B 95% CI lower` = rows[["B 95% CI lower"]],
    `B 95% CI upper` = rows[["B 95% CI upper"]],
    beta = rows$beta,
    z = rows$z,
    p = rows$p,
    `BH-adjusted p` = rows[["BH-adjusted p"]],
    `CI source` = rows[["B CI source"]],
    `Inference source` = rows[["Inference source"]],
    `Valid bootstrap` = rows[["Valid bootstrap"]],
    `Bootstrap status` = rows[["Bootstrap status"]],
    `BH family` = rows[["BH family"]],
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  if (structural_canvas_table_uses_bootstrap_inference(rows)) {
    names(result)[names(result) == "SE"] <- "Boot SE"
    names(result)[names(result) == "B 95% CI lower"] <- "Boot 95% CI lower"
    names(result)[names(result) == "B 95% CI upper"] <- "Boot 95% CI upper"
  }
  result
}

structural_canvas_pls_moderation_result_table <- function(
  bundle, kind, display_name = identity, format_values = TRUE
) {
  diagnostics <- bundle$diagnostics %||% list()
  bootstrap <- bundle$pls_bootstrap_result %||% list()
  modmed <- bundle$pls_modmed_result %||% list()
  first_table <- function(...) {
    candidates <- list(...)
    for (candidate in candidates) {
      candidate <- as.data.frame(candidate %||% data.frame(), check.names = FALSE, stringsAsFactors = FALSE)
      if (nrow(candidate)) return(candidate)
    }
    data.frame()
  }
  value <- switch(
    kind,
    pls_moderation = first_table(
      bootstrap$bootstrapped_moderation_effects,
      diagnostics$moderation_effects,
      bundle$fit$statedu_moderation_effects
    ),
    pls_simple_slopes = first_table(
      bootstrap$bootstrapped_moderation_simple_slopes,
      diagnostics$moderation_simple_slopes,
      bundle$fit$statedu_moderation_simple_slopes
    ),
    pls_moderated_mediation = modmed$moderated_mediation,
    pls_conditional_indirect = modmed$conditional_indirect,
    NULL
  )
  value <- as.data.frame(value %||% data.frame(), check.names = FALSE, stringsAsFactors = FALSE)
  if (!nrow(value)) return(value)
  if (identical(kind, "pls_moderation")) {
    adjusted <- as.data.frame(modmed$interaction_effects %||% data.frame(), check.names = FALSE)
    key_columns <- c("Predictor", "Moderator", "Outcome")
    if (nrow(adjusted) && all(c(key_columns, "BH-adjusted p") %in% names(adjusted)) &&
        all(key_columns %in% names(value))) {
      key <- do.call(paste, c(lapply(value[key_columns], as.character), sep = "\r"))
      adjusted_key <- do.call(paste, c(lapply(adjusted[key_columns], as.character), sep = "\r"))
      matched <- match(key, adjusted_key)
      if (!"BH-adjusted p" %in% names(value)) value[["BH-adjusted p"]] <- NA_real_
      replace <- which(!is.na(matched))
      if (length(replace)) {
        value[["BH-adjusted p"]][replace] <- suppressWarnings(as.numeric(
          adjusted[["BH-adjusted p"]][matched[replace]]
        ))
      }
    }
  }
  value <- structural_canvas_display_identifier_table(value, display_name)
  for (column in intersect(c("Downstream Path", "Indirect path", "Moderated path"), names(value))) {
    path <- as.character(value[[column]])
    present <- !is.na(path) & nzchar(trimws(path))
    path[present] <- structural_canvas_display_path(path[present], display_name)
    value[[column]] <- path
  }
  if (all(c("Predictor", "Moderator") %in% names(value))) {
    interaction_label <- paste(value$Predictor, "x", value$Moderator)
    for (column in intersect(c("Interaction", "Interaction Factor"), names(value))) {
      value[[column]] <- interaction_label
    }
  }
  if (isTRUE(format_values)) {
    probability_columns <- intersect(
      c("p", "Bootstrap P Val", "BH-adjusted p", "Holm-adjusted p"), names(value)
    )
    numeric_columns <- intersect(c(
      "Estimate", "Predictor main effect", "Moderator main effect", "Moderator value",
      "Direct effect", "Interaction effect", "Simple slope", "Bootstrap mean",
      "Bootstrap Mean", "Bootstrap Mean Difference", "Bootstrap SE", "95% CI lower",
      "95% CI upper", "2.5% CI", "97.5% CI", "Valid ratio", "Valid Ratio"
    ), names(value))
    for (column in numeric_columns) {
      value[[column]] <- vapply(suppressWarnings(as.numeric(value[[column]])), format_decimal3, character(1))
    }
    for (column in probability_columns) {
      value[[column]] <- vapply(suppressWarnings(as.numeric(value[[column]])), format_p, character(1))
    }
  }
  preferred <- switch(
    kind,
    pls_moderation = c(
      "Predictor", "Moderator", "Outcome", "Interaction", "Method", "Estimate",
      "Predictor main effect", "Moderator main effect",
      "Moderator main effect auto-added",
      "Bootstrap mean", "Bootstrap SE", "95% CI lower", "95% CI upper", "p",
      "BH-adjusted p",
      "Valid replicates", "Requested replicates", "Valid ratio", "Inference available",
      "Bootstrap Status", "Inference Source",
      "PLSc interaction correction"
    ),
    pls_simple_slopes = c(
      "Predictor", "Moderator", "Outcome", "Moderator level", "Moderator value",
      "Direct effect", "Interaction effect", "Simple slope", "Bootstrap mean",
      "Bootstrap SE", "95% CI lower", "95% CI upper", "p", "BH-adjusted p",
      "Valid replicates",
      "Requested replicates", "Valid ratio", "Inference available",
      "Bootstrap Status", "Inference Source",
      "PLSc interaction correction"
    ),
    pls_moderated_mediation = c(
      "Path", "Predictor", "Moderator", "Outcome", "Downstream Path", "Estimate",
      "Bootstrap Mean", "Bootstrap SE", "2.5% CI", "97.5% CI", "Bootstrap P Val",
      "BH-adjusted p", "Bootstrap Status", "Inference Source", "Valid N",
      "Requested N", "Valid Ratio"
    ),
    pls_conditional_indirect = c(
      "Path", "Predictor", "Moderator", "Outcome", "Downstream Path",
      "Moderator Level", "Moderator Position", "Estimate", "Bootstrap Mean",
      "Bootstrap SE", "2.5% CI", "97.5% CI", "Bootstrap P Val",
      "BH-adjusted p", "Bootstrap Status", "Inference Source", "Valid N",
      "Requested N", "Valid Ratio"
    ),
    names(value)
  )
  value[, intersect(preferred, names(value)), drop = FALSE]
}

structural_canvas_result_table <- function(kind, fit_result, analysis_type, labels_fn, app_language_fn = NULL,
                                           variable_table_fn = function() NULL) {
  bundle <- fit_result()
  shiny::req(!is.null(bundle))
  fit <- bundle$fit
  snapshot <- bundle$snapshot %||% list()
  labels <- labels_fn() %||% character(0)
  variable_table <- if (is.function(variable_table_fn)) variable_table_fn() else variable_table_fn
  ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
  moderation_definitions <- bundle$diagnostics$moderation_definitions %||% bundle$moderation_definitions %||% list()
  display_name <- structural_canvas_display_name_resolver(
    snapshot = snapshot,
    variable_table = variable_table,
    labels = labels,
    moderation_definitions = moderation_definitions,
    language = statedu_current_language(app_language_fn)
  )
  structural_construct_order <- structural_canvas_structural_construct_order(snapshot, display_name)
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
        # The measurement table is already display-mapped at this point.  Keep
        # the existing rule that internal product-indicator constructs are not
        # reported, but compare against their mapped names as well as the raw
        # keys so label-first output cannot accidentally bypass the filter.
        moderation_display_factors <- as.character(display_name(moderation_factors))
        measurement_table <- measurement_table[
          !measurement_table$Latent %in% unique(c(moderation_factors, moderation_display_factors)),
          , drop = FALSE
        ]
      }
      return(measurement_table)
    }
    bootstrap_state <- structural_canvas_effect_bootstrap_bundle_state(bundle)
    structural_table <- structural_canvas_lavaan_structural_result_table(
      kind, fit, ko, fmt, display_name,
      bootstrap = bundle$effect_bootstrap_result %||% NULL,
      bootstrap_state = bootstrap_state
    )
    if (!is.null(structural_table)) {
      if (kind %in% c("structural", "structural_ci", "structural_effects", "structural_effect_ci", "structural_specific_indirect") && ncol(structural_table) >= 3L) {
        names(structural_table)[1:3] <- c("Effect", "Outcome", "Predictor")
        effect_rows <- structural_canvas_lavaan_structural_effect_rows(
          fit, bundle$diagnostics$effect_definitions %||% bundle$effect_definitions %||% list(), fmt, display_name,
          bundle$effect_bootstrap_result %||% NULL, bootstrap_state = bootstrap_state
        )
        if (nrow(effect_rows)) {
          names(effect_rows) <- names(structural_table)
          structural_table <- rbind(structural_table, effect_rows)
        }
        structural_table <- structural_canvas_apply_effect_bh_families(structural_table)
        structural_table <- structural_canvas_order_structural_paths(structural_table, structural_construct_order)
      }
      if (identical(kind, "structural")) {
        direct <- structural_table[structural_table$Effect == "Direct", , drop = FALSE]
        direct[["B 95% CI"]] <- mapply(structural_canvas_effect_ci_text, direct[["B 95% CI lower"]], direct[["B 95% CI upper"]], USE.NAMES = FALSE)
        result <- direct[, c(
          "Outcome", "Predictor", "B", "SE", "B 95% CI", "beta", "z", "p", "BH-adjusted p", "R²",
          "Inference source", "B CI source", "Valid bootstrap", "Bootstrap status", "BH family"
        ), drop = FALSE]
        if (structural_canvas_table_uses_bootstrap_inference(direct)) names(result)[names(result) == "SE"] <- "Boot SE"
        return(result)
      }
      if (identical(kind, "structural_ci")) {
        direct <- structural_table[structural_table$Effect == "Direct", , drop = FALSE]
        return(direct[, c(
          "Outcome", "Predictor",
          "B 95% CI lower", "B 95% CI upper",
          "B CI source", "beta 95% CI lower", "beta 95% CI upper", "beta CI source",
          "Valid bootstrap", "Bootstrap status"
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
  summary_fit <- structural_canvas_pls_summary(fit)
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
  if (kind %in% c(
    "fit", "fit_guide", "fit_bootstrap",
    "pls_direct_effects", "pls_specific_indirect", "pls_total_indirect", "pls_total_effect",
    "structural_effects", "structural_effect_ci", "structural_specific_indirect"
  )) {
    f_square_result <- structural_canvas_pls_f_square_for_reporting(bundle, summary_fit)
    full_fit <- structural_canvas_pls_fit_result_table(
      summary_fit, bundle$diagnostics %||% list(), display_name,
      bundle$pls_bootstrap_result %||% NULL, f_square_result,
      construct_order = structural_construct_order
    )
    if (identical(kind, "fit")) return(structural_canvas_pls_fit_main_table(full_fit))
    if (identical(kind, "fit_guide")) return(structural_canvas_pls_fit_guide_table(full_fit))
    if (identical(kind, "fit_bootstrap")) return(structural_canvas_pls_fit_bootstrap_table(full_fit))
    if (kind %in% c("pls_direct_effects")) return(structural_canvas_pls_effect_table(full_fit, "Direct"))
    if (kind %in% c("pls_specific_indirect", "structural_specific_indirect")) return(structural_canvas_pls_effect_table(full_fit, "Specific indirect"))
    if (identical(kind, "pls_total_indirect")) return(structural_canvas_pls_effect_table(full_fit, "Total indirect"))
    if (identical(kind, "pls_total_effect")) return(structural_canvas_pls_effect_table(full_fit, "Total"))
    # Preserve the common result-table dispatch used by report/export code.
    # PLS structural_effects now means the complete one-estimand effect table;
    # structural_effect_ci uses the same rows because point estimates remain
    # available even when bootstrap inference was not requested.
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
  if (kind %in% c(
    "pls_moderation", "pls_simple_slopes",
    "pls_moderated_mediation", "pls_conditional_indirect"
  )) {
    return(structural_canvas_pls_moderation_result_table(bundle, kind, display_name))
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
