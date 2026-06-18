custom_truthy_pm <- function(x) {
  if (is.logical(x)) return(isTRUE(x[1]))
  if (is.numeric(x)) return(!is.na(x[1]) && x[1] != 0)
  x_chr <- tolower(trimws(as.character(x[1] %||% "")))
  nzchar(x_chr) && x_chr %in% c("1", "true", "t", "yes", "y", "on")
}

parse_custom_matrix_pm <- function(mat_cfg, row_vars, col_vars) {
  out <- list()
  idx <- 1L
  if (!is.list(mat_cfg) || length(mat_cfg) == 0L) {
    return(data.frame(target_var = character(0), predictor_var = character(0), stringsAsFactors = FALSE))
  }

  row_names <- names(mat_cfg)
  if (is.null(row_names)) row_names <- rep("", length(mat_cfg))

  for (i in seq_along(mat_cfg)) {
    target_i <- trimws(as.character(row_names[i] %||% ""))
    if (!nzchar(target_i)) next
    row_i <- mat_cfg[[i]]
    if (is.null(row_i) || length(row_i) == 0L) next

    if (is.atomic(row_i) && is.null(names(row_i))) {
      preds_i <- trimws(as.character(row_i))
      preds_i <- preds_i[!is.na(preds_i) & nzchar(preds_i)]
      for (pred_i in preds_i) {
        out[[idx]] <- data.frame(
          target_var = target_i,
          predictor_var = pred_i,
          stringsAsFactors = FALSE
        )
        idx <- idx + 1L
      }
      next
    }

    pred_names <- names(row_i)
    if (is.null(pred_names) || length(pred_names) == 0L) {
      preds_i <- trimws(as.character(unlist(row_i, use.names = FALSE)))
      preds_i <- preds_i[!is.na(preds_i) & nzchar(preds_i)]
      for (pred_i in preds_i) {
        out[[idx]] <- data.frame(
          target_var = target_i,
          predictor_var = pred_i,
          stringsAsFactors = FALSE
        )
        idx <- idx + 1L
      }
      next
    }

    for (j in seq_along(row_i)) {
      pred_j <- trimws(as.character(pred_names[j] %||% ""))
      if (!nzchar(pred_j) || !custom_truthy_pm(row_i[[j]])) next
      out[[idx]] <- data.frame(
        target_var = target_i,
        predictor_var = pred_j,
        stringsAsFactors = FALSE
      )
      idx <- idx + 1L
    }
  }

  out_df <- if (length(out) > 0L) do.call(rbind, out) else data.frame(
    target_var = character(0),
    predictor_var = character(0),
    stringsAsFactors = FALSE
  )
  if (!nrow(out_df)) return(out_df)

  out_df <- unique(out_df)
  out_df <- out_df[
    as.character(out_df$target_var) %in% row_vars &
      as.character(out_df$predictor_var) %in% col_vars,
    ,
    drop = FALSE
  ]
  rownames(out_df) <- NULL
  out_df
}

enumerate_custom_paths_pm <- function(edges_df, x_var, y_var) {
  if (!is.data.frame(edges_df) || nrow(edges_df) == 0L) return(list())

  adjacency <- split(as.character(edges_df$target_var), as.character(edges_df$predictor_var))
  out <- list()
  idx <- 1L

  walk <- function(node, path_nodes) {
    next_nodes <- unique(as.character(adjacency[[node]] %||% character(0)))
    if (!length(next_nodes)) return(invisible(NULL))
    for (next_i in next_nodes) {
      if (next_i %in% path_nodes) next
      path_next <- c(path_nodes, next_i)
      if (identical(next_i, y_var)) {
        out[[idx]] <<- path_next
        idx <<- idx + 1L
      } else {
        walk(next_i, path_next)
      }
    }
    invisible(NULL)
  }

  walk(x_var, x_var)
  out
}

build_custom_model_spec_pm <- function(process_settings, outcome_var, x_var) {
  custom_cfg <- process_settings$custom_model %||% list()
  mediator_vars <- unique(as.character(process_settings$m_vars %||% character(0)))
  mediator_vars <- mediator_vars[!is.na(mediator_vars) & nzchar(mediator_vars)]
  w_var <- as.character(process_settings$w_var %||% "")[1]

  if (length(unique(process_settings$x_vars %||% character(0))) != 1L) {
    stop("Custom process model currently requires exactly one X variable.", call. = FALSE)
  }
  if (length(unique(process_settings$y_vars %||% character(0))) != 1L) {
    stop("Custom process model currently requires exactly one Y variable.", call. = FALSE)
  }
  if (!length(mediator_vars)) {
    stop("Custom process model currently requires at least one mediator in process.m.", call. = FALSE)
  }
  if (identical(process_settings$moderator_source %||% "observed", "latent_class")) {
    stop("Custom process model currently supports observed W only.", call. = FALSE)
  }

  target_vars <- c(mediator_vars, outcome_var)
  predictor_vars <- c(x_var, mediator_vars)

  b_df <- parse_custom_matrix_pm(custom_cfg$b_matrix %||% list(), row_vars = target_vars, col_vars = predictor_vars)
  w_df <- parse_custom_matrix_pm(custom_cfg$w_matrix %||% list(), row_vars = target_vars, col_vars = predictor_vars)

  if (!nrow(b_df)) {
    stop("Custom process model requires at least one path in custom_model.b_matrix.", call. = FALSE)
  }
  if (nrow(w_df) > 0L && (!nzchar(w_var) || is.na(w_var))) {
    stop("Custom process model uses w_matrix but process.w is empty.", call. = FALSE)
  }
  if (nrow(w_df) > 0L) {
    key_b <- paste(b_df$target_var, b_df$predictor_var, sep = "||")
    key_w <- paste(w_df$target_var, w_df$predictor_var, sep = "||")
    if (!all(key_w %in% key_b)) {
      stop("Every custom_model.w_matrix path must also exist in custom_model.b_matrix.", call. = FALSE)
    }
  }

  node_map <- data.frame(
    var = c(x_var, mediator_vars, outcome_var),
    label = c(lookup_label(x_var), vapply(mediator_vars, lookup_label, character(1)), lookup_label(outcome_var)),
    export_name = c("x_pm", sprintf("m%02d_pm", seq_along(mediator_vars)), "y_pm"),
    role = c("x", rep("m", length(mediator_vars)), "y"),
    block_order = c(0L, seq_along(mediator_vars), length(mediator_vars) + 1L),
    stringsAsFactors = FALSE
  )

  predictor_order_map <- stats::setNames(seq_len(nrow(node_map)), node_map$var)
  target_order_map <- stats::setNames(node_map$block_order, node_map$var)

  b_df$edge_id <- seq_len(nrow(b_df))
  b_df$base_label <- sprintf("p%02d", seq_len(nrow(b_df)))
  b_df$predictor_label <- vapply(as.character(b_df$predictor_var), lookup_label, character(1))
  b_df$target_label <- vapply(as.character(b_df$target_var), lookup_label, character(1))
  b_df$predictor_export <- node_map$export_name[match(b_df$predictor_var, node_map$var)]
  b_df$target_export <- node_map$export_name[match(b_df$target_var, node_map$var)]
  b_df$sort_key <- unname(predictor_order_map[as.character(b_df$predictor_var)])
  b_df$block_order <- unname(target_order_map[as.character(b_df$target_var)])
  b_df$model_component <- ifelse(as.character(b_df$target_var) == outcome_var, "Outcome model", "Mediator model")

  w_df$int_label <- sprintf("q%02d", seq_len(nrow(w_df)))
  w_df$wmain_label <- sprintf("u%02d", unname(target_order_map[as.character(w_df$target_var)]))
  w_df$int_export <- sprintf("z%02d_pm", seq_len(nrow(w_df)))
  w_df$key <- paste(w_df$target_var, w_df$predictor_var, sep = "||")
  b_df$key <- paste(b_df$target_var, b_df$predictor_var, sep = "||")

  if (nrow(w_df) > 0L) {
    b_df$is_moderated <- b_df$key %in% w_df$key
    b_df$int_label <- w_df$int_label[match(b_df$key, w_df$key)]
    b_df$wmain_label <- w_df$wmain_label[match(b_df$key, w_df$key)]
    b_df$int_export <- w_df$int_export[match(b_df$key, w_df$key)]
  } else {
    b_df$is_moderated <- FALSE
    b_df$int_label <- NA_character_
    b_df$wmain_label <- NA_character_
    b_df$int_export <- NA_character_
  }

  path_nodes <- enumerate_custom_paths_pm(b_df, x_var = x_var, y_var = outcome_var)
  path_nodes <- Filter(function(x) length(x) > 2L, path_nodes)
  if (!length(path_nodes)) {
    stop("Custom process model requires at least one indirect path from X to Y through mediator(s).", call. = FALSE)
  }

  path_rows <- list()
  pidx <- 1L
  for (i in seq_along(path_nodes)) {
    nodes_i <- path_nodes[[i]]
    mediators_i <- nodes_i[seq.int(2L, length(nodes_i) - 1L)]
    path_keys_i <- paste(nodes_i[-1L], nodes_i[-length(nodes_i)], sep = "||")
    n_moderated_i <- sum(path_keys_i %in% as.character(w_df$key))
    path_rows[[pidx]] <- data.frame(
      path_id = i,
      mediator_vars = paste(mediators_i, collapse = " -> "),
      mediator_labels = paste(vapply(mediators_i, lookup_label, character(1)), collapse = " -> "),
      low_param = sprintf("il%02d", i),
      mean_param = sprintf("im%02d", i),
      high_param = sprintf("ih%02d", i),
      single_param = sprintf("ie%02d", i),
      imm_param = if (n_moderated_i == 1L) sprintf("imm%02d", i) else NA_character_,
      n_edges = length(nodes_i) - 1L,
      n_moderated_edges = n_moderated_i,
      stringsAsFactors = FALSE
    )
    pidx <- pidx + 1L
  }
  path_df <- do.call(rbind, path_rows)

  list(
    label = as.character(process_settings$custom_model_label %||% "Custom")[1],
    x_var = x_var,
    y_var = outcome_var,
    mediator_vars = mediator_vars,
    w_var = w_var,
    node_map = node_map,
    edges = b_df,
    w_edges = w_df,
    has_moderation = any(b_df$is_moderated),
    path_nodes = path_nodes,
    path_df = path_df
  )
}

build_observed_custom_mplus_data_pm <- function(df, outcome_var, x_var, mediator_vars, w_var, covariates, survey_bundle, process_settings, custom_spec) {
  needed <- unique(c(
    outcome_var,
    x_var,
    mediator_vars,
    if (isTRUE(custom_spec$has_moderation)) w_var else character(0),
    covariates,
    survey_bundle$weight_var,
    survey_bundle$strata_var,
    survey_bundle$cluster_var,
    survey_bundle$subset_var,
    process_settings$id_var
  ))
  needed <- needed[!is.na(needed) & nzchar(needed) & needed %in% names(df)]
  dat <- df[, needed, drop = FALSE]
  if (!all(c(outcome_var, x_var, mediator_vars) %in% names(dat))) return(NULL)
  if (isTRUE(custom_spec$has_moderation) && !(w_var %in% names(dat))) return(NULL)

  dat[[outcome_var]] <- safe_num_pm(dat[[outcome_var]])
  dat[[x_var]] <- safe_num_pm(dat[[x_var]])
  for (mv in mediator_vars) dat[[mv]] <- safe_num_pm(dat[[mv]])
  if (isTRUE(custom_spec$has_moderation)) dat[[w_var]] <- safe_num_pm(dat[[w_var]])

  cov_keep <- covariates[covariates %in% names(dat)]
  cov_prep <- prepare_covariates(dat, cov_keep)
  dat <- cov_prep$data
  cov_term_map <- cov_prep$term_map
  cov_rhs <- cov_prep$rhs_terms

  subset_idx <- make_subset_index_pm(dat, survey_bundle)
  if (length(subset_idx) == 0) subset_idx <- rep(TRUE, nrow(dat))

  design_terms <- c(survey_bundle$weight_var, survey_bundle$strata_var, survey_bundle$cluster_var)
  design_terms <- design_terms[!is.na(design_terms) & nzchar(design_terms) & design_terms %in% names(dat)]
  model_terms <- c(outcome_var, x_var, mediator_vars, if (isTRUE(custom_spec$has_moderation)) w_var else character(0), cov_rhs)
  model_terms <- model_terms[model_terms %in% names(dat)]
  cc_idx <- stats::complete.cases(dat[, unique(c(model_terms, design_terms)), drop = FALSE])

  if (!is.null(survey_bundle$weight_var) && survey_bundle$weight_var %in% names(dat)) {
    ww <- safe_num_pm(dat[[survey_bundle$weight_var]])
    cc_idx <- cc_idx & !is.na(ww) & ww > 0
  }

  analysis_idx <- cc_idx & subset_idx
  if (!any(analysis_idx)) return(NULL)

  weight_vec <- if (!is.null(survey_bundle$weight_var) && survey_bundle$weight_var %in% names(dat)) {
    safe_num_pm(dat[[survey_bundle$weight_var]])
  } else {
    rep(1, nrow(dat))
  }

  center_x <- if (identical(process_settings$centering_method %||% "weighted_mean", "none")) {
    0
  } else if (identical(process_settings$centering_method %||% "weighted_mean", "mean")) {
    mean(dat[[x_var]][analysis_idx], na.rm = TRUE)
  } else {
    weighted_mean_pm(dat[[x_var]][analysis_idx], weight_vec[analysis_idx])
  }

  center_w <- 0
  w_sd <- NA_real_
  low_raw <- NA_real_
  mean_raw <- NA_real_
  high_raw <- NA_real_
  low_centered <- NA_real_
  mean_centered <- NA_real_
  high_centered <- NA_real_
  if (isTRUE(custom_spec$has_moderation)) {
    center_w <- if (identical(process_settings$centering_method %||% "weighted_mean", "none")) {
      0
    } else if (identical(process_settings$centering_method %||% "weighted_mean", "mean")) {
      mean(dat[[w_var]][analysis_idx], na.rm = TRUE)
    } else {
      weighted_mean_pm(dat[[w_var]][analysis_idx], weight_vec[analysis_idx])
    }
    w_sd <- if (identical(process_settings$centering_method %||% "weighted_mean", "mean")) {
      stats::sd(dat[[w_var]][analysis_idx], na.rm = TRUE)
    } else {
      weighted_sd_pm(dat[[w_var]][analysis_idx], weight_vec[analysis_idx])
    }
    if (is.na(w_sd) || w_sd <= 0) w_sd <- stats::sd(dat[[w_var]][analysis_idx], na.rm = TRUE)
    if (is.na(w_sd) || w_sd <= 0) w_sd <- 1
    probe_mult <- suppressWarnings(as.numeric(process_settings$probe_sd_multiplier %||% 1))
    if (is.na(probe_mult) || probe_mult <= 0) probe_mult <- 1
    low_raw <- center_w - probe_mult * w_sd
    mean_raw <- center_w
    high_raw <- center_w + probe_mult * w_sd
    low_centered <- low_raw - center_w
    mean_centered <- mean_raw - center_w
    high_centered <- high_raw - center_w
  }

  cov_export_names <- if (length(cov_rhs) > 0) sprintf("c%02d", seq_along(cov_rhs)) else character(0)
  for (i in seq_along(cov_rhs)) dat[[cov_export_names[i]]] <- safe_num_pm(dat[[cov_rhs[i]]])
  if (length(cov_export_names) > 0 && nrow(cov_term_map) > 0) {
    cov_term_map$export_name <- cov_export_names[match(cov_term_map$term, cov_rhs)]
  } else {
    cov_term_map$export_name <- character(0)
  }
  if (length(cov_export_names) > 0) {
    keep_cov <- vapply(cov_export_names, function(nm) {
      if (!nm %in% names(dat)) return(FALSE)
      x <- safe_num_pm(dat[[nm]])
      ux <- unique(stats::na.omit(x))
      length(ux) > 1L
    }, logical(1))
    cov_export_names <- cov_export_names[keep_cov]
    if (nrow(cov_term_map) > 0) {
      cov_term_map <- cov_term_map[as.character(cov_term_map$export_name) %in% cov_export_names, , drop = FALSE]
    }
  }

  dat$x_pm <- dat[[x_var]] - center_x
  for (i in seq_len(nrow(custom_spec$node_map))) {
    node_i <- custom_spec$node_map[i, , drop = FALSE]
    if (identical(node_i$role[1], "m")) dat[[node_i$export_name[1]]] <- safe_num_pm(dat[[node_i$var[1]]])
  }
  dat$y_pm <- dat[[outcome_var]]

  interaction_exports <- character(0)
  if (isTRUE(custom_spec$has_moderation)) {
    dat$w_pm <- dat[[w_var]] - center_w
    for (i in seq_len(nrow(custom_spec$w_edges))) {
      edge_i <- custom_spec$w_edges[i, , drop = FALSE]
      pred_export_i <- custom_spec$edges$predictor_export[match(edge_i$key[1], custom_spec$edges$key)]
      dat[[edge_i$int_export[1]]] <- safe_num_pm(dat[[pred_export_i]]) * dat$w_pm
      interaction_exports <- c(interaction_exports, edge_i$int_export[1])
    }
  }

  has_subpop_spec <- nzchar(as.character(survey_bundle$subset_var %||% "")[1]) ||
    nzchar(as.character(survey_bundle$subset_expr %||% "")[1])
  use_subpopulation <- isTRUE(process_settings$use_subpopulation_in_mplus) && isTRUE(has_subpop_spec)
  if (use_subpopulation) {
    dat$sb_pm <- as.integer(analysis_idx)
  } else {
    dat <- dat[analysis_idx, , drop = FALSE]
    analysis_idx <- rep(TRUE, nrow(dat))
  }

  design_map <- list()
  if (!is.null(survey_bundle$weight_var) && survey_bundle$weight_var %in% names(dat)) {
    dat$wt_pm <- safe_num_pm(dat[[survey_bundle$weight_var]])
    design_map$weight_var <- "wt_pm"
  } else {
    design_map$weight_var <- NULL
  }
  if (!is.null(survey_bundle$strata_var) && survey_bundle$strata_var %in% names(dat)) {
    dat$st_pm <- safe_num_pm(dat[[survey_bundle$strata_var]])
    design_map$strata_var <- "st_pm"
  } else {
    design_map$strata_var <- NULL
  }
  if (!is.null(survey_bundle$cluster_var) && survey_bundle$cluster_var %in% names(dat)) {
    dat$cl_pm <- safe_num_pm(dat[[survey_bundle$cluster_var]])
    design_map$cluster_var <- "cl_pm"
  } else {
    design_map$cluster_var <- NULL
  }

  export_names <- c(
    if (use_subpopulation) "sb_pm" else character(0),
    design_map$weight_var %||% character(0),
    design_map$strata_var %||% character(0),
    design_map$cluster_var %||% character(0),
    "x_pm",
    if (isTRUE(custom_spec$has_moderation)) "w_pm" else character(0),
    custom_spec$node_map$export_name[custom_spec$node_map$role == "m"],
    "y_pm",
    interaction_exports,
    cov_export_names
  )
  export_names <- export_names[!is.na(export_names) & nzchar(export_names)]

  list(
    export_data = dat[, export_names, drop = FALSE],
    cov_term_map = cov_term_map,
    cov_export_names = cov_export_names,
    use_subpopulation = use_subpopulation,
    design_map = design_map,
    center_x = center_x,
    center_w = center_w,
    low_raw = low_raw,
    mean_raw = mean_raw,
    high_raw = high_raw,
    low_centered = low_centered,
    mean_centered = mean_centered,
    high_centered = high_centered,
    analysis_n = sum(analysis_idx, na.rm = TRUE),
    custom_spec = custom_spec
  )
}

build_custom_path_expr_pm <- function(path_nodes, custom_spec, w_term = NULL) {
  edges <- custom_spec$edges
  bits <- character(0)
  for (i in seq_len(length(path_nodes) - 1L)) {
    key_i <- paste(path_nodes[i + 1L], path_nodes[i], sep = "||")
    edge_i <- edges[as.character(edges$key) == key_i, , drop = FALSE]
    if (!nrow(edge_i)) next
    if (isTRUE(edge_i$is_moderated[1]) && !is.null(w_term)) {
      bits <- c(bits, paste0("(", edge_i$base_label[1], " + ", edge_i$int_label[1], "*", w_term, ")"))
    } else {
      bits <- c(bits, edge_i$base_label[1])
    }
  }
  paste(bits, collapse = "*")
}

build_custom_imm_expr_pm <- function(path_nodes, custom_spec) {
  edges <- custom_spec$edges
  bits <- character(0)
  n_moderated <- 0L

  for (i in seq_len(length(path_nodes) - 1L)) {
    key_i <- paste(path_nodes[i + 1L], path_nodes[i], sep = "||")
    edge_i <- edges[as.character(edges$key) == key_i, , drop = FALSE]
    if (!nrow(edge_i)) next
    if (isTRUE(edge_i$is_moderated[1])) {
      n_moderated <- n_moderated + 1L
      bits <- c(bits, edge_i$int_label[1])
    } else {
      bits <- c(bits, edge_i$base_label[1])
    }
  }

  if (n_moderated != 1L || !length(bits)) return(NULL)
  paste(bits, collapse = "*")
}

build_observed_custom_model_input_pm <- function(prep_obj, model_tag, outcome_var, x_var, w_var) {
  custom_spec <- prep_obj$custom_spec
  cfg_local <- if (exists("CFG", inherits = TRUE)) get("CFG", inherits = TRUE) else list()
  wrap_new_statement_local_pm <- function(values, indent = "  ", width = 78) {
    values <- as.character(values)
    values <- values[!is.na(values) & nzchar(values)]
    if (length(values) == 0) return(character(0))
    prefix <- paste0(indent, "NEW(")
    cont_prefix <- paste0(indent, "  ")
    out <- character(0)
    current <- prefix
    for (val in values) {
      candidate <- if (identical(current, prefix)) paste0(current, val) else paste(current, val)
      if (nchar(candidate, type = "width") > width) {
        out <- c(out, current)
        current <- paste0(cont_prefix, val)
      } else {
        current <- candidate
      }
    }
    c(out, paste0(current, ");"))
  }

  paths <- list(
    data_file = file.path(DIR_MPLUS_DATA, paste0(model_tag, ".dat")),
    inp_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".inp")),
    out_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".out"))
  )

  write_mplus_data(
    data = prep_obj$export_data,
    path = paths$data_file,
    missing_code = cfg_local$mplus_missing_code %||% cfg_local$missing_code %||% -9999
  )

  usevars_vec <- setdiff(names(prep_obj$export_data), "sb_pm")
  variable_lines <- c(
    "VARIABLE:",
    wrap_mplus_statement_pm("NAMES", names(prep_obj$export_data)),
    wrap_mplus_statement_pm("USEVARIABLES", usevars_vec)
  )
  if (prep_obj$use_subpopulation) variable_lines <- c(variable_lines, "  SUBPOPULATION = sb_pm EQ 1;")
  if (!is.null(prep_obj$design_map$weight_var)) variable_lines <- c(variable_lines, paste0("  WEIGHT = ", prep_obj$design_map$weight_var, ";"))
  if (!is.null(prep_obj$design_map$strata_var)) variable_lines <- c(variable_lines, paste0("  STRATIFICATION = ", prep_obj$design_map$strata_var, ";"))
  if (!is.null(prep_obj$design_map$cluster_var)) variable_lines <- c(variable_lines, paste0("  CLUSTER = ", prep_obj$design_map$cluster_var, ";"))

  type_line <- if (!is.null(prep_obj$design_map$weight_var) || !is.null(prep_obj$design_map$strata_var) || !is.null(prep_obj$design_map$cluster_var)) {
    "  TYPE = COMPLEX;"
  } else {
    "  TYPE = GENERAL;"
  }

  eq_map <- unique(custom_spec$edges[, c("target_var", "target_export", "block_order", "model_component"), drop = FALSE])
  eq_map <- eq_map[order(eq_map$block_order), , drop = FALSE]

  model_lines <- c("MODEL:")
  for (i in seq_len(nrow(eq_map))) {
    eq_i <- eq_map[i, , drop = FALSE]
    edge_i <- custom_spec$edges[as.character(custom_spec$edges$target_var) == as.character(eq_i$target_var[1]), , drop = FALSE]
    edge_i <- edge_i[order(edge_i$sort_key), , drop = FALSE]
    rhs_i <- character(0)
    if (nrow(edge_i) > 0) {
      rhs_i <- c(rhs_i, paste0(edge_i$predictor_export, " (", edge_i$base_label, ")"))
      if (any(edge_i$is_moderated)) {
        rhs_i <- c(rhs_i, paste0("w_pm (", edge_i$wmain_label[match(TRUE, edge_i$is_moderated)], ")"))
        mod_i <- edge_i[edge_i$is_moderated, , drop = FALSE]
        rhs_i <- c(rhs_i, paste0(mod_i$int_export, " (", mod_i$int_label, ")"))
      }
    }
    if (length(prep_obj$cov_export_names) > 0) rhs_i <- c(rhs_i, prep_obj$cov_export_names)
    model_lines <- c(
      model_lines,
      paste0("  ", eq_i$target_export[1], " ON"),
      paste0("    ", rhs_i)
    )
    model_lines[length(model_lines)] <- paste0(model_lines[length(model_lines)], ";")
    model_lines <- c(model_lines, paste0("  [", eq_i$target_export[1], "] (i", sprintf("%02d", eq_i$block_order[1]), ");"))
  }

  constraint_lines <- character(0)
  path_df <- custom_spec$path_df
  if (isTRUE(custom_spec$has_moderation)) {
    imm_names <- as.character(path_df$imm_param)
    imm_names <- imm_names[!is.na(imm_names) & nzchar(imm_names)]
    new_names <- c(
      "w_low", "w_mean", "w_high",
      as.vector(t(path_df[, c("low_param", "mean_param", "high_param"), drop = FALSE])),
      imm_names
    )
    constraint_lines <- c(
      "MODEL CONSTRAINT:",
      wrap_new_statement_local_pm(new_names),
      paste0("  w_low = ", sprintf("%.10f", prep_obj$low_centered), ";"),
      paste0("  w_mean = ", sprintf("%.10f", prep_obj$mean_centered), ";"),
      paste0("  w_high = ", sprintf("%.10f", prep_obj$high_centered), ";")
    )
    for (i in seq_len(nrow(path_df))) {
      nodes_i <- custom_spec$path_nodes[[i]]
      expr_low <- build_custom_path_expr_pm(nodes_i, custom_spec, "w_low")
      expr_mean <- build_custom_path_expr_pm(nodes_i, custom_spec, "w_mean")
      expr_high <- build_custom_path_expr_pm(nodes_i, custom_spec, "w_high")
      constraint_lines <- c(
        constraint_lines,
        paste0("  ", path_df$low_param[i], " = ", expr_low, ";"),
        paste0("  ", path_df$mean_param[i], " = ", expr_mean, ";"),
        paste0("  ", path_df$high_param[i], " = ", expr_high, ";")
      )
      imm_expr_i <- build_custom_imm_expr_pm(nodes_i, custom_spec)
      if (!is.null(imm_expr_i) && nzchar(as.character(path_df$imm_param[i] %||% "")[1])) {
        constraint_lines <- c(
          constraint_lines,
          paste0("  ", path_df$imm_param[i], " = ", imm_expr_i, ";")
        )
      }
    }
  } else {
    new_names <- path_df$single_param
    constraint_lines <- c("MODEL CONSTRAINT:", wrap_new_statement_local_pm(new_names))
    for (i in seq_len(nrow(path_df))) {
      expr_i <- build_custom_path_expr_pm(custom_spec$path_nodes[[i]], custom_spec, NULL)
      constraint_lines <- c(constraint_lines, paste0("  ", path_df$single_param[i], " = ", expr_i, ";"))
    }
  }

  input_lines <- c(
    paste0("TITLE: PROCESS-like custom model for ", outcome_var, " from ", x_var, if (nzchar(w_var %||% "")) paste0(" with moderator ", w_var) else "", ";"),
    "DATA:",
    paste0("  FILE = ", gsub("\\\\", "/", paths$data_file), ";"),
    variable_lines,
    "ANALYSIS:",
    type_line,
    paste0("  ESTIMATOR = ", PROCESS_SETTINGS$mplus_estimator %||% "MLR", ";"),
    model_lines,
    constraint_lines,
    "OUTPUT:",
    "  SAMPSTAT CINTERVAL;"
  )

  writeLines(input_lines, con = paths$inp_file, useBytes = TRUE)
  paths
}

fit_observed_mplus_custom_model <- function(df, outcome_var, x_var, mediator_vars, w_var, covariates, survey_bundle) {
  custom_spec <- build_custom_model_spec_pm(PROCESS_SETTINGS, outcome_var = outcome_var, x_var = x_var)
  prep_obj <- build_observed_custom_mplus_data_pm(
    df = df,
    outcome_var = outcome_var,
    x_var = x_var,
    mediator_vars = mediator_vars,
    w_var = w_var,
    covariates = covariates,
    survey_bundle = survey_bundle,
    process_settings = PROCESS_SETTINGS,
    custom_spec = custom_spec
  )
  if (is.null(prep_obj)) return(NULL)

  model_tag <- paste(
    sanitize_token_pm(DATASET_ID),
    "pmcustom",
    sanitize_token_pm(outcome_var),
    sanitize_token_pm(x_var),
    paste(vapply(mediator_vars, sanitize_token_pm, character(1)), collapse = ""),
    if (nzchar(w_var %||% "")) sanitize_token_pm(w_var) else "now",
    sep = "_"
  )
  paths <- build_observed_custom_model_input_pm(
    prep_obj = prep_obj,
    model_tag = model_tag,
    outcome_var = outcome_var,
    x_var = x_var,
    w_var = w_var
  )

  cfg_local <- if (exists("CFG", inherits = TRUE)) get("CFG", inherits = TRUE) else list()
  mplus_exe <- resolve_mplus_exe(cfg_local, default = cfg_local$mplus_exe %||% NULL, must_exist = TRUE)
  run_res <- run_mplus_model(paths$inp_file, mplus_exe = mplus_exe, workdir = dirname(paths$inp_file), quiet = TRUE)
  out_lines <- read_mplus_out_lines(paths$out_file)

  if (!isTRUE(run_res$ok) || !detect_mplus_normal_termination(out_lines)) {
    stop(
      "Mplus custom process model run failed for outcome = ", outcome_var,
      ", x = ", x_var,
      ". Check: ", paths$out_file,
      call. = FALSE
    )
  }

  parsed_main <- parse_mplus_mediation_sections_pm(out_lines)
  parsed_r2 <- parse_mplus_rsquare_pm(out_lines)
  nobs <- parse_mplus_nobs_pm(out_lines)
  if (is.na(nobs)) nobs <- prep_obj$analysis_n

  find_row <- function(section, name, lhs = NULL) {
    if (!is.data.frame(parsed_main) || nrow(parsed_main) == 0) return(data.frame())
    hit <- parsed_main[
      as.character(parsed_main$section) == section &
        as.character(parsed_main$name) == toupper(name),
      ,
      drop = FALSE
    ]
    if (!is.null(lhs) && "lhs" %in% names(hit)) {
      hit <- hit[as.character(hit$lhs) == toupper(lhs), , drop = FALSE]
    }
    if (nrow(hit) == 0) data.frame() else hit[1, , drop = FALSE]
  }

  get_custom_section_name_pm <- function(target_export) {
    target_export <- as.character(target_export %||% "")[1]
    if (identical(target_export, "y_pm")) "y_on" else paste0(tolower(target_export), "_on")
  }

  get_path_coef_values_pm <- function(path_nodes, probe_centered = NULL) {
    out_vals <- rep(NA_real_, max(0L, length(path_nodes) - 1L))
    if (!length(out_vals)) return(out_vals)

    for (k in seq_len(length(path_nodes) - 1L)) {
      key_k <- paste(path_nodes[k + 1L], path_nodes[k], sep = "||")
      edge_k <- custom_spec$edges[as.character(custom_spec$edges$key) == key_k, , drop = FALSE]
      if (!nrow(edge_k)) next

      section_k <- get_custom_section_name_pm(edge_k$target_export[1])
      row_base_k <- find_row(section_k, edge_k$predictor_export[1], lhs = edge_k$target_export[1])
      base_est_k <- if (nrow(row_base_k) == 1L) safe_num_pm(row_base_k$estimate[1]) else NA_real_
      coef_k <- base_est_k

      if (isTRUE(edge_k$is_moderated[1])) {
        row_int_k <- find_row(section_k, edge_k$int_export[1], lhs = edge_k$target_export[1])
        int_est_k <- if (nrow(row_int_k) == 1L) safe_num_pm(row_int_k$estimate[1]) else NA_real_
        if (!is.null(probe_centered) && is.finite(base_est_k) && is.finite(int_est_k)) {
          coef_k <- base_est_k + int_est_k * probe_centered
        }
      }

      out_vals[k] <- coef_k
    }

    out_vals
  }

  make_ci <- function(est, se) {
    c(
      llci = ifelse(is.na(est) || is.na(se), NA_real_, est - 1.96 * se),
      ulci = ifelse(is.na(est) || is.na(se), NA_real_, est + 1.96 * se)
    )
  }

  compute_step_test_pm_custom <- function(dep_var, rhs_terms) {
    rhs_terms <- rhs_terms[rhs_terms %in% names(prep_obj$export_data)]
    if (!length(rhs_terms) || !dep_var %in% names(prep_obj$export_data)) return(list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
    dat_fit <- prep_obj$export_data
    if (prep_obj$use_subpopulation && "sb_pm" %in% names(dat_fit)) dat_fit <- dat_fit[dat_fit$sb_pm == 1, , drop = FALSE]
    if (!nrow(dat_fit)) return(list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
    form_chr <- paste(dep_var, "~", paste(rhs_terms, collapse = " + "))

    if (requireNamespace("survey", quietly = TRUE)) {
      survey_res <- tryCatch({
        ids_formula <- if (!is.null(prep_obj$design_map$cluster_var) && prep_obj$design_map$cluster_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$cluster_var)) else ~1
        strata_formula <- if (!is.null(prep_obj$design_map$strata_var) && prep_obj$design_map$strata_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$strata_var)) else NULL
        weights_formula <- if (!is.null(prep_obj$design_map$weight_var) && prep_obj$design_map$weight_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$weight_var)) else NULL
        des <- survey::svydesign(ids = ids_formula, strata = strata_formula, weights = weights_formula, data = dat_fit, nest = TRUE)
        fit <- survey::svyglm(stats::as.formula(form_chr), design = des)
        tst <- survey::regTermTest(fit, stats::as.formula(paste0("~", paste(rhs_terms, collapse = " + "))), method = "Wald")
        df1_i <- safe_num_pm(if (!is.null(tst$df)) tst$df[1] else NA_real_)
        p_i <- safe_num_pm(if (!is.null(tst$p)) tst$p[1] else NA_real_)
        f_i <- safe_num_pm(if (!is.null(tst$Ftest)) tst$Ftest[1] else NA_real_)
        chi_i <- safe_num_pm(if (!is.null(tst$chisq)) tst$chisq[1] else NA_real_)
        if (is.na(f_i) && !is.na(chi_i) && !is.na(df1_i) && df1_i > 0) f_i <- chi_i / df1_i
        list(f_value = f_i, df1 = df1_i, p = p_i)
      }, error = function(e) NULL)
      if (!is.null(survey_res)) return(survey_res)
    }

    tryCatch({
      fit <- stats::lm(stats::as.formula(form_chr), data = dat_fit)
      fstat <- tryCatch(unname(summary(fit)$fstatistic), error = function(e) c(NA_real_, NA_real_, NA_real_))
      p_i <- if (length(fstat) >= 3 && all(!is.na(fstat[1:3]))) stats::pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE) else NA_real_
      list(f_value = safe_num_pm(fstat[1]), df1 = safe_num_pm(fstat[2]), p = safe_num_pm(p_i))
    }, error = function(e) list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
  }

  add_coef_row <- function(term, row_obj, effect_type, effect, term_label, variable_label = term_label, sort_key, model_component, category_label = "", reference_label = "", target_var = NA_character_, target_label = NA_character_, mediator_label = NA_character_, table_block_key = "", block_order = NA_real_) {
    if (!is.data.frame(row_obj) || nrow(row_obj) == 0) return(NULL)
    ci_i <- make_ci(row_obj$estimate[1], row_obj$se[1])
    data.frame(
      term = term,
      estimate = row_obj$estimate[1],
      se = row_obj$se[1],
      t_value = row_obj$z_value[1],
      p = row_obj$p[1],
      llci = ci_i["llci"],
      ulci = ci_i["ulci"],
      effect_type = effect_type,
      effect = effect,
      term_label = term_label,
      variable_label = variable_label,
      category_label = category_label,
      reference_label = reference_label,
      sort_key = sort_key,
      model_component = model_component,
      mediator = target_var,
      mediator_label = mediator_label,
      table_block_key = table_block_key,
      block_order = block_order,
      stringsAsFactors = FALSE
    )
  }

  coef_rows <- list()
  idx <- 1L
  eq_map <- unique(custom_spec$edges[, c("target_var", "target_export", "block_order", "model_component", "target_label"), drop = FALSE])
  eq_map <- eq_map[order(eq_map$block_order), , drop = FALSE]
  analysis_key <- paste(outcome_var, x_var, paste(custom_spec$mediator_vars, collapse = "|"), sep = " | ")
  mediator_set_label <- paste(vapply(custom_spec$mediator_vars, lookup_label, character(1)), collapse = " -> ")

  for (i in seq_len(nrow(eq_map))) {
    eq_i <- eq_map[i, , drop = FALSE]
    edge_i <- custom_spec$edges[as.character(custom_spec$edges$target_var) == as.character(eq_i$target_var[1]), , drop = FALSE]
    edge_i <- edge_i[order(edge_i$sort_key), , drop = FALSE]
    section_i <- if (identical(as.character(eq_i$target_export[1]), "y_pm")) {
      "y_on"
    } else {
      paste0(tolower(eq_i$target_export[1]), "_on")
    }
    block_key_i <- paste0(analysis_key, " | ", eq_i$target_var[1])
    mediator_label_i <- if (identical(eq_i$target_var[1], outcome_var)) mediator_set_label else eq_i$target_label[1]

    row_int_i <- find_row("intercepts", eq_i$target_export[1])
    tmp_row <- add_coef_row("(Intercept)", row_int_i, "Intercept", eq_i$model_component[1], "Intercept", "Intercept", 10, eq_i$model_component[1], target_var = eq_i$target_var[1], target_label = eq_i$target_label[1], mediator_label = mediator_label_i, table_block_key = block_key_i, block_order = eq_i$block_order[1])
    if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }

    for (j in seq_len(nrow(edge_i))) {
      edge_j <- edge_i[j, , drop = FALSE]
      row_j <- find_row(section_i, edge_j$predictor_export[1], lhs = eq_i$target_export[1])
      effect_type_j <- if (identical(edge_j$predictor_var[1], x_var)) "Independent variable" else "Mediator"
      tmp_row <- add_coef_row(edge_j$predictor_var[1], row_j, effect_type_j, eq_i$model_component[1], edge_j$predictor_label[1], edge_j$predictor_label[1], 20 + edge_j$sort_key[1], eq_i$model_component[1], target_var = eq_i$target_var[1], target_label = eq_i$target_label[1], mediator_label = mediator_label_i, table_block_key = block_key_i, block_order = eq_i$block_order[1])
      if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
    }

    if (any(edge_i$is_moderated)) {
      row_w_i <- find_row(section_i, "w_pm", lhs = eq_i$target_export[1])
      tmp_row <- add_coef_row(w_var, row_w_i, "Moderator", eq_i$model_component[1], lookup_label(w_var), lookup_label(w_var), 1000, eq_i$model_component[1], target_var = eq_i$target_var[1], target_label = eq_i$target_label[1], mediator_label = mediator_label_i, table_block_key = block_key_i, block_order = eq_i$block_order[1])
      if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
      mod_i <- edge_i[edge_i$is_moderated, , drop = FALSE]
      for (j in seq_len(nrow(mod_i))) {
        row_mod_j <- find_row(section_i, mod_i$int_export[j], lhs = eq_i$target_export[1])
        label_j <- paste0(mod_i$predictor_label[j], " x ", lookup_label(w_var))
        tmp_row <- add_coef_row(paste0(mod_i$predictor_var[j], ":", w_var), row_mod_j, "Interaction", eq_i$model_component[1], label_j, label_j, 1100 + mod_i$sort_key[j], eq_i$model_component[1], target_var = eq_i$target_var[1], target_label = eq_i$target_label[1], mediator_label = mediator_label_i, table_block_key = block_key_i, block_order = eq_i$block_order[1])
        if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
      }
    }

    if (nrow(prep_obj$cov_term_map) > 0) {
      for (j in seq_len(nrow(prep_obj$cov_term_map))) {
        map_i <- prep_obj$cov_term_map[j, , drop = FALSE]
        row_cov_i <- find_row(section_i, map_i$export_name[1], lhs = eq_i$target_export[1])
        tmp_row <- add_coef_row(as.character(map_i$term[1]), row_cov_i, as.character(map_i$effect_type[1]), eq_i$model_component[1], as.character(map_i$term_label[1]), as.character(map_i$variable_label[1]), 400000 + j, eq_i$model_component[1], as.character(map_i$category_label[1]), as.character(map_i$reference_label[1]), target_var = eq_i$target_var[1], target_label = eq_i$target_label[1], mediator_label = mediator_label_i, table_block_key = block_key_i, block_order = eq_i$block_order[1])
        if (is.null(tmp_row)) next
        coef_rows[[idx]] <- tmp_row
        idx <- idx + 1L
      }
    }
  }

  coef_df <- if (length(coef_rows) > 0) do.call(rbind, coef_rows) else data.frame()
  if (nrow(coef_df) > 0) {
    coef_df$outcome <- ifelse(coef_df$model_component == "Outcome model", outcome_var, coef_df$mediator)
    coef_df$outcome_label <- ifelse(coef_df$model_component == "Outcome model", lookup_label(outcome_var), coef_df$mediator_label)
    coef_df$target_outcome <- outcome_var
    coef_df$target_outcome_label <- lookup_label(outcome_var)
    coef_df$x_var <- x_var
    coef_df$x_label <- lookup_label(x_var)
    coef_df$moderator <- if (nzchar(w_var %||% "")) w_var else NA_character_
    coef_df$moderator_label <- if (nzchar(w_var %||% "")) lookup_label(w_var) else NA_character_
    coef_df$analysis_key <- analysis_key
    coef_df$n <- nobs
    coef_df$p_fmt <- fmt_p_pm(coef_df$p)
    coef_df$sig <- sig_mark_pm(coef_df$p)
    coef_df <- coef_df[order(coef_df$block_order, coef_df$sort_key, coef_df$term_label), , drop = FALSE]
    rownames(coef_df) <- NULL
  }

  wt_vec <- if ("wt_pm" %in% names(prep_obj$export_data)) safe_num_pm(prep_obj$export_data$wt_pm) else NULL
  get_r2_pm_custom <- function(var_name) {
    r2_i <- NA_real_
    hit_i <- parsed_r2[as.character(parsed_r2$name) == toupper(var_name), , drop = FALSE]
    if (nrow(hit_i) == 1) r2_i <- hit_i$estimate[1]
    resid_i <- find_row("residual_variances", var_name)
    resid_var_i <- if (nrow(resid_i) == 1) safe_num_pm(resid_i$estimate[1]) else NA_real_
    if (is.na(r2_i) && !is.na(resid_var_i) && var_name %in% names(prep_obj$export_data)) {
      var_i <- if (is.null(wt_vec)) stats::var(safe_num_pm(prep_obj$export_data[[var_name]]), na.rm = TRUE) else weighted_sd_pm(prep_obj$export_data[[var_name]], wt_vec)^2
      if (!is.na(var_i) && var_i > 0) r2_i <- max(0, min(1, 1 - resid_var_i / var_i))
    }
    r2_i
  }

  summary_rows <- list()
  sidx <- 1L
  for (i in seq_len(nrow(eq_map))) {
    eq_i <- eq_map[i, , drop = FALSE]
    edge_i <- custom_spec$edges[as.character(custom_spec$edges$target_var) == as.character(eq_i$target_var[1]), , drop = FALSE]
    rhs_i <- unique(c(edge_i$predictor_export, if (any(edge_i$is_moderated)) c("w_pm", edge_i$int_export[edge_i$is_moderated]) else character(0), prep_obj$cov_export_names))
    test_i <- compute_step_test_pm_custom(eq_i$target_export[1], rhs_i)
    summary_rows[[sidx]] <- data.frame(
      model_component = eq_i$model_component[1],
      outcome = if (identical(eq_i$target_var[1], outcome_var)) outcome_var else eq_i$target_var[1],
      outcome_label = if (identical(eq_i$target_var[1], outcome_var)) lookup_label(outcome_var) else eq_i$target_label[1],
      mediator = if (identical(eq_i$target_var[1], outcome_var)) paste(custom_spec$mediator_vars, collapse = "|") else eq_i$target_var[1],
      mediator_label = if (identical(eq_i$target_var[1], outcome_var)) mediator_set_label else eq_i$target_label[1],
      moderator = if (nzchar(w_var %||% "")) w_var else NA_character_,
      moderator_label = if (nzchar(w_var %||% "")) lookup_label(w_var) else NA_character_,
      x_var = x_var,
      x_label = lookup_label(x_var),
      target_outcome = outcome_var,
      target_outcome_label = lookup_label(outcome_var),
      n = nobs,
      r2 = get_r2_pm_custom(eq_i$target_export[1]),
      adj_r2 = NA_real_,
      f_value = test_i$f_value %||% NA_real_,
      df1 = test_i$df1 %||% NA_real_,
      df2 = NA_real_,
      p = test_i$p %||% NA_real_,
      bootstrap_enabled = FALSE,
      bootstrap_n = NA_real_,
      bootstrap_ci = NA_character_,
      estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR",
      variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design",
      table_block_key = paste0(analysis_key, " | ", eq_i$target_var[1]),
      block_order = eq_i$block_order[1],
      analysis_key = analysis_key,
      stringsAsFactors = FALSE
    )
    sidx <- sidx + 1L
  }
  model_summary <- do.call(rbind, summary_rows)
  model_summary$p_fmt <- fmt_p_pm(model_summary$p)
  model_summary$sig <- sig_mark_pm(model_summary$p)
  model_summary <- model_summary[order(model_summary$block_order), , drop = FALSE]

  new_params <- parsed_main[as.character(parsed_main$section) == "new_parameters", , drop = FALSE]
  indirect_rows <- list()
  iidx <- 1L
  if (isTRUE(custom_spec$has_moderation)) {
    cond_specs <- data.frame(
      param_col = c("low_param", "mean_param", "high_param"),
      probe_centered = c(prep_obj$low_centered, prep_obj$mean_centered, prep_obj$high_centered),
      class_contrast = c(
        paste0(lookup_label(w_var), " = M - 1 SD (", formatC(prep_obj$low_raw, format = "f", digits = 2), ")"),
        paste0(lookup_label(w_var), " = M (", formatC(prep_obj$mean_raw, format = "f", digits = 2), ")"),
        paste0(lookup_label(w_var), " = M + 1 SD (", formatC(prep_obj$high_raw, format = "f", digits = 2), ")")
      ),
      contrast_order = c(1L, 2L, 3L),
      stringsAsFactors = FALSE
    )
    for (i in seq_len(nrow(custom_spec$path_df))) {
      path_i <- custom_spec$path_df[i, , drop = FALSE]
      for (j in seq_len(nrow(cond_specs))) {
        param_name <- as.character(path_i[[cond_specs$param_col[j]]])[1]
        hit <- new_params[as.character(new_params$name) == toupper(param_name), , drop = FALSE]
        if (nrow(hit) == 0) next
        ci_i <- make_ci(hit$estimate[1], hit$se[1])
        coef_vals_i <- get_path_coef_values_pm(
          custom_spec$path_nodes[[path_i$path_id[1]]],
          probe_centered = cond_specs$probe_centered[j]
        )
        indirect_rows[[iidx]] <- data.frame(
          outcome = outcome_var,
          outcome_label = lookup_label(outcome_var),
          mediator = path_i$mediator_vars[1],
          mediator_label = path_i$mediator_labels[1],
          x_var = x_var,
          x_label = lookup_label(x_var),
          moderator = w_var,
          moderator_label = lookup_label(w_var),
          class_contrast = cond_specs$class_contrast[j],
          a = coef_vals_i[1],
          a_se = NA_real_,
          a_p = NA_real_,
          b = coef_vals_i[2],
          b_se = NA_real_,
          b_p = NA_real_,
          c = coef_vals_i[3],
          direct = NA_real_,
          total = NA_real_,
          indirect = hit$estimate[1],
          se = hit$se[1],
          z_value = hit$z_value[1],
          llci = ci_i["llci"],
          ulci = ci_i["ulci"],
          p = hit$p[1],
          p_fmt = fmt_p_pm(hit$p[1]),
          sig = sig_mark_pm(hit$p[1]),
          contrast_order = i * 10L + cond_specs$contrast_order[j],
          analysis_key = analysis_key,
          stringsAsFactors = FALSE
        )
        iidx <- iidx + 1L
      }

      if (isTRUE(safe_num_pm(path_i$n_moderated_edges[1]) == 1) && nzchar(as.character(path_i$imm_param[1] %||% "")[1])) {
        imm_hit <- new_params[as.character(new_params$name) == toupper(as.character(path_i$imm_param[1])), , drop = FALSE]
        if (nrow(imm_hit) > 0) {
          imm_ci <- make_ci(imm_hit$estimate[1], imm_hit$se[1])
          indirect_rows[[iidx]] <- data.frame(
            outcome = outcome_var,
            outcome_label = lookup_label(outcome_var),
            mediator = path_i$mediator_vars[1],
            mediator_label = path_i$mediator_labels[1],
            x_var = x_var,
            x_label = lookup_label(x_var),
            moderator = w_var,
            moderator_label = lookup_label(w_var),
            class_contrast = "Index of moderated mediation",
            a = NA_real_,
            a_se = NA_real_,
            a_p = NA_real_,
            b = NA_real_,
            b_se = NA_real_,
            b_p = NA_real_,
            c = NA_real_,
            direct = NA_real_,
            total = NA_real_,
            indirect = imm_hit$estimate[1],
            se = imm_hit$se[1],
            z_value = imm_hit$z_value[1],
            llci = imm_ci["llci"],
            ulci = imm_ci["ulci"],
            p = imm_hit$p[1],
            p_fmt = fmt_p_pm(imm_hit$p[1]),
            sig = sig_mark_pm(imm_hit$p[1]),
            contrast_order = i * 10L + 9L,
            analysis_key = analysis_key,
            stringsAsFactors = FALSE
          )
          iidx <- iidx + 1L
        }
      }
    }
  } else {
    for (i in seq_len(nrow(custom_spec$path_df))) {
      path_i <- custom_spec$path_df[i, , drop = FALSE]
      hit <- new_params[as.character(new_params$name) == toupper(path_i$single_param[1]), , drop = FALSE]
      if (nrow(hit) == 0) next
      ci_i <- make_ci(hit$estimate[1], hit$se[1])
      coef_vals_i <- get_path_coef_values_pm(custom_spec$path_nodes[[path_i$path_id[1]]], probe_centered = NULL)
      indirect_rows[[iidx]] <- data.frame(
        outcome = outcome_var,
        outcome_label = lookup_label(outcome_var),
        mediator = path_i$mediator_vars[1],
        mediator_label = path_i$mediator_labels[1],
        x_var = x_var,
        x_label = lookup_label(x_var),
        class_contrast = "Indirect effect",
        a = coef_vals_i[1],
        a_se = NA_real_,
        a_p = NA_real_,
        b = coef_vals_i[2],
        b_se = NA_real_,
        b_p = NA_real_,
        c = coef_vals_i[3],
        direct = NA_real_,
        total = NA_real_,
        indirect = hit$estimate[1],
        se = hit$se[1],
        z_value = hit$z_value[1],
        llci = ci_i["llci"],
        ulci = ci_i["ulci"],
        p = hit$p[1],
        p_fmt = fmt_p_pm(hit$p[1]),
        sig = sig_mark_pm(hit$p[1]),
        contrast_order = i,
        analysis_key = analysis_key,
        stringsAsFactors = FALSE
      )
      iidx <- iidx + 1L
    }
  }
  indirect_df <- if (length(indirect_rows) > 0) do.call(rbind, indirect_rows) else data.frame()
  if (is.data.frame(indirect_df) && nrow(indirect_df) > 0 && "contrast_order" %in% names(indirect_df)) {
    indirect_df <- indirect_df[order(indirect_df$contrast_order, indirect_df$mediator_label), , drop = FALSE]
    rownames(indirect_df) <- NULL
  }

  list(
    coefficients = coef_df,
    summary = model_summary,
    indirect = indirect_df,
    conditional_effects = data.frame(),
    run_info = data.frame(
      model_tag = model_tag,
      inp_file = paths$inp_file,
      out_file = paths$out_file,
      data_file = paths$data_file,
      stringsAsFactors = FALSE
    )
  )
}
