T0_FIGURES <- Sys.time()

log_step_start("FIGURES", "05_figures.R")
log_info("Reloading process figure inputs ...")

PROCESS_SETTINGS <- load_step_rds("PROCESS_SETTINGS", dir_rds = DIR_RDS, required = TRUE)
MODEL_RUN_SUMMARY <- load_step_rds("MODEL_RUN_SUMMARY", dir_rds = DIR_RDS, default = list())
PROCESS_MODEL_RESULTS <- load_step_rds("PROCESS_MODEL_RESULTS", dir_rds = DIR_RDS, default = data.frame())
PROCESS_MODEL_SUMMARY <- load_step_rds("PROCESS_MODEL_SUMMARY", dir_rds = DIR_RDS, default = data.frame())
PROCESS_INDIRECT_RESULTS <- load_step_rds("PROCESS_INDIRECT_RESULTS", dir_rds = DIR_RDS, default = data.frame())
PROCESS_CONDITIONAL_EFFECTS <- load_step_rds("PROCESS_CONDITIONAL_EFFECTS", dir_rds = DIR_RDS, default = data.frame())
PROCESS_DATA <- load_step_rds("PROCESS_DATA", dir_rds = DIR_RDS, default = data.frame())
SURVEY_BUNDLE <- load_step_rds("SURVEY_BUNDLE", dir_rds = DIR_RDS, default = list())
DICT <- load_step_rds("DICT", dir_rds = DIR_RDS, default = list())
VAR_SPECS <- load_step_rds("VAR_SPECS", dir_rds = DIR_RDS, default = data.frame())
SOURCE_REFERENCE_CLASS <- load_step_rds("SOURCE_REFERENCE_CLASS", dir_rds = DIR_RDS, default = NA_integer_)

`%||%` <- function(x, y) if (is.null(x)) y else x
first_nonempty_fig <- function(...) {
  xs <- list(...)
  for (x in xs) {
    if (length(x) == 0) next
    val <- as.character(x[1])
    if (!is.na(val) && nzchar(val)) return(val)
  }
  NA_character_
}

ensure_dir_local <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

ensure_dir_local(DIR_FIGURES)
ensure_dir_local(DIR_FIGURES_PNG)
ensure_dir_local(DIR_FIGURES_TIFF)
ensure_dir_local(DIR_FIGURES_PDF)

safe_num_fig <- function(x) suppressWarnings(as.numeric(as.character(x)))
sanitize_token_fig <- function(x) {
  x <- tolower(as.character(x)[1] %||% "")
  x <- gsub("[^a-z0-9]+", "", x)
  if (!nzchar(x)) "v" else x
}

weighted_mean_fig <- function(x, w = NULL) {
  x <- safe_num_fig(x)
  if (is.null(w) || length(w) == 0) return(mean(x, na.rm = TRUE))
  w <- safe_num_fig(w)
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(mean(x, na.rm = TRUE))
  stats::weighted.mean(x[ok], w[ok], na.rm = TRUE)
}

extract_pred_with_se_fig <- function(pred_obj) {
  if (is.null(pred_obj)) return(NULL)

  if (is.list(pred_obj) && !is.null(pred_obj$fit)) {
    fit <- as.numeric(pred_obj$fit)
    se <- if (!is.null(pred_obj$se.fit)) as.numeric(pred_obj$se.fit) else rep(NA_real_, length(fit))
    return(list(fit = fit, se = se))
  }

  fit <- as.numeric(pred_obj)
  se_attr <- attr(pred_obj, "se.fit", exact = TRUE)
  if (is.null(se_attr)) se_attr <- attr(pred_obj, "SE", exact = TRUE)
  if (is.null(se_attr)) {
    var_attr <- attr(pred_obj, "var", exact = TRUE)
    if (!is.null(var_attr)) {
      if (is.matrix(var_attr) && nrow(var_attr) == length(fit) && ncol(var_attr) == length(fit)) {
        se_attr <- sqrt(pmax(diag(var_attr), 0))
      } else if (length(var_attr) == length(fit)) {
        se_attr <- sqrt(pmax(as.numeric(var_attr), 0))
      } else if (length(var_attr) == 1L && length(fit) > 1L) {
        se_attr <- rep(sqrt(max(as.numeric(var_attr), 0)), length(fit))
      }
    }
  }
  if (is.null(se_attr)) {
    vcov_attr <- attr(pred_obj, "vcov", exact = TRUE)
    if (!is.null(vcov_attr)) {
      if (is.matrix(vcov_attr) && nrow(vcov_attr) == length(fit) && ncol(vcov_attr) == length(fit)) {
        se_attr <- sqrt(pmax(diag(vcov_attr), 0))
      } else if (length(vcov_attr) == length(fit)) {
        se_attr <- sqrt(pmax(as.numeric(vcov_attr), 0))
      }
    }
  }
  se <- if (!is.null(se_attr)) as.numeric(se_attr) else rep(NA_real_, length(fit))

  if (length(se) == 1L && length(fit) > 1L) se <- rep(se, length(fit))
  list(fit = fit, se = se)
}

FIGURE_MANIFEST <- if (exists("empty_figure_manifest", mode = "function")) {
  empty_figure_manifest()
} else {
  data.frame(
    figure_id = character(0),
    file_stub = character(0),
    figure_title = character(0),
    png = character(0),
    tiff = character(0),
    pdf = character(0),
    width = numeric(0),
    height = numeric(0),
    dpi = numeric(0),
    stringsAsFactors = FALSE
  )
}

dict_meta <- DICT$meta %||% DICT$META %||% data.frame()
dict_levels_fig <- DICT$levels %||% DICT$LEVELS %||% data.frame()

lookup_var_label_fig <- function(var_name) {
  if (!is.data.frame(dict_meta) || nrow(dict_meta) == 0 || !"var_name" %in% names(dict_meta)) return(var_name)
  hit <- dict_meta[as.character(dict_meta$var_name) == as.character(var_name), , drop = FALSE]
  if (nrow(hit) == 0) return(var_name)
  out <- first_nonempty_fig(hit$label_en[1], hit$var_label[1], hit$label_ko[1], var_name)
  if (!nzchar(out)) var_name else out
}

lookup_var_subtype_fig <- function(var_name) {
  hit_vs <- VAR_SPECS[as.character(VAR_SPECS$var_name) == as.character(var_name), , drop = FALSE]
  if (nrow(hit_vs) > 0 && isTRUE(hit_vs$is_continuous[1])) return("continuous")
  if (is.data.frame(dict_meta) && nrow(dict_meta) > 0 && "var_name" %in% names(dict_meta)) {
    hit <- dict_meta[as.character(dict_meta$var_name) == as.character(var_name), , drop = FALSE]
    if (nrow(hit) > 0) {
      type_i <- tolower(as.character(hit$type[1] %||% ""))
      subtype_i <- tolower(as.character(hit$subtype[1] %||% ""))
      if (type_i %in% c("continuous", "numeric", "scale") || subtype_i == "continuous") return("continuous")
    }
  }
  "categorical"
}

lookup_reference_value_fig <- function(var_name) {
  if (is.data.frame(dict_meta) && nrow(dict_meta) > 0 && "var_name" %in% names(dict_meta)) {
    hit <- dict_meta[as.character(dict_meta$var_name) == as.character(var_name), , drop = FALSE]
    if (nrow(hit) > 0 && "reference" %in% names(hit)) {
      ref <- as.character(hit$reference[1] %||% "")
      if (nzchar(ref)) return(ref)
    }
  }
  if (is.data.frame(dict_levels_fig) && nrow(dict_levels_fig) > 0) {
    lv <- dict_levels_fig[as.character(dict_levels_fig$var_name) == as.character(var_name), , drop = FALSE]
    if (nrow(lv) > 0 && "reference" %in% names(lv)) {
      ref <- as.character(lv$reference[!is.na(lv$reference) & nzchar(as.character(lv$reference))][1] %||% "")
      if (nzchar(ref)) return(ref)
    }
  }
  NA_character_
}

lookup_level_values_fig <- function(var_name, observed_values = character(0)) {
  out <- character(0)
  if (is.data.frame(dict_levels_fig) && nrow(dict_levels_fig) > 0) {
    hit <- dict_levels_fig[as.character(dict_levels_fig$var_name) == as.character(var_name), , drop = FALSE]
    if (nrow(hit) > 0 && "value" %in% names(hit)) out <- as.character(hit$value)
  }
  out <- unique(c(out[!is.na(out) & nzchar(out)], as.character(observed_values[!is.na(observed_values) & nzchar(observed_values)])))
  out
}

fmt_path_coef_fig <- function(est, p, digits = 2L) {
  est_num <- safe_num_fig(est)
  p_num <- safe_num_fig(p)
  if (is.na(est_num)) return("")
  out <- sprintf(paste0("%.", digits, "f"), est_num)
  out <- sub("^0\\.", ".", out)
  out <- sub("^-0\\.", "-.", out)
  sig <- if (is.na(p_num)) "" else if (p_num < 0.001) "***" else if (p_num < 0.01) "**" else if (p_num < 0.05) "*" else ""
  paste0(out, sig)
}

build_custom_path_layout_fig <- function(model_df) {
  if (!is.data.frame(model_df) || nrow(model_df) == 0) return(data.frame())

  mediator_df <- unique(model_df[as.character(model_df$model_component %||% "") == "Mediator model", c("outcome", "outcome_label", "block_order"), drop = FALSE])
  mediator_df <- mediator_df[!duplicated(as.character(mediator_df$outcome)), , drop = FALSE]
  mediator_df <- mediator_df[order(safe_num_fig(mediator_df$block_order)), , drop = FALSE]

  x_var <- as.character(model_df$x_var[1] %||% "")
  x_label <- as.character(model_df$x_label[1] %||% lookup_var_label_fig(x_var))
  y_var <- as.character(model_df$target_outcome[1] %||% model_df$outcome[1] %||% "")
  y_label <- as.character(model_df$target_outcome_label[1] %||% lookup_var_label_fig(y_var))
  w_var <- as.character(model_df$moderator[1] %||% "")
  w_label <- as.character(model_df$moderator_label[1] %||% lookup_var_label_fig(w_var))

  base_edges <- model_df[
    as.character(model_df$effect_type %||% "") %in% c("Independent variable", "Mediator"),
    ,
    drop = FALSE
  ]
  if (nrow(base_edges) == 0) return(data.frame())

  edge_df <- data.frame(
    source = ifelse(as.character(base_edges$effect_type) == "Independent variable", as.character(base_edges$x_var), as.character(base_edges$term)),
    target = as.character(base_edges$outcome),
    stringsAsFactors = FALSE
  )
  edge_df <- unique(edge_df)

  node_ids <- unique(c(x_var, mediator_df$outcome, y_var))
  stage_map <- stats::setNames(rep(NA_real_, length(node_ids)), node_ids)
  stage_map[x_var] <- 0
  changed <- TRUE
  while (changed) {
    changed <- FALSE
    for (i in seq_len(nrow(edge_df))) {
      src_i <- as.character(edge_df$source[i])
      tgt_i <- as.character(edge_df$target[i])
      if (!is.na(stage_map[src_i])) {
        cand_i <- stage_map[src_i] + 1
        if (is.na(stage_map[tgt_i]) || cand_i > stage_map[tgt_i]) {
          stage_map[tgt_i] <- cand_i
          changed <- TRUE
        }
      }
    }
  }
  stage_map[is.na(stage_map)] <- 1
  max_stage <- max(stage_map, na.rm = TRUE)
  if (!is.finite(max_stage) || max_stage <= 0) max_stage <- 1

  node_df <- data.frame(
    var = node_ids,
    label = c(x_label, mediator_df$outcome_label[match(setdiff(node_ids, c(x_var, y_var)), mediator_df$outcome)], y_label),
    stage = unname(stage_map[node_ids]),
    stringsAsFactors = FALSE
  )
  node_df$label <- as.character(node_df$label)
  node_df$label[node_df$var == x_var] <- x_label
  node_df$label[node_df$var == y_var] <- y_label
  node_df$label[!nzchar(node_df$label)] <- node_df$var[!nzchar(node_df$label)]
  node_df$x <- 0.10 + 0.78 * (node_df$stage / max_stage)
  node_df$y <- 0.5

  stage_levels <- sort(unique(node_df$stage[node_df$var %in% mediator_df$outcome]))
  for (st in stage_levels) {
    hit <- which(node_df$stage == st & node_df$var %in% mediator_df$outcome)
    n_hit <- length(hit)
    if (n_hit == 1L) {
      node_df$y[hit] <- 0.5
    } else if (n_hit > 1L) {
      node_df$y[hit] <- seq(0.75, 0.25, length.out = n_hit)
    }
  }
  node_df$y[node_df$var == x_var] <- 0.5
  node_df$y[node_df$var == y_var] <- 0.5

  if (nzchar(w_var) && any(as.character(model_df$effect_type %||% "") %in% c("Moderator", "Interaction"))) {
    node_df <- rbind(
      node_df,
      data.frame(
        var = w_var,
        label = if (nzchar(w_label)) w_label else w_var,
        stage = 0,
        x = 0.18,
        y = 0.88,
        stringsAsFactors = FALSE
      )
    )
  }

  node_df
}

build_custom_path_plot <- function(model_df, bw = FALSE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  if (!requireNamespace("grid", quietly = TRUE)) return(NULL)
  if (!is.data.frame(model_df) || nrow(model_df) == 0) return(NULL)

  node_df <- build_custom_path_layout_fig(model_df)
  if (!is.data.frame(node_df) || nrow(node_df) == 0) return(NULL)

  edge_df <- model_df[
    as.character(model_df$effect_type %||% "") %in% c("Independent variable", "Mediator", "Moderator"),
    ,
    drop = FALSE
  ]
  if (nrow(edge_df) == 0) return(NULL)

  edge_df$source <- ifelse(
    as.character(edge_df$effect_type) == "Independent variable",
    as.character(edge_df$x_var),
    ifelse(as.character(edge_df$effect_type) == "Moderator", as.character(edge_df$moderator), as.character(edge_df$term))
  )
  edge_df$target <- as.character(edge_df$outcome)
  edge_df$source_label <- ifelse(
    as.character(edge_df$effect_type) == "Independent variable",
    as.character(edge_df$x_label),
    ifelse(as.character(edge_df$effect_type) == "Moderator", as.character(edge_df$moderator_label), as.character(edge_df$term_label))
  )
  edge_df$edge_label <- vapply(seq_len(nrow(edge_df)), function(i) fmt_path_coef_fig(edge_df$estimate[i], edge_df$p[i]), character(1))
  edge_df <- edge_df[!duplicated(paste(edge_df$source, edge_df$target, edge_df$effect_type)), , drop = FALSE]

  edge_df$x <- node_df$x[match(edge_df$source, node_df$var)]
  edge_df$y <- node_df$y[match(edge_df$source, node_df$var)]
  edge_df$xend <- node_df$x[match(edge_df$target, node_df$var)]
  edge_df$yend <- node_df$y[match(edge_df$target, node_df$var)]
  edge_df$source_stage <- safe_num_fig(node_df$stage[match(edge_df$source, node_df$var)])
  edge_df$target_stage <- safe_num_fig(node_df$stage[match(edge_df$target, node_df$var)])
  edge_df$stage_gap <- edge_df$target_stage - edge_df$source_stage
  edge_df$xm <- (edge_df$x + edge_df$xend) / 2
  edge_df$ym <- (edge_df$y + edge_df$yend) / 2
  edge_df$dx <- edge_df$xend - edge_df$x
  edge_df$dy <- edge_df$yend - edge_df$y
  edge_df$edge_len <- sqrt(edge_df$dx^2 + edge_df$dy^2)
  edge_df$edge_len[!is.finite(edge_df$edge_len) | edge_df$edge_len == 0] <- 1
  x_var_fig <- as.character(model_df$x_var[1] %||% "")
  y_var_fig <- as.character(model_df$target_outcome[1] %||% model_df$outcome[1] %||% "")
  w_var_fig <- as.character(model_df$moderator[1] %||% "")
  mediator_vars_fig <- unique(as.character(model_df$outcome[as.character(model_df$model_component %||% "") == "Mediator model"]))
  mediator_vars_fig <- mediator_vars_fig[nzchar(mediator_vars_fig)]
  m1_var_fig <- if (length(mediator_vars_fig) >= 1L) mediator_vars_fig[1] else NA_character_
  m2_var_fig <- if (length(mediator_vars_fig) >= 2L) mediator_vars_fig[2] else NA_character_
  m3_var_fig <- if (length(mediator_vars_fig) >= 3L) mediator_vars_fig[3] else NA_character_
  edge_df$is_skip_path <- is.finite(edge_df$stage_gap) & edge_df$stage_gap > 1
  edge_df$is_skip_path <- edge_df$is_skip_path & !(as.character(edge_df$source) == x_var_fig & as.character(edge_df$target) %in% mediator_vars_fig)
  edge_df$is_skip_path <- edge_df$is_skip_path | (
    as.character(edge_df$source) == x_var_fig &
    as.character(edge_df$target) == y_var_fig &
    is.finite(edge_df$stage_gap) &
    edge_df$stage_gap >= 3
  )
  edge_df$curve_dir <- ifelse(edge_df$yend >= edge_df$y, 1, -1)
  edge_df$curve_dir[edge_df$curve_dir == 0] <- 1

  grp_target <- as.character(edge_df$target)
  to_target_count <- ave(seq_len(nrow(edge_df)), grp_target, FUN = seq_along)
  to_target_n <- ave(seq_len(nrow(edge_df)), grp_target, FUN = length)
  edge_df$label_t <- vapply(seq_len(nrow(edge_df)), function(i) {
    in_n_i <- as.numeric(to_target_n[i])
    in_k_i <- as.numeric(to_target_count[i])
    t_i <- 0.50
    if (is.finite(in_n_i) && in_n_i > 1) {
      t_i <- t_i + seq(-0.04, 0.04, length.out = in_n_i)[in_k_i]
    }
    max(0.42, min(0.58, t_i))
  }, numeric(1))
  edge_df$label_nudge <- vapply(seq_len(nrow(edge_df)), function(i) {
    in_n_i <- as.numeric(to_target_n[i])
    in_k_i <- as.numeric(to_target_count[i])
    base_i <- if (isTRUE(edge_df$is_skip_path[i])) 0.035 * edge_df$curve_dir[i] else 0.018
    spread_i <- if (is.finite(in_n_i) && in_n_i > 1) seq(-0.015, 0.015, length.out = in_n_i)[in_k_i] else 0
    base_i + spread_i
  }, numeric(1))
  if (nzchar(x_var_fig) && nzchar(y_var_fig)) {
    idx_x_m1 <- which(as.character(edge_df$source) == x_var_fig & as.character(edge_df$target) == m1_var_fig)
    idx_w_m1 <- which(as.character(edge_df$source) == w_var_fig & as.character(edge_df$target) == m1_var_fig)
    idx_x_m2 <- which(as.character(edge_df$source) == x_var_fig & as.character(edge_df$target) == m2_var_fig)
    idx_w_m2 <- which(as.character(edge_df$source) == w_var_fig & as.character(edge_df$target) == m2_var_fig)
    idx_x_m3 <- which(as.character(edge_df$source) == x_var_fig & as.character(edge_df$target) == m3_var_fig)
    idx_m1_m3 <- which(as.character(edge_df$source) == m1_var_fig & as.character(edge_df$target) == m3_var_fig)
    idx_m2_m3 <- which(as.character(edge_df$source) == m2_var_fig & as.character(edge_df$target) == m3_var_fig)
    idx_x_y  <- which(as.character(edge_df$source) == x_var_fig & as.character(edge_df$target) == y_var_fig)
    idx_m3_y <- which(as.character(edge_df$source) == m3_var_fig & as.character(edge_df$target) == y_var_fig)
    if (length(idx_x_m1) > 0) { edge_df$label_t[idx_x_m1] <- 0.40; edge_df$label_nudge[idx_x_m1] <- -0.010 }
    if (length(idx_w_m1) > 0) { edge_df$label_t[idx_w_m1] <- 0.46; edge_df$label_nudge[idx_w_m1] <- 0.016 }
    if (length(idx_x_m2) > 0) { edge_df$label_t[idx_x_m2] <- 0.42; edge_df$label_nudge[idx_x_m2] <- -0.012 }
    if (length(idx_w_m2) > 0) { edge_df$label_t[idx_w_m2] <- 0.54; edge_df$label_nudge[idx_w_m2] <- 0.016 }
    if (length(idx_x_m3) > 0) { edge_df$label_t[idx_x_m3] <- 0.46; edge_df$label_nudge[idx_x_m3] <- 0.014 }
    if (length(idx_m1_m3) > 0) { edge_df$label_t[idx_m1_m3] <- 0.56; edge_df$label_nudge[idx_m1_m3] <- 0.016 }
    if (length(idx_m2_m3) > 0) { edge_df$label_t[idx_m2_m3] <- 0.56; edge_df$label_nudge[idx_m2_m3] <- 0.016 }
    if (length(idx_x_y) > 0)  { edge_df$label_t[idx_x_y]  <- 0.70; edge_df$label_nudge[idx_x_y]  <- 0.050 }
    if (length(idx_m3_y) > 0) { edge_df$label_t[idx_m3_y] <- 0.50; edge_df$label_nudge[idx_m3_y] <- 0.018 }
  }
  edge_df$label_x <- edge_df$x + edge_df$dx * edge_df$label_t - (edge_df$dy / edge_df$edge_len) * edge_df$label_nudge
  edge_df$label_y <- edge_df$y + edge_df$dy * edge_df$label_t + (edge_df$dx / edge_df$edge_len) * edge_df$label_nudge

  interaction_df <- model_df[
    as.character(model_df$effect_type %||% "") == "Interaction" & grepl(":", as.character(model_df$term %||% ""), fixed = TRUE),
    ,
    drop = FALSE
  ]
  if (nrow(interaction_df) > 0) {
    interaction_df$source <- as.character(interaction_df$moderator)
    interaction_df$target <- as.character(interaction_df$outcome)
    interaction_df$x <- node_df$x[match(interaction_df$source, node_df$var)]
    interaction_df$y <- node_df$y[match(interaction_df$source, node_df$var)]
    base_match <- match(paste(as.character(interaction_df$x_var), as.character(interaction_df$outcome)), paste(as.character(edge_df$source), as.character(edge_df$target)))
    interaction_df$xend <- edge_df$xm[base_match]
    interaction_df$yend <- edge_df$ym[base_match]
    interaction_df$edge_label <- vapply(seq_len(nrow(interaction_df)), function(i) paste0("int ", fmt_path_coef_fig(interaction_df$estimate[i], interaction_df$p[i])), character(1))
      interaction_dx <- interaction_df$xend - interaction_df$x
      interaction_dy <- interaction_df$yend - interaction_df$y
      interaction_len <- sqrt(interaction_dx^2 + interaction_dy^2)
      interaction_len[!is.finite(interaction_len) | interaction_len == 0] <- 1
      interaction_df$label_t <- ifelse(
        as.character(interaction_df$target) == m1_var_fig,
        0.44,
        ifelse(as.character(interaction_df$target) == m2_var_fig, 0.40, 0.56)
      )
      interaction_df$label_x <- interaction_df$x + interaction_dx * interaction_df$label_t - (interaction_dy / interaction_len) * 0.030
      interaction_df$label_y <- interaction_df$y + interaction_dy * interaction_df$label_t + (interaction_dx / interaction_len) * 0.030
      idx_int_m2 <- which(as.character(interaction_df$target) == m2_var_fig)
      if (length(idx_int_m2) > 0) {
        interaction_df$label_x[idx_int_m2] <- interaction_df$label_x[idx_int_m2] - 0.035
        interaction_df$label_y[idx_int_m2] <- interaction_df$label_y[idx_int_m2] + 0.040
      }
        }

  arrow_col <- if (bw) "black" else "#37474F"
  box_fill <- if (bw) "white" else "#F7FAFC"
  x_fill <- if (bw) "grey90" else "#E3F2FD"
  y_fill <- if (bw) "grey92" else "#E8F5E9"
  w_fill <- if (bw) "grey95" else "#FFF3E0"

  node_df$fill <- box_fill
  node_df$fill[node_df$var == as.character(model_df$x_var[1] %||% "")] <- x_fill
  node_df$fill[node_df$var == as.character(model_df$target_outcome[1] %||% "")] <- y_fill
  if (any(node_df$var == as.character(model_df$moderator[1] %||% ""))) {
    node_df$fill[node_df$var == as.character(model_df$moderator[1] %||% "")] <- w_fill
  }

  edge_straight_df <- edge_df[!edge_df$is_skip_path, , drop = FALSE]
  edge_curve_df <- edge_df[edge_df$is_skip_path, , drop = FALSE]
  edge_curve_xy_df <- edge_curve_df[
    as.character(edge_curve_df$source) == x_var_fig &
      as.character(edge_curve_df$target) == y_var_fig &
      is.finite(edge_curve_df$stage_gap) &
      edge_curve_df$stage_gap >= 3,
    ,
    drop = FALSE
  ]
  edge_curve_df <- edge_curve_df[!(
    as.character(edge_curve_df$source) == x_var_fig &
      as.character(edge_curve_df$target) == y_var_fig &
      is.finite(edge_curve_df$stage_gap) &
      edge_curve_df$stage_gap >= 3
  ), , drop = FALSE]
  edge_curve_up_df <- edge_curve_df[edge_curve_df$curve_dir > 0, , drop = FALSE]
  edge_curve_down_df <- edge_curve_df[edge_curve_df$curve_dir < 0, , drop = FALSE]

  p <- ggplot2::ggplot() +
    {
      if (nrow(edge_straight_df) > 0) {
        ggplot2::geom_segment(
          data = edge_straight_df,
          ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
          linewidth = 0.7,
          color = arrow_col,
          lineend = "round",
          arrow = grid::arrow(type = "closed", length = grid::unit(0.12, "inches"))
        )
      }
    } +
      {
        if (nrow(edge_curve_xy_df) > 0) {
          ggplot2::geom_curve(
            data = edge_curve_xy_df,
            ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
            linewidth = 0.7,
            curvature = -0.18,
            color = arrow_col,
            lineend = "round",
            arrow = grid::arrow(type = "closed", length = grid::unit(0.12, "inches"))
          )
        }
      } +
      {
        if (nrow(edge_curve_up_df) > 0) {
          ggplot2::geom_curve(
            data = edge_curve_up_df,
          ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
          linewidth = 0.7,
          curvature = 0.24,
          color = arrow_col,
          lineend = "round",
          arrow = grid::arrow(type = "closed", length = grid::unit(0.12, "inches"))
        )
      }
    } +
    {
      if (nrow(edge_curve_down_df) > 0) {
        ggplot2::geom_curve(
          data = edge_curve_down_df,
          ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
          linewidth = 0.7,
          curvature = -0.24,
          color = arrow_col,
          lineend = "round",
          arrow = grid::arrow(type = "closed", length = grid::unit(0.12, "inches"))
        )
      }
    } +
    ggplot2::geom_label(
      data = node_df,
      ggplot2::aes(x = x, y = y, label = label, fill = fill),
      label.size = 0.25,
      size = 4.0,
      fontface = "bold",
      label.padding = grid::unit(0.28, "lines"),
      color = "black",
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = edge_df,
      ggplot2::aes(x = label_x, y = label_y, label = edge_label),
      size = 3.0,
      color = arrow_col
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
    ggplot2::theme_void() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0, size = 10, face = "bold"),
        plot.subtitle = ggplot2::element_text(hjust = 0, size = 8.5)
      ) +
      ggplot2::labs(
        title = "Estimated Path Model for the Custom Moderated Mediation Analysis",
        subtitle = "Path labels denote unstandardized coefficients; dashed arrows indicate moderated effects."
      )

  if (nrow(interaction_df) > 0 && all(is.finite(interaction_df$xend)) && all(is.finite(interaction_df$yend))) {
    p <- p +
      ggplot2::geom_curve(
        data = interaction_df,
        ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
        curvature = 0.25,
        linewidth = 0.45,
        linetype = "22",
        color = if (bw) "black" else "#D84315",
        arrow = grid::arrow(type = "closed", length = grid::unit(0.10, "inches"))
      ) +
      ggplot2::geom_text(
        data = interaction_df,
        ggplot2::aes(x = label_x, y = label_y, label = edge_label),
        size = 2.9,
        color = if (bw) "black" else "#D84315"
      )
  }

  p
}

get_class_labels_fig <- function(dat) {
  if (!is.data.frame(dat) || nrow(dat) == 0 || !"class_num" %in% names(dat)) return(data.frame())
  cls <- sort(unique(stats::na.omit(dat$class_num)))
  out <- data.frame(
    class_num = cls,
    class_label = paste0("Profile ", cls),
    stringsAsFactors = FALSE
  )
  if ("class_label" %in% names(dat)) {
    map <- stats::aggregate(as.character(dat$class_label), by = list(class_num = dat$class_num), FUN = function(x) {
      ux <- unique(x[!is.na(x) & nzchar(x)])
      if (length(ux) == 0) NA_character_ else ux[1]
    })
    names(map)[2] <- "class_label"
    out <- merge(out, map, by = "class_num", all.x = TRUE, sort = FALSE, suffixes = c("", "_obs"))
    out$class_label <- ifelse(
      !is.na(out$class_label_obs) & nzchar(out$class_label_obs),
      out$class_label_obs,
      out$class_label
    )
    out$class_label_obs <- NULL
  }
  out
}

compute_covariate_offset <- function(model_df, dat, covariates) {
  if (!is.data.frame(model_df) || nrow(model_df) == 0 || !is.data.frame(dat) || nrow(dat) == 0) return(0)
  covariates <- covariates[covariates %in% names(dat)]
  if (length(covariates) == 0L) return(0)
  wt_vec <- NULL
  weight_var <- as.character(SURVEY_BUNDLE$weight_var %||% "")[1]
  if (nzchar(weight_var) && weight_var %in% names(dat)) wt_vec <- safe_num_fig(dat[[weight_var]])

  cont_covs <- covariates[vapply(covariates, function(v) identical(lookup_var_subtype_fig(v), "continuous"), logical(1))]
  if (length(cont_covs) == 0L) return(0)

  offset <- 0
  for (v in cont_covs) {
    term_hit <- model_df[as.character(model_df$term) == as.character(v), , drop = FALSE]
    if (nrow(term_hit) == 0) next
    beta_i <- suppressWarnings(as.numeric(term_hit$estimate[1]))
    mean_i <- if (identical(PROCESS_SETTINGS$centering_method %||% "weighted_mean", "weighted_mean")) {
      weighted_mean_fig(dat[[v]], wt_vec)
    } else {
      suppressWarnings(mean(as.numeric(dat[[v]]), na.rm = TRUE))
    }
    if (is.na(beta_i) || is.na(mean_i)) next
    offset <- offset + beta_i * mean_i
  }
  offset
}

parse_probe_level_fig <- function(x, fallback_label = "Moderator") {
  x <- as.character(x)[1]
  mod_name <- fallback_label
  probe_level <- x
  raw_value <- NA_real_

  m <- regexec("^\\s*(.*?)\\s*=\\s*(.*?)\\s*\\((.*?)\\)\\s*$", x, perl = TRUE)
  hit <- regmatches(x, m)[[1]]
  if (length(hit) == 4) {
    if (nzchar(trimws(hit[2]))) mod_name <- trimws(hit[2])
    probe_level <- trimws(hit[3])
    raw_value <- suppressWarnings(as.numeric(trimws(hit[4])))
  }

  probe_level <- gsub("\\s+", " ", probe_level)
  probe_level <- gsub("^M\\s*-\\s*1\\s*SD$", "M-SD", probe_level, ignore.case = TRUE)
  probe_level <- gsub("^M\\s*\\+\\s*1\\s*SD$", "M+SD", probe_level, ignore.case = TRUE)
  probe_level <- gsub("^M$", "Mean", probe_level, ignore.case = TRUE)

  data.frame(
    moderator_label = mod_name,
    probe_level = probe_level,
    raw_value = raw_value,
    stringsAsFactors = FALSE
  )
}

parse_indirect_probe_level_fig <- function(x, fallback_label = "Moderator") {
  x <- as.character(x)[1]
  mod_name <- fallback_label
  probe_level <- x
  raw_value <- NA_real_

  if (identical(trimws(x), "Index of moderated mediation")) {
    return(data.frame(
      moderator_label = fallback_label,
      probe_level = "Index of moderated mediation",
      raw_value = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  m <- regexec("^Conditional indirect effect at\\s*(.*?)\\s*=\\s*(.*?)\\s*\\((.*?)\\)\\s*$", x, perl = TRUE)
  hit <- regmatches(x, m)[[1]]
  if (length(hit) == 4) {
    if (nzchar(trimws(hit[2]))) mod_name <- trimws(hit[2])
    probe_level <- trimws(hit[3])
    raw_value <- suppressWarnings(as.numeric(trimws(hit[4])))
  } else {
    m2 <- regexec("^(.*?)\\s*=\\s*(.*?)\\s*\\((.*?)\\)\\s*$", x, perl = TRUE)
    hit2 <- regmatches(x, m2)[[1]]
    if (length(hit2) == 4) {
      if (nzchar(trimws(hit2[2]))) mod_name <- trimws(hit2[2])
      probe_level <- trimws(hit2[3])
      raw_value <- suppressWarnings(as.numeric(trimws(hit2[4])))
    }
  }

  probe_level <- gsub("\\s+", " ", probe_level)
  probe_level <- gsub("^M\\s*-\\s*1\\s*SD$", "M-SD", probe_level, ignore.case = TRUE)
  probe_level <- gsub("^M\\s*\\+\\s*1\\s*SD$", "M+SD", probe_level, ignore.case = TRUE)
  probe_level <- gsub("^M$", "Mean", probe_level, ignore.case = TRUE)

  data.frame(
    moderator_label = mod_name,
    probe_level = probe_level,
    raw_value = raw_value,
    stringsAsFactors = FALSE
  )
}

prepare_covariates_fig <- function(dat, covariates) {
  dat_out <- dat
  rhs_terms <- character(0)
  cont_defaults <- list()

  for (v in covariates) {
    if (!v %in% names(dat_out)) next
    subtype_i <- lookup_var_subtype_fig(v)
    if (identical(subtype_i, "continuous")) {
      dat_out[[v]] <- safe_num_fig(dat_out[[v]])
      if (length(unique(stats::na.omit(dat_out[[v]]))) <= 1L) next
      rhs_terms <- c(rhs_terms, v)
      cont_defaults[[v]] <- dat_out[[v]]
      next
    }

    raw_chr <- as.character(dat_out[[v]])
    raw_chr[is.na(dat_out[[v]])] <- NA_character_
    level_values <- lookup_level_values_fig(v, observed_values = unique(stats::na.omit(raw_chr)))
    if (length(level_values) == 0L) next

    ref_value <- lookup_reference_value_fig(v)
    if (is.na(ref_value) || !nzchar(ref_value) || !ref_value %in% level_values) ref_value <- level_values[1]
    level_values <- c(ref_value, setdiff(level_values, ref_value))
    compare_values <- setdiff(level_values, ref_value)

    for (i in seq_along(compare_values)) {
      lv <- compare_values[i]
      term_name <- paste0("cov__", v, "__", i)
      dat_out[[term_name]] <- ifelse(is.na(raw_chr), NA_real_, as.numeric(raw_chr == lv))
      if (length(unique(stats::na.omit(dat_out[[term_name]]))) <= 1L) {
        dat_out[[term_name]] <- NULL
        next
      }
      rhs_terms <- c(rhs_terms, term_name)
    }
  }

  list(data = dat_out, rhs_terms = rhs_terms)
}

build_observed_svyglm_bundle <- function(dat, outcome_var, x_var, w_var, covariates = character(0), x_vars_all = x_var) {
  if (!requireNamespace("survey", quietly = TRUE)) return(NULL)
  if (!is.data.frame(dat) || nrow(dat) == 0) return(NULL)
  x_vars_all <- unique(as.character(x_vars_all))
  x_vars_all <- x_vars_all[!is.na(x_vars_all) & nzchar(x_vars_all)]
  if (!all(c(outcome_var, x_var, x_vars_all, w_var) %in% names(dat))) return(NULL)

  fit_dat <- dat
  fit_dat[[outcome_var]] <- safe_num_fig(fit_dat[[outcome_var]])
  fit_dat[[w_var]] <- safe_num_fig(fit_dat[[w_var]])
  for (xv in x_vars_all) fit_dat[[xv]] <- safe_num_fig(fit_dat[[xv]])

  wt_vec <- NULL
  weight_var <- as.character(SURVEY_BUNDLE$weight_var %||% "")[1]
  strata_var <- as.character(SURVEY_BUNDLE$strata_var %||% "")[1]
  cluster_var <- as.character(SURVEY_BUNDLE$cluster_var %||% "")[1]
  if (nzchar(weight_var) && weight_var %in% names(fit_dat)) wt_vec <- safe_num_fig(fit_dat[[weight_var]])

  center_w <- if (identical(PROCESS_SETTINGS$centering_method %||% "weighted_mean", "none")) {
    0
  } else if (identical(PROCESS_SETTINGS$centering_method %||% "weighted_mean", "mean")) {
    mean(fit_dat[[w_var]], na.rm = TRUE)
  } else {
    weighted_mean_fig(fit_dat[[w_var]], wt_vec)
  }

  x_map <- data.frame(
    x_var = x_vars_all,
    x_export = sprintf("x%02d_pm", seq_along(x_vars_all)),
    int_export = sprintf("xw%02d_pm", seq_along(x_vars_all)),
    stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(x_map))) {
    xv <- x_map$x_var[i]
    center_x_i <- if (identical(PROCESS_SETTINGS$centering_method %||% "weighted_mean", "none")) {
      0
    } else if (identical(PROCESS_SETTINGS$centering_method %||% "weighted_mean", "mean")) {
      mean(fit_dat[[xv]], na.rm = TRUE)
    } else {
      weighted_mean_fig(fit_dat[[xv]], wt_vec)
    }
    fit_dat[[x_map$x_export[i]]] <- fit_dat[[xv]] - center_x_i
    x_map$center_x[i] <- center_x_i
  }
  fit_dat$w_pm <- fit_dat[[w_var]] - center_w
  for (i in seq_len(nrow(x_map))) {
    fit_dat[[x_map$int_export[i]]] <- fit_dat[[x_map$x_export[i]]] * fit_dat$w_pm
  }
  fit_dat$y_pm <- fit_dat[[outcome_var]]

  cov_prep <- prepare_covariates_fig(fit_dat, covariates[covariates %in% names(fit_dat)])
  fit_dat <- cov_prep$data
  rhs_terms <- c(x_map$x_export, "w_pm", x_map$int_export, cov_prep$rhs_terms)
  keep_terms <- c("y_pm", rhs_terms)
  design_terms <- c(weight_var, strata_var, cluster_var)
  keep_terms <- unique(c(keep_terms, design_terms[design_terms %in% names(fit_dat)]))
  fit_dat <- fit_dat[stats::complete.cases(fit_dat[, keep_terms, drop = FALSE]), , drop = FALSE]
  if (nrow(fit_dat) < 5L) return(NULL)

  ids_formula <- if (nzchar(cluster_var) && cluster_var %in% names(fit_dat)) stats::as.formula(paste0("~", cluster_var)) else ~1
  strata_formula <- if (nzchar(strata_var) && strata_var %in% names(fit_dat)) stats::as.formula(paste0("~", strata_var)) else NULL
  weights_formula <- if (nzchar(weight_var) && weight_var %in% names(fit_dat)) stats::as.formula(paste0("~", weight_var)) else NULL

  design_args <- list(
    ids = ids_formula,
    data = fit_dat,
    nest = TRUE
  )
  if (!is.null(strata_formula)) design_args$strata <- strata_formula
  if (!is.null(weights_formula)) design_args$weights <- weights_formula

  design_obj <- tryCatch(
    do.call(survey::svydesign, design_args),
    error = function(e) NULL
  )
  if (is.null(design_obj)) return(NULL)

  fit_formula <- stats::as.formula(paste("y_pm ~", paste(rhs_terms, collapse = " + ")))
  fit_obj <- tryCatch(survey::svyglm(fit_formula, design = design_obj), error = function(e) NULL)
  if (is.null(fit_obj)) return(NULL)

  list(
    fit_dat = fit_dat,
    fit_obj = fit_obj,
    design_obj = design_obj,
    center_w = center_w,
    rhs_terms = rhs_terms,
    weight_var = weight_var,
    x_map = x_map,
    x_var = x_var,
    w_var = w_var,
    outcome_var = outcome_var
  )
}

build_observed_ci_plot_df <- function(dat, outcome_var, x_var, w_var, cond_df, covariates = character(0), n_points = 100L, x_vars_all = x_var) {
  fit_bundle <- build_observed_svyglm_bundle(
    dat = dat,
    outcome_var = outcome_var,
    x_var = x_var,
    w_var = w_var,
    covariates = covariates,
    x_vars_all = x_vars_all
  )
  if (!is.list(fit_bundle) || length(fit_bundle) == 0) return(data.frame())

  fit_dat <- fit_bundle$fit_dat
  fit_obj <- fit_bundle$fit_obj
  center_w <- fit_bundle$center_w
  rhs_terms <- fit_bundle$rhs_terms %||% character(0)
  weight_var <- as.character(fit_bundle$weight_var %||% "")[1]
  x_map <- fit_bundle$x_map %||% data.frame()
  focal_hit <- x_map[as.character(x_map$x_var) == as.character(x_var), , drop = FALSE]
  if (nrow(focal_hit) == 0) return(data.frame())
  center_x <- safe_num_fig(focal_hit$center_x[1])
  wt_fit <- if (nzchar(weight_var) && weight_var %in% names(fit_dat)) safe_num_fig(fit_dat[[weight_var]]) else NULL

  parsed_levels <- do.call(
    rbind,
    lapply(seq_len(nrow(cond_df)), function(i) {
      parse_probe_level_fig(
        cond_df$class_contrast[i] %||% "",
        fallback_label = cond_df$moderator_label[i] %||% cond_df$moderator[i] %||% "Moderator"
      )
    })
  )
  parsed_levels <- parsed_levels[!duplicated(parsed_levels$probe_level), , drop = FALSE]
  if (nrow(parsed_levels) == 0) return(data.frame())

  x_obs <- safe_num_fig(fit_dat[[x_var]])
  x_obs <- x_obs[!is.na(x_obs)]
  if (length(x_obs) < 2L) return(data.frame())
  x_grid <- seq(min(x_obs), max(x_obs), length.out = n_points)

  newdata_list <- lapply(seq_len(nrow(parsed_levels)), function(i) {
    lvl_i <- parsed_levels[i, , drop = FALSE]
    w_raw <- safe_num_fig(lvl_i$raw_value[1])
    nd <- data.frame(
      x = x_grid,
      stringsAsFactors = FALSE
    )
    for (k in seq_len(nrow(x_map))) {
      xm <- x_map[k, , drop = FALSE]
      nd[[xm$x_export[1]]] <- if (identical(xm$x_var[1], x_var)) x_grid - center_x else 0
    }
    nd$w_pm <- rep(w_raw - center_w, length(x_grid))
    for (k in seq_len(nrow(x_map))) {
      xm <- x_map[k, , drop = FALSE]
      nd[[xm$int_export[1]]] <- nd[[xm$x_export[1]]] * nd$w_pm
    }
    core_rhs <- c(x_map$x_export, "w_pm", x_map$int_export)
    for (tm in setdiff(rhs_terms, core_rhs)) {
      if (grepl("^cov__", tm)) {
        nd[[tm]] <- 0
      } else {
        nd[[tm]] <- if (identical(PROCESS_SETTINGS$centering_method %||% "weighted_mean", "weighted_mean")) {
          weighted_mean_fig(fit_dat[[tm]], wt_fit)
        } else {
          mean(safe_num_fig(fit_dat[[tm]]), na.rm = TRUE)
        }
      }
    }
    nd$moderator_level <- lvl_i$probe_level[1]
    nd$moderator_label <- lvl_i$moderator_label[1]
    nd
  })
  newdata_all <- do.call(rbind, newdata_list)

  pred_obj <- tryCatch(predict(fit_obj, newdata = newdata_all, se.fit = TRUE), error = function(e) NULL)
  if (is.null(pred_obj)) return(data.frame())
  pred_res <- extract_pred_with_se_fig(pred_obj)
  if (is.null(pred_res)) return(data.frame())

  newdata_all$yhat <- pred_res$fit
  newdata_all$se_fit <- pred_res$se
  newdata_all$llci <- newdata_all$yhat - 1.96 * newdata_all$se_fit
  newdata_all$ulci <- newdata_all$yhat + 1.96 * newdata_all$se_fit
  newdata_all$moderator_level <- factor(as.character(newdata_all$moderator_level), levels = c("M-SD", "Mean", "M+SD"))
  newdata_all
}

build_jn_plot_df <- function(dat, outcome_var, x_var, w_var, covariates = character(0), n_points = 200L, x_vars_all = x_var) {
  fit_bundle <- build_observed_svyglm_bundle(
    dat = dat,
    outcome_var = outcome_var,
    x_var = x_var,
    w_var = w_var,
    covariates = covariates,
    x_vars_all = x_vars_all
  )
  if (!is.list(fit_bundle) || length(fit_bundle) == 0) return(list(plot_df = data.frame(), jn_points = numeric(0), tcrit = NA_real_))

  fit_dat <- fit_bundle$fit_dat
  fit_obj <- fit_bundle$fit_obj
  design_obj <- fit_bundle$design_obj
  center_w <- fit_bundle$center_w
  x_map <- fit_bundle$x_map %||% data.frame()
  focal_hit <- x_map[as.character(x_map$x_var) == as.character(x_var), , drop = FALSE]
  if (nrow(focal_hit) == 0) return(list(plot_df = data.frame(), jn_points = numeric(0), tcrit = NA_real_))
  bx_name <- as.character(focal_hit$x_export[1])
  bint_name <- as.character(focal_hit$int_export[1])

  coef_fit <- stats::coef(fit_obj)
  vc <- tryCatch(stats::vcov(fit_obj), error = function(e) NULL)
  if (is.null(vc) || !all(c(bx_name, bint_name) %in% names(coef_fit))) {
    return(list(plot_df = data.frame(), jn_points = numeric(0), tcrit = NA_real_))
  }

  bx <- safe_num_fig(coef_fit[bx_name])
  bint <- safe_num_fig(coef_fit[bint_name])
  var_bx <- safe_num_fig(vc[bx_name, bx_name])
  var_bint <- safe_num_fig(vc[bint_name, bint_name])
  cov_b <- safe_num_fig(vc[bx_name, bint_name])
  if (any(is.na(c(bx, bint, var_bx, var_bint, cov_b)))) {
    return(list(plot_df = data.frame(), jn_points = numeric(0), tcrit = NA_real_))
  }

  w_obs <- safe_num_fig(fit_dat[[w_var]])
  w_obs <- w_obs[!is.na(w_obs)]
  if (length(w_obs) < 2L) return(list(plot_df = data.frame(), jn_points = numeric(0), tcrit = NA_real_))
  w_grid <- seq(min(w_obs), max(w_obs), length.out = n_points)
  z_grid <- w_grid - center_w

  df_design <- tryCatch(survey::degf(design_obj), error = function(e) NA_real_)
  tcrit <- if (!is.na(df_design) && is.finite(df_design) && df_design > 0) stats::qt(.975, df = df_design) else stats::qnorm(.975)

  effect <- bx + bint * z_grid
  se <- sqrt(pmax(var_bx + 2 * z_grid * cov_b + (z_grid^2) * var_bint, 0))
  llci <- effect - tcrit * se
  ulci <- effect + tcrit * se
  sig_flag <- llci > 0 | ulci < 0

  a <- bint^2 - (tcrit^2) * var_bint
  b <- 2 * bx * bint - 2 * (tcrit^2) * cov_b
  c0 <- bx^2 - (tcrit^2) * var_bx
  roots <- numeric(0)
  if (abs(a) < 1e-10) {
    if (abs(b) > 1e-10) roots <- -c0 / b
  } else {
    disc <- b^2 - 4 * a * c0
    if (!is.na(disc) && disc >= 0) {
      roots <- c((-b - sqrt(disc)) / (2 * a), (-b + sqrt(disc)) / (2 * a))
    }
  }
  jn_points <- sort(unique(center_w + roots))
  jn_points <- jn_points[is.finite(jn_points) & jn_points >= min(w_obs) & jn_points <= max(w_obs)]

  list(
    plot_df = data.frame(
      moderator_value = w_grid,
      conditional_effect = effect,
      se = se,
      llci = llci,
      ulci = ulci,
      significant = sig_flag,
      stringsAsFactors = FALSE
    ),
    jn_points = jn_points,
    tcrit = tcrit
  )
}

build_observed_prediction_plot_df <- function(model_df, cond_df, dat, x_var, w_var, covariates = character(0), n_points = 100L) {
  if (!is.data.frame(model_df) || nrow(model_df) == 0) return(data.frame())
  if (!is.data.frame(cond_df) || nrow(cond_df) == 0) return(data.frame())
  if (!is.data.frame(dat) || nrow(dat) == 0) return(data.frame())
  if (!all(c(x_var, w_var) %in% names(dat))) return(data.frame())

  x_obs <- safe_num_fig(dat[[x_var]])
  x_obs <- x_obs[!is.na(x_obs)]
  if (length(x_obs) < 2L) return(data.frame())
  x_grid <- seq(min(x_obs), max(x_obs), length.out = n_points)

  wt_vec <- NULL
  weight_var <- as.character(SURVEY_BUNDLE$weight_var %||% "")[1]
  if (nzchar(weight_var) && weight_var %in% names(dat)) wt_vec <- safe_num_fig(dat[[weight_var]])

  center_x <- if (identical(PROCESS_SETTINGS$centering_method %||% "weighted_mean", "none")) {
    0
  } else if (identical(PROCESS_SETTINGS$centering_method %||% "weighted_mean", "mean")) {
    mean(x_obs, na.rm = TRUE)
  } else {
    weighted_mean_fig(dat[[x_var]], wt_vec)
  }

  parsed_levels <- do.call(
    rbind,
    lapply(seq_len(nrow(cond_df)), function(i) {
      parse_probe_level_fig(
        cond_df$class_contrast[i] %||% "",
        fallback_label = cond_df$moderator_label[i] %||% cond_df$moderator[i] %||% "Moderator"
      )
    })
  )
  parsed_levels <- parsed_levels[!duplicated(parsed_levels$probe_level), , drop = FALSE]
  if (nrow(parsed_levels) == 0) return(data.frame())

  mean_row <- parsed_levels[tolower(parsed_levels$probe_level) == "mean", , drop = FALSE]
  center_w <- if (nrow(mean_row) > 0) safe_num_fig(mean_row$raw_value[1]) else {
    if (identical(PROCESS_SETTINGS$centering_method %||% "weighted_mean", "none")) {
      0
    } else if (identical(PROCESS_SETTINGS$centering_method %||% "weighted_mean", "mean")) {
      mean(safe_num_fig(dat[[w_var]]), na.rm = TRUE)
    } else {
      weighted_mean_fig(dat[[w_var]], wt_vec)
    }
  }

  intercept <- safe_num_fig(model_df$estimate[model_df$term == "(Intercept)"][1])
  beta_x <- safe_num_fig(model_df$estimate[model_df$term == x_var][1])
  beta_w <- safe_num_fig(model_df$estimate[model_df$term == w_var][1])
  beta_int <- safe_num_fig(model_df$estimate[model_df$term %in% c(paste0(x_var, ":", w_var), paste0(w_var, ":", x_var))][1])
  if (any(is.na(c(intercept, beta_x, beta_w, beta_int)))) return(data.frame())

  cov_offset <- compute_covariate_offset(model_df, dat, covariates)

  out_list <- lapply(seq_len(nrow(parsed_levels)), function(i) {
    lvl_i <- parsed_levels[i, , drop = FALSE]
    w_raw <- safe_num_fig(lvl_i$raw_value[1])
    w_centered <- if (is.na(w_raw)) 0 else w_raw - center_w
    x_centered <- x_grid - center_x
    data.frame(
      x = x_grid,
      yhat = intercept + cov_offset + beta_x * x_centered + beta_w * w_centered + beta_int * x_centered * w_centered,
      moderator_level = factor(lvl_i$probe_level[1], levels = c("M-SD", "Mean", "M+SD")),
      moderator_label = lvl_i$moderator_label[1],
      moderator_value = ifelse(is.na(w_raw), "", sprintf("%.2f", w_raw)),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, out_list)
  out <- out[order(out$moderator_level, out$x), , drop = FALSE]
  rownames(out) <- NULL
  out
}

build_model7_indirect_plot_df <- function(indirect_df, dat, w_var, n_points = 200L) {
  if (!is.data.frame(indirect_df) || nrow(indirect_df) == 0) return(data.frame())
  if (!is.data.frame(dat) || nrow(dat) == 0 || !w_var %in% names(dat)) return(data.frame())

  parsed <- do.call(
    rbind,
    lapply(seq_len(nrow(indirect_df)), function(i) {
      parse_indirect_probe_level_fig(
        indirect_df$class_contrast[i] %||% "",
        fallback_label = indirect_df$moderator_label[i] %||% indirect_df$moderator[i] %||% "Moderator"
      )
    })
  )

  cond_df <- indirect_df[parsed$probe_level %in% c("M-SD", "Mean", "M+SD") & !is.na(parsed$raw_value), , drop = FALSE]
  parsed_cond <- parsed[parsed$probe_level %in% c("M-SD", "Mean", "M+SD") & !is.na(parsed$raw_value), , drop = FALSE]
  if (nrow(cond_df) == 0) return(data.frame())

  idx_imm <- which(trimws(as.character(indirect_df$class_contrast)) == "Index of moderated mediation")
  imm_val <- if (length(idx_imm) > 0) safe_num_fig(indirect_df$indirect[idx_imm[1]]) else NA_real_

  w_obs <- safe_num_fig(dat[[w_var]])
  w_obs <- w_obs[!is.na(w_obs)]
  if (length(w_obs) < 2L) {
    w_obs <- safe_num_fig(parsed_cond$raw_value)
  }
  if (length(w_obs) < 2L) return(data.frame())
  w_grid <- seq(min(w_obs), max(w_obs), length.out = n_points)

  ord <- order(parsed_cond$raw_value)
  x_vals <- safe_num_fig(parsed_cond$raw_value[ord])
  y_vals <- safe_num_fig(cond_df$indirect[ord])
  ll_vals <- safe_num_fig(cond_df$llci[ord])
  ul_vals <- safe_num_fig(cond_df$ulci[ord])
  se_vals <- safe_num_fig(cond_df$se[ord])

  mean_idx <- which(parsed_cond$probe_level[ord] == "Mean")
  if (length(mean_idx) > 0) {
    w_ref <- x_vals[mean_idx[1]]
    y_ref <- y_vals[mean_idx[1]]
  } else {
    w_ref <- mean(x_vals, na.rm = TRUE)
    y_ref <- mean(y_vals, na.rm = TRUE)
  }

  if (!is.finite(imm_val)) {
    imm_val <- tryCatch(unname(stats::coef(stats::lm(y_vals ~ x_vals))[2]), error = function(e) NA_real_)
  }

  yhat <- if (is.finite(imm_val) && is.finite(w_ref) && is.finite(y_ref)) {
    y_ref + imm_val * (w_grid - w_ref)
  } else if (length(unique(x_vals)) >= 2L) {
    stats::approx(x = x_vals, y = y_vals, xout = w_grid, rule = 2)$y
  } else {
    rep(y_vals[1], length(w_grid))
  }

  llci <- if (length(unique(x_vals)) >= 2L) {
    stats::approx(x = x_vals, y = ll_vals, xout = w_grid, rule = 2)$y
  } else {
    rep(ll_vals[1], length(w_grid))
  }
  ulci <- if (length(unique(x_vals)) >= 2L) {
    stats::approx(x = x_vals, y = ul_vals, xout = w_grid, rule = 2)$y
  } else {
    rep(ul_vals[1], length(w_grid))
  }
  se_hat <- if (length(unique(x_vals)) >= 2L) {
    stats::approx(x = x_vals, y = se_vals, xout = w_grid, rule = 2)$y
  } else {
    rep(se_vals[1], length(w_grid))
  }

  data.frame(
    moderator_value = w_grid,
    indirect_effect = yhat,
    llci = llci,
    ulci = ulci,
    se = se_hat,
    stringsAsFactors = FALSE
  )
}

build_model7_indirect_plot <- function(plot_df, moderator_label, x_label, mediator_label, outcome_label, title) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  if (!is.data.frame(plot_df) || nrow(plot_df) == 0) return(NULL)

  ggplot2::ggplot(plot_df, ggplot2::aes(x = moderator_value, y = indirect_effect)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.35, color = "#666666") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = llci, ymax = ulci), fill = "#B0BEC5", alpha = 0.28, linewidth = 0, color = NA) +
    ggplot2::geom_line(ggplot2::aes(y = llci), linewidth = 0.45, alpha = 0.8, linetype = "22", color = "#607D8B") +
    ggplot2::geom_line(ggplot2::aes(y = ulci), linewidth = 0.45, alpha = 0.8, linetype = "22", color = "#607D8B") +
    ggplot2::geom_line(linewidth = 0.9, color = "#1565C0") +
    ggplot2::labs(
      title = title,
      subtitle = "Shaded band and dashed bounds indicate 95% CI.",
      x = moderator_label,
      y = paste0("Conditional indirect effect of ", x_label, " on ", outcome_label, "\nthrough ", mediator_label)
    ) +
    theme_publication_fig(base_size = 11) +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0, size = 9, lineheight = 1.05),
      plot.subtitle = ggplot2::element_text(hjust = 0, size = 8.5),
      axis.title.x = ggplot2::element_text(size = 8),
      axis.title.y = ggplot2::element_text(size = 8),
      axis.text = ggplot2::element_text(size = 8.5),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}

build_model7_indirect_plot_bw <- function(plot_df, moderator_label, x_label, mediator_label, outcome_label, title) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  if (!is.data.frame(plot_df) || nrow(plot_df) == 0) return(NULL)

  ggplot2::ggplot(plot_df, ggplot2::aes(x = moderator_value, y = indirect_effect)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.35, color = "#666666") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = llci, ymax = ulci), fill = "grey82", alpha = 0.32, linewidth = 0, color = NA) +
    ggplot2::geom_line(ggplot2::aes(y = llci), linewidth = 0.45, alpha = 0.8, linetype = "22", color = "grey35") +
    ggplot2::geom_line(ggplot2::aes(y = ulci), linewidth = 0.45, alpha = 0.8, linetype = "22", color = "grey35") +
    ggplot2::geom_line(linewidth = 0.9, color = "black") +
    ggplot2::labs(
      title = title,
      subtitle = "Shaded band and dashed bounds indicate 95% CI.",
      x = moderator_label,
      y = paste0("Conditional indirect effect of ", x_label, " on ", outcome_label, "\nthrough ", mediator_label)
    ) +
    theme_publication_fig(base_size = 11) +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0, size = 9, lineheight = 1.05),
      plot.subtitle = ggplot2::element_text(hjust = 0, size = 8.5),
      axis.title.x = ggplot2::element_text(size = 8),
      axis.title.y = ggplot2::element_text(size = 8),
      axis.text = ggplot2::element_text(size = 8.5),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}

build_moderation_plot_df <- function(model_df, dat, x_var, outcome_var, ref_class, covariates = character(0), n_points = 100L) {
  if (!is.data.frame(model_df) || nrow(model_df) == 0 || !is.data.frame(dat) || nrow(dat) == 0) return(data.frame())
  if (!all(c(x_var, outcome_var, "class_num") %in% names(dat))) return(data.frame())

  x_obs <- suppressWarnings(as.numeric(dat[[x_var]]))
  x_obs <- x_obs[!is.na(x_obs)]
  if (length(x_obs) < 2L) return(data.frame())
  x_grid <- seq(min(x_obs), max(x_obs), length.out = n_points)

  intercept <- suppressWarnings(as.numeric(model_df$estimate[model_df$term == "(Intercept)"][1]))
  beta_x <- suppressWarnings(as.numeric(model_df$estimate[model_df$term == x_var][1]))
  if (is.na(intercept) || is.na(beta_x)) return(data.frame())

  cov_offset <- compute_covariate_offset(model_df, dat, covariates)
  class_labels <- get_class_labels_fig(dat)
  classes <- sort(unique(stats::na.omit(dat$class_num)))
  if (length(classes) == 0L) return(data.frame())

  out_list <- vector("list", length(classes))
  for (i in seq_along(classes)) {
    cls <- classes[i]
    class_term <- paste0("class_fac", cls)
    int_term_1 <- paste0("class_fac", cls, ":", x_var)
    int_term_2 <- paste0(x_var, ":class_fac", cls)
    beta_class <- if (identical(cls, ref_class)) 0 else suppressWarnings(as.numeric(model_df$estimate[model_df$term == class_term][1]))
    beta_int <- if (identical(cls, ref_class)) 0 else suppressWarnings(as.numeric(model_df$estimate[model_df$term %in% c(int_term_1, int_term_2)][1]))
    if (is.na(beta_class)) beta_class <- 0
    if (is.na(beta_int)) beta_int <- 0

    class_label_i <- paste0("Class ", cls)
    if (is.data.frame(class_labels) && nrow(class_labels) > 0) {
      hit <- class_labels$class_label[class_labels$class_num == cls]
      if (length(hit) > 0 && !is.na(hit[1]) && nzchar(hit[1])) class_label_i <- paste0("Profile ", cls)
    }
    out_list[[i]] <- data.frame(
      x = x_grid,
      yhat = intercept + cov_offset + beta_class + (beta_x + beta_int) * x_grid,
      class_num = cls,
      class_label = class_label_i,
      is_reference = identical(cls, ref_class),
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, out_list)
  rownames(out) <- NULL
  out
}

build_process_moderation_plot <- function(plot_df, x_label, y_label, moderator_label, title) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  if (!is.data.frame(plot_df) || nrow(plot_df) == 0) return(NULL)

  palette_vals <- c("#1B5E20", "#1565C0", "#D84315", "#6A1B9A", "#455A64", "#8E24AA")
  n_cls <- length(unique(plot_df$class_label))
  palette_vals <- rep(palette_vals, length.out = n_cls)
  line_types <- ifelse(plot_df$is_reference, "solid", "longdash")
  names(line_types) <- plot_df$class_label

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = x, y = yhat, color = class_label, linetype = class_label)
  ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_color_manual(values = palette_vals) +
    ggplot2::scale_linetype_manual(values = stats::setNames(line_types[!duplicated(names(line_types))], names(line_types)[!duplicated(names(line_types))])) +
    ggplot2::labs(
      title = title,
      x = x_label,
      y = y_label,
      color = moderator_label,
      linetype = moderator_label
    ) +
    theme_publication_fig(base_size = 11) +
    ggplot2::theme(
      legend.position = "right",
      plot.title = ggplot2::element_text(hjust = 0, size = 9, lineheight = 1.05),
      axis.title.x = ggplot2::element_text(size = 8),
      axis.title.y = ggplot2::element_text(size = 8),
      axis.text = ggplot2::element_text(size = 8.5),
      legend.title = ggplot2::element_text(size = 9),
      legend.text = ggplot2::element_text(size = 8.5),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}

build_conditional_effect_plot <- function(df, x_label, y_label, moderator_label, title, ref_class) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  if (!is.data.frame(df) || nrow(df) == 0) return(NULL)

  df <- as.data.frame(df, stringsAsFactors = FALSE)
  df$profile_label <- paste0("Profile ", df$class_num)
  df$profile_label <- factor(df$profile_label, levels = paste0("Profile ", df$class_num[order(df$class_num)]))
  df$is_reference <- df$class_num == ref_class

  ggplot2::ggplot(df, ggplot2::aes(x = profile_label, y = conditional_effect, color = is_reference)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.35, color = "#666666") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = llci, ymax = ulci), width = 0.10, linewidth = 0.7) +
    ggplot2::geom_point(size = 2.6) +
    ggplot2::scale_color_manual(values = c("TRUE" = "#1B5E20", "FALSE" = "#1565C0"), guide = "none") +
    ggplot2::labs(
      title = title,
      x = moderator_label,
      y = paste0("Conditional effect of ", x_label, " on ", y_label)
    ) +
    theme_publication_fig(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0, size = 9, lineheight = 1.05),
      axis.title.x = ggplot2::element_text(size = 8),
      axis.title.y = ggplot2::element_text(size = 8),
      axis.text = ggplot2::element_text(size = 8.5),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}

build_observed_prediction_plot <- function(plot_df, x_label, y_label, moderator_label, title) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  if (!is.data.frame(plot_df) || nrow(plot_df) == 0) return(NULL)

  plot_df <- as.data.frame(plot_df, stringsAsFactors = FALSE)
  plot_df$moderator_level <- factor(as.character(plot_df$moderator_level), levels = c("M-SD", "Mean", "M+SD"))

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = x, y = yhat, color = moderator_level)
  ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_color_manual(
      values = c("M-SD" = "#1B5E20", "Mean" = "#1565C0", "M+SD" = "#D84315"),
      drop = FALSE
    ) +
    ggplot2::labs(
      title = title,
      x = x_label,
      y = y_label,
      color = moderator_label
    ) +
    theme_publication_fig(base_size = 11) +
    ggplot2::theme(
      legend.position = "right",
      plot.title = ggplot2::element_text(hjust = 0, size = 9, lineheight = 1.05),
      axis.title.x = ggplot2::element_text(size = 8),
      axis.title.y = ggplot2::element_text(size = 8),
      axis.text = ggplot2::element_text(size = 8.5),
      legend.title = ggplot2::element_text(size = 9),
      legend.text = ggplot2::element_text(size = 8.5),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}

build_observed_prediction_ci_plot <- function(plot_df, x_label, y_label, moderator_label, title, subtitle = "Shaded bands and dashed bounds indicate 95% CI.") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  if (!is.data.frame(plot_df) || nrow(plot_df) == 0) return(NULL)

  plot_df <- as.data.frame(plot_df, stringsAsFactors = FALSE)
  plot_df$moderator_level <- factor(as.character(plot_df$moderator_level), levels = c("M-SD", "Mean", "M+SD"))
  line_vals <- c("M-SD" = "#1B5E20", "Mean" = "#1565C0", "M+SD" = "#D84315")

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = x, y = yhat, color = moderator_level, fill = moderator_level)
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = llci, ymax = ulci),
      alpha = 0.13,
      linewidth = 0,
      color = NA,
      show.legend = FALSE
    ) +
    ggplot2::geom_line(ggplot2::aes(y = llci), linewidth = 0.45, alpha = 0.7, linetype = "22", show.legend = FALSE) +
    ggplot2::geom_line(ggplot2::aes(y = ulci), linewidth = 0.45, alpha = 0.7, linetype = "22", show.legend = FALSE) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_color_manual(values = line_vals, drop = FALSE) +
    ggplot2::scale_fill_manual(values = line_vals, drop = FALSE) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = x_label,
      y = y_label,
      color = moderator_label
    ) +
    theme_publication_fig(base_size = 11) +
    ggplot2::theme(
      legend.position = "right",
      plot.title = ggplot2::element_text(hjust = 0, size = 9, lineheight = 1.05),
      plot.subtitle = ggplot2::element_text(hjust = 0, size = 8.5),
      axis.title.x = ggplot2::element_text(size = 8),
      axis.title.y = ggplot2::element_text(size = 8),
      axis.text = ggplot2::element_text(size = 8.5),
      legend.title = ggplot2::element_text(size = 9),
      legend.text = ggplot2::element_text(size = 8.5),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}

build_observed_prediction_plot_bw <- function(plot_df, x_label, y_label, moderator_label, title) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  if (!is.data.frame(plot_df) || nrow(plot_df) == 0) return(NULL)

  plot_df <- as.data.frame(plot_df, stringsAsFactors = FALSE)
  plot_df$moderator_level <- factor(as.character(plot_df$moderator_level), levels = c("M-SD", "Mean", "M+SD"))
  line_types <- c("M-SD" = "22", "Mean" = "solid", "M+SD" = "42")

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = x, y = yhat, linetype = moderator_level)
  ) +
    ggplot2::geom_line(linewidth = 0.8, color = "black") +
    ggplot2::scale_linetype_manual(values = line_types, drop = FALSE) +
    ggplot2::labs(
      title = title,
      x = x_label,
      y = y_label,
      linetype = moderator_label
    ) +
    theme_publication_fig(base_size = 11) +
    ggplot2::theme(
      legend.position = "right",
      plot.title = ggplot2::element_text(hjust = 0, size = 9, lineheight = 1.05),
      axis.title.x = ggplot2::element_text(size = 8),
      axis.title.y = ggplot2::element_text(size = 8),
      axis.text = ggplot2::element_text(size = 8.5),
      legend.title = ggplot2::element_text(size = 9),
      legend.text = ggplot2::element_text(size = 8.5),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}

build_observed_prediction_ci_plot_bw <- function(plot_df, x_label, y_label, moderator_label, title, subtitle = "Shaded bands and dashed bounds indicate 95% CI.") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  if (!is.data.frame(plot_df) || nrow(plot_df) == 0) return(NULL)

  plot_df <- as.data.frame(plot_df, stringsAsFactors = FALSE)
  plot_df$moderator_level <- factor(as.character(plot_df$moderator_level), levels = c("M-SD", "Mean", "M+SD"))
  line_types <- c("M-SD" = "22", "Mean" = "solid", "M+SD" = "42")
  fill_vals <- c("M-SD" = "grey88", "Mean" = "grey78", "M+SD" = "grey68")

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = x, y = yhat, linetype = moderator_level, fill = moderator_level)
  ) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = llci, ymax = ulci), alpha = 0.35, linewidth = 0, color = NA, show.legend = FALSE) +
    ggplot2::geom_line(ggplot2::aes(y = llci), linewidth = 0.4, alpha = 0.7, linetype = "22", color = "grey35", show.legend = FALSE) +
    ggplot2::geom_line(ggplot2::aes(y = ulci), linewidth = 0.4, alpha = 0.7, linetype = "22", color = "grey35", show.legend = FALSE) +
    ggplot2::geom_line(linewidth = 0.8, color = "black") +
    ggplot2::scale_linetype_manual(values = line_types, drop = FALSE) +
    ggplot2::scale_fill_manual(values = fill_vals, drop = FALSE) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = x_label,
      y = y_label,
      linetype = moderator_label
    ) +
    theme_publication_fig(base_size = 11) +
    ggplot2::theme(
      legend.position = "right",
      plot.title = ggplot2::element_text(hjust = 0, size = 9, lineheight = 1.05),
      plot.subtitle = ggplot2::element_text(hjust = 0, size = 8.5),
      axis.title.x = ggplot2::element_text(size = 8),
      axis.title.y = ggplot2::element_text(size = 8),
      axis.text = ggplot2::element_text(size = 8.5),
      legend.title = ggplot2::element_text(size = 9),
      legend.text = ggplot2::element_text(size = 8.5),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}

build_jn_plot <- function(plot_df, moderator_label, x_label, y_label, title, jn_points = numeric(0)) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  if (!is.data.frame(plot_df) || nrow(plot_df) == 0) return(NULL)

  plot_df <- as.data.frame(plot_df, stringsAsFactors = FALSE)
  plot_df$sig_dir <- ifelse(
    plot_df$ulci < 0, "negative",
    ifelse(plot_df$llci > 0, "positive", "nonsignificant")
  )

  rect_df <- data.frame()
  r <- rle(as.character(plot_df$sig_dir))
  ends <- cumsum(r$lengths)
  starts <- c(1, head(ends + 1, -1))
  for (i in seq_along(r$values)) {
    if (identical(r$values[i], "nonsignificant")) next
    seg <- plot_df[starts[i]:ends[i], , drop = FALSE]
    if (nrow(seg) == 0) next
    rect_df <- rbind(
      rect_df,
      data.frame(
        xmin = min(seg$moderator_value, na.rm = TRUE),
        xmax = max(seg$moderator_value, na.rm = TRUE),
        ymin = -Inf,
        ymax = Inf,
        sig_dir = r$values[i],
        stringsAsFactors = FALSE
      )
    )
  }

  subtitle <- if (length(jn_points) == 0) {
    "No Johnson-Neyman transition point within the observed moderator range."
  } else {
    paste0(
      "Johnson-Neyman point",
      if (length(jn_points) > 1) "s" else "",
      ": ",
      paste(formatC(jn_points, format = "f", digits = 2), collapse = ", ")
    )
  }

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = moderator_value, y = conditional_effect)
  ) +
    {
      if (nrow(rect_df) > 0) {
        ggplot2::geom_rect(
          data = rect_df,
          ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = sig_dir),
          inherit.aes = FALSE,
          alpha = 0.16,
          color = NA,
          show.legend = FALSE
        )
      }
    } +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.35, color = "#666666") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = llci, ymax = ulci), fill = "#B0BEC5", alpha = 0.28, linewidth = 0, color = NA) +
    ggplot2::geom_line(ggplot2::aes(y = llci), linewidth = 0.45, alpha = 0.8, linetype = "22", color = "#607D8B") +
    ggplot2::geom_line(ggplot2::aes(y = ulci), linewidth = 0.45, alpha = 0.8, linetype = "22", color = "#607D8B") +
    ggplot2::geom_line(linewidth = 0.9, color = "#1565C0") +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = moderator_label,
      y = paste0("Conditional effect of ", x_label, " on ", y_label)
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    theme_publication_fig(base_size = 11) +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0, size = 9, lineheight = 1.05),
      plot.subtitle = ggplot2::element_text(hjust = 0, size = 8.5),
      axis.title.x = ggplot2::element_text(size = 8),
      axis.title.y = ggplot2::element_text(size = 8),
      axis.text = ggplot2::element_text(size = 8.5),
      plot.margin = ggplot2::margin(5.5, 5.5, 16, 5.5),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
  if (nrow(rect_df) > 0) {
    p <- p + ggplot2::scale_fill_manual(
      values = c("negative" = "#BBDEFB", "positive" = "#FFE0B2", "nonsignificant" = "transparent"),
      drop = FALSE
    )
  }

  if (length(jn_points) > 0) {
    y_rng <- range(c(plot_df$llci, plot_df$ulci), na.rm = TRUE)
    y_span <- diff(y_rng)
    if (!is.finite(y_span) || y_span <= 0) y_span <- max(abs(y_rng), na.rm = TRUE)
    if (!is.finite(y_span) || y_span <= 0) y_span <- 1
    y_label <- y_rng[1] + 0.03 * y_span
    x_rng <- range(plot_df$moderator_value, na.rm = TRUE)
    x_span <- diff(x_rng)
    if (!is.finite(x_span) || x_span <= 0) x_span <- 1
    x_nudge <- 0.03 * x_span

    ann_df <- do.call(
      rbind,
      lapply(jn_points, function(jp) {
        left_df <- plot_df[plot_df$moderator_value < jp, , drop = FALSE]
        right_df <- plot_df[plot_df$moderator_value > jp, , drop = FALSE]
        left_sig <- if (nrow(left_df) > 0) as.character(utils::tail(left_df$sig_dir, 1)) else "nonsignificant"
        right_sig <- if (nrow(right_df) > 0) as.character(right_df$sig_dir[1]) else "nonsignificant"

        if (identical(right_sig, "positive")) {
          x_text <- jp + x_nudge
          hjust_i <- 0
        } else if (identical(left_sig, "negative")) {
          x_text <- jp - x_nudge
          hjust_i <- 1
        } else {
          x_text <- jp + x_nudge
          hjust_i <- 0
        }

        data.frame(
          x = x_text,
          y = y_label,
          hjust = hjust_i,
          lab = paste0("JN=", formatC(jp, format = "f", digits = 2)),
          stringsAsFactors = FALSE
        )
      })
    )
    p <- p +
      ggplot2::geom_vline(xintercept = jn_points, linewidth = 0.45, linetype = "42", color = "#424242") +
      ggplot2::geom_text(
        data = ann_df,
        ggplot2::aes(x = x, y = y, label = lab, hjust = hjust),
        inherit.aes = FALSE,
        vjust = 1,
        size = 2.8,
        color = "#424242"
      )
  }

  p
}

build_jn_plot_bw <- function(plot_df, moderator_label, x_label, y_label, title, jn_points = numeric(0)) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  if (!is.data.frame(plot_df) || nrow(plot_df) == 0) return(NULL)

  plot_df <- as.data.frame(plot_df, stringsAsFactors = FALSE)
  plot_df$sig_dir <- ifelse(
    plot_df$ulci < 0, "negative",
    ifelse(plot_df$llci > 0, "positive", "nonsignificant")
  )

  rect_df <- data.frame()
  r <- rle(as.character(plot_df$sig_dir))
  ends <- cumsum(r$lengths)
  starts <- c(1, head(ends + 1, -1))
  for (i in seq_along(r$values)) {
    if (identical(r$values[i], "nonsignificant")) next
    seg <- plot_df[starts[i]:ends[i], , drop = FALSE]
    if (nrow(seg) == 0) next
    rect_df <- rbind(
      rect_df,
      data.frame(
        xmin = min(seg$moderator_value, na.rm = TRUE),
        xmax = max(seg$moderator_value, na.rm = TRUE),
        ymin = -Inf,
        ymax = Inf,
        sig_dir = r$values[i],
        stringsAsFactors = FALSE
      )
    )
  }

  subtitle <- if (length(jn_points) == 0) {
    "No Johnson-Neyman transition point within the observed moderator range."
  } else {
    paste0(
      "Johnson-Neyman point",
      if (length(jn_points) > 1) "s" else "",
      ": ",
      paste(formatC(jn_points, format = "f", digits = 2), collapse = ", ")
    )
  }

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = moderator_value, y = conditional_effect)
  ) +
    {
      if (nrow(rect_df) > 0) {
        ggplot2::geom_rect(
          data = rect_df,
          ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = sig_dir),
          inherit.aes = FALSE,
          alpha = 0.18,
          color = NA,
          show.legend = FALSE
        )
      }
    } +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.35, color = "#666666") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = llci, ymax = ulci), fill = "grey82", alpha = 0.32, linewidth = 0, color = NA) +
    ggplot2::geom_line(ggplot2::aes(y = llci), linewidth = 0.45, alpha = 0.8, linetype = "22", color = "grey35") +
    ggplot2::geom_line(ggplot2::aes(y = ulci), linewidth = 0.45, alpha = 0.8, linetype = "22", color = "grey35") +
    ggplot2::geom_line(linewidth = 0.9, color = "black") +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = moderator_label,
      y = paste0("Conditional effect of ", x_label, " on ", y_label)
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    theme_publication_fig(base_size = 11) +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0, size = 9, lineheight = 1.05),
      plot.subtitle = ggplot2::element_text(hjust = 0, size = 8.5),
      axis.title.x = ggplot2::element_text(size = 8),
      axis.title.y = ggplot2::element_text(size = 8),
      axis.text = ggplot2::element_text(size = 8.5),
      plot.margin = ggplot2::margin(5.5, 5.5, 16, 5.5),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
  if (nrow(rect_df) > 0) {
    p <- p + ggplot2::scale_fill_manual(
      values = c("negative" = "grey88", "positive" = "grey94", "nonsignificant" = "transparent"),
      drop = FALSE
    )
  }

  if (length(jn_points) > 0) {
    y_rng <- range(c(plot_df$llci, plot_df$ulci), na.rm = TRUE)
    y_span <- diff(y_rng)
    if (!is.finite(y_span) || y_span <= 0) y_span <- max(abs(y_rng), na.rm = TRUE)
    if (!is.finite(y_span) || y_span <= 0) y_span <- 1
    y_label <- y_rng[1] + 0.03 * y_span
    x_rng <- range(plot_df$moderator_value, na.rm = TRUE)
    x_span <- diff(x_rng)
    if (!is.finite(x_span) || x_span <= 0) x_span <- 1
    x_nudge <- 0.03 * x_span

    ann_df <- do.call(
      rbind,
      lapply(jn_points, function(jp) {
        left_df <- plot_df[plot_df$moderator_value < jp, , drop = FALSE]
        right_df <- plot_df[plot_df$moderator_value > jp, , drop = FALSE]
        left_sig <- if (nrow(left_df) > 0) as.character(utils::tail(left_df$sig_dir, 1)) else "nonsignificant"
        right_sig <- if (nrow(right_df) > 0) as.character(right_df$sig_dir[1]) else "nonsignificant"

        if (identical(right_sig, "positive")) {
          x_text <- jp + x_nudge
          hjust_i <- 0
        } else if (identical(left_sig, "negative")) {
          x_text <- jp - x_nudge
          hjust_i <- 1
        } else {
          x_text <- jp + x_nudge
          hjust_i <- 0
        }

        data.frame(
          x = x_text,
          y = y_label,
          hjust = hjust_i,
          lab = paste0("JN=", formatC(jp, format = "f", digits = 2)),
          stringsAsFactors = FALSE
        )
      })
    )
    p <- p +
      ggplot2::geom_vline(xintercept = jn_points, linewidth = 0.45, linetype = "42", color = "#424242") +
      ggplot2::geom_text(
        data = ann_df,
        ggplot2::aes(x = x, y = y, label = lab, hjust = hjust),
        inherit.aes = FALSE,
        vjust = 1,
        size = 2.8,
        color = "#424242"
      )
  }

  p
}

figure_rows <- list()

if (isTRUE(PROCESS_SETTINGS$custom_model_enabled) &&
    is.data.frame(PROCESS_MODEL_RESULTS) &&
    nrow(PROCESS_MODEL_RESULTS) > 0) {
  path_plot_i <- build_custom_path_plot(PROCESS_MODEL_RESULTS, bw = FALSE)
  if (!is.null(path_plot_i)) {
    row_i <- save_plot_all_formats(
      plot_obj = path_plot_i,
      file_stub = "fig1_custom_path_diagram",
      dir_png = DIR_FIGURES_PNG,
      dir_tiff = DIR_FIGURES_TIFF,
      dir_pdf = DIR_FIGURES_PDF,
      width = 8.0,
      height = 4.8,
      dpi = 600,
      figure_title = "Estimated Path Model for the Custom Moderated Mediation Analysis"
    )
    figure_rows[[length(figure_rows) + 1L]] <- row_i
  }

  path_plot_bw_i <- build_custom_path_plot(PROCESS_MODEL_RESULTS, bw = TRUE)
  if (!is.null(path_plot_bw_i)) {
    row_bw <- save_plot_all_formats(
      plot_obj = path_plot_bw_i,
      file_stub = "fig2_custom_path_diagram_bw",
      dir_png = DIR_FIGURES_PNG,
      dir_tiff = DIR_FIGURES_TIFF,
      dir_pdf = DIR_FIGURES_PDF,
      width = 8.0,
      height = 4.8,
      dpi = 600,
      figure_title = "Estimated Path Model for the Custom Moderated Mediation Analysis (B/W)"
    )
    figure_rows[[length(figure_rows) + 1L]] <- row_bw
  }
}

if (isTRUE(PROCESS_SETTINGS$process_model == 1L) &&
    identical(PROCESS_SETTINGS$moderator_source %||% "latent_class", "latent_class") &&
    is.data.frame(PROCESS_MODEL_RESULTS) &&
    nrow(PROCESS_MODEL_RESULTS) > 0 &&
    is.data.frame(PROCESS_DATA) &&
    nrow(PROCESS_DATA) > 0) {
  combos <- unique(PROCESS_MODEL_RESULTS[, c("outcome", "outcome_label", "x_var", "x_label"), drop = FALSE])
  figure_idx <- 1L

  for (i in seq_len(nrow(combos))) {
    combo_i <- combos[i, , drop = FALSE]
    outcome_i <- as.character(combo_i$outcome[1])
    outcome_label_i <- as.character(combo_i$outcome_label[1] %||% lookup_var_label_fig(outcome_i))
    x_var_i <- as.character(combo_i$x_var[1])
    x_label_i <- as.character(combo_i$x_label[1] %||% lookup_var_label_fig(x_var_i))

    coef_i <- PROCESS_MODEL_RESULTS[
      as.character(PROCESS_MODEL_RESULTS$outcome) == outcome_i,
      ,
      drop = FALSE
    ]
    if (nrow(coef_i) == 0) next

    plot_df_i <- build_moderation_plot_df(
      model_df = coef_i,
      dat = PROCESS_DATA,
      x_var = x_var_i,
      outcome_var = outcome_i,
      ref_class = SOURCE_REFERENCE_CLASS,
      covariates = PROCESS_SETTINGS$covariates %||% character(0),
      n_points = 100L
    )
    if (nrow(plot_df_i) == 0) next

    title_i <- paste0(
      "Moderating effect of profile on the association\nbetween ",
      x_label_i,
      " and ",
      outcome_label_i
    )
    moderator_label_i <- if (identical(PROCESS_SETTINGS$moderator_source %||% "latent_class", "latent_class")) {
      "Latent profile"
    } else {
      as.character(coef_i$moderator_label[1] %||% "Moderator")
    }

    plot_i <- build_process_moderation_plot(
      plot_df = plot_df_i,
      x_label = x_label_i,
      y_label = paste0("Predicted ", outcome_label_i),
      moderator_label = moderator_label_i,
      title = title_i
    )
    if (is.null(plot_i)) next

    file_stub_i <- paste0("fig", figure_idx, "_moderation_", outcome_i, "_", x_var_i)
    row_i <- save_plot_all_formats(
      plot_obj = plot_i,
      file_stub = file_stub_i,
      dir_png = DIR_FIGURES_PNG,
      dir_tiff = DIR_FIGURES_TIFF,
      dir_pdf = DIR_FIGURES_PDF,
      width = 7.2,
      height = 5.4,
      dpi = 600,
      figure_title = title_i
    )
    figure_rows[[length(figure_rows) + 1L]] <- row_i
    figure_idx <- figure_idx + 1L
  }
}

if (isTRUE(PROCESS_SETTINGS$process_model %in% c(1L, 5L, 7L, 8L, 14L, 15L, 58L, 59L)) &&
    (isTRUE(PROCESS_SETTINGS$process_model %in% c(5L, 7L, 8L, 14L, 15L, 58L, 59L)) ||
      !identical(PROCESS_SETTINGS$moderator_source %||% "latent_class", "latent_class")) &&
    is.data.frame(PROCESS_CONDITIONAL_EFFECTS) &&
    nrow(PROCESS_CONDITIONAL_EFFECTS) > 0 &&
    is.data.frame(PROCESS_MODEL_RESULTS) &&
    nrow(PROCESS_MODEL_RESULTS) > 0 &&
    is.data.frame(PROCESS_DATA) &&
    nrow(PROCESS_DATA) > 0) {
  combo_cols_obs <- c("outcome", "outcome_label", "x_var", "x_label", "moderator", "moderator_label")
  if (isTRUE(PROCESS_SETTINGS$process_model %in% c(5L, 7L, 8L, 14L, 15L, 58L, 59L)) && all(c("mediator", "mediator_label") %in% names(PROCESS_CONDITIONAL_EFFECTS))) {
    combo_cols_obs <- c(combo_cols_obs, "mediator", "mediator_label")
  }
  combos_obs <- unique(PROCESS_CONDITIONAL_EFFECTS[, combo_cols_obs, drop = FALSE])
  start_idx <- length(figure_rows) + 1L

  for (i in seq_len(nrow(combos_obs))) {
    combo_i <- combos_obs[i, , drop = FALSE]
    outcome_i <- as.character(combo_i$outcome[1])
    outcome_label_i <- as.character(combo_i$outcome_label[1] %||% lookup_var_label_fig(outcome_i))
    x_var_i <- as.character(combo_i$x_var[1])
    x_label_i <- as.character(combo_i$x_label[1] %||% lookup_var_label_fig(x_var_i))
    w_var_i <- as.character(combo_i$moderator[1] %||% PROCESS_SETTINGS$w_var %||% "W")
    w_label_i <- as.character(combo_i$moderator_label[1] %||% lookup_var_label_fig(w_var_i))
    mediator_i <- as.character(combo_i$mediator[1] %||% "")
    mediator_label_i <- as.character(combo_i$mediator_label[1] %||% lookup_var_label_fig(mediator_i))
    mediator_stub_i <- sanitize_token_fig(mediator_i)

    coef_i <- if (isTRUE(PROCESS_SETTINGS$process_model == 5L)) {
      PROCESS_MODEL_RESULTS[
        as.character(PROCESS_MODEL_RESULTS$outcome) == outcome_i &
          as.character(PROCESS_MODEL_RESULTS$x_var) == x_var_i &
          as.character(PROCESS_MODEL_RESULTS$mediator) == mediator_i &
          as.character(PROCESS_MODEL_RESULTS$model_component) == "Outcome model",
        ,
        drop = FALSE
      ]
    } else if (isTRUE(PROCESS_SETTINGS$process_model == 7L)) {
      PROCESS_MODEL_RESULTS[
        as.character(PROCESS_MODEL_RESULTS$outcome) == outcome_i &
          as.character(PROCESS_MODEL_RESULTS$x_var) == x_var_i &
          as.character(PROCESS_MODEL_RESULTS$mediator) == mediator_i &
          as.character(PROCESS_MODEL_RESULTS$model_component) == "Mediator model",
        ,
        drop = FALSE
      ]
    } else if (isTRUE(PROCESS_SETTINGS$process_model == 8L)) {
      if (identical(outcome_i, mediator_i)) {
        PROCESS_MODEL_RESULTS[
          as.character(PROCESS_MODEL_RESULTS$outcome) == outcome_i &
            as.character(PROCESS_MODEL_RESULTS$mediator) == mediator_i &
            as.character(PROCESS_MODEL_RESULTS$model_component) == "Mediator model",
          ,
          drop = FALSE
        ]
      } else {
        PROCESS_MODEL_RESULTS[
          as.character(PROCESS_MODEL_RESULTS$target_outcome %||% PROCESS_MODEL_RESULTS$outcome) == outcome_i &
            as.character(PROCESS_MODEL_RESULTS$model_component) == "Outcome model",
          ,
          drop = FALSE
        ]
      }
    } else if (isTRUE(PROCESS_SETTINGS$process_model == 14L)) {
      PROCESS_MODEL_RESULTS[
        as.character(PROCESS_MODEL_RESULTS$target_outcome %||% PROCESS_MODEL_RESULTS$outcome) == outcome_i &
          as.character(PROCESS_MODEL_RESULTS$model_component) == "Outcome model",
        ,
        drop = FALSE
      ]
    } else if (isTRUE(PROCESS_SETTINGS$process_model == 15L)) {
      PROCESS_MODEL_RESULTS[
        as.character(PROCESS_MODEL_RESULTS$target_outcome %||% PROCESS_MODEL_RESULTS$outcome) == outcome_i &
          as.character(PROCESS_MODEL_RESULTS$model_component) == "Outcome model",
        ,
        drop = FALSE
      ]
    } else if (isTRUE(PROCESS_SETTINGS$process_model == 58L) || isTRUE(PROCESS_SETTINGS$process_model == 59L)) {
      if (identical(outcome_i, mediator_i)) {
        PROCESS_MODEL_RESULTS[
          as.character(PROCESS_MODEL_RESULTS$outcome) == outcome_i &
            as.character(PROCESS_MODEL_RESULTS$mediator) == mediator_i &
            as.character(PROCESS_MODEL_RESULTS$model_component) == "Mediator model",
          ,
          drop = FALSE
        ]
      } else {
        PROCESS_MODEL_RESULTS[
          as.character(PROCESS_MODEL_RESULTS$target_outcome %||% PROCESS_MODEL_RESULTS$outcome) == outcome_i &
            as.character(PROCESS_MODEL_RESULTS$model_component) == "Outcome model",
          ,
          drop = FALSE
        ]
      }
    } else {
      PROCESS_MODEL_RESULTS[
        as.character(PROCESS_MODEL_RESULTS$outcome) == outcome_i,
        ,
        drop = FALSE
      ]
    }

    cond_i <- PROCESS_CONDITIONAL_EFFECTS[
      as.character(PROCESS_CONDITIONAL_EFFECTS$outcome) == outcome_i &
        as.character(PROCESS_CONDITIONAL_EFFECTS$x_var) == x_var_i &
        if (isTRUE(PROCESS_SETTINGS$process_model %in% c(5L, 7L, 8L, 14L, 15L, 58L, 59L)) && "mediator" %in% names(PROCESS_CONDITIONAL_EFFECTS)) {
          as.character(PROCESS_CONDITIONAL_EFFECTS$mediator) == mediator_i
        } else {
          TRUE
        },
      ,
      drop = FALSE
    ]
    if (nrow(coef_i) == 0 || nrow(cond_i) == 0) next

    figure_covariates_i <- PROCESS_SETTINGS$covariates %||% character(0)
    if (isTRUE(PROCESS_SETTINGS$process_model == 5L) && nzchar(mediator_i)) {
      mediator_terms_i <- unique(strsplit(mediator_i, "\\|", perl = TRUE)[[1]])
      mediator_terms_i <- mediator_terms_i[nzchar(mediator_terms_i)]
      figure_covariates_i <- unique(c(mediator_terms_i, figure_covariates_i))
    } else if (isTRUE(PROCESS_SETTINGS$process_model == 8L) && identical(outcome_i, mediator_i)) {
      figure_covariates_i <- unique(figure_covariates_i)
    } else if (isTRUE(PROCESS_SETTINGS$process_model == 8L) && nzchar(mediator_i)) {
      figure_covariates_i <- unique(c(PROCESS_SETTINGS$m_vars %||% character(0), figure_covariates_i))
    } else if (isTRUE(PROCESS_SETTINGS$process_model == 14L) && nzchar(mediator_i)) {
      other_mediators_i <- setdiff(PROCESS_SETTINGS$m_vars %||% character(0), mediator_i)
      figure_covariates_i <- unique(c(PROCESS_SETTINGS$x_vars %||% character(0), other_mediators_i, figure_covariates_i))
    } else if (isTRUE(PROCESS_SETTINGS$process_model == 15L) && nzchar(mediator_i) && x_var_i %in% (PROCESS_SETTINGS$m_vars %||% character(0))) {
      other_mediators_i <- setdiff(PROCESS_SETTINGS$m_vars %||% character(0), x_var_i)
      figure_covariates_i <- unique(c(PROCESS_SETTINGS$x_vars %||% character(0), other_mediators_i, figure_covariates_i))
    } else if (isTRUE(PROCESS_SETTINGS$process_model == 15L) && nzchar(mediator_i)) {
      figure_covariates_i <- unique(c(PROCESS_SETTINGS$m_vars %||% character(0), figure_covariates_i))
    } else if (isTRUE(PROCESS_SETTINGS$process_model == 58L || PROCESS_SETTINGS$process_model == 59L) && nzchar(mediator_i) && x_var_i %in% (PROCESS_SETTINGS$m_vars %||% character(0))) {
      other_mediators_i <- setdiff(PROCESS_SETTINGS$m_vars %||% character(0), x_var_i)
      figure_covariates_i <- unique(c(PROCESS_SETTINGS$x_vars %||% character(0), other_mediators_i, figure_covariates_i))
    } else if (isTRUE(PROCESS_SETTINGS$process_model == 59L) && nzchar(mediator_i)) {
      figure_covariates_i <- unique(c(PROCESS_SETTINGS$m_vars %||% character(0), figure_covariates_i))
    }

    plot_df_i <- build_observed_prediction_plot_df(
      model_df = coef_i,
      cond_df = cond_i,
      dat = PROCESS_DATA,
      x_var = x_var_i,
      w_var = w_var_i,
      covariates = figure_covariates_i,
      n_points = 100L
    )
    if (nrow(plot_df_i) == 0) next

    title_i <- paste0(
      "Conditional effect of ",
      x_label_i,
      " on ",
      outcome_label_i,
      "\nat values of ",
      w_label_i
    )
    plot_i <- build_observed_prediction_plot(
      plot_df = plot_df_i,
      x_label = x_label_i,
      y_label = paste0("Predicted ", outcome_label_i),
      moderator_label = w_label_i,
      title = title_i
    )
    if (is.null(plot_i)) next

    file_stub_i <- if (isTRUE(PROCESS_SETTINGS$process_model %in% c(5L, 7L, 8L, 14L, 15L, 58L, 59L)) && nzchar(mediator_i)) {
      paste0("fig", start_idx, "_predicted_", outcome_i, "_", x_var_i, "_", mediator_stub_i)
    } else {
      paste0("fig", start_idx, "_predicted_", outcome_i, "_", x_var_i)
    }
    row_i <- save_plot_all_formats(
      plot_obj = plot_i,
      file_stub = file_stub_i,
      dir_png = DIR_FIGURES_PNG,
      dir_tiff = DIR_FIGURES_TIFF,
      dir_pdf = DIR_FIGURES_PDF,
      width = 6.8,
      height = 4.8,
      dpi = 600,
      figure_title = title_i
    )
    figure_rows[[length(figure_rows) + 1L]] <- row_i
    start_idx <- start_idx + 1L

    plot_bw_i <- build_observed_prediction_plot_bw(
      plot_df = plot_df_i,
      x_label = x_label_i,
      y_label = paste0("Predicted ", outcome_label_i),
      moderator_label = w_label_i,
      title = title_i
    )
    if (!is.null(plot_bw_i)) {
      file_stub_bw <- if (isTRUE(PROCESS_SETTINGS$process_model %in% c(5L, 7L, 8L, 14L, 15L, 58L, 59L)) && nzchar(mediator_i)) {
        paste0("fig", start_idx, "_predicted_bw_", outcome_i, "_", x_var_i, "_", mediator_stub_i)
      } else {
        paste0("fig", start_idx, "_predicted_bw_", outcome_i, "_", x_var_i)
      }
      row_bw <- save_plot_all_formats(
        plot_obj = plot_bw_i,
        file_stub = file_stub_bw,
        dir_png = DIR_FIGURES_PNG,
        dir_tiff = DIR_FIGURES_TIFF,
        dir_pdf = DIR_FIGURES_PDF,
        width = 6.8,
        height = 4.8,
        dpi = 600,
        figure_title = paste0(title_i, " (B/W)")
      )
      figure_rows[[length(figure_rows) + 1L]] <- row_bw
      start_idx <- start_idx + 1L
    }

    x_vars_all_i <- if (isTRUE(PROCESS_SETTINGS$process_model == 14L)) {
      x_var_i
    } else if (isTRUE(PROCESS_SETTINGS$process_model == 8L)) {
      x_var_i
    } else if (isTRUE(PROCESS_SETTINGS$process_model == 15L)) {
      x_var_i
    } else if (isTRUE(PROCESS_SETTINGS$process_model == 58L || PROCESS_SETTINGS$process_model == 59L) && x_var_i %in% (PROCESS_SETTINGS$m_vars %||% character(0))) {
      PROCESS_SETTINGS$m_vars %||% x_var_i
    } else if (isTRUE(PROCESS_SETTINGS$process_model == 58L || PROCESS_SETTINGS$process_model == 59L)) {
      x_var_i
    } else {
      PROCESS_SETTINGS$x_vars %||% x_var_i
    }

    ci_df_i <- build_observed_ci_plot_df(
      dat = PROCESS_DATA,
      outcome_var = outcome_i,
      x_var = x_var_i,
      w_var = w_var_i,
      cond_df = cond_i,
      covariates = figure_covariates_i,
      n_points = 100L,
      x_vars_all = x_vars_all_i
    )
    if (nrow(ci_df_i) > 0) {
      plot_ci_i <- build_observed_prediction_ci_plot(
        plot_df = ci_df_i,
        x_label = x_label_i,
        y_label = paste0("Predicted ", outcome_label_i),
        moderator_label = w_label_i,
        title = title_i
      )
      if (!is.null(plot_ci_i)) {
        file_stub_ci <- if (isTRUE(PROCESS_SETTINGS$process_model %in% c(5L, 7L, 8L, 14L, 15L, 58L, 59L)) && nzchar(mediator_i)) {
          paste0("fig", start_idx, "_predicted_ci_", outcome_i, "_", x_var_i, "_", mediator_stub_i)
        } else {
          paste0("fig", start_idx, "_predicted_ci_", outcome_i, "_", x_var_i)
        }
        row_ci <- save_plot_all_formats(
          plot_obj = plot_ci_i,
          file_stub = file_stub_ci,
          dir_png = DIR_FIGURES_PNG,
          dir_tiff = DIR_FIGURES_TIFF,
          dir_pdf = DIR_FIGURES_PDF,
          width = 6.8,
          height = 4.8,
          dpi = 600,
          figure_title = paste0(title_i, " (95% CI)")
        )
        figure_rows[[length(figure_rows) + 1L]] <- row_ci
        start_idx <- start_idx + 1L

        plot_ci_bw_i <- build_observed_prediction_ci_plot_bw(
          plot_df = ci_df_i,
          x_label = x_label_i,
          y_label = paste0("Predicted ", outcome_label_i),
          moderator_label = w_label_i,
          title = title_i
        )
        if (!is.null(plot_ci_bw_i)) {
          file_stub_ci_bw <- if (isTRUE(PROCESS_SETTINGS$process_model %in% c(5L, 7L, 8L, 14L, 15L, 58L, 59L)) && nzchar(mediator_i)) {
            paste0("fig", start_idx, "_predicted_ci_bw_", outcome_i, "_", x_var_i, "_", mediator_stub_i)
          } else {
            paste0("fig", start_idx, "_predicted_ci_bw_", outcome_i, "_", x_var_i)
          }
          row_ci_bw <- save_plot_all_formats(
            plot_obj = plot_ci_bw_i,
            file_stub = file_stub_ci_bw,
            dir_png = DIR_FIGURES_PNG,
            dir_tiff = DIR_FIGURES_TIFF,
            dir_pdf = DIR_FIGURES_PDF,
            width = 6.8,
            height = 4.8,
            dpi = 600,
            figure_title = paste0(title_i, " (95% CI, B/W)")
          )
          figure_rows[[length(figure_rows) + 1L]] <- row_ci_bw
          start_idx <- start_idx + 1L
        }
      }
    }

    jn_res_i <- build_jn_plot_df(
      dat = PROCESS_DATA,
      outcome_var = outcome_i,
      x_var = x_var_i,
      w_var = w_var_i,
      covariates = figure_covariates_i,
      n_points = 200L,
      x_vars_all = x_vars_all_i
    )
    if (is.list(jn_res_i) && is.data.frame(jn_res_i$plot_df) && nrow(jn_res_i$plot_df) > 0) {
      title_jn_i <- paste0(
        "Conditional effect of ",
        x_label_i,
        " on ",
        outcome_label_i,
        "\nas a function of ",
        w_label_i
      )
      plot_jn_i <- build_jn_plot(
        plot_df = jn_res_i$plot_df,
        moderator_label = w_label_i,
        x_label = x_label_i,
        y_label = outcome_label_i,
        title = title_jn_i,
        jn_points = jn_res_i$jn_points
      )
      if (!is.null(plot_jn_i)) {
        file_stub_jn <- if (isTRUE(PROCESS_SETTINGS$process_model %in% c(5L, 7L, 8L, 14L, 15L, 58L, 59L)) && nzchar(mediator_i)) {
          paste0("fig", start_idx, "_johnson_neyman_", outcome_i, "_", x_var_i, "_", mediator_stub_i)
        } else {
          paste0("fig", start_idx, "_johnson_neyman_", outcome_i, "_", x_var_i)
        }
        row_jn <- save_plot_all_formats(
          plot_obj = plot_jn_i,
          file_stub = file_stub_jn,
          dir_png = DIR_FIGURES_PNG,
          dir_tiff = DIR_FIGURES_TIFF,
          dir_pdf = DIR_FIGURES_PDF,
          width = 6.8,
          height = 4.8,
          dpi = 600,
          figure_title = title_jn_i
        )
        figure_rows[[length(figure_rows) + 1L]] <- row_jn
        start_idx <- start_idx + 1L

        plot_jn_bw_i <- build_jn_plot_bw(
          plot_df = jn_res_i$plot_df,
          moderator_label = w_label_i,
          x_label = x_label_i,
          y_label = outcome_label_i,
          title = title_jn_i,
          jn_points = jn_res_i$jn_points
        )
        if (!is.null(plot_jn_bw_i)) {
          file_stub_jn_bw <- if (isTRUE(PROCESS_SETTINGS$process_model %in% c(5L, 7L, 8L, 14L, 15L, 58L, 59L)) && nzchar(mediator_i)) {
            paste0("fig", start_idx, "_johnson_neyman_bw_", outcome_i, "_", x_var_i, "_", mediator_stub_i)
          } else {
            paste0("fig", start_idx, "_johnson_neyman_bw_", outcome_i, "_", x_var_i)
          }
          row_jn_bw <- save_plot_all_formats(
            plot_obj = plot_jn_bw_i,
            file_stub = file_stub_jn_bw,
            dir_png = DIR_FIGURES_PNG,
            dir_tiff = DIR_FIGURES_TIFF,
            dir_pdf = DIR_FIGURES_PDF,
            width = 6.8,
            height = 4.8,
            dpi = 600,
            figure_title = paste0(title_jn_i, " (B/W)")
          )
          figure_rows[[length(figure_rows) + 1L]] <- row_jn_bw
          start_idx <- start_idx + 1L
        }
      }
    }
  }
}

if (isTRUE(PROCESS_SETTINGS$custom_model_enabled) &&
    nzchar(as.character(PROCESS_SETTINGS$w_var %||% "")[1]) &&
    is.data.frame(PROCESS_MODEL_RESULTS) &&
    nrow(PROCESS_MODEL_RESULTS) > 0 &&
    is.data.frame(PROCESS_DATA) &&
    nrow(PROCESS_DATA) > 0) {
  int_rows_custom <- PROCESS_MODEL_RESULTS[
    as.character(PROCESS_MODEL_RESULTS$effect_type %||% "") == "Interaction" &
      grepl(":", as.character(PROCESS_MODEL_RESULTS$term %||% ""), fixed = TRUE),
    ,
    drop = FALSE
  ]
  if (nrow(int_rows_custom) > 0) {
    int_rows_custom$..jn_key <- paste(
      as.character(int_rows_custom$table_block_key %||% ""),
      as.character(int_rows_custom$term %||% ""),
      sep = " | "
    )
    int_rows_custom <- int_rows_custom[!duplicated(int_rows_custom$..jn_key), , drop = FALSE]
    start_idx <- length(figure_rows) + 1L

    for (i in seq_len(nrow(int_rows_custom))) {
      row_i <- int_rows_custom[i, , drop = FALSE]
      term_i <- as.character(row_i$term[1] %||% "")
      parts_i <- trimws(unlist(strsplit(term_i, ":", fixed = TRUE), use.names = FALSE))
      if (length(parts_i) != 2L) next

      x_var_i <- parts_i[1]
      w_var_i <- parts_i[2]
      target_var_i <- as.character(row_i$mediator[1] %||% row_i$outcome[1] %||% "")
      if (!nzchar(x_var_i) || !nzchar(w_var_i) || !nzchar(target_var_i)) next
      if (!all(c(target_var_i, x_var_i, w_var_i) %in% names(PROCESS_DATA))) next

      target_label_i <- as.character(row_i$mediator_label[1] %||% row_i$outcome_label[1] %||% lookup_var_label_fig(target_var_i))
      x_label_i <- as.character(lookup_var_label_fig(x_var_i))
      w_label_i <- as.character(PROCESS_SETTINGS$w_label %||% lookup_var_label_fig(w_var_i))
      block_key_i <- as.character(row_i$table_block_key[1] %||% "")

      block_df_i <- PROCESS_MODEL_RESULTS[
        as.character(PROCESS_MODEL_RESULTS$table_block_key %||% "") == block_key_i,
        ,
        drop = FALSE
      ]
      other_preds_i <- unique(as.character(block_df_i$term[
        as.character(block_df_i$effect_type %||% "") %in% c("Independent variable", "Mediator") &
          !as.character(block_df_i$term %||% "") %in% c(x_var_i, w_var_i)
      ]))
      other_preds_i <- other_preds_i[nzchar(other_preds_i)]
      covariates_i <- unique(c(other_preds_i, PROCESS_SETTINGS$covariates %||% character(0)))

      jn_res_i <- build_jn_plot_df(
        dat = PROCESS_DATA,
        outcome_var = target_var_i,
        x_var = x_var_i,
        w_var = w_var_i,
        covariates = covariates_i,
        n_points = 200L,
        x_vars_all = x_var_i
      )
      if (!is.list(jn_res_i) || !is.data.frame(jn_res_i$plot_df) || nrow(jn_res_i$plot_df) == 0) next

      title_i <- paste0(
        "Conditional effect of ",
        x_label_i,
        " on ",
        target_label_i,
        "\nas a function of ",
        w_label_i
      )
      target_stub_i <- sanitize_token_fig(target_var_i)

      plot_i <- build_jn_plot(
        plot_df = jn_res_i$plot_df,
        moderator_label = w_label_i,
        x_label = x_label_i,
        y_label = target_label_i,
        title = title_i,
        jn_points = jn_res_i$jn_points
      )
      if (!is.null(plot_i)) {
        file_stub_i <- paste0("fig", start_idx, "_johnson_neyman_custom_", target_stub_i, "_", sanitize_token_fig(x_var_i))
        row_plot_i <- save_plot_all_formats(
          plot_obj = plot_i,
          file_stub = file_stub_i,
          dir_png = DIR_FIGURES_PNG,
          dir_tiff = DIR_FIGURES_TIFF,
          dir_pdf = DIR_FIGURES_PDF,
          width = 6.8,
          height = 4.8,
          dpi = 600,
          figure_title = title_i
        )
        figure_rows[[length(figure_rows) + 1L]] <- row_plot_i
        start_idx <- start_idx + 1L
      }

      plot_bw_i <- build_jn_plot_bw(
        plot_df = jn_res_i$plot_df,
        moderator_label = w_label_i,
        x_label = x_label_i,
        y_label = target_label_i,
        title = title_i,
        jn_points = jn_res_i$jn_points
      )
      if (!is.null(plot_bw_i)) {
        file_stub_bw_i <- paste0("fig", start_idx, "_johnson_neyman_custom_bw_", target_stub_i, "_", sanitize_token_fig(x_var_i))
        row_plot_bw_i <- save_plot_all_formats(
          plot_obj = plot_bw_i,
          file_stub = file_stub_bw_i,
          dir_png = DIR_FIGURES_PNG,
          dir_tiff = DIR_FIGURES_TIFF,
          dir_pdf = DIR_FIGURES_PDF,
          width = 6.8,
          height = 4.8,
          dpi = 600,
          figure_title = paste0(title_i, " (B/W)")
        )
        figure_rows[[length(figure_rows) + 1L]] <- row_plot_bw_i
        start_idx <- start_idx + 1L
      }
    }
  }
}

if ((isTRUE(PROCESS_SETTINGS$process_model %in% c(7L, 8L, 14L, 15L, 58L, 59L)) ||
     (isTRUE(PROCESS_SETTINGS$custom_model_enabled) && nzchar(as.character(PROCESS_SETTINGS$w_var %||% "")[1]))) &&
    is.data.frame(PROCESS_INDIRECT_RESULTS) &&
    nrow(PROCESS_INDIRECT_RESULTS) > 0 &&
    is.data.frame(PROCESS_DATA) &&
    nrow(PROCESS_DATA) > 0) {
  combo_cols_ind <- c("outcome", "outcome_label", "x_var", "x_label", "mediator", "mediator_label", "moderator", "moderator_label")
  combos_ind <- unique(PROCESS_INDIRECT_RESULTS[, combo_cols_ind, drop = FALSE])
  start_idx <- length(figure_rows) + 1L

  for (i in seq_len(nrow(combos_ind))) {
    combo_i <- combos_ind[i, , drop = FALSE]
    outcome_i <- as.character(combo_i$outcome[1])
    outcome_label_i <- as.character(combo_i$outcome_label[1] %||% lookup_var_label_fig(outcome_i))
    x_var_i <- as.character(combo_i$x_var[1])
    x_label_i <- as.character(combo_i$x_label[1] %||% lookup_var_label_fig(x_var_i))
    mediator_i <- as.character(combo_i$mediator[1] %||% "")
    mediator_label_i <- as.character(combo_i$mediator_label[1] %||% lookup_var_label_fig(mediator_i))
    w_var_i <- as.character(combo_i$moderator[1] %||% PROCESS_SETTINGS$w_var %||% "W")
    w_label_i <- as.character(combo_i$moderator_label[1] %||% lookup_var_label_fig(w_var_i))
    mediator_stub_i <- sanitize_token_fig(mediator_i)

    ind_i <- PROCESS_INDIRECT_RESULTS[
      as.character(PROCESS_INDIRECT_RESULTS$outcome) == outcome_i &
        as.character(PROCESS_INDIRECT_RESULTS$x_var) == x_var_i &
        as.character(PROCESS_INDIRECT_RESULTS$mediator) == mediator_i,
      ,
      drop = FALSE
    ]
    if (nrow(ind_i) == 0) next

    plot_df_i <- build_model7_indirect_plot_df(
      indirect_df = ind_i,
      dat = PROCESS_DATA,
      w_var = w_var_i,
      n_points = 200L
    )
    if (nrow(plot_df_i) == 0) next

    title_i <- paste0(
      "Conditional indirect effect of ",
      x_label_i,
      " on ",
      outcome_label_i,
      "\nthrough ",
      mediator_label_i,
      " as a function of ",
      w_label_i
    )

    plot_i <- build_model7_indirect_plot(
      plot_df = plot_df_i,
      moderator_label = w_label_i,
      x_label = x_label_i,
      mediator_label = mediator_label_i,
      outcome_label = outcome_label_i,
      title = title_i
    )
    if (!is.null(plot_i)) {
      file_stub_i <- paste0("fig", start_idx, "_indirect_effect_", outcome_i, "_", x_var_i, "_", mediator_stub_i)
      row_i <- save_plot_all_formats(
        plot_obj = plot_i,
        file_stub = file_stub_i,
        dir_png = DIR_FIGURES_PNG,
        dir_tiff = DIR_FIGURES_TIFF,
        dir_pdf = DIR_FIGURES_PDF,
        width = 6.8,
        height = 4.8,
        dpi = 600,
        figure_title = title_i
      )
      figure_rows[[length(figure_rows) + 1L]] <- row_i
      start_idx <- start_idx + 1L
    }

    plot_bw_i <- build_model7_indirect_plot_bw(
      plot_df = plot_df_i,
      moderator_label = w_label_i,
      x_label = x_label_i,
      mediator_label = mediator_label_i,
      outcome_label = outcome_label_i,
      title = title_i
    )
    if (!is.null(plot_bw_i)) {
      file_stub_bw <- paste0("fig", start_idx, "_indirect_effect_bw_", outcome_i, "_", x_var_i, "_", mediator_stub_i)
      row_bw <- save_plot_all_formats(
        plot_obj = plot_bw_i,
        file_stub = file_stub_bw,
        dir_png = DIR_FIGURES_PNG,
        dir_tiff = DIR_FIGURES_TIFF,
        dir_pdf = DIR_FIGURES_PDF,
        width = 6.8,
        height = 4.8,
        dpi = 600,
        figure_title = paste0(title_i, " (B/W)")
      )
      figure_rows[[length(figure_rows) + 1L]] <- row_bw
      start_idx <- start_idx + 1L
    }
  }
}

if (isTRUE(PROCESS_SETTINGS$process_model == 1L) &&
    identical(PROCESS_SETTINGS$moderator_source %||% "latent_class", "latent_class") &&
    is.data.frame(PROCESS_CONDITIONAL_EFFECTS) &&
    nrow(PROCESS_CONDITIONAL_EFFECTS) > 0) {
  combos_ce <- unique(PROCESS_CONDITIONAL_EFFECTS[, c("outcome", "outcome_label", "x_var", "x_label"), drop = FALSE])
  start_idx <- length(figure_rows) + 1L

  for (i in seq_len(nrow(combos_ce))) {
    combo_i <- combos_ce[i, , drop = FALSE]
    outcome_i <- as.character(combo_i$outcome[1])
    outcome_label_i <- as.character(combo_i$outcome_label[1] %||% lookup_var_label_fig(outcome_i))
    x_var_i <- as.character(combo_i$x_var[1])
    x_label_i <- as.character(combo_i$x_label[1] %||% lookup_var_label_fig(x_var_i))

    df_i <- PROCESS_CONDITIONAL_EFFECTS[
      as.character(PROCESS_CONDITIONAL_EFFECTS$outcome) == outcome_i &
        as.character(PROCESS_CONDITIONAL_EFFECTS$x_var) == x_var_i,
      ,
      drop = FALSE
    ]
    if (nrow(df_i) == 0) next

    title_i <- paste0(
      "Conditional effect of ",
      x_label_i,
      "\non ",
      outcome_label_i,
      " by profile"
    )
    plot_i <- build_conditional_effect_plot(
      df = df_i,
      x_label = x_label_i,
      y_label = outcome_label_i,
      moderator_label = "Profile",
      title = title_i,
      ref_class = SOURCE_REFERENCE_CLASS
    )
    if (is.null(plot_i)) next

    file_stub_i <- paste0("fig", start_idx, "_conditional_effect_", outcome_i, "_", x_var_i)
    row_i <- save_plot_all_formats(
      plot_obj = plot_i,
      file_stub = file_stub_i,
      dir_png = DIR_FIGURES_PNG,
      dir_tiff = DIR_FIGURES_TIFF,
      dir_pdf = DIR_FIGURES_PDF,
      width = 6.6,
      height = 4.8,
      dpi = 600,
      figure_title = title_i
    )
    figure_rows[[length(figure_rows) + 1L]] <- row_i
    start_idx <- start_idx + 1L
  }
}

if (length(figure_rows) > 0) {
  FIGURE_MANIFEST <- do.call(rbind, figure_rows)
  rownames(FIGURE_MANIFEST) <- NULL
}

FIGURE_SUMMARY <- list(
  process_model = PROCESS_SETTINGS$process_model,
  source_analysis_id = PROCESS_SETTINGS$source_analysis_id,
  variance_method = PROCESS_SETTINGS$variance_method %||% "bootstrap",
  n_models = MODEL_RUN_SUMMARY$n_models %||% 0L,
  n_coef_rows = nrow(PROCESS_MODEL_RESULTS %||% data.frame()),
  n_summary_rows = nrow(PROCESS_MODEL_SUMMARY %||% data.frame()),
  n_figures = nrow(FIGURE_MANIFEST),
  note = if (nrow(FIGURE_MANIFEST) > 0) {
    "PROCESS-style moderation plots were generated with continuous covariates fixed at their sample means and categorical covariates fixed at their reference categories."
  } else if (isTRUE(PROCESS_SETTINGS$custom_model_enabled)) {
    "No custom process-model figures were generated from the current outputs."
  } else if (!isTRUE(PROCESS_SETTINGS$process_model %in% c(1L, 5L, 7L, 8L, 14L, 15L, 58L, 59L))) {
    "Moderation figures are currently implemented for process_macro Model 1, Model 5, Model 7, Model 8, Model 14, Model 15, Model 58, and Model 59."
  } else {
    "No moderation figures were generated from the current model outputs."
  },
  created_at = Sys.time()
)

write_csv_safe(FIGURE_MANIFEST, file.path(DIR_FIGURES, "FIGURE_MANIFEST.csv"))
save_named_rds_list(
  list(
    FIGURE_MANIFEST = FIGURE_MANIFEST,
    FIGURE_SUMMARY = FIGURE_SUMMARY
  ),
  dir_rds = DIR_RDS
)

elapsed_sec <- round(as.numeric(difftime(Sys.time(), T0_FIGURES, units = "secs")), 2)
log_info("05_figures.R completed.")
log_info("n_figures         = ", FIGURE_SUMMARY$n_figures)
log_info("elapsed           = ", elapsed_sec, " sec")
log_step_end("figures", elapsed_sec, ok = TRUE)
