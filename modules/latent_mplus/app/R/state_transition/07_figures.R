T0_FIG <- Sys.time()

log_step_start("FIGURES", "07_figures.R")
log_info("Rendering SCI-style state-transition figures ...")

`%||%` <- function(x, y) if (is.null(x)) y else x

apply_state_theme <- function(base_size = 12) {
  if (exists("theme_sci", mode = "function")) {
    return(theme_sci(base_size = base_size))
  }
  ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(linewidth = 0.35, colour = "black"),
      axis.ticks = ggplot2::element_line(linewidth = 0.35, colour = "black")
    )
}

make_state_label_map <- function(dict = NULL, cfg = NULL, state_var_names = character(0)) {
  dict_levels <- if (is.list(dict) && is.data.frame(dict$levels)) dict$levels else data.frame()
  state_var_names <- unique(as.character(state_var_names))
  state_var_names <- state_var_names[!is.na(state_var_names) & nzchar(state_var_names)]
  if (is.data.frame(dict_levels) && nrow(dict_levels) > 0 &&
      all(c("var_name", "value", "value_label") %in% names(dict_levels))) {
    lv <- dict_levels[dict_levels$var_name %in% state_var_names, c("value", "value_label"), drop = FALSE]
    lv <- lv[!is.na(lv$value) & nzchar(as.character(lv$value)), , drop = FALSE]
    lv <- lv[!duplicated(as.character(lv$value)), , drop = FALSE]
    if (nrow(lv) > 0) {
      vals <- as.character(lv$value_label)
      nms <- as.character(lv$value)
      keep <- !is.na(vals) & nzchar(vals) & !is.na(nms) & nzchar(nms)
      if (any(keep)) return(stats::setNames(vals[keep], nms[keep]))
    }
  }
  x <- cfg$longitudinal$state_labels %||% cfg$state_labels %||% NULL
  if (is.null(x)) return(setNames(character(0), character(0)))
  if (is.list(x)) {
    vals <- vapply(x, function(z) as.character(z %||% NA_character_)[1], character(1))
    nms <- names(vals) %||% character(0)
    keep <- !is.na(vals) & nzchar(vals) & !is.na(nms) & nzchar(nms)
    return(stats::setNames(vals[keep], as.character(nms[keep])))
  }
  x <- as.character(x)
  stats::setNames(x, names(x))
}

state_label <- function(x, label_map) {
  key <- as.character(x)
  out <- unname(label_map[key])
  out[is.na(out) | !nzchar(out)] <- paste0("state", key[is.na(out) | !nzchar(out)])
  out
}

pretty_interval_label <- function(x) {
  x <- as.character(x)
  x <- gsub("^Y([0-9]+)_to_Y([0-9]+)$", "W\\1 to W\\2", x, perl = TRUE)
  x
}

CFG <- load_step_rds("CFG", dir_rds = DIR_RDS, default = list())
DICT <- load_step_rds("DICT", dir_rds = DIR_RDS, default = list())
LONGITUDINAL_SPEC <- load_step_rds("LONGITUDINAL_SPEC", dir_rds = DIR_RDS, default = list())
ESTIMATION_TRANSITIONS <- load_step_rds("ESTIMATION_TRANSITIONS", dir_rds = DIR_RDS, default = data.frame())
ESTIMATION_PREVALENCE <- load_step_rds("ESTIMATION_PREVALENCE", dir_rds = DIR_RDS, default = data.frame())
ESTIMATION_COVARIATES <- load_step_rds("ESTIMATION_COVARIATES", dir_rds = DIR_RDS, default = data.frame())
PANEL_LONG <- load_step_rds("PANEL_LONG", dir_rds = DIR_RDS, default = data.frame())
TRANSITION_DATA <- load_step_rds("TRANSITION_DATA", dir_rds = DIR_RDS, default = data.frame())
PREP_SUMMARY <- load_step_rds("PREP_SUMMARY", dir_rds = DIR_RDS, default = list())
STATE_LABEL_MAP <- make_state_label_map(DICT, CFG, unname(LONGITUDINAL_SPEC$state_wide_map %||% character(0)))
DICT_META <- if (is.list(DICT) && is.data.frame(DICT$meta)) as.data.frame(DICT$meta, stringsAsFactors = FALSE, check.names = FALSE) else data.frame()
DICT_LEVELS <- if (is.list(DICT) && is.data.frame(DICT$levels)) as.data.frame(DICT$levels, stringsAsFactors = FALSE, check.names = FALSE) else data.frame()

covariate_order_map <- setNames(numeric(0), character(0))
if (is.data.frame(DICT_META) && nrow(DICT_META) > 0 && all(c("var_name", "display_order") %in% names(DICT_META))) {
  cov_meta <- DICT_META
  role_col <- NULL
  if ("analysis_role" %in% names(cov_meta)) role_col <- "analysis_role"
  if (is.null(role_col) && "role" %in% names(cov_meta)) role_col <- "role"
  if (!is.null(role_col)) {
    cov_meta <- cov_meta[tolower(as.character(cov_meta[[role_col]])) == "covariate", , drop = FALSE]
  }
  cov_names <- toupper(as.character(cov_meta$var_name))
  cov_ord <- suppressWarnings(as.numeric(cov_meta$display_order))
  keep <- !is.na(cov_names) & nzchar(cov_names) & !is.na(cov_ord)
  if (any(keep)) covariate_order_map <- stats::setNames(cov_ord[keep], cov_names[keep])
  if ("var_label" %in% names(cov_meta)) {
    cov_labels <- toupper(as.character(cov_meta$var_label))
    keep_lab <- !is.na(cov_labels) & nzchar(cov_labels) & !is.na(cov_ord)
    if (any(keep_lab)) covariate_order_map <- c(covariate_order_map, stats::setNames(cov_ord[keep_lab], cov_labels[keep_lab]))
  }
}

resolve_level_order <- function(var_name, value) {
  if (is.data.frame(DICT_LEVELS) && nrow(DICT_LEVELS) > 0 && all(c("var_name", "value") %in% names(DICT_LEVELS))) {
    lv <- DICT_LEVELS[as.character(DICT_LEVELS$var_name) == as.character(var_name), , drop = FALSE]
    if (nrow(lv) > 0) {
      val_chr <- as.character(value)
      idx <- match(val_chr, as.character(lv$value))
      if (!is.na(idx)) return(idx)
    }
  }
  999999
}

save_plot_triplet <- function(plot_obj, stub, width = 10, height = 6, dpi = 600) {
  png_path <- file.path(DIR_FIGURES_PNG, paste0(stub, ".png"))

  ggplot2::ggsave(filename = png_path, plot = plot_obj, width = width, height = height, dpi = dpi)

  data.frame(
    figure_name = stub,
    file_png = png_path,
    file_tiff = NA_character_,
    file_pdf = NA_character_,
    stringsAsFactors = FALSE
  )
}

FIGURE_MANIFEST <- data.frame()
FIGURE_SUMMARY <- list(created = FALSE, n_figures = 0L)

observed_prev <- data.frame()
if (is.data.frame(PANEL_LONG) && nrow(PANEL_LONG) > 0) {
  observed_prev <- aggregate(
    list(n = PANEL_LONG$panel_id),
    by = list(wave = toupper(as.character(PANEL_LONG$panel_wave)), state = PANEL_LONG$state),
    FUN = length
  )
  wave_totals <- aggregate(list(total_n = observed_prev$n), by = list(wave = observed_prev$wave), FUN = sum)
  observed_prev <- merge(observed_prev, wave_totals, by = "wave", all.x = TRUE, sort = FALSE)
  observed_prev$prob <- ifelse(observed_prev$total_n > 0, observed_prev$n / observed_prev$total_n, NA_real_)
}

observed_transitions <- data.frame()
if (is.data.frame(TRANSITION_DATA) && nrow(TRANSITION_DATA) > 0) {
  observed_transitions <- aggregate(
    list(n = TRANSITION_DATA$panel_id),
    by = list(interval = TRANSITION_DATA$interval, from_state = TRANSITION_DATA$from_state, to_state = TRANSITION_DATA$to_state),
    FUN = length
  )
  observed_transitions$row_total <- ave(observed_transitions$n, observed_transitions$interval, observed_transitions$from_state, FUN = sum)
  observed_transitions$prob <- ifelse(observed_transitions$row_total > 0, observed_transitions$n / observed_transitions$row_total, NA_real_)
}

if (requireNamespace("ggplot2", quietly = TRUE) &&
    requireNamespace("scales", quietly = TRUE) &&
    is.data.frame(observed_transitions) &&
    nrow(observed_transitions) > 0) {
  df <- observed_transitions
  df$label <- sprintf("%.1f%%", 100 * df$prob)
  df$from_label <- factor(state_label(df$from_state, STATE_LABEL_MAP), levels = unique(state_label(sort(unique(df$from_state)), STATE_LABEL_MAP)))
  df$to_label <- factor(state_label(df$to_state, STATE_LABEL_MAP), levels = unique(state_label(sort(unique(df$to_state)), STATE_LABEL_MAP)))

  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = to_label, y = from_label, fill = prob)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = label), size = 3) +
    ggplot2::facet_wrap(~ interval) +
    ggplot2::scale_fill_gradient(low = "#f0f4f8", high = "#0f4c5c", labels = scales::percent) +
    ggplot2::labs(
      title = "Transition Probability Heatmap",
      x = "To state",
      y = "From state",
      fill = "Probability"
    ) +
    apply_state_theme(base_size = 12)

  FIGURE_MANIFEST <- rbind(FIGURE_MANIFEST, save_plot_triplet(p1, "Fig1-1_transition_heatmap", width = 10, height = 6, dpi = 600))

  p1_bw <- ggplot2::ggplot(df, ggplot2::aes(x = to_label, y = from_label, fill = prob)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = label), size = 3) +
    ggplot2::facet_wrap(~ interval) +
    ggplot2::scale_fill_gradient(low = "#f7f7f7", high = "#252525", labels = scales::percent) +
    ggplot2::labs(
      title = "Transition Probability Heatmap",
      x = "To state",
      y = "From state",
      fill = "Probability"
    ) +
    apply_state_theme(base_size = 12)

  FIGURE_MANIFEST <- rbind(FIGURE_MANIFEST, save_plot_triplet(p1_bw, "Fig1-2_transition_heatmap_bw", width = 10, height = 6, dpi = 600))

  stable_n <- aggregate(list(stable_n = df$n[df$from_state == df$to_state]), by = list(interval = df$interval[df$from_state == df$to_state]), FUN = sum)
  interval_total <- aggregate(list(total_n = df$n), by = list(interval = df$interval), FUN = sum)
  change_df <- merge(interval_total, stable_n, by = "interval", all.x = TRUE, sort = FALSE)
  change_df$stable_n[is.na(change_df$stable_n)] <- 0
  change_df$stable_prob <- ifelse(change_df$total_n > 0, change_df$stable_n / change_df$total_n, NA_real_)
  change_df$change_prob <- ifelse(!is.na(change_df$stable_prob), 1 - change_df$stable_prob, NA_real_)

  plot_df <- rbind(
    data.frame(interval = change_df$interval, type = "Stable", prob = change_df$stable_prob, stringsAsFactors = FALSE),
    data.frame(interval = change_df$interval, type = "Changed", prob = change_df$change_prob, stringsAsFactors = FALSE)
  )

  p2 <- ggplot2::ggplot(plot_df, ggplot2::aes(x = interval, y = prob, fill = type)) +
    ggplot2::geom_col(position = "stack", width = 0.7) +
    ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    ggplot2::scale_fill_manual(values = c("Stable" = "#0f4c5c", "Changed" = "#e36414")) +
    ggplot2::labs(
      title = "Stability Versus Change by Interval",
      x = "Interval",
      y = "Proportion",
      fill = NULL
    ) +
    apply_state_theme(base_size = 12)

  FIGURE_MANIFEST <- rbind(FIGURE_MANIFEST, save_plot_triplet(p2, "Fig2-1_stability_change", width = 8, height = 5, dpi = 600))

  p2_bw <- ggplot2::ggplot(plot_df, ggplot2::aes(x = interval, y = prob, fill = type)) +
    ggplot2::geom_col(position = "stack", width = 0.7) +
    ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    ggplot2::scale_fill_manual(values = c("Stable" = "#4d4d4d", "Changed" = "#d9d9d9")) +
    ggplot2::labs(
      title = "Stability Versus Change by Interval",
      x = "Interval",
      y = "Proportion",
      fill = NULL
    ) +
    apply_state_theme(base_size = 12)

  FIGURE_MANIFEST <- rbind(FIGURE_MANIFEST, save_plot_triplet(p2_bw, "Fig2-2_stability_change_bw", width = 8, height = 5, dpi = 600))
}

if (requireNamespace("ggplot2", quietly = TRUE) &&
    requireNamespace("scales", quietly = TRUE) &&
    is.data.frame(observed_prev) &&
    nrow(observed_prev) > 0) {
  prev_df <- observed_prev
  prev_df$panel_wave <- toupper(as.character(prev_df$wave))
  prev_df$prop <- prev_df$prob
  prev_df$state_label <- factor(
    state_label(prev_df$state, STATE_LABEL_MAP),
    levels = unique(state_label(sort(unique(prev_df$state)), STATE_LABEL_MAP))
  )

  p3 <- ggplot2::ggplot(prev_df, ggplot2::aes(x = panel_wave, y = prop, color = state_label, group = state_label)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    ggplot2::scale_color_brewer(palette = "Dark2", name = "State") +
    ggplot2::labs(
      title = "State Prevalence Across Waves",
      x = "Wave",
      y = "Prevalence"
    ) +
    apply_state_theme(base_size = 12)

  FIGURE_MANIFEST <- rbind(FIGURE_MANIFEST, save_plot_triplet(p3, "Fig3-1_state_prevalence", width = 8, height = 5, dpi = 600))

  state_levels_plot <- levels(prev_df$state_label)
  ref_state_plot <- suppressWarnings(as.integer(PREP_SUMMARY$reference_state %||% NA_integer_))
  ref_label_plot <- if (!is.na(ref_state_plot)) state_label(ref_state_plot, STATE_LABEL_MAP) else NA_character_
  linetype_candidates <- c("dashed", "dotted", "dotdash", "longdash", "twodash")
  linetype_values <- stats::setNames(rep(linetype_candidates, length.out = length(state_levels_plot)), state_levels_plot)
  if (!is.na(ref_label_plot) && ref_label_plot %in% state_levels_plot) {
    linetype_values[ref_label_plot] <- "solid"
  } else if (length(state_levels_plot) > 0) {
    linetype_values[state_levels_plot[1]] <- "solid"
  }
  p3_bw <- ggplot2::ggplot(prev_df, ggplot2::aes(x = panel_wave, y = prop, group = state_label, linetype = state_label, shape = state_label, colour = state_label)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    ggplot2::scale_colour_grey(start = 0.2, end = 0.6, name = "State") +
    ggplot2::scale_linetype_manual(values = linetype_values, name = "State") +
    ggplot2::scale_shape_manual(values = stats::setNames(c(16, 17, 15)[seq_along(state_levels_plot)], state_levels_plot), name = "State") +
    ggplot2::labs(
      title = "State Prevalence Across Waves",
      x = "Wave",
      y = "Prevalence"
    ) +
    apply_state_theme(base_size = 12)

  FIGURE_MANIFEST <- rbind(FIGURE_MANIFEST, save_plot_triplet(p3_bw, "Fig3-2_state_prevalence_bw", width = 8, height = 5, dpi = 600))
}

if (requireNamespace("ggplot2", quietly = TRUE) &&
    is.data.frame(ESTIMATION_COVARIATES) &&
    nrow(ESTIMATION_COVARIATES) > 0) {
  forest_df <- ESTIMATION_COVARIATES
  if (!"source_label" %in% names(forest_df)) forest_df$source_label <- forest_df$predictor
  if (!"source_var" %in% names(forest_df)) forest_df$source_var <- forest_df$predictor
  if (!"predictor_type" %in% names(forest_df)) forest_df$predictor_type <- "continuous"
  if (!"value_label" %in% names(forest_df)) forest_df$value_label <- NA_character_
  if (!"level" %in% names(forest_df)) forest_df$level <- NA_character_
  if (!"reference_label" %in% names(forest_df)) forest_df$reference_label <- NA_character_
  forest_df$Comparison <- paste0(state_label(forest_df$to_state, STATE_LABEL_MAP), " vs ", state_label(forest_df$reference_state, STATE_LABEL_MAP))
  forest_df$term <- ifelse(
    forest_df$predictor_type == "categorical" & !is.na(forest_df$value_label) & nzchar(as.character(forest_df$value_label)),
    paste0(as.character(forest_df$source_label), ": ", as.character(forest_df$value_label)),
    as.character(forest_df$source_label)
  )
  forest_df$source_order <- unname(covariate_order_map[toupper(as.character(forest_df$source_var))])
  miss_source_order <- is.na(forest_df$source_order)
  forest_df$source_order[miss_source_order] <- unname(covariate_order_map[toupper(as.character(forest_df$source_label[miss_source_order]))])
  forest_df$source_order[is.na(forest_df$source_order)] <- 999999
  forest_df$level_order <- vapply(seq_len(nrow(forest_df)), function(i) {
    resolve_level_order(forest_df$source_var[i], forest_df$level[i] %||% forest_df$value_label[i])
  }, numeric(1))
  forest_df <- forest_df[!is.na(forest_df$odds_ratio) & !is.na(forest_df$or_lcl) & !is.na(forest_df$or_ucl) &
    forest_df$odds_ratio > 0 & forest_df$or_lcl > 0 & forest_df$or_ucl > 0, , drop = FALSE]
  if (nrow(forest_df) > 0) {
    forest_df <- forest_df[order(forest_df$source_order, forest_df$level_order, forest_df$source_label, forest_df$term), , drop = FALSE]
    forest_df$term <- factor(forest_df$term, levels = rev(unique(forest_df$term)))
    interval_levels_plot <- PREP_SUMMARY$wave_order %||% character(0)
    interval_levels_plot <- if (length(interval_levels_plot) >= 2) {
      paste0(toupper(interval_levels_plot[-length(interval_levels_plot)]), "_to_", toupper(interval_levels_plot[-1]))
    } else {
      unique(as.character(forest_df$interval))
    }
    interval_levels_plot <- interval_levels_plot[interval_levels_plot %in% unique(as.character(forest_df$interval))]
    if (length(interval_levels_plot) == 0) interval_levels_plot <- unique(as.character(forest_df$interval))
    p4 <- ggplot2::ggplot(forest_df, ggplot2::aes(x = odds_ratio, y = term)) +
      ggplot2::geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.4, colour = "grey40") +
      ggplot2::geom_errorbarh(ggplot2::aes(xmin = or_lcl, xmax = or_ucl), width = 0, linewidth = 0.5, colour = "#0f4c5c") +
      ggplot2::geom_point(size = 2.0, colour = "#0f4c5c") +
      ggplot2::scale_x_log10() +
      ggplot2::facet_grid(interval ~ Comparison, scales = "free_y", space = "free_y") +
      ggplot2::labs(
        title = "Baseline Covariate Effects on Transition Risk",
        x = "RRR",
        y = NULL
      ) +
      apply_state_theme(base_size = 11) +
      ggplot2::theme(axis.line.y = ggplot2::element_blank())

    FIGURE_MANIFEST <- rbind(FIGURE_MANIFEST, save_plot_triplet(p4, "Fig4-1_covariate_forest", width = 11, height = 7, dpi = 600))

    forest_df2 <- forest_df
    forest_df2$covariate_block <- factor(
      as.character(forest_df2$source_label),
      levels = unique(as.character(forest_df2$source_label[order(forest_df2$source_order, forest_df2$source_label)]))
    )
    forest_df2$interval_label <- factor(
      pretty_interval_label(as.character(forest_df2$interval)),
      levels = pretty_interval_label(rev(interval_levels_plot))
    )
    forest_df2$interval_key <- factor(pretty_interval_label(as.character(forest_df2$interval)), levels = pretty_interval_label(interval_levels_plot))
    interval_gap <- 0.07
    top_y <- 1 + interval_gap * pmax(length(interval_levels_plot) - 1L, 0L)
    compact_y_map <- stats::setNames(
      seq(from = top_y, by = -interval_gap, length.out = length(interval_levels_plot)),
      pretty_interval_label(interval_levels_plot)
    )
    forest_df2$interval_y <- unname(compact_y_map[as.character(forest_df2$interval_key)])
    interval_colour_map <- stats::setNames(c("#0f4c5c", "#5f0f40", "#e36414")[seq_along(interval_levels_plot)], pretty_interval_label(interval_levels_plot))
    interval_shape_map <- stats::setNames(c(16, 17, 15)[seq_along(interval_levels_plot)], pretty_interval_label(interval_levels_plot))

    p4_2 <- ggplot2::ggplot(forest_df2, ggplot2::aes(x = odds_ratio, y = interval_y, colour = interval_key, shape = interval_key)) +
      ggplot2::geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.4, colour = "grey40") +
      ggplot2::geom_errorbarh(ggplot2::aes(xmin = or_lcl, xmax = or_ucl), width = 0, linewidth = 0.75) +
      ggplot2::geom_point(size = 2.8) +
      ggplot2::scale_x_log10() +
      ggplot2::scale_y_continuous(
        breaks = unname(compact_y_map[pretty_interval_label(interval_levels_plot)]),
        labels = rep("", length(unique(forest_df2$interval_y))),
        limits = c(min(compact_y_map) - interval_gap, max(compact_y_map) + interval_gap),
        expand = ggplot2::expansion(mult = c(0, 0))
      ) +
      ggplot2::scale_colour_manual(values = interval_colour_map, name = "Interval") +
      ggplot2::scale_shape_manual(values = interval_shape_map, name = "Interval") +
      ggplot2::facet_grid(covariate_block ~ Comparison, scales = "free_y", space = "free_y", switch = "y") +
      ggplot2::labs(
        title = "Baseline Covariate Effects on Transition Risk",
        x = "RRR",
        y = NULL
      ) +
      apply_state_theme(base_size = 11) +
      ggplot2::theme(
        axis.line.y = ggplot2::element_blank(),
        strip.placement = "outside",
        strip.background = ggplot2::element_blank(),
        strip.background.y = ggplot2::element_blank(),
        strip.background.y.left = ggplot2::element_blank(),
        strip.background.x = ggplot2::element_blank(),
        strip.border = ggplot2::element_blank(),
        strip.text.x = ggplot2::element_text(size = 12, face = "plain"),
        strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 0, face = "plain", size = 12),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(size = 12),
        panel.border = ggplot2::element_blank(),
        panel.spacing.y = ggplot2::unit(0.0, "lines"),
        panel.spacing.x = ggplot2::unit(0.20, "lines"),
        strip.switch.pad.grid = ggplot2::unit(0.05, "lines"),
        legend.position = "bottom",
        plot.margin = ggplot2::margin(t = 4, r = 6, b = 4, l = 4)
      )

    FIGURE_MANIFEST <- rbind(FIGURE_MANIFEST, save_plot_triplet(p4_2, "Fig4-2_covariate_forest_by_covariate", width = 11, height = 5.4, dpi = 600))

    interval_bw_colour_map <- stats::setNames(c("#1A1A1A", "#6E6E6E", "#B0B0B0")[seq_along(interval_levels_plot)], pretty_interval_label(interval_levels_plot))
    p4_5 <- p4_2 +
      ggplot2::scale_colour_manual(values = interval_bw_colour_map, name = "Interval") +
      ggplot2::scale_shape_manual(values = interval_shape_map, name = "Interval")

    FIGURE_MANIFEST <- rbind(FIGURE_MANIFEST, save_plot_triplet(p4_5, "Fig4-5_covariate_forest_by_covariate_bw", width = 11, height = 5.4, dpi = 600))

    forest_df3 <- forest_df
    forest_df3$covariate_block <- factor(
      as.character(forest_df3$source_label),
      levels = unique(as.character(forest_df3$source_label[order(forest_df3$source_order, forest_df3$source_label)]))
    )
    forest_df3$interval_key <- factor(pretty_interval_label(as.character(forest_df3$interval)), levels = pretty_interval_label(interval_levels_plot))
    forest_df3$sig_dir <- ifelse(
      !is.na(forest_df3$or_ucl) & forest_df3$or_ucl < 1,
      "sig_lt1",
      ifelse(!is.na(forest_df3$or_lcl) & forest_df3$or_lcl > 1, "sig_gt1", "ns")
    )
    forest_df3$rrr_ci_label <- paste0(
      as.character(forest_df3$interval_key), "  ",
      sprintf("%.2f", forest_df3$odds_ratio),
      " (",
      sprintf("%.2f", forest_df3$or_lcl),
      "-",
      sprintf("%.2f", forest_df3$or_ucl),
      ")"
    )
    label_levels_tbl <- unlist(lapply(levels(forest_df3$covariate_block), function(cv) {
      unlist(lapply(unique(as.character(forest_df3$Comparison)), function(cp) {
        hit <- forest_df3[
          as.character(forest_df3$covariate_block) == cv &
            as.character(forest_df3$Comparison) == cp,
          ,
          drop = FALSE
        ]
        if (nrow(hit) == 0) return(character(0))
        hit <- hit[order(match(as.character(hit$interval_key), interval_levels_plot)), , drop = FALSE]
        as.character(hit$rrr_ci_label)
      }), use.names = FALSE)
    }), use.names = FALSE)
    label_levels_tbl <- unique(label_levels_tbl[nzchar(label_levels_tbl)])
    forest_df3$rrr_ci_label <- factor(as.character(forest_df3$rrr_ci_label), levels = rev(label_levels_tbl))
    sig_colour_map <- c("ns" = "#4D4D4D", "sig_lt1" = "#D55E00", "sig_gt1" = "#0072B2")
    sig_colour_labels <- c(
      "ns" = "Not significant",
      "sig_lt1" = "Significant, RRR < 1",
      "sig_gt1" = "Significant, RRR > 1"
    )
    present_sig_breaks_43 <- names(sig_colour_labels)[names(sig_colour_labels) %in% as.character(stats::na.omit(unique(forest_df3$sig_dir)))]

    p4_3 <- ggplot2::ggplot(forest_df3, ggplot2::aes(x = odds_ratio, y = rrr_ci_label, colour = sig_dir)) +
      ggplot2::geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.4, colour = "grey40") +
      ggplot2::geom_errorbarh(ggplot2::aes(xmin = or_lcl, xmax = or_ucl), width = 0, linewidth = 0.75) +
      ggplot2::geom_point(size = 2.8) +
      ggplot2::scale_x_log10() +
      ggplot2::scale_colour_manual(values = sig_colour_map, breaks = present_sig_breaks_43, labels = unname(sig_colour_labels[present_sig_breaks_43]), name = NULL) +
      ggplot2::facet_grid(covariate_block ~ Comparison, scales = "free_y", space = "free_y", switch = "y") +
      ggplot2::labs(
        title = "Baseline Covariate Effects on Transition Risk",
        x = "RRR",
        y = NULL
      ) +
      apply_state_theme(base_size = 11) +
      ggplot2::theme(
        axis.line.y = ggplot2::element_blank(),
        strip.placement = "outside",
        strip.background = ggplot2::element_blank(),
        strip.background.y = ggplot2::element_blank(),
        strip.background.y.left = ggplot2::element_blank(),
        strip.background.x = ggplot2::element_blank(),
        strip.border = ggplot2::element_blank(),
        strip.text.x = ggplot2::element_text(size = 12, face = "plain"),
        strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 0, face = "plain", size = 12),
        axis.text.y = ggplot2::element_text(size = 8),
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(size = 12),
        panel.border = ggplot2::element_blank(),
        panel.spacing.y = ggplot2::unit(0.05, "lines"),
        panel.spacing.x = ggplot2::unit(0.20, "lines"),
        strip.switch.pad.grid = ggplot2::unit(0.05, "lines"),
        legend.position = "bottom",
        plot.margin = ggplot2::margin(t = 4, r = 6, b = 4, l = 4)
      ) +
      ggplot2::guides(colour = ggplot2::guide_legend(order = 1))

    FIGURE_MANIFEST <- rbind(FIGURE_MANIFEST, save_plot_triplet(p4_3, "Fig4-3_covariate_forest_with_rrr_ci", width = 11, height = 7.2, dpi = 600))

    sig_bw_colour_map <- c("ns" = "#4D4D4D", "sig_lt1" = "#9E9E9E", "sig_gt1" = "#000000")
    p4_6 <- p4_3 +
      ggplot2::scale_colour_manual(values = sig_bw_colour_map, breaks = present_sig_breaks_43, labels = unname(sig_colour_labels[present_sig_breaks_43]), name = NULL)

    FIGURE_MANIFEST <- rbind(FIGURE_MANIFEST, save_plot_triplet(p4_6, "Fig4-6_covariate_forest_with_rrr_ci_bw", width = 11, height = 7.2, dpi = 600))

    forest_df4 <- forest_df
    forest_df4$covariate_block <- factor(
      as.character(forest_df4$source_label),
      levels = unique(as.character(forest_df4$source_label[order(forest_df4$source_order, forest_df4$source_label)]))
    )
    forest_df4$interval_key <- factor(pretty_interval_label(as.character(forest_df4$interval)), levels = pretty_interval_label(interval_levels_plot))
    forest_df4$sig_dir <- ifelse(
      !is.na(forest_df4$or_ucl) & forest_df4$or_ucl < 1,
      "sig_lt1",
      ifelse(!is.na(forest_df4$or_lcl) & forest_df4$or_lcl > 1, "sig_gt1", "ns")
    )
    forest_df4$rrr_ci_label <- paste0(
      as.character(forest_df4$interval_key), "  ",
      sprintf("%.2f", forest_df4$odds_ratio),
      " (",
      sprintf("%.2f", forest_df4$or_lcl),
      "-",
      sprintf("%.2f", forest_df4$or_ucl),
      ")"
    )
    comparison_order_44 <- c("Dementia vs Normal", "MCI vs Normal")
    label_levels_44 <- unlist(lapply(levels(forest_df4$covariate_block), function(cv) {
      unlist(lapply(comparison_order_44, function(cp) {
        hit <- forest_df4[
          as.character(forest_df4$covariate_block) == cv &
            as.character(forest_df4$Comparison) == cp,
          ,
          drop = FALSE
        ]
        if (nrow(hit) == 0) return(character(0))
        hit <- hit[order(match(as.character(hit$interval_key), pretty_interval_label(interval_levels_plot))), , drop = FALSE]
        as.character(hit$rrr_ci_label)
      }), use.names = FALSE)
    }), use.names = FALSE)
    label_levels_44 <- unique(label_levels_44[nzchar(label_levels_44)])
    forest_df4$rrr_ci_label <- factor(as.character(forest_df4$rrr_ci_label), levels = rev(label_levels_44))
    legend_levels_44 <- c(
      "Dementia vs Normal__comp",
      "Dementia vs Normal__ns",
      "Dementia vs Normal__sig_lt1",
      "Dementia vs Normal__sig_gt1",
      "MCI vs Normal__comp",
      "MCI vs Normal__ns",
      "MCI vs Normal__sig_lt1",
      "MCI vs Normal__sig_gt1"
    )
    forest_df4$legend_key <- factor(
      paste(as.character(forest_df4$Comparison), forest_df4$sig_dir, sep = "__"),
      levels = legend_levels_44
    )
    legend_labels_44 <- c(
      "Dementia vs Normal",
      "Not significant",
      "< 1 significant",
      "> 1 significant",
      "MCI vs Normal",
      "Not significant",
      "< 1 significant",
      "> 1 significant"
    )
    names(legend_labels_44) <- legend_levels_44
    present_data_keys_44 <- as.character(stats::na.omit(unique(forest_df4$legend_key)))
    legend_breaks_present_44 <- c(
      "Dementia vs Normal__comp",
      if ("Dementia vs Normal__ns" %in% present_data_keys_44) "Dementia vs Normal__ns",
      if ("Dementia vs Normal__sig_lt1" %in% present_data_keys_44) "Dementia vs Normal__sig_lt1",
      if ("Dementia vs Normal__sig_gt1" %in% present_data_keys_44) "Dementia vs Normal__sig_gt1",
      "MCI vs Normal__comp",
      if ("MCI vs Normal__ns" %in% present_data_keys_44) "MCI vs Normal__ns",
      if ("MCI vs Normal__sig_lt1" %in% present_data_keys_44) "MCI vs Normal__sig_lt1",
      if ("MCI vs Normal__sig_gt1" %in% present_data_keys_44) "MCI vs Normal__sig_gt1"
    )
    legend_colour_map_44 <- c(
      "Dementia vs Normal__comp" = sig_colour_map[["ns"]],
      "Dementia vs Normal__ns" = sig_colour_map[["ns"]],
      "Dementia vs Normal__sig_lt1" = sig_colour_map[["sig_lt1"]],
      "Dementia vs Normal__sig_gt1" = sig_colour_map[["sig_gt1"]],
      "MCI vs Normal__comp" = sig_colour_map[["ns"]],
      "MCI vs Normal__ns" = sig_colour_map[["ns"]],
      "MCI vs Normal__sig_lt1" = sig_colour_map[["sig_lt1"]],
      "MCI vs Normal__sig_gt1" = sig_colour_map[["sig_gt1"]]
    )
    legend_shape_map_44 <- c(
      "Dementia vs Normal__comp" = 16,
      "Dementia vs Normal__ns" = 16,
      "Dementia vs Normal__sig_lt1" = 16,
      "Dementia vs Normal__sig_gt1" = 16,
      "MCI vs Normal__comp" = 15,
      "MCI vs Normal__ns" = 15,
      "MCI vs Normal__sig_lt1" = 15,
      "MCI vs Normal__sig_gt1" = 15
    )
    legend_line_map_44 <- stats::setNames(rep("solid", length(legend_levels_44)), legend_levels_44)
    p4_4 <- ggplot2::ggplot(forest_df4, ggplot2::aes(x = odds_ratio, y = rrr_ci_label, colour = legend_key, shape = legend_key, linetype = legend_key)) +
      ggplot2::geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.4, colour = "grey40") +
      ggplot2::geom_errorbarh(ggplot2::aes(xmin = or_lcl, xmax = or_ucl), width = 0, linewidth = 0.75) +
      ggplot2::geom_point(size = 2.8) +
      ggplot2::scale_x_log10() +
      ggplot2::scale_colour_manual(values = legend_colour_map_44, breaks = legend_breaks_present_44, labels = unname(legend_labels_44[legend_breaks_present_44]), name = NULL, drop = FALSE) +
      ggplot2::scale_shape_manual(values = legend_shape_map_44, breaks = legend_breaks_present_44, labels = unname(legend_labels_44[legend_breaks_present_44]), name = NULL, drop = FALSE) +
      ggplot2::scale_linetype_manual(values = legend_line_map_44, breaks = legend_breaks_present_44, labels = unname(legend_labels_44[legend_breaks_present_44]), name = NULL, drop = FALSE) +
      ggplot2::facet_grid(covariate_block ~ ., scales = "free_y", space = "free_y", switch = "y") +
      ggplot2::labs(
        title = "Baseline Covariate Effects on Transition Risk",
        x = "RRR",
        y = NULL
      ) +
      apply_state_theme(base_size = 11) +
      ggplot2::theme(
        axis.line.y = ggplot2::element_blank(),
        strip.placement = "outside",
        strip.background = ggplot2::element_blank(),
        strip.background.y = ggplot2::element_blank(),
        strip.background.y.left = ggplot2::element_blank(),
        strip.background.x = ggplot2::element_blank(),
        strip.border = ggplot2::element_blank(),
        strip.text.x = ggplot2::element_text(size = 12, face = "plain"),
        strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 0, face = "plain", size = 12),
        axis.text.y = ggplot2::element_text(size = 9),
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(size = 12),
        panel.border = ggplot2::element_blank(),
        panel.spacing.y = ggplot2::unit(0.05, "lines"),
        panel.spacing.x = ggplot2::unit(0.20, "lines"),
        strip.switch.pad.grid = ggplot2::unit(0.05, "lines"),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.box.just = "left",
        legend.justification = c(0, 0.5),
        legend.text.align = 0,
        legend.key.width = ggplot2::unit(1.5, "lines"),
        legend.spacing.x = ggplot2::unit(0.3, "lines"),
        plot.title = ggplot2::element_text(hjust = 0.5),
        plot.margin = ggplot2::margin(t = 4, r = 6, b = 4, l = 4)
      ) +
      ggplot2::guides(
        colour = ggplot2::guide_legend(
          order = 1,
          nrow = 2,
          byrow = TRUE,
          override.aes = list(
            linewidth = 0.75,
            size = 2.8,
            shape = unname(legend_shape_map_44[legend_breaks_present_44]),
            linetype = unname(legend_line_map_44[legend_breaks_present_44]),
            colour = unname(legend_colour_map_44[legend_breaks_present_44])
          )
        ),
        shape = "none",
        linetype = "none"
      )

    FIGURE_MANIFEST <- rbind(FIGURE_MANIFEST, save_plot_triplet(p4_4, "Fig4-4_covariate_forest_with_shape_legend", width = 7.6, height = 7.2, dpi = 600))

    forest_df7 <- forest_df4
    forest_df7$sig_bw <- ifelse(as.character(forest_df7$sig_dir) == "ns", "ns", "sig")
    legend_levels_47 <- c(
      "Dementia vs Normal__comp",
      "Dementia vs Normal__ns",
      "Dementia vs Normal__sig",
      "MCI vs Normal__comp",
      "MCI vs Normal__ns",
      "MCI vs Normal__sig"
    )
    forest_df7$legend_key_bw <- factor(
      paste(as.character(forest_df7$Comparison), forest_df7$sig_bw, sep = "__"),
      levels = legend_levels_47
    )
    legend_labels_47 <- c(
      "Dementia vs Normal",
      "Not significant",
      "Significant",
      "MCI vs Normal",
      "Not significant",
      "Significant"
    )
    names(legend_labels_47) <- legend_levels_47
    present_sig_keys_47 <- as.character(stats::na.omit(unique(forest_df7$legend_key_bw)))
    legend_breaks_47 <- c(
      "Dementia vs Normal__comp",
      if ("Dementia vs Normal__ns" %in% present_sig_keys_47) "Dementia vs Normal__ns",
      if ("Dementia vs Normal__sig" %in% present_sig_keys_47) "Dementia vs Normal__sig",
      "MCI vs Normal__comp",
      if ("MCI vs Normal__ns" %in% present_sig_keys_47) "MCI vs Normal__ns",
      if ("MCI vs Normal__sig" %in% present_sig_keys_47) "MCI vs Normal__sig"
    )
    legend_colour_map_47 <- stats::setNames(rep("#333333", length(legend_levels_47)), legend_levels_47)
    legend_fill_map_47 <- c(
      "Dementia vs Normal__comp" = "white",
      "Dementia vs Normal__ns" = "white",
      "Dementia vs Normal__sig" = "#333333",
      "MCI vs Normal__comp" = "white",
      "MCI vs Normal__ns" = "white",
      "MCI vs Normal__sig" = "#333333"
    )
    legend_shape_map_47 <- c(
      "Dementia vs Normal__comp" = 21,
      "Dementia vs Normal__ns" = 21,
      "Dementia vs Normal__sig" = 21,
      "MCI vs Normal__comp" = 22,
      "MCI vs Normal__ns" = 22,
      "MCI vs Normal__sig" = 22
    )
    legend_line_map_47 <- stats::setNames(rep("solid", length(legend_levels_47)), legend_levels_47)
    p4_7 <- ggplot2::ggplot(
      forest_df7,
      ggplot2::aes(
        x = odds_ratio,
        y = rrr_ci_label,
        colour = legend_key_bw,
        fill = legend_key_bw,
        shape = legend_key_bw,
        linetype = legend_key_bw
      )
    ) +
      ggplot2::geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.4, colour = "grey40") +
      ggplot2::geom_errorbarh(ggplot2::aes(xmin = or_lcl, xmax = or_ucl), width = 0, linewidth = 0.75) +
      ggplot2::geom_point(size = 2.8, stroke = 0.8) +
      ggplot2::scale_x_log10() +
      ggplot2::scale_colour_manual(
        values = legend_colour_map_47,
        breaks = legend_breaks_47,
        labels = unname(legend_labels_47[legend_breaks_47]),
        name = NULL,
        drop = FALSE
      ) +
      ggplot2::scale_fill_manual(
        values = legend_fill_map_47,
        breaks = legend_breaks_47,
        labels = unname(legend_labels_47[legend_breaks_47]),
        name = NULL,
        drop = FALSE
      ) +
      ggplot2::scale_shape_manual(
        values = legend_shape_map_47,
        breaks = legend_breaks_47,
        labels = unname(legend_labels_47[legend_breaks_47]),
        name = NULL,
        drop = FALSE
      ) +
      ggplot2::scale_linetype_manual(
        values = legend_line_map_47,
        breaks = legend_breaks_47,
        labels = unname(legend_labels_47[legend_breaks_47]),
        name = NULL,
        drop = FALSE
      ) +
      ggplot2::facet_grid(covariate_block ~ ., scales = "free_y", space = "free_y", switch = "y") +
      ggplot2::labs(
        title = "Baseline Covariate Effects on Transition Risk",
        x = "RRR",
        y = NULL
      ) +
      apply_state_theme(base_size = 11) +
      ggplot2::theme(
        axis.line.y = ggplot2::element_blank(),
        strip.placement = "outside",
        strip.background = ggplot2::element_blank(),
        strip.background.y = ggplot2::element_blank(),
        strip.background.y.left = ggplot2::element_blank(),
        strip.background.x = ggplot2::element_blank(),
        strip.border = ggplot2::element_blank(),
        strip.text.x = ggplot2::element_text(size = 12, face = "plain"),
        strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 0, face = "plain", size = 12),
        axis.text.y = ggplot2::element_text(size = 9),
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(size = 12),
        panel.border = ggplot2::element_blank(),
        panel.spacing.y = ggplot2::unit(0.05, "lines"),
        panel.spacing.x = ggplot2::unit(0.20, "lines"),
        strip.switch.pad.grid = ggplot2::unit(0.05, "lines"),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.box.just = "left",
        legend.justification = c(0, 0.5),
        legend.text.align = 0,
        legend.key.width = ggplot2::unit(1.5, "lines"),
        legend.spacing.x = ggplot2::unit(0.3, "lines"),
        plot.title = ggplot2::element_text(hjust = 0.5),
        plot.margin = ggplot2::margin(t = 4, r = 6, b = 4, l = 4)
      ) +
      ggplot2::guides(
        colour = ggplot2::guide_legend(
          order = 1,
          nrow = 2,
          byrow = TRUE,
          override.aes = list(
            linewidth = 0.75,
            size = 2.8,
            shape = unname(legend_shape_map_47[legend_breaks_47]),
            linetype = unname(legend_line_map_47[legend_breaks_47]),
            fill = unname(legend_fill_map_47[legend_breaks_47]),
            colour = unname(legend_colour_map_47[legend_breaks_47])
          )
        ),
        fill = "none",
        shape = "none",
        linetype = "none"
      )

    FIGURE_MANIFEST <- rbind(FIGURE_MANIFEST, save_plot_triplet(p4_7, "Fig4-7_covariate_forest_with_shape_legend_bw", width = 7.6, height = 7.2, dpi = 600))
  }
}

if (is.data.frame(FIGURE_MANIFEST) && nrow(FIGURE_MANIFEST) > 0) {
  FIGURE_MANIFEST$dataset_id <- DATASET_ID
  FIGURE_MANIFEST$analysis_id <- ANALYSIS_ID
  rownames(FIGURE_MANIFEST) <- NULL

  write_csv_safe(FIGURE_MANIFEST, file.path(DIR_FIGURES, "FIGURE_MANIFEST.csv"))
  save_step_rds(FIGURE_MANIFEST, "FIGURE_MANIFEST", dir_rds = DIR_RDS)

  FIGURE_SUMMARY <- list(
    created = TRUE,
    n_figures = nrow(FIGURE_MANIFEST),
    created_at = Sys.time()
  )
}

save_step_rds(FIGURE_SUMMARY, "FIGURE_SUMMARY", dir_rds = DIR_RDS)

log_step_end("figures", round(as.numeric(difftime(Sys.time(), T0_FIG, units = "secs")), 2), ok = TRUE)
