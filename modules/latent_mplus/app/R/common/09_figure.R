# ============================================================
# 09_figure.R
# Figure helpers for mixture analysis pipeline
# ------------------------------------------------------------
# 역할
# 1) 공통 figure theme / save helper 제공
# 2) model fit / class size / indicator profile 그림 함수 제공
# 3) png / tiff / pdf 동시 저장 지원
# 4) figure manifest row 생성 지원
# ============================================================

# ------------------------------------------------------------
# 0. helpers
# ------------------------------------------------------------
`%||%` <- function(x, y) if (is.null(x)) y else x

.has_pkg_fig <- function(pkg) requireNamespace(pkg, quietly = TRUE)

.ensure_dir_fig <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

.norm_path_fig <- function(x) {
  x <- gsub("\\\\", "/", as.character(x))
  x <- gsub("/+", "/", x)
  x
}

.safe_info_fig <- function(...) {
  if (exists("log_info")) log_info(...) else cat(paste0(..., "\n"))
}

.safe_warn_fig <- function(...) {
  if (exists("log_warn")) log_warn(...) else cat(paste0("[WARN] ", ..., "\n"))
}

.safe_stop_fig <- function(...) {
  stop(paste0(...), call. = FALSE)
}

fmt_pct_fig <- function(x, digits = 1) {
  ifelse(is.na(x), NA_character_,
         paste0(formatC(100 * x, format = "f", digits = digits), "%"))
}

# ------------------------------------------------------------
# 1. theme / size helpers
# ------------------------------------------------------------
theme_publication_fig <- function(base_size = 12) {
  if (!.has_pkg_fig("ggplot2")) return(NULL)

  if (exists("theme_sci", mode = "function")) {
    return(theme_sci(base_size = base_size))
  }

  ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(linewidth = 0.35, colour = "black"),
      axis.ticks = ggplot2::element_line(linewidth = 0.35, colour = "black"),
      legend.title = ggplot2::element_text(face = "bold"),
      axis.title = ggplot2::element_text(face = "bold"),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )
}

get_fig_width <- function(name, default = 8) {
  fn <- get0("fig_width", ifnotfound = NULL)
  if (is.function(fn)) {
    out <- tryCatch(fn(name), error = function(e) default)
    return(out %||% default)
  }
  default
}

get_fig_height <- function(name, default = 6) {
  fn <- get0("fig_height", ifnotfound = NULL)
  if (is.function(fn)) {
    out <- tryCatch(fn(name), error = function(e) default)
    return(out %||% default)
  }
  default
}

# ------------------------------------------------------------
# 2. manifest / save helpers
# ------------------------------------------------------------
make_figure_manifest_row <- function(file_stub,
                                     png,
                                     tiff,
                                     pdf,
                                     width,
                                     height,
                                     dpi = 600,
                                     figure_title = NA_character_) {
  data.frame(
    figure_id = file_stub,
    file_stub = file_stub,
    figure_title = figure_title,
    png = .norm_path_fig(png),
    tiff = .norm_path_fig(tiff),
    pdf = .norm_path_fig(pdf),
    width = width,
    height = height,
    dpi = dpi,
    stringsAsFactors = FALSE
  )
}

save_plot_all_formats <- function(plot_obj,
                                  file_stub,
                                  dir_png,
                                  dir_tiff,
                                  dir_pdf,
                                  width = 8,
                                  height = 6,
                                  dpi = 600,
                                  figure_title = NA_character_) {
  if (!.has_pkg_fig("ggplot2")) {
    .safe_stop_fig("ggplot2 package is required to save figures.")
  }

  .ensure_dir_fig(dir_png)
  single_output_mode <- isTRUE(getOption("easyflow.single_output", TRUE))
  if (!single_output_mode) {
    .ensure_dir_fig(dir_tiff)
    .ensure_dir_fig(dir_pdf)
  }

  png_file  <- file.path(dir_png, paste0(file_stub, ".png"))
  tiff_file <- if (single_output_mode) NA_character_ else file.path(dir_tiff, paste0(file_stub, ".tiff"))
  pdf_file  <- if (single_output_mode) NA_character_ else file.path(dir_pdf, paste0(file_stub, ".pdf"))

  ggplot2::ggsave(
    filename = png_file,
    plot = plot_obj,
    width = width,
    height = height,
    dpi = dpi,
    units = "in"
  )

  if (!single_output_mode) {
    ggplot2::ggsave(
      filename = tiff_file,
      plot = plot_obj,
      width = width,
      height = height,
      dpi = dpi,
      units = "in",
      compression = "lzw"
    )

    ggplot2::ggsave(
      filename = pdf_file,
      plot = plot_obj,
      width = width,
      height = height,
      units = "in"
    )
  }

  make_figure_manifest_row(
    file_stub = file_stub,
    png = png_file,
    tiff = tiff_file,
    pdf = pdf_file,
    width = width,
    height = height,
    dpi = dpi,
    figure_title = figure_title
  )
}

empty_figure_manifest <- function() {
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

# ------------------------------------------------------------
# 3. data prep helpers for figures
# ------------------------------------------------------------
make_fit_long_df <- function(fit_df, metrics = c("bic", "aic", "sabic")) {
  if (!is.data.frame(fit_df) || nrow(fit_df) == 0 || !"k" %in% names(fit_df)) {
    return(data.frame())
  }

  metrics <- intersect(metrics, names(fit_df))
  if (length(metrics) == 0) return(data.frame())

  out_list <- vector("list", length(metrics))
  for (i in seq_along(metrics)) {
    nm <- metrics[i]
    out_list[[i]] <- data.frame(
      k = fit_df$k,
      metric = toupper(nm),
      value = fit_df[[nm]],
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, out_list)
  out <- out[!is.na(out$value), , drop = FALSE]
  rownames(out) <- NULL
  out
}

make_class_prop_df <- function(class_df) {
  if (!is.data.frame(class_df) || nrow(class_df) == 0) return(data.frame())
  if (!all(c("class", "n") %in% names(class_df))) return(data.frame())

  out <- class_df
  if (!"prop" %in% names(out)) {
    out$prop <- out$n / sum(out$n, na.rm = TRUE)
  }
  out$label <- fmt_pct_fig(out$prop, digits = 1)
  rownames(out) <- NULL
  out
}

make_indicator_profile_df_lpa <- function(t3_df) {
  if (!is.data.frame(t3_df) || nrow(t3_df) == 0) return(data.frame())
  need <- c("var_label", "class", "mean")
  if (!all(need %in% names(t3_df))) return(data.frame())

  out <- t3_df
  out$var_label <- factor(out$var_label, levels = unique(out$var_label))
  rownames(out) <- NULL
  out
}

make_indicator_profile_df_lca <- function(t3_df) {
  if (!is.data.frame(t3_df) || nrow(t3_df) == 0) return(data.frame())
  need <- c("var_label", "class", "prop")
  if (!all(need %in% names(t3_df))) return(data.frame())

  out <- t3_df
  if ("level" %in% names(out)) {
    out$indicator_label <- paste0(out$var_label, ": ", out$level)
  } else if ("value_label" %in% names(out)) {
    out$indicator_label <- paste0(out$var_label, ": ", out$value_label)
  } else {
    out$indicator_label <- out$var_label
  }
  out$indicator_label <- factor(out$indicator_label, levels = unique(out$indicator_label))
  rownames(out) <- NULL
  out
}

# ------------------------------------------------------------
# 4. figure builders
# ------------------------------------------------------------
build_fig_model_fit <- function(fit_df,
                                metrics = c("bic", "aic", "sabic"),
                                title = "Model fit across candidate classes") {
  if (!.has_pkg_fig("ggplot2")) return(NULL)

  long_df <- make_fit_long_df(fit_df, metrics = metrics)
  if (nrow(long_df) == 0) return(NULL)

  ggplot2::ggplot(
    long_df,
    ggplot2::aes(x = k, y = value, group = metric, linetype = metric, shape = metric)
  ) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::scale_x_continuous(breaks = sort(unique(long_df$k))) +
    ggplot2::labs(
      title = title,
      x = "Number of classes (k)",
      y = "Fit index",
      linetype = "Metric",
      shape = "Metric"
    ) +
    theme_publication_fig()
}

build_fig_class_proportion <- function(class_df,
                                       title = "Class proportions") {
  if (!.has_pkg_fig("ggplot2")) return(NULL)

  df <- make_class_prop_df(class_df)
  if (nrow(df) == 0) return(NULL)

  ggplot2::ggplot(df, ggplot2::aes(x = class, y = prop)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = label), vjust = -0.4, size = 4) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = "Proportion"
    ) +
    theme_publication_fig()
}

build_fig_indicator_profile_lpa <- function(t3_df,
                                            title = "Indicator profile by class",
                                            add_errorbar = TRUE) {
  if (!.has_pkg_fig("ggplot2")) return(NULL)

  df <- make_indicator_profile_df_lpa(t3_df)
  if (nrow(df) == 0) return(NULL)

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = var_label, y = mean, group = class, linetype = class, shape = class)
  ) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = "Mean"
    ) +
    theme_publication_fig() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  if (isTRUE(add_errorbar) && "se" %in% names(df)) {
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mean - se, ymax = mean + se),
      width = 0.10,
      linewidth = 0.3
    )
  }

  p
}

build_fig_indicator_profile_lca <- function(t3_df,
                                            title = "Indicator response profile by class") {
  if (!.has_pkg_fig("ggplot2")) return(NULL)

  df <- make_indicator_profile_df_lca(t3_df)
  if (nrow(df) == 0) return(NULL)

  ggplot2::ggplot(
    df,
    ggplot2::aes(x = indicator_label, y = prop, group = class, linetype = class, shape = class)
  ) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = "Probability / proportion"
    ) +
    theme_publication_fig() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}

build_fig_indicator_profile <- function(t3_df,
                                        mixture_mode = c("lpa", "lca"),
                                        ...) {
  mixture_mode <- match.arg(tolower(mixture_mode), c("lpa", "lca"))
  if (mixture_mode == "lca") {
    build_fig_indicator_profile_lca(t3_df, ...)
  } else {
    build_fig_indicator_profile_lpa(t3_df, ...)
  }
}

# ------------------------------------------------------------
# 5. registry helpers
# ------------------------------------------------------------
append_figure_manifest <- function(manifest, row) {
  if (is.null(manifest) || !is.data.frame(manifest) || nrow(manifest) == 0) {
    out <- row
  } else {
    out <- rbind(manifest, row)
  }
  rownames(out) <- NULL
  out
}

combine_figure_manifests <- function(...) {
  xs <- list(...)
  xs <- xs[vapply(xs, is.data.frame, logical(1))]
  if (length(xs) == 0) return(empty_figure_manifest())

  out <- do.call(rbind, xs)
  rownames(out) <- NULL
  out
}

# ------------------------------------------------------------
# 6. load message
# ------------------------------------------------------------
cat("\n============================================================\n")
cat("09_figure.R loaded\n")
cat("------------------------------------------------------------\n")
cat("Figure helpers registered\n")
cat("============================================================\n\n")
