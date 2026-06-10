# ============================================================
# radar_helpers.R
# Radar / profile-like helper functions
# ------------------------------------------------------------
# 역할
# 1) radar 범위 자동 계산
# 2) radar용 long data 정리
# 3) circular radar plot
# 4) polygon/line radar plot
# ============================================================

# ------------------------------------------------------------
# 0. helpers
# ------------------------------------------------------------
`%||%` <- function(x, y) if (is.null(x)) y else x

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

auto_radar_limits <- function(x, pad = 0.05) {
  x <- safe_numeric(x)
  rng <- range(x, na.rm = TRUE)

  if (!all(is.finite(rng))) {
    return(c(0, 1))
  }

  span <- diff(rng)
  if (!is.finite(span) || span <= 0) span <- 1

  c(rng[1] - span * pad, rng[2] + span * pad)
}

# ------------------------------------------------------------
# 1. long formatter
# ------------------------------------------------------------
make_radar_long <- function(df,
                            var_col = "variable",
                            value_col = "value",
                            class_col = "Class",
                            var_levels = NULL) {
  stopifnot(all(c(var_col, value_col, class_col) %in% names(df)))

  out <- df[, c(var_col, value_col, class_col), drop = FALSE]
  names(out) <- c("variable", "value", "Class")

  out$variable <- as.character(out$variable)
  out$Class    <- as.character(out$Class)
  out$value    <- safe_numeric(out$value)

  if (!is.null(var_levels)) {
    var_levels <- unique(as.character(var_levels))
  } else {
    var_levels <- unique(out$variable)
  }

  out$variable <- factor(out$variable, levels = var_levels)
  out
}

# ------------------------------------------------------------
# 2. circular radar
# ------------------------------------------------------------
plot_radar_circular <- function(df_long,
                                title = NULL,
                                base_family = "Pretendard",
                                base_size = 10,
                                y_limits = NULL,
                                reverse_order = FALSE) {
  stopifnot(all(c("variable", "value", "Class") %in% names(df_long)))

  dd <- df_long

  lv <- levels(dd$variable)
  if (is.null(lv)) lv <- unique(as.character(dd$variable))
  if (reverse_order) lv <- rev(lv)

  dd$variable <- factor(as.character(dd$variable), levels = lv)

  if (is.null(y_limits)) {
    y_limits <- auto_radar_limits(dd$value)
  }

  ggplot2::ggplot(
    dd,
    ggplot2::aes(x = variable, y = value, group = Class, color = Class)
  ) +
    ggplot2::geom_polygon(fill = NA, linewidth = 0.8, show.legend = TRUE) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::coord_polar() +
    ggplot2::ylim(y_limits[1], y_limits[2]) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = NULL
    ) +
    theme_sci(base_family = base_family, base_size = base_size) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(linewidth = 0.25)
    )
}

# ------------------------------------------------------------
# 3. polygon / line radar alternative
# ------------------------------------------------------------
plot_radar_polygon <- function(df_long,
                               title = NULL,
                               base_family = "Pretendard",
                               base_size = 10,
                               y_limits = NULL,
                               reverse_order = FALSE,
                               x_text_angle = 30) {
  stopifnot(all(c("variable", "value", "Class") %in% names(df_long)))

  dd <- df_long

  lv <- levels(dd$variable)
  if (is.null(lv)) lv <- unique(as.character(dd$variable))
  if (reverse_order) lv <- rev(lv)

  dd$variable <- factor(as.character(dd$variable), levels = lv)

  if (is.null(y_limits)) {
    y_limits <- auto_radar_limits(dd$value)
  }

  ggplot2::ggplot(
    dd,
    ggplot2::aes(x = variable, y = value, group = Class, color = Class)
  ) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = NULL
    ) +
    scale_y_continuous_sci(limits = y_limits) +
    theme_sci(base_family = base_family, base_size = base_size) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = x_text_angle, hjust = 1)
    )
}

# ------------------------------------------------------------
# 4. panel layout helper
# ------------------------------------------------------------
auto_ncol_panels <- function(n) {
  if (n <= 4) return(2)
  if (n <= 9) return(3)
  if (n <= 16) return(4)
  5
}

auto_panel_size <- function(n) {
  width <- if (n <= 6) 8 else if (n <= 12) 10 else 12
  height <- if (n <= 6) 5 else if (n <= 12) 7 else 9
  list(width = width, height = height)
}