# ============================================================
# fig_style.R
# Common SCI-style figure helpers
# ------------------------------------------------------------
# 역할
# 1) 공통 ggplot theme 제공
# 2) 축/숫자 포맷 helper 제공
# 3) 고품질 PNG/TIFF/PDF 동시 저장 함수 제공
# ============================================================

# ------------------------------------------------------------
# 0. helpers
# ------------------------------------------------------------
`%||%` <- function(x, y) if (is.null(x)) y else x

ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(path)
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

fmt_num <- function(x, digits = 2) {
  ifelse(is.na(x), NA_character_, sprintf(paste0("%.", digits, "f"), x))
}

fmt_pct <- function(x, digits = 1) {
  ifelse(is.na(x), NA_character_, sprintf(paste0("%.", digits, "f%%"), x * 100))
}

fmt_p <- function(p) {
  p <- safe_numeric(p)
  out <- ifelse(
    is.na(p), NA_character_,
    ifelse(p < .001, "<.001", sprintf("%.3f", p))
  )
  out
}

# ------------------------------------------------------------
# 1. theme
# ------------------------------------------------------------
theme_sci <- function(base_family = "sans", base_size = 10) {
  ggplot2::theme_classic(base_family = base_family, base_size = base_size) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(
        face        = "bold",
        size        = base_size + 2,
        hjust       = 0
      ),
      plot.subtitle = ggplot2::element_text(
        size        = base_size,
        hjust       = 0
      ),
      plot.caption  = ggplot2::element_text(
        size        = base_size - 1,
        hjust       = 1
      ),
      axis.title       = ggplot2::element_text(face = "bold"),
      axis.text        = ggplot2::element_text(color = "black"),
      axis.line        = ggplot2::element_line(color = "black", linewidth = 0.35),
      axis.ticks       = ggplot2::element_line(color = "black", linewidth = 0.35),
      axis.ticks.length = grid::unit(2.5, "pt"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.border     = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      legend.title     = ggplot2::element_text(face = "bold"),
      legend.position  = "right",
      strip.text       = ggplot2::element_text(face = "bold"),
      strip.background = ggplot2::element_blank(),
      plot.margin      = ggplot2::margin(8, 12, 8, 8)
    )
}

# ------------------------------------------------------------
# 2. scales
# ------------------------------------------------------------
scale_y_continuous_sci <- function(digits = 2, ...) {
  ggplot2::scale_y_continuous(
    labels = function(x) sprintf(paste0("%.", digits, "f"), x),
    ...
  )
}

scale_x_continuous_sci <- function(digits = 2, ...) {
  ggplot2::scale_x_continuous(
    labels = function(x) sprintf(paste0("%.", digits, "f"), x),
    ...
  )
}

scale_y_percent_sci <- function(digits = 0, ...) {
  ggplot2::scale_y_continuous(
    labels = function(x) sprintf(paste0("%.", digits, "f%%"), x * 100),
    ...
  )
}

scale_x_percent_sci <- function(digits = 0, ...) {
  ggplot2::scale_x_continuous(
    labels = function(x) sprintf(paste0("%.", digits, "f%%"), x * 100),
    ...
  )
}

# ------------------------------------------------------------
# 3. safe device close
# ------------------------------------------------------------
safe_dev_off <- function() {
  try(grDevices::dev.off(), silent = TRUE)
  invisible(NULL)
}

# ------------------------------------------------------------
# 4. save figure to one high-resolution PNG
# ------------------------------------------------------------
save_figure_all <- function(
    plot,
    filename,
    dir_output,
    width = 6,
    height = 4,
    units = "in",
    dpi_png = 600,
    dpi_tiff = 600,
    compression_tiff = "lzw"
) {
  stopifnot(!missing(plot), !missing(filename), !missing(dir_output))

  dir_png  <- file.path(dir_output, "figures", "png")
  ensure_dir(dir_png)

  path_png  <- file.path(dir_png,  paste0(filename, ".png"))

  ragg::agg_png(
    filename = path_png,
    width = width,
    height = height,
    units = units,
    res = dpi_png
  )
  print(plot)
  safe_dev_off()

  invisible(list(
    png = path_png
  ))
}

# ------------------------------------------------------------
# 5. convenience plot wrappers
# ------------------------------------------------------------
rotate_x_text <- function(angle = 30, hjust = 1, vjust = 1) {
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = angle, hjust = hjust, vjust = vjust)
  )
}

remove_legend <- function() {
  ggplot2::theme(legend.position = "none")
}
