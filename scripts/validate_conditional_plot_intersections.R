if (identical(.Platform$OS.type, "windows")) {
  invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
}
source("R/setup_mediation_moderation_ui.R", encoding = "UTF-8")
`%||%` <- function(x, y) if (is.null(x)) y else x

# Predictions from M = 2 + .8 X + 1.395 W - .3 XW meet at X = 4.65.
make_lines <- function(interaction = -.3, w_effect = 1.395) {
  do.call(rbind, lapply(seq_along(c(3.5, 4.12, 4.73)), function(i) {
    x <- seq(1, 5, length.out = 40)
    w <- c(3.5, 4.12, 4.73)[[i]]
    data.frame(x = x, yhat = 2 + .8 * x + w_effect * w + interaction * x * w,
      moderator_level = c("M-SD", "Mean", "M+SD")[[i]])
  }))
}
points <- mediation_moderation_plot_intersections(make_lines())
stopifnot(nrow(points) == 1L, abs(points$x - 4.65) < 1e-10)
stopifnot(nrow(mediation_moderation_plot_intersections(make_lines(0))) == 0L)
stopifnot(nrow(mediation_moderation_plot_intersections(make_lines(0, 0))) == 0L)
spec <- list(plot_df = make_lines(), title = "Conditional effects", x_label = "X",
  y_label = "M", moderator_label = "W")
plot <- mediation_moderation_build_moderation_plot(spec)
stopifnot(is.null(plot$labels$subtitle))
built <- ggplot2::ggplot_build(plot)
stopifnot(length(built$data) == 4L, abs(built$data[[2]]$xintercept - 4.65) < 1e-10,
  built$data[[4]]$label == "x = 4.650", abs(built$data[[4]]$x - 4.65) < 1e-10)
spec$plot_df <- make_lines(w_effect = 3)
outside <- mediation_moderation_build_moderation_plot(spec)
stopifnot(grepl("outside observed range", outside$labels$subtitle, fixed = TRUE))
stopifnot(length(ggplot2::ggplot_build(outside)$data) == 1L)
message("PASS: exact intersections, duplicate roots, parallel/coincident lines, and out-of-range rendering")
