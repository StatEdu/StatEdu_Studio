.libPaths(R.home("library"))
source("R/app_bootstrap.R")
load_app_packages(check = FALSE)
source_app_modules()
statedu_apply_preferences()
directory <- "output/survival-ph-height-regression"
dir.create(directory, recursive = TRUE, showWarnings = FALSE)

for (terms in c(0L, 1L, 4L, 6L, 10L, 20L)) {
  result <- list(ph = list(y = matrix(0, 2, terms)))
  stopifnot(survival_cox_ph_plot_height(result) == max(620, 240 * ceiling(terms / 2)))
  stopifnot(survival_cox_ph_plot_height(result, 760) == max(760, 240 * ceiling(terms / 2)))
}
stopifnot(survival_cox_ph_plot_height(list()) == 620)

set.seed(42)
data <- as.data.frame(matrix(rnorm(500 * 20), 500, 20))
variables <- names(data)
data$time <- rexp(500)
data$status <- rbinom(500, 1, .7)
result <- prepare_cox_analysis_result(data, "time", "status", variables)
original <- serialize(result, NULL)
for (minimum in c(620, 760)) {
  grDevices::png(file.path(directory, paste0("ph-", minimum, ".png")),
    width = 1000, height = survival_cox_ph_plot_height(result, minimum), res = 110)
  tryCatch(survival_cox_ph_plot(result), finally = grDevices::dev.off())
}
specs <- survival_saved_plot_specs(result)
ph_spec <- Filter(function(spec) identical(spec$id, "survival_cox_ph_plot"), specs)[[1L]]
stopifnot(ph_spec$height == 2400)
data$status <- sample(0:2, nrow(data), replace = TRUE)
competing <- prepare_competing_risk_result(data, "time", "status",
  covariates = variables, regression = "both")
stopifnot(identical(ncol(competing$cause_specific$ph$y), 20L),
  length(competing$fine_gray$fit$coef) == 20L)
specs <- survival_saved_plot_specs(competing)
ph_spec <- Filter(function(spec) identical(spec$id, "survival_cause_specific_ph_plot"), specs)[[1L]]
stopifnot(ph_spec$height == 2400, identical(original, serialize(result, NULL)))
competing_original <- serialize(competing, NULL)
grDevices::png(file.path(directory, "cause-specific-ph.png"), width = 1000,
  height = ph_spec$height, res = 110)
tryCatch(ph_spec$draw(), finally = grDevices::dev.off())
stopifnot(identical(competing_original, serialize(competing, NULL)))

# Preserve ordinary serialization exactly, including URI newline escaping.
for (count in c(0L, 40L, 70000L)) for (line_break in c("", "\n", "\r\n")) {
  uri <- paste0("data:image/png;base64,", paste(rep(paste0("abCD", line_break), count), collapse = ""))
  document <- xml2::read_html(paste0('<div><img src="', uri, '"><p>unchanged</p></div>'))
  node <- xml2::xml_find_first(document, ".//body/div")
  source <- xml2::xml_attr(xml2::xml_find_first(node, ".//img"), "src")
  stopifnot(identical(as.character(node), result_node_html(node)))
  stopifnot(identical(source, xml2::xml_attr(xml2::xml_find_first(node, ".//img"), "src")))
}

# Large embedded URI and a placeholder-like existing source must survive intact.
uri <- paste0("data:image/png;base64,", paste(rep("abCD\n", 300000), collapse = ""))
document <- xml2::read_html(paste0('<div><img src="', uri, '"><img src="STATEDU_IMAGE_URI_1_END"></div>'))
node <- xml2::xml_find_first(document, ".//body/div")
sources <- xml2::xml_attr(xml2::xml_find_all(node, ".//img"), "src")
html <- result_node_html(node)
stopifnot(grepl('src="STATEDU_IMAGE_URI_1_END"', html, fixed = TRUE))
stopifnot(grepl(gsub("\n", "%0A", uri, fixed = TRUE), html, fixed = TRUE))
stopifnot(identical(sources, xml2::xml_attr(xml2::xml_find_all(node, ".//img"), "src")))
cat("PASS: PH layout/rendering, Cox and cause-specific save specs, analysis preservation, embedded URI fidelity.\n")
