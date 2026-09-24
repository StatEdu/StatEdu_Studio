Sys.setenv(STATEDU_MODULE_CACHE = "false")
invisible(Sys.setlocale("LC_ALL", "English_United States.utf8"))
source("R/app_bootstrap.R", encoding = "UTF-8")
load_app_packages(check = FALSE)
source_app_modules()
out <- "tmp/canvas-covariate-exports"

# Verify that list-assigned controls are captured from the existing fit.
set.seed(319)
n <- 150
age <- rnorm(n)
latent <- .4 * age + rnorm(n)
d <- data.frame(age = age, y1 = latent + rnorm(n), y2 = latent + rnorm(n), y3 = latent + rnorm(n))
fit <- lavaan::sem("Y =~ y1 + y2 + y3\nY ~ age", data = d)
snapshot <- list(nodes = list(list(id = "y", name = "Y", role = "latent", x = 300, y = 100)),
  edges = list(), covariates = "age")
result <- structural_canvas_result_snapshot(snapshot, fit, coefficient = "beta_p")
parameters <- lavaan::parameterEstimates(fit, standardized = TRUE)
row <- parameters[parameters$op == "~" & parameters$rhs == "age", ]
stopifnot(length(result$covariateEffects) == 1L,
  identical(result$covariateEffects[[1L]]$variable, "age"),
  identical(result$covariateEffects[[1L]]$target, "Y"),
  identical(result$covariateEffects[[1L]]$label, sprintf("%s(%s)", format_decimal3(row$std.all), format_p(row$pvalue))))
message("PASS: control effects captured from fitted parameters without refitting")

payload <- jsonlite::read_json(file.path(out, "cbsem.json"))
paths <- save_canvas_figure_snapshots(payload$files, out, "covariate-validation")
stopifnot(length(paths) == 2L)
for (index in seq_along(paths)) stopifnot(identical(
  readBin(paths[[index]], "raw", n = file.info(paths[[index]])$size),
  jsonlite::base64_dec(sub("^data:image/png;base64,", "", payload$files[[index]]$data))))
message("PASS: both PNG files saved byte-for-byte through the folder-save handler")

html <- paste(readLines(file.path(out, "cbsem.html"), encoding = "UTF-8", warn = FALSE), collapse = "\n")
for (mode in c("current", "accumulated")) {
  entries <- list(list(id = "sem", title = "SEM", html = html))
  if (mode == "accumulated") entries <- c(list(list(id = "prior", title = "Prior", html = "<p>Earlier result preserved.</p>")), entries)
  stem <- file.path(out, mode)
  write_result_collection_html(entries, paste0(stem, ".html"))
  write_result_collection_docx(entries, paste0(stem, ".docx"))
  save_result_collection_excel_file(entries, paste0(stem, ".xlsx"))
  write_result_collection_pdf(entries, paste0(stem, ".pdf"))
  write_result_collection_hwpx(entries, paste0(stem, ".hwpx"))
  for (ext in c("docx", "xlsx", "hwpx")) {
    members <- unzip(paste0(stem, ".", ext), list = TRUE)$Name
    stopifnot(sum(grepl("[.]png$", members, ignore.case = TRUE)) >= 2L)
  }
  saved <- xml2::read_html(paste0(stem, ".html"))
  stopifnot(length(xml2::xml_find_all(saved, "//img[contains(@class,'analysis-plot-image')]")) == 2L)
  stopifnot(file.info(paste0(stem, ".pdf"))$size > 1000)
  message("PASS: ", mode, " paired figures in HTML/PDF/Word/HWPX/Excel")
}
