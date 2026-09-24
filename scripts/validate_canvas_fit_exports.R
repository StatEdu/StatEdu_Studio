Sys.setenv(STATEDU_MODULE_CACHE = "false")
invisible(Sys.setlocale("LC_ALL", "English_United States.utf8"))
source("R/app_bootstrap.R", encoding = "UTF-8")
load_app_packages(check = FALSE)
source_app_modules()
out <- "tmp/canvas-fit-bounds"
html <- paste(readLines(file.path(out, "sem.html"), encoding = "UTF-8", warn = FALSE), collapse = "\n")
for (mode in c("current", "accumulated")) {
  entries <- list(list(id = "fit", title = "Fitted canvas", html = html))
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
  message("PASS: ", mode, " fitted model figures in HTML/PDF/Word/HWPX/Excel")
}
