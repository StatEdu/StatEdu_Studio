Sys.setlocale('LC_ALL', 'Korean_Korea.utf8')
source('R/app_bootstrap.R', encoding = 'UTF-8')
load_app_packages(check = FALSE)
source_app_modules()
out <- 'tmp/canvas-export-validation'
dir.create(out, recursive = TRUE, showWarnings = FALSE)
stopifnot(analysis_figure_dpi('free') == 300L,
          analysis_figure_dpi('development') == 600L,
          analysis_figure_dpi('pro') == 600L)
pages <- list(
  mm = custom_model_canvas_workspace(c('x','m','y','w','c','d'), language = 'ko'),
  cfa = structural_equation_workspace(c('x','m','y','w','c','d'), analysis_type = 'cfa', language = 'ko'),
  sem = structural_equation_workspace(c('x','m','y','w','c','d'), analysis_type = 'cbsem', language = 'ko'),
  pls = structural_equation_workspace(c('x','m','y','w','c','d'), analysis_type = 'plssem', language = 'ko'))
for (name in names(pages)) {
  html <- as.character(pages[[name]])
  doc <- xml2::read_html(html)
  stopifnot(length(xml2::xml_find_all(doc, ".//*[contains(@class,'custom-model-sidebar-actions')]//button[@data-action='run']")) == 1L)
  stopifnot(length(xml2::xml_find_all(doc, ".//*[contains(@class,'custom-model-diagram-panel')]//button[@data-action='run' or contains(@onclick,'stateduOpenCommand')]")) == 0L)
  writeLines(html, file.path(out, paste0(name, '.html')), useBytes = TRUE)
}
writeLines(as.character(div(class = 'mm-save-control', analysis_save_buttons(
  'test_html', 'test_pdf', 'test_figure', 'test_excel', 'test_add', language = 'ko'))),
  file.path(out, 'saves.html'), useBytes = TRUE)
writeLines(system.file('www/shared/bootstrap/css/bootstrap.min.css', package = 'shiny'),
  file.path(out, 'bootstrap-path.txt'))
cat('PASS: four canvas sidebars; edition DPI; browser fixtures generated.\n')
