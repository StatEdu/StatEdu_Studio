Sys.setlocale('LC_ALL', 'Korean_Korea.utf8')
source('R/app_bootstrap.R', encoding = 'UTF-8')
load_app_packages(check = FALSE)
source_app_modules()
out <- 'tmp/portrait-accumulated-exports'
dir.create(out, recursive = TRUE, showWarnings = FALSE)
set.seed(130)
d <- data.frame(group = factor(rep(c('20~34세', '35세 이상'), each = 40)),
                job = factor(rep(c('재직', '휴직', '무직', '재직'), each = 20)), y = rnorm(80))
d$y <- d$y + 3 * as.numeric(d$job) + as.numeric(d$group)
info <- data.frame(name = names(d), measurement = c('binary', 'category', 'continuous'),
                   var_label = c('GCM 엄마나이 (35세 기준)', 'GCM(M) Occupation', '양육스트레스 평균'))
for (df in c(FALSE, TRUE)) {
  result <- prepare_ttest_anova_results(d, 'y', c('group', 'job'), variable_info = info,
    options = list(normality_enabled = FALSE, mean_sd = FALSE, show_df = df, post_hoc = TRUE, effect_size = TRUE))
  html <- saved_ttest_anova_results_html(result)
  doc <- xml2::read_html(html)
  main <- xml2::xml_find_all(doc, ".//div[contains(@class,'ttest-anova-result-panel')]//table")
  stopifnot(length(main) > 0, all(xml2::xml_attr(main, 'data-result-table-orientation') == 'portrait'))
  entry <- list(title = 't-test / ANOVA', html = html)
  tables <- result_entry_tables(entry)
  main_info <- Filter(function(x) identical(x$title, '양육스트레스 평균'), tables)
  stopifnot(length(main_info) == 1, !result_docx_wide_table(main_info[[1]]))
  stem <- file.path(out, if (df) 'with-df' else 'standard')
  writeLines(html, paste0(stem, '.html'), useBytes = TRUE)
  save_ttest_anova_excel_file(result, paste0(stem, '.xlsx'))
  write_result_collection_docx(list(entry), paste0(stem, '.docx'))
  write_ttest_anova_results_pdf(result, paste0(stem, '.pdf'))
}
# Self-contained figures must survive collection normalization and Office export.
png(file.path(out, 'figure.png'), width = 600, height = 300, bg = 'transparent')
plot.new(); arrows(.2, .5, .8, .5, lty = 2); text(.5, .65, '.107 (.775)')
dev.off()
uri <- saved_results_image_data_uri(file.path(out, 'figure.png'))
entries <- lapply(c('Mediation / Moderation Effects', 'CFA', 'SEM', 'PLS-SEM'), function(title) {
  fragment <- paste0('<div class="model-canvas-report-figure"><h3>', title,
    '</h3><img class="analysis-plot-image" alt="Model results" width="600" height="300" src="', uri, '"></div>')
  list(title = title, html = result_snapshot_document_html(title, fragment))
})
entries <- normalize_result_snapshot_entries(entries)
for (entry in entries) {
  images <- result_entry_images(entry)
  stopifnot(length(images) == 1, images[[1]]$width_px == 600, images[[1]]$height_px == 300)
  unlink(images[[1]]$path)
}
save_result_collection_excel_file(entries, file.path(out, 'models.xlsx'))
write_result_collection_docx(entries, file.path(out, 'models.docx'))
for (extension in c('xlsx', 'docx')) {
  listing <- unzip(file.path(out, paste0('models.', extension)), list = TRUE)$Name
  stopifnot(sum(grepl('/media/.*png$', listing)) >= 1)
  # Word may deduplicate identical PNG bytes; each model still needs its own drawing.
  if (extension == 'docx') {
    document <- xml2::read_xml(unz(file.path(out, 'models.docx'), 'word/document.xml'))
    stopifnot(length(xml2::xml_find_all(document, "//*[local-name()='drawing']")) >= 4)
  } else stopifnot(sum(grepl('/media/.*png$', listing)) == 4)
}
cat('PASS: t-test portrait with/without df; HTML/PDF/Word/Excel generated; four accumulated model figures embedded in Word/Excel.\n')
