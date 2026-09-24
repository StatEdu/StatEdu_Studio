Sys.setlocale('LC_CTYPE', 'English_United States.utf8')
Sys.setenv(STATEDU_MODULE_CACHE = 'false')
.libPaths(R.home('library'))
source('R/app_bootstrap.R', encoding = 'UTF-8')
load_app_packages(check = FALSE); source_app_modules()
out <- 'tmp/excel-cover'
dir.create(out, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(STATEDU_LANGUAGE = 'ko')
png(file.path(out, 'figure.png'), width = 400, height = 240)
plot(1:5, 1:5, main = 'Example plot'); dev.off()
image <- saved_results_image_data_uri(file.path(out, 'figure.png'))
html <- paste0('<h2>회귀분석 결과</h2><h3>표 1. 모형 요약</h3>',
  '<table><thead><tr><th rowspan="2">변수</th><th colspan="2">계수</th></tr>',
  '<tr><th>B</th><th>p</th></tr></thead><tbody><tr><td>설명변수</td><td>0.0000</td><td>&lt;.001</td></tr></tbody></table>',
  '<p>표의 해석과 설명을 보존합니다.</p><h3>그림 1. 추세</h3><img src="', image, '">')
entries <- list(list(title = '회귀분석', html = html),
  list(title = '추가 분석', html = '<h3>표 1. 모형 요약</h3><table><tr><th>지표</th><th>값</th></tr><tr><td>R²</td><td>.450</td></tr></table>'),
  list(title = '해석', html = '<p>표 없이 저장된 해석입니다.</p>'))
read_xml_member <- function(file, member) xml2::read_xml(unz(file, member))
cover_fn <- add_result_excel_cover
for (mode in c('current', 'accumulated')) {
  selected <- if (mode == 'current') entries[1] else entries
  baseline <- file.path(out, paste0(mode, '-baseline.xlsx'))
  target <- file.path(out, paste0(mode, '.xlsx'))
  add_result_excel_cover <- function(...) invisible(NULL)
  save_result_collection_excel_file(selected, baseline)
  add_result_excel_cover <- cover_fn
  save_result_collection_excel_file(selected, target)
  old_names <- openxlsx::getSheetNames(baseline)
  new_names <- openxlsx::getSheetNames(target)
  stopifnot(length(new_names) == length(old_names) + 1L, identical(new_names[-1], old_names))
  doc <- read_xml_member(target, 'xl/workbook.xml')
  stopifnot(xml2::xml_attr(xml2::xml_find_first(doc, '//*[local-name()="workbookView"]'), 'activeTab') == '0')
  for (sheet in old_names) {
    old <- openxlsx::read.xlsx(baseline, sheet, colNames = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE)
    new <- openxlsx::read.xlsx(target, sheet, colNames = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE)
    before <- as.matrix(old); after <- as.matrix(new)[seq_len(nrow(old)), seq_len(ncol(old)), drop = FALSE]
    stopifnot(identical(unname(before[!is.na(before)]), unname(after[!is.na(before)])))
  }
  archive <- unzip(target, list = TRUE)$Name
  xml_sheets <- archive[grepl('^xl/worksheets/sheet[0-9]+[.]xml$', archive)]
  links <- unlist(lapply(xml_sheets, function(member) xml2::xml_attr(xml2::xml_find_all(read_xml_member(target, member), '//*[local-name()="hyperlink"]'), 'location')))
  stopifnot(length(links) == 2L * length(old_names))
  for (sheet in new_names) {
    dest <- paste0("'", gsub("'", "''", sheet, fixed = TRUE), "'!A1")
    stopifnot(any(grepl(dest, links, fixed = TRUE)))
  }
  cover <- openxlsx::read.xlsx(target, 1, colNames = FALSE)
  stopifnot(any(grepl('StatEdu Studio', unlist(cover), fixed = TRUE)), any(grepl('이일현|Il Hyun Lee', unlist(cover))))
  cat('PASS:', mode, 'cover first and active, bidirectional links, unchanged captured result cells\n')
}
# Collisions, apostrophes, double quotes and long/duplicate titles.
wb <- openxlsx::createWorkbook()
special <- c('표지', 'Cover', "O'Brien", 'A "quoted" result')
for (sheet in special) {openxlsx::addWorksheet(wb, sheet); openxlsx::writeData(wb, sheet, data.frame(Value = '0.0000'))}
add_result_excel_cover(wb, language = 'ko')
special_file <- file.path(out, 'special.xlsx')
openxlsx::saveWorkbook(wb, special_file, overwrite = TRUE)
result_finalize_excel_package(special_file)
stopifnot(identical(openxlsx::getSheetNames(special_file)[1], '표지_1'))
special_links <- unlist(lapply(unzip(special_file, list = TRUE)$Name[grepl('^xl/worksheets/sheet[0-9]+[.]xml$', unzip(special_file, list = TRUE)$Name)],
  function(member) xml2::xml_attr(xml2::xml_find_all(read_xml_member(special_file, member), '//*[local-name()="hyperlink"]'), 'location')))
stopifnot(all(paste0("'", gsub("'", "''", special, fixed = TRUE), "'!A1") %in% special_links))
structural_canvas_write_result_workbook(list(Fit = data.frame(Index = 'CFI', Value = .95)), file.path(out, 'structural.xlsx'))
stopifnot(length(openxlsx::getSheetNames(file.path(out, 'structural.xlsx'))) == 2L)
cat('PASS: cover name collision and special-character sheet names\n')
