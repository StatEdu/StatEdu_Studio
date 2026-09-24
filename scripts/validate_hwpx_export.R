# Requires Windows with Hancom installed; exercises the actual application writer.
source("scripts/validate_result_export_fidelity.R", encoding = "UTF-8")
figure <- file.path(out, "hwpx-model-fixture.png")
grDevices::png(figure, width = 960, height = 480, res = 120)
graphics::par(mar = rep(0, 4))
graphics::plot.new(); graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
graphics::rect(.08, .38, .32, .62); graphics::text(.20, .5, "X")
graphics::rect(.68, .38, .92, .62); graphics::text(.80, .5, "Y")
graphics::arrows(.32, .5, .68, .5, length = .12)
graphics::text(.50, .58, ".21(.092)")
grDevices::dev.off()
image_uri <- paste0("data:image/png;base64,", jsonlite::base64_enc(readBin(figure, "raw", n = file.info(figure)$size)))
second <- list(id = "hwpx-korean", title = "한글 검증", html = paste0(
  '<h2>한글 모형 결과 검증</h2><p>설명: 계수와 유의확률</p>',
  '<div data-result-table-sheet="true" data-result-table-orientation="portrait">',
  '<h3>조건부 효과</h3><table><tr><th>경로</th><th>계수(p)</th></tr>',
  '<tr><td>부모유능감 → 디지털 헬스 리터러시</td><td>.21(.092)</td></tr></table>',
  '<p>주석: 화면의 제목과 설명을 그대로 보존합니다.</p></div>',
  '<h3>모형 그림</h3><img src="', image_uri, '" style="width:640px;height:320px">'))
figure_info <- result_entry_images(second)[[1]]
stopifnot(identical(figure_info$width_px, 640), identical(figure_info$height_px, 320))
write_result_collection_hwpx(list(entry), file.path(out, "current.hwpx"))
write_result_collection_docx(list(entry, second), file.path(out, "accumulated.docx"))
write_result_collection_hwpx(list(entry, second), file.path(out, "accumulated.hwpx"))
save_result_collection_excel_file(list(entry, second), file.path(out, "accumulated.xlsx"))
write_result_collection_html(list(entry, second), file.path(out, "accumulated.html"))
stopifnot(any(grepl("^xl/media/", utils::unzip(file.path(out, "accumulated.xlsx"), list = TRUE)$Name)))

read_sections <- function(path) {
  folder <- tempfile("hwpx-test-"); dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  members <- utils::unzip(path, list = TRUE)$Name
  sections <- members[grepl("^Contents/section[0-9]+\\.xml$", members)]
  sections <- sections[order(as.integer(gsub("[^0-9]", "", sections)))]
  utils::unzip(path, files = c("mimetype", sections), exdir = folder)
  stopifnot(grepl("hwp", paste(readLines(file.path(folder, "mimetype"), warn = FALSE), collapse = "")))
  lapply(file.path(folder, sections), xml2::read_xml)
}
sections <- read_sections(file.path(out, "accumulated.hwpx"))
text <- paste(vapply(sections, function(doc) paste(xml2::xml_text(xml2::xml_find_all(doc, ".//*[local-name()='t']")), collapse = " "), character(1)), collapse = " ")
check_text(text)
stopifnot(all(vapply(c("한글 모형 결과 검증", "조건부 효과", "주석: 화면의 제목과 설명을 그대로 보존합니다.", ".21(.092)"), grepl, logical(1), x = text, fixed = TRUE)))
stopifnot(sum(vapply(sections, function(doc) length(xml2::xml_find_all(doc, ".//*[local-name()='tbl']")), integer(1))) == 3L)
stopifnot(sum(vapply(sections, function(doc) length(xml2::xml_find_all(doc, ".//*[local-name()='pic']")), integer(1))) >= 1L)
directions <- unlist(lapply(sections, function(doc) xml2::xml_attr(xml2::xml_find_all(doc, ".//*[local-name()='pagePr']"), "landscape")))
# Hancom uses WIDELY for portrait and NARROWLY for landscape (round-trip checked).
stopifnot(identical(directions, c("WIDELY", "NARROWLY", "WIDELY")))
message("PASS: actual current/accumulated HWPX conversion; all text, Korean notes, tables, figure and mixed section directions")
