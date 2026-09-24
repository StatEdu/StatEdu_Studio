if (.Platform$OS.type == 'windows') Sys.setlocale('LC_CTYPE','Korean_Korea.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
uri <- saved_results_image_data_uri('www/logo-horizontal.png')
entry <- list(title='Shared parse',html=paste0('<h2>Table title</h2><div class="result-table-with-note"><table><tr><th>X</th></tr><tr><td>1.234<sup>a</sup></td></tr></table><p class="coefficient-note">a = retained note</p></div>',
  '<section data-result-table-orientation="landscape"><div class="mm-result-diagram-section"><h3>Model</h3><p>Diagram text</p></div></section>',
  '<h2>Chart</h2><img width="400" height="200" src="',uri,'">'))
result_diagram_image_uri <- function(diagram) uri
doc <- result_entry_document(entry)
before <- result_node_html(xml2::xml_root(doc))
legacy <- result_entry_images(entry)
shared <- result_entry_images(entry,document=doc)
stopifnot(identical(before,result_node_html(xml2::xml_root(doc))))
without_path <- function(x) { x$path <- NULL; x }
stopifnot(identical(lapply(legacy,without_path),lapply(shared,without_path)),
  identical(unname(tools::md5sum(vapply(legacy,`[[`,character(1),'path'))),unname(tools::md5sum(vapply(shared,`[[`,character(1),'path')))))
unlink(vapply(c(legacy,shared),`[[`,character(1),'path'))
text <- result_entry_paragraphs(entry,document=doc)
stopifnot(identical(result_entry_tables(entry,include_docx=FALSE,document=doc),
  result_entry_tables(entry,include_docx=FALSE,document=doc,text_items=text)))
cat('PASS diagram and image content/order/orientation; shared DOM unchanged; table notes unchanged\n')

counts <- new.env(parent=emptyenv())
for (name in c('result_entry_document','result_entry_paragraphs')) {
  counts[[name]] <- 0L
  wrapper <- local({key<-name;original<-get(name,globalenv());function(...) {counts[[key]]<-counts[[key]]+1L;original(...)}})
  assign(name,wrapper,globalenv())
}
entries <- read_result_snapshot_store('sample/StatEdu_Studio_result_history_20260917_154949.efs-result')
for(layout_only in c(FALSE,TRUE)) {
  counts$result_entry_document <- counts$result_entry_paragraphs <- 0L
  model <- result_document_model(entries,layout_only=layout_only)
  stopifnot(counts$result_entry_document==length(entries), counts$result_entry_paragraphs==length(entries))
  result_document_cleanup(model)
  cat('PASS',if(layout_only)'HWPX' else 'Word','model:',counts$result_entry_document,'HTML parses and',counts$result_entry_paragraphs,'paragraph extractions for',length(entries),'entries\n')
}
