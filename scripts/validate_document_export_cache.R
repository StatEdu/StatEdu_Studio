if (.Platform$OS.type == "windows") Sys.setlocale("LC_CTYPE", "Korean_Korea.utf8")
Sys.setenv(STATEDU_MODULE_CACHE = "false")
source("R/app_bootstrap.R", encoding = "UTF-8")
load_app_packages(check = FALSE); source_app_modules()
out <- "tmp/document-cache"; dir.create(out, recursive = TRUE, showWarnings = FALSE)
entry <- function(value) list(id=value, title=value, html=paste0(
  '<h2>',value,'</h2><table data-result-table-role="main"><tr><th>Value</th></tr><tr><td>',value,
  '</td></tr></table><h2>Appendix</h2><table data-result-table-role="appendix"><tr><th>X</th></tr><tr><td>APPENDIX_VALUE</td></tr></table>'))
calls <- c(word=0L, hwpx=0L)
word_writer <- write_result_collection_docx; hwpx_writer <- write_result_collection_hwpx
write_result_collection_docx <- function(...) { calls['word'] <<- calls['word']+1L; word_writer(...) }
write_result_collection_hwpx <- function(...) { calls['hwpx'] <<- calls['hwpx']+1L; hwpx_writer(...) }
text <- function(path, format) {
  members <- if (format == 'word') 'word/document.xml' else grep('^Contents/section[0-9]+.xml$', unzip(path,list=TRUE)$Name, value=TRUE)
  paste(vapply(members,function(member)xml2::xml_text(xml2::read_xml(unz(path,member))),character(1)),collapse=' ')
}
for (format in c('word','hwpx')) {
  cache <- result_document_export_cache()
  extension <- if (format=='word') '.docx' else '.hwpx'
  first <- file.path(out,paste0(format,'-first',extension)); second <- file.path(out,paste0(format,'-second',extension))
  entries <- list(entry('FIRST_VALUE'),entry('SECOND_VALUE'))
  cache$save(entries,first,format,'main','ko'); n <- calls[format]
  cache$save(entries,second,format,'main','ko')
  stopifnot(calls[format]==n, identical(unname(tools::md5sum(first)),unname(tools::md5sum(second))))
  entries[[1]] <- entry('CHANGED_VALUE')
  cache$save(entries,second,format,'main','ko')
  stopifnot(calls[format]==n+1L, grepl('CHANGED_VALUE',text(second,format)),!grepl('FIRST_VALUE',text(second,format)))
  cache$save(rev(entries),second,format,'main','ko')
  result <- text(second,format)
  stopifnot(calls[format]==n+2L, regexpr('SECOND_VALUE',result)[1] < regexpr('CHANGED_VALUE',result)[1])
  cache$save(entries,second,format,NULL,'ko')
  stopifnot(calls[format]==n+3L, grepl('APPENDIX_VALUE',text(second,format)))
  cache$save(entries,second,format,rev(result_document_content_types()),'ko')
  stopifnot(calls[format]==n+3L)
  cache$save(entries,second,format,NULL,'en')
  stopifnot(calls[format]==n+4L)
  # A second session cannot see/reuse the first session's artifacts.
  other <- result_document_export_cache()
  other$save(entries,second,format,NULL,'en'); other$clear()
  stopifnot(calls[format]==n+5L)
  paths <- vapply(environment(cache$save)$artifacts, `[[`, character(1), 'path')
  writeLines('damaged artifact',paths)
  cache$save(entries,second,format,NULL,'en')
  stopifnot(calls[format]==n+6L,grepl('CHANGED_VALUE',text(second,format)))
  paths <- vapply(environment(cache$save)$artifacts, `[[`, character(1), 'path')
  cache$clear(); stopifnot(!any(file.exists(paths)))
  # Failed writes do not become hits on the next attempt.
  failed <- try(cache$save(entries,file.path(out,'missing-folder',paste0('fail',extension)),format,'main','ko'),silent=TRUE)
  stopifnot(inherits(failed,'try-error'),!length(environment(cache$save)$artifacts))
  cache$save(entries,second,format,'main','ko'); cache$clear()
  small <- result_document_export_cache(max_bytes=1)
  small$save(entries,second,format,'main','ko'); n <- calls[format]
  small$save(entries,second,format,'main','ko')
  stopifnot(calls[format]==n+1L,!length(environment(small$save)$artifacts))
  cat('PASS',format,'repeat fidelity; changed content/order/selection/language; session isolation; corruption, failure, size bound and cleanup\n')
}
