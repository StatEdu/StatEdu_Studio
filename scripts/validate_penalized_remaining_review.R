Sys.setlocale('LC_CTYPE','English_United States.utf8')
source('scripts/validate_penalized_advanced_i18n.R', encoding='UTF-8')
# Review actual appendix headers and cells separately from English main tables.
out <- 'tmp/penalized-remaining-review'
dir.create(out, recursive=TRUE, showWarnings=FALSE)
captured <- list(); rows <- list(); before <- serialize(fits, NULL)
for (lang in c('en','ko','ja','zh','es','fr','de','vi')) {
  options(statedu.app_language=lang)
  html <- as.character(htmltools::renderTags(tagList(lapply(fits,penalized_result_block)))$html)
  doc <- xml2::read_html(html,encoding='UTF-8')
  nodes <- xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']//th | //table[@data-result-table-role='appendix']//td")
  values <- xml2::xml_text(nodes)
  settings <- xml2::xml_find_all(doc,"//table[contains(@class,'penalized-settings-table')]//td")
  stopifnot(statedu_t('analysis.penalized.family_gaussian',lang) %in% xml2::xml_text(settings))
  if(lang!='en')stopifnot(!'Gaussian' %in% xml2::xml_text(settings))
  if (lang=='en') english <- values else {
    stopifnot(length(values)==length(english))
    unchanged <- unique(values[values==english & grepl('[A-Za-z]{2}',values)])
    rows[[lang]] <- data.frame(language=lang,unchanged=unchanged)
  }
  captured[[lang]] <- html
  stopifnot(identical(before,serialize(fits,NULL)))
}
review <- do.call(rbind,rows)
write.csv(review,file.path(out,'unchanged-appendix-candidates.csv'),row.names=FALSE,fileEncoding='UTF-8')
jsonlite::write_json(captured,file.path(out,'captured.json'),auto_unbox=TRUE)
print(review,row.names=FALSE)
cat('PASS actual advanced appendix review captured; unchanged text requires classification, not automatic translation\n')
