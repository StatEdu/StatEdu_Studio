Sys.setlocale('LC_CTYPE','English_United States.utf8')
source('scripts/validate_correlation_i18n.R',encoding='UTF-8')
# Empty/restored payload branches; not a fabricated successful analysis.
empty_cases<-list(list(),list(correlation_matrix=matrix(numeric(),0,0)))
before<-serialize(list(fit,empty_cases),NULL);captured<-list()
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 options(statedu.app_language=lang)
 stopifnot(is.null(correlation_results_ui(NULL)))
 expected<-statedu_t('analysis.correlation.no_results',lang)
 if(lang!='en')stopifnot(expected!='No correlation results to show.')
 for(item in empty_cases) {
  message<-as.character(correlation_results_ui(item))
  text<-xml2::xml_text(xml2::read_html(message,encoding='UTF-8'))
  stopifnot(identical(text,expected))
 }
 # Include real results beside the empty payload to exercise collection export.
 captured[[lang]]<-as.character(tagList(correlation_results_ui(fit),correlation_results_ui(empty_cases[[1]])))
 stopifnot(identical(before,serialize(list(fit,empty_cases),NULL)))
}
out<-'tmp/correlation-empty-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
jsonlite::write_json(captured,file.path(out,'captured.json'),auto_unbox=TRUE)
cat('PASS eight languages: empty payload message; NULL stays hidden; actual result/source unchanged\n')
