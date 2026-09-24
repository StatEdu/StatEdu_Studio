Sys.setlocale('LC_CTYPE','English_United States.utf8')
source('scripts/validate_correlation_methods_i18n.R',encoding='UTF-8')
# Compatibility fixture, not an actual historical file: both matrices are fitted above.
combined<-fits$observed;combined$latent<-fits$latent
combined<-unserialize(serialize(combined,NULL))
before<-serialize(combined,NULL);captured<-list()
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 options(statedu.app_language=lang)
 html<-as.character(correlation_results_ui(combined));doc<-xml2::read_html(html,encoding='UTF-8')
 headings<-xml2::xml_text(xml2::xml_find_all(doc,'//h3'))
 prefix<-statedu_t('analysis.correlation.latent_variable_prefix',lang)
 stopifnot(any(startsWith(headings,paste0(prefix,' '))))
 if(lang!='en')stopifnot(!any(grepl('^Latent-variable ',headings)&!grepl('Correlation / association coefficients',headings,fixed=TRUE)))
 main<-xml2::xml_find_all(doc,"//table[@data-result-table-role='main']")
 stopifnot(length(main)==2L)
 content<-xml2::xml_text(xml2::xml_find_all(main,'.//th|.//td|preceding::h3[1]'))
 if(lang=='en')baseline<-content else stopifnot(identical(content,baseline))
 stopifnot(identical(before,serialize(combined,NULL)))
 captured[[lang]]<-html
}
out<-'tmp/correlation-latent-prefix-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
jsonlite::write_json(captured,file.path(out,'captured.json'),auto_unbox=TRUE)
cat('PASS eight-language compatibility appendix prefixes; two English main matrices and source preserved\n')
