Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(932);d<-data.frame(x=rnorm(60),z=rnorm(60));d$y<-2*d$x-d$z+rnorm(60)
info<-data.frame(name=names(d),measurement='continuous',var_label=c('Gaussian','사용자 변수','Tested'))
fits<-lapply(penalized_menu_methods(),function(m)prepare_penalized_menu(d,'y',c('x','z'),m,info,resamples=2,seed=12,validation_repeats=1))
stopifnot(all(vapply(fits,function(f)length(attr(f$coefficient_comparison,'penalized_intercept_rows'))==1L,logical(1))))
before<-serialize(fits,NULL);captured<-list()
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 options(statedu.app_language=lang)
 html<-as.character(htmltools::renderTags(tagList(lapply(fits,penalized_result_block)))$html)
 doc<-xml2::read_html(html,encoding='UTF-8')
 tabs<-xml2::xml_find_all(doc,"//table[contains(@class,'penalized-coefficient-comparison-table')]")
 for(tab in tabs) {
  labels<-xml2::xml_text(xml2::xml_find_all(tab,'.//tbody/tr/td[2]'))
  stopifnot(all(c(statedu_t('analysis.penalized.intercept',lang),'Gaussian','사용자 변수')%in%labels))
 }
 main<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']"))
 if(lang=='en')baseline<-main else stopifnot(identical(main,baseline))
 # An unmarked restored table must not translate a user label by spelling.
 legacy<-fits[[1]];attr(legacy$coefficient_comparison,'penalized_intercept_rows')<-NULL
 restored<-unserialize(serialize(legacy,NULL))
 legacy_doc<-xml2::read_html(as.character(htmltools::renderTags(penalized_result_block(restored))$html))
 stopifnot('(Intercept)'%in%xml2::xml_text(xml2::xml_find_all(legacy_doc,"//table[contains(@class,'penalized-coefficient-comparison-table')]//td[2]")))
 # Synthetic label-collision probe: a separate user row has identical spelling.
 probe<-fits[[1]];original<-probe$coefficient_comparison
 extra<-original[2L,,drop=FALSE];extra$Predictor<-'(Intercept)'
 probe$coefficient_comparison<-rbind(original,extra)
 attr(probe$coefficient_comparison,'penalized_intercept_rows')<-attr(original,'penalized_intercept_rows')
 probe<-unserialize(serialize(probe,NULL))
 probe_doc<-xml2::read_html(as.character(htmltools::renderTags(penalized_result_block(probe))$html))
 probe_labels<-xml2::xml_text(xml2::xml_find_all(probe_doc,"//table[contains(@class,'penalized-coefficient-comparison-table')]//td[2]"))
 stopifnot(sum(probe_labels=='(Intercept)')==if(lang=='en')2L else 1L)
 if(lang!='en')stopifnot(sum(probe_labels==statedu_t('analysis.penalized.intercept',lang))==1L)
 stopifnot(identical(before,serialize(fits,NULL)))
 captured[[lang]]<-html
}
out<-'tmp/penalized-intercept-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
jsonlite::write_json(captured,file.path(out,'captured.json'),auto_unbox=TRUE)
cat('PASS three actual methods x eight languages; generated intercepts localized; main/user labels/legacy/source preserved\n')
