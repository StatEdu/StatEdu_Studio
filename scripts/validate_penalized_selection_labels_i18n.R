Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(934);d<-data.frame(x=rnorm(80),z=rnorm(80));d$y<-20*d$x+rnorm(80)
info<-data.frame(name=names(d),measurement='continuous',var_label=c('None','사용자 변수','결과'))
strong<-prepare_penalized_menu(d,'y',c('x','z'),'LASSO',info,resamples=2,seed=12,validation_repeats=1)
stopifnot('None'%in%strong$selected_predictors[['Selected predictors']])
ridge<-prepare_penalized_menu(d,'y',c('x','z'),'Ridge',info,resamples=2,seed=12,validation_repeats=1)
set.seed(935);d$y<-rnorm(80)
weak<-prepare_penalized_menu(d,'y',c('x','z'),'LASSO',info,resamples=2,seed=12,validation_repeats=1)
# This branch must really have no selected terms, not merely a variable named None.
stopifnot(any(weak$summary[['Selected predictors, n']]==0))
stopifnot(all(attr(strong$selected_predictors,'penalized_selection_status')=='user'),
          all(attr(ridge$selected_predictors,'penalized_selection_status')=='all'),
          any(attr(weak$selected_predictors,'penalized_selection_status')=='none'))
fits<-list(strong,ridge,weak);before<-serialize(fits,NULL);captured<-list()
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 options(statedu.app_language=lang)
 html<-as.character(htmltools::renderTags(tagList(lapply(fits,penalized_result_block)))$html)
 doc<-xml2::read_html(html,encoding='UTF-8')
 tabs<-xml2::xml_find_all(doc,"//table[contains(@class,'penalized-selected-table')]")
 vals<-lapply(tabs,function(t)xml2::xml_text(xml2::xml_find_all(t,'.//tbody/tr/td[3]')))
 stopifnot('None'%in%vals[[1]])
 stopifnot(statedu_t('analysis.ui.all_predictors_retained',lang)%in%vals[[2]])
 stopifnot(statedu_t('analysis.ui.none',lang)%in%vals[[3]])
 restored<-strong;attr(restored$selected_predictors,'penalized_selection_status')<-NULL
 restored<-unserialize(serialize(restored,NULL))
 legacy_doc<-xml2::read_html(as.character(htmltools::renderTags(penalized_result_block(restored))$html))
 stopifnot('None'%in%xml2::xml_text(xml2::xml_find_all(legacy_doc,"//table[contains(@class,'penalized-selected-table')]//td[3]")))
 main<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']"))
 if(lang=='en')baseline<-main else stopifnot(identical(main,baseline))
 stopifnot(identical(before,serialize(fits,NULL)))
 captured[[lang]]<-html
}
out<-'tmp/penalized-selection-labels-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
jsonlite::write_json(captured,file.path(out,'captured.json'),auto_unbox=TRUE)
cat('PASS actual selected user label None, no-selection LASSO, Ridge retained status x eight languages; main/source preserved\n')
