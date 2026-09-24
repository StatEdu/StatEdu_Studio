Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(936)
d<-data.frame(x=rnorm(90),z=rnorm(90),g=factor(rep(c('None','Yes','No'),30)))
d$y<-10*d$x-8*d$z+3*(d$g=='Yes')-3*(d$g=='No')+rnorm(90)
info<-data.frame(name=names(d),measurement=c('continuous','continuous','category','continuous'),
 var_label=c('Yes','No','Status','Tested'))
fits<-lapply(penalized_menu_methods(),function(m)prepare_penalized_menu(d,'y',c('x','z','g'),m,info,
 resamples=2,post_selection=TRUE,inference_splits=20,validation_repeats=2,seed=12))
mapping<-c(summary='penalized-summary-table',coefficient_comparison='penalized-coefficient-comparison-table',
 selected_predictors='penalized-selected-table',cv_settings='penalized-settings-table',selection_stability='penalized-selection-stability-table',
 inference_diagnostics='penalized-inference-diagnostics',factor_diagnostics='penalized-factor-diagnostics',validation_variability='penalized-validation-variability')
before<-serialize(fits,NULL);captured<-list();counts<-list()
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 options(statedu.app_language=lang);htmls<-character();checked<-0L
 for(i in seq_along(fits)) {
  fit<-fits[[i]];html<-as.character(htmltools::renderTags(penalized_result_block(fit))$html)
  doc<-xml2::read_html(html,encoding='UTF-8');htmls<-c(htmls,html)
  for(field in names(mapping)) {
   raw<-fit[[field]]
   if(!is.data.frame(raw)||!nrow(raw))next
   tab<-xml2::xml_find_all(doc,paste0("//table[contains(@class,'",mapping[[field]],"')]") )
   stopifnot(length(tab)==1L)
   for(column in which(names(raw)%in%c('Outcome','Predictor','Variable','Factor','Selected predictors'))) {
    expected<-as.character(raw[[column]])
    if(names(raw)[column]=='Predictor') {
     positions<-attr(raw,'penalized_intercept_rows');if(length(positions))expected[positions]<-statedu_t('analysis.penalized.intercept',lang)
    }
    if(names(raw)[column]=='Selected predictors') {
     status<-attr(raw,'penalized_selection_status')
     expected[which(status=='none')]<-statedu_t('analysis.ui.none',lang)
     expected[which(status=='all')]<-statedu_t('analysis.ui.all_predictors_retained',lang)
    }
    actual<-xml2::xml_text(xml2::xml_find_all(tab,paste0('.//tbody/tr/td[',column,']')))
    stopifnot(identical(actual,expected));checked<-checked+length(expected)
   }
  }
 }
 captured[[lang]]<-paste(htmls,collapse='\n')
 doc<-xml2::read_html(captured[[lang]],encoding='UTF-8')
 main<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']"))
 if(lang=='en')baseline<-main else stopifnot(identical(main,baseline))
 stopifnot(identical(before,serialize(fits,NULL)))
 counts[[lang]]<-checked
 cat('PASS',lang,checked,'identity/status cells across actual three-method advanced results\n')
}
out<-'tmp/penalized-user-labels-review';dir.create(out,recursive=TRUE,showWarnings=FALSE)
jsonlite::write_json(captured,file.path(out,'captured.json'),auto_unbox=TRUE)
jsonlite::write_json(counts,file.path(out,'checked-cells.json'),auto_unbox=TRUE)
