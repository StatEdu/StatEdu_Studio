Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
find_render<-function(x){
 if(missing(x)||!is.call(x))return(NULL)
 if(identical(x[[1]],as.name('<-'))&&grepl('"_result_effect_inference_details"',paste(deparse(x[[2]]),collapse=''),fixed=TRUE))return(x[[3]][[2]])
 for(item in as.list(x)[-1]){r<-find_render(item);if(!is.null(r))return(r)}
 NULL
}
expr<-find_render(body(structural_canvas_register_result_outputs));stopifnot(!is.null(expr))
out<-'tmp/inference-details-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
families<-c('Direct structural paths','Specific indirect effects','Other indirect and total effects','Fixed parameter (not tested)','Fixed effect (not tested)','Review')
table<-data.frame(Path=rep('Review → 사용자 <&> %s',6),Predictor=rep('Normality',6),Outcome=rep('Primary',6),Effect=rep(c('Direct','Specific indirect','Indirect','Total'),length.out=6),'B CI source'=rep('Model-based 95% CI',6),'beta CI source'=rep('Bootstrap percentile 95% CI (R quantile type 7); valid standardized bootstrap 90/100 (90.0%); status Adequate',6),'Inference source'=rep('Bootstrap (empirical two-sided p)',6),'Valid bootstrap'=rep('90/100 (90.0%)',6),'Bootstrap status'=rep('Adequate',6),'BH family'=families,check.names=FALSE)
original<-table
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 env<-new.env(parent=globalenv());env$analysis_type<-'cbsem';env$ui_language<-function()language;env$manuscript_result_table<-function(kind)table
 render<-function()NULL;body(render)<-expr;environment(render)<-env
 html<-as.character(render());doc<-xml2::read_html(html,encoding='UTF-8');headings<-xml2::xml_text(xml2::xml_find_all(doc,'//h5'));headers<-xml2::xml_text(xml2::xml_find_all(doc,'//th'));cells<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//td')))
 stopifnot(length(headings)==3L,length(xml2::xml_find_all(doc,'//table'))==3L,all(c('Review → 사용자 <&> %s','Normality','Primary','90/100 (90.0%)','Review')%in%cells),identical(table,original))
 if(language=='en')english<-headings else{
  stopifnot(all(headings!=english),!any(families[1:5]%in%cells),!any(c('B CI source','beta CI source','Inference source','Valid bootstrap','BH family')%in%headers))
 }
 if(language=='ja')entries[[1]]<-list(id='details',title='Inference details',html=html)
 env$manuscript_result_table<-function(kind)if(kind=='structural')table else data.frame()
 stopifnot(length(xml2::xml_find_all(xml2::read_html(as.character(render()),encoding='UTF-8'),'//table'))==1L)
 env$analysis_type<-'plssem';stopifnot(is.null(render()))
 cat('PASS:',language,'three headings, metadata, known families, protected paths and conditional sections\n')
}
saveRDS(entries,file.path(out,'entries.rds'))
