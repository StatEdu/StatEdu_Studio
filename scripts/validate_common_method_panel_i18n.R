Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
find_render<-function(x){
 if(missing(x)||!is.call(x))return(NULL)
 if(identical(x[[1]],as.name('<-'))&&grepl('"_result_common_method"',paste(deparse(x[[2]]),collapse=''),fixed=TRUE))return(x[[3]][[2]])
 for(item in as.list(x)[-1]){r<-find_render(item);if(!is.null(r))return(r)}
 NULL
}
expr<-find_render(body(structural_canvas_register_result_outputs));stopifnot(!is.null(expr))
out<-'tmp/common-method-panel-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
for(kind in c('complete','empty','disabled','pls'))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 table<-data.frame(Method='Custom screen',Statistic='Custom statistic',Value='.123',Status='OK',check.names=FALSE)
 if(kind=='empty')table<-table[FALSE,]
 bundle<-list(common_method_enabled=kind!='disabled',common_method_result=list(
 fit=data.frame(Model='Research_model',CFI=.987),
 comparison=data.frame(Comparison='Single_factor_CFA vs Research_model','Delta CFI'=.123,check.names=FALSE),
 loading_change=data.frame(Latent='사용자 <&> %s',Indicator='custom_name_1','Absolute change'=.123,check.names=FALSE)))
 env<-new.env(parent=globalenv());env$analysis_type<-if(kind=='pls')'plssem'else'cfa';env$fit_result<-function()bundle;env$appendix_result_table<-function(key)table;env$ui_language<-function()language;env$app_language_fn<-function()language
 render<-function()NULL;body(render)<-expr;environment(render)<-env
 result<-render();if(kind%in%c('disabled','pls')){stopifnot(is.null(result));next}
 html<-as.character(result);doc<-xml2::read_html(html,encoding='UTF-8')
 headings<-xml2::xml_text(xml2::xml_find_all(doc,'//h5|//h6'));notes<-xml2::xml_text(xml2::xml_find_all(doc,'//p'))
 stopifnot(length(headings)==if(kind=='empty')1L else 5L,length(notes)>0)
 if(language=='en'){english_headings<-headings;english_notes<-notes}else{
 stopifnot(headings[1]!=english_headings[1],!identical(notes,english_notes))
 if(kind=='complete')stopifnot(all(headings[c(3,4,5)]!=english_headings[c(3,4,5)]))
 }
 if(kind=='complete')stopifnot(all(c('사용자 <&> %s','custom_name_1','.123')%in%trimws(xml2::xml_text(xml2::xml_find_all(doc,'//td')))))
 if(language=='ja'){
 if(kind=='empty')html<-paste(as.character(structural_canvas_basic_html_table(data.frame(Variable='custom_name',B='.123'),role='main',language='en')),html)
 entries[[kind]]<-list(id=kind,title=kind,html=html)
 }
 cat('PASS:',kind,language,'headings, notes, optional sections, values and user labels\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
