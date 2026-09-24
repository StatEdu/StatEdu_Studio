Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
find_render<-function(x){
 if(missing(x))return(NULL)
 if(!is.call(x))return(NULL)
 if(identical(x[[1]],as.name('<-'))&&grepl('"_result_measurement_ci"',paste(deparse(x[[2]]),collapse=''),fixed=TRUE))return(x[[3]][[2]])
 for(item in as.list(x)[-1]){r<-find_render(item);if(!is.null(r))return(r)}
 NULL
}
expr<-find_render(body(structural_canvas_register_result_outputs));stopifnot(!is.null(expr))
out<-'tmp/pls-measurement-bootstrap-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
for(state in c('Adequate','Insufficient','Pending','Failed','Canceled','Not recorded','External state','Off','empty'))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 table<-data.frame(Construct=c('Review','Normality'),'Construct type'=c('Common factor','Composite'),Indicator=c('Primary','사용자 <&> %s'),Mode=c('Reflective','Formative'),check.names=FALSE)
 for(prefix in c('Loading','Weight'))for(suffix in c('Boot SE','CI lower','CI upper','t','p','BH-adjusted p'))table[[paste(prefix,suffix)]]<-if(state=='Adequate')c('.123','—')else c('','')
 table<-structural_canvas_pls_measurement_bootstrap_table(table)
 if(state=='empty')table<-table[FALSE,]
 bundle<-list(pls_bootstrap=100L,pls_bootstrap_result=list(requested_nboot=if(state=='Off')0L else 100L,nboot=70L,minimum_valid_ratio=.8,bootstrap_status=state,inference_available=state=='Adequate',failure_message=if(state=='Failed')'Review 사용자 <&> %s'else ''))
 env<-new.env(parent=globalenv());env$analysis_type<-'plssem';env$appendix_result_table<-function(kind)table;env$fit_result<-function()bundle;env$ui_language<-function()language;env$table_number<-function(kind)5L
 render<-function()NULL;body(render)<-expr;environment(render)<-env
 result<-render();if(state%in%c('Off','empty')){stopifnot(is.null(result));next}
 html<-as.character(result);doc<-xml2::read_html(html,encoding='UTF-8');note<-xml2::xml_text(xml2::xml_find_first(doc,'//p'));heading<-xml2::xml_text(xml2::xml_find_first(doc,'//h5'))
 if(language=='en'){en_note<-note;en_heading<-heading}else stopifnot(note!=en_note,heading!=en_heading)
 stopifnot(grepl('5',heading,fixed=TRUE))
 if(state!='Adequate')stopifnot(grepl('70/100',note,fixed=TRUE),grepl('80%',note,fixed=TRUE))
 if(state=='Failed')stopifnot(grepl('Review 사용자 <&> %s',note,fixed=TRUE))
 if(state=='External state')stopifnot(grepl(state,note,fixed=TRUE))
 stopifnot(length(xml2::xml_find_all(doc,'//table'))==as.integer(state=='Adequate'))
 if(state=='Adequate'){
  cells<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//td')));stopifnot(all(c(table$Construct,table$Indicator,'.123','—')%in%cells))
  if(language!='en'){
   stopifnot(!any(c('Common factor','Reflective','Formative')%in%cells))
   headers<-xml2::xml_text(xml2::xml_find_all(doc,'//th'));stopifnot(!any(grepl('Loading|Weight|BH-adjusted p',headers)))
  }
 }
 stopifnot(length(xml2::xml_find_all(doc,"//p[contains(@class,'structural-result-warning')]"))==as.integer(state!='Adequate'))
 if(language=='ja')entries[[state]]<-list(id=gsub(' ','',state),title=state,html=if(state=='Adequate')html else paste(as.character(structural_canvas_basic_html_table(data.frame(Construct='Review',Loading='.500'),role='main')),html))
 cat('PASS:',state,language,'title, notes, conditional table, user names, status and precision\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
