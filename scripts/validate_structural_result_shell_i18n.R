Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
find_assignment<-function(x,target){
 if(missing(x)||!is.call(x))return(NULL)
 if(identical(x[[1]],as.name('<-'))&&grepl(target,paste(deparse(x[[2]]),collapse=''),fixed=TRUE))return(x[[3]])
 for(item in as.list(x)[-1]){r<-find_assignment(item,target);if(!is.null(r))return(r)}
 NULL
}
body_root<-body(structural_canvas_register_result_outputs)
sections<-c('_results','_result_supplementary_container','_result_mi_section')
out<-'tmp/structural-result-shell-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
for(analysis_type in c('cfa','cbsem','sem','plssem'))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 env<-new.env(parent=globalenv());env$prefix<-'test';env$analysis_type<-analysis_type;env$fit_result<-function()list();env$ui_language<-function()language;env$app_language_fn<-function()language;env$supplementary_ready<-function()TRUE
 env$appendix_result_table<-function(key)data.frame(Item='Review',MI='.123');env$table_number<-function(kind)'1'
 env$table_heading<-eval(find_assignment(body_root,'table_heading'),env)
 snippets<-list()
 for(section in sections){
  expr<-find_assignment(body_root,paste0('"',section,'"'))[[2]]
  render<-function()NULL;body(render)<-expr;environment(render)<-env
  ui<-render();if(section=='_result_mi_section'&&analysis_type=='plssem'){stopifnot(is.null(ui));next}
  html<-as.character(ui);doc<-xml2::read_html(html,encoding='UTF-8');headers<-xml2::xml_text(xml2::xml_find_all(doc,'//h3|//h4'));notes<-xml2::xml_text(xml2::xml_find_all(doc,'//p'))
  id<-paste(analysis_type,section)
  if(language=='en')assign(id,list(headers=headers,notes=notes),envir=globalenv())else{
   english<-get(id,envir=globalenv());stopifnot(headers[1]!=english$headers[1])
   if(section=='_results')stopifnot(identical(headers[-1],english$headers[-1]))
   if(section=='_result_supplementary_container'&&analysis_type!='plssem')stopifnot(length(notes)==1,notes!=english$notes)
  }
  if(section=='_result_supplementary_container'&&analysis_type=='plssem')stopifnot(length(notes)==0)
  if(section=='_result_mi_section'){
   env$appendix_result_table<-function(key)data.frame();stopifnot(is.null(render()))
   env$appendix_result_table<-function(key)data.frame(Item='Review',MI='.123')
  }
  snippets[[section]]<-html
 }
 options(statedu.app_language=language)
 main<-as.character(structural_canvas_measurement_html_table(data.frame(Latent='Review',Indicator='사용자 <&> %s',B='.700',SE='.050',beta='.720',z='14.000',p='<.001',check.names=FALSE)))
 if(language=='en')en_main<-main else stopifnot(identical(main,en_main))
 if(language=='ja')entries[[analysis_type]]<-list(id=analysis_type,title=analysis_type,html=paste(paste(unlist(snippets),collapse=''),main))
 cat('PASS:',analysis_type,language,'localized shell, unchanged English main titles, optional sections and main table\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
