Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
find_render<-function(x){
 if(missing(x)||!is.call(x))return(NULL)
 if(identical(x[[1]],as.name('<-'))&&grepl('"_result_measurement_ci"',paste(deparse(x[[2]]),collapse=''),fixed=TRUE))return(x[[3]][[2]])
 for(item in as.list(x)[-1]){r<-find_render(item);if(!is.null(r))return(r)}
 NULL
}
expr<-find_render(body(structural_canvas_register_result_outputs));stopifnot(!is.null(expr))
out<-'tmp/measurement-ci-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
main<-data.frame(Latent='Review',Indicator='사용자 <&> %s',B='.700',SE='.050',beta='.720',z='14.000',p='<.001','R²'='.518',check.names=FALSE)
for(kind in c('r2','superscript','fallback','empty'))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 table<-data.frame(Latent=c('Review','Normality'),Indicator=c('Primary','사용자 <&> %s'),check.names=FALSE)
 for(prefix in c('B','beta',if(kind=='superscript')'R²'else'R2'))for(bound in c('lower','upper'))table[[paste(prefix,'95% CI',bound)]]<-if(bound=='lower')c('.123','')else c('.456','—')
 if(kind=='fallback')table<-table[,1:6]
 if(kind=='empty')table<-table[FALSE,]
 env<-new.env(parent=globalenv());env$analysis_type<-'cfa';env$appendix_result_table<-function(key)table;env$ui_language<-function()language;env$table_number<-function(key)8L
 render<-function()NULL;body(render)<-expr;environment(render)<-env
 result<-render();if(kind=='empty'){stopifnot(is.null(result));next}
 html<-as.character(result);doc<-xml2::read_html(html,encoding='UTF-8');heading<-xml2::xml_text(xml2::xml_find_first(doc,'//h5'));cells<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//td')));headers<-xml2::xml_text(xml2::xml_find_all(doc,'//th'))
 stopifnot(all(c(table$Latent,table$Indicator,'.123','.456','—')%in%cells),grepl('8',heading),grepl('95%',heading,fixed=TRUE))
 if(language=='en')en_heading<-heading else{
  stopifnot(heading!=en_heading)
  if(kind!='fallback')stopifnot(!any(c('Latent factor','Indicator','lower','upper')%in%headers),!any(grepl('Std. loading',headers,fixed=TRUE)))
 }
 if(kind!='fallback'){
  stopifnot(length(xml2::xml_find_all(doc,'//th[@colspan="2"]'))==3L,length(xml2::xml_find_all(doc,'//thead/tr'))==2L)
  for(j in seq_len(ncol(table)))stopifnot(identical(trimws(xml2::xml_text(xml2::xml_find_all(doc,paste0('//tbody/tr/td[',j,']')))),as.character(table[[j]])))
 }
 options(statedu.app_language=language);main_html<-as.character(structural_canvas_measurement_html_table(main))
 if(language=='en')english_main<-main_html else stopifnot(identical(main_html,english_main))
 if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=paste(main_html,html))
 cat('PASS:',kind,language,'title, bounds, CI grouping, user labels, exact values and English main table\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
