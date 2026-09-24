Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
find_render<-function(x){
 if(missing(x)||!is.call(x))return(NULL)
 if(identical(x[[1]],as.name('<-'))&&grepl('"_result_common_method"',paste(deparse(x[[2]]),collapse=''),fixed=TRUE))return(x[[3]][[2]])
 for(item in as.list(x)[-1]){r<-find_render(item);if(!is.null(r))return(r)}
 NULL
}
expr<-find_render(body(structural_canvas_register_result_outputs));stopifnot(!is.null(expr))
options(statedu.output_decimal_digits=3L,statedu.p_value_format='apa')
out<-'tmp/common-method-comparison-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
raw<-c('Review','Normality','Primary','사용자 <&> %s')
for(kind in c('complete','fit_only','comparison_only','loading_only'))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 fit<-data.frame(Model=c('Research_model','Single_factor_CFA','Common_latent_factor'),chisq=c(40,70,35),df=c(24,27,23),p=c(.021,.0001,.05),CFI=c(.987,.912,.992),RMSEA=c(.045,.081,.034),SRMR=c(.023,.051,.019),check.names=FALSE)
 comparison<-structural_canvas_common_method_fit_comparison(fit)
 loading<-data.frame(Latent=raw,Indicator=rev(raw),'Baseline beta'=c(.723,.812,.614,.932),'Method-adjusted beta'=c(.701,.755,.601,.891),'Absolute change'=c(.022,.057,.013,.041),'Method factor beta'=c(.111,.222,.333,.444),check.names=FALSE)
 if(kind=='complete'){
  custom<-comparison[1,,drop=FALSE];custom$Comparison<-'Review';custom$Note<-'Normality';comparison<-rbind(comparison,custom)
  custom_fit<-fit[1,,drop=FALSE];custom_fit$Model<-'Primary';fit<-rbind(fit,custom_fit)
 }
 if(kind=='fit_only'){comparison<-comparison[FALSE,];loading<-loading[FALSE,]}
 if(kind=='comparison_only'){fit<-fit[FALSE,];loading<-loading[FALSE,]}
 if(kind=='loading_only'){fit<-fit[FALSE,];comparison<-comparison[FALSE,]}
 bundle<-list(common_method_enabled=TRUE,common_method_result=list(fit=fit,comparison=comparison,loading_change=loading))
 env<-new.env(parent=globalenv());env$analysis_type<-'cfa';env$fit_result<-function()bundle;env$appendix_result_table<-function(key)data.frame(Method='Custom screen',Value='.123');env$ui_language<-function()language;env$app_language_fn<-function()language
 render<-function()NULL;body(render)<-expr;environment(render)<-env
 html<-as.character(render());doc<-xml2::read_html(html,encoding='UTF-8')
 for(section in c('fit','comparison','loading')){
  original<-switch(section,fit=fit,comparison=comparison,loading=loading)
  node<-xml2::xml_find_all(doc,paste0('//table[contains(@class,"structural-common-method-',section,'-table")]'))
  if(!nrow(original)){stopifnot(length(node)==0);next}
  stopifnot(length(node)==1)
  headers<-xml2::xml_text(xml2::xml_find_all(node,'./thead//th'))
  if(language!='en'){
   if(section=='fit')stopifnot(!'Model'%in%headers)
   if(section=='comparison')stopifnot(all(c('Δχ²','Δdf','Δp','ΔCFI','ΔRMSEA','ΔSRMR')%in%headers),!any(c('Comparison','Note')%in%headers))
   if(section=='loading')stopifnot(!any(names(loading)%in%headers))
  }
  for(j in seq_along(original)){
   cells<-trimws(xml2::xml_text(xml2::xml_find_all(node,paste0('./tbody/tr/td[',j,']'))))
   if(is.numeric(original[[j]])){
    expected<-vapply(original[[j]],if(names(original)[j]%in%c('p','Delta p'))format_p else format_decimal3,character(1))
    stopifnot(identical(cells,expected))
   }else if(section=='loading')stopifnot(identical(cells,original[[j]]))else{
    known<-!original[[j]]%in%raw
    stopifnot(identical(cells[!known],original[[j]][!known]))
    if(language=='en')stopifnot(identical(cells,original[[j]]))else stopifnot(all(cells[known]!=original[[j]][known]))
   }
  }
 }
 options(statedu.app_language=language)
 main<-as.character(structural_canvas_measurement_html_table(data.frame(Latent='Review',Indicator='사용자 <&> %s',B='.700',SE='.050',beta='.720',z='14.000',p='<.001',check.names=FALSE)))
 if(language=='en')english_main<-main else stopifnot(identical(main,english_main))
 if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=paste(main,html))
 cat('PASS:',kind,language,'localized program cells, raw user labels, exact numbers and English main table\n')
}
if(file.exists(file.path(out,'entries.rds')))stopifnot(identical(readRDS(file.path(out,'entries.rds')),unname(entries)))
saveRDS(unname(entries),file.path(out,'entries.rds'))
