Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
options(statedu.output_decimal_digits=3L)
out<-'tmp/multigroup-boot-diagnostics-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
for(kind in c('adequate','caution','unreliable','custom','empty'))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language)
 diagnostics<-list(requested=100L,fit_valid_replicates=95L,fit_valid_percent=95,joint_valid_replicates=90L,joint_valid_percent=90,inference_usable=TRUE,seed=20260917L,rng_kind=c('Mersenne-Twister','Inversion','Rejection'),r_version='4.5.3',ci_method='percentile',quantile_type='6',status='Adequate',failure_counts=integer(0))
 if(kind!='adequate'){
  diagnostics$status<-if(kind=='caution')'Caution'else'Unreliable';diagnostics$inference_usable<-FALSE
  diagnostics$ci_method<-'bias_corrected';diagnostics$failure_counts<-c(product_preparation=1L,fit_error=2L,nonconverged=3L,inadmissible=4L,target_extraction=5L)
 }
 produced<-structural_canvas_apply_multigroup_moderation_bootstrap(list(type='structural_path_comparison'),list(diagnostics=diagnostics))
 value<-produced$moderated_mediation_bootstrap_diagnostics;stopifnot(nrow(value)==1L)
 if(kind=='custom'){
  value$Status<-'Review';value[['CI method']]<-'Normality';value[['Centering scope']]<-'사용자 <&> %s';value[['Failure counts']]<-'사용자 <&> %s=7; fit_error=2'
 }
 produced$moderated_mediation_bootstrap_diagnostics<-value
 if(kind=='empty')produced$moderated_mediation_bootstrap_diagnostics<-data.frame()
 ui<-structural_canvas_invariance_appendix_ui(list(invariance_result=produced),language)
 if(kind=='empty'){stopifnot(is.null(ui));next}
 html<-as.character(ui);doc<-xml2::read_html(html,encoding='UTF-8');stopifnot(length(xml2::xml_find_all(doc,'//table'))==1L)
 headers<-xml2::xml_text(xml2::xml_find_all(doc,'//th'))
 if(language!='en')stopifnot(!any(c('Fit-valid','Joint-valid','Inference usable','Centering scope','Failure counts')%in%headers))
 for(j in seq_along(value)){
  cells<-trimws(xml2::xml_text(xml2::xml_find_all(doc,paste0('//tbody/tr/td[',j,']'))));name<-names(value)[j];raw<-value[[j]]
  if(name%in%c('Requested','Fit-valid','Joint-valid','Seed'))stopifnot(identical(cells,as.character(raw)))
  if(is.numeric(raw)){
   key<-paste(kind,j,sep='_');if(language=='en')assign(key,cells)else stopifnot(identical(cells,get(key)))
  }
  if(is.logical(raw))stopifnot(cells==statedu_localized_text(language,if(raw)'Yes'else'No',if(raw)'예'else'아니요'))
  if(name%in%c('RNG','R version','Quantile type'))stopifnot(identical(cells,raw))
  if(name%in%c('Status','CI method','Centering scope')){
   if(language=='en'||kind=='custom')stopifnot(identical(cells,raw))else stopifnot(cells!=raw)
  }
  if(name=='Failure counts'){
   if(language=='en')stopifnot(identical(cells,raw))else if(kind=='custom')stopifnot(startsWith(cells,'사용자 <&> %s=7; '),!grepl('fit_error',cells,fixed=TRUE))else stopifnot(cells!=raw)
   if(kind%in%c('caution','unreliable'))stopifnot(identical(regmatches(cells,gregexpr('=[0-9]+',cells))[[1]],paste0('=',1:5)))
  }
 }
 if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=html)
 cat('PASS:',kind,language,'actual diagnostic producer, translated labels and exact runtime values\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
