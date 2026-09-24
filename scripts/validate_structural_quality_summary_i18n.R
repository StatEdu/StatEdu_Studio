Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(924);n<-160;f<-rnorm(n);g<-.5*f+rnorm(n)
d<-data.frame(x1=f+rnorm(n),x2=f+rnorm(n),x3=f+rnorm(n),y1=g+rnorm(n),y2=g+rnorm(n),y3=g+rnorm(n))
fit<-lavaan::cfa('Normality =~ x1+x2+x3\n사용자요인 =~ y1+y2+y3',data=d)
cfa<-list(fit=fit,estimator='ML',missing='listwise',snapshot=list(nodes=list(),edges=list()),diagnostics=structural_canvas_fit_admissibility(fit))
pls_fit<-seminr::estimate_pls(d,seminr::constructs(seminr::composite('X',seminr::multi_items('x',1:3)),seminr::composite('Y',seminr::multi_items('y',1:3))),seminr::relationships(seminr::paths(from='X',to='Y')),assess_syntax=FALSE)
pls<-list(fit=pls_fit,estimator='PLS',missing='mean_replacement',snapshot=list(nodes=list(),edges=list()),diagnostics=list(n=n))
out<-'tmp/structural-quality-summary-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list();main_baseline<-NULL;overview_baseline<-NULL
read<-function(html)xml2::read_html(as.character(html),encoding='UTF-8')
states<-c('OK','Review','Reference only','Descriptive only','Screen only','Not assessed')
test_rows<-data.frame(Item=seq_along(states),Status=states)
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language='en') # Explicit requested language must override global state, especially for PLS tables.
 for(bundle_name in c('cfa','plssem')){
  bundle<-if(bundle_name=='cfa')cfa else pls
  html<-as.character(if(bundle_name=='cfa')structural_canvas_lavaan_quality_result_ui(bundle,'cfa',language) else structural_canvas_pls_quality_result_ui(bundle,language))
  doc<-read(html);stopifnot(length(xml2::xml_find_all(doc,'//table'))>=1L)
  sheets<-xml2::xml_find_all(doc,"//*[@data-result-table-role='appendix']")
  stopifnot(length(sheets)>0,all(xml2::xml_attr(sheets,'data-result-table-language')==language))
  summary<-xml2::xml_text(xml2::xml_find_all(doc,"//*[contains(@class,'structural-quality-status-summary')]"))
  readiness<-xml2::xml_text(xml2::xml_find_all(doc,"//*[contains(@class,'structural-quality-reporting-readiness')]"))
  stopifnot(length(summary)==1L,length(readiness)==1L,!grepl('{',readiness,fixed=TRUE))
  if(!language %in% c('en','ko'))stopifnot(!grepl('Quality status:',summary,fixed=TRUE),!grepl('Reporting readiness:',readiness,fixed=TRUE),grepl(statedu_localized_text(language,if(bundle_name=='cfa')'SEM quality checklist' else 'PLS-SEM quality checklist'),html,fixed=TRUE))
  writeLines(html,file.path(out,paste0(language,'-',bundle_name,'.html')),useBytes=TRUE)
  if(language=='ja')entries[[bundle_name]]<-list(id=bundle_name,title=bundle_name,html=html)
 }
 main<-as.character(structural_canvas_basic_html_table(structural_canvas_result_table('measurement',function()cfa,'cfa',function()c(x1='사용자 라벨'),function()'en'),role='main',language=language,title='Measurement model'))
 if(is.null(main_baseline))main_baseline<-main else stopifnot(identical(main_baseline,main))
 stopifnot(grepl('Normality',main,fixed=TRUE),grepl('사용자 라벨',main,fixed=TRUE))
 overview_data<-structural_canvas_result_table('overview',function()cfa,'cfa',function()character(),function()'en')
 stopifnot(nrow(overview_data)>0)
 overview<-as.character(structural_canvas_basic_html_table(overview_data,role='main',language=language,title='Model overview'))
 if(is.null(overview_baseline))overview_baseline<-overview else stopifnot(identical(overview_baseline,overview))
 # Synthetic failure flags exercise review rendering without changing the fitted data.
 failed<-cfa;failed$converged<-FALSE;failed$admissible<-FALSE
 failed_html<-as.character(structural_canvas_lavaan_quality_result_ui(failed,'cbsem',language))
 if(!language %in% c('en','ko'))for(source in c('Critical','Resolve before reporting'))stopifnot(grepl(statedu_localized_text(language,source),failed_html,fixed=TRUE),statedu_localized_text(language,source)!=source)
 if(language=='ja')entries$critical<-list(id='quality-critical-fixture',title='Quality review fixture',html=failed_html)
 summary<-structural_canvas_quality_status_summary_display(test_rows,language=='ko',language)
 stopifnot(length(gregexpr('=1',summary,fixed=TRUE)[[1]])==if(language=='en')5L else 6L)
 for(priority in c('Critical','Major','Advisory','none')){
  reviews<-if(priority=='none')data.frame(Priority=character()) else data.frame(Priority=rep(priority,2))
  items<-switch(priority,Critical=c('Converged','Admissible solution'),Major=c('Min AVE','Min CR'),Advisory=c('Model status','Other'),character())
  readiness_rows<-data.frame(Item=items,Value=rep('',length(items)),Status=rep('Review',length(items)),Guidance=rep('',length(items)))
  value<-structural_canvas_quality_reporting_readiness_display(reviews,readiness_rows,language=='ko',language)
  if(priority %in% c('Critical','Major'))stopifnot(grepl('2',value,fixed=TRUE))
  if(!language %in% c('en','ko'))stopifnot(!grepl('Reporting readiness:',value,fixed=TRUE))
 }
 empty<-structural_canvas_quality_status_summary_display(data.frame(),language=='ko',language)
 if(!language %in% c('en','ko'))stopifnot(empty!='Quality status: not assessed.')
 cat('PASS:',language,'actual CFA/PLS quality shell, explicit language, counts/readiness and English main\n')
}
entries$cfa$html<-paste0(overview_baseline,main_baseline,entries$cfa$html)
saveRDS(unname(entries),file.path(out,'entries.rds'))
