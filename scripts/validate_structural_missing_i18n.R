Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(781);n<-240;f<-rnorm(n)
d<-data.frame(Normality=f+rnorm(n,sd=.5),사용자변수=f+rnorm(n,sd=.5),x3=f+rnorm(n,sd=.5),x4=f+rnorm(n,sd=.5))
clean<-d;d[1,]<-c(10,-10,9,-9);d[2:4,1]<-NA;d[5:6,2]<-NA
syntax<-'factor =~ Normality + 사용자변수 + x3 + x4'
make_bundle<-function(data,method,ordered=character()){
 fit<-lavaan::cfa(syntax,data=data,missing=method,ordered=ordered)
 stopifnot(lavaan::lavInspect(fit,'converged'))
 list(fit=fit,missing=method,ordered=ordered,snapshot=list(nodes=list(),edges=list()),missing_diagnostics=structural_canvas_missing_diagnostics(data,names(data)),missing_sensitivity_method='external_analysis',missing_sensitivity_details='Normality')
}
ordinal<-as.data.frame(lapply(d,function(v)ordered(cut(v,breaks=c(-Inf,-.5,.5,Inf),labels=c('Low','Normality','High')))))
bundles<-list(listwise=make_bundle(d,'listwise'),fiml=make_bundle(d,'fiml'),pairwise=make_bundle(ordinal,'pairwise',names(ordinal)))
stopifnot(structural_canvas_mahalanobis_diagnostics(d,names(d))$flagged_n>0)
out<-'tmp/structural-missing-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
baselines<-list();entries<-list();number_baselines<-list()
extract<-function(html)xml2::read_html(as.character(html),encoding='UTF-8')
before<-new.env(parent=globalenv());if(file.exists('tmp/structural-missing-before.R'))sys.source('tmp/structural-missing-before.R',envir=before)
for(language in c('en','ko','ja','zh','es','fr','de','vi'))for(method in names(bundles)){
 options(statedu.app_language=language);bundle<-bundles[[method]];data<-if(method=='pairwise')ordinal else d
 main_data<-structural_canvas_result_table('measurement',function()bundle,'cfa',function()c(x3='사용자 라벨'),function()'en')
 main<-as.character(structural_canvas_basic_html_table(main_data,role='main',language=language,title='Measurement model'))
 if(language=='en')baselines[[method]]<-main else stopifnot(identical(baselines[[method]],main))
 aux<-as.character(structural_canvas_missing_outliers_result_ui(bundle,data,'cfa',language));doc<-extract(aux);visible<-xml2::xml_text(doc)
 if(language=='en' && exists('structural_canvas_missing_outliers_result_ui',envir=before,inherits=FALSE))stopifnot(identical(aux,as.character(before$structural_canvas_missing_outliers_result_ui(bundle,data,'cfa',language))))
 stopifnot(!grepl('{',visible,fixed=TRUE),!grepl('。.',visible,fixed=TRUE))
 tables<-xml2::xml_find_all(doc,'//table');stopifnot(length(tables)==if(method=='pairwise')3L else 4L)
 # Sensitivity details must not translate an exact match to a diagnostic phrase.
 sensitivity_details<-xml2::xml_text(xml2::xml_find_all(tables[[2]],'.//tbody/tr/td[3]'))
 stopifnot(identical(sensitivity_details,'Normality'))
 patterns<-xml2::xml_text(xml2::xml_find_all(tables[[3]],'.//tbody/tr/td[3]'))
 stopifnot(any(grepl('Normality',patterns,fixed=TRUE)),any(grepl('사용자변수',patterns,fixed=TRUE)))
 numbers<-unlist(lapply(tables,function(t){v<-trimws(xml2::xml_text(xml2::xml_find_all(t,'.//td')));v[grepl('^[0-9.%-]+$',v)]}))
 if(language=='en')number_baselines[[method]]<-numbers else stopifnot(identical(number_baselines[[method]],numbers))
 if(!language %in% c('en','ko'))for(source in c('Missing data and multivariate outliers','Variable-level missingness','Missing-assumption sensitivity record','Missingness patterns','Mahalanobis outlier candidates')){
  translated<-statedu_localized_text(language,source);stopifnot(translated!=source,grepl(translated,visible,fixed=TRUE))
 }
 if(method=='fiml'){
  review<-bundle;review$missing_sensitivity_method<-'not_assessed';review$missing_sensitivity_details<-''
  review_html<-as.character(structural_canvas_missing_outliers_result_ui(review,data,'cbsem',language))
  if(!language %in% c('en','ko'))stopifnot(!grepl('The MAR assumption underlying',review_html,fixed=TRUE),!grepl('FIML relies on MAR',review_html,fixed=TRUE))
  user<-bundle;user$missing_sensitivity_details<-'사용자 <메모> & Normality'
  stopifnot(grepl(user$missing_sensitivity_details,xml2::xml_text(extract(structural_canvas_missing_outliers_result_ui(user,data,'cfa',language))),fixed=TRUE))
 }
 html<-paste0(main,aux);writeLines(html,file.path(out,paste0(language,'-',method,'.html')),useBytes=TRUE)
 if(language=='ja')entries[[method]]<-list(id=paste0('cfa-',method),title=paste('CFA',method),html=html)
 cat('PASS:',language,method,'actual CFA, English main, preserved user text and pattern identifiers\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
# Real diagnostic failure data, rendered against a valid fit (no refitting failed data).
bundle<-bundles$listwise
failure_data<-list(d[1:3,],transform(clean,x4=x3),transform(clean,x4=as.character(x4)))
for(data in failure_data){
 diagnostic<-structural_canvas_mahalanobis_diagnostics(data,names(d));stopifnot(!diagnostic$available)
 for(language in c('ja','zh','es','fr','de','vi')){
  html<-as.character(structural_canvas_missing_outliers_result_ui(bundle,data,'cfa',language))
  expected<-statedu_localized_text(language,diagnostic$reason)
  stopifnot(expected!=diagnostic$reason,grepl(expected,html,fixed=TRUE))
 }
}
clean_bundle<-make_bundle(clean,'listwise')
stopifnot(structural_canvas_mahalanobis_diagnostics(clean,names(clean))$flagged_n==0L)
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 html<-as.character(structural_canvas_missing_outliers_result_ui(clean_bundle,clean,'cfa',language))
 stopifnot(!grepl('>Missingness patterns<',html,fixed=TRUE))
 if(!language %in% c('en','ko'))stopifnot(!grepl('No complete cases were flagged',html,fixed=TRUE))
 unspecified<-clean_bundle;unspecified$missing<-NULL
 unspecified_html<-as.character(structural_canvas_missing_outliers_result_ui(unspecified,clean,'cfa',language))
 if(!language %in% c('en','ko'))stopifnot(grepl(statedu_localized_text(language,'Not specified'),unspecified_html,fixed=TRUE),statedu_localized_text(language,'Not specified')!='Not specified')
}
cat('PASS: actual diagnostic failure data and no-missing branch\n')
pls<-list(diagnostics=list(observed=names(d)))
pls_diagnosis<-structural_canvas_pls_missing_diagnostics(d,names(d))
stopifnot(pls_diagnosis$effective_n==n,pls_diagnosis$imputed_cell_n==5L,pls_diagnosis$excluded_n==0L)
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language)
 html<-as.character(structural_canvas_missing_outliers_result_ui(pls,d,'plssem',language));doc<-extract(html);visible<-xml2::xml_text(doc)
 if(language=='en' && exists('structural_canvas_missing_outliers_result_ui',envir=before,inherits=FALSE))stopifnot(identical(html,as.character(before$structural_canvas_missing_outliers_result_ui(pls,d,'plssem',language))))
 variables<-xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[1]'))
 stopifnot(identical(variables,names(d)),!grepl('{',visible,fixed=TRUE))
 values<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[4]')))
 stopifnot(identical(values,vapply(pls_diagnosis$replacement_values[['Replacement mean']],format_decimal3,character(1))))
 if(!language %in% c('en','ko'))for(source in c('PLS missing-data handling','Indicator missingness and replacement values','Replacement mean'))stopifnot(grepl(statedu_localized_text(language,source),visible,fixed=TRUE),statedu_localized_text(language,source)!=source)
 blank<-list(diagnostics=list(observed=character()))
 warning<-as.character(structural_canvas_missing_outliers_result_ui(blank,d,'plssem',language))
 if(!language %in% c('en','ko'))stopifnot(!grepl('could not be computed',warning,fixed=TRUE))
 clean_html<-extract(structural_canvas_missing_outliers_result_ui(pls,clean,'plssem',language))
 stopifnot(!length(xml2::xml_find_all(clean_html,"//*[contains(@class,'structural-result-warning')]")))
 writeLines(html,file.path(out,paste0(language,'-pls.html')),useBytes=TRUE)
 if(language=='ja')entries$pls<-list(id='pls-missing',title='PLS missing data',html=html)
 cat('PASS:',language,'PLS diagnostic data, means, warning and no-missing branches (no PLS model fit)\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
