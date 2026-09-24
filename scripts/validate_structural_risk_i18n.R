Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(712);n<-400;f<-rnorm(n);g<-.97*f+sqrt(1-.97^2)*rnorm(n)
d<-data.frame(x1=f+rnorm(n,sd=.3),x2=f+rnorm(n,sd=.3),x3=f+rnorm(n,sd=.3),y1=g+rnorm(n,sd=.3),y2=g+rnorm(n,sd=.3),y3=g+rnorm(n,sd=.3))
fit<-lavaan::cfa('Normality =~ x1+x2+x3\n사용자요인 =~ y1+y2+y3',data=d)
stopifnot(lavaan::lavInspect(fit,'converged'),any(structural_canvas_factor_correlation_diagnostics(fit)$Severity=='Severe'))
node<-function(id,role)list(id=id,role=role)
snapshot<-list(nodes=c(lapply(names(d),node,role='indicator'),lapply(c('e1','e2','e3'),node,role='error')),edges=list(list(from='e1',to='e2',kind='covariance')))
bundle<-list(fit=fit,snapshot=snapshot,ordered=c('Normality','범주변수'))
# Composite diagnostic fixture: fitted continuous CFA plus actual sparse ordinal data.
# The extra columns exercise category diagnostics; this is not a WLSMV fit.
d$Normality<-factor(c(rep('High',398),'Sparse','Sparse'),levels=c('High','Sparse','Empty'))
d$범주변수<-factor(rep(c('사용자값','Normality'),each=200))
stopifnot(structural_canvas_error_covariance_diagnostics(snapshot)$status=='Limited')
out<-'tmp/structural-risk-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
before<-new.env(parent=globalenv());if(file.exists('tmp/structural-risk-before.R'))sys.source('tmp/structural-risk-before.R',envir=before)
baseline<-NULL;numeric_baseline<-NULL
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language)
 main_data<-structural_canvas_result_table('measurement',function()bundle,'cfa',function()c(x1='사용자 라벨'),function()'en')
 main<-as.character(structural_canvas_basic_html_table(main_data,role='main',language=language,title='Measurement model'))
 if(is.null(baseline))baseline<-main else stopifnot(identical(baseline,main))
 stopifnot(grepl('사용자 라벨',main,fixed=TRUE),grepl('Normality',main,fixed=TRUE))
 aux<-as.character(structural_canvas_risk_diagnostics_result_ui(bundle,d,'cfa',language))
 # Two supplementary headings are clarified: Correlation coefficient / Empty cell percentage.
 if(language=='en' && exists('structural_canvas_risk_diagnostics_result_ui',envir=before,inherits=FALSE)){
  old<-as.character(before$structural_canvas_risk_diagnostics_result_ui(bundle,d,'cfa',language))
  old<-gsub('>Correlation<','>Correlation coefficient<',old,fixed=TRUE)
  old<-gsub('>Empty %<','>Empty cell percentage<',old,fixed=TRUE)
  stopifnot(identical(aux,old))
 }
 doc<-xml2::read_html(aux,encoding='UTF-8');visible<-xml2::xml_text(doc)
 stopifnot(length(xml2::xml_find_all(doc,"//*[@data-result-table-role='appendix']//table"))==3L,!grepl('{',visible,fixed=TRUE))
 for(source in c('Data and model risk diagnostics','High latent correlations','Sparse ordered categories','Sparse ordered-indicator cross-tabulations','Correlated measurement errors','Limited')){
  if(!language %in% c('en','ko')){
   translated<-statedu_localized_text(language,source)
   stopifnot(translated!=source,grepl(translated,visible,fixed=TRUE))
  }
 }
 # Source category/indicator values must survive even if equal to diagnostic words.
 tables<-xml2::xml_find_all(doc,'//table')
 categories<-xml2::xml_text(xml2::xml_find_all(tables[[2]],'.//tbody/tr/td[2]'))
 stopifnot(identical(categories,c('High','Sparse','Empty')))
 if(!language %in% c('en','ko')){
  statuses<-xml2::xml_text(xml2::xml_find_all(tables[[2]],'.//tbody/tr/td[5]'))
  stopifnot(identical(statuses,unname(vapply(c('Dominant (>=95%)','Sparse','Empty'),function(s)statedu_localized_text(language,s),character(1)))))
  headers<-xml2::xml_text(xml2::xml_find_all(doc,'//th'))
  stopifnot(statedu_localized_text(language,'Empty cell percentage') %in% headers)
 }
 stopifnot(grepl('사용자요인',visible,fixed=TRUE),grepl('Normality',visible,fixed=TRUE),grepl('범주변수',visible,fixed=TRUE))
 numbers<-unlist(lapply(tables,function(t){v<-trimws(xml2::xml_text(xml2::xml_find_all(t,'.//td')));v[grepl('^[0-9.%-]+$',v)]}))
 if(is.null(numeric_baseline))numeric_baseline<-numbers else stopifnot(identical(numeric_baseline,numbers))
 complex<-bundle; complex$snapshot$edges<-rep(snapshot$edges,3)
 complex_html<-as.character(structural_canvas_risk_diagnostics_result_ui(complex,d,'cbsem',language))
 if(!language %in% c('en','ko'))stopifnot(grepl(statedu_localized_text(language,'Review complexity'),complex_html,fixed=TRUE))
 stopifnot(is.null(structural_canvas_risk_diagnostics_result_ui(bundle,d,'plssem',language)))
 html<-paste0(main,aux);writeLines(html,file.path(out,paste0(language,'.html')),useBytes=TRUE)
 if(language=='ja'){
  entries<-list(list(id='cfa-risk',title='CFA risk diagnostics',html=html))
  normality_fixture<-'tmp/structural-normality-i18n/entries.rds'
  if(file.exists(normality_fixture))entries<-c(entries,readRDS(normality_fixture))
  saveRDS(entries,file.path(out,'entries.rds'))
 }
 cat('PASS:',language,'actual CFA correlations, sparse-category fixtures, English main and user identifiers\n')
}
for(value in c('日本語。','中文。','確認！','確認？','English.','한국어.'))stopifnot(identical(result_publication_note(value),value))
cat('PASS: CJK note punctuation\n')
single_fit<-lavaan::cfa('factor =~ x1+x2+x3',data=d)
unflagged<-list(fit=single_fit,snapshot=list(nodes=list(),edges=list()),ordered=character())
for(language in c('en','ko','ja','zh','es','fr','de','vi'))stopifnot(is.null(structural_canvas_risk_diagnostics_result_ui(unflagged,d,'cfa',language)))
cat('PASS: no-risk single-factor CFA hides the diagnostic section\n')
