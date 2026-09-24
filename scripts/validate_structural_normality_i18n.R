Sys.setlocale('LC_CTYPE','English_United States.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(619);n<-180;f<-rnorm(n)
d<-data.frame(Normality=.8*f+rnorm(n,sd=.6),x2=.7*f+rnorm(n,sd=.7),x3=.9*f+rnorm(n,sd=.5),x4=.7*f+rnorm(n,sd=.6))
fit<-lavaan::cfa('factor =~ Normality + x2 + x3 + x4',data=d)
stopifnot(lavaan::lavInspect(fit,'converged'))
bundle<-list(fit=fit,ordered=character(),snapshot=list(nodes=list(),edges=list()),estimator='ML',missing='listwise')
diagnosis<-structural_canvas_mardia(d,names(d));stopifnot(diagnosis$available)
out<-'tmp/structural-normality-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
text<-function(html)xml2::xml_text(xml2::read_html(as.character(html),encoding='UTF-8'))
translate<-function(source,language){
 key<-paste0('analysis.ui.',gsub('^_+|_+$','',gsub('[^a-z0-9]+','_',tolower(source))))
 statedu_t(key,language,source)
}
before<-new.env(parent=globalenv())
if(file.exists('tmp/structural-normality-before.R'))sys.source('tmp/structural-normality-before.R',envir=before)
baseline<-NULL;numeric_baseline<-NULL
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language)
 main_data<-structural_canvas_result_table('measurement',function()bundle,'cfa',function()c(x2='사용자 라벨'),function()'en')
 main<-as.character(structural_canvas_basic_html_table(main_data,role='main',language=language,title='Measurement model'))
 if(is.null(baseline))baseline<-main else stopifnot(identical(baseline,main))
 stopifnot(grepl('Normality',main,fixed=TRUE),grepl('사용자 라벨',main,fixed=TRUE))
 aux<-structural_canvas_normality_result_ui(bundle,d,'cfa',language)
 if(language=='en' && exists('structural_canvas_normality_result_ui',envir=before,inherits=FALSE))stopifnot(identical(as.character(aux),as.character(before$structural_canvas_normality_result_ui(bundle,d,'cfa',language))))
 doc<-xml2::read_html(as.character(aux),encoding='UTF-8')
 stopifnot(length(xml2::xml_find_all(doc,"//*[@data-result-table-role='appendix']//table"))==1L)
 numbers<-xml2::xml_text(xml2::xml_find_all(doc,"//tbody/tr/td[position()>1]"))
 if(is.null(numeric_baseline))numeric_baseline<-numbers else stopifnot(identical(numeric_baseline,numbers))
 appendix<-structural_canvas_localize_appendix_table(data.frame(Test=c('Mardia skewness','Mardia kurtosis'),Estimate=1,Statistic=2),language)
 if(language!='en')stopifnot(all(appendix[[1]]!=c('Mardia skewness','Mardia kurtosis')))
 # French and German legitimately use "Test" as their header.
 for(source in c('Multivariate normality and estimator guidance',diagnosis$recommendation)){
  expected<-translate(source,language);stopifnot(grepl(expected,text(aux),fixed=TRUE))
  if(language!='en')stopifnot(expected!=source)
 }
 # Both recommendation branches, actual seeded subsampling and every computed failure reason.
 sampled<-structural_canvas_mardia(d,names(d),max_n=80L)
 stopifnot(sampled$sampled,sampled$n==80L)
 for(recommendation in c('MLR recommended','No Mardia test flag; normality not established')){
  test_bundle<-bundle;sampled$recommendation<-recommendation;test_bundle$normality_diagnostics<-sampled
  rendered<-text(structural_canvas_normality_result_ui(test_bundle,d,'cfa',language))
  stopifnot(grepl(translate(recommendation,language),rendered,fixed=TRUE),!grepl('{',rendered,fixed=TRUE))
  stopifnot(grepl(translate('For computational stability, Mardia statistics used a reproducible random subsample of complete cases.',language),rendered,fixed=TRUE))
 }
 for(reason in c('At least two continuous indicators are required.','All indicators must be numeric and continuous.','Too few complete cases for the number of indicators.','The indicator covariance matrix is singular.')){
  test_bundle<-bundle;test_bundle$normality_diagnostics<-list(available=FALSE,reason=reason)
  stopifnot(grepl(translate(reason,language),text(structural_canvas_normality_result_ui(test_bundle,d,'cfa',language)),fixed=TRUE))
  if(language!='en')stopifnot(translate(reason,language)!=reason)
 }
 stopifnot(is.null(structural_canvas_normality_result_ui(bundle,d,'plssem',language)))
 ordered_bundle<-bundle;ordered_bundle$ordered<-'x2'
 stopifnot(is.null(structural_canvas_normality_result_ui(ordered_bundle,d,'cfa',language)))
 html<-paste0(main,as.character(aux));writeLines(html,file.path(out,paste0(language,'.html')),useBytes=TRUE)
 if(language=='ja')saveRDS(list(list(id='cfa-normality',title='CFA',html=html)),file.path(out,'entries.rds'))
 cat('PASS:',language,'actual CFA, English main/user labels, normality guidance and failure branches\n')
}
