Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(951);n<-120
d<-data.frame(psu=rep(1:30,each=4),stratum=rep(1:3,each=40),wt=runif(n,.5,2),x=rnorm(n),g=factor(rep(c('사용자 범주','Model-based'),60)),empty=NA_real_,one='Yes')
d$y<-d$x+rnorm(n);d$b<-ifelse(runif(n)<plogis(d$x),'Yes','No');d$x[c(2,11)]<-NA
info<-data.frame(name=c('x','g','y','b','empty','one'),measurement=c('continuous','category','continuous','binary','continuous','binary'),var_label=c('사용자 X 50%','사용자 범주형','Model-based','사용자 이분형','사용자 결측','사용자 단일'))
input<-list(p_strata='stratum',p_cluster='psu',p_weight='wt',p_fpc='',p_variance_method='auto',p_lonely_psu='adjust',p_use_replicate_weights=FALSE,p_subpopulation='',p_subpopulation_condition='',p_subpopulation_condition_type='equals',p_subpopulation_condition_value='')
out<-'tmp/complex-regression-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);baseline<-NULL
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language)
 html<-paste(as.character(complex_sample_regression_results(d,c('y','empty'),c('x','g'),input,'p',variable_info=info,language=language)),as.character(complex_sample_regression_results(d,c('b','one'),c('x','g'),input,'p',logistic=TRUE,variable_info=info,language=language)),sep='\n')
 doc<-xml2::read_html(html,encoding='UTF-8');main<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']/ancestor::div[contains(concat(' ',normalize-space(@class),' '),' result-section ')][1]"))
 if(is.null(baseline))baseline<-main else stopifnot(identical(baseline,main))
 appendix<-xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']")
 stopifnot(length(main)==2,length(appendix)==7,identical(xml2::xml_attr(appendix,'data-result-table-language'),c(rep(language,3),rep('en',3),language)))
 translate<-function(source)statedu_t(paste0('analysis.ui.',gsub('^_+|_+$','',gsub('[^a-z0-9]+','_',tolower(source)))),language,source)
 event<-xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']//tr")
 event<-Filter(function(row){cells<-xml2::xml_text(xml2::xml_find_all(row,'./td'));length(cells)==2&&identical(trimws(cells[[1]]),'Event category')},as.list(event))
 stopifnot(length(event)==1,trimws(xml2::xml_text(xml2::xml_find_all(event[[1]],'./td')[[2]]))=='Yes')
 text<-paste(xml2::xml_text(appendix),collapse='\n')
 for(source in c('Unweighted N','Eligible design N','Complete-case excluded N','Adjusted R-squared','Model Wald/F statistic','Model Wald/F df','Model Wald/F p','Complex-sample linear regression','Complex-sample logistic regression','McFadden pseudo R-squared','Nagelkerke pseudo R-squared','Dependent variable has no usable non-missing values.','Logistic regression requires a binary dependent variable.')){
  english_logistic <- source %in% c('Complex-sample logistic regression','McFadden pseudo R-squared','Nagelkerke pseudo R-squared')
  expected<-if(english_logistic) source else translate(source);stopifnot(grepl(expected,text,fixed=TRUE));if(language!='en' && !english_logistic)stopifnot(expected!=source)
 }
 expected<-sprintf(translate('Complete-case modeling excluded %s row(s) with missing outcome or predictor values after survey design/subpopulation filtering.'),2)
 stopifnot(grepl(expected,text,fixed=TRUE))
 writeLines(html,file.path(out,paste0(language,'.html')),useBytes=TRUE)
 if(language=='ja')saveRDS(list(list(id='complex-regression',title='Complex-sample regressions',html=html)),file.path(out,'entries.rds'))
 cat('PASS:',language,'linear/logistic actual results and failure panels; main sections preserved\n')
 if(language=='ja')cat('APPENDIX:',xml2::xml_text(appendix),'\n')
}
