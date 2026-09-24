Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(947);n<-80
d<-data.frame(psu=rep(1:20,each=4),stratum=rep(1:4,each=20),wt=runif(n,.5,2),x=rnorm(n),g=rep(c('사용자 범주','Model-based'),40),empty=NA_real_)
d$x[c(2,11)]<-NA
info<-data.frame(name=c('x','g','empty'),measurement=c('continuous','category','continuous'),var_label=c('사용자 점수','사용자 집단','사용자 제외 50%'))
input<-list(p_strata='stratum',p_cluster='psu',p_weight='wt',p_fpc='',p_variance_method='auto',p_lonely_psu='adjust',p_use_replicate_weights=FALSE,p_subpopulation='',p_subpopulation_condition='',p_subpopulation_condition_type='equals',p_subpopulation_condition_value='')
out<-'tmp/complex-frequency-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);baseline<-NULL
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language)
 html<-as.character(complex_sample_frequency_result(d,c('x','g','empty'),input,'p',variable_info=info,language=language))
 doc<-xml2::read_html(html,encoding='UTF-8')
 main<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']//th|//table[@data-result-table-role='main']//td"))
 if(is.null(baseline))baseline<-main else stopifnot(identical(baseline,main))
 appendix<-xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']")
 stopifnot(length(main)>0,length(appendix)>0,all(xml2::xml_attr(appendix,'data-result-table-language')==language))
 appendix_text<-paste(xml2::xml_text(appendix),collapse='\n')
 stopifnot(grepl('사용자 제외 50%',appendix_text,fixed=TRUE))
 if(language %in% c('ja','zh','es','fr','de','vi')){
  for(source in c('Survey design','Skipped analyses','Variance estimation used Taylor linearization.')){
   expected<-statedu_localized_text(language,source)
   stopifnot(!identical(expected,source),grepl(expected,appendix_text,fixed=TRUE))
  }
  expected<-sprintf(statedu_localized_text(language,'Survey design was constructed from %s rows; final design N after exclusions/subpopulation was %s.'),80,80)
  stopifnot(grepl(expected,appendix_text,fixed=TRUE),!grepl('Variables with no usable',appendix_text,fixed=TRUE))
 }
 writeLines(html,file.path(out,paste0(language,'.html')),useBytes=TRUE)
 if(language=='ja')saveRDS(list(list(id='complex-frequency',title='Complex-sample frequencies',html=html)),file.path(out,'entries.rds'))
 cat('PASS:',language,'weighted/stratified/clustered frequency and descriptive main cells; appendix roles\n')
 if(language=='ja')cat('APPENDIX:',xml2::xml_text(appendix),'\n')
}
