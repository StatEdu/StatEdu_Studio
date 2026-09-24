Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(950);n<-120
d<-data.frame(psu=rep(1:30,each=4),stratum=rep(1:3,each=40),wt=runif(n,.5,2),x=rnorm(n),y=rnorm(n),z=sample(1:5,n,TRUE),constant=1)
d$y<-d$x+.5*d$y;d$y[c(2,11)]<-NA
info<-data.frame(name=c('x','y','z','constant'),measurement=c('continuous','continuous','ordered','continuous'),var_label=c('사용자 X 50%','Model-based','사용자 순서형','사용자 상수'))
input<-list(p_strata='stratum',p_cluster='psu',p_weight='wt',p_fpc='',p_variance_method='auto',p_lonely_psu='adjust',p_use_replicate_weights=FALSE,p_subpopulation='',p_subpopulation_condition='',p_subpopulation_condition_type='equals',p_subpopulation_condition_value='',p_correlation_p_adjust='holm',p_correlation_matrix=TRUE)
out<-'tmp/complex-correlation-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);baseline<-NULL
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language)
 html<-paste(vapply(c('pearson','spearman'),function(method){input$p_correlation_method<-method;as.character(complex_sample_correlation_result(d,c('x','y','z','constant'),input,'p',variable_info=info,language=language))},character(1)),collapse='\n')
 doc<-xml2::read_html(html,encoding='UTF-8')
 main<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']/ancestor::div[contains(concat(' ',normalize-space(@class),' '),' result-section ')][1]"))
 if(is.null(baseline))baseline<-main else stopifnot(identical(baseline,main))
 appendix<-xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']")
 stopifnot(length(main)==4,length(appendix)==4,all(xml2::xml_attr(appendix,'data-result-table-language')==language))
 text<-paste(xml2::xml_text(appendix),collapse='\n')
 if(language!='en')for(source in c('Complex-sample correlation','Spearman rank correlation','Displayed variable pairs','Holm-Bonferroni-adjusted','Shown','Original N','Survey design N')){
  key<-paste0('analysis.ui.',gsub('^_+|_+$','',gsub('[^a-z0-9]+','_',tolower(source))))
  expected<-statedu_t(key,language,source)
  stopifnot(expected!=source,grepl(expected,text,fixed=TRUE))
 }
 writeLines(html,file.path(out,paste0(language,'.html')),useBytes=TRUE)
 if(language=='ja')saveRDS(list(list(id='complex-correlation',title='Complex-sample correlations',html=html)),file.path(out,'entries.rds'))
 cat('PASS:',language,'actual Pearson/Spearman, weighted clustered stratified; main sections preserved\n')
 if(language=='ja')cat('APPENDIX:',xml2::xml_text(appendix),'\n')
}
