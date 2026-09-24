Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(948);n<-120
d<-data.frame(psu=rep(1:30,each=4),stratum=rep(1:3,each=40),wt=runif(n,.5,2),r=sample(c('사용자 범주','Model-based'),n,TRUE),c=sample(c('사용자 예','사용자 아니오'),n,TRUE),empty=NA_character_)
d$r[c(2,11)]<-NA
info<-data.frame(name=c('r','c','empty'),measurement='category',var_label=c('사용자 행 50%','사용자 열','사용자 제외'))
input<-list(p_strata='stratum',p_cluster='psu',p_weight='wt',p_fpc='',p_variance_method='auto',p_lonely_psu='adjust',p_use_replicate_weights=FALSE,p_subpopulation='',p_subpopulation_condition='',p_subpopulation_condition_type='equals',p_subpopulation_condition_value='')
out<-'tmp/complex-crosstab-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);baseline<-NULL
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language)
 rendered<-lapply(list(c('r','empty'),'empty'),function(rows)as.character(complex_sample_crosstab_results(d,rows,'c',input,'p',variable_info=info,language=language)))
 html<-paste(unlist(rendered),collapse='\n');doc<-xml2::read_html(html,encoding='UTF-8')
 main<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']//th|//table[@data-result-table-role='main']//td|//div[contains(@class,'crosstab-table-wrap')]//div[contains(@class,'coefficient-note')]"))
 if(is.null(baseline))baseline<-main else stopifnot(identical(baseline,main))
 appendix<-xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']")
 stopifnot(length(main)>0,length(appendix)==2,all(xml2::xml_attr(appendix,'data-result-table-language')==language))
 text<-paste(xml2::xml_text(appendix),collapse='\n')
 translate<-function(source)statedu_t(paste0('analysis.ui.',gsub('^_+|_+$','',gsub('[^a-z0-9]+','_',tolower(source)))),language,source)
 if(!grepl('사용자 제외',text,fixed=TRUE))cat('DIAGNOSTIC:',text,'\n')
 stopifnot(grepl('사용자 행 50%',text,fixed=TRUE),grepl('사용자 제외',text,fixed=TRUE))
 for(source in c('No complete cases are available for the selected cross-tabulation variables.')){
  expected<-translate(source)
  stopifnot(grepl(expected,text,fixed=TRUE))
  if(language!='en')stopifnot(!identical(expected,source))
 }
 for(spec in list(list('No cross-tabulation table was computed for %s.','사용자 열'),list('%s by %s excluded %s row(s) with missing row or column values after survey design/subpopulation filtering.','사용자 행 50%','사용자 열',2))){
  expected<-do.call(sprintf,c(list(translate(spec[[1]])),spec[-1]));stopifnot(grepl(expected,text,fixed=TRUE))
 }
 writeLines(html,file.path(out,paste0(language,'.html')),useBytes=TRUE)
 if(language=='ja')saveRDS(list(list(id='complex-crosstab',title='Complex-sample cross-tabulation',html=html)),file.path(out,'entries.rds'))
 cat('PASS:',language,'actual weighted crosstab, partial/all skipped results; main and notes preserved; localized missing counts/errors\n')
}
