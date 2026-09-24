Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(937);f<-rnorm(80);d<-data.frame(a=f+rnorm(80,sd=.7),b=f+rnorm(80,sd=.7),c=f+rnorm(80,sd=.7))
o<-as.data.frame(lapply(d,function(x)as.integer(cut(x,c(-Inf,-.5,.5,Inf)))))
info<-data.frame(name=names(d),measurement='ordered',var_label=c('Normality','Yes','사용자 평가자'))
fits<-list()
for(w in c('linear','quadratic'))for(n in 2:3)fits[[paste(w,n)]]<-prepare_interrater_agreement_results(o,names(o)[1:n],info,options=list(weight=w))
info$measurement<-'continuous'
fits$bootstrap<-prepare_interrater_agreement_results(d,names(d),info,options=list(normality=TRUE,bootstrap_ci=TRUE,bootstrap_resamples=50L,seed=937))
stopifnot(grepl('Bootstrap percentile',fits$bootstrap$primary$Note,fixed=TRUE),nzchar(fits$bootstrap$primary[['95% CI']]))
out<-'tmp/interrater-ordinal-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);baseline<-list();exports<-list()
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language)
 for(name in names(fits)){
  html<-as.character(interrater_agreement_results_ui(fits[[name]]));doc<-xml2::read_html(html,encoding='UTF-8')
  main<-xml2::xml_find_all(doc,"//table[@data-result-table-role='main']")
  content<-xml2::xml_text(xml2::xml_find_all(main,".//th|.//td|preceding::h3[1]|ancestor::div[@data-result-table-sheet='true'][1]/*[not(self::table)]"))
  if(language=='en')baseline[[name]]<-content
  stopifnot(length(main)==1L,xml2::xml_attr(main,'data-result-table-language')=='en',identical(content,baseline[[name]]))
  appendix<-xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']")
  stopifnot(length(appendix)>0L,all(xml2::xml_attr(appendix,'data-result-table-language')==language))
  text<-xml2::xml_text(appendix)
  if(name!='bootstrap'){
   key<-if(grepl('linear',name))'weight_linear'else 'weight_quadratic'
   if(any(grepl('^Weight:',fits[[name]]$auxiliary$Note)))stopifnot(any(grepl(statedu_t(paste0('analysis.ui.',key),language),text,fixed=TRUE)))
   note<-'analysis.ui.ordinal_alpha_using_squared_rank_distance'
   stopifnot(any(grepl(sub('[.]$','',statedu_t(note,language)),text,fixed=TRUE)))
  }else stopifnot(all(info$var_label %in% xml2::xml_text(xml2::xml_find_all(appendix,'.//td'))))
  if(language=='ja')exports[[name]]<-html
 }
 cat('PASS:',language,'two/three-rater linear/quadratic weighting and actual ICC bootstrap; English main content; appendix notes\n')
}
saveRDS(list(list(id='interrater-ordinal',title='Inter-rater agreement',html=paste(unlist(exports),collapse='\n'))),file.path(out,'entries.rds'))

