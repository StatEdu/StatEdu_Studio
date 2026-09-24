Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(936);n<-100;f<-rnorm(n)
continuous<-data.frame(a=f+rnorm(n,sd=.3),b=f+rnorm(n,sd=.3),c=f+rnorm(n,sd=.3))
nominal<-as.data.frame(lapply(continuous,function(x)as.character(cut(x,c(-Inf,-.5,.5,Inf),labels=c('Yes','No','사용자 범주')))))
info<-data.frame(name=names(continuous),measurement='continuous',var_label=c('Normality','Yes','사용자 평가자'))
ni<-info;ni$measurement<-'category'
fits<-list(icc=prepare_interrater_agreement_results(continuous,names(continuous),info,options=list(normality=TRUE)),nominal2=prepare_interrater_agreement_results(nominal,c('a','b'),ni),nominal3=prepare_interrater_agreement_results(nominal,names(nominal),ni))
out<-'tmp/interrater-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);baseline<-list();exports<-list()
phrases<-jsonlite::read_json('scripts/fixtures/interrater_i18n_phrases.json',simplifyVector=TRUE)
aux_notes<-unlist(lapply(fits,function(f)f$auxiliary$Note),use.names=FALSE)
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language);combined<-character()
 for(name in names(fits)){
  html<-as.character(interrater_agreement_results_ui(fits[[name]]));doc<-xml2::read_html(html,encoding='UTF-8')
  main<-xml2::xml_find_all(doc,"//table[@data-result-table-role='main']")
  content<-xml2::xml_text(xml2::xml_find_all(main,".//th|.//td|preceding::h3[1]|ancestor::div[@data-result-table-sheet='true'][1]/*[not(self::table)]"))
  stopifnot(length(main)==1L,xml2::xml_attr(main,'data-result-table-language')=='en')
  if(language=='en')baseline[[name]]<-content
  stopifnot(identical(content,baseline[[name]]))
  appendix<-xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']")
  stopifnot(length(appendix)>0L,all(xml2::xml_attr(appendix,'data-result-table-language')==language))
  if(name=='icc')stopifnot(all(info$var_label %in% xml2::xml_text(xml2::xml_find_all(appendix,'.//td'))))
  combined<-c(combined,xml2::xml_text(doc))
  if(language=='ja')exports[[name]]<-html
 }
 for(phrase in phrases){
  key<-paste0('analysis.ui.',gsub('^_|_$','',gsub('[^a-z0-9]+','_',tolower(phrase))))
  expected<-sub('[.]$','',statedu_t(key,language))
  if(phrase %in% phrases[4:9] && !phrase %in% aux_notes)next
  if(!any(grepl(expected,combined,fixed=TRUE)))stop(language,' missing ',phrase)
 }
 cat('PASS:',language,'actual ICC and two/three-rater nominal agreement; main English; displayed appendix phrases; rater labels\n')
}
saveRDS(list(list(id='interrater',title='Inter-rater agreement',html=paste(unlist(exports),collapse='\n'))),file.path(out,'entries.rds'))
