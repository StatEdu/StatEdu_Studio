Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(935);f<-rnorm(120);d<-as.data.frame(setNames(lapply(1:6,function(i)f+rnorm(120,sd=.8)),paste0('i',1:6)))
info<-data.frame(name=names(d),measurement='continuous',var_label=c('Normality','Yes','사용자 문항','Status','Item','사용자 항목'))
opts<-list(omega=TRUE,normality=TRUE,reliability_if_deleted=TRUE,item_total_correlation=TRUE)
fit<-function(data,vars=names(data),measurement='continuous',omega=TRUE){v<-info;v$measurement<-measurement;o<-opts;o$omega<-omega;prepare_reliability_results(data,vars,v,options=o)}
ordinal<-as.data.frame(lapply(d,function(x)as.integer(cut(x,breaks=c(-Inf,-.7,0,.7,Inf)))))
binary<-as.data.frame(lapply(d,function(x)as.integer(x>0)))
fits<-list(ordinal=fit(ordinal,measurement='ordered'),ordinal_alpha=fit(ordinal,measurement='ordered',omega=FALSE),kr20=fit(binary,measurement='binary'),pearson_alpha=fit(d,omega=FALSE))
stopifnot(fits$ordinal$method=='ordinal',fits$kr20$method=='kr20')
total<-fit(d);total$subfactor<-'Total';a<-fit(d,names(d)[1:3]);a$subfactor<-'Normality';b<-fit(d,names(d)[4:6]);b$subfactor<-'사용자 요인'
fits$factors<-list(type='reliability_factors',total=total,factors=list(a,b),options=opts)
out<-'tmp/reliability-branches-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);baseline<-list();values<-list();exports<-list()
phrases<-jsonlite::read_json('scripts/fixtures/reliability_i18n_branches.json',simplifyVector=TRUE)
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language);combined<-character()
 for(name in names(fits)){
  html<-as.character(reliability_results_ui(fits[[name]]));doc<-xml2::read_html(html,encoding='UTF-8')
  main<-xml2::xml_find_all(doc,"//table[@data-result-table-role='main']")
  content<-xml2::xml_text(xml2::xml_find_all(main,".//th|.//td|preceding::h3[1]|ancestor::div[@data-result-table-sheet='true'][1]/*[not(self::table)]"))
  stopifnot(length(main)==1L,xml2::xml_attr(main,'data-result-table-language')=='en')
  appendix<-xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']")
  stopifnot(length(appendix)==1L,xml2::xml_attr(appendix,'data-result-table-language')==language)
  cells<-xml2::xml_text(xml2::xml_find_all(appendix,'.//td'))
  if(language=='en'){baseline[[name]]<-content;values[[name]]<-cells}
  stopifnot(identical(content,baseline[[name]]),identical(cells,values[[name]]),all(info$var_label %in% cells))
  combined<-c(combined,xml2::xml_text(doc))
  if(language=='ja')exports[[name]]<-html
 }
 if(!language %in% c('en','ko'))for(phrase in phrases){
  key<-paste0('analysis.ui.',gsub('^_|_$','',gsub('[^a-z0-9]+','_',tolower(phrase))))
  expected<-sub('[.]$','',statedu_t(key,language))
  if(!any(grepl(expected,combined,fixed=TRUE)))stop(language,' missing ',phrase)
 }
 cat('PASS:',language,'ordinal, KR-20, alpha-only and subfactor paths; main English; appendix translations; labels and values\n')
}
saveRDS(list(list(id='reliability-branches',title='Reliability branches',html=paste(unlist(exports),collapse='\n'))),file.path(out,'entries.rds'))
