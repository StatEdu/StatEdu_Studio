Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(934);f<-rnorm(100);d<-as.data.frame(setNames(lapply(1:4,function(i)f+rnorm(100,sd=.5)),paste0('i',1:4)))
info<-data.frame(name=names(d),measurement='continuous',var_label=c('Normality','Yes','사용자 문항','Status'))
fit<-prepare_reliability_results(d,names(d),info,options=list(omega=TRUE,normality=TRUE,reliability_if_deleted=TRUE,item_total_correlation=TRUE))
stopifnot(fit$method=='pearson')
out<-'tmp/reliability-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);baseline<-NULL;values<-NULL
phrases<-jsonlite::read_json('scripts/fixtures/reliability_i18n_phrases.json',simplifyVector=TRUE)
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language)
 html<-as.character(reliability_results_ui(fit));doc<-xml2::read_html(html,encoding='UTF-8')
 main<-xml2::xml_find_all(doc,"//table[@data-result-table-role='main']")
 stopifnot(length(main)==1L,xml2::xml_attr(main,'data-result-table-language')=='en')
 content<-xml2::xml_text(xml2::xml_find_all(main,".//th|.//td|preceding::h3[1]|ancestor::div[@data-result-table-sheet='true'][1]/*[not(self::table)]"))
 if(language=='en')baseline<-content
 stopifnot(identical(content,baseline))
 appendix<-xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']")
 stopifnot(length(appendix)==1L,xml2::xml_attr(appendix,'data-result-table-language')==language)
 cells<-xml2::xml_text(xml2::xml_find_all(appendix,'.//td'))
 if(language=='en')values<-cells
 stopifnot(identical(cells,values),all(info$var_label %in% cells))
 text<-xml2::xml_text(doc)
 if(!language %in% c('en','ko'))for(phrase in phrases){
  key<-paste0('analysis.ui.',gsub('^_|_$','',gsub('[^a-z0-9]+','_',tolower(phrase))))
  expected<-sub('[.]$','',statedu_t(key,language))
  if(!grepl(expected,text,fixed=TRUE))stop(language,' missing ',phrase,': ',expected)
 }
 if(language=='ja')saveRDS(list(list(id='reliability',title='Reliability',html=html)),file.path(out,'entries.rds'))
 cat('PASS:',language,'actual alpha/omega; English main content; localized item-analysis appendix; item labels and values preserved\n')
}
