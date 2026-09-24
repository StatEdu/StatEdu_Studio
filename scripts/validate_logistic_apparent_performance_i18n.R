Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(926);d<-data.frame(y=factor(rep(0:1,each=40)),x=rnorm(80))
info<-data.frame(name=c('y','x'),measurement=c('binary','continuous'),var_label=c('Review','Normality'))
results<-prepare_logistic_analysis_results(d,'y','x',variable_info=info)
phrases<-c('Apparent model performance','These statistics describe the estimation sample and are not a substitute for internal validation, holdout testing, or external validation.','AUC (apparent)','Brier score (apparent)','Tjur R² (apparent)','Basis')
before<-serialize(results,NULL);captured<-list();failures<-character()
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 options(statedu.app_language=lang)
 expected<-vapply(phrases,logistic_appendix_text,character(1),language=lang)
 # Basis is a header translated by logistic_appendix_table, including its Korean map.
 expected[6]<-names(logistic_appendix_table(data.frame(Basis=''),lang))[1]
 if(lang!='en'&&any(expected==phrases))failures<-c(failures,paste(lang,'catalog'))
 html<-as.character(htmltools::renderTags(logistic_results_panel(results,info))$html);doc<-xml2::read_html(html)
 block<-xml2::xml_find_first(doc,"//div[contains(@class,'performance-panel')]");stopifnot(!inherits(block,'xml_missing'))
 text<-xml2::xml_text(block)
 if(!all(vapply(expected,function(p)grepl(p,text,fixed=TRUE),logical(1))))failures<-c(failures,paste(lang,'render'))
 main<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']"));stopifnot(length(main)>0)
 if(lang=='en')baseline<-main else stopifnot(identical(main,baseline))
 stopifnot(identical(before,serialize(results,NULL)))
 probe<-logistic_appendix_table(data.frame(Variable=phrases,Metric=phrases),lang);stopifnot(identical(probe[[1]],phrases))
 captured[[lang]]<-html
}
if(length(failures))stop(paste('Apparent performance failures:',paste(failures,collapse=', ')))
out<-'tmp/logistic-apparent-performance-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
jsonlite::write_json(captured,file.path(out,'captured.json'),auto_unbox=TRUE)
cat('PASS actual binary logistic apparent performance labels and note in eight languages; main/source/user labels preserved\n')
