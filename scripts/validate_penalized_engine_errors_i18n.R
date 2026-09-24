Sys.setlocale("LC_CTYPE","English_United States.utf8")
Sys.setenv(STATEDU_MODULE_CACHE="false")
source("R/app_bootstrap.R",encoding="UTF-8");load_app_packages(check=FALSE);source_app_modules()
messages<-character()
visit<-function(node){
 if(is.call(node)&&identical(node[[1]],as.name('stop'))&&length(node)>=2L&&is.character(node[[2]]))messages<<-c(messages,node[[2]])
 if(is.call(node)||is.expression(node))for(i in seq_along(node)){if(identical(node[[i]],quote(expr=)))next;if(!is.symbol(node[[i]]))visit(node[[i]])}
}
visit(parse('R/analysis_penalized.R',encoding='UTF-8'));messages<-unique(messages);stopifnot(length(messages)==14L)
capture_error<-function(expr){v<-tryCatch({force(expr);NULL},error=function(e)conditionMessage(e));stopifnot(!is.null(v));v}
x<-matrix(seq_len(60),30,2);y<-seq_len(30)
actual<-c(capture_error(penalized_selection_bootstrap_workers(0)),
 capture_error(penalized_training_fit(x,rep(1,30),'LASSO',1)),
 capture_error(penalized_nested_validation(x[1:8,],y[1:8],'LASSO',1)),
 capture_error(penalized_repeated_validation(x,y,'LASSO',1,repeats=0)),
 capture_error(penalized_multisplit(x,y,'Ridge',1)),
 capture_error(penalized_multisplit(x[1:8,],y[1:8],'LASSO',1)),
 capture_error(penalized_multisplit(x,y,'LASSO',1,splits=19)))
stopifnot(all(actual %in% messages))
fixture<-jsonlite::read_json('scripts/fixtures/penalized_i18n_engine_errors.json',simplifyVector=TRUE)
for(language in c('en','ko','ja','zh','es','fr','de','vi')) {
 for(message in messages) {
  english<-if(grepl(' / ',message,fixed=TRUE))sub('^.* / ','',message)else message
  stopifnot(english %in% names(fixture))
  expected<-statedu_t(fixture[[english]],language)
  stopifnot(identical(penalized_error_ui_text(message,language),expected))
  if(language!='en')stopifnot(expected!=english)
 }
 unknown<-'External glmnet error: 사용자.A 50%'
 stopifnot(identical(penalized_error_ui_text(unknown,language),unknown))
 cat('PASS:',language,'all 14 explicit engine errors; 7 actual failures; external detail preserved\n')
}
