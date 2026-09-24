Sys.setlocale('LC_CTYPE','English_United States.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
d<-data.frame(id=rep(1:4,each=2),time=1,x=1,y=1:8)
complete<-longitudinal_ipw_weights(d,d,'y','id','time','x')
d$y[c(2,5)]<-NA
intercept<-longitudinal_ipw_weights(d,d[complete.cases(d),],'y','id','time','x')
ipw_fallback_results<-list(complete=complete,intercept=intercept)
before_ipw<-serialize(ipw_fallback_results,NULL)
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(key in names(ipw_fallback_results)) {
 result<-ipw_fallback_results[[key]]
 stopifnot(all(result$weights==1))
 translated<-longitudinal_appendix_text(result$note,lang)
 if(lang=='en')stopifnot(identical(translated,result$note)) else
   if(identical(translated,result$note))stop('Untranslated IPW fallback: ',lang,' ',key)
 table<-longitudinal_appendix_table(result$diagnostics,lang)
 stopifnot(tail(table[[2]],1)==translated,identical(table[[2]][2:5],result$diagnostics$Value[2:5]))
 user<-data.frame(Variable=result$note,Details=result$note)
 stopifnot(identical(longitudinal_appendix_table(user,lang)[[1]],user$Variable))
 cat('PASS IPW fallback:',lang,key,'\n')
}
stopifnot(identical(before_ipw,serialize(ipw_fallback_results,NULL)))
