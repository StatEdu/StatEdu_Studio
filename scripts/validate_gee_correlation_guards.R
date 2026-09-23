Sys.setlocale('LC_ALL','Korean_Korea.utf8')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
for(corstr in c('exchangeable','ar1')){
 longitudinal_gee_check_simple_correlation(corstr,.3,rep(1:2,each=3),rep(c(1,2,4),2))
 for(alpha in c(1,1.1)){
  e<-tryCatch(longitudinal_gee_check_simple_correlation(corstr,alpha,rep(1:2,each=3),rep(c(1,2,4),2)),error=identity)
  stopifnot(inherits(e,'error'),grepl('estimates were not accepted',conditionMessage(e),fixed=TRUE))
 }
}
# Exchangeable lower bound depends on observed subject size, not global waves.
longitudinal_gee_check_simple_correlation('exchangeable',-.4,rep(1:2,each=2),c(1,2,3,4))
e<-tryCatch(longitudinal_gee_check_simple_correlation('exchangeable',-.5,rep(1,3),1:3),error=identity)
stopifnot(inherits(e,'error'))
cat('PASS: working correlation guards\n')
