Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
cases<-list(
 g_sample=meta_normalize_g(list(n1=1,n0=10),'means'),
 g_means=meta_normalize_g(list(n1=10,n0=10,m1=1,m0=0,sd1=0,sd0=1),'means'),
 g_pooled=meta_normalize_g(list(n1=10,n0=10,m1=1,m0=0,sd1=1e-300,sd0=1e-300),'means'),
 g_se=meta_normalize_g(list(g=0.5,se=0),'g_se'),
 g_ci=meta_normalize_g(list(g=0.5,ci_lower=0.6,ci_upper=1),'g_ci'),
 g_d=meta_normalize_g(list(d_value=Inf),'d'),
 g_t=meta_normalize_g(list(t_value=Inf),'t'),
 g_rpb=meta_normalize_g(list(r_pb=1),'r_pb'),
 g_type=meta_normalize_g(list(),'unsupported'))
valid<-meta_normalize_g(list(r_pb=0.3,n1=30,n0=40),'r_pb');before<-valid
compound<-meta_normalize_effect(list(study_id='study %s',family='g',input_type='g_se',g=0.5,se=0,moderator_continuous='bad'))
for(lang in c('en','ko','ja','zh','es','fr','de','vi','ko')) {
 for(key in names(cases)) {
  result<-cases[[key]];stopifnot(result$status=='error',result$message==statedu_t(paste0('meta.input_error.',key),'en'))
  stopifnot(meta_input_detail_text(result$message,lang)==statedu_t(paste0('meta.input_error.',key),lang))
 }
 stopifnot(valid$status=='valid',is.finite(valid$yi),valid$vi>0,
  meta_input_detail_text(valid$assumption,lang)==statedu_t('meta.input_error.g_rpb_assumption',lang),identical(valid,before),
  meta_input_detail_text(compound$message,lang)==paste(statedu_t('meta.input_error.g_se',lang),statedu_t('meta.input_error.pairs',lang)))
 for(raw in c('사용자 <&> %s',paste(cases$g_se$message,'External: 사용자 %s'),paste('External:',cases$g_se$message)))
   stopifnot(identical(meta_input_detail_text(raw,lang),raw))
 cat('PASS:',lang,'nine actual g errors; valid point-biserial assumption; actual compound error; unknown details preserved\n')
}
