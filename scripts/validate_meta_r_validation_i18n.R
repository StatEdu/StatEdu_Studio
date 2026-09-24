Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
cases<-list(r_range=meta_normalize_r(list(r=1,n=20),'r'),
 r_sample=meta_normalize_r(list(r=.3,n=3),'r'),
 r_controls=meta_normalize_r(list(r=.3,n=20,k_controls=-1),'partial_r'),
 r_df=meta_normalize_r(list(r=.3,n=5,k_controls=2),'partial_r'),
 r_z=meta_normalize_r(list(fisher_z=.3,se=0),'z_se'),
 r_t=meta_normalize_r(list(t_value=Inf,n=20),'t'),
 r_type=meta_normalize_r(list(),'unsupported'))
valid<-meta_normalize_r(list(r=.3,n=20,k_controls=2),'partial_r');before<-valid
stopifnot(valid$status=='valid',isTRUE(all.equal(valid$yi,atanh(.3))),isTRUE(all.equal(valid$vi,1/15)))
compound<-meta_normalize_effect(list(study_id='study %s',family='r',input_type='r',r=1,n=20,moderator_continuous='bad'))
for(lang in c('en','ko','ja','zh','es','fr','de','vi','ko')) {
 for(key in names(cases)) {
  result<-cases[[key]];stopifnot(result$status=='error',result$message==statedu_t(paste0('meta.input_error.',key),'en'),
   meta_input_detail_text(result$message,lang)==statedu_t(paste0('meta.input_error.',key),lang))
 }
 stopifnot(meta_input_detail_text(valid$assumption,lang)==statedu_t('meta.input_error.r_assumption',lang),identical(valid,before),
   meta_input_detail_text(compound$message,lang)==paste(statedu_t('meta.input_error.r_range',lang),statedu_t('meta.input_error.pairs',lang)))
 raw<-paste(cases$r_range$message,'사용자 <&> %s');stopifnot(meta_input_detail_text(raw,lang)==raw)
 cat('PASS:',lang,'seven actual correlation errors; partial correlation assumption/calculation; compound error; raw details\n')
}
