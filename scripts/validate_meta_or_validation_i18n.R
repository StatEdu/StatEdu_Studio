Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
cases<-list(or_nonnegative=meta_normalize_or(list(cell_a=-1,cell_b=2,cell_c=3,cell_d=4),'2x2'),
 or_integer=meta_normalize_or(list(cell_a=.5,cell_b=2,cell_c=3,cell_d=4),'2x2'),
 or_ci=meta_normalize_or(list(or_value=2,ci_lower=3,ci_upper=4),'or_ci'),
 or_log=meta_normalize_or(list(log_or=.3,se=0),'logor_se'),
 or_b=meta_normalize_or(list(logit_b=Inf,se=.2),'logistic_b'),
 or_type=meta_normalize_or(list(),'unsupported'))
corrected<-meta_normalize_or(list(cell_a=0,cell_b=2,cell_c=3,cell_d=4),'2x2')
logistic<-meta_normalize_or(list(logit_b=.3,se=.2),'logistic_b')
stopifnot(corrected$status=='warning',isTRUE(all.equal(corrected$yi,log(.5*4.5/(2.5*3.5)))),
 isTRUE(all.equal(corrected$vi,sum(1/c(.5,2.5,3.5,4.5)))),logistic$status=='valid',logistic$yi==.3,isTRUE(all.equal(logistic$vi,.04)))
notes<-c(or_correction=corrected$message,or_correction_assumption=corrected$assumption,or_coding=logistic$assumption)
compound<-meta_normalize_effect(list(study_id='study %s',family='or',input_type='logor_se',log_or=.3,se=0,moderator_continuous='bad'))
for(lang in c('en','ko','ja','zh','es','fr','de','vi','ko')) {
 for(key in names(cases)) {
  result<-cases[[key]];stopifnot(result$status=='error',result$message==statedu_t(paste0('meta.input_error.',key),'en'),
   meta_input_detail_text(result$message,lang)==statedu_t(paste0('meta.input_error.',key),lang))
 }
 for(key in names(notes))stopifnot(meta_input_detail_text(notes[[key]],lang)==statedu_t(paste0('meta.input_error.',key),lang))
 stopifnot(meta_input_detail_text(compound$message,lang)==paste(statedu_t('meta.input_error.or_log',lang),statedu_t('meta.input_error.pairs',lang)))
 raw<-paste(corrected$message,'사용자 <&> %s');stopifnot(meta_input_detail_text(raw,lang)==raw)
 cat('PASS:',lang,'six actual OR errors; three warnings/assumptions; correction and logistic conversion; compound/raw text\n')
}
