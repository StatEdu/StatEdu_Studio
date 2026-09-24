source('scripts/validate_sem_shared_inverse_compatibility.R')
source('scripts/fixtures/sem_shared_inverse_eligibility.R')
prepared<-structural_canvas_prepare_effect_bootstrap(fixture$snapshot,fixture$data,'sem','ML','fiml',FALSE,
 character(),character(),numeric(),original_result=original)
checks<-0L
for(reps in c(0,24,199,200,5000,200.5,NA,Inf)) {
 expected<-is.finite(reps)&&reps>=200&&reps==trunc(reps)
 stopifnot(identical(sem_shared_inverse_eligible(prepared,reps,4L),expected));checks<-checks+1L
}
for(workers in c(0,1,2,4,2.5,NA,Inf)) {
 expected<-is.finite(workers)&&workers>=2&&workers==trunc(workers)
 stopifnot(identical(sem_shared_inverse_eligible(prepared,5000L,workers),expected));checks<-checks+1L
}
for(kind in c('missing','nonfinite','plain_sem','unknown_method','empty_pairs','std_lv','groups','categorical','composites','conditional','constraints','estimator','representation')) {
 p<-prepared
 if(kind=='missing')p$data[1,1]<-NA_real_
 if(kind=='nonfinite')p$data[1,1]<-Inf
 if(kind=='plain_sem')p$product_specs<-list()
 if(kind=='unknown_method')p$product_specs[[1]]$method<-'unknown'
 if(kind=='empty_pairs')p$product_specs[[1]]$pairs<-p$product_specs[[1]]$pairs[FALSE,,drop=FALSE]
 if(kind=='std_lv')p$fit_template@Options$std.lv<-TRUE
 if(kind=='groups')p$fit_template@Model@nblocks<-2L
 if(kind=='categorical')p$fit_template@Model@categorical<-TRUE
 if(kind=='composites')p$fit_template@Model@composites<-TRUE
 if(kind=='conditional')p$fit_template@Model@conditional.x<-TRUE
 if(kind=='constraints')p$fit_template@Model@ceq.simple.only<-TRUE
 if(kind=='estimator')p$fit_template@Model@estimator<-'GLS'
 if(kind=='representation')p$fit_template@Model@representation<-'RAM'
 stopifnot(!sem_shared_inverse_eligible(p,5000L,4L));checks<-checks+1L
}
p<-prepared;p$product_specs[[1]]$method<-'matched_pair_dmc'
stopifnot(sem_shared_inverse_eligible(p,5000L,4L));checks<-checks+1L
stopifnot(!sem_shared_inverse_eligible(NULL,5000L,4L));checks<-checks+1L
cat('PASS:',checks,'job eligibility boundary cases; this checks policy, not end-to-end execution of excluded jobs\n')
