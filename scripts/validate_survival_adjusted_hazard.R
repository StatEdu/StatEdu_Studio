source('output/survival-adjusted-hazard-20260914/common.R')
checks<-0L
for(k in c(2L,3L))for(rate in c(.25,.8,1))for(tied in c(FALSE,TRUE)) {
  data<-make_data(120L,k,rate,421+k,tied);fit<-fit_data(data)
  for(reps in c(0L,25L)) {
    outputs<-list()
    for(v in c('baseline','current')) {
      select_variant(v);conditions<-character();set.seed(941)
      result<-withCallingHandlers(survival_adjusted_curve(fit,data,'group',reps,selected_times=c(0,1,10,20)),
        warning=function(w){conditions<<-c(conditions,conditionMessage(w));invokeRestart('muffleWarning')},
        message=function(m){conditions<<-c(conditions,conditionMessage(m));invokeRestart('muffleMessage')})
      outputs[[v]]<-list(result=result,conditions=conditions,rng=.Random.seed)
    }
    stopifnot(identical(outputs$baseline,outputs$current,num.eq=FALSE));checks<-checks+1L
  }
  for(times in list(numeric(0),c(0,0,1,NA_real_,10,1),c(10,1,20,0))) {
    select_variant('baseline');before<-tryCatch(survival_adjusted_curve_once(fit,data,'group',times),error=function(e)list(error=conditionMessage(e)))
    select_variant('current');after<-tryCatch(survival_adjusted_curve_once(fit,data,'group',times),error=function(e)list(error=conditionMessage(e)))
    stopifnot(identical(before,after,num.eq=FALSE));checks<-checks+1L
  }
}
cat('PASS:',checks,'adjusted-curve checks; exact numerical results, conditions and RNG.\n')
