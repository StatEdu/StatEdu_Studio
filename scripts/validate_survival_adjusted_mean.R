.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
current <- survival_adjusted_curve_once
baseline <- current
original <- quote(mean(values, na.rm = TRUE))
replacement <- quote(mean(values, na.rm = anyNA(values)))
replaced <- 0L
restore <- function(x) {
 if(identical(x,replacement)){replaced<<-replaced+1L;return(original)}
 if(is.call(x))for(i in seq_along(x))x[i]<-list(restore(x[[i]]))
 x
}
body(baseline)<-restore(body(baseline));stopifnot(replaced==1L)
select_variant<-function(v)assign('survival_adjusted_curve_once',if(v=='baseline')baseline else current,.GlobalEnv)
make_data<-function(n=600L,k=3L,rate=.45,seed=421,tied=FALSE) {
 set.seed(seed)
 data.frame(time=if(tied)sample(1:40,n,TRUE)else rexp(n,.05),event=rbinom(n,1,rate),group=factor(rep(seq_len(k),length.out=n)),age=rnorm(n,50,10))
}
fit_data<-function(data)survival::coxph(survival::Surv(time,event)~group+age,data=data,x=TRUE)
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
select_variant('current')
capture <- function(f) {
  conditions <- character()
  stdout <- capture.output(value <- withCallingHandlers(f(),
    warning=function(w){conditions<<-c(conditions,conditionMessage(w));invokeRestart('muffleWarning')},
    message=function(m){conditions<<-c(conditions,conditionMessage(m));invokeRestart('muffleMessage')}))
  list(value=value,conditions=conditions,stdout=stdout)
}
risks <- list(numeric(0),0,-0,1,c(0,1,2),c(NA_real_,1),c(NaN,1),
              c(NA_real_,NaN),c(Inf,0,1),c(-Inf,0,1),
              c(.Machine$double.xmin,.Machine$double.xmax),rep(1,2000))
checks <- 0L
for(risk in risks)for(hazard in c(0,-0,1,-1,Inf,-Inf,NA_real_,NaN,1e-300,1e300)) {
  before <- capture(function()mean(exp(-hazard*risk),na.rm=TRUE))
  after <- capture(function(){values<-exp(-hazard*risk);mean(values,na.rm=anyNA(values))})
  stopifnot(identical(before,after,num.eq=FALSE));checks<-checks+1L
}
cat('PASS:',checks,'exact mean/condition/stdout boundary checks.\n')
