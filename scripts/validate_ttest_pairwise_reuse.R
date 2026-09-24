.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
capture<-function(f) {
 set.seed(718);conditions<-list()
 output<-capture.output(value<-tryCatch(withCallingHandlers(f(),warning=function(e){conditions[[length(conditions)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleWarning')},message=function(e){conditions[[length(conditions)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleMessage')}),error=function(e)list(error=class(e),message=conditionMessage(e))))
 list(value=value,conditions=conditions,output=output,rng=.Random.seed)
}
# Test observable side effects and ensure only quiet deterministic calls are reused.
for(signal in c('quiet','warning','message','rng','error')) {
 calls<-0L
 original<-function(values,groups,method) {
  calls<<-calls+1L
  if(signal=='warning')warning('test warning')
  if(signal=='message')message('test message')
  if(signal=='error')stop('test error')
  value<-if(signal=='rng')runif(1)else sum(values)
  matrix(value,2,2,dimnames=list(c('a','b'),c('a','b')))
 }
 invoke<-function(f)capture(function()lapply(1:2,function(i)tryCatch(f(1:4,c('a','a','b','b'),'holm'),error=function(e)list(class=class(e),message=conditionMessage(e)))))
 a<-invoke(original);calls<-0L;b<-invoke(ttest_cached_nonparametric_pairwise(original))
 stopifnot(identical(a,b,num.eq=FALSE),calls==if(signal=='quiet')1L else 2L)
}
calls<-0L
f<-ttest_cached_nonparametric_pairwise(function(values,groups,method){calls<<-calls+1L;list(values,groups,method)})
for(pass in 1:2)for(i in 1:18)stopifnot(identical(f(i,'g','holm'),list(i,'g','holm')))
stopifnot(calls==20L)
invisible(f(1L,'changed','holm'));invisible(f(1L,'g','bonferroni'));invisible(f(1,'g','holm'))
stopifnot(calls==23L)
cat('PASS quiet/signal/RNG/error guards, 16-entry bound and exact keys\n')
old<-new.env(parent=.GlobalEnv);new<-new.env(parent=.GlobalEnv)
for(e in list(old,new))sys.source('R/analysis_ttest_anova.R',e)
old$ttest_cached_nonparametric_pairwise<-function(original)original
count<-0L
for(k in c(2L,4L,8L))for(scenario in c('ordinary','tied','missing','constant'))for(method in c('holm','bonferroni'))for(extra in c(FALSE,TRUE)) {
 set.seed(939);g<-rep(seq_len(k),length.out=240);d<-data.frame(y=rnorm(240)+g,y2=rnorm(240)+g*.5,g=factor(g))
 if(scenario=='tied'){d$y<-round(d$y);d$y2<-round(d$y2)}
 if(scenario=='missing'){d$y[1:12]<-NA;d$y2[13:24]<-NA}
 if(scenario=='constant'){d$y[]<-1;d$y2[]<-2}
 info<-data.frame(name=names(d),measurement=c('continuous','continuous','category'))
 invoke<-function(e)capture(function()e$prepare_ttest_anova_results(d,c('y','y2'),'g',variable_info=info,options=list(force_nonparametric=TRUE,post_hoc=TRUE,add_mean_sd=extra,nonparametric_post_hoc_method=method,effect_size=TRUE)))
 a<-invoke(old);b<-invoke(new);stopifnot(identical(a,b,num.eq=FALSE),is.null(a$value$error))
 if(scenario!='constant')stopifnot(length(a$value$results)>0)
 count<-count+1L
}
cat('PASS complete results/diagnostics/stdout/RNG:',count,'\n')
# Verify raw matrices, real call reduction and a fresh cache for the next analysis.
set.seed(939);g<-rep(1:4,each=100);d<-data.frame(y=rnorm(400)+g,g=factor(g));info<-data.frame(name=names(d),measurement=c('continuous','category'))
real<-new$ttest_nonparametric_pairwise;matrices<-list()
new$ttest_nonparametric_pairwise<-function(values,groups,method) {
 value<-real(values,groups,method);matrices[[length(matrices)+1L]]<<-value;value
}
invoke<-function(e)capture(function()e$prepare_ttest_anova_results(d,'y','g',variable_info=info,options=list(force_nonparametric=TRUE,post_hoc=TRUE,add_mean_sd=TRUE)))
old$ttest_nonparametric_pairwise<-new$ttest_nonparametric_pairwise
a<-invoke(old);reference_matrices<-matrices;stopifnot(length(matrices)==2L);matrices<-list()
b<-invoke(new);stopifnot(identical(a,b,num.eq=FALSE),length(matrices)==1L,identical(matrices[[1]],reference_matrices[[1]],num.eq=FALSE),identical(matrices[[1]],reference_matrices[[2]],num.eq=FALSE))
b<-invoke(new);stopifnot(identical(a,b,num.eq=FALSE),length(matrices)==2L)
cat('PASS raw pairwise matrices, calls 2->1 and per-analysis cache lifetime\n')
