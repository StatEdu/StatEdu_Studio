.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
capture<-function(f) {
 set.seed(718);conditions<-list()
 output<-capture.output(value<-tryCatch(withCallingHandlers(f(),warning=function(e){conditions[[length(conditions)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleWarning')},message=function(e){conditions[[length(conditions)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleMessage')}),error=function(e)list(error=class(e),message=conditionMessage(e))))
 list(value=value,conditions=conditions,output=output,rng=.Random.seed)
}
count<-0L
for(k in c(2L,4L))for(n in c(0L,1L,4L,100L,20000L))for(kind in c('normal','tied','constant','missing','infinite')) {
 set.seed(939);values<-rnorm(n);groups<-rep(seq_len(k),length.out=n)
 if(kind=='tied')values<-round(values)
 if(kind=='constant')values[]<-1
 if(kind=='missing'&&n>0)values[1]<-NA
 if(kind=='infinite'&&n>0)values[1]<-Inf
 a<-capture(function()if(k==2L)stats::wilcox.test(values ~ as.factor(groups),exact=FALSE,correct=FALSE)else stats::kruskal.test(values ~ as.factor(groups)))
 b<-capture(function()ttest_nonparametric_main_test(values,groups,if(k==2L)'mw'else 'kw'))
 stopifnot(identical(a,b,num.eq=FALSE));count<-count+1L
}
cat('PASS raw main htest/diagnostics/stdout/RNG:',count,'\n')
old<-new.env(parent=.GlobalEnv);new<-new.env(parent=.GlobalEnv)
for(e in list(old,new))sys.source('R/analysis_ttest_anova.R',e)
cache<-old$ttest_cached_nonparametric_test
old$ttest_cached_nonparametric_test<-function(original) {
 if(identical(original,old$ttest_nonparametric_main_test))original else cache(original)
}
count<-0L
for(k in c(2L,4L,8L))for(kind in c('ordinary','tied','missing','constant'))for(extra in c(FALSE,TRUE))for(force in c(FALSE,TRUE)) {
 set.seed(939);g<-rep(seq_len(k),length.out=240);d<-data.frame(y=rnorm(240)+g,y2=rnorm(240)+g*.5,g=factor(g))
 if(kind=='tied'){d$y<-round(d$y);d$y2<-round(d$y2)}
 if(kind=='missing'){d$y[1:12]<-NA;d$y2[13:24]<-NA}
 if(kind=='constant'){d$y[]<-1;d$y2[]<-2}
 info<-data.frame(name=names(d),measurement=c('continuous','continuous','category'))
 invoke<-function(e)capture(function()e$prepare_ttest_anova_results(d,c('y','y2'),'g',variable_info=info,options=list(force_nonparametric=force,normality_enabled=FALSE,post_hoc=TRUE,add_mean_sd=extra,effect_size=TRUE)))
 a<-invoke(old);b<-invoke(new);stopifnot(identical(a,b,num.eq=FALSE),is.null(a$value$error))
 if(kind!='constant')stopifnot(length(a$value$results)>0)
 count<-count+1L
}
cat('PASS complete parametric/nonparametric results/diagnostics/stdout/RNG:',count,'\n')
# Inject observable diagnostics at the raw main test, within the actual analysis path.
original<-new$ttest_nonparametric_main_test
for(k in c(2L,4L))for(signal in c('quiet','warning','message','rng','error')) {
 set.seed(939);g<-rep(seq_len(k),each=30);d<-data.frame(y=rnorm(length(g))+g,g=factor(g));info<-data.frame(name=names(d),measurement=c('continuous','category'))
 calls<-0L;raw<-list()
 f<-function(values,groups,method) {
  if(identical(method,'mw_z'))return(original(values,groups,method))
  calls<<-calls+1L
  if(signal=='warning')warning('test main warning')
  if(signal=='message')message('test main message')
  if(signal=='rng')invisible(runif(1))
  if(signal=='error')stop('test main error')
  value<-original(values,groups,method);raw[[length(raw)+1L]]<<-value;value
 }
 old$ttest_nonparametric_main_test<-f;new$ttest_nonparametric_main_test<-f
 invoke<-function(e)capture(function()e$prepare_ttest_anova_results(d,'y','g',variable_info=info,options=list(force_nonparametric=TRUE,post_hoc=TRUE,add_mean_sd=TRUE)))
 a<-invoke(old);reference_raw<-raw;stopifnot(calls==2L);calls<-0L;raw<-list()
 b<-invoke(new);stopifnot(identical(a,b,num.eq=FALSE),calls==if(signal=='quiet')1L else 2L)
 if(signal=='quiet') {
  stopifnot(identical(raw[[1]],reference_raw[[1]],num.eq=FALSE),identical(raw[[1]],reference_raw[[2]],num.eq=FALSE))
  b<-invoke(new);stopifnot(identical(a,b,num.eq=FALSE),calls==2L)
 }
}
cat('PASS 10 integrated guard cases, raw htest reuse, calls 2->1 and fresh next analysis\n')
