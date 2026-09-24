.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
capture<-function(f) {
 set.seed(718);conditions<-list()
 output<-capture.output(value<-tryCatch(withCallingHandlers(f(),warning=function(e){conditions[[length(conditions)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleWarning')},message=function(e){conditions[[length(conditions)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleMessage')}),error=function(e)list(error=class(e),message=conditionMessage(e))))
 list(value=value,conditions=conditions,output=output,rng=.Random.seed)
}
old<-new.env(parent=.GlobalEnv);new<-new.env(parent=.GlobalEnv)
for(e in list(old,new))sys.source('R/analysis_ttest_anova.R',e)
replacements<-0L
restore<-function(node) {
 if(identical(node,quote(effect_size(values,groups,test_type)))) {
  replacements<<-replacements+1L;return(quote(ttest_effect_size(values,groups,test_type)))
 }
 if(is.call(node)||is.pairlist(node)||is.expression(node))for(i in seq_along(node)) {
  if(identical(node[[i]],quote(expr=)))next
  node[i]<-list(restore(node[[i]]))
 }
 node
}
body(old$ttest_single_result)<-restore(body(old$ttest_single_result));stopifnot(replacements==1L)
make_values<-function(n,kind) {
 set.seed(939);x<-rnorm(n)
 if(kind=='tied')x<-round(x)
 if(kind=='constant')x[]<-1
 if(kind=='near')x<-1+seq_len(n)*.Machine$double.eps
 if(kind=='near_large')x<-1e12+seq_len(n)*.0001220703125
 if(kind=='missing'&&n>0)x[1]<-NA_real_
 if(kind=='infinite'&&n>0)x[1]<-Inf
 x
}
count<-0L
for(k in c(2L,4L))for(n in c(0L,1L,4L,20L,100L,20000L))for(kind in c('normal','tied','constant','near','near_large','missing','infinite')) {
 x<-make_values(n,kind);g<-rep(seq_len(k),length.out=n);method<-if(k==2L)'mw'else 'kw'
 provider<-ttest_cached_nonparametric_test(ttest_effect_size)
 a<-capture(function()ttest_effect_size(x,g,method))
 b<-capture(function()provider(x,g,method));c<-capture(function()provider(x,g,method))
 stopifnot(identical(a,b,num.eq=FALSE),identical(a,c,num.eq=FALSE));count<-count+1L
}
cat('PASS effect values/diagnostics/stdout/RNG:',count,'\n')
count<-0L
for(k in c(2L,4L))for(kind in c('normal','tied','constant','near','near_large','missing'))for(extra in c(FALSE,TRUE)) {
 x<-make_values(100L,kind);d<-data.frame(y=x,y2=rev(x),g=factor(rep(seq_len(k),each=100L/k)))
 info<-data.frame(name=names(d),measurement=c('continuous','continuous','category'))
 invoke<-function(e)capture(function()e$prepare_ttest_anova_results(d,c('y','y2'),'g',variable_info=info,options=list(force_nonparametric=TRUE,post_hoc=TRUE,add_mean_sd=extra,effect_size=TRUE)))
 a<-invoke(old);b<-invoke(new);stopifnot(identical(a,b,num.eq=FALSE),is.null(a$value$error));count<-count+1L
}
cat('PASS full results/diagnostics/stdout/RNG:',count,'\n')
for(k in c(2L,4L)) {
 name<-if(k==2L)'ttest_cliffs_delta'else 'ttest_kruskal_epsilon_squared'
 original_raw<-new[[name]];raw<-list()
 f_raw<-function(values,groups) {
  value<-original_raw(values,groups);raw[[length(raw)+1L]]<<-value;value
 }
 old[[name]]<-f_raw;new[[name]]<-f_raw
 for(kind in c('normal','tied','constant','near','near_large','missing')) {
  x<-make_values(100L,kind);d<-data.frame(y=x,g=factor(rep(seq_len(k),each=100L/k)))
  info<-data.frame(name=names(d),measurement=c('continuous','category'))
  invoke<-function(e)capture(function()e$prepare_ttest_anova_results(d,'y','g',variable_info=info,options=list(force_nonparametric=TRUE,add_mean_sd=TRUE,effect_size=TRUE)))
  raw<-list();a<-invoke(old);reference_raw<-raw
  raw<-list();b<-invoke(new);stopifnot(identical(a,b,num.eq=FALSE))
  if(length(reference_raw)==0L) {
   stopifnot(kind=='constant',length(raw)==0L)
  } else {
   stopifnot(length(reference_raw)==2L,length(raw)==if(k==4L)1L else 2L)
   stopifnot(identical(reference_raw[[1]],raw[[1]],num.eq=FALSE),identical(reference_raw[[2]],raw[[1]],num.eq=FALSE))
  }
 }
 old[[name]]<-original_raw;new[[name]]<-original_raw
}
cat('PASS 12 integrated raw numeric cases, KW reuse, MW bypass, constant skip\n')
original<-new$ttest_effect_size
for(k in c(2L,4L))for(signal in c('quiet','warning','message','rng','error')) {
 d<-data.frame(y=make_values(100L,'normal'),g=factor(rep(seq_len(k),each=100L/k)))
 info<-data.frame(name=names(d),measurement=c('continuous','category'))
 calls<-0L;raw<-list()
 f<-function(values,groups,method) {
  calls<<-calls+1L
  if(signal=='warning')warning('test effect warning')
  if(signal=='message')message('test effect message')
  if(signal=='rng')invisible(runif(1))
  if(signal=='error')stop('test effect error')
  value<-original(values,groups,method);raw[[length(raw)+1L]]<<-value;value
 }
 old$ttest_effect_size<-f;new$ttest_effect_size<-f
 invoke<-function(e)capture(function()e$prepare_ttest_anova_results(d,'y','g',variable_info=info,options=list(force_nonparametric=TRUE,post_hoc=TRUE,add_mean_sd=TRUE,effect_size=TRUE)))
 a<-invoke(old);reference_raw<-raw;reference_calls<-calls;calls<-0L;raw<-list()
 b<-invoke(new);stopifnot(identical(a,b,num.eq=FALSE),calls==if(signal=='quiet'&&k==4L)1L else reference_calls)
 if(signal=='quiet'&&k==4L) {
  stopifnot(reference_calls==2L,identical(raw[[1]],reference_raw[[1]],num.eq=FALSE),identical(raw[[1]],reference_raw[[2]],num.eq=FALSE))
  b<-invoke(new);stopifnot(identical(a,b,num.eq=FALSE),calls==2L)
 }
}
cat('PASS 10 integrated guards, effect reuse, calls 2->1 and fresh next analysis\n')

