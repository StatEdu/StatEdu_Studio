.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
old<-new.env(parent=.GlobalEnv);new<-new.env(parent=.GlobalEnv)
for(e in list(old,new))sys.source('R/analysis_nonparametric_paired.R',e)
replacements<-0L
restore<-function(node) {
 if(!is.call(node))return(node)
 if(identical(node[[1]],quote(paired_rm_wilcox_engine()))) {node[1]<-list(quote(stats::wilcox.test));replacements<<-replacements+1L}
 for(i in seq_along(node)[-1L])if(!identical(node[[i]],quote(expr=)))node[i]<-list(restore(node[[i]]))
 node
}
f<-old$nonparametric_paired_analyze_pair;body(f)<-restore(body(f));old$nonparametric_paired_analyze_pair<-f
stopifnot(replacements==1L)
capture<-function(f) {
 set.seed(718);conditions<-list()
 output<-capture.output(value<-tryCatch(withCallingHandlers(f(),warning=function(e){conditions[[length(conditions)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleWarning')},message=function(e){conditions[[length(conditions)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleMessage')}),error=function(e)list(error=class(e),message=conditionMessage(e))))
 list(value=value,conditions=conditions,output=output,rng=.Random.seed)
}
count<-0L
for(kind in c('continuous','ordered'))for(scenario in c('ordinary','missing','zero','infinite','constantdiff','factor'))for(median_iqr in c(FALSE,TRUE))for(unified in c(FALSE,TRUE)) {
 set.seed(939);d<-data.frame(pre=rnorm(80),post=rnorm(80))
 if(kind=='ordered')d[]<-lapply(d,round)
 if(scenario=='missing'){d$pre[1:4]<-NA;d$post[3:8]<-NA}
 if(scenario=='zero')d$post<-d$pre
 if(scenario=='infinite')d$post[1:2]<-c(Inf,-Inf)
 if(scenario=='constantdiff')d$post<-d$pre+1
 if(scenario=='factor')d[]<-lapply(d,function(x)factor(round(x)))
 info<-data.frame(name=names(d),measurement=kind)
 invoke<-function(e)capture(function() {
  options<-list(median_iqr=median_iqr,add_mean_sd=TRUE,effect_size=TRUE)
  if(unified)e$prepare_nonparametric_paired_unified_results(d,list(c('pre','post')),variable_info=info,options=options)
  else e$prepare_nonparametric_paired_results(d,'pre','post',variable_info=info,options=options)
 })
 a<-invoke(old);b<-invoke(new);stopifnot(identical(a,b,num.eq=FALSE))
 if(scenario %in% c('ordinary','missing'))stopifnot(is.null(a$value$error))
 count<-count+1L
}
cat('PASS complete and unified results/conditions/stdout/RNG:',count,'\n')
candidate<-paired_rm_wilcox_engine();count<-0L
for(kind in c('continuous','ordered'))for(missing_rate in c(0,.1)) {
 set.seed(939);d<-as.data.frame(matrix(if(kind=='continuous')rnorm(20000*16)else sample(1:5,20000*16,TRUE),20000,16))
 if(missing_rate>0)d[matrix(runif(20000*16)<missing_rate,20000,16)]<-NA
 for(i in 1:8) {
  pair<-paired_complete_data(paired_numeric(d[[i]]),paired_numeric(d[[i+8]]))
  stopifnot(identical(capture(function()stats::wilcox.test(pair$y,pair$x,paired=TRUE,exact=FALSE)),capture(function()candidate(pair$y,pair$x,paired=TRUE,exact=FALSE)),num.eq=FALSE))
  count<-count+1L
 }
}
cat('PASS large raw htest/conditions/stdout/RNG:',count,'\n')
