.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
capture<-function(f) {
 set.seed(718);conditions<-list()
 output<-capture.output(value<-tryCatch(withCallingHandlers(f(),warning=function(e){conditions[[length(conditions)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleWarning')},message=function(e){conditions[[length(conditions)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleMessage')}),error=function(e)list(error=class(e),message=conditionMessage(e))))
 list(value=value,conditions=conditions,output=output,rng=.Random.seed)
}
reference<-get('wilcox.test.default',asNamespace('stats'));saved<-deparse(reference)
candidate<-paired_rm_wilcox_engine()
stopifnot(!identical(deparse(candidate),saved))
altered<-reference;body(altered)<-quote(stop('changed implementation'))
stopifnot(identical(paired_rm_build_wilcox_engine(altered),stats::wilcox.test))
set.seed(939);count<-0L
for(n in c(0L,1L,2L,10L,100L))for(kind in c('continuous','tied','zero','missing','infinite'))for(alternative in c('two.sided','less','greater'))for(correct in c(FALSE,TRUE)) {
 x<-rnorm(n);y<-rnorm(n)
 if(kind=='tied'){x<-round(x);y<-round(y)}
 if(kind=='zero')y<-x
 if(kind=='missing'&&n>0)y[1]<-NA_real_
 if(kind=='infinite'&&n>0)x[1]<-Inf
 invoke<-function(f)capture(function()f(x,y,paired=TRUE,exact=FALSE,alternative=alternative,correct=correct))
 stopifnot(identical(invoke(reference),invoke(candidate),num.eq=FALSE));count<-count+1L
}
for(tied in c(FALSE,TRUE))for(paired in c(FALSE,TRUE))for(exact in c(FALSE,TRUE))for(alternative in c('two.sided','less','greater'))for(correct in c(FALSE,TRUE)) {
 set.seed(939);x<-rnorm(8);y<-rnorm(8)
 if(tied){x<-round(x);y<-round(y)}
 invoke<-function(f)capture(function()f(x,y,paired=paired,exact=exact,alternative=alternative,correct=correct,conf.int=TRUE,digits.rank=5))
 stopifnot(identical(invoke(reference),invoke(candidate),num.eq=FALSE));count<-count+1L
}
cat('PASS raw htest, conditions, stdout and RNG:',count,'\n')
count<-0L
for(n in c(0L,1L,100L))for(kind in c('continuous','tied','constant','infinite')) {
 set.seed(939);x<-rnorm(n)
 if(kind=='tied')x<-round(x)
 if(kind=='constant')x[]<-1
 if(kind=='infinite'&&n>0)x[1]<-Inf
 r<-rank(abs(x));old<-table(r);new<-tabulate(2*r,nbins=2*length(r));new<-new[new>0L]
 stopifnot(identical(as.integer(old),new,num.eq=FALSE),identical(sum(old^3-old),sum(new^3-new),num.eq=FALSE));count<-count+1L
}
cat('PASS ordered tie counts and raw correction sums:',count,'\n')
count<-0L
for(kind in c('continuous','ordered')) {
 set.seed(939);d<-matrix(if(kind=='continuous')rnorm(20000*6)else sample(1:5,20000*6,TRUE),20000,6)
 for(pair in combn(1:6,2,simplify=FALSE)) {
  x<-d[,pair[1]];y<-d[,pair[2]]
  invoke<-function(f)capture(function()f(y,x,paired=TRUE,exact=FALSE))
  stopifnot(identical(invoke(reference),invoke(candidate),num.eq=FALSE));count<-count+1L
 }
}
cat('PASS large raw paired htest, conditions, stdout and RNG:',count,'\n')
old<-new.env(parent=.GlobalEnv);new<-new.env(parent=.GlobalEnv)
for(e in list(old,new))for(file in c('R/analysis_paired_rm.R','R/analysis_nonparametric_paired.R'))sys.source(file,e)
old$paired_rm_wilcox_engine<-function()stats::wilcox.test
count<-0L
for(kind in c('continuous','ordered'))for(scenario in c('ordinary','missing','unchanged','late'))for(adjustment in c('holm','bonferroni')) {
 set.seed(939);d<-as.data.frame(matrix(if(kind=='continuous')rnorm(80*4)else sample(1:5,80*4,TRUE),80,4));names(d)<-paste0('t',1:4)
 if(scenario=='missing')d[1:3,2]<-NA
 if(scenario %in% c('unchanged','late'))d[]<-lapply(d,function(x)d[[1]])
 if(scenario=='late')d[80,4]<-d[80,4]+1
 info<-data.frame(name=names(d),measurement=kind)
 for(name in c('prepare_paired_rm_single_result','prepare_nonparametric_paired_rm_single_result')) {
  invoke<-function(e)capture(function()e[[name]](d,names(d),variable_info=info,options=list(assumption_check=TRUE,median_iqr=TRUE,posthoc_adjustment=adjustment)))
  a<-invoke(old);b<-invoke(new);stopifnot(identical(a,b,num.eq=FALSE))
  if(scenario %in% c('ordinary','missing'))stopifnot(is.null(a$value$error))
  count<-count+1L
 }
}
stopifnot(identical(deparse(get('wilcox.test.default',asNamespace('stats'))),saved),identical(deparse(paired_rm_build_wilcox_engine()),deparse(candidate)))
cat('PASS full repeated analysis, conditions, stdout and RNG:',count,'\nPASS fallback, warm engine and unmodified stats namespace\n')
