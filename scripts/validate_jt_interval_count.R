.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
capture<-function(f) {
 set.seed(718);conditions<-list()
 output<-capture.output(value<-tryCatch(withCallingHandlers(f(),warning=function(e){conditions[[length(conditions)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleWarning')},message=function(e){conditions[[length(conditions)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleMessage')}),error=function(e)list(error=class(e),message=conditionMessage(e))))
 list(value=value,conditions=conditions,output=output,rng=.Random.seed)
}
old<-new.env(parent=.GlobalEnv);new<-new.env(parent=.GlobalEnv)
for(e in list(old,new))sys.source('R/analysis_ttest_anova.R',e)
code<-as.list(body(old$ttest_jt_test));remove<-vapply(code,function(x)is.call(x)&&identical(x[[1]],as.name('<-'))&&identical(x[[2]],as.name('sorted_values')),logical(1))
stopifnot(sum(remove)==1L);code<-as.call(code[!remove]);replacements<-0L
restore<-function(node) {
 if(is.call(node)&&identical(node[[1]],as.name('if'))&&identical(node[[2]],quote(!is.null(sorted_values)))) {
  replacements<<-replacements+1L;return(node[[4]])
 }
 if(is.call(node))for(i in seq_along(node))if(!identical(node[[i]],quote(expr=)))node[i]<-list(restore(node[[i]]))
 node
}
body(old$ttest_jt_test)<-restore(code);stopifnot(replacements==1L)
make_values<-function(n,kind) {
 x<-rnorm(n)
 if(kind=='tied')x<-round(x)
 if(kind=='constant')x[]<-1
 if(kind=='near')x<-1+seq_len(n)*.Machine$double.eps
 if(kind=='near_large')x<-1e12+seq_len(n)*.0001220703125
 if(kind=='subnormal')x<-seq_len(n)*(.Machine$double.xmin*.Machine$double.eps)
 if(kind=='extreme')x<-rep(c(-.Machine$double.xmax,.Machine$double.xmax,0,-0),length.out=n)
 if(kind=='missing'&&n>0)x[1]<-NA_real_
 if(kind=='infinite')x<-rep(c(-Inf,Inf,0,1),length.out=n)
 x
}
count<-0L
for(k in c(2L,4L,8L))for(n in c(0L,1L,4L,20L,100L,1000L))for(kind in c('normal','tied','constant','near','near_large','subnormal','extreme','missing','infinite'))for(alternative in c('two.sided','less','greater')) {
 set.seed(939);x<-make_values(n,kind);g<-rep(seq_len(k),length.out=n)
 invoke<-function(e)capture(function()e$ttest_jt_test(x,g,alternative))
 stopifnot(identical(invoke(old),invoke(new),num.eq=FALSE));count<-count+1L
}
cat('PASS raw JT statistic/z/p/r, diagnostics/stdout/RNG:',count,'\n')
count<-0L
for(k in c(2L,4L,8L))for(kind in c('normal','tied','near','constant','missing','infinite'))for(extra in c(FALSE,TRUE))for(measure in c('continuous','ordered')) {
 set.seed(939);g<-rep(seq_len(k),length.out=120L);x<-make_values(120L,kind)
 d<-data.frame(y=x,y2=rev(x),g=factor(g));info<-data.frame(name=names(d),measurement=c(measure,measure,'ordered'))
 invoke<-function(e)capture(function()e$prepare_ttest_anova_results(d,c('y','y2'),'g',variable_info=info,options=list(force_nonparametric=TRUE,post_hoc=TRUE,add_mean_sd=extra,effect_size=TRUE,trend_analysis=TRUE)))
 a<-invoke(old);b<-invoke(new);stopifnot(identical(a,b,num.eq=FALSE),is.null(a$value$error));count<-count+1L
}
cat('PASS full results/diagnostics/stdout/RNG:',count,'\n')
# Invalid alternative and unused/missing/blank group labels preserve the old path.
for(alternative in c('invalid','two.sided')) {
 x<-c(3,1,4,1,5,NA,9);g<-factor(c('10','2','10','','2','10',NA),levels=c('2','10','unused',''))
 invoke<-function(e)capture(function()e$ttest_jt_test(x,g,alternative))
 stopifnot(identical(invoke(old),invoke(new),num.eq=FALSE))
}
cat('PASS invalid alternative and group label boundaries\n')
