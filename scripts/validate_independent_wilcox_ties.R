.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
capture<-function(f) {
 set.seed(718);conditions<-list()
 output<-capture.output(value<-tryCatch(withCallingHandlers(f(),warning=function(e){conditions[[length(conditions)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleWarning')},message=function(e){conditions[[length(conditions)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleMessage')}),error=function(e)list(error=class(e),message=conditionMessage(e))))
 list(value=value,conditions=conditions,output=output,rng=.Random.seed)
}
reference<-get('wilcox.test.default',asNamespace('stats'));saved<-deparse(reference)
pair_reference<-stats::pairwise.wilcox.test;pair_saved<-deparse(pair_reference)
pair_candidate<-ttest_pairwise_wilcox_engine();stopifnot(!identical(environment(pair_candidate),environment(pair_reference)))
candidate<-environment(pair_candidate)$wilcox.test
altered<-reference;body(altered)<-quote(stop('changed implementation'))
altered_pair<-pair_reference;body(altered_pair)<-quote(stop('changed implementation'))
stopifnot(identical(ttest_build_pairwise_wilcox_engine(wilcox_reference=altered),pair_reference),identical(ttest_build_pairwise_wilcox_engine(pairwise_reference=altered_pair),pair_reference))
make_values<-function(n,kind) {
 x<-rnorm(n)
 if(kind=='tied')x<-round(x)
 if(kind=='constant')x[]<-1
 if(kind=='near')x<-1+seq_len(n)*.Machine$double.eps
 if(kind=='missing'&&n>0)x[1]<-NA_real_
 if(kind=='infinite'&&n>0)x[1]<-Inf
 x
}
count<-0L
for(n in c(0L,1L,2L,10L,100L,5000L))for(kind in c('normal','tied','constant','near','missing','infinite'))for(alternative in c('two.sided','less','greater'))for(correct in c(FALSE,TRUE)) {
 set.seed(939);x<-make_values(n,kind);y<-make_values(n+1L,kind)
 invoke<-function(f)capture(function()f(x,y,paired=FALSE,exact=FALSE,alternative=alternative,correct=correct))
 stopifnot(identical(invoke(reference),invoke(candidate),num.eq=FALSE));count<-count+1L
}
for(tied in c(FALSE,TRUE))for(paired in c(FALSE,TRUE))for(exact in c(FALSE,TRUE))for(alternative in c('two.sided','less','greater'))for(correct in c(FALSE,TRUE)) {
 set.seed(939);x<-rnorm(8);y<-rnorm(8);if(tied){x<-round(x);y<-round(y)}
 invoke<-function(f)capture(function()f(x,y,paired=paired,exact=exact,alternative=alternative,correct=correct,conf.int=TRUE,digits.rank=5))
 stopifnot(identical(invoke(reference),invoke(candidate),num.eq=FALSE));count<-count+1L
}
cat('PASS raw htest/diagnostics/stdout/RNG:',count,'\n')
count<-0L
for(n in c(1L,100L,20000L))for(kind in c('normal','tied','constant','near','infinite')) {
 set.seed(939);r<-rank(make_values(n,kind));a<-table(r);b<-tabulate(2*r,nbins=2*length(r));b<-b[b>0L]
 stopifnot(identical(as.integer(a),b,num.eq=FALSE),identical(sum(a^3-a),sum(b^3-b),num.eq=FALSE));count<-count+1L
}
cat('PASS ordered tie counts and raw correction sums:',count,'\n')
count<-0L
for(k in c(2L,4L,8L))for(kind in c('normal','tied','constant','near','missing','infinite'))for(method in c('holm','bonferroni')) {
 set.seed(939);x<-make_values(120L,kind);g<-factor(rep(seq_len(k),length.out=120L),levels=seq_len(k+1L))
 invoke<-function(f)capture(function()f(x,g,p.adjust.method=method,exact=FALSE))
 stopifnot(identical(invoke(pair_reference),invoke(pair_candidate),num.eq=FALSE));count<-count+1L
}
cat('PASS raw pairwise htest/diagnostics/stdout/RNG:',count,'\n')
old<-new.env(parent=.GlobalEnv);new<-new.env(parent=.GlobalEnv)
for(e in list(old,new))sys.source('R/analysis_ttest_anova.R',e)
old$ttest_pairwise_wilcox_engine<-function()stats::pairwise.wilcox.test
count<-0L
for(k in c(2L,4L,8L))for(kind in c('normal','tied','constant','near','missing','infinite'))for(extra in c(FALSE,TRUE))for(method in c('holm','bonferroni')) {
 set.seed(939);g<-rep(seq_len(k),length.out=120L);x<-make_values(120L,kind)
 if(kind %in% c('normal','tied'))x<-x+g*.3
 d<-data.frame(y=x,y2=rev(x),g=factor(g));info<-data.frame(name=names(d),measurement=c('continuous','continuous','category'))
 invoke<-function(e)capture(function()e$prepare_ttest_anova_results(d,c('y','y2'),'g',variable_info=info,options=list(force_nonparametric=TRUE,post_hoc=TRUE,add_mean_sd=extra,effect_size=TRUE,nonparametric_post_hoc_method=method)))
 a<-invoke(old);b<-invoke(new);stopifnot(identical(a,b,num.eq=FALSE),is.null(a$value$error));count<-count+1L
}
stopifnot(identical(deparse(get('wilcox.test.default',asNamespace('stats'))),saved),identical(deparse(stats::pairwise.wilcox.test),pair_saved),identical(deparse(environment(ttest_build_pairwise_wilcox_engine())$wilcox.test),deparse(candidate)))
cat('PASS full results/diagnostics/stdout/RNG:',count,'\nPASS fallback, warm engine, unchanged stats namespace\n')
