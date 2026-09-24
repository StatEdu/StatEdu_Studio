.libPaths(R.home('library'));source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
base<-new.env(parent=.GlobalEnv);current<-new.env(parent=.GlobalEnv)
current$correlation_eta_result<-correlation_eta_result
base$correlation_eta_result<-correlation_eta_result
replacements<-0L
restore<-function(node) {
 if(identical(node,quote(stats::setNames(tabulate(groups,nbins=nlevels(groups)),levels(groups))))) {replacements<<-replacements+1L;return(quote(table(groups)))}
 if(is.call(node))for(i in seq_along(node))if(!identical(node[[i]],quote(expr=)))node[i]<-list(restore(node[[i]]))
 node
}
body(base$correlation_eta_result)<-restore(body(base$correlation_eta_result));stopifnot(replacements==1L)
capture<-function(f) {
 set.seed(99);conditions<-list()
 stdout<-capture.output(value<-tryCatch(withCallingHandlers(f(),warning=function(w){conditions[[length(conditions)+1L]]<<-list(class(w),conditionMessage(w));invokeRestart('muffleWarning')},message=function(m){conditions[[length(conditions)+1L]]<<-list(class(m),conditionMessage(m));invokeRestart('muffleMessage')}),error=function(e)list(error=class(e),message=conditionMessage(e))))
 list(value=value,conditions=conditions,stdout=stdout,rng=.Random.seed)
}
audit<-function(f,x,y) {
 scope<-new.env(parent=environment(f));scope$tables<-list()
 scope$summary<-function(object,...) {value<-base::summary(object,...);scope$tables[[length(scope$tables)+1L]]<-value;value}
 environment(f)<-scope
 capture(function(){value<-f(x,y);list(value=value,anova=scope$tables)})
}
state_function<-function(f) {
 replace_state<-function(node) {
  if(is.call(node)&&identical(node[[1]],as.name('<-'))&&identical(node[[2]],as.name('eta')))return(quote(return(list(counts=as.numeric(counts),count_names=names(counts),means=means,ss_total=ss_total,ss_between=ss_between))))
  if(is.call(node))for(i in seq_along(node))if(!identical(node[[i]],quote(expr=)))node[i]<-list(replace_state(node[[i]]))
  node
 }
 body(f)<-replace_state(body(f));f
}
state_base<-state_function(base$correlation_eta_result)
state_current<-state_function(current$correlation_eta_result)
count<-0L;tables<-0L
saved<-options('contrasts')
for(contrast in c('contr.treatment','contr.sum','contr.helmert'))for(k in c(2L,3L,6L))for(n in c(3L,12L,100L))for(kind in c('normal','near','tied','large','missing','infinite','constant','ordered')) {
 options(contrasts=c(contrast,'contr.poly'));set.seed(17)
 x<-rnorm(n);y<-factor(rep(seq_len(k),length.out=n),levels=c(seq_len(k),99L))
 if(kind=='near')x<-1+x*1e-15
 if(kind=='tied')x<-round(x)
 if(kind=='large')x<-x*1e100
 if(kind=='missing'){x[1]<-NA;y[n]<-NA}
 if(kind=='infinite')x[1]<-Inf
 if(kind=='constant')x[]<-1
 if(kind=='ordered')y<-ordered(y)
 a<-audit(base$correlation_eta_result,x,y);b<-audit(current$correlation_eta_result,x,y)
 stopifnot(identical(a,b,num.eq=FALSE));count<-count+1L
 stopifnot(identical(audit(state_base,x,y),audit(state_current,x,y),num.eq=FALSE))
 if(is.list(a$value)&&length(a$value$anova))tables<-tables+1L
}
options(saved)
cat('PASS',count,'exact result AND count/mean/sum-of-squares captures including',tables,'raw ANOVA summaries\n')
