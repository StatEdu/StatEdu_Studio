.libPaths(R.home('library'));source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
reference<-survival_weighted_rank_test;replacements<-0L
restore<-function(node) {
 if(is.call(node)&&identical(node[[1]],as.name('if'))&&identical(node[[2]],quote(k >= 20L))) {
  node[[4]]<-quote(for(i in seq_len(k))for(j in seq_len(k)) {
   value<-if(i==j)d_total*(n_total-d_total)*n_group[[i]]*(n_total-n_group[[i]])/(n_total^2*(n_total-1)) else -d_total*(n_total-d_total)*n_group[[i]]*n_group[[j]]/(n_total^2*(n_total-1))
   variance[i,j]<-variance[i,j]+weight^2*value
  })
  replacements<<-replacements+1L;return(node)
 }
 if(is.call(node))for(i in seq_along(node))if(!identical(node[[i]],quote(expr=)))node[i]<-list(restore(node[[i]]))
 node
}
body(reference)<-restore(body(reference));stopifnot(replacements==1L)
state_function<-function(f) {
 replace<-function(node) {
  if(identical(node,quote(keep <- seq_len(k - 1L))))return(quote(return(list(variance=variance,score=observed_minus_expected))))
  if(is.call(node))for(i in seq_along(node))if(!identical(node[[i]],quote(expr=)))node[i]<-list(replace(node[[i]]))
  node
 }
 body(f)<-replace(body(f));f
}
capture<-function(f) {
 set.seed(718);conditions<-list()
 output<-capture.output(value<-tryCatch(withCallingHandlers(f(),warning=function(e){conditions[[length(conditions)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleWarning')},message=function(e){conditions[[length(conditions)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleMessage')}),error=function(e)list(error=class(e),message=conditionMessage(e))))
 list(value=value,conditions=conditions,output=output,rng=.Random.seed)
}
count<-0L;diagnostics<-0L
for(k in c(2L,4L,19L,20L))for(n in c(100L,100000L))for(method in c('logrank','breslow','tarone_ware')) {
 d<-data.frame(time=rep(c(1,2),each=n/2),event=rep(c(TRUE,FALSE),each=n/2),g=factor(rep(seq_len(k),length.out=n)))
 for(state in c(FALSE,TRUE)) {
  a<-if(state)state_function(reference)else reference
  b<-if(state)state_function(survival_weighted_rank_test)else survival_weighted_rank_test
  invoke<-function(f)capture(function()f(d,'time','event','g',method))
  x<-invoke(a);y<-invoke(b);stopifnot(identical(x,y,num.eq=FALSE));diagnostics<-diagnostics+length(x$conditions);count<-count+1L
 }
}
cat('PASS large-tie result/score/variance/stdout/diagnostics/RNG:',count,'; reference diagnostics:',diagnostics,'\n')
