.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
ae<-new.env(parent=.GlobalEnv);be<-new.env(parent=.GlobalEnv)
for(env in list(ae,be)){
 fn<-factor_analysis_subfactor_reliability;environment(fn)<-env
 env$factor_analysis_subfactor_reliability<-fn
}
# Force recomputation in the reference; archived tests also use pre-change source.
replacements<-0L
restore<-function(node){
 if(is.call(node)&&identical(node[[1]],as.name('<-'))&&identical(node[[2]],as.name('total_reusable'))){node[[3]]<-FALSE;replacements<<-replacements+1L;return(node)}
 if(is.call(node))for(i in seq_along(node))node[i]<-list(restore(node[[i]]))
 node
}
body(ae$factor_analysis_subfactor_reliability)<-restore(body(ae$factor_analysis_subfactor_reliability));stopifnot(replacements==1L)
base_reliability<-prepare_reliability_results
run<-function(env,result,signal){set.seed(720);calls<-0L;conditions<-list()
 env$prepare_reliability_results<-function(...) {
  calls<<-calls+1L
  if(signal=='warning')warning('test warning',call.=FALSE)
  if(signal=='message')message('test message')
  if(signal=='rng')runif(1)
  if(signal=='error')stop('test error',call.=FALSE)
  base_reliability(...)
 }
 output<-capture.output(value<-withCallingHandlers(env$factor_analysis_subfactor_reliability(result),warning=function(w){conditions[[length(conditions)+1L]]<<-list(class(w),conditionMessage(w));invokeRestart('muffleWarning')},message=function(m){conditions[[length(conditions)+1L]]<<-list(class(m),conditionMessage(m));invokeRestart('muffleMessage')}))
 list(snapshot=list(value=value,conditions=conditions,output=output,rng=.Random.seed),calls=calls)
}
count<-0L
for(measurement in c('continuous','ordered'))for(shape in c('single','split','reordered','disabled'))for(signal in c('quiet','warning','message','rng','error')){
 set.seed(934);n<-120L;p<-4L;d<-as.data.frame(matrix(rnorm(n*p),n,p)+rnorm(n))
 if(measurement=='ordered')d[]<-lapply(d,function(x)as.integer(cut(x,c(-Inf,-1,-.3,.3,1,Inf))))
 loadings<-matrix(.8,p,if(shape=='split')2L else 1L,dimnames=list(names(d),if(shape=='split')c('F1','F2')else 'F1'))
 if(shape=='split'){loadings[1:2,2]<-.1;loadings[3:4,1]<-.1}
 if(shape=='reordered')loadings<-loadings[rev(seq_len(p)),,drop=FALSE]
 result<-list(matrix=d,variables=names(d),loadings=loadings,display_names=setNames(names(d),names(d)),variable_info=data.frame(name=names(d),measurement=measurement),labels=character(),category_table=NULL,options=list(subfactor_reliability=shape!='disabled'))
 a<-run(ae,result,signal);b<-run(be,result,signal)
 stopifnot(identical(a$snapshot,b$snapshot,num.eq=FALSE))
 if(shape=='single'&&signal=='quiet')stopifnot(a$calls==2L,b$calls==1L)else stopifnot(a$calls==b$calls)
 count<-count+1L
}
cat('PASS:',count,'result and reuse-guard comparisons\n')

