.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
# Restore the pre-change suitability branch as the independent reference path.
original<-prepare_pca_results;candidate<-prepare_pca_results
replacements<-0L
restore<-function(node) {
 if(is.call(node)&&identical(node[[1L]],as.name('<-'))&&identical(node[[2L]],as.name('suitability_corr'))) {
  stopifnot(identical(node[[3L]][[2L]],quote(!identical(matrix_type, 'covariance'))))
  node[[3L]][[2L]]<-quote(identical(matrix_type, 'polychoric'))
  replacements<<-replacements+1L
  return(node)
 }
 if(is.call(node))for(i in seq_along(node))node[i]<-list(restore(node[[i]]))
 node
}
body(original)<-restore(body(original));stopifnot(replacements==1L)
capture<-function(fn,d,info,opts){set.seed(718);conditions<-list();value<-withCallingHandlers(fn(d,names(d),variable_info=info,options=opts),warning=function(w){conditions[[length(conditions)+1L]]<<-list(class(w),conditionMessage(w));invokeRestart('muffleWarning')},message=function(m){conditions[[length(conditions)+1L]]<<-list(class(m),conditionMessage(m));invokeRestart('muffleMessage')});list(value=value,conditions=conditions,rng=.Random.seed)}
count<-0L
for(kind in c('correlation','covariance','polychoric','mixed_fallback')) for(rotation in c('none','varimax','oblimin')) for(missing in c(FALSE,TRUE)) {
 set.seed(921);n<-240L;p<-6L;d<-as.data.frame(matrix(rnorm(n*p),n,p))
 if(kind=='polychoric')d[]<-lapply(d,function(x)as.integer(cut(x,breaks=c(-Inf,-.8,-.2,.2,.8,Inf))))
 if(missing)d[seq(1,n,13),1]<-NA_real_
 d$constant<-1
 info<-data.frame(name=names(d),measurement=if(kind=='polychoric')'ordered'else 'continuous')
 opts<-list(matrix_type=if(kind=='mixed_fallback')'polychoric'else kind,criterion='fixed',n_components=2L,rotation=rotation,save_component_scores=TRUE)
 a<-capture(original,d,info,opts);b<-capture(candidate,d,info,opts)
 stopifnot(identical(a,b,num.eq=FALSE));count<-count+1L
}
cat('PASS:',count,'whole PCA comparisons\n')

