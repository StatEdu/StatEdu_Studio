.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
original<-function(data,variables) {
 frame<-data[,variables,drop=FALSE]
 matrix<-as.data.frame(lapply(frame,function(values)suppressWarnings(as.numeric(as.character(values)))),check.names=FALSE)
 names(matrix)<-variables
 matrix
}
candidate<-factor_analysis_numeric_matrix
stopifnot(identical(body(pca_numeric_matrix),body(candidate)))
check<-function(fn,d){set.seed(719);conditions<-list();value<-tryCatch(withCallingHandlers(fn(d,names(d)),warning=function(w){conditions[[length(conditions)+1L]]<<-list(class(w),conditionMessage(w));invokeRestart('muffleWarning')},message=function(m){conditions[[length(conditions)+1L]]<<-list(class(m),conditionMessage(m));invokeRestart('muffleMessage')}),error=function(e)list(class(e),conditionMessage(e)));list(value=value,conditions=conditions,rng=.Random.seed)}
as.character.input_test<-function(x,...) { message('custom conversion');rep('7',length(x)) }
set.seed(925)
cases<-list(integer(),1L,c(NA_integer_,0L,1L,-1L,2147483647L,-2147483647L),rep(NA_integer_,12),seq_len(10000),sample.int(.Machine$integer.max,10000),-sample.int(.Machine$integer.max,10000),setNames(1:3,letters[1:3]),structure(1:3,label='label'),factor(c('30','10',NA)),ordered(c('30','10',NA)),structure(1:3,class='Date'),structure(1:3,class='input_test'),c(NA,NaN,Inf,-Inf,-0,pi),c('1','2.5','bad',NA),c(TRUE,FALSE,NA))
count<-0L
for(v in cases){d<-structure(list(x=v),class='data.frame',row.names=.set_row_names(length(v)));stopifnot(identical(check(original,d),check(candidate,d),num.eq=FALSE));count<-count+1L}
for(analysis in c('pca','fa')) for(matrix_type in if(analysis=='pca')c('correlation','covariance','polychoric','fallback')else c('pearson','polychoric')) for(rotation in c('none','varimax','oblimin')) for(missing in c(FALSE,TRUE)) {
 set.seed(926);n<-240L;p<-6L;latent<-rnorm(n);d<-as.data.frame(matrix(rnorm(n*p),n,p)+latent)
 d[]<-lapply(d,function(x)as.integer(cut(x,c(-Inf,-1,-.3,.3,1,Inf))))
 if(missing)d[seq(1,n,17),1]<-NA_integer_
 info<-data.frame(name=names(d),measurement=if(matrix_type=='polychoric')'ordered'else 'continuous')
 opts<-list(matrix_type=if(matrix_type=='fallback')'polychoric'else matrix_type,criterion='fixed',n_components=2L,n_factors=1L,rotation=rotation,normality=FALSE,method='pa',save_component_scores=TRUE,save_factor_scores=TRUE)
 run<-function(fn){factor_analysis_numeric_matrix<<-pca_numeric_matrix<<-fn;check(function(d,vars)if(analysis=='pca')prepare_pca_results(d,vars,variable_info=info,options=opts)else prepare_factor_analysis_results(d,vars,variable_info=info,options=opts),d)}
 a<-run(original);b<-run(candidate);stopifnot(identical(a$value$type,if(analysis=="pca")"pca"else "factor_analysis"),identical(a,b,num.eq=FALSE));count<-count+1L
}
cat('PASS:',count,'input and whole-analysis comparisons\n')

