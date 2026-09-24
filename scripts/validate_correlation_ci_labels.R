source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
reference<-correlation_pair_rows_and_matrices
restore<-function(expr) {
 if(is.call(expr)&&identical(expr[[1]],as.name('correlation_ci_matrix_from_pairs'))) {
  expr$prepared_ci<-NULL;return(expr)
 }
 if(is.call(expr))for(i in seq_along(expr))expr[i]<-list(restore(expr[[i]]))
 expr
}
body(reference)<-restore(body(reference))
args<-commandArgs(TRUE)
if(length(args)){e<-new.env(parent=.GlobalEnv);sys.source(args[[1]],e);reference<-e$correlation_pair_rows_and_matrices;environment(reference)<-.GlobalEnv}
capture<-function(expr) {
 diagnostics<-character();set.seed(741)
 value<-withCallingHandlers(tryCatch(force(expr),error=function(e)list(error=conditionMessage(e))),
  warning=function(w){diagnostics<<-c(diagnostics,paste('warning',conditionMessage(w)));invokeRestart('muffleWarning')},
  message=function(m){diagnostics<<-c(diagnostics,paste('message',conditionMessage(m)));invokeRestart('muffleMessage')})
 list(value=value,diagnostics=diagnostics,rng=.Random.seed)
}
variables<-c('x','y','z');labels<-setNames(c('X','Y','Z'),variables)
pair<-function(x,y,ci)list(x_name=x,y_name=y,result=list(type1='Continuous',type2='Continuous',n=30L,label='Pearson',coefficient=.3,p=.04,ci=ci,reason='reason'))
cases<-list(list(),list(pair('x','y',c(-.1,.5))),list(pair('x','y',c(NA,NA)),pair('y','z',c(-Inf,Inf))),
 list(pair('x','y',c(-0,.000001)),pair('x','z',c(-.5,.3)),pair('x','y',c(-.2,.2))),list(pair('x','y',numeric())))
run<-function(fn,pairs)capture(fn(NULL,variables,labels,NULL,pairs))
checks<-0L
for(digits in 0:6)for(pformat in c('apa','leading_zero'))for(pairs in cases) {
 options(statedu.output_decimal_digits=digits,statedu.p_value_format=pformat)
 stopifnot(identical(run(reference,pairs),run(correlation_pair_rows_and_matrices,pairs),num.eq=FALSE));checks<-checks+1L
}
original_format<-format_decimal3
for(kind in c('warning','message')) {
 format_decimal3<-local({mode<-kind;function(...) {
  if(mode=='warning')warning('format diagnostic',call.=FALSE)else message('format diagnostic')
  original_format(...)
 }})
 a<-run(reference,cases[[2]]);b<-run(correlation_pair_rows_and_matrices,cases[[2]])
 stopifnot(length(a$diagnostics)>0L,identical(a,b,num.eq=FALSE));checks<-checks+1L
}
format_decimal3<-original_format
cat('PASS:',checks,'exact CI formatting/matrix/diagnostic/RNG comparisons.\n')
