source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
reference<-survival_life_table
restore<-function(expr) {
  if(is.call(expr)&&identical(expr[[1]],as.name('list'))&&identical(names(expr)[2],'Strata')) {
    expr[[1]]<-as.name('data.frame')
    expr<-as.call(c(as.list(expr),list(check.names=FALSE,stringsAsFactors=FALSE)))
    return(expr)
  }
  if(identical(expr,quote(if (length(rows) == 0L) return(NULL))))return(quote(return(do.call(rbind,rows))))
  if(is.call(expr))for(i in seq_along(expr))expr[i]<-list(restore(expr[[i]]))
  expr
}
body(reference)<-restore(body(reference))
args<-commandArgs(TRUE)
if(length(args)){old<-new.env(parent=.GlobalEnv);sys.source(args[[1]],old);reference<-old$survival_life_table}
set.seed(991)
capture<-function(expr) {
  warnings<-messages<-character()
  value<-withCallingHandlers(tryCatch(force(expr),error=function(e)list(error=conditionMessage(e))),
    warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart('muffleWarning')},
    message=function(m){messages<<-c(messages,conditionMessage(m));invokeRestart('muffleMessage')})
  list(value=value,warnings=warnings,messages=messages,rng=.Random.seed)
}
d<-data.frame(time=sample(0:20,100,TRUE),event=sample(c(TRUE,FALSE),100,TRUE),group=rep(c('a','b'),50))
missing<-d;missing$group<-NA_character_
factor_data<-d;factor_data$group<-factor(factor_data$group,levels=c('b','a','unused'))
checks<-0L
for(data in list(d,d[FALSE,],d[1,,drop=FALSE],missing,factor_data))for(group in c('','group'))
  for(breaks in list(numeric(),c(1,5,10),c(NA,Inf,-1,5,5,30))) {
    stopifnot(identical(capture(reference(data,'time','event',group,breaks)),
      capture(survival_life_table(data,'time','event',group,breaks)),num.eq=FALSE))
    checks<-checks+1L
  }
cat('PASS:',checks,'exact life-table construction/condition/RNG comparisons.\n')
