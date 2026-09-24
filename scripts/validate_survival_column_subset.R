source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
reference<-survival_life_table
restore<-function(expr) {
  if(is.call(expr)&&identical(expr[[1]],as.name('<-'))&&identical(expr[[2]],as.name('group_source')))
    return(quote(group_source <- data))
  if(is.call(expr))for(i in seq_along(expr))expr[i]<-list(restore(expr[[i]]))
  expr
}
body(reference)<-restore(body(reference))
args<-commandArgs(TRUE)
if(length(args)){old<-new.env(parent=.GlobalEnv);sys.source(args[[1]],old);reference<-old$survival_life_table}
capture<-function(expr) {
  diagnostics<-character()
  set.seed(741)
  value<-withCallingHandlers(tryCatch(force(expr),error=function(e)list(error=conditionMessage(e))),
    warning=function(w){diagnostics<<-c(diagnostics,paste('warning',conditionMessage(w)));invokeRestart('muffleWarning')},
    message=function(m){diagnostics<<-c(diagnostics,paste('message',conditionMessage(m)));invokeRestart('muffleMessage')})
  list(value=value,diagnostics=diagnostics,rng=.Random.seed)
}
`[.subset_probe`<-function(x,...) {warning('unused column subset',call.=FALSE);NextMethod()}
d<-data.frame(time=c(0,5,10,15,20,25),event=c(TRUE,FALSE,TRUE,FALSE,NA,TRUE),group=c('a','b','a','b','a','b'))
cases<-list(d,d[FALSE,])
x<-d;x$group<-factor(x$group,levels=c('b','a','unused'));cases[[3]]<-x
x<-d;x$extra<-structure(1:6,class='subset_probe');cases[[4]]<-x
x<-d;x$extra<-as.Date('2020-01-01')+1:6;cases[[5]]<-x
x<-d;x$extra<-I(matrix(1:12,6));cases[[6]]<-x
x<-d;x$group[1]<-NA;cases[[7]]<-x
x<-d;x$group[]<-NA_character_;cases[[8]]<-x
x<-d;x$group[1]<-'All';cases[[9]]<-x
x<-d;x$extra<-ordered(letters[1:6]);cases[[10]]<-x
x<-d;class(x)<-c('custom_frame','data.frame');cases[[11]]<-x
x<-d;x$duplicate<-x$time;names(x)[4]<-'time';cases[[12]]<-x
checks<-0L
for(data in cases)for(group in c('','group'))for(breaks in list(numeric(),c(5,10,20))) {
  stopifnot(identical(capture(reference(data,'time','event',group,breaks)),
                      capture(survival_life_table(data,'time','event',group,breaks)),num.eq=FALSE))
  checks<-checks+1L
}
cat('PASS:',checks,'exact column-subset result/diagnostic/RNG comparisons.\n')
