source('R/utils.R',encoding='UTF-8')
source('R/analysis_survival.R',encoding='UTF-8')
reference <- survival_life_table
reference_body <- body(reference)
loop <- which(vapply(as.list(reference_body),function(x) is.call(x) && identical(x[[1L]],as.name('for')),logical(1)))
stopifnot(length(loop)==1L,identical(reference_body[[loop]][[4L]][[2L]][[2L]],as.name('current_groups')))
reference_body[[loop]][[4L]][[2L]] <- quote(NULL)
body(reference) <- reference_body
original <- paste(deparse(body(reference),width.cutoff=500L),collapse='\n')
stopifnot(grepl('current_groups == group_value',original,fixed=TRUE))
body(reference) <- parse(text=sub('current_groups == group_value','as.factor(data[[group]]) == group_value',original,fixed=TRUE))[[1L]]
args <- commandArgs(TRUE)
if(length(args)) {
  previous <- new.env(parent=.GlobalEnv);sys.source(args[[1L]],previous)
  reference <- previous$survival_life_table
}
capture_group <- function(expr) {
  warnings <- messages <- character()
  value <- withCallingHandlers(tryCatch(force(expr),error=function(e) list(error=conditionMessage(e))),
    warning=function(w) {warnings <<- c(warnings,conditionMessage(w));invokeRestart('muffleWarning')},
    message=function(m) {messages <<- c(messages,conditionMessage(m));invokeRestart('muffleMessage')})
  list(value=value,warnings=warnings,messages=messages,rng=.Random.seed)
}
set.seed(714)
d <- data.frame(time=sample(0:20,150,TRUE),event=sample(c(TRUE,FALSE),150,TRUE),group=rep(c('A','B','C'),50))
datasets <- list(d)
x<-d;x$group<-factor(x$group,levels=c('C','B','A','unused'));datasets[[2]]<-x
x<-d;x$group<-ordered(x$group,levels=c('C','B','A'));datasets[[3]]<-x
x<-d;x$group<-rep(1:3,50);datasets[[4]]<-x
x<-d;x$group[1:4]<-NA;datasets[[5]]<-x
x<-d;x$group[x$group=='A']<-'All';datasets[[6]]<-x
x<-d;x$event[1:3]<-NA;datasets[[7]]<-x
x<-d;x$time[1:3]<-NA;datasets[[8]]<-x
x<-d;x$time[]<-0;datasets[[9]]<-x
datasets[[10]]<-d[FALSE,]
x<-d;x$group[]<-NA_character_;datasets[[11]]<-x
x<-d;x$event[]<-FALSE;datasets[[12]]<-x
count <- 0L
for(data in datasets) for(group in c('group','')) for(breaks in list(numeric(0),c(0,1,5,10,20),c(NA,Inf,-1,5,5))) {
  before<-capture_group(reference(data,'time','event',group,breaks))
  after<-capture_group(survival_life_table(data,'time','event',group,breaks))
  stopifnot(identical(before,after,num.eq=FALSE));count<-count+1L
}
local({
  calls <- 0L
  fun <- survival_life_table
  environment(fun) <- list2env(list(as.factor=function(x) {calls<<-calls+1L;base::as.factor(x)}),parent=.GlobalEnv)
  fun(d,'time','event','group',c(5,10))
  stopifnot(calls==1L)
  fun(d,'time','event','',c(5,10))
  stopifnot(calls==1L)
})
cat('PASS:',count,'exact life-table/condition/RNG comparisons; one conversion per grouped call.\n')
local({
  noisy <- function(fun) {
    environment(fun) <- list2env(list(as.factor=function(x) {
      warning('factor warning',call.=FALSE);message('factor message');base::as.factor(x)
    }),parent=.GlobalEnv)
    capture_group(fun(d,'time','event','group',c(5,10)))
  }
  before<-noisy(reference);after<-noisy(survival_life_table)
  stopifnot(identical(before,after,num.eq=FALSE),length(after$warnings)==4L,length(after$messages)==4L)
})
