.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
reference <- function(values) {
  if (!is.data.frame(values) && !is.matrix(values)) return(FALSE)
  if (nrow(values) == 0 || ncol(values) < 2) return(FALSE)
  any(apply(as.matrix(values), 1, function(row) length(unique(row[!is.na(row)])) > 1))
}
capture <- function(f) {
  set.seed(718); conditions <- list()
  output <- capture.output(value <- tryCatch(withCallingHandlers(f(),
    warning=function(e) {conditions[[length(conditions)+1L]] <<- list(class(e),conditionMessage(e));invokeRestart('muffleWarning')},
    message=function(e) {conditions[[length(conditions)+1L]] <<- list(class(e),conditionMessage(e));invokeRestart('muffleMessage')}),
    error=function(e) list(error=class(e),message=conditionMessage(e))))
  list(value=value,conditions=conditions,output=output,rng=.Random.seed)
}
set.seed(939); fixtures <- list(NULL,1:3)
for(n in c(0L,1L,2L,100L)) for(k in c(1L,2L,3L,8L)) for(pool in list(c(-Inf,-1,-0,0,1,Inf),c(NA_real_,NaN,-1,0,1),1:5,c('a','b',NA),c(TRUE,FALSE,NA))) {
  m <- matrix(sample(pool,n*k,TRUE),n,k)
  fixtures <- c(fixtures,list(m,as.data.frame(m)))
}
late <- matrix(1,100,8);late[100,8] <- 2
fixtures <- c(fixtures,list(matrix(1,100,8),late,matrix(c(1,1,1,1+.Machine$double.eps),2),matrix(complex(real=c(1,2)),2,2),data.frame(x=factor(c('a','b')),y=factor(c('a','a'))),matrix(NA_real_,3,3),matrix(1,0,0),matrix(1,3,0)))
for(d in fixtures) stopifnot(identical(capture(function() reference(d)),capture(function() paired_rm_has_within_subject_change(d)),num.eq=FALSE))
cat('PASS boundary cases:',length(fixtures),'\n')
old <- new.env(parent=.GlobalEnv);new <- new.env(parent=.GlobalEnv)
for(e in list(old,new)) for(file in c('R/analysis_paired_rm.R','R/analysis_nonparametric_paired.R','R/analysis_mixed_rm_anova.R')) sys.source(file,e)
old$paired_rm_has_within_subject_change <- reference
count <- 0L
for(kind in c('continuous','ordered','binary')) for(scenario in c('ordinary','missing','unchanged','late')) {
  set.seed(939);d <- as.data.frame(matrix(if(kind=='continuous') rnorm(80*4) else sample(if(kind=='binary')0:1 else 1:5,80*4,TRUE),80,4));names(d)<-paste0('t',1:4)
  if(scenario=='missing') d[1:3,2] <- NA
  if(scenario %in% c('unchanged','late')) d[] <- lapply(d,function(x)d[[1]])
  if(scenario=='late') d[80,4] <- if(kind=='binary') 1-d[80,4] else d[80,4]+1
  info <- data.frame(name=names(d),measurement=kind)
  for(name in c('prepare_paired_rm_single_result','prepare_nonparametric_paired_rm_single_result')) {
    invoke <- function(e) capture(function()e[[name]](d,names(d),variable_info=info,options=list(assumption_check=TRUE,median_iqr=TRUE)))
    a<-invoke(old);b<-invoke(new)
    stopifnot(identical(a,b,num.eq=FALSE))
    if(scenario %in% c('ordinary','missing')) stopifnot(is.null(a$value$error))
    count<-count+1L
  }
}
cat('PASS full paired/repeated results, diagnostics, stdout and RNG:',count,'\n')
for(scenario in c('ordinary','missing','unchanged')) {
 set.seed(939);d<-as.data.frame(matrix(rnorm(80*4),80,4));names(d)<-paste0('t',1:4);vars<-names(d)
 if(scenario=='missing')d[1,2]<-NA
 if(scenario=='unchanged')d[]<-lapply(d,function(x)d[[1]])
 d$group<-rep(c('a','b'),each=40)
 invoke<-function(e)capture(function()e$prepare_mixed_rm_anova_results(d,'group',vars))
 a<-invoke(old);b<-invoke(new)
 stopifnot(identical(a,b,num.eq=FALSE))
 if(scenario!='unchanged')stopifnot(is.null(a$value$error))
}
cat('PASS full mixed repeated results, diagnostics, stdout and RNG: 3\n')
