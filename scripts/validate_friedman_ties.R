.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
capture <- function(f) {
  set.seed(718); conditions <- list()
  output <- capture.output(value <- tryCatch(withCallingHandlers(f(),
    warning=function(e) {conditions[[length(conditions)+1L]] <<- list(class(e),conditionMessage(e));invokeRestart('muffleWarning')},
    message=function(e) {conditions[[length(conditions)+1L]] <<- list(class(e),conditionMessage(e));invokeRestart('muffleMessage')}),
    error=function(e) list(error=class(e),message=conditionMessage(e))))
  list(value=value,conditions=conditions,output=output,rng=.Random.seed)
}
original <- get('friedman.test.default',asNamespace('stats'))
saved_body <- body(original)
engine <- paired_rm_build_friedman_engine()
stopifnot(!identical(body(engine),saved_body))
altered <- original;body(altered)<-quote(stop('changed implementation'))
stopifnot(identical(paired_rm_build_friedman_engine(altered),stats::friedman.test))
set.seed(939);fixtures<-list()
for(n in c(0L,1L,2L,5L,100L))for(k in c(1L,2L,3L,6L,20L))for(kind in c('continuous','tied','constant','missing','infinite')) {
  y<-matrix(rnorm(n*k),n,k)
  if(kind=='tied')y<-round(y)
  if(kind=='constant')y[]<-1
  if(kind=='missing'&&length(y))y[1]<-NA_real_
  if(kind=='infinite'&&length(y))y[1]<-Inf
  fixtures[[length(fixtures)+1L]]<-y
}
fixtures<-c(fixtures,list(matrix(c(NA,NaN),3,4),matrix(c(-Inf,Inf,0),3,3),matrix(c(-0,0,1,1+.Machine$double.eps),4,4),matrix(1:12,4,3),matrix(1,0,0),matrix(1,3,0),matrix(c(TRUE,FALSE),4,3),matrix(letters[1:3],4,3),matrix(complex(real=1:3),4,3),structure(matrix(1:12,4,3),dimnames=list(letters[1:4],LETTERS[1:3]))))
for(y in fixtures) {
  a<-capture(function()stats::friedman.test(y));b<-capture(function()paired_rm_friedman_test(y))
  stopifnot(identical(a,b,num.eq=FALSE))
}
cat('PASS raw htest/conditions/stdout/RNG:',length(fixtures),'\n')
stopifnot(identical(deparse(paired_rm_build_friedman_engine()),deparse(engine)))
count<-0L
for(n in c(2L,20L,100L))for(k in c(3L,6L,20L,100L))for(tied in c(FALSE,TRUE)) {
  set.seed(939);y<-matrix(rnorm(n*k),n,k);if(tied)y<-round(y)
  r<-t(apply(y,1L,rank));old<-tapply(c(r),row(r),table)
  new<-lapply(seq_len(nrow(r)),function(i){z<-tabulate(2*r[i,],nbins=2*ncol(r));z[z>0L]})
  # Compare frequency order and storage type, omitting only table/list metadata.
  stopifnot(identical(unname(lapply(old,as.integer)),new,num.eq=FALSE))
  stopifnot(identical(sum(unlist(lapply(old,function(u)u^3-u))),sum(unlist(lapply(new,function(u)u^3-u))),num.eq=FALSE))
  count<-count+1L
}
cat('PASS ordered tie counts and raw correction sums:',count,'\n')
old<-new.env(parent=.GlobalEnv);new<-new.env(parent=.GlobalEnv)
for(e in list(old,new))for(file in c('R/analysis_paired_rm.R','R/analysis_nonparametric_paired.R'))sys.source(file,e)
old$paired_rm_friedman_test<-stats::friedman.test
count<-0L
for(kind in c('continuous','ordered','binary'))for(scenario in c('ordinary','missing','unchanged','late'))for(assumption in c(FALSE,TRUE)) {
  set.seed(939);d<-as.data.frame(matrix(if(kind=='continuous')rnorm(80*4)else sample(if(kind=='binary')0:1 else 1:5,80*4,TRUE),80,4));names(d)<-paste0('t',1:4)
  if(scenario=='missing')d[1:3,2]<-NA
  if(scenario %in% c('unchanged','late'))d[]<-lapply(d,function(x)d[[1]])
  if(scenario=='late')d[80,4]<-if(kind=='binary')1-d[80,4]else d[80,4]+1
  info<-data.frame(name=names(d),measurement=kind)
  for(name in c('prepare_paired_rm_single_result','prepare_nonparametric_paired_rm_single_result')) {
    invoke<-function(e)capture(function()e[[name]](d,names(d),variable_info=info,options=list(assumption_check=assumption,median_iqr=TRUE,posthoc_adjustment='holm')))
    a<-invoke(old);b<-invoke(new);stopifnot(identical(a,b,num.eq=FALSE))
    if(scenario %in% c('ordinary','missing'))stopifnot(is.null(a$value$error))
    count<-count+1L
  }
}
stopifnot(identical(body(get('friedman.test.default',asNamespace('stats'))),saved_body))
cat('PASS full analysis/conditions/stdout/RNG:',count,'\nPASS fallback and unmodified stats namespace\n')
