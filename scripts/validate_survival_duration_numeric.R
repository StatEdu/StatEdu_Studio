.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
statedu_apply_preferences(statedu_initial_preferences())
root<-'output/survival-duration-numeric-20260915';dir.create(root,recursive=TRUE,showWarnings=FALSE)
old<-new.env(parent=.GlobalEnv);new<-new.env(parent=.GlobalEnv)
sys.source('scripts/fixtures/survival_duration_reference.R',old)
new$survival_coerce_duration<-survival_coerce_duration
for(env in list(old,new))env$survival_coerce_duration<-compiler::cmpfun(env$survival_coerce_duration)
capture<-function(env,values){
 ds<-list();stdout<-capture.output(value<-tryCatch(withCallingHandlers(env$survival_coerce_duration(values),
 warning=function(e){ds[[length(ds)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleWarning')},
 message=function(e){ds[[length(ds)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleMessage')}),error=function(e)list(error=conditionMessage(e),class=class(e))))
 list(value=value,diagnostics=ds,stdout=stdout,rng=.Random.seed)
}
as.character.duration_probe<-function(x,...){warning('duration warning');message('duration message');runif(1);as.character(unclass(x))}
set.seed(20260915)
cases<-list(integer(),numeric(),1:10,c(NA_integer_,0L,-1L,.Machine$integer.max),
 c(0,-0,NA_real_,NaN,Inf,-Inf,1e-300,1e300,.Machine$double.xmin,.Machine$double.eps),
 rnorm(1000),runif(1000),NULL,TRUE,c(FALSE,NA),complex(),c(1+2i,NA_complex_),as.raw(0:5),
 c('',' ',' 1 ','invalid','NaN','Inf',NA_character_,'한글'),factor(c('1',' 2 ','bad',NA)),
 as.Date(c('2020-01-01',NA)),as.POSIXct(c('2020-01-01',NA),tz='UTC'),as.POSIXlt('2020-01-01',tz='UTC'),
 list(1,NULL,'bad'),setNames(c(1,NA),c('a','b')),matrix(c(1,NA,2,3),2),array(1:8,c(2,2,2)),
 structure(c(1,NA,2),class='duration_probe'),structure(c(1,NA),label='time'))
checks<-0L
for(values in cases){
 seed<-.Random.seed;a<-capture(old,values);.Random.seed<-seed;b<-capture(new,values)
 if(!identical(a,b,num.eq=FALSE)){saveRDS(list(values=values,a=a,b=b),file.path(root,'mismatch.rds'));stop('Duration mismatch')}
 checks<-checks+1L
}
for(i in 1:40){
 values<-if(i%%2L)sample(c(NA_integer_,-100:100),500,replace=TRUE)else rnorm(500)*10^sample(-200:200,1)
 values[seq.int(1,500,37)]<-NA
 seed<-.Random.seed;a<-capture(old,values);.Random.seed<-seed;b<-capture(new,values)
 stopifnot(identical(a,b,num.eq=FALSE));checks<-checks+1L
}
cat('PASS:',checks,'exact duration values, missing/invalid masks, attributes, diagnostics, stdout and RNG comparisons\n')
