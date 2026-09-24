.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
statedu_apply_preferences(statedu_initial_preferences())
root<-'output/correlation-display-reader-20260915';dir.create(root,recursive=TRUE,showWarnings=FALSE)
run<-as.integer(commandArgs(trailingOnly=TRUE)[1L]);if(is.na(run))run<-1L
old<-new.env(parent=.GlobalEnv);new<-new.env(parent=.GlobalEnv)
sys.source('scripts/fixtures/correlation_display_reader_reference.R',old);sys.source('R/analysis_correlation.R',new)
for(env in list(old,new))for(name in ls(env))if(is.function(env[[name]]))env[[name]]<-compiler::cmpfun(env[[name]])
set.seed(20260915);data<-as.data.frame(matrix(rnorm(1000L*20L),ncol=20L));names(data)<-paste0('v',seq_len(20L))
capture<-function(env,info){
 ds<-list();stdout<-capture.output(value<-withCallingHandlers(env$prepare_correlation_results(data,names(data),variable_info=info[seq_len(20L),],category_table=info[,c('name','var_label')],options=list(continuous_method='pearson',normality=TRUE)),
 warning=function(e){ds[[length(ds)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleWarning')},
 message=function(e){ds[[length(ds)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleMessage')}))
 list(value=value,diagnostics=ds,stdout=stdout,rng=.Random.seed)
}
times<-list()
for(n in c(20L,10000L)){
 info<-data.frame(name=paste0('v',seq_len(n)),measurement='continuous',var_label=paste0('Variable ',seq_len(n)))
 expected<-capture(old,info);stopifnot(identical(expected,capture(new,info),num.eq=FALSE))
 for(iteration in 1:3)for(version in if((iteration+run)%%2L)c('old','new')else c('new','old')){
  gc();start<-Sys.time();actual<-capture(get(version),info)
  elapsed<-as.numeric(difftime(Sys.time(),start,units='secs'))
  stopifnot(identical(expected,actual,num.eq=FALSE))
  times[[length(times)+1L]]<-data.frame(run,metadata_rows=n,iteration,version,seconds=elapsed)
 }
}
write.csv(do.call(rbind,times),file.path(root,paste0('times-',run,'.csv')),row.names=FALSE)
print(aggregate(seconds~metadata_rows+version,do.call(rbind,times),median))
