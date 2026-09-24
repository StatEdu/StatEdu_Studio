.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
statedu_apply_preferences(statedu_initial_preferences())
root<-'output/correlation-reader-combined-20260915';dir.create(root,recursive=TRUE,showWarnings=FALSE)
run<-as.integer(commandArgs(trailingOnly=TRUE)[1L]);if(is.na(run))run<-1L
focused<-identical(commandArgs(trailingOnly=TRUE)[2L],'small-missing')
old<-new.env(parent=.GlobalEnv);new<-new.env(parent=.GlobalEnv)
sys.source('scripts/fixtures/correlation_measurement_lookup_reference.R',old);sys.source('R/analysis_correlation.R',new)
for(env in list(old,new))for(name in ls(env))if(is.function(env[[name]]))env[[name]]<-compiler::cmpfun(env[[name]])
set.seed(20260915);data<-as.data.frame(matrix(rnorm(1000L*20L),ncol=20L));names(data)<-paste0('v',seq_len(20L))
capture<-function(env,info,selected){
 ds<-list();stdout<-capture.output(value<-withCallingHandlers(env$prepare_correlation_results(selected,names(selected),variable_info=info,category_table=info[,c('name','var_label')],options=list(continuous_method='pearson',normality=TRUE)),
 warning=function(e){ds[[length(ds)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleWarning')},
 message=function(e){ds[[length(ds)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleMessage')}))
 list(value=value,diagnostics=ds,stdout=stdout,rng=.Random.seed)
}
times<-list()
for(n in if(focused)20L else c(20L,10000L))for(kind in if(focused)'missing' else c('complete','missing')){
 info<-data.frame(name=paste0('v',seq_len(n)),measurement='continuous',var_label=paste0('Variable ',seq_len(n)))
 selected<-data
 if(kind=='missing')for(i in seq_along(selected))selected[[i]][seq.int(i,1000L,by=10L)]<-NA_real_
 expected<-capture(old,info,selected)
 stopifnot(nrow(expected$value$pairwise_table)==190L,identical(expected,capture(new,info,selected),num.eq=FALSE))
 for(iteration in seq_len(if(focused)15L else 3L))for(version in if((iteration+run)%%2L)c('old','new')else c('new','old')){
  gc();start<-Sys.time();actual<-capture(get(version),info,selected)
  elapsed<-as.numeric(difftime(Sys.time(),start,units='secs'))
  stopifnot(identical(expected,actual,num.eq=FALSE))
  times[[length(times)+1L]]<-data.frame(run,metadata_rows=n,kind,iteration,version,seconds=elapsed)
 }
}
write.csv(do.call(rbind,times),file.path(root,paste0(if(focused)'focused-times-' else 'times-',run,'.csv')),row.names=FALSE)
print(aggregate(seconds~metadata_rows+kind+version,do.call(rbind,times),median))
