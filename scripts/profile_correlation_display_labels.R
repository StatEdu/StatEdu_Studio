.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
statedu_apply_preferences(statedu_initial_preferences())
root<-'output/correlation-display-label-profile-20260915';dir.create(root,recursive=TRUE,showWarnings=FALSE)
run<-as.integer(commandArgs(trailingOnly=TRUE)[1L]);if(is.na(run))run<-1L
set.seed(20260915)
data<-as.data.frame(matrix(rnorm(1000L*20L),ncol=20L));names(data)<-paste0('v',seq_len(20L))
make_info<-function(n)data.frame(name=paste0('v',seq_len(n)),measurement=rep('continuous',n),var_label=paste0('Variable ',seq_len(n)))
capture<-function(info){
 diagnostics<-list();stdout<-capture.output(value<-withCallingHandlers(
  prepare_correlation_results(data,names(data),variable_info=make_info(20L),category_table=info[,c('name','var_label')],options=list(continuous_method='pearson',normality=TRUE)),
  warning=function(e){diagnostics[[length(diagnostics)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleWarning')},
  message=function(e){diagnostics[[length(diagnostics)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleMessage')}))
 list(value=value,diagnostics=diagnostics,stdout=stdout,rng=.Random.seed)
}
expected<-capture(make_info(20L));stopifnot(nrow(expected$value$pairwise_table)==190L)
timings<-list()
for(n in if(run%%2L)c(20L,10000L)else c(10000L,20L)){
 info<-make_info(n)
 stopifnot(identical(expected,capture(info),num.eq=FALSE))
 for(iteration in seq_len(3L)){
  gc();start<-Sys.time();actual<-capture(info)
  elapsed<-as.numeric(difftime(Sys.time(),start,units='secs'))
  stopifnot(identical(expected,actual,num.eq=FALSE))
  timings[[length(timings)+1L]]<-data.frame(run,label_rows=n,iteration,seconds=elapsed)
 }
}
info<-make_info(10000L);path<-file.path(root,paste0('wide-',run,'.out'))
Rprof(path,interval=0.01)
for(i in seq_len(3L))stopifnot(identical(expected,capture(info),num.eq=FALSE))
Rprof(NULL)
p<-summaryRprof(path)
write.csv(p$by.total,file.path(root,paste0('total-',run,'.csv')))
write.csv(p$by.self,file.path(root,paste0('self-',run,'.csv')))
write.csv(do.call(rbind,timings),file.path(root,paste0('times-',run,'.csv')),row.names=FALSE)
saveRDS(expected,file.path(root,paste0('result-',run,'.rds')))
print(aggregate(seconds~label_rows,do.call(rbind,timings),median))
print(head(p$by.self,10L));cat('Sampled seconds:',p$sampling.time,'\n')
cat('PASS: full correlation results, diagnostics, stdout and RNG identical across metadata sizes\n')
