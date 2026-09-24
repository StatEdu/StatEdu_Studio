source('scripts/validate_survival_reason_empty.R')
for(env in list(old,new))for(name in ls(env))if(is.function(env[[name]]))env[[name]]<-compiler::cmpfun(env[[name]])
run_id<-as.integer(commandArgs(trailingOnly=TRUE)[1]);if(is.na(run_id))run_id<-1L
root<-'output/survival-reason-empty-20260915';dir.create(root,recursive=TRUE,showWarnings=FALSE)
timings<-list()
focused<-length(commandArgs(trailingOnly=TRUE))>1L
kinds<-if(focused)commandArgs(trailingOnly=TRUE)[2L]else c('complete','sparse','overlap','disjoint')
for(n in c(1000L,100000L))for(kind in kinds){
 set.seed(778);data<-data.frame(time=rexp(n,.01),event=rbinom(n,1,.7),group=factor(rep(letters[1:3],length.out=n)))
 for(i in 1:6)data[[paste0('x',i)]]<-rnorm(n)
 if(kind=='sparse'){data$event[seq.int(1L,n,20L)]<-NA;data$group[seq.int(2L,n,20L)]<-NA}
 if(kind=='overlap'){rows<-seq.int(1L,n,2L);data$event[rows]<-NA;data$group[rows]<-NA;for(i in 1:6)data[[paste0('x',i)]][rows]<-NA}
 if(kind=='disjoint')for(i in 1:6)data[[paste0('x',i)]][which(seq_len(n)%%12L==i)]<-NA
 args<-list(data=data,settings=survival_legacy_settings('time','event','1','group',paste0('x',1:6)))
 expected<-capture(old,'survival_preflight',args);stopifnot(isTRUE(expected$value$ok))
 stopifnot(identical(expected,capture(new,'survival_preflight',args),num.eq=FALSE))
 for(iteration in seq_len(if(focused)15L else 5L))for(version in if((iteration+run_id)%%2L)c('old','new')else c('new','old')){
  gc();start<-Sys.time();actual<-capture(get(version),'survival_preflight',args);elapsed<-as.numeric(difftime(Sys.time(),start,units='secs'))
  stopifnot(identical(expected,actual,num.eq=FALSE))
  timings[[length(timings)+1L]]<-data.frame(run=run_id,n,kind,iteration,version,seconds=elapsed)
 }
}
out<-do.call(rbind,timings);write.csv(out,file.path(root,paste0(if(focused)'focused-'else '', 'times-',run_id,'.csv')),row.names=FALSE)
print(aggregate(seconds~n+kind+version,out,median));cat('PASS:',nrow(out)+2L*length(kinds),'full result comparisons\n')
