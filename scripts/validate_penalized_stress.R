# Reproducible exploratory stress study; not a proof of FWER control.
source('R/analysis_penalized.R',encoding='UTF-8')
reps<-as.integer(Sys.getenv('STATEDU_STRESS_REPS','50'));splits<-50L
out<-'tmp/penalized-stress';dir.create(out,recursive=TRUE,showWarnings=FALSE)
cases<-list(null=list(n=100,p=10,rho=0,beta=numeric()),collinear=list(n=100,p=12,rho=.95,beta=c(1,-1)),small=list(n=24,p=8,rho=.2,beta=c(1)),high_dimensional=list(n=60,p=100,rho=.3,beta=c(1.5,-1,1,.8,-.8)))
worker<-function(job){
 cfg<-job$cfg;set.seed(620000+job$scenario_id*1000+job$rep)
 z<-rnorm(cfg$n);x<-sqrt(cfg$rho)*z+sqrt(1-cfg$rho)*matrix(rnorm(cfg$n*cfg$p),cfg$n,cfg$p)
 colnames(x)<-paste0('x',seq_len(cfg$p));beta<-c(cfg$beta,rep(0,cfg$p-length(cfg$beta)));y<-as.numeric(x%*%beta+rnorm(cfg$n))
 active<-which(beta!=0);null<-which(beta==0)
 do.call(rbind,lapply(c('LASSO','Elastic Net'),function(method){
  r<-suppressWarnings(penalized_multisplit(x,y,method,seed=720000+job$rep,splits=50L))
  status<-vapply(r$audit,`[[`,character(1),'status')
  data.frame(scenario=job$scenario,rep=job$rep,method=method,n=cfg$n,p=cfg$p,
   false_rejection=any(r$p[null]<.05),power=if(length(active))mean(r$p[active]<.05)else NA_real_,
   failed_split_rate=mean(!status%in%c('Tested','No variables selected')),
   empty_split_rate=mean(status=='No variables selected'),
   screening_rate=mean(vapply(r$audit,function(a)all(colnames(x)[active]%in%a$selected),logical(1))))
 }))
}
cluster<-parallel::makePSOCKcluster(4L)
tryCatch({
 parallel::clusterEvalQ(cluster,{source('R/analysis_penalized.R',encoding='UTF-8');requireNamespace('glmnet',quietly=TRUE)})
 all<-list()
 for(ci in seq_along(cases))for(start in seq(1,reps,by=10)){
  jobs<-lapply(start:min(reps,start+9),function(i)list(scenario=names(cases)[ci],scenario_id=ci,cfg=cases[[ci]],rep=i))
  all<-c(all,parallel::parLapply(cluster,jobs,worker));raw<-do.call(rbind,all)
  write.csv(raw,file.path(out,'replicates.csv'),row.names=FALSE)
  message(names(cases)[ci],': ',min(reps,start+9),'/',reps)
 }
 groups<-split(raw,interaction(raw$scenario,raw$method,drop=TRUE))
 summary<-do.call(rbind,lapply(groups,function(d){ci<-binom.test(sum(d$false_rejection),nrow(d))$conf.int
  data.frame(scenario=d$scenario[1],method=d$method[1],replicates=nrow(d),n=d$n[1],p=d$p[1],FWER=mean(d$false_rejection),FWER_low=ci[1],FWER_high=ci[2],power=mean(d$power),power_MCSE=sd(d$power)/sqrt(nrow(d)),failed_splits=mean(d$failed_split_rate),empty_splits=mean(d$empty_split_rate),screening=mean(d$screening_rate))}))
 write.csv(summary,file.path(out,'summary.csv'),row.names=FALSE);print(summary,row.names=FALSE)
},finally=parallel::stopCluster(cluster))
