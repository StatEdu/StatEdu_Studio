source('scripts/validate_survival_reason_label.R')
normalize<-function(x){
 a<-attributes(x)
 if(is.call(x))x<-as.call(lapply(as.list(x),normalize))
 else if(is.pairlist(x))x<-as.pairlist(lapply(as.list(x),normalize))
 else if(is.list(x))for(i in seq_along(x))x[i]<-list(normalize(x[[i]]))
 if(!is.null(a)){a$.Environment<-NULL;for(i in seq_along(a))a[i]<-list(normalize(a[[i]]));attributes(x)<-a}
 x
}
checks<-0L
for(pattern in c('complete','overlap','disjoint'))for(kind in c('basic','strata','cluster','spline')){
 set.seed(778);n<-600L
 data<-data.frame(time=rexp(n,.01),event=rbinom(n,1,.7),group=factor(rep(letters[1:3],length.out=n)),cluster_id=rep(1:60,each=10))
 for(i in 1:6){
  x<-rnorm(n)
  if(pattern=='overlap')x[seq.int(1L,n,5L)]<-NA
  if(pattern=='disjoint')x[which(seq_len(n)%%12L==i)]<-NA
  data[[paste0('x',i)]]<-x
 }
 args<-list(data=data,time='time',event='event',covariates=paste0('x',1:6),adjusted_bootstrap_reps=0L)
 if(kind=='strata')args$strata<-'group'
 if(kind=='cluster')args$cluster<-'cluster_id'
 if(kind=='spline'){args$spline_covariate<-'x1';args$spline_df<-4L}
 a<-capture(old,'prepare_cox_analysis_result',args);b<-capture(new,'prepare_cox_analysis_result',args)
 stopifnot(inherits(a$value$fit,'coxph'),a$value$n==n*switch(pattern,complete=1,overlap=.8,disjoint=.5),all(is.finite(coef(a$value$fit))))
 a$value<-normalize(a$value);b$value<-normalize(b$value)
 stopifnot(identical(a,b,num.eq=FALSE));checks<-checks+1L
}
cat('PASS:',checks,'successful full Cox comparisons; formula environment attributes excluded\n')
