.libPaths(R.home('library'))
source('R/analysis_mixed_rm_anova.R')
set.seed(231)
saved_options<-options()
capture<-function(fn){
 conditions<-character()
 stdout<-capture.output(value<-tryCatch(withCallingHandlers(fn(),
  warning=function(w){conditions<<-c(conditions,paste('warning',conditionMessage(w)));invokeRestart('muffleWarning')},
  message=function(m){conditions<<-c(conditions,paste('message',conditionMessage(m)));invokeRestart('muffleMessage')}),error=function(e)list(error=conditionMessage(e))))
 list(value=value,conditions=conditions,stdout=stdout,rng=.Random.seed)
}
original<-mixed_rm_adjusted_pair_fit_test
batch_calls<-0L;covariance_checks<-0L
mixed_rm_adjusted_pair_fit_test<-function(fit,safe_covariates,design=NULL,cov_beta=NULL){
 if(!is.null(cov_beta)){
  stopifnot(identical(cov_beta,stats::vcov(fit)))
  covariance_checks<<-covariance_checks+1L
  batch_calls<<-batch_calls+1L
 }
 original(fit,safe_covariates,design,cov_beta)
}
cases<-0L
for(kind in c('plain','reversed','unused','single_level','two_factors','collinear','missing_factor',
             'missing_response','ordered','contrast_matrix','contrast_function','global_sum','na_exclude'))for(trial in 1:5){
 options(na.action='na.omit',contrasts=c(unordered='contr.treatment',ordered='contr.poly'))
 n<-120L;y<-matrix(rnorm(n*6),n,6)
 d<-data.frame(x=rnorm(n),f=factor(rep(c('A','B','C'),length.out=n)))
 if(kind=='reversed')d$f<-factor(d$f,levels=rev(levels(d$f)))
 if(kind=='unused')d$f<-factor(d$f,levels=c(levels(d$f),'unused'))
 if(kind=='single_level')d$f<-factor(rep('A',n),levels=c('A','B'))
 if(kind=='two_factors')d$f2<-factor(rep(c('X','Y'),each=3,length.out=n))
 if(kind=='collinear')d$f2<-d$f
 if(kind=='missing_factor')d$f[1]<-NA
 if(kind=='missing_response'){y[1,1]<-NA;y[2,2]<-NA}
 if(kind=='ordered')d$f<-ordered(d$f)
 if(kind=='contrast_matrix')contrasts(d$f)<-contr.sum(3)
 if(kind=='contrast_function')attr(d$f,'contrasts')<-'contr.sum'
 if(kind=='global_sum')options(contrasts=c(unordered='contr.sum',ordered='contr.poly'))
 if(kind=='na_exclude')options(na.action='na.exclude')
 pairs<-combn(ncol(y),2,simplify=FALSE)
 a<-capture(function()lapply(pairs,function(p)mixed_rm_adjusted_pair_test(y[,p[2]]-y[,p[1]],d)))
 batch_calls<-0L
 b<-capture(function()mixed_rm_adjusted_pair_tests(y,d,pairs))
 stopifnot(identical(a,b))
 eligible<-kind %in% c('plain','reversed','unused','single_level','two_factors','collinear')
 stopifnot(batch_calls==if(eligible)length(pairs)else 0L)
 cases<-cases+1L
}
mixed_rm_adjusted_pair_fit_test<-original
options(saved_options)
cat('PASS:',cases,'factor scenarios,',covariance_checks,'exact covariances, exact tests/conditions/stdout/RNG, batch/fallback paths verified\n')
