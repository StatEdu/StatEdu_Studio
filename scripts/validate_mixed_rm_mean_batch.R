.libPaths(R.home('library'))
source('R/analysis_mixed_rm_anova.R')
set.seed(619);saved_options<-options()
capture<-function(fn){
 conditions<-character()
 stdout<-capture.output(value<-tryCatch(withCallingHandlers(fn(),
  warning=function(w){conditions<<-c(conditions,paste('warning',conditionMessage(w)));invokeRestart('muffleWarning')},
  message=function(m){conditions<<-c(conditions,paste('message',conditionMessage(m)));invokeRestart('muffleMessage')}),error=function(e)list(error=conditionMessage(e))))
 list(value=value,conditions=conditions,stdout=stdout,rng=.Random.seed)
}
cases<-0L
for(kind in c('numeric','factor','ordered','unused','one_level','collinear','ill_conditioned',
 'missing_y','missing_x','infinite','constant','empty','large','tiny','named','single_time','custom_contrasts'))for(policy in c('na.omit','na.exclude','na.fail','na.pass')){
 options(na.action=policy,contrasts=c(unordered='contr.treatment',ordered='contr.poly'))
 n<-80L;y<-matrix(rnorm(n*6),n,6);d<-data.frame(x=rnorm(n))
 if(kind %in% c('factor','ordered','unused','one_level','custom_contrasts'))d$f<-factor(rep(c('A','B'),length.out=n))
 if(kind=='ordered')d$f<-ordered(d$f)
 if(kind=='unused')d$f<-factor(d$f,levels=c('B','A','unused'))
 if(kind=='one_level')d$f<-factor(rep('A',n))
 if(kind=='custom_contrasts')contrasts(d$f)<-contr.sum(2)
 if(kind=='collinear')d$x2<-d$x
 if(kind=='ill_conditioned')d$x2<-d$x+rnorm(n,sd=1e-6)
 if(kind=='missing_y'){y[1,1]<-NA;y[2,2]<-NA}
 if(kind=='missing_x')d$x[1]<-NA
 if(kind=='infinite')y[1,1]<-Inf
 if(kind=='constant')d$x<-1
 if(kind=='empty')d<-data.frame()
 if(kind=='large')y<-y*1e100
 if(kind=='tiny')y<-y*1e-150
 if(kind=='named')dimnames(y)<-list(paste0('row',1:n),LETTERS[1:6])
 if(kind=='single_time')y<-y[,1,drop=FALSE]
 a<-capture(function()vapply(seq_len(ncol(y)),function(i)mixed_rm_adjusted_time_estimate(y[,i],d),numeric(1)))
 b<-capture(function()mixed_rm_adjusted_time_estimates(y,d))
 stopifnot(identical(a,b));cases<-cases+1L
}
# A shared reference grid proves that eligible inputs use the new path.
options(na.action='na.omit')
d<-data.frame(x=rnorm(80));y<-matrix(rnorm(480),80,6)
original<-mixed_rm_covariate_reference_grid;calls<-0L
mixed_rm_covariate_reference_grid<-function(d){calls<<-calls+1L;original(d)}
first<-mixed_rm_adjusted_time_estimates(y,d);stopifnot(calls==1L)
y[,1]<-y[,1]+1
second<-mixed_rm_adjusted_time_estimates(y,d);stopifnot(calls==2L,!identical(first,second))
individual<-vapply(1:6,function(i)mixed_rm_adjusted_time_estimate(y[,i],d),numeric(1))
stopifnot(identical(second,individual),calls==8L)
mixed_rm_covariate_reference_grid<-original;options(saved_options)
cat('PASS:',cases,'exact unrounded mean/conditions/stdout/RNG comparisons; shared grid and fresh data verified\n')
