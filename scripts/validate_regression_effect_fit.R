source('R/analysis_regression.R')
old<-new.env();new<-new.env()
new$coefficient_effect_sizes<-coefficient_effect_sizes
old$coefficient_effect_sizes<-coefficient_effect_sizes
hits<-0L
rewrite<-function(x){
 if(is.call(x)&&identical(x[[1]],as.name('regression_bootstrap_lm_fit'))){x[[1]]<-quote(stats::lm.fit);hits<<-hits+1L;return(x)}
 if(is.call(x)||is.expression(x))for(i in seq_along(x))x[i]<-list(rewrite(x[[i]]))
 x
}
body(old$coefficient_effect_sizes)<-rewrite(body(old$coefficient_effect_sizes));stopifnot(hits==1L)
capture<-function(fn,model){
 set.seed(17);conditions<-character()
 stdout<-capture.output(value<-tryCatch(withCallingHandlers(fn(model),warning=function(w){conditions<<-c(conditions,conditionMessage(w));invokeRestart('muffleWarning')},message=function(m){conditions<<-c(conditions,conditionMessage(m));invokeRestart('muffleMessage')}),error=function(e)list(error=conditionMessage(e))))
 list(value=value,conditions=conditions,stdout=stdout,rng=.Random.seed)
}
count<-0L
for(n in c(20L,200L,2000L))for(kind in c('ordinary','weighted','zero_weight','missing','exclude','singular','perfect','offset','no_intercept','factor','near_singular','multiple')){
 set.seed(621);d<-data.frame(x=rnorm(n),z=rnorm(n),y=rnorm(n),w=runif(n))
 if(kind=='zero_weight')d$w[1]<-0
 if(kind%in%c('missing','exclude'))d$x[1]<-NA
 if(kind=='singular')d$z<-d$x
 if(kind=='near_singular')d$z<-d$x+1e-10*d$z
 if(kind=='perfect')d$y<-d$x
 if(kind=='factor')d$z<-factor(rep(1:3,length.out=n))
 fit<-suppressWarnings(if(kind%in%c('weighted','zero_weight'))lm(y~x+z,d,weights=w)else if(kind=='exclude')lm(y~x+z,d,na.action=na.exclude)else if(kind=='offset')lm(y~x+z,d,offset=z)else if(kind=='no_intercept')lm(y~x+z-1,d)else if(kind=='multiple')lm(cbind(y,w)~x+z,d)else lm(y~x+z,d))
 stopifnot(identical(capture(old$coefficient_effect_sizes,fit),capture(new$coefficient_effect_sizes,fit),num.eq=FALSE))
 count<-count+1L
}
cat('PASS:',count,'effect-size boundary cases, conditions/stdout/RNG identical\n')
