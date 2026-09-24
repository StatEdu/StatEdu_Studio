source('native/fine_gray/tests/common.R')
count<-0L;errors<-0L;nonconverged<-0L
for(seed in 1:5)for(kind in c('plain','ties','groups','binary','timevarying','extreme_small','extreme_large','missing')) {
 set.seed(seed);d<-make(80,3,kind)
 # Independent perturbations preserve the requested scenario while varying inputs.
 set.seed(seed);d$x<-d$x+matrix(rnorm(length(d$x),sd=.1),nrow(d$x));d$time<-d$time+runif(length(d$time),0,.01)
 if(kind=='ties')d$time<-round(d$time,1)
 if(kind=='extreme_small')d$x<-d$x*1e-50
 if(kind=='extreme_large')d$x<-d$x*1e50
 if(kind=='missing')d$x[1,1]<-NA_real_
 a<-capture_all(versions$installed,d);b<-capture_all(versions$original,d);c<-capture_all(versions$candidate,d)
 stopifnot(identical(a,b,num.eq=FALSE),identical(a,c,num.eq=FALSE))
 count<-count+1L;errors<-errors+!is.null(a$value$value$error);nonconverged<-nonconverged+identical(a$value$value$converged,FALSE)
}
write.csv(data.frame(conditions=count,errors=errors,nonconverged=nonconverged),file.path(root,'extra-coverage.csv'),row.names=FALSE)
cat('PASS',count,'additional native comparisons; errors',errors,'nonconverged',nonconverged,'\n')
