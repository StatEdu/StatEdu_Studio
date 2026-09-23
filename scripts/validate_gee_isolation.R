Sys.setlocale('LC_CTYPE','Korean_Korea.utf8')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(812)
d<-data.frame(id=rep(1:60,each=2),time=rep(1:2,60),x=rnorm(120))
d$y<-1+.4*d$x+rep(rnorm(60),each=2)+rnorm(120)
fit<-longitudinal_fit_model(d,'y','id','time','x','gee','gaussian','unstructured')$model
reference<-geepack::geeglm(y~x,data=d,id=id,waves=time,family=gaussian(),corstr='exchangeable',control=geepack::geese.control(epsilon=1e-10,maxit=100))
stopifnot(max(abs(coef(fit)-coef(reference)))<1e-10)
stopifnot(identical(attr(fit,'statedu_requested_corstr'),'unstructured'))
d$time[2]<-1
err<-tryCatch(longitudinal_fit_model(d,'y','id','time','x','gee','gaussian','unstructured'),error=identity)
stopifnot(inherits(err,'error'),grepl('unique',conditionMessage(err)))
cat('Two-wave equivalent GEE and duplicate-wave guard passed.\n')

# Repeated copies of a subject outcome yield perfect correlation and unstable Wald tests.
constant <- data.frame(id = rep(1:60, each = 2), time = rep(1:2, 60),
                       x = rep(rnorm(60), each = 2), y = rep(rnorm(60), each = 2))
err <- tryCatch(longitudinal_fit_model(constant, 'y', 'id', 'time', 'x',
  'gee', 'gaussian', 'unstructured'), error = identity)
stopifnot(inherits(err, 'error'), grepl('singular', conditionMessage(err)))
stopifnot(is.null(longitudinal_gee_check_unstructured(c(.2, .3, .4))))
for (alpha in list(1, c(.9, .9, -.9), c(NA_real_))) {
  err <- tryCatch(longitudinal_gee_check_unstructured(alpha), error = identity)
  stopifnot(inherits(err, 'error'))
}
cat('Singular working correlation rejection passed.\n')
