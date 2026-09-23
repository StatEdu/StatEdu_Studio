Sys.setlocale('LC_ALL','Korean_Korea.utf8')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
if(!requireNamespace('mmrm',quietly=TRUE))stop('Install mmrm to run repeated LMM validation.')
set.seed(7502)
d<-data.frame(id=rep(1:80,each=2),time=rep(1:2,80),group=rep(rep(0:1,each=40),each=2))
d$visit<-factor(d$time);d$y<-2+d$group+.3*d$time+rep(rnorm(80),each=2)+rnorm(160)
d$interaction<-d$group*as.numeric(d$time==2)
fit<-longitudinal_repeated_lmm(d,'y','id','time',c('group','visit','interaction'),'UN')
# Saturated group/visit means have exact between-subject residual df = 80-2.
stopifnot(max(abs(fit$coef_table$df-78))<1e-6,fit$gradient_max<1e-7)
ols<-coef(lm(y~group*visit,d))
stopifnot(max(abs(fit$coef_table$B-ols))<1e-7)
scaled<-d;scaled$y<-7*d$y
second<-longitudinal_repeated_lmm(scaled,'y','id','time',c('group','visit','interaction'),'UN')
stopifnot(max(abs(second$coef_table$SE-7*fit$coef_table$SE))<1e-6,
 max(abs(second$coef_table$p-fit$coef_table$p))<1e-6)
ar<-longitudinal_repeated_lmm(d,'y','id','time',c('group','visit'),'AR1')
stopifnot(ar$gradient_max<1e-7,all(is.finite(ar$coef_table$p)))
intercept<-longitudinal_repeated_lmm(d,'y','id','time',character(0),'UN')
stopifnot(nrow(intercept$coef_table)==1)
d$time[2]<-d$time[1]
err<-tryCatch(longitudinal_repeated_lmm(d,'y','id','time','group','UN'),error=identity)
stopifnot(inherits(err,'error'),grepl('unique',conditionMessage(err)))
cat('Repeated LMM exact df, mean, scaling, AR1, intercept, and invalid-input tests passed.\n')
