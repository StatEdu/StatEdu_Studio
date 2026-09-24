Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(35);x<-matrix(rnorm(100*8),100,8,dimnames=list(NULL,paste0('x',1:8)))
y<-2*x[,1]+x[,2]+rnorm(100);grid<-c(.1,.5,.9);seed<-391L
reference<-function(b,alphas){
 set.seed(seed+10000L+b);rows<-sample.int(nrow(x),nrow(x),replace=TRUE)
 fits<-lapply(alphas,function(a){set.seed(seed+20000L+b)
   glmnet::cv.glmnet(x[rows,,drop=FALSE],y[rows],alpha=a,nfolds=5,standardize=TRUE,family='gaussian',keep=FALSE)})
 best<-which.min(vapply(fits,function(f)min(f$cvm),numeric(1)));fit<-fits[[best]]
 selected<-function(rule){co<-as.matrix(coef(fit,s=rule))[-1,,drop=FALSE];rownames(co)[abs(co[,1])>1e-8]}
 list(lambda.min=selected('lambda.min'),lambda.1se=selected('lambda.1se'),alpha=alphas[[best]])
}
for(alphas in list(1,grid))for(b in 1:4){
 actual<-penalized_selection_bootstrap_one(b,x,y,alphas,5,seed)
 stopifnot(identical(actual,reference(b,alphas)))
}
cl<-parallel::makePSOCKcluster(2)
serial<-lapply(1:4,penalized_selection_bootstrap_one,model_x=x,model_y=y,alpha=grid,nfolds=5,seed=seed)
concurrent<-tryCatch(parallel::parLapply(cl,1:4,penalized_selection_bootstrap_one,model_x=x,model_y=y,alpha=grid,nfolds=5,seed=seed),finally=parallel::stopCluster(cl))
stopifnot(identical(serial,concurrent))
stopifnot(is.null(penalized_selection_bootstrap_one(1,x,rep(1,100),grid,5,seed)))
message('PASS: independent resampling and alpha/lambda tuning, LASSO equivalence, serial/parallel determinism, failed resample')
