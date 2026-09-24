Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(913);x<-matrix(rnorm(90*5),90,5,dimnames=list(NULL,paste0('x',1:5)));y<-2*x[,1]+rnorm(90)
for(method in c('Ridge','LASSO','Elastic Net')){
 result<-penalized_repeated_validation(x,y,method,452,alpha_grid=c(.2,.8),repeats=3)
 stopifnot(result$repeats==3,identical(result$seeds,c(452,552,652)))
 stopifnot(identical(result,penalized_repeated_validation(x,y,method,452,c(.2,.8),3)))
 for(i in 1:3){
  run<-result$runs[[i]];pred<-run$prediction
  stopifnot(isTRUE(all.equal(result$metrics$RMSE[i],sqrt(mean((y-pred)^2)))),
    isTRUE(all.equal(result$metrics$MAE[i],mean(abs(y-pred)))),
    isTRUE(all.equal(result$metrics$R2[i],1-sum((y-pred)^2)/sum((y-mean(y))^2))))
  stopifnot(identical(sort(unlist(lapply(run$audit,`[[`,'test'))),seq_along(y)))
  stopifnot(all(vapply(run$audit,function(a)!length(intersect(a$train,a$test)),logical(1))))
 }
 stopifnot(identical(result$rmse,mean(result$metrics$RMSE)))
 single<-penalized_repeated_validation(x,y,method,452,c(.2,.8),1)
 stopifnot(identical(single$runs[[1]],result$runs[[1]]))
 # Alter one outer fold's held-out outcomes: its predictions must not change.
 changed<-y;test<-result$runs[[2]]$audit[[1]]$test;changed[test]<-changed[test]+20
 altered<-penalized_nested_validation(x,changed,method,552,c(.2,.8))
 stopifnot(identical(altered$prediction[test],result$runs[[2]]$prediction[test]))
}
for(bad in list(0,21,1.5,NA_real_))stopifnot(inherits(try(penalized_repeated_validation(x,y,'Ridge',1,repeats=bad),silent=TRUE),'try-error'))
stopifnot(inherits(try(penalized_repeated_validation(x,rep(1,90),'Ridge',1,repeats=2),silent=TRUE),'try-error'))
d<-data.frame(y,x);info<-data.frame(name=names(d),measurement='continuous')
r<-prepare_penalized_menu(d,'y',colnames(x),'Ridge',info,resamples=2,validation_repeats=3)
stopifnot(nrow(r$validation_variability)==3,r$publication_summary$`CV repetitions`==3,
 all(r$validation_variability$Repetitions==3),grepl('penalized-validation-variability',as.character(penalized_result_block(r)),fixed=TRUE))
message('PASS: per-repeat held-out metrics, mean aggregation, deterministic seeds, fold isolation, single-repeat compatibility, validation and variability table')
