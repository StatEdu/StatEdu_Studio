Sys.setenv(STATEDU_MODULE_CACHE="false")
source("R/app_bootstrap.R",encoding="UTF-8");load_app_packages(check=FALSE);source_app_modules()
set.seed(111);x<-matrix(rnorm(120*5),120,5,dimnames=list(NULL,paste0("x",1:5)));y<-4*x[,1]-2*x[,2]+rnorm(120)
a<-penalized_nested_validation(x,y,"Elastic Net",67)
stopifnot(all(tabulate(a$foldid)==24),all(is.finite(a$prediction)))
k<-1;test<-a$audit[[k]]$test;train<-a$audit[[k]]$train
changed_y<-y;changed_y[test]<-changed_y[test]+1000
b<-penalized_nested_validation(x,changed_y,"Elastic Net",67)
stopifnot(identical(a$prediction[test],b$prediction[test]),!length(intersect(train,test)))
stopifnot(isTRUE(all.equal(a$rmse,sqrt(mean((y-a$prediction)^2)))))
for(method in c("LASSO","Elastic Net")) {
 r<-penalized_multisplit(x,y,method,100,splits=20)
 stopifnot(identical(r,penalized_multisplit(x,y,method,100,splits=20)),all(r$p>=0&r$p<=1))
 for(i in seq_along(r$audit)) {
  s<-r$audit[[i]];stopifnot(!length(intersect(s$train,s$test)),length(c(s$train,s$test))==120)
  if(s$status=="Tested") {
   independent<-summary(lm(y[s$test]~x[s$test,s$selected,drop=FALSE]))$coefficients[-1,4]
   stopifnot(isTRUE(all.equal(unname(s$raw_p),unname(independent),tolerance=1e-10)))
   expected<-pmin(1,independent*length(s$selected))
   stopifnot(isTRUE(all.equal(unname(r$adjusted_split_p[i,s$selected]),unname(expected),tolerance=1e-10)))
  }
 }
 stopifnot(r$p[1]<.05,r$p[2]<.05)
}
stopifnot(identical(as.numeric(penalized_multisplit_aggregate(cbind(c(.01,.02,1,1),rep(1,4)))),c(.04,1)))
constant<-penalized_multisplit(x,rep(1,120),"LASSO",100,20)
stopifnot(all(constant$p==1),all(constant$tested_count==0),all(vapply(constant$audit,`[[`,character(1),"status")=="Tuning failed"))
stopifnot(inherits(try(penalized_multisplit(x,y,"Ridge",100,20),silent=TRUE),"try-error"))
d<-data.frame(y,x);info<-data.frame(name=names(d),measurement="continuous")
res<-prepare_penalized_menu(d,"y",colnames(x),"LASSO",info,resamples=2,seed=12,post_selection=TRUE,inference_splits=20)
stopifnot(nrow(res$publication_coefficients)==6,nrow(res$publication_inference)==5,
 all(c("Nested CV RMSE","Nested CV MAE","Nested CV R²")%in%names(res$publication_summary)),!"CV RMSE"%in%names(res$publication_summary))
html<-as.character(penalized_result_block(res));stopifnot(grepl("Table 4. Post-selection",html,fixed=TRUE),!grepl("High >=",html,fixed=TRUE))
message("PASS: nested CV held-out independence; OLS reference p-values; multiplicity and fixed-quantile aggregation; deterministic splitting; failed splits; main tables")
# Coverage status boundaries and failure/selection distinctions.
mock<-function(selected,tested,status=rep('Tested',20))list(p=1,selected_count=selected,tested_count=tested,splits=20,audit=lapply(status,function(s)list(status=s)))
stopifnot(penalized_multisplit_status(mock(0,0,rep('Tuning failed',20)))=='All splits failed',
 penalized_multisplit_status(mock(0,0,rep('No variables selected',20)))=='Never selected',
 penalized_multisplit_status(mock(5,0))=='No estimable tests',
 penalized_multisplit_status(mock(9,9))=='Fewer than half tested',
 penalized_multisplit_status(mock(10,10))=='Selection varies across splits',
 penalized_multisplit_status(mock(20,12))=='Some selected splits failed',
 penalized_multisplit_status(mock(20,20))=='Tested in all splits')
message('PASS: coverage-status boundaries and zero-test failure distinctions')
# A constant candidate is never independently tested: do not print p=1 as a test result.
d$constant<-1
constant_result<-prepare_penalized_menu(d,'y',c(colnames(x),'constant'),'LASSO',info,resamples=2,seed=12,post_selection=TRUE,inference_splits=20)
constant_row<-constant_result$publication_inference[constant_result$publication_inference$Predictor=='constant',,drop=FALSE]
stopifnot(nrow(constant_row)==1,constant_row[['Multi-split p']]=='—',constant_row$Status=='Never selected')
message('PASS: untested candidate displays em dash in actual result table')
