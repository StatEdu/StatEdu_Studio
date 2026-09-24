Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(924);d<-data.frame(x=rnorm(80),z=rnorm(80),group=factor(rep(c('A','B'),40)))
d$y<-2*d$x+2*(d$group=='B')+rnorm(80)
formula<-y~x+z+group;baseline<-lm(formula,d)
reference<-list(list(formula=formula,coef_table=data.frame(Term=names(coef(baseline)),B=unname(coef(baseline)))))
fit<-function(method,repeats,inference)fit_penalized_models(reference,d,seed=391,
 alpha_grid=c(.2,.8),selection_bootstrap_resamples=3,selection_bootstrap_workers=1,
 methods=method,validation_repeats=repeats,post_selection=inference,inference_splits=20)
for(method in c('Ridge','LASSO','Elastic Net')){
 single<-fit(method,1,FALSE);repeated<-fit(method,3,method!='Ridge')
 for(field in c('publication_coefficients','publication_stability','selection_stability'))
   stopifnot(identical(single[[field]],repeated[[field]]))
 stopifnot(is.null(single$validation_variability),nrow(repeated$validation_variability)==3)
 if(method!='Ridge'){
  inferred<-fit(method,1,TRUE)
  for(field in c('publication_inference','publication_factor_inference','inference_diagnostics','factor_diagnostics'))
    stopifnot(identical(inferred[[field]],repeated[[field]]))
  stopifnot(identical(single$validation_results,inferred$validation_results))
 }else stopifnot(is.null(repeated$publication_inference),nrow(repeated$publication_stability)==0)
}
message('PASS: inference toggle and CV repetitions preserve coefficients/stability; repeat count preserves individual/group inference; inference preserves CV; single repeat omits variability')
