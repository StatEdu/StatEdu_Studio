Sys.setenv(STATEDU_MODULE_CACHE='false', STATEDU_PENALIZED_BOOTSTRAP_WORKERS='')
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
options(statedu.penalized.bootstrap.worker_files_warm=NULL)
stopifnot(penalized_selection_bootstrap_workers(NULL,500,1)==1L,
  penalized_selection_bootstrap_workers(NULL,20,9)==1L,
  penalized_selection_bootstrap_workers(1,500,9)==1L)
cores<-parallel::detectCores(logical=FALSE)
if(is.finite(cores)&&cores>2)stopifnot(penalized_selection_bootstrap_workers(NULL,500,9)>1L)
set.seed(91);d<-data.frame(x=rnorm(90),z=rnorm(90),w=rnorm(90));d$y<-2*d$x-d$z+rnorm(90)
fit<-lm(y~x+z+w,d)
args<-list(results=list(list(formula=formula(fit),coef_table=data.frame(Term=names(coef(fit)),B=unname(coef(fit))))),
  data=d,methods='Elastic Net',alpha_grid=c(.1,.3,.6,.9),seed=817L,
  selection_bootstrap_resamples=250L,nested_validation=TRUE,validation_repeats=1L)
t1<-system.time(serial<-do.call(fit_penalized_models,c(args,list(selection_bootstrap_workers=1L))))[['elapsed']]
options(statedu.penalized.bootstrap.worker_files_warm=NULL)
t2<-system.time(automatic<-do.call(fit_penalized_models,args))[['elapsed']]
stopifnot(identical(serial,automatic))
cat(sprintf('PASS: cold policy, explicit override, full result equality. Serial %.2fs; automatic %.2fs\n',t1,t2))
