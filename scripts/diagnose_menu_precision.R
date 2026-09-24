# Diagnostic reference only: align historical numerical settings, never edit product code.
args<-commandArgs(trailingOnly=TRUE)
host<-normalizePath(getwd(),winslash='/');root<-file.path(host,'output/menu-before-after-20260915')
setwd(file.path(root,'before'));.libPaths(R.home('library'))
Sys.setenv(STATEDU_MODULE_CACHE_DIR=file.path(root,'cache-before'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
statedu_apply_preferences(statedu_initial_preferences());options(statedu.app_language='en')
make_info<-function(names,measurements)data.frame(name=names,measurement=measurements)
active<-FALSE
for(e in parse(file.path(host,'scripts/benchmark_analysis_pipeline_all.R'))){
 if(is.call(e)&&identical(e[[1]],as.name('set.seed')))active<-TRUE
 if(active)eval(e,.GlobalEnv)
 if(is.call(e)&&identical(e[[1]],as.name('<-'))&&identical(e[[2]],as.name('cases')))break
}
for(e in parse(file.path(root,'after/R/analysis_pca.R'))) {
 if(is.call(e)&&identical(e[[1]],as.name('<-'))&&identical(e[[2]],as.name('pca_principal'))){eval(e,.GlobalEnv);break}
}
counts<-c(pca=0L,gee=0L)
rewrite<-function(x){
 if(!is.call(x))return(x)
 if(identical(x[[1]],quote(psych::principal))){x[[1]]<-as.name('pca_principal');counts['pca']<<-counts['pca']+1L}
 if(identical(x[[1]],quote(geepack::geeglm))){x$control<-quote(geepack::geese.control(epsilon=1e-10,maxit=100));counts['gee']<<-counts['gee']+1L}
 for(i in seq_along(x)[-1L])if(!identical(x[[i]],quote(expr=)))x[i]<-list(rewrite(x[[i]]))
 x
}
for(name in ls(.GlobalEnv,pattern='^(pca_|prepare_pca|longitudinal_fit_model)')){
 f<-get(name);if(!is.function(f)||identical(name,'pca_principal'))next
 body(f)<-rewrite(body(f));assign(name,f,.GlobalEnv)
}
stopifnot(counts['pca']==2L,counts['gee']==2L)
set.seed(20260915)
pca<-prepare_pca_results(item_data,names(item_data),item_info,options=list(criterion='eigen',rotation='varimax'))
long<-data.frame(id=rep(1:60,each=3),time=rep(0:2,60),x=x1,y=y)
set.seed(20260915)
gee<-prepare_longitudinal_analysis_result(long,'y','id','time',predictors='x',model_type='gee',family='gaussian')
current_pca<-readRDS(file.path(root,'after-1-12.rds'))$result$value
current_gee<-readRDS(file.path(root,'after-1-22.rds'))$result$value
checks<-list(pca_loadings=identical(pca$loadings,current_pca$loadings,num.eq=FALSE),
 pca_scores=identical(pca$scores,current_pca$scores,num.eq=FALSE),
 gee_coefficients=identical(gee[[1]]$coef_table,current_gee[[1]]$coef_table,num.eq=FALSE),
 gee_model_coefficients=identical(gee[[1]]$model$coefficients,current_gee[[1]]$model$coefficients,num.eq=FALSE))
saveRDS(list(replacements=counts,checks=checks,pca=pca,gee=gee),file.path(root,'precision-aligned-reference.rds'))
print(checks);stopifnot(all(unlist(checks)))
