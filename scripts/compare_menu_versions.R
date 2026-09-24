args <- commandArgs(trailingOnly=TRUE)
host_root <- normalizePath(getwd(),winslash='/')
version <- args[[1]]; run <- as.integer(args[[2]])
out <- file.path(host_root,'output/menu-before-after-20260915')
setwd(file.path(out,version))
.libPaths(R.home('library'))
Sys.setenv(STATEDU_MODULE_CACHE_DIR=file.path(out,paste0('cache-',version)),STATEDU_NO_PACKAGE_INSTALL='true')
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
statedu_apply_preferences(statedu_initial_preferences());options(statedu.app_language='en')
make_info <- function(names,measurements) data.frame(name=names,measurement=measurements)
parsed <- parse(file.path(host_root,'scripts/benchmark_analysis_pipeline_all.R'))
active <- FALSE
for(e in parsed) {
 if(is.call(e) && identical(e[[1]],as.name('set.seed'))) active <- TRUE
 if(active) eval(e,envir=.GlobalEnv)
 if(is.call(e) && identical(e[[1]],as.name('<-')) && identical(e[[2]],as.name('cases'))) break
}
# The original fixture definitions above are evaluated unchanged in both snapshots.
set.seed(20260915)
sv <- data.frame(time=rexp(n,.01),status=rbinom(n,1,.7),group=group2,x=x1)
cr <- sv;cr$status <- sample(0:2,n,replace=TRUE)
long <- data.frame(id=rep(1:60,each=3),time=rep(0:2,60),x=x1,y=y)
survey_data <- data.frame(y=y,x1=x1,x2=x2,g=group2,h=group3,psu=rep(1:30,each=6),wt=runif(n,.5,2),binary=as.numeric(log_data$outcome)-1)
survey_info <- make_info(names(survey_data),c('continuous','continuous','continuous','binary','category','category','continuous','binary'))
survey_input <- list(p_strata='',p_cluster='psu',p_weight='wt',p_fpc='',p_variance_method='auto',p_lonely_psu='adjust',p_correlation_method='pearson',p_correlation_p_adjust='holm',p_correlation_matrix=TRUE,p_use_replicate_weights=FALSE,p_replicate_weights=character(),p_replicate_type='auto',p_replicate_combined_weights=FALSE,p_subpopulation='',p_subpopulation_condition='',p_subpopulation_condition_type='equals',p_subpopulation_condition_value='')
add <- function(name,calc,ui=NULL) cases[[length(cases)+1L]] <<- list(name,calc,ui)
add('One-group repeated ANOVA',function() prepare_paired_rm_results(mixed_data,variables=c('pre','post','post2'),variable_info=mixed_info))
add('Nonparametric independent',function() prepare_ttest_anova_results(tt_data,'y','group',tt_info,options=list(force_nonparametric=TRUE,effect_size=TRUE)))
add('Correlation Spearman',function() prepare_correlation_results(cor_data,names(cor_data),cor_info,options=list(continuous_method='spearman')))
add('Kaplan-Meier',function() prepare_km_analysis_result(sv,'time','status','group',event_value='1',rate_times=c(0,50,100)))
add('Cox regression',function() prepare_cox_analysis_result(sv,'time','status','x',event_value='1'))
add('Competing risks',function() prepare_competing_risk_result(cr,'time','status',group='group',rate_times=c(0,50,100)))
add('Longitudinal GEE',function() prepare_longitudinal_analysis_result(long,'y','id','time',predictors='x',model_type='gee',family='gaussian'))
add('Longitudinal LMM',function() prepare_longitudinal_analysis_result(long,'y','id','time',predictors='x',model_type='lmm',family='gaussian'))
add('Complex frequencies',function() complex_sample_frequency_result(survey_data,c('y','g'),survey_input,'p',variable_info=survey_info))
add('Complex crosstabs',function() complex_sample_crosstab_result(survey_data,'g','h',survey_input,'p',variable_info=survey_info))
add('Complex t-test ANOVA',function() complex_sample_group_result(survey_data,'y','g',survey_input,'p',variable_info=survey_info))
add('Complex correlation',function() complex_sample_correlation_result(survey_data,c('y','x1','x2'),survey_input,'p',variable_info=survey_info))
add('Complex regression',function() complex_sample_single_regression_result(survey_data,'y',c('x1','x2'),survey_input,'p',variable_info=survey_info))
add('Complex logistic',function() complex_sample_single_regression_result(survey_data,'binary',c('x1','x2'),survey_input,'p',logistic=TRUE,variable_info=survey_info))
source(file.path(host_root,'scripts/compare_menu_extra_cases.R'))
capture <- function(f) {
 set.seed(20260915);diagnostics<-list();error<-NULL
 stdout<-capture.output(value<-withCallingHandlers(tryCatch(f(),error=function(e){error<<-list(class=class(e),message=conditionMessage(e));NULL}),
 warning=function(e){diagnostics[[length(diagnostics)+1L]]<<-list(class=class(e),message=conditionMessage(e));invokeRestart('muffleWarning')},
 message=function(e){diagnostics[[length(diagnostics)+1L]]<<-list(class=class(e),message=conditionMessage(e));invokeRestart('muffleMessage')}))
 list(value=value,error=error,diagnostics=diagnostics,stdout=stdout,rng=.Random.seed)
}
ids<-seq_along(cases)
filter<-Sys.getenv('STATEDU_COMPARE_IDS','')
records<-list()
if(nzchar(filter)) {
 ids<-intersect(ids,as.integer(strsplit(filter,',',fixed=TRUE)[[1L]]))
 existing_path<-file.path(out,paste0(version,'-',run,'-times.csv'))
 if(file.exists(existing_path)){existing<-read.csv(existing_path);existing<-existing[!existing$id%in%ids,];records<-split(existing,existing$id)}
}
for(i in if(run%%2)ids else rev(ids)) {
 case<-cases[[i]];cat(version,run,i,case[[1]],'\n');flush.console()
 first<-capture(case[[2]]);samples<-numeric(3)
 for(j in 1:3){gc();start<-Sys.time();value<-capture(case[[2]]);samples[j]<-as.numeric(difftime(Sys.time(),start,units='secs'))}
 ui<-NULL;render_seconds<-NA_real_
 if(length(case)>=3L && is.function(case[[3]]) && is.null(value$error)) {
   ui_times<-numeric(3)
   for(j in 1:3){start<-Sys.time();ui<-capture(function() htmltools::renderTags(case[[3]](value$value))$html);ui_times[j]<-as.numeric(difftime(Sys.time(),start,units='secs'))}
   render_seconds<-median(ui_times)
 }
 saveRDS(list(result=value,ui=ui),file.path(out,paste0(version,'-',run,'-',i,'.rds')))
 records[[length(records)+1L]]<-data.frame(version,run,id=i,menu=case[[1]],seconds=median(samples),minimum=min(samples),maximum=max(samples),render_seconds,error=if(is.null(value$error))''else value$error$message,ui_error=if(is.null(ui$error))''else ui$error$message)
 write.csv(do.call(rbind,records),file.path(out,paste0(version,'-',run,'-times.csv')),row.names=FALSE)
}
cat('FINISHED',version,run,'\n')
