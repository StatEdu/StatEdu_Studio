Sys.setlocale('LC_CTYPE','English_United States.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
data<-data.frame(time=1:4,event=c(0,1,0,1),x=1:4)
catch<-function(expr)tryCatch(force(expr),error=identity)
errors<-list(
 time=catch(prepare_km_single_analysis_result(data,character(), 'event')),
 event=catch(prepare_km_single_analysis_result(data,'time',character())),
 start_stop=catch(prepare_cox_analysis_result(data,character(),'event','x')),
 covariate=catch(prepare_cox_analysis_result(data,'time','event',character())),
 ties=catch(prepare_cox_analysis_result(data,'time','event','x',ties_method='invalid')),
 rmst=catch(survival_parse_rmst_tau('0')),
 strata_covariate=catch(prepare_cox_analysis_result(data,'time','event','x',strata='x')),
 cluster_covariate=catch(prepare_cox_analysis_result(data,'time','event','x',cluster='x')),
 cluster_start=catch(prepare_cox_analysis_result(data,'time','event','x',cluster='cluster',start='start')),
 cluster_strata=catch(prepare_cox_analysis_result(data,'time','event','x',cluster='group',strata='group')),
 spline_covariate=catch(prepare_cox_analysis_result(data,'time','event','x',spline_covariate='other')),
 spline_df=catch(prepare_cox_analysis_result(data,'time','event','x',spline_covariate='x',spline_df=2)),
 spline_robust=catch(prepare_cox_analysis_result(data,'time','event','x',spline_covariate='x',cluster='group')),
 tv_covariate=catch(prepare_cox_analysis_result(data,'time','event','x',time_varying_covariate='other')),
 tv_spline=catch(prepare_cox_analysis_result(data,'time','event','x',time_varying_covariate='x',spline_covariate='x')),
 tv_robust=catch(prepare_cox_analysis_result(data,'time','event','x',time_varying_covariate='x',cluster='group')),
 tv_start=catch(prepare_cox_analysis_result(data,'time','event','x',time_varying_covariate='x',start='start')),
 exact_start=catch(prepare_cox_analysis_result(data,'time','event','x',ties_method='exact',start='start')),
 exact_robust=catch(prepare_cox_analysis_result(data,'time','event','x',ties_method='exact',cluster='group')),
 exact_tv=catch(prepare_cox_analysis_result(data,'time','event','x',ties_method='exact',time_varying_covariate='x')))
sample_data<-data.frame(time=1:12,event=rep(c(0,1),6),x=1:12,group=rep('only',12),category=factor(rep(c('A','B'),6)),few=rep(c(1,2),6))
errors$strata_count<-catch(prepare_cox_analysis_result(sample_data,'time','event','x',strata='group'))
errors$cluster_count<-catch(prepare_cox_analysis_result(sample_data,'time','event','x',cluster='group'))
errors$spline_numeric<-catch(prepare_cox_analysis_result(sample_data,'time','event','category',spline_covariate='category'))
errors$spline_unique<-catch(prepare_cox_analysis_result(sample_data,'time','event','few',spline_covariate='few'))
errors$tv_numeric<-catch(prepare_cox_analysis_result(sample_data,'time','event','category',time_varying_covariate='category'))
errors$tv_unique<-catch(prepare_cox_analysis_result(sample_data,'time','event','few',time_varying_covariate='few'))
errors$adjusted_missing<-catch(survival_adjusted_curve(NULL,sample_data,'missing'))
errors$adjusted_category<-catch(survival_adjusted_curve(NULL,sample_data,'x'))
single_group<-sample_data;single_group$group<-factor(single_group$group)
errors$adjusted_levels<-catch(survival_adjusted_curve(NULL,single_group,'group'))
errors$delayed_life_table<-catch(prepare_km_single_analysis_result(sample_data,'time','event',entry='entry',analysis_method='life_table'))
errors$rmst_range<-catch(prepare_km_single_analysis_result(sample_data,'time','event',rmst_tau=100))
competing_data<-data.frame(time=1:63,event=rep(0:2,21),x=seq_len(63),one=factor(rep('only',63)),many=factor(rep(1:21,3)))
errors$competing_covariate<-catch(prepare_competing_risk_result(competing_data,'time','event',regression='fine_gray'))
errors$cengroup_model<-catch(prepare_competing_risk_result(competing_data,'time','event',censoring_group='one'))
errors$cengroup_missing<-catch(prepare_competing_risk_result(competing_data,'time','event',regression='fine_gray',covariates='x',censoring_group='missing'))
errors$cengroup_role<-catch(prepare_competing_risk_result(competing_data,'time','event',regression='fine_gray',covariates='x',censoring_group='time'))
errors$interest_code<-catch(prepare_competing_risk_result(competing_data,'time','event',event_map=data.frame(raw_value=c('0','1','2'),role=c('censored','event_of_interest','event_of_interest'))))
errors$cengroup_min<-catch(prepare_competing_risk_result(competing_data,'time','event',regression='fine_gray',covariates='x',censoring_group='one'))
errors$cengroup_max<-catch(prepare_competing_risk_result(competing_data,'time','event',regression='fine_gray',covariates='x',censoring_group='many'))
# Simulate unavailable dependencies without changing installed packages.
without_package<-function(fn) {
 env<-new.env(parent=environment(fn));env$requireNamespace<-function(...)FALSE
 environment(fn)<-env;fn
}
errors$cmprsk<-catch(without_package(prepare_competing_risk_result)(competing_data,'time','event'))
errors$ggplot_survival<-catch(without_package(survival_km_ggplot)(NULL))
errors$ggplot_forest<-catch(without_package(survival_cox_ggplot)(NULL))
errors$ggplot_survival_export<-catch(without_package(save_survival_km_figure_files)(NULL,tempdir()))
errors$ggplot_cox_export<-catch(without_package(save_survival_cox_figure_files)(NULL,tempdir()))
# Force an intercept-only design at the design-matrix boundary.
original_model_matrix<-stats::model.matrix
assignInNamespace('model.matrix',function(object,data,...)matrix(1,nrow(data),1,dimnames=list(NULL,'(Intercept)')),ns='stats')
errors$fine_gray_design<-catch(prepare_competing_risk_result(competing_data,'time','event',covariates='x',regression='fine_gray'))
assignInNamespace('model.matrix',original_model_matrix,ns='stats')
errors$times_invalid<-catch(survival_parse_times_strict('1, nope'))
errors$tv_times_invalid<-catch(survival_parse_times_strict('-1','Time-varying HR reporting times'))
errors$adjusted_times_invalid<-catch(survival_parse_times_strict('Inf','Adjusted-survival time points'))
set.seed(917)
followup_data<-data.frame(time=seq_len(60)+.25,event=rep(c(0,1,1),20),x=rnorm(60),group=factor(rep(c('A','B'),30)))
errors$tv_time_range<-catch(prepare_cox_analysis_result(followup_data,'time','event','x',time_varying_covariate='x',time_varying_times='100'))
errors$adjusted_time_range<-catch(prepare_cox_analysis_result(followup_data,'time','event',c('x','group'),adjusted_group='group',adjusted_times='100'))
followup_data$start<-0;followup_data$id<-seq_len(nrow(followup_data))
followup_data$stratum<-factor(rep(1:3,20));followup_data$cluster<-rep(1:15,4)
errors$adjusted_start<-catch(prepare_cox_analysis_result(followup_data,'time','event',c('x','group'),start='start',stop='time',subject_id='id',adjusted_group='group'))
errors$adjusted_strata<-catch(prepare_cox_analysis_result(followup_data,'time','event',c('x','group'),strata='stratum',adjusted_group='group'))
errors$adjusted_cluster<-catch(prepare_cox_analysis_result(followup_data,'time','event',c('x','group'),cluster='cluster',adjusted_group='group'))
errors$adjusted_tv<-catch(prepare_cox_analysis_result(followup_data,'time','event',c('x','group'),time_varying_covariate='x',adjusted_group='group'))
callbacks<-list()
walk<-function(node) {
 if(!is.call(node)&&!is.expression(node))return()
 if(is.call(node)&&identical(node[[1]],as.name('tryCatch'))) {
  handler<-as.list(node)[-1L][['error']]
  if(!is.null(handler)&&grepl('survival_input_error_text',paste(deparse(handler),collapse=' '),fixed=TRUE))callbacks[[length(callbacks)+1L]]<<-handler
 }
 for(child in as.list(node))if(!missing(child)&&(is.call(child)||is.expression(child)))walk(child)
}
walk(parse('R/server_survival.R',encoding='UTF-8'));stopifnot(length(callbacks)==3L)
notices<-character();cleared<-FALSE
showNotification<-function(ui,...)notices<<-c(notices,as.character(ui))
km_result<-cox_result<-competing_result<-function(value){stopifnot(is.null(value));cleared<<-TRUE}
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 app_language_fn<-function()lang
 for(key in names(errors)) {
  error<-errors[[key]];stopifnot(inherits(error,'error'))
  expected<-statedu_t(paste0('survival.input_error.',key),lang)
  english<-statedu_t(paste0('survival.input_error.',key),'en')
  if(key %in% c('tv_time_range','adjusted_time_range')) {
   expected<-sprintf(expected,'60.25');english<-sprintf(english,'60.25')
  }
  stopifnot(conditionMessage(error)==english,survival_input_error_text(error,lang)==expected)
  if(startsWith(key,'ggplot_'))stopifnot(result_export_error_text(error,lang)==expected)
  for(callback in callbacks) {
   notices<-character();cleared<-FALSE;eval(callback)(error)
   stopifnot(cleared,length(notices)==1L,endsWith(notices[[1]],expected))
  }
 }
 external<-simpleError('External: D:/사용자 %s/자료.csv')
 stopifnot(identical(survival_input_error_text(external,lang),conditionMessage(external)))
 for(key in c('tv_time_range','adjusted_time_range'))for(value in c('1.20e+05','60,25')) {
  e<-simpleError(sprintf(statedu_t(paste0('survival.input_error.',key),'en'),value))
  stopifnot(survival_input_error_text(e,lang)==sprintf(statedu_t(paste0('survival.input_error.',key),lang),value))
 }
 custom<-catch(survival_parse_times_strict('bad','사용자 <&> %s'))
 stopifnot(identical(survival_input_error_text(custom,lang),conditionMessage(custom)))
 cat('PASS:',lang,length(errors),'actual validation errors; three actual failure callbacks; result reset; external detail preservation\n')
}
