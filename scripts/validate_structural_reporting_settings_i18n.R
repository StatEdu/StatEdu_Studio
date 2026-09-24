Sys.setlocale('LC_CTYPE','English_United States.utf8')
source('scripts/validate_structural_quality_summary_i18n.R',encoding='UTF-8')
settings_out<-'tmp/structural-reporting-settings-i18n';dir.create(settings_out,recursive=TRUE,showWarnings=FALSE)
settings_entries<-list()
cfa_settings<-cfa
cfa_settings$reliability_bootstrap<-101L;cfa_settings$reliability_seed<-1201L
cfa_settings$htmt_bootstrap<-102L;cfa_settings$htmt_seed<-1202L;cfa_settings$htmt_ci_method<-'percentile'
cfa_settings$bollen_stine_bootstrap<-103L;cfa_settings$bollen_stine_seed<-1203L
cfa_settings$effect_bootstrap<-104L;cfa_settings$effect_bootstrap_seed<-1204L
cfa_settings$invariance_enabled<-TRUE;cfa_settings$invariance_group<-'Requested 사용자 <&>'
cfa_settings$mi_holdout_enabled<-TRUE;cfa_settings$mi_holdout_fraction<-.35;cfa_settings$mi_holdout_seed<-1301L
cfa_settings$common_method_enabled<-TRUE;cfa_settings$common_method_methods<-c('harman','single_factor_cfa','common_latent_factor')
cfa_settings$ordered<-c('Requested','None recorded','Normality 사용자')
pls_settings<-pls;pls_settings$pls_bootstrap<-200L;pls_settings$pls_seed<-1401L
pls_settings$pls_bootstrap_result<-list(nboot=180L,valid_ratio=.9,bootstrap_status='Adequate')
pls_settings$pls_predict_folds<-5L;pls_settings$pls_predict_reps<-7L;pls_settings$pls_predict_seed<-1402L
pls_settings$invariance_result<-list(type='pls_micom');pls_settings$invariance_group<-'Review Requested <&>'
pls_executed<-pls_settings
pls_executed$pls_predict_result<-list(folds=6L,reps=8L,seed=1403L)
pls_executed$pls_bootstrap_result$rng<-'Requested RNG 사용자 <&>'
pls_executed$mi_holdout_result<-list();pls_executed$converged<-FALSE;pls_executed$admissible<-FALSE
fixtures<-list(cfa=cfa_settings,pls_requested=pls_settings,pls_executed=pls_executed,pls_none=pls)
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 htmls<-list()
 for(name in names(fixtures)){
  bundle<-fixtures[[name]];type<-if(name=='cfa')'cfa' else 'plssem'
  values<-structural_canvas_reporting_settings_display(bundle,type,language)
  html<-as.character(structural_canvas_reporting_context_result_ui(bundle,type,language));doc<-read(html)
  table<-xml2::xml_find_all(doc,'//table')[[1]]
  rendered_values<-trimws(xml2::xml_text(xml2::xml_find_all(table,'.//tbody/tr/td[2]')))
  if(language!='en')stopifnot(all(unname(values) %in% rendered_values))
  if(language=='en')stopifnot(identical(rendered_values,structural_canvas_reporting_context_rows(bundle,type)$Value))
  if(name=='cfa'){
   for(value in c('101','102','103','104','1201','1202','1203','1204'))stopifnot(grepl(value,values[['Bootstrap settings']],fixed=TRUE))
   stopifnot(grepl(bundle$invariance_group,values[['Group analysis']],fixed=TRUE))
   stopifnot(paste(bundle$ordered,collapse=', ') %in% rendered_values)
  }
  if(name=='pls_requested'){
   for(value in c('200','1401','180/200','90%','80%'))stopifnot(grepl(value,values[['Bootstrap settings']],fixed=TRUE))
   stopifnot(grepl(bundle$invariance_group,values[['Group analysis']],fixed=TRUE))
  }
  if(name=='pls_executed')stopifnot(grepl(bundle$pls_bootstrap_result$rng,values[['Bootstrap settings']],fixed=TRUE))
  if(language!='en'){
   stopifnot(!any(grepl('(^|[;:,] )(seed|folds|reps|fraction|converged|admissible|whole-draw minimum)=',values)))
   stopifnot(!any(grepl('{',values,fixed=TRUE)))
  }
  htmls[[name]]<-html
  if(language=='ja')settings_entries[[name]]<-list(id=name,title=name,html=html)
 }
 writeLines(paste(unlist(htmls),collapse='\n'),file.path(settings_out,paste0(language,'-settings.html')),useBytes=TRUE)
 cat('PASS:',language,'CFA/PLS bootstrap, requested/executed/absent settings, numbers and literal group/RNG names\n')
}
saveRDS(unname(settings_entries),file.path(settings_out,'entries.rds'))
