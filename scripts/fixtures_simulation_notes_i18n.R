out<-'tmp/simulation-notes-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
b<-list(sample_size_regression_design='mediation',sample_size_regression_target='power',sample_size_regression_alpha='0.05',sample_size_regression_power='0.8',sample_size_regression_n='100',sample_size_regression_ratio='1',sample_size_regression_alternative='two.sided',sample_size_regression_dropout='0',sample_size_regression_a='0.3',sample_size_regression_b='0.3',sample_size_regression_covariates='0',sample_size_regression_simulations='30',sample_size_regression_bootstraps='100')
l<-list(sample_size_lmm_target='power',sample_size_lmm_alpha='0.05',sample_size_lmm_power='0.8',sample_size_lmm_n='30',sample_size_lmm_dropout='0',sample_size_lmm_mode='simple',sample_size_lmm_design='one_group_repeated',sample_size_lmm_effect='0.5',sample_size_lmm_time_points='3',sample_size_lmm_icc='0.3',sample_size_lmm_simulations='20')
results<-c(lapply(c('sobel','monte_carlo','bootstrap'),function(d)sample_size_calculate('regression',modifyList(b,list(sample_size_regression_mediation_method=d)))),list(sample_size_calculate('lmm',l)))
designs<-c('sobel','monte_carlo','bootstrap','lme');formula_keys<-paste0('sample_size.result.',c('planning_mediation_sobel','planning_mediation_mc','planning_mediation_bootstrap','planning_lmm_lme'))
note_keys<-paste0('sample_size.result.',c('note_mediation_sobel','note_mediation_mc','note_bootstrap_counts','note_lmm_counts'))
raw<-serialize(results,NULL)
clean<-function(x)gsub('[[:space:]\u00a0]+','',x,perl=TRUE)
for(lang in c('en','ko','ja','zh','es','fr','de','vi')){
 for(i in 1:4){
  stopifnot(is.null(results[[i]]$error),is.finite(results[[i]]$power))
  expected<-statedu_t(note_keys[i],lang,fallback='')
  if(i==3)expected<-sprintf(expected,'30','100')
  if(i==4)expected<-sprintf(expected,'20')
  stopifnot(nzchar(expected),identical(sample_size_result_text(results[[i]]$method_note,lang),expected))
  rendered<-xml2::xml_text(xml2::read_html(as.character(sample_size_results_ui(results[[i]],lang))))
  parts<-trimws(strsplit(result_sci_note_text(estimation=expected),';',fixed=TRUE)[[1]])
  for(part in parts)stopifnot(grepl(clean(sub('[.。]$','',part)),clean(rendered),fixed=TRUE))
 }
 # Keep digit strings, including leading zeros, without numeric coercion.
 english<-sprintf(statedu_t(note_keys[3],'en'),'0030','0100')
 stopifnot(identical(sample_size_result_text(english,lang),sprintf(statedu_t(note_keys[3],lang),'0030','0100')))
 for(unknown in c(paste0(english,' extra'),paste0('Prefix ',english),'User %s <&> text'))stopifnot(identical(sample_size_result_text(unknown,lang),unknown))
 stopifnot(is.na(sample_size_result_text(NA_character_,lang)),identical(sample_size_result_text('',lang),''),is.null(sample_size_result_text(NULL,lang)))
}
stopifnot(identical(raw,serialize(results,NULL)))
cat('PASS four actual method notes x eight languages, rendered counts, strict boundaries, null/NA and source preservation\n')
