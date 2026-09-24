# Reuse established real calculations, then verify method prose rather than formula prose.
source('scripts/fixtures_design_formula_i18n.R',encoding='UTF-8')
results <- results[4:13]; designs <- designs[4:13]
out <- 'tmp/reliability-sem-notes-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
formula_keys <- paste0('sample_size.result.',c('precision_mean','precision_proportion','precision_correlation','reliability_alpha','reliability_icc','reliability_kappa','note_agreement_approx','note_sem_rmsea','note_sem_parameter','sem_complexity'))
for(i in seq_along(results))stopifnot(is.null(results[[i]]$error),identical(results[[i]]$method_note,statedu_t(formula_keys[i],'en')))
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 stopifnot(grepl('mean difference +/- 1.96 * SD of paired differences',statedu_t(formula_keys[7],lang),fixed=TRUE),grepl('df * (RMSEA_alt^2 - RMSEA_null^2)',statedu_t(formula_keys[8],lang),fixed=TRUE),grepl('atanh(parameter)',statedu_t(formula_keys[9],lang),fixed=TRUE))
}
cat('PASS ten precision/reliability/SEM method descriptions: seven UI wrappers and three core-only reliability cases, eight-language expressions\n')
