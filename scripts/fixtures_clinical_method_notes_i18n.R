source('scripts/fixtures_clinical_planning_i18n.R',encoding='UTF-8')
out <- 'tmp/clinical-method-notes-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
pick <- c(1:5,8,9:13,16)
results <- results[pick];designs <- designs[pick]
formula_keys <- rep(paste0('sample_size.result.',c('note_plan_survival','note_plan_tost_exact','note_plan_tost_normal','note_plan_ni_normal','note_plan_ni_normal','note_plan_auc')),2)
for(i in seq_along(results))stopifnot(is.null(results[[i]]$error),identical(results[[i]]$method_note,statedu_t(formula_keys[i],'en')))
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))stopifnot(grepl('Schoenfeld',statedu_t(formula_keys[1],lang),fixed=TRUE),grepl('TOSTER::power_t_TOST',statedu_t(formula_keys[2],lang),fixed=TRUE))
cat('PASS twelve survival/equivalence/AUC method-note snapshots and eight-language engine tokens; Buderer precision notes excluded\n')
