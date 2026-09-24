# Loaded by the shared result/export validation harness.
out<-'tmp/anova-formula-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
a<-list(effect_size_anova_eta2='0.06',effect_size_anova_partial_eta2='0.08',effect_size_anova_f='4.5',effect_size_anova_groups='3',effect_size_anova_total_n='90')
b<-list(effect_size_ancova_f='0.25',effect_size_ancova_covariate_r2='0.30',effect_size_ancova_partial_eta2='0.08',effect_size_ancova_pillai='0.10',effect_size_ancova_wilks='0.90',effect_size_ancova_dependent_variables='2',effect_size_ancova_f_statistic='4.5',effect_size_ancova_groups='3',effect_size_ancova_total_n='90')
designs<-c('partial_eta_from_f','omega_from_f','f_from_eta2','ancova_adjusted_f','manova_pillai','manova_wilks','ancova_partial_eta_from_f')
results<-c(lapply(designs[1:3],function(design)effect_size_anova_calculate(modifyList(a,list(effect_size_anova_design=design)))),
 lapply(designs[4:7],function(design)effect_size_ancova_calculate(modifyList(b,list(effect_size_ancova_design=design)))))
formula_keys<-paste0('sample_size.result.',c('anova_eta','anova_omega','anova_f','ancova_adjusted','manova_pillai','manova_wilks','ancova_eta'))
tokens<-list(
 c('df_effect = groups - 1','df_error = total N - groups','F * df_effect / (F * df_effect + df_error)'),
 c('df_effect = groups - 1','df_error = total N - groups','(F * df_effect - df_effect) / (F * df_effect + df_error + 1)'),
 'sqrt(eta-squared / [1 - eta-squared])',
 'unadjusted f / sqrt(1 - covariate R-squared)',
 c("f2 = Pillai's V / (1 - Pillai's V)",'f = sqrt(f2)'),
 c('s = min(number of dependent variables, groups - 1)','eta2 = 1 - lambda^(1/s)','f2 = eta2 / (1 - eta2)','f = sqrt(f2)'),
 c('df_effect = groups - 1','df_error = total N - groups','F * df_effect / (F * df_effect + df_error)','sqrt(partial eta-squared / [1 - partial eta-squared])'))
for(language in c('en','ko','ja','zh','es','fr','de','vi'))for(i in seq_along(tokens)) {
 text<-statedu_t(formula_keys[[i]],language,fallback='')
 stopifnot(nzchar(text),all(vapply(tokens[[i]],grepl,logical(1),x=text,fixed=TRUE)))
}
for(i in seq_along(results))stopifnot(is.null(results[[i]]$error),results[[i]]$formula_note==statedu_t(formula_keys[[i]],'en'))
cat('PASS 7 actual ANOVA/ANCOVA/MANOVA formula branches and mathematical tokens x 8 languages\n')
