# Actual calculator results for the shared five-format validation harness.
out<-'tmp/rank-regression-formula-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
n<-list(effect_size_nonparametric_u='700',effect_size_nonparametric_n1='50',effect_size_nonparametric_n2='50',effect_size_nonparametric_w_positive='350',effect_size_nonparametric_w_negative='150',effect_size_nonparametric_h='10',effect_size_nonparametric_n='90',effect_size_nonparametric_groups='3',effect_size_nonparametric_chi_square='12',effect_size_nonparametric_measurements='3')
m<-list(effect_size_mcnemar_b='0',effect_size_mcnemar_c='10',effect_size_mcnemar_p01='0.20',effect_size_mcnemar_p10='0.10')
r<-list(effect_size_regression_full_r2='0.25',effect_size_regression_reduced_r2='0.10',effect_size_regression_or='1.8',effect_size_regression_delta_r2='0.05')
designs<-c('rank_biserial_from_u','rank_biserial_paired','kruskal_epsilon','friedman_w','matched_or_counts','cohen_g','matched_or_probs','hierarchical_f2','logistic_or','moderation_f2')
results<-c(lapply(designs[1:4],function(design)effect_size_nonparametric_calculate(modifyList(n,list(effect_size_nonparametric_design=design)))),
 lapply(designs[5:7],function(design)effect_size_mcnemar_calculate(modifyList(m,list(effect_size_mcnemar_design=design)))),
 lapply(designs[8:10],function(design)effect_size_regression_calculate(modifyList(r,list(effect_size_regression_design=design)))))
formula_keys<-paste0('sample_size.result.',c('rank_biserial','paired_rank','epsilon','kendall','matched_counts','matched_g','matched_probs','incremental_f2','logistic_d','interaction_f2'))
tokens<-list('2U / (n1 n2) - 1','(W+ - W-) / (W+ + W-)','(H - k + 1) / (N - k)','Friedman chi-square / [N * (m - 1)]',c('b / c','0.5'),'p01 / (p01 + p10) - 0.5',c('p01 / p10','log(p01 / p10)'),'(R2_full - R2_reduced) / (1 - R2_full)',c('log(OR)','log(OR) * sqrt(3) / pi'),'delta R-squared / (1 - delta R-squared)')
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(i in seq_along(tokens)) {
 value<-statedu_t(formula_keys[[i]],lang,fallback='')
 stopifnot(nzchar(value),all(vapply(tokens[[i]],grepl,logical(1),x=value,fixed=TRUE)))
}
for(i in seq_along(results))stopifnot(is.null(results[[i]]$error),results[[i]]$formula_note==statedu_t(formula_keys[[i]],'en'))
stopifnot(isTRUE(all.equal(results[[5]]$odds_ratio,0.5/10.5)),isTRUE(all.equal(results[[8]]$f_squared,0.15/0.75)))
cat('PASS 10 actual rank/McNemar/regression branches; formula tokens x 8 languages and 0.5 correction\n')
