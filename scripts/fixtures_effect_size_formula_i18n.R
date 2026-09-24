# Loaded by validate_sample_size_result_i18n.R after the application bootstrap.
out<-'tmp/effect-size-formula-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
p<-list(effect_size_proportion_p1='0.50',effect_size_proportion_p2='0.25',effect_size_proportion_event1='0',effect_size_proportion_nonevent1='50',effect_size_proportion_event2='10',effect_size_proportion_nonevent2='40')
corr<-list(effect_size_correlation_r='0.25',effect_size_correlation_f='6.25',effect_size_correlation_df='98',effect_size_correlation_r2='0.09')
designs<-c('risk_difference','risk_ratio','odds_ratio_table','point_biserial','r_from_f','r_from_r2')
results<-c(lapply(designs[1:3],function(design)effect_size_proportion_calculate(modifyList(p,list(effect_size_proportion_design=design)))),
 lapply(designs[4:6],function(design)effect_size_correlation_calculate(modifyList(corr,list(effect_size_correlation_design=design)))))
formula_keys<-paste0('sample_size.result.',c('risk_difference','risk_ratio','odds_ratio','point_biserial','correlation_f','correlation_r2'))
tokens<-c('p1 - p2','p1 / p2','[p1 / (1 - p1)] / [p2 / (1 - p2)]','d = 2r / sqrt(1 - r^2)','r = sqrt(F / (F + df_error))','r = sqrt(R-squared)')
for(language in c('en','ko','ja','zh','es','fr','de','vi'))for(i in seq_along(tokens))stopifnot(grepl(tokens[[i]],statedu_t(formula_keys[[i]],language),fixed=TRUE))
stopifnot(isTRUE(all.equal(results[[3]]$primary_effect_size,(0.5/50.5)/(10.5/40.5))))
chi<-list(effect_size_chisquare_statistic='12',effect_size_chisquare_n='120',effect_size_chisquare_rows='2',effect_size_chisquare_columns='3',effect_size_chisquare_observed='0.20, 0.50, 0.30',effect_size_chisquare_expected='0.33, 0.33, 0.34')
for(design in c('cohens_w','cramers_v','phi','cohens_w_from_probs')) {
 result<-effect_size_chisquare_calculate(modifyList(chi,list(effect_size_chisquare_design=design)))
 stopifnot(is.null(result$error))
 for(language in c('en','ko','ja','zh','es','fr','de','vi'))stopifnot(identical(sample_size_result_text(result$formula_note,language),result$formula_note))
}
cat('PASS mathematical tokens in 6 translated formulas, zero-cell correction and 4 unchanged chi-square formulas\n')
