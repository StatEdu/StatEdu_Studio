source('scripts/fixtures_lmm_correlation_errors_i18n.R',encoding='UTF-8')
out<-'tmp/lmm-mean-lengths-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
effect_base<-list(effect_size_lmm_design='glimmpse',effect_size_lmm_lmm_design='two_group_repeated',effect_size_lmm_group1_means='0,.2,.4',effect_size_lmm_group2_means='0,.1,.8',effect_size_lmm_residual_sd='1',effect_size_lmm_rho='.3')
errors<-list();error_keys<-character()
add_error<-function(value,key){errors[[length(errors)+1L]]<<-value;error_keys<<-c(error_keys,paste0('sample_size.result.',key))}
for(design in c('one_group_repeated','two_group_repeated')){
 add_error(effect_size_lmm_calculate(modifyList(effect_base,list(effect_size_lmm_lmm_design=design,effect_size_lmm_group1_means='0'))),'error_lmm_two_times')
 add_error(sample_size_calculate('lmm',modifyList(base,list(sample_size_lmm_design=design,sample_size_lmm_group1_means='0'))),'error_lmm_group1_times')
}
for(value in c('0,.2','0,.2,.4,.6')){
 add_error(effect_size_lmm_calculate(modifyList(effect_base,list(effect_size_lmm_group2_means=value))),'error_lmm_equal_lengths')
 add_error(sample_size_calculate('lmm',modifyList(base,list(sample_size_lmm_design='two_group_repeated',sample_size_lmm_group2_means=value))),'error_lmm_group2_times')
}
before_errors<-serialize(errors,NULL)
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(i in seq_along(errors)){
 stopifnot(identical(errors[[i]]$error,statedu_t(error_keys[i],'en')))
 expected<-statedu_t(error_keys[i],lang,fallback='')
 actual<-xml2::xml_text(xml2::xml_find_first(xml2::read_html(as.character(sample_size_results_ui(errors[[i]],lang))),'//div[@class="analysis-warning"]'))
 stopifnot(nzchar(expected),identical(actual,expected))
 if(lang!='en')stopifnot(actual!=errors[[i]]$error)
 unknown<-paste0(errors[[i]]$error,' custom');stopifnot(identical(sample_size_result_text(unknown,lang),unknown))
}
stopifnot(length(errors)==8L,identical(before_errors,serialize(errors,NULL)))
# Two-time-point valid controls through both effect and planning wrappers.
effect_results<-lapply(c('one_group_repeated','two_group_repeated'),function(d)effect_size_lmm_calculate(modifyList(effect_base,list(effect_size_lmm_lmm_design=d,effect_size_lmm_group1_means='0,.4',effect_size_lmm_group2_means='0,.8'))))
for(x in effect_results)stopifnot(is.null(x$error),isTRUE(all.equal(x$primary_effect_size,.4)))
plan<-sample_size_calculate('lmm',modifyList(base,list(sample_size_lmm_design='two_group_repeated',sample_size_lmm_group1_means='0,.4',sample_size_lmm_group2_means='0,.8')))
stopifnot(is.null(plan$error),is.finite(plan$power),plan$power>=0,plan$power<=1)
results<-c(results,effect_results,list(plan));designs<-c(designs,'one-group-effect','two-group-effect','two-group-power');formula_keys<-c(formula_keys,rep('sample_size.result.lmm_glimmpse',2),'sample_size.result.planning_lmm_gls')
cat('PASS eight actual mean-list errors x eight languages; two effect references and valid two-time-point LMM power\n')
