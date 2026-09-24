out<-'tmp/factor-friedman-errors-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
base<-list(sample_size_anova_target='power',sample_size_anova_alpha='.05',sample_size_anova_power='.8',sample_size_anova_n='150',sample_size_anova_dropout='0',sample_size_anova_effect='.15',sample_size_anova_groups='3',sample_size_anova_measurements='3',sample_size_anova_correlation='.5',sample_size_anova_epsilon='1',sample_size_anova_factor_a='2',sample_size_anova_factor_b='3',sample_size_anova_effect_test='interaction')
calc<-function(design,extra=list())suppressWarnings(sample_size_calculate('anova',modifyList(modifyList(base,list(sample_size_anova_design=design)),extra)))
errors<-list();keys<-character()
for(f in c('factor_a','factor_b'))for(v in c('0','1','NaN','Inf')){errors<-c(errors,list(calc('two_way',setNames(list(v),paste0('sample_size_anova_',f)))));keys<-c(keys,'sample_size.result.error_anova_factor_levels')}
for(v in c('1','2','NaN','Inf')){errors<-c(errors,list(calc('friedman',list(sample_size_anova_measurements=v))));keys<-c(keys,'sample_size.result.error_friedman_measurement_count')}
# Nonpositive/nonfinite effect sizes are intercepted by the shared positive validator.
for(v in c('1.1','2')){errors<-c(errors,list(calc('friedman',list(sample_size_anova_effect=v))));keys<-c(keys,'sample_size.result.error_friedman_w_range')}
before_errors<-serialize(errors,NULL)
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(i in seq_along(errors)){
 stopifnot(identical(errors[[i]]$error,statedu_t(keys[i],'en')))
 expected<-statedu_t(keys[i],lang,fallback='')
 actual<-xml2::xml_text(xml2::xml_find_first(xml2::read_html(as.character(sample_size_results_ui(errors[[i]],lang))),'//div[@class="analysis-warning"]'))
 stopifnot(nzchar(expected),identical(actual,expected));if(lang!='en')stopifnot(actual!=errors[[i]]$error)
 unknown<-paste0(errors[[i]]$error,' custom');stopifnot(identical(sample_size_result_text(unknown,lang),unknown))
}
stopifnot(length(errors)==14L,identical(before_errors,serialize(errors,NULL)))
effects<-c('main_a','main_b','interaction')
results<-lapply(effects,function(e)calc('two_way',list(sample_size_anova_effect_test=e)))
for(i in 1:3){df1<-c(1,2,2)[i];stopifnot(is.null(results[[i]]$error),isTRUE(all.equal(results[[i]]$power,pf(qf(.95,df1,144),df1,144,ncp=150*.15^2,lower.tail=FALSE))))}
friedman<-lapply(c(.15,1),function(w)calc('friedman',list(sample_size_anova_effect=as.character(w))))
for(i in 1:2)stopifnot(is.null(friedman[[i]]$error),isTRUE(all.equal(friedman[[i]]$power,pchisq(qchisq(.95,2),2,ncp=150*2*c(.15,1)[i],lower.tail=FALSE))))
results<-c(results,friedman);designs<-c(effects,'friedman-.15','friedman-1')
formula_keys<-paste0('sample_size.result.',c(rep('planning_anova_f',3),rep('planning_anova_rank',2)))
cat('PASS fourteen actual factorial/Friedman errors x eight languages; five independent F/chi-square references\n')
