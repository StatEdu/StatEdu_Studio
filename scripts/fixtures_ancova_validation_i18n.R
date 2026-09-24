out<-'tmp/ancova-validation-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
base<-list(effect_size_ancova_f='.25',effect_size_ancova_covariate_r2='.2',effect_size_ancova_pillai='.1',effect_size_ancova_wilks='.9',effect_size_ancova_dependent_variables='2',effect_size_ancova_groups='3')
calc<-function(design,field,value)effect_size_ancova_calculate(modifyList(base,setNames(list(design,value),c('effect_size_ancova_design',paste0('effect_size_ancova_',field)))))
plan<-list(sample_size_ancova_design='ancova',sample_size_ancova_target='power',sample_size_ancova_alpha='.05',sample_size_ancova_power='.8',sample_size_ancova_n='150',sample_size_ancova_dropout='0',sample_size_ancova_effect='.25',sample_size_ancova_groups='3',sample_size_ancova_covariates='1',sample_size_ancova_covariate_r2='.2')
errors<-list();keys<-character()
for(value in c('-.1','1','NaN','Inf')){
 errors<-c(errors,list(calc('ancova_adjusted_f','covariate_r2',value),sample_size_calculate('ancova',modifyList(plan,list(sample_size_ancova_covariate_r2=value)))))
 keys<-c(keys,rep('sample_size.result.error_ancova_covariate_r2',2))
}
for(value in c('0','-1','NaN','Inf')){errors<-c(errors,list(calc('manova_wilks','dependent_variables',value)));keys<-c(keys,'sample_size.result.error_manova_dependents')}
for(field in c('pillai','wilks'))for(value in c('0','1','-.1','NaN','Inf')){
 errors<-c(errors,list(calc(paste0('manova_',field),field,value)));keys<-c(keys,paste0('sample_size.result.error_',field,'_range'))
}
before_errors<-serialize(errors,NULL)
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(i in seq_along(errors)){
 stopifnot(identical(errors[[i]]$error,statedu_t(keys[i],'en')))
 expected<-statedu_t(keys[i],lang,fallback='')
 actual<-xml2::xml_text(xml2::xml_find_first(xml2::read_html(as.character(sample_size_results_ui(errors[[i]],lang))),'//div[@class="analysis-warning"]'))
 stopifnot(nzchar(expected),identical(actual,expected));if(lang!='en')stopifnot(actual!=errors[[i]]$error)
 unknown<-paste0(errors[[i]]$error,' custom');stopifnot(identical(sample_size_result_text(unknown,lang),unknown))
}
stopifnot(length(errors)==22L,identical(before_errors,serialize(errors,NULL)))
adjusted<-lapply(c(0,.2),function(v)calc('ancova_adjusted_f','covariate_r2',as.character(v)))
for(i in 1:2)stopifnot(is.null(adjusted[[i]]$error),isTRUE(all.equal(adjusted[[i]]$cohen_f,.25/sqrt(1-c(0,.2)[i]))))
results<-list(calc('manova_pillai','pillai','.1'),calc('manova_wilks','dependent_variables','1'),calc('manova_wilks','dependent_variables','2'),sample_size_calculate('ancova',modifyList(plan,list(sample_size_ancova_covariate_r2='0'))))
stopifnot(isTRUE(all.equal(results[[1]]$f_squared,1/9)),isTRUE(all.equal(results[[2]]$f_squared,1/9)),isTRUE(all.equal(results[[3]]$f_squared,.9^(-.5)-1)),isTRUE(all.equal(results[[4]]$power,pf(qf(.95,2,146),2,146,ncp=150*.25^2,lower.tail=FALSE))))
designs<-c('pillai','wilks-one-dependent','wilks-two-dependents','ancova-zero-r2')
formula_keys<-paste0('sample_size.result.',c('note_pillai','note_wilks','note_wilks','planning_ancova'))
results<-c(results,adjusted);designs<-c(designs,'adjusted-zero-r2','adjusted-positive-r2');formula_keys<-c(formula_keys,rep('sample_size.result.ancova_adjusted',2))
cat('PASS twenty-two ANCOVA/MANOVA errors x eight languages and six independent valid calculation references\n')
