out<-'tmp/ancova-planning-errors-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
base<-list(sample_size_ancova_design='ancova',sample_size_ancova_target='power',sample_size_ancova_alpha='.05',sample_size_ancova_power='.8',sample_size_ancova_n='150',sample_size_ancova_dropout='0',sample_size_ancova_effect='.15',sample_size_ancova_groups='3',sample_size_ancova_outcomes='2',sample_size_ancova_covariates='0',sample_size_ancova_covariate_r2='.2')
calc<-function(design,field,value)suppressWarnings(sample_size_calculate('ancova',modifyList(base,setNames(list(design,value),c('sample_size_ancova_design',paste0('sample_size_ancova_',field))))))
errors<-list();keys<-character()
for(design in c('ancova','ranked_ancova','manova'))for(value in c('-1','NaN','Inf')){
 errors<-c(errors,list(calc(design,'covariates',value)));keys<-c(keys,'sample_size.result.error_ancova_covariates')
}
for(value in c('1','0','NaN','Inf')){errors<-c(errors,list(calc('manova','outcomes',value)));keys<-c(keys,'sample_size.result.error_manova_outcomes')}
# Zero/negative/nonfinite effects are intercepted by the shared positive-effect validator.
for(value in c('1','1.1')){errors<-c(errors,list(calc('manova','effect',value)));keys<-c(keys,'sample_size.result.error_manova_planning_pillai')}
before_errors<-serialize(errors,NULL)
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(i in seq_along(errors)){
 stopifnot(identical(errors[[i]]$error,statedu_t(keys[i],'en')))
 expected<-statedu_t(keys[i],lang,fallback='')
 actual<-xml2::xml_text(xml2::xml_find_first(xml2::read_html(as.character(sample_size_results_ui(errors[[i]],lang))),'//div[@class="analysis-warning"]'))
 stopifnot(nzchar(expected),identical(actual,expected));if(lang!='en')stopifnot(actual!=errors[[i]]$error)
 unknown<-paste0(errors[[i]]$error,' custom');stopifnot(identical(sample_size_result_text(unknown,lang),unknown))
}
stopifnot(length(errors)==15L,identical(before_errors,serialize(errors,NULL)))
cases<-expand.grid(design=c('ancova','ranked_ancova','manova'),target=c('sample_size','power'),stringsAsFactors=FALSE)
results<-lapply(1:6,function(i)calc(cases$design[i],'target',cases$target[i]))
reference<-function(n,design){
 if(design=='manova'){df1<-4;df2<-n-5;lambda<-n*.15/.85}else{df1<-2;df2<-n-3;lambda<-n*.15^2/.8*if(design=='ranked_ancova').955 else 1}
 pf(qf(.95,df1,df2),df1,df2,ncp=lambda,lower.tail=FALSE)
}
for(i in 1:6){
 r<-results[[i]];stopifnot(is.null(r$error))
 if(i<=3)stopifnot(isTRUE(all.equal(r$estimated_power,reference(r$total,cases$design[i]))),reference(r$total,cases$design[i])>=.8,reference(r$total-3,cases$design[i])<.8)
 else stopifnot(isTRUE(all.equal(r$power,reference(150,cases$design[i]))))
}
designs<-paste(cases$design,cases$target,sep='-')
formula_keys<-rep(paste0('sample_size.result.',c('planning_ancova','planning_rank_ancova','planning_manova')),2)
cat('PASS fifteen actual planning errors x eight languages; six zero-covariate calculations checked against noncentral-F references\n')
