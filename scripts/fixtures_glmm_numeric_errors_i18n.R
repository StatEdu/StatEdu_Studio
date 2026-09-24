out<-'tmp/glmm-numeric-errors-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
base<-list(effect_size_glmm_binary_scale='coefficient',effect_size_glmm_count_scale='coefficient',effect_size_glmm_sd='2')
calc<-function(design,value,extra=list())suppressWarnings(effect_size_glmm_calculate(modifyList(modifyList(base,list(effect_size_glmm_design=design,effect_size_glmm_coefficient=value)),extra)))
input_designs<-c('binary_logit','count_log','continuous_gaussian')
input_keys<-paste0('sample_size.result.',c('error_glmm_logit_numeric','error_glmm_log_numeric','error_glmm_gaussian_numeric'))
errors<-list();keys<-character()
for(i in 1:3)for(value in c('bad','NaN','Inf','-Inf')){errors<-c(errors,list(calc(input_designs[i],value)));keys<-c(keys,input_keys[i])}
before_errors<-serialize(errors,NULL)
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(i in seq_along(errors)){
 stopifnot(identical(errors[[i]]$error,statedu_t(keys[i],'en')))
 expected<-statedu_t(keys[i],lang,fallback='')
 actual<-xml2::xml_text(xml2::xml_find_first(xml2::read_html(as.character(sample_size_results_ui(errors[[i]],lang))),'//div[@class="analysis-warning"]'))
 stopifnot(nzchar(expected),identical(actual,expected));if(lang!='en')stopifnot(actual!=errors[[i]]$error)
 unknown<-paste0(errors[[i]]$error,' custom');stopifnot(identical(sample_size_result_text(unknown,lang),unknown))
}
stopifnot(length(errors)==12L,identical(before_errors,serialize(errors,NULL)))
cases<-expand.grid(design=input_designs,B=c(-.5,0,.5),stringsAsFactors=FALSE)
results<-lapply(1:9,function(i)calc(cases$design[i],as.character(cases$B[i])))
for(i in 1:9){
 r<-results[[i]];b<-cases$B[i];d<-cases$design[i]
 stopifnot(is.null(r$error),isTRUE(all.equal(r$fixed_effect_coefficient,b)))
 expected<-if(d=='continuous_gaussian')b/2 else exp(b)
 stopifnot(isTRUE(all.equal(r$primary_effect_size,expected)))
 if(d=='binary_logit')stopifnot(isTRUE(all.equal(r$effect_size_d,b*sqrt(3)/pi)))
}
# Alternative ratio inputs should not validate an unused coefficient field.
alternates<-list(calc('binary_logit','bad',list(effect_size_glmm_binary_scale='odds_ratio',effect_size_glmm_or='1')),calc('count_log','bad',list(effect_size_glmm_count_scale='incidence_rate_ratio',effect_size_glmm_irr='1')))
for(r in alternates)stopifnot(is.null(r$error),r$primary_effect_size==1,r$fixed_effect_coefficient==0)
designs<-paste(cases$design,cases$B,sep='-')
formula_keys<-rep(paste0('sample_size.result.',c('note_glmm_logit','note_glmm_log','note_glmm_identity')),3)
cat('PASS twelve actual GLMM numeric errors x eight languages; nine signed/zero references and two alternative-input controls\n')
