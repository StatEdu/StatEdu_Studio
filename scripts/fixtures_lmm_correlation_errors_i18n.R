out<-'tmp/lmm-correlation-errors-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
capture<-function(...)tryCatch(sample_size_lmm_correlation_matrix(...),error=function(e)list(error=conditionMessage(e)))
errors<-list(capture(1,.3),capture(3,.3,'unstructured','0.3'),capture(4,.3,'unstructured','0.3,0.2,0.3'),capture(3,.3,'unstructured','1,0,0'),capture(3,.3,'unstructured','-1,0,0'),capture(3,.3,'unstructured','.9,.9,-.9'),capture(3,-.6,'exchangeable'))
keys<-paste0('sample_size.result.',c('error_time_points_min',rep('error_lmm_pair_count',2),rep('error_unstructured_range',2),'error_unstructured_pd','error_exchangeable_pd'))
before_errors<-serialize(errors,NULL)
for(lang in c('en','ko','ja','zh','es','fr','de','vi')){
 for(i in seq_along(errors)){
  expected<-statedu_t(keys[i],lang,fallback='');en<-statedu_t(keys[i],'en')
  if(i%in%c(2,3)){count<-c('3','6')[i-1];times<-c('3','4')[i-1];expected<-sprintf(expected,count,times);en<-sprintf(en,count,times)}
  stopifnot(identical(errors[[i]]$error,en),nzchar(expected))
  actual<-xml2::xml_text(xml2::xml_find_first(xml2::read_html(as.character(sample_size_results_ui(errors[[i]],lang))),'//div[@class="analysis-warning"]'))
  stopifnot(identical(actual,expected))
  if(lang!='en')stopifnot(actual!=en)
 }
 raw<-sprintf(statedu_t('sample_size.result.error_lmm_pair_count','en'),'006','004')
 stopifnot(identical(sample_size_result_text(raw,lang),sprintf(statedu_t('sample_size.result.error_lmm_pair_count',lang),'006','004')))
 for(unknown in c(paste0(raw,' custom'),sub('006','6.0',raw,fixed=TRUE)))stopifnot(identical(sample_size_result_text(unknown,lang),unknown))
}
stopifnot(identical(before_errors,serialize(errors,NULL)))
structures<-c('exchangeable','ar1','unstructured')
for(structure in structures){
 actual<-sample_size_lmm_correlation_matrix(3,.3,structure,'.3,.2,.3')
 expected<-if(structure=='exchangeable')matrix(.3,3,3)+diag(.7,3)else if(structure=='ar1').3^abs(outer(1:3,1:3,'-'))else matrix(c(1,.3,.2,.3,1,.3,.2,.3,1),3,3)
 stopifnot(isTRUE(all.equal(actual,expected)),all(eigen(actual,symmetric=TRUE)$values>0))
}
base<-list(sample_size_lmm_target='power',sample_size_lmm_alpha='.05',sample_size_lmm_power='.8',sample_size_lmm_n='30',sample_size_lmm_dropout='0',sample_size_lmm_mode='glimmpse',sample_size_lmm_design='one_group_repeated',sample_size_lmm_time_points='3',sample_size_lmm_simulations='20',sample_size_lmm_group1_means='0,.2,.4',sample_size_lmm_residual_sd='1',sample_size_lmm_rho='.3',sample_size_lmm_correlations='.3,.2,.3')
set.seed(915);seed_before<-.Random.seed
results<-lapply(structures,function(s)sample_size_calculate('lmm',modifyList(base,list(sample_size_lmm_correlation_structure=s))))
stopifnot(identical(seed_before,.Random.seed))
for(x in results)stopifnot(is.null(x$error),is.finite(x$power),x$power>=0,x$power<=1)
designs<-structures;formula_keys<-rep('sample_size.result.planning_lmm_gls',3)
cat('PASS seven actual matrix errors x eight languages, count slots and unchanged errors, three reference matrices and real LMM outputs\n')
