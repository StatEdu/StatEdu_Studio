out<-'tmp/model-planning-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
methods<-c(rep('anova',6),rep('ancova',3),rep('regression',4))
designs<-c('one_way','two_way','repeated_one_group','mixed_repeated','kruskal_wallis','friedman','ancova','ranked_ancova','manova','multiple','hierarchical','moderation','logistic')
formula_keys<-paste0('sample_size.result.',c(rep('planning_anova_f',4),rep('planning_anova_rank',2),'planning_ancova','planning_rank_ancova','planning_manova',rep('planning_regression_f2',3),'planning_logistic'))
inputs<-lapply(seq_along(methods),function(i){
 m<-methods[i];v<-list(target='sample_size',alpha='0.05',power='0.8',n='150',ratio='1',alternative='two.sided',dropout='0',design=designs[i],effect='0.15',groups='3',measurements='3',factor_a='2',factor_b='3',effect_test='interaction',correlation='0.5',epsilon='0.8',outcomes='2',covariates='1',covariate_r2='0.2',predictors='3',tested='1',total_predictors='4',interactions='1',or='1.8',p0='0.3',predictor_prevalence='0.5')
 setNames(v,paste0('sample_size_',m,'_',names(v)))
})
results<-lapply(seq_along(methods),function(i)sample_size_calculate(methods[i],inputs[[i]]))
power_results<-lapply(seq_along(methods),function(i){v<-inputs[[i]];v[[paste0('sample_size_',methods[i],'_target')]]<-'power';sample_size_calculate(methods[i],v)})
for(i in seq_along(results))for(r in list(results[[i]],power_results[[i]]))if(!is.null(r$error)||!identical(r$formula_note,statedu_t(formula_keys[i],'en')))stop('Unexpected result: ',designs[i],' ',r$error,' / ',r$formula_note)
for(r in power_results)stopifnot(is.finite(r$power),r$power>=0,r$power<=1)
for(lang in c('en','ko','ja','zh','es','fr','de','vi')){
 stopifnot(grepl('f2 = V / (1 - V)',statedu_t('sample_size.result.planning_manova',lang),fixed=TRUE),grepl('f_adjusted = f / sqrt(1 - R2)',statedu_t('sample_size.result.planning_ancova',lang),fixed=TRUE))
 for(i in seq_along(power_results)){
  expected<-sample_size_result_text(power_results[[i]]$formula_note,lang)
  stopifnot(identical(expected,statedu_t(formula_keys[i],lang)),nzchar(expected))
  rendered<-xml2::xml_text(xml2::read_html(as.character(sample_size_results_ui(power_results[[i]],lang))))
  parts<-trimws(strsplit(result_sci_note_text(estimation=expected),';',fixed=TRUE)[[1]])
  clean<-function(x)gsub('[[:space:]\u00a0]+','',x,perl=TRUE)
  for(part in parts)stopifnot(grepl(clean(sub('[.。]$','',part)),clean(rendered),fixed=TRUE))
 }
}
# Also export achieved-power snapshots for the seven distinct descriptions.
pick<-c(1,5,7,8,9,10,13)
results<-c(results,power_results[pick]);designs<-c(designs,paste0(designs[pick],'-power'));formula_keys<-c(formula_keys,formula_keys[pick])
cat('PASS 13 sample-size + 13 achieved-power cases, eight-language rendering; legacy ANOVA rank branches are not current menu options\n')
