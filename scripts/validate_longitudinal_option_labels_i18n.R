Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
groups<-list(model=longitudinal_model_choices(),family=longitudinal_family_choices(),correlation=longitudinal_correlation_choices(),
 trim=longitudinal_weight_trim_choices(),mi=longitudinal_mi_outcome_choices(),
 lmm=c('Random effects (ML)'='exchangeable','Repeated UN (REML)'='reml_un','Repeated AR(1) (REML)'='reml_ar1'))
for(model in c('gee','lmm','glmm','panel_fe','panel_re')) {
 groups[[paste0(model,'_weights')]]<-longitudinal_weight_type_choices(model,TRUE)
 groups[[paste0(model,'_missing')]]<-longitudinal_missing_strategy_choices(model)
}
missing<-character()
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(group in names(groups)) {
 choices<-groups[[group]];localized<-longitudinal_ui_choices(choices,lang)
 stopifnot(identical(unname(choices),unname(localized)))
 if(lang!='en')for(i in seq_along(choices))if(names(choices)[i]==names(localized)[i] && names(choices)[i]!='AR(1)')missing<-unique(c(missing,paste(lang,names(choices)[i])))
}
if(length(missing)) {writeLines(missing,'tmp/longitudinal-options-missing.txt',useBytes=TRUE);stop('Untranslated options: tmp/longitudinal-options-missing.txt')}
cat('PASS',length(groups),'option groups in 8 languages; values preserved\n')
info<-data.frame(name=c('id','time','y','x'),measurement='continuous',var_label=c('사용자 ID','시점','결과','Warning'))
variants<-list(gee='unstructured_adjusted',lmm='exchangeable',lmm='reml_un',lmm='reml_ar1',glmm='exchangeable',panel_fe='exchangeable',panel_re='exchangeable')
for(i in seq_along(variants))for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 state<-longitudinal_setup_state(info$name,info,outcome='y',id='id',time='time',predictors='x',model_type=names(variants)[i],corstr=variants[[i]],language=lang)
 if(lang=='en')baseline<-state else for(key in c('outcome','id','time','predictors','model_type','family','corstr','random_slope','missing_strategy','weight_type'))stopifnot(identical(state[[key]],baseline[[key]]))
 doc<-xml2::read_html(as.character(longitudinal_setup_panel(state)),encoding='UTF-8')
 title<-longitudinal_independent_variables_label(1,lang)
 stopifnot(grepl(title,xml2::xml_text(doc),fixed=TRUE))
 if(lang!='en')stopifnot(title!='Independent variables (1)')
 for(field in c('model_type','family','corstr')) {
  select<-xml2::xml_find_all(doc,paste0('//select[@id="longitudinal_',field,'"]'))
  if(length(select)) {
   selected<-xml2::xml_attr(xml2::xml_find_all(select,'.//option[@selected]'),'value')
   stopifnot(length(selected)==1L,identical(selected,state[[field]]))
  }
 }
}
cat('PASS 56 rendered setup states; selected values and assignments preserved\n')
