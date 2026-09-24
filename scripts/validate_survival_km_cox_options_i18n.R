Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
variables<-c('Review','Normality','None','사용자 <&> %s','entry','start','stop','id')
controls<-function(doc)lapply(xml2::xml_find_all(doc,'//select|//input'),function(n)list(id=xml2::xml_attr(n,'id'),value=xml2::xml_attr(n,'value'),checked=xml2::xml_attr(n,'checked'),min=xml2::xml_attr(n,'min'),max=xml2::xml_attr(n,'max'),step=xml2::xml_attr(n,'step'),multiple=xml2::xml_attr(n,'multiple'),options=xml2::xml_attr(xml2::xml_find_all(n,'./option'),'value'),selected=xml2::xml_attr(xml2::xml_find_all(n,'./option[@selected]'),'value')))
cases<-list(list(fun=survival_km_setup_panel,args=list()),list(fun=survival_cox_setup_panel,args=list()))
for(shape in c('single_record','entry_exit'))for(tab in c('analysis','tables','plots'))cases[[length(cases)+1]]<-list(fun=survival_km_setup_panel,args=list(data_shape=shape,option_tab=tab,time='Review',event='Normality',group='None',entry='entry',event_value='사용자 <&> %s',analysis_method='life_table',test_method='tarone_ware',output_tables='survival_time',plot_versions='bw',plot_types='cumhaz',show_ci=FALSE,show_censor=FALSE,rmst_tau='365'))
for(shape in c('single_record','entry_exit','start_stop'))for(ties in c('efron','breslow','exact'))cases[[length(cases)+1]]<-list(fun=survival_cox_setup_panel,args=list(data_shape=shape,ties_method=ties,time='Review',event='Normality',entry='entry',start='start',stop='stop',subject_id='id',covariates=c('None','사용자 <&> %s'),strata='None',cluster='id',spline_covariate='None',spline_df=5,time_varying_covariate='사용자 <&> %s',time_varying_times='1, 5, 10',adjusted_group='None',adjusted_bootstrap_reps=400,adjusted_times='12, 36'))
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 for(case in cases) {
  render<-function(language)xml2::read_html(as.character(do.call(case$fun,c(list(selected_names=variables,language=language),case$args))),encoding='UTF-8')
  doc<-render(lang);en<-render('en')
  stopifnot(identical(controls(doc),controls(en)))
  stopifnot(identical(xml2::xml_attr(xml2::xml_find_all(doc,'//*[@data-display-if]'),'data-display-if'),xml2::xml_attr(xml2::xml_find_all(en,'//*[@data-display-if]'),'data-display-if')))
  if(lang!='en') {
   note<-xml2::xml_text(xml2::xml_find_first(doc,'//*[contains(@class,"survival-event-variable-note")]'))
   stopifnot(nzchar(note),!grepl('Binary or categorical',note,fixed=TRUE))
   for(phrase in c('Cox data structure','Tied-event handling','Partial-likelihood method','Continuous nonlinear effect','Time-varying coefficient','Efron (default)'))stopifnot(!grepl(phrase,xml2::xml_text(doc),fixed=TRUE))
  }
 }
 # All variable option labels, including dictionary collisions, remain user text.
 doc<-xml2::read_html(as.character(survival_cox_setup_panel(variables,covariates=variables,language=lang)),encoding='UTF-8')
 for(id in c('survival_cox_spline_covariate','survival_cox_time_varying_covariate','survival_cox_adjusted_group')) {
  labels<-xml2::xml_text(xml2::xml_find_all(doc,paste0('//select[@id="',id,'"]/option')))
  stopifnot(identical(labels[-1],variables))
 }
 cat('PASS:',lang,length(cases),'default/restored KM/Cox configurations; controls and conditions unchanged; localized note/options; raw variable labels preserved\n')
}
