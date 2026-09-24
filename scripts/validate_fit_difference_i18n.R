Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/fit-difference-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
for(est in c('ML','MLR')){
 syntax<-'visual =~ x1+x2+x3\ntextual =~ x4+x5+x6\nspeed =~ x7+x8+x9'
 base<-lavaan::cfa(syntax,data=lavaan::HolzingerSwineford1939,estimator=est)
 modified<-lavaan::cfa(paste(syntax,'x1 ~~ x2',sep='\n'),data=lavaan::HolzingerSwineford1939,estimator=est)
 bundle<-list(comparison_type='mi',baseline_fit=base,fit=modified)
 original<-structural_canvas_model_difference_report(bundle)
 for(language in c('en','ko','ja','zh','es','fr','de','vi')){
  html<-as.character(structural_canvas_fit_difference_result_ui(bundle,language));doc<-xml2::read_html(html,encoding='UTF-8')
  notes<-xml2::xml_text(xml2::xml_find_all(doc,'//p'));heading<-xml2::xml_text(xml2::xml_find_first(doc,'//h5'));cells<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//td')))
  stopifnot(length(notes)==2L)
  if(language=='en'){en_notes<-notes;en_heading<-heading;en_cells<-cells}else stopifnot(all(notes!=en_notes),heading!=en_heading,identical(cells,en_cells))
  if(est=='MLR')stopifnot(grepl('satorra.bentler.2001',notes[[1]],fixed=TRUE))
  if(language=='ja')entries[[est]]<-list(id=est,title=est,html=html)
 }
 stopifnot(identical(original,structural_canvas_model_difference_report(bundle)))
 cat('PASS:',est,'actual nested comparison in eight languages; unchanged statistics\n')
}
reasons<-c('eligibility was not established','Models use different sample sizes.','Models use different observed variables.','Models do not use the same analyzed observations and values.','Models use different group structures.','Models use incompatible estimator families.','Models use different ML likelihood conventions.','Models do not have different finite degrees of freedom.','A strict free-parameter nesting relation was not verified.','Nesting was verified, but lavaan did not return a usable difference test.','One or both models are inadmissible.','One or both models are inadmissible (model 1: Review 사용자 <&> %s).','External reason Review 사용자 <&> %s')
for(i in seq_along(reasons)){
 reason<-reasons[[i]];report<-data.frame(Available=FALSE,Reason=reason)
 env<-new.env(parent=environment(structural_canvas_fit_difference_result_ui));env$structural_canvas_model_difference_report<-function(bundle)report
 render<-structural_canvas_fit_difference_result_ui;environment(render)<-env
 for(language in c('en','ko','ja','zh','es','fr','de','vi')){
  html<-as.character(render(list(comparison_type='mi',baseline_fit=TRUE),language));doc<-xml2::read_html(html,encoding='UTF-8');note<-xml2::xml_text(xml2::xml_find_first(doc,'//p'))
  if(language=='en')english<-note else {
   stopifnot(note!=english)
   if(i<length(reasons))stopifnot(!grepl(reason,note,fixed=TRUE))
  }
  if(grepl('Review',reason,fixed=TRUE))stopifnot(grepl('Review 사용자 <&> %s',note,fixed=TRUE))
  stopifnot(length(xml2::xml_find_all(doc,'//table'))==0L)
  if(language=='ja')entries[[paste0('reason',i)]]<-list(id=paste0('reason',i),title=paste0('Reason ',i),html=paste(as.character(structural_canvas_basic_html_table(data.frame(Model='Review',df='24'),role='main')),html))
 }
}
stopifnot(is.null(structural_canvas_fit_difference_result_ui(list(),'ja')))
saveRDS(unname(entries),file.path(out,'entries.rds'))
