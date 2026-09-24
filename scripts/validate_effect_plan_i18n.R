Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/effect-plan-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
for(engine in c('CB-SEM','PLS','PLSC'))for(method in if(engine=='CB-SEM')c('all_pairs_dmc','matched_pair_dmc','all_pairs_mean_centered')else c('two_stage','product_indicator','orthogonal'))for(requested in c(FALSE,TRUE)){
 snapshot<-list(moderations=if(requested)list(list())else list(),moderationMethod=method)
 plan<-structural_canvas_structural_effect_plan(snapshot,if(engine=='CB-SEM')'cbsem'else'plssem',engine)
 for(language in c('en','ko','ja','zh','es','fr','de','vi')){
  html<-as.character(structural_canvas_effect_plan_ui(plan,language));doc<-xml2::read_html(html,encoding='UTF-8')
  cells<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//td')))
  stopifnot(length(cells)==length(as.matrix(plan)))
  heading<-xml2::xml_text(xml2::xml_find_first(doc,'//h5'));note<-xml2::xml_text(xml2::xml_find_first(doc,'//p'))
  if(language=='en'){en_head<-heading;en_note<-note;en_cells<-cells}else{
   stopifnot(heading!=en_head,note!=en_note)
   # German Mediation/Moderation are the same words as English.
   allowed<-if(language=='de')c('Mediation','Moderation')else character()
   stopifnot(!any(cells==en_cells & !en_cells%in%allowed))
   headers<-xml2::xml_text(xml2::xml_find_all(doc,'//th'));stopifnot(!any(c('Effect','Method','Limitation')%in%headers))
  }
  stopifnot(identical(plan,structural_canvas_structural_effect_plan(snapshot,if(engine=='CB-SEM')'cbsem'else'plssem',engine)))
  if(language=='ja'&&requested){key<-paste(engine,method,sep='-');entries[[key]]<-list(id=key,title=key,html=html)}
  cat('PASS:',engine,method,requested,language,'title, note, program cells and source plan preservation\n')
 }
}
raw<-data.frame(Effect='Review 사용자 <&> %s',Status='External status',Method='Normality',Limitation='Primary',check.names=FALSE)
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 doc<-xml2::read_html(as.character(structural_canvas_effect_plan_ui(raw,language)),encoding='UTF-8')
 stopifnot(identical(trimws(xml2::xml_text(xml2::xml_find_all(doc,'//td'))),unname(unlist(raw))))
}
stopifnot(is.null(structural_canvas_effect_plan_ui(data.frame())),is.null(structural_canvas_effect_plan_ui(NULL)))
saveRDS(unname(entries),file.path(out,'entries.rds'))
