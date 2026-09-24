Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
info<-data.frame(name=c('stratum','psu','weight','rep1'),measurement=c('category','category','continuous','continuous'),var_label=c('사용자 층','Model-based','사용자 가중치','사용자 복제'))
selected<-list(p_strata='stratum',p_cluster='psu',p_weight='weight',p_variance_method='taylor',p_lonely_psu='average',p_use_replicate_weights=TRUE,p_replicate_weights='rep1',p_replicate_type='bootstrap')
out<-'tmp/complex-design-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 html<-as.character(complex_sample_design_setup_panel('p',info$name,info,language=language,selected=selected))
 doc<-xml2::read_html(html,encoding='UTF-8')
 for(kind in c('variance','lonely','replicate')){
  id<-switch(kind,variance='variance_method',lonely='lonely_psu',replicate='replicate_type')
  options<-xml2::xml_find_all(doc,paste0('//select[@id="p_',id,'"]/option'))
  expected<-complex_sample_method_choices(kind,language)
  stopifnot(identical(xml2::xml_attr(options,'value'),unname(expected)),identical(xml2::xml_text(options),names(expected)))
  choice<-xml2::xml_attr(xml2::xml_find_first(doc,paste0('//select[@id="p_',id,'"]/option[@selected]')),'value')
  stopifnot(identical(choice,selected[[paste0('p_',id)]]))
 }
 stopifnot(grepl('사용자 층',html,fixed=TRUE),grepl('Model-based',html,fixed=TRUE))
 writeLines(html,file.path(out,paste0(language,'.html')),useBytes=TRUE)
 cat('PASS:',language,'design options translated; values, selection and user labels preserved\n')
}
