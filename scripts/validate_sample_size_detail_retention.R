Sys.setlocale('LC_CTYPE','Korean_Korea.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
parse_ui<-function(ui)xml2::read_html(as.character(ui))
text_values<-function(ui){
 nodes<-xml2::xml_find_all(parse_ui(ui),'//input[@type="text"][@id]')
 setNames(as.list(xml2::xml_attr(nodes,'value')),xml2::xml_attr(nodes,'id'))
}
selections<-function(ui){
 parsed<-parse_ui(ui)
 nodes<-xml2::xml_find_all(parsed,'//select[@id]')
 out<-list()
 for(node in nodes) {
  id<-xml2::xml_attr(node,'id')
  for(option in xml2::xml_find_all(node,'.//option')) {
   val<-xml2::xml_attr(option,'value')
   if(!is.na(val))out[[length(out)+1L]]<-setNames(list(val),id)
  }
 }
 for(node in xml2::xml_find_all(parsed,'//input[@type="radio"][@name]')) {
  out[[length(out)+1L]]<-setNames(list(xml2::xml_attr(node,'value')),xml2::xml_attr(node,'name'))
 }
 out
}
languages<-c('en','ko','ja','zh','es','fr','de','vi')
count<-0L;field_count<-0L
check<-function(builder,base){
 original<-shiny::isolate(builder(base,'en'))
 values<-text_values(original)
 if(!length(values))return(invisible(NULL))
 # Distinct valid numeric text, vector text, blank and markup exercise exact retention.
 edits<-setNames(as.list(rep(c('7.125','0.17, 0.29, 0.54','','사용자 <&> %s'),length.out=length(values))),names(values))
 edited<-utils::modifyList(base,edits)
 for(lang in languages){
  rendered<-shiny::isolate(builder(edited,lang))
  retained<-text_values(rendered)
  stopifnot(identical(retained[names(edits)],edits))
 }
 count<<-count+1L;field_count<<-field_count+length(edits)
}
for(method in names(effect_size_method_labels())) {
 builder<-get(paste0('effect_size_',method,'_inputs_ui'))
 cases<-c(list(list()),selections(effect_size_analysis_panel(method,'en')))
 for(base in cases) {
  check(builder,base)
  for(option in selections(shiny::isolate(builder(base,'en'))))check(builder,utils::modifyList(base,option))
 }
}
for(method in names(sample_size_method_labels())) {
 builder<-local({m<-method;function(input,language)sample_size_inputs_ui(m,input,language)})
 for(target in c('sample_size','power')) {
  base<-setNames(list(target),paste0('sample_size_',method,'_target'))
  check(builder,base)
  for(option in selections(shiny::isolate(builder(base,'en'))))check(builder,utils::modifyList(base,option))
 }
}
cat(sprintf('PASS %s rendered configurations, %s text fields x 8 languages: exact numeric/vector/blank/special text retained\n',count,field_count))
source('scripts/validate_sample_size.R',encoding='UTF-8')
