Sys.setlocale('LC_CTYPE','Korean_Korea.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
doc<-function(ui)xml2::read_html(as.character(ui))
radios<-function(ui)xml2::xml_find_all(doc(ui),'//input[@type="radio"]')
languages<-c('en','ko','ja','zh','es','fr','de','vi')
count<-0L
for(kind in c('sample','effect')) {
 methods<-if(kind=='sample')names(sample_size_method_labels())else names(effect_size_method_labels())
 build<-if(kind=='sample')sample_size_analysis_panel else effect_size_analysis_panel
 for(method in methods) {
  defaults<-shiny::isolate(build(method,'en'))
  options<-radios(defaults)
  for(option in options) {
   id<-xml2::xml_attr(option,'name');selected<-xml2::xml_attr(option,'value')
   input<-setNames(list(selected),id)
   for(lang in languages) {
    ui<-shiny::isolate(build(method,lang,input))
    actual<-xml2::xml_attr(xml2::xml_find_all(doc(ui),'//input[@type="radio"][@checked]'),'value')
    stopifnot(identical(actual,selected))
   }
   count<-count+1L
  }
  if(length(options)) {
   invalid<-setNames(list('invalid_design'),xml2::xml_attr(options[[1]],'name'))
   stopifnot(identical(xml2::xml_attr(xml2::xml_find_all(doc(defaults),'//input[@checked]'),'value'),
      xml2::xml_attr(xml2::xml_find_all(doc(shiny::isolate(build(method,'en',invalid))),'//input[@checked]'),'value')))
  }
 }
}
shiny::testServer(function(input,output,session){
 lang<-reactiveVal('en')
 output$effect<-renderUI(effect_size_analysis_panel('ttest',lang(),input))
 output$sample<-renderUI(sample_size_analysis_panel('ttest',lang(),input))
},{
 session$setInputs(effect_size_ttest_design='paired_t',sample_size_ttest_target='power')
 for(language in c(languages,'ko')) {
  lang(language);session$flushReact()
  stopifnot(xml2::xml_attr(xml2::xml_find_first(doc(output$effect$html),'//input[@checked]'),'value')=='paired_t',
    xml2::xml_attr(xml2::xml_find_first(doc(output$sample$html),'//input[@checked]'),'value')=='power')
 }
})
cat(sprintf('PASS %s top-level options x 8 languages, invalid-choice fallback, reactive language round trip\n',count))
source('scripts/validate_sample_size.R',encoding='UTF-8')
