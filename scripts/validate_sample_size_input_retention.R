Sys.setlocale('LC_CTYPE','Korean_Korea.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
doc<-function(ui)xml2::read_html(as.character(ui))
value<-function(ui,id)xml2::xml_attr(xml2::xml_find_first(doc(ui),paste0('//input[@id="',id,'"]')),'value')
languages<-c('en','ko','ja','zh','es','fr','de','vi','ko')
for(method in names(sample_size_method_labels()))for(lang in languages) {
 prefix<-paste0('sample_size_',method,'_')
 saved<-setNames(as.list(c('0.013','0.87','157','1.7','12.5','one.sided')),paste0(prefix,c('alpha','power','n','ratio','dropout','alternative')))
 for(target in c('sample_size','power')) {
  ui<-shiny::isolate(sample_size_common_inputs(method,target,TRUE,TRUE,language=lang,input=saved))
  fields<-c('alpha','ratio',if(target=='sample_size')c('power','dropout')else 'n')
  for(field in fields)stopifnot(value(ui,paste0(prefix,field))==saved[[paste0(prefix,field)]])
  stopifnot(xml2::xml_attr(xml2::xml_find_first(doc(ui),'//option[@selected]'),'value')=='one.sided')
 }
}
shiny::testServer(function(input,output,session){
 language<-reactiveVal('en')
 output$sample<-renderUI(sample_size_inputs_ui('ttest',sample_size_input_snapshot('ttest',input),language()))
 output$effect<-renderUI(effect_size_ttest_inputs_ui(input,language()))
},{
 session$setInputs(sample_size_ttest_design='paired',sample_size_ttest_effect='0.72',sample_size_ttest_alpha='0.013',sample_size_ttest_power='0.87',sample_size_ttest_dropout='12.5',sample_size_ttest_alternative='one.sided',effect_size_ttest_design='paired_t',effect_size_ttest_t='4.25',effect_size_ttest_n='83')
 for(lang in languages){
  language(lang);session$flushReact()
  stopifnot(value(output$sample$html,'sample_size_ttest_effect')=='0.72',value(output$sample$html,'sample_size_ttest_alpha')=='0.013',value(output$sample$html,'sample_size_ttest_power')=='0.87',value(output$effect$html,'effect_size_ttest_t')=='4.25',value(output$effect$html,'effect_size_ttest_n')=='83')
 }
 session$setInputs(sample_size_ttest_alpha='',effect_size_ttest_t='')
 language('ja');session$flushReact()
 stopifnot(value(output$sample$html,'sample_size_ttest_alpha')=='',value(output$effect$html,'effect_size_ttest_t')=='')
})
cat('PASS all-method common inputs, both targets, 8 languages plus Korean return; real reactive t-test inputs and blank edits retained\n')
source('scripts/validate_sample_size.R',encoding='UTF-8')
