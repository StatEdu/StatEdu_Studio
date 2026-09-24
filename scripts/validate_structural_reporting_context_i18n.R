Sys.setlocale('LC_CTYPE','English_United States.utf8')
source('scripts/validate_structural_reporting_settings_i18n.R',encoding='UTF-8')
context_out<-'tmp/structural-reporting-context-i18n';dir.create(context_out,recursive=TRUE,showWarnings=FALSE)
context_entries<-list()
cfa_context<-cfa_settings
cfa_context$snapshot<-list(nodes=list(list(id='a',name='Normality 사용자 <&>',role='latent',constructType='commonFactor',measurementMode='reflective')),edges=list())
cfa_context$missing_diagnostics<-list(available=TRUE,incomplete_n=4L)
cfa_context$missing<-'fiml';cfa_context$missing_sensitivity_method<-'complete_case_comparison';cfa_context$missing_sensitivity_details<-'사용자 설명'
pls_context<-pls_settings;pls_context$estimator<-'PLSC';pls_context$estimator_requested<-'AUTO'
pls_context$estimator_selection_mode<-'Mixed model: common factors corrected; composites uncorrected'
pls_context$snapshot<-list(nodes=list(
 list(id='a',name='Review',role='latent',constructType='commonFactor',measurementMode='reflective'),
 list(id='b',name='Normality 사용자 <&>',role='latent',constructType='composite',measurementMode='formative',constructSpecificationMigration='Requested; 사용자 <&>')
),edges=list())
pls_context$missing_diagnostics<-list(available=TRUE,imputed_cell_n=3L)
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 if(language!='en'){
  for(method in c('not_assessed','complete_case_comparison','multiple_imputation','delta_pattern_mixture','external_analysis','other_documented')){
   sensitivity_bundle<-cfa_context;sensitivity_bundle$missing_sensitivity_method<-method
   sensitivity<-structural_canvas_missing_sensitivity_rows(sensitivity_bundle)
   for(source in c(sensitivity$`Sensitivity assessment`,sensitivity$Status)){
    if(structural_canvas_reporting_text(source,language)==source)stop('Untranslated sensitivity: ',language,' ',source)
   }
  }
  for(convention in c('normal','wishart')){
   likelihood<-cfa_context;likelihood$ml_likelihood<-convention
   source<-structural_canvas_ml_likelihood_label(likelihood)
   stopifnot(structural_canvas_reporting_text(source,language)!=source)
  }
 }
 for(kind in c('cfa','plssem')){
  bundle<-if(kind=='cfa')cfa_context else pls_context
  raw<-structural_canvas_reporting_context_rows(bundle,kind)
  html<-as.character(structural_canvas_reporting_context_result_ui(bundle,kind,language));doc<-read(html)
  cells<-xml2::xml_text(xml2::xml_find_all(doc,'//td'))
  stopifnot('Normality 사용자 <&>' %in% cells)
  if(kind=='plssem')stopifnot('Review' %in% cells,grepl('Requested; 사용자 <&>',xml2::xml_text(doc),fixed=TRUE))
  if(language!='en'){
   for(item in raw$Item){
    translated<-structural_canvas_reporting_text(item,language)
    if(translated==item)stop('Untranslated item: ',language,' ',item)
    if(!translated %in% cells)stop('Missing translated item: ',language,' ',item,' => ',translated)
   }
   for(source in c('Shared specification for all constructs','Declared type','Engine representation','Estimand',
                  if(kind=='cfa')c('lavaan reflective latent factor','Common factor with explicit measurement error','Complete-case comparison') else c('Rule-based recommendation accepted','Mixed model: common factors corrected; composites uncorrected','seminr Mode B composite','Formative composite'))){
    translated<-structural_canvas_reporting_text(source,language)
    if(translated==source)stop('Untranslated description: ',language,' ',source)
    stopifnot(grepl(translated,xml2::xml_text(doc),fixed=TRUE))
   }
  }
  writeLines(html,file.path(context_out,paste0(language,'-',kind,'.html')),useBytes=TRUE)
  if(language=='ja')context_entries[[kind]]<-list(id=kind,title=kind,html=html)
 }
 cat('PASS:',language,'reporting items, estimator/sensitivity text, compact construct specifications and literal names\n')
}
saveRDS(unname(context_entries),file.path(context_out,'entries.rds'))
