Sys.setlocale('LC_CTYPE','Korean_Korea.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('scripts/validate_calculators.R',encoding='UTF-8')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
cases <- list(
 hint8=list(data=hint8_items,ids=hint8_item_specs()$id,handler=register_hint8_calculator_handlers,setup='hint8_calculator_setup',summary='hint8_calculator_summary',run='run_hint8_calculator',loaded='hint8_loaded_message'),
 eq5d=list(data=eq5d_items,ids=eq5d_item_specs()$id,handler=register_eq5d_calculator_handlers,setup='eq5d_calculator_setup',summary='eq5d_calculator_summary',run='run_eq5d_calculator',loaded='eq5d_loaded_message'),
 metabolic=list(data=metabolic_data,ids=paste0('metabolic_',names(metabolic_data)),handler=register_metabolic_calculator_handlers,setup='metabolic_calculator_setup',summary='metabolic_calculator_summary',run='run_metabolic_calculator',loaded='metabolic_loaded_message'),
 frs=list(data=frs_data,ids=paste0('frs_',names(frs_data)),handler=register_frs_calculator_handlers,setup='frs_calculator_setup',summary='frs_calculator_summary',run='run_frs_calculator',loaded='frs_loaded_message'),
 ascvd10=list(data=ascvd10_data,ids=paste0('ascvd10_',names(ascvd10_data)),handler=register_ascvd10_calculator_handlers,setup='ascvd10_calculator_setup',summary='ascvd10_calculator_summary',run='run_ascvd10_calculator',loaded='ascvd10_loaded_message'),
 mbss=list(data=mbss_data,ids=paste0('mbss_',names(mbss_data)),handler=register_metabolic_severity_calculator_handlers,setup='mbss_calculator_setup',summary='mbss_calculator_summary',run='run_mbss_calculator',loaded='mbss_loaded_message'))
specs<-list(hint8=hint8_item_specs(),eq5d=eq5d_item_specs(),metabolic=metabolic_variable_specs(),
 frs=frs_variable_specs(),ascvd10=ascvd10_variable_specs(),mbss=metabolic_severity_variable_specs())
for(language in c('en','ko','ja','zh','es','fr','de','vi'))for(spec in specs)for(label in spec$label) {
 text<-sub('^(LQ|EQ)[0-9]+ \\((.*)\\)$','\\2',label)
 key<-gsub('^_+|_+$','',gsub('[^a-z0-9]+','_',tolower(text)))
 if(!text %in% c('SBP','DBP'))stopifnot(nzchar(statedu_t(paste0('calculator.field.',key),language,fallback='')))
 if(grepl('^(LQ|EQ)',label))stopifnot(startsWith(calculator_field_label(label,language),sub(' \\(.*$','',label)))
}
audit<-new.env();audit$rows<-list()
for(kind in names(cases)) {
 config<-cases[[kind]]
 shiny::testServer(function(input,output,session) {
  lang<-reactiveVal('en');writes<-new.env(parent=emptyenv());stats<-new.env();stats$calls<-0L
  args<-list(input=input,output=output,session=session,dataset_fn=function()config$data,
    current_data_file_fn=function()list(name='사용자_<&> %s.csv'),variable_info_fn=function()NULL,
    add_calculated_variable_fn=function(name,values,...) {stats$calls<-stats$calls+1L;writes[[name]]<-values},
    language_fn=lang)
  do.call(config$handler,args)
 }, {
  session$flushReact()
  selected<-setNames(as.list(names(config$data)),config$ids)
  do.call(session$setInputs,c(selected,list(eq5d_output_var='사용자_<&> %s',ascvd10_output_var='사용자_<&> %s',hint8_profile_11111111_as_one=TRUE,frs_lipid_unit='mmol_l')))
  do.call(session$setInputs,setNames(list(1L),config$run));session$flushReact()
  baseline<-output[[config$summary]];stopifnot(length(writes)>0L)
  saved<-serialize(as.list(writes),NULL)
  call_count<-stats$calls
  for(language in c('ko','en','ja','zh','es','fr','de','vi','ko')) {
   lang(language);session$flushReact()
   html<-output[[config$summary]]$html
   stopifnot(nzchar(html),!grepl('calculator.status.',html,fixed=TRUE))
   if(language!='en')stopifnot(html!=baseline$html)
   text<-xml2::xml_text(xml2::read_html(html))
   if(kind %in% c('eq5d','ascvd10'))stopifnot(grepl('사용자_<&> %s',text,fixed=TRUE))
   stopifnot(identical(hint8_loaded_message_text(language=language),statedu_t('calculator.status.no_data',language)))
   loaded<-output[[config$loaded]]
   preview<-jsonlite::fromJSON(output[[sub('summary','preview',config$summary)]],simplifyVector=FALSE)$x
   stopifnot(identical(preview$options$language,datatable_language_options(language)))
   stopifnot(grepl('사용자_<&> %s.csv',loaded,fixed=TRUE),grepl(as.character(nrow(config$data)),loaded,fixed=TRUE))
   setup<-xml2::read_html(output[[config$setup]]$html)
   if(kind=='frs') {
     stopifnot(xml2::xml_attr(xml2::xml_find_first(setup,'//select[@id="frs_lipid_unit"]/option[@selected]'),'value')=='mmol_l')
     text<-xml2::xml_text(setup)
     for(label in c('Male = 1, Female = 2','Yes = 1, No = 0','Lipids','Score','10-year risk','Risk group','Heart age'))
       stopifnot(grepl(calculator_field_label(label,language),text,fixed=TRUE))
     stopifnot(grepl('mmol/L -> mg/dL',text,fixed=TRUE))
   }
   if(language=='ja')for(label in xml2::xml_find_all(setup,'//label[@for]')) {
     if(xml2::xml_attr(label,'for') %in% names(selected))
       audit$rows[[length(audit$rows)+1L]]<-data.frame(calculator=kind,input=xml2::xml_attr(label,'for'),label=xml2::xml_text(label))
   }
   for(id in names(selected)) {
     node<-xml2::xml_find_first(setup,paste0('//select[@id="',id,'"]/option[@selected]'))
     if(!identical(xml2::xml_attr(node,'value'),selected[[id]]))print(list(kind=kind,language=language,id=id,actual=xml2::xml_attr(node,'value'),expected=selected[[id]]))
     stopifnot(identical(xml2::xml_attr(node,'value'),selected[[id]]),identical(input[[id]],selected[[id]]))
     index<-match(id,config$ids);expected<-calculator_field_label(specs[[kind]]$label[index],language)
     if(kind=='ascvd10' && !specs[[kind]]$required[index])expected<-paste0(expected,' (',calculator_field_label('optional',language),')')
     label<-xml2::xml_text(xml2::xml_find_first(setup,paste0('//label[@for="',id,'"]')))
     stopifnot(identical(label,expected))
   }
   stopifnot(identical(saved,serialize(as.list(writes),NULL)),identical(stats$calls,call_count))
  }
 })
 cat('PASS',kind,'8-language server round trip: translated summary/load status; selected variables and calculated values preserved\n')
}
dir.create('tmp/calculator-status',recursive=TRUE,showWarnings=FALSE)
write.csv(do.call(rbind,audit$rows),'tmp/calculator-status/japanese-variable-label-audit.csv',row.names=FALSE,fileEncoding='UTF-8')
