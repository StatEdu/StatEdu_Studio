Sys.setlocale('LC_CTYPE','English_United States.utf8')
source('scripts/validate_structural_pls_dynamic_quality_i18n.R',encoding='UTF-8')
measurement_out<-'tmp/structural-measurement-diagnostics-i18n'
dir.create(measurement_out,recursive=TRUE,showWarnings=FALSE)
measurement_entries<-readRDS(file.path(dynamic_out,'entries.rds'))
guide<-structural_canvas_result_table('measurement_guide',function()main_bundle,'plssem',function()c(x1='사용자 라벨'),function()'en')
stopifnot(nrow(guide)==6L)
authored<-formative_bundle
authored$snapshot$nodes[[1]]$compositeDomainDefinition<-'Normality'
authored$snapshot$nodes[[1]]$compositeIndicatorRationale<-'Review'
authored$snapshot$nodes[[1]]$compositeContentValidityEvidence<-'Missing 사용자 <&>'
cfa_guide<-structural_canvas_result_table('measurement_diagnostics',function()cfa,'cfa',function()c(x1='사용자 라벨'),function()'en')
stopifnot(nrow(cfa_guide)>0)
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 html<-as.character(structural_canvas_measurement_diagnostics_ui(guide,authored,'plssem',2,language))
 doc<-read(html)
 evidence<-xml2::xml_find_first(doc,"//table[contains(@class,'structural-formative-evidence-table')]")
 cells<-xml2::xml_text(xml2::xml_find_all(evidence,'.//td'))
 for(value in c('Normality','Review','Missing 사용자 <&>','Normality 사용자 <&>','사용자 영역','<출처&>'))stopifnot(value %in% cells)
 if(!language %in% c('en','ko')){
  for(value in c('Domain definition','Indicator inclusion rationale','Content-validity procedure/source','Redundancy evidence','Documented'))stopifnot(statedu_localized_text(language,value)!=value)
  headings<-xml2::xml_text(xml2::xml_find_all(doc,'//h5'))
  stopifnot(!any(grepl('Supplementary Table|Formative-composite',headings)),grepl('2',headings[1]),!grepl('{number}',html,fixed=TRUE))
 }
 cfa_html<-as.character(structural_canvas_measurement_diagnostics_ui(cfa_guide,cfa,'cfa',3,language))
 stopifnot(!grepl('{number}',cfa_html,fixed=TRUE))
 flags<-c('Review residual variance','Review cross-loading','Loading CI includes 0','Weak loading review','No loading flag','Not assessed')
 flag_table<-cfa_guide[rep(1L,length(flags)),,drop=FALSE];flag_table$Guidance<-flags
 flag_html<-as.character(structural_canvas_measurement_diagnostics_ui(flag_table,cfa,'cfa',3,language))
 if(language!='en'){
  flag_cells<-xml2::xml_text(xml2::xml_find_all(read(flag_html),'//td'))
  stopifnot(!any(flags %in% flag_cells))
  heads<-xml2::xml_text(xml2::xml_find_all(read(paste0(html,cfa_html)),'//th'))
  stopifnot(!any(c('Construct','Loading','Weight','Mode','Latent','Std. residual variance','Cross-loading','Item VIF','Max cross-loading','Content-validity procedure/source','Redundancy evidence') %in% heads))
 }
 context_html<-as.character(structural_canvas_reporting_context_result_ui(main_bundle,'plssem',language))
 if(!language %in% c('en','ko'))for(text in c('Reporting checklist','Construct specification and computational representation','Only construct-specific differences remain in the table; values shared by every construct are reported once above.')){
  translated<-statedu_localized_text(language,text)
  stopifnot(translated!=text,grepl(translated,xml2::xml_text(read(context_html)),fixed=TRUE))
 }
 writeLines(paste0(html,cfa_html,flag_html,context_html),file.path(measurement_out,paste0(language,'-measurement.html')),useBytes=TRUE)
 if(language=='ja')measurement_entries<-c(measurement_entries,list(list(id='measurement-guide',title='Measurement diagnostics',html=paste0(html,cfa_html,flag_html,context_html))))
 cat('PASS:',language,'CFA/PLS measurement supplementary rendering and authored text preservation\n')
}
saveRDS(measurement_entries,file.path(measurement_out,'entries.rds'))
