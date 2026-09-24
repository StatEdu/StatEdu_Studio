Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/km-crossing-note-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv');d$second_group<-rep(c('A','B'),length.out=nrow(d))
results<-list(single=prepare_km_analysis_result(d,'time','status',group='sex'),multiple=prepare_km_analysis_result(d,'time','status',group=c('sex','second_group')))
for(kind in names(results))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language);r<-results[[kind]]
 rows<-survival_km_crossing_summary_table(r);stopifnot(nrow(rows)>0)
 doc<-xml2::read_html(as.character(survival_km_results_panel(r,language=language)),encoding='UTF-8')
 notes<-xml2::xml_text(xml2::xml_find_all(doc,'//*[contains(@class,"result-note")]'))
 note<-as.character(survival_km_crossing_note(language));note_text<-xml2::xml_text(xml2::xml_find_first(xml2::read_html(note,encoding='UTF-8'),'//div'))
 stopifnot(grepl(note_text,xml2::xml_text(doc),fixed=TRUE))
 if(language!='en')stopifnot(!grepl('This descriptive screen',note_text,fixed=TRUE))
 stopifnot(grepl('RMST',note_text,fixed=TRUE),grepl('Kaplan',note_text,fixed=TRUE))
 html<-as.character(tagList(tags$h3(survival_appendix_title('Survival-curve crossing screen',language)),survival_simple_table(rows,table_language=language),HTML(note)))
 table<-xml2::read_html(html,encoding='UTF-8')
 # Numerical columns retain the formatted producer values across all UI languages.
 values<-xml2::xml_text(xml2::xml_find_all(table,'//tbody/tr/td'))
 numeric_values<-values[grepl('^[0-9.+-]+$',trimws(values))]
 if(language=='en')baseline<-numeric_values else stopifnot(identical(numeric_values,baseline))
 if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=html)
 cat('PASS:',kind,language,'actual KM panel, crossing note and diagnostic values\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
