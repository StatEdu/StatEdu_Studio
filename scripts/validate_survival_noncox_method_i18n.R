Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/survival-noncox-method-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv');d$entry<-0
cases<-list(km=prepare_km_single_analysis_result(d,'time','status'),
 delayed=prepare_km_single_analysis_result(d,'time','status',entry='entry'),
 life=prepare_km_single_analysis_result(d,'time','status',analysis_method='life_table'))
grid<-expand.grid(group=c(FALSE,TRUE),cs=c(FALSE,TRUE),fg=c(FALSE,TRUE),censor=c(FALSE,TRUE))
for(i in seq_len(nrow(grid))){
 g<-grid[i,];cases[[paste0('competing',i)]]<-list(type='competing_risk',group=if(g$group)'group' else '',
 cause_specific=if(g$cs)list() else NULL,fine_gray=if(g$fg)list() else NULL,censoring_group=if(g$censor)'Review <&> %s' else '')
}
for(kind in names(cases))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 r<-cases[[kind]];r$time_origin<-'Origin <&> %s';r$time_unit<-'Unit %d'
 sentence<-survival_method_sentence(r,language)
 for(x in c(r$time_origin,r$time_unit))stopifnot(grepl(x,sentence,fixed=TRUE))
 if(!language%in%c('en','ko')){
  tr<-function(x)statedu_localized_text(language,x)
  stopifnot(!grepl('Time was measured from|the Kaplan–Meier method|the actuarial life-table method|cumulative incidence functions',sentence))
  if(grepl('^competing',kind)){
   stopifnot(grepl(tr("Gray's test"),sentence,fixed=TRUE)==nzchar(r$group),
    grepl(tr('cause-specific Cox regression'),sentence,fixed=TRUE)==is.list(r$cause_specific),
    grepl(tr('Fine–Gray regression'),sentence,fixed=TRUE)==is.list(r$fine_gray),
    grepl('Review <&> %s',sentence,fixed=TRUE)==(is.list(r$fine_gray)&&nzchar(r$censoring_group)))
  }
 }
 blank<-r;blank$time_origin<-'';blank$time_unit<-''
 if(!language%in%c('en','ko'))stopifnot(!grepl('not specified',survival_method_sentence(blank,language),fixed=TRUE))
 table<-if(kind%in%c('km','delayed','life'))survival_km_summary_table(r) else data.frame(Variable='Review <&> %s',N=10)
 html<-as.character(tagList(survival_simple_table(table,table_role='main',table_language='en'),tags$p(sentence)))
 parsed<-xml2::read_html(html,encoding='UTF-8');stopifnot(sentence%in%xml2::xml_text(xml2::xml_find_all(parsed,'//p')))
 cells<-xml2::xml_text(xml2::xml_find_all(parsed,'//th|//td'))
 if(language=='en')baseline<-cells else stopifnot(identical(cells,baseline))
 if(language=='ja'&&kind%in%c('km','delayed','life','competing1','competing16'))entries[[kind]]<-list(id=kind,title=kind,html=html)
 cat('PASS:',kind,language,'method selection, raw metadata and English table\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
