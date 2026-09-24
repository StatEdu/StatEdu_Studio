Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('scripts/validate_sem_structural_reporting_tables.R',encoding='UTF-8')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/specific-indirect-main-language';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
for(kind in c('model','bootstrap')){
 table<-if(kind=='model')specific_model else specific_boot
 table$Path<-rep('Review → Normality → 사용자 <&> %s',nrow(table))
 lower<-intersect(c('Boot 95% CI lower','B 95% CI lower'),names(table))[[1]]
 upper<-intersect(c('Boot 95% CI upper','B 95% CI upper'),names(table))[[1]]
 for(language in c('en','ko','ja','zh','es','fr','de','vi')){
  options(statedu.app_language=language)
  html<-as.character(structural_canvas_specific_indirect_html_table(table,language=language))
  doc<-xml2::read_html(html,encoding='UTF-8');headers<-xml2::xml_text(xml2::xml_find_all(doc,'//th'))
  if(language=='en')english<-html else stopifnot(identical(html,english))
  stopifnot(headers[[1]]=='Path',all(c('95% CI','LLCI','ULCI')%in%headers))
  stopifnot(identical(trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[1]'))),table$Path))
  stopifnot(identical(trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[4]'))),as.character(table[[lower]])))
  stopifnot(identical(trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[5]'))),as.character(table[[upper]])))
  cat('PASS:',kind,language,'English main table, user path, exact CI values and grouped CI headers\n')
  if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=html)
 }
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
