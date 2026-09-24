Sys.setlocale('LC_CTYPE','English_United States.utf8')
source('scripts/validate_structural_quality_summary_i18n.R',encoding='UTF-8')
detail_out<-'tmp/structural-pls-quality-details-i18n'
dir.create(detail_out,recursive=TRUE,showWarnings=FALSE)
raw<-structural_canvas_pls_quality_rows(pls)
stopifnot(nrow(raw)==18L)
pls_main_baseline<-NULL
main_bundle<-pls
main_bundle$snapshot<-list(
 nodes=c(lapply(c('X','Y'),function(v)list(id=v,name=v,role='latent',constructType='composite')),
         lapply(names(d),function(v)list(id=v,name=v,role='indicator'))),
 edges=lapply(names(d),function(v)list(from=if(substr(v,1,1)=='x')'X' else 'Y',to=v)))
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 main_data<-structural_canvas_result_table('measurement',function()main_bundle,'plssem',function()c(x1='사용자 라벨'),function()'en')
 stopifnot(nrow(main_data)>0)
 main_html<-as.character(structural_canvas_basic_html_table(main_data,role='main',language=language,title='Measurement model'))
 if(is.null(pls_main_baseline))pls_main_baseline<-main_html else stopifnot(identical(main_html,pls_main_baseline))
 html<-as.character(structural_canvas_pls_quality_result_ui(pls,language))
 doc<-read(html);table<-tail(xml2::xml_find_all(doc,'//table'),1)[[1]]
 rows<-xml2::xml_find_all(table,'.//tbody/tr');stopifnot(length(rows)==18L)
 for(i in seq_along(rows)){
  cells<-trimws(xml2::xml_text(xml2::xml_find_all(rows[[i]],'./td')))
  if(!language %in% c('en','ko'))for(column in c('Item','Guidance')){
   original<-raw[[column]][i];translated<-unname(statedu_localized_text(language,original))
   if(translated==original)stop('Untranslated ',language,' ',column,': ',original)
   stopifnot(identical(cells[if(column=='Item')1L else 4L],translated))
  }
  if(grepl('^[0-9.%-]+$',raw$Value[i]))stopifnot(cells[2]==raw$Value[i])
 }
 writeLines(html,file.path(detail_out,paste0(language,'-plssem.html')),useBytes=TRUE)
 if(!language %in% c('en','ko')){
  values<-data.frame(Value=c('mean_replacement','not recorded','Not executed','Executed; no comparable indicator metrics','2/3 indicator metrics favor PLS over LM'))
  translated<-structural_canvas_quality_display_rows(values,FALSE,language)$Value
  stopifnot(all(translated!=values$Value),!grepl('{',translated[5],fixed=TRUE),grepl('2',translated[5]),grepl('3',translated[5]))
  # Generated labels must not change unrelated user text or technical values.
  protected<-data.frame(Value=c('사용자 라벨','Normality','MLR','0.700'))
  stopifnot(identical(structural_canvas_quality_display_rows(protected,FALSE,language),protected))
 }
 cat('PASS:',language,'actual PLS 18 Item/Guidance rows and numeric values\n')
}
export_entries<-readRDS(file.path(out,'entries.rds'))
export_entries<-c(export_entries,list(list(id='pls-measurement',title='PLS measurement',html=pls_main_baseline)))
saveRDS(export_entries,file.path(detail_out,'entries.rds'))
