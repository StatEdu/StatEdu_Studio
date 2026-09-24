Sys.setlocale('LC_CTYPE','English_United States.utf8')
previous<-list()
for(language in c('en','ko')){
 path<-file.path('tmp/structural-quality-summary-i18n',paste0(language,'-cfa.html'))
 if(file.exists(path))previous[[language]]<-paste(readLines(path,encoding='UTF-8'),collapse='\n')
}
source('scripts/validate_structural_quality_summary_i18n.R',encoding='UTF-8')
for(language in names(previous))stopifnot(identical(previous[[language]],paste(readLines(file.path(out,paste0(language,'-cfa.html')),encoding='UTF-8'),collapse='\n')))
sem_fit<-lavaan::sem('Normality =~ x1+x2+x3\n사용자요인 =~ y1+y2+y3\n사용자요인 ~ Normality',data=d)
sem<-cfa;sem$fit<-sem_fit;sem$diagnostics<-structural_canvas_fit_admissibility(sem_fit);sem$modified_model<-TRUE
stopifnot(lavaan::lavInspect(sem_fit,'converged'))
detail_out<-'tmp/structural-cfa-quality-details-i18n';dir.create(detail_out,recursive=TRUE,showWarnings=FALSE)
detail_entries<-readRDS(file.path(out,'entries.rds'))
for(kind in c('cfa','cbsem')){
 bundle<-if(kind=='cfa')cfa else sem
 raw<-structural_canvas_lavaan_quality_rows(bundle,kind);stopifnot(nrow(raw)==20L)
 for(language in c('en','ko','ja','zh','es','fr','de','vi')){
  html<-as.character(structural_canvas_lavaan_quality_result_ui(bundle,kind,language))
  doc<-xml2::read_html(html,encoding='UTF-8');table<-tail(xml2::xml_find_all(doc,'//table'),1)[[1]]
  rows<-xml2::xml_find_all(table,'.//tbody/tr');stopifnot(length(rows)==20L)
  for(i in seq_along(rows)){
   cells<-trimws(xml2::xml_text(xml2::xml_find_all(rows[[i]],'./td')))
   if(!language %in% c('en','ko')){
    stopifnot(identical(cells[1],unname(statedu_localized_text(language,raw$Item[i]))))
    translated<-unname(statedu_localized_text(language,raw$Guidance[i]))
    if(!identical(cells[4],translated))stop(paste(language,kind,i,raw$Item[i],"ACTUAL",cells[4],"EXPECTED",translated));stopifnot(translated!=raw$Guidance[i])
    if(!raw$Item[i] %in% c('CFI','TLI','RMSEA','SRMR'))stopifnot(cells[1]!=raw$Item[i])
   }
   if(grepl('^[0-9.%-]+$',raw$Value[i]) || raw$Item[i]=='Fit statistic source')stopifnot(cells[2]==raw$Value[i])
  }
  if(!language %in% c('en','ko')){
   stopifnot(!any(xml2::xml_text(xml2::xml_find_all(table,'.//td')) %in% c('TRUE','FALSE')))
   failed<-bundle;failed$converged<-FALSE;failed$admissible<-FALSE
   failed_doc<-xml2::read_html(as.character(structural_canvas_lavaan_quality_result_ui(failed,kind,language)),encoding='UTF-8')
   no<-statedu_localized_text(language,'No','아니오');stopifnot(any(xml2::xml_text(xml2::xml_find_all(failed_doc,'//td'))==no))
  }
  writeLines(html,file.path(detail_out,paste0(language,'-',kind,'.html')),useBytes=TRUE)
  if(language=='ja' && kind=='cbsem')detail_entries<-c(detail_entries,list(list(id='sem-quality-detail',title='SEM quality details',html=html)))
  cat('PASS:',language,kind,'all 20 Item/Guidance rows; numeric and fit-source values preserved\n')
 }
}
saveRDS(detail_entries,file.path(detail_out,'entries.rds'))
