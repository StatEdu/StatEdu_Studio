Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out <- 'tmp/pls-fit-guide-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries <- list()
source_table <- data.frame(Effect=c(rep('Direct',4),'Indirect','Direct'),
 Outcome=c('Review','Normality','Primary','사용자 <&> %s','omitted indirect','omitted blank'),
 Predictor=c('Scoring','Items','Review','이름 <&> %s','indirect','blank'),
 f2=c('.020','.150','.350','','.999',''),'Inner VIF'=c('1.001','2.020','3.300','4.400','9.999',''),check.names=FALSE)
for (state in c('mixed','vif-only','empty')) {
 input <- source_table
 if(state=='vif-only')input$f2 <- ''
 if(state=='empty')input <- input[FALSE,]
 table <- structural_canvas_pls_fit_guide_table(input)
 if(state=='empty'){stopifnot(is.null(structural_canvas_pls_fit_guide_ui(table,'ja')));next}
 stopifnot(nrow(table)==4L)
 for(language in c('en','ko','ja','zh','es','fr','de','vi')) {
  html <- as.character(structural_canvas_pls_fit_guide_ui(table,language))
  doc <- xml2::read_html(html,encoding='UTF-8')
  cells <- trimws(xml2::xml_text(xml2::xml_find_all(doc,'//td')))
  stopifnot(identical(unname(cells),unname(as.character(t(as.matrix(table))))))
  title <- xml2::xml_text(xml2::xml_find_first(doc,'//h5'))
  note <- xml2::xml_text(xml2::xml_find_first(doc,'//p'))
  headers <- xml2::xml_text(xml2::xml_find_all(doc,'//th'))
  if(language=='en'){en_title<-title;en_note<-note} else {
   stopifnot(title!=en_title,note!=en_note,!'Outcome'%in%headers)
   if(language!='es')stopifnot(!'Predictor'%in%headers) # Spanish uses the same spelling.
  }
  stopifnot(all(vapply(c('.02','.15','.35','f²','Inner VIF'),grepl,logical(1),x=note,fixed=TRUE)))
  if(language=='ja')entries[[state]] <- list(id=state,title=state,html=html)
  cat('PASS:',state,language,'localized title, headers and note; unchanged user values and precision\n')
 }
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
