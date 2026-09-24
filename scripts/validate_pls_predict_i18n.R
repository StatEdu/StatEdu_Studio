Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out <- 'tmp/pls-predict-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
for(state in c('repeated','single','items-only','constructs-only','empty'))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 pls<-matrix(c(1,3,2,NA_real_),1,dimnames=list('RMSE',c('Review','Normality','Primary','사용자 <&> %s')))
 lm<-matrix(rep(2,4),1,dimnames=dimnames(pls))
 summary<-list(PLS_out_of_sample=pls,LM_out_of_sample=lm,construct_error=matrix(c(.123,.234,.345,.456),1,dimnames=list('RMSE',c('Review','Normality','Primary','사용자 <&> %s'))))
 if(state%in%c('constructs-only','empty'))summary$PLS_out_of_sample<-summary$LM_out_of_sample<-NULL
 if(state%in%c('items-only','empty'))summary$construct_error<-NULL
 prediction<-list(summary=summary,folds=10L,reps=if(state=='single')1L else 2L,seed=20260917L)
 prediction$repetition_summaries<-if(state=='single')list(summary)else list(summary,modifyList(summary,list(PLS_out_of_sample=pls+.5)))
 bundle<-list(pls_predict_result=prediction)
 original<-structural_canvas_pls_predict_tables(prediction)
 html<-as.character(structural_canvas_pls_predict_result_ui(bundle,language));doc<-xml2::read_html(html,encoding='UTF-8')
 headings<-xml2::xml_text(xml2::xml_find_all(doc,'//h4|//h5'));note<-xml2::xml_text(xml2::xml_find_first(doc,'//p'))
 if(language=='en'){en_headings<-headings;en_note<-note}else stopifnot(all(headings!=en_headings),note!=en_note)
 stopifnot(grepl('20260917',note,fixed=TRUE),grepl('10',note,fixed=TRUE),grepl(as.character(prediction$reps),note,fixed=TRUE))
 actual<-xml2::xml_find_all(doc,'//table');stopifnot(length(actual)==sum(vapply(original,nrow,integer(1))>0))
 index<-0L
 for(kind in names(original)){
  table<-original[[kind]];if(!nrow(table))next
  index<-index+1L;cells<-trimws(xml2::xml_text(xml2::xml_find_all(actual[[index]],'.//td')))
  for(col in names(table)){
   if(col=='Assessment')next
   values<-if(is.numeric(table[[col]]))vapply(table[[col]],format_decimal3,character(1))else as.character(table[[col]])
   stopifnot(all(values%in%cells))
  }
  if(language!='en'&&kind=='items'){
   stopifnot(!any(c('PLS lower error','LM lower error','Tie','Not available')%in%cells))
   stopifnot(!any(c('Assessment','Error metric','PLS out-of-sample','LM benchmark','PLS lower %')%in%xml2::xml_text(xml2::xml_find_all(actual[[index]],'.//th'))))
  }
 }
 stopifnot(identical(original,structural_canvas_pls_predict_tables(prediction)))
 if(language=='ja'&&state!='empty')entries[[state]]<-list(id=state,title=state,html=html)
 cat('PASS:',state,language,'headings, note, assessment, raw names and three-decimal values\n')
}
stopifnot(is.null(structural_canvas_pls_predict_result_ui(NULL)),is.null(structural_canvas_pls_predict_result_ui(list())))
saveRDS(unname(entries),file.path(out,'entries.rds'))
