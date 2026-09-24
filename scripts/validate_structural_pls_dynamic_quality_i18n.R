Sys.setlocale('LC_CTYPE','English_United States.utf8')
source('scripts/validate_structural_pls_quality_details_i18n.R',encoding='UTF-8')
dynamic_out<-'tmp/structural-pls-dynamic-quality-i18n'
dir.create(dynamic_out,recursive=TRUE,showWarnings=FALSE)
dynamic_entries<-readRDS(file.path(detail_out,'entries.rds'))
formative_bundle<-pls
formative_bundle$snapshot<-list(nodes=list(
 list(id='f',name='Normality 사용자 <&>',role='latent',constructType='composite',measurementMode='formative'),
 list(id='g',name='Review',role='latent',constructType='composite',measurementMode='formative',
      compositeDomainDefinition='사용자 영역',compositeIndicatorRationale='Normality',compositeContentValidityEvidence='<출처&>')
),edges=list())
formative_bundle$redundancy_construct<-'Review'
formative_bundle$redundancy_result<-list(available=TRUE)
raw<-structural_canvas_pls_quality_rows(formative_bundle)
stopifnot(nrow(raw)==20L,identical(tail(raw$Status,2),c('Review','OK')))
# Failure fixtures test all generated reasons without forcing numerical failures
# in seminr. The underlying successful model remains unchanged.
reasons<-c('estimator-consistent reduced-model fitting was incomplete',
 'the full-model R-squared is unavailable or outside [0, 1)',
 'the reduced model did not converge','the reduced model was numerically inadmissible',
 'the reduced model used a different estimator','the reduced model used a different PLSc common-factor specification',
 'the reduced-model R-squared is unavailable or outside [0, 1)',
 'the f-squared formula produced a non-finite value',
 'reduced-model estimation failed: Normality 사용자 <&>')
failure<-raw[raw$Item=='Max f2',,drop=FALSE]
failure$Status<-'Review';failure$Value<-''
attr(failure,'quality_f_square_failures')<-reasons
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 display<-structural_canvas_pls_quality_display_rows(raw,language)
 html<-as.character(structural_canvas_pls_quality_result_ui(formative_bundle,language))
 doc<-read(html);tables<-xml2::xml_find_all(doc,'//table')
 cells<-xml2::xml_text(xml2::xml_find_all(doc,'//td'))
 stopifnot(any(grepl('Normality 사용자 <&>',cells,fixed=TRUE)),any(grepl('Review',cells,fixed=TRUE)))
 if(language!='en'){
  stopifnot(!any(grepl('Formative evidence:',cells,fixed=TRUE)))
  prefix<-statedu_localized_text(language,'Formative evidence','형성형 근거')
  for(name in c('Normality 사용자 <&>','Review'))stopifnot(paste0(prefix,': ',name) %in% cells)
  stopifnot(display[nrow(display),match('Guidance',names(raw))]!=raw$Guidance[nrow(raw)])
  if(language!='ko')for(s in c('Domain','Indicator rationale','Content-validity procedure/source','Redundancy evidence','Recorded','Missing','Available','Not documented'))stopifnot(statedu_localized_text(language,s)!=s)
 }
 localized_failure<-structural_canvas_pls_quality_display_rows(failure,language)
 failure_html<-as.character(structural_canvas_basic_html_table(localized_failure,language=language))
 rendered<-xml2::xml_text(read(failure_html))
 if(language!='en'){
  stopifnot(!grepl('{reason}',rendered,fixed=TRUE),grepl('Normality 사용자 <&>',rendered,fixed=TRUE))
  for(reason in head(reasons,-1))stopifnot(!grepl(reason,rendered,fixed=TRUE))
 }
 # Source content and statuses remain raw for English reports and audit exports.
 stopifnot(identical(raw,structural_canvas_pls_quality_rows(formative_bundle)))
 writeLines(paste0(html,failure_html),file.path(dynamic_out,paste0(language,'-dynamic.html')),useBytes=TRUE)
 if(language=='ja')dynamic_entries<-c(dynamic_entries,list(list(id='pls-dynamic',title='PLS dynamic quality',html=paste0(html,failure_html))))
 cat('PASS:',language,'formative documented/missing evidence, user names, and all generated f2 failure reasons\n')
}
saveRDS(dynamic_entries,file.path(dynamic_out,'entries.rds'))
