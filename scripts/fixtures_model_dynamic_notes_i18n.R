out<-'tmp/model-dynamic-notes-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
l<-list(sample_size_lmm_target='power',sample_size_lmm_alpha='0.05',sample_size_lmm_power='0.8',sample_size_lmm_n='30',sample_size_lmm_dropout='0',sample_size_lmm_mode='glimmpse',sample_size_lmm_design='one_group_repeated',sample_size_lmm_time_points='3',sample_size_lmm_simulations='20',sample_size_lmm_group1_means='0, 0.2, 0.4',sample_size_lmm_residual_sd='1',sample_size_lmm_rho='0.3',sample_size_lmm_correlations='0.3, 0.2, 0.3')
s<-list(sample_size_sem_target='power',sample_size_sem_alpha='0.05',sample_size_sem_power='0.8',sample_size_sem_n='120',sample_size_sem_dropout='0',sample_size_sem_test='parameter',sample_size_sem_parameter='-0.3',sample_size_sem_simulations='100')
structures<-c('exchangeable','ar1','unstructured');cases<-expand.grid(type=c('path','loading','correlation'),complexity=c('simple','moderate','complex'),stringsAsFactors=FALSE)
results<-c(lapply(structures,function(d)sample_size_calculate('lmm',modifyList(l,list(sample_size_lmm_correlation_structure=d)))),lapply(1:9,function(i)sample_size_calculate('sem',modifyList(s,list(sample_size_sem_parameter_type=cases$type[i],sample_size_sem_complexity=cases$complexity[i])))))
designs<-c(paste('gls',structures),paste('sem',cases$type,cases$complexity));formula_keys<-paste0('sample_size.result.',c(rep('planning_lmm_gls',3),rep('planning_sem_parameter',9)))
raw<-serialize(results,NULL);clean<-function(x)gsub('[[:space:]\u00a0]+','',x,perl=TRUE)
for(lang in c('en','ko','ja','zh','es','fr','de','vi')){
 label<-function(k)statedu_t(paste0('sample_size.result.note_value_',k),lang,fallback='')
 for(i in seq_along(results)){
  stopifnot(is.null(results[[i]]$error),is.finite(results[[i]]$power))
  if(i<=3)expected<-sprintf(statedu_t('sample_size.result.note_gls_structure_counts',lang),label(structures[i]),'20') else {
   j<-i-3;expected<-sprintf(statedu_t('sample_size.result.note_sem_parameter_counts',lang),label(cases$type[j]),'-0.30',label(cases$complexity[j]),'100')
  }
  stopifnot(nzchar(expected),identical(sample_size_result_text(results[[i]]$method_note,lang),expected))
  rendered<-xml2::xml_text(xml2::read_html(as.character(sample_size_results_ui(results[[i]],lang))))
  parts<-trimws(strsplit(result_sci_note_text(estimation=expected),';',fixed=TRUE)[[1]])
  for(part in parts)stopifnot(grepl(clean(sub('[.。]$','',part)),clean(rendered),fixed=TRUE))
 }
 edge<-sprintf(statedu_t('sample_size.result.note_sem_parameter_counts','en'),'standardized path','-0.00','simple','00100')
 stopifnot(identical(sample_size_result_text(edge,lang),sprintf(statedu_t('sample_size.result.note_sem_parameter_counts',lang),label('path'),'-0.00',label('simple'),'00100')))
 unknown<-c(paste0(edge,' appended'),sub('standardized path','custom user label',edge,fixed=TRUE),sub('simple complexity','unknown complexity',edge,fixed=TRUE),sub('-0.00','0.3',edge,fixed=TRUE))
 for(x in unknown)stopifnot(identical(sample_size_result_text(x,lang),x))
}
stopifnot(identical(raw,serialize(results,NULL)))
cat('PASS 3 GLIMMPSE structures + 9 SEM type/complexity combinations x eight languages; exact numeric strings, strict boundaries and source preservation\n')
