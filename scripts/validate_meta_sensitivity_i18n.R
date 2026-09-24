Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
labels<-c('Normality','Status','Yes','사용자 %s <&>')
rows<-do.call(rbind,lapply(1:4,function(i)meta_normalize_effect(list(study_id=labels[i],study_name=labels[i],outcome=labels[i],predictor=labels[i],family='g',input_type='g_se',g=i/10,se=.1))))
result<-meta_fit_model(rows,'g',model='fixed')
dep<-meta_dependency_sensitivity(result);leave<-meta_leave_one_study_out(result);trim<-meta_trimfill(result)
original<-list(result,dep,leave,trim)
make<-function(lang)list(meta_dependency_sensitivity_table(dep,'g',lang),meta_leave_one_study_out_table(leave,'g',lang),meta_study_results_table(result,lang),meta_trimfill_results_table(trim,lang))
english<-make('en')
header_keys<-list(c('rho','studies','lower','upper','tau'),c('omitted','lower','upper','change'),c('id','study','year','outcome','predictor','lower','upper','weight'),c('estimator','side','observed','missing','original','adjusted','adjusted_lower','adjusted_upper'))
columns<-list(c(1,2,4,5,6),c(1,3,4,5),c(1,2,3,4,5,7,8,9),1:8)
for(lang in c('en','ko','ja','zh','es','fr','de','vi','ko')) {
 tables<-make(lang)
 for(i in seq_along(tables)) {
  tab<-tables[[i]]
  stopifnot(identical(unname(names(tab)[columns[[i]]]),unname(vapply(header_keys[[i]],function(key)statedu_t(paste0('meta.sensitivity.',key),lang),character(1)))))
  for(j in seq_along(tab))if(!(i==4&&j==2))stopifnot(identical(tab[[j]],english[[i]][[j]]))
  html<-xml2::read_html(as.character(meta_result_section('Appendix',tab,'appendix',lang)),encoding='UTF-8')
  cells<-xml2::xml_text(xml2::xml_find_all(html,'//td'))
  if(i %in% c(2,3))stopifnot(all(labels %in% cells))
  note<-meta_table_note(tab,lang)
  stopifnot(nzchar(note),!grepl('meta.sensitivity.',note,fixed=TRUE))
  stopifnot(grepl(statedu_t('meta.note.CI',lang),note,fixed=TRUE))
  if(i==1)stopifnot(grepl(statedu_t('meta.note.τ²',lang),note,fixed=TRUE))
 }
 for(side in c('left','right'))stopifnot(meta_trimfill_results_table(modifyList(trim,list(side=side)),lang)[[2]]==statedu_t(paste0('meta.sensitivity.',side),lang))
 stopifnot(identical(original,list(result,dep,leave,trim)))
 cat('PASS sensitivity:',lang,'headers, raw labels, numeric values, notes, directions\n')
}
