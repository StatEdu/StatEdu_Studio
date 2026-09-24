Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(940);s<-rnorm(36);d<-as.data.frame(setNames(lapply(1:6,function(i)s+i/4+rnorm(36,sd=.5)),paste0('x',1:6)))
info<-data.frame(name=names(d),measurement='continuous',var_label=c('Normality','Yes','사용자 변수','Status','사용자 항목','Factor'))
fit<-prepare_one_group_rm_anova_results(d,input_format='wide',experimental_variables=names(d)[1:3],control_variables=names(d)[4:6],variable_info=info,options=list(assumption_check=TRUE,posthoc=TRUE,treatment_labels=c('사용자 처치','Normality'),time_labels=c('Yes','사용자 시점','Status')))
out<-'tmp/one-group-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);baseline<-NULL
phrases<-jsonlite::read_json('scripts/fixtures/one_group_i18n_prose.json',simplifyVector=TRUE)
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language);html<-as.character(one_group_rm_anova_results_ui(fit));doc<-xml2::read_html(html,encoding='UTF-8')
 main<-xml2::xml_find_all(doc,"//table[@data-result-table-role='main']")
 content<-lapply(main,function(t)xml2::xml_text(xml2::xml_find_all(t,".//th|.//td|preceding::h3[1]|ancestor::div[@data-result-table-sheet='true'][1]/*[not(self::table)]")))
 stopifnot(length(main)>0L,all(xml2::xml_attr(main,'data-result-table-language')=='en'))
 if(language=='en')baseline<-content
 stopifnot(identical(content,baseline))
 appendix<-xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']")
 stopifnot(all(xml2::xml_attr(appendix,'data-result-table-language')==language))
 normality<-appendix[[length(appendix)]]
 stopifnot(all(c('사용자 처치','Normality') %in% xml2::xml_text(xml2::xml_find_all(normality,'.//th'))),all(c('Yes','사용자 시점','Status') %in% xml2::xml_text(xml2::xml_find_all(normality,'.//td'))))
  cells<-xml2::xml_text(xml2::xml_find_all(appendix,'.//td'))
  for(phrase in phrases){
    key<-paste0('analysis.ui.',gsub('^_|_$','',gsub('[^a-z0-9]+','_',tolower(phrase))))
    if(!statedu_t(key,language) %in% cells)stop(language,' missing prose: ',phrase)
  }
  stopifnot(sprintf(statedu_t('analysis.one_group.excluded_subjects',language),'0') %in% cells)
 stopifnot(paste(fit$treatment_labels,collapse=', ') %in% cells)
 for(key in c('input_format','input_fields','treatment_levels','within_subject_treatment_x_time_repeated_measures_anova'))stopifnot(statedu_t(paste0('analysis.ui.',key),language) %in% xml2::xml_text(xml2::xml_find_all(appendix,'.//th|.//td')))
 if(language=='ja')saveRDS(list(list(id='one-group',title='Within-subject repeated measures',html=html)),file.path(out,'entries.rds'))
 cat('PASS:',language,'actual wide within-subject analysis; main English; treatment/time labels; selected appendix headings\n')
}
