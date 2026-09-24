Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(941);s<-rnorm(40);d<-as.data.frame(setNames(lapply(1:6,function(i)s+rnorm(40,sd=.2)),paste0('x',1:6)))
d$x2<-d$x2+5;d$x3<-d$x3+10;d[1,]<-d[1,]+100;d[2,1]<-NA
info<-data.frame(name=names(d),measurement='continuous')
run<-function(assumption)prepare_one_group_rm_anova_results(d,input_format='wide',experimental_variables=names(d)[1:3],control_variables=names(d)[4:6],variable_info=info,options=list(assumption_check=assumption,posthoc=TRUE,treatment_labels=c('사용자 처치','Normality'),time_labels=c('Yes','사용자 시점','Status')))
fits<-list(flagged=run(TRUE),unchecked=run(FALSE))
phrases<-jsonlite::read_json('scripts/fixtures/one_group_i18n_branches.json',simplifyVector=TRUE)
raw<-unlist(lapply(fits,function(f)unlist(f$recommendation)),use.names=FALSE)
stopifnot(all(phrases[1:5] %in% raw),'Excluded subjects: 1.' %in% raw)
out<-'tmp/one-group-branches-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);baseline<-list();exports<-list()
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language);all_cells<-character()
 for(name in names(fits)){
  html<-as.character(one_group_rm_anova_results_ui(fits[[name]]));doc<-xml2::read_html(html,encoding='UTF-8')
  main<-xml2::xml_find_all(doc,"//table[@data-result-table-role='main']")
  content<-lapply(main,function(t)xml2::xml_text(xml2::xml_find_all(t,".//th|.//td|preceding::h3[1]|ancestor::div[@data-result-table-sheet='true'][1]/*[not(self::table)]")))
  if(language=='en')baseline[[name]]<-content
  stopifnot(identical(content,baseline[[name]]))
  all_cells<-c(all_cells,xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']//td")))
  if(language=='ja')exports[[name]]<-html
 }
 for(phrase in phrases[phrases %in% raw]){
  key<-paste0('analysis.ui.',gsub('^_|_$','',gsub('[^a-z0-9]+','_',tolower(phrase))))
  if(!statedu_t(key,language) %in% all_cells)stop(language,' missing ',phrase)
 }
 stopifnot(sprintf(statedu_t('analysis.one_group.excluded_subjects',language),'1') %in% all_cells)
 # Unreached interpretation guards are fixture checks, not actual failed fits.
 guards<-data.frame(Recommendation=phrases[6:7]);localized<-mixed_rm_appendix_table(guards)
 for(i in 1:2)if(localized[[1]][i]!=result_appendix_ui_text(phrases[i+5],language))stop(language,': ',localized[[1]][i],' != ',result_appendix_ui_text(phrases[i+5],language))
 cat('PASS:',language,'significant interaction, missing subject and nonnormal data; English main; actual recommendations and exclusion count\n')
}
saveRDS(list(list(id='one-group-branches',title='Within-subject repeated measures',html=paste(unlist(exports),collapse='\n'))),file.path(out,'entries.rds'))
