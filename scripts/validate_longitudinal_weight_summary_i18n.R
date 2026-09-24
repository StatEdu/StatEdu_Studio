Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(917)
weight_data<-data.frame(id=rep(1:20,each=3),time=rep(1:3,20),x=rnorm(60),y=rnorm(60),check.names=FALSE)
weight_name<-'Warning'
weight_data[[weight_name]]<-seq(.5,2,length.out=60)
weight_data$y[c(2,8,15,23,37)]<-NA
weight_cases<-weight_data[complete.cases(weight_data),]
weight_results<-setNames(lapply(c('none','sampling','longitudinal','ipw','combined'),function(mode)
 longitudinal_prepare_analysis_weights(weight_data,weight_cases,'y','id','time',c('time','x'),weight_name,mode,if(mode=='longitudinal')'p05_95' else 'p01_99')),c('none','sampling','longitudinal','ipw','combined'))
before_weights<-serialize(weight_results,NULL)
for(lang in c('ko','ja','zh','es','fr','de','vi'))for(trim in c('none','p01_99','p05_95')) {
 label<-longitudinal_weight_trim_label(trim)
 stopifnot(longitudinal_appendix_text(label,lang)!=label)
}
untranslated_weight_items<-character(0)
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(mode in names(weight_results)) {
 original<-longitudinal_display_weight_summary_table(list(weight_summary=weight_results[[mode]]$summary))
 translated<-longitudinal_appendix_table(original,lang)
 if(!identical(translated[[2]][1],weight_name))stop('Weight name changed: ',lang,' ',mode)
 if(lang!='en') {
  for(i in seq_len(nrow(original))) {
   if(identical(original$Item[i],translated[[1]][i]))untranslated_weight_items<-unique(c(untranslated_weight_items,paste(lang,original$Item[i])))
  }
  note_index<-which(original$Item=='Note')
  if(identical(original$Value[note_index],translated[[2]][note_index]))stop('Untranslated note: ',lang,' ',mode)
  for(i in which(original$Item %in% c('Weight type','Trimming','Normalization','IPW diagnostic note'))) {
   if(identical(original$Value[i],translated[[2]][i]))untranslated_weight_items<-unique(c(untranslated_weight_items,paste(lang,'value:',original$Value[i])))
  }
 }
 numeric_rows<-which(original$Item %in% c('Effective sample size','Predicted observation probability: min','Predicted observation probability: median','Predicted observation probability: max','Probability clipping count','Generated IPW effective sample size','Weight clipping count'))
 for(i in which(original$Item %in% c('Base weight summary','IPW summary','Final weight summary','Generated IPW summary') & nzchar(original$Value))) {
  numbers<-sub('^[^=]+=', '',strsplit(original$Value[i],'; ',fixed=TRUE)[[1]])
  expected<-do.call(sprintf,c(list(statedu_t('longitudinal.weight_stats',lang)),as.list(numbers)))
  stopifnot(identical(translated[[2]][i],expected))
 }
 stopifnot(identical(original$Value[numeric_rows],translated[[2]][numeric_rows]))
 cat('PASS weight summary:',lang,mode,'\n')
}
stopifnot(identical(before_weights,serialize(weight_results,NULL)))
if(length(untranslated_weight_items))stop('Untranslated items: ',paste(untranslated_weight_items,collapse='; '))
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 precise<-'min=-1.2300e-09; median=.5000; max=2.000E+02'
 expected<-sprintf(statedu_t('longitudinal.weight_stats',lang),'-1.2300e-09','.5000','2.000E+02')
 stopifnot(identical(longitudinal_appendix_text(precise,lang),expected))
 user<-data.frame(Variable=precise,Details=precise)
 stopifnot(identical(longitudinal_appendix_table(user,lang)[[1]],precise))
 unknown<-'min=user; median=custom; max=label'
 stopifnot(identical(longitudinal_appendix_text(unknown,lang),unknown))
}
