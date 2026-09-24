Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/longitudinal-missing-summary-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
d<-data.frame(id=rep(1:6,each=3),time=rep(c('Warning','Value','Failed'),6),
 y=seq_len(18),x=seq_len(18)/10,site=rep(1:2,each=9),exposure=1)
m<-d;m$y[c(2,5)]<-NA;m$x[c(7,8)]<-NA;m$id[10]<-NA;m$time[11]<-NA;m$site[12]<-NA;m$exposure[13]<-NA
tables<-list()
for(kind in c('complete','rowwise','available')) {
 raw<-if(kind=='complete')d else m;complete<-complete.cases(raw)
 keep<-if(kind=='available')!is.na(raw$y)&!is.na(raw$id)&!is.na(raw$time) else complete
 retained<-raw[keep,,drop=FALSE]
 tables[[paste0(kind,'_structure')]]<-longitudinal_data_structure_summary(retained,nrow(raw),sum(keep),sum(!keep),'id','time','site',
  missing_method_label=longitudinal_missing_method_label(if(kind=='available')'available' else 'complete_case'))
 tables[[paste0(kind,'_patterns')]]<-longitudinal_missing_pattern_summary(raw,complete,keep,'id','time','y','x','site','exposure')
 tables[[paste0(kind,'_time')]]<-longitudinal_missing_by_time_summary(raw,'time','y',names(raw))
}
stopifnot(nrow(longitudinal_missing_pattern_summary(d[FALSE,],NULL,NULL,'id','time','y','x'))==0,
 nrow(longitudinal_missing_by_time_summary(d,'absent','y',names(d)))==0)
before<-serialize(list(d,m,tables),NULL);missing<-list()
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(name in names(tables)) {
 original<-tables[[name]];translated<-longitudinal_appendix_table(original,lang)
 source<-c(names(original),if('Item'%in%names(original))original$Item else character())
 target<-c(names(translated),if('Item'%in%names(original))translated[[1]] else character())
 absent<-which(nzchar(source)&source==target)
 if(lang!='en' && length(absent))for(i in absent)missing[[length(missing)+1L]]<-data.frame(language=lang,table=name,english=source[i])
 if('Item'%in%names(original)) {
  numeric_rows<-grepl('^[0-9]+([.][0-9]+)?( \\([0-9.]+%\\))?$',original$Value)
  stopifnot(identical(original$Value[numeric_rows],translated[[2]][numeric_rows]))
 } else {
  for(i in seq_along(original))stopifnot(identical(original[[i]],translated[[i]]))
  # Old in-memory results may not carry the new user-column attribute.
  legacy<-original;attr(legacy,'result_user_columns')<-NULL
  displayed<-longitudinal_display_missing_by_time_table(list(missing_by_time=legacy))
  rendered<-longitudinal_appendix_table(displayed,lang)
  stopifnot(identical(rendered[[1]],original$Time),
    identical(rendered[[ncol(rendered)]],vapply(original$`Any missing %`,longitudinal_format_number,character(1))))
 }
 probe<-data.frame(Variable='Subjects / clusters retained',Details='Subjects / clusters retained')
 stopifnot(identical(longitudinal_appendix_table(probe,lang)[[1]],probe$Variable),identical(before,serialize(list(d,m,tables),NULL)))
}
if(length(missing)) {
 missing<-do.call(rbind,missing);write.csv(missing,file.path(out,'missing.csv'),row.names=FALSE,fileEncoding='UTF-8')
 cat(paste(unique(missing$english),collapse='\n'),'\n');stop('Untranslated summary labels',call.=FALSE)
}
saveRDS(tables,file.path(out,'tables.rds'))
cat('PASS nine structure/missingness/time tables in eight languages; counts, percentages, time values and source data preserved\n')
