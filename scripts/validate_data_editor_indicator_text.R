Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
values <- c('Normality','morning','return','사용자 <&> %s', 'C:\\runs\\new')
for(sep in c(',', '\n', '\r\n', '\r')) {
 text <- paste(values,collapse=sep)
 stopifnot(identical(wide_long_parse_indicator_values(text,length(values)),values),
           identical(merge_parse_indicator_values(text,length(values)),values))
}
for(sep in c('\n','\r\n')) {
 text <- paste(paste0(c('x1','x2'),'=',values[1:2]),collapse=sep)
 stopifnot(identical(wide_long_manual_time_values(text,c('x1','x2')),setNames(values[1:2],c('x1','x2'))))
}
files <- list(data.frame(id=1),data.frame(id=2))
merged <- merge_add_cases(files,'id','시점','Normality,morning')
stopifnot(identical(merged[['시점']],values[1:2]))
spec <- wide_long_make_spec(c('x1','x2'),'측정값',index_name='시점',index_values='Normality,morning')
long <- wide_long_transform_configured(data.frame(id=1,x1=10,x2=20),list(spec),id_variables='id')
stopifnot(setequal(long[['시점']],values[1:2]),identical(long[['측정값']][match(values[1:2],long[['시점']])],c(10,20)))
cat('PASS: literal r/n and backslashes preserved; comma/LF/CRLF/CR separators; manual time mapping; actual merge/reshape labels and values\n')
