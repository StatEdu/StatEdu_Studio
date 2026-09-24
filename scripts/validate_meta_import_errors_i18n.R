Sys.setlocale('LC_CTYPE','English_United States.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
capture <- function(expr) {value<-tryCatch(expr,error=identity);stopifnot(inherits(value,'statedu_meta_import_error'));value}
make_file <- function(mode, family='g', include_data=FALSE) {
 path<-tempfile(fileext='.xlsx'); wb<-openxlsx::createWorkbook();openxlsx::addWorksheet(wb,'StatEdu')
 openxlsx::writeData(wb,'StatEdu',data.frame(key=c('entry_mode','target_family'),value=c(mode,family)),startRow=12,colNames=FALSE)
 if(include_data) {
   openxlsx::addWorksheet(wb,'All_Input')
   openxlsx::writeData(wb,'All_Input',data.frame(study_name='사용자 <&> %s',yi='0.25'))
 }
 openxlsx::saveWorkbook(wb,path,overwrite=TRUE)
 list(name='사용자 %s.xlsx',datapath=path)
}
files<-list(all_sheet=make_file('ALL'),type_sheets=make_file('ENTRY_MODE'),
 family=make_file('ALL',"사용자 '%s' <&>"),mode=make_file('CUSTOM'),valid=make_file('ALL',include_data=TRUE))
errors<-lapply(files[names(files)!='valid'],function(file)capture(meta_read_effect_input_file(file,'g')))
errors$extension<-capture(meta_read_effect_input_file(list(name='file.txt'),'g'))
# Simulate the package check only; do not remove the installed package.
without_readxl<-meta_read_effect_input_file
environment(without_readxl)<-list2env(list(requireNamespace=function(...)FALSE),parent=environment(meta_read_effect_input_file))
errors$readxl<-capture(without_readxl(files$valid,'g'))
before<-lapply(files,function(file)readBin(file$datapath,'raw',n=file.info(file$datapath)$size))
for(lang in c('en','ko','ja','zh','es','fr','de','vi','ko')) {
 for(key in names(errors)) {
   error<-errors[[key]];stopifnot(error$key==key)
   expected<-do.call(sprintf,c(list(statedu_t(paste0('meta.import_error.',key),lang)),error$values))
   actual<-meta_import_error_text(error,lang)
   stopifnot(identical(actual,expected))
   if(lang=='en')stopifnot(actual==conditionMessage(error))else stopifnot(actual!=conditionMessage(error))
   for(value in error$values)stopifnot(grepl(value,actual,fixed=TRUE))
 }
 imported<-meta_read_effect_input_file(files$valid,'g')
 stopifnot(imported$study_name=='사용자 <&> %s',imported$yi=='0.25')
 raw<-'External: D:/사용자 %s <&>/file.xlsx';stopifnot(meta_import_error_text(simpleError(raw),lang)==raw)
 cat('PASS:',lang,'six import errors (readxl check simulated); workbook values/raw diagnostics preserved; valid import unchanged\n')
}
for(i in seq_along(files))stopifnot(identical(before[[i]],readBin(files[[i]]$datapath,'raw',n=file.info(files[[i]]$datapath)$size)))
unlink(vapply(files,function(file)file$datapath,character(1)))
