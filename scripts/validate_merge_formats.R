Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
root <- 'tmp/merge-format-fixtures';dir.create(root,recursive=TRUE,showWarnings=FALSE)
data <- list(data.frame(id=c(1,2),Review=c(10,20),Group=c('Review','Normality')),
             data.frame(id=c(2,3),Review=c(30,40),Group=c('Normality','Review')))
formats <- list(
 sav=list(ext='sav',write=function(d,p)haven::write_sav(d,p)),
 sas=list(ext='sas7bdat',write=function(d,p)haven::write_sas(d,p)),
 xpt=list(ext='xpt',write=function(d,p)haven::write_xpt(d,p,version=8)),
 dta=list(ext='dta',write=function(d,p)haven::write_dta(d,p)),
 xlsx=list(ext='xlsx',write=function(d,p)openxlsx::write.xlsx(d,p)),
 dat_space=list(ext='dat',delimiter='whitespace',write=function(d,p)write.table(d,p,row.names=FALSE,quote=FALSE,sep=' ')),
 dat_tab=list(ext='dat',delimiter='tab',write=function(d,p)write.table(d,p,row.names=FALSE,quote=FALSE,sep='\t')),
 dat_comma=list(ext='dat',delimiter='comma',write=function(d,p)write.table(d,p,row.names=FALSE,quote=FALSE,sep=','))
)
manifest <- list()
for(name in names(formats)) {
 format <- formats[[name]]
 paths <- file.path(root,paste0(name,c('_a.','_b.'),format$ext))
 for(i in 1:2)format$write(data[[i]],paths[[i]])
 uploaded <- data.frame(name=paste0(c('사용자.','second.'),format$ext),datapath=paths)
 files <- merge_uploaded_files(uploaded,list(merge_dat_delimiter=format$delimiter %||% 'whitespace',merge_dat_has_names=TRUE))
 for(i in 1:2) {
  stopifnot(identical(names(files[[i]]),names(data[[i]])))
  for(column in names(data[[i]]))stopifnot(identical(as.character(files[[i]][[column]]),as.character(data[[i]][[column]])))
 }
 for(mode in c('left','inner','full')) {
  result <- merge_add_variables(files,'id',mode)
  stopifnot(nrow(result)==switch(mode,left=2L,inner=1L,full=3L),identical(names(result),c('id','Review','Group','Review_1','Group_1')))
 }
 result <- merge_add_cases(files,c('id','Review','Group'),'사용자_시점','시작,종료')
 stopifnot(identical(as.character(result$Group),c('Review','Normality','Normality','Review')),
           identical(result[['사용자_시점']],c('시작','시작','종료','종료')))
 manifest[[name]] <- list(paths=normalizePath(paths,winslash='/'),names=as.character(uploaded$name),delimiter=format$delimiter %||% 'whitespace')
 cat('PASS:',name,'actual upload reader; variable joins; case merge; user values\n')
}
jsonlite::write_json(manifest,file.path(root,'manifest.json'),auto_unbox=TRUE,pretty=TRUE)
