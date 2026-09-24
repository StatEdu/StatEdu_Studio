Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
root <- 'tmp/merge-headerless-fixtures';dir.create(root,recursive=TRUE,showWarnings=FALSE)
data <- list(data.frame(id=c(1,2),Review=c(10,20),Group=c('Review','Normality')),
             data.frame(id=c(2,3),Review=c(30,40),Group=c('Normality','Review')))
specs <- list(csv=list(ext='csv',sep=',',delimiter='comma',prefix='V'),
 dat_space=list(ext='dat',sep=' ',delimiter='whitespace',prefix='X'),
 dat_tab=list(ext='dat',sep='\t',delimiter='tab',prefix='X'),
 dat_comma=list(ext='dat',sep=',',delimiter='comma',prefix='X'))
manifest <- list()
for(name in names(specs)) {
 spec <- specs[[name]];paths <- file.path(root,paste0(name,c('_a.','_b.'),spec$ext))
 for(i in 1:2)write.table(data[[i]],paths[[i]],sep=spec$sep,row.names=FALSE,col.names=FALSE,quote=FALSE)
 uploaded <- data.frame(name=paste0(c('사용자.','second.'),spec$ext),datapath=paths)
 options <- list(merge_csv_header=FALSE,merge_dat_has_names=FALSE,merge_dat_delimiter=spec$delimiter)
 files <- merge_uploaded_files(uploaded,options)
 expected_names <- paste0(spec$prefix,1:3)
 for(i in 1:2) {
  stopifnot(identical(names(files[[i]]),expected_names),nrow(files[[i]])==2L)
  for(j in 1:3)stopifnot(identical(as.character(files[[i]][[j]]),as.character(data[[i]][[j]])))
 }
 for(mode in c('left','inner','full')) {
  result <- merge_add_variables(files,expected_names[[1]],mode)
  stopifnot(nrow(result)==switch(mode,left=2L,inner=1L,full=3L),ncol(result)==5L)
 }
 cases <- merge_add_cases(files,expected_names,'사용자_시점','시작,종료')
 stopifnot(nrow(cases)==4L,identical(as.character(cases[[3]]),c('Review','Normality','Normality','Review')))
 # Enabling headers consumes the first data row; disabling restores all rows.
 header_options<-options;header_options$merge_csv_header<-TRUE;header_options$merge_dat_has_names<-TRUE
 stopifnot(all(vapply(merge_uploaded_files(uploaded,header_options),nrow,integer(1))==1L))
 stopifnot(all(vapply(merge_uploaded_files(uploaded,options),nrow,integer(1))==2L))
 manifest[[name]] <- list(paths=normalizePath(paths,winslash='/'),names=as.character(uploaded$name),delimiter=spec$delimiter,key=expected_names[[1]])
 cat('PASS:',name,'headerless names/first row, variable and case merge, header toggle recovery\n')
}
jsonlite::write_json(manifest,file.path(root,'manifest.json'),auto_unbox=TRUE,pretty=TRUE)
