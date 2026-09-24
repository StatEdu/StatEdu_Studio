Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
# Extract the existing title inventory without duplicating it in this check.
nodes<-as.list(body(survival_appendix_title))
assignment<-Filter(function(x)is.call(x)&&identical(x[[1]],as.name('<-'))&&identical(x[[2]],as.name('labels')),nodes)
stopifnot(length(assignment)==1)
labels<-eval(assignment[[1]][[3]])
records<-list()
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(title in names(labels)) {
 actual<-survival_appendix_title(title,lang)
 stopifnot(length(actual)==1,nzchar(actual))
 if(lang=='en')stopifnot(identical(actual,title)) else stopifnot(actual!=title)
 if(lang=='ko')stopifnot(identical(actual,unname(labels[[title]])))
 records[[length(records)+1L]]<-data.frame(language=lang,english=title,display=actual)
}
out<-'tmp/survival-appendix-title-catalog';dir.create(out,recursive=TRUE,showWarnings=FALSE)
write.csv(do.call(rbind,records),file.path(out,'titles.csv'),row.names=FALSE,fileEncoding='UTF-8')
cat('PASS',length(labels),'appendix title mappings across eight languages; existing Korean titles preserved\n')
