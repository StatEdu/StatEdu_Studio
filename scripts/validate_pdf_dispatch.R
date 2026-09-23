Sys.setlocale('LC_ALL','Korean_Korea.utf8')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
writers<-setdiff(ls(pattern='^write_.*pdf$'),'write_pdf_from_html')
reaches<-function(start,target) {
 queue<-start;seen<-character()
 while(length(queue)) {
  n<-queue[1];queue<-queue[-1]
  if(n==target)return(TRUE)
  if(n%in%seen)next
  seen<-c(seen,n)
  if(!exists(n,envir=.GlobalEnv,mode='function',inherits=FALSE))next
  calls<-intersect(all.names(body(get(n,envir=.GlobalEnv)),functions=TRUE,unique=TRUE),ls(envir=.GlobalEnv))
  queue<-unique(c(queue,setdiff(calls,seen)))
 }
 FALSE
}
for(n in writers)if(!reaches(n,'saved_result_sheet_document'))stop(n)
cat(length(writers),'PDF writer paths reach the shared pagination function:\n',paste(writers,collapse='\n'),'\n')
