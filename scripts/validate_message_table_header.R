.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
root <- 'output/message-table-header-review-20260915'
dir.create(root,recursive=TRUE,showWarnings=FALSE)
old <- new.env(parent=.GlobalEnv);new <- new.env(parent=.GlobalEnv)
sys.source('scripts/fixtures/message_table_header_reference.R',old);sys.source('R/data_ui_tables.R',new)
old$message_table_datatable<-compiler::cmpfun(old$message_table_datatable)
new$message_table_datatable<-compiler::cmpfun(new$message_table_datatable)
capture <- function(env,args) {
 ds<-list(); value<-NULL
 stdout<-capture.output(value<-tryCatch(withCallingHandlers(do.call(env$message_table_datatable,args),warning=function(e){ds[[length(ds)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleWarning')},message=function(e){ds[[length(ds)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleMessage')}),error=function(e)list(error_class=class(e),error=conditionMessage(e))))
 if(inherits(value,'htmlwidget')) {
   value$elementId <- 'comparison-widget'
   html <- as.character(htmltools::renderTags(value)$html)
   hook <- value$preRenderHook
   value$preRenderHook <- list(formals=formals(hook),body=body(hook))
 } else html <- NULL
 list(value=value,html=html,diagnostics=ds,stdout=stdout,rng=.Random.seed)
}
set.seed(20260915);checks<-0L
for(lang in list('en','ko','bad','',NULL,NA_character_,c('ko','en'))) for(msg in list('No data','안내 <b>&"',NULL,NA_character_,character(),c('a','b'),42)) for(esc in c(TRUE,FALSE)) for(opt in list(list(dom='t'),list(dom='t',paging=FALSE,ordering=FALSE))) {
 args<-list(message=msg,language=lang,escape=esc,options=opt)
 before<-.Random.seed;a<-capture(old,args);.Random.seed<-before;b<-capture(new,args)
 if(!identical(a,b,num.eq=FALSE)){saveRDS(list(args=args,old=a,new=b),file.path(root,'mismatch.rds'));stop('Exact mismatch')}
 checks<-checks+1L
}
cat('Fixed-ID HTML, widget payload, hook definition, diagnostics/stdout/RNG checks:',checks,'\n')

