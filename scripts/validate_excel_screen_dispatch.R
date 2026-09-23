Sys.setlocale('LC_ALL','Korean_Korea.utf8')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
functions<-Filter(function(n)grepl('save_screen_excel_file',paste(deparse(body(get(n))),collapse=''),fixed=TRUE),ls(pattern='^save_.*_excel_file$'))
stopifnot(length(functions)==19L)
for(n in functions){
 f<-get(n);body_call<-body(f)[[2]];html_name<-as.character(body_call[[2]][[1]])
 e<-new.env(parent=environment(f));e[[html_name]]<-function(...) '<html><body><table><tr><th>value</th></tr><tr><td>0.0000</td></tr></table></body></html>'
 e$save_screen_excel_file<-function(html,file){stopifnot(grepl('0.0000',html,fixed=TRUE));TRUE};environment(f)<-e
 args<-list();for(a in names(formals(f)))if(identical(formals(f)[[a]],quote(expr=)))args[a]<-list(if(a=='file')tempfile(fileext='.xlsx') else list())
 stopifnot(isTRUE(do.call(f,args)))
}
cat('19 Excel entry-point dispatch checks passed (stub screen fixture).\n')
