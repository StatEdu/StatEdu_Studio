Sys.setlocale('LC_ALL','Korean_Korea.utf8')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
d<-data.frame(id=rep(1:10,each=2),time=rep(1:2,10),y=seq_len(20))
info<-data.frame(name=names(d),var_label=names(d),measurement='continuous')
shiny::testServer(function(input,output,session){
 register_longitudinal_handlers(input,output,session,function()names(d),function()d,function()info,function()character(),function()NULL,function(){})
}, {
 session$flushReact()
 session$setInputs(longitudinal_model_type='lmm',longitudinal_corstr='exchangeable')
 session$flushReact()
 a<-output$longitudinal_setup
 session$setInputs(longitudinal_corstr='reml_un');session$flushReact()
 b<-output$longitudinal_setup
 stopifnot(grepl('reml_un',b$html,fixed=TRUE))
 session$setInputs(longitudinal_corstr='reml_un');session$flushReact()
 c<-output$longitudinal_setup
 stopifnot(identical(b$html,c$html))
 session$setInputs(longitudinal_corstr='reml_ar1');session$flushReact()
 e<-output$longitudinal_setup
 stopifnot(!identical(c$html,e$html))
})
cat('Repeated covariance input rebind does not rerender; actual covariance change does.\n')
