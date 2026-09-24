for(e in parse('scripts/compare_menu_versions.R')) {
 if(is.call(e)&&identical(e[[1]],as.name('<-'))&&identical(e[[2]],as.name('ids')))break
 eval(e,.GlobalEnv)
}
options(statedu.isolated_lavaan_bootstrap_worker=TRUE,statedu.pls.bootstrap.workers=8L)
metadata_state<-structural_canvas_lavaan_worker_metadata_fast_path_install()
run_checks<-function(){
 on.exit(metadata_state$restore(),add=TRUE)
 set.seed(20260814);z<-rnorm(180)
 d<-data.frame(x1=.8*z+rnorm(180,sd=.6),x2=.7*z+rnorm(180,sd=.7),x3=.9*z+rnorm(180,sd=.5))
 syntax<-'eta1 =~ x1 + x2 + x3'
 fit<-lavaan::cfa(syntax,data=d,estimator='ML',missing='listwise',auto.cov.lv.x=FALSE)
 pls<-run_structural_canvas_analysis(struct$snapshot,struct$data,'plssem')
 for(kind in if(run==1L)c('CFA','PLS')else c('PLS','CFA')){
  gc();start<-Sys.time()
  captured<-capture(if(kind=='CFA')function() structural_canvas_reliability_bootstrap(syntax,d,reps=1000L,seed=20260826L,estimator='ML',missing='listwise',original_fit=fit,workers=8L,chunk_size=250L,return_draws=TRUE)else function()structural_canvas_run_pls_bootstrap('plssem',5000L,pls,20260826L))
  seconds<-as.numeric(difftime(Sys.time(),start,units='secs'))
  saveRDS(list(result=captured,seconds=seconds,kind=kind,version=version,iteration=run),file.path(out,paste0('extra-boot-',kind,'-',version,'-',run,'.rds')))
  cat(kind,version,run,seconds,'error=',if(is.null(captured$error))'none'else captured$error$message,'\n')
 }
}
run_checks()
