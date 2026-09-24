.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
root<-'output/survival-plot-jit-validation-20260914'
dir.create(root,recursive=TRUE,showWarnings=FALSE)
current<-survival_draw_plot_with_risk_table
baseline<-environment(current)$draw
stopifnot(is.function(baseline),identical(formals(baseline),formals(current)))
initial_jit<-compiler::enableJIT(3)
on_exit_restore<-function(){invisible(compiler::enableJIT(initial_jit))}
run<-function(){
 on.exit(on_exit_restore(),add=TRUE)
 # Exercise lazy arguments and exceptional exits at every supported caller setting.
 for(level in 0:3)for(fails in c(FALSE,TRUE)){
  test<-current;environment(test)<-new.env(parent=environment(current))
  environment(test)$draw<-function(main_plot,risk_plot,newpage=TRUE){
   stopifnot(compiler::enableJIT(0)==0L)
   force(risk_plot);force(main_plot)
  }
  invisible(compiler::enableJIT(level));forced<-0L
  value<-tryCatch(test({forced<-forced+1L;if(fails)stop('expected failure');17L},NULL),error=conditionMessage)
  observed<-compiler::enableJIT(level)
  stopifnot(identical(observed,as.integer(level)),forced==1L,
   identical(value,if(fails)'expected failure'else 17L))
 }
 invisible(compiler::enableJIT(3))
 data<-read.csv('scripts/fixtures/survival_validation.csv')
 km<-prepare_km_analysis_result(data,'time','status','sex',rate_times='100, 200, 400')
 set.seed(410);d<-data.frame(time=rexp(120L,.05),status=rep(0:2,40),group=factor(rep(c('A','B'),60)))
 competing<-prepare_competing_risk_result(d,'time','status',group='group',rate_times='5, 10, 20')
 cases<-list()
 for(type in c('survival','event','cumhaz','log_survival'))for(version in c('color','bw')){
  cases[[paste(type,version,sep='-')]]<-local({t<-type;v<-version;function(f)f(survival_km_ggplot(km,t,v),survival_km_risk_table_plot(km,v))})
 }
 cases$no_ci_censor<-function(f){k<-km;k$show_ci<-k$show_censor<-FALSE;f(survival_km_ggplot(k),survival_km_risk_table_plot(k))}
 cases$no_risk<-function(f)f(survival_km_ggplot(km),NULL)
 cases$no_caption<-function(f)f(survival_km_ggplot(km)+ggplot2::labs(caption=NULL),survival_km_risk_table_plot(km))
 cases$competing<-function(f)f(survival_competing_ggplot(competing),survival_competing_risk_table_plot(competing))
 for(name in names(cases)){
  values<-list()
  for(mode in c('baseline','current')){
   set.seed(941);conditions<-character()
   path<-file.path(root,paste0(name,'-',mode,'.png'))
   png(path,width=1000,height=800,res=120)
   stdout<-capture.output(withCallingHandlers(cases[[name]](get(mode)),
    warning=function(w){conditions<<-c(conditions,paste('warning',conditionMessage(w)));invokeRestart('muffleWarning')},
    message=function(m){conditions<<-c(conditions,paste('message',conditionMessage(m)));invokeRestart('muffleMessage')}))
   dev.off()
   values[[mode]]<-list(stdout=stdout,conditions=conditions,rng=.Random.seed,bytes=readBin(path,'raw',file.info(path)$size))
   stopifnot(compiler::enableJIT(3)==3L)
  }
  stopifnot(identical(values$baseline,values$current,num.eq=FALSE))
 }
 cat('PASS:',length(cases),'PNG/conditions/stdout/RNG pairs; eight caller-level lazy-argument/success/error restoration checks.\n')
}
run()
