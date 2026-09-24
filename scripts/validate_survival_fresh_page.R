.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
root<-'output/survival-fresh-page-validation-20260914'
dir.create(root,recursive=TRUE,showWarnings=FALSE)
baseline<-survival_draw_plot_with_risk_table
current<-function(main_plot,risk_plot)survival_draw_plot_with_risk_table(main_plot,risk_plot,newpage=FALSE)
invisible(compiler::enableJIT(3))
run<-function(){
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
 rows<-list()
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
  a<-values$baseline;b<-values$current
  same_png<-identical(a$bytes,b$bytes);a$bytes<-b$bytes<-NULL
  rows[[length(rows)+1L]]<-data.frame(case=name,png_equal=same_png,conditions_stdout_rng_equal=identical(a,b,num.eq=FALSE))
 }
 write.csv(do.call(rbind,rows),file.path(root,'validation.csv'),row.names=FALSE)
 print(do.call(rbind,rows))
 stopifnot(all(vapply(rows,function(x)x$png_equal && x$conditions_stdout_rng_equal,logical(1))))
}
run()


# Default calls must clear an existing page exactly as they clear a fresh page.
km<-prepare_km_analysis_result(read.csv('scripts/fixtures/survival_validation.csv'),'time','status','sex',rate_times='100, 200, 400')
for(busy in c(FALSE,TRUE)){
 path<-file.path(root,paste0('default-page-',busy,'.png'))
 png(path,width=1000,height=800,res=120)
 if(busy){grid::grid.newpage();grid::grid.rect(gp=grid::gpar(fill='red',col='red'))}
 set.seed(941)
 invisible(capture.output(survival_draw_plot_with_risk_table(survival_km_ggplot(km),survival_km_risk_table_plot(km))))
 dev.off()
}
a<-file.path(root,'default-page-FALSE.png');b<-file.path(root,'default-page-TRUE.png')
stopifnot(identical(readBin(a,'raw',file.info(a)$size),readBin(b,'raw',file.info(b)$size)))
cat('PASS: 12 fresh-device PNG/conditions/stdout/RNG pairs; default call clears pre-existing page.\n')
