.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
root<-'output/km-screen-layout-20260915'
baseline_env<-new.env(parent=.GlobalEnv);sys.source(file.path(root,'before.R'),baseline_env)
old<-baseline_env$survival_draw_plot_with_risk_table
d<-read.csv('scripts/fixtures/survival_validation.csv')
km<-prepare_km_analysis_result(d,'time','status','sex',rate_times='0,100,200,300,400,500')
snapshot<-serialize(km,NULL)
sizes<-list(screen=c(700,650,160),narrow=c(500,650,160),saved=c(1000,720,110))
for(name in names(sizes))for(version in c('old','new')) {
 dims<-sizes[[name]]
 png(file.path(root,paste0(name,'-',version,'.png')),width=dims[1],height=dims[2],res=dims[3])
 set.seed(99);before<-.Random.seed
 f<-if(version=='old')old else survival_draw_plot_with_risk_table
 suppressMessages(f(survival_km_ggplot(km),survival_km_risk_table_plot(km)))
 dev.off()
 stopifnot(identical(before,.Random.seed),identical(snapshot,serialize(km,NULL)))
}
for(groups in c(1L,4L,6L)) {
 d$g<-rep(seq_len(groups),length.out=nrow(d))
 k<-prepare_km_analysis_result(d,'time','status','g',rate_times='0,100,200,300,400,500')
 png(file.path(root,paste0('groups-',groups,'.png')),width=700,height=survival_km_plot_height(k),res=160)
 suppressMessages(survival_draw_plot_with_risk_table(survival_km_ggplot(k),survival_km_risk_table_plot(k)))
 dev.off()
}
write_survival_results_html(km,file.path(root,'current.html'),language='ko')
saveRDS(km,file.path(root,'analysis.rds'))
cat('PASS: three device sizes, 1/2/4/6 groups, unchanged analysis serialization and RNG\n')
