.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
# Verify stable plot identity and explicit covariate keys without creating PNGs.
calls<-list();context_value<-1L
cache<-correlation_export_image_cache(max_bytes=48*1024^2,max_entries=32L,
 context=function()context_value,render=function(plot_function,result,width,height,res){
  calls[[length(calls)+1L]]<<-list(fn=plot_function,result=result,width=width,height=height,res=res)
  'data:image/png;base64,test'
 })
item<-list(options=list(plot_adjusted_means=TRUE,plot_raw_overlay=TRUE,
 plot_regression_lines=TRUE,plot_linearity_diagnostics=TRUE),method='ANCOVA',
 covariates=c('x','z'),fit_data=data.frame(x=1:10,z=11:20))
a<-as.character(htmltools::tagList(ancova_plot_sections(item,cache$render)))
b<-as.character(htmltools::tagList(ancova_plot_sections(item,cache$render)))
stopifnot(identical(a,b),length(calls)==5L,
 identical(calls[[4]]$fn,draw_ancova_linearity_diagnostic_plot),
 identical(calls[[5]]$fn,draw_ancova_linearity_diagnostic_plot),
 identical(calls[[4]]$result$linearity_covariate,'x'),identical(calls[[5]]$result$linearity_covariate,'z'))
context_value<-2L;invisible(ancova_plot_sections(item,cache$render));stopifnot(length(calls)==10L)
item$fit_data$x[1]<-99;invisible(ancova_plot_sections(item,cache$render));stopifnot(length(calls)==15L)
cache$clear();stopifnot(length(environment(cache$render)$entries)==0L)
cat('PASS: five stable ANCOVA plot keys, explicit covariates, result/context invalidation and clear\n')
