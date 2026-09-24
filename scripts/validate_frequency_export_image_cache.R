.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
entries<-function(cache)environment(environment(cache$render)$images$render)$entries
calls<-0L
mock<-function(...){calls<<-calls+1L;'image'}
cache<-frequency_export_image_cache(render=mock)
for(i in 1:20)invisible(cache$render(list(x=1),'bar',paste0('v',i)))
for(i in 1:20)invisible(cache$render(list(x=1),'bar',paste0('v',i)))
stopifnot(calls==20L,length(entries(cache))==20L)
for(args in list(list(type='pie'),list(name='different'),list(result=list(x=2)),
 list(width=421),list(height=321),list(res=120))){
 base<-modifyList(list(result=list(x=1),type='bar',name='v1'),args)
 before<-calls;invisible(do.call(cache$render,base));invisible(do.call(cache$render,base))
 stopifnot(calls==before+1L)
}
previous<-getOption('statedu.cache_validation');options(statedu.cache_validation='changed')
before<-calls;invisible(cache$render(list(x=1),'bar','v1'));stopifnot(calls==before+1L)
options(statedu.cache_validation=previous)
original_draw<-draw_frequency_plot;draw_frequency_plot<-function(...)NULL
before<-calls;invisible(cache$render(list(x=1),'bar','v1'));stopifnot(calls==before+1L)
draw_frequency_plot<-original_draw
cache$clear();stopifnot(length(entries(cache))==0L)
small<-frequency_export_image_cache(render=mock,max_entries=2L)
for(i in 1:3)invisible(small$render(list(x=1),'bar',paste0('v',i)))
stopifnot(length(entries(small))==2L)
tiny<-frequency_export_image_cache(render=mock,max_bytes=1)
before<-calls;invisible(tiny$render(list(x=1),'bar','v1'));invisible(tiny$render(list(x=1),'bar','v1'))
stopifnot(calls==before+2L,length(entries(tiny))==0L)
other<-frequency_export_image_cache(render=mock)
stopifnot(length(entries(other))==0L,length(entries(small))==2L)
cat('PASS: 20 repeat hits, result/type/name/function/context/size invalidation, isolation, clearing, entry/byte bounds\n')
