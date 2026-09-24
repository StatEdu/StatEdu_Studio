.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
root<-'output/category-input-chunks-20260915'
dir.create(root,recursive=TRUE,showWarnings=FALSE)
old<-new.env(parent=.GlobalEnv);new<-new.env(parent=.GlobalEnv)
sys.source('scripts/fixtures/category_input_collect_reference.R',old)
sys.source('R/data_category_labels.R',new)
new$collect_category_label_inputs_from_table_original<-new$collect_category_label_inputs_from_table
new$collect_category_label_inputs_from_table<-function(...)new$collect_category_label_event_inputs(...)
# Keep the wrapper's inner collector pointed at the unchanged implementation.
environment(new$collect_category_label_event_inputs)<-list2env(list(collect_category_label_inputs_from_table=new$collect_category_label_inputs_from_table_original),parent=new)
for(env in list(old,new))env$collect_category_label_inputs_from_table<-compiler::cmpfun(env$collect_category_label_inputs_from_table)
capture<-function(env,args){
 ds<-list();stdout<-capture.output(v<-tryCatch(withCallingHandlers(shiny::isolate(do.call(env$collect_category_label_inputs_from_table,args)),warning=function(e){ds[[length(ds)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleWarning')},message=function(e){ds[[length(ds)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleMessage')}),error=function(e)list(error=conditionMessage(e),class=class(e))))
 list(value=v,diagnostics=ds,stdout=stdout,rng=.Random.seed)
}
make_table<-function(n)data.frame(source_order=seq_len(n),name=if(n)paste0('v',seq_len(n))else character())
make_input<-function(tab,pairs,kind='plain'){
 result<-list()
 for(i in seq_len(nrow(tab)))for(field in category_label_edit_columns(pairs)){
  id<-paste0('category_',field,'_input_',tab$source_order[[i]])
  result[id]<-list(switch(kind,empty=NULL,vector=c('first','second'),missing=NA_character_,list=list(NULL),factor=factor('x'),number=1,plain='한글 <&"'))
 }
 result
}
set.seed(20260915);checks<-0L
for(n in c(0L,1L,64L,65L,129L))for(pairs in c(1L,11L))for(kind in c('plain','empty','vector','missing','list','factor','number'))for(surface in c('list','environment','reactive')){
 tab<-make_table(n);values<-make_input(tab,pairs,kind)
 input<-switch(surface,list=values,environment=list2env(values,parent=emptyenv()),reactive=do.call(shiny::reactiveValues,values))
 args<-list(table_data=tab,input=input,max_pairs=pairs)
 seed<-.Random.seed;a<-capture(old,args);.Random.seed<-seed;b<-capture(new,args)
 if(!identical(a,b,num.eq=FALSE)){saveRDS(list(a=a,b=b),file.path(root,'mismatch.rds'));stop('Exact input collection mismatch')}
 checks<-checks+1L
}
cat('Exact collection comparisons:',checks,'\n')
order_checks<-0L
for(kind in c('plain','duplicate','blank','missing','factor','special')){
 tab<-make_table(3L)
 if(kind=='duplicate'){tab$name[2]<-tab$name[1];tab$source_order[2]<-tab$source_order[1]}
 if(kind=='blank'){tab$name[1]<-'';tab$source_order[2]<-''}
 if(kind=='missing'){tab$name[1]<-NA_character_;tab$source_order[2]<-NA_character_}
 if(kind=='factor'){tab$name<-factor(tab$name);tab$source_order<-factor(tab$source_order)}
 if(kind=='special')tab$source_order<-c('한글',' <&"','back\\slash')
 run<-function(env){
  trace<-character();input<-new.env(parent=emptyenv())
  ids<-unique(unlist(lapply(as.character(tab$source_order),function(order)paste0('category_',category_label_edit_columns(),'_input_',order))))
  for(id in ids)local({key<-id;makeActiveBinding(key,function(){trace<<-c(trace,key);key},input)})
  result<-capture(env,list(table_data=tab,input=input))
  list(result=result,trace=trace)
 }
 seed<-.Random.seed;a<-run(old);.Random.seed<-seed;b<-run(new)
 stopifnot(identical(a,b,num.eq=FALSE));order_checks<-order_checks+1L
}
cat('Input access order comparisons:',order_checks,'\n')
trace_run<-function(env){
 tab<-make_table(129L);tab$name[65L]<-tab$name[1L]
 actual_input<-do.call(shiny::reactiveValues,make_input(tab,11L))
 trace<-character()
 proxy<-structure(list(impl=list(get=function(key){trace<<-c(trace,key);actual_input[[key]]}),ns=identity),class='reactivevalues')
 result<-capture(env,list(table_data=tab,input=proxy))
 list(result=result,trace=trace)
}
seed<-.Random.seed;a<-trace_run(old);.Random.seed<-seed;b<-trace_run(new)
stopifnot(identical(a,b,num.eq=FALSE),length(a$trace)==129L*25L)
cat('Chunked reactive input access order: 3225 reads identical\n')
fallback_checks<-0L
for(kind in c('factor_name','missing_name','named_order','pair_vector','pair_zero','pair_fraction')){
 tab<-make_table(65L);pairs<-11L
 if(kind=='factor_name')tab$name<-factor(tab$name)
 if(kind=='missing_name')tab$name[1L]<-NA_character_
 if(kind=='named_order')names(tab$source_order)<-tab$name
 if(kind=='pair_vector')pairs<-c(1L,2L)
 if(kind=='pair_zero')pairs<-0L
 if(kind=='pair_fraction')pairs<-1.5
 input<-shiny::reactiveValues()
 args<-list(table_data=tab,input=input,max_pairs=pairs)
 seed<-.Random.seed;a<-capture(old,args);.Random.seed<-seed;b<-capture(new,args)
 stopifnot(identical(a,b,num.eq=FALSE));fallback_checks<-fallback_checks+1L
}
cat('Large-table fallback comparisons:',fallback_checks,'\n')
