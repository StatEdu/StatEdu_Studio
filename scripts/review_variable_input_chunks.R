.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
root<-'output/variable-input-chunk-review-20260915';dir.create(root,recursive=TRUE,showWarnings=FALSE)
run<-as.integer(commandArgs(trailingOnly=TRUE)[1L]);if(is.na(run))run<-1L
old<-compiler::cmpfun(collect_variable_input_values)
# Research only: unconditional isolation is deliberately not used in product code.
candidate<-compiler::cmpfun(function(info,input,prefixes,allowed_values=NULL,keep_blank=FALSE){
 result<-character()
 for(start in seq.int(1L,nrow(info),64L)){
  rows<-seq.int(start,min(nrow(info),start+63L))
  batch<-shiny::isolate(old(info[rows,,drop=FALSE],input,prefixes,allowed_values,keep_blank))
  for(name in names(batch))result[name]<-batch[[name]]
 }
 result
})
times<-list()
for(n in c(1000L,5000L))for(kind in c('primary','fallback')){
 info<-data.frame(source_order=seq_len(n),name=paste0('v',seq_len(n)))
 prefixes<-c('var_label_input_','category_var_label_input_')
 input<-do.call(shiny::reactiveValues,setNames(rep(list('label'),n),paste0(prefixes[[if(kind=='primary')1L else 2L]],seq_len(n))))
 expected<-shiny::isolate(old(info,input,prefixes))
 stopifnot(identical(expected,shiny::isolate(candidate(info,input,prefixes)),num.eq=FALSE))
 for(version in if(run%%2L)c('old','candidate')else c('candidate','old')){
  gc();start<-Sys.time();actual<-shiny::isolate(get(version)(info,input,prefixes))
  elapsed<-as.numeric(difftime(Sys.time(),start,units='secs'))
  stopifnot(identical(expected,actual,num.eq=FALSE))
  times[[length(times)+1L]]<-data.frame(run,n,kind,version,seconds=elapsed)
  cat(n,kind,version,elapsed,'seconds\n')
 }
}
write.csv(do.call(rbind,times),file.path(root,paste0('times-',run,'.csv')),row.names=FALSE)
dependency_test<-function(fn){
 calls<-0L;values<-list()
 shiny::testServer(function(input,output,session){
  info<-data.frame(source_order=1L,name='v1')
  shiny::observe({
   value<-fn(info,input,c('var_label_input_','category_var_label_input_'))
   calls<<-calls+1L;values[[calls]]<<-value
  })
 },{
  session$setInputs(var_label_input_1='first')
  before<-calls
  session$setInputs(var_label_input_1='second')
  after<-calls
 })
 list(calls=calls,values=values)
}
deps<-list(old=dependency_test(old),candidate=dependency_test(candidate))
stopifnot(deps$old$calls==2L,deps$candidate$calls==1L)
saveRDS(deps,file.path(root,paste0('dependencies-',run,'.rds')))
cat('Confirmed: common reactive caller refreshes twice with original, once with isolated candidate; do not apply globally\n')
