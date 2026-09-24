.libPaths(R.home('library'))
Sys.setenv(R_LIBS_USER=normalizePath(R.home('library'),winslash='/'),STATEDU_NO_PACKAGE_INSTALL='true')
id<-as.integer(commandArgs(TRUE)[1]);stopifnot(!is.na(id),id %in% 1:3)
root<-paste0('output/sem-product-hessian-audit-20260915/run-',id)
dir.create(root,recursive=TRUE,showWarnings=FALSE)
stopifnot(!length(list.files(root,pattern='[.]rds$')))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
wanted<-c('node','edge','stable_data','stable_snapshot','stable_fixture');found<-0L
for(x in parse('scripts/benchmark_sem_product_bootstrap_5000.R')) {
 if(is.call(x)&&identical(x[[1]],as.name('<-'))&&as.character(x[[2]])[1] %in% wanted) {
  eval(x);found<-found+1L
 }
}
stopifnot(found==length(wanted))
fixture<-stable_fixture('all_pairs_dmc',9L,20260833L)
original<-suppressWarnings(run_structural_canvas_analysis(fixture$snapshot,fixture$data,'sem',
 estimator='ML',missing='fiml',std_lv=FALSE,ordered=character(0),nominal=character(0),residual_variance_fixes=numeric(0)))
stopifnot(original$converged,original$admissible,
 original$moderation_definitions[[1L]]$product_indicator_count==9L)
prepared<-structural_canvas_prepare_effect_bootstrap(fixture$snapshot,fixture$data,'sem','ML','fiml',FALSE,
 character(0),character(0),numeric(0),original_result=original)
options(statedu.isolated_lavaan_bootstrap_worker=TRUE,
 statedu.internal.disable_sem_bootstrap_fixed_index=FALSE,
 statedu.internal.sem_bootstrap_information_mode='expected')
capture<-function(){
 set.seed(99);conditions<-list()
 stdout<-capture.output(value<-withCallingHandlers(structural_canvas_effect_bootstrap_prepared(
  prepared,reps=24L,seed=fixture$bootstrap_seed,ci_method='bias_corrected',workers=4L,chunk_size=100L,return_draws=TRUE),
  warning=function(w){conditions[[length(conditions)+1L]]<<-list(class(w),conditionMessage(w));invokeRestart('muffleWarning')},
  message=function(m){conditions[[length(conditions)+1L]]<<-list(class(m),conditionMessage(m));invokeRestart('muffleMessage')}))
 list(value=value,rng=.Random.seed,conditions=conditions,stdout=stdout)
}
original_body<-body(structural_canvas_effect_bootstrap_fixed_index_worker)
audit_root<-normalizePath(root,winslash='/',mustWork=TRUE)
audited_body<-substitute({
 ns<-asNamespace('lavaan');saved_hessian<-get('lav_model_hessian',ns)
 was_locked<-bindingIsLocked('lav_model_hessian',ns)
 on.exit({
  if(bindingIsLocked('lav_model_hessian',ns))unlockBinding('lav_model_hessian',ns)
  assign('lav_model_hessian',saved_hessian,ns)
  if(was_locked)lockBinding('lav_model_hessian',ns)
 },add=TRUE)
 audited_hessian<-saved_hessian
 nodes<-as.list(body(audited_hessian));found_grad<-0L
 for(i in seq_along(nodes)) {
  node<-nodes[[i]]
  if(is.call(node)&&identical(node[[1]],as.name('<-'))&&identical(node[[2]],as.name('grad_at'))) {
   old_grad_body<-node[[3]][[3]]
   node[[3]][[3]]<-bquote({
    .audit_inputs[[length(.audit_inputs)+1L]]<<-serialize(x_p,NULL,version=2)
    .(old_grad_body)
   })
   nodes[[i]]<-node;found_grad<-found_grad+1L
  }
 }
 stopifnot(found_grad==1L)
 changed_body<-as.call(nodes)
 prefix<-file.path(AUDIT_ROOT,paste0('hessian-',Sys.getpid(),'-',block$mode,'-',block$positions[[1L]],'-'))
 audit_env<-new.env(parent=ns)
 audit_env$.audit_state<-new.env(parent=emptyenv());audit_env$.audit_state$index<-0L
 environment(audited_hessian)<-audit_env
 body(audited_hessian)<-bquote({
  .audit_inputs<-list()
  .audit_state$index<-.audit_state$index+1L
  .audit_call<-.audit_state$index
  on.exit({
   saveRDS(list(method=method,h=h,parameter_count=length(x),gradient_inputs=.audit_inputs,
    repeated_inputs=sum(duplicated(.audit_inputs)),gradient_calls=length(.audit_inputs)),
    paste0(.(prefix),.audit_call,'.rds'))
  },add=TRUE)
  .(changed_body)
 })
 if(was_locked)unlockBinding('lav_model_hessian',ns)
 assign('lav_model_hessian',audited_hessian,ns)
 if(was_locked)lockBinding('lav_model_hessian',ns)
 ORIGINAL_BODY
},list(AUDIT_ROOT=audit_root,ORIGINAL_BODY=original_body))
results<-list()
for(version in if(id%%2L)c('baseline','audited')else c('audited','baseline')) {
 body(structural_canvas_effect_bootstrap_fixed_index_worker)<-if(version=='baseline')original_body else audited_body
 results[[version]]<-capture()
}
body(structural_canvas_effect_bootstrap_fixed_index_worker)<-original_body
timings<-lapply(results,function(x)attr(x$value,'timings'))
for(t in timings)stopifnot(t$fixed_index$active,t$fixed_index$product_aware,t$fixed_index$fallbacks==0L)
results<-lapply(results,function(x){attr(x$value,'timings')<-NULL;x})
stopifnot(identical(results$baseline,results$audited,num.eq=FALSE))
draws<-attr(results$audited$value,'bootstrap_draws')
stopifnot(length(draws$sample_indices)==24L,length(draws$valid_mask)==24L,
 nrow(draws$raw)==24L,nrow(draws$standardized)==24L,sum(draws$valid_mask)==24L)
saveRDS(list(results=results,timings=timings),file.path(root,'comparison.rds'))
files<-list.files(root,pattern='^hessian-.*[.]rds$',full.names=TRUE)
stopifnot(length(files)>0L)
rows<-lapply(files,function(path){x<-readRDS(path);data.frame(file=basename(path),method=x$method,h=x$h,
 parameter_count=x$parameter_count,gradient_calls=x$gradient_calls,repeated_inputs=x$repeated_inputs)})
summary<-do.call(rbind,rows);write.csv(summary,file.path(root,'calls.csv'),row.names=FALSE)
print(aggregate(cbind(gradient_calls,repeated_inputs)~method+parameter_count,summary,sum))
cat('PASS:',id,'Hessian calls',nrow(summary),'full result/draws/parent diagnostics/stdout/RNG exact\n')
