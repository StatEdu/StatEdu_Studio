.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
source('scripts/fixtures/sem_gradient_shared_inverse_guarded.R')
wanted<-c('node','edge','stable_data','stable_snapshot','stable_fixture')
for(x in parse('scripts/benchmark_sem_product_bootstrap_5000.R'))
 if(is.call(x)&&identical(x[[1]],as.name('<-'))&&as.character(x[[2]])[1] %in% wanted)eval(x)
fixture<-stable_fixture('all_pairs_dmc',9L,20260833L)
original<-suppressWarnings(run_structural_canvas_analysis(fixture$snapshot,fixture$data,'sem',
 estimator='ML',missing='fiml',std_lv=FALSE,ordered=character(),nominal=character(),residual_variance_fixes=numeric()))
stopifnot(original$converged,original$admissible)
ns<-asNamespace('lavaan');reference<-get('lav_model_grad',ns)
args<-list(lavmodel=original$fit@Model,glist=original$fit@Model@GLIST,
 lavsamplestats=original$fit@SampleStats,lavdata=original$fit@Data,lavcache=original$fit@Cache)
capture<-function(fun,args){
 set.seed(99);diagnostics<-list()
 stdout<-capture.output(value<-tryCatch(withCallingHandlers(do.call(fun,args),
 warning=function(w){diagnostics[[length(diagnostics)+1L]]<<-list(class(w),conditionMessage(w));invokeRestart('muffleWarning')},
 message=function(m){diagnostics[[length(diagnostics)+1L]]<<-list(class(m),conditionMessage(m));invokeRestart('muffleMessage')}),
 error=function(e)list(error=conditionMessage(e),class=class(e))))
 list(value=value,diagnostics=diagnostics,stdout=stdout,rng=.Random.seed)
}
checks<-0L
for(kind in c('supported','full','ceq','missing','no_group_weight')) {
 a<-args
 if(kind=='full')a$type<-'full'
 if(kind=='ceq')a$ceq_simple<-TRUE
 if(kind=='missing')a$lavdata@Mp[[1]]$pat[1,1]<-FALSE
 if(kind=='no_group_weight')a$group_weight<-FALSE
 shared<-make_guarded_shared_inverse_gradient(TRUE);stopifnot(shared$available)
 stopifnot(identical(capture(reference,a),capture(shared$gradient,a),num.eq=FALSE))
 if(kind %in% c('supported','no_group_weight'))stopifnot(shared$state$calls==3L,shared$state$hits==2L)
 else stopifnot(shared$state$calls==0L,shared$state$hits==0L)
 checks<-checks+1L
}
with_binding<-function(name,fun,action){
 saved<-get(name,ns);locked<-bindingIsLocked(name,ns)
 on.exit({
  if(bindingIsLocked(name,ns))unlockBinding(name,ns)
  assign(name,saved,ns);if(locked)lockBinding(name,ns)
  stopifnot(identical(get(name,ns),saved),identical(bindingIsLocked(name,ns),locked))
 },add=TRUE)
 if(locked)unlockBinding(name,ns);assign(name,fun,ns);if(locked)lockBinding(name,ns)
 action()
}
names_to_check<-c('lav_model_grad','lav_lisrel_ibinv','lav_model_sigma','lav_lisrel_sigma',
 'lav_model_mu','lav_lisrel_mu','lav_lisrel_df_dmlist')
for(name in names_to_check)for(change in c('body','formals','environment')) {
 fun<-get(name,ns)
 if(change=='body')body(fun)<-bquote({.(body(fun))})
 if(change=='formals')formals(fun)<-c(formals(fun),alist(.compatibility_probe=NULL))
 if(change=='environment')environment(fun)<-new.env(parent=ns)
 with_binding(name,fun,function(){
  shared<-make_guarded_shared_inverse_gradient()
  stopifnot(!shared$available,identical(shared$gradient,get('lav_model_grad',ns)))
 })
 checks<-checks+1L
}
for(kind in c('diagnostics','error')) {
 fun<-get('lav_lisrel_ibinv',ns);original_body<-body(fun)
 body(fun)<-if(kind=='diagnostics')bquote({warning('inverse probe');message('inverse message');runif(1);.(original_body)})else quote(stop('inverse error probe'))
 with_binding('lav_lisrel_ibinv',fun,function(){
  shared<-make_guarded_shared_inverse_gradient();stopifnot(!shared$available)
  expected<-capture(get('lav_model_grad',ns),args)
  stopifnot(identical(expected,capture(shared$gradient,args),num.eq=FALSE))
  if(kind=='diagnostics')stopifnot(length(expected$diagnostics)==6L)
  else stopifnot(identical(expected$value$error,'inverse error probe'))
 })
 checks<-checks+1L
}
cat('PASS:',checks,'scope/compatibility/diagnostic/RNG/restoration cases\n')
