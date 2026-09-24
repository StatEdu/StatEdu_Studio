.libPaths(R.home('library'))
Sys.setenv(R_LIBS_USER=normalizePath(R.home('library'),winslash='/'),STATEDU_NO_PACKAGE_INSTALL='true')
id<-as.integer(commandArgs(TRUE)[1]);stopifnot(!is.na(id),id %in% 1:3)
root<-paste0('output/sem-product-worker-profile-20260915/run-',id)
dir.create(root,recursive=TRUE,showWarnings=FALSE)
stopifnot(!length(list.files(root,pattern='[.]Rprof$')))
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
  prepared,reps=200L,seed=fixture$bootstrap_seed,ci_method='bias_corrected',workers=4L,chunk_size=100L,return_draws=TRUE),
  warning=function(w){conditions[[length(conditions)+1L]]<<-list(class(w),conditionMessage(w));invokeRestart('muffleWarning')},
  message=function(m){conditions[[length(conditions)+1L]]<<-list(class(m),conditionMessage(m));invokeRestart('muffleMessage')}))
 list(value=value,rng=.Random.seed,conditions=conditions,stdout=stdout)
}
original_body<-body(structural_canvas_effect_bootstrap_fixed_index_worker)
profile_root<-normalizePath(root,winslash='/',mustWork=TRUE)
profile_body<-bquote({
 profile_file<-file.path(.(profile_root),paste0('worker-',Sys.getpid(),'-',block$mode,'-',block$positions[[1L]],'.Rprof'))
 Rprof(profile_file,interval=.01)
 on.exit(Rprof(NULL),add=TRUE)
 .(original_body)
})
results<-list()
for(version in if(id%%2L)c('baseline','profiled')else c('profiled','baseline')) {
 body(structural_canvas_effect_bootstrap_fixed_index_worker)<-if(version=='baseline')original_body else profile_body
 results[[version]]<-capture()
}
body(structural_canvas_effect_bootstrap_fixed_index_worker)<-original_body
baseline<-results$baseline;profiled<-results$profiled
baseline_timings<-attr(baseline$value,'timings');profiled_timings<-attr(profiled$value,'timings')
for(t in list(baseline_timings,profiled_timings))stopifnot(t$fixed_index$active,t$fixed_index$product_aware,
 t$fixed_index$fallbacks==0L,t$workers==4L,t$chunk_size==100L)
attr(baseline$value,'timings')<-NULL;attr(profiled$value,'timings')<-NULL
draws<-attr(profiled$value,'bootstrap_draws')
stopifnot(identical(baseline,profiled,num.eq=FALSE),length(draws$sample_indices)==200L,
 length(draws$valid_mask)==200L,nrow(draws$raw)==200L,nrow(draws$standardized)==200L,sum(draws$valid_mask)==200L)
saveRDS(list(baseline=baseline,profiled=profiled,baseline_timings=baseline_timings,profiled_timings=profiled_timings),file.path(root,'comparison.rds'))
files<-list.files(root,pattern='[.]Rprof$',full.names=TRUE)
stopifnot(length(files)>0)
profiles<-lapply(files,summaryRprof)
total<-sum(vapply(profiles,function(x)x$sampling.time,numeric(1)))
for(field in c('by.self','by.total')) {
 tables<-lapply(profiles,function(p){d<-p[[field]];if(!nrow(d))return(NULL);data.frame(name=rownames(d),self=d$self.time,total=d$total.time)})
 combined<-aggregate(cbind(self,total)~name,do.call(rbind,tables),sum)
 combined$sample_percent<-100*combined[[if(field=='by.self')'self' else 'total']]/total
 combined<-combined[order(combined$sample_percent,decreasing=TRUE),]
 write.csv(combined,file.path(root,paste0(field,'.csv')),row.names=FALSE)
 if(field=='by.total')print(head(combined,14))
}
cat('PASS',id,'profiles',length(files),'summed worker sampling seconds',total,'valid',sum(attr(profiled$value,'bootstrap_draws')$valid_mask),'\n')
