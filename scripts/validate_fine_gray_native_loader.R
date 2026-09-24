.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
root<-Sys.getenv('STATEDU_FINE_GRAY_TEST_OUTPUT','output/fine-gray-loader-20260914');dir.create(root,recursive=TRUE,showWarnings=FALSE)
candidate<-Sys.getenv('STATEDU_FINE_GRAY_TEST_DLL','output/fine-gray-native-build-20260914/candidate.dll')
stopifnot(file.exists(candidate))
previous<-Sys.getenv('STATEDU_FINE_GRAY_NATIVE_DLL',unset=NA_character_)
main<-function() {
 on.exit(if(is.na(previous))Sys.unsetenv('STATEDU_FINE_GRAY_NATIVE_DLL')else Sys.setenv(STATEDU_FINE_GRAY_NATIVE_DLL=previous),add=TRUE)
 original<-cmprsk::crr
 Sys.unsetenv('STATEDU_FINE_GRAY_NATIVE_DLL');stopifnot(identical(survival_fine_gray_engine(),original))
 for(path in c(file.path(root,'missing.dll'),file.path(root,'invalid.dll'))) {
  if(basename(path)=='invalid.dll')writeLines('invalid candidate',path)
  Sys.setenv(STATEDU_FINE_GRAY_NATIVE_DLL=path);stopifnot(identical(survival_fine_gray_engine(),original))
 }
 Sys.setenv(STATEDU_FINE_GRAY_NATIVE_DLL=candidate)
 for(gate in c('runtime','platform','architecture')) {
  probe<-survival_fine_gray_engine;e<-new.env(parent=environment(probe))
  if(gate=='runtime')e$getRversion<-function()numeric_version('4.5.2')
  if(gate=='platform'){e$.Platform<-.Platform;e$.Platform$OS.type<-'unix'}
  if(gate=='architecture'){e$R.version<-R.version;e$R.version$arch<-'aarch64'}
  environment(probe)<-e;stopifnot(identical(probe(),original))
 }
 # Force loader/symbol failures in a private function environment.
 for(stage in c('load','symbol')) {
  probe<-survival_fine_gray_engine;e<-new.env(parent=environment(probe))
  e$getLoadedDLLs<-function()list()
  if(stage=='load')e$dyn.load<-function(...)stop('test load failure')
  else {e$dyn.load<-function(...)NULL;e$getNativeSymbolInfo<-function(...)stop('test symbol failure')}
  environment(probe)<-e;stopifnot(identical(probe(),original))
 }
 engine<-survival_fine_gray_engine();stopifnot(!identical(engine,original),identical(cmprsk::crr,original))
 dll_count<-length(getLoadedDLLs());invisible(survival_fine_gray_engine());stopifnot(length(getLoadedDLLs())==dll_count)
 baseline_path<-Sys.getenv('STATEDU_FINE_GRAY_BASELINE','')
 baseline<-if(nzchar(baseline_path)) {e<-new.env(parent=.GlobalEnv);sys.source(baseline_path,e);f<-e$prepare_competing_risk_result;environment(f)<-.GlobalEnv;f} else prepare_competing_risk_result
 run<-function(f,d,kind,regression) {
  set.seed(88);conditions<-character()
  stdout<-capture.output(value<-withCallingHandlers(
   f(d,'time','status',group='group',covariates=paste0('V',1:10),regression=regression,censoring_group=if(kind=='groups')'group' else ''),
   warning=function(w){conditions<<-c(conditions,conditionMessage(w));invokeRestart('muffleWarning')},
   message=function(m){conditions<<-c(conditions,conditionMessage(m));invokeRestart('muffleMessage')}))
  stopifnot(length(value$fine_gray$fit$coef)>=10L)
  if(regression=='both') {
   stopifnot(ncol(value$cause_specific$ph$y)>=10L)
   attr(value$cause_specific$fit$terms,'.Environment')<-.GlobalEnv
   attr(value$cause_specific$fit$formula,'.Environment')<-.GlobalEnv
   if(!is.null(value$cause_specific$fit$model))attr(attr(value$cause_specific$fit$model,'terms'),'.Environment')<-.GlobalEnv
  }
  list(value=value,conditions=conditions,stdout=stdout,rng=.Random.seed)
 }
 for(kind in c('plain','ties','factor','missing','groups'))for(regression in c('both','fine_gray')) {
  set.seed(44);d<-as.data.frame(matrix(rnorm(3000),300,10));d$time<-rexp(300);d$status<-sample(0:2,300,TRUE);d$group<-rep(letters[1:3],100)
  if(kind=='ties')d$time<-round(d$time,1)
  if(kind=='factor')d$V1<-factor(rep(letters[1:3],100))
  if(kind=='missing')d$V1[seq(1,300,20)]<-NA_real_
  Sys.unsetenv('STATEDU_FINE_GRAY_NATIVE_DLL');a<-run(baseline,d,kind,regression);b<-run(prepare_competing_risk_result,d,kind,regression)
  Sys.setenv(STATEDU_FINE_GRAY_NATIVE_DLL=candidate);c<-run(prepare_competing_risk_result,d,kind,regression)
  if(!identical(a,b,num.eq=FALSE)||!identical(a,c,num.eq=FALSE)){print(all.equal(a,c));stop('Integration mismatch')}
  if(kind=='plain'&&regression=='both') {saveRDS(a,file.path(root,'baseline-result.rds'));saveRDS(c,file.path(root,'native-result.rds'))}
  cat('PASS',kind,regression,'\n')
 }
 stopifnot(identical(cmprsk::crr,original))
 cat('PASS: default/missing/hash/load/symbol fallback, enabled engine, repeated load, namespace unchanged, 10 full-result comparisons\n')
}
main()
