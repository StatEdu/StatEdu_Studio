Sys.setenv(LC_ALL="English_United States.utf8",LANG="English_United States.utf8",STATEDU_MODULE_CACHE="false")
invisible(Sys.setlocale("LC_CTYPE","English_United States.utf8"))
source("R/app_bootstrap.R",encoding="UTF-8");load_app_packages(check=FALSE);source_app_modules()
statedu_apply_preferences(statedu_default_preferences());options(statedu.app_language="en")
active <- FALSE
for(e in parse("scripts/validate_sem_canvas.R")) {
  if(is.call(e)&&identical(e[[1]],as.name("set.seed")))active<-TRUE
  if(active)eval(e)
  if(is.call(e)&&identical(e[[1]],as.name("<-"))&&identical(e[[2]],as.name("snapshot")))break
}
original <- data
records <- list()
for(group in 0:1) {
  selected <- original[seq_len(nrow(original))%%2L==group,,drop=FALSE]
  scoped <- selected;attr(scoped,"statedu_scope_excluded")<-"scope_group"
  for(mode in c("cfa","cbsem","plssem")) {
    set.seed(916);a<-run_structural_canvas_analysis(snapshot,scoped,mode)
    set.seed(916);b<-run_structural_canvas_analysis(snapshot,selected,mode)
    equal<-isTRUE(all.equal(a,b,check.attributes=FALSE))
    if(!equal)print(head(all.equal(a,b,check.attributes=FALSE),5))
    records[[length(records)+1L]]<-data.frame(menu=mode,group=group,pass=equal)
    message(if(equal)"PASS: "else"FAIL: ",mode," scoped versus manual subset group=",group)
  }
  # Include the excluded group column among requested predictors, as in a saved menu.
  selected$scope_group<-group;scoped<-selected;attr(scoped,"statedu_scope_excluded")<-"scope_group"
  for(method in c("Ridge","LASSO","Elastic Net")) {
    a<-prepare_penalized_menu(scoped,"y1",c("x1","x2","scope_group"),method,seed=916L,resamples=2L,validation_repeats=1L)
    b<-prepare_penalized_menu(selected,"y1",c("x1","x2"),method,seed=916L,resamples=2L,validation_repeats=1L)
    equal<-isTRUE(all.equal(a,b,check.attributes=FALSE))
    if(!equal)print(head(all.equal(a,b,check.attributes=FALSE),5))
    records[[length(records)+1L]]<-data.frame(menu=method,group=group,pass=equal)
    message(if(equal)"PASS: "else"FAIL: ",method," scoped versus manual subset group=",group)
  }
}
report<-do.call(rbind,records)
write.csv(report,"tmp/analysis-scope/structural-penalized-audit.csv",row.names=FALSE)
stopifnot(all(report$pass))
