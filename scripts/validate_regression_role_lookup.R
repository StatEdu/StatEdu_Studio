.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
root<-'output/regression-role-lookup-20260915'
old<-new.env(parent=.GlobalEnv);new<-new.env(parent=.GlobalEnv)
dir.create(root,recursive=TRUE,showWarnings=FALSE)
sys.source('scripts/fixtures/regression_role_lookup_reference.R',old);sys.source('R/data_regression_setup.R',new)
capture<-function(env,args,fn='regression_variable_table_data'){
 ds<-list();stdout<-capture.output(v<-tryCatch(withCallingHandlers(do.call(env[[fn]],args),warning=function(e){ds[[length(ds)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleWarning')},message=function(e){ds[[length(ds)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleMessage')}),error=function(e)list(error=conditionMessage(e),class=class(e))))
 list(value=v,diagnostics=ds,stdout=stdout,rng=.Random.seed)
}
set.seed(20260915);checks<-0L
for(selected in list(character(),NULL,c('a','b','c'),c('b','a','b','','c'),c(NA,'a','b'),c(z='a',y='b'),factor(c('b','a')),1:3))for(role in list(character(),c('b','a'),c(NA,'b'),factor(c('a','b')),1:3))for(overlap in c(FALSE,TRUE)){
 info<-data.frame(name=c('a','b','c','a'),var_label=c('가','<b>&','c','last'),measurement=c('binary','continuous','ordered','category'))
 args<-list(selected=selected,info=info,dependent=role,independent=if(overlap)role else c('b','c'),controls=c('a','c'),label_overrides=c(a='override'))
 seed<-.Random.seed;a<-capture(old,args);.Random.seed<-seed;b<-capture(new,args)
 if(!identical(a,b,num.eq=FALSE)){saveRDS(list(args=args,a=a,b=b),file.path(root,'mismatch.rds'));stop('Exact mismatch')};checks<-checks+1L
}
cat('Exact full table, diagnostics, stdout, RNG:',checks,'\n')
old$roles_for_variables<-function(variables,dependent=character(),independent=character(),controls=character()) vapply(variables,role_for_variable,character(1),dependent=dependent,independent=independent,controls=controls)
new$roles_for_variables<-roles_for_variables
direct<-0L
for(vars in list(character(),NULL,c('a','b',NA),c(z='a',y='b'),factor(c('b','a')),1:3,matrix(c('a','b'),1),list('a','b')))for(role in list(character(),c('a','b'),NA_character_,factor('a'),1:3)) {
 args<-list(variables=vars,dependent=role,independent=c('a','b'),controls=c('a','b',NA))
 seed<-.Random.seed;a<-capture(old,args,'roles_for_variables');.Random.seed<-seed;b<-capture(new,args,'roles_for_variables')
 stopifnot(identical(a,b,num.eq=FALSE));direct<-direct+1L
}
cat('Direct role vector comparisons:',direct,'\n')
args<-list(selected=c('b','a','c','b'),info=data.frame(name=c('a','b','c'),var_label=c('가','<&','다'),measurement=c('binary','continuous','ordered')),dependent='a',independent=c('a','b'),controls=c('b','c'))
render<-function(env){
 widget<-DT::datatable(do.call(env$regression_variable_table_data,args),rownames=FALSE)
 widget$elementId<-'regression-role-comparison'
 as.character(htmltools::renderTags(widget)$html)
}
stopifnot(identical(render(old),render(new),num.eq=FALSE))
cat('Fixed-ID variable list HTML comparison: 1\n')

