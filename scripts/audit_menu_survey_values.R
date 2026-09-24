# Extract unrounded survey estimates from the display-producing menu functions.
for(e in parse('scripts/compare_menu_versions.R')) {
 if(is.call(e)&&identical(e[[1]],as.name('<-'))&&identical(e[[2]],as.name('ids')))break
 eval(e,.GlobalEnv)
}
for(e in parse(file.path(host_root,'scripts/summarize_menu_versions.R'))) {
 if(is.call(e)&&identical(e[[1]],as.name('<-'))&&identical(e[[2]],as.name('canonical'))){eval(e,.GlobalEnv);break}
}
ignored<-c('.Environment','timing','timings')
menu_survey_calls<-list()
menu_survey_capture<-function(value,name) {
 payload<-if(inherits(value,'htest'))value[c('statistic','parameter','p.value','estimate','conf.int')]else if(inherits(value,'glm'))list(coefficients=coef(value),vcov=vcov(value),deviance=value$deviance,df.residual=value$df.residual)else value
 menu_survey_calls[[length(menu_survey_calls)+1L]]<<-list(function_name=name,value=payload)
 invisible(NULL)
}
run<-function(){
 ns<-asNamespace('survey');traced<-character()
 on.exit(for(name in traced)untrace(name,where=ns),add=TRUE)
 extra_traces<-list()
 on.exit(for(item in extra_traces)untrace(item$name,where=item$where),add=TRUE)
 for(name in c('wilcox.test','ttest_mann_whitney_z','ttest_cliffs_delta')){
  where<-if(name=='wilcox.test')asNamespace('stats')else .GlobalEnv
  trace(name,where=where,exit=substitute(.GlobalEnv$menu_survey_capture(returnValue(),NAME),list(NAME=name)),print=FALSE)
  extra_traces[[length(extra_traces)+1L]]<-list(name=name,where=where)
 }
 for(name in c('svymean','svytotal','svyvar','svyglm','svychisq','svyttest','regTermTest','svyquantile')){
  trace(name,where=ns,exit=substitute(.GlobalEnv$menu_survey_capture(returnValue(),NAME),list(NAME=name)),print=FALSE)
  traced<-c(traced,name)
 }
 results<-list()
 for(i in c(17L,24:29)){
  menu_survey_calls<<-list();actual<-capture(cases[[i]][[2]])
  expected<-readRDS(file.path(out,paste0(version,'-1-',i,'.rds')))$result
  unchanged<-identical(canonical(actual),canonical(expected),num.eq=FALSE)
  results[[as.character(i)]]<-list(calls=menu_survey_calls,unchanged=unchanged)
  cat(i,'calls',length(menu_survey_calls),'unchanged',unchanged,'\n')
 }
 saveRDS(results,file.path(out,paste0('survey-audit-',version,'.rds')))
}
run()
