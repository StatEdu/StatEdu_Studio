.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
statedu_apply_preferences(statedu_initial_preferences())
root<-'output/correlation-reader-combined-20260915';dir.create(root,recursive=TRUE,showWarnings=FALSE)
old<-new.env(parent=.GlobalEnv);new<-new.env(parent=.GlobalEnv)
sys.source('scripts/fixtures/correlation_measurement_lookup_reference.R',old)
sys.source('R/analysis_correlation.R',new)
for(env in list(old,new))for(name in ls(env))if(is.function(env[[name]]))env[[name]]<-compiler::cmpfun(env[[name]])
capture<-function(env,args){
 ds<-list();stdout<-capture.output(value<-withCallingHandlers(do.call(env$prepare_correlation_results,args),
 warning=function(e){ds[[length(ds)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleWarning')},
 message=function(e){ds[[length(ds)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleMessage')}))
 list(value=value,diagnostics=ds,stdout=stdout,rng=.Random.seed)
}
set.seed(20260915)
data<-data.frame(a=rnorm(60),b=rnorm(60),c=rep(1:3,20),d=rep(0:1,30),e=factor(rep(c('A','B','C'),20)),constant=1)
info<-data.frame(name=c(names(data),paste0('unused',seq_len(9994L))),
 measurement=c('continuous','continuous','ordered','binary','category','continuous',rep('continuous',9994L)))
info$var_label<-paste0('info ',info$name)
category<-data.frame(name=info$name,var_label=paste0('범주 <& ',info$name))
checks<-0L
for(kind in c('complete','missing','scope'))for(method in c('pearson','spearman','kendall','auto'))for(normality in c(FALSE,TRUE)){
 selected<-data
 if(kind=='missing'){selected$a[c(1,4,9)]<-NA_real_;selected$c[c(2,6)]<-NA_integer_}
 if(kind=='scope')attr(selected,'statedu_scope_excluded')<-'b'
 args<-list(data=selected,variables=names(data),variable_info=info,category_table=category,
 labels=c(a='override',b=''),options=list(continuous_method=method,normality=normality))
 seed<-.Random.seed;a<-capture(old,args);.Random.seed<-seed;b<-capture(new,args)
 if(!identical(a,b,num.eq=FALSE)){saveRDS(list(args=args,a=a,b=b),file.path(root,'mismatch.rds'));stop('Combined mismatch')}
 stopifnot(nrow(a$value$pairwise_table)>0L,nrow(a$value$omitted_table)==1L)
 if(kind=='scope')stopifnot(!'b'%in%a$value$variables)
 checks<-checks+1L
}
cat('PASS:',checks,'combined large-metadata mixed-method/missing/scope comparisons; successful analyses and constant omission checked\n')
for(language in c('en','ko')){
 options(statedu.app_language=language)
 render<-function(result)as.character(htmltools::renderTags(correlation_results_ui(result))$html)
 stopifnot(identical(render(a$value),render(b$value),num.eq=FALSE))
}
cat('Current result HTML comparisons: 2\n')
