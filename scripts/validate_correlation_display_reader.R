.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
statedu_apply_preferences(statedu_initial_preferences())
root<-'output/correlation-display-reader-20260915';dir.create(root,recursive=TRUE,showWarnings=FALSE)
old<-new.env(parent=.GlobalEnv);new<-new.env(parent=.GlobalEnv)
sys.source('scripts/fixtures/correlation_display_reader_reference.R',old)
sys.source('R/analysis_correlation.R',new)
for(env in list(old,new))for(name in ls(env))if(is.function(env[[name]]))env[[name]]<-compiler::cmpfun(env[[name]])
capture<-function(env,args,fn='prepare_correlation_results'){
 ds<-list();stdout<-capture.output(value<-tryCatch(withCallingHandlers(do.call(env[[fn]],args),
 warning=function(e){ds[[length(ds)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleWarning')},
 message=function(e){ds[[length(ds)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleMessage')}),error=function(e)list(error=conditionMessage(e),class=class(e))))
 list(value=value,diagnostics=ds,stdout=stdout,rng=.Random.seed)
}
as.character.measurement_probe<-function(x,...){warning('measurement conversion');message('measurement message');runif(1);unclass(x)}
set.seed(20260915);data<-data.frame(a=rnorm(30),b=rnorm(30),c=rep(1:3,10),d=rep(0:1,15))
checks<-0L
for(kind in c('plain','duplicate','missing','unknown','factor','probe','mixed','empty','null','missing_column'))for(method in c('pearson','spearman','kendall','auto'))for(normality in c(FALSE,TRUE)){
 info<-data.frame(name=names(data),measurement='continuous',var_label=names(data))
 if(kind=='duplicate')info<-rbind(info,transform(info,measurement='category'))
 if(kind=='missing'){info$name[1]<-NA_character_;info$measurement[2]<-NA_character_}
 if(kind=='unknown')info$measurement<-c('Continuous','BAD','ordinal','binary')
 if(kind=='factor')info$measurement<-factor(info$measurement)
 if(kind=='probe')info$measurement<-structure(info$measurement,class='measurement_probe')
 if(kind=='mixed')info$measurement<-c('continuous','continuous','ordered','binary')
 if(kind=='empty')info<-info[FALSE,,drop=FALSE]
 if(kind=='null')info<-NULL
 if(kind=='missing_column')info$measurement<-NULL
 args<-list(data=data,variables=names(data),variable_info=info,options=list(continuous_method=method,normality=normality))
 seed<-.Random.seed;a<-capture(old,args);.Random.seed<-seed;b<-capture(new,args)
 if(!identical(a,b,num.eq=FALSE)){saveRDS(list(args=args,a=a,b=b),file.path(root,'mismatch.rds'));stop('Full analysis mismatch')}
 checks<-checks+1L
}
cat('Full analysis/diagnostics/stdout/RNG comparisons:',checks,'\n')
for(fn in c('correlation_numeric_data','correlation_normality_summary')){
 for(kind in c('plain','probe','missing')){
  info<-data.frame(name=names(data),measurement='continuous')
  if(kind=='probe')info$measurement<-structure(info$measurement,class='measurement_probe')
  if(kind=='missing')info$measurement[1]<-NA_character_
  args<-list(data=data,variables=names(data),variable_info=info)
  seed<-.Random.seed;a<-capture(old,args,fn);.Random.seed<-seed;b<-capture(new,args,fn)
  stopifnot(identical(a,b,num.eq=FALSE))
 }
}
cat('Standalone preparation comparisons: 6\n')
label_checks<-0L
as.character.label_probe<-function(x,...){warning('label conversion');message('label message');runif(1);unclass(x)}
for(kind in c('plain','duplicate','blank','missing','factor','probe','overrides','empty_override','missing_column','null','omitted','override_factor'))for(normality in c(FALSE,TRUE)){
 selected<-data;selected$constant<-0
 info<-data.frame(name=names(selected),measurement='continuous',var_label=paste0('info ',names(selected)))
 category<-data.frame(name=names(selected),var_label=paste0('category ',names(selected)))
 labels<-setNames(paste0('override ',names(selected)),names(selected))
 if(kind=='duplicate')category<-rbind(category,transform(category,var_label='duplicate'))
 if(kind=='blank')category$var_label<-c('', '  ','한글 <&"','', '')
 if(kind=='missing'){category$name[1]<-NA_character_;category$var_label[2]<-NA_character_}
 if(kind=='factor')category$var_label<-factor(category$var_label)
 if(kind=='probe')category$var_label<-structure(category$var_label,class='label_probe')
 if(kind=='overrides')category<-category[FALSE,,drop=FALSE]
 if(kind=='empty_override'){category<-NULL;labels[]<-''}
 if(kind=='missing_column')category$var_label<-NULL
 if(kind=='null')category<-NULL
 if(kind=='omitted')selected$a<-NA_real_
 if(kind=='override_factor')labels<-factor(labels)
 args<-list(data=selected,variables=names(selected),variable_info=info,labels=labels,category_table=category,options=list(continuous_method='pearson',normality=normality))
 seed<-.Random.seed;a<-capture(old,args);.Random.seed<-seed;b<-capture(new,args)
 if(!identical(a,b,num.eq=FALSE)){saveRDS(list(args=args,a=a,b=b),file.path(root,'label-mismatch.rds'));stop('Label result mismatch')}
 label_checks<-label_checks+1L
}
cat('Label precedence / omission / diagnostic comparisons:',label_checks,'\n')
extra_checks<-0L
for(latent in c(FALSE,TRUE))for(missing in c(FALSE,TRUE)){
 selected<-data[,c('a','c','d')];if(missing)selected$a[c(2,7)]<-NA_real_
 info<-data.frame(name=names(selected),measurement=c('continuous','ordered','binary'))
 args<-list(data=selected,variables=names(selected),variable_info=info,options=list(continuous_method='auto',normality=TRUE,latent_correlations=latent))
 seed<-.Random.seed;a<-capture(old,args);.Random.seed<-seed;b<-capture(new,args)
 stopifnot(identical(a,b,num.eq=FALSE),is.null(a$value$error));extra_checks<-extra_checks+1L
}
cat('Missing data / latent correlation comparisons:',extra_checks,'\n')
for(language in c('en','ko')){
 options(statedu.app_language=language)
 args<-list(data=data,variables=names(data),variable_info=data.frame(name=names(data),measurement='continuous'),options=list(continuous_method='pearson',normality=TRUE))
 args$category_table<-data.frame(name=names(data),var_label=paste0('표시 <& ',names(data)))
 seed<-.Random.seed;a<-capture(old,args);.Random.seed<-seed;b<-capture(new,args)
 stopifnot(identical(a,b,num.eq=FALSE))
 render<-function(result)as.character(htmltools::renderTags(correlation_results_ui(result))$html)
 stopifnot(identical(render(a$value),render(b$value),num.eq=FALSE))
}
cat('Current result HTML comparisons: 2\n')
# A normally shaped metadata table must still repeat a diagnostic-emitting lookup.
for(env in list(old,new)){
 base_lookup<-env$correlation_measurement_lookup
 env$correlation_measurement_lookup<-local({original<-base_lookup;function(...){warning('lookup diagnostic');original(...)}})
}
args<-list(data=data,variables=names(data),variable_info=data.frame(name=names(data),measurement='continuous'),options=list(normality=TRUE))
seed<-.Random.seed;a<-capture(old,args);.Random.seed<-seed;b<-capture(new,args)
stopifnot(identical(a,b,num.eq=FALSE),length(a$diagnostics)>1L)
cat('Diagnostic lookup repetition comparison: 1\n')
for(env in list(old,new)){
 env$frequency_variable_display_name<-frequency_variable_display_name
 environment(env$frequency_variable_display_name)<-env
 env$category_var_label_lookup_static<-local({original<-category_var_label_lookup_static;function(table){warning('category label lookup');original(table)}})
}
args$category_table<-data.frame(name=names(data),var_label=names(data))
seed<-.Random.seed;a<-capture(old,args);.Random.seed<-seed;b<-capture(new,args)
stopifnot(identical(a,b,num.eq=FALSE))
cat('Diagnostic category-label lookup repetition comparison: 1\n')
