out<-'tmp/cluster-effect-errors-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
base<-list(effect_size_cluster_effect='-.5',effect_size_cluster_p1='.5',effect_size_cluster_p2='.65',effect_size_cluster_size='20',effect_size_cluster_periods='3',effect_size_cluster_icc='.05')
calc<-function(design,extra=list())suppressWarnings(effect_size_cluster_calculate(modifyList(modifyList(base,list(effect_size_cluster_design=design)),extra)))
types<-c('parallel_binary','stepped_wedge','parallel_continuous')
errors<-list();keys<-character()
for(d in types){
 for(v in c('0','-1','NaN','Inf')){errors<-c(errors,list(calc(d,list(effect_size_cluster_size=v))));keys<-c(keys,'sample_size.result.error_cluster_size_positive')}
 for(v in c('0','1','-.1','NaN','Inf')){errors<-c(errors,list(calc(d,list(effect_size_cluster_icc=v))));keys<-c(keys,'sample_size.result.error_cluster_icc_range')}
}
for(v in c('1','2','NaN','Inf')){errors<-c(errors,list(calc('stepped_wedge',list(effect_size_cluster_periods=v))));keys<-c(keys,'sample_size.result.error_cluster_effect_periods')}
before_errors<-serialize(errors,NULL)
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(i in seq_along(errors)){
 stopifnot(identical(errors[[i]]$error,statedu_t(keys[i],'en')))
 expected<-statedu_t(keys[i],lang,fallback='')
 actual<-xml2::xml_text(xml2::xml_find_first(xml2::read_html(as.character(sample_size_results_ui(errors[[i]],lang))),'//div[@class="analysis-warning"]'))
 stopifnot(nzchar(expected),identical(actual,expected));if(lang!='en')stopifnot(actual!=errors[[i]]$error)
 unknown<-paste0(errors[[i]]$error,' custom');stopifnot(identical(sample_size_result_text(unknown,lang),unknown))
}
stopifnot(length(errors)==31L,identical(before_errors,serialize(errors,NULL)))
cases<-expand.grid(design=types,size=c(1,20),stringsAsFactors=FALSE)
results<-lapply(1:6,function(i)calc(cases$design[i],list(effect_size_cluster_size=as.character(cases$size[i]))))
for(i in 1:6){
 r<-results[[i]];de<-(1+(cases$size[i]-1)*.05)*if(cases$design[i]=='stepped_wedge')1.5 else 1
 effect<-if(cases$design[i]=='parallel_binary')2*asin(sqrt(.5))-2*asin(sqrt(.65))else -.5
 stopifnot(is.null(r$error),isTRUE(all.equal(r$design_effect,de)),isTRUE(all.equal(r$planning_effect_size,effect/sqrt(de))))
}
designs<-paste(cases$design,cases$size,sep='-');formula_keys<-rep(paste0('sample_size.result.',c('note_cluster_binary','note_cluster_stepped','cluster_continuous')),2)
cat('PASS thirty-one actual cluster effect errors x eight languages; six independent design-effect references including size=1 and periods=3\n')
