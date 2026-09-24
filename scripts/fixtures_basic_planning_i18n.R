# Actual planning wrappers in sample-size and achieved-power modes.
out<-'tmp/basic-planning-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
methods<-c(rep('ttest',4),rep('nonparametric',3),rep('proportion',2),'chisquare','correlation')
designs<-c('two_sample','one_sample','paired','two_sample','two_independent','kruskal_wallis','friedman','one_proportion','two_proportion','chisquare','correlation')
formula_keys<-paste0('sample_size.result.',c(rep('planning_t',4),'planning_rank_pair',rep('planning_rank_omnibus',2),rep('planning_proportion',2),'planning_chisquare','planning_correlation'))
inputs<-lapply(seq_along(methods),function(i){
 m<-methods[i];v<-list(target='sample_size',alpha='0.05',power='0.8',n='100',ratio=if(i==4)'2' else '1',alternative='two.sided',dropout='0',design=designs[i],effect='0.3',groups='3',measurements='3',p1='0.65',p2='0.5',df='2',r='0.3')
 setNames(v,paste0('sample_size_',m,'_',names(v)))
})
results<-lapply(seq_along(methods),function(i)sample_size_calculate(methods[i],inputs[[i]]))
power_results<-lapply(seq_along(methods),function(i){v<-inputs[[i]];v[[paste0('sample_size_',methods[i],'_target')]]<-'power';sample_size_calculate(methods[i],v)})
for(i in seq_along(results))for(r in list(results[[i]],power_results[[i]]))stopifnot(is.null(r$error),identical(r$formula_note,statedu_t(formula_keys[i],'en')))
for(r in power_results)stopifnot(is.finite(r$power),r$power>=0,r$power<=1)
ref<-stats::power.t.test(delta=.3,sd=1,sig.level=.05,power=.8,type='two.sample',alternative='two.sided')
stopifnot(results[[1]]$group1==ceiling(ref$n))
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(i in seq_along(power_results)){
 expected<-sample_size_result_text(power_results[[i]]$formula_note,lang)
 stopifnot(identical(expected,statedu_t(formula_keys[i],lang)),nzchar(expected))
 rendered<-xml2::xml_text(xml2::read_html(as.character(sample_size_results_ui(power_results[[i]],lang))))
 parts<-trimws(strsplit(result_sci_note_text(estimation=expected),';',fixed=TRUE)[[1]])
 clean<-function(x)gsub('[[:space:]\u00a0]+','',x,perl=TRUE)
 for(part in parts)stopifnot(grepl(clean(sub('[.。]$','',part)),clean(rendered),fixed=TRUE))
}
cat('PASS 11 sample-size + 11 achieved-power wrapper cases, eight-language power rendering and exact t-test reference\n')
