out<-'tmp/proportion-input-errors-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
fields<-c('event1','nonevent1','event2','nonevent2')
table_calc<-function(values)suppressWarnings(effect_size_proportion_calculate(c(list(effect_size_proportion_design='odds_ratio_table'),setNames(as.list(as.character(values)),paste0('effect_size_proportion_',fields)))))
pair_calc<-function(design,p01,p10)suppressWarnings(effect_size_mcnemar_calculate(list(effect_size_mcnemar_design=design,effect_size_mcnemar_p01=p01,effect_size_mcnemar_p10=p10)))
errors<-list();keys<-character()
for(i in 1:4)for(v in c('-1','bad','NaN','Inf')){x<-c('10','20','15','25');x[i]<-v;errors<-c(errors,list(table_calc(x)));keys<-c(keys,'sample_size.result.error_2x2_counts')}
for(d in c('matched_or_probs','cohen_g'))for(f in c('p01','p10'))for(v in c('0','1','-.1','NaN','Inf')){
 errors<-c(errors,list(pair_calc(d,if(f=='p01')v else '.25',if(f=='p10')v else '.25')));keys<-c(keys,paste0('sample_size.result.error_mcnemar_',f,'_range'))
}
before_errors<-serialize(errors,NULL)
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(i in seq_along(errors)){
 stopifnot(identical(errors[[i]]$error,statedu_t(keys[i],'en')))
 expected<-statedu_t(keys[i],lang,fallback='')
 actual<-xml2::xml_text(xml2::xml_find_first(xml2::read_html(as.character(sample_size_results_ui(errors[[i]],lang))),'//div[@class="analysis-warning"]'))
 stopifnot(nzchar(expected),identical(actual,expected));if(lang!='en')stopifnot(actual!=errors[[i]]$error)
 unknown<-paste0(errors[[i]]$error,' custom');stopifnot(identical(sample_size_result_text(unknown,lang),unknown))
}
stopifnot(length(errors)==36L,identical(before_errors,serialize(errors,NULL)))
tables<-lapply(0:4,function(i){x<-c(10,20,15,25);if(i>0)x[i]<-0;x})
results<-lapply(tables,table_calc)
for(i in 1:5){
 x<-tables[[i]];if(any(x==0))x<-x+.5;r<-results[[i]]
 stopifnot(is.null(r$error),isTRUE(all.equal(r$odds_ratio,x[1]*x[4]/(x[2]*x[3]))))
}
pair<-list(pair_calc('matched_or_probs','.75','.25'),pair_calc('cohen_g','.75','.25'))
stopifnot(is.null(pair[[1]]$error),is.null(pair[[2]]$error),pair[[1]]$odds_ratio==3,pair[[2]]$cohen_g==.25)
results<-c(results,pair);designs<-c('table-positive',paste0('table-zero-',fields),'pair-or','pair-g')
formula_keys<-paste0('sample_size.result.',c(rep('note_proportion_transform',5),'note_matched_probabilities','matched_g'))
cat('PASS thirty-six actual proportion errors x eight languages; seven valid references including zero-cell correction and probability sum=1\n')
