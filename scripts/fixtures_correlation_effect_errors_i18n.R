out<-'tmp/correlation-effect-errors-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
errors<-list();keys<-character()
for(design in c('pearson_r','fisher_z'))for(value in c('-1','1','NaN','Inf')){
 errors<-c(errors,list(effect_size_correlation_calculate(list(effect_size_correlation_design=design,effect_size_correlation_r=value))));keys<-c(keys,'sample_size.result.error_correlation_single')
}
for(field in c('effect_size_correlation_r1','effect_size_correlation_r2_compare'))for(value in c('-1','1','NaN','Inf')){
 input<-list(effect_size_correlation_design='cohens_q',effect_size_correlation_r1='-.3',effect_size_correlation_r2_compare='.2');input[[field]]<-value
 errors<-c(errors,list(effect_size_correlation_calculate(input)));keys<-c(keys,'sample_size.result.error_correlation_pair')
}
before_errors<-serialize(errors,NULL)
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(i in seq_along(errors)){
 stopifnot(identical(errors[[i]]$error,statedu_t(keys[i],'en')))
 expected<-statedu_t(keys[i],lang,fallback='')
 actual<-xml2::xml_text(xml2::xml_find_first(xml2::read_html(as.character(sample_size_results_ui(errors[[i]],lang))),'//div[@class="analysis-warning"]'))
 stopifnot(nzchar(expected),identical(actual,expected));if(lang!='en')stopifnot(actual!=errors[[i]]$error)
 unknown<-paste0(errors[[i]]$error,' custom');stopifnot(identical(sample_size_result_text(unknown,lang),unknown))
}
stopifnot(length(errors)==16L,identical(before_errors,serialize(errors,NULL)))
cases<-expand.grid(design=c('pearson_r','fisher_z'),r=c(-.3,0,.3),stringsAsFactors=FALSE)
results<-lapply(seq_len(nrow(cases)),function(i)effect_size_correlation_calculate(list(effect_size_correlation_design=cases$design[i],effect_size_correlation_r=as.character(cases$r[i]))))
for(i in seq_along(results))stopifnot(is.null(results[[i]]$error),isTRUE(all.equal(results[[i]]$primary_effect_size,if(cases$design[i]=='pearson_r')cases$r[i]else atanh(cases$r[i]))))
qresults<-lapply(c(FALSE,TRUE),function(reverse)effect_size_correlation_calculate(list(effect_size_correlation_design='cohens_q',effect_size_correlation_r1=if(reverse)'.2'else'-.3',effect_size_correlation_r2_compare=if(reverse)'-.3'else'.2')))
stopifnot(isTRUE(all.equal(qresults[[1]]$cohens_q,atanh(-.3)-atanh(.2))),identical(qresults[[1]]$cohens_q,-qresults[[2]]$cohens_q))
results<-c(results,qresults);designs<-c(paste(cases$design,cases$r),'q-forward','q-reverse')
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(i in which(cases$design=='fisher_z')){
 actual<-xml2::xml_text(xml2::read_html(as.character(sample_size_results_ui(results[[i]],lang))))
 stopifnot(grepl("Fisher's z = atanh(r)",actual,fixed=TRUE))
}
# Formula-only Fisher z stays unchanged; export representative Pearson/q results.
selected<-c(which(cases$design=='pearson_r'),7:8)
results<-results[selected];designs<-designs[selected]
formula_keys<-paste0('sample_size.result.',c(rep('note_pearson',3),rep('note_cohens_q',2)))
cat('PASS sixteen actual correlation-effect errors x eight languages; eight signed/zero/Fisher-z/Cohen-q references\n')
