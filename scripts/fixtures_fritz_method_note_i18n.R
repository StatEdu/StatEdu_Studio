out <- 'tmp/fritz-method-note-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
base <- list(sample_size_regression_design='mediation',sample_size_regression_alpha='.05',sample_size_regression_power='.8',sample_size_regression_n='100',sample_size_regression_ratio='1',sample_size_regression_alternative='two.sided',sample_size_regression_dropout='10',sample_size_regression_mediation_method='fritz_mackinnon',sample_size_regression_target='sample_size',sample_size_regression_covariates='0')
tests <- c('baron_kenny_complete','baron_kenny_small','baron_kenny_medium','baron_kenny_large','joint_significance','sobel','prodclin','percentile_bootstrap','bias_corrected_bootstrap')
levels <- c('small','halfway','medium','large')
cases <- expand.grid(a=levels,b=levels,test=tests,stringsAsFactors=FALSE)
all_results <- lapply(seq_len(nrow(cases)),function(i) sample_size_calculate('regression',modifyList(base,list(sample_size_regression_a_effect=cases$a[i],sample_size_regression_b_effect=cases$b[i],sample_size_regression_fritz_test=cases$test[i]))))
stopifnot(all(vapply(all_results,function(x)is.null(x$error),logical(1))))
ss <- which(cases$a=='small' & cases$b=='small')
stopifnot(identical(as.numeric(vapply(all_results[ss],function(x)x$total,numeric(1))),c(20886,562,531,530,530,667,539,558,462)))
for(i in seq_along(all_results)){
 x<-all_results[[i]];effects<-c(small=.14,halfway=.26,medium=.39,large=.59)
 stopifnot(x$a_path==effects[cases$a[i]],x$b_path==effects[cases$b[i]],x$indirect_effect==x$a_path*x$b_path,x$adjusted_total==ceiling(x$total/.9))
}
before_fritz <- serialize(all_results,NULL)
clean <- function(x)gsub('[[:space:]\u00a0]+','',x,perl=TRUE)
for(lang in c('en','ko','ja','zh','es','fr','de','vi')){
 for(i in seq_along(all_results)){
  x<-all_results[[i]];test_index<-match(cases$test[i],tests)
  test_label<-if(test_index<=4)sprintf(statedu_t('sample_size.result.note_fritz_causal',lang),c('0','.14','.39','.59')[test_index])else sample_size_label(lang,x$fritz_mackinnon_test)
  expected<-sprintf(statedu_t('sample_size.result.note_fritz_table',lang),statedu_t(paste0('sample_size.result.note_fritz_',cases$a[i]),lang),statedu_t(paste0('sample_size.result.note_fritz_',cases$b[i]),lang),test_label)
  stopifnot(identical(sample_size_result_text(x$method_note,lang),expected))
  if(lang=='en')stopifnot(identical(expected,x$method_note))
 }
 raw<-all_results[[1]]$method_note
 for(unknown in c(paste0(raw,' extra'),sub('small','custom',raw,fixed=TRUE),sub("c' = 0","c' = .2",raw,fixed=TRUE),sub('.80','.90',raw,fixed=TRUE)))stopifnot(identical(sample_size_result_text(unknown,lang),unknown))
}
stopifnot(identical(before_fritz,serialize(all_results,NULL)))
# Export one result per test, covering all four effect categories in both slots.
selected <- vapply(seq_along(tests),function(j)which(cases$test==tests[j] & cases$a==levels[(j-1)%%4+1] & cases$b==levels[j%%4+1])[1],integer(1))
results<-all_results[selected];designs<-tests;formula_keys<-rep('sample_size.result.planning_mediation_fritz',length(results))
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(x in results){
 expected<-sample_size_result_text(x$method_note,lang)
 actual<-xml2::xml_text(xml2::read_html(as.character(sample_size_results_ui(x,lang))))
 for(part in trimws(strsplit(result_sci_note_text(estimation=expected),';',fixed=TRUE)[[1]]))stopifnot(grepl(clean(sub('[.。]$','',part)),clean(actual),fixed=TRUE))
}
cat('PASS 144 empirical mediation cases x 8 languages, nine SS table references, path effects/dropout, strict matching; nine representative exports\n')
