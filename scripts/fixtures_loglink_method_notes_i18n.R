out<-'tmp/loglink-method-notes-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
cases<-expand.grid(design=c('poisson_irr','negative_binomial_irr','gamma_mean_ratio'),scale=c('ratio','log_ratio'),ratio=c(.5,2),stringsAsFactors=FALSE)
results<-lapply(seq_len(nrow(cases)),function(i)effect_size_rates_calculate(list(effect_size_rates_design=cases$design[i],effect_size_rates_input_scale=cases$scale[i],effect_size_rates_ratio=as.character(cases$ratio[i]),effect_size_rates_log_ratio=as.character(log(cases$ratio[i])))))
designs<-paste(cases$design,cases$scale,cases$ratio)
formula_keys<-paste0('sample_size.result.',ifelse(cases$design=='gamma_mean_ratio','gamma_ratio','count_ratio'))
note_keys<-paste0('sample_size.result.',ifelse(cases$design=='gamma_mean_ratio','note_loglink_mean_ratio','note_loglink_irr'))
for(i in seq_along(results)){
 x<-results[[i]];stopifnot(is.null(x$error),isTRUE(all.equal(x$ratio,cases$ratio[i])),isTRUE(all.equal(x$log_ratio,log(cases$ratio[i]))),isTRUE(all.equal(x$primary_effect_size,cases$ratio[i])))
 if(cases$design[i]=='gamma_mean_ratio')stopifnot(isTRUE(all.equal(x$mean_ratio,cases$ratio[i])),isTRUE(all.equal(x$log_mean_ratio,log(cases$ratio[i]))))else stopifnot(isTRUE(all.equal(x$incidence_rate_ratio,cases$ratio[i])),isTRUE(all.equal(x$log_incidence_rate_ratio,log(cases$ratio[i]))))
}
before_notes<-serialize(results,NULL)
clean<-function(x)gsub('[[:space:]\u00a0]+','',x,perl=TRUE)
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(i in seq_along(results)){
 expected<-statedu_t(note_keys[i],lang,fallback='')
 stopifnot(nzchar(expected),grepl('exp(beta)',expected,fixed=TRUE),grepl('log',expected,fixed=TRUE),identical(sample_size_result_text(results[[i]]$method_note,lang),expected))
 if(lang=='en')stopifnot(identical(expected,results[[i]]$method_note))
 actual<-xml2::xml_text(xml2::read_html(as.character(sample_size_results_ui(results[[i]],lang))))
 for(part in trimws(strsplit(result_sci_note_text(estimation=expected),';',fixed=TRUE)[[1]]))stopifnot(grepl(clean(sub('[.。]$','',part)),clean(actual),fixed=TRUE))
 unknown<-paste0(results[[i]]$method_note,' custom');stopifnot(identical(sample_size_result_text(unknown,lang),unknown))
}
stopifnot(identical(before_notes,serialize(results,NULL)))
# Existing neutral-effect rejection must remain intact for both input scales.
for(design in unique(cases$design))for(scale in unique(cases$scale)){
 x<-effect_size_rates_calculate(list(effect_size_rates_design=design,effect_size_rates_input_scale=scale,effect_size_rates_ratio='1',effect_size_rates_log_ratio='0'))
 stopifnot(identical(x$error,if(scale=='ratio')'Ratio must be different from 1.'else'log ratio must be finite and different from 0.'))
}
cat('PASS 12 log-link effect cases x 8 languages; ratio/log references, negative/positive coefficients, unchanged neutral-effect rejection\n')
