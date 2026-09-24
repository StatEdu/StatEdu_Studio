# Valid-rate snapshots retain the existing export regression coverage.
source('scripts/fixtures_loglink_method_notes_i18n.R',encoding='UTF-8')
out<-'tmp/rate-validation-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
errors<-list();error_keys<-character()
add_error<-function(value,key){errors[[length(errors)+1L]]<<-value;error_keys<<-c(error_keys,paste0('sample_size.result.',key))}
for(design in c('poisson_irr','negative_binomial_irr','gamma_mean_ratio')){
 add_error(effect_size_rates_calculate(list(effect_size_rates_design=design,effect_size_rates_input_scale='ratio',effect_size_rates_ratio='1')),'error_rate_ratio_neutral')
 for(log_value in c('0','Inf','NaN'))add_error(effect_size_rates_calculate(list(effect_size_rates_design=design,effect_size_rates_input_scale='log_ratio',effect_size_rates_log_ratio=log_value)),'error_rate_log_ratio')
}
rate_base<-list(sample_size_rates_target='sample_size',sample_size_rates_alpha='.05',sample_size_rates_power='.8',sample_size_rates_ratio='1',sample_size_rates_alternative='two.sided',sample_size_rates_dropout='0',sample_size_rates_rate1='.3',sample_size_rates_rate2='.2',sample_size_rates_dispersion='.5')
for(design in c('two_rate_ratio','negative_binomial')){
 input<-modifyList(rate_base,list(sample_size_rates_design=design))
 add_error(sample_size_calculate('rates',modifyList(input,list(sample_size_rates_rate2='.3'))),'error_rates_equal')
 for(dispersion in c('-1','Inf','NaN'))add_error(sample_size_calculate('rates',modifyList(input,list(sample_size_rates_dispersion=dispersion))),'error_rate_dispersion')
 # Zero dispersion is allowed and must continue to produce a valid result.
 valid<-sample_size_calculate('rates',modifyList(input,list(sample_size_rates_dispersion='0')))
 stopifnot(is.null(valid$error),is.finite(valid$total),valid$total>0)
}
before_errors<-serialize(errors,NULL)
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(i in seq_along(errors)){
 stopifnot(identical(errors[[i]]$error,statedu_t(error_keys[i],'en')))
 actual<-xml2::xml_text(xml2::xml_find_first(xml2::read_html(as.character(sample_size_results_ui(errors[[i]],lang))),'//div[@class="analysis-warning"]'))
 expected<-statedu_t(error_keys[i],lang,fallback='')
 stopifnot(nzchar(expected),identical(actual,expected))
 if(lang!='en')stopifnot(!identical(expected,errors[[i]]$error))
 unknown<-paste0(errors[[i]]$error,' custom');stopifnot(identical(sample_size_result_text(unknown,lang),unknown))
}
stopifnot(identical(before_errors,serialize(errors,NULL)),length(errors)==20L)
cat('PASS 20 actual rate validation failures x 8 languages, two zero-dispersion controls; errors stay transient warnings\n')
