Sys.setlocale('LC_CTYPE','Korean_Korea.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
capture<-function(expr)tryCatch(expr,error=conditionMessage)
errors<-c(
 group_n_error=capture(sample_size_effect_size('independent_means',mean1=105,mean2=100,sd1=10,sd2=10,n1=1,n2=50)),
 groups_error=capture(sample_size_effect_size_anova('partial_eta_from_f',f_value=4.5,groups=1,total_n=90)),
 total_n_error=capture(sample_size_effect_size_anova('partial_eta_from_f',f_value=4.5,groups=3,total_n=3))
)
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(key in names(errors)) {
 stopifnot(errors[[key]]==statedu_t(paste0('sample_size.result.',key),'en'))
 ui<-sample_size_results_ui(list(error=errors[[key]]),lang)
 actual<-xml2::xml_text(xml2::xml_find_first(xml2::read_html(as.character(ui)),'//div[@class="analysis-warning"]'))
 stopifnot(actual==statedu_t(paste0('sample_size.result.',key),lang))
}
cat('PASS 3 actual validation errors x 8 languages\n')
source('scripts/validate_sample_size.R',encoding='UTF-8')
