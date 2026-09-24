Sys.setlocale('LC_CTYPE','Korean_Korea.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
cases<-list(
 hedges_note=list(method='effectsize',input=list(sample_size_effectsize_design='hedges_g')),
 empirical_power_note=list(method='regression',input=list(sample_size_regression_design='mediation',sample_size_regression_mediation_method='fritz_mackinnon'))
)
for(language in c('en','ko','ja','zh','es','fr','de','vi'))for(key in names(cases)) {
 config<-cases[[key]]
 expected<-statedu_t(paste0('sample_size.setup.',key),language,fallback='')
 stopifnot(nzchar(expected))
 ui<-shiny::isolate(sample_size_inputs_ui(config$method,config$input,language))
 notes<-xml2::xml_text(xml2::xml_find_all(xml2::read_html(as.character(ui)),'//div[@class="sample-size-method-note"]'))
 stopifnot(expected %in% notes)
 if(key=='empirical_power_note')stopifnot(grepl('.80',expected,fixed=TRUE),grepl('0.80',expected,fixed=TRUE))
}
cat('PASS 2 setup descriptions x 8 languages; original power constants preserved\n')
