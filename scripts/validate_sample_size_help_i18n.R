Sys.setlocale('LC_CTYPE','Korean_Korea.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
references<-list(d=c('0.2','0.5','0.8'),dz=c('0.2','0.5','0.8'),f=c('0.1','0.25','0.4'),w=c('0.1','0.3','0.5'),r=c('0.1','0.3','0.5'),f2=c('0.02','0.15','0.35'),beta=c('0.1','0.3','0.5'),loading=c('0.3','0.5','0.7'))
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 for(key in c('effect_size_tooltip','omnibus_fixed_effect','optional_pairwise','fritz_test'))stopifnot(nzchar(statedu_t(paste0('sample_size.setup.',key),lang,fallback='')))
 for(type in names(references)) {
  tooltip<-sample_size_effect_size_tooltip(lang,type)
  actual<-regmatches(tooltip,gregexpr('[0-9]+[.][0-9]+',tooltip))[[1]]
  stopifnot(identical(actual,references[[type]]),length(strsplit(tooltip,'\n')[[1]])==3L)
  label<-as.character(sample_size_effect_size_label(lang,'Effect size d',type))
  stopifnot(grepl(htmltools::htmlEscape(tooltip,attribute=TRUE),label,fixed=TRUE))
 }
 lmm<-xml2::read_html(as.character(shiny::isolate(effect_size_lmm_inputs_ui(list(effect_size_lmm_design='spss_output'),lang))))
 headings<-xml2::xml_text(xml2::xml_find_all(lmm,'//h4'))
 stopifnot(identical(headings,c(statedu_t('sample_size.setup.omnibus_fixed_effect',lang),statedu_t('sample_size.setup.optional_pairwise',lang))))
 input<-list(sample_size_regression_design='mediation',sample_size_regression_mediation_method='fritz_mackinnon')
 ui<-xml2::read_html(as.character(shiny::isolate(sample_size_inputs_ui('regression',input,lang))))
 stopifnot(xml2::xml_text(xml2::xml_find_first(ui,'//label[@for="sample_size_regression_fritz_test"]'))==statedu_t('sample_size.setup.fritz_test',lang))
}
cat('PASS 8 effect-size tooltip types x 8 languages; original numeric thresholds and 3 setup headings retained\n')
source('scripts/validate_sample_size.R',encoding='UTF-8')
