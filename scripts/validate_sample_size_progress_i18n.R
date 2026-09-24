Sys.setlocale('LC_CTYPE','Korean_Korea.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
fixtures<-c('Calculating...','Starting','Starting... 1%','Calculation stopped.',
 'Running mediation Monte Carlo 12/100 (n = 80)', 'Running mediation bootstrap 21/200',
 'Running LMM simulations 15/300 (n = 120)','Running GLIMMPSE-style simulations 14/150',
 'Running stepped-wedge simulations 5/50 (clusters = 24)',
 'Approximate SEM parameter-power simulation... 37%')
keys<-c('calculating','starting','starting_percent','stopped','mediation_mc','mediation_bootstrap','lmm','glimmpse','stepped_wedge','sem')
digits<-function(x)regmatches(x,gregexpr('[0-9]+',x))[[1]]
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 for(i in seq_along(fixtures)) {
  stopifnot(nzchar(statedu_t(paste0('sample_size.progress.',keys[[i]]),lang,fallback='')))
  translated<-sample_size_progress_text(fixtures[[i]],lang)
  stopifnot(identical(digits(translated),digits(fixtures[[i]])))
  if(lang!='en')stopifnot(translated!=fixtures[[i]])
  ui<-xml2::read_html(as.character(sample_size_results_ui(list(progress=TRUE,id='p',cancel_id='stop',text=fixtures[[i]]),lang)))
  shown<-xml2::xml_text(xml2::xml_find_first(ui,'//div[@class="sample-size-progress-text"]'))
  stopifnot(identical(shown,translated),length(xml2::xml_find_all(ui,'//button[@id="stop"]'))==1L)
 }
 external<-'External <error>& %s\nfile abc'
 stopifnot(identical(sample_size_progress_text(external,lang),external))
 warning<-xml2::read_html(as.character(sample_size_results_ui(list(error=external),lang)))
 stopifnot(xml2::xml_text(xml2::xml_find_first(warning,'//div[@class="analysis-warning"]'))==external)
 stopped<-xml2::read_html(as.character(sample_size_results_ui(list(error='Calculation stopped.'),lang)))
 stopifnot(xml2::xml_text(xml2::xml_find_first(stopped,'//div[@class="analysis-warning"]'))==sample_size_progress_text('Calculation stopped.',lang))
}
cat('PASS 10 progress messages x 8 languages; counters, percentages, sample/cluster suffixes, stop control and external errors preserved\n')
source('scripts/validate_sample_size.R',encoding='UTF-8')
