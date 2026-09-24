Sys.setlocale("LC_CTYPE","English_United States.utf8")
Sys.setenv(STATEDU_MODULE_CACHE="false")
source("R/app_bootstrap.R",encoding="UTF-8");load_app_packages(check=FALSE);source_app_modules()
set.seed(927);d<-data.frame(x=rnorm(90),g=factor(rep(c('A','B','C'),30)));d$y<-d$x+3*(d$g=='B')-2*(d$g=='C')+rnorm(90)
info<-data.frame(name=names(d),measurement=c('continuous','category','continuous'),var_label=c('사용자.A','집단','Tested'))
fits<-lapply(penalized_menu_methods(),function(m)prepare_penalized_menu(d,'y',c('x','g'),m,info,resamples=2,post_selection=TRUE,inference_splits=20,validation_repeats=2,seed=12))
for(i in seq_along(fits)) {
 f<-fits[[i]];stopifnot(all(f$validation_variability$Repetitions==2))
 if(i>1L)stopifnot(nrow(f$publication_factor_inference)>0,sum(f$inference_diagnostics$Splits)==20,sum(f$factor_diagnostics$Splits)==20)
}
sources<-jsonlite::read_json('scripts/fixtures/penalized_i18n_advanced.json',simplifyVector=TRUE)
out<-'tmp/penalized-advanced-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
baseline<-NULL
for(language in c('en','ko','ja','zh','es','fr','de','vi')) {
 options(statedu.app_language=language)
 html<-as.character(htmltools::renderTags(tagList(lapply(fits,penalized_result_block)))$html)
 doc<-xml2::read_html(html,encoding='UTF-8')
 main<-xml2::xml_find_all(doc,"//table[@data-result-table-role='main']")
 content<-lapply(main,function(t)list(cells=xml2::xml_text(xml2::xml_find_all(t,'.//th|.//td')),notes=xml2::xml_text(xml2::xml_find_all(t,"ancestor::div[@data-result-table-sheet='true'][1]/*[not(self::table)]"))))
 stopifnot(length(main)>0,all(xml2::xml_attr(main,'data-result-table-language')=='en'))
 if(language=='en')baseline<-content else stopifnot(identical(content,baseline))
 headers<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']//th"))
 if(language!='en')stopifnot(!any(headers %in% sources[1:4]))
 probe<-result_appendix_localize_table(data.frame(Variable=sources,Status=sources),language)
 stopifnot(identical(probe[[1]],sources))
 if(language!='en')stopifnot(all(probe[[2]]!=sources))
 tables<-xml2::xml_find_all(doc,"//table[contains(@class,'penalized-inference-diagnostics') or contains(@class,'penalized-factor-diagnostics') or contains(@class,'penalized-validation-variability')]")
 stopifnot(length(tables)==7L)
 for(t in tables)stopifnot('Tested' %in% xml2::xml_text(xml2::xml_find_all(t,'.//td')))
 diagnostics<-xml2::xml_find_all(doc,"//table[contains(@class,'penalized-inference-diagnostics') or contains(@class,'penalized-factor-diagnostics')]")
 for(t in diagnostics) {
  statuses<-xml2::xml_text(xml2::xml_find_all(t,'.//tbody/tr/td[3]'))
  stopifnot(statedu_t('analysis.ui.tested',language) %in% statuses)
  stopifnot(sum(as.numeric(xml2::xml_text(xml2::xml_find_all(t,'.//tbody/tr/td[4]'))))==20)
 }
 if(language=='ja') {
  # Capture the actual changed supplementary tables, plus rare status variants.
  sections<-lapply(tables,function(t)HTML(as.character(t)))
  sections<-c(sections,list(analysis_result_table_section('診断状態',probe)))
  saveRDS(list(list(id='penalized-advanced',title='Advanced diagnostics',html=as.character(tagList(sections)))),file.path(out,'entries.rds'))
 }
 cat('PASS:',language,'actual advanced options; all main tables English; diagnostic headers and user outcome preserved\n')
}
