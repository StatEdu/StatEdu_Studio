Sys.setlocale("LC_CTYPE","English_United States.utf8")
Sys.setenv(STATEDU_MODULE_CACHE="false")
source("R/app_bootstrap.R",encoding="UTF-8");load_app_packages(check=FALSE);source_app_modules()
set.seed(925);d<-data.frame(x=rnorm(60),z=rnorm(60));d$y<-2*d$x-d$z+rnorm(60)
info<-data.frame(name=names(d),measurement="continuous",var_label=c("사용자.A","Model-based","사용자 결과"))
fits<-lapply(penalized_menu_methods(),function(method)prepare_penalized_menu(d,"y",c("x","z"),method,info,resamples=2,seed=12,validation_repeats=1))
sources<-jsonlite::read_json("scripts/fixtures/penalized_i18n_headers.json",simplifyVector=TRUE)
appendix_sources<-jsonlite::read_json("scripts/fixtures/penalized_i18n_appendices.json",simplifyVector=TRUE)
out<-"tmp/penalized-i18n";dir.create(out,recursive=TRUE,showWarnings=FALSE)
baseline<-NULL
for(language in c("en","ko","ja","zh","es","fr","de","vi")) {
 options(statedu.app_language=language)
 html<-as.character(htmltools::renderTags(tagList(lapply(fits,penalized_result_block)))$html)
 doc<-xml2::read_html(html,encoding="UTF-8")
 main<-xml2::xml_find_all(doc,"//table[@data-result-table-role='main']")
 content<-lapply(main,function(t)list(cells=xml2::xml_text(xml2::xml_find_all(t,".//th|.//td")),notes=xml2::xml_text(xml2::xml_find_all(t,"ancestor::div[@data-result-table-sheet='true'][1]/*[not(self::table)]"))))
 stopifnot(length(main)>=3,all(xml2::xml_attr(main,"data-result-table-language")=="en"))
 if(language=="en")baseline<-content else stopifnot(identical(content,baseline))
 headers<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']//th"))
 if(language!="en")stopifnot(!any(headers %in% sources))
 stopifnot(statedu_t("analysis.penalized.selection_frequency_percent",language) %in% headers)
 translations<-vapply(appendix_sources,result_appendix_ui_text,character(1),language=language)
 if(language!="en") {
  stopifnot(all(translations!=appendix_sources))
  visible<-xml2::xml_text(doc)
  for(i in seq_len(10L)) {
   expected<-if(i%%2L==0L) xml2::xml_text(xml2::read_html(as.character(result_note_tag(translations[[i]])),encoding="UTF-8")) else translations[[i]]
   stopifnot(grepl(trimws(expected),visible,fixed=TRUE),!grepl(appendix_sources[[i]],visible,fixed=TRUE))
  }
 }
 if(language=="ja") {
  appendix_out<-"tmp/penalized-appendices-i18n";dir.create(appendix_out,recursive=TRUE,showWarnings=FALSE)
  sections<-lapply(seq(1L,16L,by=2L),function(i) analysis_result_table_section(translations[[i]],data.frame(Item="説明",Value=translations[[i+1L]])))
  saveRDS(list(list(id="penalized-appendices",title="Penalized appendices",html=as.character(tagList(sections)))),file.path(appendix_out,"entries.rds"))
 }
 if(language=="ja")saveRDS(list(list(id="penalized",title="Ridge / LASSO / Elastic Net",html=html)),file.path(out,"entries.rds"))
 cat("PASS:",language,"all three methods; supplementary headers; percentage unit; English main tables\n")
}
