Sys.setlocale("LC_CTYPE","English_United States.utf8")
Sys.setenv(STATEDU_MODULE_CACHE="false")
source("R/app_bootstrap.R",encoding="UTF-8")
load_app_packages(check=FALSE); source_app_modules()
set.seed(919)
d <- data.frame(x=rnorm(140),pois=rpois(140,3),nb=rnbinom(140,mu=5,size=.6))
d$x[seq(3,140,11)] <- NA_real_
info <- data.frame(name=names(d),measurement="continuous",var_label=c("사용자.A","사용자 포아송","사용자 음이항"))
options(statedu.app_language="en")
fits <- lapply(c("pois","nb"),function(y) prepare_generalized_analysis_result(d,y,"x",family="count",missing_strategy="mi",missing_imputations=2,missing_iterations=2,variable_info=info))
stopifnot(identical(fits[[1]]$family,"count"),identical(fits[[2]]$family,"negative_binomial"))
sources <- c("Poisson family was prespecified and held fixed across all imputed datasets.","Negative-binomial family was prespecified and held fixed across all imputed datasets.")
out <- "tmp/glm-count-mi-multilingual"
dir.create(out,recursive=TRUE,showWarnings=FALSE)
baseline <- NULL
for(language in c("en","ko","ja","zh","es","fr","de","vi")) {
 options(statedu.app_language=language)
 sections <- list()
 for(i in seq_along(fits)) {
  raw <- fits[[i]]$mi_fit_diagnostics
  stopifnot(all(raw$Family==fits[[i]]$family),all(raw$Link=="log"),all(raw$N==140))
  translated <- generalized_appendix_table(raw,language)
  for(j in seq_along(raw)) if(is.numeric(raw[[j]]) || is.logical(raw[[j]]) || names(raw)[[j]]=="Term signature") stopifnot(identical(raw[[j]],translated[[j]]))
  note <- generalized_appendix_text(sources[[i]],language)
  if(language!="en") stopifnot(note!=sources[[i]],all(translated[[2]]!=raw$Family),all(translated[[3]]!=raw$Link))
  sections <- c(sections,list(analysis_result_table_section(generalized_appendix_text("Imputation-specific model checks",language),translated),analysis_result_table_section(generalized_appendix_text("Model overview",language),data.frame(Item=generalized_appendix_text("Family",language),Value=note))))
 }
 html <- as.character(htmltools::renderTags(tagList(lapply(fits,function(f) generalized_results_panel(f,info))))$html)
 doc <- xml2::read_html(html,encoding="UTF-8")
 main <- xml2::xml_find_all(doc,"//table[@data-result-table-role='main']")
 content <- lapply(main,function(t) list(cells=xml2::xml_text(xml2::xml_find_all(t,".//th|.//td")),notes=xml2::xml_text(xml2::xml_find_all(t,"ancestor::div[@data-result-table-sheet='true'][1]/*[not(self::table)]"))))
 stopifnot(length(main)>0L,all(xml2::xml_attr(main,"data-result-table-language")=="en"))
 if(language=="en") baseline <- content else {
  stopifnot(identical(content,baseline))
  appendix <- xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']"))
  for(source in sources) stopifnot(!any(grepl(source,appendix,fixed=TRUE)))
 }
 if(language=="ja") {
  saveRDS(list(list(id="glm-count-mi",title="GLM count MI",html=as.character(tagList(sections)))),file.path(out,"entries.rds"))
  writeLines(html,file.path(out,"actual-results-ja.html"),useBytes=TRUE)
 }
 cat("PASS:",language,"actual Poisson/negative-binomial MI; fixed families; localized diagnostics/notes; English main tables\n")
}
