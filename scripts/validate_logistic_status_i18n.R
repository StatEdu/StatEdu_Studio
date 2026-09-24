Sys.setlocale("LC_CTYPE","English_United States.utf8")
Sys.setenv(STATEDU_MODULE_CACHE="false")
source("R/app_bootstrap.R",encoding="UTF-8")
load_app_packages(check=FALSE); source_app_modules()
set.seed(922)
d <- data.frame(y=factor(rep(0:1,each=40)),x=rnorm(80))
info <- data.frame(name=c("y","x"),measurement=c("binary","continuous"),var_label=c("사용자 결과","Converged"))
results <- prepare_logistic_analysis_results(d,"y","x",variable_info=info)
stopifnot(isTRUE(results[[1]]$convergence$ok))
states <- c("Converged","Not converged","Unknown model convergence status")
probe <- data.frame(Variable=states,Status=states,check.names=FALSE)
out <- "tmp/logistic-status-i18n"
dir.create(out,recursive=TRUE,showWarnings=FALSE)
baseline <- NULL
for(language in c("en","ko","ja","zh","es","fr","de","vi")) {
 options(statedu.app_language=language)
 translated <- logistic_appendix_table(probe,language)
 expected <- vapply(c("converged","not_converged","unknown"),function(key) statedu_t(paste0("analysis.logistic_status.",key),language),character(1))
 stopifnot(identical(translated[[1]],states),identical(unname(translated[[2]]),unname(expected)))
 html <- as.character(htmltools::renderTags(logistic_results_panel(results,info))$html)
 doc <- xml2::read_html(html,encoding="UTF-8")
 main <- xml2::xml_find_all(doc,"//table[@data-result-table-role='main']")
 content <- lapply(main,function(t) list(cells=xml2::xml_text(xml2::xml_find_all(t,".//th|.//td")),notes=xml2::xml_text(xml2::xml_find_all(t,"ancestor::div[@data-result-table-sheet='true'][1]/*[not(self::table)]"))))
 stopifnot(length(main)>0L,all(xml2::xml_attr(main,"data-result-table-language")=="en"))
 if(language=="en") baseline <- content else stopifnot(identical(content,baseline))
 appendix <- xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']//td"))
 stopifnot(any(appendix==expected[[1]]))
 if(language=="ja") {
  fixture <- tagList(logistic_results_panel(results,info),analysis_result_table_section("収束状態",translated))
  saveRDS(list(list(id="logistic-status",title="Logistic regression",html=as.character(fixture))),file.path(out,"entries.rds"))
 }
 cat("PASS:",language,"convergence states; actual binary fit; user text preserved; main tables English\n")
}
