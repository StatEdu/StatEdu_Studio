Sys.setlocale("LC_CTYPE","English_United States.utf8")
Sys.setenv(STATEDU_MODULE_CACHE="false")
source("R/app_bootstrap.R",encoding="UTF-8")
load_app_packages(check=FALSE); source_app_modules()
set.seed(917)
d <- data.frame(x=rnorm(100),y=rnorm(100))
d$x[seq(3,100,9)] <- NA_real_
d$y[seq(7,100,13)] <- NA_real_
info <- data.frame(name=c("x","y"),measurement="continuous",var_label=c("Model-based","사용자 결과"))
options(statedu.app_language="en")
fits <- lapply(c("observed","impute"),function(policy) prepare_generalized_analysis_result(d,"y","x",family="gaussian",missing_strategy="mi",missing_imputations=2,missing_iterations=2,mi_outcome=policy,variable_info=info))
stopifnot(all(fits[[1]]$mi_fit_diagnostics$N==92L),all(fits[[2]]$mi_fit_diagnostics$N==100L))
out <- "tmp/glm-mi-multilingual"
dir.create(out,recursive=TRUE,showWarnings=FALSE)
baseline <- NULL
for(language in c("en","ko","ja","zh","es","fr","de","vi")) {
 options(statedu.app_language=language)
 sections <- list()
 for(f in fits) {
   raw <- f$mi_fit_diagnostics
   translated <- generalized_appendix_table(raw,language)
   for(i in seq_along(raw)) if(is.numeric(raw[[i]]) || is.logical(raw[[i]]) || names(raw)[[i]]=="Term signature") stopifnot(identical(raw[[i]],translated[[i]]))
   if(language!="en") {
     stopifnot(all(translated[[2]]!=raw$Family),all(translated[[3]]!=raw$Link))
     for(header in c("Residual df","Converged","Poisson dispersion","SE method","Term signature")) stopifnot(names(translated)[match(header,names(raw))]!=header)
   }
   adversarial <- raw
   adversarial[["Term signature"]] <- c("Model-based","Intercept only")
   stopifnot(identical(generalized_appendix_table(adversarial,language)[["Term signature"]],adversarial[["Term signature"]]) ||
     identical(generalized_appendix_table(adversarial,language)[[match("Term signature",names(raw))]],adversarial[["Term signature"]]))
   pooled <- generalized_appendix_table(f$mi_pooling_diagnostics,language)
   stopifnot(identical(pooled[[1]],f$mi_pooling_diagnostics[[1]]))
   for(i in which(vapply(f$mi_pooling_diagnostics,is.numeric,logical(1)))) stopifnot(identical(pooled[[i]],f$mi_pooling_diagnostics[[i]]))
   sections <- c(sections,list(analysis_result_table_section(generalized_appendix_text("Imputation-specific model checks",language),translated),
     analysis_result_table_section(generalized_appendix_text("Multiple-imputation pooling diagnostics",language),pooled)))
 }
 html <- as.character(htmltools::renderTags(tagList(lapply(fits,function(f) generalized_results_panel(f,info))))$html)
 doc <- xml2::read_html(html,encoding="UTF-8")
 main <- xml2::xml_find_all(doc,"//table[@data-result-table-role='main']")
 content <- lapply(main,function(t) list(cells=xml2::xml_text(xml2::xml_find_all(t,".//th|.//td")),notes=xml2::xml_text(xml2::xml_find_all(t,"ancestor::div[@data-result-table-sheet='true'][1]/*[not(self::table)]"))))
 stopifnot(length(main)>0L,all(xml2::xml_attr(main,"data-result-table-language")=="en"))
 if(language=="en") baseline <- content else stopifnot(identical(content,baseline))
 if(language=="ja") {
   saveRDS(list(list(id="glm-mi",title="GLM MI",html=as.character(tagList(sections)))),file.path(out,"entries.rds"))
   writeLines(html,file.path(out,"actual-results-ja.html"),useBytes=TRUE)
 }
 cat("PASS:",language,"actual MI observed/impute policies; diagnostics localized; numeric/logical/user terms preserved; main tables English\n")
}
