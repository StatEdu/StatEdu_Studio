Sys.setlocale("LC_CTYPE","English_United States.utf8")
Sys.setenv(STATEDU_MODULE_CACHE="false")
source("R/app_bootstrap.R",encoding="UTF-8")
load_app_packages(check=FALSE); source_app_modules()
set.seed(918)
d <- data.frame(x=rnorm(160),gamma=rgamma(160,shape=2),binary=rbinom(160,1,.45),count=rnbinom(160,mu=4,size=.7),exposure=runif(160,1,4))
info <- data.frame(name=names(d),measurement=c("continuous","continuous","binary","continuous","continuous"),var_label=c("사용자.A","사용자 감마","사용자 이분형","사용자 계수","Model-based"))
options(statedu.app_language="en")
fits <- list(gamma=prepare_generalized_analysis_result(d,"gamma","x",family="gamma",link="inverse",show_vif=TRUE,variable_info=info),
 binary=prepare_generalized_analysis_result(d,"binary","x",family="binomial",show_vif=TRUE,se_type="HC3",variable_info=info),
 count=prepare_generalized_analysis_result(d,"count","x",exposure="exposure",family="count",show_vif=TRUE,variable_info=info))
stopifnot(fits$count$family=="negative_binomial",fits$gamma$link=="inverse",fits$binary$se_type_used=="HC3")
sources <- jsonlite::read_json("scripts/fixtures/glm_i18n_option_notes.json",simplifyVector=TRUE)
out <- "tmp/glm-options-multilingual"
dir.create(out,recursive=TRUE,showWarnings=FALSE)
baseline <- NULL
for(language in c("en","ko","ja","zh","es","fr","de","vi")) {
 options(statedu.app_language=language)
 probe <- generalized_appendix_table(data.frame(Variable=sources,Item=sources),language)
 stopifnot(identical(probe[[1]],sources))
 if(language!="en") stopifnot(all(probe[[2]]!=sources))
 html <- as.character(htmltools::renderTags(tagList(lapply(fits,function(f) generalized_results_panel(f,info))))$html)
 doc <- xml2::read_html(html,encoding="UTF-8")
 main <- xml2::xml_find_all(doc,"//table[@data-result-table-role='main']")
 content <- lapply(main,function(t) list(cells=xml2::xml_text(xml2::xml_find_all(t,".//th|.//td")),notes=xml2::xml_text(xml2::xml_find_all(t,"ancestor::div[@data-result-table-sheet='true'][1]/*[not(self::table)]"))))
 stopifnot(length(main)>=3,all(xml2::xml_attr(main,"data-result-table-language")=="en"))
 if(language=="en") baseline<-content else stopifnot(identical(content,baseline))
 cells <- xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']//td"))
 stopifnot(any(grepl("log(Model-based)",cells,fixed=TRUE)))
 if(language!="en") stopifnot(!any(cells %in% sources))
 if(language=="ko") stopifnot(grepl("역수",generalized_appendix_value_text("Strictly positive continuous outcome; inverse link.",language),fixed=TRUE))
 if(language=="ja") {
   # Export the actual count result, plus the rarely reached message variants.
   fixture <- tagList(generalized_results_panel(fits$count,info),analysis_result_table_section("診断メッセージ",probe))
   saveRDS(list(list(id="glm-options",title="GLM options",html=as.character(fixture))),file.path(out,"entries.rds"))
   writeLines(html,file.path(out,"actual-results-ja.html"),useBytes=TRUE)
 }
 cat("PASS:",language,"Gamma inverse, binary HC3, negative binomial offset; English main tables and user labels preserved\n")
}
