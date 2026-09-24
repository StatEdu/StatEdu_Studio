Sys.setlocale("LC_CTYPE","English_United States.utf8")
Sys.setenv(STATEDU_MODULE_CACHE="false")
source("R/app_bootstrap.R",encoding="UTF-8")
load_app_packages(check=FALSE); source_app_modules()
set.seed(923)
d <- data.frame(x=rnorm(160),ord=ordered(rep(1:3,length.out=160)),cat=factor(rep(1:3,length.out=160)))
info <- data.frame(name=names(d),measurement=c("continuous","ordered","category"))
fits <- lapply(c("ord","cat"),function(y) prepare_logistic_analysis_results(d,y,"x",variable_info=info))
stopifnot(fits[[1]][[1]]$method=="Ordinal logistic regression",fits[[2]][[1]]$method=="Multinomial logistic regression")
out <- "tmp/logistic-odds-i18n"; dir.create(out,recursive=TRUE,showWarnings=FALSE)
baseline <- NULL
performance_sources <- jsonlite::read_json("scripts/fixtures/logistic_i18n_performance.json",simplifyVector=TRUE)
user_predictors <- "사용자.A, Model-based, rate%"
performance_sources <- vapply(performance_sources,function(s) if(grepl("%s",s,fixed=TRUE)) sprintf(s,user_predictors) else s,character(1))
reference_vif_sources <- jsonlite::read_json("scripts/fixtures/logistic_i18n_reference_vif.json",simplifyVector=TRUE)
reference_vif_sources <- vapply(reference_vif_sources,function(s) {
 if(startsWith(s,"Reference for")) sprintf(s,"사용자.A 50%","1.5") else if(grepl("%s",s,fixed=TRUE)) sprintf(s,"12.34") else s
},character(1))
for(language in c("en","ko","ja","zh","es","fr","de","vi")) {
 options(statedu.app_language=language)
 html <- as.character(htmltools::renderTags(tagList(lapply(fits,function(f) logistic_results_panel(f,info))))$html)
 doc <- xml2::read_html(html,encoding="UTF-8")
 main <- xml2::xml_find_all(doc,"//table[@data-result-table-role='main']")
 content <- lapply(main,function(t) list(cells=xml2::xml_text(xml2::xml_find_all(t,".//th|.//td")),notes=xml2::xml_text(xml2::xml_find_all(t,"ancestor::div[@data-result-table-sheet='true'][1]/*[not(self::table)]"))))
 stopifnot(length(main)>0,all(xml2::xml_attr(main,"data-result-table-language")=="en"))
 if(language=="en") baseline <- content else stopifnot(identical(content,baseline))
 appendix <- paste(xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']")),collapse="\n")
 if(language!="en") for(s in c("Proportional odds assumption met","Specified model","The proportional-odds decision used")) stopifnot(!grepl(s,appendix,fixed=TRUE))
 sources <- c("Proportional odds assumption met","Proportional odds assumption not met","Proportional odds assumption not assessable","x²=1.24(.265); Final hierarchical model")
 probe <- logistic_appendix_table(data.frame(Variable=sources,Status=sources),language)
 stopifnot(identical(probe[[1]],sources),grepl("x²=1.24(.265)",probe[[2]][[4]],fixed=TRUE))
 if(language!="en") stopifnot(all(probe[[2]]!=sources))
 performance_probe <- logistic_appendix_table(data.frame(Variable=performance_sources,Item=performance_sources),language)
 stopifnot(identical(performance_probe[[1]],unname(performance_sources)),grepl(user_predictors,tail(performance_probe[[2]],1),fixed=TRUE))
 if(language!="en") {
  stopifnot(all(performance_probe[[2]]!=performance_sources))
  for(s in c("Apparent (in-sample); descriptive only","Log loss (apparent)","Ranked probability score (apparent)","Multiclass Brier score (apparent)","Linearity in the logit is not established", "The independence of irrelevant alternatives")) stopifnot(!grepl(s,appendix,fixed=TRUE))
 }
 reference_probe <- logistic_appendix_table(data.frame(Variable=reference_vif_sources,Item=reference_vif_sources),language)
 stopifnot(identical(reference_probe[[1]],unname(reference_vif_sources)),grepl("사용자.A 50%",reference_probe[[2]][[1]],fixed=TRUE),grepl("1.5",reference_probe[[2]][[1]],fixed=TRUE),grepl("12.34",reference_probe[[2]][[2]],fixed=TRUE))
 if(language!="en") {
  stopifnot(all(reference_probe[[2]]!=reference_vif_sources))
  for(s in c("Reference for cat was not set", "max VIF=", "No multicollinearity problem", "No notable issues", "Odds-ratio confidence intervals use")) stopifnot(!grepl(s,appendix,fixed=TRUE))
 }
 if(language=="ja") {
  fixture <- paste0(html,as.character(analysis_result_table_section("比例オッズ判定",probe)),as.character(analysis_result_table_section("性能と仮定",performance_probe)),as.character(analysis_result_table_section("参照カテゴリとVIF",reference_probe)))
  saveRDS(list(list(id="logistic-odds",title="Logistic regression",html=fixture)),file.path(out,"entries.rds"))
 }
 cat("PASS:",language,"actual ordinal/multinomial; odds diagnostics; English main tables; user text preserved\n")
}
