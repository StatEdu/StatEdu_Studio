Sys.setlocale("LC_CTYPE","English_United States.utf8")
Sys.setenv(STATEDU_MODULE_CACHE="false")
source("R/app_bootstrap.R",encoding="UTF-8")
load_app_packages(check=FALSE); source_app_modules()
info <- data.frame(name=c("사용자.A","ordinal_y","category_y","Model-based"),measurement=c("binary","ordered","category","continuous"))
d <- data.frame(y=1:4,x=2:5)
capture_error <- function(expr) {
 value <- tryCatch({force(expr); NULL},error=function(e) conditionMessage(e))
 stopifnot(!is.null(value)); value
}
errors <- c(capture_error(prepare_logistic_analysis_results(d,character(),"x")),capture_error(prepare_logistic_analysis_results(d,"y",character())))
stopifnot(identical(errors,c("Select at least one dependent variable.","Select at least one Block 1 variable.")))
for(language in c("en","ko","ja","zh","es","fr","de","vi")) {
 setup <- logistic_setup_state(info$name,info$name[1:3],info$name[4],character(),character(),info,language=language)
 stopifnot(identical(setup$dependents,info$name[1:3]),identical(setup$block1,info$name[4]))
 expected <- vapply(c("binary","ordered","category"),function(x) statedu_t(paste0("analysis.logistic_setup.",x),language),character(1))
 stopifnot(identical(setup$model_family,paste(expected,collapse="; ")))
 doc <- xml2::read_html(as.character(htmltools::renderTags(logistic_setup_panel(setup,NULL))$html),encoding="UTF-8")
 text <- xml2::xml_text(doc)
 for(label in c(expected,statedu_t("analysis.logistic_setup.options",language),info$name)) stopifnot(grepl(label,text,fixed=TRUE))
 stopifnot(identical(logistic_model_family_label(character(),info,language),statedu_t("analysis.logistic_setup.select_type",language)))
 for(i in seq_along(errors)) stopifnot(identical(logistic_error_ui_text(errors[[i]],language),statedu_t(paste0("analysis.logistic_setup.",c("missing_dependent","missing_block")[[i]]),language)))
 if(language!="en") stopifnot(!grepl("Model options",text,fixed=TRUE),all(expected!=c("Binary logistic regression","Ordinal logistic regression","Multinomial logistic regression")))
 unknown <- "External error for 사용자.A: 50%"
 stopifnot(identical(logistic_error_ui_text(unknown,language),unknown))
 cat("PASS:",language,"rendered setup; mixed families; raw selections/user names; actual validation errors\n")
}
