Sys.setlocale("LC_CTYPE", "English_United States.utf8")
Sys.setenv(STATEDU_MODULE_CACHE="false")
source("R/app_bootstrap.R",encoding="UTF-8")
load_app_packages(check=FALSE); source_app_modules()
sources <- jsonlite::read_json("scripts/fixtures/glm_i18n_input_errors.json",simplifyVector=TRUE)
capture_error <- function(expr) tryCatch({force(expr); stop("Expected validation error")},error=function(e) conditionMessage(e))
d <- data.frame(x=1:5,y=c(-1,0,1,2,3),exposure=c(0,1,2,3,4))
info <- data.frame(name=names(d),measurement="continuous")
actual <- c(
 capture_error(prepare_generalized_analysis_result(d,character(),"x",variable_info=info)),
 capture_error(prepare_generalized_analysis_result(d,"y",character(),variable_info=info)),
 capture_error(prepare_generalized_analysis_result(d[1:2,],"y","x",variable_info=info)),
 capture_error(generalized_validate_outcome(d,"y","gamma")),
 capture_error(generalized_validate_outcome(d,"y","count")),
 capture_error(generalized_validate_offset(d,"exposure")),
 capture_error(generalized_validate_outcome(d,"y","binomial")))
stopifnot(all(actual[1:6] %in% names(sources)),actual[[7]]=="Binary logistic GLM requires two observed outcome levels.")
aliases <- c("Binary logistic GLM requires exactly two observed outcome values.",actual[[7]])
for(language in c("en","ko","ja","zh","es","fr","de","vi")) {
 for(source in names(sources)) {
  value <- generalized_error_ui_text(source,language)
  stopifnot(identical(value,statedu_t(sources[[source]],language)))
  if(language!="en") stopifnot(value!=source)
 }
 for(source in actual) {
  value <- generalized_error_ui_text(source,language)
  if(language=="en") stopifnot(identical(value,source)) else stopifnot(value!=source)
 }
 for(source in aliases) if(language!="en") stopifnot(identical(generalized_error_ui_text(source,language),statedu_t("analysis.glm_error.binary",language)))
 unknown <- "User variable '사용자.A': invalid value in Model-based"
 stopifnot(identical(generalized_error_ui_text(unknown,language),unknown),is.na(generalized_error_ui_text(NA_character_,language)))
 cat("PASS:",language,"input error translations; actual validation failures; external/user text preserved\n")
}
server <- paste(readLines("R/server_generalized.R",encoding="UTF-8"),collapse="\n")
stopifnot(grepl("generalized_error_ui_text(conditionMessage(e), language)",server,fixed=TRUE))
