Sys.setlocale("LC_CTYPE","English_United States.utf8")
Sys.setenv(STATEDU_MODULE_CACHE="false")
source("R/app_bootstrap.R",encoding="UTF-8")
load_app_packages(check=FALSE); source_app_modules()
sources <- jsonlite::read_json("scripts/fixtures/glm_i18n_mi_errors.json",simplifyVector=TRUE)
sources <- c(sources,jsonlite::read_json("scripts/fixtures/glm_i18n_mi_remaining_errors.json",simplifyVector=TRUE))
capture_error <- function(expr) {
 message <- tryCatch({force(expr); NULL},error=function(e) conditionMessage(e))
 stopifnot(!is.null(message)); message
}
one <- data.frame(Term="사용자.A",B=1,SE=.2)
other <- data.frame(Term="다른 변수",B=1,SE=.2)
robust <- one; attr(robust,"se_type_used") <- "HC3"
actual <- c(
 capture_error(generalized_pool_coef_tables(list(one),m_expected=5)),
 capture_error(generalized_pool_coef_tables(list(one,other),dfcom=10)),
 capture_error(generalized_pool_coef_tables(list(one,robust),dfcom=10)),
 capture_error(generalized_pool_coef_tables(list(one,one))))
stopifnot(grepl("all 5 fitted",actual[[1]],fixed=TRUE),grepl("imputation(s): 2",actual[[2]],fixed=TRUE))
scalar_errors <- c(
 capture_error(generalized_rubin_pool_scalar(1,.2,10)),
 capture_error(generalized_rubin_pool_scalar(c(1,2),c(.2,.2),0)),
 capture_error(generalized_rubin_pool_scalar(c(1e308,-1e308),c(.2,.2),10)),
 capture_error(generalized_fit_mi(data.frame(y=1:5,x=2:6),y~x,"gaussian","identity",FALSE,FALSE,FALSE)))
stopifnot(identical(unname(unlist(sources[scalar_errors])),paste0("analysis.glm_error.",c("mi_rubin","mi_df_positive","mi_df_failed","mi_not_needed"))))
actual <- c(actual,scalar_errors)
detail <- "Imputation 2: 사용자.A / Model-based; 50% failure\nexternal detail (x=3)."
compound <- paste("MI model fitting failed; partial pooling is not allowed.",detail)
for(language in c("en","ko","ja","zh","es","fr","de","vi")) {
 for(source in names(sources)) {
  dynamic <- grepl("%s",source,fixed=TRUE)
  arg <- if(grepl("all %s",source,fixed=TRUE)) "5" else if(grepl("imputation(s)",source,fixed=TRUE)) "2, 5" else detail
  message <- if(dynamic) sprintf(source,arg) else source
  expected <- if(dynamic) sprintf(statedu_t(sources[[source]],language),arg) else statedu_t(sources[[source]],language)
  stopifnot(identical(generalized_error_ui_text(message,language),expected))
  if(language!="en") stopifnot(message!=expected)
 }
 for(message in actual) {
  localized <- generalized_error_ui_text(message,language)
  if(language=="en") stopifnot(identical(localized,message)) else stopifnot(localized!=message)
 }
 stopifnot(grepl(detail,generalized_error_ui_text(compound,language),fixed=TRUE))
 unknown <- paste("External error:",actual[[1]])
 stopifnot(identical(generalized_error_ui_text(unknown,language),unknown))
 cat("PASS:",language,"MI errors; actual pooling failures; numbers and external detail preserved\n")
}
