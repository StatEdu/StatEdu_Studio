Sys.setlocale("LC_CTYPE","English_United States.utf8")
Sys.setenv(STATEDU_MODULE_CACHE="false")
source("R/app_bootstrap.R",encoding="UTF-8")
load_app_packages(check=FALSE); source_app_modules()
capture_error <- function(expr) {
 value <- tryCatch({force(expr); NULL},error=function(e) conditionMessage(e))
 stopifnot(!is.null(value)); value
}
d <- data.frame(y=1:5,x=2:6)
actual <- c(
 capture_error(generalized_fit_model(d,y~x,"gaussian","identity",FALSE,FALSE,FALSE,weights=1:2)),
 capture_error(generalized_fit_model(d,y~x,"count","log",FALSE,FALSE,FALSE,count_family_lock="invalid")),
 capture_error(generalized_family_object("gamma","logit")))
keys <- paste0("analysis.glm_error.",c("weights","count_lock","link"))

# Inspect the AST, not comments or test copies, for all explicit engine stop messages.
messages <- character()
visit <- function(node) {
 if(is.call(node) && identical(node[[1]],as.name("stop")) && length(node)>=2L) {
  arg <- node[[2]]
  if(is.character(arg)) messages <<- c(messages,arg)
  else if(is.call(arg) && identical(arg[[1]],as.name("sprintf")) && is.character(arg[[2]])) messages <<- c(messages,arg[[2]])
 }
 if(is.call(node) || is.expression(node)) for(i in seq_along(node)) {
  if(identical(node[[i]],quote(expr=))) next
  if(!is.symbol(node[[i]])) visit(node[[i]])
 }
}
visit(parse("R/analysis_generalized.R",encoding="UTF-8"))
messages <- unique(messages)
stopifnot(length(messages)>=25)
messages <- vapply(messages,function(x) {
 if(grepl("%d",x,fixed=TRUE)) sprintf(x,5L) else if(grepl("%s",x,fixed=TRUE)) sprintf(x,"gamma") else x
},character(1))
for(language in c("en","ko","ja","zh","es","fr","de","vi")) {
 for(i in seq_along(actual)) {
  expected <- statedu_t(keys[[i]],language)
  if(i==3L) expected <- sprintf(expected,"gamma")
  stopifnot(identical(generalized_error_ui_text(actual[[i]],language),expected))
 }
 for(message in messages) {
  translated <- generalized_error_ui_text(message,language)
  if(language=="en") stopifnot(identical(message,translated)) else stopifnot(message!=translated)
 }
 unknown <- "External error: 사용자.A (50%)"
 stopifnot(identical(generalized_error_ui_text(unknown,language),unknown))
 cat("PASS:",language,"3 actual configuration errors;",length(messages),"distinct explicit GLM engine error messages covered\n")
}
