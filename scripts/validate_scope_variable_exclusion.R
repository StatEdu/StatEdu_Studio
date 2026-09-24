Sys.setenv(STATEDU_MODULE_CACHE="false")
source("R/app_bootstrap.R",encoding="UTF-8");load_app_packages(check=FALSE);source_app_modules()
set.seed(81)
d <- data.frame(sex=rep(c("Male","Female"),each=60),x=rnorm(120),z=rnorm(120))
d$y <- 2*d$x + .5*d$z + rnorm(120)
info <- data.frame(name=names(d),measurement=c("binary",rep("continuous",3)),stringsAsFactors=FALSE)
selected <- d[d$sex=="Male",,drop=FALSE]
attr(selected,"statedu_scope_excluded") <- "sex"
plain <- selected;attr(plain,"statedu_scope_excluded") <- NULL
# Actual engines must match an analysis in which the user manually removed sex.
compare <- function(fn,args) {
  manual <- args
  for(n in intersect(names(args),c("variables","predictors","block1","block2","block3")))manual[[n]]<-setdiff(args[[n]],"sex")
  a <- do.call(fn,c(list(data=selected,variable_info=info),args))
  b <- do.call(fn,c(list(data=plain,variable_info=info),manual))
  stopifnot(isTRUE(all.equal(a,b,check.attributes=FALSE)))
  invisible(a)
}
compare(prepare_frequencies_results,list(variables=c("sex","x","z")))
compare(prepare_correlation_results,list(variables=c("sex","x","z")))
compare(prepare_pca_results,list(variables=c("sex","x","z")))
compare(prepare_regression_analysis_results,list(dependents="y",predictors=c("sex","x","z"),boot_r=10,residual_diagnostics=FALSE,auto_method=FALSE))
compare(prepare_hierarchical_analysis_results,list(dependents="y",block1=c("sex","x"),block2="z",block3=character(),boot_r=10,residual_diagnostics=FALSE,auto_method=FALSE))
compare(prepare_generalized_analysis_result,list(outcome="y",predictors=c("sex","x","z"),family="gaussian",assumption_checks=FALSE))
stopifnot(identical(d$sex,rep(c("Male","Female"),each=60)))
# The shared dispatcher supplies the exclusion to both case-only and every split group.
shiny::testServer(function(input,output,session){
  scope <- register_analysis_scope(input,output,session,function()d,function()"en",function()"file")
}, {
  session$flushReact()
  session$setInputs(scope_cases_variable="sex",scope_cases_values="Male",scope_cases_apply=1)
  stopifnot(nrow(scope$dataset())==60L,identical(analysis_scope_excluded(scope$dataset()),"sex"))
  stopifnot(identical(prepare_frequencies_results(scope$dataset(),c("sex","x"),info)$variables,"x"))
  session$setInputs(scope_cases_clear=1,scope_split_variable="sex",scope_split_apply=1)
  stopifnot(nrow(scope$dataset())==120L,identical(analysis_scope_excluded(scope$dataset()),"sex"))
  session$setInputs(scope_split_clear=1)
  stopifnot(!length(analysis_scope_excluded(scope$dataset())),identical(prepare_frequencies_results(scope$dataset(),c("sex","x"),info)$variables,c("sex","x")))
})
snapshot <- list(nodes=list(list(id="a",role="independent",variableId="sex"),list(id="b",role="dependent",variableId="y"),list(id="c",role="independent",variableId="x")),
  edges=list(list(id="ab",from="a",to="b"),list(id="cb",from="c",to="b")),covariates=c("sex","z"),moderations=list(list(from="a",toEdge="cb")))
clean <- analysis_scope_model_snapshot(snapshot,selected)
stopifnot(length(clean$nodes)==2L,length(clean$edges)==1L,clean$edges[[1]]$id=="cb",identical(clean$covariates,list("z")),length(clean$moderations)==0L,length(snapshot$nodes)==3L)
stopifnot(grepl('"covariates":["z"]',jsonlite::toJSON(clean,auto_unbox=TRUE),fixed=TRUE))
snapshot$nodes[[1]]$role <- "indicator"
snapshot$nodes <- c(snapshot$nodes,list(list(id="err",role="error",name="e1")))
snapshot$edges <- c(snapshot$edges,list(list(id="err_a",from="err",to="a")))
clean <- analysis_scope_model_snapshot(snapshot,selected)
stopifnot(length(clean$nodes)==2L,length(clean$edges)==1L)
# A variable shared by case selection and split is omitted once; additional
# split variables are omitted even when a range retains several values.
attr(selected,"statedu_scope_excluded") <- c("sex","z")
stopifnot(identical(prepare_frequencies_results(selected,c("sex","z","x"),info)$variables,"x"))
error <- tryCatch(prepare_correlation_results(selected,c("sex","x"),info),error=identity)
stopifnot(inherits(error,"error"),grepl("at least two",conditionMessage(error)))
message("PASS: scope exclusions match manual omission in frequencies, correlation, PCA, regression, hierarchical regression, GLM; scope reset restores variables; canvas paths pruned non-destructively")
