# Compare every loaded-data engine with a manually selected identical subset.
Sys.setenv(LC_ALL="English_United States.utf8", LANG="English_United States.utf8", STATEDU_MODULE_CACHE="false")
invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
source("R/app_bootstrap.R",encoding="UTF-8"); load_app_packages(check=FALSE); source_app_modules()
statedu_apply_preferences(statedu_default_preferences())
options(statedu.app_language="en")
make_info <- function(names,measurements) data.frame(name=names,measurement=measurements)
active <- FALSE
for(e in parse("scripts/benchmark_analysis_pipeline_all.R")) {
  if(is.call(e) && identical(e[[1]],as.name("set.seed"))) active <- TRUE
  if(active) eval(e)
  if(is.call(e) && identical(e[[1]],as.name("<-")) && identical(e[[2]],as.name("cases"))) break
}
# Reuse the established survival, longitudinal and complex-sample fixtures.
active <- FALSE
for(e in parse("scripts/compare_menu_versions.R")) {
  if(is.call(e) && identical(e[[1]],as.name("set.seed"))) active <- TRUE
  if(is.call(e) && identical(e[[1]],as.name("source")) && active) break
  if(active) eval(e)
}
add("Hierarchical regression",function() prepare_hierarchical_analysis_results(reg_data,"y","x1","x2",character(),variable_info=reg_info,residual_diagnostics=FALSE,boot_r=10L))
fixture_names <- ls()[vapply(mget(ls()),function(x)is.data.frame(x)&&nrow(x)==180L,logical(1))]
original <- mget(fixture_names)
records <- list()
capture <- function(fn) {
  set.seed(916)
  tryCatch(suppressWarnings(fn()),error=function(e)structure(list(message=conditionMessage(e)),class="scope_test_error"))
}
for(group in 0:1) {
  for(name in fixture_names) {
    data <- original[[name]]
    # Keep all repeated observations for each longitudinal subject together.
    keys <- if(name=="long") data$id else seq_len(nrow(data))
    data <- data[keys%%2L==group,,drop=FALSE]
    assign(name,data)
  }
  manual <- lapply(cases,function(case)capture(case[[2]]))
  for(name in fixture_names) {
    data <- get(name); attr(data,"statedu_scope_excluded") <- "scope_group"
    assign(name,data)
  }
  for(i in seq_along(cases)) {
    case <- cases[[i]]; actual <- capture(case[[2]])
    error <- if(inherits(actual,"scope_test_error")) actual$message else if(inherits(manual[[i]],"scope_test_error")) manual[[i]]$message else ""
    equal <- !nzchar(error) && isTRUE(all.equal(actual,manual[[i]],check.attributes=FALSE))
    if(!equal && !nzchar(error)) error <- paste(head(all.equal(actual,manual[[i]],check.attributes=FALSE),3),collapse="; ")
    records[[length(records)+1L]] <- data.frame(menu=case[[1]],group=group,pass=equal,error=error)
    message(if(equal) "PASS: " else "FAIL: ",case[[1]]," group=",group,if(nzchar(error))paste0(" — ",error))
  }
}
dir.create("tmp/analysis-scope",recursive=TRUE,showWarnings=FALSE)
report <- do.call(rbind,records)
write.csv(report,"tmp/analysis-scope/menu-engine-audit.csv",row.names=FALSE)
stopifnot(all(report$pass))
