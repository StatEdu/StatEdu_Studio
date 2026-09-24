Sys.setenv(LC_ALL="English_United States.utf8", LANG="English_United States.utf8")
invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
for (module in c("utils.R", "result_labels.R", "analysis_scope.R", "analysis_regression.R", "setup_mediation_moderation_ui.R")) {
  source(file.path("R", module), encoding="UTF-8")
}
set.seed(916)
d <- data.frame(q7=rep(0:1, each=80), X=rnorm(160), W=rnorm(160))
d$M <- .6*d$X + rnorm(160)
d$Y <- .3*d$X + .7*d$M + .4*d$X*d$W + rnorm(160)
d$C <- factor(rep(1:2,80))
run_worker <- function(data, moderation=FALSE) {
  args <- list(data=data, roles=list(y="Y", x="X", mediators=if(moderation) character() else "M",
    w=if(moderation) "W" else character(), covariates=c("C",if(length(analysis_scope_excluded(data))) "q7" else character())),
    mediator_arrangement="parallel", moderated_paths=if(moderation) "xy" else character(),
    boot_r=20L, seed=916L, language=Sys.getenv("STATEDU_TEST_LANGUAGE", "en"), residual_diagnostics=FALSE, auto_method=FALSE)
  if(identical(Sys.getenv("STATEDU_TEST_CUSTOM"),"true")) {
    args$custom_path_model <- TRUE
    args$direct_x <- "X"
    args$x_to_m <- list(M="X")
    args$m_to_y <- if(moderation) character() else "M"
  }
  job <- mediation_moderation_start_bootstrap_job(args)
  on.exit(mediation_moderation_cleanup_bootstrap_job(job), add=TRUE)
  job$process$wait(timeout=60000)
  if(job$process$is_alive()) {job$process$kill(); stop("Worker timeout")}
  if(!identical(job$process$get_exit_status(), 0L)) stop(paste(readLines(job$error_file, warn=FALSE), collapse="\n"))
  readRDS(job$result_file)
}
for (moderation in c(FALSE, TRUE)) {
  for (group in 0:1) {
    selected <- analysis_scope_filter(d, list(variable="q7", values=as.character(group)))
    attr(selected,"statedu_scope_excluded") <- "q7"
    actual <- run_worker(selected, moderation)
    manual <- selected; attr(manual,"statedu_scope_excluded") <- NULL
    expected <- run_worker(manual, moderation)
    stopifnot(isTRUE(all.equal(actual, expected, check.attributes=FALSE)))
    message("PASS: ", if(moderation) "moderation" else "mediation", " scoped worker matches manual subset, q7=",group)
  }
}
stopifnot(nrow(d)==160L, identical(d$q7, rep(0:1,each=80)))
