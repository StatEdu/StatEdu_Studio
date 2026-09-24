.libPaths(R.home('library'))
invisible(compiler::enableJIT(3))
args <- commandArgs(TRUE)
mode <- args[[1]]; id <- args[[2]]
profile_path <- if (length(args) >= 3L) args[[3]] else NULL
stopifnot(mode %in% c('baseline', 'current'))
root <- 'output/server-first-analysis-20260913'
dir.create(root, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(STATEDU_USER_SETTINGS_DIR=file.path(root,paste0('settings-',mode,'-',id)),
  STATEDU_RESULT_STORE=file.path(root,paste0('results-',mode,'-',id,'.json')),
  STATEDU_STOP_ON_SESSION_END='0', STATEDU_TIMING='0')
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
if (mode == 'baseline') {
  baseline <- new.env(parent=.GlobalEnv)
  sys.source('output/server-compilation-review-20260913/baseline.R', baseline)
  create_app_server <- baseline$create_app_server
  environment(create_app_server) <- .GlobalEnv
}
captured <- new.env(parent=emptyenv())
for (name in c('prepare_correlation_results','prepare_km_analysis_result')) {
  wrapper <- local({
    original <- get(name, envir=.GlobalEnv); key <- name
    function(...) { value <- original(...); captured[[key]] <- value; value }
  })
  assign(name, wrapper, envir=.GlobalEnv)
}
helper_times <- list()
if (!is.null(profile_path)) {
  for (name in c('prepare_km_analysis_result', 'survival_km_results_panel',
                 'survival_km_ggplot', 'survival_km_risk_table_plot',
                 'survival_draw_plot_with_risk_table', 'survival_simple_table')) {
    wrapper <- local({
      original <- get(name, envir=.GlobalEnv); key <- name
      function(...) {
        started <- proc.time()[['elapsed']]
        on.exit({helper_times[[length(helper_times)+1L]] <<- data.frame(
          helper=key,elapsed=proc.time()[['elapsed']]-started)}, add=TRUE)
        original(...)
      }
    })
    assign(name, wrapper, envir=.GlobalEnv)
  }
}
warnings <- notifications <- character()
original_notification <- showNotification
showNotification <- function(ui, ...) {
  notifications <<- c(notifications, paste(as.character(ui), collapse=' '))
  original_notification(ui, ...)
}
timings <- numeric()
stage <- function(name, expr) {
  profiling <- identical(name, 'survival_run') && !is.null(profile_path)
  if (profiling) {
    Rprof(profile_path, interval=0.005)
    on.exit(Rprof(NULL), add=TRUE)
  }
  timings[[name]] <<- system.time(withCallingHandlers(force(expr), warning=function(w) {
    warnings <<- c(warnings,conditionMessage(w));invokeRestart('muffleWarning')
  }))[['elapsed']]
}
stage('factory', server <- create_app_server(read_app_config()$version))
mock <- shiny::MockShinySession$new()
shiny::withReactiveDomain(mock, {
  stage('initialization', {server(mock$input,mock$output,mock);mock$flushReact()})
  stage('upload_selection', {
    path <- normalizePath('scripts/fixtures/survival_validation.csv',winslash='/')
    mock$setInputs(header=TRUE, file=data.frame(name=basename(path),size=file.info(path)$size,
      type='text/csv',datapath=path))
    mock$setInputs(apply_variable_request=list(select_all_loaded=TRUE,
      measurements=c(id='id',time='continuous',status='category',sex='category',
                     ph.ecog='category',age='continuous')))
  })
  stage('correlation_setup', {
    mock$setInputs(main_menu='Correlation')
    mock$setInputs(correlation_available=c('time','age'),correlation_move=1L,
      correlation_continuous_method='pearson',correlation_normality=FALSE,
      correlation_latent_correlations=FALSE,correlation_p_ci=TRUE,
      correlation_significance_levels=TRUE,correlation_scatter_plot=FALSE,correlation_matrix_plot=FALSE)
  })
  stage('correlation_run', mock$setInputs(run_correlation=1L))
  stopifnot(exists('prepare_correlation_results',envir=captured,inherits=FALSE))
  stage('survival_setup', {
    mock$setInputs(main_menu='analysis_survival_km')
    mock$setInputs(survival_km_available='time',survival_km_time_move=1L)
    mock$setInputs(survival_km_available='status',survival_km_event_move=1L)
    mock$setInputs(survival_km_available='sex',survival_km_group_move=1L)
    mock$setInputs(survival_km_event_value='1',survival_km_rate_times='100, 200, 400',
      survival_km_analysis_method='km',survival_km_test_method='logrank',
      survival_km_output_tables=c('survival_table','survival_time'),
      survival_km_plot_types='survival',survival_km_plot_versions='color',
      survival_km_show_ci=TRUE,survival_km_show_censor=TRUE,survival_km_data_shape='single_record')
  })
  stage('survival_run', mock$setInputs(run_survival_km=1L))
  stopifnot(exists('prepare_km_analysis_result',envir=captured,inherits=FALSE))
  captured$correlation_html <- mock$getOutput('correlation_results')
  captured$survival_html <- mock$getOutput('survival_km_results')
})
mock$close()
stopifnot(nzchar(captured$correlation_html$html),nzchar(captured$survival_html$html))
row <- data.frame(mode=mode,id=id,as.list(timings),total=sum(timings),warnings=length(warnings))
print(row); print(notifications); print(warnings)
write.csv(row,file.path(root,paste0(mode,'-',id,'.csv')),row.names=FALSE)
saveRDS(list(results=as.list(captured),warnings=warnings,notifications=notifications),
        file.path(root,paste0(mode,'-',id,'.rds')))
if (!is.null(profile_path)) {
  write.csv(do.call(rbind,helper_times),paste0(profile_path,'.helpers.csv'),row.names=FALSE)
  profile <- summaryRprof(profile_path)
  write.csv(profile$by.total,paste0(profile_path,'.total.csv'))
  write.csv(profile$by.self,paste0(profile_path,'.self.csv'))
}
