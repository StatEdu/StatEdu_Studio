Sys.setlocale('LC_CTYPE','English_United States.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8')
load_app_packages(check=FALSE);source_app_modules()
captured <- list()
structural_canvas_show_notification <- function(message,type,duration) {
  captured[[length(captured)+1L]] <<- list(message=message,type=type,duration=duration)
}
healthy <- list(admissible=TRUE,converged=TRUE,post_check=TRUE,identified=TRUE,
  df=-2,theta_min_eigenvalue=-0.12345,latent_min_eigenvalue=-0.56789,
  parameter_min_eigenvalue=-1.23456e-12,theta_condition_number=1.23456e12,
  latent_condition_number=2.34567e13,parameter_condition_number=3.45678e14)
fields <- c('converged','post_check','identified','negative_residuals','negative_latent_variances',
  'non_psd_theta','non_psd_latent_covariance','near_singular_theta','near_singular_latent_covariance',
  'non_psd_parameter_covariance','near_singular_parameter_covariance','invalid_correlations',
  'ill_conditioned_theta','ill_conditioned_latent_covariance','ill_conditioned_parameter_covariance')
literal <- c('Review 사용자 <&> %s','Normality, X.1')
values <- c(list(FALSE,FALSE,FALSE,literal,literal),rep(list(TRUE),10))
expected <- c('','',format_decimal3(healthy$df),rep(paste(literal,collapse=', '),2),
  rep(c(format_decimal3(healthy$theta_min_eigenvalue),format_decimal3(healthy$latent_min_eigenvalue)),2),
  rep(format(healthy$parameter_min_eigenvalue,scientific=TRUE,digits=3),2),'',
  vapply(healthy[c('theta_condition_number','latent_condition_number','parameter_condition_number')],
    format,character(1),scientific=TRUE,digits=3))
cases <- lapply(seq_along(fields),function(i) {
  result <- healthy;result[[fields[i]]] <- values[[i]]
  if(i<=12)result$admissible <- FALSE
  result
})
all_bad <- healthy
for(i in seq_along(fields))all_bad[[fields[i]]] <- values[[i]]
all_bad$admissible <- FALSE
english <- character(length(cases))
for(language in c('en','ko','ja','zh','es','fr','de','vi')) {
  captured <- list();structural_canvas_notify_solution_diagnostics(healthy,language)
  stopifnot(length(captured)==0L)
  for(i in seq_along(cases)) {
    captured <- list();structural_canvas_notify_solution_diagnostics(cases[[i]],language)
    stopifnot(length(captured)==1L)
    entry <- captured[[1]]
    if(language=='en')english[i] <- entry$message else {
      stopifnot(entry$message!=english[i])
      # Check the detail separately so a translated wrapper cannot hide an English fallback.
      english_detail <- sub('^[^:]+: ', '', english[i])
      english_detail <- sub('\\. (Interpret fit|Small data).*$', '', english_detail)
      stopifnot(!grepl(english_detail,entry$message,fixed=TRUE))
    }
    if(nzchar(expected[i]))stopifnot(grepl(expected[i],entry$message,fixed=TRUE))
    if(i<=12)stopifnot(entry$type=='error',is.null(entry$duration)) else
      stopifnot(entry$type=='warning',entry$duration==12)
  }
  captured <- list();structural_canvas_notify_solution_diagnostics(all_bad,language)
  stopifnot(length(captured)==2L,captured[[1]]$type=='error',captured[[2]]$type=='warning')
  cat('PASS:',language,'15 independent conditions, combined warnings, healthy silence, literal names and numeric precision\n')
}
