structural_canvas_show_notification <- function(message, type = "message", duration = 5) {
  domain <- shiny::getDefaultReactiveDomain()
  if (is.null(domain) || !is.function(domain$sendNotification)) return(invisible(FALSE))
  showNotification(message, type = type, duration = duration)
  invisible(TRUE)
}

structural_canvas_notify_identification_warnings <- function(identification_warnings) {
  if (nrow(identification_warnings)) {
    structural_canvas_show_notification(paste0("Identification warning: ", paste(paste0(identification_warnings$Element, " — ", identification_warnings$Message), collapse = "; ")), type = "warning", duration = 12)
  }
}

structural_canvas_notify_missing_covariances <- function(missing_covariances, analysis_type, language = NULL) {
  if (analysis_type %in% c("cfa", "cbsem") && length(missing_covariances)) {
    ko <- identical(normalize_app_language(language), "ko")
    message <- if (ko) {
      paste0("외생 잠재변수 사이의 공분산 경로가 없습니다: ", paste(missing_covariances, collapse = ", "), ". 해당 공분산은 0으로 고정됩니다.")
    } else {
      paste0("Missing covariance paths between exogenous latent variables: ", paste(missing_covariances, collapse = ", "), ". These covariances will be fixed to zero.")
    }
    structural_canvas_show_notification(message, type = "warning", duration = 10)
  }
  invisible(TRUE)
}

structural_canvas_notify_ignored_pls_covariances <- function(result, analysis_type, language = NULL) {
  ignored_covariances <- result$ignored_covariances %||% character(0)
  if (identical(analysis_type, "plssem") && length(ignored_covariances)) {
    ko <- identical(normalize_app_language(language), "ko")
    message <- if (ko) {
      paste0("PLS-SEM은 공분산 경로를 추정하지 않으므로 제외했습니다: ", paste(ignored_covariances, collapse = ", "), ".")
    } else {
      paste0("PLS-SEM does not estimate covariance paths; excluded: ", paste(ignored_covariances, collapse = ", "), ".")
    }
    structural_canvas_show_notification(message, type = "warning", duration = 10)
  }
  invisible(TRUE)
}

structural_canvas_notify_solution_diagnostics <- function(result) {
  if (!isTRUE(result$admissible)) {
    details <- c(
      if (!isTRUE(result$converged)) "the model did not converge",
      if (!isTRUE(result$post_check)) "lavaan post-estimation checks failed",
      if (!isTRUE(result$identified)) paste0("model degrees of freedom are invalid (df = ", format_decimal3(result$df), ")"),
      if (length(result$negative_residuals)) paste0("negative residual variances: ", paste(result$negative_residuals, collapse = ", ")),
      if (length(result$negative_latent_variances)) paste0("negative latent variances: ", paste(result$negative_latent_variances, collapse = ", ")),
      if (isTRUE(result$non_psd_theta)) paste0("residual covariance matrix is not positive semidefinite (minimum eigenvalue = ", format_decimal3(result$theta_min_eigenvalue), ")"),
      if (isTRUE(result$non_psd_latent_covariance)) paste0("latent covariance matrix is not positive semidefinite (minimum eigenvalue = ", format_decimal3(result$latent_min_eigenvalue), ")"),
      if (isTRUE(result$near_singular_theta)) paste0("residual covariance matrix is near singular or on the boundary (minimum eigenvalue = ", format_decimal3(result$theta_min_eigenvalue), ")"),
      if (isTRUE(result$near_singular_latent_covariance)) paste0("latent covariance matrix is near singular or on the boundary (minimum eigenvalue = ", format_decimal3(result$latent_min_eigenvalue), ")"),
      if (isTRUE(result$non_psd_parameter_covariance)) paste0("parameter-estimate covariance matrix is not positive semidefinite (minimum eigenvalue = ", format(result$parameter_min_eigenvalue, scientific = TRUE, digits = 3), ")"),
      if (isTRUE(result$near_singular_parameter_covariance)) paste0("parameter-estimate covariance matrix is near singular (minimum eigenvalue = ", format(result$parameter_min_eigenvalue, scientific = TRUE, digits = 3), ")"),
      if (isTRUE(result$invalid_correlations)) "one or more absolute latent correlations are at least 1"
    )
    structural_canvas_show_notification(paste0("Potentially inadmissible solution: ", paste(details, collapse = "; "), ". Interpret fit, AVE, CR, and validity results with caution."), type = "error", duration = NULL)
  }

  conditioning_details <- c(
    if (isTRUE(result$ill_conditioned_theta)) paste0("residual covariance condition number = ", format(result$theta_condition_number, scientific = TRUE, digits = 3)),
    if (isTRUE(result$ill_conditioned_latent_covariance)) paste0("latent covariance condition number = ", format(result$latent_condition_number, scientific = TRUE, digits = 3)),
    if (isTRUE(result$ill_conditioned_parameter_covariance)) paste0("parameter-estimate covariance condition number = ", format(result$parameter_condition_number, scientific = TRUE, digits = 3))
  )
  if (length(conditioning_details)) {
    structural_canvas_show_notification(paste0("Numerically ill-conditioned solution: ", paste(conditioning_details, collapse = "; "), ". Small data or specification changes may produce unstable estimates."), type = "warning", duration = 12)
  }
}
