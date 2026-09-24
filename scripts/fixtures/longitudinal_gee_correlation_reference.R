longitudinal_gee_check_simple_correlation <- function(corstr, alpha, id, waves) {
  if (!corstr %in% c("exchangeable", "ar1")) return(invisible(NULL))
  if (length(alpha) != 1L || !is.finite(alpha)) {
    stop("GEE returned an invalid working correlation parameter; estimates were not accepted.", call. = FALSE)
  }
  groups <- split(waves, id)
  for (wave in groups) {
    if (length(wave) < 2L) next
    correlation <- if (identical(corstr, "exchangeable")) {
      value <- matrix(alpha, length(wave), length(wave))
      diag(value) <- 1
      value
    } else outer(wave, wave, function(a, b) alpha^abs(a - b))
    if (any(!is.finite(correlation))) {
      stop("GEE returned an invalid working correlation matrix; estimates were not accepted.", call. = FALSE)
    }
    values <- eigen(correlation, symmetric = TRUE, only.values = TRUE)$values
    if (min(values) <= sqrt(.Machine$double.eps) * max(values)) {
      stop(sprintf("GEE %s working correlation is singular or numerically non-positive-definite (minimum eigenvalue %.6g); estimates were not accepted.", corstr, min(values)), call. = FALSE)
    }
  }
  invisible(NULL)
}


