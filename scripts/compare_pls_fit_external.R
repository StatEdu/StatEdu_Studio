pls_fit_read_comparison_csv <- function(path, source_name) {
  if (!file.exists(path)) stop(source_name, " CSV was not found: ", path, call. = FALSE)
  value <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  required <- c("Model", "Fit", "srmr", "d_G", "d_ULS")
  missing <- setdiff(required, names(value))
  if (length(missing)) stop(source_name, " CSV is missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  value <- value[, required, drop = FALSE]
  value$Model <- tolower(trimws(as.character(value$Model)))
  value$Fit <- tolower(trimws(as.character(value$Fit)))
  if (any(!nzchar(value$Model))) stop(source_name, " CSV must contain non-empty Model values.", call. = FALSE)
  if (any(!value$Fit %in% c("saturated", "estimated"))) stop(source_name, " CSV Fit values must be saturated or estimated.", call. = FALSE)
  key <- paste(value$Model, value$Fit, sep = "::")
  if (anyDuplicated(key)) stop(source_name, " CSV must contain unique Model/Fit combinations.", call. = FALSE)
  for (metric in c("srmr", "d_G", "d_ULS")) {
    value[[metric]] <- suppressWarnings(as.numeric(value[[metric]]))
    if (any(!is.finite(value[[metric]]))) stop(source_name, " CSV contains a non-finite ", metric, " value.", call. = FALSE)
  }
  value
}

pls_fit_compare_external <- function(statedu_path, external_path, absolute_tolerance = 1e-6, relative_tolerance = 1e-4) {
  if (length(absolute_tolerance) != 1L || !is.finite(absolute_tolerance) || absolute_tolerance < 0 ||
      length(relative_tolerance) != 1L || !is.finite(relative_tolerance) || relative_tolerance < 0) {
    stop("Comparison tolerances must be finite non-negative numbers.", call. = FALSE)
  }
  statedu <- pls_fit_read_comparison_csv(statedu_path, "StatEdu")
  external <- pls_fit_read_comparison_csv(external_path, "External")
  statedu_key <- paste(statedu$Model, statedu$Fit, sep = "::")
  external_key <- paste(external$Model, external$Fit, sep = "::")
  if (!setequal(statedu_key, external_key)) stop("StatEdu and external CSV files must contain the same Model/Fit combinations.", call. = FALSE)
  external <- external[match(statedu_key, external_key), , drop = FALSE]
  metrics <- c("srmr", "d_G", "d_ULS")
  rows <- lapply(seq_len(nrow(statedu)), function(index) do.call(rbind, lapply(metrics, function(metric) {
    statedu_value <- statedu[[metric]][[index]]
    external_value <- external[[metric]][[index]]
    absolute_error <- abs(statedu_value - external_value)
    relative_error <- absolute_error / max(abs(statedu_value), abs(external_value), .Machine$double.eps)
    data.frame(
      Model = statedu$Model[[index]], Fit = statedu$Fit[[index]], Metric = metric,
      StatEdu = statedu_value, External = external_value,
      `Absolute error` = absolute_error, `Relative error` = relative_error,
      Pass = absolute_error <= absolute_tolerance || relative_error <= relative_tolerance,
      stringsAsFactors = FALSE, check.names = FALSE
    )
  })))
  do.call(rbind, rows)
}

pls_fit_external_arguments <- function(arguments) {
  values <- list(absolute_tolerance = 1e-6, relative_tolerance = 1e-4, report = "")
  for (argument in arguments) {
    pair <- strsplit(sub("^--", "", argument), "=", fixed = TRUE)[[1L]]
    if (length(pair) != 2L) next
    key <- gsub("-", "_", pair[[1L]], fixed = TRUE)
    values[[key]] <- pair[[2L]]
  }
  values$absolute_tolerance <- suppressWarnings(as.numeric(values$absolute_tolerance))
  values$relative_tolerance <- suppressWarnings(as.numeric(values$relative_tolerance))
  values
}

if (sys.nframe() == 0L) {
  arguments <- pls_fit_external_arguments(commandArgs(trailingOnly = TRUE))
  if (is.null(arguments$statedu) || is.null(arguments$external)) {
    stop("Usage: Rscript scripts/compare_pls_fit_external.R --statedu=statedu.csv --external=external.csv [--absolute-tolerance=1e-6] [--relative-tolerance=1e-4] [--report=comparison.csv]", call. = FALSE)
  }
  comparison <- pls_fit_compare_external(arguments$statedu, arguments$external, arguments$absolute_tolerance, arguments$relative_tolerance)
  if (nzchar(arguments$report)) utils::write.csv(comparison, arguments$report, row.names = FALSE, na = "")
  print(comparison, row.names = FALSE)
  if (!all(comparison$Pass)) stop("External PLS fit comparison exceeded the configured tolerance.", call. = FALSE)
  cat("External PLS fit comparison passed.\n")
}
