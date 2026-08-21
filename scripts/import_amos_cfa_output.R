amos_output_arguments <- function(arguments) {
  values <- list(input = "", data = file.path("sample", "HolzingerSwineford1939.csv"), output = "")
  for (argument in arguments) {
    pair <- strsplit(sub("^--", "", argument), "=", fixed = TRUE)[[1L]]
    if (length(pair) == 2L) values[[gsub("-", "_", pair[[1L]], fixed = TRUE)]] <- pair[[2L]]
  }
  values
}

amos_output_numeric <- function(document, h_code, require_x = TRUE) {
  xpath <- if (require_x) {
    sprintf("//td[@h='%s' and @x]", h_code)
  } else {
    sprintf("//td[@h='%s']", h_code)
  }
  nodes <- xml2::xml_find_all(document, xpath)
  if (!length(nodes)) stop("AMOS output value was not found for h=", h_code, call. = FALSE)
  value <- if (require_x) xml2::xml_attr(nodes[[1L]], "x") else xml2::xml_text(nodes[[1L]])
  value <- suppressWarnings(as.numeric(trimws(value)))
  if (!is.finite(value)) stop("AMOS output value was not numeric for h=", h_code, call. = FALSE)
  value
}

amos_output_path_rows <- function(document, node_caption, section, correlation = FALSE) {
  container <- xml2::xml_find_first(
    document, sprintf("//div[@nodecaption=%s]", shQuote(node_caption, type = "sh"))
  )
  if (inherits(container, "xml_missing")) stop("AMOS output table was not found: ", node_caption, call. = FALSE)
  rows <- xml2::xml_find_all(container, ".//tbody/tr")
  parsed <- lapply(rows, function(row) {
    variables <- xml2::xml_attr(xml2::xml_find_all(row, ".//td[@vname]"), "vname")
    estimate <- xml2::xml_attr(xml2::xml_find_first(row, ".//td[@x]"), "x")
    if (length(variables) < 2L || is.na(estimate)) return(NULL)
    item <- if (correlation) {
      paste(sort(variables[1:2]), collapse = " <-> ")
    } else {
      paste0(variables[[2L]], " -> ", variables[[1L]])
    }
    data.frame(Section = section, Item = item, Value = as.numeric(estimate), stringsAsFactors = FALSE)
  })
  parsed <- Filter(Negate(is.null), parsed)
  if (!length(parsed)) stop("AMOS output table contained no numeric rows: ", node_caption, call. = FALSE)
  do.call(rbind, parsed)
}

amos_srmr_from_standardized_solution <- function(standardized_loadings, latent_correlations, data) {
  indicators <- paste0("x", seq_len(9L))
  factor_of <- c(rep("visual", 3L), rep("textual", 3L), rep("speed", 3L))
  names(factor_of) <- indicators
  loading <- setNames(standardized_loadings$Value, sub("^.* -> ", "", standardized_loadings$Item))
  if (!all(indicators %in% names(loading))) stop("AMOS standardized loadings were incomplete.", call. = FALSE)
  factor_correlation <- diag(3L)
  dimnames(factor_correlation) <- list(c("visual", "textual", "speed"), c("visual", "textual", "speed"))
  for (index in seq_len(nrow(latent_correlations))) {
    pair <- strsplit(latent_correlations$Item[[index]], " <-> ", fixed = TRUE)[[1L]]
    factor_correlation[pair[[1L]], pair[[2L]]] <- latent_correlations$Value[[index]]
    factor_correlation[pair[[2L]], pair[[1L]]] <- latent_correlations$Value[[index]]
  }
  implied <- diag(length(indicators))
  dimnames(implied) <- list(indicators, indicators)
  for (i in seq_along(indicators)) for (j in seq_along(indicators)) {
    if (i == j) next
    implied[i, j] <- loading[[indicators[[i]]]] * loading[[indicators[[j]]]] *
      factor_correlation[factor_of[[indicators[[i]]]], factor_of[[indicators[[j]]]]]
  }
  observed <- stats::cor(data[, indicators, drop = FALSE])
  residual <- observed - implied
  sqrt(sum(residual[lower.tri(residual, diag = TRUE)]^2) / sum(lower.tri(residual, diag = TRUE)))
}

import_amos_cfa_output <- function(input_path, data_path) {
  stopifnot(requireNamespace("xml2", quietly = TRUE))
  if (!file.exists(input_path)) stop("AMOS output file was not found: ", input_path, call. = FALSE)
  if (!file.exists(data_path)) stop("Benchmark data file was not found: ", data_path, call. = FALSE)
  document <- xml2::read_html(input_path, encoding = "UTF-8")
  unstandardized <- amos_output_path_rows(
    document, "Regression Weights:", "loading_unstandardized"
  )
  standardized <- amos_output_path_rows(
    document, "Standardized Regression Weights:", "loading_standardized"
  )
  correlations <- amos_output_path_rows(
    document, "Correlations:", "latent_correlation", correlation = TRUE
  )
  benchmark_data <- utils::read.csv(data_path, check.names = FALSE, stringsAsFactors = FALSE)
  srmr <- amos_srmr_from_standardized_solution(standardized, correlations, benchmark_data)
  fit <- data.frame(
    Section = "fit",
    Item = c(
      "Chi-square", "Degrees of freedom", "Chi-square p", "CFI", "TLI", "RMSEA",
      "RMSEA 90% CI lower", "RMSEA 90% CI upper", "SRMR"
    ),
    Value = c(
      amos_output_numeric(document, "7801"), amos_output_numeric(document, "7802", FALSE),
      amos_output_numeric(document, "7803"), amos_output_numeric(document, "7816"),
      amos_output_numeric(document, "7815"), amos_output_numeric(document, "7829"),
      amos_output_numeric(document, "7830"), amos_output_numeric(document, "7831"), srmr
    ),
    stringsAsFactors = FALSE
  )
  results <- rbind(fit, unstandardized, standardized, correlations)
  results[order(results$Section, results$Item), , drop = FALSE]
}

if (sys.nframe() == 0L) {
  arguments <- amos_output_arguments(commandArgs(trailingOnly = TRUE))
  results <- import_amos_cfa_output(arguments$input, arguments$data)
  if (nzchar(arguments$output)) {
    dir.create(dirname(arguments$output), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(results, arguments$output, row.names = FALSE, na = "")
  }
  options(digits = 17)
  print(results, row.names = FALSE)
  cat("Imported ", nrow(results), " full-precision values from AMOS CFA output.\n", sep = "")
}
