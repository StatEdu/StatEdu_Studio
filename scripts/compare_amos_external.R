amos_compare_arguments <- function(arguments) {
  values <- list(
    statedu = "", amos = "", report = "",
    absolute_tolerance = NA_real_, relative_tolerance = 1e-6,
    reported_decimal_places = 3L
  )
  for (argument in arguments) {
    pair <- strsplit(sub("^--", "", argument), "=", fixed = TRUE)[[1L]]
    if (length(pair) != 2L) next
    name <- gsub("-", "_", pair[[1L]], fixed = TRUE)
    values[[name]] <- pair[[2L]]
  }
  values$reported_decimal_places <- as.integer(values$reported_decimal_places)
  values$relative_tolerance <- as.numeric(values$relative_tolerance)
  values$absolute_tolerance <- as.numeric(values$absolute_tolerance)
  if (!is.finite(values$absolute_tolerance)) {
    values$absolute_tolerance <- 0.5 * 10^(-values$reported_decimal_places)
  }
  values
}

read_amos_comparison_values <- function(path, label) {
  if (!nzchar(path) || !file.exists(path)) stop(label, " CSV was not found: ", path, call. = FALSE)
  values <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  required <- c("Section", "Item", "Value")
  if (!all(required %in% names(values))) {
    stop(label, " CSV must contain Section, Item, and Value columns.", call. = FALSE)
  }
  values <- values[, required, drop = FALSE]
  values$Section <- trimws(as.character(values$Section))
  values$Item <- trimws(as.character(values$Item))
  values$Value <- suppressWarnings(as.numeric(values$Value))
  values$Key <- paste(values$Section, values$Item, sep = "::")
  if (any(!nzchar(values$Section)) || any(!nzchar(values$Item)) || anyDuplicated(values$Key)) {
    stop(label, " CSV contains blank or duplicated Section/Item keys.", call. = FALSE)
  }
  if (any(!is.finite(values$Value))) stop(label, " CSV contains blank or non-numeric values.", call. = FALSE)
  values
}

compare_amos_external <- function(statedu_path, amos_path, absolute_tolerance, relative_tolerance) {
  statedu <- read_amos_comparison_values(statedu_path, "StatEdu")
  amos <- read_amos_comparison_values(amos_path, "AMOS")
  if (!setequal(statedu$Key, amos$Key)) {
    missing_amos <- setdiff(statedu$Key, amos$Key)
    extra_amos <- setdiff(amos$Key, statedu$Key)
    stop(
      "StatEdu and AMOS comparison keys differ. Missing in AMOS: ",
      paste(missing_amos, collapse = ", "), "; extra in AMOS: ", paste(extra_amos, collapse = ", "),
      call. = FALSE
    )
  }
  amos <- amos[match(statedu$Key, amos$Key), , drop = FALSE]
  absolute_error <- abs(statedu$Value - amos$Value)
  scale <- pmax(abs(statedu$Value), abs(amos$Value), .Machine$double.eps)
  relative_error <- absolute_error / scale
  passed <- absolute_error <= absolute_tolerance | relative_error <= relative_tolerance
  data.frame(
    Section = statedu$Section, Item = statedu$Item,
    StatEdu = statedu$Value, AMOS = amos$Value,
    AbsoluteError = absolute_error, RelativeError = relative_error,
    AbsoluteTolerance = absolute_tolerance, RelativeTolerance = relative_tolerance,
    Status = ifelse(passed, "PASS", "FAIL"), stringsAsFactors = FALSE
  )
}

if (sys.nframe() == 0L) {
  arguments <- amos_compare_arguments(commandArgs(trailingOnly = TRUE))
  comparison <- compare_amos_external(
    arguments$statedu, arguments$amos,
    arguments$absolute_tolerance, arguments$relative_tolerance
  )
  if (nzchar(arguments$report)) {
    dir.create(dirname(arguments$report), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(comparison, arguments$report, row.names = FALSE, na = "")
  }
  print(comparison, row.names = FALSE)
  if (any(comparison$Status != "PASS")) {
    stop("One or more StatEdu versus AMOS values exceeded the comparison tolerance.", call. = FALSE)
  }
  cat("StatEdu versus AMOS comparison passed for ", nrow(comparison), " values.\n", sep = "")
}
