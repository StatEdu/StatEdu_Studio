structural_canvas_admissibility_reason_text <- function(reason, language) {
  tr <- function(en, ko) statedu_localized_text(language, en, ko)
  fixed <- c(nonconvergence = "수렴 실패", "lavaan post.check failure" = "lavaan 사후 점검 실패",
    "invalid degrees of freedom" = "유효하지 않은 자유도", "absolute latent correlation at least 1" = "잠재상관 절대값이 1 이상",
    "inadmissible trial fit" = "허용 불가능한 시험 적합")
  if (reason %in% names(fixed)) return(tr(reason, unname(fixed[[reason]])))
  prefixes <- c("negative residual variance: " = "음의 잔차분산: %s", "negative latent variance: " = "음의 잠재분산: %s",
    "non-positive-definite or boundary residual covariance matrix: " = "양의 정부호가 아니거나 경계에 있는 잔차 공분산행렬: %s",
    "non-positive-definite or boundary latent covariance matrix: " = "양의 정부호가 아니거나 경계에 있는 잠재 공분산행렬: %s")
  for (prefix in names(prefixes)) if (startsWith(reason, prefix))
    return(sprintf(tr(paste0(prefix, "%s"), unname(prefixes[[prefix]])), substring(reason, nchar(prefix) + 1L)))
  pattern <- "^non-positive-definite or unexplained boundary parameter covariance matrix \\(boundary dimensions = ([0-9]+); explicit equality constraints = ([0-9]+)\\)$"
  parts <- regmatches(reason, regexec(pattern, reason))[[1L]]
  if (length(parts)) return(sprintf(tr(
    "non-positive-definite or unexplained boundary parameter covariance matrix (boundary dimensions = %s; explicit equality constraints = %s)",
    "양의 정부호가 아니거나 설명되지 않은 경계의 모수 공분산행렬(경계 차원 = %s; 명시적 동일화 제약 = %s)"), parts[[2]], parts[[3]]))
  reason
}

structural_canvas_holdout_reason_text <- function(comparison, language) {
  legacy <- as.character(comparison$table[["Admissibility reasons"]])
  reasons <- comparison$admissibility_reasons
  # Old saved comparisons may only contain joined text, including user names.
  if (!is.list(reasons) || length(reasons) != length(legacy)) return(legacy)
  vapply(reasons, function(values) {
    if (!length(values)) return(statedu_localized_text(language, "None", "없음"))
    paste(vapply(values, structural_canvas_admissibility_reason_text, character(1), language = language), collapse = "; ")
  }, character(1), USE.NAMES = FALSE)
}

# Structural equation canvas modification-index result tables.

structural_canvas_mi_skipped_text <- function(mi, language) {
  legacy <- if ("skipped_details" %in% names(mi)) as.character(mi$skipped_details) else rep("", nrow(mi))
  legacy[is.na(legacy)] <- ""
  if (!"skipped_records" %in% names(mi)) return(legacy)
  tr <- function(en, ko) statedu_localized_text(language, en, ko)
  vapply(seq_len(nrow(mi)), function(i) {
    records <- mi$skipped_records[[i]]
    if (!length(records)) return(legacy[[i]])
    paste(vapply(records, function(record) {
      detail <- if (nzchar(record$error %||% "")) sprintf(tr("fit error: %s", "적합 오류: %s"), record$error)
        else paste(vapply(record$reasons, structural_canvas_admissibility_reason_text, character(1), language = language), collapse = "; ")
      paste0(record$path, " [", detail, "]")
    }, character(1)), collapse = " | ")
  }, character(1))
}

structural_canvas_mi_result_table <- function(bundle, snapshot, fit, ko, fmt, display_name, residual_name) {
  mi <- bundle$mi %||% structural_canvas_allowed_mi(snapshot, fit)
  if (!nrow(mi)) return(data.frame())
  theory_mi <- identical(bundle$mi_mode %||% "theory", "theory")
  if (!theory_mi) {
    mi <- mi[mi$op == "~~" & mi$lhs != mi$rhs, , drop = FALSE]
    if (!nrow(mi)) return(data.frame())
  }
  relation <- vapply(seq_len(nrow(mi)), function(index) {
    lhs <- if (identical(mi$op[[index]], "~~")) residual_name(mi$lhs[[index]]) else display_name(mi$lhs[[index]])
    rhs <- if (identical(mi$op[[index]], "~~")) residual_name(mi$rhs[[index]]) else display_name(mi$rhs[[index]])
    if (identical(mi$op[[index]], "~~")) paste(lhs, "<-->", rhs) else if (identical(mi$op[[index]], "=~")) paste(lhs, "=~", rhs) else paste(rhs, "-->", lhs)
  }, character(1))
  epc <- if ("epc" %in% names(mi)) mi$epc else rep(NA_real_, nrow(mi))
  standardized_epc <- if ("sepc.all" %in% names(mi)) mi$sepc.all else rep(NA_real_, nrow(mi))
  if (!theory_mi) {
    return(data.frame(
      Covariance = relation,
      MI = fmt(mi$mi),
      `MI p` = vapply(mi$`MI p`, format_p, character(1)),
      `BH-adjusted p` = vapply(mi$`BH-adjusted p`, format_p, character(1)),
      `MI tests` = mi$`Multiplicity family size`,
      EPC = fmt(epc),
      `Std. EPC` = fmt(standardized_epc),
      check.names = FALSE
    ))
  }
  step <- if ("step" %in% names(mi)) as.integer(mi$step) else seq_len(nrow(mi))
  skipped <- if ("skipped_inadmissible" %in% names(mi)) as.integer(mi$skipped_inadmissible) else rep(0L, nrow(mi))
  skipped_details <- if ("skipped_details" %in% names(mi)) as.character(mi$skipped_details) else rep("", nrow(mi))
  skipped_details[is.na(skipped_details)] <- ""
  table <- data.frame(
    Step = step,
    `Skipped unsafe` = skipped,
    Covariance = relation,
    MI = fmt(mi$mi),
    `MI p` = vapply(mi$`MI p`, format_p, character(1)),
    `BH-adjusted p` = vapply(mi$`BH-adjusted p`, format_p, character(1)),
    `MI tests` = mi$`Multiplicity family size`,
    EPC = fmt(epc),
    `Std. EPC` = fmt(standardized_epc),
    CFI = fmt(mi$cfi_after),
    TLI = fmt(mi$tli_after),
    RMSEA = fmt(mi$rmsea_after),
    SRMR = fmt(mi$srmr_after),
    check.names = FALSE
  )
  skipped_rows <- nzchar(trimws(skipped_details))
  skipped_table <- data.frame(
    Step = step[skipped_rows],
    `Skipped details` = skipped_details[skipped_rows],
    check.names = FALSE
  )
  attr(table, "skipped_details") <- skipped_table
  attr(table, "skipped_source") <- mi
  table
}
