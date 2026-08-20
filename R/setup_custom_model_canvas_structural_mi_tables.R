# Structural equation canvas modification-index result tables.

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
  table
}
