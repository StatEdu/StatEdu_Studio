coefficient_display_table <- function(result) {
  coef_table <- result$coef_table
  if (!is.data.frame(coef_table) || nrow(coef_table) == 0) {
    return(coef_table)
  }

  use_hc3 <- isTRUE(result$use_hc3)
  use_bootstrap <- isTRUE(result$use_bootstrap)

  keep_columns <- function(table, columns) {
    table[, intersect(columns, names(table)), drop = FALSE]
  }

  if (!use_bootstrap) {
    if (use_hc3) {
      return(keep_columns(coef_table, c("Term", "B", "HC3 SE", "t", "p", "sr2", "f2", "Tolerance", "VIF")))
    }
    return(keep_columns(coef_table, c("Term", "B", "SE", "beta", "t", "p", "sr2", "f2", "Tolerance", "VIF")))
  }

  boot_table <- result$boot_table
  if (!is.data.frame(boot_table) || nrow(boot_table) == 0) {
    if (use_hc3) {
      return(keep_columns(coef_table, c("Term", "B", "HC3 SE", "sr2", "f2", "Tolerance", "VIF")))
    }
    return(keep_columns(coef_table, c("Term", "B", "sr2", "f2", "Tolerance", "VIF")))
  }

  boot_match <- match(coef_table$Term, boot_table$Term)

  if (use_hc3) {
    data.frame(
      Term = coef_table$Term,
      B = coef_table$B,
      `HC3 SE` = coef_table[["HC3 SE"]],
      LLCI = boot_table$Boot_LLCI[boot_match],
      ULCI = boot_table$Boot_ULCI[boot_match],
      `Boot p` = boot_table$Boot_p[boot_match],
      sr2 = coef_table$sr2,
      f2 = coef_table$f2,
      Tolerance = coef_table$Tolerance,
      VIF = coef_table$VIF,
      check.names = FALSE
    )
  } else {
    data.frame(
      Term = coef_table$Term,
      B = coef_table$B,
      `Boot SE` = boot_table$Boot_SE[boot_match],
      LLCI = boot_table$Boot_LLCI[boot_match],
      ULCI = boot_table$Boot_ULCI[boot_match],
      `Boot p` = boot_table$Boot_p[boot_match],
      sr2 = coef_table$sr2,
      f2 = coef_table$f2,
      Tolerance = coef_table$Tolerance,
      VIF = coef_table$VIF,
      check.names = FALSE
    )
  }
}

