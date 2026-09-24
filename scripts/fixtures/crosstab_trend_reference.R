crosstab_trend_reference <- function(tab, row_measure = "", col_measure = "") {
  if (nrow(tab) == 2 && ncol(tab) >= 2) {
    event_row <- crosstab_binary_row_index(tab)
    test <- crosstab_prop_trend_test(as.numeric(tab[event_row, ]), colSums(tab), score = seq_len(ncol(tab)))
    return(list(method = "Cochran-Armitage trend test", detail = "cochran_armitage", statistic = unname(test$statistic), df = unname(test$parameter), p = test$p.value, odds_ratio = crosstab_trend_or(tab), gamma = NA_real_))
  }
  if (ncol(tab) == 2 && nrow(tab) >= 2) {
    event_col <- 2L
    test <- crosstab_prop_trend_test(as.numeric(tab[, event_col]), rowSums(tab), score = seq_len(nrow(tab)))
    return(list(method = "Cochran-Armitage trend test", detail = "cochran_armitage", statistic = unname(test$statistic), df = unname(test$parameter), p = test$p.value, odds_ratio = crosstab_trend_or(tab), gamma = NA_real_))
  }
  if (identical(row_measure, "ordered") && identical(col_measure, "ordered")) {
    row_scores <- seq_len(nrow(tab))
    col_scores <- seq_len(ncol(tab))
    expanded <- as.data.frame(as.table(tab), stringsAsFactors = FALSE)
    expanded$row_score <- row_scores[match(expanded$Var1, rownames(tab))]
    expanded$col_score <- col_scores[match(expanded$Var2, colnames(tab))]
    values <- expanded[rep(seq_len(nrow(expanded)), expanded$Freq), c("row_score", "col_score"), drop = FALSE]
    if (nrow(values) < 3) {
      return(list(method = "Score-based ordered-by-ordered trend association", detail = "ordered_score", statistic = NA_real_, df = 1, p = NA_real_, odds_ratio = NA_real_, gamma = crosstab_gamma(tab)))
    }
    r <- suppressWarnings(stats::cor(values$row_score, values$col_score))
    statistic <- (nrow(values) - 1) * r^2
    p <- stats::pchisq(statistic, df = 1, lower.tail = FALSE)
    return(list(method = "Score-based ordered-by-ordered trend association", detail = "ordered_score", statistic = statistic, df = 1, p = p, odds_ratio = NA_real_, gamma = crosstab_gamma(tab)))
  }
  NULL
}
