crosstab_gamma_reference <- function(tab) {
  concordant <- 0
  discordant <- 0
  for (i in seq_len(nrow(tab))) {
    for (j in seq_len(ncol(tab))) {
      n_ij <- tab[i, j]
      if (n_ij == 0) next
      lower_rows <- if (i < nrow(tab)) seq.int(i + 1, nrow(tab)) else integer(0)
      higher_cols <- if (j < ncol(tab)) seq.int(j + 1, ncol(tab)) else integer(0)
      lower_cols <- if (j > 1) seq_len(j - 1) else integer(0)
      if (length(lower_rows) > 0 && length(higher_cols) > 0) {
        concordant <- concordant + n_ij * sum(tab[lower_rows, higher_cols, drop = FALSE])
      }
      if (length(lower_rows) > 0 && length(lower_cols) > 0) {
        discordant <- discordant + n_ij * sum(tab[lower_rows, lower_cols, drop = FALSE])
      }
    }
  }
  denom <- concordant + discordant
  if (denom == 0) return(NA_real_)
  (concordant - discordant) / denom
}
