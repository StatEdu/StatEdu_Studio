# Sample size and power analysis helpers.

sample_size_method_labels <- function() {
  c(
    ttest = "t-test",
    anova = "ANOVA",
    ancova = "ANCOVA / MANOVA",
    gee = "GEE",
    lmm = "LMM",
    nonparametric = "Nonparametric",
    proportion = "Proportion",
    chisquare = "Chi-square",
    mcnemar = "McNemar",
    regression = "Regression",
    survival = "Survival / Cox",
    correlation = "Correlation",
    equivalence = "Equivalence / NI",
    diagnostic = "ROC AUC",
    rates = "Count / Rate Regression",
    cluster = "Cluster Trial",
    precision = "Precision / CI",
    reliability = "Reliability / Agreement",
    sem = "SEM / CFA"
  )
}

effect_size_method_labels <- function() {
  labels <- sample_size_method_labels()
  labels[setdiff(names(labels), c("equivalence", "cluster", "precision", "reliability", "sem"))]
}

sample_size_ttest_effect_conversions <- function(d, df = NULL) {
  d <- as.numeric(d)
  f_squared <- d^2 / 4
  out <- list(
    effect_size_d = d,
    point_biserial_r = d / sqrt(d^2 + 4),
    cohen_f = abs(d) / 2,
    f_squared = f_squared,
    eta_squared = f_squared / (1 + f_squared)
  )
  if (!is.null(df) && is.finite(df) && df > 0) {
    out$hedges_g <- d * (1 - 3 / (4 * df - 1))
  }
  out
}

sample_size_effect_size <- function(
  design = "independent_means",
  mean1 = NULL,
  mean2 = NULL,
  sd1 = NULL,
  sd2 = NULL,
  n1 = NULL,
  n2 = NULL,
  mean_difference = NULL,
  sd_difference = NULL,
  null_mean = 0,
  t_value = NULL,
  df = NULL,
  n = NULL,
  r = NULL
) {
  refs <- c(
    "Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences (2nd ed.). Lawrence Erlbaum.",
    "Rosenthal, R. (1994). Parametric measures of effect size. In H. Cooper & L. V. Hedges (Eds.), The Handbook of Research Synthesis. Russell Sage Foundation.",
    "Lakens, D. (2013). Calculating and reporting effect sizes to facilitate cumulative science: a practical primer for t-tests and ANOVAs. Frontiers in Psychology, 4, 863."
  )
  if (identical(design, "independent_t_n")) {
    sample_size_validate_positive(abs(t_value), "t statistic")
    sample_size_validate_positive(n1, "Group 1 n")
    sample_size_validate_positive(n2, "Group 2 n")
    df_value <- n1 + n2 - 2
    d <- t_value * sqrt(1 / n1 + 1 / n2)
    conversions <- sample_size_ttest_effect_conversions(d, df_value)
    return(c(list(
      result_type = "effect_size",
      design_label = "Independent t-test: d from t and group n",
      effect_size_label = "Cohen's d",
      primary_effect_size = d,
      primary_effect_size_label = "Cohen's d",
      t_value = t_value,
      degrees_freedom = df_value,
      group1_n = n1,
      group2_n = n2,
      method_note = "For an independent two-sample t-test, Cohen's d = t * sqrt(1/n1 + 1/n2). Hedges' g applies the small-sample correction.",
      formula = "d = t * sqrt(1/n1 + 1/n2); g = d * [1 - 3 / (4df - 1)].",
      references = refs
    ), conversions))
  }
  if (identical(design, "independent_t_df_equal")) {
    sample_size_validate_positive(abs(t_value), "t statistic")
    sample_size_validate_positive(df, "Degrees of freedom")
    d <- 2 * t_value / sqrt(df + 2)
    conversions <- sample_size_ttest_effect_conversions(d, df)
    return(c(list(
      result_type = "effect_size",
      design_label = "Independent t-test: d from t and df (equal n)",
      effect_size_label = "Cohen's d",
      primary_effect_size = d,
      primary_effect_size_label = "Cohen's d",
      t_value = t_value,
      degrees_freedom = df,
      method_note = "For equal-sized independent groups, Cohen's d = 2t / sqrt(df + 2). Use the group-n option when n1 and n2 differ.",
      formula = "d = 2t / sqrt(df + 2).",
      references = refs
    ), conversions))
  }
  if (identical(design, "one_sample_t")) {
    sample_size_validate_positive(abs(t_value), "t statistic")
    sample_size_validate_positive(n, "Sample size")
    d <- t_value / sqrt(n)
    conversions <- sample_size_ttest_effect_conversions(d, n - 1)
    return(c(list(
      result_type = "effect_size",
      design_label = "One-sample t-test: d from t and n",
      effect_size_label = "Cohen's d",
      primary_effect_size = d,
      primary_effect_size_label = "Cohen's d",
      t_value = t_value,
      sample_size = n,
      degrees_freedom = n - 1,
      method_note = "For a one-sample t-test, Cohen's d = t / sqrt(n).",
      formula = "d = t / sqrt(n).",
      references = refs
    ), conversions))
  }
  if (identical(design, "paired_t")) {
    sample_size_validate_positive(abs(t_value), "t statistic")
    sample_size_validate_positive(n, "Number of pairs")
    d <- t_value / sqrt(n)
    conversions <- sample_size_ttest_effect_conversions(d, n - 1)
    return(c(list(
      result_type = "effect_size",
      design_label = "Paired t-test: dz from t and pairs",
      effect_size_label = "Cohen's dz",
      primary_effect_size = d,
      primary_effect_size_label = "Cohen's dz",
      t_value = t_value,
      sample_size = n,
      degrees_freedom = n - 1,
      method_note = "For a paired t-test, Cohen's dz = t / sqrt(number of pairs).",
      formula = "dz = t / sqrt(n pairs).",
      references = refs
    ), conversions))
  }
  if (identical(design, "independent_r")) {
    sample_size_validate_probability(abs(r), "Point-biserial r", lower = 0, upper = 1)
    d <- 2 * r / sqrt(1 - r^2)
    conversions <- sample_size_ttest_effect_conversions(d)
    return(c(list(
      result_type = "effect_size",
      design_label = "Independent t-test: d from point-biserial r",
      effect_size_label = "Cohen's d",
      primary_effect_size = d,
      primary_effect_size_label = "Cohen's d",
      correlation_r = r,
      method_note = "For a two-group contrast, Cohen's d can be converted from point-biserial r as d = 2r / sqrt(1 - r^2).",
      formula = "d = 2r / sqrt(1 - r^2).",
      references = refs
    ), conversions))
  }
  if (identical(design, "paired_means")) {
    sample_size_validate_positive(abs(mean_difference), "Mean difference")
    sample_size_validate_positive(sd_difference, "SD of paired differences")
    d <- mean_difference / sd_difference
    conversions <- sample_size_ttest_effect_conversions(d)
    return(c(list(
      result_type = "effect_size",
      design_label = "Paired means",
      effect_size_label = "Cohen's dz",
      primary_effect_size = d,
      primary_effect_size_label = "Cohen's dz",
      method_note = "Cohen's dz = mean paired difference / SD of paired differences.",
      formula = "dz = mean paired difference / SD of paired differences.",
      references = refs
    ), conversions))
  }

  primary_hedges <- identical(design, "hedges_g")
  sample_size_validate_positive(abs(mean1 - mean2), "Mean difference")
  if (identical(design, "one_sample_mean")) {
    sample_size_validate_positive(sd1, "SD")
    d <- (mean1 - null_mean) / sd1
    conversions <- sample_size_ttest_effect_conversions(d)
    return(c(list(
      result_type = "effect_size",
      design_label = "One-sample mean",
      effect_size_label = "Cohen's d",
      primary_effect_size = d,
      primary_effect_size_label = "Cohen's d",
      method_note = "Cohen's d = (sample mean - null mean) / SD.",
      formula = "d = (sample mean - null mean) / SD.",
      references = refs
    ), conversions))
  }

  sample_size_validate_positive(sd1, "Group 1 SD")
  sample_size_validate_positive(sd2, "Group 2 SD")
  n1 <- as.integer(n1)
  n2 <- as.integer(n2)
  if (!is.finite(n1) || n1 < 2 || !is.finite(n2) || n2 < 2) {
    stop("Both group sample sizes must be at least 2.", call. = FALSE)
  }
  df <- n1 + n2 - 2
  pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / df)
  d <- (mean1 - mean2) / pooled_sd
  conversions <- sample_size_ttest_effect_conversions(d, df)
  c(list(
    result_type = "effect_size",
    design_label = if (primary_hedges) "Independent means: Hedges' g" else "Independent means: Cohen's d",
    effect_size_label = if (primary_hedges) "Cohen's d" else "Cohen's d",
    primary_effect_size = if (primary_hedges) conversions$hedges_g else d,
    primary_effect_size_label = if (primary_hedges) "Hedges' g" else "Cohen's d",
    pooled_sd = pooled_sd,
    method_note = "Cohen's d uses the pooled SD; Hedges' g applies the small-sample correction J = 1 - 3 / (4df - 1).",
    formula = "pooled SD = sqrt(((n1 - 1)SD1^2 + (n2 - 1)SD2^2) / (n1 + n2 - 2)); d = (M1 - M2) / pooled SD.",
    references = refs
  ), conversions)
}

sample_size_effect_size_proportion <- function(
  design = "cohens_h",
  p1 = NULL,
  p2 = NULL,
  event1 = NULL,
  nonevent1 = NULL,
  event2 = NULL,
  nonevent2 = NULL
) {
  if (identical(design, "odds_ratio_table")) {
    counts <- c(event1, nonevent1, event2, nonevent2)
    if (any(!is.finite(counts)) || any(counts < 0)) {
      stop("All 2x2 table counts must be greater than or equal to 0.", call. = FALSE)
    }
    corrected <- if (any(counts == 0)) counts + 0.5 else counts
    event1 <- corrected[[1]]
    nonevent1 <- corrected[[2]]
    event2 <- corrected[[3]]
    nonevent2 <- corrected[[4]]
    p1 <- event1 / (event1 + nonevent1)
    p2 <- event2 / (event2 + nonevent2)
  } else {
    sample_size_validate_probability(p1, "Proportion 1")
    sample_size_validate_probability(p2, "Proportion 2")
  }

  risk_difference <- p1 - p2
  risk_ratio <- p1 / p2
  odds1 <- p1 / (1 - p1)
  odds2 <- p2 / (1 - p2)
  odds_ratio <- odds1 / odds2
  cohens_h <- 2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2))
  design_label <- switch(
    design,
    risk_difference = "Risk difference",
    risk_ratio = "Risk ratio",
    odds_ratio = "Odds ratio from proportions",
    odds_ratio_table = "Odds ratio from 2x2 table",
    "Cohen's h"
  )
  primary_value <- switch(
    design,
    risk_difference = risk_difference,
    risk_ratio = risk_ratio,
    odds_ratio = odds_ratio,
    odds_ratio_table = odds_ratio,
    cohens_h
  )
  primary_label <- switch(
    design,
    risk_difference = "Risk difference",
    risk_ratio = "Risk ratio",
    odds_ratio = "Odds ratio",
    odds_ratio_table = "Odds ratio",
    "Cohen's h"
  )

  list(
    result_type = "effect_size",
    design_label = design_label,
    primary_effect_size = primary_value,
    primary_effect_size_label = primary_label,
    proportion1 = p1,
    proportion2 = p2,
    cohens_h = cohens_h,
    risk_difference = risk_difference,
    risk_ratio = risk_ratio,
    odds_ratio = odds_ratio,
    log_odds_ratio = log(odds_ratio),
    method_note = "Proportion effect sizes are calculated from two proportions; Cohen's h uses the arcsine-square-root transformation."
  )
}

sample_size_effect_size_chisquare <- function(
  design = "cohens_w",
  chi_square = NULL,
  n = NULL,
  rows = NULL,
  columns = NULL,
  observed = NULL,
  expected = NULL
) {
  if (identical(design, "cohens_w_from_probs")) {
    observed <- as.numeric(strsplit(observed %||% "", "\\s*,\\s*")[[1]])
    expected <- as.numeric(strsplit(expected %||% "", "\\s*,\\s*")[[1]])
    if (length(observed) != length(expected) || length(observed) < 2) {
      stop("Observed and expected proportions must have the same length and at least 2 categories.", call. = FALSE)
    }
    if (any(!is.finite(observed)) || any(!is.finite(expected)) || any(observed < 0) || any(expected <= 0)) {
      stop("Observed proportions must be nonnegative and expected proportions must be positive.", call. = FALSE)
    }
    observed <- observed / sum(observed)
    expected <- expected / sum(expected)
    w <- sqrt(sum((observed - expected)^2 / expected))
    return(list(
      result_type = "effect_size",
      design_label = "Cohen's w from category proportions",
      primary_effect_size = w,
      primary_effect_size_label = "Cohen's w",
      cohens_w = w,
      categories = length(observed),
      method_note = "Cohen's w is calculated from observed and expected category proportions."
    ))
  }

  sample_size_validate_positive(chi_square, "Chi-square statistic")
  sample_size_validate_positive(n, "Sample size")
  phi <- sqrt(chi_square / n)
  if (identical(design, "phi")) {
    return(list(
      result_type = "effect_size",
      design_label = "Phi coefficient",
      primary_effect_size = phi,
      primary_effect_size_label = "Phi",
      phi = phi,
      cohens_w = phi,
      method_note = "Phi = sqrt(chi-square / N); for a 2x2 table, phi equals Cohen's w."
    ))
  }

  rows <- as.integer(rows)
  columns <- as.integer(columns)
  if (!is.finite(rows) || rows < 2 || !is.finite(columns) || columns < 2) {
    stop("Rows and columns must both be at least 2.", call. = FALSE)
  }
  min_dimension <- min(rows - 1, columns - 1)
  cramer_v <- sqrt(chi_square / (n * min_dimension))
  if (identical(design, "cramers_v")) {
    return(list(
      result_type = "effect_size",
      design_label = "Cramer's V",
      primary_effect_size = cramer_v,
      primary_effect_size_label = "Cramer's V",
      cramer_v = cramer_v,
      phi = phi,
      method_note = "Cramer's V = sqrt(chi-square / [N * min(r - 1, c - 1)])."
    ))
  }

  list(
    result_type = "effect_size",
    design_label = "Cohen's w from chi-square",
    primary_effect_size = phi,
    primary_effect_size_label = "Cohen's w",
    cohens_w = phi,
    phi = phi,
    cramer_v = cramer_v,
    method_note = "Cohen's w = sqrt(chi-square / N) for goodness-of-fit or contingency chi-square effect-size planning."
  )
}

sample_size_effect_size_correlation <- function(
  design = "r_from_t",
  r = NULL,
  r1 = NULL,
  r2 = NULL,
  t_value = NULL,
  f_value = NULL,
  df = NULL,
  r_squared = NULL
) {
  if (identical(design, "point_biserial")) {
    sample_size_validate_probability(abs(r), "Point-biserial r", lower = 0, upper = 1)
    if (abs(r) >= 1) stop("Point-biserial r must be less than 1 in absolute value.", call. = FALSE)
    d <- 2 * r / sqrt(1 - r^2)
    conversions <- sample_size_ttest_effect_conversions(d)
    return(c(list(
      result_type = "effect_size",
      design_label = "Point-biserial correlation",
      primary_effect_size = r,
      primary_effect_size_label = "Point-biserial r",
      correlation_r = r,
      r_squared = r^2,
      fisher_z = atanh(r),
      method_note = "For a two-group contrast, point-biserial r can be converted to Cohen's d as d = 2r / sqrt(1 - r^2)."
    ), conversions))
  }

  if (identical(design, "fisher_z")) {
    if (!is.finite(r) || abs(r) >= 1) stop("Correlation r must be finite and less than 1 in absolute value.", call. = FALSE)
    z <- atanh(r)
    return(list(
      result_type = "effect_size",
      design_label = "Fisher's z from correlation",
      primary_effect_size = z,
      primary_effect_size_label = "Fisher's z",
      correlation_r = r,
      fisher_z = z,
      r_squared = r^2,
      method_note = "Fisher's z = atanh(r)."
    ))
  }

  if (identical(design, "cohens_q")) {
    if (!is.finite(r1) || abs(r1) >= 1 || !is.finite(r2) || abs(r2) >= 1) {
      stop("Both correlations must be finite and less than 1 in absolute value.", call. = FALSE)
    }
    q <- atanh(r1) - atanh(r2)
    return(list(
      result_type = "effect_size",
      design_label = "Cohen's q for two correlations",
      primary_effect_size = q,
      primary_effect_size_label = "Cohen's q",
      correlation_r = r1,
      comparison_r = r2,
      cohens_q = q,
      method_note = "Cohen's q is the difference between two Fisher-z transformed correlations."
    ))
  }

  if (identical(design, "r_from_r2")) {
    sample_size_validate_probability(r_squared, "R-squared", lower = 0, upper = 1)
    r_value <- sqrt(r_squared)
    return(list(
      result_type = "effect_size",
      design_label = "Correlation from R-squared",
      primary_effect_size = r_value,
      primary_effect_size_label = "Pearson r",
      correlation_r = r_value,
      r_squared = r_squared,
      fisher_z = atanh(r_value),
      method_note = "For a single predictor, r = sqrt(R-squared); the sign is not identifiable from R-squared alone."
    ))
  }

  sample_size_validate_positive(df, "Degrees of freedom")
  if (identical(design, "r_from_f")) {
    sample_size_validate_positive(f_value, "F statistic")
    r_value <- sqrt(f_value / (f_value + df))
    return(list(
      result_type = "effect_size",
      design_label = "Correlation from F statistic",
      primary_effect_size = r_value,
      primary_effect_size_label = "Pearson r",
      correlation_r = r_value,
      r_squared = r_value^2,
      fisher_z = atanh(r_value),
      method_note = "For a one-degree-of-freedom effect, r = sqrt(F / [F + df_error])."
    ))
  }

  if (!is.finite(t_value) || isTRUE(all.equal(t_value, 0))) {
    stop("t statistic must be finite and different from 0.", call. = FALSE)
  }
  r_value <- sign(t_value) * sqrt(t_value^2 / (t_value^2 + df))
  list(
    result_type = "effect_size",
    design_label = "Correlation from t statistic",
    primary_effect_size = r_value,
    primary_effect_size_label = "Pearson r",
    correlation_r = r_value,
    r_squared = r_value^2,
    fisher_z = atanh(r_value),
    method_note = "r = sign(t) * sqrt(t^2 / [t^2 + df])."
  )
}

sample_size_effect_size_anova <- function(
  design = "f_from_eta2",
  eta_squared = NULL,
  partial_eta_squared = NULL,
  f_value = NULL,
  df_effect = NULL,
  df_error = NULL,
  groups = NULL,
  total_n = NULL
) {
  if (identical(design, "f_from_eta2")) {
    sample_size_validate_probability(eta_squared, "Eta squared", lower = 0, upper = 1)
    f <- sqrt(eta_squared / (1 - eta_squared))
    return(list(
      result_type = "effect_size",
      design_label = "Cohen's f from eta squared",
      primary_effect_size = f,
      primary_effect_size_label = "Cohen's f",
      cohen_f = f,
      eta_squared = eta_squared,
      method_note = "Cohen's f = sqrt(eta-squared / [1 - eta-squared])."
    ))
  }

  if (identical(design, "f_from_partial_eta2")) {
    sample_size_validate_probability(partial_eta_squared, "Partial eta squared", lower = 0, upper = 1)
    f <- sqrt(partial_eta_squared / (1 - partial_eta_squared))
    return(list(
      result_type = "effect_size",
      design_label = "Cohen's f from partial eta squared",
      primary_effect_size = f,
      primary_effect_size_label = "Cohen's f",
      cohen_f = f,
      partial_eta_squared = partial_eta_squared,
      method_note = "Cohen's f = sqrt(partial eta-squared / [1 - partial eta-squared])."
    ))
  }

  sample_size_validate_positive(f_value, "F statistic")
  if (!is.null(groups) || !is.null(total_n)) {
    sample_size_validate_positive(groups, "Number of groups")
    sample_size_validate_positive(total_n, "Total sample size")
    if (groups < 2) stop("Number of groups must be at least 2.", call. = FALSE)
    if (total_n <= groups) stop("Total sample size must be greater than the number of groups.", call. = FALSE)
    df_effect <- groups - 1
    df_error <- total_n - groups
  } else {
    sample_size_validate_positive(df_effect, "Effect degrees of freedom")
    sample_size_validate_positive(df_error, "Error degrees of freedom")
  }
  partial_eta <- (f_value * df_effect) / (f_value * df_effect + df_error)
  cohen_f <- sqrt(partial_eta / (1 - partial_eta))
  partial_omega <- max(0, (f_value * df_effect - df_effect) / (f_value * df_effect + df_error + 1))

  if (identical(design, "omega_from_f")) {
    return(list(
      result_type = "effect_size",
      design_label = "Partial omega squared from F",
      primary_effect_size = partial_omega,
      primary_effect_size_label = "Partial omega squared",
      omega_squared = partial_omega,
      partial_eta_squared = partial_eta,
      cohen_f = cohen_f,
      method_note = "Partial omega squared is approximated from F, number of groups, and total sample size."
    ))
  }

  list(
    result_type = "effect_size",
    design_label = "Partial eta squared from F",
    primary_effect_size = partial_eta,
    primary_effect_size_label = "Partial eta squared",
    partial_eta_squared = partial_eta,
    cohen_f = cohen_f,
    omega_squared = partial_omega,
    method_note = "For one-way ANOVA, df_effect = groups - 1 and df_error = total N - groups; partial eta squared = F * df_effect / (F * df_effect + df_error)."
  )
}

sample_size_effect_size_ancova <- function(
  design = "ancova_adjusted_f",
  effect_size_f = NULL,
  covariate_r2 = NULL,
  partial_eta_squared = NULL,
  pillai_trace = NULL,
  wilks_lambda = NULL,
  dependent_variables = NULL,
  f_value = NULL,
  df_effect = NULL,
  df_error = NULL,
  groups = NULL,
  total_n = NULL
) {
  if (identical(design, "ancova_adjusted_f")) {
    sample_size_validate_positive(effect_size_f, "Unadjusted Cohen's f")
    if (!is.finite(covariate_r2) || covariate_r2 < 0 || covariate_r2 >= 1) {
      stop("Covariate R-squared must be at least 0 and less than 1.", call. = FALSE)
    }
    adjusted_f <- effect_size_f / sqrt(1 - covariate_r2)
    return(list(
      result_type = "effect_size",
      design_label = "ANCOVA adjusted Cohen's f",
      primary_effect_size = adjusted_f,
      primary_effect_size_label = "Adjusted Cohen's f",
      cohen_f = adjusted_f,
      unadjusted_cohen_f = effect_size_f,
      covariate_r2 = covariate_r2,
      method_note = "ANCOVA adjusted f = unadjusted f / sqrt(1 - covariate R-squared)."
    ))
  }

  if (identical(design, "ancova_f_from_partial_eta2")) {
    sample_size_validate_probability(partial_eta_squared, "Partial eta squared", lower = 0, upper = 1)
    cohen_f <- sqrt(partial_eta_squared / (1 - partial_eta_squared))
    return(list(
      result_type = "effect_size",
      design_label = "ANCOVA Cohen's f from partial eta squared",
      primary_effect_size = cohen_f,
      primary_effect_size_label = "Cohen's f",
      cohen_f = cohen_f,
      partial_eta_squared = partial_eta_squared,
      method_note = "Cohen's f = sqrt(partial eta-squared / [1 - partial eta-squared])."
    ))
  }

  if (identical(design, "manova_pillai")) {
    sample_size_validate_probability(pillai_trace, "Pillai's trace V", lower = 0, upper = 1)
    f_squared <- pillai_trace / (1 - pillai_trace)
    cohen_f <- sqrt(f_squared)
    return(list(
      result_type = "effect_size",
      design_label = "MANOVA effect size from Pillai's trace",
      primary_effect_size = pillai_trace,
      primary_effect_size_label = "Pillai's trace V",
      pillai_trace = pillai_trace,
      f_squared = f_squared,
      cohen_f = cohen_f,
      method_note = "For MANOVA planning, Pillai's trace is transformed to f2 = V / (1 - V)."
    ))
  }

  if (identical(design, "manova_wilks")) {
    sample_size_validate_probability(wilks_lambda, "Wilks' lambda", lower = 0, upper = 1)
    sample_size_validate_positive(dependent_variables, "Number of dependent variables")
    sample_size_validate_positive(groups, "Number of groups")
    if (groups < 2) stop("Number of groups must be at least 2.", call. = FALSE)
    s <- min(dependent_variables, groups - 1)
    multivariate_eta <- 1 - wilks_lambda^(1 / s)
    f_squared <- multivariate_eta / (1 - multivariate_eta)
    cohen_f <- sqrt(f_squared)
    return(list(
      result_type = "effect_size",
      design_label = "MANOVA effect size from Wilks' lambda",
      primary_effect_size = wilks_lambda,
      primary_effect_size_label = "Wilks' lambda",
      wilks_lambda = wilks_lambda,
      dependent_variables = dependent_variables,
      groups = groups,
      multivariate_eta_squared = multivariate_eta,
      f_squared = f_squared,
      cohen_f = cohen_f,
      method_note = "Wilks' lambda is transformed with s = min(number of dependent variables, groups - 1): eta2 = 1 - lambda^(1/s), f2 = eta2 / (1 - eta2)."
    ))
  }

  sample_size_validate_positive(f_value, "F statistic")
  if (!is.null(groups) || !is.null(total_n)) {
    sample_size_validate_positive(groups, "Number of groups")
    sample_size_validate_positive(total_n, "Total sample size")
    if (groups < 2) stop("Number of groups must be at least 2.", call. = FALSE)
    if (total_n <= groups) stop("Total sample size must be greater than the number of groups.", call. = FALSE)
    df_effect <- groups - 1
    df_error <- total_n - groups
  } else {
    sample_size_validate_positive(df_effect, "Effect degrees of freedom")
    sample_size_validate_positive(df_error, "Error degrees of freedom")
  }
  partial_eta <- (f_value * df_effect) / (f_value * df_effect + df_error)
  cohen_f <- sqrt(partial_eta / (1 - partial_eta))
  list(
    result_type = "effect_size",
    design_label = "ANCOVA partial eta squared from F",
    primary_effect_size = partial_eta,
    primary_effect_size_label = "Partial eta squared",
    partial_eta_squared = partial_eta,
    cohen_f = cohen_f,
    method_note = "For one-way ANCOVA/group contrast planning, df_effect = groups - 1 and df_error = total N - groups; partial eta squared = F * df_effect / (F * df_effect + df_error)."
  )
}

sample_size_effect_size_nonparametric <- function(
  design = "rank_biserial_from_u",
  u = NULL,
  w_positive = NULL,
  w_negative = NULL,
  h = NULL,
  chi_square = NULL,
  n1 = NULL,
  n2 = NULL,
  n = NULL,
  groups = NULL,
  measurements = NULL
) {
  if (identical(design, "rank_biserial_from_u")) {
    sample_size_validate_positive(u, "Mann-Whitney U")
    sample_size_validate_positive(n1, "Group 1 n")
    sample_size_validate_positive(n2, "Group 2 n")
    max_u <- n1 * n2
    if (u > max_u) stop("Mann-Whitney U cannot exceed n1 * n2.", call. = FALSE)
    r_rb <- 2 * u / max_u - 1
    return(list(
      result_type = "effect_size",
      design_label = "Rank-biserial correlation from Mann-Whitney U",
      primary_effect_size = r_rb,
      primary_effect_size_label = "Rank-biserial r",
      rank_biserial = r_rb,
      cliffs_delta = r_rb,
      method_note = "Rank-biserial correlation = 2U / (n1 n2) - 1; this is equivalent to Cliff's delta orientation."
    ))
  }

  if (identical(design, "rank_biserial_paired")) {
    sample_size_validate_positive(w_positive, "Positive rank sum")
    sample_size_validate_positive(w_negative, "Negative rank sum")
    total_rank <- w_positive + w_negative
    r_rb <- (w_positive - w_negative) / total_rank
    return(list(
      result_type = "effect_size",
      design_label = "Rank-biserial correlation for paired Wilcoxon",
      primary_effect_size = r_rb,
      primary_effect_size_label = "Rank-biserial r",
      rank_biserial = r_rb,
      method_note = "Paired rank-biserial correlation = (W+ - W-) / (W+ + W-)."
    ))
  }

  if (identical(design, "kruskal_epsilon")) {
    sample_size_validate_positive(h, "Kruskal-Wallis H")
    sample_size_validate_positive(n, "Total sample size")
    groups <- as.integer(groups)
    if (!is.finite(groups) || groups < 2) stop("Number of groups must be at least 2.", call. = FALSE)
    epsilon <- max(0, (h - groups + 1) / (n - groups))
    return(list(
      result_type = "effect_size",
      design_label = "Kruskal-Wallis epsilon squared",
      primary_effect_size = epsilon,
      primary_effect_size_label = "Epsilon squared",
      epsilon_squared = epsilon,
      method_note = "Kruskal-Wallis epsilon squared = (H - k + 1) / (N - k), bounded at 0."
    ))
  }

  sample_size_validate_positive(chi_square, "Friedman chi-square")
  sample_size_validate_positive(n, "Participants")
  measurements <- as.integer(measurements)
  if (!is.finite(measurements) || measurements < 2) stop("Measurements must be at least 2.", call. = FALSE)
  kendall_w <- chi_square / (n * (measurements - 1))
  kendall_w <- max(0, min(1, kendall_w))
  list(
    result_type = "effect_size",
    design_label = "Friedman Kendall's W",
    primary_effect_size = kendall_w,
    primary_effect_size_label = "Kendall's W",
    kendall_w = kendall_w,
    method_note = "Kendall's W = Friedman chi-square / [N * (m - 1)]."
  )
}

sample_size_effect_size_mcnemar <- function(
  design = "matched_or_probs",
  p01 = NULL,
  p10 = NULL,
  b = NULL,
  c = NULL
) {
  if (identical(design, "matched_or_counts")) {
    if (!is.finite(b) || b < 0) stop("b discordant count must be nonnegative.", call. = FALSE)
    if (!is.finite(c) || c < 0) stop("c discordant count must be nonnegative.", call. = FALSE)
    if (b + c <= 0) stop("At least one discordant pair is required.", call. = FALSE)
    b_adjusted <- if (b == 0 || c == 0) b + 0.5 else b
    c_adjusted <- if (b == 0 || c == 0) c + 0.5 else c
    odds_ratio <- b_adjusted / c_adjusted
    discordant_probability <- b_adjusted / (b_adjusted + c_adjusted)
    cohen_g <- discordant_probability - 0.5
    return(list(
      result_type = "effect_size",
      design_label = "Matched-pair odds ratio from paired 2x2 table",
      primary_effect_size = odds_ratio,
      primary_effect_size_label = "Matched-pair odds ratio",
      odds_ratio = odds_ratio,
      log_odds_ratio = log(odds_ratio),
      discordant_probability = discordant_probability,
      cohen_g = cohen_g,
      discordant_pairs = b + c,
      method_note = "Matched-pair odds ratio = b / c for discordant pairs; a 0.5 continuity correction is used if either discordant cell is zero."
    ))
  }

  sample_size_validate_probability(p01, "p01")
  sample_size_validate_probability(p10, "p10")
  if (p01 + p10 <= 0) stop("At least one discordant probability must be positive.", call. = FALSE)
  if (p01 + p10 > 1) stop("p01 + p10 must not exceed 1.", call. = FALSE)
  discordant_probability <- p01 / (p01 + p10)
  cohen_g <- discordant_probability - 0.5
  discordant_difference <- p01 - p10

  if (identical(design, "cohen_g")) {
    return(list(
      result_type = "effect_size",
      design_label = "Cohen's g for McNemar discordant pairs",
      primary_effect_size = cohen_g,
      primary_effect_size_label = "Cohen's g",
      cohen_g = cohen_g,
      discordant_probability = discordant_probability,
      discordant_difference = discordant_difference,
      method_note = "Cohen's g = p01 / (p01 + p10) - 0.5 for the discordant-pair direction."
    ))
  }

  if (p10 == 0) stop("p10 must be positive for an odds ratio; use table counts if a continuity correction is needed.", call. = FALSE)
  odds_ratio <- p01 / p10
  list(
    result_type = "effect_size",
    design_label = "Matched-pair odds ratio from discordant probabilities",
    primary_effect_size = odds_ratio,
    primary_effect_size_label = "Matched-pair odds ratio",
    odds_ratio = odds_ratio,
    log_odds_ratio = log(odds_ratio),
    discordant_probability = discordant_probability,
    cohen_g = cohen_g,
    discordant_difference = discordant_difference,
    method_note = "Matched-pair odds ratio = p01 / p10 using the two discordant probabilities."
  )
}

sample_size_effect_size_regression <- function(
  design = "f2_from_r2",
  r_squared = NULL,
  full_r_squared = NULL,
  reduced_r_squared = NULL,
  odds_ratio = NULL,
  interaction_delta_r2 = NULL
) {
  if (identical(design, "f2_from_r2")) {
    sample_size_validate_probability(r_squared, "R-squared", lower = 0, upper = 1)
    f2 <- r_squared / (1 - r_squared)
    return(list(
      result_type = "effect_size",
      design_label = "Multiple regression Cohen's f-squared from R-squared",
      primary_effect_size = f2,
      primary_effect_size_label = "Cohen's f-squared",
      f_squared = f2,
      r_squared = r_squared,
      method_note = "Cohen's f2 = R-squared / (1 - R-squared)."
    ))
  }

  if (identical(design, "hierarchical_f2")) {
    sample_size_validate_probability(full_r_squared, "Full model R-squared", lower = 0, upper = 1)
    if (!is.finite(reduced_r_squared) || reduced_r_squared < 0 || reduced_r_squared >= full_r_squared) {
      stop("Reduced model R-squared must be at least 0 and less than full model R-squared.", call. = FALSE)
    }
    delta_r2 <- full_r_squared - reduced_r_squared
    f2 <- delta_r2 / (1 - full_r_squared)
    return(list(
      result_type = "effect_size",
      design_label = "Hierarchical regression Cohen's f-squared for R-squared increase",
      primary_effect_size = f2,
      primary_effect_size_label = "Incremental Cohen's f-squared",
      f_squared = f2,
      full_r_squared = full_r_squared,
      reduced_r_squared = reduced_r_squared,
      delta_r_squared = delta_r2,
      method_note = "Incremental f2 = (R2_full - R2_reduced) / (1 - R2_full)."
    ))
  }

  if (identical(design, "logistic_or")) {
    if (!is.finite(odds_ratio) || odds_ratio <= 0 || isTRUE(all.equal(odds_ratio, 1))) {
      stop("Odds ratio must be greater than 0 and different from 1.", call. = FALSE)
    }
    log_or <- log(odds_ratio)
    d <- log_or * sqrt(3) / pi
    return(list(
      result_type = "effect_size",
      design_label = "Logistic regression odds ratio conversion",
      primary_effect_size = odds_ratio,
      primary_effect_size_label = "Odds ratio",
      odds_ratio = odds_ratio,
      log_odds_ratio = log_or,
      effect_size_d = d,
      effect_size_label = "Approximate Cohen's d",
      method_note = "Approximate Cohen's d = log(OR) * sqrt(3) / pi under the logistic latent-variable scale."
    ))
  }

  sample_size_validate_probability(interaction_delta_r2, "Interaction delta R-squared", lower = 0, upper = 1)
  f2 <- interaction_delta_r2 / (1 - interaction_delta_r2)
  list(
    result_type = "effect_size",
    design_label = "Moderation interaction Cohen's f-squared",
    primary_effect_size = f2,
    primary_effect_size_label = "Interaction Cohen's f-squared",
    f_squared = f2,
    delta_r_squared = interaction_delta_r2,
    method_note = "For a single interaction increment, f2 = delta R-squared / (1 - delta R-squared)."
  )
}

sample_size_effect_size_gee <- function(
  design = "continuous_means",
  mean1 = NULL,
  mean2 = NULL,
  pre_mean1 = NULL,
  post_mean1 = NULL,
  pre_mean2 = NULL,
  post_mean2 = NULL,
  coefficient = NULL,
  sd = NULL,
  effect_size = NULL,
  p1 = NULL,
  p2 = NULL,
  time_points = 3,
  rho = 0.3,
  structure = "exchangeable",
  correlations = NULL
) {
  continuous_result <- function(d, label, note, contrast = NULL) {
    out <- list(
      result_type = "effect_size",
      design_label = label,
      primary_effect_size = d,
      primary_effect_size_label = "Cohen's d",
      effect_size_d = d,
      effect_size_label = "Cohen's d",
      common_outcome_sd = sd,
      method_note = note
    )
    if (!is.null(contrast)) out$mean_difference <- contrast
    out
  }

  if (identical(design, "continuous_means") || identical(design, "continuous_followup_means")) {
    sample_size_validate_positive(sd, "Common outcome SD")
    if (!is.finite(mean1) || !is.finite(mean2)) stop("Means must be numeric.", call. = FALSE)
    contrast <- mean1 - mean2
    d <- contrast / sd
    return(continuous_result(
      d,
      "GEE follow-up estimated mean difference",
      "Cohen's d = (estimated mean group 1 - estimated mean group 2) / common outcome SD.",
      contrast
    ))
  }

  if (identical(design, "continuous_change_means")) {
    sample_size_validate_positive(sd, "Common outcome SD")
    if (!is.finite(pre_mean1) || !is.finite(post_mean1) || !is.finite(pre_mean2) || !is.finite(post_mean2)) {
      stop("Pre and post means must be numeric.", call. = FALSE)
    }
    contrast <- (post_mean1 - pre_mean1) - (post_mean2 - pre_mean2)
    d <- contrast / sd
    return(continuous_result(
      d,
      "GEE pre-post change difference",
      "Cohen's d = [(post - pre) group 1 - (post - pre) group 2] / common outcome SD.",
      contrast
    ))
  }

  if (identical(design, "continuous_parameter_b")) {
    sample_size_validate_positive(sd, "Common outcome SD")
    if (!is.finite(coefficient)) stop("Parameter estimate B must be numeric.", call. = FALSE)
    d <- coefficient / sd
    result <- continuous_result(
      d,
      "GEE group x time parameter estimate standardized effect",
      "Cohen's d = GEE group x time parameter estimate B / common outcome SD.",
      coefficient
    )
    result$parameter_estimate <- coefficient
    return(result)
  }

  if (identical(design, "continuous_d")) {
    sample_size_validate_positive(abs(effect_size), "Effect size d")
    return(continuous_result(
      effect_size,
      "GEE continuous outcome supplied standardized mean difference",
      "Uses the supplied standardized mean difference d."
    ))
  }

  sample_size_validate_probability(p1, "Proportion 1")
  sample_size_validate_probability(p2, "Proportion 2")
  h <- 2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2))
  risk_difference <- p1 - p2
  odds1 <- p1 / (1 - p1)
  odds2 <- p2 / (1 - p2)
  odds_ratio <- odds1 / odds2
  list(
    result_type = "effect_size",
    design_label = "GEE binary outcome proportion effect",
    primary_effect_size = h,
    primary_effect_size_label = "Cohen's h",
    cohens_h = h,
    risk_difference = risk_difference,
    risk_ratio = p1 / p2,
    odds_ratio = odds_ratio,
    log_odds_ratio = log(odds_ratio),
    proportion1 = p1,
    proportion2 = p2,
    method_note = "Cohen's h = 2 asin(sqrt(p1)) - 2 asin(sqrt(p2))."
  )
}

sample_size_effect_size_lmm <- function(
  design = "simple_fixed",
  lmm_design = "two_group_repeated",
  effect_size = NULL,
  group1_means = NULL,
  group2_means = NULL,
  residual_sd = NULL,
  time_points = 3,
  icc = 0.3,
  rho = 0.5,
  structure = "exchangeable",
  correlations = NULL
) {
  repeated_planning_effect <- function(d, measurements, correlation) {
    design_effect <- 1 + (measurements - 1) * correlation
    list(
      design_effect = design_effect,
      planning_effect_size = d * sqrt(measurements / design_effect)
    )
  }

  if (identical(design, "simple_fixed")) {
    sample_size_validate_positive(abs(effect_size), "Standardized fixed effect")
    time_points <- as.integer(time_points)
    if (!is.finite(time_points) || time_points < 2) stop("Time points must be at least 2.", call. = FALSE)
    sample_size_validate_probability(icc, "ICC", lower = 0, upper = 1)
    adjusted <- repeated_planning_effect(effect_size, time_points, icc)
    return(list(
      result_type = "effect_size",
      design_label = if (identical(lmm_design, "one_group_repeated")) "LMM one-group repeated fixed effect" else "LMM two-group repeated fixed effect",
      primary_effect_size = effect_size,
      primary_effect_size_label = "Standardized fixed effect",
      effect_size_d = effect_size,
      effect_size_label = "Standardized fixed effect",
      planning_effect_size = adjusted$planning_effect_size,
      design_effect = adjusted$design_effect,
      time_points = time_points,
      intraclass_correlation = icc,
      method_note = "Repeated-measures planning effect = standardized fixed effect * sqrt(m / [1 + (m - 1)ICC])."
    ))
  }

  g1 <- sample_size_parse_numeric_vector(group1_means, "Group 1 means")
  sample_size_validate_positive(residual_sd, "Residual SD")
  time_points <- length(g1)
  if (time_points < 2) stop("At least two time points are required.", call. = FALSE)
  structure_label <- sample_size_lmm_structure_label(structure)
  mean_correlation <- if (identical(structure, "ar1") || identical(structure, "unstructured")) {
    cors <- sample_size_lmm_correlation_matrix(time_points, rho, structure, correlations)
    mean(cors[upper.tri(cors)])
  } else {
    sample_size_validate_probability(rho, "Correlation rho", lower = -1, upper = 1)
    rho
  }

  if (identical(lmm_design, "two_group_repeated")) {
    g2 <- sample_size_parse_numeric_vector(group2_means, "Group 2 means")
    if (length(g2) != time_points) stop("Group mean vectors must have the same length.", call. = FALSE)
    change_difference <- (g2[[time_points]] - g2[[1]]) - (g1[[time_points]] - g1[[1]])
    d <- change_difference / residual_sd
    design_label <- "LMM GLIMMPSE-style group by time effect"
  } else {
    change_difference <- g1[[time_points]] - g1[[1]]
    d <- change_difference / residual_sd
    design_label <- "LMM GLIMMPSE-style one-group time effect"
  }

  adjusted <- repeated_planning_effect(d, time_points, mean_correlation)
  list(
    result_type = "effect_size",
    design_label = design_label,
    primary_effect_size = d,
    primary_effect_size_label = "Standardized change effect",
    effect_size_d = d,
    effect_size_label = "Standardized change effect",
    change_difference = change_difference,
    residual_sd = residual_sd,
    planning_effect_size = adjusted$planning_effect_size,
    design_effect = adjusted$design_effect,
    time_points = time_points,
    working_correlation = structure_label,
    method_note = "GLIMMPSE-style effect uses the last-minus-first mean contrast divided by residual SD, with repeated-measures planning adjustment."
  )
}

sample_size_effect_size_survival <- function(
  design = "hazard_ratio",
  hazard_ratio = NULL
) {
  if (!is.finite(hazard_ratio) || hazard_ratio <= 0 || isTRUE(all.equal(hazard_ratio, 1))) {
    stop("Hazard ratio must be greater than 0 and different from 1.", call. = FALSE)
  }
  log_hr <- log(hazard_ratio)
  result <- list(
    result_type = "effect_size",
    design_label = "Survival / Cox hazard ratio effect",
    primary_effect_size = hazard_ratio,
    primary_effect_size_label = "Hazard ratio",
    hazard_ratio = hazard_ratio,
    log_hazard_ratio = log_hr,
    method_note = "Log hazard ratio = log(HR)."
  )

  result
}

sample_size_effect_size_equivalence <- function(
  outcome = "mean",
  objective = "noninferiority",
  true_difference = 0,
  margin = NULL,
  sd = NULL,
  p1 = NULL,
  p2 = NULL
) {
  sample_size_validate_positive(margin, "Margin")
  is_equivalence <- identical(objective, "equivalence")
  objective_label <- if (is_equivalence) "Equivalence" else "Non-inferiority"

  if (identical(outcome, "proportion")) {
    sample_size_validate_probability(p1, "Proportion 1")
    sample_size_validate_probability(p2, "Proportion 2")
    effect <- p1 - p2
    pooled <- (p1 + p2) / 2
    pooled_sd <- sqrt(max(1e-12, pooled * (1 - pooled)))
    standardized_effect <- effect / pooled_sd
    standardized_margin <- margin / pooled_sd
    outcome_label <- "proportion difference"
  } else {
    sample_size_validate_positive(sd, "SD")
    if (!is.finite(true_difference)) stop("Expected true difference must be numeric.", call. = FALSE)
    effect <- true_difference
    standardized_effect <- effect / sd
    standardized_margin <- margin / sd
    outcome_label <- "mean difference"
  }

  distance <- if (is_equivalence) margin - abs(effect) else margin + effect
  if (!is.finite(distance)) stop("Distance to boundary could not be calculated.", call. = FALSE)
  standardized_distance <- if (identical(outcome, "proportion")) distance / pooled_sd else distance / sd
  inside_margin <- distance > 0

  list(
    result_type = "effect_size",
    design_label = paste(objective_label, outcome_label, "margin distance"),
    primary_effect_size = standardized_distance,
    primary_effect_size_label = "Standardized distance to margin",
    observed_effect = effect,
    equivalence_margin = margin,
    distance_to_margin = distance,
    standardized_effect = standardized_effect,
    standardized_margin = standardized_margin,
    standardized_distance = standardized_distance,
    inside_margin = if (inside_margin) "Yes" else "No",
    method_note = if (is_equivalence) {
      "Equivalence distance = margin - abs(observed effect); positive values are inside the equivalence margin."
    } else {
      "Non-inferiority distance = margin + observed effect when the non-inferiority boundary is -margin; positive values are above the boundary."
    }
  )
}

sample_size_effect_size_diagnostic <- function(
  design = "auc",
  auc = NULL,
  null_auc = 0.5
) {
  sample_size_validate_probability(auc, "AUC")
  sample_size_validate_probability(null_auc, "Null AUC")
  if (auc <= null_auc) stop("AUC must be greater than null AUC.", call. = FALSE)
  auc_difference <- auc - null_auc
  auc_d <- sqrt(2) * stats::qnorm(auc)
  list(
    result_type = "effect_size",
    design_label = "ROC AUC effect vs null",
    primary_effect_size = auc,
    primary_effect_size_label = "AUC",
    auc = auc,
    null_auc = null_auc,
    auc_difference = auc_difference,
    auc_cohen_d = auc_d,
    method_note = "AUC is reported directly; AUC difference = AUC - null AUC; approximate Cohen's d = sqrt(2) * qnorm(AUC)."
  )
}

sample_size_effect_size_rates <- function(
  design = "poisson_irr",
  input_scale = "ratio",
  ratio = NULL,
  log_ratio = NULL
) {
  if (identical(input_scale, "log_ratio")) {
    if (!is.finite(log_ratio) || isTRUE(all.equal(log_ratio, 0))) {
      stop("log ratio must be finite and different from 0.", call. = FALSE)
    }
    log_effect <- log_ratio
    effect_ratio <- exp(log_ratio)
  } else {
    sample_size_validate_positive(ratio, "Ratio")
    if (isTRUE(all.equal(ratio, 1))) stop("Ratio must be different from 1.", call. = FALSE)
    effect_ratio <- ratio
    log_effect <- log(ratio)
  }

  is_gamma <- identical(design, "gamma_mean_ratio")
  ratio_label <- if (is_gamma) "Mean ratio" else "Incidence rate ratio"
  design_label <- switch(
    design,
    negative_binomial_irr = "Negative binomial regression incidence rate ratio",
    gamma_mean_ratio = "Gamma regression mean ratio",
    "Poisson regression incidence rate ratio"
  )
  result <- list(
    result_type = "effect_size",
    design_label = design_label,
    primary_effect_size = effect_ratio,
    primary_effect_size_label = ratio_label,
    ratio = effect_ratio,
    log_ratio = log_effect,
    method_note = sprintf("%s = exp(beta); log %s = beta from a log-link regression model.", ratio_label, tolower(ratio_label))
  )

  if (is_gamma) {
    result$mean_ratio <- effect_ratio
    result$log_mean_ratio <- log_effect
  } else {
    result$incidence_rate_ratio <- effect_ratio
    result$log_incidence_rate_ratio <- log_effect
  }

  result
}

sample_size_effect_size_cluster <- function(
  design = "parallel_continuous",
  effect_size = NULL,
  p1 = NULL,
  p2 = NULL,
  cluster_size = 20,
  icc = 0.05,
  periods = 5
) {
  sample_size_validate_positive(cluster_size, "Cluster size")
  sample_size_validate_probability(icc, "ICC", lower = 0, upper = 1)
  design_effect <- 1 + (cluster_size - 1) * icc

  if (identical(design, "parallel_binary")) {
    sample_size_validate_probability(p1, "Proportion 1")
    sample_size_validate_probability(p2, "Proportion 2")
    h <- 2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2))
    return(list(
      result_type = "effect_size",
      design_label = "Parallel cluster trial binary outcome",
      primary_effect_size = h,
      primary_effect_size_label = "Cohen's h",
      cohens_h = h,
      risk_difference = p1 - p2,
      risk_ratio = p1 / p2,
      proportion1 = p1,
      proportion2 = p2,
      cluster_size = cluster_size,
      intraclass_correlation = icc,
      design_effect = design_effect,
      planning_effect_size = h / sqrt(design_effect),
      method_note = "Binary cluster effect uses Cohen's h; planning effect = h / sqrt(1 + (m - 1)ICC)."
    ))
  }

  sample_size_validate_positive(abs(effect_size), "Effect size d")
  if (identical(design, "stepped_wedge")) {
    periods <- as.integer(periods)
    if (!is.finite(periods) || periods < 3) stop("Periods must be at least 3 for stepped-wedge designs.", call. = FALSE)
    period_multiplier <- periods / max(1, periods - 1)
    stepped_design_effect <- design_effect * period_multiplier
    return(list(
      result_type = "effect_size",
      design_label = "Stepped-wedge cluster trial continuous outcome",
      primary_effect_size = effect_size,
      primary_effect_size_label = "Effect size d",
      effect_size_d = effect_size,
      effect_size_label = "Effect size d",
      cluster_size = cluster_size,
      intraclass_correlation = icc,
      periods = periods,
      design_effect = stepped_design_effect,
      planning_effect_size = effect_size / sqrt(stepped_design_effect),
      method_note = "Stepped-wedge planning effect applies a simple cluster design effect multiplied by periods / (periods - 1)."
    ))
  }

  list(
    result_type = "effect_size",
    design_label = "Parallel cluster trial continuous outcome",
    primary_effect_size = effect_size,
    primary_effect_size_label = "Effect size d",
    effect_size_d = effect_size,
    effect_size_label = "Effect size d",
    cluster_size = cluster_size,
    intraclass_correlation = icc,
    design_effect = design_effect,
    planning_effect_size = effect_size / sqrt(design_effect),
    method_note = "Continuous cluster planning effect = d / sqrt(1 + (m - 1)ICC)."
  )
}

sample_size_effect_size_precision <- function(
  parameter = "mean",
  estimate = NULL,
  half_width = NULL,
  sd = NULL,
  proportion = NULL,
  r = NULL
) {
  sample_size_validate_positive(half_width, "Desired CI half-width")

  if (identical(parameter, "mean")) {
    sample_size_validate_positive(sd, "SD")
    if (!is.finite(estimate)) stop("Expected mean must be numeric.", call. = FALSE)
    standardized_half_width <- half_width / sd
    relative_half_width <- if (!isTRUE(all.equal(estimate, 0))) abs(half_width / estimate) else NA_real_
    return(list(
      result_type = "effect_size",
      design_label = "Mean CI precision effect",
      primary_effect_size = standardized_half_width,
      primary_effect_size_label = "Standardized half-width",
      estimate = estimate,
      half_width = half_width,
      sd = sd,
      standardized_half_width = standardized_half_width,
      relative_half_width = relative_half_width,
      method_note = "Standardized half-width = desired mean CI half-width / SD."
    ))
  }

  if (identical(parameter, "proportion")) {
    sample_size_validate_probability(proportion, "Expected proportion")
    bernoulli_sd <- sqrt(proportion * (1 - proportion))
    standardized_half_width <- half_width / bernoulli_sd
    relative_half_width <- half_width / proportion
    return(list(
      result_type = "effect_size",
      design_label = "Proportion CI precision effect",
      primary_effect_size = standardized_half_width,
      primary_effect_size_label = "Bernoulli-standardized half-width",
      proportion = proportion,
      half_width = half_width,
      bernoulli_sd = bernoulli_sd,
      standardized_half_width = standardized_half_width,
      relative_half_width = relative_half_width,
      method_note = "Bernoulli-standardized half-width = desired proportion CI half-width / sqrt(p[1 - p])."
    ))
  }

  if (!is.finite(r) || abs(r) >= 1) {
    stop("Expected r must be finite and less than 1 in absolute value.", call. = FALSE)
  }
  upper_r <- min(0.999999, r + half_width)
  lower_r <- max(-0.999999, r - half_width)
  fisher_z_half_width <- max(abs(atanh(upper_r) - atanh(r)), abs(atanh(r) - atanh(lower_r)))
  list(
    result_type = "effect_size",
    design_label = "Correlation CI precision effect",
    primary_effect_size = fisher_z_half_width,
    primary_effect_size_label = "Fisher z half-width",
    correlation_r = r,
    half_width = half_width,
    fisher_z = atanh(r),
    fisher_z_half_width = fisher_z_half_width,
    relative_half_width = if (!isTRUE(all.equal(r, 0))) abs(half_width / r) else NA_real_,
    method_note = "Correlation precision uses Fisher's z transformation; z half-width is computed from r +/- desired raw-r half-width."
  )
}

sample_size_effect_size_reliability <- function(
  design = "alpha",
  reliability = NULL,
  reference = NULL,
  items = 5,
  categories = 2,
  sd_difference = NULL
) {
  if (identical(design, "bland_altman")) {
    sample_size_validate_positive(sd_difference, "SD of paired differences")
    loa_half_width <- 1.96 * sd_difference
    loa_total_width <- 2 * loa_half_width
    return(list(
      result_type = "effect_size",
      design_label = "Bland-Altman limits of agreement width",
      primary_effect_size = loa_total_width,
      primary_effect_size_label = "Limits of agreement total width",
      sd_difference = sd_difference,
      loa_half_width = loa_half_width,
      loa_total_width = loa_total_width,
      method_note = "Bland-Altman limits of agreement are approximately mean difference +/- 1.96 * SD of paired differences."
    ))
  }

  sample_size_validate_probability(reliability, "Expected reliability")
  if (identical(design, "kappa")) {
    categories <- as.integer(categories)
    if (!is.finite(categories) || categories < 2) stop("Number of categories must be at least 2.", call. = FALSE)
    pe <- 1 / categories
    observed_agreement <- reliability * (1 - pe) + pe
    return(list(
      result_type = "effect_size",
      design_label = "Cohen's kappa agreement effect",
      primary_effect_size = reliability,
      primary_effect_size_label = "Cohen's kappa",
      reliability = reliability,
      categories = categories,
      chance_agreement = pe,
      observed_agreement = observed_agreement,
      method_note = "Cohen's kappa is reported directly; observed agreement assumes equal category prevalence."
    ))
  }

  sample_size_validate_probability(reference, "Reference reliability")
  reliability_difference <- reliability - reference

  if (identical(design, "alpha")) {
    items <- as.integer(items)
    if (!is.finite(items) || items < 2) stop("Number of items must be at least 2.", call. = FALSE)
    transformed <- log(1 - reliability)
    reference_transformed <- log(1 - reference)
    average_inter_item_r <- reliability / (items - reliability * (items - 1))
    return(list(
      result_type = "effect_size",
      design_label = "Cronbach's alpha effect vs reference",
      primary_effect_size = reliability_difference,
      primary_effect_size_label = "Alpha difference",
      reliability = reliability,
      reference_reliability = reference,
      reliability_difference = reliability_difference,
      transformed_reliability = transformed,
      transformed_difference = transformed - reference_transformed,
      items = items,
      average_inter_item_r = average_inter_item_r,
      method_note = "Alpha difference = alpha - reference alpha; Bonett-style transformed alpha uses log(1 - alpha)."
    ))
  }

  if (identical(design, "icc")) {
    items <- as.integer(items)
    if (!is.finite(items) || items < 2) stop("Raters / measurements must be at least 2.", call. = FALSE)
    transformed <- atanh(reliability)
    reference_transformed <- atanh(reference)
    return(list(
      result_type = "effect_size",
      design_label = "ICC reliability effect vs reference",
      primary_effect_size = reliability_difference,
      primary_effect_size_label = "ICC difference",
      reliability = reliability,
      reference_reliability = reference,
      reliability_difference = reliability_difference,
      transformed_reliability = transformed,
      transformed_difference = transformed - reference_transformed,
      items = items,
      method_note = "ICC difference = ICC - reference ICC; transformed ICC uses Fisher's z."
    ))
  }

  stop("Unsupported reliability effect-size design.", call. = FALSE)
}

sample_size_effect_size_sem <- function(
  design = "rmsea",
  df = NULL,
  null_rmsea = NULL,
  alternative_rmsea = NULL,
  parameter_type = "path",
  parameter = NULL,
  latent_variables = NULL,
  measured_variables = NULL,
  structural_paths = NULL,
  free_parameters = NULL,
  expected_loading = NULL,
  expected_path = NULL,
  complexity = "moderate"
) {
  if (identical(design, "parameter")) {
    if (!is.finite(parameter) || abs(parameter) >= 1 || isTRUE(all.equal(parameter, 0))) {
      stop("Expected standardized parameter must be nonzero and between -1 and 1.", call. = FALSE)
    }
    parameter_label <- switch(
      parameter_type,
      loading = "Standardized loading",
      correlation = "Latent correlation",
      "Standardized path"
    )
    fisher_z <- atanh(parameter)
    return(list(
      result_type = "effect_size",
      design_label = paste("SEM/CFA", parameter_label, "effect"),
      primary_effect_size = parameter,
      primary_effect_size_label = parameter_label,
      sem_parameter = parameter,
      sem_parameter_type = parameter_label,
      fisher_z = fisher_z,
      absolute_parameter = abs(parameter),
      method_note = "Standardized SEM parameter effect is the expected standardized coefficient; Fisher's z is atanh(parameter)."
    ))
  }

  if (identical(design, "complexity")) {
    latent_variables <- as.integer(latent_variables)
    measured_variables <- as.integer(measured_variables)
    structural_paths <- as.integer(structural_paths)
    free_parameters <- as.integer(free_parameters)
    if (!is.finite(latent_variables) || latent_variables < 1) stop("Latent variables must be at least 1.", call. = FALSE)
    if (!is.finite(measured_variables) || measured_variables < latent_variables) stop("Measured variables must be at least latent variables.", call. = FALSE)
    if (!is.finite(structural_paths) || structural_paths < 0) stop("Structural paths must be 0 or greater.", call. = FALSE)
    if (!is.finite(free_parameters) || free_parameters < 1) stop("Free parameters must be at least 1.", call. = FALSE)
    sample_size_validate_positive(abs(expected_loading), "Expected loading")
    sample_size_validate_positive(abs(expected_path), "Expected path")
    if (abs(expected_loading) >= 1 || abs(expected_path) >= 1) {
      stop("Expected loading and path must be between -1 and 1.", call. = FALSE)
    }
    parameter_ratio <- switch(complexity, simple = 10, complex = 20, 15)
    structure_burden <- 10 * measured_variables + 20 * latent_variables + 15 * structural_paths
    loading_z <- atanh(expected_loading)
    path_z <- atanh(expected_path)
    return(list(
      result_type = "effect_size",
      design_label = "SEM/CFA model complexity effect",
      primary_effect_size = structure_burden / free_parameters,
      primary_effect_size_label = "Structure burden per free parameter",
      latent_variables = latent_variables,
      measured_variables = measured_variables,
      structural_paths = structural_paths,
      free_parameters = free_parameters,
      parameter_ratio = parameter_ratio,
      structure_burden = structure_burden,
      structure_burden_per_parameter = structure_burden / free_parameters,
      expected_loading = expected_loading,
      expected_path = expected_path,
      loading_fisher_z = loading_z,
      path_fisher_z = path_z,
      method_note = "Complexity effect summarizes observed/latent/path burden per free parameter plus Fisher-z transformed expected loading and path effects."
    ))
  }

  df <- as.integer(df)
  if (!is.finite(df) || df < 1) stop("Model degrees of freedom must be at least 1.", call. = FALSE)
  sample_size_validate_probability(null_rmsea, "Null RMSEA")
  sample_size_validate_probability(alternative_rmsea, "Alternative RMSEA")
  rmsea_difference <- alternative_rmsea - null_rmsea
  ncp_difference_per_n <- df * (alternative_rmsea^2 - null_rmsea^2)
  list(
    result_type = "effect_size",
    design_label = "SEM/CFA RMSEA effect",
    primary_effect_size = abs(rmsea_difference),
    primary_effect_size_label = "RMSEA difference",
    df = df,
    null_rmsea = null_rmsea,
    alternative_rmsea = alternative_rmsea,
    rmsea_difference = rmsea_difference,
    ncp_difference_per_n = ncp_difference_per_n,
    method_note = "RMSEA effect is the difference between alternative and null RMSEA; noncentrality difference per N is df * (RMSEA_alt^2 - RMSEA_null^2)."
  )
}

sample_size_round_up <- function(x) {
  ceiling(as.numeric(x))
}

sample_size_tail_multiplier <- function(alternative) {
  if (identical(alternative, "two.sided")) 2 else 1
}

sample_size_z_alpha <- function(alpha, alternative) {
  stats::qnorm(1 - alpha / sample_size_tail_multiplier(alternative))
}

sample_size_ttest_design_label <- function(design) {
  switch(
    design,
    two_sample = "Independent two-sample t-test",
    paired = "Paired t-test",
    "One-sample t-test"
  )
}

sample_size_nonparametric_design_label <- function(design) {
  switch(
    design,
    two_independent = "Mann-Whitney U test",
    paired = "Wilcoxon signed-rank test for paired samples",
    kruskal_wallis = "Kruskal-Wallis test",
    friedman = "Friedman test",
    "One-sample Wilcoxon signed-rank test for median shift"
  )
}

sample_size_validate_probability <- function(x, name, lower = 0, upper = 1) {
  if (!is.finite(x) || x <= lower || x >= upper) {
    stop(sprintf("%s must be greater than %.2f and less than %.2f.", name, lower, upper), call. = FALSE)
  }
}

sample_size_validate_positive <- function(x, name) {
  if (!is.finite(x) || x <= 0) {
    stop(sprintf("%s must be greater than 0.", name), call. = FALSE)
  }
}

sample_size_drop_adjust <- function(n, dropout) {
  if (!is.finite(dropout) || dropout < 0 || dropout >= 0.9) {
    stop("Dropout rate must be between 0 and 89%.", call. = FALSE)
  }
  sample_size_round_up(n / (1 - dropout))
}

sample_size_ttest <- function(target, design, effect_size, alpha, power = NULL, n = NULL, ratio = 1, alternative = "two.sided", dropout = 0) {
  sample_size_validate_positive(abs(effect_size), "Effect size")
  sample_size_validate_probability(alpha, "Alpha")
  sample_size_validate_positive(ratio, "Allocation ratio")
  za <- sample_size_z_alpha(alpha, alternative)
  ttest_type <- switch(
    design,
    two_sample = "two.sample",
    paired = "paired",
    "one.sample"
  )

  if (identical(target, "sample_size")) {
    sample_size_validate_probability(power, "Power")
    if (!identical(design, "two_sample") || isTRUE(all.equal(ratio, 1))) {
      exact <- stats::power.t.test(
        delta = abs(effect_size),
        sd = 1,
        sig.level = alpha,
        power = power,
        type = ttest_type,
        alternative = alternative
      )
      if (identical(design, "two_sample")) {
        group_n <- sample_size_round_up(exact$n)
        adjusted_group_n <- sample_size_drop_adjust(group_n, dropout)
        return(list(
          group1 = group_n,
          group2 = group_n,
          total = group_n * 2,
          adjusted_group1 = adjusted_group_n,
          adjusted_group2 = adjusted_group_n,
          adjusted_total = adjusted_group_n * 2,
          dropout_rate = dropout,
          design_label = sample_size_ttest_design_label(design),
          method_note = "Exact independent two-sample t-test power calculation using Cohen's d."
        ))
      }
      total <- sample_size_round_up(exact$n)
      total_label <- if (identical(design, "paired")) "Pairs" else "Participants"
      adjusted_label <- if (identical(design, "paired")) "Pairs with dropout" else "Participants with dropout"
      return(list(
        total = total,
        total_label = total_label,
        adjusted_total = sample_size_drop_adjust(total, dropout),
        adjusted_total_label = adjusted_label,
        dropout_rate = dropout,
        design_label = sample_size_ttest_design_label(design),
        method_note = if (identical(design, "paired")) {
          "Exact paired t-test power calculation using dz, the standardized mean of paired differences."
        } else {
          "Exact one-sample t-test power calculation using standardized mean difference."
        }
      ))
    }
    zb <- stats::qnorm(power)
    base_n <- ((1 + 1 / ratio) * (za + zb)^2) / effect_size^2
    group1 <- sample_size_round_up(base_n)
    group2 <- sample_size_round_up(group1 * ratio)
    adjusted_group1 <- sample_size_drop_adjust(group1, dropout)
    adjusted_group2 <- sample_size_drop_adjust(group2, dropout)
    return(list(
      group1 = group1,
      group2 = group2,
      total = group1 + group2,
      adjusted_group1 = adjusted_group1,
      adjusted_group2 = adjusted_group2,
      adjusted_total = adjusted_group1 + adjusted_group2,
      dropout_rate = dropout,
      design_label = sample_size_ttest_design_label(design),
      method_note = "Normal approximation for unequal-allocation independent means using Cohen's d."
    ))
  }

  sample_size_validate_positive(n, "Sample size")
  if (!identical(design, "two_sample") || isTRUE(all.equal(ratio, 1))) {
    exact <- stats::power.t.test(
      n = n,
      delta = abs(effect_size),
      sd = 1,
      sig.level = alpha,
      type = ttest_type,
      alternative = alternative
    )
    return(list(
      power = exact$power,
      design_label = sample_size_ttest_design_label(design),
      method_note = paste0("Exact ", tolower(sample_size_ttest_design_label(design)), " power calculation.")
    ))
  }
  if (identical(design, "two_sample")) {
    z_effect <- abs(effect_size) * sqrt(n / (1 + 1 / ratio))
  } else {
    z_effect <- abs(effect_size) * sqrt(n)
  }
  list(
    power = stats::pnorm(z_effect - za),
    design_label = sample_size_ttest_design_label(design),
    method_note = "Approximate power for unequal-allocation independent means based on the normal distribution."
  )
}

sample_size_nonparametric <- function(
  target,
  design,
  effect_size,
  alpha,
  power = NULL,
  n = NULL,
  ratio = 1,
  alternative = "two.sided",
  dropout = 0,
  are = 0.955,
  groups = 3,
  measurements = 3
) {
  if (design %in% c("kruskal_wallis", "friedman")) {
    return(sample_size_anova(
      target = target,
      design = design,
      groups = groups,
      effect_size = effect_size,
      alpha = alpha,
      power = power,
      n = n,
      dropout = dropout,
      measurements = measurements
    ))
  }
  sample_size_validate_positive(are, "Asymptotic relative efficiency")
  ttest_design <- switch(
    design,
    two_independent = "two_sample",
    paired = "paired",
    "one_sample"
  )
  label <- sample_size_nonparametric_design_label(design)

  if (identical(target, "sample_size")) {
    base <- sample_size_ttest(
      target = "sample_size",
      design = ttest_design,
      effect_size = effect_size,
      alpha = alpha,
      power = power,
      ratio = ratio,
      alternative = alternative,
      dropout = 0
    )
    if (identical(design, "two_independent")) {
      group1 <- sample_size_round_up(base$group1 / are)
      group2 <- sample_size_round_up(base$group2 / are)
      adjusted_group1 <- sample_size_drop_adjust(group1, dropout)
      adjusted_group2 <- sample_size_drop_adjust(group2, dropout)
      return(list(
        group1 = group1,
        group2 = group2,
        total = group1 + group2,
        adjusted_group1 = adjusted_group1,
        adjusted_group2 = adjusted_group2,
        adjusted_total = adjusted_group1 + adjusted_group2,
        dropout_rate = dropout,
        design_label = label,
        method_note = "Approximate nonparametric sample size using ARE = 0.955 relative to the corresponding t-test under normal data."
      ))
    }
    total <- sample_size_round_up(base$total / are)
    total_label <- if (identical(design, "paired")) "Pairs" else "Participants"
    adjusted_label <- if (identical(design, "paired")) "Pairs with dropout" else "Participants with dropout"
    return(list(
      total = total,
      total_label = total_label,
      adjusted_total = sample_size_drop_adjust(total, dropout),
      adjusted_total_label = adjusted_label,
      dropout_rate = dropout,
      design_label = label,
      method_note = "Approximate nonparametric sample size using ARE = 0.955 relative to the corresponding t-test under normal data."
    ))
  }

  sample_size_validate_positive(n, "Sample size")
  effective_n <- n * are
  result <- sample_size_ttest(
    target = "power",
    design = ttest_design,
    effect_size = effect_size,
    alpha = alpha,
    n = effective_n,
    ratio = ratio,
    alternative = alternative
  )
  result$design_label <- label
  result$method_note <- "Approximate nonparametric power using ARE = 0.955 relative to the corresponding t-test under normal data."
  result
}

sample_size_proportion <- function(target, design, p1, p2 = NULL, alpha, power = NULL, n = NULL, ratio = 1, alternative = "two.sided", dropout = 0) {
  sample_size_validate_probability(p1, "Proportion 1")
  sample_size_validate_probability(alpha, "Alpha")
  sample_size_validate_positive(ratio, "Allocation ratio")
  za <- sample_size_z_alpha(alpha, alternative)

  if (identical(design, "two_proportion")) {
    sample_size_validate_probability(p2, "Proportion 2")
    effect <- abs(p1 - p2)
    sample_size_validate_positive(effect, "Proportion difference")
    pooled <- (p1 + ratio * p2) / (1 + ratio)
    variance_null <- pooled * (1 - pooled) * (1 + 1 / ratio)
    variance_alt <- p1 * (1 - p1) + p2 * (1 - p2) / ratio
    if (identical(target, "sample_size")) {
      sample_size_validate_probability(power, "Power")
      zb <- stats::qnorm(power)
      group1 <- sample_size_round_up(((za * sqrt(variance_null) + zb * sqrt(variance_alt)) / effect)^2)
      group2 <- sample_size_round_up(group1 * ratio)
      adjusted_group1 <- sample_size_drop_adjust(group1, dropout)
      adjusted_group2 <- sample_size_drop_adjust(group2, dropout)
      return(list(
        group1 = group1,
        group2 = group2,
        total = group1 + group2,
        adjusted_group1 = adjusted_group1,
        adjusted_group2 = adjusted_group2,
        adjusted_total = adjusted_group1 + adjusted_group2,
        dropout_rate = dropout,
        method_note = "Normal approximation for two independent proportions."
      ))
    }
    sample_size_validate_positive(n, "Sample size")
    z_effect <- effect * sqrt(n) - za * sqrt(variance_null)
    return(list(power = stats::pnorm(z_effect / sqrt(variance_alt)), method_note = "Approximate power for two independent proportions."))
  }

  effect <- abs(p1 - 0.5)
  sample_size_validate_positive(effect, "Difference from null proportion 0.50")
  variance <- p1 * (1 - p1)
  if (identical(target, "sample_size")) {
    sample_size_validate_probability(power, "Power")
    total <- sample_size_round_up(((za + stats::qnorm(power))^2 * variance) / effect^2)
    return(list(total = total, adjusted_total = sample_size_drop_adjust(total, dropout), dropout_rate = dropout, method_note = "Approximation for one proportion against null p = 0.50."))
  }
  sample_size_validate_positive(n, "Sample size")
  list(power = stats::pnorm(abs(effect) * sqrt(n / variance) - za), method_note = "Approximate power for one proportion against null p = 0.50.")
}

sample_size_correlation <- function(target, r, alpha, power = NULL, n = NULL, alternative = "two.sided", dropout = 0) {
  if (!is.finite(r) || abs(r) <= 0 || abs(r) >= 1) {
    stop("Expected r must be greater than 0 and less than 1 in absolute value.", call. = FALSE)
  }
  sample_size_validate_probability(alpha, "Alpha")
  zr <- abs(atanh(r))
  za <- sample_size_z_alpha(alpha, alternative)
  if (identical(target, "sample_size")) {
    sample_size_validate_probability(power, "Power")
    total <- sample_size_round_up(((za + stats::qnorm(power)) / zr)^2 + 3)
    return(list(total = total, adjusted_total = sample_size_drop_adjust(total, dropout), dropout_rate = dropout, method_note = "Fisher z approximation for Pearson correlation."))
  }
  sample_size_validate_positive(n, "Sample size")
  list(power = stats::pnorm(zr * sqrt(n - 3) - za), method_note = "Fisher z approximation for Pearson correlation.")
}

sample_size_anova <- function(
  target,
  design = "one_way",
  groups = 3,
  effect_size,
  alpha,
  power = NULL,
  n = NULL,
  dropout = 0,
  factor_a_levels = 2,
  factor_b_levels = 2,
  effect = "interaction",
  measurements = 3,
  repeated_correlation = 0.5,
  epsilon = 1
) {
  sample_size_validate_positive(effect_size, "Effect size f")
  sample_size_validate_probability(alpha, "Alpha")
  if (!is.finite(epsilon) || epsilon <= 0 || epsilon > 1) {
    stop("Nonsphericity epsilon must be greater than 0 and less than or equal to 1.", call. = FALSE)
  }
  if (!is.finite(repeated_correlation) || repeated_correlation <= -1 || repeated_correlation >= 1) {
    stop("Average repeated-measures correlation must be greater than -1 and less than 1.", call. = FALSE)
  }

  if (identical(design, "kruskal_wallis")) {
    groups <- as.integer(groups)
    if (!is.finite(groups) || groups < 2) {
      stop("Number of groups must be at least 2.", call. = FALSE)
    }
    df1 <- groups - 1
    cells <- groups
    design_label <- "Kruskal-Wallis test"
    method_note <- "Approximate Kruskal-Wallis sample size using Cohen's f and noncentral chi-square distribution."
    power_for_total <- function(total_n) {
      lambda <- total_n * effect_size^2
      crit <- stats::qchisq(1 - alpha, df1)
      stats::pchisq(crit, df1, ncp = lambda, lower.tail = FALSE)
    }
  } else if (identical(design, "friedman")) {
    measurements <- as.integer(measurements)
    if (!is.finite(measurements) || measurements < 3) {
      stop("Number of measurements must be at least 3.", call. = FALSE)
    }
    if (!is.finite(effect_size) || effect_size <= 0 || effect_size > 1) {
      stop("Kendall's W must be greater than 0 and less than or equal to 1.", call. = FALSE)
    }
    df1 <- measurements - 1
    cells <- 1
    design_label <- "Friedman test"
    method_note <- "Approximate Friedman test using Kendall's W and noncentral chi-square distribution."
    power_for_total <- function(total_n) {
      lambda <- total_n * df1 * effect_size
      crit <- stats::qchisq(1 - alpha, df1)
      stats::pchisq(crit, df1, ncp = lambda, lower.tail = FALSE)
    }
  } else if (identical(design, "repeated_one_group")) {
    measurements <- as.integer(measurements)
    if (!is.finite(measurements) || measurements < 2) {
      stop("Number of measurements must be at least 2.", call. = FALSE)
    }
    cells <- 1
    df1_base <- measurements - 1
    df1 <- df1_base * epsilon
    design_label <- "One-group repeated-measures ANOVA"
    method_note <- "Approximate one-group repeated-measures ANOVA using Cohen's f, average repeated-measures correlation, epsilon, and noncentral F distribution."
    power_for_total <- function(total_n) {
      df2 <- (total_n - 1) * df1_base * epsilon
      if (df2 <= 0) return(0)
      lambda <- total_n * measurements * effect_size^2 * epsilon / max(1e-8, 1 - repeated_correlation)
      fcrit <- stats::qf(1 - alpha, df1, df2)
      stats::pf(fcrit, df1, df2, ncp = lambda, lower.tail = FALSE)
    }
  } else if (identical(design, "mixed_repeated")) {
    groups <- as.integer(groups)
    measurements <- as.integer(measurements)
    if (!is.finite(groups) || groups < 2) {
      stop("Number of groups must be at least 2.", call. = FALSE)
    }
    if (!is.finite(measurements) || measurements < 2) {
      stop("Number of measurements must be at least 2.", call. = FALSE)
    }
    cells <- groups
    df1 <- switch(
      effect,
      group = groups - 1,
      time = (measurements - 1) * epsilon,
      (groups - 1) * (measurements - 1) * epsilon
    )
    design_label <- "Mixed repeated-measures ANOVA"
    method_note <- "Approximate mixed repeated-measures ANOVA using Cohen's f, average repeated-measures correlation, epsilon, and noncentral F distribution."
    power_for_total <- function(total_n) {
      if (identical(effect, "group")) {
        df2 <- total_n - groups
        lambda <- total_n * effect_size^2
      } else {
        df2 <- (total_n - groups) * (measurements - 1) * epsilon
        lambda <- total_n * measurements * effect_size^2 * epsilon / max(1e-8, 1 - repeated_correlation)
      }
      if (df2 <= 0) return(0)
      fcrit <- stats::qf(1 - alpha, df1, df2)
      stats::pf(fcrit, df1, df2, ncp = lambda, lower.tail = FALSE)
    }
  } else {
    if (identical(design, "two_way")) {
      factor_a_levels <- as.integer(factor_a_levels)
      factor_b_levels <- as.integer(factor_b_levels)
      if (!is.finite(factor_a_levels) || factor_a_levels < 2 || !is.finite(factor_b_levels) || factor_b_levels < 2) {
        stop("Both factors must have at least 2 levels.", call. = FALSE)
      }
      cells <- factor_a_levels * factor_b_levels
      df1 <- switch(
        effect,
        main_a = factor_a_levels - 1,
        main_b = factor_b_levels - 1,
        (factor_a_levels - 1) * (factor_b_levels - 1)
      )
      design_label <- "Two-way ANOVA"
      method_note <- "Balanced fixed-effects two-way ANOVA using Cohen's f and noncentral F distribution."
    } else {
      groups <- as.integer(groups)
      if (!is.finite(groups) || groups < 2) {
        stop("Number of groups must be at least 2.", call. = FALSE)
      }
      cells <- groups
      df1 <- groups - 1
      design_label <- "One-way ANOVA"
      method_note <- "One-way fixed-effects ANOVA using Cohen's f and noncentral F distribution."
    }
    power_for_total <- function(total_n) {
      df2 <- total_n - cells
      if (df2 <= 0) return(0)
      lambda <- total_n * effect_size^2
      fcrit <- stats::qf(1 - alpha, df1, df2)
      stats::pf(fcrit, df1, df2, ncp = lambda, lower.tail = FALSE)
    }
  }

  if (identical(target, "sample_size")) {
    sample_size_validate_probability(power, "Power")
    lower <- cells + 2
    upper <- max(cells + 10, 20)
    while (power_for_total(upper) < power && upper < 1e6) upper <- upper * 2
    total <- sample_size_round_up(stats::uniroot(function(x) power_for_total(x) - power, c(lower, upper))$root)
    per_cell <- sample_size_round_up(total / cells)
    adjusted_per_cell <- sample_size_drop_adjust(per_cell, dropout)
    result <- list(
      design_label = design_label,
      total = per_cell * cells,
      adjusted_total = adjusted_per_cell * cells,
      dropout_rate = dropout,
      method_note = method_note
    )
    if (identical(design, "two_way")) {
      result$per_cell <- per_cell
      result$adjusted_per_cell <- adjusted_per_cell
    } else if (identical(design, "mixed_repeated")) {
      result$per_group <- per_cell
      result$adjusted_per_group <- adjusted_per_cell
    } else if (identical(design, "repeated_one_group") || identical(design, "friedman")) {
      result$total_label <- "Participants"
      result$adjusted_total_label <- "Participants with dropout"
    } else {
      result$per_group <- per_cell
      result$adjusted_per_group <- adjusted_per_cell
    }
    return(result)
  }

  sample_size_validate_positive(n, "Total sample size")
  list(power = power_for_total(n), design_label = design_label, method_note = method_note)
}

sample_size_ancova <- function(
  target,
  design = "ancova",
  groups = 2,
  outcomes = 2,
  effect_size,
  covariates = 1,
  covariate_r2 = 0.3,
  alpha,
  power = NULL,
  n = NULL,
  dropout = 0
) {
  groups <- as.integer(groups)
  covariates <- as.integer(covariates)
  outcomes <- as.integer(outcomes)
  if (!is.finite(groups) || groups < 2) stop("Number of groups must be at least 2.", call. = FALSE)
  if (!is.finite(covariates) || covariates < 0) stop("Number of covariates must be 0 or greater.", call. = FALSE)
  sample_size_validate_positive(effect_size, if (identical(design, "manova")) "Effect size" else "Effect size f")
  sample_size_validate_probability(alpha, "Alpha")

  if (identical(design, "manova")) {
    if (!is.finite(outcomes) || outcomes < 2) stop("Number of outcomes must be at least 2.", call. = FALSE)
    if (!is.finite(effect_size) || effect_size <= 0 || effect_size >= 1) {
      stop("Pillai's trace V must be greater than 0 and less than 1.", call. = FALSE)
    }
    cells <- groups
    df1 <- outcomes * (groups - 1)
    f2 <- effect_size / max(1e-8, 1 - effect_size)
    design_label <- "MANOVA"
    method_note <- "Approximate MANOVA power using Pillai's trace transformed to an F-style noncentrality parameter."
    power_for_total <- function(total_n) {
      df2 <- total_n - groups - outcomes
      if (df2 <= 0) return(0)
      lambda <- total_n * f2
      fcrit <- stats::qf(1 - alpha, df1, df2)
      stats::pf(fcrit, df1, df2, ncp = lambda, lower.tail = FALSE)
    }
  } else {
    if (!is.finite(covariate_r2) || covariate_r2 < 0 || covariate_r2 >= 1) {
      stop("Covariate R-squared must be at least 0 and less than 1.", call. = FALSE)
    }
    cells <- groups
    df1 <- groups - 1
    adjusted_f <- effect_size / sqrt(max(1e-8, 1 - covariate_r2))
    rank_efficiency <- if (identical(design, "ranked_ancova")) 0.955 else 1
    design_label <- if (identical(design, "ranked_ancova")) "Ranked ANCOVA" else "ANCOVA"
    method_note <- if (identical(design, "ranked_ancova")) {
      "Rank-transform ANCOVA approximation using covariate R-squared adjustment and a Wilcoxon-style asymptotic relative efficiency factor."
    } else {
      "ANCOVA approximation using Cohen's f, covariate R-squared residual-variance adjustment, and noncentral F distribution."
    }
    power_for_total <- function(total_n) {
      df2 <- total_n - groups - covariates
      if (df2 <= 0) return(0)
      lambda <- total_n * rank_efficiency * adjusted_f^2
      fcrit <- stats::qf(1 - alpha, df1, df2)
      stats::pf(fcrit, df1, df2, ncp = lambda, lower.tail = FALSE)
    }
  }

  if (identical(target, "sample_size")) {
    sample_size_validate_probability(power, "Power")
    lower <- cells + covariates + outcomes + 2
    total <- sample_size_find_discrete_n(power_for_total, lower, power)
    per_group <- sample_size_round_up(total / groups)
    adjusted_per_group <- sample_size_drop_adjust(per_group, dropout)
    return(list(
      design_label = design_label,
      per_group = per_group,
      total = per_group * groups,
      adjusted_per_group = adjusted_per_group,
      adjusted_total = adjusted_per_group * groups,
      dropout_rate = dropout,
      estimated_power = power_for_total(per_group * groups),
      method_note = method_note
    ))
  }

  sample_size_validate_positive(n, "Total sample size")
  list(power = power_for_total(n), design_label = design_label, method_note = method_note)
}

sample_size_regression_power <- function(total_n, tested_predictors, total_predictors, effect_size, alpha) {
  df1 <- tested_predictors
  df2 <- total_n - total_predictors - 1
  if (df2 <= 0) return(0)
  lambda <- effect_size * (df1 + df2 + 1)
  fcrit <- stats::qf(1 - alpha, df1, df2)
  stats::pf(fcrit, df1, df2, ncp = lambda, lower.tail = FALSE)
}

sample_size_logistic_regression_power <- function(
  total_n,
  odds_ratio,
  p0,
  predictor_prevalence,
  covariate_r2,
  alpha,
  alternative = "two.sided"
) {
  beta <- abs(log(odds_ratio))
  information <- total_n * p0 * (1 - p0) * predictor_prevalence * (1 - predictor_prevalence) * (1 - covariate_r2)
  z_effect <- beta * sqrt(max(0, information))
  zcrit <- sample_size_z_alpha(alpha, alternative)
  stats::pnorm(z_effect - zcrit)
}

sample_size_logistic_regression <- function(
  target,
  odds_ratio,
  p0,
  predictor_prevalence = 0.5,
  covariate_r2 = 0,
  alpha,
  power = NULL,
  n = NULL,
  dropout = 0,
  alternative = "two.sided"
) {
  if (!is.finite(odds_ratio) || odds_ratio <= 0 || isTRUE(all.equal(odds_ratio, 1))) {
    stop("Odds ratio must be greater than 0 and different from 1.", call. = FALSE)
  }
  sample_size_validate_probability(p0, "Baseline event probability")
  sample_size_validate_probability(predictor_prevalence, "Predictor prevalence")
  if (!is.finite(covariate_r2) || covariate_r2 < 0 || covariate_r2 >= 1) {
    stop("Covariate R-squared must be greater than or equal to 0 and less than 1.", call. = FALSE)
  }
  sample_size_validate_probability(alpha, "Alpha")
  power_for_total <- function(total_n) {
    sample_size_logistic_regression_power(
      total_n,
      odds_ratio,
      p0,
      predictor_prevalence,
      covariate_r2,
      alpha,
      alternative
    )
  }
  method_note <- "Approximate logistic regression sample size using a Hsieh-style Wald test approximation for an odds ratio."

  if (identical(target, "sample_size")) {
    sample_size_validate_probability(power, "Power")
    total_n <- sample_size_find_discrete_n(power_for_total, 10, power)
    return(list(
      design_label = "Logistic regression",
      total = total_n,
      total_label = "Participants",
      adjusted_total = sample_size_drop_adjust(total_n, dropout),
      adjusted_total_label = "Participants with dropout",
      dropout_rate = dropout,
      method_note = method_note
    ))
  }

  sample_size_validate_positive(n, "Total sample size")
  list(power = power_for_total(n), design_label = "Logistic regression", method_note = method_note)
}

sample_size_mediation_power <- function(total_n, a_path, b_path, covariates, alpha, alternative = "two.sided") {
  df <- total_n - covariates - 3
  if (df <= 0) return(0)
  denom <- b_path^2 * max(1e-8, 1 - a_path^2) + a_path^2 * max(1e-8, 1 - b_path^2)
  if (denom <= 0) return(0)
  z_effect <- abs(a_path * b_path) * sqrt(df / denom)
  zcrit <- sample_size_z_alpha(alpha, alternative)
  stats::pnorm(z_effect - zcrit)
}

sample_size_mediation_monte_carlo_power <- function(
  total_n,
  a_path,
  b_path,
  covariates,
  alpha,
  alternative = "two.sided",
  simulations = 400,
  draws = 500,
  progress = NULL
) {
  df <- total_n - covariates - 3
  if (df <= 0) return(0)
  se_a <- sqrt(max(1e-8, 1 - a_path^2) / df)
  se_b <- sqrt(max(1e-8, 1 - b_path^2) / df)
  simulations <- max(50L, as.integer(simulations))
  draws <- max(100L, as.integer(draws))
  seed <- 830000L + as.integer(round(total_n * 10))
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    if (is.null(old_seed)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
    } else {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)

  a_hat <- stats::rnorm(simulations, mean = a_path, sd = se_a)
  b_hat <- stats::rnorm(simulations, mean = b_path, sd = se_b)
  significant <- logical(simulations)
  probs <- if (identical(alternative, "two.sided")) c(alpha / 2, 1 - alpha / 2) else c(alpha, 1 - alpha)
  for (index in seq_len(simulations)) {
    if (!is.null(progress) && (index == 1L || index == simulations || index %% max(1L, floor(simulations / 10L)) == 0L)) {
      progress(index / simulations, sprintf("Running mediation Monte Carlo %s/%s", index, simulations))
    }
    a_draws <- stats::rnorm(draws, mean = a_hat[[index]], sd = se_a)
    b_draws <- stats::rnorm(draws, mean = b_hat[[index]], sd = se_b)
    ci <- stats::quantile(a_draws * b_draws, probs = probs, names = FALSE, type = 8)
    significant[[index]] <- ci[[1]] > 0 || ci[[2]] < 0
  }
  mean(significant)
}

sample_size_mediation_bootstrap_power <- function(
  total_n,
  a_path,
  b_path,
  covariates,
  alpha,
  alternative = "two.sided",
  simulations = 30,
  bootstraps = 100,
  progress = NULL
) {
  total_n <- as.integer(round(total_n))
  if (!is.finite(total_n) || total_n <= covariates + 4) return(0)
  simulations <- max(30L, as.integer(simulations))
  bootstraps <- max(100L, as.integer(bootstraps))
  seed <- 910000L + total_n
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    if (is.null(old_seed)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
    } else {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)

  indirect_significant <- logical(simulations)
  probs <- if (identical(alternative, "two.sided")) c(alpha / 2, 1 - alpha / 2) else c(alpha, 1 - alpha)
  x <- stats::rnorm(total_n)
  covariate_matrix <- if (covariates > 0) {
    matrix(stats::rnorm(total_n * covariates), nrow = total_n, ncol = covariates)
  } else {
    NULL
  }

  estimate_indirect <- function(index) {
    x_i <- x[index]
    m_i <- mediator[index]
    y_i <- outcome[index]
    cov_i <- if (!is.null(covariate_matrix)) covariate_matrix[index, , drop = FALSE] else NULL
    m_data <- data.frame(m = m_i, x = x_i)
    y_data <- data.frame(y = y_i, m = m_i, x = x_i)
    if (!is.null(cov_i)) {
      colnames(cov_i) <- paste0("c", seq_len(ncol(cov_i)))
      m_data <- cbind(m_data, as.data.frame(cov_i))
      y_data <- cbind(y_data, as.data.frame(cov_i))
    }
    cov_terms <- if (!is.null(cov_i)) paste(colnames(cov_i), collapse = " + ") else ""
    m_formula <- stats::as.formula(paste("m ~ x", if (nzchar(cov_terms)) paste("+", cov_terms) else ""))
    y_formula <- stats::as.formula(paste("y ~ m + x", if (nzchar(cov_terms)) paste("+", cov_terms) else ""))
    a_hat <- stats::coef(stats::lm(m_formula, data = m_data))[["x"]]
    b_hat <- stats::coef(stats::lm(y_formula, data = y_data))[["m"]]
    a_hat * b_hat
  }

  for (sim_index in seq_len(simulations)) {
    if (!is.null(progress)) {
      progress((sim_index - 1L) / simulations, sprintf("Running mediation bootstrap %s/%s", sim_index, simulations))
    }
    x <- stats::rnorm(total_n)
    covariate_matrix <- if (covariates > 0) {
      matrix(stats::rnorm(total_n * covariates), nrow = total_n, ncol = covariates)
    } else {
      NULL
    }
    mediator <- a_path * x + stats::rnorm(total_n, sd = sqrt(max(1e-8, 1 - a_path^2)))
    outcome <- b_path * mediator + stats::rnorm(total_n, sd = sqrt(max(1e-8, 1 - b_path^2)))
    boot_indirect <- numeric(bootstraps)
    for (boot_index in seq_len(bootstraps)) {
      sample_index <- sample.int(total_n, total_n, replace = TRUE)
      boot_indirect[[boot_index]] <- estimate_indirect(sample_index)
    }
    ci <- stats::quantile(boot_indirect, probs = probs, names = FALSE, type = 8)
    indirect_significant[[sim_index]] <- ci[[1]] > 0 || ci[[2]] < 0
    if (!is.null(progress) && (sim_index == simulations || sim_index %% max(1L, floor(simulations / 10L)) == 0L)) {
      progress(sim_index / simulations, sprintf("Running mediation bootstrap %s/%s", sim_index, simulations))
    }
  }
  mean(indirect_significant)
}

sample_size_find_discrete_n <- function(power_for_total, lower, target_power) {
  lower <- max(2L, as.integer(ceiling(lower)))
  upper <- max(lower + 10L, 30L)
  while (power_for_total(upper) < target_power && upper < 1e6) upper <- upper * 2L
  if (upper >= 1e6 && power_for_total(upper) < target_power) {
    stop("Required sample size exceeded 1,000,000.", call. = FALSE)
  }
  while (lower < upper) {
    mid <- floor((lower + upper) / 2)
    if (power_for_total(mid) >= target_power) {
      upper <- mid
    } else {
      lower <- mid + 1L
    }
  }
  lower
}

sample_size_regression <- function(
  target,
  design = "multiple",
  effect_size = NULL,
  alpha,
  power = NULL,
  n = NULL,
  dropout = 0,
  predictors = 3,
  tested_predictors = 1,
  total_predictors = 3,
  interaction_terms = 1,
  a_path = NULL,
  b_path = NULL,
  covariates = 0,
  odds_ratio = NULL,
  p0 = NULL,
  predictor_prevalence = 0.5,
  covariate_r2 = 0,
  alternative = "two.sided",
  mediation_method = "monte_carlo",
  mediation_simulations = 30,
  mediation_bootstraps = 100,
  progress = NULL
) {
  sample_size_validate_probability(alpha, "Alpha")
  covariates <- as.integer(covariates %||% 0)
  if (!is.finite(covariates) || covariates < 0) {
    stop("Covariates must be 0 or greater.", call. = FALSE)
  }

  if (identical(design, "logistic")) {
    return(sample_size_logistic_regression(
      target = target,
      odds_ratio = odds_ratio,
      p0 = p0,
      predictor_prevalence = predictor_prevalence,
      covariate_r2 = covariate_r2,
      alpha = alpha,
      power = power,
      n = n,
      dropout = dropout,
      alternative = alternative
    ))
  } else if (identical(design, "mediation")) {
    if (!is.finite(a_path) || abs(a_path) <= 0 || abs(a_path) >= 1) {
      stop("Path a must be greater than 0 and less than 1 in absolute value.", call. = FALSE)
    }
    if (!is.finite(b_path) || abs(b_path) <= 0 || abs(b_path) >= 1) {
      stop("Path b must be greater than 0 and less than 1 in absolute value.", call. = FALSE)
    }
    if (identical(mediation_method, "sobel")) {
      power_for_total <- function(total_n) sample_size_mediation_power(total_n, a_path, b_path, covariates, alpha, alternative)
      method_note <- "Approximate mediation power using standardized a and b paths with a Sobel z approximation."
    } else if (identical(mediation_method, "bootstrap")) {
      power_for_total <- function(total_n) {
        sample_size_mediation_bootstrap_power(
          total_n,
          a_path,
          b_path,
          covariates,
          alpha,
          alternative,
          simulations = mediation_simulations,
          bootstraps = mediation_bootstraps,
          progress = progress
        )
      }
      method_note <- sprintf(
        "Bootstrap indirect effect CI simulation (%s simulations x %s bootstrap samples). This is slow and approximate.",
        as.integer(mediation_simulations),
        as.integer(mediation_bootstraps)
      )
    } else {
      power_for_total <- function(total_n) {
        sample_size_mediation_monte_carlo_power(
          total_n,
          a_path,
          b_path,
          covariates,
          alpha,
          alternative,
          progress = progress
        )
      }
      method_note <- "Approximate mediation power using Monte Carlo percentile confidence intervals for the indirect effect."
    }
    design_label <- "Mediation effect"
    lower <- covariates + 6
  } else {
    sample_size_validate_positive(effect_size, "Effect size f2")
    if (identical(design, "multiple")) {
      predictors <- as.integer(predictors)
      if (!is.finite(predictors) || predictors < 1) stop("Number of predictors must be at least 1.", call. = FALSE)
      tested <- predictors
      total <- predictors
      design_label <- "Multiple regression"
      method_note <- "Overall multiple regression R-squared test using Cohen's f2 and noncentral F distribution."
    } else if (identical(design, "hierarchical")) {
      tested <- as.integer(tested_predictors)
      total <- as.integer(total_predictors)
      design_label <- "Hierarchical regression"
      method_note <- "Incremental R-squared test using Cohen's f2 and noncentral F distribution."
    } else {
      tested <- as.integer(interaction_terms)
      total <- as.integer(total_predictors)
      design_label <- "Moderation regression"
      method_note <- "Interaction-term incremental R-squared test using Cohen's f2 and noncentral F distribution."
    }
    if (!is.finite(tested) || tested < 1) stop("Tested predictors must be at least 1.", call. = FALSE)
    if (!is.finite(total) || total < tested) stop("Total predictors must be greater than or equal to tested predictors.", call. = FALSE)
    power_for_total <- function(total_n) sample_size_regression_power(total_n, tested, total, effect_size, alpha)
    lower <- total + 3
  }

  if (identical(target, "sample_size")) {
    sample_size_validate_probability(power, "Power")
    if (identical(design, "mediation") && !identical(mediation_method, "sobel")) {
      total_n <- sample_size_find_discrete_n(power_for_total, lower, power)
    } else {
      upper <- max(lower + 10, 30)
      while (power_for_total(upper) < power && upper < 1e6) upper <- upper * 2
      total_n <- sample_size_round_up(stats::uniroot(function(x) power_for_total(x) - power, c(lower, upper))$root)
    }
    return(list(
      design_label = design_label,
      total = total_n,
      total_label = "Participants",
      adjusted_total = sample_size_drop_adjust(total_n, dropout),
      adjusted_total_label = "Participants with dropout",
      dropout_rate = dropout,
      a_path = if (identical(design, "mediation")) a_path else NULL,
      b_path = if (identical(design, "mediation")) b_path else NULL,
      indirect_effect = if (identical(design, "mediation")) a_path * b_path else NULL,
      covariates = if (identical(design, "mediation")) covariates else NULL,
      method_note = method_note
    ))
  }

  sample_size_validate_positive(n, "Total sample size")
  list(
    power = power_for_total(n),
    design_label = design_label,
    a_path = if (identical(design, "mediation")) a_path else NULL,
    b_path = if (identical(design, "mediation")) b_path else NULL,
    indirect_effect = if (identical(design, "mediation")) a_path * b_path else NULL,
    covariates = if (identical(design, "mediation")) covariates else NULL,
    method_note = method_note
  )
}

sample_size_pooled_sd <- function(n1, sd1, n2, sd2) {
  sample_size_validate_positive(n1, "Group 1 n")
  sample_size_validate_positive(sd1, "Group 1 SD")
  sample_size_validate_positive(n2, "Group 2 n")
  sample_size_validate_positive(sd2, "Group 2 SD")
  df <- n1 + n2 - 2
  if (!is.finite(df) || df <= 0) stop("Combined degrees of freedom must be greater than 0.", call. = FALSE)
  sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / df)
}

sample_size_gee_structure_label <- function(structure) {
  switch(
    structure,
    ar1 = "AR(1)",
    unstructured = "Unstructured",
    "Exchangeable"
  )
}

sample_size_gee_design_effect <- function(time_points, rho, structure = "exchangeable", correlations = NULL) {
  time_points <- as.integer(time_points)
  if (!is.finite(time_points) || time_points < 2) {
    stop("Time points must be at least 2.", call. = FALSE)
  }
  if (identical(structure, "unstructured")) {
    values <- sample_size_parse_numeric_vector(correlations, "Unstructured working correlations")
    expected <- time_points * (time_points - 1) / 2
    if (length(values) != expected) {
      stop(sprintf("Unstructured working correlations must include %d pairwise correlations for %d time points.", expected, time_points), call. = FALSE)
    }
    if (any(values < 0 | values >= 1)) {
      stop("Unstructured working correlations must be greater than or equal to 0 and less than 1.", call. = FALSE)
    }
    return(1 + 2 * sum(values) / time_points)
  }
  if (!is.finite(rho) || rho < 0 || rho >= 1) {
    stop("Working correlation rho must be greater than or equal to 0 and less than 1.", call. = FALSE)
  }
  if (identical(structure, "ar1")) {
    correlations <- vapply(seq_len(time_points - 1L), function(lag) (time_points - lag) * rho^lag, numeric(1))
    return(1 + 2 * sum(correlations) / time_points)
  }
  1 + (time_points - 1) * rho
}

sample_size_gee <- function(
  target,
  outcome = "continuous",
  effect_size = NULL,
  p1 = NULL,
  p2 = NULL,
  alpha,
  power = NULL,
  n = NULL,
  ratio = 1,
  alternative = "two.sided",
  dropout = 0,
  time_points = 3,
  rho = 0.3,
  structure = "exchangeable",
  correlations = NULL
) {
  sample_size_validate_probability(alpha, "Alpha")
  sample_size_validate_positive(ratio, "Allocation ratio")
  design_effect <- sample_size_gee_design_effect(time_points, rho, structure, correlations)
  structure_label <- sample_size_gee_structure_label(structure)
  design_label <- paste("GEE", if (identical(outcome, "binary")) "binary outcome" else "continuous outcome")
  method_note <- sprintf(
    "Approximate GEE sample size using independent-sample calculation multiplied by design effect %.3f (%s working correlation).",
    design_effect,
    structure_label
  )

  if (identical(target, "sample_size")) {
    sample_size_validate_probability(power, "Power")
    if (identical(outcome, "binary")) {
      base <- sample_size_proportion(
        target = "sample_size",
        design = "two_proportion",
        p1 = p1,
        p2 = p2,
        alpha = alpha,
        power = power,
        ratio = ratio,
        alternative = alternative,
        dropout = 0
      )
    } else {
      base <- sample_size_ttest(
        target = "sample_size",
        design = "two_sample",
        effect_size = effect_size,
        alpha = alpha,
        power = power,
        ratio = ratio,
        alternative = alternative,
        dropout = 0
      )
    }
    group1 <- sample_size_round_up(base$group1 * design_effect)
    group2 <- sample_size_round_up(base$group2 * design_effect)
    adjusted_group1 <- sample_size_drop_adjust(group1, dropout)
    adjusted_group2 <- sample_size_drop_adjust(group2, dropout)
    return(list(
      design_label = design_label,
      independent_group1 = base$group1,
      independent_group2 = base$group2,
      independent_total = base$total,
      design_effect = design_effect,
      group1 = group1,
      group2 = group2,
      total = group1 + group2,
      adjusted_group1 = adjusted_group1,
      adjusted_group2 = adjusted_group2,
      adjusted_total = adjusted_group1 + adjusted_group2,
      dropout_rate = dropout,
      method_note = method_note
    ))
  }

  sample_size_validate_positive(n, "Sample size per group")
  effective_n <- n / design_effect
  if (identical(outcome, "binary")) {
    result <- sample_size_proportion(
      target = "power",
      design = "two_proportion",
      p1 = p1,
      p2 = p2,
      alpha = alpha,
      n = effective_n,
      ratio = ratio,
      alternative = alternative
    )
  } else {
    result <- sample_size_ttest(
      target = "power",
      design = "two_sample",
      effect_size = effect_size,
      alpha = alpha,
      n = effective_n,
      ratio = ratio,
      alternative = alternative
    )
  }
  result$design_label <- design_label
  result$method_note <- method_note
  result$design_effect <- design_effect
  result
}

sample_size_lmm_power_once <- function(
  participants,
  design,
  effect_size,
  alpha,
  time_points,
  icc,
  simulations,
  progress = NULL
) {
  if (!requireNamespace("nlme", quietly = TRUE)) {
    stop("The nlme package is required for LMM sample size simulation.", call. = FALSE)
  }
  participants <- as.integer(round(participants))
  simulations <- max(20L, as.integer(simulations))
  random_sd <- sqrt(icc)
  residual_sd <- sqrt(1 - icc)
  time_value <- seq(0, 1, length.out = time_points)
  significant <- logical(simulations)

  for (sim_index in seq_len(simulations)) {
    if (!is.null(progress) && (sim_index == 1L || sim_index == simulations || sim_index %% max(1L, floor(simulations / 10L)) == 0L)) {
      progress(sim_index / simulations, sprintf("Running LMM simulations %s/%s", sim_index, simulations))
    }
    if (identical(design, "two_group_repeated")) {
      group <- rep(c(0, 1), each = participants * time_points)
      id <- rep(seq_len(participants * 2L), each = time_points)
      time <- rep(time_value, times = participants * 2L)
      random_intercept <- stats::rnorm(participants * 2L, 0, random_sd)
      y <- effect_size * group * time + random_intercept[id] + stats::rnorm(length(id), 0, residual_sd)
      data <- data.frame(y = y, group = group, time = time, id = factor(id))
      fit <- try(nlme::lme(y ~ group * time, random = ~ 1 | id, data = data, method = "REML"), silent = TRUE)
      term <- "group:time"
    } else {
      id <- rep(seq_len(participants), each = time_points)
      time <- rep(time_value, times = participants)
      random_intercept <- stats::rnorm(participants, 0, random_sd)
      y <- effect_size * time + random_intercept[id] + stats::rnorm(length(id), 0, residual_sd)
      data <- data.frame(y = y, time = time, id = factor(id))
      fit <- try(nlme::lme(y ~ time, random = ~ 1 | id, data = data, method = "REML"), silent = TRUE)
      term <- "time"
    }
    if (!inherits(fit, "try-error")) {
      coefficients <- try(summary(fit)$tTable, silent = TRUE)
      if (!inherits(coefficients, "try-error") && term %in% rownames(coefficients)) {
        significant[[sim_index]] <- is.finite(coefficients[term, "p-value"]) && coefficients[term, "p-value"] < alpha
      }
    }
  }
  mean(significant)
}

sample_size_lmm_power <- function(
  participants,
  design,
  effect_size,
  alpha,
  time_points,
  icc,
  simulations,
  progress = NULL
) {
  seed <- 720000L + as.integer(round(participants * 10)) + if (identical(design, "two_group_repeated")) 1000L else 0L
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    if (is.null(old_seed)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
    } else {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)
  sample_size_lmm_power_once(participants, design, effect_size, alpha, time_points, icc, simulations, progress = progress)
}

sample_size_parse_numeric_vector <- function(x, name) {
  if (is.numeric(x) && length(x) > 1L) return(as.numeric(x))
  values <- unlist(strsplit(as.character(x %||% ""), "[,;[:space:]]+"))
  values <- values[nzchar(values)]
  numbers <- suppressWarnings(as.numeric(values))
  if (length(numbers) == 0L || any(!is.finite(numbers))) {
    stop(sprintf("%s must be a numeric vector, for example: 0, 0.2, 0.5.", name), call. = FALSE)
  }
  numbers
}

sample_size_lmm_structure_label <- function(structure) {
  switch(
    structure,
    ar1 = "AR(1)",
    unstructured = "Unstructured",
    "Exchangeable"
  )
}

sample_size_lmm_correlation_matrix <- function(time_points, rho, structure = "exchangeable", correlations = NULL) {
  time_points <- as.integer(time_points)
  if (!is.finite(time_points) || time_points < 2L) {
    stop("Time points must be at least 2.", call. = FALSE)
  }
  if (identical(structure, "unstructured")) {
    values <- sample_size_parse_numeric_vector(correlations, "Unstructured correlations")
    expected <- time_points * (time_points - 1L) / 2L
    if (length(values) != expected) {
      stop(sprintf("Unstructured correlations must include %d pairwise correlations for %d time points.", expected, time_points), call. = FALSE)
    }
    if (any(values <= -1 | values >= 1)) {
      stop("Unstructured correlations must be greater than -1 and less than 1.", call. = FALSE)
    }
    corr <- diag(1, time_points)
    corr[upper.tri(corr)] <- values
    corr <- t(corr)
    corr[upper.tri(corr)] <- values
    if (inherits(try(chol(corr), silent = TRUE), "try-error")) {
      stop("Unstructured correlations must form a positive definite correlation matrix.", call. = FALSE)
    }
    return(corr)
  }
  sample_size_validate_probability(rho, "Correlation rho", lower = -1, upper = 1)
  index <- seq_len(time_points)
  if (identical(structure, "ar1")) {
    corr <- rho^abs(outer(index, index, "-"))
    if (inherits(try(chol(corr), silent = TRUE), "try-error")) {
      stop("Correlation rho must form a positive definite AR(1) correlation matrix.", call. = FALSE)
    }
    return(corr)
  }
  corr <- matrix(rho, nrow = time_points, ncol = time_points) + diag(1 - rho, time_points)
  if (inherits(try(chol(corr), silent = TRUE), "try-error")) {
    stop("Correlation rho must form a positive definite exchangeable correlation matrix.", call. = FALSE)
  }
  corr
}

sample_size_lmm_generate_profiles <- function(n, means, sigma) {
  z <- matrix(stats::rnorm(n * length(means)), nrow = n)
  sweep(z %*% chol(sigma), 2, means, "+")
}

sample_size_lmm_glimmpse_power_once <- function(
  participants,
  design,
  group1_means,
  group2_means,
  residual_sd,
  rho,
  structure,
  correlations,
  alpha,
  simulations,
  progress = NULL
) {
  if (!requireNamespace("nlme", quietly = TRUE)) {
    stop("The nlme package is required for LMM sample size simulation.", call. = FALSE)
  }
  participants <- as.integer(round(participants))
  simulations <- max(20L, as.integer(simulations))
  time_points <- length(group1_means)
  sigma <- residual_sd^2 * sample_size_lmm_correlation_matrix(time_points, rho, structure, correlations)
  significant <- logical(simulations)

  for (sim_index in seq_len(simulations)) {
    if (!is.null(progress) && (sim_index == 1L || sim_index == simulations || sim_index %% max(1L, floor(simulations / 10L)) == 0L)) {
      progress(sim_index / simulations, sprintf("Running GLIMMPSE-style simulations %s/%s", sim_index, simulations))
    }
    group1_y <- sample_size_lmm_generate_profiles(participants, group1_means, sigma)
    if (identical(design, "two_group_repeated")) {
      group2_y <- sample_size_lmm_generate_profiles(participants, group2_means, sigma)
      y <- as.vector(t(rbind(group1_y, group2_y)))
      id <- rep(seq_len(participants * 2L), each = time_points)
      group <- factor(rep(c("Group 1", "Group 2"), each = participants * time_points))
      term <- "group:time_factor"
    } else {
      y <- as.vector(t(group1_y))
      id <- rep(seq_len(participants), each = time_points)
      group <- NULL
      term <- "time_factor"
    }
    data <- data.frame(
      y = y,
      id = factor(id),
      time_index = rep(seq_len(time_points), times = if (identical(design, "two_group_repeated")) participants * 2L else participants),
      time_factor = factor(rep(seq_len(time_points), times = if (identical(design, "two_group_repeated")) participants * 2L else participants))
    )
    if (!is.null(group)) data$group <- group
    correlation <- if (identical(structure, "ar1")) {
      nlme::corAR1(value = rho, form = ~ time_index | id)
    } else if (identical(structure, "unstructured")) {
      nlme::corSymm(value = sample_size_parse_numeric_vector(correlations, "Unstructured correlations"), form = ~ time_index | id)
    } else {
      nlme::corCompSymm(value = rho, form = ~ 1 | id)
    }
    formula <- if (identical(design, "two_group_repeated")) y ~ group * time_factor else y ~ time_factor
    fit <- try(
      nlme::gls(
        formula,
        data = data,
        correlation = correlation,
        method = "REML",
        control = nlme::glsControl(msMaxIter = 60, returnObject = TRUE)
      ),
      silent = TRUE
    )
    if (!inherits(fit, "try-error")) {
      tests <- try(stats::anova(fit), silent = TRUE)
      if (!inherits(tests, "try-error") && term %in% rownames(tests) && "p-value" %in% colnames(tests)) {
        p_value <- tests[term, "p-value"]
        significant[[sim_index]] <- is.finite(p_value) && p_value < alpha
      }
    }
  }
  mean(significant)
}

sample_size_lmm_glimmpse_power <- function(
  participants,
  design,
  group1_means,
  group2_means,
  residual_sd,
  rho,
  structure,
  correlations,
  alpha,
  simulations,
  progress = NULL
) {
  seed <- 760000L + as.integer(round(participants * 10)) + if (identical(design, "two_group_repeated")) 1000L else 0L
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    if (is.null(old_seed)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
    } else {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)
  sample_size_lmm_glimmpse_power_once(
    participants,
    design,
    group1_means,
    group2_means,
    residual_sd,
    rho,
    structure,
    correlations,
    alpha,
    simulations,
    progress = progress
  )
}

sample_size_lmm <- function(
  target,
  mode = "simple",
  design = "two_group_repeated",
  effect_size = NULL,
  alpha,
  power = NULL,
  n = NULL,
  dropout = 0,
  time_points = 3,
  icc = 0.3,
  simulations = 100,
  group1_means = NULL,
  group2_means = NULL,
  residual_sd = 1,
  rho = 0.5,
  structure = "exchangeable",
  correlations = NULL,
  progress = NULL
) {
  sample_size_validate_probability(alpha, "Alpha")
  simulations <- as.integer(simulations)
  if (!is.finite(simulations) || simulations < 20) {
    stop("Simulations must be at least 20.", call. = FALSE)
  }

  if (identical(mode, "glimmpse")) {
    group1_means <- sample_size_parse_numeric_vector(group1_means, "Group 1 means")
    if (length(group1_means) < 2L) stop("Group 1 means must include at least two time points.", call. = FALSE)
    time_points <- length(group1_means)
    if (identical(design, "two_group_repeated")) {
      group2_means <- sample_size_parse_numeric_vector(group2_means, "Group 2 means")
      if (length(group2_means) != time_points) {
        stop("Group 2 means must have the same number of time points as Group 1 means.", call. = FALSE)
      }
    } else {
      group2_means <- group1_means
    }
    sample_size_validate_positive(residual_sd, "Residual SD")
    sample_size_lmm_correlation_matrix(time_points, rho, structure, correlations)
    structure_label <- sample_size_lmm_structure_label(structure)
    design_label <- if (identical(design, "one_group_repeated")) {
      "LMM GLIMMPSE-style one-group repeated measures"
    } else {
      "LMM GLIMMPSE-style two-group repeated measures"
    }
    method_note <- sprintf(
      "GLIMMPSE-style mean/covariance simulation using nlme::gls with %s repeated-measures correlation (%s simulations).",
      structure_label,
      simulations
    )
    power_function <- function(participants, progress = NULL) {
      sample_size_lmm_glimmpse_power(
        participants,
        design,
        group1_means,
        group2_means,
        residual_sd,
        rho,
        structure,
        correlations,
        alpha,
        simulations,
        progress = progress
      )
    }
  } else {
    sample_size_validate_positive(abs(effect_size), "Effect size")
    sample_size_validate_probability(icc, "ICC", lower = 0, upper = 1)
    time_points <- as.integer(time_points)
    if (!is.finite(time_points) || time_points < 2) {
      stop("Time points must be at least 2.", call. = FALSE)
    }
    design_label <- if (identical(design, "one_group_repeated")) {
      "LMM one-group repeated measures"
    } else {
      "LMM two-group repeated measures"
    }
    method_note <- sprintf(
      "Simulation-based LMM power using nlme::lme with random intercepts (%s simulations). Results depend on variance assumptions, ICC, time points, and model structure.",
      simulations
    )
    power_function <- function(participants, progress = NULL) {
      sample_size_lmm_power(participants, design, effect_size, alpha, time_points, icc, simulations, progress = progress)
    }
  }

  n_label <- if (identical(design, "two_group_repeated")) "Participants per group" else "Participants"
  power_cache <- new.env(parent = emptyenv())
  evaluation_index <- 0L
  max_evaluations <- if (identical(target, "sample_size")) 20 else 1
  power_for_participants <- function(participants) {
    key <- as.character(as.integer(round(participants)))
    if (!exists(key, envir = power_cache, inherits = FALSE)) {
      evaluation_index <<- evaluation_index + 1L
      eval_start <- min(0.94, 0.02 + (evaluation_index - 1L) * 0.92 / max_evaluations)
      eval_span <- 0.92 / max_evaluations
      nested_progress <- NULL
      if (!is.null(progress)) {
        nested_progress <- function(value, text) {
          progress(min(0.98, eval_start + eval_span * value), sprintf("%s (n = %s)", text, key))
        }
      }
      assign(key, power_function(participants, progress = nested_progress), envir = power_cache)
    }
    get(key, envir = power_cache, inherits = FALSE)
  }

  if (identical(target, "sample_size")) {
    sample_size_validate_probability(power, "Power")
    lower <- 4L
    upper <- 12L
    while (power_for_participants(upper) < power && upper < 5000L) upper <- upper * 2L
    if (upper >= 5000L && power_for_participants(upper) < power) {
      stop("Required sample size exceeded 5,000 participants per unit.", call. = FALSE)
    }
    while (lower < upper) {
      mid <- floor((lower + upper) / 2L)
      if (power_for_participants(mid) >= power) upper <- mid else lower <- mid + 1L
    }
    participants <- lower
    adjusted_participants <- sample_size_drop_adjust(participants, dropout)
    result <- list(
      design_label = design_label,
      total_observations = participants * time_points * if (identical(design, "two_group_repeated")) 2L else 1L,
      adjusted_total_observations = adjusted_participants * time_points * if (identical(design, "two_group_repeated")) 2L else 1L,
      dropout_rate = dropout,
      estimated_power = power_for_participants(participants),
      method_note = method_note
    )
    if (identical(design, "two_group_repeated")) {
      result$group1 <- participants
      result$group2 <- participants
      result$total <- participants * 2L
      result$adjusted_group1 <- adjusted_participants
      result$adjusted_group2 <- adjusted_participants
      result$adjusted_total <- adjusted_participants * 2L
    } else {
      result$total <- participants
      result$total_label <- n_label
      result$adjusted_total <- adjusted_participants
      result$adjusted_total_label <- "Participants with dropout"
    }
    return(result)
  }

  sample_size_validate_positive(n, n_label)
  list(
    power = power_for_participants(n),
    design_label = design_label,
    total_observations = as.integer(round(n)) * time_points * if (identical(design, "two_group_repeated")) 2L else 1L,
    method_note = method_note
  )
}

sample_size_survival <- function(
  target,
  hazard_ratio,
  event_probability,
  alpha,
  power = NULL,
  n = NULL,
  ratio = 1,
  alternative = "two.sided",
  dropout = 0
) {
  if (!is.finite(hazard_ratio) || hazard_ratio <= 0 || isTRUE(all.equal(hazard_ratio, 1))) {
    stop("Hazard ratio must be greater than 0 and different from 1.", call. = FALSE)
  }
  sample_size_validate_probability(event_probability, "Overall event probability")
  sample_size_validate_probability(alpha, "Alpha")
  sample_size_validate_positive(ratio, "Allocation ratio")
  allocation_group1 <- 1 / (1 + ratio)
  allocation_group2 <- ratio / (1 + ratio)
  information_fraction <- allocation_group1 * allocation_group2
  log_hr <- abs(log(hazard_ratio))
  schoenfeld_signal <- log_hr * sqrt(event_probability * information_fraction)
  za <- sample_size_z_alpha(alpha, alternative)
  design_label <- "Log-rank / Cox proportional hazards"
  method_note <- "Schoenfeld event-based log-rank/Cox approximation using hazard ratio, allocation ratio, and overall event probability."

  if (identical(target, "sample_size")) {
    sample_size_validate_probability(power, "Power")
    required_events <- sample_size_round_up((za + stats::qnorm(power))^2 / (information_fraction * log_hr^2))
    total <- sample_size_round_up(required_events / event_probability)
    group1 <- sample_size_round_up(total * allocation_group1)
    group2 <- sample_size_round_up(total * allocation_group2)
    adjusted_group1 <- sample_size_drop_adjust(group1, dropout)
    adjusted_group2 <- sample_size_drop_adjust(group2, dropout)
    return(list(
      design_label = design_label,
      required_events = required_events,
      group1 = group1,
      group2 = group2,
      total = group1 + group2,
      adjusted_group1 = adjusted_group1,
      adjusted_group2 = adjusted_group2,
      adjusted_total = adjusted_group1 + adjusted_group2,
      dropout_rate = dropout,
      hazard_ratio = hazard_ratio,
      log_hazard_ratio = log(hazard_ratio),
      event_probability = event_probability,
      allocation_ratio = ratio,
      information_fraction = information_fraction,
      schoenfeld_signal = schoenfeld_signal,
      method_note = method_note
    ))
  }

  sample_size_validate_positive(n, "Total sample size")
  observed_events <- n * event_probability
  achieved_power <- stats::pnorm(sqrt(observed_events * information_fraction) * log_hr - za)
  list(
    power = achieved_power,
    design_label = design_label,
    required_events = observed_events,
    hazard_ratio = hazard_ratio,
    log_hazard_ratio = log(hazard_ratio),
    event_probability = event_probability,
    allocation_ratio = ratio,
    information_fraction = information_fraction,
    schoenfeld_signal = schoenfeld_signal,
    method_note = method_note
  )
}

sample_size_equivalence <- function(
  target,
  outcome = "mean",
  objective = "noninferiority",
  true_difference = 0,
  margin,
  sd = NULL,
  p1 = NULL,
  p2 = NULL,
  alpha,
  power = NULL,
  n = NULL,
  ratio = 1,
  dropout = 0
) {
  sample_size_validate_positive(margin, "Margin")
  sample_size_validate_probability(alpha, "Alpha")
  sample_size_validate_positive(ratio, "Allocation ratio")
  group_variance_multiplier <- 1 + 1 / ratio
  is_equivalence <- identical(objective, "equivalence")
  objective_label <- if (is_equivalence) "Equivalence" else "Non-inferiority"

  if (identical(outcome, "proportion")) {
    sample_size_validate_probability(p1, "Expected proportion 1")
    sample_size_validate_probability(p2, "Expected proportion 2")
    true_difference <- p1 - p2
    variance <- p1 * (1 - p1) + p2 * (1 - p2) / ratio
    outcome_label <- "two proportions"
  } else {
    sample_size_validate_positive(sd, "SD")
    variance <- sd^2 * group_variance_multiplier
    outcome_label <- "two means"
  }

  if (is_equivalence) {
    distance <- margin - abs(true_difference)
    if (!is.finite(distance) || distance <= 0) {
      stop("Expected true difference must be inside the equivalence margin.", call. = FALSE)
    }
    za <- stats::qnorm(1 - alpha)
    design_label <- paste(objective_label, outcome_label)
    method_note <- "Approximate TOST equivalence sample size using normal approximation for the difference."
  } else {
    distance <- margin + true_difference
    if (!is.finite(distance) || distance <= 0) {
      stop("Expected true difference must be above the non-inferiority boundary.", call. = FALSE)
    }
    za <- stats::qnorm(1 - alpha)
    design_label <- paste(objective_label, outcome_label)
    method_note <- "Approximate one-sided non-inferiority sample size using normal approximation for the difference."
  }

  if (identical(target, "sample_size")) {
    sample_size_validate_probability(power, "Power")
    group1 <- sample_size_round_up(((za + stats::qnorm(power))^2 * variance) / distance^2)
    group2 <- sample_size_round_up(group1 * ratio)
    adjusted_group1 <- sample_size_drop_adjust(group1, dropout)
    adjusted_group2 <- sample_size_drop_adjust(group2, dropout)
    return(list(
      design_label = design_label,
      group1 = group1,
      group2 = group2,
      total = group1 + group2,
      adjusted_group1 = adjusted_group1,
      adjusted_group2 = adjusted_group2,
      adjusted_total = adjusted_group1 + adjusted_group2,
      dropout_rate = dropout,
      method_note = method_note
    ))
  }

  sample_size_validate_positive(n, "Sample size per group")
  achieved_power <- stats::pnorm(distance * sqrt(n / variance) - za)
  list(power = achieved_power, design_label = design_label, method_note = method_note)
}

sample_size_auc_variance_hanley_mcneil <- function(auc, n_cases, n_controls) {
  q1 <- auc / (2 - auc)
  q2 <- 2 * auc^2 / (1 + auc)
  (auc * (1 - auc) + (n_cases - 1) * (q1 - auc^2) + (n_controls - 1) * (q2 - auc^2)) / (n_cases * n_controls)
}

sample_size_diagnostic <- function(
  target,
  design = "sensitivity",
  sensitivity = NULL,
  specificity = NULL,
  prevalence = NULL,
  precision = NULL,
  auc = NULL,
  null_auc = 0.5,
  alpha,
  power = NULL,
  n = NULL,
  ratio = 1,
  dropout = 0
) {
  sample_size_validate_probability(alpha, "Alpha")
  z_alpha <- stats::qnorm(1 - alpha / 2)

  if (design %in% c("sensitivity", "specificity")) {
    sample_size_validate_probability(prevalence, "Prevalence")
    sample_size_validate_positive(precision, "Precision")
    if (precision >= 1) stop("Precision must be less than 1.", call. = FALSE)
    if (identical(design, "sensitivity")) {
      sample_size_validate_probability(sensitivity, "Sensitivity")
      target_probability <- sensitivity
      disease_fraction <- prevalence
      design_label <- "Sensitivity precision"
      method_note <- "Buderer precision-based diagnostic accuracy sample size for sensitivity."
    } else {
      sample_size_validate_probability(specificity, "Specificity")
      target_probability <- specificity
      disease_fraction <- 1 - prevalence
      design_label <- "Specificity precision"
      method_note <- "Buderer precision-based diagnostic accuracy sample size for specificity."
    }

    if (identical(target, "sample_size")) {
      diseased_or_nondiseased <- sample_size_round_up(z_alpha^2 * target_probability * (1 - target_probability) / precision^2)
      total <- sample_size_round_up(diseased_or_nondiseased / disease_fraction)
      adjusted_total <- sample_size_drop_adjust(total, dropout)
      return(list(
        design_label = design_label,
        required_events = diseased_or_nondiseased,
        total = total,
        total_label = "Total participants",
        adjusted_total = adjusted_total,
        adjusted_total_label = "Total participants with dropout",
        dropout_rate = dropout,
        method_note = method_note
      ))
    }

    sample_size_validate_positive(n, "Total sample size")
    available <- n * disease_fraction
    achieved_precision <- z_alpha * sqrt(target_probability * (1 - target_probability) / available)
    return(list(
      power = NA_real_,
      design_label = design_label,
      required_events = available,
      method_note = sprintf("%s Achieved half-width is approximately %.3f.", method_note, achieved_precision)
    ))
  }

  sample_size_validate_probability(auc, "Expected AUC")
  sample_size_validate_probability(null_auc, "Null AUC")
  sample_size_validate_positive(ratio, "Control/case ratio")
  if (auc <= null_auc) stop("Expected AUC must be greater than null AUC.", call. = FALSE)
  design_label <- "ROC AUC vs null"
  method_note <- "Hanley-McNeil normal approximation for testing one ROC AUC against a null AUC."
  power_for_cases <- function(n_cases) {
    n_controls <- n_cases * ratio
    se <- sqrt(sample_size_auc_variance_hanley_mcneil(auc, n_cases, n_controls))
    stats::pnorm((auc - null_auc) / se - z_alpha)
  }

  if (identical(target, "sample_size")) {
    sample_size_validate_probability(power, "Power")
    cases <- sample_size_find_discrete_n(power_for_cases, 5, power)
    controls <- sample_size_round_up(cases * ratio)
    adjusted_cases <- sample_size_drop_adjust(cases, dropout)
    adjusted_controls <- sample_size_drop_adjust(controls, dropout)
    return(list(
      design_label = design_label,
      group1 = cases,
      group2 = controls,
      total = cases + controls,
      adjusted_group1 = adjusted_cases,
      adjusted_group2 = adjusted_controls,
      adjusted_total = adjusted_cases + adjusted_controls,
      dropout_rate = dropout,
      method_note = method_note
    ))
  }

  sample_size_validate_positive(n, "Number of cases")
  list(power = power_for_cases(n), design_label = design_label, method_note = method_note)
}

sample_size_precision <- function(
  target,
  parameter = "mean",
  confidence_level = 0.95,
  half_width = NULL,
  sd = NULL,
  proportion = NULL,
  r = NULL,
  n = NULL,
  dropout = 0
) {
  sample_size_validate_probability(confidence_level, "Confidence level")
  alpha <- 1 - confidence_level
  z <- stats::qnorm(1 - alpha / 2)

  if (identical(parameter, "mean")) {
    sample_size_validate_positive(sd, "SD")
    design_label <- "Mean CI precision"
    method_note <- "Normal-approximation sample size for estimating a mean with a specified confidence interval half-width."
    half_width_for_n <- function(total_n) z * sd / sqrt(total_n)
  } else if (identical(parameter, "proportion")) {
    sample_size_validate_probability(proportion, "Expected proportion")
    design_label <- "Proportion CI precision"
    method_note <- "Wald normal-approximation sample size for estimating a proportion with a specified confidence interval half-width."
    half_width_for_n <- function(total_n) z * sqrt(proportion * (1 - proportion) / total_n)
  } else {
    if (!is.finite(r) || abs(r) >= 1) {
      stop("Expected r must be finite and less than 1 in absolute value.", call. = FALSE)
    }
    design_label <- "Correlation CI precision"
    method_note <- "Fisher z-transformation sample size for estimating Pearson correlation with an approximate confidence interval half-width."
    half_width_for_n <- function(total_n) {
      if (total_n <= 3) return(Inf)
      tanh(atanh(r) + z / sqrt(total_n - 3)) - r
    }
  }

  if (!identical(target, "sample_size")) {
    sample_size_validate_positive(n, "Sample size")
    return(list(
      design_label = design_label,
      achieved_half_width = half_width_for_n(n),
      method_note = method_note
    ))
  }

  sample_size_validate_positive(half_width, "Desired CI half-width")
  if (identical(parameter, "correlation")) {
    z_half <- atanh(min(0.999999, r + half_width)) - atanh(r)
    if (!is.finite(z_half) || z_half <= 0) {
      stop("Desired CI half-width is not valid for the expected correlation.", call. = FALSE)
    }
    total <- sample_size_round_up((z / z_half)^2 + 3)
  } else {
    total <- sample_size_find_discrete_n(function(total_n) half_width / half_width_for_n(total_n), 2, 1)
  }

  list(
    design_label = design_label,
    total = total,
    total_label = "Participants",
    adjusted_total = sample_size_drop_adjust(total, dropout),
    adjusted_total_label = "Participants with dropout",
    dropout_rate = dropout,
    method_note = method_note
  )
}

sample_size_mcnemar <- function(
  target,
  p01,
  p10,
  alpha,
  power = NULL,
  n = NULL,
  alternative = "two.sided",
  dropout = 0
) {
  sample_size_validate_probability(p01, "p01 discordant proportion")
  sample_size_validate_probability(p10, "p10 discordant proportion")
  discordant <- p01 + p10
  difference <- abs(p01 - p10)
  sample_size_validate_positive(discordant, "Total discordant proportion")
  sample_size_validate_positive(difference, "Discordant proportion difference")
  if (discordant >= 1) stop("p01 + p10 must be less than 1.", call. = FALSE)
  za <- sample_size_z_alpha(alpha, alternative)
  design_label <- "McNemar paired binary"
  method_note <- "Normal approximation for McNemar's paired binary test using discordant pair proportions p01 and p10."
  power_for_n <- function(total_n) {
    stats::pnorm(difference * sqrt(total_n) / sqrt(discordant) - za)
  }

  if (identical(target, "sample_size")) {
    sample_size_validate_probability(power, "Power")
    total <- sample_size_round_up(((za + stats::qnorm(power))^2 * discordant) / difference^2)
    return(list(
      design_label = design_label,
      total = total,
      total_label = "Pairs",
      adjusted_total = sample_size_drop_adjust(total, dropout),
      adjusted_total_label = "Pairs with dropout",
      dropout_rate = dropout,
      method_note = method_note
    ))
  }

  sample_size_validate_positive(n, "Pairs")
  list(power = power_for_n(n), design_label = design_label, method_note = method_note)
}

sample_size_rates <- function(
  target,
  design = "two_rate_ratio",
  rate1,
  rate2 = NULL,
  alpha,
  power = NULL,
  n = NULL,
  ratio = 1,
  alternative = "two.sided",
  dropout = 0,
  half_width = NULL,
  dispersion = 0
) {
  sample_size_validate_positive(rate1, "Rate 1")
  sample_size_validate_probability(alpha, "Alpha")
  sample_size_validate_positive(ratio, "Person-time ratio")
  za <- sample_size_z_alpha(alpha, alternative)

  if (identical(design, "single_rate_precision")) {
    sample_size_validate_positive(half_width, "Desired CI half-width")
    design_label <- "Single Poisson rate precision"
    method_note <- "Normal approximation for estimating one Poisson incidence rate with a specified confidence interval half-width."
    if (identical(target, "sample_size")) {
      person_time <- (za^2 * rate1) / half_width^2
      adjusted_person_time <- person_time / (1 - dropout)
      sample_size_drop_adjust(1, dropout)
      return(list(
        design_label = design_label,
        total = ceiling(person_time),
        total_label = "Person-time",
        adjusted_total = ceiling(adjusted_person_time),
        adjusted_total_label = "Person-time with dropout",
        dropout_rate = dropout,
        method_note = method_note
      ))
    }
    sample_size_validate_positive(n, "Person-time")
    achieved_half_width <- za * sqrt(rate1 / n)
    return(list(design_label = design_label, achieved_half_width = achieved_half_width, method_note = method_note))
  }

  sample_size_validate_positive(rate2, "Rate 2")
  if (isTRUE(all.equal(rate1, rate2))) stop("Rate 1 and Rate 2 must be different.", call. = FALSE)
  if (!is.finite(dispersion) || dispersion < 0) stop("Dispersion must be greater than or equal to 0.", call. = FALSE)
  difference <- abs(rate1 - rate2)
  if (identical(design, "negative_binomial")) {
    variance <- (rate1 + dispersion * rate1^2) + (rate2 + dispersion * rate2^2) / ratio
    design_label <- "Two negative binomial rates"
    method_note <- "Zhu-Lakkis style Wald approximation for comparing two negative binomial rates with overdispersion."
  } else {
    variance <- rate1 + rate2 / ratio
    design_label <- "Two Poisson rates"
    method_note <- "Wald normal approximation for comparing two independent Poisson incidence rates with person-time allocation ratio."
  }
  power_for_time <- function(person_time1) {
    stats::pnorm(difference * sqrt(person_time1 / variance) - za)
  }

  if (identical(target, "sample_size")) {
    sample_size_validate_probability(power, "Power")
    time1 <- ((za + stats::qnorm(power))^2 * variance) / difference^2
    time2 <- time1 * ratio
    adjusted_time1 <- time1 / (1 - dropout)
    adjusted_time2 <- time2 / (1 - dropout)
    sample_size_drop_adjust(1, dropout)
    return(list(
      design_label = design_label,
      group1 = ceiling(time1),
      group2 = ceiling(time2),
      total = ceiling(time1 + time2),
      total_label = "Total person-time",
      adjusted_group1 = ceiling(adjusted_time1),
      adjusted_group2 = ceiling(adjusted_time2),
      adjusted_total = ceiling(adjusted_time1 + adjusted_time2),
      adjusted_total_label = "Total person-time with dropout",
      dropout_rate = dropout,
      method_note = method_note
    ))
  }

  sample_size_validate_positive(n, "Person-time in Group 1")
  list(power = power_for_time(n), design_label = design_label, method_note = method_note)
}

sample_size_cluster <- function(
  target,
  design = "parallel",
  outcome = "continuous",
  effect_size = NULL,
  p1 = NULL,
  p2 = NULL,
  alpha,
  power = NULL,
  n = NULL,
  ratio = 1,
  alternative = "two.sided",
  dropout = 0,
  cluster_size = 20,
  icc = 0.05,
  periods = 5,
  simulations = 100,
  progress = NULL
) {
  sample_size_validate_positive(cluster_size, "Cluster size")
  sample_size_validate_probability(icc, "ICC", lower = 0, upper = 1)
  if (identical(design, "stepped_wedge")) {
    return(sample_size_stepped_wedge(
      target = target,
      effect_size = effect_size,
      alpha = alpha,
      power = power,
      n = n,
      dropout = dropout,
      cluster_size = cluster_size,
      icc = icc,
      periods = periods,
      simulations = simulations,
      progress = progress
    ))
  }
  design_effect <- 1 + (cluster_size - 1) * icc
  design_label <- if (identical(outcome, "binary")) "Cluster trial binary outcome" else "Cluster trial continuous outcome"
  method_note <- sprintf(
    "Parallel cluster randomized trial using individual-randomized sample size multiplied by design effect %.3f = 1 + (m - 1) ICC.",
    design_effect
  )

  if (identical(target, "sample_size")) {
    sample_size_validate_probability(power, "Power")
    if (identical(outcome, "binary")) {
      base <- sample_size_proportion("sample_size", "two_proportion", p1, p2, alpha, power, ratio = ratio, alternative = alternative, dropout = 0)
    } else {
      base <- sample_size_ttest("sample_size", "two_sample", effect_size, alpha, power, ratio = ratio, alternative = alternative, dropout = 0)
    }
    group1 <- sample_size_round_up(base$group1 * design_effect)
    group2 <- sample_size_round_up(base$group2 * design_effect)
    clusters_group1 <- sample_size_round_up(group1 / cluster_size)
    clusters_group2 <- sample_size_round_up(group2 / cluster_size)
    group1 <- clusters_group1 * ceiling(cluster_size)
    group2 <- clusters_group2 * ceiling(cluster_size)
    adjusted_group1 <- sample_size_drop_adjust(group1, dropout)
    adjusted_group2 <- sample_size_drop_adjust(group2, dropout)
    return(list(
      design_label = design_label,
      independent_group1 = base$group1,
      independent_group2 = base$group2,
      independent_total = base$total,
      design_effect = design_effect,
      group1 = group1,
      group2 = group2,
      clusters_group1 = clusters_group1,
      clusters_group2 = clusters_group2,
      total = group1 + group2,
      adjusted_group1 = adjusted_group1,
      adjusted_group2 = adjusted_group2,
      adjusted_total = adjusted_group1 + adjusted_group2,
      dropout_rate = dropout,
      method_note = method_note
    ))
  }

  sample_size_validate_positive(n, "Sample size per group")
  effective_n <- n / design_effect
  result <- if (identical(outcome, "binary")) {
    sample_size_proportion("power", "two_proportion", p1, p2, alpha, n = effective_n, ratio = ratio, alternative = alternative)
  } else {
    sample_size_ttest("power", "two_sample", effect_size, alpha, n = effective_n, ratio = ratio, alternative = alternative)
  }
  result$design_label <- design_label
  result$design_effect <- design_effect
  result$method_note <- method_note
  result
}

sample_size_stepped_wedge_treatment <- function(clusters, periods) {
  cluster_index <- seq_len(clusters)
  switch_period <- 2L + floor((cluster_index - 1L) * (periods - 1L) / clusters)
  matrix(
    as.integer(outer(switch_period, seq_len(periods), FUN = function(switch_at, period) period >= switch_at)),
    nrow = clusters,
    ncol = periods
  )
}

sample_size_stepped_wedge_power_once <- function(
  clusters,
  effect_size,
  alpha,
  cluster_size,
  icc,
  periods,
  simulations,
  progress = NULL
) {
  if (!requireNamespace("nlme", quietly = TRUE)) {
    stop("The nlme package is required for stepped-wedge simulation.", call. = FALSE)
  }
  clusters <- as.integer(round(clusters))
  periods <- as.integer(periods)
  cluster_size <- as.integer(round(cluster_size))
  cluster_sd <- sqrt(icc)
  residual_sd <- sqrt(1 - icc)
  treatment_matrix <- sample_size_stepped_wedge_treatment(clusters, periods)
  significant <- logical(simulations)

  for (sim_index in seq_len(simulations)) {
    if (!is.null(progress) && (sim_index == 1L || sim_index == simulations || sim_index %% max(1L, floor(simulations / 10L)) == 0L)) {
      progress(sim_index / simulations, sprintf("Running stepped-wedge simulations %s/%s", sim_index, simulations))
    }
    cluster_effect <- stats::rnorm(clusters, 0, cluster_sd)
    rows <- expand.grid(
      subject = seq_len(cluster_size),
      period = seq_len(periods),
      cluster = seq_len(clusters)
    )
    rows$treatment <- treatment_matrix[cbind(rows$cluster, rows$period)]
    period_effect <- 0.05 * (rows$period - 1L)
    rows$y <- effect_size * rows$treatment + period_effect + cluster_effect[rows$cluster] + stats::rnorm(nrow(rows), 0, residual_sd)
    fit <- try(
      nlme::lme(
        y ~ treatment + factor(period),
        random = ~ 1 | cluster,
        data = rows,
        method = "REML",
        control = nlme::lmeControl(msMaxIter = 60, returnObject = TRUE)
      ),
      silent = TRUE
    )
    if (!inherits(fit, "try-error")) {
      coefficients <- try(summary(fit)$tTable, silent = TRUE)
      if (!inherits(coefficients, "try-error") && "treatment" %in% rownames(coefficients)) {
        p_value <- coefficients["treatment", "p-value"]
        significant[[sim_index]] <- is.finite(p_value) && p_value < alpha
      }
    }
  }
  mean(significant)
}

sample_size_stepped_wedge_power <- function(
  clusters,
  effect_size,
  alpha,
  cluster_size,
  icc,
  periods,
  simulations,
  progress = NULL
) {
  seed <- 810000L + as.integer(round(clusters * 10)) + periods * 100L
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    if (is.null(old_seed)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
    } else {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)
  sample_size_stepped_wedge_power_once(clusters, effect_size, alpha, cluster_size, icc, periods, simulations, progress)
}

sample_size_stepped_wedge <- function(
  target,
  effect_size,
  alpha,
  power = NULL,
  n = NULL,
  dropout = 0,
  cluster_size = 20,
  icc = 0.05,
  periods = 5,
  simulations = 100,
  progress = NULL
) {
  sample_size_validate_positive(abs(effect_size), "Effect size")
  sample_size_validate_probability(alpha, "Alpha")
  sample_size_validate_positive(cluster_size, "Cluster size per period")
  sample_size_validate_probability(icc, "ICC", lower = 0, upper = 1)
  periods <- as.integer(periods)
  if (!is.finite(periods) || periods < 3) stop("Periods must be at least 3.", call. = FALSE)
  simulations <- as.integer(simulations)
  if (!is.finite(simulations) || simulations < 20) stop("Simulations must be at least 20.", call. = FALSE)
  design_label <- "Stepped-wedge cluster trial"
  method_note <- sprintf(
    "Simulation-based stepped-wedge cluster trial power using a Hussey-Hughes style mixed model with fixed period effects and random cluster intercepts (%s simulations).",
    simulations
  )
  power_cache <- new.env(parent = emptyenv())
  evaluation_index <- 0L
  max_evaluations <- if (identical(target, "sample_size")) 20 else 1
  power_for_clusters <- function(clusters) {
    key <- as.character(as.integer(round(clusters)))
    if (!exists(key, envir = power_cache, inherits = FALSE)) {
      evaluation_index <<- evaluation_index + 1L
      eval_start <- min(0.94, 0.02 + (evaluation_index - 1L) * 0.92 / max_evaluations)
      eval_span <- 0.92 / max_evaluations
      nested_progress <- NULL
      if (!is.null(progress)) {
        nested_progress <- function(value, text) {
          progress(min(0.98, eval_start + eval_span * value), sprintf("%s (clusters = %s)", text, key))
        }
      }
      assign(
        key,
        sample_size_stepped_wedge_power(clusters, effect_size, alpha, cluster_size, icc, periods, simulations, nested_progress),
        envir = power_cache
      )
    }
    get(key, envir = power_cache, inherits = FALSE)
  }

  if (identical(target, "sample_size")) {
    sample_size_validate_probability(power, "Power")
    clusters <- sample_size_find_discrete_n(power_for_clusters, periods - 1L, power)
    adjusted_cluster_size <- sample_size_drop_adjust(cluster_size, dropout)
    return(list(
      design_label = design_label,
      total = clusters,
      total_label = "Clusters",
      total_observations = clusters * periods * ceiling(cluster_size),
      adjusted_total_observations = clusters * periods * adjusted_cluster_size,
      dropout_rate = dropout,
      estimated_power = power_for_clusters(clusters),
      method_note = method_note
    ))
  }

  sample_size_validate_positive(n, "Clusters")
  list(
    power = power_for_clusters(n),
    design_label = design_label,
    total_observations = as.integer(round(n)) * periods * ceiling(cluster_size),
    method_note = method_note
  )
}

sample_size_reliability <- function(
  target,
  design = "alpha",
  reliability,
  confidence_level = 0.95,
  half_width,
  items = 5,
  categories = 2,
  dropout = 0
) {
  if (!identical(target, "sample_size")) {
    stop("Reliability / Agreement currently calculates required sample size only.", call. = FALSE)
  }
  sample_size_validate_probability(confidence_level, "Confidence level")
  sample_size_validate_positive(half_width, "Desired CI half-width")
  z <- stats::qnorm(1 - (1 - confidence_level) / 2)

  if (identical(design, "bland_altman")) {
    sample_size_validate_positive(reliability, "SD of paired differences")
    total <- sample_size_round_up((z * sqrt(3) * reliability / half_width)^2)
    design_label <- "Bland-Altman LoA precision"
    method_note <- "Approximate sample size for precision of Bland-Altman limits of agreement using SD of paired differences and desired LoA confidence interval half-width."
  } else if (identical(design, "alpha")) {
    sample_size_validate_probability(reliability, "Expected reliability")
    items <- as.integer(items)
    if (!is.finite(items) || items < 2) stop("Number of items must be at least 2.", call. = FALSE)
    transformed <- log(1 - reliability)
    se_target <- half_width / max(1e-8, 1 - reliability)
    precision_total <- sample_size_round_up((z / se_target)^2 + 2)
    minimum_subjects <- items + 1L
    total <- max(precision_total, minimum_subjects)
    design_label <- "Cronbach's alpha precision"
    method_note <- sprintf(
      "Bonett-style log(1 - alpha) normal approximation for coefficient alpha precision with %s items; final n is not allowed below items + 1.",
      items
    )
  } else if (identical(design, "icc")) {
    sample_size_validate_probability(reliability, "Expected reliability")
    items <- as.integer(items)
    if (!is.finite(items) || items < 2) stop("Number of raters/measurements must be at least 2.", call. = FALSE)
    transformed <- atanh(reliability)
    se_target <- half_width / max(1e-8, 1 - reliability^2)
    total <- sample_size_round_up((z / se_target)^2 / items + 2)
    design_label <- "ICC precision"
    method_note <- sprintf("Approximate Fisher z precision method for intraclass correlation with %s raters/measurements.", items)
  } else {
    sample_size_validate_probability(reliability, "Expected reliability")
    categories <- as.integer(categories)
    if (!is.finite(categories) || categories < 2) stop("Number of categories must be at least 2.", call. = FALSE)
    pe <- 1 / categories
    po <- reliability * (1 - pe) + pe
    variance <- po * (1 - po) / max(1e-8, (1 - pe)^2)
    total <- sample_size_round_up(z^2 * variance / half_width^2)
    design_label <- "Cohen's kappa precision"
    method_note <- "Large-sample normal approximation for Cohen's kappa precision assuming equal category prevalence."
  }

  list(
    design_label = design_label,
    total = total,
    total_label = "Subjects",
    adjusted_total = sample_size_drop_adjust(total, dropout),
    adjusted_total_label = "Subjects with dropout",
    dropout_rate = dropout,
    precision_total = if (identical(design, "alpha")) precision_total else NULL,
    minimum_subjects = if (identical(design, "alpha")) minimum_subjects else NULL,
    method_note = method_note
  )
}

sample_size_sem_parameter_power <- function(
  total_n,
  parameter,
  complexity = "moderate",
  alpha,
  simulations = 1000,
  progress = NULL
) {
  complexity_factor <- switch(
    complexity,
    simple = 1,
    complex = 0.65,
    0.8
  )
  effective_n <- max(4, total_n * complexity_factor)
  se <- sqrt(max(1e-8, 1 - parameter^2) / max(1, effective_n - 3))
  simulations <- max(100L, as.integer(simulations))
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    if (is.null(old_seed)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
    } else {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(840000 + as.integer(total_n) + as.integer(round(abs(parameter) * 1000)) + simulations)

  hits <- 0L
  chunk <- max(10L, ceiling(simulations / 20))
  completed <- 0L
  while (completed < simulations) {
    current <- min(chunk, simulations - completed)
    estimates <- stats::rnorm(current, mean = parameter, sd = se)
    p_values <- 2 * stats::pnorm(-abs(estimates / se))
    hits <- hits + sum(p_values < alpha)
    completed <- completed + current
    if (is.function(progress)) {
      progress(completed / simulations, sprintf("SEM Monte Carlo... %s%%", round(100 * completed / simulations)))
    }
  }
  hits / simulations
}

sample_size_sem_parameter_analytic_power <- function(total_n, parameter, complexity = "moderate", alpha) {
  complexity_factor <- switch(
    complexity,
    simple = 1,
    complex = 0.65,
    0.8
  )
  effective_n <- max(4, total_n * complexity_factor)
  se <- sqrt(max(1e-8, 1 - parameter^2) / max(1, effective_n - 3))
  delta <- abs(parameter) / se
  zcrit <- stats::qnorm(1 - alpha / 2)
  stats::pnorm(-zcrit - delta) + stats::pnorm(delta - zcrit)
}

sample_size_sem_complexity <- function(
  target,
  latent_variables,
  measured_variables,
  structural_paths,
  free_parameters,
  expected_loading,
  expected_path,
  complexity = "moderate",
  alpha,
  power = NULL,
  n = NULL,
  dropout = 0
) {
  latent_variables <- as.integer(latent_variables)
  measured_variables <- as.integer(measured_variables)
  structural_paths <- as.integer(structural_paths)
  free_parameters <- as.integer(free_parameters)
  if (!is.finite(latent_variables) || latent_variables < 1) stop("Latent variables must be at least 1.", call. = FALSE)
  if (!is.finite(measured_variables) || measured_variables < latent_variables) stop("Measured variables must be at least the number of latent variables.", call. = FALSE)
  if (!is.finite(structural_paths) || structural_paths < 0) stop("Structural paths must be 0 or greater.", call. = FALSE)
  if (!is.finite(free_parameters) || free_parameters < 1) stop("Free parameters must be at least 1.", call. = FALSE)
  sample_size_validate_probability(alpha, "Alpha")
  sample_size_validate_positive(abs(expected_loading), "Expected loading")
  sample_size_validate_positive(abs(expected_path), "Expected path coefficient")
  if (abs(expected_loading) >= 1 || abs(expected_path) >= 1) {
    stop("Expected loading and path coefficient must be between -1 and 1.", call. = FALSE)
  }

  parameter_ratio <- switch(
    complexity,
    simple = 10,
    complex = 20,
    15
  )
  complexity_multiplier <- switch(
    complexity,
    simple = 1,
    complex = 1.4,
    1.2
  )
  parameter_rule_n <- sample_size_round_up(parameter_ratio * free_parameters)
  structure_rule_n <- sample_size_round_up(complexity_multiplier * (10 * measured_variables + 20 * latent_variables + 15 * structural_paths))
  design_label <- "SEM/CFA model complexity heuristic"
  method_note <- sprintf(
    "Complexity-based planning estimate using %s cases per free parameter, observed/latent variable burden, and approximate power for loading/path detectability.",
    parameter_ratio
  )
  power_for_n <- function(total_n) {
    min(
      sample_size_sem_parameter_analytic_power(total_n, expected_loading, complexity, alpha),
      sample_size_sem_parameter_analytic_power(total_n, expected_path, complexity, alpha)
    )
  }

  if (identical(target, "sample_size")) {
    sample_size_validate_probability(power, "Power")
    effect_rule_n <- sample_size_find_discrete_n(power_for_n, 20, power)
    total <- max(parameter_rule_n, structure_rule_n, effect_rule_n)
    return(list(
      design_label = design_label,
      parameter_rule_n = parameter_rule_n,
      structure_rule_n = structure_rule_n,
      effect_rule_n = effect_rule_n,
      total = total,
      total_label = "Recommended participants",
      adjusted_total = sample_size_drop_adjust(total, dropout),
      adjusted_total_label = "Recommended participants with dropout",
      dropout_rate = dropout,
      estimated_power = power_for_n(total),
      method_note = method_note
    ))
  }

  sample_size_validate_positive(n, "Sample size")
  list(
    power = power_for_n(n),
    design_label = design_label,
    parameter_rule_n = parameter_rule_n,
    structure_rule_n = structure_rule_n,
    method_note = method_note
  )
}

sample_size_sem_estimated_df <- function(latent_variables, measured_variables, structural_paths) {
  latent_variables <- as.integer(latent_variables)
  measured_variables <- as.integer(measured_variables)
  structural_paths <- as.integer(structural_paths)
  if (!is.finite(latent_variables) || latent_variables < 1) stop("Latent variables must be at least 1.", call. = FALSE)
  if (!is.finite(measured_variables) || measured_variables < latent_variables) {
    stop("Measured variables must be at least the number of latent variables.", call. = FALSE)
  }
  if (!is.finite(structural_paths) || structural_paths < 0) stop("Structural paths must be 0 or greater.", call. = FALSE)

  observed_moments <- measured_variables * (measured_variables + 1) / 2
  free_loadings <- measured_variables - latent_variables
  residual_variances <- measured_variables
  latent_variances <- latent_variables
  free_parameters <- free_loadings + residual_variances + latent_variances + structural_paths
  df <- observed_moments - free_parameters
  if (!is.finite(df) || df < 1) {
    stop("Estimated model degrees of freedom must be at least 1. Add measured variables or simplify the model.", call. = FALSE)
  }
  list(
    df = as.integer(round(df)),
    observed_moments = as.integer(round(observed_moments)),
    free_parameters = as.integer(round(free_parameters)),
    latent_variables = latent_variables,
    measured_variables = measured_variables,
    structural_paths = structural_paths
  )
}

sample_size_sem <- function(
  target,
  test = "close_fit",
  df,
  df_source = "direct",
  null_rmsea,
  alternative_rmsea,
  parameter_type = "path",
  parameter = NULL,
  complexity = "moderate",
  simulations = 1000,
  latent_variables = NULL,
  measured_variables = NULL,
  structural_paths = NULL,
  free_parameters = NULL,
  expected_loading = NULL,
  expected_path = NULL,
  alpha,
  power = NULL,
  n = NULL,
  dropout = 0,
  progress = NULL
) {
  sample_size_validate_probability(alpha, "Alpha")

  if (identical(test, "complexity")) {
    return(sample_size_sem_complexity(
      target = target,
      latent_variables = latent_variables,
      measured_variables = measured_variables,
      structural_paths = structural_paths,
      free_parameters = free_parameters,
      expected_loading = expected_loading,
      expected_path = expected_path,
      complexity = complexity,
      alpha = alpha,
      power = power,
      n = n,
      dropout = dropout
    ))
  }

  if (identical(test, "parameter")) {
    if (is.null(parameter) || length(parameter) == 0 || !is.finite(parameter)) {
      stop("Expected standardized parameter must be numeric.", call. = FALSE)
    }
    sample_size_validate_positive(abs(parameter), "Expected standardized parameter")
    if (abs(parameter) >= 1) stop("Expected standardized parameter must be between -1 and 1.", call. = FALSE)
    simulations <- max(100L, as.integer(simulations))
    if (!is.finite(simulations)) stop("Simulations must be numeric.", call. = FALSE)
    parameter_label <- switch(
      parameter_type,
      loading = "standardized loading",
      correlation = "latent correlation",
      "standardized path"
    )
    complexity_label <- switch(
      complexity,
      simple = "simple",
      complex = "complex",
      "moderate"
    )
    design_label <- sprintf("SEM/CFA parameter-level Monte Carlo (%s)", parameter_label)
    method_note <- sprintf(
      "Monte Carlo power for a %s of %.2f using %s model complexity and %s simulations per evaluated sample size.",
      parameter_label,
      parameter,
      complexity_label,
      simulations
    )
    power_for_n <- function(total_n) {
      sample_size_sem_parameter_power(
        total_n = total_n,
        parameter = parameter,
        complexity = complexity,
        alpha = alpha,
        simulations = simulations,
        progress = progress
      )
    }

    if (identical(target, "sample_size")) {
      sample_size_validate_probability(power, "Power")
      total <- sample_size_find_discrete_n(power_for_n, 20, power)
      return(list(
        design_label = design_label,
        total = total,
        total_label = "Participants",
        adjusted_total = sample_size_drop_adjust(total, dropout),
        adjusted_total_label = "Participants with dropout",
        dropout_rate = dropout,
        estimated_power = power_for_n(total),
        method_note = method_note
      ))
    }

    sample_size_validate_positive(n, "Sample size")
    return(list(power = power_for_n(n), design_label = design_label, method_note = method_note))
  }

  estimated_df <- NULL
  if (identical(df_source, "structure")) {
    estimated_df <- sample_size_sem_estimated_df(latent_variables, measured_variables, structural_paths)
    df <- estimated_df$df
  }

  df <- as.integer(df)
  if (!is.finite(df) || df < 1) stop("Model degrees of freedom must be at least 1.", call. = FALSE)
  sample_size_validate_probability(null_rmsea, "Null RMSEA")
  sample_size_validate_probability(alternative_rmsea, "Alternative RMSEA")
  if (identical(test, "close_fit") && alternative_rmsea <= null_rmsea) {
    stop("For the close-fit test, alternative RMSEA must be greater than null RMSEA.", call. = FALSE)
  }
  if (identical(test, "not_close_fit") && alternative_rmsea >= null_rmsea) {
    stop("For the not-close-fit test, alternative RMSEA must be less than null RMSEA.", call. = FALSE)
  }

  design_label <- if (identical(test, "not_close_fit")) "SEM/CFA RMSEA not-close-fit test" else "SEM/CFA RMSEA close-fit test"
  method_note <- if (identical(df_source, "structure")) {
    "MacCallum-Browne-Sugawara RMSEA power analysis using estimated model df from observed variables, latent variables, and structural paths."
  } else {
    "MacCallum-Browne-Sugawara RMSEA power analysis using the noncentral chi-square distribution."
  }
  power_for_n <- function(total_n) {
    lambda_null <- max(0, (total_n - 1) * df * null_rmsea^2)
    lambda_alt <- max(0, (total_n - 1) * df * alternative_rmsea^2)
    if (identical(test, "not_close_fit")) {
      crit <- stats::qchisq(alpha, df, ncp = lambda_null)
      stats::pchisq(crit, df, ncp = lambda_alt)
    } else {
      crit <- stats::qchisq(1 - alpha, df, ncp = lambda_null)
      stats::pchisq(crit, df, ncp = lambda_alt, lower.tail = FALSE)
    }
  }

  if (identical(target, "sample_size")) {
    sample_size_validate_probability(power, "Power")
    total <- sample_size_find_discrete_n(power_for_n, max(5, df + 2), power)
    return(c(list(
      design_label = design_label,
      total = total,
      total_label = "Participants",
      adjusted_total = sample_size_drop_adjust(total, dropout),
      adjusted_total_label = "Participants with dropout",
      dropout_rate = dropout,
      estimated_power = power_for_n(total),
      method_note = method_note
    ), estimated_df))
  }

  sample_size_validate_positive(n, "Sample size")
  c(list(power = power_for_n(n), design_label = design_label, method_note = method_note), estimated_df)
}

sample_size_chisquare <- function(target, df, effect_size, alpha, power = NULL, n = NULL, dropout = 0) {
  df <- as.integer(df)
  if (!is.finite(df) || df < 1) {
    stop("Degrees of freedom must be at least 1.", call. = FALSE)
  }
  sample_size_validate_positive(effect_size, "Effect size w")
  sample_size_validate_probability(alpha, "Alpha")
  power_for_n <- function(total_n) {
    lambda <- total_n * effect_size^2
    crit <- stats::qchisq(1 - alpha, df)
    stats::pchisq(crit, df, ncp = lambda, lower.tail = FALSE)
  }
  if (identical(target, "sample_size")) {
    sample_size_validate_probability(power, "Power")
    upper <- 20
    while (power_for_n(upper) < power && upper < 1e6) upper <- upper * 2
    total <- sample_size_round_up(stats::uniroot(function(x) power_for_n(x) - power, c(df + 1, upper))$root)
    return(list(total = total, adjusted_total = sample_size_drop_adjust(total, dropout), dropout_rate = dropout, method_note = "Chi-square test using Cohen's w and noncentral chi-square distribution."))
  }
  sample_size_validate_positive(n, "Sample size")
  list(power = power_for_n(n), method_note = "Chi-square test using Cohen's w and noncentral chi-square distribution.")
}
