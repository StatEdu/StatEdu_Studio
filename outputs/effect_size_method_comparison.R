source("R/utils.R")
source("R/sample_size.R")

has_effectsize <- requireNamespace("effectsize", quietly = TRUE)

num <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  as.numeric(x[[1]])
}

first_numeric_column <- function(x, preferred = character(0)) {
  if (!is.data.frame(x)) x <- as.data.frame(x)
  names_x <- names(x)
  selected <- intersect(preferred, names_x)
  if (length(selected) > 0) return(as.numeric(x[[selected[[1]]]][[1]]))
  numeric_columns <- names_x[vapply(x, is.numeric, logical(1))]
  if (length(numeric_columns) == 0) return(NA_real_)
  as.numeric(x[[numeric_columns[[1]]]][[1]])
}

tol <- 1e-10
verdict <- function(app, ref, tolerance = tol) {
  if (!is.finite(app) || !is.finite(ref)) return("not directly comparable")
  if (abs(app - ref) <= tolerance) "match" else "review"
}

pct_diff <- function(app, ref) {
  if (!is.finite(app) || !is.finite(ref) || ref == 0) return(NA_real_)
  100 * (app - ref) / abs(ref)
}

row <- function(method, scenario, compared_field, app_value, reference_value, reference, evidence, note = "") {
  data.frame(
    Method = method,
    Scenario = scenario,
    Compared_Field = compared_field,
    App_Value = app_value,
    Reference = reference,
    Reference_Value = reference_value,
    Difference = app_value - reference_value,
    Percent_Diff = pct_diff(app_value, reference_value),
    Evidence = evidence,
    Verdict = verdict(app_value, reference_value),
    Note = note,
    stringsAsFactors = FALSE
  )
}

rows <- list()

add <- function(...) {
  rows[[length(rows) + 1L]] <<- row(...)
}

# t-test: equal-sized independent t conversion. The app uses the exact equal-n
# denominator df + 2; effectsize::t_to_d uses df_error as a common approximation.
app <- sample_size_effect_size(design = "independent_t_df_equal", t_value = 2.5, df = 78)
effectsize_t_default <- if (has_effectsize) {
  first_numeric_column(effectsize::t_to_d(t = 2.5, df_error = 78, ci = NULL), "d")
} else {
  NA_real_
}
ref <- 2 * 2.5 / sqrt(78 + 2)
add(
  "t-test",
  "Independent t, equal n: t=2.5, df=78",
  "Cohen's d",
  num(app$primary_effect_size),
  ref,
  "Exact equal-n independent-t formula",
  "d = 2t / sqrt(df + 2)",
  if (is.finite(effectsize_t_default)) sprintf("effectsize::t_to_d default gives %.12f because it uses 2t/sqrt(df_error), a common approximation rather than the equal-n exact formula.", effectsize_t_default) else ""
)

# Proportion: Cohen's h from two proportions.
app <- sample_size_effect_size_proportion(design = "cohens_h", p1 = 0.65, p2 = 0.50)
ref <- 2 * asin(sqrt(0.65)) - 2 * asin(sqrt(0.50))
add("Proportion", "p1=.65, p2=.50", "Cohen's h", num(app$primary_effect_size), ref, "Cohen arcsine formula", "h = 2asin(sqrt(p1)) - 2asin(sqrt(p2))")

# Chi-square: Cramer's V, with effectsize adjustment disabled to match the app definition.
app <- sample_size_effect_size_chisquare(design = "cramers_v", chi_square = 12.5, n = 200, rows = 3, columns = 4)
ref <- if (has_effectsize) {
  first_numeric_column(effectsize::chisq_to_cramers_v(chisq = 12.5, n = 200, nrow = 3, ncol = 4, adjust = FALSE, ci = NULL), "Cramers_v")
} else {
  sqrt(12.5 / (200 * min(3 - 1, 4 - 1)))
}
add("Chi-square", "Chi-square=12.5, N=200, 3x4 table", "Cramer's V", num(app$primary_effect_size), ref, if (has_effectsize) "effectsize::chisq_to_cramers_v(adjust=FALSE)" else "Cramer's V formula", "Package default uses adjusted V, so adjust=FALSE is required.")

# Correlation: r from t statistic.
app <- sample_size_effect_size_correlation(design = "r_from_t", t_value = 2.5, df = 78)
ref <- if (has_effectsize) {
  first_numeric_column(effectsize::t_to_r(t = 2.5, df_error = 78, ci = NULL), "r")
} else {
  sqrt(2.5^2 / (2.5^2 + 78))
}
add("Correlation", "t=2.5, df=78", "Pearson r", num(app$primary_effect_size), ref, if (has_effectsize) "effectsize::t_to_r" else "t-to-r formula", "r = sign(t) * sqrt(t^2 / [t^2 + df])")

# ANOVA: partial eta squared from F.
app <- sample_size_effect_size_anova(design = "eta_from_f", f_value = 5.2, df_effect = 2, df_error = 87)
ref <- if (has_effectsize) {
  first_numeric_column(effectsize::F_to_eta2(f = 5.2, df = 2, df_error = 87, ci = NULL), "Eta2_partial")
} else {
  (5.2 * 2) / (5.2 * 2 + 87)
}
add("ANOVA", "F=5.2, df_effect=2, df_error=87", "Partial eta squared", num(app$primary_effect_size), ref, if (has_effectsize) "effectsize::F_to_eta2" else "partial eta squared formula", "eta_p^2 = F*df_effect / (F*df_effect + df_error)")

# ANCOVA: adjusted Cohen's f.
app <- sample_size_effect_size_ancova(design = "ancova_adjusted_f", effect_size_f = 0.25, covariate_r2 = 0.30)
ref <- 0.25 / sqrt(1 - 0.30)
add("ANCOVA", "unadjusted f=.25, covariate R2=.30", "Adjusted Cohen's f", num(app$primary_effect_size), ref, "ANCOVA adjustment formula", "adjusted f = f / sqrt(1 - R2_covariate)")

# Nonparametric: Mann-Whitney rank-biserial correlation.
app <- sample_size_effect_size_nonparametric(design = "rank_biserial_from_u", u = 1200, n1 = 40, n2 = 45)
ref <- 2 * 1200 / (40 * 45) - 1
add("Nonparametric", "Mann-Whitney U=1200, n1=40, n2=45", "Rank-biserial r", num(app$primary_effect_size), ref, "rank-biserial formula", "r_rb = 2U / (n1*n2) - 1")

# McNemar: matched-pair odds ratio from discordant counts.
app <- sample_size_effect_size_mcnemar(design = "matched_or_counts", b = 18, c = 10)
ref <- 18 / 10
add("McNemar", "Discordant counts b=18, c=10", "Matched-pair odds ratio", num(app$primary_effect_size), ref, "matched-pair OR formula", "OR = b / c")

# Regression: Cohen's f squared from R2.
app <- sample_size_effect_size_regression(design = "f2_from_r2", r_squared = 0.20)
ref <- 0.20 / (1 - 0.20)
add("Regression", "Multiple regression R2=.20", "Cohen's f-squared", num(app$primary_effect_size), ref, "Cohen f2 formula", "f2 = R2 / (1 - R2)")

# GEE: binary Cohen's h from marginal proportions.
app <- sample_size_effect_size_gee(design = "binary_proportions", p1 = 0.65, p2 = 0.50)
ref <- 2 * asin(sqrt(0.65)) - 2 * asin(sqrt(0.50))
add("GEE", "Binary marginal proportions p1=.65, p2=.50", "Cohen's h", num(app$primary_effect_size), ref, "Cohen arcsine formula", "Same h definition as two-proportion planning.")

# LMM: simple standardized fixed effect and repeated-measures planning effect.
app <- sample_size_effect_size_lmm(design = "simple_fixed", lmm_design = "two_group_repeated", effect_size = 0.30, time_points = 3, icc = 0.30)
ref <- 0.30
add("LMM", "simple fixed effect d=.30, m=3, ICC=.30", "Standardized fixed effect", num(app$primary_effect_size), ref, "identity check", "Primary effect is the supplied standardized fixed effect.")
ref_planning <- 0.30 * sqrt(3 / (1 + (3 - 1) * 0.30))
add("LMM", "simple fixed effect d=.30, m=3, ICC=.30", "Repeated-measures planning effect", num(app$planning_effect_size), ref_planning, "repeated-measures design-effect formula", "d_planning = d * sqrt(m / [1 + (m - 1)ICC])")

# Survival / Cox: hazard ratio and log HR.
app <- sample_size_effect_size_survival(hazard_ratio = 0.70)
add("Survival / Cox", "HR=.70", "Hazard ratio", num(app$primary_effect_size), 0.70, "identity check", "Primary effect is HR.")
add("Survival / Cox", "HR=.70", "log hazard ratio", num(app$log_hazard_ratio), log(0.70), "log transform", "logHR = log(HR)")

# Equivalence / NI: standardized distance to margin.
app <- sample_size_effect_size_equivalence(outcome = "mean", objective = "equivalence", true_difference = 0.05, margin = 0.20, sd = 1)
ref <- (0.20 - abs(0.05)) / 1
add("Equivalence / NI", "Mean equivalence: difference=.05, margin=.20, SD=1", "Standardized distance to margin", num(app$primary_effect_size), ref, "equivalence margin-distance formula", "D = (margin - abs(theta_hat)) / SD")

# ROC AUC: AUC-to-d conversion.
app <- sample_size_effect_size_diagnostic(auc = 0.70, null_auc = 0.50)
add("ROC AUC", "AUC=.70 vs null=.50", "AUC", num(app$primary_effect_size), 0.70, "identity check", "Primary effect is AUC.")
add("ROC AUC", "AUC=.70 vs null=.50", "Approximate Cohen's d", num(app$auc_cohen_d), sqrt(2) * qnorm(0.70), "AUC-to-d binormal approximation", "d = sqrt(2) * qnorm(AUC)")

# Count / rate regression: incidence rate ratio and log IRR.
app <- sample_size_effect_size_rates(design = "poisson_irr", input_scale = "ratio", ratio = 1.50)
add("Count / Rate Regression", "IRR=1.50", "Incidence rate ratio", num(app$primary_effect_size), 1.50, "identity check", "Primary effect is IRR.")
add("Count / Rate Regression", "IRR=1.50", "log incidence rate ratio", num(app$log_incidence_rate_ratio), log(1.50), "log transform", "logIRR = log(IRR)")

# Cluster trial: continuous planning effect.
app <- sample_size_effect_size_cluster(design = "parallel_continuous", effect_size = 0.50, cluster_size = 20, icc = 0.05)
ref <- 0.50 / sqrt(1 + (20 - 1) * 0.05)
add("Cluster Trial", "parallel continuous: d=.50, m=20, ICC=.05", "Planning effect size", num(app$planning_effect_size), ref, "cluster design-effect formula", "d_planning = d / sqrt(1 + [m - 1]ICC)")

# Precision / CI: standardized mean half-width.
app <- sample_size_effect_size_precision(parameter = "mean", estimate = 10, half_width = 1.5, sd = 6)
ref <- 1.5 / 6
add("Precision / CI", "Mean estimate=10, half-width=1.5, SD=6", "Standardized half-width", num(app$primary_effect_size), ref, "CI precision formula", "standardized half-width = half-width / SD")

# Reliability / Agreement: alpha difference and transformed alpha.
app <- sample_size_effect_size_reliability(design = "alpha", reliability = 0.80, reference = 0.70, items = 5)
add("Reliability / Agreement", "alpha=.80 vs reference=.70, items=5", "Alpha difference", num(app$primary_effect_size), 0.80 - 0.70, "alpha difference formula", "Delta alpha = alpha - reference")
add("Reliability / Agreement", "alpha=.80 vs reference=.70, items=5", "Average inter-item r", num(app$average_inter_item_r), 0.80 / (5 - 0.80 * (5 - 1)), "Cronbach alpha relation", "r_bar = alpha / [k - alpha(k - 1)]")

# SEM / CFA: RMSEA difference and NCP difference per N.
app <- sample_size_effect_size_sem(design = "rmsea", df = 20, null_rmsea = 0.05, alternative_rmsea = 0.08)
add("SEM / CFA", "df=20, RMSEA0=.05, RMSEA1=.08", "RMSEA difference", num(app$primary_effect_size), abs(0.08 - 0.05), "RMSEA difference formula", "abs(RMSEA_alt - RMSEA_null)")
add("SEM / CFA", "df=20, RMSEA0=.05, RMSEA1=.08", "NCP difference per N", num(app$ncp_difference_per_n), 20 * (0.08^2 - 0.05^2), "RMSEA noncentrality relation", "df * (RMSEA_alt^2 - RMSEA_null^2)")

out <- do.call(rbind, rows)
out$App_Value <- round(out$App_Value, 12)
out$Reference_Value <- round(out$Reference_Value, 12)
out$Difference <- round(out$Difference, 12)
out$Percent_Diff <- round(out$Percent_Diff, 8)

write.csv(out, "outputs/effect_size_method_comparison.csv", row.names = FALSE, fileEncoding = "UTF-8")

summary <- aggregate(Method ~ Verdict, data = out, FUN = length)
names(summary)[names(summary) == "Method"] <- "Count"
write.csv(summary, "outputs/effect_size_method_comparison_summary.csv", row.names = FALSE, fileEncoding = "UTF-8")

if (requireNamespace("openxlsx", quietly = TRUE)) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "comparison")
  openxlsx::writeData(wb, "comparison", out)
  openxlsx::addWorksheet(wb, "summary")
  openxlsx::writeData(wb, "summary", summary)
  openxlsx::saveWorkbook(wb, "outputs/effect_size_method_comparison.xlsx", overwrite = TRUE)
}

print(out)
cat("\nSummary:\n")
print(summary)
