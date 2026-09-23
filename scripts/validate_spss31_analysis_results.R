all_args <- commandArgs(FALSE)
file_arg <- all_args[grep("^--file=", all_args)]
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[[1]]) else "scripts/validate_spss31_analysis_results.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)

source(file.path(repo_root, "R", "app_bootstrap.R"))
load_app_packages(check = FALSE)
source_app_modules()

fixture_path <- file.path(repo_root, "scripts", "fixtures", "survival_validation.csv")
reference_path <- file.path(repo_root, "sample", "spss31_analysis_cells.csv")
stopifnot(file.exists(fixture_path), file.exists(reference_path))
d <- utils::read.csv(fixture_path, check.names = FALSE, stringsAsFactors = FALSE)
reference <- utils::read.csv(reference_path, check.names = FALSE, stringsAsFactors = FALSE)

# These deterministic variables are also constructed in run_spss_classical_validation.py.
d$rare <- as.integer(d$id <= 5L)
d$item1 <- d$age / 10 + (d$id %% 5) * .20
d$item2 <- d$item1 * .80 + ((d$id * 3) %% 7) * .15
d$item3 <- d$item1 * 1.10 + ((d$id * 5) %% 6) * .10
d$item4 <- d$item1 * .90 + ((d$id * 7) %% 8) * .12
d$rm1 <- d$age + (d$id %% 4) * .25
d$rm2 <- d$rm1 + 2.5 + ((d$id * 2) %% 5) * .20
d$rm3 <- d$rm1 + 5.0 + ((d$id * 3) %% 7) * .15

checks <- data.frame(
  Section = character(), Metric = character(), Actual = numeric(), Reference = numeric(),
  AbsError = numeric(), Tolerance = numeric(), stringsAsFactors = FALSE
)

ref_cell <- function(command_index, subtype, path) {
  selected <- reference$CommandIndex == command_index & reference$SubType == subtype & reference$Path == path
  if (sum(selected) != 1L) {
    stop("SPSS reference cell is missing or duplicated: ", paste(command_index, subtype, path, sep = " / "))
  }
  reference$Number[selected]
}

expect_close <- function(section, metric, actual, expected, tolerance = 1e-9) {
  actual <- unname(as.numeric(actual))
  expected <- unname(as.numeric(expected))
  error <- abs(actual - expected)
  checks <<- rbind(checks, data.frame(
    Section = section, Metric = metric, Actual = actual, Reference = expected,
    AbsError = error, Tolerance = tolerance, stringsAsFactors = FALSE
  ))
  if (!is.finite(error) || error > tolerance) {
    stop(sprintf("%s / %s differs: actual=%.15g, SPSS=%.15g, error=%.3g, tolerance=%.3g",
                 section, metric, actual, expected, error, tolerance))
  }
}

# Crosstabs: cell counts, Pearson chi-square without continuity correction, and Fisher exact p.
validate_crosstab <- function(command_index, row_name, row_values, row_levels, spss_labels = as.character(row_levels)) {
  tab <- table(factor(row_values, levels = row_levels), factor(d$status, levels = c(0, 1)))
  for (i in seq_along(row_levels)) {
    for (j in seq_along(c(0, 1))) {
      path <- sprintf(
        "row:%s > group:%s > cat:%s > row:Statistics > cat:Count > column:status > group:status > cat:%s",
        row_name, row_name, spss_labels[[i]], c(0, 1)[[j]]
      )
      expect_close("Crosstabs", sprintf("%s count %s/%s", row_name, row_levels[[i]], c(0, 1)[[j]]),
                   tab[i, j], ref_cell(command_index, "Crosstabulation", path), 0)
    }
  }
  pearson <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
  fisher <- stats::fisher.test(tab)
  pearson_path <- "row:Statistics > group:A > cat:Pearson Chi-Square > column:Values > cat:Value"
  fisher_path <- "row:Statistics > group:B > cat:Fisher's Exact Test > column:Values > group:C > cat:Exact Sig. (2-sided)"
  expect_close("Crosstabs", paste(row_name, "Pearson chi-square"), pearson$statistic,
               ref_cell(command_index, "Chi Square Tests", pearson_path))
  expect_close("Crosstabs", paste(row_name, "Fisher two-sided p"), fisher$p.value,
               ref_cell(command_index, "Chi Square Tests", fisher_path))
}
validate_crosstab(3L, "sex", d$sex, c(1, 2))
validate_crosstab(4L, "rare", d$rare, c(0, 1), c(".00", "1.00"))

# Pearson and Spearman correlation. SPSS uses the large-sample t approximation for Spearman p.
pearson <- stats::cor.test(d$time, d$age, method = "pearson")
pearson_prefix <- "row:Variables > cat:time > row:Statistics > cat:%s > column:Variables > cat:age"
expect_close("Correlation", "Pearson r", pearson$estimate,
             ref_cell(5L, "Correlations", sprintf(pearson_prefix, "Pearson Correlation")))
expect_close("Correlation", "Pearson two-sided p", pearson$p.value,
             ref_cell(5L, "Correlations", sprintf(pearson_prefix, "Sig. (2-tailed)")))
rho <- stats::cor(d$time, d$age, method = "spearman")
rho_p <- 2 * stats::pt(-abs(rho * sqrt((nrow(d) - 2) / (1 - rho^2))), df = nrow(d) - 2)
rho_prefix <- "row:Type > cat:Spearman's rho > row:Variables1 > cat:time > row:Statistics > cat:%s > column:Variables2 > cat:age"
expect_close("Correlation", "Spearman rho", rho,
             ref_cell(6L, "Correlations", sprintf(rho_prefix, "Correlation Coefficient")))
expect_close("Correlation", "Spearman asymptotic p", rho_p,
             ref_cell(6L, "Correlations", sprintf(rho_prefix, "Sig. (2-tailed)")))

# Reliability: StatEdu and this comparator both use psych::alpha, while SPSS is the external oracle.
items <- d[c("item1", "item2", "item3", "item4")]
alpha <- suppressWarnings(psych::alpha(items, check.keys = FALSE, warnings = FALSE))
expect_close("Reliability", "Cronbach alpha", alpha$total$raw_alpha,
             ref_cell(7L, "Reliability Statistics", "column:Statistics > cat:Cronbach's Alpha"))
expect_close("Reliability", "Standardized alpha", alpha$total$std.alpha,
             ref_cell(7L, "Reliability Statistics", "column:Statistics > cat:Cronbach's Alpha Based on Standardized Items"))
for (item in names(items)) {
  path <- sprintf("row:Variables > cat:%s > column:Statistics > cat:Corrected Item-Total Correlation", item)
  expect_close("Reliability", paste(item, "corrected item-total r"), alpha$item.stats[item, "r.drop"],
               ref_cell(7L, "Item Total Statistics", path))
}

# Independent t tests (pooled and Welch) and Levene's mean-centred variance test.
d$sex_f <- factor(d$sex, levels = c(1, 2))
t_equal <- stats::t.test(age ~ sex_f, data = d, var.equal = TRUE)
t_welch <- stats::t.test(age ~ sex_f, data = d, var.equal = FALSE)
t_prefix <- function(assumption, metric) sprintf(
  "row:Dependent variables > cat:age > row:Assumptions > cat:%s > column:Statistics > group:t-test for Equality of Means > %s",
  assumption, metric
)
expect_close("Independent t test", "pooled t", t_equal$statistic,
             ref_cell(8L, "Independent Samples Test", t_prefix("Equal variances assumed", "cat:t")))
expect_close("Independent t test", "pooled p", t_equal$p.value,
             ref_cell(8L, "Independent Samples Test", t_prefix("Equal variances assumed", "group:Significance > cat:Two-Sided p")))
expect_close("Independent t test", "Welch df", t_welch$parameter,
             ref_cell(8L, "Independent Samples Test", t_prefix("Equal variances not assumed", "cat:df")))
expect_close("Independent t test", "Welch p", t_welch$p.value,
             ref_cell(8L, "Independent Samples Test", t_prefix("Equal variances not assumed", "group:Significance > cat:Two-Sided p")))
levene <- car::leveneTest(d$age, d$sex_f, center = mean)
levene_prefix <- "row:Dependent variables > cat:age > row:Assumptions > cat:Equal variances assumed > column:Statistics > group:Levene's Test for Equality of Variances > cat:%s"
expect_close("Independent t test", "Levene F", levene[["F value"]][[1]],
             ref_cell(8L, "Homogeneity of Variance Test", sprintf(levene_prefix, "F")))
expect_close("Independent t test", "Levene p", levene[["Pr(>F)"]][[1]],
             ref_cell(8L, "Homogeneity of Variance Test", sprintf(levene_prefix, "Sig.")))

# One-way ANOVA and rank tests.
d$ecog_f <- factor(d$ph.ecog, levels = c(0, 1, 2))
oneway <- stats::aov(age ~ ecog_f, data = d)
oneway_tab <- summary(oneway)[[1]]
anova_prefix <- "layer:Dependent Variable > cat:age > row:Source > cat:%s > column:Statistics > cat:%s"
expect_close("One-way ANOVA", "between SS", oneway_tab["ecog_f", "Sum Sq"],
             ref_cell(9L, "ANOVA", sprintf(anova_prefix, "Between Groups", "Sum of Squares")))
expect_close("One-way ANOVA", "F", oneway_tab["ecog_f", "F value"],
             ref_cell(9L, "ANOVA", sprintf(anova_prefix, "Between Groups", "F")))
expect_close("One-way ANOVA", "p", oneway_tab["ecog_f", "Pr(>F)"],
             ref_cell(9L, "ANOVA", sprintf(anova_prefix, "Between Groups", "Sig.")))

mw <- stats::wilcox.test(age ~ sex_f, data = d, exact = FALSE, correct = FALSE)
mw_u <- min(unname(mw$statistic), prod(table(d$sex_f)) - unname(mw$statistic))
mw_prefix <- "row:Statistics > group:A > cat:%s > column:Dependent Variables > cat:age"
expect_close("Nonparametric", "Mann-Whitney U", mw_u,
             ref_cell(10L, "Mann Whitney Test Statistics", sprintf(mw_prefix, "Mann-Whitney U")))
expect_close("Nonparametric", "Mann-Whitney asymptotic p", mw$p.value,
             ref_cell(10L, "Mann Whitney Test Statistics", sprintf(mw_prefix, "Asymp. Sig. (2-tailed)")), 1e-8)
kw <- stats::kruskal.test(age ~ ecog_f, data = d)
kw_prefix <- "row:Statistics > cat:%s > column:Dependent Variables > cat:age"
expect_close("Nonparametric", "Kruskal-Wallis H", kw$statistic,
             ref_cell(10L, "Kruskal Wallis Test Statistics", sprintf(kw_prefix, "Kruskal-Wallis H")))
expect_close("Nonparametric", "Kruskal-Wallis p", kw$p.value,
             ref_cell(10L, "Kruskal Wallis Test Statistics", sprintf(kw_prefix, "Asymp. Sig.")))

# Multiple linear regression.
linear <- stats::lm(time ~ age + sex + ph.ecog, data = d)
linear_summary <- summary(linear)
reg_summary_prefix <- "row:Model > cat:1 > column:Statistics > cat:%s"
expect_close("Linear regression", "R squared", linear_summary$r.squared,
             ref_cell(11L, "Model Summary", sprintf(reg_summary_prefix, "R Square")))
expect_close("Linear regression", "Adjusted R squared", linear_summary$adj.r.squared,
             ref_cell(11L, "Model Summary", sprintf(reg_summary_prefix, "Adjusted R Square")))
reg_f <- unname(linear_summary$fstatistic)
expect_close("Linear regression", "model F", reg_f[[1]],
             ref_cell(11L, "ANOVA", "row:Model > cat:1 > row:Source > cat:Regression > column:Statistics > cat:F"))
term_map <- c("(Intercept)" = "(Constant)", age = "age", sex = "sex", ph.ecog = "ph_ecog")
for (term in names(term_map)) {
  prefix <- sprintf("row:Model > cat:1 > row:Variables > cat:%s > column:Statistics", term_map[[term]])
  expect_close("Linear regression", paste(term, "B"), stats::coef(linear)[[term]],
               ref_cell(11L, "Coefficients", paste(prefix, "group:Unstandardized Coefficients > cat:B", sep = " > ")))
  expect_close("Linear regression", paste(term, "SE"), linear_summary$coefficients[term, "Std. Error"],
               ref_cell(11L, "Coefficients", paste(prefix, "group:Unstandardized Coefficients > cat:Std. Error", sep = " > ")))
}

# Binary logistic regression with the reference categories explicitly matching the SPSS syntax.
# INDICATOR(1) selects the first sex category; INDICATOR(0) selects the last ecog category.
d$ecog_logit <- factor(d$ph.ecog, levels = c(2, 0, 1))
logistic <- stats::glm(status ~ age + sex_f + ecog_logit, data = d, family = stats::binomial(),
                       control = stats::glm.control(epsilon = 1e-12, maxit = 100))
logistic_summary <- summary(logistic)
expect_close("Logistic regression", "-2 log likelihood", stats::deviance(logistic),
             ref_cell(12L, "Model Summary", "row:Step > cat:1 > column:Statistics > cat:-2 Log likelihood"), 1e-6)
expect_close("Logistic regression", "omnibus LR chi-square", logistic$null.deviance - logistic$deviance,
             ref_cell(12L, "Omnibus Tests of Model Coefficients", "row:Step > cat:Step 1 > row:Model > cat:Model > column:Statistics > cat:Chi-square"), 1e-6)
logit_map <- c("(Intercept)" = "Constant", age = "age", sex_f2 = "sex(1)", ecog_logit0 = "ph_ecog(1)", ecog_logit1 = "ph_ecog(2)")
for (term in names(logit_map)) {
  prefix <- sprintf("row:Variable > group:Step 1 > cat:%s > column:Statistics", logit_map[[term]])
  expect_close("Logistic regression", paste(term, "B"), stats::coef(logistic)[[term]],
               ref_cell(12L, "Variables in the Equation", paste(prefix, "cat:B", sep = " > ")), 1e-6)
  expect_close("Logistic regression", paste(term, "SE"), logistic_summary$coefficients[term, "Std. Error"],
               ref_cell(12L, "Variables in the Equation", paste(prefix, "cat:S.E.", sep = " > ")), 1e-6)
}

# ANCOVA: SPSS Type III equals the partial (Type II) tests for this additive no-interaction model.
ancova <- stats::lm(time ~ age + ecog_f, data = d)
ancova_tab <- car::Anova(ancova, type = 2)
ancova_prefix <- "layer:Dependent Variable > cat:time > row:Source > group:A > cat:%s > column:Statistics > cat:%s"
expect_close("ANCOVA", "group SS", ancova_tab["ecog_f", "Sum Sq"],
             ref_cell(13L, "Test of Between Subjects Fixed Effects", sprintf(ancova_prefix, "ph_ecog", "Type III Sum of Squares")), 1e-7)
expect_close("ANCOVA", "group F", ancova_tab["ecog_f", "F value"],
             ref_cell(13L, "Test of Between Subjects Fixed Effects", sprintf(ancova_prefix, "ph_ecog", "F")))
expect_close("ANCOVA", "age F", ancova_tab["age", "F value"],
             ref_cell(13L, "Test of Between Subjects Fixed Effects", sprintf(ancova_prefix, "age", "F")))
group_ss <- ancova_tab["ecog_f", "Sum Sq"]
partial_eta <- group_ss / (group_ss + stats::deviance(ancova))
expect_close("ANCOVA", "group partial eta squared", partial_eta,
             ref_cell(13L, "Test of Between Subjects Fixed Effects", sprintf(ancova_prefix, "ph_ecog", "Partial Eta Squared")))

# Repeated-measures ANOVA directly exercises StatEdu's computational helpers.
rm_values <- as.matrix(d[c("rm1", "rm2", "rm3")])
rm_anova <- paired_rm_anova(rm_values)
rm_sphericity <- paired_rm_sphericity(rm_values)
within_prefix <- "layer:Measure > cat:MEASURE_1 > row:Source > cat:timepoint > row:Epsilon Corrections > cat:Sphericity Assumed > column:Statistics > cat:%s"
expect_close("Repeated measures", "within SS", rm_anova$ss_time,
             ref_cell(14L, "Tests of Within Subjects Effects", sprintf(within_prefix, "Type III Sum of Squares")), 1e-8)
expect_close("Repeated measures", "within F", rm_anova$f,
             ref_cell(14L, "Tests of Within Subjects Effects", sprintf(within_prefix, "F")), 1e-6)
expect_close("Repeated measures", "Mauchly W", rm_sphericity$w,
             ref_cell(14L, "Mauchly Test", "layer:Measure > cat:MEASURE_1 > row:Within Subjects Effect > cat:timepoint > column:Statistics > cat:Mauchly's W"), 1e-9)
expect_close("Repeated measures", "Greenhouse-Geisser epsilon", rm_sphericity$epsilon,
             ref_cell(14L, "Mauchly Test", "layer:Measure > cat:MEASURE_1 > row:Within Subjects Effect > cat:timepoint > column:Statistics > group:Epsilon > cat:Greenhouse-Geisser"), 1e-9)

stopifnot(nrow(checks) >= 50L)
cat(sprintf(
  paste0(
    "SPSS 31 general-analysis external validation passed: %d numeric checks across %d sections; ",
    "maximum absolute error %.3g. Repeated-measures checks directly exercise StatEdu helpers.\n"
  ),
  nrow(checks), length(unique(checks$Section)), max(checks$AbsError)
))
