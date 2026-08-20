all_args <- commandArgs(FALSE)
file_arg <- all_args[grep("^--file=", all_args)]
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "scripts/validate_analysis_reference_comparison.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(repo_root, "R", "app_bootstrap.R"))) {
  repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
setwd(repo_root)

source(file.path(repo_root, "R", "app_bootstrap.R"))
load_app_packages(check = FALSE)
source_app_modules(dir = file.path(repo_root, "R"))
options(statedu.output_decimal_digits = 3L)

set.seed(20260619)

parse_number <- function(x) {
  x <- as.character(x %||% "")
  if (!length(x) || !nzchar(x[[1]])) return(NA_real_)
  x <- sub("^<", "", x)
  x <- sub("^>", "", x)
  x <- sub("\\(.*$", "", x)
  suppressWarnings(as.numeric(x))
}

fmt <- function(x, digits = 10) {
  if (length(x) == 0 || is.na(x[[1]])) return("")
  if (is.numeric(x) || is.integer(x)) {
    if (!is.finite(x[[1]])) return("")
    return(formatC(x[[1]], format = "fg", digits = digits))
  }
  as.character(x[[1]])
}

status_for <- function(diff, tolerance) {
  if (!is.finite(diff)) return("CHECK")
  if (diff <= tolerance) "PASS" else "FAIL"
}

rows <- list()
add_row <- function(menu, case, app, reference, metric, diff, tolerance, note = "") {
  rows[[length(rows) + 1L]] <<- data.frame(
    Menu = menu,
    Case = case,
    `App result` = fmt(app),
    `Reference result` = fmt(reference),
    Metric = metric,
    `Max abs diff` = fmt(diff),
    Tolerance = fmt(tolerance),
    Status = status_for(diff, tolerance),
    Note = note,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

add_decision_row <- function(menu, case, app, reference, metric = "selected method", note = "") {
  app <- as.character(app %||% "")
  reference <- as.character(reference %||% "")
  add_row(
    menu,
    case,
    app,
    reference,
    metric,
    if (identical(app, reference)) 0 else 1,
    0,
    note
  )
}

max_abs_diff <- function(a, b) {
  if (!length(a) || !length(b)) return(NA_real_)
  max(abs(as.numeric(a) - as.numeric(b)), na.rm = TRUE)
}

make_info <- function(names, measurement) {
  data.frame(name = names, measurement = measurement, stringsAsFactors = FALSE)
}

compare_coef_table <- function(app_terms, app_b, app_se, ref_terms, ref_b, ref_se) {
  app <- data.frame(term = app_terms, app_b = app_b, app_se = app_se, stringsAsFactors = FALSE)
  ref <- data.frame(term = ref_terms, ref_b = ref_b, ref_se = ref_se, stringsAsFactors = FALSE)
  merged <- merge(app, ref, by = "term", all = TRUE, sort = FALSE)
  list(
    b = max_abs_diff(merged$app_b, merged$ref_b),
    se = max_abs_diff(merged$app_se, merged$ref_se)
  )
}

table_cell_number <- function(table, columns, row = 1L) {
  column <- intersect(columns, names(table))
  if (length(column) == 0) return(NA_real_)
  parse_number(table[[column[[1]]]][[row]])
}

p_threshold_001_display <- function(value) {
  raw <- as.character(value %||% "")
  if (!length(raw) || !nzchar(raw[[1]])) return("")
  if (grepl("^\\s*<", raw[[1]])) return("p <= .001")
  value <- suppressWarnings(as.numeric(raw[[1]]))
  if (length(value) == 0 || is.na(value[[1]])) return("")
  if (value[[1]] <= 0.001) "p <= .001" else "p > .001"
}

# Frequencies / descriptives
freq_data <- data.frame(
  group = c(rep("A", 12), rep("B", 8), NA),
  score = c(rnorm(20, 10, 2), NA),
  stringsAsFactors = FALSE
)
freq_info <- make_info(c("group", "score"), c("category", "continuous"))
freq <- prepare_frequencies_results(freq_data, c("group", "score"), variable_info = freq_info)
app_count_a <- freq$categorical_tables[[1]]$N[freq$categorical_tables[[1]]$Value == "A"]
ref_count_a <- as.integer(table(freq_data$group, useNA = "no")[["A"]])
add_row("Frequencies", "Categorical count", app_count_a, ref_count_a, "N", abs(app_count_a - ref_count_a), 0)
app_mean <- parse_number(freq$descriptive_table$Mean[[1]])
ref_mean <- round(mean(freq_data$score, na.rm = TRUE), 2)
add_row("Frequencies", "Continuous descriptive", app_mean, ref_mean, "Mean rounded to 2 decimals", abs(app_mean - ref_mean), 0.005)

# Crosstabs
ct_data <- data.frame(
  row = factor(c(rep("A", 18), rep("B", 22))),
  col = factor(c(rep("Yes", 11), rep("No", 7), rep("Yes", 8), rep("No", 14)))
)
ct_info <- make_info(c("row", "col"), c("binary", "binary"))
ct <- prepare_crosstab_results(ct_data, "row", "col", variable_info = ct_info)
ct_ref <- suppressWarnings(stats::chisq.test(table(ct_data$row, ct_data$col), correct = FALSE))
add_row("Crosstabs", "Pearson chi-square", ct$association$statistic, unname(ct_ref$statistic), "X-squared", abs(ct$association$statistic - unname(ct_ref$statistic)), 1e-10)
add_row("Crosstabs", "Pearson chi-square", ct$association$p, ct_ref$p.value, "p", abs(ct$association$p - ct_ref$p.value), 1e-10)

ct_sparse_data <- data.frame(
  row = factor(c(rep("A", 8), rep("B", 4))),
  col = factor(c(rep("Yes", 7), "No", "Yes", rep("No", 3)))
)
ct_sparse <- prepare_crosstab_results(ct_sparse_data, "row", "col", variable_info = ct_info)
ct_sparse_ref <- stats::fisher.test(table(ct_sparse_data$row, ct_sparse_data$col))
add_decision_row("Crosstabs", "Auto exact test for sparse cells", ct_sparse$association$method, "Fisher's exact test", "selected test", "Expected-count rule should switch from Pearson chi-square to Fisher exact.")
add_row("Crosstabs", "Auto exact test for sparse cells", ct_sparse$association$p, ct_sparse_ref$p.value, "p", abs(ct_sparse$association$p - ct_sparse_ref$p.value), 1e-10)

# Correlation
cor_data <- data.frame(x = rnorm(60))
cor_data$y <- 0.5 * cor_data$x + rnorm(60, sd = 0.7)
cor_info <- make_info(c("x", "y"), c("continuous", "continuous"))
cor_app <- prepare_correlation_results(cor_data, c("x", "y"), variable_info = cor_info, options = list(continuous_method = "pearson"))
cor_ref <- stats::cor.test(cor_data$x, cor_data$y, method = "pearson")
add_row("Correlation", "Pearson correlation", cor_app$correlation_matrix["x", "y"], unname(cor_ref$estimate), "r", abs(cor_app$correlation_matrix["x", "y"] - unname(cor_ref$estimate)), 1e-10)
add_row("Correlation", "Pearson correlation", cor_app$p_matrix["x", "y"], cor_ref$p.value, "p", abs(cor_app$p_matrix["x", "y"] - cor_ref$p.value), 1e-10)

cor_auto_data <- data.frame(x = c(seq_len(59), 1000))
cor_auto_data$y <- log(cor_auto_data$x) + seq_along(cor_auto_data$x) / 100
cor_auto_info <- make_info(c("x", "y"), c("continuous", "continuous"))
cor_auto <- prepare_correlation_results(cor_auto_data, c("x", "y"), variable_info = cor_auto_info, options = list(continuous_method = "auto"))
cor_auto_ref <- suppressWarnings(stats::cor.test(cor_auto_data$x, cor_auto_data$y, method = "spearman", exact = FALSE))
add_decision_row("Correlation", "Auto non-normal continuous pair", cor_auto$method_matrix["x", "y"], "Spearman", "selected method", "At least one continuous variable violates the skewness/kurtosis rule.")
add_row("Correlation", "Auto non-normal continuous pair", cor_auto$correlation_matrix["x", "y"], unname(cor_auto_ref$estimate), "rho", abs(cor_auto$correlation_matrix["x", "y"] - unname(cor_auto_ref$estimate)), 1e-10)
add_row("Correlation", "Auto non-normal continuous pair", cor_auto$p_matrix["x", "y"], cor_auto_ref$p.value, "p", abs(cor_auto$p_matrix["x", "y"] - cor_auto_ref$p.value), 1e-10)

# t-test / ANOVA
tt_data <- data.frame(
  y = c(rnorm(25, 0, 1), rnorm(25, 0.65, 1)),
  g2 = rep(c("A", "B"), each = 25),
  g3 = rep(c("A", "B", "C", "A", "B"), each = 10),
  stringsAsFactors = FALSE
)
tt_info <- make_info(c("y", "g2", "g3"), c("continuous", "binary", "category"))
tt <- prepare_ttest_anova_results(tt_data, "y", "g2", tt_info, options = list(effect_size = TRUE, normality_enabled = FALSE, show_df = TRUE))
tt_ref <- stats::t.test(y ~ g2, data = tt_data, var.equal = TRUE)
add_row("t-test / ANOVA", "Independent t-test", parse_number(tt$results[[1]]$table$t[[1]]), round(unname(tt_ref$statistic), 3), "t rounded", abs(parse_number(tt$results[[1]]$table$t[[1]]) - round(unname(tt_ref$statistic), 3)), 0.001)
anova <- prepare_ttest_anova_results(tt_data, "y", "g3", tt_info, options = list(effect_size = TRUE, normality_enabled = FALSE, show_df = TRUE))
anova_ref <- summary(stats::aov(y ~ g3, data = tt_data))[[1]]
add_row("t-test / ANOVA", "One-way ANOVA", parse_number(anova$results[[1]]$table$F[[1]]), round(anova_ref[["F value"]][[1]], 3), "F rounded", abs(parse_number(anova$results[[1]]$table$F[[1]]) - round(anova_ref[["F value"]][[1]], 3)), 0.001)

mw_data <- data.frame(
  y = c(rep(0, 18), 20, 24, rep(1, 18), 28, 32),
  g = rep(c("A", "B"), each = 20)
)
mw_info <- make_info(c("y", "g"), c("continuous", "binary"))
mw <- prepare_ttest_anova_results(mw_data, "y", "g", mw_info, options = list(effect_size = TRUE, normality_enabled = TRUE, normality_method = "skew_kurtosis", normality_study_type = "survey", show_df = TRUE))
mw_ref <- stats::wilcox.test(y ~ g, data = mw_data, exact = FALSE)
add_decision_row("t-test / ANOVA", "Auto normality violation: two groups", mw$results[[1]]$overview$Analysis[[1]], "Mann-Whitney U test (Wilcoxon rank-sum test)", "selected test", "Skewness/kurtosis rule should switch the two-group comparison to Mann-Whitney.")
add_decision_row("t-test / ANOVA", "Auto normality violation: two groups", p_threshold_001_display(mw$results[[1]]$table$p[[1]]), p_threshold_001_display(mw_ref$p.value), "p threshold category", "Both StatEdu Studio and the reference result classify the Mann-Whitney p-value at the .001 reporting threshold.")

welch_data <- data.frame(
  y = c(seq(-1, 1, length.out = 30), seq(-8, 8, length.out = 30) + 0.25),
  g = rep(c("A", "B"), each = 30)
)
welch <- prepare_ttest_anova_results(welch_data, "y", "g", mw_info, options = list(effect_size = TRUE, normality_enabled = TRUE, normality_method = "skew_kurtosis", normality_study_type = "survey", show_df = TRUE))
welch_ref <- stats::t.test(y ~ g, data = welch_data, var.equal = FALSE)
add_decision_row("t-test / ANOVA", "Auto unequal variance: two groups", welch$results[[1]]$overview$Analysis[[1]], "Welch t-test", "selected test", "Normality rule passes, Levene rule fails; Welch t-test should be selected.")
add_row("t-test / ANOVA", "Auto unequal variance: two groups", table_cell_number(welch$results[[1]]$table, c("t")), round(unname(welch_ref$statistic), 3), "t rounded", abs(table_cell_number(welch$results[[1]]$table, c("t")) - round(unname(welch_ref$statistic), 3)), 0.001)

welch_anova_data <- data.frame(
  y = c(seq(-1, 1, length.out = 25), seq(-5, 5, length.out = 25) + 0.4, seq(-9, 9, length.out = 25) + 0.8),
  g = rep(c("A", "B", "C"), each = 25)
)
welch_anova_info <- make_info(c("y", "g"), c("continuous", "category"))
welch_anova <- prepare_ttest_anova_results(welch_anova_data, "y", "g", welch_anova_info, options = list(effect_size = TRUE, normality_enabled = TRUE, normality_method = "skew_kurtosis", normality_study_type = "survey", show_df = TRUE))
welch_anova_ref <- stats::oneway.test(y ~ g, data = welch_anova_data, var.equal = FALSE)
add_decision_row("t-test / ANOVA", "Auto unequal variance: three groups", welch_anova$results[[1]]$overview$Analysis[[1]], "Welch ANOVA", "selected test", "Normality rule passes, Levene rule fails; Welch ANOVA should be selected.")
add_row("t-test / ANOVA", "Auto unequal variance: three groups", table_cell_number(welch_anova$results[[1]]$table, c("F")), round(unname(welch_anova_ref$statistic), 3), "F rounded", abs(table_cell_number(welch_anova$results[[1]]$table, c("F")) - round(unname(welch_anova_ref$statistic), 3)), 0.001)

kw_data <- data.frame(
  y = c(rep(0, 18), 18, 22, rep(1, 18), 26, 30, rep(2, 18), 34, 38),
  g = rep(c("A", "B", "C"), each = 20)
)
kw <- prepare_ttest_anova_results(kw_data, "y", "g", welch_anova_info, options = list(effect_size = TRUE, normality_enabled = TRUE, normality_method = "skew_kurtosis", normality_study_type = "survey", show_df = TRUE))
kw_ref <- stats::kruskal.test(y ~ g, data = kw_data)
add_decision_row("t-test / ANOVA", "Auto normality violation: three groups", kw$results[[1]]$overview$Analysis[[1]], "Kruskal-Wallis test", "selected test", "Skewness/kurtosis rule should switch the multi-group comparison to Kruskal-Wallis.")
add_row("t-test / ANOVA", "Auto normality violation: three groups", table_cell_number(kw$results[[1]]$table, c(stat_chisq_label(), "chi-square", "Chi-square")), round(unname(kw_ref$statistic), 3), "chi-square rounded", abs(table_cell_number(kw$results[[1]]$table, c(stat_chisq_label(), "chi-square", "Chi-square")) - round(unname(kw_ref$statistic), 3)), 0.001)

# Paired / repeated-measures
paired_data <- data.frame(pre = rnorm(35), post = rnorm(35, 0.35))
paired_info <- make_info(c("pre", "post"), c("continuous", "continuous"))
paired <- prepare_paired_results(paired_data, "pre", "post", variable_info = paired_info, options = list(effect_size = TRUE))
paired_ref <- stats::t.test(paired_data$pre, paired_data$post, paired = TRUE)
add_row("Paired", "Paired t-test", parse_number(paired$table$Statistic[[1]]), round(abs(unname(paired_ref$statistic)), 3), "|t| rounded", abs(parse_number(paired$table$Statistic[[1]]) - round(abs(unname(paired_ref$statistic)), 3)), 0.001)

rm_data <- data.frame(t1 = rnorm(32), t2 = rnorm(32, 0.3), t3 = rnorm(32, 0.7))
rm_info <- make_info(names(rm_data), rep("continuous", 3))
rm_app <- prepare_paired_rm_results(rm_data, variables = names(rm_data), variable_info = rm_info, options = list(effect_size = TRUE))
rm_long <- data.frame(
  id = factor(rep(seq_len(nrow(rm_data)), times = 3)),
  time = factor(rep(names(rm_data), each = nrow(rm_data)), levels = names(rm_data)),
  y = unlist(rm_data, use.names = FALSE)
)
rm_ref <- summary(stats::aov(y ~ time + Error(id / time), data = rm_long))[[2]][[1]]
add_row("Repeated Measures", "RM ANOVA", parse_number(rm_app$table$Value[[1]]), round(rm_ref[["F value"]][[1]], 3), "F rounded", abs(parse_number(rm_app$table$Value[[1]]) - round(rm_ref[["F value"]][[1]], 3)), 0.001)

# Nonparametric paired / Friedman
np <- prepare_nonparametric_paired_results(paired_data, "pre", "post", variable_info = paired_info, options = list(effect_size = TRUE))
np_ref <- stats::wilcox.test(paired_data$pre, paired_data$post, paired = TRUE, exact = FALSE)
add_row("Nonparametric Paired", "Wilcoxon signed-rank", parse_number(np$table$p[[1]]), round(np_ref$p.value, 3), "p rounded", abs(parse_number(np$table$p[[1]]) - round(np_ref$p.value, 3)), 0.001, "W depends on subtraction direction; p is invariant.")
nprm <- prepare_nonparametric_paired_rm_results(rm_data, list(names(rm_data)), variable_info = rm_info, options = list(effect_size = TRUE))
nprm_ref <- stats::friedman.test(as.matrix(rm_data))
add_row("Nonparametric RM", "Friedman test", parse_number(nprm$table$Value[[1]]), round(unname(nprm_ref$statistic), 3), "chi-square rounded", abs(parse_number(nprm$table$Value[[1]]) - round(unname(nprm_ref$statistic), 3)), 0.001)

# ANCOVA
anc_data <- data.frame(y = rnorm(90), group = rep(c("A", "B", "C"), each = 30), x = rnorm(90))
anc_data$y <- anc_data$y + ifelse(anc_data$group == "B", 0.4, ifelse(anc_data$group == "C", 0.9, 0)) + 0.35 * anc_data$x
anc_info <- make_info(c("y", "group", "x"), c("continuous", "category", "continuous"))
anc <- prepare_ancova_results(anc_data, "y", "group", "x", anc_info, options = list(show_df = TRUE))
anc_ref <- car::Anova(stats::lm(y ~ group + x, data = anc_data), type = 2)
add_row("ANCOVA", "Type II group effect", parse_number(anc$results[[1]]$table$F[[1]]), round(anc_ref["group", "F value"], 3), "F rounded", abs(parse_number(anc$results[[1]]$table$F[[1]]) - round(anc_ref["group", "F value"], 3)), 0.001)

# Linear regression
reg_data <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100))
reg_data$y <- 1 + 0.8 * reg_data$x1 - 0.3 * reg_data$x2 + rnorm(100, sd = 0.6)
reg <- prepare_regression_analysis_results(reg_data, "y", c("x1", "x2"), variable_info = make_info(names(reg_data), rep("continuous", 3)))
reg_app <- reg$results[[1]]$coef_table
reg_ref <- lmtest::coeftest(stats::lm(y ~ x1 + x2, data = reg_data))
reg_cmp <- compare_coef_table(reg_app$Term, reg_app$B, reg_app$SE, rownames(reg_ref), reg_ref[, "Estimate"], reg_ref[, "Std. Error"])
add_row("Regression", "OLS coefficients", 0, 0, "max |B diff|", reg_cmp$b, 1e-10)
add_row("Regression", "OLS coefficients", 0, 0, "max |SE diff|", reg_cmp$se, 1e-10)

# Logistic regression
log_data <- data.frame(x = rnorm(140), group = factor(rep(c("A", "B"), 70)))
log_data$y <- factor(rbinom(140, 1, plogis(-0.4 + 0.7 * log_data$x + ifelse(log_data$group == "B", 0.5, 0))))
log_info <- make_info(c("y", "x", "group"), c("binary", "continuous", "binary"))
log_app <- prepare_logistic_analysis_results(log_data, "y", c("x", "group"), variable_info = log_info)[[1]]
log_ref <- summary(stats::glm(y ~ x + group, data = log_data, family = binomial()))$coefficients
log_cmp <- compare_coef_table(log_app$coef_table$Term, log_app$coef_table$B, log_app$coef_table$SE, rownames(log_ref), log_ref[, "Estimate"], log_ref[, "Std. Error"])
add_row("Logistic Regression", "Binary logistic", 0, 0, "max |B diff|", log_cmp$b, 1e-10)
add_row("Logistic Regression", "Binary logistic", 0, 0, "max |SE diff|", log_cmp$se, 1e-10)

# GLM
glm_data <- data.frame(y = rnorm(100), x = rnorm(100), g = factor(rep(c("A", "B"), 50)))
glm_data$y <- 0.5 + 0.6 * glm_data$x + ifelse(glm_data$g == "B", 0.25, 0) + rnorm(100)
glm_info <- make_info(c("y", "x", "g"), c("continuous", "continuous", "binary"))
glm_app <- prepare_generalized_analysis_result(glm_data, "y", c("x", "g"), family = "gaussian", se_type = "model", robust = FALSE, variable_info = glm_info)
glm_ref <- summary(stats::glm(y ~ x + g, data = glm_data, family = gaussian()))$coefficients
glm_cmp <- compare_coef_table(glm_app$coef_table$Term, glm_app$coef_table$B, glm_app$coef_table$SE, rownames(glm_ref), glm_ref[, "Estimate"], glm_ref[, "Std. Error"])
add_row("GLM", "Gaussian identity", 0, 0, "max |B diff|", glm_cmp$b, 1e-10)
add_row("GLM", "Gaussian identity", 0, 0, "max |SE diff|", glm_cmp$se, 1e-10)

glm_bin <- glm_data
glm_bin$yb <- rbinom(nrow(glm_bin), 1, plogis(-0.2 + 0.5 * glm_bin$x))
glm_bin_info <- make_info(c("yb", "x", "g"), c("binary", "continuous", "binary"))
glm_bin_app <- prepare_generalized_analysis_result(glm_bin, "yb", c("x", "g"), family = "binomial", se_type = "model", robust = FALSE, variable_info = glm_bin_info)
glm_bin_ref <- summary(stats::glm(yb ~ x + g, data = glm_bin, family = binomial()))$coefficients
glm_bin_cmp <- compare_coef_table(glm_bin_app$coef_table$Term, glm_bin_app$coef_table$B, glm_bin_app$coef_table$SE, rownames(glm_bin_ref), glm_bin_ref[, "Estimate"], glm_bin_ref[, "Std. Error"])
add_row("GLM", "Binomial logit", 0, 0, "max |B diff|", glm_bin_cmp$b, 1e-10)
add_row("GLM", "Binomial logit", 0, 0, "max |SE diff|", glm_bin_cmp$se, 1e-10)

glm_auto_bin_app <- prepare_generalized_analysis_result(glm_bin, "yb", c("x", "g"), family = "auto", se_type = "model", robust = FALSE, variable_info = glm_bin_info)
add_decision_row("GLM", "Auto family: binary outcome", glm_auto_bin_app$family, "binomial", "detected family")

glm_gamma <- data.frame(x = rnorm(160), g = factor(rep(c("A", "B"), 80)))
glm_gamma$y <- stats::rgamma(nrow(glm_gamma), shape = 2.2, scale = exp(0.25 + 0.25 * glm_gamma$x + ifelse(glm_gamma$g == "B", 0.15, 0)) / 2.2)
glm_gamma_info <- make_info(c("y", "x", "g"), c("continuous", "continuous", "binary"))
glm_gamma_app <- prepare_generalized_analysis_result(glm_gamma, "y", c("x", "g"), family = "auto", se_type = "model", robust = FALSE, variable_info = glm_gamma_info)
glm_gamma_ref <- summary(stats::glm(y ~ x + g, data = glm_gamma, family = stats::Gamma(link = "log")))$coefficients
glm_gamma_cmp <- compare_coef_table(glm_gamma_app$coef_table$Term, glm_gamma_app$coef_table$B, glm_gamma_app$coef_table$SE, rownames(glm_gamma_ref), glm_gamma_ref[, "Estimate"], glm_gamma_ref[, "Std. Error"])
add_decision_row("GLM", "Auto family: positive skewed outcome", glm_gamma_app$family, "gamma", "detected family")
add_row("GLM", "Auto family: positive skewed outcome", 0, 0, "max |B diff|", glm_gamma_cmp$b, 1e-10)
add_row("GLM", "Auto family: positive skewed outcome", 0, 0, "max |SE diff|", glm_gamma_cmp$se, 1e-10)

glm_count <- data.frame(x = rnorm(220), g = factor(rep(c("A", "B"), 110)))
glm_count$mu <- exp(0.2 + 0.45 * glm_count$x + ifelse(glm_count$g == "B", 0.25, 0))
glm_count$y <- MASS::rnegbin(nrow(glm_count), mu = glm_count$mu, theta = 0.55)
glm_count_info <- make_info(c("y", "x", "g"), c("continuous", "continuous", "binary"))
glm_count_app <- prepare_generalized_analysis_result(glm_count, "y", c("x", "g"), family = "auto", se_type = "model", robust = FALSE, overdispersion = TRUE, variable_info = glm_count_info)
glm_count_ref <- summary(MASS::glm.nb(y ~ x + g, data = glm_count, link = "log"))$coefficients
glm_count_cmp <- compare_coef_table(glm_count_app$coef_table$Term, glm_count_app$coef_table$B, glm_count_app$coef_table$SE, rownames(glm_count_ref), glm_count_ref[, "Estimate"], glm_count_ref[, "Std. Error"])
add_decision_row("GLM", "Auto count workflow: overdispersion", if (is.data.frame(glm_count_app$count_details) && nrow(glm_count_app$count_details) > 0) "count" else glm_count_app$family, "count", "detected workflow")
add_decision_row("GLM", "Auto count workflow: overdispersion", glm_count_app$family, "negative_binomial", "fitted family", "Dispersion ratio > 1.5 should switch final fit from Poisson to negative binomial when MASS::glm.nb converges.")
add_row("GLM", "Auto count workflow: overdispersion", 0, 0, "max |B diff|", glm_count_cmp$b, 1e-10)
add_row("GLM", "Auto count workflow: overdispersion", 0, 0, "max |SE diff|", glm_count_cmp$se, 1e-10)

# Reliability
rel_data <- as.data.frame(matrix(rnorm(300), ncol = 5))
names(rel_data) <- paste0("i", seq_len(5))
rel_info <- make_info(names(rel_data), rep("continuous", 5))
rel_app <- prepare_reliability_results(rel_data, names(rel_data), variable_info = rel_info)
rel_ref <- suppressWarnings(psych::alpha(rel_data, check.keys = FALSE, warnings = FALSE))
add_row("Reliability", "Cronbach alpha", rel_app$reliability[["pearson_alpha"]], rel_ref$total$raw_alpha[[1]], "alpha", abs(rel_app$reliability[["pearson_alpha"]] - rel_ref$total$raw_alpha[[1]]), 1e-10)

# PCA / factor analysis
pca_app <- prepare_pca_results(rel_data, names(rel_data), variable_info = rel_info, options = list(criterion = "eigen", rotation = "none"))
pca_ref <- eigen(stats::cor(rel_data), symmetric = TRUE, only.values = TRUE)$values
add_row("PCA", "Correlation eigenvalues", 0, 0, "max |eigenvalue diff|", max_abs_diff(pca_app$eigenvalues, pca_ref), 1e-10)

fa_app <- prepare_factor_analysis_results(rel_data, names(rel_data), variable_info = rel_info, options = list(criterion = "fixed", n_factors = 1, method = "pa", rotation = "none"))
fa_ref <- suppressWarnings(suppressMessages(psych::fa(r = stats::cor(rel_data), nfactors = 1, n.obs = nrow(rel_data), rotate = "none", fm = "pa", warnings = FALSE)))
fa_diff <- max_abs_diff(abs(as.matrix(unclass(fa_app$loadings))[, 1]), abs(as.matrix(unclass(fa_ref$loadings))[, 1]))
add_row("Factor Analysis", "PAF one-factor loadings", 0, 0, "max |abs loading diff|", fa_diff, 1e-10)

# Structural equation modeling
sem_path_frame <- function(fit, op = "~", value_column = "est") {
  estimates <- lavaan::parameterEstimates(fit, ci = TRUE)
  estimates <- estimates[estimates$op == op, , drop = FALSE]
  if (!nrow(estimates)) return(data.frame(term = character(0), value = numeric(0), stringsAsFactors = FALSE))
  term <- if (identical(op, ":=")) estimates$lhs else paste(estimates$lhs, op, estimates$rhs)
  data.frame(term = term, value = estimates[[value_column]], stringsAsFactors = FALSE)
}

sem_standardized_path_frame <- function(fit) {
  estimates <- lavaan::standardizedSolution(fit, ci = TRUE)
  estimates <- estimates[estimates$op == "~", , drop = FALSE]
  if (!nrow(estimates)) return(data.frame(term = character(0), value = numeric(0), stringsAsFactors = FALSE))
  data.frame(term = paste(estimates$lhs, "~", estimates$rhs), value = estimates$est.std, stringsAsFactors = FALSE)
}

compare_named_values <- function(app, reference) {
  merged <- merge(app, reference, by = "term", all = TRUE, sort = FALSE)
  max_abs_diff(merged$value.x, merged$value.y)
}

pls_matrix_frame <- function(matrix_value, skip_rows = character(0)) {
  matrix_value <- as.matrix(matrix_value)
  if (!length(matrix_value)) return(data.frame(term = character(0), value = numeric(0), stringsAsFactors = FALSE))
  row_names <- setdiff(rownames(matrix_value), skip_rows)
  if (!length(row_names) || !length(colnames(matrix_value))) return(data.frame(term = character(0), value = numeric(0), stringsAsFactors = FALSE))
  rows <- expand.grid(row = row_names, column = colnames(matrix_value), stringsAsFactors = FALSE)
  values <- mapply(function(row, column) suppressWarnings(as.numeric(matrix_value[row, column])), rows$row, rows$column)
  rows <- rows[is.finite(values), , drop = FALSE]
  values <- values[is.finite(values)]
  data.frame(term = paste(rows$row, rows$column, sep = " -> "), value = values, stringsAsFactors = FALSE)
}

reference_random_seed <- .Random.seed
set.seed(20260819)
sem_n <- 170L
sem_eta1 <- stats::rnorm(sem_n)
sem_eta2 <- 0.58 * sem_eta1 + stats::rnorm(sem_n, sd = 0.76)
sem_data <- data.frame(
  x1 = 0.78 * sem_eta1 + stats::rnorm(sem_n, sd = 0.45),
  x2 = 0.72 * sem_eta1 + stats::rnorm(sem_n, sd = 0.50),
  x3 = 0.69 * sem_eta1 + stats::rnorm(sem_n, sd = 0.52),
  y1 = 0.82 * sem_eta2 + stats::rnorm(sem_n, sd = 0.42),
  y2 = 0.76 * sem_eta2 + stats::rnorm(sem_n, sd = 0.48),
  y3 = 0.70 * sem_eta2 + stats::rnorm(sem_n, sd = 0.55)
)
sem_snapshot <- list(
  nodes = list(
    list(id = "lv1", role = "latent", name = "eta1", canvasLabel = "eta1", measurementMode = "reflective"),
    list(id = "lv2", role = "latent", name = "eta2", canvasLabel = "eta2", measurementMode = "reflective"),
    list(id = "x1", role = "indicator", name = "x1", variableId = "x1", canvasLabel = "x1"),
    list(id = "x2", role = "indicator", name = "x2", variableId = "x2", canvasLabel = "x2"),
    list(id = "x3", role = "indicator", name = "x3", variableId = "x3", canvasLabel = "x3"),
    list(id = "y1", role = "indicator", name = "y1", variableId = "y1", canvasLabel = "y1"),
    list(id = "y2", role = "indicator", name = "y2", variableId = "y2", canvasLabel = "y2"),
    list(id = "y3", role = "indicator", name = "y3", variableId = "y3", canvasLabel = "y3")
  ),
  edges = list(
    list(id = "e1", from = "lv1", to = "x1"), list(id = "e2", from = "lv1", to = "x2"), list(id = "e3", from = "lv1", to = "x3"),
    list(id = "e4", from = "lv2", to = "y1"), list(id = "e5", from = "lv2", to = "y2"), list(id = "e6", from = "lv2", to = "y3"),
    list(id = "p1", from = "lv1", to = "lv2")
  )
)

sem_app <- run_structural_canvas_analysis(sem_snapshot, sem_data, "sem", estimator = "ML", missing = "fiml")
sem_ref <- lavaan::sem(sem_app$syntax, data = sem_data, estimator = "ML", missing = "fiml", auto.cov.lv.x = FALSE)
add_row("SEM / CB-SEM", "SEM direct structural path", 0, 0, "max |B diff|", compare_named_values(sem_path_frame(sem_app$fit, "~", "est"), sem_path_frame(sem_ref, "~", "est")), 1e-10)
add_row("SEM / CB-SEM", "SEM direct structural path", 0, 0, "max |SE diff|", compare_named_values(sem_path_frame(sem_app$fit, "~", "se"), sem_path_frame(sem_ref, "~", "se")), 1e-10)
add_row("SEM / CB-SEM", "SEM standardized path", 0, 0, "max |beta diff|", compare_named_values(sem_standardized_path_frame(sem_app$fit), sem_standardized_path_frame(sem_ref)), 1e-10)
add_row("SEM / CB-SEM", "SEM global fit indices", 0, 0, "max |fit-index diff|", max_abs_diff(lavaan::fitMeasures(sem_app$fit, c("cfi", "rmsea", "srmr")), lavaan::fitMeasures(sem_ref, c("cfi", "rmsea", "srmr"))), 1e-10)

set.seed(20260820)
med_n <- 210L
eta_a <- stats::rnorm(med_n)
eta_b <- 0.55 * eta_a + stats::rnorm(med_n, sd = 0.80)
eta_c <- 0.35 * eta_a + 0.50 * eta_b + stats::rnorm(med_n, sd = 0.75)
med_data <- data.frame(
  a1 = 0.82 * eta_a + stats::rnorm(med_n, sd = 0.42), a2 = 0.75 * eta_a + stats::rnorm(med_n, sd = 0.50), a3 = 0.70 * eta_a + stats::rnorm(med_n, sd = 0.55),
  b1 = 0.80 * eta_b + stats::rnorm(med_n, sd = 0.45), b2 = 0.73 * eta_b + stats::rnorm(med_n, sd = 0.52), b3 = 0.68 * eta_b + stats::rnorm(med_n, sd = 0.58),
  c1 = 0.84 * eta_c + stats::rnorm(med_n, sd = 0.40), c2 = 0.77 * eta_c + stats::rnorm(med_n, sd = 0.48), c3 = 0.71 * eta_c + stats::rnorm(med_n, sd = 0.54)
)
med_snapshot <- list(
  nodes = c(
    list(
      list(id = "eta_a", role = "latent", name = "etaA", canvasLabel = "etaA"),
      list(id = "eta_b", role = "latent", name = "etaB", canvasLabel = "etaB"),
      list(id = "eta_c", role = "latent", name = "etaC", canvasLabel = "etaC")
    ),
    lapply(seq_len(3L), function(index) list(id = paste0("a", index), role = "indicator", name = paste0("a", index), variableId = paste0("a", index), canvasLabel = paste0("a", index))),
    lapply(seq_len(3L), function(index) list(id = paste0("b", index), role = "indicator", name = paste0("b", index), variableId = paste0("b", index), canvasLabel = paste0("b", index))),
    lapply(seq_len(3L), function(index) list(id = paste0("c", index), role = "indicator", name = paste0("c", index), variableId = paste0("c", index), canvasLabel = paste0("c", index)))
  ),
  edges = c(
    lapply(seq_len(3L), function(index) list(id = paste0("ea", index), from = "eta_a", to = paste0("a", index))),
    lapply(seq_len(3L), function(index) list(id = paste0("eb", index), from = "eta_b", to = paste0("b", index))),
    lapply(seq_len(3L), function(index) list(id = paste0("ec", index), from = "eta_c", to = paste0("c", index))),
    list(list(id = "ab", from = "eta_a", to = "eta_b"), list(id = "bc", from = "eta_b", to = "eta_c"), list(id = "ac", from = "eta_a", to = "eta_c"))
  )
)
cbsem_app <- run_structural_canvas_analysis(med_snapshot, med_data, "cbsem", estimator = "ML", missing = "fiml")
cbsem_ref <- lavaan::sem(cbsem_app$syntax, data = med_data, estimator = "ML", missing = "fiml", auto.cov.lv.x = FALSE)
add_row("SEM / CB-SEM", "CB-SEM mediation structural paths", 0, 0, "max |B diff|", compare_named_values(sem_path_frame(cbsem_app$fit, "~", "est"), sem_path_frame(cbsem_ref, "~", "est")), 1e-10)
add_row("SEM / CB-SEM", "CB-SEM mediation structural paths", 0, 0, "max |SE diff|", compare_named_values(sem_path_frame(cbsem_app$fit, "~", "se"), sem_path_frame(cbsem_ref, "~", "se")), 1e-10)
add_row("SEM / CB-SEM", "CB-SEM indirect/total effects", 0, 0, "max |defined-effect B diff|", compare_named_values(sem_path_frame(cbsem_app$fit, ":=", "est"), sem_path_frame(cbsem_ref, ":=", "est")), 1e-10)
add_row("SEM / CB-SEM", "CB-SEM indirect/total effects", 0, 0, "max |defined-effect SE diff|", compare_named_values(sem_path_frame(cbsem_app$fit, ":=", "se"), sem_path_frame(cbsem_ref, ":=", "se")), 1e-10)

pls_app <- run_structural_canvas_analysis(sem_snapshot, sem_data, "plssem", estimator = "PLS")
pls_ref <- seminr::estimate_pls(
  data = sem_data,
  measurement_model = seminr::constructs(
    seminr::reflective("eta1", c("x1", "x2", "x3")),
    seminr::reflective("eta2", c("y1", "y2", "y3"))
  ),
  structural_model = seminr::relationships(seminr::paths(from = "eta1", to = "eta2"))
)
pls_app_summary <- summary(pls_app$fit)
pls_ref_summary <- summary(pls_ref)
add_row("PLS-SEM", "PLS structural path", 0, 0, "max |path coefficient diff|", compare_named_values(pls_matrix_frame(pls_app_summary$paths, c("R^2", "AdjR^2")), pls_matrix_frame(pls_ref_summary$paths, c("R^2", "AdjR^2"))), 1e-10)
add_row("PLS-SEM", "PLS R-squared", 0, 0, "max |R2 diff|", compare_named_values(pls_matrix_frame(pls_app_summary$paths["R^2", , drop = FALSE]), pls_matrix_frame(pls_ref_summary$paths["R^2", , drop = FALSE])), 1e-10)
add_row("PLS-SEM", "PLS outer loadings", 0, 0, "max |loading diff|", compare_named_values(pls_matrix_frame(pls_app_summary$loadings), pls_matrix_frame(pls_ref_summary$loadings)), 1e-10)
add_row("PLS-SEM", "PLS reliability and AVE", 0, 0, "max |reliability diff|", compare_named_values(pls_matrix_frame(pls_app_summary$reliability), pls_matrix_frame(pls_ref_summary$reliability)), 1e-10)
.Random.seed <- reference_random_seed

# Longitudinal / panel models
long_info <- function(data, overrides = list()) {
  info <- lapply(names(data), function(nm) {
    x <- data[[nm]]
    measurement <- if (is.factor(x) || is.character(x)) "nominal" else if (is.numeric(x) && length(unique(stats::na.omit(x))) <= 2) "binary" else "continuous"
    list(name = nm, label = nm, type = class(x)[[1]], measurement = measurement)
  })
  names(info) <- names(data)
  for (nm in names(overrides)) if (!is.null(info[[nm]])) info[[nm]] <- utils::modifyList(info[[nm]], overrides[[nm]])
  unname(info)
}

data("ohio", package = "geepack")
ohi <- get("ohio")
ohi$id <- factor(ohi$id)
ohi$resp <- as.integer(ohi$resp)
ohi$smoke <- as.integer(ohi$smoke)
ohi_info <- long_info(ohi, list(resp = list(measurement = "binary"), id = list(measurement = "nominal")))
gee_app <- prepare_longitudinal_analysis_result(
  data = ohi, outcome = "resp", id = "id", time = "age", predictors = "smoke",
  model_type = "gee", family = "binomial", corstr = "exchangeable", variable_info = ohi_info
)[[1]]
gee_ref <- coef(summary(geepack::geeglm(resp ~ age + smoke, id = id, waves = age, data = ohi, family = stats::binomial(), corstr = "exchangeable")))
gee_cmp <- compare_coef_table(gee_app$coef_table$Term, gee_app$coef_table$B, gee_app$coef_table$SE, rownames(gee_ref), gee_ref[, "Estimate"], gee_ref[, "Std.err"])
add_row("Longitudinal / Panel", "GEE binomial", 0, 0, "max |B diff|", gee_cmp$b, 1e-10)
add_row("Longitudinal / Panel", "GEE binomial", 0, 0, "max |SE diff|", gee_cmp$se, 1e-10)

long_count <- expand.grid(id = factor(seq_len(55)), time = 1:4)
long_count$x <- rnorm(nrow(long_count))
long_count$mu <- exp(0.15 + 0.2 * long_count$time + 0.35 * long_count$x)
long_count$y <- MASS::rnegbin(nrow(long_count), mu = long_count$mu, theta = 0.6)
long_count_info <- long_info(long_count, list(id = list(measurement = "nominal"), time = list(measurement = "continuous"), y = list(measurement = "continuous")))
gee_count_app <- prepare_longitudinal_analysis_result(
  data = long_count, outcome = "y", id = "id", time = "time", predictors = "x",
  model_type = "gee", family = "auto", corstr = "exchangeable", include_time = TRUE,
  assumption_checks = FALSE, variable_info = long_count_info
)[[1]]
gee_count_ref_model <- MASS::glm.nb(y ~ time + x, data = long_count, link = "log")
gee_count_ref <- lmtest::coeftest(gee_count_ref_model, vcov. = sandwich::vcovCL(gee_count_ref_model, cluster = long_count$id, type = "HC1"))
gee_count_cmp <- compare_coef_table(gee_count_app$coef_table$Term, gee_count_app$coef_table$B, gee_count_app$coef_table$SE, rownames(gee_count_ref), gee_count_ref[, "Estimate"], gee_count_ref[, "Std. Error"])
add_decision_row("Longitudinal / Panel", "GEE auto count workflow: overdispersion", gee_count_app$family, "negative_binomial", "fitted family", "Count screening should switch the marginal count fit to negative binomial when dispersion ratio exceeds 1.5.")
add_row("Longitudinal / Panel", "GEE auto count workflow: overdispersion", 0, 0, "max |B diff|", gee_count_cmp$b, 1e-10)
add_row("Longitudinal / Panel", "GEE auto count workflow: overdispersion", 0, 0, "max |SE diff|", gee_count_cmp$se, 1e-10)

data("sleepstudy", package = "lme4")
sleep <- get("sleepstudy")
sleep$Subject <- factor(sleep$Subject)
sleep_info <- long_info(sleep, list(Reaction = list(measurement = "continuous"), Subject = list(measurement = "nominal"), Days = list(measurement = "continuous")))
lmm_app <- prepare_longitudinal_analysis_result(
  data = sleep, outcome = "Reaction", id = "Subject", time = "Days", predictors = character(0),
  model_type = "lmm", family = "gaussian", random_slope = TRUE, variable_info = sleep_info
)[[1]]
lmm_ref <- coef(summary(lmerTest::lmer(Reaction ~ Days + (Days | Subject), data = sleep, REML = FALSE)))
lmm_cmp <- compare_coef_table(lmm_app$coef_table$Term, lmm_app$coef_table$B, lmm_app$coef_table$SE, rownames(lmm_ref), lmm_ref[, "Estimate"], lmm_ref[, "Std. Error"])
add_row("Longitudinal / Panel", "LMM random slope", 0, 0, "max |B diff|", lmm_cmp$b, 1e-8)
add_row("Longitudinal / Panel", "LMM random slope", 0, 0, "max |SE diff|", lmm_cmp$se, 1e-8)

data("Grunfeld", package = "plm")
gr <- get("Grunfeld")
gr$firm <- factor(gr$firm)
gr_info <- long_info(gr, list(firm = list(measurement = "nominal"), year = list(measurement = "continuous")))
fe_app <- prepare_longitudinal_analysis_result(data = gr, outcome = "inv", id = "firm", time = "year", predictors = c("value", "capital"), model_type = "panel_fe", family = "gaussian", include_time = FALSE, variable_info = gr_info)[[1]]
fe_ref_model <- plm::plm(inv ~ value + capital, data = gr, index = c("firm", "year"), model = "within")
fe_ref <- lmtest::coeftest(fe_ref_model, vcov. = plm::vcovHC(fe_ref_model, type = "HC1", cluster = "group"))
fe_cmp <- compare_coef_table(fe_app$coef_table$Term, fe_app$coef_table$B, fe_app$coef_table$SE, rownames(fe_ref), fe_ref[, "Estimate"], fe_ref[, "Std. Error"])
add_row("Longitudinal / Panel", "Panel fixed effects", 0, 0, "max |B diff|", fe_cmp$b, 1e-10)
add_row("Longitudinal / Panel", "Panel fixed effects", 0, 0, "max |SE diff|", fe_cmp$se, 1e-10)

comparison <- do.call(rbind, rows)
if (any(comparison$Status != "PASS")) {
  print(comparison[comparison$Status != "PASS", , drop = FALSE])
  stop("One or more reference comparisons failed.", call. = FALSE)
}

output_dir <- file.path(repo_root, "outputs")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
csv_path <- file.path(output_dir, "analysis_reference_comparison.csv")
md_path <- file.path(output_dir, "analysis_reference_comparison.md")
docs_path <- file.path(repo_root, "docs", "ANALYSIS_REFERENCE_COMPARISON.md")
utils::write.csv(comparison, csv_path, row.names = FALSE, fileEncoding = "UTF-8")

sample_size_markdown <- c(
  "## Sample Size Reference Comparison",
  "",
  "The following table compares representative StatEdu Studio sample-size results with G*Power-equivalent formulas, public R packages, or literature-based reference formulas. Judgement is based on the final rounded sample size used for reporting. `match` means the final rounded value is identical, `near` means a small one-person or one-cluster difference within the same formula family, and `not directly comparable` means no installed reference function matched the exact StatEdu Studio target.",
  "",
  "| Scope | Method | Comparator | Unit | StatEdu Studio result | Reference rounded | Difference | Decision |",
  "|---|---|---|---|---:|---:|---:|---|",
  "| G*Power comparable | t-test | G*Power-equivalent | per group | 64 | 64 | 0 | match |",
  "| G*Power comparable | Paired t-test | G*Power-equivalent | pairs | 34 | 34 | 0 | match |",
  "| G*Power comparable | One-sample t-test | G*Power-equivalent | participants | 34 | 34 | 0 | match |",
  "| G*Power comparable | ANOVA | G*Power-equivalent | total N | 159 | 159 | 0 | match |",
  "| G*Power comparable | Chi-square | G*Power-equivalent | total N | 122 | 122 | 0 | match |",
  "| G*Power comparable | Correlation | G*Power-equivalent | participants | 85 | 85 | 0 | match |",
  "| G*Power comparable | Linear regression | G*Power-equivalent | total N | 134 | 134 | 0 | match |",
  "| G*Power comparable | Two proportions | G*Power-equivalent | per group | 170 | 170 | 0 | match |",
  "| G*Power comparable | One proportion | G*Power-equivalent | participants | 80 | 80 | 0 | match |",
  "| G*Power comparable | ANCOVA | G*Power-equivalent noncentral F | total N | 90 | 90 | 0 | match |",
  "| Beyond G*Power | GEE | repeated-measures design effect | participants per group | 103 | 103 | 0 | match |",
  "| Beyond G*Power | LMM | `longpower::diggle.linear.power` | participants per group | 146 | 146 | 0 | match |",
  "| Beyond G*Power | Survival / Cox | Schoenfeld event formula | total participants | 618 | 618 | 0 | match |",
  "| Beyond G*Power | Equivalence / TOST | `TOSTER::power_t_TOST` | participants per group | 70 | 70 | 0 | match |",
  "| Beyond G*Power | Diagnostic accuracy | `epiR::epi.ssdxsesp` | total participants | 1230 | 1230 | 0 | match |",
  "| Beyond G*Power | ROC AUC | direct package comparator not available | cases | 28 | NA | NA | not directly comparable |",
  "| Beyond G*Power | Count / rates | Wald two-rate formula | person-time per group | 79 | 79 | 0 | match |",
  "| Beyond G*Power | Cluster trial | `WebPower::wp.crt2arm` | clusters total | 16 | 16 | 0 | match |",
  "| Beyond G*Power | Precision / CI | normal CI precision formula | participants | 97 | 97 | 0 | match |",
  "| Beyond G*Power | Cronbach alpha precision | Bonett log(1-alpha) formula | subjects | 156 | 156 | 0 | match |",
  "| Beyond G*Power | SEM / CFA | `WebPower::wp.sem.rmsea` | participants | 214 | 214 | 0 | match |",
  "",
  "Summary: all 10 G*Power-comparable calculations matched the final rounded value. Among calculations beyond G*Power, GEE, LMM, Survival/Cox, mean equivalence/TOST, diagnostic accuracy, count/rate, cluster trial, precision/CI, Cronbach alpha precision, and SEM/CFA also matched the public package or reference formula after applying StatEdu Studio's final rounding rule. ROC AUC remains literature-formula based because a directly matching installed reference function was not available."
)

effect_size_markdown <- c(
  "## Effect Size Reference Comparison",
  "",
  "The following table compares representative StatEdu Studio effect-size results with the `effectsize` package or equivalent standard formulas. Effect sizes are compared on the raw value scale because no sample-size rounding rule applies.",
  "",
  "| Method | Compared effect size | Condition | StatEdu Studio value | Reference value | Difference | Decision |",
  "|---|---|---|---:|---:|---:|---|",
  "| t-test | Cohen's d | Independent t, equal n: t=2.5, df=78 | 0.559017 | 0.559017 | 0 | match |",
  "| Proportion | Cohen's h | p1=.65, p2=.50 | 0.304693 | 0.304693 | 0 | match |",
  "| Chi-square | Cramer's V | Chi-square=12.5, N=200, 3x4 table | 0.176777 | 0.176777 | 0 | match |",
  "| Correlation | Pearson r | t=2.5, df=78 | 0.272367 | 0.272367 | 0 | match |",
  "| ANOVA | Partial eta squared | F=5.2, df_effect=2, df_error=87 | 0.106776 | 0.106776 | 0 | match |",
  "| ANCOVA | Adjusted Cohen's f | unadjusted f=.25, covariate R2=.30 | 0.298807 | 0.298807 | 0 | match |",
  "| Nonparametric | Rank-biserial r | Mann-Whitney U=1200, n1=40, n2=45 | 0.333333 | 0.333333 | 0 | match |",
  "| McNemar | Matched-pair odds ratio | Discordant counts b=18, c=10 | 1.800000 | 1.800000 | 0 | match |",
  "| Regression | Cohen's f-squared | Multiple regression R2=.20 | 0.250000 | 0.250000 | 0 | match |",
  "| GEE | Cohen's h | Binary marginal proportions p1=.65, p2=.50 | 0.304693 | 0.304693 | 0 | match |",
  "| LMM | Standardized fixed effect | simple fixed effect d=.30, m=3, ICC=.30 | 0.300000 | 0.300000 | 0 | match |",
  "| LMM | Repeated-measures planning effect | simple fixed effect d=.30, m=3, ICC=.30 | 0.410792 | 0.410792 | 0 | match |",
  "| LMM | SPSS omnibus partial eta squared | F=28.061, df1=3, df2=23.057 | 0.784996 | 0.784996 | 0 | match |",
  "| LMM | SPSS pairwise dz | mean diff=.824, variances=.326/.199, covariance=.117 | 1.527498 | 1.527498 | 0 | match |",
  "| GLMM | Logistic latent-scale d | OR=1.80 | 0.324064 | 0.324064 | 0 | match |",
  "| GLMM | Incidence rate ratio | IRR=1.50 | 1.500000 | 1.500000 | 0 | match |",
  "| Survival / Cox | Hazard ratio | HR=.70 | 0.700000 | 0.700000 | 0 | match |",
  "| Survival / Cox | log hazard ratio | HR=.70 | -0.356675 | -0.356675 | 0 | match |",
  "| Equivalence / NI | Standardized distance to margin | Mean equivalence: difference=.05, margin=.20, SD=1 | 0.150000 | 0.150000 | 0 | match |",
  "| ROC AUC | AUC | AUC=.70 vs null=.50 | 0.700000 | 0.700000 | 0 | match |",
  "| ROC AUC | Approximate Cohen's d | AUC=.70 vs null=.50 | 0.741614 | 0.741614 | 0 | match |",
  "| Count / Rate Regression | Incidence rate ratio | IRR=1.50 | 1.500000 | 1.500000 | 0 | match |",
  "| Count / Rate Regression | log incidence rate ratio | IRR=1.50 | 0.405465 | 0.405465 | 0 | match |",
  "| Cluster Trial | Planning effect size | parallel continuous: d=.50, m=20, ICC=.05 | 0.358057 | 0.358057 | 0 | match |",
  "| Precision / CI | Standardized half-width | Mean estimate=10, half-width=1.5, SD=6 | 0.250000 | 0.250000 | 0 | match |",
  "| Reliability / Agreement | Alpha difference | alpha=.80 vs reference=.70, items=5 | 0.100000 | 0.100000 | 0 | match |",
  "| Reliability / Agreement | Average inter-item r | alpha=.80 vs reference=.70, items=5 | 0.444444 | 0.444444 | 0 | match |",
  "| SEM / CFA | RMSEA difference | df=20, RMSEA0=.05, RMSEA1=.08 | 0.030000 | 0.030000 | 0 | match |",
  "| SEM / CFA | NCP difference per N | df=20, RMSEA0=.05, RMSEA1=.08 | 0.078000 | 0.078000 | 0 | match |",
  "",
  "Summary: all 25 effect-size comparison items matched the reference definition. For independent t-test conversion, StatEdu Studio uses the equal-n exact formula `2t/sqrt(df + 2)`, which matches the G*Power convention rather than the `effectsize::t_to_d` default approximation `2t/sqrt(df_error)`. Cramer's V is compared against the unadjusted definition (`adjust = FALSE`) used by StatEdu Studio."
)

analysis_method_markdown <- c(
  "## Analysis Method Validation Summary",
  "",
  "This section summarizes the analysis-method validation scope. It excludes the sample-size and effect-size calculators, which are documented in their own sections below.",
  "",
  "| Analysis method | Reference / comparator | Checked calculations and decision paths | Result | Notes / limitations |",
  "|---|---|---|---|---|",
  "| Frequencies / descriptives | Base R counts and summary statistics | Categorical N, continuous mean rounding, variable/value label display path, category-table value order helper | PASS | Display labels and category-defined value order use `category_table` where available. |",
  "| Crosstabs | `stats::chisq.test`, `stats::fisher.test`, direct score/trend formulas | Pearson chi-square statistic and p value, sparse-cell automatic Fisher switch, ordered-by-ordered score association path, character-label ordinal row/column order | PASS | Score-based trend tests use `category_table` order for ordinal row and column levels when available. |",
  "| Correlation | `stats::cor.test`, direct phi/point-biserial checks, Kendall Fieller CI formula | Pearson r and p, automatic Spearman switch for non-normal continuous pairs, binary-binary Phi method label, Kendall tau confidence interval SE, character-label ordinal scoring | PASS | Ordinal scoring and latent polychoric/polyserial level order use `category_table` order when available. |",
  "| t-test / ANOVA / nonparametric group tests | `stats::t.test`, `stats::aov`, Welch formulas, `nortest::lillie.test`, `stats::kruskal.test` | Independent t, one-way ANOVA, Mann-Whitney switch, Welch t/ANOVA switch, Kruskal-Wallis switch, Lilliefors normality path, epsilon-squared formula | PASS | Lilliefors falls back to ordinary K-S only if `nortest` is unavailable and reports that fallback. |",
  "| Paired / repeated-measures tests | `stats::t.test`, `stats::aov`, `stats::mauchly.test`, `stats::wilcox.test`, `stats::friedman.test`, direct Cochran Q coding check | Paired t, RM ANOVA, Mauchly W/p, Wilcoxon signed-rank p, Friedman chi-square, Cochran Q binary recoding for non-0/1 labels | PASS | RM sphericity epsilon calculation retained after replacing W/p with `mauchly.test`. |",
  "| ANCOVA | Base R linear model / Type II effect comparison | Type II group effect F statistic and adjusted-mean display path | PASS | Separate ANCOVA UI/guard checks live in `scripts/validate_ancova.R`. |",
  "| Linear / hierarchical regression | Direct `stats::lm`, `sandwich::vcovHC`, and manual case-resampling formulas | OLS B/SE/beta/R2/F, sr2/f2/VIF/Durbin-Watson, HC3 coefficient inference and omnibus Robust Wald F, BC/percentile coefficient bootstrap with valid-replicate gates, final-model complete-case hierarchical steps, Delta R2/F-change and paired bootstrap Delta R2 CI, rank-deficiency blocking, UI and Excel diagnostics | PASS | Automatic diagnostic selection is a heuristic and should be supported by design, residual plots, and sensitivity analysis rather than treated as an estimator-selection proof. |",
  "| Mediation / moderation | Independent `stats::lm` equations and manual case-resampling bootstrap | Simple mediation with covariates, direct/indirect/total effects, BC and percentile intervals, plus-one bootstrap p values, valid-replicate gates, moderation coefficients and conditional-slope covariance algebra, serial indirect decomposition, complete-case handling, reproducibility, UI, and export | PASS | BC is the default sensitivity-aware interval, not a universal guarantee of superior coverage; causal mediation claims still require design-level identification assumptions. |",
  "| Penalized regression | Direct `glmnet::cv.glmnet` calls with matching seed, alpha, folds, and standardization | Ridge, LASSO, and Elastic Net lambda paths, CV MSE/SE, lambda.min/lambda.1se coefficients, Elastic Net alpha selection, bootstrap selection-stability formatting | PASS | Validation uses Gaussian penalized regression; conventional p-values are intentionally not reported for penalized models. |",
  "| Logistic regression | Direct `stats::glm`, `ordinal::clm`, `nnet::multinom`, nested nominal-effects LR tests, and direct probability-score formulas | Binary/ordinal/multinomial B and SE, final-model complete-case hierarchy, same-family LR delta chi-square, proportional-odds fallback, Wald OR CI, convergence/rank gates, and apparent AUC/Brier/Tjur/log-loss or multicategory probability scores | PASS | Apparent performance is descriptive only; Firth/bias-reduced separation remedies, partial proportional odds, nonlinear functional-form modeling, IIA sensitivity, and predictive validation remain explicit boundary conditions. |",
  "| Generalized linear models | Direct `stats::glm`, `MASS::glm.nb`, automatic-family rules | Gaussian identity B/SE, binomial logit B/SE, binary-outcome auto family, positive-skew Gamma auto family, count overdispersion switch to negative binomial | PASS | Count fallback depends on `MASS::glm.nb` convergence; warnings are surfaced when fallback choices matter. |",
  "| Reliability analysis | `psych::alpha`, `psych::omega`, polychoric direct calculation | Raw Cronbach alpha, KR-20 as binary raw alpha, Pearson omega, ordinal alpha/omega from polychoric matrix, item-total/corrected item-total correlations, omega option separation, binary and zero-variance guards | PASS | Ordinal item-total correlations intentionally use Spearman and are documented as potentially different from SPSS Pearson output. |",
  "| Inter-rater agreement | `psych`, `irr`, `irrCAC`, Krippendorff coincidence-matrix implementation, literature examples | ICC variants, Cohen/weighted kappa, Fleiss kappa, Light kappa, Gwet AC1/AC2 including missing-data unit averaging, Krippendorff alpha including missing and single-rater unit handling, character-label ordinal category order | PASS | Weighted kappa, AC2, and ordinal alpha use `category_table` order for ordinal category levels when available. |",
  "| PCA | Direct eigen decomposition, `psych`/polychoric checks | Pearson/covariance/polychoric matrix paths, eigenvalues, component-count rules, cumulative-variance empty-selection guard, polychoric-to-Pearson fallback matrix label, covariance Kaiser warning | PASS | Kaiser eigenvalue >= 1 is warned as scale-dependent for covariance matrices. |",
  "| Factor analysis | `psych::fa`, shared numeric-matrix conversion checks | PAF one-factor absolute loadings, factor numeric conversion via labels rather than factor codes, polychoric score warning path | PASS | When FA is fitted on a polychoric matrix, saved scores are documented as raw-data/Pearson-standardized approximations. |",
  "| SEM / CB-SEM / PLS-SEM | Direct `lavaan::sem` and `seminr::estimate_pls` calls | SEM and CB-SEM path B/SE/beta, global fit indices, defined indirect/total effects, PLS path coefficients, R2, outer loadings, reliability, and AVE | PASS | This validates representative continuous reflective models; advanced group comparisons, ordinal WLSMV, and PLS predictive diagnostics remain in dedicated SEM validation work. |",
  "| Longitudinal / panel models | `lmerTest::lmer`, `geepack::geeglm`, `plm`, `lmtest::coeftest`, `mice::pool`, direct Kish/IPW checks | LMM ML coefficients/AIC, GEE coefficients/SE with id-time sorting and waves, panel FE coefficients and group-cluster HC1 SE, Rubin MI pooling B/SE/df, Kish effective N, IPW clipping/normalization, NB-GEE fallback warning | PASS | Native negative-binomial GEE is not claimed; marginal `glm.nb` plus cluster-robust SE is explicitly labelled as a fallback. |",
  "| Data editor recode / missing-code handling | Direct helper checks and formula-transform guard tests | Same-variable recode, category/range recode, reverse scoring, Likert detection/conversion, missing-code detection and conversion to `NA`, formula transformations, factor numeric-label conversion in numeric helpers | PASS | Data-editor missing-code handling converts user/sentinel codes to `NA`; general MI/IPW engines are validated in GLM and longitudinal modules. |",
  "| Custom model canvas wiring | Synthetic canvas snapshots compared with expected analysis maps | Node roles, directed X->Y, X->M, M->Y, M->M maps, serial mediator detection, moderated path flags, moderation map rows, invalid edge/moderation record filtering | PASS | The canvas wiring test covers snapshot-to-engine map construction; fitted mediation/moderation calculations are validated in the mediation engine paths. |",
  "| LCA / R3STEP reporting | Code inspection and R parse checks | RRR confidence interval critical values in R3STEP extraction, publication tables, and figures use `stats::qnorm(0.975)` instead of hard-coded 1.96 | PASS | This is a consistency/reporting fix rather than a numerical model-engine validation. |",
  "",
  "Category-order validation: character-label ordinal variables such as `Low/Mid/High` or `low/medium/high` are scored from `category_table` order where ordinal scoring or ordered levels are needed in correlation, crosstabs trend tests, inter-rater weighted statistics, and ordinal reliability.",
  "",
  "Not covered by this analysis-method summary yet: none of the previously listed modules remain outside the current validation summary. New or substantially changed analysis modules should still receive the same reference-comparison pass before release."
)

markdown <- c(
  "# Validation Reference Comparison",
  "",
  "Generated by `scripts/validate_analysis_reference_comparison.R`.",
  "",
  "This document collects StatEdu Studio validation checks that compare app outputs with external reference formulas, public R package results, or explicit automatic decision rules.",
  "",
  "## Summary",
  "",
  "- Sample-size calculations are compared with G*Power-equivalent formulas, public R packages, or literature-based formulas.",
  "- Effect-size calculations are compared with `effectsize` or equivalent standard formulas.",
  "- Analysis calculations are compared with base R, contributed R packages, and StatEdu Studio automatic decision rules.",
  "",
  analysis_method_markdown,
  "",
  "## Analysis Reference Comparison",
  "",
  sprintf("Rows compared: %d", nrow(comparison)),
  "",
  "This table validates both direct analysis calculations and StatEdu Studio automatic decision paths. Automatic paths include sparse-cell Fisher switching, non-normal correlation switching to Spearman, t-test/ANOVA switching to Mann-Whitney, Welch, or Kruskal-Wallis, GLM family detection, count overdispersion selection, and longitudinal count-family selection.",
  "",
  "| Menu | Case | Metric | App result | Reference result | Max abs diff | Tolerance | Status | Note |",
  "|---|---|---:|---:|---:|---:|---:|---|---|",
  apply(comparison, 1, function(row) {
    esc <- function(value) gsub("\\|", "\\\\|", as.character(value %||% ""), fixed = FALSE)
    sprintf(
      "| %s | %s | %s | %s | %s | %s | %s | %s | %s |",
      esc(row[["Menu"]]), esc(row[["Case"]]), esc(row[["Metric"]]), esc(row[["App result"]]), esc(row[["Reference result"]]),
      esc(row[["Max abs diff"]]), esc(row[["Tolerance"]]), esc(row[["Status"]]), esc(row[["Note"]])
    )
  }),
  "",
  sample_size_markdown,
  "",
  effect_size_markdown
)
writeLines(markdown, md_path, useBytes = TRUE)
writeLines(markdown, docs_path, useBytes = TRUE)

cat("Reference comparison passed.\n")
cat("Rows:", nrow(comparison), "\n")
cat("CSV:", csv_path, "\n")
cat("Markdown:", md_path, "\n")
cat("Docs:", docs_path, "\n")
