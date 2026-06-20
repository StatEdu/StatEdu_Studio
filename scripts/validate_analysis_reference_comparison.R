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
add_row("t-test / ANOVA", "Auto normality violation: two groups", table_cell_number(mw$results[[1]]$table, c("p")), round(mw_ref$p.value, 3), "p rounded", abs(table_cell_number(mw$results[[1]]$table, c("p")) - round(mw_ref$p.value, 3)), 0.001)

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
rel_ref <- suppressWarnings(psych::alpha(stats::cor(rel_data), n.obs = nrow(rel_data), check.keys = FALSE, warnings = FALSE))
add_row("Reliability", "Cronbach alpha", rel_app$reliability[["pearson_alpha"]], rel_ref$total$std.alpha[[1]], "alpha", abs(rel_app$reliability[["pearson_alpha"]] - rel_ref$total$std.alpha[[1]]), 1e-10)

# PCA / factor analysis
pca_app <- prepare_pca_results(rel_data, names(rel_data), variable_info = rel_info, options = list(criterion = "eigen", rotation = "none"))
pca_ref <- eigen(stats::cor(rel_data), symmetric = TRUE, only.values = TRUE)$values
add_row("PCA", "Correlation eigenvalues", 0, 0, "max |eigenvalue diff|", max_abs_diff(pca_app$eigenvalues, pca_ref), 1e-10)

fa_app <- prepare_factor_analysis_results(rel_data, names(rel_data), variable_info = rel_info, options = list(criterion = "fixed", n_factors = 1, method = "pa", rotation = "none"))
fa_ref <- suppressWarnings(suppressMessages(psych::fa(r = stats::cor(rel_data), nfactors = 1, n.obs = nrow(rel_data), rotate = "none", fm = "pa", warnings = FALSE)))
fa_diff <- max_abs_diff(abs(as.matrix(unclass(fa_app$loadings))[, 1]), abs(as.matrix(unclass(fa_ref$loadings))[, 1]))
add_row("Factor Analysis", "PAF one-factor loadings", 0, 0, "max |abs loading diff|", fa_diff, 1e-10)

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

markdown <- c(
  "# Analysis Reference Comparison",
  "",
  sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
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
  })
)
writeLines(markdown, md_path, useBytes = TRUE)
writeLines(markdown, docs_path, useBytes = TRUE)

cat("Reference comparison passed.\n")
cat("Rows:", nrow(comparison), "\n")
cat("CSV:", csv_path, "\n")
cat("Markdown:", md_path, "\n")
cat("Docs:", docs_path, "\n")
