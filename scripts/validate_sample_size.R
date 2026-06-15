all_args <- commandArgs(FALSE)
file_arg <- all_args[grep("^--file=", all_args)]
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "scripts/validate_sample_size.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(repo_root, "R", "app_bootstrap.R"))) {
  repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

source(file.path(repo_root, "R", "app_bootstrap.R"))
source_app_modules(dir = file.path(repo_root, "R"))
suppressPackageStartupMessages(library(shiny))

expect_true <- function(value, label) {
  if (!isTRUE(value)) stop(label, call. = FALSE)
}

near <- function(a, b, tolerance = 1e-8) {
  isTRUE(is.finite(a) && is.finite(b) && abs(a - b) <= tolerance)
}

message("Checking exact t-test calculations against stats::power.t.test...")
two_sample <- sample_size_ttest("sample_size", "two_sample", 0.5, 0.05, power = 0.8)
two_sample_ref <- stats::power.t.test(delta = 0.5, sd = 1, sig.level = 0.05, power = 0.8, type = "two.sample", alternative = "two.sided")
expect_true(two_sample$group1 == ceiling(two_sample_ref$n), "Expected independent t-test n/group to match stats::power.t.test")

one_sample <- sample_size_ttest("sample_size", "one_sample", 0.5, 0.05, power = 0.8)
one_sample_ref <- stats::power.t.test(delta = 0.5, sd = 1, sig.level = 0.05, power = 0.8, type = "one.sample", alternative = "two.sided")
expect_true(one_sample$total == ceiling(one_sample_ref$n), "Expected one-sample t-test n to match stats::power.t.test")

paired <- sample_size_ttest("sample_size", "paired", 0.5, 0.05, power = 0.8)
paired_ref <- stats::power.t.test(delta = 0.5, sd = 1, sig.level = 0.05, power = 0.8, type = "paired", alternative = "two.sided")
expect_true(paired$total == ceiling(paired_ref$n), "Expected paired t-test n to match stats::power.t.test")

two_sample_power <- sample_size_ttest("power", "two_sample", 0.5, 0.05, n = 64)
two_sample_power_ref <- stats::power.t.test(n = 64, delta = 0.5, sd = 1, sig.level = 0.05, type = "two.sample", alternative = "two.sided")
expect_true(near(two_sample_power$power, two_sample_power_ref$power), "Expected independent t-test power to match stats::power.t.test")

message("Checking built-in and closed-form reference calculations...")
proportion <- sample_size_proportion("sample_size", "two_proportion", p1 = 0.5, p2 = 0.65, alpha = 0.05, power = 0.8, ratio = 1)
proportion_ref <- stats::power.prop.test(p1 = 0.5, p2 = 0.65, sig.level = 0.05, power = 0.8, alternative = "two.sided")
expect_true(abs(proportion$group1 - ceiling(proportion_ref$n)) <= 2, "Expected two-proportion n/group to agree with stats::power.prop.test within approximation tolerance")

correlation <- sample_size_correlation("sample_size", r = 0.3, alpha = 0.05, power = 0.8)
correlation_power <- sample_size_correlation("power", r = 0.3, alpha = 0.05, n = correlation$total)$power
correlation_power_previous <- sample_size_correlation("power", r = 0.3, alpha = 0.05, n = correlation$total - 1)$power
expect_true(correlation_power >= 0.8 && correlation_power_previous < 0.8, "Expected correlation sample size to be minimal for target power")

anova <- sample_size_anova("sample_size", design = "one_way", groups = 3, effect_size = 0.25, alpha = 0.05, power = 0.8)
anova_power <- sample_size_anova("power", design = "one_way", groups = 3, effect_size = 0.25, alpha = 0.05, n = anova$total)$power
anova_previous_balanced <- sample_size_anova("power", design = "one_way", groups = 3, effect_size = 0.25, alpha = 0.05, n = anova$total - 3)$power
expect_true(anova_power >= 0.8 && anova_previous_balanced < 0.8, "Expected one-way ANOVA sample size to be minimal among balanced allocations")

unadjusted <- sample_size_anova("sample_size", design = "one_way", groups = 2, effect_size = 0.25, alpha = 0.05, power = 0.8)
adjusted <- sample_size_ancova("sample_size", design = "ancova", groups = 2, effect_size = 0.25, covariates = 1, covariate_r2 = 0.3, alpha = 0.05, power = 0.8)
expect_true(adjusted$total < unadjusted$total, "Expected ANCOVA with positive covariate R-squared to require fewer participants than unadjusted ANOVA")

regression <- sample_size_regression("sample_size", design = "multiple", effect_size = 0.15, alpha = 0.05, power = 0.8, predictors = 3)
regression_power <- sample_size_regression("power", design = "multiple", effect_size = 0.15, alpha = 0.05, n = regression$total, predictors = 3)$power
regression_power_previous <- sample_size_regression("power", design = "multiple", effect_size = 0.15, alpha = 0.05, n = regression$total - 1, predictors = 3)$power
expect_true(regression_power >= 0.8 && regression_power_previous < 0.8, "Expected multiple regression sample size to be minimal for target power")

chisquare <- sample_size_chisquare("sample_size", df = 1, effect_size = 0.3, alpha = 0.05, power = 0.8)
chisquare_power <- sample_size_chisquare("power", df = 1, effect_size = 0.3, alpha = 0.05, n = chisquare$total)$power
chisquare_power_previous <- sample_size_chisquare("power", df = 1, effect_size = 0.3, alpha = 0.05, n = chisquare$total - 1)$power
expect_true(chisquare_power >= 0.8 && chisquare_power_previous < 0.8, "Expected chi-square sample size to be minimal for target power")

survival <- sample_size_survival("sample_size", hazard_ratio = 0.7, event_probability = 0.5, alpha = 0.05, power = 0.8, ratio = 1)
survival_power <- sample_size_survival("power", hazard_ratio = 0.7, event_probability = 0.5, alpha = 0.05, n = survival$total, ratio = 1)$power
expect_true(survival$total >= survival$required_events && survival_power >= 0.8, "Expected survival total N to cover required events and target power")
expect_true(near(survival$schoenfeld_signal, abs(log(0.7)) * sqrt(0.5 * 0.25)), "Expected survival sample size result to report Schoenfeld planning signal")

dropout <- sample_size_ttest("sample_size", "two_sample", 0.5, 0.05, power = 0.8, dropout = 0.1)
expect_true(dropout$adjusted_group1 == ceiling(dropout$group1 / 0.9), "Expected dropout adjustment to round each group upward")
expect_true(dropout$adjusted_total == 2 * dropout$adjusted_group1, "Expected adjusted total to equal the sum of adjusted groups")

message("Checking effect-size calculator...")
independent_effect <- sample_size_effect_size("independent_means", mean1 = 105, mean2 = 100, sd1 = 10, sd2 = 10, n1 = 50, n2 = 50)
expect_true(near(independent_effect$effect_size_d, 0.5), "Expected independent-means Cohen's d to equal 0.5")
expect_true(!is.null(independent_effect$hedges_g) && independent_effect$hedges_g < independent_effect$effect_size_d, "Expected Hedges' g to apply a small-sample correction")

one_sample_effect <- sample_size_effect_size("one_sample_mean", mean1 = 105, mean2 = 100, sd1 = 10, null_mean = 100)
expect_true(near(one_sample_effect$effect_size_d, 0.5), "Expected one-sample Cohen's d to equal 0.5")

paired_effect <- sample_size_effect_size("paired_means", mean_difference = 5, sd_difference = 10)
expect_true(near(paired_effect$effect_size_d, 0.5), "Expected paired Cohen's dz to equal 0.5")

independent_t_effect <- sample_size_effect_size("independent_t_n", t_value = 2.5, n1 = 50, n2 = 50)
expect_true(near(independent_t_effect$effect_size_d, 2.5 * sqrt(1 / 50 + 1 / 50)), "Expected independent t-to-d conversion to use group sizes")

independent_equal_t_effect <- sample_size_effect_size("independent_t_df_equal", t_value = 2.5, df = 98)
expect_true(near(independent_equal_t_effect$effect_size_d, 2 * 2.5 / sqrt(100)), "Expected equal-n t-to-d conversion to use df")

one_sample_t_effect <- sample_size_effect_size("one_sample_t", t_value = 3.5, n = 49)
expect_true(near(one_sample_t_effect$effect_size_d, 0.5), "Expected one-sample t-to-d conversion to equal t / sqrt(n)")

paired_t_effect <- sample_size_effect_size("paired_t", t_value = 3.5, n = 49)
expect_true(near(paired_t_effect$effect_size_d, 0.5), "Expected paired t-to-dz conversion to equal t / sqrt(n pairs)")

r_to_d_effect <- sample_size_effect_size("independent_r", r = 0.25)
expect_true(near(r_to_d_effect$effect_size_d, 2 * 0.25 / sqrt(1 - 0.25^2)), "Expected r-to-d conversion")

proportion_effect <- sample_size_effect_size_proportion("cohens_h", p1 = 0.5, p2 = 0.65)
expect_true(
  near(proportion_effect$cohens_h, 2 * asin(sqrt(0.5)) - 2 * asin(sqrt(0.65))),
  "Expected Cohen's h to use the arcsine-square-root transformation"
)
proportion_rr <- sample_size_effect_size_proportion("risk_ratio", p1 = 0.5, p2 = 0.25)
expect_true(near(proportion_rr$primary_effect_size, 2), "Expected risk ratio to equal p1 / p2")
proportion_or <- sample_size_effect_size_proportion("odds_ratio", p1 = 0.5, p2 = 0.25)
expect_true(near(proportion_or$primary_effect_size, 3), "Expected odds ratio to equal odds1 / odds2")

chisquare_w <- sample_size_effect_size_chisquare("cohens_w", chi_square = 10, n = 100, rows = 2, columns = 2)
expect_true(near(chisquare_w$cohens_w, sqrt(10 / 100)), "Expected Cohen's w to equal sqrt(chi-square / N)")
chisquare_v <- sample_size_effect_size_chisquare("cramers_v", chi_square = 12, n = 120, rows = 2, columns = 3)
expect_true(near(chisquare_v$cramer_v, sqrt(12 / (120 * 1))), "Expected Cramer's V to use min(r - 1, c - 1)")
chisquare_probs <- sample_size_effect_size_chisquare("cohens_w_from_probs", observed = "0.20, 0.50, 0.30", expected = "0.33, 0.33, 0.34")
expect_true(is.finite(chisquare_probs$cohens_w) && chisquare_probs$cohens_w > 0, "Expected Cohen's w from category proportions to be finite and positive")

correlation_from_t <- sample_size_effect_size_correlation("r_from_t", t_value = 2.5, df = 98)
expect_true(near(correlation_from_t$correlation_r, sqrt(2.5^2 / (2.5^2 + 98))), "Expected r from t to use t-to-r conversion")
correlation_from_f <- sample_size_effect_size_correlation("r_from_f", f_value = 6.25, df = 98)
expect_true(near(correlation_from_f$correlation_r, abs(correlation_from_t$correlation_r)), "Expected r from F to match absolute r from t when F = t^2")
correlation_from_r2 <- sample_size_effect_size_correlation("r_from_r2", r_squared = 0.09)
expect_true(near(correlation_from_r2$correlation_r, 0.3), "Expected r from R-squared to equal sqrt(R-squared)")
correlation_q <- sample_size_effect_size_correlation("cohens_q", r1 = 0.5, r2 = 0.3)
expect_true(near(correlation_q$cohens_q, atanh(0.5) - atanh(0.3)), "Expected Cohen's q to equal Fisher-z difference")
correlation_point_biserial <- sample_size_effect_size_correlation("point_biserial", r = 0.25)
expect_true(near(correlation_point_biserial$effect_size_d, 2 * 0.25 / sqrt(1 - 0.25^2)), "Expected point-biserial r to convert to Cohen's d")

anova_f <- sample_size_effect_size_anova("f_from_eta2", eta_squared = 0.06)
expect_true(near(anova_f$cohen_f, sqrt(0.06 / 0.94)), "Expected Cohen's f to convert from eta squared")
anova_partial_eta <- sample_size_effect_size_anova("partial_eta_from_f", f_value = 4.5, df_effect = 2, df_error = 87)
expect_true(near(anova_partial_eta$partial_eta_squared, 4.5 * 2 / (4.5 * 2 + 87)), "Expected partial eta squared to convert from F")
anova_partial_eta_n <- sample_size_effect_size_anova("partial_eta_from_f", f_value = 4.5, groups = 3, total_n = 90)
expect_true(near(anova_partial_eta_n$partial_eta_squared, anova_partial_eta$partial_eta_squared), "Expected ANOVA F conversion to derive df from groups and total N")
anova_omega <- sample_size_effect_size_anova("omega_from_f", f_value = 4.5, df_effect = 2, df_error = 87)
expect_true(is.finite(anova_omega$omega_squared) && anova_omega$omega_squared >= 0, "Expected partial omega squared from F to be finite and nonnegative")

ancova_adjusted <- sample_size_effect_size_ancova("ancova_adjusted_f", effect_size_f = 0.25, covariate_r2 = 0.30)
expect_true(near(ancova_adjusted$cohen_f, 0.25 / sqrt(0.70)), "Expected ANCOVA adjusted f to apply covariate R-squared correction")
ancova_partial_eta <- sample_size_effect_size_ancova("ancova_partial_eta_from_f", f_value = 4.5, df_effect = 2, df_error = 87)
expect_true(near(ancova_partial_eta$partial_eta_squared, 4.5 * 2 / (4.5 * 2 + 87)), "Expected ANCOVA partial eta squared to convert from F")
ancova_partial_eta_n <- sample_size_effect_size_ancova("ancova_partial_eta_from_f", f_value = 4.5, groups = 3, total_n = 90)
expect_true(near(ancova_partial_eta_n$partial_eta_squared, ancova_partial_eta$partial_eta_squared), "Expected ANCOVA F conversion to derive df from groups and total N")
manova_pillai <- sample_size_effect_size_ancova("manova_pillai", pillai_trace = 0.10)
expect_true(near(manova_pillai$f_squared, 0.10 / 0.90), "Expected MANOVA Pillai trace to convert to f-squared")
manova_wilks <- sample_size_effect_size_ancova("manova_wilks", wilks_lambda = 0.90, dependent_variables = 2, groups = 3)
expect_true(near(manova_wilks$f_squared, 0.90^(-1 / 2) - 1), "Expected MANOVA Wilks' lambda to convert to f-squared")

nonparam_u <- sample_size_effect_size_nonparametric("rank_biserial_from_u", u = 700, n1 = 50, n2 = 50)
expect_true(near(nonparam_u$rank_biserial, 2 * 700 / (50 * 50) - 1), "Expected rank-biserial r from U")
nonparam_paired <- sample_size_effect_size_nonparametric("rank_biserial_paired", w_positive = 350, w_negative = 150)
expect_true(near(nonparam_paired$rank_biserial, 0.4), "Expected paired rank-biserial r")
nonparam_kw <- sample_size_effect_size_nonparametric("kruskal_epsilon", h = 10, n = 90, groups = 3)
expect_true(near(nonparam_kw$epsilon_squared, (10 - 3 + 1) / (90 - 3)), "Expected Kruskal-Wallis epsilon squared")
nonparam_friedman <- sample_size_effect_size_nonparametric("friedman_w", chi_square = 12, n = 30, measurements = 3)
expect_true(near(nonparam_friedman$kendall_w, 12 / (30 * (3 - 1))), "Expected Friedman Kendall's W")

mcnemar_or <- sample_size_effect_size_mcnemar("matched_or_probs", p01 = 0.20, p10 = 0.10)
expect_true(near(mcnemar_or$odds_ratio, 2), "Expected McNemar matched-pair odds ratio to equal p01 / p10")
mcnemar_table <- sample_size_effect_size_mcnemar("matched_or_counts", b = 20, c = 10)
expect_true(near(mcnemar_table$log_odds_ratio, log(2)), "Expected McNemar table log odds ratio to equal log(b / c)")
mcnemar_g <- sample_size_effect_size_mcnemar("cohen_g", p01 = 0.20, p10 = 0.10)
expect_true(near(mcnemar_g$cohen_g, 0.20 / 0.30 - 0.5), "Expected McNemar Cohen's g to use discordant-pair probability minus 0.5")

regression_f2 <- sample_size_effect_size_regression("f2_from_r2", r_squared = 0.13)
expect_true(near(regression_f2$f_squared, 0.13 / 0.87), "Expected regression f-squared to equal R-squared / (1 - R-squared)")
regression_hier <- sample_size_effect_size_regression("hierarchical_f2", full_r_squared = 0.25, reduced_r_squared = 0.10)
expect_true(near(regression_hier$f_squared, 0.15 / 0.75), "Expected hierarchical regression f-squared to use incremental R-squared")
regression_logistic <- sample_size_effect_size_regression("logistic_or", odds_ratio = 1.8)
expect_true(near(regression_logistic$effect_size_d, log(1.8) * sqrt(3) / pi), "Expected logistic OR conversion to approximate Cohen's d")
regression_mediation_sample <- sample_size_regression("sample_size", design = "mediation", a_path = 0.3, b_path = 0.3, covariates = 2, alpha = 0.05, power = 0.8, mediation_method = "sobel")
expect_true(near(regression_mediation_sample$indirect_effect, 0.09), "Expected mediation sample-size result to report indirect effect a*b")
expect_true(regression_mediation_sample$covariates == 2, "Expected mediation sample-size result to report covariate count")
regression_mediation_fritz <- sample_size_regression(
  "sample_size",
  design = "mediation",
  alpha = 0.05,
  power = 0.8,
  mediation_method = "fritz_mackinnon",
  a_effect = "small",
  b_effect = "small",
  fritz_mackinnon_test = "bias_corrected_bootstrap"
)
expect_true(regression_mediation_fritz$total == 462, "Expected Fritz & MacKinnon S-S bias-corrected bootstrap sample size to equal Table 3")
expect_true(near(regression_mediation_fritz$a_path, 0.14), "Expected Fritz & MacKinnon small a path to equal .14")
expect_true(near(regression_mediation_fritz$b_path, 0.14), "Expected Fritz & MacKinnon small b path to equal .14")
expect_true(regression_mediation_fritz$mediation_method == "fritz_mackinnon", "Expected mediation method to be reported for method-specific references")
regression_mediation_ui <- htmltools::renderTags(sample_size_inputs_ui("regression", list(
  sample_size_regression_target = "sample_size",
  sample_size_regression_design = "mediation"
)))$html
expect_true(grepl("Path a beta: predictor -&gt; mediator", regression_mediation_ui, fixed = TRUE), "Expected mediation UI to explain path a beta")
expect_true(grepl("Path b beta: mediator -&gt; outcome", regression_mediation_ui, fixed = TRUE), "Expected mediation UI to explain path b beta")
expect_true(grepl("Number of covariates", regression_mediation_ui, fixed = TRUE), "Expected mediation UI to label covariate count")

gee_continuous <- sample_size_effect_size_gee("continuous_means", mean1 = 0.5, mean2 = 0, sd = 1)
expect_true(near(gee_continuous$effect_size_d, 0.5), "Expected GEE continuous d to equal standardized mean difference")
expect_true(is.null(gee_continuous$design_effect), "Expected GEE effect-size calculator to omit planning design effect")
gee_change <- sample_size_effect_size_gee("continuous_change_means", pre_mean1 = 0, post_mean1 = 0.5, pre_mean2 = 0.1, post_mean2 = 0.2, sd = 1)
expect_true(near(gee_change$effect_size_d, 0.4), "Expected GEE change-score contrast to convert to Cohen's d")
gee_parameter <- sample_size_effect_size_gee("continuous_parameter_b", coefficient = 0.5, sd = 2)
expect_true(near(gee_parameter$effect_size_d, 0.25), "Expected GEE parameter estimate B to convert to Cohen's d")
gee_pooled_sd <- sample_size_pooled_sd(n1 = 50, sd1 = 1, n2 = 50, sd2 = 2)
expect_true(near(gee_pooled_sd, sqrt(((50 - 1) * 1^2 + (50 - 1) * 2^2) / 98)), "Expected pooled SD from group n and SD")
gee_binary <- sample_size_effect_size_gee("binary_props", p1 = 0.5, p2 = 0.65)
expect_true(
  near(gee_binary$cohens_h, 2 * asin(sqrt(0.5)) - 2 * asin(sqrt(0.65))),
  "Expected GEE binary Cohen's h from proportions"
)

lmm_simple <- sample_size_effect_size_lmm("simple_fixed", effect_size = 0.3, time_points = 3, icc = 0.3)
expect_true(near(lmm_simple$design_effect, 1 + (3 - 1) * 0.3), "Expected LMM simple design effect from ICC")
lmm_glimmpse <- sample_size_effect_size_lmm("glimmpse_vectors", lmm_design = "two_group_repeated", group1_means = "0, 0.2, 0.4", group2_means = "0, 0.1, 0.8", residual_sd = 1, rho = 0.5)
expect_true(near(lmm_glimmpse$effect_size_d, 0.4), "Expected LMM GLIMMPSE-style group-by-time effect from change contrast")
lmm_unstructured_corr <- sample_size_lmm_correlation_matrix(3, NA, "unstructured", "0.5, 0.3, 0.5")
expect_true(near(lmm_unstructured_corr[1, 2], 0.5) && near(lmm_unstructured_corr[1, 3], 0.3) && near(lmm_unstructured_corr[2, 3], 0.5), "Expected LMM unstructured correlations to fill the upper-triangle pairs")
lmm_unstructured <- sample_size_effect_size_lmm("glimmpse_vectors", lmm_design = "two_group_repeated", group1_means = "0, 0.2, 0.4", group2_means = "0, 0.1, 0.8", residual_sd = 1, structure = "unstructured", correlations = "0.5, 0.3, 0.5")
expect_true(near(lmm_unstructured$design_effect, 1 + 2 * mean(c(0.5, 0.3, 0.5))), "Expected LMM unstructured design effect to use mean pairwise correlation")

survival_hr <- sample_size_effect_size_survival("hazard_ratio", hazard_ratio = 0.7)
expect_true(near(survival_hr$log_hazard_ratio, log(0.7)), "Expected survival log hazard ratio to equal log(HR)")

equivalence_mean <- sample_size_effect_size_equivalence("mean", "equivalence", true_difference = 0.1, margin = 0.5, sd = 1)
expect_true(near(equivalence_mean$standardized_distance, 0.4), "Expected equivalence standardized distance from margin")
equivalence_prop <- sample_size_effect_size_equivalence("proportion", "noninferiority", margin = 0.1, p1 = 0.8, p2 = 0.78)
expect_true(near(equivalence_prop$observed_effect, 0.02), "Expected equivalence proportion effect to equal p1 - p2")

diagnostic_auc <- sample_size_effect_size_diagnostic("auc", auc = 0.75, null_auc = 0.50)
expect_true(near(diagnostic_auc$primary_effect_size, 0.75), "Expected diagnostic effect-size primary value to be AUC")
expect_true(near(diagnostic_auc$auc_cohen_d, sqrt(2) * stats::qnorm(0.75)), "Expected AUC to Cohen's d conversion")

rates_poisson <- sample_size_effect_size_rates("poisson_irr", ratio = 1.5)
expect_true(near(rates_poisson$incidence_rate_ratio, 1.5) && near(rates_poisson$log_incidence_rate_ratio, log(1.5)), "Expected Poisson IRR to convert to log IRR")
rates_nb <- sample_size_effect_size_rates("negative_binomial_irr", input_scale = "log_ratio", log_ratio = log(1.5))
expect_true(near(rates_nb$incidence_rate_ratio, 1.5), "Expected negative binomial log IRR to convert to IRR")
rates_gamma <- sample_size_effect_size_rates("gamma_mean_ratio", ratio = 1.25)
expect_true(near(rates_gamma$log_mean_ratio, log(1.25)), "Expected gamma mean ratio to convert to log mean ratio")

cluster_cont <- sample_size_effect_size_cluster("parallel_continuous", effect_size = 0.5, cluster_size = 20, icc = 0.05)
expect_true(near(cluster_cont$design_effect, 1 + (20 - 1) * 0.05), "Expected cluster design effect")
cluster_binary <- sample_size_effect_size_cluster("parallel_binary", p1 = 0.5, p2 = 0.65, cluster_size = 20, icc = 0.05)
expect_true(near(cluster_binary$cohens_h, 2 * asin(sqrt(0.5)) - 2 * asin(sqrt(0.65))), "Expected cluster binary Cohen's h")
cluster_sw <- sample_size_effect_size_cluster("stepped_wedge", effect_size = 0.4, cluster_size = 20, icc = 0.05, periods = 5)
expect_true(near(cluster_sw$design_effect, (1 + (20 - 1) * 0.05) * 5 / 4), "Expected stepped-wedge planning design effect")

precision_mean <- sample_size_effect_size_precision("mean", estimate = 1, half_width = 0.1, sd = 1)
expect_true(near(precision_mean$standardized_half_width, 0.1), "Expected mean precision standardized half-width")
precision_prop <- sample_size_effect_size_precision("proportion", half_width = 0.1, proportion = 0.5)
expect_true(near(precision_prop$standardized_half_width, 0.2), "Expected proportion precision standardized half-width")
precision_cor <- sample_size_effect_size_precision("correlation", half_width = 0.1, r = 0.3)
expect_true(is.finite(precision_cor$fisher_z_half_width) && precision_cor$fisher_z_half_width > 0, "Expected correlation Fisher z half-width")

reliability_alpha <- sample_size_effect_size_reliability("alpha", reliability = 0.8, reference = 0.7, items = 5)
expect_true(near(reliability_alpha$reliability_difference, 0.1), "Expected alpha reliability difference")
reliability_icc <- sample_size_effect_size_reliability("icc", reliability = 0.8, reference = 0.7, items = 2)
expect_true(near(reliability_icc$transformed_difference, atanh(0.8) - atanh(0.7)), "Expected ICC Fisher z difference")
reliability_ba <- sample_size_effect_size_reliability("bland_altman", sd_difference = 1)
expect_true(near(reliability_ba$loa_total_width, 3.92), "Expected Bland-Altman LoA total width")
reliability_kappa <- sample_size_effect_size_reliability("kappa", reliability = 0.8, categories = 2)
expect_true(near(reliability_kappa$primary_effect_size, 0.8), "Expected Cohen's kappa effect to report kappa directly")
reliability_alpha_sample <- sample_size_reliability("sample_size", design = "alpha", reliability = 0.8, confidence_level = 0.95, half_width = 0.10, items = 20)
expect_true(reliability_alpha_sample$total >= 21, "Expected Cronbach alpha sample size to be at least items + 1")
expect_true(reliability_alpha_sample$minimum_subjects == 21, "Expected Cronbach alpha minimum-subjects rule to equal items + 1")

sem_rmsea <- sample_size_effect_size_sem("rmsea", df = 50, null_rmsea = 0.05, alternative_rmsea = 0.08)
expect_true(near(sem_rmsea$ncp_difference_per_n, 50 * (0.08^2 - 0.05^2)), "Expected SEM RMSEA NCP difference")
sem_parameter <- sample_size_effect_size_sem("parameter", parameter_type = "path", parameter = 0.3)
expect_true(near(sem_parameter$fisher_z, atanh(0.3)), "Expected SEM parameter Fisher z")
sem_complexity <- sample_size_effect_size_sem("complexity", latent_variables = 3, measured_variables = 12, structural_paths = 3, free_parameters = 30, expected_loading = 0.5, expected_path = 0.3)
expect_true(near(sem_complexity$structure_burden_per_parameter, (10 * 12 + 20 * 3 + 15 * 3) / 30), "Expected SEM burden per free parameter")
sem_estimated_df <- sample_size_sem_estimated_df(latent_variables = 3, measured_variables = 12, structural_paths = 3)
expect_true(sem_estimated_df$df == 51, "Expected SEM df estimate from latent, measured, and path counts")
sem_direct_ui <- htmltools::renderTags(sample_size_inputs_ui("sem", list(
  sample_size_sem_target = "sample_size",
  sample_size_sem_test = "close_fit",
  sample_size_sem_df_source = "direct"
)))$html
expect_true(grepl("Model degrees of freedom", sem_direct_ui, fixed = TRUE), "Expected SEM direct df input to show model df field")
expect_true(!grepl("Latent variables", sem_direct_ui, fixed = TRUE), "Expected SEM direct df input to hide structure-count fields")
sem_structure_ui <- htmltools::renderTags(sample_size_inputs_ui("sem", list(
  sample_size_sem_target = "sample_size",
  sample_size_sem_test = "close_fit",
  sample_size_sem_df_source = "structure"
)))$html
expect_true(grepl("Latent variables", sem_structure_ui, fixed = TRUE), "Expected SEM structure df input to show latent variable field")
expect_true(!grepl("Model degrees of freedom", sem_structure_ui, fixed = TRUE), "Expected SEM structure df input to hide direct df field")
sem_not_close_ui <- htmltools::renderTags(sample_size_inputs_ui("sem", list(
  sample_size_sem_target = "sample_size",
  sample_size_sem_test = "not_close_fit",
  sample_size_sem_df_source = "structure"
)))$html
expect_true(grepl('id="sample_size_sem_null_rmsea"', sem_not_close_ui, fixed = TRUE) && grepl('value="0.08"', sem_not_close_ui, fixed = TRUE), "Expected SEM not-close-fit default null RMSEA to be 0.08")
expect_true(grepl('id="sample_size_sem_alternative_rmsea"', sem_not_close_ui, fixed = TRUE) && grepl('value="0.05"', sem_not_close_ui, fixed = TRUE), "Expected SEM not-close-fit default alternative RMSEA to be 0.05")
sem_result_html <- htmltools::renderTags(sample_size_results_ui(sample_size_sem(
  target = "sample_size",
  test = "close_fit",
  df_source = "structure",
  latent_variables = 4,
  measured_variables = 16,
  structural_paths = 6,
  null_rmsea = 0.05,
  alternative_rmsea = 0.08,
  alpha = 0.05,
  power = 0.95
)))$html
expect_true(grepl("n (Participants)", sem_result_html, fixed = TRUE), "Expected final SEM sample size row to be labeled with n")
expect_true(grepl("sample-size-primary-effect", sem_result_html, fixed = TRUE), "Expected final SEM sample size row to be emphasized")
sem_not_close_result <- sample_size_sem(
  target = "sample_size",
  test = "not_close_fit",
  df_source = "structure",
  latent_variables = 3,
  measured_variables = 12,
  structural_paths = 3,
  null_rmsea = 0.08,
  alternative_rmsea = 0.05,
  alpha = 0.05,
  power = 0.95
)
expect_true(sem_not_close_result$total > 0, "Expected SEM not-close-fit default values to calculate a positive sample size")
ttest_default_power_ui <- htmltools::renderTags(sample_size_inputs_ui("ttest", list(sample_size_ttest_target = "sample_size")))$html
expect_true(grepl('value="0.95"', ttest_default_power_ui, fixed = TRUE), "Expected sample-size power default to be 0.95")
reliability_panel_html <- htmltools::renderTags(sample_size_analysis_panel("reliability"))$html
expect_true(!grepl(">Power<", reliability_panel_html, fixed = TRUE), "Expected Reliability / Agreement sample-size panel to hide unsupported Power target")
expect_true(grepl('id="sample_size_reliability_target"', reliability_panel_html, fixed = TRUE), "Expected Reliability / Agreement panel to keep a hidden sample-size target")

message("Checking effect-size UI calculation wrappers...")
effect_input <- function(values) {
  input <- list()
  for (name in names(values)) input[[name]] <- values[[name]]
  input
}

effect_wrapper_cases <- list(
  ttest = function() effect_size_ttest_calculate(effect_input(list(
    effect_size_ttest_design = "independent_t_n",
    effect_size_ttest_t = "2.5",
    effect_size_ttest_n1 = "50",
    effect_size_ttest_n2 = "50"
  ))),
  proportion = function() effect_size_proportion_calculate(effect_input(list(
    effect_size_proportion_design = "cohens_h",
    effect_size_proportion_p1 = "0.50",
    effect_size_proportion_p2 = "0.65"
  ))),
  chisquare = function() effect_size_chisquare_calculate(effect_input(list(
    effect_size_chisquare_design = "cohens_w",
    effect_size_chisquare_statistic = "10",
    effect_size_chisquare_n = "100",
    effect_size_chisquare_rows = "2",
    effect_size_chisquare_columns = "2"
  ))),
  correlation = function() effect_size_correlation_calculate(effect_input(list(
    effect_size_correlation_design = "r_from_t",
    effect_size_correlation_t = "2.5",
    effect_size_correlation_df = "98"
  ))),
  anova = function() effect_size_anova_calculate(effect_input(list(
    effect_size_anova_design = "f_from_eta2",
    effect_size_anova_eta2 = "0.06"
  ))),
  ancova = function() effect_size_ancova_calculate(effect_input(list(
    effect_size_ancova_design = "ancova_adjusted_f",
    effect_size_ancova_f = "0.25",
    effect_size_ancova_covariate_r2 = "0.30"
  ))),
  nonparametric = function() effect_size_nonparametric_calculate(effect_input(list(
    effect_size_nonparametric_design = "rank_biserial_from_u",
    effect_size_nonparametric_u = "700",
    effect_size_nonparametric_n1 = "50",
    effect_size_nonparametric_n2 = "50"
  ))),
  mcnemar = function() effect_size_mcnemar_calculate(effect_input(list(
    effect_size_mcnemar_design = "matched_or_probs",
    effect_size_mcnemar_p01 = "0.20",
    effect_size_mcnemar_p10 = "0.10"
  ))),
  regression = function() effect_size_regression_calculate(effect_input(list(
    effect_size_regression_design = "f2_from_r2",
    effect_size_regression_r2 = "0.13"
  ))),
  gee = function() effect_size_gee_calculate(effect_input(list(
    effect_size_gee_design = "continuous_means",
    effect_size_gee_mean1 = "0.50",
    effect_size_gee_mean2 = "0",
    effect_size_gee_sd = "1",
    effect_size_gee_sd_mode = "direct"
  ))),
  lmm = function() effect_size_lmm_calculate(effect_input(list(
    effect_size_lmm_design = "simple_fixed",
    effect_size_lmm_lmm_design = "two_group_repeated",
    effect_size_lmm_effect = "0.30",
    effect_size_lmm_time_points = "3",
    effect_size_lmm_icc = "0.30"
  ))),
  survival = function() effect_size_survival_calculate(effect_input(list(
    effect_size_survival_design = "hazard_ratio",
    effect_size_survival_hr = "0.70"
  ))),
  equivalence = function() effect_size_equivalence_calculate(effect_input(list(
    effect_size_equivalence_outcome = "mean",
    effect_size_equivalence_objective = "equivalence",
    effect_size_equivalence_difference = "0.10",
    effect_size_equivalence_margin = "0.50",
    effect_size_equivalence_sd = "1"
  ))),
  diagnostic = function() effect_size_diagnostic_calculate(effect_input(list(
    effect_size_diagnostic_design = "auc",
    effect_size_diagnostic_auc = "0.75",
    effect_size_diagnostic_null_auc = "0.50"
  ))),
  rates = function() effect_size_rates_calculate(effect_input(list(
    effect_size_rates_design = "poisson_irr",
    effect_size_rates_input_scale = "ratio",
    effect_size_rates_ratio = "1.50"
  ))),
  cluster = function() effect_size_cluster_calculate(effect_input(list(
    effect_size_cluster_design = "parallel_continuous",
    effect_size_cluster_effect = "0.50",
    effect_size_cluster_size = "20",
    effect_size_cluster_icc = "0.05"
  ))),
  precision = function() effect_size_precision_calculate(effect_input(list(
    effect_size_precision_parameter = "mean",
    effect_size_precision_estimate = "1",
    effect_size_precision_half_width = "0.10",
    effect_size_precision_sd = "1"
  ))),
  reliability = function() effect_size_reliability_calculate(effect_input(list(
    effect_size_reliability_design = "kappa",
    effect_size_reliability_value = "0.80",
    effect_size_reliability_categories = "2"
  ))),
  sem = function() effect_size_sem_calculate(effect_input(list(
    effect_size_sem_design = "parameter",
    effect_size_sem_parameter_type = "path",
    effect_size_sem_parameter = "0.30"
  )))
)

for (name in names(effect_wrapper_cases)) {
  result <- effect_wrapper_cases[[name]]()
  expect_true(is.null(result$error), sprintf("Expected %s effect-size UI wrapper to complete: %s", name, result$error %||% ""))
  expect_true(identical(result$result_type, "effect_size"), sprintf("Expected %s wrapper to return an effect-size result", name))
  expect_true(!is.null(result$primary_effect_size), sprintf("Expected %s wrapper to include a primary effect size", name))
}

message("Checking all sample-size menu calculations with representative inputs...")
base_input <- function(method, target = "sample_size") {
  input <- list()
  input[[paste0("sample_size_", method, "_target")]] <- target
  input[[paste0("sample_size_", method, "_alpha")]] <- "0.05"
  input[[paste0("sample_size_", method, "_alternative")]] <- "two.sided"
  input[[paste0("sample_size_", method, "_ratio")]] <- "1"
  if (identical(target, "sample_size")) {
    input[[paste0("sample_size_", method, "_power")]] <- "0.95"
    input[[paste0("sample_size_", method, "_dropout")]] <- "0"
  } else {
    input[[paste0("sample_size_", method, "_n")]] <- "100"
  }
  input
}

merge_input <- function(input, values) {
  for (name in names(values)) input[[name]] <- values[[name]]
  input
}

cases <- list(
  ttest = merge_input(base_input("ttest"), list(sample_size_ttest_design = "two_sample", sample_size_ttest_effect = "0.50")),
  proportion = merge_input(base_input("proportion"), list(sample_size_proportion_design = "two_proportion", sample_size_proportion_p1 = "0.50", sample_size_proportion_p2 = "0.65")),
  chisquare = merge_input(base_input("chisquare"), list(sample_size_chisquare_effect = "0.30", sample_size_chisquare_df = "1")),
  correlation = merge_input(base_input("correlation"), list(sample_size_correlation_r = "0.30")),
  anova = merge_input(base_input("anova"), list(sample_size_anova_design = "one_way", sample_size_anova_groups = "3", sample_size_anova_effect = "0.25")),
  ancova = merge_input(base_input("ancova"), list(sample_size_ancova_design = "ancova", sample_size_ancova_groups = "2", sample_size_ancova_effect = "0.25", sample_size_ancova_covariates = "1", sample_size_ancova_covariate_r2 = "0.30")),
  nonparametric = merge_input(base_input("nonparametric"), list(sample_size_nonparametric_design = "two_independent", sample_size_nonparametric_effect = "0.50")),
  mcnemar = merge_input(base_input("mcnemar"), list(sample_size_mcnemar_p01 = "0.20", sample_size_mcnemar_p10 = "0.10")),
  regression = merge_input(base_input("regression"), list(sample_size_regression_design = "multiple", sample_size_regression_effect = "0.15", sample_size_regression_predictors = "3")),
  gee = merge_input(base_input("gee"), list(sample_size_gee_outcome = "continuous", sample_size_gee_effect = "0.50", sample_size_gee_p1 = "0.50", sample_size_gee_p2 = "0.65", sample_size_gee_time_points = "3", sample_size_gee_rho = "0.50", sample_size_gee_correlation_structure = "exchangeable")),
  lmm = merge_input(base_input("lmm"), list(sample_size_lmm_mode = "simple", sample_size_lmm_design = "two_group_repeated", sample_size_lmm_effect = "0.50", sample_size_lmm_time_points = "3", sample_size_lmm_icc = "0.20", sample_size_lmm_simulations = "20")),
  survival = merge_input(base_input("survival"), list(sample_size_survival_hr = "0.70", sample_size_survival_event_probability = "0.50")),
  equivalence = merge_input(base_input("equivalence"), list(sample_size_equivalence_outcome = "mean", sample_size_equivalence_objective = "noninferiority", sample_size_equivalence_difference = "0", sample_size_equivalence_margin = "0.5", sample_size_equivalence_sd = "1", sample_size_equivalence_p1 = "0.50", sample_size_equivalence_p2 = "0.50")),
  diagnostic = merge_input(base_input("diagnostic"), list(sample_size_diagnostic_design = "sensitivity", sample_size_diagnostic_sensitivity = "0.85", sample_size_diagnostic_specificity = "0.85", sample_size_diagnostic_prevalence = "0.20", sample_size_diagnostic_precision = "0.10", sample_size_diagnostic_auc = "0.75", sample_size_diagnostic_null_auc = "0.50")),
  rates = merge_input(base_input("rates"), list(sample_size_rates_design = "two_rate_ratio", sample_size_rates_rate1 = "0.10", sample_size_rates_rate2 = "0.20", sample_size_rates_half_width = "0.05", sample_size_rates_dispersion = "0")),
  cluster = merge_input(base_input("cluster"), list(sample_size_cluster_design = "parallel", sample_size_cluster_outcome = "continuous", sample_size_cluster_effect = "0.50", sample_size_cluster_p1 = "0.50", sample_size_cluster_p2 = "0.65", sample_size_cluster_size = "20", sample_size_cluster_icc = "0.05", sample_size_cluster_periods = "4", sample_size_cluster_simulations = "20")),
  precision = merge_input(base_input("precision"), list(sample_size_precision_parameter = "mean", sample_size_precision_confidence = "0.95", sample_size_precision_half_width = "0.10", sample_size_precision_sd = "1", sample_size_precision_proportion = "0.50", sample_size_precision_r = "0.30")),
  reliability = merge_input(base_input("reliability"), list(sample_size_reliability_design = "alpha", sample_size_reliability_value = "0.80", sample_size_reliability_confidence = "0.95", sample_size_reliability_half_width = "0.10", sample_size_reliability_items = "5", sample_size_reliability_categories = "2")),
  sem = merge_input(base_input("sem"), list(sample_size_sem_test = "close_fit", sample_size_sem_df_source = "structure", sample_size_sem_latent_variables = "3", sample_size_sem_measured_variables = "12", sample_size_sem_structural_paths = "3", sample_size_sem_null_rmsea = "0.05", sample_size_sem_alternative_rmsea = "0.08"))
)

for (method in names(cases)) {
  result <- sample_size_calculate(method, cases[[method]])
  expect_true(is.null(result$error), sprintf("Expected %s sample-size calculation to complete: %s", method, result$error %||% ""))
  expect_true(
    !is.null(result$total) || !is.null(result$group1) || !is.null(result$per_group) || !is.null(result$required_events),
    sprintf("Expected %s sample-size result to include an interpretable sample-size field", method)
  )
}

message("Checking achieved power mode for applicable sample-size menus...")
power_checks <- list(
  ttest = list(
    sample = quote(sample_size_ttest("sample_size", "two_sample", 0.5, 0.05, power = 0.8)),
    power = function(result) sample_size_ttest("power", "two_sample", 0.5, 0.05, n = result$group1)$power
  ),
  proportion = list(
    sample = quote(sample_size_proportion("sample_size", "two_proportion", p1 = 0.5, p2 = 0.65, alpha = 0.05, power = 0.8)),
    power = function(result) sample_size_proportion("power", "two_proportion", p1 = 0.5, p2 = 0.65, alpha = 0.05, n = result$group1)$power
  ),
  chisquare = list(
    sample = quote(sample_size_chisquare("sample_size", df = 1, effect_size = 0.3, alpha = 0.05, power = 0.8)),
    power = function(result) sample_size_chisquare("power", df = 1, effect_size = 0.3, alpha = 0.05, n = result$total)$power
  ),
  correlation = list(
    sample = quote(sample_size_correlation("sample_size", r = 0.3, alpha = 0.05, power = 0.8)),
    power = function(result) sample_size_correlation("power", r = 0.3, alpha = 0.05, n = result$total)$power
  ),
  anova = list(
    sample = quote(sample_size_anova("sample_size", design = "one_way", groups = 3, effect_size = 0.25, alpha = 0.05, power = 0.8)),
    power = function(result) sample_size_anova("power", design = "one_way", groups = 3, effect_size = 0.25, alpha = 0.05, n = result$total)$power
  ),
  ancova = list(
    sample = quote(sample_size_ancova("sample_size", design = "ancova", groups = 2, effect_size = 0.25, covariates = 1, covariate_r2 = 0.3, alpha = 0.05, power = 0.8)),
    power = function(result) sample_size_ancova("power", design = "ancova", groups = 2, effect_size = 0.25, covariates = 1, covariate_r2 = 0.3, alpha = 0.05, n = result$total)$power
  ),
  nonparametric = list(
    sample = quote(sample_size_nonparametric("sample_size", "two_independent", 0.5, 0.05, power = 0.8)),
    power = function(result) sample_size_nonparametric("power", "two_independent", 0.5, 0.05, n = result$group1)$power
  ),
  mcnemar = list(
    sample = quote(sample_size_mcnemar("sample_size", p01 = 0.2, p10 = 0.1, alpha = 0.05, power = 0.8)),
    power = function(result) sample_size_mcnemar("power", p01 = 0.2, p10 = 0.1, alpha = 0.05, n = result$total)$power
  ),
  regression = list(
    sample = quote(sample_size_regression("sample_size", design = "multiple", effect_size = 0.15, alpha = 0.05, power = 0.8, predictors = 3)),
    power = function(result) sample_size_regression("power", design = "multiple", effect_size = 0.15, alpha = 0.05, n = result$total, predictors = 3)$power
  ),
  gee = list(
    sample = quote(sample_size_gee("sample_size", outcome = "continuous", effect_size = 0.5, alpha = 0.05, power = 0.8, time_points = 3, rho = 0.5)),
    power = function(result) sample_size_gee("power", outcome = "continuous", effect_size = 0.5, alpha = 0.05, n = result$group1, time_points = 3, rho = 0.5)$power
  ),
  lmm = list(
    sample = quote(sample_size_lmm("sample_size", mode = "simple", design = "two_group_repeated", effect_size = 0.5, alpha = 0.05, power = 0.8, time_points = 3, icc = 0.2, simulations = 20)),
    power = function(result) sample_size_lmm("power", mode = "simple", design = "two_group_repeated", effect_size = 0.5, alpha = 0.05, n = result$group1, time_points = 3, icc = 0.2, simulations = 20)$power
  ),
  survival = list(
    sample = quote(sample_size_survival("sample_size", hazard_ratio = 0.7, event_probability = 0.5, alpha = 0.05, power = 0.8)),
    power = function(result) sample_size_survival("power", hazard_ratio = 0.7, event_probability = 0.5, alpha = 0.05, n = result$total)$power
  ),
  equivalence = list(
    sample = quote(sample_size_equivalence("sample_size", outcome = "mean", objective = "noninferiority", true_difference = 0, margin = 0.5, sd = 1, alpha = 0.05, power = 0.8)),
    power = function(result) sample_size_equivalence("power", outcome = "mean", objective = "noninferiority", true_difference = 0, margin = 0.5, sd = 1, alpha = 0.05, n = result$group1)$power
  ),
  diagnostic_auc = list(
    sample = quote(sample_size_diagnostic("sample_size", design = "auc", auc = 0.75, null_auc = 0.5, alpha = 0.05, power = 0.8)),
    power = function(result) sample_size_diagnostic("power", design = "auc", auc = 0.75, null_auc = 0.5, alpha = 0.05, n = result$group1)$power
  ),
  rates = list(
    sample = quote(sample_size_rates("sample_size", design = "two_rate_ratio", rate1 = 0.1, rate2 = 0.2, alpha = 0.05, power = 0.8)),
    power = function(result) sample_size_rates("power", design = "two_rate_ratio", rate1 = 0.1, rate2 = 0.2, alpha = 0.05, n = result$group1)$power
  ),
  cluster = list(
    sample = quote(sample_size_cluster("sample_size", design = "parallel", outcome = "continuous", effect_size = 0.5, alpha = 0.05, power = 0.8, cluster_size = 20, icc = 0.05)),
    power = function(result) sample_size_cluster("power", design = "parallel", outcome = "continuous", effect_size = 0.5, alpha = 0.05, n = result$group1, cluster_size = 20, icc = 0.05)$power
  ),
  sem = list(
    sample = quote(sample_size_sem("sample_size", test = "close_fit", df = 50, null_rmsea = 0.05, alternative_rmsea = 0.08, alpha = 0.05, power = 0.8)),
    power = function(result) sample_size_sem("power", test = "close_fit", df = 50, null_rmsea = 0.05, alternative_rmsea = 0.08, alpha = 0.05, n = result$total)$power
  )
)

for (name in names(power_checks)) {
  sample_result <- eval(power_checks[[name]]$sample)
  achieved_power <- power_checks[[name]]$power(sample_result)
  expect_true(
    is.finite(achieved_power) && achieved_power >= 0.79,
    sprintf("Expected %s power mode to reproduce target power; got %.3f", name, achieved_power)
  )
}

message("All sample-size validations passed.")
