Sys.setenv(STATEDU_NO_PACKAGE_INSTALL = "true")

source(file.path("R", "app_bootstrap.R"))
load_app_packages(check = FALSE)
source_app_modules()

stopifnot(exists("prepare_mixed_rm_anova_results"))

set.seed(42)
n_per_group <- 12
group <- rep(c("Control", "Treatment"), each = n_per_group)
baseline <- rnorm(n_per_group * 2, 50, 4)
data <- data.frame(
  group = group,
  age = rnorm(n_per_group * 2, 45, 7),
  sex = rep(c("Female", "Male"), length.out = n_per_group * 2),
  pre = baseline + rnorm(n_per_group * 2, 0, 1),
  post = baseline + ifelse(group == "Treatment", 6, 1) + rnorm(n_per_group * 2, 0, 1),
  post2 = baseline + ifelse(group == "Treatment", 9, 1.5) + rnorm(n_per_group * 2, 0, 1),
  stringsAsFactors = FALSE
)
variable_info <- data.frame(
  name = names(data),
  measurement = c("category", "continuous", "binary", "continuous", "continuous", "continuous"),
  stringsAsFactors = FALSE
)

result <- prepare_mixed_rm_anova_results(
  data,
  group_variable = "group",
  repeated_variables = c("pre", "post", "post2"),
  variable_info = variable_info,
  options = list(assumption_check = TRUE, posthoc = TRUE, posthoc_adjustment = "holm", time_labels = c("Pre", "Post", "Post 2"))
)

stopifnot(identical(result$type, "mixed_rm_anova"))
stopifnot(is.data.frame(result$anova), all(c("Group", "Time", "Group x Time") %in% result$anova$Effect))
stopifnot(is.data.frame(result$anova), all(c("Mauchly W", "p_sphericity", "epsilon(GG)", "epsilon(HF)", "Correction", "ES", "post-hoc") %in% names(result$anova)))
stopifnot(!"partial eta2" %in% names(result$anova))
anova_labels <- attr(result$anova, "column_display_labels", exact = TRUE)
stopifnot(identical(as.character(anova_labels[["epsilon(GG)"]]), "epsilon\n(GG)"))
stopifnot(identical(as.character(anova_labels[["epsilon(HF)"]]), "epsilon\n(HF)"))
anova_markers <- attr(result$anova, "note_markers", exact = TRUE)
stopifnot(is.data.frame(anova_markers), any(anova_markers$column == "ES" & anova_markers$marker == "1"))
stopifnot(grepl("1 ES = partial \u03b7\u00b2.", result$method_note, fixed = TRUE))
stopifnot(is.data.frame(result$descriptives), nrow(result$descriptives) == 2L)
stopifnot(all(c("Group", "N", "Pre", "Post", "Post 2", "F", "p", "post-hoc") %in% names(result$descriptives)))
summary_markers <- attr(result$descriptives, "column_header_markers", exact = TRUE)
stopifnot(is.data.frame(summary_markers), identical(as.character(summary_markers$marker), c("a", "b", "c")))
stopifnot(is.data.frame(result$assumption), any(result$assumption$Item == "Sphericity"))
stopifnot(!any(result$assumption$Item == "Cell normality"))
stopifnot(any(result$assumption$Item == "Total cases"), any(result$assumption$Item == "Excluded cases"))
stopifnot(any(result$assumption$Item == "Levene homogeneity"))
levene_row <- result$assumption[result$assumption$Item == "Levene homogeneity", , drop = FALSE]
stopifnot(nrow(levene_row) == 1L)
stopifnot(grepl("Pre=", levene_row$Detail[[1]], fixed = TRUE))
stopifnot(grepl("raw values by group.", levene_row$Detail[[1]], fixed = TRUE))
mauchly_row <- result$assumption[result$assumption$Item == "Sphericity", , drop = FALSE]
stopifnot(nrow(mauchly_row) == 1L)
stopifnot(!grepl("p=NA", mauchly_row$Detail[[1]], fixed = TRUE))
stopifnot(!grepl("W=;", mauchly_row$Detail[[1]], fixed = TRUE))
assumption_recommendation <- result$recommendation[result$recommendation$Item == "Assumption decision", , drop = FALSE]
stopifnot(nrow(assumption_recommendation) == 1L)
stopifnot(grepl("Levene:", assumption_recommendation$Reason[[1]], fixed = TRUE))
stopifnot(is.data.frame(result$normality), all(c("Time", "Control", "Treatment") %in% names(result$normality)), nrow(result$normality) == 3L)
stopifnot(is.data.frame(result$posthoc), nrow(result$posthoc) > 0)
stopifnot(is.data.frame(result$recommendation), any(result$recommendation$Item == "Primary effect"))
stopifnot(any(result$recommendation$Recommendation == "Group x Time"))
stopifnot(any(result$recommendation$Item == "Analysis population" & result$recommendation$Recommendation == "PP / complete-case repeated-measures ANOVA"))
stopifnot(any(result$posthoc$Family == "Time comparison overall" & result$posthoc$Stratum == "Overall" & result$posthoc$Contrast == "a-b"))
stopifnot(!any(result$posthoc$Family == "Time comparison within each group"))
stopifnot(any(result$posthoc$Family == "Group comparison at each time" & result$posthoc$Stratum == "a"))
stopifnot(grepl("Time markers: a = Pre; b = Post; c = Post 2.", result$posthoc_note, fixed = TRUE))
stopifnot(grepl("significant parent omnibus effect", result$posthoc_note, fixed = TRUE))
stopifnot(!grepl("Group ", result$anova$`post-hoc`[result$anova$Effect == "Time"], fixed = TRUE))
stopifnot(nzchar(result$descriptives$`post-hoc`[[1]]))
stopifnot(grepl(">", result$anova$`post-hoc`[result$anova$Effect == "Time"], fixed = TRUE))
stopifnot(!grepl("p_adj", result$anova$`post-hoc`[result$anova$Effect == "Time"], fixed = TRUE))
stopifnot(grepl(">", result$descriptives$`post-hoc`[[1]], fixed = TRUE))
stopifnot(!grepl("p_adj", result$descriptives$`post-hoc`[[1]], fixed = TRUE))
stopifnot(any(nzchar(result$anova$`post-hoc`[result$anova$Effect %in% c("Time", "Group x Time")])))
stopifnot(any(grepl("Group x Time", result$overview$Value, fixed = TRUE)))
chain_rows <- data.frame(Contrast = c("c-d", "c-b", "d-b"), `p adjusted` = c("<.001", "<.001", "<.001"), stringsAsFactors = FALSE, check.names = FALSE)
stopifnot(identical(mixed_rm_significant_order_notation(c("b", "c", "d"), c(b = 1, c = 3, d = 2), chain_rows), "c>d>b"))
tier_rows <- data.frame(Contrast = c("c-b", "c-a", "b-a", "d-b", "d-a"), `p adjusted` = c("<.001", "<.001", "<.001", "<.001", "<.001"), stringsAsFactors = FALSE, check.names = FALSE)
stopifnot(identical(mixed_rm_significant_order_notation(c("a", "b", "c", "d"), c(a = 1, b = 2, c = 4, d = 3.9), tier_rows), "c,d>b>a"))

set.seed(100)
hetero_data <- data.frame(
  group = rep(c("A", "B"), each = 30),
  pre = c(rnorm(30, 10, .2), rnorm(30, 10, 5)),
  post = c(rnorm(30, 11, .2), rnorm(30, 11, 5)),
  post2 = c(rnorm(30, 12, .2), rnorm(30, 12, 5)),
  stringsAsFactors = FALSE
)
hetero_info <- data.frame(
  name = names(hetero_data),
  measurement = c("category", "continuous", "continuous", "continuous"),
  stringsAsFactors = FALSE
)
hetero_result <- prepare_mixed_rm_anova_results(
  hetero_data,
  group_variable = "group",
  repeated_variables = c("pre", "post", "post2"),
  variable_info = hetero_info,
  options = list(assumption_check = TRUE, posthoc = FALSE, time_labels = c("Pre", "Post", "Post 2"))
)
hetero_levene <- hetero_result$assumption[hetero_result$assumption$Item == "Levene homogeneity", , drop = FALSE]
stopifnot(nrow(hetero_levene) == 1L)
stopifnot(identical(as.character(hetero_levene$Result[[1]]), "Potential violation"))
hetero_assumption_recommendation <- hetero_result$recommendation[hetero_result$recommendation$Item == "Assumption decision", , drop = FALSE]
stopifnot(grepl("Between-group homogeneity was flagged", hetero_assumption_recommendation$Recommendation[[1]], fixed = TRUE))

setup_state <- mixed_rm_anova_setup_state(
  selected_names = names(data),
  group_variable = "group",
  repeated_variables = c("pre", "post", "post2"),
  covariates = "age",
  variable_table = variable_info,
  options_tab = "Between",
  analysis_population = "itt",
  time_labels = c("Pre", "Post", "Post 2"),
  language = "ko"
)
setup_html <- paste(htmltools::renderTags(mixed_rm_anova_setup_panel(setup_state))$html, collapse = "\n")
stopifnot(grepl("mixed_rm_anova_options_tab", setup_html, fixed = TRUE))
stopifnot(grepl("mixed_rm_anova_analysis_population", setup_html, fixed = TRUE))
stopifnot(identical(setup_state$analysis_population, "itt"))
stopifnot(grepl("\uac00\uc815\uac80\ud1a0", setup_html, fixed = TRUE))
stopifnot(grepl("\uac80\ud1a0", setup_html, fixed = TRUE), grepl("\ucd9c\ub825", setup_html, fixed = TRUE), grepl("\ub77c\ubca8", setup_html, fixed = TRUE))
stopifnot(grepl("\uc694\uc57d \uc635\uc158", setup_html, fixed = TRUE), grepl("\uad70\ub0b4 \uc2dc\uc810 \ube44\uad50", setup_html, fixed = TRUE))
stopifnot(grepl("\uc2dc\uc810\ubcc4 \uad70\uac04 \ube44\uad50", setup_html, fixed = TRUE))
stopifnot(!grepl("\ub2e8\uc21c \ucc28\uc774", setup_html, fixed = TRUE), !grepl("\ucd94\uc815 \ud3c9\uade0 \ucc28\uc774", setup_html, fixed = TRUE))
stopifnot(grepl("mixed_rm_anova_posthoc", setup_html, fixed = TRUE))
stopifnot(grepl("mixed_rm_anova_within_group_comparison", setup_html, fixed = TRUE))
stopifnot(grepl("mixed_rm_anova_between_time_group_comparison", setup_html, fixed = TRUE))

setup_state_between_enabled <- mixed_rm_anova_setup_state(
  selected_names = names(data),
  group_variable = "group",
  repeated_variables = c("pre", "post", "post2"),
  covariates = "age",
  variable_table = variable_info,
  options_tab = "Output",
  between_time_group_comparison = TRUE,
  time_labels = c("Pre", "Post", "Post 2"),
  language = "ko"
)
setup_html_between_enabled <- paste(htmltools::renderTags(mixed_rm_anova_setup_panel(setup_state_between_enabled))$html, collapse = "\n")
stopifnot(grepl("mixed_rm_anova_between_time_group_comparison", setup_html_between_enabled, fixed = TRUE))

setup_state_en <- mixed_rm_anova_setup_state(
  selected_names = names(data),
  group_variable = "group",
  repeated_variables = c("pre", "post", "post2"),
  covariates = "age",
  variable_table = variable_info,
  options_tab = "Output",
  time_labels = c("Pre", "Post", "Post 2"),
  language = "en"
)
setup_html_en <- paste(htmltools::renderTags(mixed_rm_anova_setup_panel(setup_state_en))$html, collapse = "\n")
stopifnot(grepl("Independent variables", setup_html_en, fixed = TRUE))
stopifnot(grepl("Check assumptions", setup_html_en, fixed = TRUE))
stopifnot(grepl("Summary options", setup_html_en, fixed = TRUE), grepl("Within-group time comparison", setup_html_en, fixed = TRUE))
stopifnot(grepl("Between-group comparison", setup_html_en, fixed = TRUE))
stopifnot(!grepl("Simple difference", setup_html_en, fixed = TRUE), !grepl("Estimated mean difference", setup_html_en, fixed = TRUE))

two_time <- prepare_mixed_rm_anova_results(
  data,
  group_variable = "group",
  repeated_variables = c("pre", "post"),
  variable_info = variable_info,
  options = list(assumption_check = FALSE, posthoc = FALSE, time_labels = c("Pre", "Post"))
)
stopifnot(is.data.frame(two_time$assumption), nrow(two_time$assumption) == 0L)
stopifnot(is.data.frame(two_time$normality), nrow(two_time$normality) == 0L)
stopifnot(is.data.frame(two_time$anova), any(two_time$anova$Correction == "Not required"))
stopifnot(!"post-hoc" %in% names(two_time$descriptives))
stopifnot(all(!nzchar(two_time$anova$`post-hoc`)))

no_assumption_review <- prepare_mixed_rm_anova_results(
  data,
  group_variable = "group",
  repeated_variables = c("pre", "post", "post2"),
  variable_info = variable_info,
  options = list(assumption_check = FALSE, posthoc = FALSE, time_labels = c("Pre", "Post", "Post 2"))
)
stopifnot(is.data.frame(no_assumption_review$assumption), nrow(no_assumption_review$assumption) == 0L)
stopifnot(is.data.frame(no_assumption_review$normality), nrow(no_assumption_review$normality) == 0L)
stopifnot(any(no_assumption_review$recommendation$Item == "Assumption decision"))
no_assumption_decision <- no_assumption_review$recommendation[no_assumption_review$recommendation$Item == "Assumption decision", , drop = FALSE]
stopifnot(grepl("Assumption review was not requested", no_assumption_decision$Recommendation[[1]], fixed = TRUE))
stopifnot(grepl("Assumption review was not requested", no_assumption_decision$Reason[[1]], fixed = TRUE))

no_within <- prepare_mixed_rm_anova_results(
  data,
  group_variable = "group",
  repeated_variables = c("pre", "post", "post2"),
  variable_info = variable_info,
  options = list(assumption_check = TRUE, posthoc = TRUE, within_group_comparison = FALSE, time_labels = c("Pre", "Post", "Post 2"))
)
stopifnot(!any(c("F", "p", "post-hoc") %in% names(no_within$descriptives)))

between_simple <- prepare_mixed_rm_anova_results(
  data,
  group_variable = "group",
  repeated_variables = c("pre", "post", "post2"),
  variable_info = variable_info,
  options = list(assumption_check = TRUE, posthoc = TRUE, between_time_group_comparison = TRUE, time_labels = c("Pre", "Post", "Post 2"))
)
stopifnot(nrow(between_simple$descriptives) == 3L)
stopifnot(identical(as.character(between_simple$descriptives$Group[[3]]), "Between groups"))
stopifnot(identical(as.character(between_simple$descriptives$N[[3]]), "F(p)"))
stopifnot(grepl("(", between_simple$descriptives$Pre[[3]], fixed = TRUE))
stopifnot(!grepl("F=", between_simple$descriptives$Pre[[3]], fixed = TRUE))
stopifnot(!grepl("p=", between_simple$descriptives$Pre[[3]], fixed = TRUE))
stopifnot(!grepl(">", between_simple$descriptives$Pre[[3]], fixed = TRUE))
stopifnot(!grepl("Time ", result$anova$`post-hoc`[result$anova$Effect == "Group x Time"], fixed = TRUE))

missing_data <- data
missing_data$post[2] <- NA_real_
missing_result <- prepare_mixed_rm_anova_results(
  missing_data,
  group_variable = "group",
  repeated_variables = c("pre", "post", "post2"),
  variable_info = variable_info,
  options = list(assumption_check = TRUE, posthoc = FALSE, time_labels = c("Pre", "Post", "Post 2"))
)
stopifnot(identical(as.character(missing_result$assumption$Result[missing_result$assumption$Item == "Total cases"]), as.character(nrow(data))))
stopifnot(identical(as.character(missing_result$assumption$Result[missing_result$assumption$Item == "Excluded cases"]), "1"))
stopifnot(identical(as.character(missing_result$assumption$Result[missing_result$assumption$Item == "Complete cases"]), as.character(nrow(data) - 1L)))

three_group <- data.frame(
  group = rep(c("A", "B", "C"), each = 8),
  age = rnorm(24, 45, 7),
  pre = c(rnorm(8, 40, 1), rnorm(8, 50, 1), rnorm(8, 62, 1)),
  post = c(rnorm(8, 42, 1), rnorm(8, 53, 1), rnorm(8, 65, 1)),
  post2 = c(rnorm(8, 43, 1), rnorm(8, 55, 1), rnorm(8, 68, 1)),
  stringsAsFactors = FALSE
)
three_info <- data.frame(
  name = names(three_group),
  measurement = c("category", "continuous", "continuous", "continuous", "continuous"),
  stringsAsFactors = FALSE
)
three_between <- prepare_mixed_rm_anova_results(
  three_group,
  group_variable = "group",
  repeated_variables = c("pre", "post", "post2"),
  variable_info = three_info,
  options = list(assumption_check = FALSE, posthoc = TRUE, between_time_group_comparison = TRUE, time_labels = c("Pre", "Post", "Post 2"))
)
stopifnot(any(three_between$descriptives$Group == "post-hoc"))
three_posthoc_row <- three_between$descriptives[three_between$descriptives$Group == "post-hoc", , drop = FALSE]
stopifnot(any(grepl(">", unlist(three_posthoc_row[, c("Pre", "Post", "Post 2")]), fixed = TRUE)))

adjusted <- prepare_mixed_rm_anova_results(
  data,
  group_variable = "group",
  repeated_variables = c("pre", "post", "post2"),
  covariates = "age",
  variable_info = variable_info,
  options = list(assumption_check = TRUE, posthoc = FALSE, time_labels = c("Pre", "Post", "Post 2"))
)
stopifnot(any(adjusted$overview$Item == "Covariates"))
stopifnot(any(adjusted$anova$Effect == "age"))
stopifnot(any(adjusted$anova$Effect == "age x Time"))
stopifnot(any(adjusted$recommendation$Item == "Covariate x Time"))
adjusted_levene_row <- adjusted$assumption[adjusted$assumption$Item == "Levene homogeneity", , drop = FALSE]
stopifnot(nrow(adjusted_levene_row) == 1L)
stopifnot(grepl("adjusted residuals (group + covariates).", adjusted_levene_row$Detail[[1]], fixed = TRUE))
stopifnot(all(c("F", "p") %in% names(adjusted$descriptives)))
stopifnot(is.data.frame(adjusted$observed_descriptives), is.data.frame(adjusted$adjusted_descriptives))
stopifnot(all(c("F", "p") %in% names(adjusted$observed_descriptives)))
stopifnot(all(c("F", "p") %in% names(adjusted$adjusted_descriptives)))
stopifnot(grepl("M", adjusted$observed_descriptives_note, fixed = TRUE), grepl("covariate-adjusted time comparisons", adjusted$adjusted_descriptives_note, fixed = TRUE))

adjusted_with_posthoc <- prepare_mixed_rm_anova_results(
  data,
  group_variable = "group",
  repeated_variables = c("pre", "post", "post2"),
  covariates = "age",
  variable_info = variable_info,
  options = list(assumption_check = TRUE, posthoc = TRUE, time_labels = c("Pre", "Post", "Post 2"))
)
stopifnot(nzchar(adjusted_with_posthoc$anova$`post-hoc`[adjusted_with_posthoc$anova$Effect == "Time"]))
stopifnot(is.data.frame(adjusted_with_posthoc$posthoc), nrow(adjusted_with_posthoc$posthoc) > 0)
stopifnot(all(c("Observed overall", "Adjusted overall") %in% adjusted_with_posthoc$posthoc$Family))
stopifnot(all(c("Observed within group", "Adjusted within group") %in% adjusted_with_posthoc$posthoc$Family))
stopifnot(any(adjusted_with_posthoc$posthoc$Method == "Covariate-adjusted paired difference"))
stopifnot("post-hoc" %in% names(adjusted_with_posthoc$adjusted_descriptives))
adjusted_posthoc_html <- paste(htmltools::renderTags(mixed_rm_anova_results_ui(adjusted_with_posthoc))$html, collapse = "\n")
stopifnot(grepl("Post-hoc comparisons", adjusted_posthoc_html, fixed = TRUE))
stopifnot(grepl("Adjusted within group", adjusted_posthoc_html, fixed = TRUE))
stopifnot(grepl("border-top:2px solid", adjusted_posthoc_html, fixed = TRUE))

set.seed(7)
cov_time_n <- 80
cov_time_z <- rep(seq(-2, 2, length.out = 40), 2)
cov_time_data <- data.frame(
  group = rep(c("A", "B"), each = 40),
  z = cov_time_z,
  pre = 10 + 0 * cov_time_z + rnorm(cov_time_n, 0, .15),
  post = 10 + 1 * cov_time_z + rnorm(cov_time_n, 0, .15),
  post2 = 10 + 2 * cov_time_z + rnorm(cov_time_n, 0, .15),
  stringsAsFactors = FALSE
)
cov_time_info <- data.frame(
  name = names(cov_time_data),
  measurement = c("category", rep("continuous", 4)),
  stringsAsFactors = FALSE
)
cov_time_result <- prepare_mixed_rm_anova_results(
  cov_time_data,
  group_variable = "group",
  repeated_variables = c("pre", "post", "post2"),
  covariates = "z",
  variable_info = cov_time_info,
  options = list(assumption_check = FALSE, posthoc = TRUE, time_labels = c("Pre", "Post", "Post 2"))
)
stopifnot(any(cov_time_result$anova$Effect == "z x Time" & cov_time_result$anova$p == "<.001"))
stopifnot(grepl("adjusted estimates are time-specific", cov_time_result$adjusted_descriptives_note, fixed = TRUE))
stopifnot(grepl("not a post-hoc family", cov_time_result$posthoc_note, fixed = TRUE))

nonsig_time <- data.frame(
  group = rep(c("A", "B"), each = 80),
  x1 = c(seq(-40, 40, length.out = 80), seq(40, -40, length.out = 80)),
  x2 = rep(c(1.9, 2.1), 80),
  x3 = rep(c(2.6, 2.8), 80),
  x4 = rep(c(2.3, 2.5), 80),
  x5 = rep(c(1.4, 1.6), 80),
  stringsAsFactors = FALSE
)
nonsig_time_result <- prepare_mixed_rm_anova_results(
  nonsig_time,
  group_variable = "group",
  repeated_variables = c("x1", "x2", "x3", "x4", "x5"),
  variable_info = data.frame(
    name = names(nonsig_time),
    measurement = c("category", rep("continuous", 5)),
    stringsAsFactors = FALSE
  ),
  options = list(assumption_check = FALSE, posthoc = TRUE, time_labels = c("pre", "post1", "post2", "post3", "post4"))
)
nonsig_summary_p <- vapply(nonsig_time_result$observed_descriptives$p, mixed_rm_parse_p, numeric(1))
if (any(is.finite(nonsig_summary_p) & nonsig_summary_p >= .05)) {
  stopifnot(all(nonsig_time_result$observed_descriptives$`post-hoc`[is.finite(nonsig_summary_p) & nonsig_summary_p >= .05] == "n.s."))
}

categorical_adjusted <- prepare_mixed_rm_anova_results(
  data,
  group_variable = "group",
  repeated_variables = c("pre", "post", "post2"),
  covariates = c("age", "sex"),
  variable_info = variable_info,
  options = list(assumption_check = TRUE, posthoc = FALSE, time_labels = c("Pre", "Post", "Post 2"))
)
stopifnot(any(categorical_adjusted$anova$Effect == "sex"))
stopifnot(any(categorical_adjusted$anova$Effect == "sex x Time"))
stopifnot(grepl("estimated marginal means", categorical_adjusted$descriptives_note, fixed = TRUE))
stopifnot(grepl("\u00b1", categorical_adjusted$descriptives$Pre[[1]], fixed = TRUE))
categorical_html <- paste(htmltools::renderTags(mixed_rm_anova_results_ui(categorical_adjusted))$html, collapse = "\n")
stopifnot(grepl("Observed mean summary", categorical_html, fixed = TRUE), grepl("Adjusted mean summary", categorical_html, fixed = TRUE))

factorial <- prepare_mixed_rm_anova_results(
  data,
  group_variable = c("group", "sex"),
  repeated_variables = c("pre", "post", "post2"),
  variable_info = variable_info,
  options = list(assumption_check = TRUE, posthoc = FALSE, time_labels = c("Pre", "Post", "Post 2"))
)
stopifnot(all(c("group", "sex", "group x sex", "Time", "group x Time", "sex x Time", "group x sex x Time") %in% factorial$anova$Effect))
stopifnot(any(factorial$overview$Item == "Independent variables" & factorial$overview$Value == "group, sex"))
stopifnot(any(factorial$overview$Item == "Primary effect" & factorial$overview$Value == "group x sex x Time interaction"))
stopifnot(any(factorial$recommendation$Item == "Recommended model" & grepl("factorial", factorial$recommendation$Recommendation, fixed = TRUE)))
stopifnot(any(factorial$recommendation$Item == "Primary effect" & factorial$recommendation$Recommendation == "group x sex x Time"))
stopifnot(grepl("Control / Female", factorial$assumption$Detail[factorial$assumption$Item == "Groups"], fixed = TRUE))
stopifnot(all(c("group", "sex") %in% names(factorial$descriptives)))
stopifnot(!"Group" %in% names(factorial$descriptives))

factorial_posthoc <- prepare_mixed_rm_anova_results(
  data,
  group_variable = c("group", "sex"),
  repeated_variables = c("pre", "post", "post2"),
  variable_info = variable_info,
  options = list(assumption_check = TRUE, posthoc = TRUE, time_labels = c("Pre", "Post", "Post 2"))
)
three_way_posthoc <- factorial_posthoc$anova$`post-hoc`[factorial_posthoc$anova$Effect == "group x sex x Time"]
stopifnot(length(three_way_posthoc) == 1L, !nzchar(three_way_posthoc))

itt_missing <- prepare_mixed_rm_anova_results(
  missing_data,
  group_variable = "group",
  repeated_variables = c("pre", "post", "post2"),
  covariates = "age",
  variable_info = variable_info,
  options = list(assumption_check = TRUE, posthoc = FALSE, analysis_population = "itt", time_labels = c("Pre", "Post", "Post 2"))
)
stopifnot(identical(itt_missing$analysis_population, "itt"))
stopifnot(any(itt_missing$overview$Item == "Analysis population" & itt_missing$overview$Value == "ITT / available repeated-measures mixed model"))
stopifnot(is.data.frame(itt_missing$mixed_model_overview), any(itt_missing$mixed_model_overview$Value == "Linear mixed model"))
stopifnot(is.data.frame(itt_missing$mixed_model_coefficients), nrow(itt_missing$mixed_model_coefficients) > 0)
stopifnot(any(itt_missing$mixed_model_overview$Item == "Reference levels" & grepl("Time = Pre", itt_missing$mixed_model_overview$Value, fixed = TRUE)))
stopifnot(any(itt_missing$mixed_model_overview$Item == "Formula" & grepl(".statedu_rm_time:age", itt_missing$mixed_model_overview$Value, fixed = TRUE)))
stopifnot(!any(grepl(".statedu_rm_time", itt_missing$mixed_model_coefficients$Term, fixed = TRUE)))
stopifnot(any(grepl("Time: Post vs Pre", itt_missing$mixed_model_coefficients$Term, fixed = TRUE)))
stopifnot(any(grepl("Time: Post vs Pre x age", itt_missing$mixed_model_coefficients$Term, fixed = TRUE)))
stopifnot(any(itt_missing$recommendation$Item == "Mixed-model decision" & grepl("fitted LMM", itt_missing$recommendation$Recommendation, fixed = TRUE)))

set.seed(99)
itt_no_complete <- data.frame(
  group = rep(c("Control", "Treatment"), each = 12),
  age = rnorm(24, 50, 5),
  pre = rnorm(24, 10, 1),
  post = rnorm(24, 12, 1),
  post2 = rnorm(24, 14, 1),
  stringsAsFactors = FALSE
)
itt_no_complete$post[seq(1, 24, 2)] <- NA_real_
itt_no_complete$pre[seq(2, 24, 2)] <- NA_real_
itt_no_complete_result <- prepare_mixed_rm_anova_results(
  itt_no_complete,
  group_variable = "group",
  repeated_variables = c("pre", "post", "post2"),
  covariates = "age",
  variable_info = variable_info,
  options = list(assumption_check = TRUE, posthoc = TRUE, analysis_population = "itt", time_labels = c("Pre", "Post", "Post 2"))
)
stopifnot(identical(itt_no_complete_result$analysis_population, "itt"))
stopifnot(!is.data.frame(itt_no_complete_result$anova) || nrow(itt_no_complete_result$anova) == 0)
stopifnot(any(itt_no_complete_result$assumption$Item == "Complete-case RM ANOVA" & itt_no_complete_result$assumption$Result == "Not available"))
stopifnot(is.data.frame(itt_no_complete_result$mixed_model_coefficients), nrow(itt_no_complete_result$mixed_model_coefficients) > 0)
stopifnot(any(itt_no_complete_result$mixed_model_overview$Item == "Formula" & grepl(".statedu_rm_time:age", itt_no_complete_result$mixed_model_overview$Value, fixed = TRUE)))

count_data <- data.frame(
  group = rep(c("A", "B"), each = 15),
  pre = rpois(30, 8),
  post = rpois(30, rep(c(9, 14), each = 15)),
  post2 = rpois(30, rep(c(10, 18), each = 15)),
  stringsAsFactors = FALSE
)
count_info <- data.frame(
  name = names(count_data),
  measurement = c("category", "continuous", "continuous", "continuous"),
  stringsAsFactors = FALSE
)
count_itt <- prepare_mixed_rm_anova_results(
  count_data,
  group_variable = "group",
  repeated_variables = c("pre", "post", "post2"),
  variable_info = count_info,
  options = list(assumption_check = TRUE, posthoc = FALSE, analysis_population = "itt", time_labels = c("Pre", "Post", "Post 2"))
)
stopifnot(is.data.frame(count_itt$mixed_model_overview), any(grepl("Generalized linear mixed model", count_itt$mixed_model_overview$Value, fixed = TRUE)))
stopifnot(is.data.frame(count_itt$mixed_model_coefficients), "exp(B)" %in% names(count_itt$mixed_model_coefficients))

multi_setup_state <- mixed_rm_anova_setup_state(
  selected_names = names(data),
  group_variable = c("group", "sex"),
  repeated_variables = c("pre", "post", "post2"),
  variable_table = variable_info,
  language = "en"
)
stopifnot(identical(multi_setup_state$group_variable, c("group", "sex")))

estimated_between <- prepare_mixed_rm_anova_results(
  data,
  group_variable = "group",
  repeated_variables = c("pre", "post", "post2"),
  covariates = c("age", "sex"),
  variable_info = variable_info,
  options = list(assumption_check = TRUE, posthoc = TRUE, between_time_group_comparison = TRUE, time_labels = c("Pre", "Post", "Post 2"))
)
stopifnot(nrow(estimated_between$descriptives) == 3L)
stopifnot(nrow(estimated_between$observed_descriptives) == 3L, nrow(estimated_between$adjusted_descriptives) == 3L)
stopifnot(grepl("simple mean differences", estimated_between$observed_descriptives_note, fixed = TRUE))
stopifnot(grepl("estimated marginal mean differences", estimated_between$adjusted_descriptives_note, fixed = TRUE))
stopifnot(grepl("estimated marginal mean differences", estimated_between$descriptives_note, fixed = TRUE))

ui <- mixed_rm_anova_results_ui(result)
html <- paste(htmltools::renderTags(ui)$html, collapse = "\n")
stopifnot(grepl("Repeated-measures ANOVA", html, fixed = TRUE))
stopifnot(grepl("Group x Time", html, fixed = TRUE))
stopifnot(grepl("Group x time summary", html, fixed = TRUE))
stopifnot(grepl("Normality review", html, fixed = TRUE))
stopifnot(regexpr("Group x time summary", html, fixed = TRUE)[[1]] < regexpr("Repeated-measures ANOVA", html, fixed = TRUE)[[1]])
stopifnot(grepl("Post-hoc comparisons", html, fixed = TRUE))
stopifnot(regexpr("Repeated-measures ANOVA", html, fixed = TRUE)[[1]] < regexpr("Post-hoc comparisons", html, fixed = TRUE)[[1]])
stopifnot(grepl("coefficient-header-break", html, fixed = TRUE))
stopifnot(grepl("<span>epsilon</span>", html, fixed = TRUE), grepl("<span>(GG)</span>", html, fixed = TRUE), grepl("<span>(HF)</span>", html, fixed = TRUE))
stopifnot(grepl("vertical-align:super;\">a</sup>", html, fixed = TRUE), grepl("vertical-align:super;\">b</sup>", html, fixed = TRUE), grepl("vertical-align:super;\">c</sup>", html, fixed = TRUE))
stopifnot(grepl("coefficient-footnote-marker", html, fixed = TRUE), grepl("partial \u03b7\u00b2", html, fixed = TRUE))

tmp <- tempfile(fileext = ".xlsx")
save_mixed_rm_anova_excel_file(result, tmp)
stopifnot(file.exists(tmp), file.info(tmp)$size > 0)

message("Mixed repeated-measures ANOVA validation passed.")
