invisible(try(Sys.setlocale("LC_CTYPE", "English_United States.utf8"), silent = TRUE))

source(file.path("R", "analysis_meta.R"), encoding = "UTF-8")

make_effect <- function(id, g_value, se_value, row_id, region, mean_age, year) {
  meta_normalize_effect(
    list(
      included = TRUE,
      study_id = id,
      study_name = paste(id, "et al."),
      publication_year = year,
      outcome = "Outcome",
      predictor = if (row_id %% 2L) "Treatment A" else "Treatment B",
      moderator_categorical = paste0("region=", region),
      moderator_continuous = paste0("mean_age=", mean_age),
      family = "g",
      input_type = "g_se",
      direction = "positive",
      g = g_value,
      se = se_value
    ),
    row_id = row_id
  )
}

effects <- do.call(rbind, list(
  make_effect("Study A", 0.20, 0.10, 1L, "Asia", 30, 2020),
  make_effect("Study B", 0.55, 0.15, 2L, "Europe", 35, 2021),
  make_effect("Study C", 0.95, 0.20, 3L, "Asia", 40, 2022),
  make_effect("Study D", 0.35, 0.12, 4L, "Europe", 45, 2023)
))

yi <- effects$yi
vi <- effects$vi
weights <- 1 / vi
expected_fixed <- sum(weights * yi) / sum(weights)
expected_q <- sum(weights * (yi - expected_fixed)^2)
expected_dl <- max(0, (expected_q - (length(yi) - 1)) / (sum(weights) - sum(weights^2) / sum(weights)))

fixed <- meta_fit_model(effects, "g", model = "fixed", conf_level = 0.95)
stopifnot(inherits(fixed, "statedu_meta_model"))
stopifnot(abs(fixed$estimate_analysis - expected_fixed) < 1e-12)
stopifnot(abs(fixed$q - expected_q) < 1e-12)
stopifnot(fixed$tau2 == 0)
stopifnot(abs(sum(fixed$studies$weight) - 100) < 1e-10)

dl <- meta_fit_model(effects, "g", model = "random", tau_method = "DL")
stopifnot(abs(dl$tau2 - expected_dl) < 1e-10)
stopifnot(all(is.finite(dl$prediction)), dl$prediction[[1]] < dl$estimate, dl$prediction[[2]] > dl$estimate)

pm <- meta_fit_model(effects, "g", model = "random", tau_method = "PM")
pm_weights <- 1 / (vi + pm$tau2)
pm_mean <- sum(pm_weights * yi) / sum(pm_weights)
pm_q <- sum(pm_weights * (yi - pm_mean)^2)
stopifnot(pm$tau2 > 0, abs(pm_q - (length(yi) - 1)) < 1e-6)

reml <- meta_fit_model(effects, "g", model = "random", tau_method = "REML")
stopifnot(is.finite(reml$tau2), reml$tau2 >= 0)
stopifnot(reml$ci[[1]] < reml$estimate, reml$ci[[2]] > reml$estimate)

stopifnot(abs(meta_transform_effect(atanh(0.4), "r") - 0.4) < 1e-12)
stopifnot(abs(meta_transform_effect(log(2.5), "or") - 2.5) < 1e-12)
main_summary <- meta_model_summary_table(reml, "en")
stopifnot(nrow(main_summary) == 1L, ncol(main_summary) == 5L)
stopifnot(identical(names(main_summary), c("Model", "k", "Hedges' g (95% CI)", "p", "95% PI")))
stopifnot(main_summary$Model[[1]] == "Random (REML)")
stopifnot(meta_format_p_value(0.0002) == "<.001", meta_format_p_value(0.027) == ".027")
stopifnot(nrow(meta_heterogeneity_table(reml, "ko")) == 1L)
stopifnot(nrow(meta_study_results_table(reml, "ko")) == nrow(effects))

catalog <- meta_moderator_catalog(effects, "g")
stopifnot(all(c("builtin::publication_year", "categorical::region", "continuous::mean_age") %in% catalog$key))

categorical_moderator <- meta_fit_moderator(reml, "categorical::region")
stopifnot(inherits(categorical_moderator, "statedu_meta_moderator"))
stopifnot(categorical_moderator$type == "categorical", nrow(categorical_moderator$subgroups) == 2L)
stopifnot(sum(categorical_moderator$subgroups$k) == nrow(effects))
stopifnot(nrow(meta_moderator_test_table(categorical_moderator, "ko")) == 1L)
stopifnot(nrow(meta_subgroup_results_table(categorical_moderator, "g", "ko")) == 2L)

continuous_moderator <- meta_fit_moderator(reml, "continuous::mean_age")
stopifnot(continuous_moderator$type == "continuous", abs(continuous_moderator$center - mean(c(30, 35, 40, 45))) < 1e-12)
stopifnot(nrow(meta_moderator_coefficient_table(continuous_moderator, "ko")) == 2L)

year_moderator <- meta_fit_moderator(reml, "builtin::publication_year")
stopifnot(year_moderator$name == "publication_year", year_moderator$k == nrow(effects))

fixed_continuous <- meta_fit_moderator(fixed, "continuous::mean_age")
manual_design <- cbind(1, c(30, 35, 40, 45) - mean(c(30, 35, 40, 45)))
manual_coefficients <- solve(crossprod(manual_design, manual_design * weights), crossprod(manual_design, yi * weights))
stopifnot(max(abs(fixed_continuous$coefficients$estimate - as.vector(manual_coefficients))) < 1e-10)

dependent_effects <- rbind(
  effects,
  make_effect("Study A", 0.40, 0.11, 5L, "Asia", 30, 2020),
  make_effect("Study B", 0.70, 0.16, 6L, "Europe", 35, 2021)
)
dependent_fit <- meta_fit_model(dependent_effects, "g", model = "random", tau_method = "REML")
dependency <- meta_dependency_summary(dependent_fit$rows)
stopifnot(isTRUE(dependency$dependent), dependency$studies == 4L, dependency$effects == 6L)

three_level <- meta_fit_three_level(dependent_fit)
stopifnot(three_level$method == "three_level", three_level$studies == 4L)
stopifnot(all(is.finite(c(three_level$estimate, three_level$standard_error, three_level$sigma2_within, three_level$sigma2_between))))

rve <- meta_fit_rve(dependent_fit)
stopifnot(rve$method == "rve_cr2", rve$studies == 4L, is.finite(rve$df), rve$df > 0)
stopifnot(all(is.finite(c(rve$estimate, rve$standard_error, rve$p_value))))
stopifnot(is.matrix(rve$covariance), nrow(rve$coefficients) == 1L)
rve_models <- meta_fit_rve_models(dependent_fit, compare = TRUE)
stopifnot(identical(names(rve_models), c("rve_cr1", "rve_cr2", "rve_cr3")))
stopifnot(identical(unname(vapply(rve_models, `[[`, character(1), "correction")), c("CR1", "CR2", "CR3")))
stopifnot(all(vapply(rve_models, function(model) all(is.finite(c(model$standard_error, model$df, model$p_value))), logical(1))))

dependent_moderator <- meta_fit_moderator(dependent_fit, "continuous::mean_age")
stopifnot(is.list(dependent_moderator$cr2), dependent_moderator$cr2$method == "rve_cr2")
stopifnot(nrow(dependent_moderator$cr2$coefficients) == 2L)
stopifnot(all(is.finite(dependent_moderator$cr2$coefficients$df)))
stopifnot(nrow(meta_moderator_cr2_table(dependent_moderator, "ko")) == 2L)
dependent_moderator_comparison <- meta_fit_moderator(dependent_fit, "continuous::mean_age", rve_compare = TRUE)
stopifnot(identical(names(dependent_moderator_comparison$robust_models), c("CR1", "CR2", "CR3")))
stopifnot(nrow(meta_moderator_robust_comparison_table(dependent_moderator_comparison, "ko")) == 6L)

dependent_fit$dependency_models <- list(three_level = three_level, rve = rve)
stopifnot(nrow(meta_dependency_results_table(dependent_fit, "ko")) == 2L)
sensitivity <- meta_dependency_sensitivity(dependent_fit)
stopifnot(nrow(sensitivity) == 5L, all(is.finite(sensitivity$estimate)))
stopifnot(nrow(meta_dependency_sensitivity_table(sensitivity, "g", "ko")) == 5L)
leave_one_out <- meta_leave_one_study_out(dependent_fit)
stopifnot(nrow(leave_one_out) == 4L, all(is.finite(leave_one_out$estimate)))
stopifnot(nrow(meta_leave_one_study_out_table(leave_one_out, "g", "ko")) == 4L)

grouped <- meta_fit_grouped_models(dependent_effects, "g", "predictor", "random", "REML", 0.95, dependency_method = "independent")
stopifnot(inherits(grouped, "statedu_meta_grouped"), length(grouped$fits) == 2L)
stopifnot(nrow(meta_grouped_results_table(grouped, "g", "ko")) == 2L)
grouped_dependency <- meta_fit_grouped_models(dependent_effects, "g", "outcome", "random", "REML", 0.95, dependency_method = "auto")
stopifnot(length(grouped_dependency$fits[[1]]$dependency_models) == 2L)
stopifnot(nrow(meta_grouped_results_table(grouped_dependency, "g", "ko")) == 3L)
grouped_dependency_comparison <- meta_fit_grouped_models(dependent_effects, "g", "outcome", "random", "REML", 0.95, dependency_method = "auto", rve_compare = TRUE)
stopifnot(length(grouped_dependency_comparison$fits[[1]]$dependency_models) == 4L)
stopifnot(nrow(meta_grouped_results_table(grouped_dependency_comparison, "g", "ko")) == 5L)

trimfill <- meta_trimfill(dependent_fit, rho = 0.5)
stopifnot(trimfill$method == "L0", trimfill$k_observed == 4L, trimfill$k0 >= 0L)
stopifnot(is.finite(trimfill$adjusted_fit$estimate))
stopifnot(nrow(meta_trimfill_results_table(trimfill, "ko")) == 1L)

egger <- meta_egger_test(reml)
stopifnot(isTRUE(egger$available), egger$df == nrow(effects) - 2L)
stopifnot(all(is.finite(c(egger$intercept, egger$standard_error, egger$statistic, egger$p_value, egger$ci))))
stopifnot(nrow(meta_egger_results_table(egger, "ko")) == 1L)

plot_file <- tempfile(fileext = ".png")
grDevices::png(plot_file, width = 1100, height = 700, res = 110)
draw_meta_forest_plot(reml, "ko")
grDevices::dev.off()
stopifnot(file.exists(plot_file), file.info(plot_file)$size > 0)
unlink(plot_file)

funnel_file <- tempfile(fileext = ".png")
grDevices::png(funnel_file, width = 900, height = 700, res = 110)
draw_meta_funnel_plot(reml, "ko")
grDevices::dev.off()
stopifnot(file.exists(funnel_file), file.info(funnel_file)$size > 0)
unlink(funnel_file)

invalid <- effects
invalid$status[[1]] <- "error"
fit_error <- try(meta_fit_model(invalid, "g"), silent = TRUE)
stopifnot(inherits(fit_error, "try-error"))

message("Meta-analysis model validation passed.")
