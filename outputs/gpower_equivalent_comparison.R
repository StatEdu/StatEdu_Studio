source("R/utils.R")
source("R/sample_size.R")

num1 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (!length(x) || !is.finite(x[[1]])) return(NA_real_)
  x[[1]]
}

add_row <- function(method, gpower_test, condition, app_n, gp_n, unit, note = "", gp_rounded_n = NULL) {
  app_n <- num1(app_n)
  gp_n <- num1(gp_n)
  gp_rounded_n <- if (is.null(gp_rounded_n)) ceiling(gp_n) else num1(gp_rounded_n)
  diff <- if (is.finite(app_n) && is.finite(gp_rounded_n)) app_n - gp_rounded_n else NA_real_
  pct <- if (is.finite(diff) && gp_rounded_n != 0) 100 * diff / gp_rounded_n else NA_real_
  data.frame(
    Method = method,
    GPower_Test = gpower_test,
    Condition = condition,
    Unit = unit,
    App_N = round(app_n, 3),
    GPower_Equivalent_Raw_N = round(gp_n, 3),
    GPower_Rounded_N = round(gp_rounded_n, 3),
    Difference = round(diff, 3),
    Percent_Diff = round(pct, 3),
    Match = if (!is.finite(diff)) "not checked" else if (diff == 0) "yes" else "review",
    Note = note,
    stringsAsFactors = FALSE
  )
}

alpha <- 0.05
power <- 0.80
rows <- list()

ancova_reference_total <- function(groups, effect_size, covariates, covariate_r2, alpha, power) {
  df1 <- groups - 1
  adjusted_f <- effect_size / sqrt(1 - covariate_r2)
  power_for_total <- function(total_n) {
    df2 <- total_n - groups - covariates
    if (df2 <= 0) return(0)
    fcrit <- stats::qf(1 - alpha, df1, df2)
    stats::pf(fcrit, df1, df2, ncp = total_n * adjusted_f^2, lower.tail = FALSE)
  }
  stats::uniroot(
    function(total_n) power_for_total(total_n) - power,
    c(groups + covariates + 2, 1e6)
  )$root
}

rows[[length(rows) + 1]] <- add_row(
  "t-test",
  "Means: Difference between two independent means",
  "two-sided, d=0.50, alpha=.05, power=.80, allocation 1:1",
  sample_size_ttest("sample_size", "two_sample", 0.50, alpha, power)$group1,
  stats::power.t.test(delta = 0.50, sd = 1, sig.level = alpha, power = power, type = "two.sample")$n,
  "per group",
  "G*Power reports fractional n; app rounds up."
)

rows[[length(rows) + 1]] <- add_row(
  "Paired t-test",
  "Means: Difference between two dependent means",
  "two-sided, dz=0.50, alpha=.05, power=.80",
  sample_size_ttest("sample_size", "paired", 0.50, alpha, power)$total,
  stats::power.t.test(delta = 0.50, sd = 1, sig.level = alpha, power = power, type = "paired")$n,
  "pairs",
  "G*Power reports fractional n; app rounds up."
)

rows[[length(rows) + 1]] <- add_row(
  "One-sample t-test",
  "Means: Difference from constant",
  "two-sided, d=0.50, alpha=.05, power=.80",
  sample_size_ttest("sample_size", "one_sample", 0.50, alpha, power)$total,
  stats::power.t.test(delta = 0.50, sd = 1, sig.level = alpha, power = power, type = "one.sample")$n,
  "participants",
  "G*Power reports fractional n; app rounds up."
)

rows[[length(rows) + 1]] <- add_row(
  "ANOVA",
  "F tests: ANOVA fixed effects, omnibus, one-way",
  "3 groups, f=0.25, alpha=.05, power=.80",
  sample_size_anova("sample_size", "one_way", groups = 3, effect_size = 0.25, alpha = alpha, power = power)$total,
  pwr::pwr.anova.test(k = 3, f = 0.25, sig.level = alpha, power = power)$n * 3,
  "total N",
  "G*Power/pwr returns per-group fractional n; G*Power-style rounded total is ceiling(per-group n) * groups.",
  ceiling(pwr::pwr.anova.test(k = 3, f = 0.25, sig.level = alpha, power = power)$n) * 3
)

rows[[length(rows) + 1]] <- add_row(
  "Chi-square",
  "Chi-square tests: Goodness-of-fit / contingency tables",
  "df=3, w=0.30, alpha=.05, power=.80",
  sample_size_chisquare("sample_size", df = 3, effect_size = 0.30, alpha = alpha, power = power)$total,
  pwr::pwr.chisq.test(w = 0.30, df = 3, sig.level = alpha, power = power)$N,
  "total N",
  "G*Power reports fractional N; app rounds up."
)

rows[[length(rows) + 1]] <- add_row(
  "Correlation",
  "Exact/Correlation: Bivariate normal model",
  "two-sided, rho=0.30, alpha=.05, power=.80",
  sample_size_correlation("sample_size", r = 0.30, alpha = alpha, power = power)$total,
  pwr::pwr.r.test(r = 0.30, sig.level = alpha, power = power)$n,
  "participants",
  "Both use Fisher-z style approximation; app rounds up."
)

rows[[length(rows) + 1]] <- add_row(
  "Linear regression",
  "F tests: Linear multiple regression, R2 deviation from zero",
  "overall R2-style f2=0.10, predictors=5",
  sample_size_regression("sample_size", "multiple", effect_size = 0.10, predictors = 5, alpha = alpha, power = power)$total,
  pwr::pwr.f2.test(u = 5, f2 = 0.10, sig.level = alpha, power = power)$v + 5 + 1,
  "total N",
  "Matches G*Power omnibus multiple regression setup; app rounds up."
)

rows[[length(rows) + 1]] <- add_row(
  "Two proportions",
  "z tests: Difference between two independent proportions",
  "two-sided, p1=.50, p2=.65, alpha=.05, power=.80",
  sample_size_proportion("sample_size", "two_proportion", 0.50, 0.65, alpha, power)$group1,
  pwr::pwr.2p.test(h = pwr::ES.h(0.50, 0.65), sig.level = alpha, power = power)$n,
  "per group",
  "Matches the G*Power-equivalent rounded per-group result under the app rounding rule."
)

rows[[length(rows) + 1]] <- add_row(
  "One proportion",
  "z tests: Difference from constant",
  "two-sided, p=.65 vs .50, alpha=.05, power=.80",
  sample_size_proportion("sample_size", "one_proportion", p1 = 0.65, alpha = alpha, power = power)$total,
  ((stats::qnorm(1 - alpha / 2) + stats::qnorm(power))^2 * 0.65 * (1 - 0.65)) / (0.65 - 0.50)^2,
  "participants",
  "G*Power z-test style normal approximation; app rounds up."
)

rows[[length(rows) + 1]] <- add_row(
  "ANCOVA",
  "F tests: ANCOVA fixed effects",
  "2 groups, f=.25, covariates=1, covariate R2=.30",
  sample_size_ancova("sample_size", "ancova", groups = 2, effect_size = 0.25, covariates = 1, covariate_r2 = 0.30, alpha = alpha, power = power)$total,
  ancova_reference_total(groups = 2, effect_size = 0.25, covariates = 1, covariate_r2 = 0.30, alpha = alpha, power = power),
  "total N",
  "ANCOVA noncentral F reference with lambda = adjusted f^2 * N and df2 = N - groups - covariates; app rounds to balanced groups."
)

out <- do.call(rbind, rows)
write.csv(out, "outputs/gpower_equivalent_comparison.csv", row.names = FALSE, fileEncoding = "UTF-8")
print(out)
