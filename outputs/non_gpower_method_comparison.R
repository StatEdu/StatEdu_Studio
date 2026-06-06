source("R/utils.R")
source("R/sample_size.R")

num1 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (!length(x) || !is.finite(x[[1]])) return(NA_real_)
  x[[1]]
}

add_row <- function(method, scenario, unit, app_n, reference, ref_raw_n = NA_real_, ref_rounded_n = NULL, evidence = "package", note = "") {
  app_n <- num1(app_n)
  ref_raw_n <- num1(ref_raw_n)
  ref_rounded_n <- if (is.null(ref_rounded_n)) ceiling(ref_raw_n) else num1(ref_rounded_n)
  diff <- if (is.finite(app_n) && is.finite(ref_rounded_n)) app_n - ref_rounded_n else NA_real_
  rel <- if (is.finite(diff) && ref_rounded_n != 0) 100 * diff / ref_rounded_n else NA_real_
  verdict <- if (!is.finite(diff)) {
    "not directly comparable"
  } else if (diff == 0) {
    "match"
  } else if (abs(rel) <= 5) {
    "near"
  } else {
    "review"
  }
  data.frame(
    Method = method,
    Scenario = scenario,
    Unit = unit,
    App_N = round(app_n, 3),
    Reference = reference,
    Reference_Raw_N = round(ref_raw_n, 3),
    Reference_Rounded_N = round(ref_rounded_n, 3),
    Difference_vs_Rounded = round(diff, 3),
    Percent_Diff = round(rel, 3),
    Evidence = evidence,
    Verdict = verdict,
    Note = note,
    stringsAsFactors = FALSE
  )
}

alpha <- 0.05
power <- 0.80
rows <- list()

gee_app <- sample_size_gee(
  "sample_size",
  outcome = "continuous",
  effect_size = 0.5,
  time_points = 3,
  rho = 0.30,
  alpha = alpha,
  power = power
)
gee_de <- 1 + (3 - 1) * 0.30
gee_ref_raw <- stats::power.t.test(
  delta = 0.5 / sqrt(gee_de),
  sd = 1,
  sig.level = alpha,
  power = power,
  type = "two.sample"
)$n
gee_ref_rounded <- ceiling(stats::power.t.test(
  delta = 0.5,
  sd = 1,
  sig.level = alpha,
  power = power,
  type = "two.sample"
)$n) * gee_de
rows[[length(rows) + 1]] <- add_row(
  "GEE",
  "continuous, two groups, d=.50, 3 repeated time points, exchangeable rho=.30",
  "participants per group",
  gee_app$group1,
  "Independent t-test + repeated-measures design effect",
  gee_ref_raw,
  ceiling(gee_ref_rounded),
  evidence = "reference formula",
  note = "App multiplies rounded independent two-sample n by DE=1+(m-1)rho, then rounds again. This is a planning approximation, not a full GEE package calculation."
)

lmm_app <- sample_size_lmm(
  "sample_size",
  mode = "simple",
  design = "two_group_repeated",
  effect_size = 0.5,
  time_points = 3,
  icc = 0.30,
  simulations = 300,
  alpha = alpha,
  power = 0.95
)
lmm_ref <- longpower::diggle.linear.power(
  n = NULL,
  delta = 0.5,
  t = c(0, 0.5, 1),
  sigma2 = 1,
  R = matrix(c(1, .3, .3, .3, 1, .3, .3, .3, 1), 3),
  sig.level = alpha,
  power = 0.95
)$n[1]
rows[[length(rows) + 1]] <- add_row(
  "LMM",
  "two-group repeated slope/change, d=.50, 3 time points, ICC/correlation=.30, power=.95",
  "participants per group",
  lmm_app$group1,
  "longpower::diggle.linear.power",
  lmm_ref,
  evidence = "package",
  note = "App simple two-group LMM now uses longpower::diggle.linear.power; differences should be limited to rounding."
)

surv_app <- sample_size_survival(
  "sample_size",
  hazard_ratio = 0.70,
  event_probability = 0.40,
  alpha = alpha,
  power = power
)
surv_ref <- ((stats::qnorm(1 - alpha / 2) + stats::qnorm(power))^2 / ((1 / 4) * log(0.70)^2)) / 0.40
surv_ref_rounded <- ceiling(ceiling(surv_ref) * 0.5) * 2
rows[[length(rows) + 1]] <- add_row(
  "Survival / Cox",
  "HR=.70, overall event probability=.40, allocation 1:1",
  "total participants",
  surv_app$total,
  "Schoenfeld event-based formula",
  surv_ref,
  surv_ref_rounded,
  evidence = "reference formula",
  note = "App uses the Schoenfeld event-based approximation, then rounds allocation-specific group sizes and sums them."
)

eq_app <- sample_size_equivalence(
  "sample_size",
  outcome = "mean",
  objective = "equivalence",
  true_difference = 0,
  margin = 0.5,
  sd = 1,
  alpha = alpha,
  power = power
)
eq_ref <- TOSTER::power_t_TOST(
  n = NULL,
  delta = 0,
  sd = 1,
  eqb = 0.5,
  alpha = alpha,
  power = power,
  type = "two.sample"
)$n
rows[[length(rows) + 1]] <- add_row(
  "Equivalence / TOST",
  "two independent means, equivalence margin=.50 SD, true difference=0",
  "participants per group",
  eq_app$group1,
  "TOSTER::power_t_TOST",
  eq_ref,
  evidence = "package",
  note = "App mean equivalence now uses exact t-based TOSTER::power_t_TOST; differences should be limited to rounding."
)

diag_app <- sample_size_diagnostic(
  "sample_size",
  design = "sensitivity",
  sensitivity = 0.80,
  prevalence = 0.20,
  precision = 0.05,
  alpha = alpha
)
diag_ref <- epiR::epi.ssdxsesp(
  se = 0.80,
  sp = 0.90,
  Py = 0.20,
  epsilon = 0.05,
  error = "absolute",
  conf.level = 0.95
)$total.n[1]
rows[[length(rows) + 1]] <- add_row(
  "Diagnostic accuracy",
  "sensitivity=.80, specificity=.90, prevalence=.20, absolute precision=.05",
  "total participants",
  diag_app$total,
  "epiR::epi.ssdxsesp",
  diag_ref,
  evidence = "package",
  note = "epiR returns the maximum total N needed for sensitivity/specificity precision; this scenario is driven by sensitivity and matches the app sensitivity calculation."
)

auc_app <- sample_size_diagnostic(
  "sample_size",
  design = "auc",
  auc = 0.70,
  null_auc = 0.50,
  ratio = 1,
  alpha = alpha,
  power = power
)
rows[[length(rows) + 1]] <- add_row(
  "ROC AUC",
  "AUC=.70 vs null AUC=.50, case/control ratio=1",
  "cases",
  auc_app$group1,
  "No direct installed package comparator",
  NA_real_,
  evidence = "not available",
  note = "App uses Hanley-McNeil AUC variance. Installed packages did not expose a direct one-AUC sample-size API for this scenario."
)

rate_app <- sample_size_rates(
  "sample_size",
  design = "two_rate_ratio",
  rate1 = 1.0,
  rate2 = 1.5,
  alpha = alpha,
  power = power
)
rate_ref <- ((stats::qnorm(1 - alpha / 2) + stats::qnorm(power))^2 * (1.0 + 1.5)) / (1.5 - 1.0)^2
rows[[length(rows) + 1]] <- add_row(
  "Count / rates",
  "two Poisson rates, rate1=1.0, rate2=1.5, allocation 1:1",
  "person-time per group",
  rate_app$group1,
  "Wald two-rate formula",
  rate_ref,
  evidence = "reference formula",
  note = "This is the same Wald rate approximation implemented by the app."
)

cluster_app <- sample_size_cluster(
  "sample_size",
  design = "parallel",
  outcome = "continuous",
  effect_size = 0.5,
  cluster_size = 20,
  icc = 0.05,
  alpha = alpha,
  power = power
)
cluster_ref <- WebPower::wp.crt2arm(
  n = 20,
  f = 0.5,
  icc = 0.05,
  power = power,
  alpha = alpha
)$J
rows[[length(rows) + 1]] <- add_row(
  "Cluster trial",
  "parallel CRT, continuous d=.50, cluster size=20, ICC=.05",
  "clusters total",
  cluster_app$clusters_group1 + cluster_app$clusters_group2,
  "WebPower::wp.crt2arm",
  cluster_ref,
  ceiling(cluster_ref / 2) * 2,
  evidence = "package",
  note = "App continuous parallel CRT now uses WebPower::wp.crt2arm and rounds raw total clusters to balanced clusters per group."
)

precision_app <- sample_size_precision(
  "sample_size",
  parameter = "mean",
  confidence_level = 0.95,
  half_width = 0.20,
  sd = 1
)
precision_ref <- (stats::qnorm(0.975) * 1 / 0.20)^2
rows[[length(rows) + 1]] <- add_row(
  "Precision / CI",
  "mean CI half-width=.20, SD=1, confidence=.95",
  "participants",
  precision_app$total,
  "Normal CI precision formula",
  precision_ref,
  evidence = "reference formula",
  note = "This is the same normal-approximation CI half-width formula implemented by the app."
)

reliability_app <- sample_size_reliability(
  "sample_size",
  design = "icc",
  reliability = 0.70,
  items = 3,
  half_width = 0.10,
  confidence_level = 0.95
)
rows[[length(rows) + 1]] <- add_row(
  "Reliability / agreement",
  "ICC precision, expected ICC=.70, 3 raters, half-width=.10",
  "subjects",
  reliability_app$total,
  "No direct installed package comparator",
  NA_real_,
  evidence = "not available",
  note = "ICC.Sample.Size targets ICC hypothesis-test power, while the app currently calculates CI-width precision."
)

sem_app <- sample_size_sem(
  "sample_size",
  test = "close_fit",
  df_source = "direct",
  df = 50,
  null_rmsea = 0.05,
  alternative_rmsea = 0.08,
  alpha = alpha,
  power = power
)
sem_ref <- WebPower::wp.sem.rmsea(
  n = NULL,
  df = 50,
  rmsea0 = 0.05,
  rmsea1 = 0.08,
  alpha = alpha,
  power = power,
  type = "close"
)$n
rows[[length(rows) + 1]] <- add_row(
  "SEM / CFA",
  "RMSEA close-fit test, H0 RMSEA=.05, H1 RMSEA=.08, df=50",
  "participants",
  sem_app$total,
  "WebPower::wp.sem.rmsea",
  sem_ref,
  evidence = "package",
  note = "Both use MacCallum-style noncentral chi-square/RMSEA power; app rounds up."
)

out <- do.call(rbind, rows)
write.csv(out, "outputs/non_gpower_method_comparison.csv", row.names = FALSE, fileEncoding = "UTF-8")
print(out)
