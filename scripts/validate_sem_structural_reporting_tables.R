if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]])) {
  validation_locale <- Sys.setlocale("LC_ALL", "Korean_Korea.utf8")
  if (is.na(validation_locale) || !isTRUE(l10n_info()[["UTF-8"]])) {
    stop("SEM reporting validation requires a Windows UTF-8 locale.")
  }
}

source(file.path("R", "utils.R"), encoding = "UTF-8")
suppressPackageStartupMessages(library(shiny))
source(file.path("R", "setup_custom_model_canvas_structural_core.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_bootstrap.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_tables.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_render_tables.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_identification_diagnostics.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_export_report.R"), encoding = "UTF-8")

if (!requireNamespace("lavaan", quietly = TRUE)) stop("lavaan is required for SEM reporting validation.")

duplicate_label_snapshot <- list(nodes = list(
  list(id = "eta_a", role = "latent", name = "etaA", canvasLabel = "Shared construct"),
  list(id = "eta_b", role = "latent", name = "etaB", canvasLabel = "Shared construct"),
  list(id = "eta_c", role = "latent", name = "etaC", canvasLabel = "Unique construct")
))
duplicate_label_name <- structural_canvas_display_name_resolver(
  snapshot = duplicate_label_snapshot,
  language = "en"
)
stopifnot(
  identical(
    duplicate_label_name(c("etaA", "etaB", "etaC")),
    c("Shared construct [etaA]", "Shared construct [etaB]", "Unique construct")
  ),
  identical(
    structural_canvas_display_path(c("etaA -> etaC", "etaB -> etaC"), duplicate_label_name),
    c("Shared construct [etaA] → Unique construct", "Shared construct [etaB] → Unique construct")
  )
)

set.seed(20260825)
n <- 240L
x <- stats::rnorm(n)
m <- .55 * x + stats::rnorm(n, sd = .75)
y <- .35 * x + .45 * m + stats::rnorm(n, sd = .70)
fit <- lavaan::sem(
  "m ~ a*x
   y ~ b*m + c*x
   sp := a*b
   ind := a*b
   tot := c + a*b",
  data = data.frame(x = x, m = m, y = y)
)
stopifnot(lavaan::lavInspect(fit, "converged"))

fmt <- function(value) vapply(as.numeric(value), format_decimal3, character(1))
identity_name <- function(value) as.character(value)

parameters <- lavaan::parameterEstimates(fit)
targets <- parameters[parameters$op %in% c("~", ":="), c("lhs", "op", "rhs", "est"), drop = FALSE]
fake_bootstrap <- data.frame(
  lhs = targets$lhs,
  op = targets$op,
  rhs = targets$rhs,
  estimate = targets$est,
  se = rep(.100, nrow(targets)),
  lower = targets$est - .20,
  upper = targets$est + .20,
  p = c(.010, .020, .030, .040, .050, .060),
  beta_estimate = targets$est,
  beta_se = rep(.110, nrow(targets)),
  beta_lower = targets$est - .22,
  beta_upper = targets$est + .22,
  beta_p = c(.010, .020, .030, .040, .050, .060),
  beta_valid = c(90L, 60L, 40L, 90L, 90L, 90L),
  beta_status = "Estimated",
  valid = c(90L, 60L, 40L, 90L, 90L, 90L),
  requested = 100L,
  valid_percent = c(90, 60, 40, 90, 90, 90),
  ci_method = "bias_corrected",
  quantile_type = 6L,
  status = c("Adequate", "Caution", "Unreliable", "Adequate", "Adequate", "Adequate"),
  stringsAsFactors = FALSE
)

direct <- structural_canvas_lavaan_structural_result_table(
  "structural", fit, FALSE, fmt, identity_name, bootstrap = fake_bootstrap
)
stopifnot(nrow(direct) == 3L)
stopifnot(identical(direct[["Bootstrap status"]], c("Adequate", "Caution", "Unreliable")))
stopifnot(identical(direct[["Valid bootstrap"]], c("90/100 (90.0%)", "60/100 (60.0%)", "40/100 (40.0%)")))
stopifnot(nzchar(direct$SE[[1L]]), nzchar(direct$p[[1L]]), nzchar(direct[["B 95% CI lower"]][[1L]]))
stopifnot(nzchar(direct$SE[[2L]]), nzchar(direct$p[[2L]]), nzchar(direct[["B 95% CI lower"]][[2L]]))
stopifnot(all(nzchar(direct$z[1:2])), !nzchar(direct$z[[3L]]))
stopifnot(!nzchar(direct$SE[[3L]]), !nzchar(direct$z[[3L]]), !nzchar(direct$p[[3L]]))
stopifnot(!nzchar(direct[["B 95% CI lower"]][[3L]]), !nzchar(direct[["B 95% CI upper"]][[3L]]))
stopifnot(is.na(direct$p_numeric[[3L]]))
stopifnot(grepl("valid 40/100 (40.0%); status Unreliable", direct[["beta CI source"]][[3L]], fixed = TRUE))

effect_definitions <- list(
  list(label = "sp", type = "Specific indirect", outcome = "y", predictor = "x", path = c("x", "m", "y")),
  list(label = "ind", type = "Indirect", outcome = "y", predictor = "x", paths = list(c("x", "m", "y"))),
  list(label = "tot", type = "Total", outcome = "y", predictor = "x", paths = list(c("x", "y"), c("x", "m", "y")))
)
effect_rows <- structural_canvas_lavaan_structural_effect_rows(
  fit, effect_definitions, fmt, identity_name, bootstrap = fake_bootstrap
)
stopifnot(all(nzchar(effect_rows$z)))
stopifnot(all(grepl("valid standardized bootstrap", effect_rows[["beta CI source"]], fixed = TRUE)))
names(effect_rows) <- names(direct)
combined <- structural_canvas_apply_effect_bh_families(rbind(direct, effect_rows))
stopifnot(identical(unique(combined[["BH family"]][combined$Effect == "Direct"]), "Direct structural paths"))
stopifnot(identical(unique(combined[["BH family"]][combined$Effect == "Specific indirect"]), "Specific indirect effects"))
stopifnot(identical(unique(combined[["BH family"]][combined$Effect %in% c("Indirect", "Total")]), "Other indirect and total effects"))
stopifnot(!nzchar(combined[["BH-adjusted p"]][combined$Effect == "Direct"][[3L]]))

summary_table <- structural_canvas_effect_summary_table(combined, ci = FALSE)
stopifnot(all(c(
  "Outcome", "Predictor", "Effect", "B", "Boot SE", "B 95% CI", "beta", "p", "BH-adjusted p",
  "CI source", "Inference source", "Valid bootstrap", "Bootstrap status", "BH family"
) %in% names(summary_table)))
stopifnot(any(summary_table$Effect == "Indirect"), any(summary_table$Effect == "Total"))

specific_boot <- structural_canvas_specific_indirect_table(combined)
stopifnot(all(c("Boot SE", "Boot 95% CI lower", "Boot 95% CI upper", "BH-adjusted p", "CI source", "Valid bootstrap", "Bootstrap status") %in% names(specific_boot)))
stopifnot(nzchar(specific_boot[["BH-adjusted p"]][[1L]]))

model_effect_rows <- structural_canvas_lavaan_structural_effect_rows(
  fit, effect_definitions, fmt, identity_name, bootstrap = NULL
)
model_effect_rows <- structural_canvas_apply_effect_bh_families(model_effect_rows)
specific_model <- structural_canvas_specific_indirect_table(model_effect_rows)
stopifnot(all(c("SE", "B 95% CI lower", "B 95% CI upper") %in% names(specific_model)))
stopifnot(!any(c("Boot SE", "Boot 95% CI lower", "Boot 95% CI upper") %in% names(specific_model)))
stopifnot(identical(specific_model[["CI source"]][[1L]], "Model-based 95% CI"))
stopifnot(identical(specific_model[["Bootstrap status"]][[1L]], "Not requested"))

specific_html <- htmltools::renderTags(structural_canvas_specific_indirect_html_table(specific_model))$html
specific_doc <- xml2::read_html(specific_html, encoding = "UTF-8")
specific_headers <- xml2::xml_text(xml2::xml_find_all(specific_doc, "//th"))
stopifnot(
  identical(specific_headers, c("Path", "B", "SE", "95% CI", "beta", "z", "p", "BH p", "LLCI", "ULCI")),
  length(xml2::xml_find_all(specific_doc, "//th[@colspan='2' and normalize-space(.)='95% CI']")) == 1L,
  identical(trimws(xml2::xml_text(xml2::xml_find_all(specific_doc, "//tbody/tr/td[4]"))), as.character(specific_model[["B 95% CI lower"]])),
  identical(trimws(xml2::xml_text(xml2::xml_find_all(specific_doc, "//tbody/tr/td[5]"))), as.character(specific_model[["B 95% CI upper"]]))
)

measurement_probe <- data.frame(
  Latent = "eta", Indicator = "x", B = ".700", SE = ".050", beta = ".720", z = "14.000", p = "<.001", `R²` = ".518",
  check.names = FALSE
)
measurement_html <- htmltools::renderTags(structural_canvas_measurement_html_table(measurement_probe))$html
stopifnot(grepl("Std. loading", measurement_html, fixed = TRUE), grepl("&lambda;", measurement_html, fixed = TRUE))

effect_ci_probe <- data.frame(
  Outcome = c("y", "z"),
  Predictor = c("x", "x"),
  `Direct beta 95% CI` = c(".100 ~ .300", ".200 ~ .400"),
  `Direct CI source` = c(
    "Model-based 95% CI",
    "Bootstrap bias-corrected and accelerated (BCa) 95% CI (R quantile type 6); valid standardized bootstrap 60/100 (60.0%); status Caution"
  ),
  `Indirect beta 95% CI` = c(".050 ~ .200", ".020 ~ .180"),
  `Indirect CI source` = c(
    "Bootstrap bias-corrected (BC) 95% CI (R quantile type 6)",
    "Bootstrap percentile 95% CI (R quantile type 7); valid standardized bootstrap 90/100 (90.0%); status Adequate"
  ),
  `Total beta 95% CI` = c("", ""),
  `Total CI source` = c(
    "Not estimated - insufficient valid standardized bootstrap replicates",
    "Not estimated - insufficient valid standardized bootstrap replicates; valid 40/100 (40.0%); status Unreliable"
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
effect_ci_ko_html <- htmltools::renderTags(structural_canvas_effect_summary_html_table(effect_ci_probe, ci = TRUE, language = "ko"))$html
effect_ci_en_html <- htmltools::renderTags(structural_canvas_effect_summary_html_table(effect_ci_probe, ci = TRUE, language = "en"))$html
effect_ci_ko_note <- htmltools::renderTags(structural_canvas_effect_ci_source_note(effect_ci_probe, language = "ko"))$html
stopifnot(
  grepl("직접효과", effect_ci_ko_html, fixed = TRUE),
  grepl("간접효과", effect_ci_ko_html, fixed = TRUE),
  grepl("총효과", effect_ci_ko_html, fixed = TRUE),
  grepl("모형기반 95% CI", effect_ci_ko_html, fixed = TRUE),
  grepl("부트스트랩 편향보정(BC) 95% CI (R 분위수 유형 6)", effect_ci_ko_html, fixed = TRUE),
  grepl("부트스트랩 편향보정·가속(BCa) 95% CI (R 분위수 유형 6); 유효 표준화 부트스트랩 60/100 (60.0%); 상태 주의", effect_ci_ko_html, fixed = TRUE),
  grepl("부트스트랩 백분위수 95% CI (R 분위수 유형 7); 유효 표준화 부트스트랩 90/100 (90.0%); 상태 충분", effect_ci_ko_html, fixed = TRUE),
  grepl("미산출 - 유효 표준화 부트스트랩 반복 부족; 유효 40/100 (40.0%); 상태 신뢰 불가", effect_ci_ko_html, fixed = TRUE),
  grepl("신뢰구간 산출 근거", effect_ci_ko_note, fixed = TRUE),
  grepl("모형기반 95% CI", effect_ci_ko_note, fixed = TRUE),
  grepl("Direct effect", effect_ci_en_html, fixed = TRUE),
  grepl("Model-based 95% CI", effect_ci_en_html, fixed = TRUE),
  grepl("Bootstrap percentile 95% CI", effect_ci_en_html, fixed = TRUE),
  identical(effect_ci_probe[["Direct CI source"]][[1L]], "Model-based 95% CI"),
  identical(
    effect_ci_probe[["Direct CI source"]][[2L]],
    "Bootstrap bias-corrected and accelerated (BCa) 95% CI (R quantile type 6); valid standardized bootstrap 60/100 (60.0%); status Caution"
  )
)

stopifnot(
  !structural_canvas_bootstrap_inference_usable(49L, 100L),
  structural_canvas_bootstrap_inference_usable(50L, 100L),
  identical(structural_canvas_bootstrap_status(50L, 100L), "Caution"),
  identical(structural_canvas_bootstrap_status(80L, 100L), "Adequate")
)
fake_bca <- fake_bootstrap[1L, , drop = FALSE]
fake_bca$ci_method <- "bca"
stopifnot(grepl("BCa", structural_canvas_effect_bootstrap_ci_label(fake_bca), fixed = TRUE))

fixed_fit <- lavaan::sem("y ~ 0.4*x", data = data.frame(x = x, y = y))
fixed_parameter <- lavaan::parameterEstimates(fixed_fit)
fixed_parameter <- fixed_parameter[fixed_parameter$op == "~", , drop = FALSE]
fixed_bootstrap <- data.frame(
  lhs = fixed_parameter$lhs, op = fixed_parameter$op, rhs = fixed_parameter$rhs,
  estimate = fixed_parameter$est, se = 0, lower = fixed_parameter$est,
  upper = fixed_parameter$est, p = .001, beta_estimate = fixed_parameter$est,
  beta_se = 0, beta_lower = fixed_parameter$est, beta_upper = fixed_parameter$est,
  beta_p = .001, beta_valid = 100L, beta_status = "Estimated",
  valid = 100L, requested = 100L, valid_percent = 100,
  ci_method = "percentile", quantile_type = 6L, status = "Adequate",
  stringsAsFactors = FALSE
)
fixed_direct <- structural_canvas_lavaan_structural_result_table(
  "structural", fixed_fit, FALSE, fmt, identity_name, bootstrap = fixed_bootstrap
)
fixed_direct <- structural_canvas_apply_effect_bh_families(fixed_direct)
stopifnot(
  isTRUE(all.equal(as.numeric(fixed_direct$B[[1L]]), .4, tolerance = 1e-12)),
  all(!nzchar(unlist(fixed_direct[1L, c("SE", "B 95% CI lower", "B 95% CI upper", "z", "p", "BH-adjusted p")], use.names = FALSE))),
  identical(fixed_direct[["Inference source"]][[1L]], "Fixed parameter - no inferential test"),
  identical(fixed_direct[["B CI source"]][[1L]], "Not applicable - fixed parameter"),
  identical(fixed_direct[["BH family"]][[1L]], "Fixed parameter (not tested)")
)
localized_fixed <- structural_canvas_localize_reporting_metadata(fixed_direct, "ko")
stopifnot(
  localized_fixed[["추론 산출 근거"]][[1L]] == "고정모수 - 추론검정 없음",
  localized_fixed[["B CI 산출 근거"]][[1L]] == "해당 없음 - 고정모수",
  localized_fixed[["부트스트랩 상태"]][[1L]] == "해당 없음 - 고정모수",
  localized_fixed[["BH 검정군"]][[1L]] == "고정모수(검정 제외)"
)

effect_test_data <- data.frame(x = x, m = m, y = y)
all_fixed_effect_fit <- lavaan::sem(
  "m ~ 0.5*x
   y ~ 0.4*m + 0.3*x
   fixed_sp := 0.5*0.4
   fixed_ind := 0.5*0.4
   fixed_tot := 0.3 + 0.5*0.4",
  data = effect_test_data
)
all_fixed_effect_definitions <- list(
  list(
    label = "fixed_sp", type = "Specific indirect", predictor = "x", outcome = "y",
    paths = list(c("x", "m", "y")), path_labels = list(c("0.5", "0.4")), path = c("x", "m", "y")
  ),
  list(
    label = "fixed_ind", type = "Indirect", predictor = "x", outcome = "y",
    paths = list(c("x", "m", "y")), path_labels = list(c("0.5", "0.4"))
  ),
  list(
    label = "fixed_tot", type = "Total", predictor = "x", outcome = "y",
    paths = list(c("x", "y"), c("x", "m", "y"))
  )
)
all_fixed_parameters <- lavaan::parameterEstimates(all_fixed_effect_fit)
all_fixed_parameters <- all_fixed_parameters[all_fixed_parameters$op == ":=", c("lhs", "op", "rhs", "est"), drop = FALSE]
all_fixed_bootstrap <- data.frame(
  lhs = all_fixed_parameters$lhs,
  op = all_fixed_parameters$op,
  rhs = all_fixed_parameters$rhs,
  estimate = all_fixed_parameters$est,
  se = rep(.001, nrow(all_fixed_parameters)),
  lower = all_fixed_parameters$est - .002,
  upper = all_fixed_parameters$est + .002,
  p = rep(2 / 101, nrow(all_fixed_parameters)),
  beta_estimate = all_fixed_parameters$est,
  beta_se = rep(.001, nrow(all_fixed_parameters)),
  beta_lower = all_fixed_parameters$est - .002,
  beta_upper = all_fixed_parameters$est + .002,
  beta_p = rep(2 / 101, nrow(all_fixed_parameters)),
  beta_valid = 100L,
  beta_status = "Estimated",
  valid = 100L,
  requested = 100L,
  valid_percent = 100,
  ci_method = "percentile",
  quantile_type = 6L,
  status = "Adequate",
  stringsAsFactors = FALSE
)
all_fixed_effect_rows <- structural_canvas_lavaan_structural_effect_rows(
  all_fixed_effect_fit, all_fixed_effect_definitions, fmt, identity_name, bootstrap = all_fixed_bootstrap
)
all_fixed_blank_columns <- c(
  "SE", "B 95% CI lower", "B 95% CI upper", "beta 95% CI lower", "beta 95% CI upper", "z", "p", "BH-adjusted p"
)
stopifnot(
  nrow(all_fixed_effect_rows) == 3L,
  all(nzchar(all_fixed_effect_rows$B)),
  all(nzchar(all_fixed_effect_rows$beta)),
  all(!nzchar(unlist(all_fixed_effect_rows[, all_fixed_blank_columns, drop = FALSE], use.names = FALSE))),
  all(is.na(all_fixed_effect_rows$p_numeric)),
  all(all_fixed_effect_rows[["B CI source"]] == "Fixed effect - no inferential test"),
  all(all_fixed_effect_rows[["beta CI source"]] == "Fixed effect - no inferential test"),
  all(all_fixed_effect_rows[["Inference source"]] == "Fixed effect - no inferential test"),
  all(all_fixed_effect_rows[["Bootstrap status"]] == "Fixed effect - no inferential test"),
  all(!nzchar(all_fixed_effect_rows[["Valid bootstrap"]]))
)
all_fixed_effect_rows <- structural_canvas_apply_effect_bh_families(all_fixed_effect_rows)
stopifnot(
  all(all_fixed_effect_rows[["BH family"]] == "Fixed effect (not tested)"),
  all(!nzchar(all_fixed_effect_rows[["BH-adjusted p"]]))
)
all_fixed_effect_summary <- structural_canvas_effect_summary_table(all_fixed_effect_rows, ci = FALSE)
all_fixed_effect_ci <- structural_canvas_effect_summary_table(all_fixed_effect_rows, ci = TRUE)
all_fixed_specific <- structural_canvas_specific_indirect_table(all_fixed_effect_rows)
stopifnot(
  "SE" %in% names(all_fixed_effect_summary), !"Boot SE" %in% names(all_fixed_effect_summary),
  "SE" %in% names(all_fixed_specific), !"Boot SE" %in% names(all_fixed_specific),
  all(!nzchar(all_fixed_effect_summary$p)),
  all(!nzchar(all_fixed_specific$p)),
  all(all_fixed_effect_ci[["Indirect CI source"]] == "Fixed effect - no inferential test"),
  all(all_fixed_effect_ci[["Total CI source"]] == "Fixed effect - no inferential test")
)
all_fixed_effect_ko_html <- htmltools::renderTags(
  structural_canvas_effect_summary_html_table(all_fixed_effect_summary, language = "ko")
)$html
all_fixed_effect_ci_ko_html <- htmltools::renderTags(
  structural_canvas_effect_summary_html_table(all_fixed_effect_ci, ci = TRUE, language = "ko")
)$html
all_fixed_specific_ko_html <- htmltools::renderTags(
  structural_canvas_specific_indirect_html_table(all_fixed_specific, language = "ko")
)$html
stopifnot(
  grepl("고정효과 - 추론검정 없음", all_fixed_effect_ko_html, fixed = TRUE),
  grepl("고정효과 - 추론검정 없음", all_fixed_effect_ci_ko_html, fixed = TRUE),
  !grepl("고정효과 - 추론검정 없음", all_fixed_specific_ko_html, fixed = TRUE),
  grepl("고정효과(검정 제외)", all_fixed_effect_ko_html, fixed = TRUE)
)

mixed_effect_fit <- lavaan::sem(
  "m ~ 0.5*x
   y ~ b*m + 0.3*x
   mixed_sp := 0.5*b",
  data = effect_test_data
)
mixed_effect_definition <- list(
  label = "mixed_sp", type = "Specific indirect", predictor = "x", outcome = "y",
  paths = list(c("x", "m", "y")), path_labels = list(c("0.5", "b")), path = c("x", "m", "y")
)
mixed_effect_rows <- structural_canvas_lavaan_structural_effect_rows(
  mixed_effect_fit, list(mixed_effect_definition), fmt, identity_name, bootstrap = NULL
)
stopifnot(
  !structural_canvas_lavaan_effect_is_constant(mixed_effect_fit, mixed_effect_definition),
  nzchar(mixed_effect_rows$SE[[1L]]),
  nzchar(mixed_effect_rows$p[[1L]]),
  mixed_effect_rows[["Inference source"]][[1L]] == "Model-based normal-theory"
)

zero_path_fit <- lavaan::sem(
  "m ~ 0*x
   y ~ b*m + c*x
   zero_sp := 0*b
   zero_ind := 0*b
   zero_tot := c + 0*b",
  data = effect_test_data
)
zero_path_definitions <- list(
  list(
    label = "zero_sp", type = "Specific indirect", predictor = "x", outcome = "y",
    paths = list(c("x", "m", "y")), path_labels = list(c("0", "b")), path = c("x", "m", "y")
  ),
  list(
    label = "zero_ind", type = "Indirect", predictor = "x", outcome = "y",
    paths = list(c("x", "m", "y")), path_labels = list(c("0", "b"))
  ),
  list(
    label = "zero_tot", type = "Total", predictor = "x", outcome = "y",
    paths = list(c("x", "y"), c("x", "m", "y"))
  )
)
zero_path_rows <- structural_canvas_lavaan_structural_effect_rows(
  zero_path_fit, zero_path_definitions, fmt, identity_name, bootstrap = NULL
)
stopifnot(
  structural_canvas_lavaan_effect_is_constant(zero_path_fit, zero_path_definitions[[1L]]),
  structural_canvas_lavaan_effect_is_constant(zero_path_fit, zero_path_definitions[[2L]]),
  !structural_canvas_lavaan_effect_is_constant(zero_path_fit, zero_path_definitions[[3L]]),
  all(zero_path_rows[["Inference source"]][1:2] == "Fixed effect - no inferential test"),
  zero_path_rows[["Inference source"]][[3L]] == "Model-based normal-theory",
  all(!nzchar(zero_path_rows$p[1:2])),
  nzchar(zero_path_rows$p[[3L]])
)

all_fixed_raw <- lavaan::parameterEstimates(all_fixed_effect_fit)
all_fixed_raw <- all_fixed_raw[all_fixed_raw$op %in% c("~", ":="), c("lhs", "op", "rhs", "est"), drop = FALSE]
all_fixed_raw_sources <- structural_canvas_effect_bootstrap_fixed_sources(
  all_fixed_effect_fit, all_fixed_raw, all_fixed_effect_definitions
)
raw_bootstrap_diagnostics <- data.frame(
  lhs = all_fixed_raw$lhs,
  op = all_fixed_raw$op,
  rhs = all_fixed_raw$rhs,
  estimate = all_fixed_raw$est,
  se = .001,
  lower = all_fixed_raw$est - .002,
  upper = all_fixed_raw$est + .002,
  p = 2 / 101,
  beta_estimate = all_fixed_raw$est,
  beta_se = .001,
  beta_lower = all_fixed_raw$est - .002,
  beta_upper = all_fixed_raw$est + .002,
  beta_p = 2 / 101,
  beta_valid = 100L,
  beta_status = "Estimated",
  valid = 100L,
  requested = 100L,
  valid_percent = 100,
  ci_method = "percentile",
  quantile_type = 6L,
  status = "Adequate",
  stringsAsFactors = FALSE
)
raw_bootstrap_diagnostics <- structural_canvas_suppress_fixed_bootstrap_inference(
  raw_bootstrap_diagnostics, all_fixed_raw_sources
)
raw_inference_columns <- c("se", "lower", "upper", "p", "beta_se", "beta_lower", "beta_upper", "beta_p")
stopifnot(
  all(nzchar(all_fixed_raw_sources)),
  all(vapply(raw_bootstrap_diagnostics[raw_inference_columns], function(value) all(is.na(value)), logical(1))),
  all(raw_bootstrap_diagnostics$status == all_fixed_raw_sources),
  all(raw_bootstrap_diagnostics$beta_status == all_fixed_raw_sources),
  all(raw_bootstrap_diagnostics$inference_source == all_fixed_raw_sources),
  all(is.finite(raw_bootstrap_diagnostics$estimate)),
  all(raw_bootstrap_diagnostics$valid == 100L)
)

mixed_raw <- lavaan::parameterEstimates(mixed_effect_fit)
mixed_raw <- mixed_raw[mixed_raw$op %in% c("~", ":="), c("lhs", "op", "rhs", "est"), drop = FALSE]
mixed_sources <- structural_canvas_effect_bootstrap_fixed_sources(
  mixed_effect_fit, mixed_raw, list(mixed_effect_definition)
)
stopifnot(
  identical(mixed_sources[mixed_raw$lhs == "mixed_sp"], ""),
  identical(mixed_sources[mixed_raw$lhs == "m" & mixed_raw$rhs == "x"], "Fixed parameter - no inferential test"),
  identical(mixed_sources[mixed_raw$lhs == "y" & mixed_raw$rhs == "m"], "")
)

pending_state <- structural_canvas_effect_bootstrap_reporting_state(
  requested = 100L, result = NULL, pending = TRUE
)
canceled_state <- structural_canvas_effect_bootstrap_reporting_state(
  requested = 100L, result = NULL, canceled = TRUE
)
failed_state <- structural_canvas_effect_bootstrap_reporting_state(
  requested = 100L, result = NULL, error = "synthetic failure"
)
unavailable_state <- structural_canvas_effect_bootstrap_reporting_state(
  requested = 100L, result = data.frame()
)
completed_state <- structural_canvas_effect_bootstrap_reporting_state(
  requested = 100L, result = fake_bootstrap
)
stopifnot(
  pending_state$source == "Bootstrap pending - inference suppressed",
  canceled_state$source == "Bootstrap canceled - inference suppressed",
  failed_state$source == "Bootstrap failed - inference suppressed",
  unavailable_state$source == "Bootstrap unavailable - inference suppressed",
  isTRUE(completed_state$complete)
)
for (reporting_state in list(pending_state, canceled_state, failed_state, unavailable_state)) {
  suppressed_direct <- structural_canvas_lavaan_structural_result_table(
    "structural", fit, FALSE, fmt, identity_name,
    bootstrap = NULL, bootstrap_state = reporting_state
  )
  suppressed_effects <- structural_canvas_lavaan_structural_effect_rows(
    fit, effect_definitions, fmt, identity_name,
    bootstrap = NULL, bootstrap_state = reporting_state
  )
  expected_source <- reporting_state$source
  for (suppressed_table in list(suppressed_direct, suppressed_effects)) {
    stopifnot(
      all(nzchar(suppressed_table$B)),
      all(nzchar(suppressed_table$beta)),
      all(!nzchar(suppressed_table$SE)),
      all(!nzchar(suppressed_table[["B 95% CI lower"]])),
      all(!nzchar(suppressed_table[["B 95% CI upper"]])),
      all(!nzchar(suppressed_table[["beta 95% CI lower"]])),
      all(!nzchar(suppressed_table[["beta 95% CI upper"]])),
      all(!nzchar(suppressed_table$z)),
      all(!nzchar(suppressed_table$p)),
      all(is.na(suppressed_table$p_numeric)),
      all(suppressed_table[["B CI source"]] == expected_source),
      all(suppressed_table[["beta CI source"]] == expected_source),
      all(suppressed_table[["Inference source"]] == expected_source),
      all(suppressed_table[["Bootstrap status"]] == expected_source),
      all(!nzchar(suppressed_table[["Valid bootstrap"]]))
    )
  }
}
pending_effects <- structural_canvas_lavaan_structural_effect_rows(
  fit, effect_definitions, fmt, identity_name,
  bootstrap = NULL, bootstrap_state = pending_state
)
pending_effects <- structural_canvas_apply_effect_bh_families(pending_effects)
pending_summary <- structural_canvas_effect_summary_table(pending_effects, ci = FALSE)
pending_summary_ko_html <- htmltools::renderTags(
  structural_canvas_effect_summary_html_table(pending_summary, language = "ko")
)$html
stopifnot(
  all(!nzchar(pending_summary[["BH-adjusted p"]])),
  grepl("부트스트랩 대기 중 - 추론값 억제", pending_summary_ko_html, fixed = TRUE)
)
pending_export_notes <- structural_canvas_export_notes(list(
  ordered = character(0),
  diagnostics = list(admissible = TRUE),
  snapshot = list(nodes = list(), edges = list()),
  effect_bootstrap = 100L,
  effect_bootstrap_result = NULL,
  effect_bootstrap_pending = TRUE,
  effect_bootstrap_canceled = FALSE,
  effect_bootstrap_error = ""
))
stopifnot(
  any(pending_export_notes$Section == "Structural-effect bootstrap"),
  any(grepl("pending; model-based inferential values were not substituted", pending_export_notes$Note, fixed = TRUE))
)

cat("SEM structural reporting table validations passed.\n")
