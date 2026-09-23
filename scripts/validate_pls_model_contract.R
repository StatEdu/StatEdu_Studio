source(file.path("scripts", "validate_cfa_common.R"), encoding = "UTF-8")

pls_contract_snapshot <- list(
  nodes = list(
    list(id = "eta1", role = "latent", name = "Eta 1", constructType = "commonFactor", measurementMode = "reflective"),
    list(id = "eta2", role = "latent", name = "Eta 2", constructType = "commonFactor", measurementMode = "reflective"),
    list(id = "x1", role = "indicator", name = "x1", variableId = "x1"),
    list(id = "x2", role = "indicator", name = "x2", variableId = "x2"),
    list(id = "y1", role = "indicator", name = "y1", variableId = "y1"),
    list(id = "y2", role = "indicator", name = "y2", variableId = "y2")
  ),
  edges = list(
    list(id = "mx1", from = "eta1", to = "x1", free = TRUE),
    list(id = "mx2", from = "eta1", to = "x2", free = TRUE),
    list(id = "my1", from = "eta2", to = "y1", free = TRUE),
    list(id = "my2", from = "eta2", to = "y2", free = TRUE),
    list(id = "path12", from = "eta1", to = "eta2", free = TRUE)
  ),
  covariates = character(0),
  covariateTargets = list()
)

expect_pls_contract_error <- function(snapshot, english_fragment, korean_fragment) {
  error <- tryCatch({
    structural_canvas_validate_pls_model_contract(snapshot, "plssem")
    NULL
  }, error = identity)
  stopifnot(
    inherits(error, "error"),
    grepl("PLS model contract blocked estimation:", conditionMessage(error), fixed = TRUE),
    grepl(english_fragment, conditionMessage(error), fixed = TRUE),
    grepl(korean_fragment, structural_canvas_error_message(error, "ko"), fixed = TRUE),
    identical(structural_canvas_error_message(error, "en"), conditionMessage(error))
  )
  invisible(error)
}

stopifnot(isTRUE(structural_canvas_validate_pls_model_contract(pls_contract_snapshot, "plssem")))
stopifnot(isTRUE(structural_canvas_validate_pls_model_contract(pls_contract_snapshot, "sem")))

covariate_snapshot <- pls_contract_snapshot
covariate_snapshot$covariates <- "age"
covariate_snapshot$covariateTargets <- list(age = "eta2")
expect_pls_contract_error(
  covariate_snapshot,
  "observed covariates/control variables and covariateTargets",
  "관측 통제변수와 통제변수 대상"
)

stale_target_snapshot <- pls_contract_snapshot
stale_target_snapshot$covariateTargets <- list(age = "eta2")
expect_pls_contract_error(
  stale_target_snapshot,
  "observed covariates/control variables and covariateTargets",
  "통제효과가 조용히 제외"
)

modifier_cases <- list(
  fixed_flag = list(free = FALSE),
  fixed_value = list(fixedValue = .5),
  start_value = list(startValue = .2),
  parameter_name = list(parameterName = "path_a"),
  equality_label = list(equalityLabel = "equal_a")
)
for (modifier in modifier_cases) {
  modified_snapshot <- pls_contract_snapshot
  for (name in names(modifier)) modified_snapshot$edges[[5L]][[name]] <- modifier[[name]]
  expect_pls_contract_error(
    modified_snapshot,
    "fixed/free constraints, fixed values, start values, parameter names, and equality labels",
    "해당 지정이 조용히 무시"
  )
}

residual_modifier_snapshot <- pls_contract_snapshot
residual_modifier_snapshot$nodes <- c(
  residual_modifier_snapshot$nodes,
  list(list(id = "zeta2", role = "disturbance", name = "zeta2", free = FALSE, fixedValue = .3))
)
expect_pls_contract_error(
  residual_modifier_snapshot,
  "fixed/free constraints, fixed values, start values, parameter names, and equality labels",
  "고정·자유 모수 제약"
)

cycle_snapshot <- pls_contract_snapshot
cycle_snapshot$edges <- c(cycle_snapshot$edges, list(list(id = "path21", from = "eta2", to = "eta1", free = TRUE)))
expect_pls_contract_error(
  cycle_snapshot,
  "directed structural paths must be acyclic",
  "상호회귀경로 또는 피드백 순환"
)

cross_loaded_snapshot <- pls_contract_snapshot
cross_loaded_snapshot$edges <- c(cross_loaded_snapshot$edges, list(list(id = "cross_x1", from = "eta2", to = "x1", free = TRUE)))
expect_pls_contract_error(
  cross_loaded_snapshot,
  "each indicator may belong to only one construct",
  "각 측정지표는 하나의 구성개념에만 소속"
)

duplicate_edge_snapshot <- pls_contract_snapshot
duplicate_edge_snapshot$edges <- c(duplicate_edge_snapshot$edges, list(list(id = "mx1_duplicate", from = "eta1", to = "x1", free = TRUE)))
duplicate_error <- expect_pls_contract_error(
  duplicate_edge_snapshot,
  "each measurement edge must be unique",
  "동일한 측정경로를 중복 배치"
)
stopifnot(grepl("x1 (duplicate measurement edge)", conditionMessage(duplicate_error), fixed = TRUE))

# The public server execution entry point must apply the same gate before
# constructing or estimating a PLS model.
server_error <- tryCatch({
  run_structural_canvas_analysis(
    covariate_snapshot,
    data.frame(x1 = 1:4, x2 = 2:5, y1 = 3:6, y2 = 4:7, age = 20:23),
    "plssem",
    estimator = "PLS"
  )
  NULL
}, error = identity)
stopifnot(
  inherits(server_error, "error"),
  grepl("observed covariates/control variables and covariateTargets", conditionMessage(server_error), fixed = TRUE)
)

message("PLS model-contract validation passed.")
