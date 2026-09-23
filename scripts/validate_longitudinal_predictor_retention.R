# Run from the repository root with the bundled R runtime.
`%||%` <- function(x, y) if (is.null(x)) y else x
source(file.path("R", "analysis_longitudinal.R"))
set.seed(20260905)

check_case <- function(predictors, model_type = "lmm", random_slope = FALSE,
                       cluster = character(0), weighted = FALSE) {
  n <- 320L
  data <- data.frame(id = factor(rep(1:80, each = 4)), time = rep(0:3, 80))
  data$site <- factor(rep(1:10, each = 32))
  for (name in predictors) data[[name]] <- rnorm(n)
  eta <- 0.4 + 0.1 * data$time + 0.25 * data[[predictors[[1]]]] +
    rep(rnorm(80, sd = 0.6), each = 4)
  data$y <- if (model_type == "lmm") eta + rnorm(n) else rpois(n, exp(eta))
  terms <- c("time", predictors)
  fixed <- longitudinal_formula("y", terms)
  fit <- longitudinal_fit_model(
    data, "y", "id", "time", terms, model_type,
    if (model_type == "lmm") "gaussian" else "poisson", "exchangeable",
    random_slope = random_slope, cluster = cluster,
    weights = if (weighted) rep(1, n) else NULL
  )
  expected <- colnames(model.matrix(fixed, data))
  missing <- setdiff(expected, fit$coef_table$Term)
  cat(sprintf("%s: %d predictors, %d deparse lines, missing: %s\n",
              model_type, length(predictors), length(deparse(fixed)),
              paste(missing, collapse = ", ")))
  stopifnot(length(missing) == 0L,
            setequal(all.vars(fit$formula), c("y", terms, "id", cluster)))
  # Count-family screening builds its own GLMM formula.
  if (model_type == "glmm") {
    screen <- longitudinal_fit_count_screen_model(
      data, fixed, "glmm", "poisson", "id", "time", cluster, random_slope
    )
    stopifnot(inherits(screen, "merMod"),
              setequal(names(lme4::fixef(screen)), expected))
  }
}

check_case(paste0("predictor_", 1:8))
check_case(paste0("long_independent_variable_", 1:4))
check_case(paste0("x", 1:12))
check_case(paste0("long independent variable ", 1:4), random_slope = TRUE,
           cluster = "site", weighted = TRUE)
check_case(paste0("predictor_", 1:8), model_type = "glmm")
cat("Longitudinal predictor retention checks passed.\n")
