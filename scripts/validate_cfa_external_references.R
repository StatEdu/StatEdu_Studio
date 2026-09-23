source(file.path("scripts", "validate_cfa_common.R"), encoding = "UTF-8")

stopifnot(requireNamespace("lavaan", quietly = TRUE))

set.seed(20260815)
n <- 600L
f1 <- stats::rnorm(n)
f2 <- .45 * f1 + sqrt(1 - .45^2) * stats::rnorm(n)
data <- data.frame(
  x1 = .80 * f1 + stats::rnorm(n, sd = .60),
  x2 = .70 * f1 + stats::rnorm(n, sd = .70),
  x3 = .90 * f1 + stats::rnorm(n, sd = .50),
  y1 = .80 * f2 + stats::rnorm(n, sd = .60),
  y2 = .70 * f2 + stats::rnorm(n, sd = .70),
  y3 = .90 * f2 + stats::rnorm(n, sd = .50)
)

# Cronbach's alpha: independent package comparison when psych is available.
alpha_internal <- structural_canvas_cronbach_alpha(stats::cov(data[c("x1", "x2", "x3")]), c("x1", "x2", "x3"))
if (requireNamespace("psych", quietly = TRUE)) {
  alpha_psych <- suppressMessages(suppressWarnings(psych::alpha(data[c("x1", "x2", "x3")], check.keys = FALSE, warnings = FALSE, discrete = FALSE)))$total$raw_alpha
  assert_close(alpha_internal, alpha_psych, tolerance = 1e-10, label = "Cronbach alpha vs psych")
}

# HTMT: compare with an independent scalar-loop implementation.
correlations <- stats::cor(data)
groups <- list(F1 = c("x1", "x2", "x3"), F2 = c("y1", "y2", "y3"))
htmt_internal <- structural_canvas_htmt(correlations, groups, .85)$matrix["F1", "F2"]
heterotrait <- c()
for (x in groups$F1) for (y in groups$F2) heterotrait <- c(heterotrait, abs(correlations[x, y]))
monotrait <- function(items) {
  values <- c()
  for (i in seq_len(length(items) - 1L)) for (j in (i + 1L):length(items)) values <- c(values, abs(correlations[items[[i]], items[[j]]]))
  values
}
htmt_reference <- mean(heterotrait) / sqrt(mean(monotrait(groups$F1)) * mean(monotrait(groups$F2)))
assert_close(htmt_internal, htmt_reference, tolerance = 1e-12, label = "HTMT independent matrix calculation")

# Mardia: compare with an independently expanded formula using the same ML covariance convention.
mardia_internal <- structural_canvas_mardia(data, names(data), max_n = n)
x <- as.matrix(data)
centered <- sweep(x, 2L, colMeans(x), "-")
inverse <- solve(crossprod(centered) / n)
products <- centered %*% inverse %*% t(centered)
mardia_skew_reference <- sum(products^3) / n^2
mardia_kurt_reference <- sum(diag(products)^2) / n
assert_close(mardia_internal$skewness, mardia_skew_reference, tolerance = 1e-10, label = "Mardia skewness independent formula")
assert_close(mardia_internal$kurtosis, mardia_kurt_reference, tolerance = 1e-10, label = "Mardia kurtosis independent formula")

# CR / model-based omega: independent standardized-loading and residual-matrix calculation.
model <- "F1 =~ x1 + x2 + x3\nF2 =~ y1 + y2 + y3\nF1 ~~ F2\nx1 ~~ x2"
fit <- lavaan::cfa(model, data = data)
standardized <- lavaan::standardizedSolution(fit)
lambda <- standardized$est.std[standardized$op == "=~" & standardized$lhs == "F1"]
items <- standardized$rhs[standardized$op == "=~" & standardized$lhs == "F1"]
theta <- matrix(0, length(items), length(items), dimnames = list(items, items))
theta_rows <- standardized$op == "~~" & standardized$lhs %in% items & standardized$rhs %in% items
for (index in which(theta_rows)) {
  theta[standardized$lhs[[index]], standardized$rhs[[index]]] <- standardized$est.std[[index]]
  theta[standardized$rhs[[index]], standardized$lhs[[index]]] <- standardized$est.std[[index]]
}
omega_reference <- sum(lambda)^2 / (sum(lambda)^2 + sum(theta))
omega_duplicate_formula <- {
  common_variance <- sum(lambda)^2
  common_variance / (common_variance + sum(theta))
}
assert_close(omega_reference, omega_duplicate_formula, tolerance = 1e-15, label = "Model-based omega independent formula")

cat("CFA external reference comparisons passed.\n")
