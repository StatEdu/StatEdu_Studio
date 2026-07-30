source("R/analysis_reliability.R")

`%||%` <- function(x, y) if (is.null(x)) y else x
named_value <- function(values, name, default = NULL) {
  if (is.null(values) || !name %in% names(values)) return(default)
  values[[name]]
}
frequency_value_order <- function(values) {
  as.character(sort(unique(values), na.last = TRUE))
}

assert_close <- function(actual, expected, tolerance = 1e-10, label = "value") {
  if (!isTRUE(all.equal(actual, expected, tolerance = tolerance))) {
    stop(sprintf("%s mismatch: actual=%0.12f expected=%0.12f", label, actual, expected), call. = FALSE)
  }
}

expect_validation_message <- function(expr, pattern, label) {
  tryCatch(
    {
      force(expr)
      stop(sprintf("%s did not raise a validation error.", label), call. = FALSE)
    },
    shiny.silent.error = function(error) {
      if (!grepl(pattern, conditionMessage(error), fixed = TRUE)) {
        stop(sprintf("%s message mismatch: %s", label, conditionMessage(error)), call. = FALSE)
      }
    },
    error = function(error) {
      if (!grepl(pattern, conditionMessage(error), fixed = TRUE)) {
        stop(sprintf("%s message mismatch: %s", label, conditionMessage(error)), call. = FALSE)
      }
    }
  )
}

set.seed(20260730)
factor_score <- rnorm(120)
continuous_items <- data.frame(
  i1 = factor_score + rnorm(120, sd = 0.4),
  i2 = 2.5 * factor_score + rnorm(120, sd = 0.8),
  i3 = 0.6 * factor_score + rnorm(120, sd = 0.5),
  i4 = 4.0 * factor_score + rnorm(120, sd = 1.2)
)
factor_label_items <- data.frame(
  i1 = factor(c("2.5", "3.5", "4.5")),
  i2 = factor(c("1.25", "2.25", "3.25"))
)
factor_label_matrix <- reliability_numeric_matrix(factor_label_items, names(factor_label_items), "continuous")
if (!identical(factor_label_matrix$i1, c(2.5, 3.5, 4.5)) ||
    !identical(factor_label_matrix$i2, c(1.25, 2.25, 3.25))) {
  stop("Reliability numeric conversion should use factor labels rather than integer level codes.", call. = FALSE)
}

psych_alpha <- suppressWarnings(psych::alpha(continuous_items, check.keys = FALSE, warnings = FALSE))
app_values <- reliability_pearson_values(continuous_items)
if (!isTRUE(reliability_include_omega(list())) ||
    !isTRUE(reliability_include_omega(list(omega = TRUE, ordinal = FALSE))) ||
    isTRUE(reliability_include_omega(list(omega = FALSE, ordinal = TRUE)))) {
  stop("Omega reporting should be controlled by options$omega, independent of the ordinal option.", call. = FALSE)
}

assert_close(
  app_values[["pearson_alpha"]],
  psych_alpha$total$raw_alpha[[1]],
  label = "Cronbach raw alpha"
)

if (abs(app_values[["pearson_alpha"]] - psych_alpha$total$std.alpha[[1]]) < 1e-4) {
  stop("Validation data should distinguish raw alpha from standardized alpha.", call. = FALSE)
}

alpha_drop <- psych_alpha$alpha.drop
raw_drop <- alpha_drop[["raw_alpha"]] %||% alpha_drop[["raw.alpha"]]
for (item in names(continuous_items)) {
  deletion_value <- reliability_pearson_values(continuous_items[, setdiff(names(continuous_items), item), drop = FALSE])
  item_index <- match(item, rownames(alpha_drop))
  assert_close(
    deletion_value[["pearson_alpha"]],
    raw_drop[[item_index]],
    label = sprintf("Cronbach raw alpha if %s deleted", item)
  )
}

binary_items <- data.frame(
  b1 = c(1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1),
  b2 = c(1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1),
  b3 = c(1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1),
  b4 = c(1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1)
)
kr20_ref <- suppressWarnings(psych::alpha(binary_items, check.keys = FALSE, warnings = FALSE))$total$raw_alpha[[1]]
assert_close(reliability_kr20_value(binary_items), kr20_ref, label = "KR-20 raw alpha equivalence")
assert_close(reliability_kr20_value(binary_items), reliability_alpha_value(binary_items), label = "KR-20 formula")

invalid_binary <- data.frame(
  b1 = c(0, 1, 2, 1),
  b2 = c(0, 1, 0, 1)
)
invalid_binary_info <- data.frame(name = names(invalid_binary), measurement = "binary")
expect_validation_message(
  prepare_reliability_results(invalid_binary, names(invalid_binary), variable_info = invalid_binary_info),
  "Binary reliability (KR-20) requires items with exactly two observed values. Check: b1",
  "Invalid binary category count"
)

constant_items <- data.frame(
  i1 = c(1, 1, 1, 1),
  i2 = c(1, 2, 3, 4)
)
constant_info <- data.frame(name = names(constant_items), measurement = "continuous")
expect_validation_message(
  prepare_reliability_results(constant_items, names(constant_items), variable_info = constant_info),
  "Item(s) with zero variance were found: i1. Remove constant items before reliability analysis.",
  "Zero variance item"
)

cat("Reliability validation passed.\n")
