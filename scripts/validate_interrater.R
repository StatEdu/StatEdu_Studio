source("R/analysis_interrater_agreement.R")

assert_close <- function(actual, expected, tolerance = 1e-10, label = "value") {
  if (!isTRUE(all.equal(actual, expected, tolerance = tolerance))) {
    stop(sprintf("%s mismatch: actual=%0.12f expected=%0.12f", label, actual, expected), call. = FALSE)
  }
}

reference_gwet_ac <- function(frame, levels, ordinal = FALSE, weight = "quadratic") {
  agreement_weights <- if (isTRUE(ordinal)) {
    interrater_weight_matrix(levels, weight = weight, agreement = TRUE)
  } else {
    diag(length(levels))
  }
  total_unit_agreement <- 0
  total_unit_proportions <- numeric(length(levels))
  eligible_units <- 0
  rated_units <- 0

  for (row_index in seq_len(nrow(frame))) {
    values <- match(as.character(unlist(frame[row_index, , drop = TRUE], use.names = FALSE)), levels)
    values <- values[!is.na(values)]
    if (length(values) > 0) {
      counts <- tabulate(values, nbins = length(levels))
      total_unit_proportions <- total_unit_proportions + counts / length(values)
      rated_units <- rated_units + 1L
    }
    if (length(values) < 2) next
    pairs <- utils::combn(values, 2L)
    total_unit_agreement <- total_unit_agreement + sum(agreement_weights[cbind(pairs[1, ], pairs[2, ])]) / ncol(pairs)
    eligible_units <- eligible_units + 1L
  }

  p <- total_unit_proportions / rated_units
  pa <- total_unit_agreement / eligible_units
  pe <- sum(agreement_weights) * sum(p * (1 - p)) / (length(levels) * (length(levels) - 1))
  (pa - pe) / (1 - pe)
}

reference_krippendorff_alpha_nominal <- function(frame, levels) {
  values <- matrix(match(as.character(as.matrix(frame)), levels), nrow = nrow(frame), dimnames = dimnames(frame))
  distance <- function(x, y) as.numeric(x != y)
  observed_num <- 0
  observed_den <- 0

  for (row_index in seq_len(nrow(values))) {
    row <- values[row_index, ]
    row <- row[!is.na(row)]
    if (length(row) < 2) next
    pairs <- utils::combn(row, 2L)
    unit_weight <- 1 / (length(row) - 1)
    observed_num <- observed_num + sum(distance(pairs[1, ], pairs[2, ])) * unit_weight
    observed_den <- observed_den + ncol(pairs) * unit_weight
  }

  eligible_rows <- apply(!is.na(values), 1L, sum) >= 2
  pooled <- values[eligible_rows, , drop = FALSE]
  pooled <- pooled[!is.na(pooled)]
  counts <- tabulate(as.integer(pooled), nbins = length(levels))
  distance_matrix <- outer(seq_along(levels), seq_along(levels), distance)
  pair_weights <- outer(counts, counts)
  pair_weights[lower.tri(pair_weights, diag = TRUE)] <- 0
  expected <- sum(distance_matrix * pair_weights) / choose(sum(counts), 2L)
  1 - (observed_num / observed_den) / expected
}

ordinal_frame <- data.frame(
  r1 = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5),
  r2 = c(1, 2, 2, 3, 3, 4, 4, 5, 5, 5),
  r3 = c(1, 1, 2, 2, 3, 3, 4, 4, 4, 5)
)
ordinal_levels <- as.character(1:5)
assert_close(
  interrater_gwet_ac(ordinal_frame, ordinal_levels, ordinal = TRUE, weight = "quadratic"),
  reference_gwet_ac(ordinal_frame, ordinal_levels, ordinal = TRUE, weight = "quadratic"),
  label = "Gwet AC2"
)

missing_nominal_frame <- data.frame(
  r1 = c(1, 1, 2, 2, 3, 3, 1, NA),
  r2 = c(1, 2, 2, 3, 3, NA, 2, 1),
  r3 = c(1, 1, 2, 3, NA, 3, NA, NA)
)
missing_nominal_levels <- as.character(1:3)
assert_close(
  interrater_gwet_ac(missing_nominal_frame, missing_nominal_levels, ordinal = FALSE),
  reference_gwet_ac(missing_nominal_frame, missing_nominal_levels, ordinal = FALSE),
  label = "Gwet AC1 with missing ratings"
)
assert_close(
  interrater_gwet_ac(missing_nominal_frame, missing_nominal_levels, ordinal = FALSE),
  0.5029126214,
  tolerance = 1e-10,
  label = "Gwet AC1 missing fixed reference"
)

missing_ordinal_frame <- data.frame(
  r1 = c(1, 2, 3, 4, 5, 4, NA, 2),
  r2 = c(1, 2, 4, 4, 5, NA, 3, 3),
  r3 = c(1, 3, 3, NA, 5, 4, NA, NA)
)
missing_ordinal_levels <- as.character(1:5)
assert_close(
  interrater_gwet_ac(missing_ordinal_frame, missing_ordinal_levels, ordinal = TRUE, weight = "quadratic"),
  reference_gwet_ac(missing_ordinal_frame, missing_ordinal_levels, ordinal = TRUE, weight = "quadratic"),
  label = "Gwet AC2 with missing ratings"
)
assert_close(
  interrater_gwet_ac(missing_ordinal_frame, missing_ordinal_levels, ordinal = TRUE, weight = "quadratic"),
  0.9263944796,
  tolerance = 1e-10,
  label = "Gwet AC2 missing fixed reference"
)

krippendorff_2011_nominal <- data.frame(
  r1 = c(1, 2, 3, 3, 2, 1, 4, 1, 2, NA, NA, NA),
  r2 = c(1, 2, 3, 3, 2, 2, 4, 1, 2, 5, NA, 3),
  r3 = c(NA, 3, 3, 3, 2, 3, 4, 2, 2, 5, 1, NA),
  r4 = c(1, 2, 3, 3, 2, 4, 4, 1, 2, 5, 1, NA)
)
nominal_levels <- as.character(1:5)
alpha <- interrater_krippendorff_alpha(krippendorff_2011_nominal, nominal_levels, level = "nominal")
assert_close(alpha, reference_krippendorff_alpha_nominal(krippendorff_2011_nominal, nominal_levels), label = "Krippendorff alpha")
assert_close(alpha, 0.7434211, tolerance = 1e-7, label = "Krippendorff 2011 nominal example")

factor_frame <- data.frame(
  r1 = factor(c("2.5", "3.5", "4.5")),
  r2 = factor(c("2.5", "3.0", "4.0"))
)
converted <- as.data.frame(lapply(factor_frame, function(values) suppressWarnings(as.numeric(as.character(values)))), check.names = FALSE)
if (!identical(converted$r1, c(2.5, 3.5, 4.5))) {
  stop("Continuous factor labels should convert through character labels, not factor level codes.", call. = FALSE)
}

cat("Inter-rater agreement validation passed.\n")
