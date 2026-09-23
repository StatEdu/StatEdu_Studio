# Correlation and association analysis helpers.

correlation_variable_display_name <- function(name, variable_info = NULL, labels = character(0), category_table = NULL) {
  frequency_variable_display_name(name, variable_info, labels, category_table)
}

correlation_display_name_reader <- function(variable_info = NULL, labels = character(0), category_table = NULL) {
  plain_text <- function(x) is.character(x) && is.null(attributes(x))
  cacheable <- identical(class(category_table), "data.frame") &&
    plain_text(category_table$name) && plain_text(category_table$var_label) &&
    is.character(labels) && (is.null(attributes(labels)) || identical(names(attributes(labels)), "names"))
  display_labels <- NULL
  function(name) {
    if (!cacheable) return(correlation_variable_display_name(name, variable_info, labels, category_table))
    if (is.null(display_labels)) {
      quiet <- TRUE
      merged <- withCallingHandlers({
        current <- labels
        category_labels <- category_var_label_lookup_static(category_table)
        if (length(category_labels) > 0) current[names(category_labels)] <- category_labels
        current
      }, warning = function(w) quiet <<- FALSE, message = function(m) quiet <<- FALSE)
      if (quiet) display_labels <<- merged
    } else {
      merged <- display_labels
    }
    display_variable_name_static(name, variable_info, merged, label_only = TRUE)
  }
}

correlation_measurement_lookup <- function(variable_info = NULL) {
  if (!is.data.frame(variable_info) || !all(c("name", "measurement") %in% names(variable_info))) {
    return(character(0))
  }
  values <- tolower(as.character(variable_info$measurement))
  values[values == "ordinal"] <- "ordered"
  stats::setNames(values, as.character(variable_info$name))
}

correlation_measurement <- function(name, variable_info = NULL) {
  measurements <- correlation_measurement_lookup(variable_info)
  measurement <- named_value(measurements, name, "continuous")
  if (measurement %in% c("continuous", "ordered", "binary", "category")) {
    return(measurement)
  }
  "continuous"
}

correlation_measurement_label <- function(measurement) {
  switch(
    measurement,
    continuous = "Continuous",
    ordered = "Ordinal",
    binary = "Binary",
    category = "Nominal",
    measurement
  )
}

correlation_measurement_reader <- function(variable_info = NULL) {
  plain_text <- function(x) is.character(x) && is.null(attributes(x))
  cacheable <- identical(class(variable_info), "data.frame") &&
    plain_text(variable_info$name) && plain_text(variable_info$measurement)
  measurements <- NULL
  function(name) {
    if (!cacheable) return(correlation_measurement(name, variable_info))
    if (is.null(measurements)) {
      quiet <- TRUE
      lookup <- withCallingHandlers(correlation_measurement_lookup(variable_info),
        warning = function(w) quiet <<- FALSE, message = function(m) quiet <<- FALSE)
      # Repeat diagnostic-emitting conversions at their original call sites.
      if (quiet) measurements <<- lookup
    } else {
      lookup <- measurements
    }
    measurement <- named_value(lookup, name, "continuous")
    if (measurement %in% c("continuous", "ordered", "binary", "category")) return(measurement)
    "continuous"
  }
}

correlation_numeric_vector <- function(values) {
  suppressWarnings(as.numeric(values))
}

correlation_ordered_score <- function(values, name = NULL, category_table = NULL) {
  if (is.character(values) || is.factor(values)) {
    values <- as.character(values)
    values[!nzchar(trimws(values))] <- NA_character_
  }
  category_order <- frequency_category_value_order(name, category_table)
  if (length(category_order) > 0) {
    return(frequency_ordered_score(values, name = name, category_table = category_table))
  }
  numeric <- suppressWarnings(as.numeric(values))
  non_missing <- values[!is.na(values)]
  if (length(non_missing) == 0) {
    return(numeric)
  }
  if (sum(!is.na(numeric)) >= 3) {
    return(numeric)
  }
  ordered_values <- frequency_value_order(non_missing, name = name, category_table = category_table)
  as.numeric(match(as.character(values), ordered_values))
}

correlation_binary_score <- function(values) {
  if (is.factor(values)) {
    raw <- as.character(values)
  } else {
    raw <- as.character(values)
  }
  raw[is.na(values)] <- NA_character_
  raw[!is.na(raw) & !nzchar(trimws(raw))] <- NA_character_
  levels <- sort(unique(raw[!is.na(raw)]))
  if (length(levels) != 2) {
    return(rep(NA_real_, length(values)))
  }
  ifelse(raw == levels[[2]], 1, ifelse(raw == levels[[1]], 0, NA_real_))
}

correlation_factor_vector <- function(values) {
  raw <- as.character(values)
  raw[is.na(values)] <- NA_character_
  raw[!is.na(raw) & !nzchar(trimws(raw))] <- NA_character_
  factor(raw)
}

correlation_analysis_vector <- function(values, measurement, name = NULL, category_table = NULL) {
  switch(
    measurement,
    continuous = correlation_numeric_vector(values),
    ordered = correlation_ordered_score(values, name = name, category_table = category_table),
    binary = correlation_binary_score(values),
    category = correlation_factor_vector(values),
    correlation_numeric_vector(values)
  )
}

correlation_numeric_data <- function(data, variables, variable_info = NULL, category_table = NULL, prepared_vectors = NULL, measurement_reader = NULL) {
  variables <- intersect(as.character(variables %||% character(0)), names(data))
  measurement_reader <- measurement_reader %||% correlation_measurement_reader(variable_info)
  out <- data.frame(lapply(variables, function(name) {
    measurement <- measurement_reader(name)
    if (identical(measurement, "category")) {
      return(rep(NA_real_, nrow(data)))
    }
    prepared_vectors[[name]] %||% correlation_analysis_vector(data[[name]], measurement, name = name, category_table = category_table)
  }), check.names = FALSE)
  names(out) <- variables
  out
}

correlation_complete_pair <- function(x, y) {
  if (typeof(x) %in% c("integer", "double") && typeof(y) %in% c("integer", "double") &&
      is.null(attributes(x)) && is.null(attributes(y)) && length(x) == length(y) &&
      !anyNA(x) && !anyNA(y)) {
    return(list(x = x, y = y, n = length(x)))
  }
  complete <- stats::complete.cases(x, y)
  list(x = x[complete], y = y[complete], n = sum(complete))
}

correlation_normality_summary <- function(data, variables, variable_info = NULL, labels = character(0), category_table = NULL, prepared_vectors = NULL, measurement_reader = NULL, display_reader = NULL) {
  measurement_reader <- measurement_reader %||% correlation_measurement_reader(variable_info)
  display_reader <- display_reader %||% correlation_display_name_reader(variable_info, labels, category_table)
  numeric_data <- correlation_numeric_data(data, variables, variable_info, category_table, prepared_vectors, measurement_reader)
  rows <- lapply(names(numeric_data), function(name) {
    measurement <- measurement_reader(name)
    values <- numeric_data[[name]]
    values <- values[!is.na(values)]
    if (!identical(measurement, "continuous")) {
      return(data.frame(
        Name = name,
        Variable = display_reader(name),
        N = "",
        Skewness = "",
        Kurtosis = "",
        Normality = "not assessed",
        normal = FALSE,
        check.names = FALSE
      ))
    }
    skew <- sample_skewness(values)
    kurtosis <- sample_excess_kurtosis(values)
    satisfied <- is.finite(skew) && is.finite(kurtosis) && abs(skew) <= 2 && abs(kurtosis) <= 7
    data.frame(
      Name = name,
      Variable = display_reader(name),
      N = length(values),
      Skewness = format_decimal3(skew),
      Kurtosis = format_decimal3(kurtosis),
      Normality = if (isTRUE(satisfied)) "satisfied" else "not satisfied",
      normal = isTRUE(satisfied),
      check.names = FALSE
    )
  })
  if (length(rows) == 0) {
    return(data.frame())
  }
  do.call(rbind, rows)
}

correlation_ci <- function(r, n, level = 0.95, method = "pearson") {
  method <- as.character(method %||% "pearson")
  minimum_n <- if (identical(method, "kendall")) 5L else 4L
  if (!is.finite(r) || n < minimum_n || abs(r) >= 1) {
    return(c(NA_real_, NA_real_))
  }
  z <- atanh(r)
  se <- if (identical(method, "kendall")) sqrt(0.437 / (n - 4)) else 1 / sqrt(n - 3)
  critical <- stats::qnorm(1 - (1 - level) / 2)
  tanh(c(z - critical * se, z + critical * se))
}

correlation_polycor_inference <- function(fit, level = 0.95) {
  unavailable <- function(coefficient = NA_real_, reason = "Estimator output was unavailable.") {
    list(
      coefficient = coefficient,
      se = NA_real_,
      statistic = NA_real_,
      p = NA_real_,
      ci = c(NA_real_, NA_real_),
      ci_method = "",
      inference_status = "unavailable",
      inference_reason = reason
    )
  }
  if (!is.list(fit) || is.null(fit$rho) || length(fit$rho) == 0L) {
    return(unavailable(reason = "polycor did not return a correlation estimate."))
  }

  coefficient <- suppressWarnings(as.numeric(fit$rho[[1]]))
  if (!is.finite(coefficient)) {
    return(unavailable(reason = "polycor returned a non-finite correlation estimate."))
  }
  if (abs(coefficient) > 1) {
    return(unavailable(reason = "polycor returned a correlation estimate outside [-1, 1]."))
  }
  boundary_limit <- .9999 - sqrt(.Machine$double.eps)
  if (abs(coefficient) >= boundary_limit) {
    return(unavailable(coefficient, "The latent-response correlation reached the polycor boundary; asymptotic inference is not reliable."))
  }

  covariance <- fit$var %||% NULL
  variance <- if (is.matrix(covariance) && nrow(covariance) >= 1L && ncol(covariance) >= 1L) {
    suppressWarnings(as.numeric(covariance[1L, 1L]))
  } else {
    NA_real_
  }
  if (!is.finite(variance) || variance <= 0) {
    return(unavailable(coefficient, "polycor did not return a finite positive asymptotic variance."))
  }
  se <- sqrt(variance)

  critical <- stats::qnorm(1 - (1 - level) / 2)
  fisher_se <- se / (1 - coefficient^2)
  if (!is.finite(fisher_se) || fisher_se <= 0) {
    return(unavailable(coefficient, "The Fisher-z delta standard error was not finite and positive."))
  }
  statistic <- atanh(coefficient) / fisher_se
  ci <- tanh(atanh(coefficient) + c(-1, 1) * critical * fisher_se)
  p <- 2 * stats::pnorm(abs(statistic), lower.tail = FALSE)
  inference_method <- "polycor two-step asymptotic SE (thresholds treated as fixed); Fisher-z delta Wald CI and p"

  list(
    coefficient = coefficient,
    se = se,
    statistic = statistic,
    p = p,
    ci = ci,
    ci_method = inference_method,
    inference_status = "ok",
    inference_reason = ""
  )
}

correlation_sig <- function(p) {
  if (!is.finite(p)) return("")
  if (p < .001) return("***")
  if (p < .01) return("**")
  if (p < .05) return("*")
  ""
}

correlation_normality_satisfied <- function(normality_table, name) {
  if (!is.data.frame(normality_table) || nrow(normality_table) == 0 || !"Name" %in% names(normality_table)) {
    return(FALSE)
  }
  if (identical(class(normality_table), "data.frame") && "normal" %in% names(normality_table) &&
      is.logical(normality_table$normal) && is.null(attributes(normality_table$normal)) &&
      length(name) <= 1L) {
    matched <- as.character(normality_table$Name) == as.character(name)
    # An earlier NA selector creates the first NA row in the original subset.
    first <- match(TRUE, is.na(matched) | matched)
    if (is.na(first) || first > nrow(normality_table) || is.na(matched[[first]])) return(FALSE)
    return(isTRUE(normality_table$normal[[first]]))
  }
  row <- normality_table[as.character(normality_table$Name) == as.character(name), , drop = FALSE]
  if (nrow(row) == 0 || !"normal" %in% names(row)) {
    return(FALSE)
  }
  isTRUE(row$normal[[1]])
}

correlation_method_for_pair <- function(
  x_measure,
  y_measure,
  continuous_method = "auto",
  x_name = NULL,
  y_name = NULL,
  normality_table = NULL,
  normality_checked = FALSE,
  normality_lookup = NULL
) {
  pair <- sort(c(x_measure, y_measure))
  if (identical(pair, c("continuous", "continuous"))) {
    method <- as.character(continuous_method %||% "auto")
    method <- if (method %in% c("auto", "pearson", "spearman", "kendall")) method else "auto"
    if (identical(method, "auto")) {
      x_normal <- isTRUE(normality_checked) && if (is.null(normality_lookup)) correlation_normality_satisfied(normality_table, x_name) else isTRUE(normality_lookup[x_name])
      y_normal <- isTRUE(normality_checked) && if (is.null(normality_lookup)) correlation_normality_satisfied(normality_table, y_name) else isTRUE(normality_lookup[y_name])
      if (isTRUE(x_normal) && isTRUE(y_normal)) {
        return(list(method = "pearson", label = "Pearson", reason = "Auto selected Pearson because both continuous variables satisfied normality."))
      }
      return(list(method = "spearman", label = "Spearman", reason = "Auto selected Spearman because at least one continuous variable did not satisfy normality."))
    }
    label <- switch(method, pearson = "Pearson", spearman = "Spearman", kendall = "Kendall")
    return(list(method = method, label = label, reason = sprintf("%s was selected for two continuous variables.", label)))
  }
  if (identical(pair, c("binary", "binary"))) {
    return(list(method = "phi", label = "Phi", reason = "Phi was selected for two binary variables."))
  }
  if (all(pair %in% c("continuous", "binary"))) {
    return(list(method = "point_biserial", label = "Point-biserial", reason = "Point-biserial was selected for a continuous variable and a binary variable."))
  }
  if (all(pair %in% c("continuous", "ordered"))) {
    return(list(method = "spearman", label = "Spearman", reason = "Spearman was selected because one variable is ordinal; polyserial can be added as an advanced option."))
  }
  if (identical(pair, c("ordered", "ordered"))) {
    return(list(method = "spearman", label = "Spearman", reason = "Spearman was selected for two ordinal variables; polychoric can be added as an advanced option."))
  }
  if (all(pair %in% c("binary", "ordered"))) {
    return(list(method = "spearman", label = "Spearman", reason = "Spearman was selected for binary-ordinal variables; polychoric can be added as an advanced option."))
  }
  if (any(pair == "category") && any(pair == "continuous")) {
    return(list(method = "eta", label = "Eta", reason = "Eta was selected for a nominal and continuous variable; ANOVA is recommended for detailed group comparison."))
  }
  if (all(pair %in% c("binary", "category", "ordered"))) {
    return(list(method = "cramers_v", label = "Cramer's V", reason = "Cramer's V was selected for categorical variables."))
  }
  list(method = "spearman", label = "Spearman", reason = "Spearman was selected as a stable fallback for mixed measurement levels.")
}

correlation_latent_method_for_pair <- function(x_measure, y_measure) {
  pair <- sort(c(x_measure, y_measure))
  if (identical(pair, c("continuous", "continuous"))) {
    return(list(method = "pearson", label = "Pearson", reason = "Pearson was retained for two continuous variables in the latent-variable set."))
  }
  if (all(pair %in% c("continuous", "ordered", "binary")) && any(pair == "continuous") && any(pair %in% c("ordered", "binary"))) {
    return(list(method = "polyserial", label = "Polyserial", reason = "Polyserial was selected for one continuous variable and one ordinal/binary variable."))
  }
  if (identical(pair, c("binary", "binary"))) {
    return(list(method = "tetrachoric", label = "Tetrachoric", reason = "Tetrachoric was selected for two binary variables."))
  }
  if (all(pair %in% c("ordered", "binary"))) {
    return(list(method = "polychoric", label = "Polychoric", reason = "Polychoric was selected for ordinal/binary variables."))
  }
  if (any(pair == "category") && any(pair == "continuous")) {
    return(list(method = "eta", label = "Eta", reason = "Eta was retained because latent-variable correlations are not applied to nominal-continuous pairs."))
  }
  if (all(pair %in% c("binary", "category", "ordered"))) {
    return(list(method = "cramers_v", label = "Cramer's V", reason = "Cramer's V was retained because latent-variable correlations are not applied to nominal categorical pairs."))
  }
  list(method = "spearman", label = "Spearman", reason = "Spearman was retained as a stable fallback for this measurement-level combination.")
}

correlation_rank_test <- function(limit, cache_unique = FALSE) {
  entries <- list()
  cached_test <- NULL
  cached_rank <- function(x, ...) {
    if (length(list(...)) || !typeof(x) %in% c("integer", "double") || !is.null(attributes(x))) return(base::rank(x, ...))
    for (i in seq_along(entries)) {
      if (identical(entries[[i]]$x, x, num.eq = FALSE)) {
        hit <- entries[[i]]
        if (is.null(hit$rank)) hit$rank <- base::rank(x)
        entries <<- c(entries[-i], list(hit))
        return(hit$rank)
      }
    }
    value <- base::rank(x)
    entries <<- c(if (length(entries) >= limit) entries[-1L] else entries, list(list(x = x, rank = value)))
    value
  }
  cached_unique <- function(x, ...) {
    if (length(list(...)) || !typeof(x) %in% c("integer", "double") || !is.null(attributes(x))) return(base::unique(x, ...))
    for (i in seq_along(entries)) {
      if (identical(entries[[i]]$x, x, num.eq = FALSE)) {
        if (is.null(entries[[i]]$unique)) entries[[i]]$unique <<- base::unique(x)
        return(entries[[i]]$unique)
      }
    }
    value <- base::unique(x)
    entries <<- c(if (length(entries) >= limit) entries[-1L] else entries, list(list(x = x, unique = value)))
    value
  }
  function(x, y, method, exact) {
    if (!identical(method, "spearman") || !is.null(attributes(x)) || !is.null(attributes(y))) {
      return(stats::cor.test(x, y, method = method, exact = exact))
    }
    if (is.null(cached_test)) {
      test <- utils::getS3method("cor.test", "default", envir = asNamespace("stats"))
      scope <- new.env(parent = environment(test))
      scope$rank <- cached_rank
      if (isTRUE(cache_unique)) scope$unique <- cached_unique
      environment(test) <- scope
      cached_test <<- test
    }
    cached_test(x, y, method = method, exact = exact)
  }
}

correlation_kendall_tau <- function(x, y) {
  if (!is.numeric(x) || !is.numeric(y) || !is.null(attributes(x)) || !is.null(attributes(y)) ||
      length(x) != length(y) || length(x) < 512L || length(x) > 10000L ||
      any(!is.finite(x)) || any(!is.finite(y))) return(NULL)
  xx <- unique(x)
  yy <- unique(y)
  if (length(xx) < 2L || length(yy) < 2L || length(xx) > 64L || length(yy) > 64L) return(NULL)
  xx <- sort(xx)
  yy <- sort(yy)
  counts <- matrix(tabulate(match(x, xx) + (match(y, yy) - 1L) * length(xx),
    nbins = length(xx) * length(yy)), nrow = length(xx))
  previous <- numeric(length(yy))
  numerator <- 0
  for (i in seq_along(xx)) {
    cumulative <- cumsum(previous)
    lower <- c(0, head(cumulative, -1L))
    higher <- sum(previous) - cumulative
    numerator <- numerator + sum(counts[i, ] * (lower - higher))
    previous <- previous + counts[i, ]
  }
  n <- as.double(length(x))
  n0 <- n * (n - 1) / 2
  nx <- rowSums(counts)
  ny <- colSums(counts)
  denominator_x <- n0 - sum(nx * (nx - 1) / 2)
  denominator_y <- n0 - sum(ny * (ny - 1) / 2)
  # Keep the doubled counts and separate square roots: regrouping changes bits.
  value <- 2 * numerator / (sqrt(2 * denominator_x) * sqrt(2 * denominator_y))
  max(-1, min(1, value))
}

correlation_kendall_cor <- function(x, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman")) {
  value <- if (identical(method, "kendall") && identical(use, "everything")) correlation_kendall_tau(x, y) else NULL
  if (is.null(value)) stats::cor(x, y, use = use, method = method) else value
}

correlation_build_kendall_engine <- function(reference = utils::getS3method("cor.test", "default", envir = asNamespace("stats")),
    version = as.character(getRversion()), platform = R.version$platform) {
  code_hash <- function(f) digest::digest(paste(deparse(f), collapse = "\n"), algo = "sha256", serialize = FALSE)
  if (!identical(version, "4.5.3") || !identical(platform, "x86_64-w64-mingw32") ||
      !identical(code_hash(reference), "2c8258e44bdccc3f46cab94e96783070f1a16f30589d502dfc4f32004d9ef01e") ||
      !identical(code_hash(stats::cor), "3b8301e8d8ae3cf639f2e0b56e029050761b7ca3869c5d9c92d58e3692da2eb6")) return(stats::cor.test)
  scope <- new.env(parent = environment(reference))
  scope$cor <- correlation_kendall_cor
  environment(reference) <- scope
  reference
}

correlation_kendall_engine <- local({
  engine <- NULL
  function() {
    if (is.null(engine)) engine <<- correlation_build_kendall_engine()
    engine
  }
})

correlation_test_result <- function(x, y, method, label, rank_test = NULL) {
  pair <- correlation_complete_pair(x, y)
  x <- pair$x
  y <- pair$y
  n <- pair$n
  if (n < 3 || stats::sd(x) == 0 || stats::sd(y) == 0) {
    return(list(n = n, coefficient = NA_real_, p = NA_real_, ci = c(NA_real_, NA_real_), method = method, label = label))
  }
  cor_method <- if (identical(method, "point_biserial")) "pearson" else method
  test <- try(if (identical(cor_method, "spearman") && is.function(rank_test)) {
    rank_test(x, y, method = cor_method, exact = FALSE)
  } else if (identical(cor_method, "kendall")) {
    correlation_kendall_engine()(x, y, method = cor_method, exact = FALSE)
  } else stats::cor.test(x, y, method = cor_method, exact = FALSE), silent = TRUE)
  if (inherits(test, "try-error")) {
    return(list(n = n, coefficient = NA_real_, p = NA_real_, ci = c(NA_real_, NA_real_), method = method, label = label))
  }
  coefficient <- unname(as.numeric(test$estimate[[1]]))
  ci <- if (!is.null(test$conf.int)) {
    as.numeric(test$conf.int[1:2])
  } else if (cor_method %in% c("pearson", "spearman", "kendall")) {
    correlation_ci(coefficient, n, method = cor_method)
  } else {
    c(NA_real_, NA_real_)
  }
  list(n = n, coefficient = coefficient, p = as.numeric(test$p.value), ci = ci, method = method, label = label)
}

correlation_phi_result <- function(x, y, label = "Phi") {
  correlation_test_result(x, y, "pearson", label)
}

correlation_cramers_v_result <- function(x, y) {
  pair <- correlation_complete_pair(x, y)
  x <- droplevels(factor(pair$x))
  y <- droplevels(factor(pair$y))
  n <- length(x)
  if (n < 3 || nlevels(x) < 2 || nlevels(y) < 2) {
    return(list(n = n, coefficient = NA_real_, p = NA_real_, ci = c(NA_real_, NA_real_), method = "cramers_v", label = "Cramer's V"))
  }
  table <- table(x, y)
  test <- suppressWarnings(try(stats::chisq.test(table, correct = FALSE), silent = TRUE))
  if (inherits(test, "try-error")) {
    return(list(n = n, coefficient = NA_real_, p = NA_real_, ci = c(NA_real_, NA_real_), method = "cramers_v", label = "Cramer's V"))
  }
  min_dim <- min(nrow(table) - 1, ncol(table) - 1)
  coefficient <- if (min_dim > 0) sqrt(as.numeric(test$statistic) / (n * min_dim)) else NA_real_
  list(n = n, coefficient = coefficient, p = as.numeric(test$p.value), ci = c(NA_real_, NA_real_), method = "cramers_v", label = "Cramer's V")
}

correlation_eta_result <- function(continuous, group) {
  pair <- correlation_complete_pair(continuous, group)
  values <- as.numeric(pair$x)
  groups <- droplevels(factor(pair$y))
  n <- length(values)
  if (n < 3 || stats::sd(values) == 0 || nlevels(groups) < 2) {
    return(list(n = n, coefficient = NA_real_, p = NA_real_, ci = c(NA_real_, NA_real_), method = "eta", label = "Eta"))
  }
  grand <- mean(values)
  ss_total <- sum((values - grand)^2)
  means <- tapply(values, groups, mean)
  counts <- stats::setNames(tabulate(groups, nbins = nlevels(groups)), levels(groups))
  ss_between <- sum(as.numeric(counts) * (means[names(counts)] - grand)^2)
  eta <- if (ss_total > 0) sqrt(ss_between / ss_total) else NA_real_
  # This single-factor model needs the lm fit and the unchanged ANOVA summary.
  fit <- try(structure(stats::lm(values ~ groups), class = c("aov", "lm")), silent = TRUE)
  p <- if (inherits(fit, "try-error")) {
    NA_real_
  } else {
    anova <- summary(fit)[[1]]
    as.numeric(anova[["Pr(>F)"]][[1]])
  }
  list(n = n, coefficient = eta, p = p, ci = c(NA_real_, NA_real_), method = "eta", label = "Eta")
}

correlation_ordered_vector <- function(values, levels, cache = NULL, name = NULL) {
  cacheable <- is.environment(cache) && is.character(name) && !is.object(name) &&
    length(name) == 1L && !is.na(name) && nzchar(name) &&
    is.numeric(values) && is.null(attributes(values)) &&
    (is.character(levels) || is.numeric(levels)) && is.null(attributes(levels))
  if (!cacheable) return(ordered(values, levels = levels))
  if (exists(name, envir = cache, inherits = FALSE)) {
    entry <- cache[[name]]
    if (identical(entry$values, values, num.eq = FALSE) && identical(entry$levels, levels, num.eq = FALSE)) {
      return(entry$ordered)
    }
  }
  quiet <- TRUE
  result <- withCallingHandlers({
    distinct <- if (length(values) >= 128L && typeof(values) %in% c("integer", "double")) unique(values) else NULL
    if (!is.null(distinct) && length(distinct) <= 64L && all(is.finite(distinct))) {
      ordered(distinct, levels = levels)[match(values, distinct)]
    } else {
      ordered(values, levels = levels)
    }
  },
    warning = function(w) quiet <<- FALSE, message = function(m) quiet <<- FALSE)
  if (quiet) cache[[name]] <- list(values = values, levels = levels, ordered = result)
  result
}

correlation_polyserial_result <- function(continuous, ordinal, label = "Polyserial", levels = NULL, ordered_cache = NULL, ordinal_name = NULL) {
  pair <- correlation_complete_pair(continuous, ordinal)
  x <- as.numeric(pair$x)
  resolved_levels <- levels %||% if (is.factor(pair$y)) base::levels(pair$y) else frequency_value_order(pair$y)
  y <- correlation_ordered_vector(pair$y, resolved_levels, ordered_cache, ordinal_name)
  n <- pair$n
  if (n < 4 || stats::sd(x) == 0 || length(unique(y)) < 2 || !requireNamespace("polycor", quietly = TRUE)) {
    return(list(n = n, coefficient = NA_real_, se = NA_real_, statistic = NA_real_, p = NA_real_, ci = c(NA_real_, NA_real_), ci_method = "", inference_status = "unavailable", inference_reason = "Polyserial correlation requires sufficient variation, at least four complete cases, and the polycor package.", method = "polyserial", label = label))
  }
  fit_warning <- character(0)
  fit <- withCallingHandlers(
    try(polycor::polyserial(x, y, ML = FALSE, std.err = TRUE), silent = TRUE),
    warning = function(condition) {
      fit_warning <<- c(fit_warning, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )
  if (inherits(fit, "try-error")) {
    return(list(n = n, coefficient = NA_real_, se = NA_real_, statistic = NA_real_, p = NA_real_, ci = c(NA_real_, NA_real_), ci_method = "", inference_status = "unavailable", inference_reason = "polycor could not estimate the polyserial correlation.", method = "polyserial", label = label))
  }
  inference <- correlation_polycor_inference(fit)
  if (length(fit_warning) > 0L && identical(inference$inference_status, "ok")) {
    inference$inference_status <- "ok_with_warning"
    inference$inference_reason <- paste(unique(fit_warning), collapse = " ")
  }
  list(
    n = n,
    coefficient = inference$coefficient,
    se = inference$se,
    statistic = inference$statistic,
    p = inference$p,
    ci = inference$ci,
    ci_method = inference$ci_method,
    inference_status = inference$inference_status,
    inference_reason = inference$inference_reason,
    method = "polyserial",
    label = label
  )
}

correlation_build_polychor_engine <- function() {
 fallback <- list(fit=polycor::polychor,cached=FALSE)
 result <- tryCatch({
 if (!identical(as.character(utils::packageVersion('polycor')),'0.8.2') ||
     !identical(as.character(utils::packageVersion('mvtnorm')),'1.3.3')) return(fallback)
 mv<-asNamespace('mvtnorm');pc<-asNamespace('polycor')
 refs<-list(polychor=get('polychor',pc),binBvn=get('binBvn',pc),pmvnorm=get('pmvnorm',mv),checkmvArgs=get('checkmvArgs',mv),chkcorr=get('chkcorr',mv))
 expected<-c(polychor='f3a8b4b411ba32ebd0f29aaca0f3f789f11a09724a195c8ce2f865d803999f1d',binBvn='494e513c6af9939c76b34663c4ed796bd8a26fe6a14cbce4edb8c9c4e60b19cf',pmvnorm='2b4738ab105eebe7801b8c78fab22f44ec38cd6606e36fdf91a1bae01ecae305',checkmvArgs='839f6e632233c2511ea4b8c7ffc515b18e49de2d0d99ceaa144f52f8f9683602',chkcorr='b3d4b6d1597a1f71569014809b535197259f86deb80f974731f1f5b2643ac40a')
 actual<-vapply(refs,function(f)digest::digest(list(formals(f),body(f)),algo='sha256'),character(1))
 if(!identical(actual,expected))return(fallback)
 check_env<-new.env(parent=mv);prob_env<-new.env(parent=mv);poly_env<-new.env(parent=pc)
 last<-NULL;last_result<-NULL;hits<-misses<-0L
 original<-get('chkcorr',mv)
 cached_check<-function(x) {
  if(!is.object(x)&&identical(x,last,num.eq=FALSE)&&!is.null(last_result)) {
   hits<<-hits+1L;return(last_result)
  }
  misses<<-misses+1L;quiet<-TRUE
  result<-withCallingHandlers(original(x),warning=function(w)quiet<<-FALSE,message=function(m)quiet<<-FALSE)
  if(quiet&&!is.object(x)){last<<-x;last_result<<-result}
  result
 }
 check_env$chkcorr<-cached_check
 check<-get('checkmvArgs',mv);environment(check)<-check_env;prob_env$checkmvArgs<-check
 prob<-get('pmvnorm',mv);environment(prob)<-prob_env;poly_env$cached_pmvnorm<-prob
 replace<-function(x) {
  if(identical(x,quote(mvtnorm::pmvnorm)))return(as.name('cached_pmvnorm'))
  if(is.call(x))for(i in seq_along(x))x[i]<-list(replace(x[[i]]))
  x
 }
 bin<-get('binBvn',pc);body(bin)<-replace(body(bin));environment(bin)<-poly_env
 poly_env$binBvn<-compiler::cmpfun(bin)
 fit<-get('polychor',pc);environment(fit)<-poly_env
 list(fit=function(...) {
   last<<-NULL;last_result<<-NULL
   on.exit({last<<-NULL;last_result<<-NULL})
   fit(...)
  },check=cached_check,counts=function()c(hits=hits,misses=misses),cached=TRUE)
 },error=function(e)NULL,warning=function(w)NULL)
 if(is.null(result))fallback else result
}

correlation_cached_polychor <- local({
  engine <- NULL
  function(...) {
    if (is.null(engine)) engine <<- correlation_build_polychor_engine()
    engine$fit(...)
  }
})
correlation_polychoric_result <- function(x, y, method = "polychoric", label = "Polychoric", x_levels = NULL, y_levels = NULL, ordered_cache = NULL, x_name = NULL, y_name = NULL) {
  pair <- correlation_complete_pair(x, y)
  resolved_x_levels <- x_levels %||% if (is.factor(pair$x)) base::levels(pair$x) else frequency_value_order(pair$x)
  resolved_y_levels <- y_levels %||% if (is.factor(pair$y)) base::levels(pair$y) else frequency_value_order(pair$y)
  x <- correlation_ordered_vector(pair$x, resolved_x_levels, ordered_cache, x_name)
  y <- correlation_ordered_vector(pair$y, resolved_y_levels, ordered_cache, y_name)
  n <- pair$n
  if (n < 4 || length(unique(x)) < 2 || length(unique(y)) < 2 || !requireNamespace("polycor", quietly = TRUE)) {
    return(list(n = n, coefficient = NA_real_, se = NA_real_, statistic = NA_real_, p = NA_real_, ci = c(NA_real_, NA_real_), ci_method = "", inference_status = "unavailable", inference_reason = "Polychoric/tetrachoric correlation requires sufficient variation, at least four complete cases, and the polycor package.", method = method, label = label))
  }
  fit_warning <- character(0)
  fit <- withCallingHandlers(
    try(correlation_cached_polychor(x, y, ML = FALSE, std.err = TRUE), silent = TRUE),
    warning = function(condition) {
      fit_warning <<- c(fit_warning, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )
  if (inherits(fit, "try-error")) {
    return(list(n = n, coefficient = NA_real_, se = NA_real_, statistic = NA_real_, p = NA_real_, ci = c(NA_real_, NA_real_), ci_method = "", inference_status = "unavailable", inference_reason = "polycor could not estimate the polychoric/tetrachoric correlation.", method = method, label = label))
  }
  inference <- correlation_polycor_inference(fit)
  if (length(fit_warning) > 0L && identical(inference$inference_status, "ok")) {
    inference$inference_status <- "ok_with_warning"
    inference$inference_reason <- paste(unique(fit_warning), collapse = " ")
  }
  list(
    n = n,
    coefficient = inference$coefficient,
    se = inference$se,
    statistic = inference$statistic,
    p = inference$p,
    ci = inference$ci,
    ci_method = inference$ci_method,
    inference_status = inference$inference_status,
    inference_reason = inference$inference_reason,
    method = method,
    label = label
  )
}

correlation_pair_result <- function(
  data,
  x_name,
  y_name,
  x_measure,
  y_measure,
  continuous_method = "auto",
  normality_table = NULL,
  normality_checked = FALSE,
  category_table = NULL,
  prepared_vectors = NULL,
  normality_lookup = NULL,
  rank_test = NULL
) {
  selection <- correlation_method_for_pair(
    x_measure,
    y_measure,
    continuous_method,
    x_name = x_name,
    y_name = y_name,
    normality_table = normality_table,
    normality_checked = normality_checked,
    normality_lookup = normality_lookup
  )
  x <- prepared_vectors[[x_name]] %||% correlation_analysis_vector(data[[x_name]], x_measure, name = x_name, category_table = category_table)
  y <- prepared_vectors[[y_name]] %||% correlation_analysis_vector(data[[y_name]], y_measure, name = y_name, category_table = category_table)

  result <- switch(
    selection$method,
    pearson = correlation_test_result(x, y, "pearson", selection$label),
    spearman = correlation_test_result(x, y, "spearman", selection$label, rank_test),
    kendall = correlation_test_result(x, y, "kendall", selection$label),
    point_biserial = correlation_test_result(x, y, "point_biserial", selection$label),
    phi = correlation_phi_result(x, y, selection$label),
    cramers_v = correlation_cramers_v_result(x, y),
    eta = {
      if (identical(x_measure, "continuous")) correlation_eta_result(x, y) else correlation_eta_result(y, x)
    },
    correlation_test_result(x, y, "spearman", "Spearman", rank_test)
  )
  result$reason <- selection$reason
  result$type1 <- correlation_measurement_label(x_measure)
  result$type2 <- correlation_measurement_label(y_measure)
  result
}

correlation_latent_pair_result <- function(data, x_name, y_name, x_measure, y_measure, category_table = NULL, prepared_vectors = NULL, level_cache = NULL, ordered_cache = NULL) {
  selection <- correlation_latent_method_for_pair(x_measure, y_measure)
  x <- prepared_vectors[[x_name]] %||% correlation_analysis_vector(data[[x_name]], x_measure, name = x_name, category_table = category_table)
  y <- prepared_vectors[[y_name]] %||% correlation_analysis_vector(data[[y_name]], y_measure, name = y_name, category_table = category_table)
  value_order <- function(values, name) {
    cacheable <- is.environment(level_cache) && is.character(name) && length(name) == 1L &&
      !is.na(name) && nzchar(name) && !is.null(prepared_vectors[[name]]) && is.null(attributes(values))
    if (cacheable && exists(name, envir = level_cache, inherits = FALSE)) return(level_cache[[name]])
    quiet <- TRUE
    levels <- withCallingHandlers(frequency_value_order(values, name = name, category_table = category_table),
      warning = function(w) quiet <<- FALSE, message = function(m) quiet <<- FALSE)
    if (cacheable && quiet) level_cache[[name]] <- levels
    levels
  }
  # Continuous scores never supply ordinal thresholds to the selected estimator.
  x_levels <- if (identical(x_measure, "continuous") && is.null(category_table) &&
                  is.numeric(x) && is.null(attributes(x)) && is.character(x_name) &&
                  !is.object(x_name) && length(x_name) == 1L && !is.na(x_name)) NULL else
    value_order(x, x_name)
  y_levels <- if (identical(y_measure, "continuous") && is.null(category_table) &&
                  is.numeric(y) && is.null(attributes(y)) && is.character(y_name) &&
                  !is.object(y_name) && length(y_name) == 1L && !is.na(y_name)) NULL else
    value_order(y, y_name)
  result <- switch(
    selection$method,
    pearson = correlation_test_result(x, y, "pearson", selection$label),
    polyserial = {
      if (identical(x_measure, "continuous")) {
        correlation_polyserial_result(x, y, selection$label, levels = y_levels, ordered_cache = ordered_cache, ordinal_name = y_name)
      } else {
        correlation_polyserial_result(y, x, selection$label, levels = x_levels, ordered_cache = ordered_cache, ordinal_name = x_name)
      }
    },
    polychoric = correlation_polychoric_result(x, y, "polychoric", selection$label, x_levels = x_levels, y_levels = y_levels, ordered_cache = ordered_cache, x_name = x_name, y_name = y_name),
    tetrachoric = correlation_polychoric_result(x, y, "tetrachoric", selection$label, x_levels = x_levels, y_levels = y_levels, ordered_cache = ordered_cache, x_name = x_name, y_name = y_name),
    cramers_v = correlation_cramers_v_result(x, y),
    eta = {
      if (identical(x_measure, "continuous")) correlation_eta_result(x, y) else correlation_eta_result(y, x)
    },
    correlation_test_result(x, y, "spearman", "Spearman")
  )
  inference_reason <- trimws(as.character(result$inference_reason %||% ""))
  result$reason <- paste(
    c(selection$reason, if (nzchar(inference_reason)) paste0("Inference note: ", inference_reason) else NULL),
    collapse = " "
  )
  result$type1 <- correlation_measurement_label(x_measure)
  result$type2 <- correlation_measurement_label(y_measure)
  result
}

correlation_matrix_from_pairs <- function(variables, display_names, pair_results, value = "coefficient") {
  matrix <- matrix(NA_real_, nrow = length(variables), ncol = length(variables))
  dimnames(matrix) <- list(unname(display_names[variables]), unname(display_names[variables]))
  if (length(pair_results) == 0) {
    return(matrix[0, 0, drop = FALSE])
  }
  for (item in pair_results) {
    i <- match(item$x_name, variables)
    j <- match(item$y_name, variables)
    if (!is.na(i) && !is.na(j)) {
      cell_value <- if (identical(value, "p")) item$result$p else item$result$coefficient
      matrix[i, j] <- cell_value
      matrix[j, i] <- cell_value
    }
  }
  matrix
}

correlation_ci_matrix_from_pairs <- function(variables, display_names, pair_results, prepared_ci = NULL) {
  matrix <- matrix("", nrow = length(variables), ncol = length(variables))
  dimnames(matrix) <- list(unname(display_names[variables]), unname(display_names[variables]))
  if (length(pair_results) == 0) {
    return(matrix[0, 0, drop = FALSE])
  }
  pair_index <- 0L
  for (item in pair_results) {
    pair_index <- pair_index + 1L
    i <- match(item$x_name, variables)
    j <- match(item$y_name, variables)
    if (!is.na(i) && !is.na(j) && all(is.finite(item$result$ci))) {
      cell_value <- prepared_ci[[pair_index]] %||% sprintf("%s~%s", format_decimal3(item$result$ci[[1]]), format_decimal3(item$result$ci[[2]]))
      matrix[i, j] <- cell_value
      matrix[j, i] <- cell_value
    }
  }
  matrix
}

correlation_method_matrix_from_pairs <- function(variables, display_names, pair_results) {
  matrix <- matrix("", nrow = length(variables), ncol = length(variables))
  dimnames(matrix) <- list(unname(display_names[variables]), unname(display_names[variables]))
  if (length(pair_results) == 0) {
    return(matrix[0, 0, drop = FALSE])
  }
  for (item in pair_results) {
    i <- match(item$x_name, variables)
    j <- match(item$y_name, variables)
    if (!is.na(i) && !is.na(j)) {
      matrix[i, j] <- item$result$label %||% ""
      matrix[j, i] <- item$result$label %||% ""
    }
  }
  matrix
}

correlation_pair_rows_and_matrices <- function(data, variables, display_names, measurements, pair_results, label_prefix = "") {
  rows <- list()
  ci_labels <- vector("list", length(pair_results))
  for (item in pair_results) {
    x_name <- item$x_name
    y_name <- item$y_name
    pair <- item$result
    rows[[length(rows) + 1]] <- list(
      Variable1 = display_names[[x_name]],
      Variable2 = display_names[[y_name]],
      Type1 = pair$type1,
      Type2 = pair$type2,
      N = pair$n,
      Method = pair$label,
      r = format_decimal3(pair$coefficient),
      p = format_p(pair$p),
      `95% CI` = if (all(is.finite(pair$ci))) {
        quiet <- TRUE
        ci_label <- withCallingHandlers(
          sprintf("%s~%s", format_decimal3(pair$ci[[1]]), format_decimal3(pair$ci[[2]])),
          warning = function(w) quiet <<- FALSE,
          message = function(m) quiet <<- FALSE
        )
        if (quiet && !is.object(pair$ci)) ci_labels[[length(rows) + 1L]] <- ci_label
        ci_label
      } else {
        ""
      },
      Sig = correlation_sig(pair$p),
      Reason = pair$reason
    )
  }
  # Format in the original pair order, then build the complete table once.
  pairwise_table <- if (length(rows)) {
    columns <- lapply(seq_along(rows[[1L]]), function(index) {
      unlist(lapply(rows, `[[`, index), use.names = FALSE)
    })
    names(columns) <- names(rows[[1L]])
    as.data.frame(columns, check.names = FALSE)
  } else data.frame()
  list(
    pairwise_table = pairwise_table,
    correlation_matrix = correlation_matrix_from_pairs(variables, display_names, pair_results),
    p_matrix = correlation_matrix_from_pairs(variables, display_names, pair_results, value = "p"),
    ci_matrix = correlation_ci_matrix_from_pairs(variables, display_names, pair_results, prepared_ci = ci_labels),
    method_matrix = correlation_method_matrix_from_pairs(variables, display_names, pair_results)
  )
}

prepare_correlation_results <- function(
  data,
  variables,
  variable_info = NULL,
  labels = character(0),
  category_table = NULL,
  options = list()
) {
  if (length(attr(data, "statedu_scope_excluded"))) analysis_scope_prepare_variables(data, environment(), c("variables"))
  requested_variables <- as.character(variables %||% character(0))
  variables <- intersect(requested_variables, names(data))
  if (length(variables) < 2) {
    missing <- setdiff(requested_variables, names(data))
    detail <- if (length(missing) > 0) {
      sprintf("Selected variables were not found in the active data: %s.", paste(head(missing, 5), collapse = ", "))
    } else {
      "Select at least two variables for correlation analysis."
    }
    stop(detail, call. = FALSE)
  }

  measurement_reader <- correlation_measurement_reader(variable_info)
  measurements <- stats::setNames(
    vapply(variables, measurement_reader, character(1)),
    variables
  )
  # Keep quiet preflight conversions for pairs in this analysis invocation.
  # Recompute diagnostic-emitting conversions so their conditions are preserved.
  prepared_vectors <- stats::setNames(vector("list", length(variables)), variables)
  counts <- vapply(variables, function(name) {
    quiet <- TRUE
    values <- withCallingHandlers(
      correlation_analysis_vector(data[[name]], measurements[[name]], name = name, category_table = category_table),
      warning = function(w) quiet <<- FALSE,
      message = function(m) quiet <<- FALSE
    )
    if (quiet) prepared_vectors[[name]] <<- values
    valid <- !is.na(values)
    valid_count <- sum(valid)
    valid_values <- if (valid_count == length(values) && is.null(attributes(values))) values else values[valid]
    c(valid = valid_count, unique = length(unique(valid_values)))
  }, c(valid = 0L, unique = 0L))
  valid_counts <- stats::setNames(counts["valid", ], variables)
  unique_counts <- stats::setNames(counts["unique", ], variables)
  keep_variables <- valid_counts >= 3 & unique_counts >= 2
  omitted_names <- names(valid_counts)[!keep_variables]
  variables <- names(valid_counts)[keep_variables]
  if (length(variables) < 2) {
    count_text <- paste(sprintf("%s=N %s, unique %s", names(valid_counts), valid_counts, unique_counts), collapse = "; ")
    stop(sprintf("At least two selected variables must have three or more valid values and at least two unique values. Current counts: %s.", count_text), call. = FALSE)
  }
  measurements <- measurements[variables]
  prepared_vectors <- prepared_vectors[variables]

  continuous_method <- as.character(options$continuous_method %||% "auto")
  if (!continuous_method %in% c("auto", "pearson", "spearman", "kendall")) {
    continuous_method <- "auto"
  }
  options$continuous_method <- continuous_method
  normality_checked <- isTRUE(options$normality) || identical(continuous_method, "auto")
  if (identical(continuous_method, "auto") && !isTRUE(options$normality)) {
    options$normality <- TRUE
    options$normality_for_auto <- TRUE
  }
  display_reader <- correlation_display_name_reader(variable_info, labels, category_table)
  normality_table <- if (isTRUE(normality_checked)) {
    correlation_normality_summary(data, variables, variable_info, labels, category_table, prepared_vectors, measurement_reader, display_reader)
  } else {
    data.frame()
  }
  display_names <- stats::setNames(
    vapply(variables, display_reader, character(1)),
    variables
  )
  use_latent_correlations <- isTRUE(options$latent_correlations)
  level_cache <- if (use_latent_correlations) new.env(parent = emptyenv()) else NULL
  ordered_cache <- if (use_latent_correlations) new.env(parent = emptyenv()) else NULL
  normality_lookup <- NULL
  if (identical(continuous_method, "auto") && !use_latent_correlations &&
      identical(class(normality_table), "data.frame") && all(c("Name", "normal") %in% names(normality_table)) &&
      is.character(normality_table$Name) && !is.object(normality_table$Name) &&
      is.logical(normality_table$normal) && is.null(attributes(normality_table$normal)) &&
      !anyNA(normality_table$Name) && !anyDuplicated(normality_table$Name) &&
      all(nzchar(normality_table$Name))) {
    normality_lookup <- stats::setNames(normality_table$normal, normality_table$Name)
  }
  pair_results <- list()
  rank_test <- if (!use_latent_correlations &&
    !(identical(continuous_method, "kendall") && all(measurements == "continuous"))) {
    correlation_rank_test(length(variables), cache_unique = all(valid_counts[variables] == nrow(data)))
  } else NULL
  for (i in seq_len(length(variables) - 1)) {
    for (j in seq.int(i + 1, length(variables))) {
      x_name <- variables[[i]]
      y_name <- variables[[j]]
      pair <- if (isTRUE(use_latent_correlations)) {
        correlation_latent_pair_result(data, x_name, y_name, measurements[[x_name]], measurements[[y_name]], category_table = category_table, prepared_vectors = prepared_vectors, level_cache = level_cache, ordered_cache = ordered_cache)
      } else {
        correlation_pair_result(
          data,
          x_name,
          y_name,
          measurements[[x_name]],
          measurements[[y_name]],
          continuous_method,
          normality_table = normality_table,
          normality_checked = normality_checked,
          category_table = category_table,
          prepared_vectors = prepared_vectors,
          normality_lookup = normality_lookup,
          rank_test = rank_test
        )
      }
      pair_results[[length(pair_results) + 1]] <- list(x_name = x_name, y_name = y_name, result = pair)
    }
  }
  primary <- correlation_pair_rows_and_matrices(data, variables, display_names, measurements, pair_results)

  list(
    variables = variables,
    labels = display_names,
    measurements = measurements,
    data = data[, variables, drop = FALSE],
    options = options,
    normality_table = normality_table,
    omitted_table = if (length(omitted_names) > 0) {
      data.frame(
        Variable = vapply(omitted_names, display_reader, character(1)),
        `Valid N` = as.integer(valid_counts[omitted_names]),
        `Unique values` = as.integer(unique_counts[omitted_names]),
        Reason = ifelse(
          valid_counts[omitted_names] < 3,
          "Omitted because fewer than three valid values were available.",
          "Omitted because fewer than two unique values were available."
        ),
        check.names = FALSE
      )
    } else {
      NULL
    },
    pairwise_table = primary$pairwise_table,
    correlation_matrix = primary$correlation_matrix,
    p_matrix = primary$p_matrix,
    ci_matrix = primary$ci_matrix,
    method_matrix = primary$method_matrix,
    latent = NULL,
    matrix_method = "heterogeneous"
  )
}
