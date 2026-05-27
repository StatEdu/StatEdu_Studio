# Model summary table helpers.

regression_method_label <- function(result) {
  if (isTRUE(result$use_hc3) && isTRUE(result$use_bootstrap)) {
    "Bootstrap + HC3 Regression"
  } else if (isTRUE(result$use_bootstrap)) {
    "Bootstrap Regression"
  } else if (isTRUE(result$use_hc3)) {
    "HC3 Regression"
  } else {
    "OLS Regression"
  }
}

package_version_label <- function(package) {
  version <- tryCatch(
    as.character(utils::packageVersion(package)),
    error = function(e) NA_character_
  )
  if (is.na(version) || !nzchar(version)) {
    package
  } else {
    sprintf("%s %s", package, version)
  }
}

regression_package_label <- function(result) {
  packages <- c("stats", "lmtest", "nortest")
  if (isTRUE(result$use_hc3) || isTRUE(result$use_bootstrap)) {
    packages <- c(packages, "sandwich")
  }
  paste(vapply(unique(packages), package_version_label, character(1)), collapse = "; ")
}

regression_bootstrap_value <- function(result, field) {
  if (!isTRUE(result$use_bootstrap)) {
    return("")
  }
  value <- result[[field]]
  if (is.null(value) || length(value) == 0 || is.na(value[[1]])) {
    return("")
  }
  if (identical(field, "bootstrap_seed")) {
    return(as.character(as.integer(value[[1]])))
  }
  format(as.integer(value[[1]]), big.mark = ",", scientific = FALSE, trim = TRUE)
}

model_overview_data_frame <- function(results, variable_table = NULL, labels = character(0)) {
  if (!is.list(results) || length(results) == 0) {
    return(data.frame())
  }
  dependents <- vapply(results, function(result) all.vars(result$formula)[[1]], character(1))
  dependent_labels <- vapply(
    dependents,
    display_variable_name_static,
    character(1),
    table = variable_table,
    labels = labels,
    label_only = TRUE
  )
  dependent_labels <- mapply(function(label, result) {
    if (!is.null(result$hierarchical_step) && nzchar(result$hierarchical_step)) {
      sprintf("%s %s", label, result$hierarchical_step)
    } else {
      label
    }
  }, dependent_labels, results, USE.NAMES = FALSE)
  rows <- c("N", "\ubd84\uc11d", "\uc0ac\uc720")
  regression_overview_reason <- function(result) {
    parts <- c(
      if (isTRUE(result$normality_p > .05)) "\uc815\uaddc\uc131 \ub9cc\uc871" else "\uc815\uaddc\uc131 \ubd88\ub9cc\uc871",
      if (isTRUE(result$homogeneity_p > .05)) "\ub4f1\ubd84\uc0b0\uc131 \ub9cc\uc871" else "\ub4f1\ubd84\uc0b0\uc131 \ubd88\ub9cc\uc871"
    )
    if (isTRUE(result$use_bootstrap)) {
      parts <- c(parts, "Bootstrap \uc0ac\uc6a9")
    }
    if (isTRUE(result$use_hc3)) {
      parts <- c(parts, "HC3 \uc0ac\uc6a9")
    }
    paste(parts[nzchar(parts)], collapse = "\n")
  }
  values <- lapply(results, function(result) {
    c(
      "N" = as.character(result$n),
      "\ubd84\uc11d" = result$method,
      "\uc0ac\uc720" = regression_overview_reason(result)
    )
  })
  table <- data.frame(Item = rows, stringsAsFactors = FALSE, check.names = FALSE)
  for (index in seq_along(values)) {
    table[[dependent_labels[[index]]]] <- unname(values[[index]][rows])
  }
  table
}

regression_assumption_review_data_frame <- function(results, variable_table = NULL, labels = character(0)) {
  if (!is.list(results) || length(results) == 0) {
    return(data.frame())
  }
  dependents <- vapply(results, function(result) all.vars(result$formula)[[1]], character(1))
  dependent_labels <- vapply(
    dependents,
    display_variable_name_static,
    character(1),
    table = variable_table,
    labels = labels,
    label_only = TRUE
  )
  dependent_labels <- mapply(function(label, result) {
    if (!is.null(result$hierarchical_step) && nzchar(result$hierarchical_step)) {
      sprintf("%s %s", label, result$hierarchical_step)
    } else {
      label
    }
  }, dependent_labels, results, USE.NAMES = FALSE)
  rows <- c(
    "\uc794\ucc28 \uc815\uaddc\uc131",
    "\uc794\ucc28 \ub4f1\ubd84\uc0b0\uc131",
    "\uc790\uae30\uc0c1\uad00",
    "\ud328\ud0a4\uc9c0"
  )
  values <- lapply(results, function(result) {
    c(
      "\uc794\ucc28 \uc815\uaddc\uc131" = sprintf(
        "K-S z=%s(%s)\n%s",
        format_decimal3(result$normality_statistic),
        format_p(result$normality_p),
        if (isTRUE(result$normality_p > .05)) "\uc815\uaddc\uc131 \ub9cc\uc871" else "\uc815\uaddc\uc131 \ubd88\ub9cc\uc871"
      ),
      "\uc794\ucc28 \ub4f1\ubd84\uc0b0\uc131" = sprintf(
        "%s=%s(%s)\n%s",
        stat_chisq_label(FALSE),
        format_decimal3(result$homogeneity_statistic),
        format_p(result$homogeneity_p),
        if (isTRUE(result$homogeneity_p > .05)) "\ub4f1\ubd84\uc0b0\uc131 \ub9cc\uc871" else "\ub4f1\ubd84\uc0b0\uc131 \ubd88\ub9cc\uc871"
      ),
      "\uc790\uae30\uc0c1\uad00" = sprintf(
        "d=%s\n%s",
        format_decimal3(result$dw_d),
        as.character(result$dw_result$Value[match("Decision", result$dw_result$Item)] %||% "")
      ),
      "\ud328\ud0a4\uc9c0" = regression_package_label(result)
    )
  })
  table <- data.frame(Item = rows, stringsAsFactors = FALSE, check.names = FALSE)
  for (index in seq_along(values)) {
    table[[dependent_labels[[index]]]] <- unname(values[[index]][rows])
  }
  table
}

combined_dw_data_frame <- function(results, variable_table = NULL, labels = character(0)) {
  if (!is.list(results) || length(results) == 0) {
    return(data.frame())
  }
  dependents <- vapply(results, function(result) all.vars(result$formula)[[1]], character(1))
  dependent_labels <- vapply(
    dependents,
    display_variable_name_static,
    character(1),
    table = variable_table,
    labels = labels,
    label_only = TRUE
  )
  rows <- as.character(results[[1]]$dw_result$Item)
  table <- data.frame(Item = rows, stringsAsFactors = FALSE, check.names = FALSE)
  for (index in seq_along(results)) {
    result <- results[[index]]
    values <- vapply(rows, function(row_name) {
      row_index <- match(row_name, result$dw_result$Item)
      if (is.na(row_index)) {
        return("")
      }
      value <- result$dw_result$Value[[row_index]]
      if (is.numeric(value)) {
        format_decimal3(value)
      } else {
        as.character(value %||% "")
      }
    }, character(1))
    table[[dependent_labels[[index]]]] <- values
  }
  table
}

