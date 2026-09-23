# Shared retained-profile reporting helpers.
#
# The primary profile table and modal-assignment descriptives answer different
# questions.  These helpers keep their provenance explicit so that model
# standard errors can never be relabelled as modal-class standard deviations.

profile_or <- function(x, fallback) {
  if (is.null(x) || length(x) == 0L) fallback else x
}

profile_empty_model_result <- function(reason = "Model-estimated indicator means are unavailable.") {
  list(
    available = FALSE,
    reason = as.character(reason)[1],
    source_type = "retained_mplus_model",
    spread_type = "SE",
    data = data.frame()
  )
}

profile_mplus_number <- function(x) {
  x <- gsub("[dD]", "E", trimws(as.character(x)), perl = TRUE)
  suppressWarnings(as.numeric(x))
}

profile_parse_mplus_means <- function(lines = NULL,
                                      path = NULL,
                                      indicators,
                                      model_tag = NA_character_,
                                      expected_k = NULL,
                                      conf_level = 0.95,
                                      require_normal_termination = TRUE) {
  indicators <- unique(trimws(as.character(indicators)))
  indicators <- indicators[!is.na(indicators) & nzchar(indicators)]
  if (length(indicators) == 0L) {
    return(profile_empty_model_result("No continuous indicators were specified."))
  }

  if (is.null(lines)) {
    path <- as.character(profile_or(path, ""))[1]
    if (is.na(path)) path <- ""
    if (!nzchar(path) || !file.exists(path)) {
      return(profile_empty_model_result("The retained Mplus output file was not found."))
    }
    lines <- tryCatch(
      readLines(path, warn = FALSE, encoding = "UTF-8"),
      error = function(e) character(0)
    )
  }
  lines <- as.character(profile_or(lines, character(0)))
  if (length(lines) == 0L) {
    return(profile_empty_model_result("The retained Mplus output file is empty or unreadable."))
  }

  upper_all <- toupper(lines)
  if (isTRUE(require_normal_termination) &&
      !any(grepl("THE MODEL ESTIMATION TERMINATED NORMALLY", upper_all, fixed = TRUE))) {
    return(profile_empty_model_result("The retained Mplus model did not terminate normally."))
  }

  conf_level <- suppressWarnings(as.numeric(conf_level)[1])
  if (!is.finite(conf_level) || conf_level <= 0 || conf_level >= 1) conf_level <- 0.95
  critical <- stats::qnorm(1 - (1 - conf_level) / 2)

  indicator_key <- toupper(indicators)
  names(indicator_key) <- indicator_key
  indicator_original <- stats::setNames(indicators, indicator_key)

  in_results <- FALSE
  in_means <- FALSE
  current_class <- NA_integer_
  rows <- list()

  for (line in trimws(lines)) {
    if (!nzchar(line)) next
    upper <- toupper(line)

    if (grepl("MODEL RESULTS", upper, fixed = TRUE)) {
      if (isTRUE(in_results) && length(rows) > 0L) break
      in_results <- TRUE
      in_means <- FALSE
      current_class <- NA_integer_
      next
    }
    if (!isTRUE(in_results)) next

    if (grepl("^LATENT CLASS\\s+[0-9]+", upper, perl = TRUE)) {
      current_class <- suppressWarnings(as.integer(sub(
        "^LATENT CLASS\\s+([0-9]+).*$", "\\1", upper, perl = TRUE
      )))
      in_means <- FALSE
      next
    }
    if (identical(upper, "MEANS")) {
      in_means <- TRUE
      next
    }
    if (grepl(
      "^(VARIANCES|INTERCEPTS|THRESHOLDS|CATEGORICAL LATENT VARIABLES|QUALITY OF NUMERICAL RESULTS|STANDARDIZED MODEL RESULTS|TECHNICAL [0-9]+ OUTPUT|SAVEDATA INFORMATION)",
      upper,
      perl = TRUE
    )) {
      in_means <- FALSE
      if (grepl("^(STANDARDIZED MODEL RESULTS|TECHNICAL [0-9]+ OUTPUT|SAVEDATA INFORMATION)", upper, perl = TRUE) &&
          length(rows) > 0L) break
      next
    }
    if (!isTRUE(in_means) || is.na(current_class)) next

    parts <- strsplit(line, "\\s+", perl = TRUE)[[1]]
    if (length(parts) < 3L) next
    variable_key <- toupper(parts[[1]])
    if (!(variable_key %in% indicator_key)) next

    estimate <- profile_mplus_number(parts[[2]])
    standard_error <- profile_mplus_number(parts[[3]])
    if (!is.finite(estimate) || !is.finite(standard_error) || standard_error <= 0) next

    rows[[length(rows) + 1L]] <- data.frame(
      model_tag = as.character(model_tag)[1],
      class_num = as.integer(current_class),
      Class = paste0("Class ", current_class),
      class = paste0("Class ", current_class),
      var_name = unname(indicator_original[[variable_key]]),
      Mean = as.numeric(estimate),
      SE = as.numeric(standard_error),
      LLCI = as.numeric(estimate - critical * standard_error),
      ULCI = as.numeric(estimate + critical * standard_error),
      conf_level = conf_level,
      source_type = "retained_mplus_model",
      estimate_source = "retained_mplus_model",
      spread_type = "SE",
      stringsAsFactors = FALSE
    )
  }

  if (length(rows) == 0L) {
    return(profile_empty_model_result("No valid class-specific continuous-indicator means were found in the retained Mplus output."))
  }
  data <- do.call(rbind, rows)
  data$class_num <- suppressWarnings(as.integer(data$class_num))
  data$var_name <- as.character(data$var_name)

  expected_k <- suppressWarnings(as.integer(expected_k)[1])
  if (is.na(expected_k) || expected_k < 1L) expected_k <- max(data$class_num, na.rm = TRUE)
  expected_grid <- expand.grid(
    var_name = indicators,
    class_num = seq_len(expected_k),
    stringsAsFactors = FALSE
  )
  expected_grid <- expected_grid[, c("class_num", "var_name"), drop = FALSE]
  observed_key <- paste(data$class_num, toupper(data$var_name), sep = "||")
  expected_key <- paste(expected_grid$class_num, toupper(expected_grid$var_name), sep = "||")

  if (anyDuplicated(observed_key) || nrow(data) != nrow(expected_grid) ||
      !setequal(observed_key, expected_key)) {
    return(profile_empty_model_result(
      "Class-specific indicator means are incomplete or duplicated for the retained model."
    ))
  }

  data <- data[match(expected_key, observed_key), , drop = FALSE]
  rownames(data) <- NULL
  list(
    available = TRUE,
    reason = "",
    source_type = "retained_mplus_model",
    spread_type = "SE",
    data = data
  )
}

profile_resolve_retained_out_file <- function(best_tag,
                                              best_model_row = data.frame(),
                                              registry = data.frame(),
                                              fit_summary = data.frame(),
                                              search_dirs = character(0)) {
  tag <- trimws(as.character(profile_or(best_tag, ""))[1])
  if (is.na(tag)) tag <- ""
  if (!nzchar(tag)) {
    return(list(available = FALSE, path = NA_character_, reason = "The retained model tag is missing."))
  }

  paths_from_rows <- function(x) {
    if (!is.data.frame(x) || nrow(x) == 0L || !"out_file" %in% names(x)) return(character(0))
    tag_col <- intersect(c("model_tag", "best_tag", "tag"), names(x))
    if (length(tag_col) > 0L) {
      x <- x[as.character(x[[tag_col[[1]]]]) == tag, , drop = FALSE]
    } else {
      out_basename <- tools::file_path_sans_ext(basename(as.character(x$out_file)))
      x <- x[out_basename == tag, , drop = FALSE]
    }
    if (nrow(x) == 0L) return(character(0))

    if ("status" %in% names(x)) {
      status <- tolower(trimws(as.character(x$status)))
      x <- x[is.na(status) | !nzchar(status) | status %in% c("ok", "success", "completed"), , drop = FALSE]
    }
    if (nrow(x) == 0L) return(character(0))
    if ("parse_ok" %in% names(x)) {
      parse_ok <- x$parse_ok
      keep <- is.na(parse_ok) | parse_ok %in% TRUE
      x <- x[keep, , drop = FALSE]
    }
    if (nrow(x) == 0L) return(character(0))
    as.character(x$out_file)
  }

  candidates <- c(
    paths_from_rows(best_model_row),
    paths_from_rows(registry),
    paths_from_rows(fit_summary),
    file.path(as.character(search_dirs), paste0(tag, ".out"))
  )
  candidates <- unique(candidates[!is.na(candidates) & nzchar(trimws(candidates))])
  if (length(candidates) > 0L) {
    candidates <- candidates[file.exists(candidates)]
    if (length(candidates) > 0L) {
      sizes <- suppressWarnings(file.info(candidates)$size)
      candidates <- candidates[!is.na(sizes) & sizes > 0]
    }
  }
  if (length(candidates) == 0L) {
    return(list(
      available = FALSE,
      path = NA_character_,
      reason = paste0("No readable Mplus output matched retained model ", tag, ".")
    ))
  }
  list(available = TRUE, path = candidates[[1]], reason = "")
}

profile_modal_descriptives <- function(classified,
                                       continuous = character(0),
                                       categorical = character(0),
                                       weight_var = NULL,
                                       standardize = FALSE) {
  data <- if (is.data.frame(classified)) classified else data.frame()
  if (nrow(data) == 0L) return(data.frame())
  if (!"class_num" %in% names(data)) {
    class_col <- intersect(c("Class", "class", "profile", "Profile"), names(data))
    if (length(class_col) == 0L) return(data.frame())
    data$class_num <- suppressWarnings(as.integer(gsub("[^0-9]", "", as.character(data[[class_col[[1]]]]))))
  }
  data$class_num <- suppressWarnings(as.integer(data$class_num))
  data <- data[!is.na(data$class_num), , drop = FALSE]
  if (nrow(data) == 0L) return(data.frame())

  continuous <- intersect(unique(as.character(continuous)), names(data))
  categorical <- intersect(unique(as.character(categorical)), names(data))
  weight_name <- trimws(as.character(profile_or(weight_var, ""))[1])
  if (is.na(weight_name)) weight_name <- ""
  use_weights <- nzchar(weight_name) && weight_name %in% names(data)
  if (isTRUE(use_weights)) {
    data$.profile_weight <- suppressWarnings(as.numeric(data[[weight_name]]))
  } else {
    data$.profile_weight <- 1
  }

  if (isTRUE(standardize) && length(continuous) > 0L) {
    for (variable in continuous) {
      x <- suppressWarnings(as.numeric(data[[variable]]))
      mean_all <- mean(x, na.rm = TRUE)
      sd_all <- stats::sd(x, na.rm = TRUE)
      data[[variable]] <- if (is.finite(sd_all) && sd_all > 0) (x - mean_all) / sd_all else NA_real_
    }
  }

  weighted_sd <- function(x, w) {
    keep <- is.finite(x) & is.finite(w) & w > 0
    x <- x[keep]
    w <- w[keep]
    if (length(x) < 2L || sum(w) <= 0) return(NA_real_)
    mu <- sum(w * x) / sum(w)
    denom <- sum(w) - sum(w^2) / sum(w)
    if (!is.finite(denom) || denom <= 0) return(NA_real_)
    sqrt(sum(w * (x - mu)^2) / denom)
  }

  rows <- list()
  classes <- sort(unique(data$class_num))
  for (class_id in classes) {
    subset <- data[data$class_num == class_id, , drop = FALSE]
    weights <- suppressWarnings(as.numeric(subset$.profile_weight))
    for (variable in continuous) {
      x <- suppressWarnings(as.numeric(subset[[variable]]))
      keep <- is.finite(x) & is.finite(weights) & weights > 0
      if (!any(keep)) next
      mean_value <- sum(weights[keep] * x[keep]) / sum(weights[keep])
      rows[[length(rows) + 1L]] <- data.frame(
        class_num = class_id,
        Class = paste0("Class ", class_id),
        class = paste0("Class ", class_id),
        var_name = variable,
        indicator_type = "continuous",
        category = "",
        Mean = mean_value,
        SD = weighted_sd(x, weights),
        n = sum(keep),
        percent = NA_real_,
        source_type = "modal_class_descriptive",
        estimate_source = "modal_class_assignment",
        spread_type = "SD",
        stringsAsFactors = FALSE
      )
    }
    for (variable in categorical) {
      values <- subset[[variable]]
      valid <- !is.na(values) & is.finite(weights) & weights > 0
      levels <- sort(unique(as.character(values[valid])))
      if (length(levels) == 0L) next
      total_weight <- sum(weights[valid])
      for (level in levels) {
        level_hit <- valid & as.character(values) == level
        rows[[length(rows) + 1L]] <- data.frame(
          class_num = class_id,
          Class = paste0("Class ", class_id),
          class = paste0("Class ", class_id),
          var_name = variable,
          indicator_type = "categorical",
          category = level,
          Mean = NA_real_,
          SD = NA_real_,
          n = sum(level_hit),
          percent = if (total_weight > 0) 100 * sum(weights[level_hit]) / total_weight else NA_real_,
          source_type = "modal_class_descriptive",
          estimate_source = "modal_class_assignment",
          spread_type = if (isTRUE(use_weights)) "weighted_percent" else "percent",
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(rows) == 0L) return(data.frame())
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}
