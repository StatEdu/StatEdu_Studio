# ============================================================
# 13_r3step_core.R
# Pure helpers for Mplus automatic R3STEP
# ============================================================

`%||%` <- function(x, y) if (is.null(x)) y else x

r3step_empty_table <- function() {
  data.frame(
    analysis = character(0),
    model_type = character(0),
    method = character(0),
    best_k = integer(0),
    best_tag = character(0),
    model_structure = character(0),
    predictor = character(0),
    source_var = character(0),
    var_name = character(0),
    var_label = character(0),
    level = character(0),
    value_label = character(0),
    predictor_type = character(0),
    outcome_class = character(0),
    reference_class = character(0),
    reference_level = character(0),
    comparison = character(0),
    estimate = numeric(0),
    se = numeric(0),
    stat = numeric(0),
    p = numeric(0),
    p_fmt = character(0),
    sig = character(0),
    rrr = numeric(0),
    rrr_fmt = character(0),
    llci = numeric(0),
    llci_fmt = character(0),
    ulci = numeric(0),
    ulci_fmt = character(0),
    inp_file = character(0),
    out_file = character(0),
    stringsAsFactors = FALSE
  )
}

r3step_fmt_p <- function(p, digits = 3L) {
  p <- suppressWarnings(as.numeric(p))
  out <- rep(NA_character_, length(p))
  ok <- is.finite(p)
  out[ok] <- ifelse(
    p[ok] < 0.001,
    "<.001",
    formatC(p[ok], format = "f", digits = digits)
  )
  out
}

r3step_sig <- function(p) {
  p <- suppressWarnings(as.numeric(p))
  out <- rep("", length(p))
  out[is.finite(p) & p < 0.001] <- "***"
  out[is.finite(p) & p >= 0.001 & p < 0.01] <- "**"
  out[is.finite(p) & p >= 0.01 & p < 0.05] <- "*"
  out
}

r3step_make_alias_map <- function(spec, reserved = character(0), prefix = "R3X") {
  spec <- as.data.frame(spec, stringsAsFactors = FALSE)
  if (nrow(spec) == 0L || !"predictor" %in% names(spec)) {
    stop("R3STEP predictor specification is empty.", call. = FALSE)
  }

  predictor <- as.character(spec$predictor)
  if (anyNA(predictor) || any(!nzchar(trimws(predictor)))) {
    stop("R3STEP predictor names must be non-empty.", call. = FALSE)
  }
  if (anyDuplicated(toupper(predictor))) {
    stop("R3STEP predictor names must be unique.", call. = FALSE)
  }

  prefix <- toupper(gsub("[^A-Z0-9]", "", as.character(prefix)[1]))
  if (!nzchar(prefix)) prefix <- "R3X"
  prefix <- substr(prefix, 1L, 3L)

  used <- unique(toupper(as.character(reserved)))
  aliases <- character(length(predictor))
  cursor <- 1L

  for (i in seq_along(predictor)) {
    repeat {
      candidate <- sprintf("%s%04d", prefix, cursor)
      cursor <- cursor + 1L
      if (nchar(candidate) <= 8L && !candidate %in% used) break
    }
    aliases[i] <- candidate
    used <- c(used, candidate)
  }

  spec$mplus_alias <- aliases
  spec
}

r3step_wrap_statement <- function(keyword, vars, indent = "  ", width = 78L) {
  vars <- unique(as.character(vars))
  vars <- vars[!is.na(vars) & nzchar(trimws(vars))]
  if (length(vars) == 0L) return(character(0))

  first_prefix <- paste0(indent, keyword, " = ")
  continuation <- paste0(indent, "  ")
  current <- first_prefix
  out <- character(0)

  for (value in vars) {
    candidate <- if (identical(current, first_prefix)) {
      paste0(current, value)
    } else {
      paste(current, value)
    }
    if (nchar(candidate, type = "width") > width) {
      out <- c(out, current)
      current <- paste0(continuation, value)
    } else {
      current <- candidate
    }
  }

  c(out, paste0(current, ";"))
}

r3step_normalize_starts <- function(x, default = "500 100") {
  value <- paste(as.character(x %||% default), collapse = " ")
  value <- trimws(gsub("\\s+", " ", value))
  nums <- suppressWarnings(as.integer(strsplit(value, " ", fixed = TRUE)[[1]]))
  nums <- nums[is.finite(nums) & nums >= 0L]
  if (length(nums) >= 2L) return(paste(nums[1:2], collapse = " "))
  if (length(nums) == 1L) return(paste(nums[1], max(1L, floor(nums[1] / 5L))))
  default
}

r3step_model_lines <- function(
    mixture_type,
    model_structure,
    indicators_continuous = character(0),
    indicators_categorical = character(0)
) {
  mixture_type <- tolower(as.character(mixture_type)[1])
  model_structure <- tolower(as.character(model_structure)[1])
  cont <- unique(as.character(indicators_continuous))
  cont <- cont[!is.na(cont) & nzchar(cont)]

  if (mixture_type == "lca" || length(cont) == 0L) return("  %OVERALL%")

  if (model_structure == "model1") {
    return(c(
      "  %OVERALL%",
      paste0("  ", paste(cont, collapse = " "), ";"),
      paste0("  [", paste(cont, collapse = " "), "];" )
    ))
  }

  if (model_structure == "model2") {
    return(c(
      "  %OVERALL%",
      paste0("  [", paste(cont, collapse = " "), "];" )
    ))
  }

  if (model_structure == "model3") {
    pairs <- character(0)
    if (length(cont) >= 2L) {
      pairs <- utils::combn(
        cont,
        2L,
        FUN = function(z) paste0("  ", z[1], " WITH ", z[2], ";"),
        simplify = TRUE
      )
    }
    return(c(
      "  %OVERALL%",
      paste0("  ", paste(cont, collapse = " "), ";"),
      paste0("  [", paste(cont, collapse = " "), "];" ),
      pairs
    ))
  }

  c(
    "  %OVERALL%",
    paste0("  [", paste(cont, collapse = " "), "];" )
  )
}

r3step_build_input_lines <- function(
    title,
    data_file,
    data_names,
    indicators,
    predictor_aliases,
    categorical = character(0),
    best_k,
    mixture_type,
    model_structure,
    indicators_continuous = character(0),
    indicators_categorical = character(0),
    missing_code = -9999,
    estimator = "MLR",
    starts = "500 100",
    stiterations = 20L,
    processors = 4L,
    lrtstarts = NULL,
    id_var = NULL,
    weight_var = NULL,
    strata_var = NULL,
    cluster_var = NULL,
    output_options = character(0)
) {
  data_names <- unique(as.character(data_names))
  indicators <- unique(as.character(indicators))
  predictor_aliases <- unique(as.character(predictor_aliases))
  categorical <- intersect(unique(as.character(categorical)), indicators)

  if (length(indicators) == 0L) stop("R3STEP requires indicators.", call. = FALSE)
  if (length(predictor_aliases) == 0L) stop("R3STEP requires auxiliary predictors.", call. = FALSE)
  if (!all(c(indicators, predictor_aliases) %in% data_names)) {
    stop("R3STEP data names do not contain all indicators and auxiliary predictors.", call. = FALSE)
  }
  if (any(nchar(predictor_aliases) > 8L) || anyDuplicated(toupper(predictor_aliases))) {
    stop("Mplus R3STEP aliases must be unique and no longer than 8 characters.", call. = FALSE)
  }

  survey_names <- c(id_var, weight_var, strata_var, cluster_var)
  survey_names <- survey_names[!is.na(survey_names) & nzchar(survey_names)]
  if (!all(survey_names %in% data_names)) {
    stop("R3STEP data names do not contain all survey/id variables.", call. = FALSE)
  }

  variable_lines <- c(
    "VARIABLE:",
    r3step_wrap_statement("NAMES", data_names),
    r3step_wrap_statement("USEVARIABLES", indicators)
  )
  if (length(categorical) > 0L) {
    variable_lines <- c(variable_lines, r3step_wrap_statement("CATEGORICAL", categorical))
  }
  if (!is.null(id_var) && nzchar(id_var)) {
    variable_lines <- c(variable_lines, paste0("  IDVARIABLE = ", id_var, ";"))
  }
  if (!is.null(weight_var) && nzchar(weight_var)) {
    variable_lines <- c(variable_lines, paste0("  WEIGHT = ", weight_var, ";"))
  }
  if (!is.null(strata_var) && nzchar(strata_var)) {
    variable_lines <- c(variable_lines, paste0("  STRATIFICATION = ", strata_var, ";"))
  }
  if (!is.null(cluster_var) && nzchar(cluster_var)) {
    variable_lines <- c(variable_lines, paste0("  CLUSTER = ", cluster_var, ";"))
  }
  variable_lines <- c(
    variable_lines,
    paste0("  MISSING = ALL (", missing_code, ");"),
    paste0("  CLASSES = c(", as.integer(best_k), ");"),
    r3step_wrap_statement("AUXILIARY", c(predictor_aliases, "(R3STEP)"))
  )

  complex <- any(vapply(list(strata_var, cluster_var), function(x) {
    !is.null(x) && length(x) > 0L && !is.na(x[1]) && nzchar(as.character(x[1]))
  }, logical(1)))
  type_line <- if (complex) "  TYPE = MIXTURE COMPLEX;" else "  TYPE = MIXTURE;"

  analysis_lines <- c(
    "ANALYSIS:",
    type_line,
    paste0("  ESTIMATOR = ", toupper(as.character(estimator)[1]), ";"),
    paste0("  STARTS = ", r3step_normalize_starts(starts), ";"),
    paste0("  STITERATIONS = ", as.integer(stiterations), ";"),
    paste0("  PROCESSORS = ", as.integer(processors), ";")
  )
  lrtstarts <- trimws(paste(as.character(lrtstarts %||% ""), collapse = " "))
  if (nzchar(lrtstarts)) {
    analysis_lines <- c(analysis_lines, paste0("  LRTSTARTS = ", lrtstarts, ";"))
  }

  model_lines <- c(
    "MODEL:",
    r3step_model_lines(
      mixture_type = mixture_type,
      model_structure = model_structure,
      indicators_continuous = indicators_continuous,
      indicators_categorical = indicators_categorical
    )
  )

  output_options <- unique(trimws(as.character(output_options)))
  output_options <- output_options[nzchar(output_options)]
  output_lines <- if (length(output_options) > 0L) {
    c("OUTPUT:", paste0("  ", sub(";?$", ";", output_options)))
  } else {
    character(0)
  }

  c(
    paste0("TITLE: ", title, ";"),
    "",
    "DATA:",
    paste0("  FILE = ", gsub("\\\\", "/", as.character(data_file)[1]), ";"),
    "",
    variable_lines,
    "",
    analysis_lines,
    "",
    model_lines,
    if (length(output_lines) > 0L) c("", output_lines) else character(0)
  )
}

r3step_parse_number <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("D", "E", x, fixed = TRUE)
  x <- gsub("d", "e", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

r3step_parse_native_output <- function(
    lines,
    spec,
    best_k,
    best_tag = NA_character_,
    model_structure = NA_character_,
    inp_file = NA_character_,
    out_file = NA_character_,
    reference_class = NULL,
    require_normal_termination = TRUE
) {
  empty <- r3step_empty_table()
  fail <- function(reason, heading_found = FALSE, reference_class = NA_integer_) {
    list(
      table = empty,
      inference_available = FALSE,
      reason = as.character(reason)[1],
      heading_found = heading_found,
      reference_class = reference_class
    )
  }

  lines <- as.character(lines)
  if (length(lines) == 0L) return(fail("Mplus output is empty."))
  normalized <- toupper(trimws(gsub("\\s+", " ", lines)))

  fatal <- grepl("\\*\\*\\* ERROR|DID NOT TERMINATE NORMALLY|NO CONVERGENCE", normalized)
  if (any(fatal)) {
    return(fail(paste0("Mplus R3STEP failed: ", trimws(lines[which(fatal)[1]]))))
  }
  if (isTRUE(require_normal_termination) &&
      !any(grepl("MODEL ESTIMATION TERMINATED NORMALLY", normalized, fixed = TRUE))) {
    return(fail("Mplus did not report normal model termination."))
  }

  head_idx <- which(grepl(
    "TESTS OF CATEGORICAL LATENT VARIABLE MULTINOMIAL LOGISTIC REGRESSIONS",
    normalized,
    fixed = TRUE
  ))
  if (length(head_idx) == 0L) {
    return(fail("The native Mplus R3STEP result block was not found."))
  }

  start <- NA_integer_
  for (idx in head_idx) {
    probe <- seq.int(idx, min(length(lines), idx + 12L))
    proc <- probe[grepl("3-STEP PROCEDURE", normalized[probe], fixed = TRUE)]
    if (length(proc) > 0L) {
      start <- proc[1] + 1L
      break
    }
  }
  if (!is.finite(start)) {
    return(fail("The Mplus multinomial block was not identified as the 3-step procedure."))
  }

  spec <- as.data.frame(spec, stringsAsFactors = FALSE)
  required_spec <- c(
    "mplus_alias", "predictor", "source_var", "var_name", "var_label",
    "level", "value_label", "predictor_type", "reference_level"
  )
  for (nm in setdiff(required_spec, names(spec))) spec[[nm]] <- NA_character_
  spec$mplus_alias <- toupper(as.character(spec$mplus_alias))
  if (nrow(spec) == 0L || anyNA(spec$mplus_alias) || any(!nzchar(spec$mplus_alias)) ||
      anyDuplicated(spec$mplus_alias)) {
    return(fail("The R3STEP alias map is missing or invalid.", heading_found = TRUE))
  }

  major_heading <- function(x) {
    grepl(
      "^(MODEL RESULTS|QUALITY OF NUMERICAL RESULTS|TECHNICAL [0-9]+ OUTPUT|SAVEDATA INFORMATION|PLOT INFORMATION|CONFIDENCE INTERVALS|RESULTS IN PROBABILITY SCALE|LOGISTIC REGRESSION ODDS RATIO RESULTS|ODDS RATIOS|MODEL COMMAND|DIAGRAM INFORMATION|ESTIMATED SAMPLE STATISTICS|BEGINNING TIME)",
      x
    )
  }

  overall_end <- length(lines)
  heading_after <- which(seq_along(lines) > start & vapply(normalized, major_heading, logical(1)))
  if (length(heading_after) > 0L) overall_end <- heading_after[1] - 1L

  parameter_idx <- which(
    seq_along(lines) >= start & seq_along(lines) <= overall_end &
      grepl("^PARAMETERIZATION USING REFERENCE CLASS [0-9]+", normalized)
  )
  segment_starts <- c(start, parameter_idx + 1L)
  segment_ends <- c(parameter_idx - 1L, overall_end)
  segment_labels <- c(
    NA_integer_,
    suppressWarnings(as.integer(sub(
      "^PARAMETERIZATION USING REFERENCE CLASS ([0-9]+).*$",
      "\\1",
      normalized[parameter_idx]
    )))
  )

  parse_segment <- function(from, to, explicit_reference = NA_integer_) {
    rows <- list()
    current_class <- NA_integer_
    if (!is.finite(from) || !is.finite(to) || from > to) return(NULL)

    for (i in seq.int(from, to)) {
      text_i <- normalized[i]
      class_hit <- regexec("^C#([0-9]+)\\s+ON$", text_i, perl = TRUE)
      class_parts <- regmatches(text_i, class_hit)[[1]]
      if (length(class_parts) == 2L) {
        current_class <- suppressWarnings(as.integer(class_parts[2]))
        next
      }
      if (!is.finite(current_class) || !nzchar(text_i)) next

      parts <- strsplit(trimws(lines[i]), "\\s+")[[1]]
      if (length(parts) < 2L) next
      alias <- toupper(parts[1])
      spec_idx <- match(alias, spec$mplus_alias)
      if (is.na(spec_idx)) next

      nums <- r3step_parse_number(parts[-1])
      if (length(nums) < 2L || !is.finite(nums[1]) || !is.finite(nums[2]) || nums[2] <= 0) {
        return(list(valid = FALSE, reason = paste0("Invalid coefficient or SE for ", alias, ".")))
      }
      estimate <- nums[1]
      se <- nums[2]
      stat <- if (length(nums) >= 3L && is.finite(nums[3])) nums[3] else estimate / se
      p <- if (length(nums) >= 4L && is.finite(nums[4])) nums[4] else 2 * stats::pnorm(-abs(stat))
      if (!is.finite(stat) || !is.finite(p) || p < 0 || p > 1) {
        return(list(valid = FALSE, reason = paste0("Invalid Wald statistic or p-value for ", alias, ".")))
      }

      rows[[length(rows) + 1L]] <- data.frame(
        class_num = current_class,
        spec_idx = spec_idx,
        estimate = estimate,
        se = se,
        stat = stat,
        p = p,
        stringsAsFactors = FALSE
      )
    }

    if (length(rows) == 0L) return(NULL)
    parsed_i <- do.call(rbind, rows)
    parsed_key_i <- paste(parsed_i$class_num, parsed_i$spec_idx, sep = "::")
    if (anyDuplicated(parsed_key_i)) {
      return(list(valid = FALSE, reason = "Duplicate coefficients were found in an R3STEP parameterization."))
    }
    classes_i <- sort(unique(parsed_i$class_num))
    inferred_reference <- setdiff(seq_len(as.integer(best_k)), classes_i)
    if (length(inferred_reference) != 1L) {
      return(list(valid = FALSE, reason = "The R3STEP reference class was not uniquely identifiable."))
    }
    if (is.finite(explicit_reference) && explicit_reference != inferred_reference) {
      return(list(valid = FALSE, reason = "The R3STEP parameterization label and coefficient blocks disagree."))
    }

    expected_i <- expand.grid(
      class_num = setdiff(seq_len(as.integer(best_k)), inferred_reference),
      spec_idx = seq_len(nrow(spec)),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    expected_key_i <- paste(expected_i$class_num, expected_i$spec_idx, sep = "::")
    if (!setequal(expected_key_i, parsed_key_i)) {
      return(list(valid = FALSE, reason = "The native R3STEP coefficient block is incomplete."))
    }
    parsed_i <- parsed_i[match(expected_key_i, parsed_key_i), , drop = FALSE]
    list(valid = TRUE, data = parsed_i, reference = inferred_reference)
  }

  segments <- Map(parse_segment, segment_starts, segment_ends, segment_labels)
  valid_segment <- vapply(segments, function(x) is.list(x) && isTRUE(x$valid), logical(1))
  requested_reference <- suppressWarnings(as.integer(reference_class)[1])

  selected <- NA_integer_
  if (is.finite(requested_reference)) {
    selected <- which(valid_segment & vapply(segments, function(x) {
      is.list(x) && isTRUE(x$valid) && identical(as.integer(x$reference), requested_reference)
    }, logical(1)))[1]
  } else {
    selected <- which(valid_segment)[1]
  }

  if (!is.finite(selected)) {
    invalid_reasons <- unique(vapply(
      segments,
      function(x) if (is.list(x) && !isTRUE(x$valid)) as.character(x$reason %||% "") else "",
      character(1)
    ))
    invalid_reasons <- invalid_reasons[nzchar(invalid_reasons)]
    reason <- if (is.finite(requested_reference)) {
      paste0("No complete native R3STEP parameterization used reference Class ", requested_reference, ".")
    } else if (length(invalid_reasons) > 0L) {
      invalid_reasons[1]
    } else {
      "No valid coefficients were found in the native Mplus R3STEP block."
    }
    return(fail(reason, heading_found = TRUE, reference_class = requested_reference))
  }

  parsed <- segments[[selected]]$data
  reference <- as.integer(segments[[selected]]$reference)
  expected <- expand.grid(
    class_num = setdiff(seq_len(as.integer(best_k)), reference),
    spec_idx = seq_len(nrow(spec)),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  expected_key <- paste(expected$class_num, expected$spec_idx, sep = "::")
  parsed_key <- paste(parsed$class_num, parsed$spec_idx, sep = "::")
  parsed <- parsed[match(expected_key, parsed_key), , drop = FALSE]
  meta <- spec[parsed$spec_idx, , drop = FALSE]
  critical <- stats::qnorm(0.975)
  rrr <- exp(parsed$estimate)
  llci <- exp(parsed$estimate - critical * parsed$se)
  ulci <- exp(parsed$estimate + critical * parsed$se)
  reference_label <- paste0("Class ", reference)
  outcome_label <- paste0("Class ", parsed$class_num)

  out <- data.frame(
    analysis = "multivariable",
    model_type = "mplus_native_r3step",
    method = "Mplus automatic R3STEP (Vermunt correction)",
    best_k = as.integer(best_k),
    best_tag = as.character(best_tag)[1],
    model_structure = as.character(model_structure)[1],
    predictor = as.character(meta$predictor),
    source_var = as.character(meta$source_var),
    var_name = as.character(meta$var_name),
    var_label = as.character(meta$var_label),
    level = as.character(meta$level),
    value_label = as.character(meta$value_label),
    predictor_type = as.character(meta$predictor_type),
    outcome_class = outcome_label,
    reference_class = reference_label,
    reference_level = as.character(meta$reference_level),
    comparison = paste0(outcome_label, " vs ", reference_label),
    estimate = parsed$estimate,
    se = parsed$se,
    stat = parsed$stat,
    p = parsed$p,
    p_fmt = r3step_fmt_p(parsed$p),
    sig = r3step_sig(parsed$p),
    rrr = rrr,
    rrr_fmt = formatC(rrr, format = "f", digits = 3L),
    llci = llci,
    llci_fmt = formatC(llci, format = "f", digits = 3L),
    ulci = ulci,
    ulci_fmt = formatC(ulci, format = "f", digits = 3L),
    inp_file = as.character(inp_file)[1],
    out_file = as.character(out_file)[1],
    stringsAsFactors = FALSE
  )

  list(
    table = out,
    inference_available = TRUE,
    reason = NA_character_,
    heading_found = TRUE,
    reference_class = reference,
    parsed_until_line = segment_ends[selected]
  )
}
