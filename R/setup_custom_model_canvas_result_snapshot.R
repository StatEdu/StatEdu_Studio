# Custom model canvas result diagram snapshot helpers.

# Shared persistence helpers for interactive mediation/moderation, CFA, SEM,
# and PLS-SEM canvas results. Result files deliberately keep model and result
# snapshots separate so ordinary model files always reopen in edit mode.
canvas_analysis_result_type <- function(value) {
  value <- tolower(trimws(as.character(value %||% "")))
  if (identical(value, "sem")) value <- "cbsem"
  value
}

canvas_analysis_result_extension <- function(analysis_type) {
  switch(canvas_analysis_result_type(analysis_type), custom_mm = "stmmr", cfa = "stcfar", cbsem = "stsemr", plssem = "stplsr", "stresult")
}

canvas_analysis_result_label <- function(analysis_type, language = statedu_initial_language()) {
  ko <- identical(normalize_app_language(language), "ko")
  switch(
    canvas_analysis_result_type(analysis_type),
    custom_mm = if (ko) "매개·조절 사용자 정의 모델 분석 결과" else "Custom mediation/moderation analysis result",
    cfa = if (ko) "CFA 분석 결과" else "CFA analysis result",
    cbsem = if (ko) "SEM 분석 결과" else "SEM analysis result",
    plssem = if (ko) "PLS-SEM 분석 결과" else "PLS-SEM analysis result",
    if (ko) "분석 결과" else "Analysis result"
  )
}

canvas_analysis_result_request_path <- function(request, analysis_type, must_exist = FALSE) {
  path <- trimws(as.character(if (is.list(request)) request$path %||% "" else ""))
  if (!nzchar(path)) return("")
  extension <- canvas_analysis_result_extension(analysis_type)
  if (!identical(tolower(tools::file_ext(path)), extension)) {
    if (isTRUE(must_exist)) stop(sprintf("Expected a .%s result file.", extension), call. = FALSE)
    path <- paste0(path, ".", extension)
  }
  normalizePath(path, winslash = "/", mustWork = must_exist)
}

choose_canvas_analysis_result_path <- function(analysis_type, save = TRUE, language = statedu_initial_language()) {
  extension <- canvas_analysis_result_extension(analysis_type)
  label <- canvas_analysis_result_label(analysis_type, language)
  default_name <- paste0(gsub("[^A-Za-z0-9]+", "-", canvas_analysis_result_type(analysis_type)), "-result-", format(Sys.time(), "%Y%m%d-%H%M"), ".", extension)
  filter <- sprintf("%s (*.%s)|*.%s", label, extension, extension)
  action <- if (isTRUE(save)) if (identical(normalize_app_language(language), "ko")) "분석 결과 저장" else "Save analysis result" else if (identical(normalize_app_language(language), "ko")) "분석 결과 불러오기" else "Open analysis result"
  path <- if (isTRUE(save)) choose_windows_save_file(default_name, paste(action, label), filter, extension) else choose_windows_open_file(paste(action, label), filter)
  if (!is_dialog_path(path)) return("")
  canvas_analysis_result_request_path(list(path = path[[1L]]), analysis_type, must_exist = !isTRUE(save))
}

canvas_analysis_result_signature_canonical_value <- function(value, ignore_names = FALSE) {
  if (is.environment(value)) {
    return(list(
      storage_type = "environment",
      class = enc2utf8(as.character(class(value))),
      name = enc2utf8(as.character(environmentName(value) %||% ""))
    ))
  }
  if (isS4(value)) {
    slot_names <- sort(methods::slotNames(value))
    return(list(
      storage_type = "S4",
      class = enc2utf8(as.character(class(value))),
      slots = stats::setNames(
        lapply(slot_names, function(slot_name) {
          canvas_analysis_result_signature_canonical_value(methods::slot(value, slot_name))
        }),
        enc2utf8(slot_names)
      )
    ))
  }

  value_attributes <- attributes(value) %||% list()
  if (isTRUE(ignore_names)) value_attributes$names <- NULL
  if (length(value_attributes)) {
    attribute_names <- sort(names(value_attributes) %||% character(0))
    value_attributes <- stats::setNames(
      lapply(attribute_names, function(attribute_name) {
        canvas_analysis_result_signature_canonical_value(value_attributes[[attribute_name]])
      }),
      enc2utf8(attribute_names)
    )
  } else {
    value_attributes <- list()
  }

  plain_value <- value
  attributes(plain_value) <- NULL
  if (is.character(plain_value)) {
    plain_value <- enc2utf8(plain_value)
  } else if (is.list(plain_value) || is.pairlist(plain_value)) {
    plain_value <- lapply(plain_value, canvas_analysis_result_signature_canonical_value)
  } else if (is.language(plain_value)) {
    plain_value <- enc2utf8(paste(deparse(plain_value, width.cutoff = 500L), collapse = "\n"))
  }

  list(
    storage_type = typeof(value),
    values = plain_value,
    attributes = value_attributes
  )
}

canvas_analysis_result_signature_payload <- function(data) {
  if (is.null(data)) data <- data.frame()
  rows <- as.integer(NROW(data))
  columns <- as.integer(NCOL(data))
  variables <- enc2utf8(as.character(names(data) %||% colnames(data) %||% character(0)))

  data_attributes <- attributes(data) %||% list()
  for (attribute_name in c("names", "row.names", "dim", "dimnames", "class")) {
    data_attributes[[attribute_name]] <- NULL
  }
  if (length(data_attributes)) {
    attribute_names <- sort(names(data_attributes) %||% character(0))
    data_attributes <- stats::setNames(
      lapply(attribute_names, function(attribute_name) {
        canvas_analysis_result_signature_canonical_value(data_attributes[[attribute_name]])
      }),
      enc2utf8(attribute_names)
    )
  } else {
    data_attributes <- list()
  }

  column_values <- if (columns < 1L) {
    list()
  } else if (is.data.frame(data) || (is.list(data) && is.null(dim(data)))) {
    lapply(seq_len(columns), function(index) data[[index]])
  } else {
    lapply(seq_len(columns), function(index) data[, index, drop = TRUE])
  }

  list(
    canonicalization = "statedu-analysis-data-v1",
    rows = rows,
    columns = columns,
    variables = variables,
    data_storage_type = typeof(data),
    data_class = enc2utf8(as.character(class(data))),
    data_attributes = data_attributes,
    column_values = lapply(
      column_values,
      canvas_analysis_result_signature_canonical_value,
      ignore_names = TRUE
    )
  )
}

canvas_analysis_result_dataset_signature <- function(data) {
  payload <- canvas_analysis_result_signature_payload(data)
  list(
    signature_version = 2L,
    rows = payload$rows,
    columns = payload$columns,
    variables = payload$variables,
    algorithm = "sha256",
    canonicalization = payload$canonicalization,
    content_sha256 = digest::digest(
      payload,
      algo = "sha256",
      serialize = TRUE,
      serializeVersion = 3
    ),
    hash_scope = "Values, row order, column names, storage types, classes, missing values, and column/data attributes; row names excluded",
    match_policy = "content-sha256",
    legacy_structure_only = FALSE
  )
}

# PLS bootstrap paths and effect draws are transient inference workspaces.  A
# 50,000-resample run can otherwise make a .stplsr file hundreds of MB even
# though every user-facing table has already been summarized.  Keep the fitted
# model and analysis/validation data for result rendering, workbook export, and
# audit diagnostics; remove only the raw per-resample registries.  The walk is
# recursive because moderated-mediation MGA keeps compact group results below
# the invariance result.
canvas_analysis_result_compact_pls <- function(result) {
  transient_fields <- c(
    "draws",
    "statedu_boot_paths",
    "statedu_moderation_draws",
    "statedu_effect_draws",
    "boot_paths",
    "bootstrap_draws",
    "path_draws",
    "moderation_draws",
    "effect_draws",
    "specific_indirect_draws",
    "conditional_indirect_draws",
    "moderated_mediation_draws"
  )
  compact_value <- function(value) {
    if (!is.list(value) || is.data.frame(value)) return(value)
    value_names <- names(value)
    if (length(value_names)) {
      for (field in transient_fields) {
        while (field %in% (names(value) %||% character(0))) {
          value[[match(field, names(value))]] <- NULL
        }
      }
    }
    if (!length(value)) return(value)
    for (index in seq_along(value)) value[[index]] <- compact_value(value[[index]])
    value
  }
  compact_value(result)
}

canvas_analysis_result_package <- function(analysis_type, result, request = list(), data = NULL) {
  analysis_type <- canvas_analysis_result_type(analysis_type)
  stored_result <- if (identical(analysis_type, "plssem")) {
    canvas_analysis_result_compact_pls(result)
  } else {
    result
  }
  list(
    format = "statedu-canvas-analysis-result",
    format_version = 2L,
    analysis_type = analysis_type,
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    app_version = if (exists("saved_results_app_version", mode = "function")) saved_results_app_version() else "",
    data_signature = canvas_analysis_result_dataset_signature(data),
    source_snapshot = request$source %||% stored_result$snapshot %||% stored_result$custom_model_canvas_snapshot %||% NULL,
    result_snapshot = request$result %||% stored_result$custom_model_canvas_result_snapshot %||% NULL,
    result_snapshots = request$results %||% NULL,
    active_result_group_key = request$activeResultGroupKey %||% "overall",
    result = stored_result
  )
}

write_canvas_analysis_result <- function(path, package) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(package, path, compress = "gzip", version = 3)
  invisible(path)
}

read_canvas_analysis_result <- function(path, expected_type) {
  package <- readRDS(path)
  if (!is.list(package) || !identical(package$format %||% "", "statedu-canvas-analysis-result")) stop("This is not a StatEdu canvas analysis result file.", call. = FALSE)
  format_version <- as.integer(package$format_version %||% 0L)
  if (!format_version %in% c(1L, 2L)) stop("This analysis result file version is not supported.", call. = FALSE)
  actual <- canvas_analysis_result_type(package$analysis_type)
  expected <- canvas_analysis_result_type(expected_type)
  if (!identical(actual, expected)) stop(sprintf("This result belongs to %s, not %s.", actual, expected), call. = FALSE)
  if (is.null(package$result)) stop("The analysis result payload is missing.", call. = FALSE)
  signature <- package$data_signature %||% list()
  content_sha256 <- tolower(trimws(as.character(signature$content_sha256 %||% "")))
  content_signature <- identical(as.integer(signature$signature_version %||% 0L), 2L) &&
    identical(tolower(as.character(signature$algorithm %||% "")), "sha256") &&
    identical(as.character(signature$canonicalization %||% ""), "statedu-analysis-data-v1") &&
    grepl("^[0-9a-f]{64}$", content_sha256)
  if (identical(format_version, 2L) && !isTRUE(content_signature)) {
    stop("This analysis result file does not contain a valid data fingerprint.", call. = FALSE)
  }
  if (isTRUE(content_signature)) {
    package$data_signature$match_policy <- "content-sha256"
    package$data_signature$legacy_structure_only <- FALSE
  } else {
    package$data_signature$signature_version <- 1L
    package$data_signature$match_policy <- "legacy-structure-only"
    package$data_signature$legacy_structure_only <- TRUE
  }
  package
}

canvas_analysis_result_save_request <- function(request, analysis_type, result, data, language) {
  path <- canvas_analysis_result_request_path(request, analysis_type, must_exist = FALSE)
  if (!nzchar(path)) path <- choose_canvas_analysis_result_path(analysis_type, TRUE, language)
  if (!nzchar(path)) return("")
  write_canvas_analysis_result(path, canvas_analysis_result_package(analysis_type, result, request, data))
  path
}

canvas_analysis_result_signature_matches <- function(saved, data, format_version = NULL) {
  if (is.null(data)) return(TRUE)
  if (is.null(saved)) return(FALSE)
  current <- canvas_analysis_result_dataset_signature(data)
  structure_matches <- identical(as.integer(saved$rows %||% -1L), current$rows) &&
    identical(as.integer(saved$columns %||% -1L), current$columns) &&
    identical(as.character(saved$variables %||% character(0)), current$variables)
  if (!isTRUE(structure_matches)) return(FALSE)

  saved_hash <- tolower(trimws(as.character(saved$content_sha256 %||% "")))
  if (nzchar(saved_hash)) {
    return(
      identical(as.integer(saved$signature_version %||% 0L), current$signature_version) &&
        identical(tolower(as.character(saved$algorithm %||% "")), current$algorithm) &&
        identical(as.character(saved$canonicalization %||% ""), current$canonicalization) &&
        identical(saved_hash, tolower(current$content_sha256))
    )
  }

  # Version-1 files predate content fingerprints.  Preserve their historical
  # structure-only matching policy, but never silently downgrade a version-2
  # file whose fingerprint is missing or damaged.
  if (!is.null(format_version) && as.integer(format_version) >= 2L) return(FALSE)
  TRUE
}

canvas_analysis_result_load_request <- function(request, analysis_type, language, data = NULL) {
  path <- canvas_analysis_result_request_path(request, analysis_type, must_exist = TRUE)
  if (!nzchar(path)) path <- choose_canvas_analysis_result_path(analysis_type, FALSE, language)
  if (!nzchar(path)) return(NULL)
  package <- read_canvas_analysis_result(path, analysis_type)
  if (!canvas_analysis_result_signature_matches(package$data_signature, data, package$format_version)) stop("The loaded data do not match the data used for this analysis result.", call. = FALSE)
  list(path = path, package = package)
}

custom_model_canvas_edge_label_from_result <- function(result, equation, term, response = NULL) {
  custom_model_canvas_edge_info_from_result(result, equation, term, response = response)$label
}

custom_model_canvas_edge_info_from_result <- function(result, equation, term, response = NULL) {
  term <- as.character(term %||% "")[[1]]
  equation <- as.character(equation %||% "")[[1]]
  response <- as.character(response %||% "")[[1]]
  if (!nzchar(term) || !nzchar(equation)) {
    return(list(label = "", p = NA_real_, significant = FALSE, matched = FALSE))
  }
  for (path_result in result$path_results %||% list()) {
    if (!is.list(path_result)) next
    if (!identical(as.character(path_result$equation %||% "")[[1]], equation)) next
    if (nzchar(response)) {
      path_response <- tryCatch(all.vars(stats::formula(path_result$model))[[1]], error = function(e) "")
      if (!identical(path_response, response)) next
    }
    info <- mediation_moderation_path_coefficient_info(path_result, term)
    label <- as.character(info$label %||% "")[[1]]
    if (nzchar(label)) {
      info$matched <- TRUE
      return(info)
    }
  }
  list(label = "", p = NA_real_, significant = FALSE, matched = FALSE)
}

custom_model_canvas_result_edge_label <- function(result, from_node, to_node) {
  custom_model_canvas_result_edge_info(result, from_node, to_node)$label
}

custom_model_canvas_result_edge_info <- function(result, from_node, to_node) {
  from_role <- custom_model_canvas_record_value(from_node, "role", "")
  to_role <- custom_model_canvas_record_value(to_node, "role", "")
  from_var <- custom_model_canvas_node_variable(from_node)
  to_var <- custom_model_canvas_node_variable(to_node)
  if (identical(from_role, "independent") && identical(to_role, "mediator")) {
    return(custom_model_canvas_edge_info_from_result(result, paste("M model:", to_var), from_var, response = to_var))
  }
  if (identical(from_role, "mediator") && identical(to_role, "mediator")) {
    return(custom_model_canvas_edge_info_from_result(result, paste("M model:", to_var), from_var, response = to_var))
  }
  if (identical(from_role, "mediator") && identical(to_role, "dependent")) {
    return(custom_model_canvas_edge_info_from_result(result, "Y model", from_var, response = to_var))
  }
  if (identical(from_role, "independent") && identical(to_role, "dependent")) {
    return(custom_model_canvas_edge_info_from_result(result, "Y model", from_var, response = to_var))
  }
  list(label = "", p = NA_real_, significant = FALSE, matched = FALSE)
}

custom_model_canvas_result_moderation_label <- function(result, moderation, nodes, edge_by_id) {
  custom_model_canvas_result_moderation_info(result, moderation, nodes, edge_by_id)$label
}

custom_model_canvas_result_moderation_info <- function(result, moderation, nodes, edge_by_id) {
  source <- nodes[[custom_model_canvas_record_value(moderation, "from")]] %||% NULL
  target_edge <- edge_by_id[[custom_model_canvas_record_value(moderation, "toEdge")]] %||% NULL
  if (is.null(source) || is.null(target_edge)) {
    return(list(label = "", p = NA_real_, significant = FALSE, matched = FALSE))
  }
  from_node <- nodes[[custom_model_canvas_record_value(target_edge, "from")]] %||% NULL
  to_node <- nodes[[custom_model_canvas_record_value(target_edge, "to")]] %||% NULL
  if (is.null(from_node) || is.null(to_node)) {
    return(list(label = "", p = NA_real_, significant = FALSE, matched = FALSE))
  }
  moderator <- custom_model_canvas_node_variable(source)
  moderated_var <- custom_model_canvas_node_variable(from_node)
  from_role <- custom_model_canvas_record_value(from_node, "role", "")
  to_role <- custom_model_canvas_record_value(to_node, "role", "")
  if (identical(from_role, "independent") && identical(to_role, "mediator")) {
    return(custom_model_canvas_edge_info_from_result(
      result,
      paste("M model:", custom_model_canvas_node_variable(to_node)),
      paste0(moderated_var, ":", moderator),
      response = custom_model_canvas_node_variable(to_node)
    ))
  }
  if (identical(from_role, "mediator") && identical(to_role, "dependent")) {
    return(custom_model_canvas_edge_info_from_result(result, "Y model", paste0(moderated_var, ":", moderator), response = custom_model_canvas_node_variable(to_node)))
  }
  if (identical(from_role, "independent") && identical(to_role, "dependent")) {
    return(custom_model_canvas_edge_info_from_result(result, "Y model", paste0(moderated_var, ":", moderator), response = custom_model_canvas_node_variable(to_node)))
  }
  list(label = "", p = NA_real_, significant = FALSE, matched = FALSE)
}

custom_model_canvas_result_snapshot <- function(snapshot, result) {
  snapshot <- snapshot %||% list()
  snapshot$nonce <- NULL
  style <- snapshot$style %||% list()
  label_size <- custom_model_canvas_numeric_value(style$labelFontSize %||% style$fontSize, 12)
  if (is.na(label_size)) label_size <- 12
  nodes <- custom_model_canvas_records(snapshot$nodes)
  node_ids <- vapply(nodes, custom_model_canvas_record_value, character(1), key = "id")
  names(nodes) <- node_ids
  edges <- custom_model_canvas_records(snapshot$edges)
  edge_ids <- vapply(edges, custom_model_canvas_record_value, character(1), key = "id")
  names(edges) <- edge_ids
  edge_by_id <- edges

  edges <- lapply(edges, function(edge) {
    from_node <- nodes[[custom_model_canvas_record_value(edge, "from")]] %||% NULL
    to_node <- nodes[[custom_model_canvas_record_value(edge, "to")]] %||% NULL
    info <- if (is.null(from_node) || is.null(to_node)) {
      list(label = "", p = NA_real_, significant = FALSE, matched = FALSE)
    } else {
      custom_model_canvas_result_edge_info(result, from_node, to_node)
    }
    edge$label <- as.character(info$label %||% "")[[1]]
    edge$p <- custom_model_canvas_numeric_value(info$p, NA_real_)
    edge$significant <- isTRUE(info$significant) && nzchar(edge$label)
    edge$resultMatched <- isTRUE(info$matched)
    edge$dashEligible <- isTRUE(info$matched) && is.finite(edge$p)
    edge$labelPosition <- custom_model_canvas_numeric_value(edge$labelPosition, 50)
    if (is.na(edge$labelPosition)) edge$labelPosition <- 50
    edge$labelOffsetX <- custom_model_canvas_numeric_value(edge$labelOffsetX, 0)
    if (is.na(edge$labelOffsetX)) edge$labelOffsetX <- 0
    edge$labelOffsetY <- custom_model_canvas_numeric_value(edge$labelOffsetY, -10)
    if (is.na(edge$labelOffsetY)) edge$labelOffsetY <- -10
    edge$labelFontSize <- custom_model_canvas_numeric_value(edge$labelFontSize, label_size)
    if (is.na(edge$labelFontSize)) edge$labelFontSize <- label_size
    edge
  })

  moderations <- custom_model_canvas_records(snapshot$moderations)
  moderations <- lapply(moderations, function(moderation) {
    info <- custom_model_canvas_result_moderation_info(result, moderation, nodes, edge_by_id)
    moderation$label <- as.character(info$label %||% "")[[1]]
    moderation$p <- custom_model_canvas_numeric_value(info$p, NA_real_)
    moderation$significant <- isTRUE(info$significant) && nzchar(moderation$label)
    moderation$resultMatched <- isTRUE(info$matched)
    moderation$dashEligible <- isTRUE(info$matched) && is.finite(moderation$p)
    moderation$labelOffsetX <- custom_model_canvas_numeric_value(moderation$labelOffsetX, 0)
    if (is.na(moderation$labelOffsetX)) moderation$labelOffsetX <- 0
    moderation$labelOffsetY <- custom_model_canvas_numeric_value(moderation$labelOffsetY, -10)
    if (is.na(moderation$labelOffsetY)) moderation$labelOffsetY <- -10
    moderation$labelFontSize <- custom_model_canvas_numeric_value(moderation$labelFontSize, label_size)
    if (is.na(moderation$labelFontSize)) moderation$labelFontSize <- label_size
    moderation
  })

  snapshot$edges <- unname(edges)
  snapshot$moderations <- unname(moderations)
  snapshot$dashNonsignificant <- isTRUE(snapshot$dashNonsignificant %||% TRUE)
  snapshot
}
