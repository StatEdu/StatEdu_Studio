# Auto-extracted shared functions for easyflow_statistics.

prepare_data <- function(data) {
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = TRUE)
  data[] <- lapply(data, function(x) {
    if (inherits(x, "haven_labelled_spss")) x <- haven::zap_missing(x)
    if (inherits(x, "haven_labelled")) x <- haven::zap_labels(x)
    if (is.character(x)) return(factor(x))
    x
  })
  data
}

read_sav_robust <- function(path) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop(
      "SPSS SAV files require the CRAN package 'haven'. Install it with install.packages(\"haven\").",
      call. = FALSE
    )
  }

  source_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  read_path <- source_path

  # Some Windows/R setups fail when haven reads an uploaded file from a
  # non-ASCII or extensionless temporary path. Copy to an ASCII .sav path first.
  tmp_path <- tempfile(pattern = "easyflow_sav_", fileext = ".sav")
  copied <- tryCatch(
    file.copy(source_path, tmp_path, overwrite = TRUE),
    error = function(e) FALSE
  )
  if (isTRUE(copied) && file.exists(tmp_path)) {
    read_path <- tmp_path
    on.exit(unlink(tmp_path), add = TRUE)
  }

  supports_encoding <- "encoding" %in% names(formals(haven::read_sav))
  encodings <- if (supports_encoding) {
    c(NA_character_, "UTF-8", "CP949", "EUC-KR", "latin1")
  } else {
    NA_character_
  }
  errors <- character(0)

  for (encoding in encodings) {
    encoding_label <- if (is.na(encoding)) "default" else encoding
    result <- tryCatch(
      {
        args <- list(file = read_path, user_na = TRUE)
        if (!is.na(encoding) && supports_encoding) {
          args$encoding <- encoding
        }
        do.call(haven::read_sav, args)
      },
      error = function(e) {
        errors <<- c(errors, sprintf("%s: %s", encoding_label, conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(result)) {
      return(result)
    }
  }

  stop(
    paste0(
      "Could not read the SPSS SAV file. Tried encodings: ",
      paste(ifelse(is.na(encodings), "default", encodings), collapse = ", "),
      ". Last errors: ",
      paste(utils::tail(errors, 3), collapse = " | ")
    ),
    call. = FALSE
  )
}

copy_data_file_for_reading <- function(path, original_name = path) {
  source_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  extension <- tolower(tools::file_ext(as.character(original_name %||% path)))
  fileext <- if (nzchar(extension)) paste0(".", extension) else ""
  tmp_path <- tempfile(pattern = "easyflow_data_", fileext = fileext)
  copied <- tryCatch(
    file.copy(source_path, tmp_path, overwrite = TRUE),
    error = function(e) FALSE
  )
  if (!isTRUE(copied) || !file.exists(tmp_path)) {
    stop(
      "Could not copy the data file to a local temporary file before reading. ",
      "Make sure the file is fully synced and not locked by OneDrive, Synology Drive, or another program.",
      call. = FALSE
    )
  }
  tmp_path
}

csv_encoding_score <- function(data) {
  text <- names(data)
  character_columns <- vapply(data, is.character, logical(1))
  if (any(character_columns)) {
    samples <- unlist(lapply(data[character_columns], function(column) utils::head(stats::na.omit(as.character(column)), 20)), use.names = FALSE)
    text <- c(text, samples)
  }
  text <- text[nzchar(text)]
  if (length(text) == 0) {
    return(0)
  }
  invalid_count <- sum(is.na(suppressWarnings(iconv(text, from = "", to = "UTF-8"))))
  replacement_count <- sum(suppressWarnings(grepl("\uFFFD", text, fixed = TRUE)))
  latin1_supplement_count <- sum(suppressWarnings(grepl("[\u00A0-\u00FF]", text)))
  cjk_count <- sum(suppressWarnings(grepl("[\u3130-\u318F\uAC00-\uD7AF]", text)))
  (cjk_count * 10) - (invalid_count * 1000) - (replacement_count * 100) - (latin1_supplement_count * 4)
}

repair_text_encoding <- function(values) {
  if (length(values) == 0) {
    return(values)
  }
  values <- as.character(values)
  repaired <- values
  broken <- is.na(suppressWarnings(iconv(repaired, from = "", to = "UTF-8"))) |
    suppressWarnings(grepl("\uFFFD", repaired, fixed = TRUE))
  if (!any(broken, na.rm = TRUE)) {
    return(values)
  }
  for (encoding in c("CP949", "EUC-KR", "UTF-8", "latin1")) {
    converted <- suppressWarnings(iconv(values[broken], from = encoding, to = "UTF-8"))
    usable <- !is.na(converted)
    if (any(usable)) {
      repaired[which(broken)[usable]] <- converted[usable]
      broken[which(broken)[usable]] <- FALSE
    }
    if (!any(broken, na.rm = TRUE)) {
      break
    }
  }
  repaired
}

normalize_text_encoding <- function(data) {
  if (is.null(data)) {
    return(data)
  }
  names(data) <- repair_text_encoding(names(data))
  data[] <- lapply(data, function(column) {
    if (is.character(column)) {
      return(repair_text_encoding(column))
    }
    column
  })
  data
}

read_csv_robust <- function(path, csv_header = TRUE) {
  encodings <- c("UTF-8", "UTF-8-BOM", "CP949", "EUC-KR", "latin1")
  best_result <- NULL
  best_score <- -Inf
  errors <- character(0)

  for (encoding in encodings) {
    result <- tryCatch(
      suppressWarnings(
        {
          read_encoding <- if (identical(encoding, "UTF-8-BOM")) "UTF-8" else encoding
          readr::read_csv(
            path,
            col_names = csv_header,
            locale = readr::locale(encoding = read_encoding),
            show_col_types = FALSE,
            progress = FALSE
          )
        }
      ),
      error = function(e) {
        errors <<- c(errors, sprintf("%s: %s", encoding, conditionMessage(e)))
        NULL
      }
    )
    if (is.null(result)) {
      next
    }
    score <- csv_encoding_score(result)
    if (score > best_score) {
      best_score <- score
      best_result <- result
    }
  }

  if (!is.null(best_result)) {
    return(normalize_text_encoding(best_result))
  }

  stop(
    paste0(
      "Could not read the CSV file. Tried encodings: ",
      paste(encodings, collapse = ", "),
      ". Last errors: ",
      paste(utils::tail(errors, 3), collapse = " | ")
    ),
    call. = FALSE
  )
}

read_excel_legacy <- function(path) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop(
      "Legacy Excel XLS files require the CRAN package 'readxl'. Install it with install.packages(\"readxl\").",
      call. = FALSE
    )
  }
  normalize_text_encoding(as.data.frame(readxl::read_excel(path), stringsAsFactors = FALSE, check.names = FALSE))
}

excel_data_file_extension <- function(name) {
  tolower(tools::file_ext(as.character(name %||% ""))) %in% c("xlsx", "xls")
}

normalize_excel_start_cell <- function(value = "A1") {
  value <- toupper(trimws(as.character(value %||% "A1")))
  if (length(value) == 0 || !nzchar(value[[1]])) {
    value <- "A1"
  }
  value <- value[[1]]
  if (!grepl("^[A-Z]+[1-9][0-9]*$", value)) {
    stop("Excel start cell must use A1 notation, for example A1 or B4.", call. = FALSE)
  }
  value
}

excel_sheet_names <- function(path, original_name = path) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop(
      "Excel sheet selection requires the CRAN package 'readxl'. Install it with install.packages(\"readxl\").",
      call. = FALSE
    )
  }
  read_path <- copy_data_file_for_reading(path, original_name)
  on.exit(unlink(read_path), add = TRUE)
  readxl::excel_sheets(read_path)
}

read_excel_configured <- function(path, sheet = NULL, start_cell = "A1", col_names = TRUE, n_max = Inf) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop(
      "Excel files with sheet/start-cell options require the CRAN package 'readxl'. Install it with install.packages(\"readxl\").",
      call. = FALSE
    )
  }
  if (!requireNamespace("cellranger", quietly = TRUE)) {
    stop(
      "Excel start-cell imports require the CRAN package 'cellranger'. Install it with install.packages(\"cellranger\").",
      call. = FALSE
    )
  }
  start_cell <- normalize_excel_start_cell(start_cell)
  args <- list(
    path = path,
    range = cellranger::anchored(start_cell, dim = c(NA, NA)),
    col_names = isTRUE(col_names)
  )
  if (!is.null(sheet) && nzchar(as.character(sheet %||% ""))) {
    args$sheet <- as.character(sheet)
  }
  if (is.finite(n_max)) {
    args$n_max <- as.integer(n_max)
  }
  normalize_text_encoding(as.data.frame(do.call(readxl::read_excel, args), stringsAsFactors = FALSE, check.names = FALSE))
}

read_excel_preview <- function(path, original_name, sheet = NULL, start_cell = "A1", col_names = TRUE, n_max = 20) {
  read_path <- copy_data_file_for_reading(path, original_name)
  on.exit(unlink(read_path), add = TRUE)
  utils::head(read_excel_configured(read_path, sheet = sheet, start_cell = start_cell, col_names = col_names), n_max)
}

read_sas_robust <- function(path, ext = "sas7bdat") {
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop(
      "SAS files require the CRAN package 'haven'. Install it with install.packages(\"haven\").",
      call. = FALSE
    )
  }
  result <- if (identical(ext, "xpt")) {
    haven::read_xpt(path)
  } else {
    haven::read_sas(path)
  }
  normalize_text_encoding(result)
}

read_stata_robust <- function(path) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop(
      "Stata DTA files require the CRAN package 'haven'. Install it with install.packages(\"haven\").",
      call. = FALSE
    )
  }
  normalize_text_encoding(haven::read_dta(path))
}

read_input_data <- function(
  path,
  original_name,
  csv_header = TRUE,
  dat_delimiter = "whitespace",
  dat_has_names = FALSE,
  excel_sheet = NULL,
  excel_start_cell = "A1",
  excel_col_names = TRUE
) {
  ext <- tolower(tools::file_ext(original_name))
  read_path <- copy_data_file_for_reading(path, original_name)
  on.exit(unlink(read_path), add = TRUE)

  if (identical(ext, "sav")) {
    return(normalize_text_encoding(read_sav_robust(read_path)))
  }

  if (identical(ext, "csv")) {
    return(read_csv_robust(read_path, csv_header = csv_header))
  }

  if (identical(ext, "xlsx") && (is.null(excel_sheet) || (!nzchar(excel_sheet))) && identical(normalize_excel_start_cell(excel_start_cell), "A1") && isTRUE(excel_col_names)) {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop(
        "Excel files require the CRAN package 'openxlsx'. Install it with install.packages(\"openxlsx\").",
        call. = FALSE
      )
    }
    return(normalize_text_encoding(openxlsx::read.xlsx(read_path, detectDates = TRUE)))
  }

  if (ext %in% c("xlsx", "xls")) {
    return(read_excel_configured(read_path, sheet = excel_sheet, start_cell = excel_start_cell, col_names = excel_col_names))
  }

  if (ext %in% c("sas7bdat", "xpt")) {
    return(read_sas_robust(read_path, ext = ext))
  }

  if (identical(ext, "dta")) {
    return(read_stata_robust(read_path))
  }

  if (identical(ext, "dat")) {
    if (identical(dat_delimiter, "comma")) {
      return(readr::read_delim(read_path, delim = ",", col_names = dat_has_names, show_col_types = FALSE, progress = FALSE))
    }
    if (identical(dat_delimiter, "tab")) {
      return(readr::read_tsv(read_path, col_names = dat_has_names, show_col_types = FALSE, progress = FALSE))
    }
    return(readr::read_table(read_path, col_names = dat_has_names, show_col_types = FALSE, progress = FALSE))
  }

  stop("Unsupported file type: .", ext, call. = FALSE)
}

supported_data_file_extension <- function(name) {
  tolower(tools::file_ext(as.character(name %||% ""))) %in% c("sav", "sas7bdat", "xpt", "dta", "csv", "dat", "xlsx", "xls")
}

valid_data_file_path <- function(path) {
  path <- as.character(path %||% "")
  nzchar(path) && file.exists(path) && !dir.exists(path)
}

valid_data_file_value <- function(file) {
  is.list(file) &&
    valid_data_file_path(file$path) &&
    supported_data_file_extension(file$name %||% file$path) &&
    !isTRUE(file$excel_pending)
}

current_data_file_value <- function(uploaded, active_file = NULL) {
  if (!is.null(uploaded)) {
    file <- list(path = uploaded$datapath, name = uploaded$name, restored = FALSE)
    if (valid_data_file_value(file)) {
      return(file)
    }
    return(NULL)
  }
  if (valid_data_file_value(active_file)) {
    return(active_file)
  }
  NULL
}

read_current_data_file <- function(file, input) {
  if (!valid_data_file_value(file)) {
    stop("No supported data file is available. Use a SAV, SAS, Stata, Excel, CSV, or DAT file.", call. = FALSE)
  }
  read_input_data(
    file$path,
    file$name,
    csv_header = input$header,
    dat_delimiter = input$dat_delimiter %||% "whitespace",
    dat_has_names = isTRUE(input$dat_has_names),
    excel_sheet = file$excel_sheet %||% NULL,
    excel_start_cell = file$excel_start_cell %||% "A1",
    excel_col_names = isTRUE(file$excel_col_names %||% TRUE)
  )
}

numeric_integer_like <- function(values, tolerance = sqrt(.Machine$double.eps)) {
  values <- stats::na.omit(as.numeric(values))
  length(values) > 0 && all(abs(values - round(values)) < tolerance)
}

infer_measurement <- function(x) {
  values <- stats::na.omit(as.vector(x))
  unique_n <- length(unique(values))

  if (is.logical(x)) return("binary")
  if (is.ordered(x)) return("ordered")
  if (is.factor(x)) return(if (nlevels(x) <= 2) "binary" else "category")
  if (is.character(x)) return(if (unique_n <= 2) "binary" else "category")
  if ((is.numeric(x) || is.integer(x)) && unique_n <= 2) return("binary")
  if ((is.numeric(x) || is.integer(x)) && unique_n <= 12 && numeric_integer_like(values)) return("category")
  "continuous"
}

variable_min <- function(x) {
  values <- stats::na.omit(as.vector(x))
  if (length(values) == 0 || !(is.numeric(values) || is.integer(values))) return("")
  as.character(min(values))
}

variable_max <- function(x) {
  values <- stats::na.omit(as.vector(x))
  if (length(values) == 0 || !(is.numeric(values) || is.integer(values))) return("")
  as.character(max(values))
}

variable_label <- function(x) {
  label <- attr(x, "label", exact = TRUE)
  if (is.null(label) || length(label) == 0 || is.na(label[[1]])) return("")
  as.character(label[[1]])
}

value_label_pairs <- function(x, prepared_x = x, max_pairs = 6) {
  labelled_values <- attr(x, "labels", exact = TRUE)
  if (!is.null(labelled_values) && length(labelled_values) > 0) {
    values <- as.character(unname(labelled_values))
    labels <- names(labelled_values)
  } else {
    values <- sort(unique(stats::na.omit(as.vector(prepared_x))))
    values <- as.character(utils::head(values, max_pairs))
    labels <- rep("", length(values))
  }

  output <- stats::setNames(rep("", max_pairs * 2), as.vector(rbind(paste0("value_", seq_len(max_pairs)), paste0("label_", seq_len(max_pairs)))))
  if (length(values) > 0) {
    for (i in seq_len(min(length(values), max_pairs))) {
      output[[paste0("value_", i)]] <- values[[i]]
      output[[paste0("label_", i)]] <- labels[[i]] %||% ""
    }
  }
  output
}

variable_summary_table <- function(data, input, raw_data = data) {
  raw_data <- as.data.frame(raw_data, stringsAsFactors = FALSE, check.names = TRUE)
  labels <- vapply(seq_along(data), function(i) variable_label(raw_data[[i]]), character(1))
  value_labels <- do.call(rbind, lapply(seq_along(data), function(i) {
    as.data.frame(as.list(value_label_pairs(raw_data[[i]], data[[i]])), stringsAsFactors = FALSE, check.names = FALSE)
  }))
  cbind(data.frame(
    source_order = seq_along(data),
    name = names(data),
    var_label = labels,
    measurement = vapply(data, infer_measurement, character(1)),
    storage_type = vapply(data, function(x) class(x)[1], character(1)),
    n_unique = vapply(data, function(x) length(unique(stats::na.omit(as.vector(x)))), integer(1)),
    n_missing = vapply(data, function(x) sum(is.na(x)), integer(1)),
    min_value = vapply(data, variable_min, character(1)),
    max_value = vapply(data, variable_max, character(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  ), value_labels)
}

apply_measurement_overrides <- function(table_data, overrides = character(0)) {
  if (length(overrides) == 0 || is.null(table_data) || nrow(table_data) == 0) {
    return(table_data)
  }

  matched <- table_data$name %in% names(overrides)
  table_data$measurement[matched] <- unname(overrides[table_data$name[matched]])
  table_data
}

apply_var_label_overrides_to_info <- function(info, labels = character(0)) {
  if (is.null(info) || nrow(info) == 0 || length(labels) == 0) {
    return(info)
  }
  matched <- info$name %in% names(labels)
  info$var_label[matched] <- unname(labels[info$name[matched]])
  info
}

apply_variable_overrides <- function(info, measurement_overrides = character(0), var_label_overrides = character(0)) {
  info <- apply_measurement_overrides(info, measurement_overrides)
  apply_var_label_overrides_to_info(info, var_label_overrides)
}

base_variable_info_value <- function(
  has_data_file = FALSE,
  data = NULL,
  input = NULL,
  raw_data = NULL,
  restored_info = NULL,
  measurement_overrides = character(0),
  var_label_overrides = character(0)
) {
  info <- if (isTRUE(has_data_file)) {
    variable_summary_table(data, input, raw_data)
  } else {
    restored_info
  }
  apply_variable_overrides(info, measurement_overrides, var_label_overrides)
}

ensure_variable_info_columns <- function(info) {
  if (is.null(info)) {
    return(NULL)
  }
  required <- c("source_order", "name", "var_label", "measurement", "storage_type", "n_unique", "n_missing", "min_value", "max_value")
  for (column in required) {
    if (!column %in% names(info)) {
      info[[column]] <- ""
    }
  }
  info
}

select_variable_info_source <- function(data_view = "info", selection_applied = FALSE, step3_info = NULL, base_info = NULL) {
  if (isTRUE(selection_applied) && !is.null(step3_info)) {
    return(step3_info)
  }
  base_info
}

prepare_variable_info_table <- function(info, labels = character(0)) {
  if (is.null(info)) {
    return(NULL)
  }
  info <- ensure_variable_info_columns(info)
  apply_var_label_overrides_to_info(info, labels)
}

variable_info_table_value <- function(
  data_view = "info",
  selection_applied = FALSE,
  step3_info = NULL,
  base_info = NULL,
  measurement_overrides = character(0),
  labels = character(0)
) {
  info <- select_variable_info_source(
    data_view = data_view,
    selection_applied = selection_applied,
    step3_info = step3_info,
    base_info = base_info
  )
  info <- apply_measurement_overrides(info, measurement_overrides)
  prepare_variable_info_table(info, labels)
}

data_step_value <- function(has_data_file = FALSE, has_restored_info = FALSE, active_step = "step1", selection_applied = FALSE) {
  if (!isTRUE(has_data_file) && !isTRUE(has_restored_info)) {
    return("load_data")
  }
  if (identical(active_step, "step3")) {
    return("category_labels")
  }
  if (isTRUE(selection_applied)) {
    return("category_labels")
  }
  "select_analysis_variables"
}

continuous_names_from_info <- function(info) {
  if (is.null(info) || nrow(info) == 0 || !all(c("name", "measurement") %in% names(info))) {
    return(character(0))
  }
  as.character(info$name[info$measurement == "continuous"])
}

available_names_from_data_state <- function(data = NULL, restored_info = NULL, has_data_file = FALSE) {
  if (isTRUE(has_data_file) && !is.null(data)) {
    return(names(data))
  }
  if (is.null(restored_info) || !"name" %in% names(restored_info)) {
    return(character(0))
  }
  as.character(restored_info$name)
}

clean_measurement_overrides <- function(values) {
  updates <- settings_name_value_pairs(values)
  if (length(updates) == 0) {
    updates <- settings_named_vector(values)
  }
  if (length(updates) == 0 || is.null(names(updates))) {
    return(character(0))
  }
  names(updates) <- trimws(names(updates))
  valid_names <- !is.na(names(updates)) & nzchar(names(updates))
  valid_values <- !is.na(updates) & updates %in% c("binary", "category", "ordered", "continuous")
  updates <- updates[valid_names & valid_values]
  if (length(updates) > 0) {
    names(updates) <- sub("^.*\\.", "", names(updates))
    names(updates) <- trimws(names(updates))
    updates <- updates[!is.na(names(updates)) & nzchar(names(updates))]
  }
  updates
}

clean_var_label_overrides <- function(values, allow_blank = TRUE) {
  labels <- settings_name_value_pairs(values)
  if (length(labels) == 0) {
    labels <- settings_named_vector(values)
  }
  if (length(labels) == 0 || is.null(names(labels))) {
    return(character(0))
  }
  labels <- labels[!is.na(names(labels)) & nzchar(names(labels))]
  if (!isTRUE(allow_blank)) {
    labels <- labels[nzchar(trimws(as.character(labels)))]
  }
  labels
}

merge_named_overrides <- function(current = character(0), updates = character(0), drop_blank_names = TRUE) {
  current <- current %||% character(0)
  updates <- updates %||% character(0)

  if (isTRUE(drop_blank_names) && length(current) > 0) {
    current <- current[!is.na(names(current)) & nzchar(trimws(names(current)))]
  }
  if (length(updates) == 0 || is.null(names(updates))) {
    return(list(values = current, changed = FALSE, updates = character(0)))
  }

  updates <- updates[!is.na(names(updates)) & nzchar(trimws(names(updates)))]
  if (length(updates) == 0) {
    return(list(values = current, changed = FALSE, updates = character(0)))
  }

  changed <- vapply(names(updates), function(name) {
    !identical(named_value(current, name, ""), as.character(updates[[name]] %||% ""))
  }, logical(1))
  if (!any(changed, na.rm = TRUE)) {
    return(list(values = current, changed = FALSE, updates = updates))
  }

  current[names(updates)] <- updates
  list(values = current, changed = TRUE, updates = updates)
}

variable_info_state_updates <- function(
  state = NULL,
  direct_measurements = character(0),
  direct_labels = character(0)
) {
  measurements <- settings_state_measurements(state)
  if (length(direct_measurements) > 0) {
    measurements[names(direct_measurements)] <- direct_measurements
  }
  measurements <- measurements[measurements %in% c("binary", "category", "ordered", "continuous")]

  labels <- settings_named_vector(state$var_labels %||% character(0))
  if (length(direct_labels) > 0) {
    labels[names(direct_labels)] <- direct_labels
  }

  list(measurements = measurements, labels = labels)
}

needs_direct_measurement_inputs <- function(state = NULL) {
  is.null(state) || length(settings_state_measurements(state)) == 0
}

merge_variable_info_state <- function(
  info,
  measurement_overrides = character(0),
  var_label_overrides = character(0),
  state = NULL,
  direct_measurements = character(0),
  direct_labels = character(0),
  selected_names = character(0),
  selected_only = FALSE
) {
  if (is.null(info) || nrow(info) == 0) {
    return(list(info = info, measurements = character(0), labels = character(0)))
  }

  info <- apply_variable_overrides(info, measurement_overrides, var_label_overrides)

  updates <- variable_info_state_updates(state, direct_measurements, direct_labels)
  if (length(updates$measurements) > 0) {
    info <- apply_measurement_overrides(info, updates$measurements)
  }
  if (length(updates$labels) > 0) {
    info <- apply_var_label_overrides_to_info(info, updates$labels)
  }

  if (isTRUE(selected_only)) {
    info <- info[info$name %in% selected_names, , drop = FALSE]
  }

  list(info = info, measurements = updates$measurements, labels = updates$labels)
}

measurement_select_html <- function(name, value, source_order) {
  value <- tolower(as.character(value %||% ""))
  if (identical(value, "ordinal")) value <- "ordered"
  if (identical(value, "nominal")) value <- "category"
  choices <- unique(c("binary", "category", "ordered", "continuous", value))
  choices <- choices[nzchar(choices)]
  choice_labels <- ifelse(choices == "ordered", "ordinal", choices)
  sprintf(
    paste0(
      '<span class="measurement-control">',
      '<span class="measurement-symbol measurement-%s" title="%s" aria-label="%s"></span>',
      '<select id="measurement_input_%s" class="measurement-select" data-name="%s" ',
      'onchange="if(window.Shiny){Shiny.setInputValue(&quot;variable_measurement_update&quot;,',
      '{name:this.getAttribute(&quot;data-name&quot;),value:this.value,nonce:Date.now()+Math.random()},',
      '{priority:&quot;event&quot;});} if(window.easyflowUpdateMeasurementControl){window.easyflowUpdateMeasurementControl(this);}">%s</select>',
      '</span>'
    ),
    htmltools::htmlEscape(value),
    htmltools::htmlEscape(ifelse(value == "ordered", "ordinal", value)),
    htmltools::htmlEscape(ifelse(value == "ordered", "ordinal", value)),
    htmltools::htmlEscape(source_order),
    htmltools::htmlEscape(name),
    paste(
      sprintf(
        '<option value="%s" %s>%s</option>',
        choices,
        ifelse(choices == value, "selected", ""),
        choice_labels
      ),
      collapse = ""
    )
  )
}

variable_table_display_data <- function(
  info,
  checked_names = character(0),
  selected_names = character(0),
  assigned_elsewhere = character(0),
  dependent = character(0),
  independent = character(0),
  controls = character(0),
  selection_applied = FALSE,
  active_role = "dependent",
  measurement_overrides = character(0)
) {
  table_data <- apply_measurement_overrides(info, measurement_overrides)
  if (isTRUE(selection_applied)) {
    visible_names <- setdiff(as.character(selected_names), as.character(assigned_elsewhere))
    table_data <- table_data[table_data$name %in% unique(c(checked_names, visible_names)), , drop = FALSE]
  }
  table_data$role <- vapply(
    table_data$name,
    role_for_variable,
    character(1),
    dependent = dependent,
    independent = independent,
    controls = controls
  )
  table_data <- table_data[, c("source_order", "name", "var_label", "role", "measurement", "storage_type", "n_unique", "n_missing", "min_value", "max_value"), drop = FALSE]
  disabled_names <- if (isTRUE(selection_applied) && identical(active_role, "dependent")) {
    setdiff(table_data$name[table_data$measurement != "continuous"], checked_names)
  } else {
    character(0)
  }
  table_data$measurement <- mapply(
    measurement_select_html,
    table_data$name,
    table_data$measurement,
    table_data$source_order,
    USE.NAMES = FALSE
  )
  cbind(
    selected = sprintf(
      '<input type="checkbox" class="variable-select" data-name="%s" %s %s>',
      htmltools::htmlEscape(table_data$name),
      ifelse(table_data$name %in% checked_names, "checked", ""),
      ifelse(table_data$name %in% disabled_names, "disabled title=\"Dependent variable must be continuous\"", "")
    ),
    table_data,
    stringsAsFactors = FALSE
  )
}

variable_table_render_state <- function(
  info,
  checked_names = character(0),
  selected_names = character(0),
  assigned_elsewhere = character(0),
  dependent = character(0),
  independent = character(0),
  controls = character(0),
  selection_applied = FALSE,
  active_role = "dependent",
  measurement_overrides = character(0)
) {
  list(
    checked_names = checked_names,
    table_data = variable_table_display_data(
      info,
      checked_names = checked_names,
      selected_names = selected_names,
      assigned_elsewhere = assigned_elsewhere,
      dependent = dependent,
      independent = independent,
      controls = controls,
      selection_applied = selection_applied,
      active_role = active_role,
      measurement_overrides = measurement_overrides
    )
  )
}

collect_variable_input_values <- function(info, input, prefixes, allowed_values = NULL, keep_blank = FALSE) {
  if (is.null(info) || !all(c("source_order", "name") %in% names(info))) {
    return(character(0))
  }

  prefixes <- as.character(prefixes)
  collected <- character(0)
  for (row_index in seq_len(nrow(info))) {
    source_order <- as.character(info$source_order[[row_index]])
    name <- as.character(info$name[[row_index]])
    if (!nzchar(source_order) || !nzchar(name)) {
      next
    }
    value <- NULL
    for (prefix in prefixes) {
      value <- input[[paste0(prefix, source_order)]]
      if (!is.null(value)) {
        break
      }
    }
    if (is.null(value) || length(value) == 0) {
      next
    }
    value <- as.character(value[[1]])
    if (!isTRUE(keep_blank) && !nzchar(trimws(value))) {
      next
    }
    if (!is.null(allowed_values) && !value %in% allowed_values) {
      next
    }
    collected[name] <- value
  }
  collected
}

collect_variable_inputs_from_table <- function(variable_info_fn, input, prefixes, allowed_values = NULL, keep_blank = FALSE) {
  info <- tryCatch(variable_info_fn(reactive_labels = FALSE), error = function(e) NULL)
  collect_variable_input_values(
    info,
    input,
    prefixes = prefixes,
    allowed_values = allowed_values,
    keep_blank = keep_blank
  )
}

collect_var_label_inputs_from_table <- function(variable_info_fn, input) {
  collect_variable_inputs_from_table(
    variable_info_fn,
    input,
    prefixes = c("var_label_input_", "category_var_label_input_")
  )
}

collect_measurement_inputs_from_table <- function(variable_info_fn, input) {
  collect_variable_inputs_from_table(
    variable_info_fn,
    input,
    prefixes = c("measurement_input_", "category_measurement_input_"),
    allowed_values = c("binary", "category", "ordered", "continuous")
  )
}
