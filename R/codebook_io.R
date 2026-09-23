# Coding-book import, validation, matching, and metadata merge helpers.

codebook_required_columns <- function() {
  c("변수명", "변수라벨", "변수유형", "값", "값라벨")
}

codebook_allowed_types <- function() {
  c("이분형", "범주형", "서열", "연속형", "문자형")
}

normalize_codebook_type <- function(value) {
  value <- trimws(as.character(value %||% ""))
  shortcuts <- c(
    "이" = "이분형",
    "범" = "범주형",
    "서" = "서열",
    "연" = "연속형",
    "문" = "문자형"
  )
  if (value %in% names(shortcuts)) unname(shortcuts[[value]]) else value
}

codebook_measurement <- function(value) {
  value <- normalize_codebook_type(value)
  unname(c(
    "이분형" = "binary",
    "범주형" = "category",
    "서열" = "ordered",
    "연속형" = "continuous",
    # Character data remain unchanged. Statistical models consume them as
    # categorical predictors (factors) when the selected analysis permits it.
    "문자형" = "category"
  )[[value]] %||% "")
}

codebook_text <- function(value) {
  value <- as.character(value %||% "")
  value[is.na(value)] <- ""
  trimws(value)
}

read_codebook_excel <- function(
  path,
  file_name = basename(path),
  max_pairs = statedu_category_label_max_pairs()
) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("코딩북을 읽으려면 readxl 패키지가 필요합니다.")
  }
  extension <- tolower(tools::file_ext(file_name %||% path))
  if (!extension %in% c("xlsx", "xls")) {
    stop("코딩북은 .xlsx 또는 .xls 파일만 사용할 수 있습니다.")
  }

  sheets <- readxl::excel_sheets(path)
  if (length(sheets) == 0) {
    stop("코딩북에 읽을 수 있는 시트가 없습니다.")
  }
  sheet <- if ("코딩북" %in% sheets) "코딩북" else sheets[[1]]
  raw <- as.data.frame(
    readxl::read_excel(path, sheet = sheet, col_types = "text", .name_repair = "minimal"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(raw) <- codebook_text(names(raw))
  required <- codebook_required_columns()
  missing_columns <- setdiff(required, names(raw))
  if (length(missing_columns) > 0) {
    stop(sprintf("필수 열이 없습니다: %s", paste(missing_columns, collapse = ", ")))
  }
  if (anyDuplicated(names(raw))) {
    stop("코딩북에 이름이 같은 열이 두 개 이상 있습니다.")
  }

  raw <- raw[, required, drop = FALSE]
  for (column in required) raw[[column]] <- codebook_text(raw[[column]])
  raw$.excel_row <- seq_len(nrow(raw)) + 1L
  keep <- apply(raw[, required, drop = FALSE], 1L, function(row) any(nzchar(row)))
  raw <- raw[keep, , drop = FALSE]
  if (nrow(raw) == 0) {
    stop("코딩북에 입력된 변수가 없습니다.")
  }

  expanded <- raw
  current_name <- ""
  current_label <- ""
  current_type <- ""
  errors <- character(0)
  warnings <- character(0)

  for (row_index in seq_len(nrow(expanded))) {
    row_number <- expanded$.excel_row[[row_index]]
    supplied_name <- expanded$변수명[[row_index]]
    supplied_label <- expanded$변수라벨[[row_index]]
    supplied_type <- normalize_codebook_type(expanded$변수유형[[row_index]])

    if (nzchar(supplied_name)) {
      current_name <- supplied_name
      current_label <- supplied_label
      current_type <- supplied_type
      if (!nzchar(current_type)) {
        errors <- c(errors, sprintf("%s행: '%s'의 변수유형이 비어 있습니다.", row_number, current_name))
      }
    } else if (!nzchar(current_name)) {
      errors <- c(errors, sprintf("%s행: 위에서 이어받을 변수명이 없습니다.", row_number))
    } else {
      if (nzchar(supplied_label) && !identical(supplied_label, current_label)) {
        errors <- c(errors, sprintf("%s행: 변수라벨은 변수 첫 행에 한 번만 입력해야 합니다.", row_number))
      }
      if (nzchar(supplied_type) && !identical(supplied_type, current_type)) {
        errors <- c(errors, sprintf("%s행: 변수유형은 변수 첫 행에 한 번만 입력해야 합니다.", row_number))
      }
    }

    expanded$변수명[[row_index]] <- current_name
    expanded$변수라벨[[row_index]] <- current_label
    expanded$변수유형[[row_index]] <- current_type
  }

  invalid_type_rows <- which(nzchar(expanded$변수유형) & !expanded$변수유형 %in% codebook_allowed_types())
  if (length(invalid_type_rows) > 0) {
    errors <- c(errors, vapply(invalid_type_rows, function(i) {
      sprintf("%s행: 허용되지 않은 변수유형 '%s'입니다.", expanded$.excel_row[[i]], expanded$변수유형[[i]])
    }, character(1)))
  }

  block_starts <- which(nzchar(raw$변수명))
  repeated_blocks <- unique(raw$변수명[block_starts][duplicated(raw$변수명[block_starts])])
  if (length(repeated_blocks) > 0) {
    errors <- c(errors, sprintf("같은 변수가 여러 구간에서 다시 시작됩니다: %s", paste(repeated_blocks, collapse = ", ")))
  }

  label_without_value <- which(!nzchar(expanded$값) & nzchar(expanded$값라벨))
  if (length(label_without_value) > 0) {
    errors <- c(errors, vapply(label_without_value, function(i) {
      sprintf("%s행: 값라벨은 있지만 값이 없습니다.", expanded$.excel_row[[i]])
    }, character(1)))
  }
  value_without_label <- which(nzchar(expanded$값) & !nzchar(expanded$값라벨))
  if (length(value_without_label) > 0) {
    warnings <- c(warnings, sprintf("값라벨이 비어 있는 값이 %s개 있습니다.", length(value_without_label)))
  }

  variables <- expanded[!duplicated(expanded$변수명), c("변수명", "변수라벨", "변수유형"), drop = FALSE]
  blank_labels <- variables$변수명[!nzchar(variables$변수라벨)]
  if (length(blank_labels) > 0) {
    warnings <- c(warnings, sprintf("변수라벨이 비어 있는 변수가 %s개 있습니다.", length(blank_labels)))
  }

  value_rows <- expanded[nzchar(expanded$값), c("변수명", "값", "값라벨", ".excel_row"), drop = FALSE]
  if (nrow(value_rows) > 0) {
    duplicate_key <- paste(value_rows$변수명, value_rows$값, sep = "\r")
    duplicate_groups <- split(seq_len(nrow(value_rows)), duplicate_key)
    for (indices in duplicate_groups[lengths(duplicate_groups) > 1L]) {
      labels <- unique(value_rows$값라벨[indices])
      variable <- value_rows$변수명[[indices[[1]]]]
      value <- value_rows$값[[indices[[1]]]]
      if (length(labels) > 1L) {
        errors <- c(errors, sprintf("'%s'의 값 '%s'에 서로 다른 값라벨이 지정되어 있습니다.", variable, value))
      } else {
        warnings <- c(warnings, sprintf("'%s'의 값 '%s'가 중복되어 한 번만 적용됩니다.", variable, value))
      }
    }
    value_rows <- value_rows[!duplicated(duplicate_key), , drop = FALSE]
  }

  value_counts <- table(value_rows$변수명)
  too_many <- names(value_counts)[value_counts > max_pairs]
  if (length(too_many) > 0) {
    errors <- c(errors, sprintf(
      "현재 한 변수에 적용할 수 있는 값라벨은 최대 %s개입니다: %s",
      max_pairs,
      paste(too_many, collapse = ", ")
    ))
  }

  continuous_with_values <- intersect(
    variables$변수명[variables$변수유형 == "연속형"],
    value_rows$변수명
  )
  if (length(continuous_with_values) > 0) {
    warnings <- c(warnings, sprintf(
      "연속형 변수의 값라벨은 적용하지 않습니다: %s",
      paste(continuous_with_values, collapse = ", ")
    ))
  }

  errors <- unique(errors)
  warnings <- unique(warnings)
  if (length(errors) > 0) {
    condition <- simpleError(paste(errors, collapse = "\n"))
    attr(condition, "codebook_errors") <- errors
    stop(condition)
  }

  list(
    sheet = sheet,
    variables = variables,
    value_labels = value_rows,
    warnings = warnings,
    source_rows = expanded
  )
}

match_codebook_to_variables <- function(codebook, variable_info) {
  if (!is.data.frame(variable_info) || !"name" %in% names(variable_info)) {
    stop("먼저 데이터를 불러와야 합니다.")
  }
  info_names <- as.character(variable_info$name)
  variables <- codebook$variables
  variables$measurement <- vapply(variables$변수유형, codebook_measurement, character(1))
  variables$matched_name <- ifelse(variables$변수명 %in% info_names, variables$변수명, "")
  variables$status <- ifelse(nzchar(variables$matched_name), "일치", "데이터에 없음")

  normalized_info <- tolower(trimws(info_names))
  unmatched <- which(!nzchar(variables$matched_name))
  for (row_index in unmatched) {
    candidates <- info_names[normalized_info == tolower(trimws(variables$변수명[[row_index]]))]
    if (length(candidates) == 1L) {
      variables$status[[row_index]] <- paste0("이름 확인: ", candidates[[1]])
    }
  }

  variables$value_label_count <- vapply(variables$변수명, function(name) {
    sum(codebook$value_labels$변수명 == name)
  }, integer(1))
  matched_names <- variables$matched_name[nzchar(variables$matched_name)]
  matched_info <- variable_info[match(matched_names, info_names), , drop = FALSE]
  existing_labels <- stats::setNames(as.character(matched_info$var_label %||% ""), matched_names)
  existing_measurements <- stats::setNames(as.character(matched_info$measurement %||% ""), matched_names)
  variables$existing_label <- unname(existing_labels[variables$matched_name])
  variables$existing_measurement <- unname(existing_measurements[variables$matched_name])
  variables$existing_label[is.na(variables$existing_label)] <- ""
  variables$existing_measurement[is.na(variables$existing_measurement)] <- ""

  warnings <- codebook$warnings %||% character(0)
  character_names <- variables$matched_name[variables$변수유형 == "문자형" & nzchar(variables$matched_name)]
  if (length(character_names) > 0 && "n_unique" %in% names(variable_info)) {
    character_info <- variable_info[match(character_names, info_names), , drop = FALSE]
    high_cardinality <- character_names[suppressWarnings(as.numeric(character_info$n_unique)) > 50]
    if (length(high_cardinality) > 0) {
      warnings <- c(warnings, sprintf(
        "고유값이 50개를 넘는 문자형 변수는 ID·이름·주관식 응답인지 확인하세요: %s",
        paste(high_cardinality, collapse = ", ")
      ))
    }
  }

  list(
    codebook = codebook,
    variables = variables,
    matched_names = matched_names,
    data_only_names = setdiff(info_names, matched_names),
    codebook_only_names = variables$변수명[!nzchar(variables$matched_name)],
    warnings = unique(warnings)
  )
}

codebook_category_table <- function(
  current,
  variable_info,
  match_result,
  overwrite = TRUE,
  max_pairs = statedu_category_label_max_pairs()
) {
  columns <- category_label_edit_columns(max_pairs)
  table <- normalize_category_label_table(current, columns, variable_info)
  variables <- match_result$variables
  codebook <- match_result$codebook

  for (row_index in which(nzchar(variables$matched_name) & variables$measurement != "continuous")) {
    name <- variables$matched_name[[row_index]]
    source_name <- variables$변수명[[row_index]]
    if (!name %in% table$name) table <- rbind(table, new_category_label_row(name, columns))
    table_row <- match(name, table$name)
    incoming <- codebook$value_labels[codebook$value_labels$변수명 == source_name, , drop = FALSE]
    if (nrow(incoming) == 0) next

    existing_values <- character(0)
    existing_labels <- character(0)
    for (i in seq_len(max_pairs)) {
      value <- trimws(as.character(table[[paste0("value_", i)]][[table_row]] %||% ""))
      label <- as.character(table[[paste0("label_", i)]][[table_row]] %||% "")
      if (nzchar(value)) {
        existing_values <- c(existing_values, value)
        existing_labels <- c(existing_labels, label)
      }
    }

    if (isTRUE(overwrite)) {
      values <- as.character(incoming$값)
      labels <- as.character(incoming$값라벨)
    } else {
      values <- existing_values
      labels <- existing_labels
      for (i in seq_len(nrow(incoming))) {
        value <- as.character(incoming$값[[i]])
        label <- as.character(incoming$값라벨[[i]])
        existing_index <- match(value, values)
        if (is.na(existing_index)) {
          values <- c(values, value)
          labels <- c(labels, label)
        } else if (!nzchar(labels[[existing_index]])) {
          labels[[existing_index]] <- label
        }
      }
    }

    for (i in seq_len(max_pairs)) {
      table[[paste0("value_", i)]][[table_row]] <- if (i <= length(values)) values[[i]] else ""
      table[[paste0("label_", i)]][[table_row]] <- if (i <= length(labels)) labels[[i]] else ""
    }
  }

  # Recalculate reference labels without changing the stored reference value.
  apply_category_label_snapshot(table, list(), base = variable_info, max_pairs = max_pairs)$table
}
