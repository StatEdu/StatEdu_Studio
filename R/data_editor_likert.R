# Automatic Likert label detection and batch conversion.

likert_ui_text <- function(en, ko_hex, language = getOption("statedu.app_language", statedu_initial_language())) {
  statedu_text(language, en, statedu_utf8(ko_hex))
}

likert_dictionary <- function(custom = NULL) {
  dictionaries <- list(
    importance_ko_5_common = c(statedu_utf8("eca084ed988020eca491ec9a94ed9598eca78020ec958aec9d8c"), statedu_utf8("eca491ec9a94ed9598eca78020ec958aec9d8c"), statedu_utf8("ebb3b4ed86b5"), statedu_utf8("eca491ec9a94ed95a8"), statedu_utf8("eba7a4ec9ab020eca491ec9a94ed95a8")),
    awareness_ko_5_common = c(statedu_utf8("eca084ed988020ebaaa8eba684"), statedu_utf8("ebaaa8eba684"), statedu_utf8("ebb3b4ed86b5"), statedu_utf8("ec958ceab3a020ec9e88ec9d8c"), statedu_utf8("eba7a4ec9ab020ec9e9820ec958ceab3a020ec9e88ec9d8c")),
    agreement_ko_5_common = c(statedu_utf8("eca084ed988020eab7b8eba087eca78020ec958aeb8ba4"), statedu_utf8("eab7b8eba087eca78020ec958aeb8ba4"), statedu_utf8("ebb3b4ed86b5"), statedu_utf8("eab7b8eba087eb8ba4"), statedu_utf8("eba7a4ec9ab020eab7b8eba087eb8ba4")),
    satisfaction_ko_5_common = c(statedu_utf8("eca084ed988020eba78ceca1b1ed9598eca78020ec958aec9d8c"), statedu_utf8("eba78ceca1b1ed9598eca78020ec958aec9d8c"), statedu_utf8("ebb3b4ed86b5"), statedu_utf8("eba78ceca1b1ed95a8"), statedu_utf8("eba7a4ec9ab020eba78ceca1b1ed95a8")),
    need_ko_5_common = c(statedu_utf8("eca084ed988020ed9584ec9a94ed9598eca78020ec958aec9d8c"), statedu_utf8("ed9584ec9a94ed9598eca78020ec958aec9d8c"), statedu_utf8("ebb3b4ed86b5"), statedu_utf8("ed9584ec9a94ed95a8"), statedu_utf8("eba7a4ec9ab020ed9584ec9a94ed95a8")),
    agreement_ko_5 = c(statedu_utf8("eca084ed988020ec9584eb8b88eb8ba4"), statedu_utf8("ec9584eb8b88eb8ba4"), statedu_utf8("ebb3b4ed86b5ec9db4eb8ba4"), statedu_utf8("eab7b8eba087eb8ba4"), statedu_utf8("eba7a4ec9ab020eab7b8eba087eb8ba4")),
    agreement_ko_5_alt = c(statedu_utf8("eca084ed988020eab7b8eba087eca78020ec958aeb8ba4"), statedu_utf8("eab7b8eba087eca78020ec958aeb8ba4"), statedu_utf8("ebb3b4ed86b5ec9db4eb8ba4"), statedu_utf8("eab7b8eba087eb8ba4"), statedu_utf8("eba7a4ec9ab020eab7b8eba087eb8ba4")),
    agreement_ko_5_formal = c(statedu_utf8("eba7a4ec9ab020eab7b8eba087eca78020ec958aeb8ba4"), statedu_utf8("eab7b8eba087eca78020ec958aeb8ba4"), statedu_utf8("ebb3b4ed86b5"), statedu_utf8("eab7b8eba087eb8ba4"), statedu_utf8("eba7a4ec9ab020eab7b8eba087eb8ba4")),
    satisfaction_ko_5 = c(statedu_utf8("eba7a4ec9ab020ebb688eba78ceca1b1"), statedu_utf8("ebb688eba78ceca1b1"), statedu_utf8("ebb3b4ed86b5"), statedu_utf8("eba78ceca1b1"), statedu_utf8("eba7a4ec9ab020eba78ceca1b1")),
    importance_ko_5 = c(statedu_utf8("eca084ed988020eca491ec9a94ed9598eca78020ec958aeb8ba4"), statedu_utf8("eca491ec9a94ed9598eca78020ec958aeb8ba4"), statedu_utf8("ebb3b4ed86b5"), statedu_utf8("eca491ec9a94ed9598eb8ba4"), statedu_utf8("eba7a4ec9ab020eca491ec9a94ed9598eb8ba4")),
    frequency_ko_5 = c(statedu_utf8("eca084ed988020ec9786ec9d8c"), statedu_utf8("eb939cebacbceab28c"), statedu_utf8("eab080eb8194"), statedu_utf8("ec9e90eca3bc"), statedu_utf8("ed95adec8381")),
    agreement_en_5 = c("strongly disagree", "disagree", "neutral", "agree", "strongly agree"),
    satisfaction_en_5 = c("very dissatisfied", "dissatisfied", "neutral", "satisfied", "very satisfied"),
    frequency_en_5 = c("never", "rarely", "sometimes", "often", "always"),
    importance_ko_4_common = c(statedu_utf8("eca084ed988020eca491ec9a94ed9598eca78020ec958aec9d8c"), statedu_utf8("eca491ec9a94ed9598eca78020ec958aec9d8c"), statedu_utf8("eca491ec9a94ed95a8"), statedu_utf8("eba7a4ec9ab020eca491ec9a94ed95a8")),
    awareness_ko_4_common = c(statedu_utf8("eca084ed988020ebaaa8eba684"), statedu_utf8("ebaaa8eba684"), statedu_utf8("ec958ceab3a020ec9e88ec9d8c"), statedu_utf8("eba7a4ec9ab020ec9e9820ec958ceab3a020ec9e88ec9d8c")),
    agreement_ko_4_common = c(statedu_utf8("eca084ed988020eab7b8eba087eca78020ec958aeb8ba4"), statedu_utf8("eab7b8eba087eca78020ec958aeb8ba4"), statedu_utf8("eab7b8eba087eb8ba4"), statedu_utf8("eba7a4ec9ab020eab7b8eba087eb8ba4")),
    satisfaction_ko_4_common = c(statedu_utf8("eca084ed988020eba78ceca1b1ed9598eca78020ec958aec9d8c"), statedu_utf8("eba78ceca1b1ed9598eca78020ec958aec9d8c"), statedu_utf8("eba78ceca1b1ed95a8"), statedu_utf8("eba7a4ec9ab020eba78ceca1b1ed95a8")),
    need_ko_4_common = c(statedu_utf8("eca084ed988020ed9584ec9a94ed9598eca78020ec958aec9d8c"), statedu_utf8("ed9584ec9a94ed9598eca78020ec958aec9d8c"), statedu_utf8("ed9584ec9a94ed95a8"), statedu_utf8("eba7a4ec9ab020ed9584ec9a94ed95a8")),
    agreement_ko_4 = c(statedu_utf8("eca084ed988020ec9584eb8b88eb8ba4"), statedu_utf8("ec9584eb8b88eb8ba4"), statedu_utf8("eab7b8eba087eb8ba4"), statedu_utf8("eba7a4ec9ab020eab7b8eba087eb8ba4")),
    agreement_ko_4_alt = c(statedu_utf8("eca084ed988020eab7b8eba087eca78020ec958aeb8ba4"), statedu_utf8("eab7b8eba087eca78020ec958aeb8ba4"), statedu_utf8("eab7b8eba087eb8ba4"), statedu_utf8("eba7a4ec9ab020eab7b8eba087eb8ba4")),
    agreement_ko_4_formal = c(statedu_utf8("eba7a4ec9ab020eab7b8eba087eca78020ec958aeb8ba4"), statedu_utf8("eab7b8eba087eca78020ec958aeb8ba4"), statedu_utf8("eab7b8eba087eb8ba4"), statedu_utf8("eba7a4ec9ab020eab7b8eba087eb8ba4")),
    satisfaction_ko_4 = c(statedu_utf8("eba7a4ec9ab020ebb688eba78ceca1b1"), statedu_utf8("ebb688eba78ceca1b1"), statedu_utf8("eba78ceca1b1"), statedu_utf8("eba7a4ec9ab020eba78ceca1b1")),
    importance_ko_4 = c(statedu_utf8("eca084ed988020eca491ec9a94ed9598eca78020ec958aeb8ba4"), statedu_utf8("eca491ec9a94ed9598eca78020ec958aeb8ba4"), statedu_utf8("eca491ec9a94ed9598eb8ba4"), statedu_utf8("eba7a4ec9ab020eca491ec9a94ed9598eb8ba4")),
    frequency_ko_4 = c(statedu_utf8("eca084ed988020ec9786ec9d8c"), statedu_utf8("eb939cebacbceab28c"), statedu_utf8("ec9e90eca3bc"), statedu_utf8("ed95adec8381")),
    agreement_en_4 = c("strongly disagree", "disagree", "agree", "strongly agree"),
    satisfaction_en_4 = c("very dissatisfied", "dissatisfied", "satisfied", "very satisfied"),
    frequency_en_4 = c("never", "rarely", "often", "always")
  )
  if (length(custom %||% list()) > 0) {
    dictionaries <- c(dictionaries, custom)
  }
  dictionaries
}

likert_normalize_label <- function(x) {
  x <- tolower(trimws(as.character(x %||% "")))
  x <- gsub("[[:space:][:punct:]]+", "", x)
  x
}

likert_custom_dictionary_path <- function() {
  root <- Sys.getenv("APPDATA", unset = "")
  if (!nzchar(root)) {
    root <- path.expand("~")
  }
  file.path(root, "StatEdu Studio", "likert_custom_dictionaries.rds")
}

read_likert_custom_dictionaries <- function() {
  path <- likert_custom_dictionary_path()
  if (!file.exists(path)) {
    return(list())
  }
  dictionaries <- tryCatch(readRDS(path), error = function(e) list())
  if (!is.list(dictionaries) || is.null(names(dictionaries))) {
    return(list())
  }
  dictionaries <- dictionaries[nzchar(names(dictionaries))]
  dictionaries <- lapply(dictionaries, function(labels) {
    labels <- trimws(as.character(labels %||% character(0)))
    labels[nzchar(labels)]
  })
  dictionaries[vapply(dictionaries, function(labels) length(labels) >= 3 && length(labels) <= 11, logical(1))]
}

write_likert_custom_dictionaries <- function(dictionaries) {
  path <- likert_custom_dictionary_path()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(dictionaries %||% list(), path)
  invisible(path)
}

likert_present_values <- function(values) {
  values <- trimws(as.character(as.vector(values)))
  values <- values[!is.na(values) & nzchar(values)]
  unique(values)
}

likert_numeric_text_mapping <- function(unique_values) {
  text <- trimws(as.character(unique_values))
  numeric <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", text)))
  valid <- !is.na(numeric) & grepl("[0-9]", text)
  if (length(text) < 3 || length(text) > 11 || !all(valid)) {
    return(NULL)
  }
  if (length(unique(numeric)) != length(numeric)) {
    return(NULL)
  }
  ordered_index <- order(numeric)
  data.frame(
    label = text[ordered_index],
    value = numeric[ordered_index],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

likert_mapping_signature <- function(mapping) {
  mapping <- as.data.frame(mapping, stringsAsFactors = FALSE, check.names = FALSE)
  paste(sprintf("%s=%s", mapping$label, mapping$value), collapse = "|")
}

likert_mapping_values <- function(mapping) {
  suppressWarnings(as.numeric(as.data.frame(mapping, stringsAsFactors = FALSE)$value))
}

likert_mapping_is_superset <- function(candidate, observed) {
  candidate <- as.data.frame(candidate, stringsAsFactors = FALSE, check.names = FALSE)
  observed <- as.data.frame(observed, stringsAsFactors = FALSE, check.names = FALSE)
  candidate_values <- likert_mapping_values(candidate)
  observed_values <- likert_mapping_values(observed)
  if (length(candidate_values) <= length(observed_values)) {
    return(FALSE)
  }
  if (!all(observed_values %in% candidate_values)) {
    return(FALSE)
  }
  for (value in observed_values) {
    candidate_label <- candidate$label[match(value, candidate_values)]
    observed_label <- observed$label[match(value, observed_values)]
    if (!identical(likert_normalize_label(candidate_label), likert_normalize_label(observed_label))) {
      return(FALSE)
    }
  }
  TRUE
}

likert_best_group_signature <- function(index, mappings, dictionaries, signatures) {
  if (!identical(as.character(dictionaries[[index]]), "numeric_text")) {
    return(signatures[[index]])
  }
  candidates <- which(as.character(dictionaries) == "numeric_text")
  candidates <- candidates[candidates != index]
  candidates <- candidates[vapply(candidates, function(candidate) {
    likert_mapping_is_superset(mappings[[candidate]], mappings[[index]])
  }, logical(1))]
  if (length(candidates) == 0) {
    return(signatures[[index]])
  }
  candidate_sizes <- vapply(candidates, function(candidate) nrow(as.data.frame(mappings[[candidate]])), integer(1))
  signatures[[candidates[[which.max(candidate_sizes)]]]]
}

likert_group_representative_mapping <- function(group) {
  if (is.null(group) || !is.data.frame(group) || nrow(group) == 0) {
    return(NULL)
  }
  mappings <- if ("representative_mapping" %in% names(group)) {
    group$representative_mapping
  } else if ("mapping" %in% names(group)) {
    group$mapping
  } else {
    NULL
  }
  if (is.null(mappings)) {
    return(NULL)
  }
  sizes <- vapply(mappings, function(mapping) nrow(as.data.frame(mapping)), integer(1))
  mappings[[which.max(sizes)]]
}

likert_variable_choice_labels <- function(group, representative_mapping = NULL) {
  if (is.null(group) || !is.data.frame(group) || nrow(group) == 0) {
    return(character(0))
  }
  variables <- as.character(group$variable)
  representative_levels <- if (!is.null(representative_mapping)) {
    nrow(as.data.frame(representative_mapping, stringsAsFactors = FALSE, check.names = FALSE))
  } else if ("n_levels" %in% names(group)) {
    max(group$n_levels, na.rm = TRUE)
  } else {
    NA_integer_
  }
  labels <- htmltools::htmlEscape(variables)
  if (is.finite(representative_levels) && "n_levels" %in% names(group)) {
    partial <- group$n_levels < representative_levels
    labels[partial] <- sprintf(
      "%s <span class=\"likert-partial-level-note\">[observed %s of %s levels]</span>",
      labels[partial],
      group$n_levels[partial],
      representative_levels
    )
  }
  stats::setNames(variables, labels)
}

likert_variables_checkbox_group <- function(input_id, choices, selected = unname(choices)) {
  choices <- choices %||% character(0)
  values <- unname(as.character(choices))
  labels <- names(choices)
  if (is.null(labels) || length(labels) != length(values)) {
    labels <- htmltools::htmlEscape(values)
  }
  selected <- as.character(selected %||% character(0))
  div(
    id = input_id,
    class = "form-group shiny-input-checkboxgroup shiny-input-container",
    div(
      class = "shiny-options-group",
      lapply(seq_along(values), function(index) {
        div(
          class = "checkbox",
          tags$label(
            tags$input(
              type = "checkbox",
              name = input_id,
              value = values[[index]],
              checked = if (values[[index]] %in% selected) "checked" else NULL
            ),
            tags$span(HTML(labels[[index]]))
          )
        )
      })
    )
  )
}

detect_likert_mapping <- function(values, dictionaries = likert_dictionary()) {
  unique_values <- likert_present_values(values)
  if (length(unique_values) < 3 || length(unique_values) > 11) {
    return(NULL)
  }

  numeric_mapping <- likert_numeric_text_mapping(unique_values)
  if (!is.null(numeric_mapping)) {
    return(list(
      mapping = numeric_mapping,
      representative_mapping = numeric_mapping,
      signature = likert_mapping_signature(numeric_mapping),
      dictionary = "numeric_text",
      score = 0.82
    ))
  }

  normalized_values <- likert_normalize_label(unique_values)
  dictionary_names <- names(dictionaries)
  dictionary_sizes <- vapply(dictionaries, length, integer(1))
  exact_names <- dictionary_names[dictionary_sizes == length(unique_values)]
  superset_names <- dictionary_names[dictionary_sizes != length(unique_values)]
  for (dict_name in c(exact_names, superset_names)) {
    labels <- dictionaries[[dict_name]]
    normalized_labels <- likert_normalize_label(labels)
    matched <- match(normalized_values, normalized_labels)
    if (all(!is.na(matched))) {
      ordered <- order(matched)
      observed_mapping <- data.frame(
        label = unique_values[ordered],
        value = matched[ordered],
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      representative_mapping <- data.frame(
        label = labels,
        value = seq_along(labels),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      return(list(
        mapping = observed_mapping,
        representative_mapping = representative_mapping,
        signature = paste0("dictionary:", dict_name),
        dictionary = dict_name,
        score = 0.95
      ))
    }
  }

  NULL
}

detect_likert_variables <- function(data, dictionaries = likert_dictionary()) {
  if (is.null(data) || !is.data.frame(data) || ncol(data) == 0) {
    return(data.frame(check.names = FALSE))
  }
  rows <- lapply(names(data), function(name) {
    values <- data[[name]]
    if (!(is.character(values) || is.factor(values) || is.ordered(values))) {
      return(NULL)
    }
    detected <- detect_likert_mapping(values, dictionaries = dictionaries)
    if (is.null(detected)) {
      return(NULL)
    }
    mapping <- detected$mapping
    representative_mapping <- detected$representative_mapping %||% mapping
    signature <- detected$signature %||% likert_mapping_signature(representative_mapping)
    data.frame(
      variable = name,
      group_id = paste0("likert_", sprintf("%03d", match(signature, signature))),
      source_index = match(name, names(data)),
      signature = signature,
      dictionary = detected$dictionary,
      n_levels = nrow(mapping),
      mapping = I(list(mapping)),
      representative_mapping = I(list(representative_mapping)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(data.frame(check.names = FALSE))
  }
  detected <- do.call(rbind, rows)
  exact_signatures <- as.character(detected$signature)
  grouped_signatures <- vapply(seq_len(nrow(detected)), function(index) {
    likert_best_group_signature(index, detected$mapping, detected$dictionary, exact_signatures)
  }, character(1))
  signatures <- unique(grouped_signatures)
  detected$signature <- grouped_signatures
  detected$group_id <- paste0("likert_", match(grouped_signatures, signatures))
  detected
}

likert_group_summary <- function(detected) {
  if (is.null(detected) || !is.data.frame(detected) || nrow(detected) == 0) {
    return(data.frame(check.names = FALSE))
  }
  group_ids <- unique(as.character(detected$group_id))
  group_order <- order(suppressWarnings(as.integer(sub("^likert_", "", group_ids))))
  group_ids <- group_ids[group_order]
  rows <- lapply(group_ids, function(group_id) {
    group <- detected[detected$group_id == group_id, , drop = FALSE]
    if ("source_index" %in% names(group)) {
      group <- group[order(group$source_index), , drop = FALSE]
    }
    representative_mapping <- likert_group_representative_mapping(group)
    levels <- if (!is.null(representative_mapping)) {
      nrow(as.data.frame(representative_mapping, stringsAsFactors = FALSE, check.names = FALSE))
    } else {
      max(group$n_levels, na.rm = TRUE)
    }
    data.frame(
      group_id = group$group_id[[1]],
      variables = paste(group$variable, collapse = "\n"),
      variable_count = nrow(group),
      levels = levels,
      dictionary = group$dictionary[[1]],
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  do.call(rbind, rows)
}

likert_mapping_from_input <- function(input, mapping, prefix = "likert_map") {
  if (is.null(mapping) || !is.data.frame(mapping) || nrow(mapping) == 0) {
    return(mapping)
  }
  mapping$value <- vapply(seq_len(nrow(mapping)), function(index) {
    value <- suppressWarnings(as.numeric(input[[paste0(prefix, "_", index)]] %||% mapping$value[[index]]))
    if (is.na(value)) mapping$value[[index]] else value
  }, numeric(1))
  mapping
}

recode_likert_values <- function(values, mapping, reverse = FALSE) {
  mapping <- as.data.frame(mapping, stringsAsFactors = FALSE, check.names = FALSE)
  text <- trimws(as.character(as.vector(values)))
  output <- rep(NA_real_, length(text))
  for (index in seq_len(nrow(mapping))) {
    matched <- !is.na(text) & text == as.character(mapping$label[[index]])
    output[matched] <- as.numeric(mapping$value[[index]])
  }
  if (isTRUE(reverse)) {
    present <- sort(unique(stats::na.omit(as.numeric(mapping$value))))
    if (length(present) > 1) {
      output <- ifelse(is.na(output), NA_real_, min(present) + max(present) - output)
    }
  }
  output
}

likert_category_payload <- function(variables, mapping, reverse = FALSE) {
  mapping <- as.data.frame(mapping, stringsAsFactors = FALSE, check.names = FALSE)
  values <- as.numeric(mapping$value)
  labels <- as.character(mapping$label)
  if (isTRUE(reverse)) {
    values <- min(values, na.rm = TRUE) + max(values, na.rm = TRUE) - values
  }
  order_index <- order(values)
  values <- values[order_index]
  labels <- labels[order_index]
  rows <- list()
  for (variable in variables) {
    row <- list(reference = "", reference_label = "")
    for (index in seq_along(values)) {
      row[[paste0("value_", index)]] <- as.character(values[[index]])
      row[[paste0("label_", index)]] <- labels[[index]]
    }
    rows[[variable]] <- row
  }
  rows
}

likert_detection_table_display <- function(summary) {
  if (is.null(summary) || nrow(summary) == 0) {
    return(data.frame(Message = likert_ui_text("No Likert-style text variables were detected.", "ed9884ec9eac20eb8db0ec9db4ed84b0ec9790ec849c204c696b65727420ed9895ec8b9dec9d9820ed858dec8aa4ed8ab820ebb380ec8898eab08020eab090eca780eb9098eca78020ec958aec9598ec8ab5eb8b88eb8ba42e"), check.names = FALSE))
  }
  variables <- vapply(strsplit(as.character(summary$variables), "\n", fixed = TRUE), function(items) {
    items <- items[nzchar(items)]
    display_items <- if (length(items) <= 2) {
      items
    } else {
      c(items[[1]], sprintf("... %s item(s) hidden ...", length(items) - 2L), items[[length(items)]])
    }
    classes <- if (length(items) <= 2) {
      rep("likert-variable-line", length(display_items))
    } else {
      c("likert-variable-line", "likert-variable-line likert-variable-hidden-count", "likert-variable-line")
    }
    paste(sprintf("<div class=\"%s\">%s</div>", classes, htmltools::htmlEscape(display_items)), collapse = "")
  }, character(1))
  output <- data.frame(
    Select = sprintf(
      '<input type="radio" name="likert_group_select" class="likert-group-select" value="%s">',
      htmltools::htmlEscape(summary$group_id)
    ),
    Group = summary$group_id,
    Items = variables,
    Count = summary$variable_count,
    Levels = summary$levels,
    Detection = summary$dictionary,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  output
}

likert_review_panel_ui <- function(choices, mapping) {
  choices <- choices %||% character(0)
  values <- unname(as.character(choices))
  if (length(values) == 0 || is.null(mapping) || !is.data.frame(mapping) || nrow(mapping) == 0) {
    return(div(class = "empty-message", div(likert_ui_text("Select a detected Likert group.", "eab090eca780eb909c204c696b65727420eab7b8eba3b9ec9d8420ec84a0ed839ded9598ec84b8ec9a942e"))))
  }
  div(
    class = "likert-review-grid",
    div(
      class = "likert-review-block likert-items-block",
      div(class = "likert-review-title", likert_ui_text("Item text", "ebacb8ed95ad20ed858dec8aa4ed8ab8")),
      likert_variables_checkbox_group(
        "likert_variables",
        choices = choices,
        selected = values
      )
    ),
    div(
      class = "likert-review-block likert-labels-block",
      div(class = "likert-review-title", likert_ui_text("Original label", "ec9b90eb9e9820eb9dbcebb2a8")),
      div(
        class = "likert-label-list",
        lapply(seq_len(nrow(mapping)), function(index) {
          div(class = "likert-label-row", as.character(mapping$label[[index]] %||% ""))
        })
      )
    ),
    div(
      class = "likert-review-block likert-values-block",
      div(class = "likert-review-title", likert_ui_text("Numeric value", "ec88abec9e9020eab092")),
      div(
        class = "likert-value-list",
        lapply(seq_len(nrow(mapping)), function(index) {
          numericInput(paste0("likert_map_", index), NULL, value = mapping$value[[index]], step = 1, width = "92px")
        })
      )
    )
  )
}

likert_custom_dictionary_ui <- function() {
  div(
    class = "likert-custom-dictionary",
    singleton(tags$script(HTML(
      "
      (function() {
        if (window.easyflowLikertDisclosureBound) return;
        window.easyflowLikertDisclosureBound = true;
        document.addEventListener('click', function(event) {
          var button = event.target.closest('.likert-disclosure-button[data-efs-toggle-target]');
          if (!button) return;
          event.preventDefault();
          var target = document.getElementById(button.getAttribute('data-efs-toggle-target'));
          if (!target) return;
          var topBefore = button.getBoundingClientRect().top;
          var shouldOpen = target.hasAttribute('hidden');
          if (shouldOpen) {
            target.removeAttribute('hidden');
          } else {
            target.setAttribute('hidden', 'hidden');
          }
          button.classList.toggle('is-open', shouldOpen);
          button.setAttribute('aria-expanded', shouldOpen ? 'true' : 'false');
          var icon = button.querySelector('.likert-toggle-icon');
          if (icon) icon.textContent = shouldOpen ? '-' : '+';
          var shinyInput = button.getAttribute('data-efs-toggle-input');
          if (shinyInput && window.Shiny) {
            Shiny.setInputValue(shinyInput, shouldOpen, {priority: 'event'});
          }
          window.requestAnimationFrame(function() {
            window.scrollBy(0, button.getBoundingClientRect().top - topBefore);
          });
        });
      })();
      "
    ))),
    tags$button(
      type = "button",
      class = "likert-disclosure-button likert-primary-toggle",
      `data-efs-toggle-target` = "likert-custom-dictionary-panel",
      `aria-expanded` = "false",
      span(class = "likert-toggle-icon", "+"),
      span(likert_ui_text("Add detection dictionary", "eab090eca78020ec82aceca08420ecb694eab080"))
    ),
    div(
      id = "likert-custom-dictionary-panel",
      class = "likert-disclosure-panel",
      hidden = "hidden",
      div(
        class = "likert-custom-grid",
        textInput(
          "likert_custom_dictionary_name",
          likert_ui_text("Detection name", "eab090eca78020ec9db4eba684"),
          value = "",
          placeholder = "e.g., need_ko_5",
          width = "220px"
        ),
        textAreaInput(
          "likert_custom_dictionary_levels",
          likert_ui_text("Labels from low to high", "eb82aeec9d8020eab092ebb680ed84b020eb8692ec9d8020eab09220ec889cec849cec9d9820eb9dbcebb2a8"),
          value = "",
          placeholder = "strongly disagree\ndisagree\nneutral\nagree\nstrongly agree",
          rows = 5,
          width = "420px"
        ),
        div(
          class = "likert-custom-actions",
          actionButton("add_likert_custom_dictionary", likert_ui_text("Add dictionary", "ec82aceca08420ecb694eab080"), class = "btn btn-default"),
          actionButton("clear_likert_custom_dictionaries", likert_ui_text("Clear custom", "ec82acec9aa9ec9e9020ec82aceca08420ecb488eab8b0ed9994"), class = "btn btn-default")
        )
      ),
      uiOutput("likert_custom_dictionary_status")
    )
  )
}

likert_dictionary_registry_item <- function(name, labels, custom = FALSE) {
  labels <- trimws(as.character(labels %||% character(0)))
  labels <- labels[nzchar(labels)]
  tags$details(
    class = paste("likert-dictionary-registry-item", if (isTRUE(custom)) "is-custom" else "is-built-in"),
    tags$summary(
      span(class = "likert-dictionary-chip", sprintf("%s (%s)", name, length(labels)))
    ),
    tags$ol(
      class = "likert-dictionary-label-list",
      lapply(seq_along(labels), function(index) {
        tags$li(sprintf("%s = %s", index, labels[[index]]))
      })
    )
  )
}

likert_dictionary_manager_ui <- function(built_in, custom, selected = NULL) {
  built_in <- built_in %||% list()
  custom <- custom %||% list()
  built_in_entries <- if (length(built_in) > 0) {
    stats::setNames(paste0("built_in::", names(built_in)), names(built_in))
  } else {
    character(0)
  }
  custom_entries <- if (length(custom) > 0) {
    stats::setNames(paste0("custom::", names(custom)), names(custom))
  } else {
    character(0)
  }
  entries <- c(
    built_in_entries,
    custom_entries
  )
  if (length(entries) == 0) {
    return(div(class = "likert-dictionary-empty", likert_ui_text("No detection dictionaries are registered.", "eb93b1eba19deb909c20eab090eca78020ec82aceca084ec9db420ec9786ec8ab5eb8b88eb8ba42e")))
  }

  if (is.null(selected) || !selected %in% unname(entries)) {
    selected <- unname(entries)[[1]]
  }
  div(
    class = "likert-dictionary-manager",
    div(
      class = "likert-dictionary-list-panel",
      div(class = "likert-dictionary-panel-title", likert_ui_text("Detection dictionaries", "eab090eca78020ec82aceca084")),
      tags$select(
        id = "likert_dictionary_selected",
        class = "likert-dictionary-listbox",
        size = min(max(length(entries), 5), 10),
        lapply(names(entries), function(label) {
          value <- unname(entries[[label]])
          prefix <- if (startsWith(value, "custom::")) {
            likert_ui_text("Custom", "ec82acec9aa9ec9e90")
          } else {
            likert_ui_text("Built-in", "eab8b0ebb3b8")
          }
          tags$option(
            value = value,
            selected = if (identical(value, selected)) "selected" else NULL,
            sprintf("[%s] %s", prefix, label)
          )
        })
      )
    ),
    uiOutput("likert_dictionary_detail_panel")
  )
}

likert_dictionary_detail_ui <- function(built_in, custom, selected = NULL) {
  built_in <- built_in %||% list()
  custom <- custom %||% list()
  built_in_entries <- if (length(built_in) > 0) {
    stats::setNames(paste0("built_in::", names(built_in)), names(built_in))
  } else {
    character(0)
  }
  custom_entries <- if (length(custom) > 0) {
    stats::setNames(paste0("custom::", names(custom)), names(custom))
  } else {
    character(0)
  }
  entries <- c(
    built_in_entries,
    custom_entries
  )
  if (length(entries) == 0) {
    return(div(class = "likert-dictionary-empty", likert_ui_text("No detection dictionaries are registered.", "eb93b1eba19deb909c20eab090eca78020ec82aceca084ec9db420ec9786ec8ab5eb8b88eb8ba42e")))
  }

  if (is.null(selected) || !selected %in% unname(entries)) {
    selected <- unname(entries)[[1]]
  }
  selected_type <- sub("::.*$", "", selected)
  selected_name <- sub("^[^:]+::", "", selected)
  selected_labels <- if (identical(selected_type, "custom")) custom[[selected_name]] else built_in[[selected_name]]
  selected_labels <- trimws(as.character(selected_labels %||% character(0)))
  selected_labels <- selected_labels[nzchar(selected_labels)]
  is_custom <- identical(selected_type, "custom")

  div(
    class = "likert-dictionary-detail-panel",
    div(
      class = "likert-dictionary-panel-title",
      if (is_custom) {
        likert_ui_text("Edit selected dictionary", "ec84a0ed839ded959c20ec82aceca08420ed8eb8eca791")
      } else {
        likert_ui_text("View selected dictionary", "ec84a0ed839ded959c20ec82aceca08420ebb3b4eab8b0")
      }
    ),
    div(
      if (is_custom) {
        tagList(
          textInput("likert_dictionary_edit_name", likert_ui_text("Detection name", "eab090eca78020ec9db4eba684"), value = selected_name, width = "260px"),
          textAreaInput(
            "likert_dictionary_edit_levels",
            likert_ui_text("Labels from low to high", "eb82aeec9d8020eab092ebb680ed84b020eb8692ec9d8020eab09220ec889cec849cec9d9820eb9dbcebb2a8"),
            value = paste(selected_labels, collapse = "\n"),
            rows = max(5, min(length(selected_labels), 10)),
            width = "420px"
          ),
          div(
            class = "likert-dictionary-edit-actions",
            actionButton("update_likert_custom_dictionary", likert_ui_text("Update selected", "ec84a0ed839d20ed95adebaaa920ec9785eb8db0ec9db4ed8ab8"), class = "btn btn-primary"),
            actionButton("delete_likert_custom_dictionary", likert_ui_text("Delete selected", "ec84a0ed839d20ed95adebaaa920ec82adeca09c"), class = "btn btn-default")
          )
        )
      } else {
        tagList(
          div(class = "likert-dictionary-readonly-name", selected_name),
          tags$ol(
            class = "likert-dictionary-label-list likert-dictionary-label-list-readonly",
            lapply(seq_along(selected_labels), function(index) {
              tags$li(sprintf("%s = %s", index, selected_labels[[index]]))
            })
          ),
          div(class = "likert-dictionary-readonly-note", likert_ui_text("Built-in dictionaries are read-only. Add a custom dictionary to edit or delete it.", "eab8b0ebb3b820ec82aceca084ec9d8020ec9dbdeab8b020eca084ec9aa9ec9e85eb8b88eb8ba42e20ed8eb8eca791ed9598eab1b0eb829820ec82adeca09ced9598eba0a4eba9b420ec82acec9aa9ec9e9020ec82aceca084ec9d8420ecb694eab080ed9598ec84b8ec9a942e"))
        )
      }
    )
  )
}

data_editor_likert_panel <- function(language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  options(statedu.app_language = language)
  div(
    class = "page-shell",
    div(
      class = "app-heading",
      h1(likert_ui_text("Auto Likert Conversion", "ec9e90eb8f99204c696b65727420ebb380ed9998", language)),
      div(
        likert_ui_text("Detect text Likert items, map labels to numbers, and apply the same rule to grouped variables.", "ed858dec8aa4ed8ab8204c696b65727420ebacb8ed95adec9d8420eab090eca780ed9598eab3a020eb9dbcebb2a8ec9d8420ec88abec9e90eba19c20eba7a4ed9591ed959c20eb92a42c20eab099ec9d8020eab7b8eba3b920ebb380ec8898ec979020eb8f99ec9dbc20eab79cecb999ec9d8420eca081ec9aa9ed95a9eb8b88eb8ba42e", language),
        class = "app-subtitle"
      )
    ),
    div(
      class = "workspace-panel frequencies-workspace-panel data-editor-workspace",
      analysis_workspace_heading(likert_ui_text("Likert label conversion", "4c696b65727420eb9dbcebb2a820ebb380ed9998", language), "likert", language = language),
      analysis_workspace_body(
        "likert",
        uiOutput("likert_status"),
        likert_custom_dictionary_ui(),
        div(class = "data-editor-result-output", DT::DTOutput("likert_groups")),
        uiOutput("likert_review_panel"),
        div(
          class = "likert-action-row",
          checkboxInput("likert_apply_same_pattern", likert_ui_text("Apply this rule to every variable in the same detected group", "eab099ec9d8020eab090eca78020eab7b8eba3b9ec9d9820ebaaa8eb93a020ebb380ec8898ec979020ec9db420eab79cecb99920eca081ec9aa9"), value = TRUE),
          checkboxInput("likert_reverse", likert_ui_text("Reverse items after conversion", "ebb380ed999820ed9b8420ebacb8ed95ad20ec97adecbd94eb94a9"), value = FALSE),
          div(
            class = "likert-measurement-control",
            selectInput(
              "likert_measurement",
              likert_ui_text("Variable type after conversion", "ebb380ed999820ed9b8420ebb380ec889820ec9ca0ed9895"),
              choices = stats::setNames(
                c("continuous", "ordered", "category", "binary"),
                c(
                  likert_ui_text("Continuous", "ec97b0ec868ded9895"),
                  likert_ui_text("Ordinal", "ec889cec849ced9895"),
                  likert_ui_text("Categorical", "ebb294eca3bced9895"),
                  likert_ui_text("Binary", "ec9db4ebb684ed9895")
                )
              ),
              selected = "continuous",
              width = "220px",
              selectize = FALSE
            )
          ),
          actionButton("apply_likert_conversion", likert_ui_text("Convert selected group", "ec84a0ed839d20eab7b8eba3b920ebb380ed9998"), class = "btn btn-primary")
        ),
        uiOutput("likert_message")
      )
    )
  )
}

register_likert_conversion_handlers <- function(
  input,
  output,
  session,
  dataset_fn,
  raw_dataset_fn,
  current_data_file_fn,
  selected_names_fn,
  update_existing_variable_fn,
  apply_category_label_snapshot_fn,
  mark_settings_dirty,
  language_fn = NULL
) {
  dismissed_file <- reactiveVal("")
  last_message <- reactiveVal(NULL)
  custom_dictionaries <- reactiveVal(read_likert_custom_dictionaries())
  selected_dictionary <- reactiveVal(NULL)

  detected <- reactive({
    file <- current_data_file_fn()
    if (is.null(file)) {
      return(data.frame(check.names = FALSE))
    }
    data <- tryCatch(raw_dataset_fn(), error = function(e) tryCatch(dataset_fn(), error = function(e) NULL))
    detect_likert_variables(data, dictionaries = likert_dictionary(custom_dictionaries()))
  })

  summary <- reactive({
    likert_group_summary(detected())
  })

  output$likert_status <- renderUI({
    statedu_current_language(language_fn)
    groups <- summary()
    if (is.null(groups) || nrow(groups) == 0) {
      return(div(class = "empty-message", div(likert_ui_text("No Likert-style text variables were detected in the current data.", "ed9884ec9eac20eb8db0ec9db4ed84b0ec9790ec849c204c696b65727420ed9895ec8b9dec9d9820ed858dec8aa4ed8ab820ebb380ec8898eab08020eab090eca780eb9098eca78020ec958aec9598ec8ab5eb8b88eb8ba42e"))))
    }
    div(
      class = "recode-same-status",
      sprintf(likert_ui_text("%s Likert group(s), %s variable(s) detected.", "2573eab09c204c696b65727420eab7b8eba3b92c202573eab09c20ebb380ec8898eab08020eab090eca780eb9098ec9788ec8ab5eb8b88eb8ba42e"), nrow(groups), sum(groups$variable_count))
    )
  })

  output$likert_custom_dictionary_status <- renderUI({
    statedu_current_language(language_fn)
    built_in <- likert_dictionary()
    custom <- custom_dictionaries()
    registry_open <- isTRUE(input$likert_dictionary_registry_open)
    selected_current <- isolate(selected_dictionary())
    manager <- div(
      class = "likert-dictionary-registry",
      div(
        class = "likert-custom-status-title",
        sprintf(likert_ui_text("Registered detection dictionaries: %s built-in, %s custom", "eb93b1eba19deb909c20eab090eca78020ec82aceca0843a20eab8b0ebb3b8202573eab09c2c20ec82acec9aa9ec9e90202573eab09c"), length(built_in), length(custom))
      ),
      likert_dictionary_manager_ui(built_in, custom, selected_current)
    )
    div(
      class = "likert-custom-status",
      tags$button(
        type = "button",
        class = paste("likert-disclosure-button likert-secondary-toggle", if (registry_open) "is-open" else ""),
        `data-efs-toggle-target` = "likert-dictionary-registry-panel",
        `data-efs-toggle-input` = "likert_dictionary_registry_open",
        `aria-expanded` = if (registry_open) "true" else "false",
        span(class = "likert-toggle-icon", if (registry_open) "-" else "+"),
        span(likert_ui_text("Show registered detection dictionaries", "eb93b1eba19deb909c20eab090eca78020ec82aceca08420ebb3b4eab8b0"))
      ),
      div(
        id = "likert-dictionary-registry-panel",
        class = "likert-disclosure-panel",
        hidden = if (registry_open) NULL else "hidden",
        manager
      )
    )
  })

  output$likert_dictionary_detail_panel <- renderUI({
    statedu_current_language(language_fn)
    likert_dictionary_detail_ui(likert_dictionary(), custom_dictionaries(), selected_dictionary())
  })

  observeEvent(input$likert_dictionary_selected, {
    selected_dictionary(input$likert_dictionary_selected)
  }, ignoreInit = FALSE)

  observeEvent(input$add_likert_custom_dictionary, {
    labels <- trimws(unlist(strsplit(as.character(input$likert_custom_dictionary_levels %||% ""), "\\r?\\n")))
    labels <- labels[nzchar(labels)]
    if (length(labels) < 3 || length(labels) > 11) {
      showNotification(likert_ui_text("Enter 3 to 11 labels, one per line, from low to high.", "eb9dbcebb2a8ec9d8420eb82aeec9d8020eab092ebb680ed84b020eb8692ec9d8020eab09220ec889cec849ceba19c20ed959c20eca484ec979020ed9598eb8298ec94a92033eab09cec9790ec849c203131eab09ceab98ceca78020ec9e85eba0a5ed9598ec84b8ec9a942e"), type = "warning", duration = 6)
      return()
    }
    if (anyDuplicated(likert_normalize_label(labels))) {
      showNotification(likert_ui_text("Custom dictionary labels must be unique.", "ec82acec9aa9ec9e9020ec82aceca08420eb9dbcebb2a8ec9d8020eca491ebb3b5eb90a020ec889820ec9786ec8ab5eb8b88eb8ba42e"), type = "warning", duration = 6)
      return()
    }
    custom <- custom_dictionaries()
    name <- trimws(as.character(input$likert_custom_dictionary_name %||% ""))
    if (!nzchar(name)) {
      name <- sprintf("custom_%s", length(custom) + 1L)
    }
    name <- gsub("[^A-Za-z0-9_]+", "_", name)
    name <- gsub("^_+|_+$", "", name)
    if (!nzchar(name)) {
      name <- sprintf("custom_%s", length(custom) + 1L)
    }
    if (!startsWith(name, "custom_")) {
      name <- paste0("custom_", name)
    }
    original_name <- name
    suffix <- 2L
    while (name %in% names(likert_dictionary(custom))) {
      name <- sprintf("%s_%s", original_name, suffix)
      suffix <- suffix + 1L
    }
    custom[[name]] <- labels
    custom_dictionaries(custom)
    selected_dictionary(paste0("custom::", name))
    write_likert_custom_dictionaries(custom)
    updateTextInput(session, "likert_custom_dictionary_name", value = "")
    updateTextAreaInput(session, "likert_custom_dictionary_levels", value = "")
    session$sendCustomMessage("easyflow-clear-likert-selection", list())
    showNotification(sprintf(likert_ui_text("Added custom Likert detection dictionary: %s", "ec82acec9aa9ec9e90204c696b65727420eab090eca78020ec82aceca084ec9d8420ecb694eab080ed9688ec8ab5eb8b88eb8ba43a202573"), name), type = "message", duration = 5)
  }, ignoreInit = TRUE)

  observeEvent(input$update_likert_custom_dictionary, {
    selected <- selected_dictionary()
    if (is.null(selected) || !startsWith(selected, "custom::")) {
      showNotification(likert_ui_text("Only custom detection dictionaries can be edited.", "ec82acec9aa9ec9e9020eab090eca78020ec82aceca084eba78c20ed8eb8eca791ed95a020ec889820ec9e88ec8ab5eb8b88eb8ba42e"), type = "warning", duration = 5)
      return()
    }
    old_name <- sub("^custom::", "", selected)
    custom <- custom_dictionaries()
    if (!old_name %in% names(custom)) {
      showNotification(likert_ui_text("Selected custom dictionary was not found.", "ec84a0ed839ded959c20ec82acec9aa9ec9e9020ec82aceca084ec9d8420ecb0beec9d8420ec889820ec9786ec8ab5eb8b88eb8ba42e"), type = "warning", duration = 5)
      return()
    }
    labels <- trimws(unlist(strsplit(as.character(input$likert_dictionary_edit_levels %||% ""), "\\r?\\n")))
    labels <- labels[nzchar(labels)]
    if (length(labels) < 3 || length(labels) > 11) {
      showNotification(likert_ui_text("Enter 3 to 11 labels, one per line, from low to high.", "eb9dbcebb2a8ec9d8420eb82aeec9d8020eab092ebb680ed84b020eb8692ec9d8020eab09220ec889cec849ceba19c20ed959c20eca484ec979020ed9598eb8298ec94a92033eab09cec9790ec849c203131eab09ceab98ceca78020ec9e85eba0a5ed9598ec84b8ec9a942e"), type = "warning", duration = 6)
      return()
    }
    if (anyDuplicated(likert_normalize_label(labels))) {
      showNotification(likert_ui_text("Custom dictionary labels must be unique.", "ec82acec9aa9ec9e9020ec82aceca08420eb9dbcebb2a8ec9d8020eca491ebb3b5eb90a020ec889820ec9786ec8ab5eb8b88eb8ba42e"), type = "warning", duration = 6)
      return()
    }
    new_name <- trimws(as.character(input$likert_dictionary_edit_name %||% ""))
    new_name <- gsub("[^A-Za-z0-9_]+", "_", new_name)
    new_name <- gsub("^_+|_+$", "", new_name)
    if (!nzchar(new_name)) {
      showNotification(likert_ui_text("Enter a detection name.", "eab090eca78020ec9db4eba684ec9d8420ec9e85eba0a5ed9598ec84b8ec9a942e"), type = "warning", duration = 5)
      return()
    }
    if (!startsWith(new_name, "custom_")) {
      new_name <- paste0("custom_", new_name)
    }
    if (!identical(new_name, old_name) && new_name %in% names(likert_dictionary(custom))) {
      showNotification(likert_ui_text("A detection dictionary with that name already exists.", "eab099ec9d8020ec9db4eba684ec9d9820eab090eca78020ec82aceca084ec9db420ec9db4ebafb820ec9e88ec8ab5eb8b88eb8ba42e"), type = "warning", duration = 6)
      return()
    }
    custom[[old_name]] <- NULL
    custom[[new_name]] <- labels
    custom_dictionaries(custom)
    selected_dictionary(paste0("custom::", new_name))
    write_likert_custom_dictionaries(custom)
    session$sendCustomMessage("easyflow-clear-likert-selection", list())
    showNotification(sprintf(likert_ui_text("Updated custom Likert detection dictionary: %s", "ec82acec9aa9ec9e90204c696b65727420eab090eca78020ec82aceca084ec9d8420ec9785eb8db0ec9db4ed8ab8ed9688ec8ab5eb8b88eb8ba43a202573"), new_name), type = "message", duration = 5)
  }, ignoreInit = TRUE)

  observeEvent(input$delete_likert_custom_dictionary, {
    selected <- selected_dictionary()
    if (is.null(selected) || !startsWith(selected, "custom::")) {
      showNotification(likert_ui_text("Only custom detection dictionaries can be deleted.", "ec82acec9aa9ec9e9020eab090eca78020ec82aceca084eba78c20ec82adeca09ced95a020ec889820ec9e88ec8ab5eb8b88eb8ba42e"), type = "warning", duration = 5)
      return()
    }
    name <- sub("^custom::", "", selected)
    custom <- custom_dictionaries()
    if (!name %in% names(custom)) {
      showNotification(likert_ui_text("Selected custom dictionary was not found.", "ec84a0ed839ded959c20ec82acec9aa9ec9e9020ec82aceca084ec9d8420ecb0beec9d8420ec889820ec9786ec8ab5eb8b88eb8ba42e"), type = "warning", duration = 5)
      return()
    }
    custom[[name]] <- NULL
    custom_dictionaries(custom)
    selected_dictionary(NULL)
    write_likert_custom_dictionaries(custom)
    session$sendCustomMessage("easyflow-clear-likert-selection", list())
    showNotification(sprintf(likert_ui_text("Deleted custom Likert detection dictionary: %s", "ec82acec9aa9ec9e90204c696b65727420eab090eca78020ec82aceca084ec9d8420ec82adeca09ced9688ec8ab5eb8b88eb8ba43a202573"), name), type = "message", duration = 5)
  }, ignoreInit = TRUE)

  observeEvent(input$clear_likert_custom_dictionaries, {
    custom_dictionaries(list())
    selected_dictionary(NULL)
    write_likert_custom_dictionaries(list())
    session$sendCustomMessage("easyflow-clear-likert-selection", list())
    showNotification(likert_ui_text("Custom Likert detection dictionaries were cleared.", "ec82acec9aa9ec9e90204c696b65727420eab090eca78020ec82aceca084ec9d8420ecb488eab8b0ed9994ed9688ec8ab5eb8b88eb8ba42e"), type = "message", duration = 4)
  }, ignoreInit = TRUE)

  output$likert_groups <- DT::renderDT({
    table <- likert_detection_table_display(summary())
    if ("Message" %in% names(table)) {
      return(DT::datatable(table, rownames = FALSE, options = list(dom = "t", ordering = FALSE)))
    }
    DT::datatable(
      table,
      rownames = FALSE,
      escape = FALSE,
      selection = list(mode = "single", selected = 1, target = "row"),
      class = "compact stripe hover likert-group-table",
      options = list(
        pageLength = 8,
        lengthChange = FALSE,
        scrollX = FALSE,
        autoWidth = FALSE,
        ordering = FALSE,
        columnDefs = list(
          list(width = "58px", targets = 0, orderable = FALSE, searchable = FALSE, className = "dt-center likert-select-col"),
          list(width = "76px", targets = 1, className = "likert-group-col"),
          list(width = "650px", targets = 2, className = "likert-variables-col"),
          list(width = "56px", targets = 3, className = "dt-center likert-count-col"),
          list(width = "56px", targets = 4, className = "dt-center likert-levels-col"),
          list(width = "140px", targets = 5, className = "likert-dictionary-col")
        ),
        drawCallback = DT::JS(
          "function(settings){",
          "  var inputs = $(this.api().table().body()).find('input.likert-group-select');",
          "  if(!inputs.length) return;",
          "  var value = window.easyflowLikertSelected || inputs.filter(':checked').first().val() || inputs.first().val();",
          "  if(!inputs.filter('[value=\"' + value + '\"]').length) value = inputs.first().val();",
          "  window.easyflowLikertSelected = value;",
          "  inputs.prop('checked', false);",
          "  inputs.filter('[value=\"' + value + '\"]').prop('checked', true);",
          "  if(value && window.Shiny){ Shiny.setInputValue('likert_group_selected', value, {priority:'event'}); }",
          "}"
        )
      ),
      callback = DT::JS(
        "function setLikertSelection(input){",
        "  if(!input || !input.length) return;",
        "  var value = input.val();",
        "  window.easyflowLikertSelected = value;",
        "  $(table.rows().nodes()).find('input.likert-group-select').prop('checked', false);",
        "  input.prop('checked', true);",
        "  table.rows().deselect();",
        "  table.row(input.closest('tr')).select();",
        "  if(value && window.Shiny){ Shiny.setInputValue('likert_group_selected', value, {priority:'event'}); }",
        "}",
        "table.on('click', 'input.likert-group-select', function(e){",
        "  e.stopPropagation();",
        "  setLikertSelection($(this));",
        "});",
        "table.on('click', 'tbody tr', function(e){",
        "  if($(e.target).closest('input.likert-group-select').length) return;",
        "  setLikertSelection($(this).find('input.likert-group-select').first());",
        "});",
        "table.on('select', function(e, dt, type, indexes){",
        "  if(type === 'row'){",
        "    var selected = $(table.rows(indexes).nodes()).find('input.likert-group-select');",
        "    if(selected.length && selected.val() !== window.easyflowLikertSelected){ setLikertSelection(selected.first()); }",
        "  }",
        "});"
      )
    )
  })

  selected_group_id <- reactive({
    groups <- summary()
    if (is.null(groups) || nrow(groups) == 0) {
      return("")
    }
    direct <- as.character(input$likert_group_selected %||% "")
    if (nzchar(direct) && direct %in% as.character(groups$group_id)) {
      return(direct)
    }
    groups$group_id[[1]]
  })

  selected_group <- reactive({
    group_id <- selected_group_id()
    data <- detected()
    if (!nzchar(group_id) || is.null(data) || nrow(data) == 0) {
      return(NULL)
    }
    data[data$group_id == group_id, , drop = FALSE]
  })

  output$likert_review_panel <- renderUI({
    statedu_current_language(language_fn)
    group <- selected_group()
    if (is.null(group) || nrow(group) == 0) {
      return(likert_review_panel_ui(character(0), NULL))
    }
    representative_mapping <- likert_group_representative_mapping(group)
    likert_review_panel_ui(likert_variable_choice_labels(group, representative_mapping), representative_mapping)
  })

  output$likert_message <- renderUI({
    statedu_current_language(language_fn)
    message <- last_message()
    if (is.null(message)) {
      return(NULL)
    }
    div(class = "recode-same-status", message)
  })

  observeEvent(current_data_file_fn(), {
    file <- current_data_file_fn()
    groups <- summary()
    file_key <- as.character(file$name %||% file$path %||% "")
    if (is.null(file) || !nzchar(file_key) || identical(dismissed_file(), file_key) || is.null(groups) || nrow(groups) == 0) {
      return()
    }
    showModal(modalDialog(
      title = likert_ui_text("Likert text variables detected", "4c696b65727420ed858dec8aa4ed8ab820ebb380ec8898eab08020eab090eca780eb9098ec9788ec8ab5eb8b88eb8ba4"),
      sprintf(likert_ui_text("%s Likert group(s), %s variable(s) were detected. Review and convert them before analysis?", "2573eab09c204c696b65727420eab7b8eba3b92c202573eab09c20ebb380ec8898eab08020eab090eca780eb9098ec9788ec8ab5eb8b88eb8ba42e20ebb684ec849d20eca084ec979020eab280ed86a0ed9598eab3a020ebb380ed9998ed9598ec8b9ceab2a0ec8ab5eb8b88eab98c3f"), nrow(groups), sum(groups$variable_count)),
      footer = tagList(
        modalButton(likert_ui_text("Later", "eb8298eca491ec9790")),
        actionButton("open_likert_conversion", likert_ui_text("Review and convert", "eab280ed86a020ebb08f20ebb380ed9998"), class = "btn-primary")
      ),
      easyClose = TRUE
    ))
    dismissed_file(file_key)
  }, ignoreInit = TRUE)

  observeEvent(input$open_likert_conversion, {
    removeModal()
    updateNavbarPage(session, "main_menu", selected = "data_editor_likert")
  }, ignoreInit = TRUE)

  observeEvent(input$apply_likert_conversion, {
    group <- selected_group()
    data <- tryCatch(raw_dataset_fn(), error = function(e) tryCatch(dataset_fn(), error = function(e) NULL))
    if (is.null(group) || nrow(group) == 0 || is.null(data)) {
      showNotification(likert_ui_text("No detected Likert group is available.", "ec82acec9aa920eab080eb8aa5ed959c204c696b65727420eab7b8eba3b9ec9db420ec9786ec8ab5eb8b88eb8ba42e"), type = "warning", duration = 5)
      return()
    }
    variables <- if (isTRUE(input$likert_apply_same_pattern)) {
      group$variable
    } else {
      intersect(as.character(input$likert_variables %||% character(0)), group$variable)
    }
    variables <- intersect(variables, names(data))
    if (length(variables) == 0) {
      showNotification(likert_ui_text("Select at least one variable to convert.", "ebb380ed9998ed95a020ebb380ec8898eba5bc20ed9598eb829820ec9db4ec838120ec84a0ed839ded9598ec84b8ec9a942e"), type = "warning", duration = 5)
      return()
    }
    mapping <- likert_mapping_from_input(input, likert_group_representative_mapping(group))
    if (any(is.na(mapping$value)) || length(unique(mapping$value)) != nrow(mapping)) {
      showNotification(likert_ui_text("Numeric mapping values must be complete and unique.", "ec88abec9e9020eba7a4ed959120eab092ec9d8020ebb9a0eca790ec9786ec9db420ec9e85eba0a5eb9098ec96b4ec95bc20ed9598eba9b020eca491ebb3b5eb90a020ec889820ec9786ec8ab5eb8b88eb8ba42e"), type = "warning", duration = 6)
      return()
    }
    measurement <- as.character(input$likert_measurement %||% "continuous")
    if (!measurement %in% c("continuous", "ordered", "category", "binary")) {
      measurement <- "continuous"
    }

    converted <- character(0)
    for (variable in variables) {
      values <- recode_likert_values(data[[variable]], mapping, reverse = isTRUE(input$likert_reverse))
      ok <- update_existing_variable_fn(variable, values, measurement = measurement)
      if (isTRUE(ok)) {
        converted <- c(converted, variable)
      }
    }
    if (length(converted) == 0) {
      last_message(likert_ui_text("No variables were converted.", "ebb380ed9998eb909c20ebb380ec8898eab08020ec9786ec8ab5eb8b88eb8ba42e"))
      return()
    }
    session$sendCustomMessage(
      "easyflow-update-measurements",
      as.list(stats::setNames(rep(measurement, length(converted)), converted))
    )

    if (is.function(apply_category_label_snapshot_fn)) {
      apply_category_label_snapshot_fn(list(
        category_labels = likert_category_payload(converted, mapping, reverse = isTRUE(input$likert_reverse)),
        measurements = stats::setNames(rep(measurement, length(converted)), converted),
        var_labels = character(0)
      ))
    }
    if (is.function(mark_settings_dirty)) {
      mark_settings_dirty()
    }
    group_label <- as.character(group$dictionary[[1]] %||% group$group_id[[1]] %||% "Likert")
    last_message(sprintf(
      likert_ui_text("Converted %s Likert variable(s) from %s: %s", "2573eab09c204c696b65727420ebb380ec8898eba5bc202573ec9790ec849c20ebb380ed9998ed9688ec8ab5eb8b88eb8ba43a202573"),
      length(converted),
      group_label,
      paste(converted, collapse = ", ")
    ))
  }, ignoreInit = TRUE)

  invisible(TRUE)
}
