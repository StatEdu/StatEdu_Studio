script_path <- if (length(grep("^--file=", commandArgs(FALSE), value = TRUE)) > 0) {
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1]])
} else {
  "scripts/validate_i18n_contract.R"
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)

read_text <- function(path) {
  paste(readLines(file.path(repo_root, path), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

assert <- function(condition, message) {
  if (!isTRUE(condition)) {
    stop(message, call. = FALSE)
  }
}

message("Checking i18n contract...")

source_roots <- c(
  "R",
  "modules/latent_mplus/app/R"
)
source_files <- unlist(lapply(source_roots, function(root_path) {
  absolute_root <- file.path(repo_root, root_path)
  if (!dir.exists(absolute_root)) {
    return(character(0))
  }
  list.files(absolute_root, pattern = "[.]R$", recursive = TRUE, full.names = TRUE)
}), use.names = FALSE)
module_entry_files <- c(
  "modules/latent_mplus/app/app.R",
  "modules/latent_mplus/app/run_app.R",
  "modules/latent_mplus/app/run_latent_mplus.R"
)
module_entry_files <- file.path(repo_root, module_entry_files[file.exists(file.path(repo_root, module_entry_files))])
source_files <- unique(c(source_files, module_entry_files))
source_paths <- sub(paste0("^", gsub("([\\^$.|?*+(){}\\[\\]\\\\])", "\\\\\\1", repo_root), "/?"), "", normalizePath(source_files, winslash = "/", mustWork = TRUE))

direct_calls <- character(0)
for (path in source_paths) {
  lines <- readLines(file.path(repo_root, path), warn = FALSE, encoding = "UTF-8")
  hits <- grep("statedu_text\\s*\\(", lines, perl = TRUE)
  if (length(hits) > 0) {
    direct_calls <- c(direct_calls, sprintf("%s:%s", path, hits))
  }
}
assert(length(direct_calls) == 0, paste("Direct statedu_text() call(s) found:", paste(direct_calls, collapse = ", ")))

mojibake_markers <- c(
  intToUtf8(0xFFFD), # replacement character
  intToUtf8(0x907A),
  intToUtf8(0x5BC3),
  intToUtf8(0x936E),
  intToUtf8(0x5AC4),
  intToUtf8(0x6FE1),
  intToUtf8(0x745C),
  intToUtf8(0x6028)
)
mojibake_hits <- character(0)
for (path in c("R/labels.R", "scripts/validate_i18n_contract.R")) {
  lines <- readLines(file.path(repo_root, path), warn = FALSE, encoding = "UTF-8")
  for (marker in mojibake_markers) {
    hits <- grep(marker, lines, fixed = TRUE)
    if (length(hits) > 0) {
      mojibake_hits <- c(mojibake_hits, sprintf("%s:%s", path, hits))
    }
  }
}
assert(length(mojibake_hits) == 0, paste("Potential mojibake marker(s) found:", paste(unique(mojibake_hits), collapse = ", ")))

source(file.path(repo_root, "R", "utils.R"), encoding = "UTF-8")
source(file.path(repo_root, "R", "labels.R"), encoding = "UTF-8")
source(file.path(repo_root, "R", "sample_size_ui.R"), encoding = "UTF-8")
source(file.path(repo_root, "R", "setup_complex_sample_ui.R"), encoding = "UTF-8")
source(file.path(repo_root, "R", "setup_mediation_moderation_ui.R"), encoding = "UTF-8")

languages <- statedu_supported_languages()
assert(all(c("ko", "en") %in% languages), "Supported languages must include ko and en.")
assert(all(c("ja", "zh") %in% languages), "External language packs must register ja and zh.")
assert(all(c("es", "fr", "de", "vi") %in% languages), "Expanded language packs must register es, fr, de, and vi.")
assert(identical(statedu_default_language(), "ko"), "Default language must remain Korean.")
assert(identical(normalize_app_language("korean"), "ko"), "Korean language alias is not normalized.")
assert(identical(normalize_app_language("english"), "en"), "English language alias is not normalized.")
assert(identical(normalize_app_language("japanese"), "ja"), "Japanese language alias is not normalized.")
assert(identical(normalize_app_language("chinese"), "zh"), "Chinese language alias is not normalized.")
assert(identical(normalize_app_language("spanish"), "es"), "Spanish language alias is not normalized.")
assert(identical(normalize_app_language("french"), "fr"), "French language alias is not normalized.")
assert(identical(normalize_app_language("german"), "de"), "German language alias is not normalized.")
assert(identical(normalize_app_language("vietnamese"), "vi"), "Vietnamese language alias is not normalized.")

table <- statedu_translation_table()
missing_language_rows <- names(Filter(function(key) {
  values <- statedu_translation_row(key)
  is.null(values) || !all(languages %in% names(values))
}, names(table)))
assert(length(missing_language_rows) == 0, paste("Translation row(s) missing supported language values:", paste(missing_language_rows, collapse = ", ")))

label_lines <- readLines(file.path(repo_root, "R", "labels.R"), warn = FALSE, encoding = "UTF-8")
label_key_matches <- regmatches(label_lines, gregexpr("^\\s*([A-Za-z0-9_.]+)\\s*=\\s*row\\(", label_lines, perl = TRUE))
label_keys <- unlist(lapply(label_key_matches, function(match) {
  if (length(match) == 0 || identical(match, character(0))) {
    return(character(0))
  }
  sub("^\\s*([A-Za-z0-9_.]+)\\s*=\\s*row\\($", "\\1", match, perl = TRUE)
}), use.names = FALSE)
duplicated_label_keys <- unique(label_keys[duplicated(label_keys)])
assert(length(duplicated_label_keys) == 0, paste("Duplicate translation key(s) in labels.R:", paste(duplicated_label_keys, collapse = ", ")))

all_source_text <- paste(vapply(c(source_paths, "scripts/validate_i18n_contract.R"), read_text, character(1)), collapse = "\n")
extract_literal_arg <- function(pattern, text) {
  matches <- regmatches(text, gregexpr(pattern, text, perl = TRUE))[[1]]
  if (length(matches) == 1 && identical(matches, "")) {
    return(character(0))
  }
  sub(pattern, "\\1", matches, perl = TRUE)
}

latent_translation_key <- function(en) {
  value <- tolower(trimws(as.character(en %||% "")))
  value <- gsub("[^a-z0-9]+", "_", value)
  value <- gsub("^_+|_+$", "", value)
  if (!nzchar(value)) {
    return("latent.text")
  }
  paste0("latent.", value)
}

latent_literal_value <- function(expr) {
  if (is.character(expr) && length(expr) == 1) {
    return(expr)
  }
  if (is.call(expr) && identical(as.character(expr[[1]]), "latent_utf8") && length(expr) >= 2 && is.character(expr[[2]])) {
    return(statedu_utf8(expr[[2]]))
  }
  NULL
}

latent_call_keys <- local({
  keys <- character(0)
  add_label <- function(expr) {
    label <- latent_literal_value(expr)
    if (!is.null(label) && nzchar(label)) {
      keys <<- c(keys, latent_translation_key(label))
    }
  }
  walk <- function(expr) {
    if (is.call(expr) && identical(as.character(expr[[1]]), "latent_text") && length(expr) >= 2) {
      add_label(expr[[2]])
    }
    if (is.call(expr) && identical(as.character(expr[[1]]), "latent_choices") && length(expr) >= 3) {
      label_expr <- expr[[3]]
      if (is.call(label_expr) && identical(as.character(label_expr[[1]]), "c") && length(label_expr) >= 2) {
        for (i in seq.int(2, length(label_expr))) {
          add_label(label_expr[[i]])
        }
      }
    }
    if (is.recursive(expr)) {
      for (i in seq_along(expr)) {
        walk(expr[[i]])
      }
    }
  }
  for (path in source_paths[grepl("modules/latent_mplus/app/R/latent_ui[.]R$", source_paths)]) {
    walk(parse(file.path(repo_root, path), encoding = "UTF-8"))
  }
  unique(keys)
})

referenced_keys <- unique(c(
  extract_literal_arg("statedu_t\\(\\s*\"([A-Za-z0-9_.]+)\"", all_source_text),
  extract_literal_arg("statedu_translate\\(\\s*\"([A-Za-z0-9_.]+)\"", all_source_text),
  extract_literal_arg("statedu_translation_row\\(\\s*\"([A-Za-z0-9_.]+)\"", all_source_text),
  paste0("ui.", extract_literal_arg("statedu_ui_label\\(\\s*\"([A-Za-z0-9_]+)\"", all_source_text)),
  latent_call_keys
))
referenced_keys <- referenced_keys[nzchar(referenced_keys)]
missing_referenced_keys <- setdiff(referenced_keys, names(table))
assert(length(missing_referenced_keys) == 0, paste("Referenced translation key(s) missing from labels.R:", paste(sort(missing_referenced_keys), collapse = ", ")))

latent_table_keys <- names(table)[startsWith(names(table), "latent.")]
assert(length(latent_table_keys) > 0, "Latent translation namespace is empty.")
for (language in setdiff(languages, c("ko", "en"))) {
  overlay <- statedu_locale_overlay(language)
  missing_overlay_keys <- setdiff(latent_table_keys, names(overlay))
  extra_overlay_keys <- setdiff(names(overlay)[startsWith(names(overlay), "latent.")], latent_table_keys)
  assert(length(missing_overlay_keys) == 0, paste("Latent translation key(s) missing from", language, "overlay:", paste(sort(missing_overlay_keys), collapse = ", ")))
  assert(length(extra_overlay_keys) == 0, paste("Latent translation key(s) extra in", language, "overlay:", paste(sort(extra_overlay_keys), collapse = ", ")))
}

assert(identical(statedu_t("ui.data", "ko"), statedu_utf8("eb8db0ec9db4ed84b0")), "Korean ui.data translation mismatch.")
assert(identical(statedu_t("ui.data", "en"), "Data"), "English ui.data translation mismatch.")
assert(identical(statedu_t("ui.data", "ja"), statedu_utf8("e38387e383bce382bf")), "Japanese ui.data translation mismatch.")
assert(identical(statedu_t("ui.data", "zh"), statedu_utf8("e695b0e68dae")), "Chinese ui.data translation mismatch.")
assert(identical(statedu_t("ui.open_data_file", "ja"), statedu_utf8("e38387e383bce382bfe38395e382a1e382a4e383abe38292e9968be3818f")), "Japanese open-data-file translation mismatch.")
assert(identical(statedu_language_display_name("ja", "ko"), statedu_utf8("ec9dbcebb3b8ec96b4")), "Japanese display name in Korean mismatch.")
assert(identical(statedu_t("result.empty_message", "en"), "Click Add result after an analysis to collect results here."), "Result empty English text mismatch.")
assert(identical(statedu_t("result.file_empty", "ko"), statedu_utf8("ec84a0ed839ded959c20ed8c8cec9dbcec979020eca080ec9ea5eb909c20eab2b0eab3bc20ed95adebaaa9ec9db420ec9786ec8ab5eb8b88eb8ba42e")), "Result file-empty Korean text mismatch.")

assert(identical(sample_size_ui_text("en", "calculate"), "Calculate"), "Sample-size English label mismatch.")
assert(identical(sample_size_ui_text("ko", "calculate"), statedu_utf8("eab384ec82b0")), "Sample-size Korean label mismatch.")
assert(identical(sample_size_label("en", "Power"), "Power"), "Sample-size English Power label mismatch.")
assert(identical(sample_size_label("ko", "Power"), statedu_utf8("eab280eca095eba0a5")), "Sample-size Korean Power label mismatch.")

assert(identical(complex_sample_ui_text("design_tab", "en"), "Design variables"), "Complex-sample English design tab mismatch.")
assert(identical(complex_sample_ui_text("design_tab", "ko"), statedu_utf8("ec84a4eab384ebb380ec8898")), "Complex-sample Korean design tab mismatch.")
assert(identical(complex_sample_yes_no(TRUE, "en"), "Yes"), "Complex-sample English yes/no mismatch.")
assert(identical(complex_sample_yes_no(FALSE, "ko"), statedu_utf8("ec9584eb8b88ec98a4")), "Complex-sample Korean yes/no mismatch.")

assert(identical(mediation_moderation_title("en"), "Mediation / Moderation"), "Mediation/moderation English title mismatch.")
assert(identical(mediation_moderation_title("ko"), statedu_utf8("eba7a4eab09cc2b7eca1b0eca088")), "Mediation/moderation Korean title mismatch.")

cat("i18n contract validation passed.\n")
