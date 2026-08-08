# Update-check helpers for StatEdu Studio.

statedu_update_manifest_url <- function() {
  Sys.getenv(
    "STATEDU_UPDATE_MANIFEST_URL",
    "https://studio.statedu.com/releases/latest.json"
  )
}

statedu_normalize_version <- function(version) {
  version <- trimws(as.character(version %||% "")[[1]])
  match <- regexpr("[0-9]+(\\.[0-9]+)*", version)
  if (is.na(match) || match < 1) {
    return("")
  }
  regmatches(version, match)[[1]]
}

statedu_compare_versions <- function(current_version, latest_version) {
  current <- statedu_normalize_version(current_version)
  latest <- statedu_normalize_version(latest_version)
  if (!nzchar(current) || !nzchar(latest)) {
    return(NA_integer_)
  }
  utils::compareVersion(current, latest)
}

statedu_manifest_value <- function(manifest, key, default = "") {
  if (is.null(manifest) || !is.list(manifest)) {
    return(default)
  }
  manifest_names <- names(manifest)
  if (is.null(manifest_names) || !key %in% manifest_names || is.null(manifest[[key]]) || length(manifest[[key]]) == 0) {
    return(default)
  }
  value <- manifest[[key]][[1]]
  if (is.null(value) || is.na(value)) {
    return(default)
  }
  as.character(value)
}

statedu_manifest_language_value <- function(manifest,
                                            base_key,
                                            language = statedu_initial_language(),
                                            default_ko = "",
                                            default_en = default_ko,
                                            use_generic = TRUE) {
  language <- normalize_app_language(language)
  language_suffix <- function(code) {
    if (!nzchar(code)) return("En")
    paste0(toupper(substr(code, 1L, 1L)), substr(code, 2L, nchar(code)))
  }
  suffix <- language_suffix(language)
  fallback_suffix <- if (identical(language, "ko")) "Ko" else "En"
  language_keys <- c(
    paste0(base_key, suffix),
    paste0(base_key, "_", tolower(language)),
    paste0(base_key, "_", suffix),
    paste0(base_key, fallback_suffix),
    paste0(base_key, "_", tolower(fallback_suffix)),
    paste0(base_key, "_", fallback_suffix)
  )
  for (key in language_keys) {
    value <- statedu_manifest_value(manifest, key)
    if (nzchar(value)) {
      return(value)
    }
  }
  if (isTRUE(use_generic)) {
    value <- statedu_manifest_value(manifest, base_key)
    if (nzchar(value)) {
      return(value)
    }
  }
  if (identical(language, "ko")) default_ko else default_en
}

statedu_check_update <- function(
  current_version,
  manifest_url = statedu_update_manifest_url(),
  timeout = 8
) {
  manifest_url <- as.character(manifest_url %||% "")[[1]]
  current_version <- as.character(current_version %||% "")[[1]]
  if (!nzchar(manifest_url)) {
    return(list(
      status = "error",
      current_version = current_version,
      latest_version = "",
      manifest_url = manifest_url,
      message = "Update manifest URL is empty."
    ))
  }

  previous_timeout <- getOption("timeout")
  on.exit(options(timeout = previous_timeout), add = TRUE)
  options(timeout = max(as.numeric(timeout %||% 8), 1))

  manifest <- tryCatch(
    jsonlite::fromJSON(manifest_url, simplifyVector = TRUE),
    error = function(error) error
  )
  if (inherits(manifest, "error")) {
    return(list(
      status = "error",
      current_version = current_version,
      latest_version = "",
      manifest_url = manifest_url,
      message = conditionMessage(manifest)
    ))
  }

  latest_version <- statedu_manifest_value(manifest, "version")
  comparison <- statedu_compare_versions(current_version, latest_version)
  if (is.na(comparison)) {
    return(list(
      status = "error",
      current_version = current_version,
      latest_version = latest_version,
      manifest_url = manifest_url,
      manifest = manifest,
      message = "Invalid version metadata."
    ))
  }

  status <- if (comparison < 0) "update_available" else "current"
  list(
    status = status,
    current_version = current_version,
    latest_version = latest_version,
    manifest_url = manifest_url,
    manifest = manifest,
    message = statedu_manifest_value(manifest, "messageEn")
  )
}

statedu_update_status_title <- function(result, language = statedu_initial_language()) {
  status <- result$status %||% "error"
  if (identical(status, "update_available")) {
    return(statedu_t("update.new_version_available", language))
  }
  if (identical(status, "current")) {
    return(statedu_t("update.current_version_latest", language))
  }
  statedu_t("update.current_version_latest", language)
}

statedu_update_message <- function(result, language = statedu_initial_language()) {
  manifest <- result$manifest %||% list()
  if (identical(result$status %||% "", "error")) {
    return(statedu_t("update.keep_using_installed", language))
  }
  message <- statedu_manifest_language_value(manifest, "message", language)
  if (nzchar(message)) {
    return(message)
  }
  statedu_update_status_title(result, language)
}

statedu_update_modal <- function(result, language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  manifest <- result$manifest %||% list()
  download_url <- statedu_manifest_language_value(
    manifest,
    "downloadUrl",
    language,
    default_ko = "https://studio.statedu.com/download/",
    default_en = "https://studio.statedu.com/en/download/",
    use_generic = identical(language, "ko")
  )
  release_notes_url <- statedu_manifest_language_value(
    manifest,
    "releaseNotesUrl",
    language,
    use_generic = TRUE
  )
  latest_version <- result$latest_version %||% statedu_manifest_value(manifest, "version")
  if (!nzchar(latest_version) && identical(result$status %||% "", "error")) {
    latest_version <- result$current_version %||% ""
  }
  release_date <- statedu_manifest_value(manifest, "releaseDate")
  minimum_supported <- statedu_manifest_value(manifest, "minimumSupportedVersion")
  channel <- statedu_manifest_value(manifest, "channel")

  rows <- list(
    about_info_row(statedu_t("update.current_version", language), paste0("v", result$current_version %||% "")),
    about_info_row(statedu_t("update.latest_version", language), if (nzchar(latest_version)) paste0("v", latest_version) else "-")
  )
  if (nzchar(release_date)) {
    rows <- c(rows, list(about_info_row(statedu_t("about.release_date", language), release_date)))
  }
  if (nzchar(channel)) {
    rows <- c(rows, list(about_info_row(statedu_t("update.channel", language), channel)))
  }
  if (nzchar(minimum_supported)) {
    rows <- c(rows, list(about_info_row(statedu_t("update.minimum_supported_version", language), paste0("v", minimum_supported))))
  }

  link_tags <- list()
  if (nzchar(download_url)) {
    link_tags <- c(
      link_tags,
      list(
        tags$a(
          class = "btn btn-primary",
          href = download_url,
          target = "_blank",
          rel = "noopener noreferrer",
          statedu_t("update.check_latest_version", language)
        )
      )
    )
  }
  if (nzchar(release_notes_url)) {
    link_tags <- c(
      link_tags,
      list(
        tags$a(
          class = "btn btn-default",
          href = release_notes_url,
          target = "_blank",
          rel = "noopener noreferrer",
          statedu_t("update.release_notes", language)
        )
      )
    )
  }

  modalDialog(
    title = statedu_t("update.result_title", language),
    div(
      class = "about-application-document statedu-update-result",
      h3(statedu_update_status_title(result, language)),
      p(statedu_update_message(result, language)),
      div(class = "about-info-grid", rows),
      div(class = "about-update-actions", do.call(tagList, link_tags))
    ),
    easyClose = TRUE,
    footer = modalButton(statedu_t("ui.close", language))
  )
}
