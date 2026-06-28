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
  if (is.null(manifest) || is.null(manifest[[key]]) || length(manifest[[key]]) == 0) {
    return(default)
  }
  value <- manifest[[key]][[1]]
  if (is.null(value) || is.na(value)) {
    return(default)
  }
  as.character(value)
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
  options(timeout = max(as.numeric(timeout %||% 8), previous_timeout))

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
    return(statedu_text(language, "A new version is available", statedu_utf8("ec838820ebb284eca084ec9db420ec9e88ec8ab5eb8b88eb8ba4")))
  }
  if (identical(status, "current")) {
    return(statedu_text(language, "You are on the latest version", statedu_utf8("ed9884ec9eac20ebb284eca084ec9db420ecb59cec8ba0ec9e85eb8b88eb8ba4")))
  }
  statedu_text(language, "Update check failed", statedu_utf8("ed9995ec9db820ec8ba4ed8ca8"))
}

statedu_update_message <- function(result, language = statedu_initial_language()) {
  manifest <- result$manifest %||% list()
  if (identical(result$status %||% "", "error")) {
    return(statedu_text(
      language,
      "Could not check for updates. Check your internet connection or the update manifest.",
      statedu_utf8("ec9785eb8db0ec9db4ed8ab8eba5bc20ed9995ec9db8ed95a020ec889820ec9786ec8ab5eb8b88eb8ba42e")
    ))
  }
  message_key <- if (identical(normalize_app_language(language), "ko")) "messageKo" else "messageEn"
  message <- statedu_manifest_value(manifest, message_key)
  if (nzchar(message)) {
    return(message)
  }
  statedu_update_status_title(result, language)
}

statedu_update_modal <- function(result, language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  manifest <- result$manifest %||% list()
  download_url <- statedu_manifest_value(manifest, "downloadUrl", "https://studio.statedu.com/download/")
  release_notes_url <- statedu_manifest_value(manifest, "releaseNotesUrl")
  latest_version <- result$latest_version %||% statedu_manifest_value(manifest, "version")
  release_date <- statedu_manifest_value(manifest, "releaseDate")
  minimum_supported <- statedu_manifest_value(manifest, "minimumSupportedVersion")
  channel <- statedu_manifest_value(manifest, "channel")

  rows <- list(
    about_info_row(statedu_text(language, "Current version", statedu_utf8("ed9884ec9eac20ebb284eca084")), paste0("v", result$current_version %||% "")),
    about_info_row(statedu_text(language, "Latest version", statedu_utf8("ecb59cec8ba020ebb284eca084")), if (nzchar(latest_version)) paste0("v", latest_version) else "-")
  )
  if (nzchar(release_date)) {
    rows <- c(rows, list(about_info_row(statedu_text(language, "Release date", statedu_utf8("ebb0b0ed8facec9dbc")), release_date)))
  }
  if (nzchar(channel)) {
    rows <- c(rows, list(about_info_row(statedu_text(language, "Channel", statedu_utf8("ecb184eb8490")), channel)))
  }
  if (nzchar(minimum_supported)) {
    rows <- c(rows, list(about_info_row(statedu_text(language, "Minimum supported version", statedu_utf8("ecb59cec868c20eca780ec9b9020ebb284eca084")), paste0("v", minimum_supported))))
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
          statedu_text(language, "Download page", statedu_utf8("eb8ba4ec9ab4eba19ceb939c20ed8e98ec9db4eca780"))
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
          statedu_text(language, "Release notes", statedu_utf8("eba6b4eba6acec8aa420eb85b8ed8ab8"))
        )
      )
    )
  }

  modalDialog(
    title = statedu_text(language, "Update check result", statedu_utf8("ec9785eb8db0ec9db4ed8ab820ed9995ec9db820eab2b0eab3bc")),
    div(
      class = "about-application-document statedu-update-result",
      h3(statedu_update_status_title(result, language)),
      p(statedu_update_message(result, language)),
      div(class = "about-info-grid", rows),
      div(class = "about-update-actions", do.call(tagList, link_tags))
    ),
    easyClose = TRUE,
    footer = modalButton(statedu_text(language, "Close", statedu_utf8("eb8babeab8b0")))
  )
}
