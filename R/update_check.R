# Update-check helpers for StatEdu Studio.

# This policy controls supported versions, not Pro license entitlements.
# Future Pro builds must set STATEDU_PRODUCT_EDITION=pro and publish an
# editions.pro policy; the legacy top-level manifest applies only to free.
statedu_version_policy <- function(manifest, current_version,
                                   edition = Sys.getenv("STATEDU_PRODUCT_EDITION", "free")) {
  invalid <- list(valid = FALSE, blocked = FALSE, minimum_version = "")
  scalar <- function(x) is.character(x) && length(x) == 1L && !is.na(x)
  version <- function(x) scalar(x) && grepl("^[0-9]+\\.[0-9]+\\.[0-9]+(-[A-Za-z0-9.-]+)?$", x)
  if (!is.list(manifest) || !version(current_version)) return(invalid)
  if (!is.null(manifest$editions)) {
    if (!is.list(manifest$editions) || !is.list(manifest$editions[[edition]])) return(invalid)
    manifest <- manifest$editions[[edition]]
  } else if (!identical(edition, "free")) return(invalid)
  latest <- manifest$latest_version %||% manifest$version
  minimum <- manifest$minimum_version %||% manifest$minimumSupportedVersion
  if (!version(latest)) return(invalid)
  if (is.null(minimum)) return(list(valid = TRUE, blocked = FALSE, minimum_version = ""))
  if (!version(minimum) || statedu_compare_versions(minimum, latest) > 0L) return(invalid)
  list(valid = TRUE, blocked = statedu_compare_versions(current_version, minimum) < 0L,
       minimum_version = minimum)
}

statedu_startup_update_policy <- function(current_version,
    manifest_url = statedu_update_manifest_url(),
    cache_path = file.path(statedu_user_settings_dir(), "version-policy.rds"),
    check = statedu_check_update) {
  # Do not permit an insecure transport to establish a mandatory policy.
  result <- tryCatch({
    if (!grepl("^https://", manifest_url)) stop("HTTPS required")
    check(current_version, manifest_url = manifest_url, timeout = 8)
  }, error = function(e) list(status = "error"))
  if (!identical(result$status, "error") &&
      isTRUE(statedu_version_policy(result$manifest, current_version)$valid)) {
    tryCatch({
      dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
      temporary <- tempfile("policy-", tmpdir = dirname(cache_path))
      on.exit(unlink(temporary), add = TRUE)
      saveRDS(list(url = manifest_url, manifest = result$manifest), temporary)
      if (!file.copy(temporary, cache_path, overwrite = TRUE)) warning("Policy cache write failed")
    }, error = function(e) warning("Policy cache write failed: ", conditionMessage(e)))
    result$policy_source <- "network"
    return(result)
  }
  cached <- tryCatch(if (file.exists(cache_path)) readRDS(cache_path) else NULL,
    error = function(e) NULL)
  if (is.list(cached) && identical(cached$url, manifest_url)) {
    policy <- statedu_version_policy(cached$manifest, current_version)
    if (isTRUE(policy$valid)) {
      return(list(status = if (policy$blocked) "update_required" else "current",
        current_version = current_version, minimum_version = policy$minimum_version,
        manifest = cached$manifest, policy_source = "cache"))
    }
  }
  list(status = "error", current_version = current_version, policy_source = "unavailable")
}

statedu_required_update_text <- function(language) {
  if (identical(normalize_app_language(language), "ko")) {
    list(title = "StatEdu Studio 업데이트 필요",
      message = "현재 버전의 지원이 종료되었습니다. 계속 사용하려면 지원되는 버전으로 업데이트해 주세요.",
      download = "업데이트 안내", restart = "업데이트 설치 후 앱을 다시 실행해 주세요.")
  } else {
    list(title = "StatEdu Studio update required",
      message = "This version is no longer supported. Update to a supported version to continue.",
      download = "Update information", restart = "Restart the app after installing the update.")
  }
}

statedu_required_update_ui <- function(result, request = NULL) {
  if (!isTRUE(statedu_request_token_authorized(request))) return(statedu_token_rejection_response())
  language <- statedu_initial_language(request)
  text <- statedu_required_update_text(language)
  # Use the download landing page, not the old manifest's Windows-only EXE.
  # Store packages can bake in their own HTTPS store listing through this setting.
  url <- Sys.getenv("STATEDU_UPDATE_PAGE_URL", if (identical(language, "ko"))
    "https://studio.statedu.com/download/" else "https://studio.statedu.com/en/download/")
  if (!grepl("^https://", url)) url <- "https://studio.statedu.com/download/"
  shiny::fluidPage(
    shiny::tags$head(shiny::tags$title(text$title)),
    shiny::tags$main(style = "max-width:680px;margin:80px auto;padding:32px;",
      shiny::h2(text$title), shiny::p(text$message),
      shiny::p(statedu_t("update.current_version", language), ": ", result$current_version),
      shiny::p(statedu_t("update.minimum_supported_version", language), ": ", result$minimum_version),
      shiny::tags$a(class = "btn btn-primary", href = url, target = "_blank",
        rel = "noopener noreferrer", text$download), shiny::p(text$restart)))
}

# Select the whole Shiny application before registering analysis observers.
# Removing the notice in the browser cannot enable the normal analysis server.
statedu_guarded_application <- function(result, normal_ui, normal_server) {
  if (identical(result$status, "update_required")) {
    return(list(ui = function(request) statedu_required_update_ui(result, request),
                server = function(input, output, session) {}))
  }
  list(ui = normal_ui, server = normal_server)
}

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

  edition <- Sys.getenv("STATEDU_PRODUCT_EDITION", "free")
  edition_manifest <- if (is.list(manifest$editions)) manifest$editions[[edition]] else manifest
  latest_version <- statedu_manifest_value(edition_manifest, "latest_version",
    statedu_manifest_value(edition_manifest, "version"))
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

  policy <- statedu_version_policy(manifest, current_version)
  if (!isTRUE(policy$valid)) {
    return(list(status = "error", current_version = current_version,
      latest_version = latest_version, manifest_url = manifest_url,
      message = "Invalid minimum version policy."))
  }
  status <- if (isTRUE(policy$blocked)) "update_required" else if (comparison < 0) "update_available" else "current"
  list(
    status = status,
    current_version = current_version,
    latest_version = latest_version,
    minimum_version = policy$minimum_version,
    manifest_url = manifest_url,
    manifest = manifest,
    message = statedu_manifest_value(manifest, "messageEn")
  )
}

statedu_update_status_title <- function(result, language = statedu_initial_language()) {
  status <- result$status %||% "error"
  if (identical(status, "update_required")) {
    return(statedu_required_update_text(language)$title)
  }
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
  if (identical(result$status, "update_required")) {
    return(statedu_required_update_text(language)$message)
  }
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
  minimum_supported <- result$minimum_version %||% statedu_manifest_value(manifest, "minimum_version",
    statedu_manifest_value(manifest, "minimumSupportedVersion"))
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
