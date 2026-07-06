# Result and About menu UI.

result_tab_panel <- function(language = statedu_initial_language()) {
  tabPanel(
    statedu_ui_label("result", language),
    value = "result",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(statedu_ui_label("result", language)),
        div(statedu_t("result.subtitle", language), class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel",
        style = "min-width:980px;overflow-x:auto;",
        h3(statedu_ui_label("result", language)),
        div(
          class = "result-toolbar",
          div(
            class = "result-toolbar-group result-toolbar-primary",
            actionButton("open_result_history_dialog", statedu_t("result.open", language), class = "btn-default"),
            analysis_save_button("save_result_history_dialog", statedu_t("result.save", language), "result_history", class = "btn-default"),
            actionButton("clear_saved_results", statedu_t("result.clear", language), class = "btn-default")
          ),
          uiOutput("result_export_controls")
        ),
        uiOutput("saved_results_list")
      )
    )
  )
}

about_read_utf8_text <- function(path) {
  info <- file.info(path)
  if (is.na(info$size) || info$size <= 0) {
    return("")
  }
  bytes <- readBin(path, what = "raw", n = info$size)
  text <- rawToChar(bytes)
  Encoding(text) <- "UTF-8"
  sub("^\ufeff", "", enc2utf8(text))
}

about_decode_r_unicode_escapes <- function(text) {
  text <- enc2utf8(text %||% "")
  tokens <- unique(unlist(regmatches(text, gregexpr("<U\\+[0-9A-Fa-f]{4,6}>", text, perl = TRUE)), use.names = FALSE))
  for (token in tokens) {
    code <- sub("^<U\\+([0-9A-Fa-f]{4,6})>$", "\\1", token)
    decoded <- intToUtf8(strtoi(code, base = 16L))
    text <- gsub(token, decoded, text, fixed = TRUE, useBytes = TRUE)
  }
  enc2utf8(text)
}

about_markdown_document <- function(path) {
  resolved_path <- about_resolve_document_path(path)
  if (!nzchar(resolved_path)) {
    return(div(class = "empty-message", sprintf("Document not found: %s", path)))
  }
  text <- enc2utf8(about_read_utf8_text(resolved_path))
  html <- suppressWarnings(markdown::markdownToHTML(
    text = text,
    fragment.only = TRUE,
    encoding = "UTF-8"
  ))
  div(
    class = "about-markdown-document",
    HTML(about_decode_r_unicode_escapes(html))
  )
}

about_document_roots <- function() {
  roots <- c(
    getwd(),
    normalizePath(".", winslash = "/", mustWork = FALSE),
    Sys.getenv("STATEDU_APP_DIR", ""),
    file.path(getwd(), "dist", "electron", "win-unpacked", "resources", "app", "app")
  )
  unique(normalizePath(roots[nzchar(roots)], winslash = "/", mustWork = FALSE))
}

about_resolve_document_path <- function(path) {
  if (file.exists(path)) {
    return(path)
  }
  candidates <- file.path(about_document_roots(), path)
  existing <- candidates[file.exists(candidates)]
  if (length(existing) > 0) {
    return(existing[[1]])
  }
  ""
}

about_document_language_label <- function(language) {
  language <- normalize_app_language(language)
  statedu_language_display_name(language, language)
}

about_document_specs <- function(language = "ko") {
  language <- normalize_app_language(language)
  if (!identical(language, "ko")) {
    return(list(
      overview = list(title = "Overview", path = "README.md", subtitle = "Project scope, current version, validation, and citation."),
      user_guide = list(title = "User Guide", path = file.path("docs", "USER_GUIDE_EN.md"), subtitle = "Step-by-step operating guide for loading data, selecting variables, running analyses, and saving results."),
      analysis_methods = list(title = "Analyses", path = file.path("docs", "ANALYSIS_METHODS_EN.md"), subtitle = "Implementation inventory of analysis menus, statistical outputs, tables, and export coverage."),
      method_notes = list(title = "Method Notes", path = file.path("docs", "METHOD_NOTES_EN.md"), subtitle = "Interpretive notes on method choice, assumptions, warnings, and result interpretation."),
      validation = list(title = "Validation", path = file.path("docs", "ANALYSIS_REFERENCE_COMPARISON_PUBLIC.md"), subtitle = "Reference comparisons for public 1.0 calculations and automatic decision paths."),
      version_history = list(title = "Version History", path = "CHANGELOG.md", subtitle = "Release notes and version history.")
    ))
  }

  list(
    overview = list(title = statedu_ko("doc_overview_title"), path = "README_KO.md", subtitle = statedu_ko("doc_overview_subtitle")),
    user_guide = list(title = statedu_ko("doc_user_guide_title"), path = file.path("docs", "USER_GUIDE_KO.md"), subtitle = statedu_ko("doc_user_guide_subtitle")),
    analysis_methods = list(title = statedu_ko("doc_analyses_title"), path = file.path("docs", "ANALYSIS_METHODS_KO.md"), subtitle = statedu_ko("doc_analyses_subtitle")),
    method_notes = list(title = statedu_ko("doc_method_notes_title"), path = file.path("docs", "METHOD_NOTES_KO.md"), subtitle = statedu_ko("doc_method_notes_subtitle")),
    validation = list(title = statedu_ko("doc_validation_title"), path = file.path("docs", "ANALYSIS_REFERENCE_COMPARISON_PUBLIC_KO.md"), subtitle = statedu_ko("doc_validation_subtitle")),
    version_history = list(title = statedu_ko("doc_version_history_title"), path = "CHANGELOG_KO.md", subtitle = statedu_ko("doc_version_history_subtitle"))
  )
}

about_text_document <- function(path) {
  resolved_path <- about_resolve_document_path(path)
  if (!nzchar(resolved_path)) {
    return(div(
      class = "empty-message",
      sprintf("Document not found: %s. This file is generated during the Electron packaging step.", path)
    ))
  }
  lines <- strsplit(about_read_utf8_text(resolved_path), "\r?\n")[[1]]
  div(
    class = "about-markdown-document",
    tags$pre(class = "license-notice-text", paste(lines, collapse = "\n"))
  )
}

about_license_report_document <- function(path = "license_report.csv") {
  resolved_path <- about_resolve_document_path(path)
  if (!nzchar(resolved_path)) {
    return(NULL)
  }
  report <- tryCatch(
    utils::read.csv(resolved_path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )
  if (!is.data.frame(report) || nrow(report) == 0) {
    return(NULL)
  }
  if (!"Scope" %in% names(report) && "Package" %in% names(report)) {
    report$Scope <- ifelse(
      report$Package == "R",
      "R runtime",
      ifelse(report$Package %in% required_packages, "Direct StatEdu Studio package", "Bundled dependency")
    )
  }
  columns <- intersect(c("Scope", "Component", "Version", "License", "URL", "Notes"), names(report))
  if (length(columns) == 0) {
    return(NULL)
  }
  div(
    class = "about-markdown-document",
    h3("License report"),
    tags$table(
      class = "about-license-report-table",
      tags$thead(tags$tr(lapply(columns, tags$th))),
      tags$tbody(lapply(seq_len(nrow(report)), function(row_index) {
        tags$tr(lapply(columns, function(column) tags$td(as.character(report[[column]][[row_index]] %||% ""))))
      }))
    )
  )
}

about_oss_license_document <- function() {
  notices <- about_text_document("THIRD-PARTY-NOTICES.txt")
  report <- about_license_report_document()
  if (is.null(report)) {
    return(notices)
  }
  tagList(notices, report)
}

about_info_row <- function(label, value) {
  div(
    class = "about-info-row",
    div(class = "about-info-label", label),
    div(class = "about-info-value", value)
  )
}

about_citation_field <- function(field, fallback = "") {
  path <- "CITATION.cff"
  if (!file.exists(path)) {
    return(fallback)
  }
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  pattern <- paste0("^", field, ":\\s*\"?([^\"\n]+)\"?\\s*$")
  match <- grep(pattern, lines, value = TRUE)
  if (length(match) == 0) {
    return(fallback)
  }
  sub(pattern, "\\1", match[[1]])
}

about_update_document <- function(language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  div(
    class = "about-application-document",
    div(
      class = "about-update-panel",
      h2(statedu_t("about.updates_title", language)),
      p(statedu_t("about.updates_detail", language)),
      actionButton(
        "check_updates",
        statedu_t("about.check_updates", language),
        class = "btn btn-primary"
      )
    )
  )
}

about_application_document <- function(version, language = "ko") {
  language <- normalize_app_language(language)
  release_date <- about_citation_field("date-released", "2026-05-26")
  doi <- about_citation_field("doi", "")
  repository <- about_citation_field("repository-code", "https://github.com/StatEdu/StatEdu_Studio")
  citation <- sprintf(
    "LEE, I. H. (2026). StatEdu Studio (Version %s) [Computer software].",
    version
  )
  doi_citation <- if (nzchar(doi)) {
    paste(citation, paste0("https://doi.org/", doi))
  } else {
    citation
  }

  div(
    class = "about-application-document",
    h2("StatEdu Studio"),
    p(statedu_t("about.app_description", language)),
    div(
      class = "about-info-grid",
      about_info_row(statedu_t("about.version", language), paste0("v", version)),
      about_info_row(statedu_t("about.release_date", language), release_date),
      about_info_row(statedu_t("about.developer", language), "IL HYUN LEE"),
      about_info_row(statedu_t("about.organization", language), "StatEdu"),
      about_info_row(statedu_t("about.contact", language), tags$a(href = "mailto:dr.leeilhyun@gmail.com", "dr.leeilhyun@gmail.com")),
      about_info_row(statedu_t("about.runtime", language), statedu_t("about.local_windows_shiny", language)),
      about_info_row(statedu_t("about.data_handling", language), statedu_t("about.data_handling_detail", language)),
      about_info_row(statedu_t("about.repository", language), tags$a(href = repository, target = "_blank", rel = "noopener noreferrer", repository)),
      about_info_row(
        "DOI",
        if (nzchar(doi)) {
          tags$a(href = paste0("https://doi.org/", doi), target = "_blank", rel = "noopener noreferrer", doi)
        } else {
          statedu_t("about.pending_registration", language)
        }
      )
    ),
    h3(statedu_t("about.citation", language)),
    p(doi_citation)
  )
}

about_info_tab_panel <- function(version, language = "ko") {
  language <- normalize_app_language(language)
  tabPanel(
    statedu_ui_label("about", language),
    value = "about",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(statedu_ui_label("about", language)),
        div(statedu_t("about.info_subtitle", language), class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel about-workspace-panel",
        style = "min-width:980px;overflow-x:auto;",
        about_application_document(version, language)
      )
    )
  )
}

about_preferences_tab_panel <- function(language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  zoom_percent <- statedu_initial_result_zoom()
  preferences <- statedu_initial_preferences()
  tabPanel(
    statedu_ui_label("preferences", language),
    value = "about_preferences",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(statedu_ui_label("preferences", language)),
        div(statedu_t("preferences.subtitle", language), class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel about-workspace-panel preferences-workspace-panel",
        style = "min-width:980px;overflow-x:auto;",
        div(
          class = "preferences-section preferences-language-control",
          h3(statedu_t("preferences.language_title", language)),
          selectInput(
            "app_language",
            statedu_t("preferences.language_title", language),
            choices = statedu_language_choices(language),
            selected = language,
            width = "320px",
            selectize = FALSE
          ),
          actionButton(
            "apply_app_language",
            statedu_t("preferences.language_apply", language),
            onclick = "return easyflowApplyAppLanguage();",
            class = "btn btn-primary"
          ),
          div(statedu_t("preferences.language_detail", language), class = "step-summary-detail")
        ),
        div(
          class = "preferences-section preferences-output-control",
          h3(statedu_t("preferences.output_title", language)),
          div(
            class = "preferences-output-grid",
            sliderInput(
              "result_zoom_percent",
              statedu_t("preferences.result_zoom", language),
              min = 80,
              max = 200,
              value = zoom_percent,
              step = 5,
              post = "%",
              width = "360px"
            ),
            actionButton(
              "apply_result_zoom",
              statedu_t("preferences.apply_result_zoom", language),
              class = "btn btn-primary"
            )
          ),
          div(
            statedu_t("preferences.result_zoom_detail", language),
            class = "step-summary-detail"
          )
        ),
        div(
          class = "preferences-section preferences-analysis-defaults",
          h3(statedu_t("preferences.defaults_title", language)),
          div(
            class = "preferences-defaults-grid",
            selectInput(
              "output_decimal_digits",
              statedu_t("preferences.decimal_digits", language),
              choices = stats::setNames(as.character(0:5), as.character(0:5)),
              selected = as.character(preferences$output_decimal_digits %||% 3L),
              width = "220px",
              selectize = FALSE
            ),
            selectInput(
              "p_value_format",
              statedu_t("preferences.p_value_format", language),
              choices = stats::setNames(
                c("apa", "leading_zero"),
                c(".123, <.001", "0.123, <0.001")
              ),
              selected = normalize_p_value_format(preferences$p_value_format),
              width = "220px",
              selectize = FALSE
            ),
            selectInput(
              "multiple_correction_default",
              statedu_t("preferences.multiple_correction", language),
              choices = stats::setNames(
                c("holm", "bonferroni"),
                c("Holm-Bonferroni", "Bonferroni")
              ),
              selected = normalize_multiple_correction_default(preferences$multiple_correction_default),
              width = "240px",
              selectize = FALSE
            ),
            checkboxInput(
              "selected_variables_only_default",
              statedu_t("preferences.selected_variables_only", language),
              value = normalize_selected_variables_only_default(preferences$selected_variables_only_default)
            ),
            div(
              class = "preferences-save-dir-control",
              div(class = "control-label", statedu_t("preferences.default_save_dir", language)),
              div(
                class = "preferences-save-dir-row",
                textInput(
                  "default_save_dir",
                  label = NULL,
                  value = normalize_default_save_dir(preferences$default_save_dir),
                  placeholder = statedu_t("preferences.default_save_dir_placeholder", language),
                  width = "100%"
                ),
                actionButton(
                  "browse_default_save_dir",
                  if (identical(language, "ko")) "\ucc3e\uc544\ubcf4\uae30" else "Browse",
                  class = "btn btn-default"
                )
              )
            )
          ),
          actionButton(
            "apply_general_preferences",
            statedu_t("preferences.save_defaults", language),
            class = "btn btn-primary"
          ),
          div(
            statedu_t("preferences.defaults_detail", language),
            class = "step-summary-detail"
          )
        )
      )
    )
  )
}
about_markdown_tab_panel <- function(title, value, path, subtitle = "StatEdu Studio documentation.", language = statedu_initial_language()) {
  options(statedu.app_language = normalize_app_language(language))
  tabPanel(
    title,
    value = value,
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(title),
        div(subtitle, class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel about-workspace-panel",
        style = "min-width:980px;overflow-x:auto;",
        about_markdown_document(path)
      )
    )
  )
}

about_license_tab_panel <- function(language = statedu_initial_language()) {
  tabPanel(
    statedu_ui_label("open_source_licenses", language),
    value = "about_oss_licenses",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(statedu_ui_label("open_source_licenses", language)),
        div(statedu_t("about.oss_license_subtitle", language), class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel about-workspace-panel",
        style = "min-width:980px;overflow-x:auto;",
        about_oss_license_document()
      )
    )
  )
}

about_source_license_tab_panel <- function(language = statedu_initial_language()) {
  tabPanel(
    statedu_ui_label("source_license", language),
    value = "about_source_license",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(statedu_ui_label("source_license", language)),
        div(statedu_t("about.source_license_subtitle", language), class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel about-workspace-panel",
        style = "min-width:980px;overflow-x:auto;",
        h3(statedu_t("about.source_code_offer", language)),
        about_text_document("SOURCE-OFFER.txt"),
        h3(statedu_t("about.application_license", language)),
        about_text_document("LICENSE")
      )
    )
  )
}

about_update_tab_panel <- function(language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  title <- statedu_t("about.check_updates_title", language)
  tabPanel(
    title,
    value = "about_update",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(title),
        div(statedu_t("about.check_updates_subtitle", language), class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel about-workspace-panel",
        style = "min-width:980px;overflow-x:auto;",
        about_update_document(language)
      )
    )
  )
}

help_update_label <- function(language = statedu_initial_language()) {
  statedu_t("about.check_updates_title", normalize_app_language(language))
}

help_request_urls <- function(language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  list(
    bug = "https://studio.statedu.com/help/bug/",
    feature = "https://studio.statedu.com/help/feature/",
    analysis = "https://studio.statedu.com/help/analysis/",
    qa = if (identical(language, "ko")) {
      "https://statedu.com/qna/?qna_action=write&qna_topic=StatEdu%20Studio"
    } else {
      "https://statedu.com/en/qna/?qna_action=write&qna_topic=StatEdu%20Studio"
    }
  )
}

help_request_specs <- function(language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  urls <- help_request_urls(language)
  list(
    bug = list(
      title = statedu_ui_label("bug_report", language),
      subtitle = statedu_t("help.bug_subtitle", language),
      detail = statedu_t("help.bug_detail", language),
      url = urls$bug
    ),
    feature = list(
      title = statedu_ui_label("feature_request", language),
      subtitle = statedu_t("help.feature_subtitle", language),
      detail = statedu_t("help.feature_detail", language),
      url = urls$feature
    ),
    analysis = list(
      title = statedu_ui_label("analysis_request", language),
      subtitle = statedu_t("help.analysis_subtitle", language),
      detail = statedu_t("help.analysis_detail", language),
      url = urls$analysis
    ),
    qa = list(
      title = statedu_ui_label("qna", language),
      subtitle = statedu_t("help.qa_subtitle", language),
      detail = statedu_t("help.qa_detail", language),
      url = urls$qa
    )
  )
}

help_request_tab_panel <- function(kind, value, version, language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  spec <- help_request_specs(language)[[kind]]
  web_form_note <- if (identical(kind, "qa")) {
    statedu_t("help.qa_web_form_note", language)
  } else {
    statedu_t("help.support_web_form_note", language)
  }
  button_label <- if (identical(kind, "qa")) {
    statedu_t("help.open_qa", language)
  } else {
    statedu_t("help.open_support_form", language)
  }
  tabPanel(
    spec$title,
    value = value,
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(spec$title),
        div(statedu_t("help.request_subtitle", language), class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel about-workspace-panel",
        style = "max-width:980px;",
        h3(spec$title),
        p(spec$subtitle),
        p(spec$detail),
        div(
          class = "step-summary-detail",
          web_form_note
        ),
        tags$a(
          href = spec$url,
          target = "_blank",
          rel = "noopener noreferrer",
          class = "btn btn-primary",
          button_label
        )
      )
    )
  )
}

help_tab_panel <- function(version, language = statedu_initial_language()) {
  navbarMenu(
    statedu_ui_label("help", language),
    lazy_tab_panel(statedu_ui_label("bug_report", language), "help_bug", "lazy_help_bug"),
    lazy_tab_panel(statedu_ui_label("feature_request", language), "help_feature", "lazy_help_feature"),
    lazy_tab_panel(statedu_ui_label("analysis_request", language), "help_analysis_request", "lazy_help_analysis_request"),
    lazy_tab_panel(statedu_ui_label("qna", language), "help_qa", "lazy_help_qa"),
    lazy_tab_panel(help_update_label(language), "about_update", "lazy_about_update")
  )
}

about_tab_panel <- function(version, language = statedu_initial_language()) {
  navbarMenu(
    statedu_ui_label("about", language),
    about_preferences_tab_panel(language),
    lazy_tab_panel(statedu_ui_label("overview", language), "about_overview", "lazy_about_overview"),
    lazy_tab_panel(statedu_ui_label("user_guide", language), "about_user_guide", "lazy_about_user_guide"),
    lazy_tab_panel(statedu_ui_label("analyses", language), "about_analysis_methods", "lazy_about_analysis_methods"),
    lazy_tab_panel(statedu_ui_label("method_notes", language), "about_method_notes", "lazy_about_method_notes"),
    lazy_tab_panel(statedu_ui_label("validation", language), "about_validation", "lazy_about_validation"),
    lazy_tab_panel(statedu_ui_label("version_history", language), "about_version_history", "lazy_about_version_history"),
    lazy_tab_panel(statedu_ui_label("source_license", language), "about_source_license", "lazy_about_source_license"),
    lazy_tab_panel(statedu_ui_label("open_source_licenses", language), "about_oss_licenses", "lazy_about_oss_licenses"),
    lazy_tab_panel(statedu_ui_label("about", language), "about", "lazy_about_info")
  )
}
