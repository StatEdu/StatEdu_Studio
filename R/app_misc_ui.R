# Result and About menu UI.

result_tab_panel <- function(language = statedu_initial_language()) {
  h <- statedu_utf8
  tabPanel(
    statedu_ui_label("result", language),
    value = "result",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(statedu_ui_label("result", language)),
        div(statedu_text(language, "Review and manage saved analysis results.", h("eca080ec9ea5eb909c20ebb684ec849d20eab2b0eab3bceba5bc20ed9995ec9db8ed9598eab3a020eab480eba6aced95a9eb8b88eb8ba42e")), class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel",
        style = "min-width:980px;overflow-x:auto;",
        h3(statedu_ui_label("result", language)),
        div(
          class = "result-toolbar",
          div(
            class = "result-toolbar-group result-toolbar-primary",
            actionButton("open_result_history_dialog", statedu_text(language, "Open result", h("eab2b0eab3bc20ec97b4eab8b0")), class = "btn-default"),
            analysis_save_button("save_result_history_dialog", statedu_text(language, "Save result", h("eab2b0eab3bc20eca080ec9ea5")), "result_history", class = "btn-default"),
            actionButton("clear_saved_results", statedu_text(language, "Clear results", h("eab2b0eab3bc20ebb984ec9ab0eab8b0")), class = "btn-default")
          ),
          div(
            class = "result-toolbar-group result-toolbar-export",
            analysis_save_button("save_result_collection_html_dialog", statedu_ui_label("save_html", language), "html", class = "btn-default"),
            analysis_save_button("save_result_collection_pdf_dialog", statedu_ui_label("save_pdf", language), "pdf", class = "btn-default"),
            analysis_save_button("save_result_collection_excel_dialog", statedu_ui_label("save_excel", language), "excel", class = "btn-default"),
            analysis_save_button("save_result_collection_word_dialog", statedu_ui_label("save_word", language), "word", class = "btn-default")
          )
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
    Sys.getenv("EASYFLOW_APP_DIR", ""),
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
  if (identical(normalize_app_language(language), "en")) "English" else "Korean"
}

about_document_specs <- function(language = "ko") {
  language <- normalize_app_language(language)
  if (identical(language, "en")) {
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
    overview = list(title = statedu_utf8("eab09cec9a94"), path = "README_KO.md", subtitle = statedu_utf8("ed9484eba19ceca09ded8ab820ebb294ec9c842c20ed9884ec9eac20ebb284eca0842c20eab280eca69d2c20ec9db8ec9aa920eca095ebb3b4eba5bc20eca09ceab3b5ed95a9eb8b88eb8ba42e")),
    user_guide = list(title = statedu_utf8("ec82acec9aa9ec9e9020eab080ec9db4eb939c"), path = file.path("docs", "USER_GUIDE_KO.md"), subtitle = statedu_utf8("eb8db0ec9db4ed84b020ebb688eb9facec98a4eab8b02c20ebb380ec889820ec84a0ed839d2c20ebb684ec849d20ec8ba4ed96892c20eab2b0eab3bc20eca080ec9ea520eca088ecb0a8eba5bc20ec9588eb82b4ed95a9eb8b88eb8ba42e")),
    analysis_methods = list(title = statedu_utf8("ebb684ec849d"), path = file.path("docs", "ANALYSIS_METHODS_KO.md"), subtitle = statedu_utf8("537461744564752053747564696f20312e30ec9d9820ebb684ec849d20eba994eb89b42c20ed86b5eab38420ecb69ceba0a52c20ed919c2c20eb82b4ebb3b4eb82b4eab8b020ebb294ec9c84eba5bc20eca095eba6aced95a9eb8b88eb8ba42e")),
    method_notes = list(title = statedu_utf8("ebb0a9ebb295eba1a020eb85b8ed8ab8"), path = file.path("docs", "METHOD_NOTES_KO.md"), subtitle = statedu_utf8("ebb684ec849d20ebb0a9ebb29520ec84a0ed839d2c20eab080eca0952c20eab2bdeab3a02c20eab2b0eab3bc20ed95b4ec849dec979020eb8c80ed959c20eb85b8ed8ab8eba5bc20eca09ceab3b5ed95a9eb8b88eb8ba42e")),
    validation = list(title = statedu_utf8("eab280eca69d"), path = file.path("docs", "ANALYSIS_REFERENCE_COMPARISON_PUBLIC_KO.md"), subtitle = statedu_utf8("eab3b5eab09c20312e3020eab384ec82b0eab3bc20ec9e90eb8f9920ed8c90eb8ba820eab2bdeba19cec9d9820eab8b0eca48020ebb984eab590eba5bc20eca09ceab3b5ed95a9eb8b88eb8ba42e")),
    version_history = list(title = statedu_utf8("ebb284eca08420eab8b0eba19d"), path = "CHANGELOG_KO.md", subtitle = statedu_utf8("eba6b4eba6acec8aa420eb85b8ed8ab8ec998020ebb284eca08420eab8b0eba19dec9d8420eca09ceab3b5ed95a9eb8b88eb8ba42e"))
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
      h2(statedu_text(language, "Updates", statedu_utf8("ec9785eb8db0ec9db4ed8ab8"))),
      p(statedu_text(
        language,
        "Check the public update manifest when you need to confirm the latest release.",
        statedu_utf8("ebb284ed8abcec9d8420eb8884eba5bc20eb958ceba78c20eab3b5eab09c20ec9785eb8db0ec9db4ed8ab820eca095ebb3b4eba5bc20ed9995ec9db8ed95a9eb8b88eb8ba42e")
      )),
      actionButton(
        "check_updates",
        statedu_text(language, "Check for updates", statedu_utf8("ec9785eb8db0ec9db4ed8ab820ed9995ec9db8")),
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
    p(statedu_text(language,
      "A local Shiny application for assumption-guided statistical analysis and publication-ready result tables.",
      statedu_utf8("eab080eca09520eab280ed86a0eba5bc20eab8b0ebb098ec9cbceba19c20ed86b5eab384ebb684ec849dec9d8420ec8898ed9689ed9598eab3a020eb85bcebacb82febb3b4eab3a0ec849cec9aa920eab2b0eab3bced919ceba5bc20ec839dec84b1ed9598eb8a9420eba19cecbbac205368696e7920ec95a0ed948ceba6acecbc80ec9db4ec8598ec9e85eb8b88eb8ba42e")
    )),
    div(
      class = "about-info-grid",
      about_info_row(statedu_text(language, "Version", statedu_utf8("ebb284eca084")), paste0("v", version)),
      about_info_row(statedu_text(language, "Release date", statedu_utf8("eba6b4eba6acec8aa420eb82a0eca79c")), release_date),
      about_info_row(statedu_text(language, "Developer", statedu_utf8("eab09cebb09cec9e90")), "IL HYUN LEE"),
      about_info_row(statedu_text(language, "Organization", statedu_utf8("eab8b0eab480")), "StatEdu"),
      about_info_row(statedu_text(language, "Contact", statedu_utf8("ec97b0eb9dbdecb298")), tags$a(href = "mailto:dr.leeilhyun@gmail.com", "dr.leeilhyun@gmail.com")),
      about_info_row(statedu_text(language, "Runtime", statedu_utf8("ec8ba4ed968920ed9998eab2bd")), statedu_text(language, "Local Windows Shiny app", statedu_utf8("eba19cecbbac2057696e646f7773205368696e7920ec95b1"))),
      about_info_row(statedu_text(language, "Data handling", statedu_utf8("eb8db0ec9db4ed84b020ecb298eba6ac")), statedu_text(language, "Data are analyzed locally on the user's PC and are not sent to an external server.", statedu_utf8("eb8db0ec9db4ed84b0eb8a9420ec82acec9aa9ec9e90205043ec9790ec849c20eba19cecbbaceba19c20ebb684ec849deb9098eba9b020ec99b8ebb68020ec849cebb284eba19c20eca084ec86a1eb9098eca78020ec958aec8ab5eb8b88eb8ba42e"))),
      about_info_row(statedu_text(language, "Repository", statedu_utf8("eca080ec9ea5ec868c")), tags$a(href = repository, target = "_blank", rel = "noopener noreferrer", repository)),
      about_info_row(
        "DOI",
        if (nzchar(doi)) {
          tags$a(href = paste0("https://doi.org/", doi), target = "_blank", rel = "noopener noreferrer", doi)
        } else {
          statedu_text(language, "Pending registration", statedu_utf8("eb93b1eba19d20eb8c80eab8b020eca491"))
        }
      )
    ),
    h3(statedu_text(language, "Citation", statedu_utf8("ec9db8ec9aa9"))),
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
        div(statedu_text(language, "Version, developer information, and documentation.", statedu_utf8("ebb284eca0842c20eab09cebb09cec9e9020eca095ebb3b42c20ebacb8ec849c20eca095ebb3b4eba5bc20eca09ceab3b5ed95a9eb8b88eb8ba42e")), class = "app-subtitle")
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
  tabPanel(
    statedu_ui_label("preferences", language),
    value = "about_preferences",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(statedu_ui_label("preferences", language)),
        div(statedu_text(language, "Application-wide settings.", statedu_utf8("ec95b120eca084ecb2b4ec979020eca081ec9aa9eb9098eb8a9420ec84a4eca095ec9e85eb8b88eb8ba42e")), class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel about-workspace-panel preferences-workspace-panel",
        style = "min-width:980px;overflow-x:auto;",
        div(
          class = "session-language-control preferences-language-control",
          selectInput(
            "app_language",
            statedu_text(language, "Language", statedu_utf8("ec96b8ec96b4")),
            choices = stats::setNames(c("ko", "en"), c(statedu_utf8("ed959ceab5adec96b4"), "English")),
            selected = language,
            width = "320px",
            selectize = FALSE
          ),
          actionButton(
            "apply_app_language",
            statedu_text(language, "Apply language", statedu_utf8("ec96b8ec96b420eca081ec9aa9")),
            onclick = "return easyflowApplyAppLanguage();",
            class = "btn btn-primary"
          ),
          div(statedu_text(language, "Applies to in-app UI and documentation. Result tables remain in English.", statedu_utf8("ec95b1205549ec998020ebacb8ec849cec979020eca081ec9aa9eb90a9eb8b88eb8ba42e20eab2b0eab3bced919ceb8a9420ec9881ec96b4eba19c20ec9ca0eca780eb90a9eb8b88eb8ba42e")), class = "step-summary-detail")
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
        div(statedu_text(language, "Third-party license notices for bundled runtime components.", statedu_utf8("ed95a8eabb9820ebb0b0ed8faceb9098eb8a9420eb9fb0ed8380ec9e8420eab5acec84b1ec9a94ec868cec9d9820eca09c33ec9e9020eb9dbcec9db4ec84a0ec8aa420eab3a0eca780ec9e85eb8b88eb8ba42e")), class = "app-subtitle")
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
        div(statedu_text(language, "StatEdu Studio source availability and application license.", statedu_utf8("537461744564752053747564696f20ec868cec8aa420eca09ceab3b520ebb294ec9c84ec998020ec95a0ed948ceba6acecbc80ec9db4ec859820eb9dbcec9db4ec84a0ec8aa4ec9e85eb8b88eb8ba42e")), class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel about-workspace-panel",
        style = "min-width:980px;overflow-x:auto;",
        h3(statedu_text(language, "Source Code Offer", statedu_utf8("ec868cec8aa420ecbd94eb939c20eca09ceab3b5"))),
        about_text_document("SOURCE-OFFER.txt"),
        h3(statedu_text(language, "Application License", statedu_utf8("ec95a0ed948ceba6acecbc80ec9db4ec859820eb9dbcec9db4ec84a0ec8aa4"))),
        about_text_document("LICENSE")
      )
    )
  )
}

about_update_tab_panel <- function(language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  title <- statedu_text(language, "Check for Updates", statedu_utf8("ec9785eb8db0ec9db4ed8ab820ed9995ec9db8"))
  tabPanel(
    title,
    value = "about_update",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(title),
        div(statedu_text(language, "Confirm whether a newer public release is available.", statedu_utf8("ec8388eba19cec9ab420eab3b5eab09c20ebb0b0ed8faceab08020ec9e88eb8a94eca78020ed9995ec9db8ed95a9eb8b88eb8ba42e")), class = "app-subtitle")
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
  statedu_text(
    normalize_app_language(language),
    "Check for Updates",
    statedu_utf8("ec9785eb8db0ec9db4ed8ab820ed9995ec9db8")
  )
}

help_request_specs <- function(language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  list(
    bug = list(
      title = statedu_ui_label("bug_report", language),
      subtitle = statedu_text(language, "Report a problem found while using StatEdu Studio.", statedu_utf8("537461744564752053747564696f20ec82acec9aa920eca49120ebb09ceab2aced959c20ebacb8eca09ceba5bc20ec958ceba0a4eca3bcec84b8ec9a942e")),
      detail = statedu_text(language, "Include reproduction steps, expected result, actual result, error messages, and screenshots when possible.", statedu_utf8("ec9eaced988420eb8ba8eab3842c20ec9888ec838120eab2b0eab3bc2c20ec8ba4eca09c20eab2b0eab3bc2c20ec98a4eba59820eba994ec8b9ceca7802c20eab080eb8aa5ed9598eba9b420ed9994eba9b420ecbaa1ecb298eba5bc20ed95a8eabb9820ebb3b4eb82b4eca3bcec84b8ec9a942e")),
      subject = "StatEdu Studio bug report"
    ),
    feature = list(
      title = statedu_ui_label("feature_request", language),
      subtitle = statedu_text(language, "Share an idea that would make the workflow easier.", statedu_utf8("ec82acec9aa920ed9d90eba684ec9d8420eb8d9420ed8eb8ed9598eab28c20eba78ceb93a420ec9584ec9db4eb9494ec96b4eba5bc20ec958ceba0a4eca3bcec84b8ec9a942e")),
      detail = statedu_text(language, "Describe the feature, why it is needed, and the expected behavior.", statedu_utf8("ec9b90ed9598eb8a9420eab8b0eb8aa52c20ed9584ec9a94ed959c20ec9db4ec9ca02c20eab8b0eb8c8020eb8f99ec9e91ec9d8420eca081ec96b4eca3bcec84b8ec9a942e")),
      subject = "StatEdu Studio feature request"
    ),
    analysis = list(
      title = statedu_ui_label("analysis_request", language),
      subtitle = statedu_text(language, "Request a new analysis method or an extension of an existing analysis.", statedu_utf8("ec838820ebb684ec849d20eab8b0ebb29520eb9890eb8a9420eab8b0eca1b420ebb684ec849d20ed9995ec9ea5ec9d8420ec9a94ecb2aded95a9eb8b88eb8ba42e")),
      detail = statedu_text(language, "Include the study design, variable types, desired result table, references, or examples.", statedu_utf8("ec97b0eab5ac20ec84a4eab3842c20ebb380ec889820ec9ca0ed98952c20ec9b90ed9598eb8a9420eab2b0eab3bced919c2c20ecb0b8eab3a020ebacb8ed978cec9db4eb829820ec9888ec8b9ceba5bc20ed95a8eabb9820ebb3b4eb82b4eca3bcec84b8ec9a942e")),
      subject = "StatEdu Studio analysis request"
    ),
    qa = list(
      title = statedu_ui_label("qna", language),
      subtitle = statedu_text(language, "Ask a question about usage, interpretation, installation, or release.", statedu_utf8("ec82acec9aa920ebb0a9ebb2952c20eab2b0eab3bc20ed95b4ec849d2c20ec84a4ecb99820eb9890eb8a9420ebb0b0ed8fac20eab480eba0a820eca788ebacb8ec9d8420ebb3b4eb8385eb8b88eb8ba42e")),
      detail = statedu_text(language, "Include the question and the current screen or workflow step.", statedu_utf8("eca788ebacb820eb82b4ec9aa9eab3bc20ed9884ec9eac20ed9994eba9b420eb9890eb8a9420ec9e91ec978520eb8ba8eab384eba5bc20ed95a8eabb9820eca081ec96b4eca3bcec84b8ec9a942e")),
      subject = "StatEdu Studio Q&A"
    )
  )
}

help_request_tab_panel <- function(kind, value, version, language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  spec <- help_request_specs(language)[[kind]]
  support_email <- "dr.leeilhyun@gmail.com"
  mail_body <- paste(
    "StatEdu Studio",
    paste0("Version: ", version),
    "",
    "Please describe the request here:",
    sep = "\n"
  )
  mailto <- paste0(
    "mailto:", support_email,
    "?subject=", utils::URLencode(spec$subject, reserved = TRUE),
    "&body=", utils::URLencode(mail_body, reserved = TRUE)
  )
  tabPanel(
    spec$title,
    value = value,
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(spec$title),
        div(statedu_text(language, "Choose a support request type and send the details.", statedu_utf8("ebacb8ec9d98eba5bc20ebb3b4eb82bc20ed95adebaaa9ec9d8420ec84a0ed839ded9598ec84b8ec9a942e")), class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel about-workspace-panel",
        style = "max-width:980px;",
        h3(spec$title),
        p(spec$subtitle),
        p(spec$detail),
        div(
          class = "step-summary-detail",
          statedu_text(language, "Support requests open in your default mail app. Attach data files only when needed and after checking personal information.", statedu_utf8("eca780ec9b9020ec9a94ecb2adec9d8020eab8b0ebb3b820eba994ec9dbc20ec95b1ec9cbceba19c20ec9e91ec84b1eb90a9eb8b88eb8ba42e20eb8db0ec9db4ed84b020ed8c8cec9dbcec9d8020eab09cec9db8eca095ebb3b4eba5bc20ed9995ec9db8ed959c20eb92a420ed9584ec9a94ed959c20eab2bdec9ab0ec9790eba78c20ecb2a8ebb680ed9598ec84b8ec9a942e"))
        ),
        tags$a(
          href = mailto,
          class = "btn btn-primary",
          statedu_text(language, "Open mail app", statedu_utf8("eba994ec9dbc20ec95b120ec97b4eab8b0"))
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
