# Result and About menu UI.

result_tab_panel <- function() {
  tabPanel(
    "Result",
    value = "result",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1("Result"),
        div("Review and manage saved analysis results.", class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel",
        style = "min-width:980px;overflow-x:auto;",
        h3("Result"),
        div(
          class = "result-toolbar",
          analysis_save_button("save_result_collection_html_dialog", "Save HTML", "html", class = "btn-default"),
          analysis_save_button("save_result_collection_pdf_dialog", "Save PDF", "pdf", class = "btn-default"),
          analysis_save_button("save_result_collection_excel_dialog", "Save Excel", "excel", class = "btn-default"),
          analysis_save_button("save_result_collection_word_dialog", "Save Word", "word", class = "btn-default"),
          actionButton("clear_saved_results", "Clear results", class = "btn-default")
        ),
        uiOutput("saved_results_list")
      )
    )
  )
}

about_markdown_document <- function(path) {
  if (!file.exists(path)) {
    return(div(class = "empty-message", sprintf("Document not found: %s", path)))
  }
  div(
    class = "about-markdown-document",
    shiny::includeMarkdown(path)
  )
}

about_text_document <- function(path) {
  if (!file.exists(path)) {
    return(div(
      class = "empty-message",
      sprintf("Document not found: %s. This file is generated during the Electron packaging step.", path)
    ))
  }
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  div(
    class = "about-markdown-document",
    tags$pre(class = "license-notice-text", paste(lines, collapse = "\n"))
  )
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

about_application_document <- function(version) {
  release_date <- about_citation_field("date-released", "2026-05-26")
  doi <- about_citation_field("doi", "10.22934/statedu.easyflow.statistics")
  repository <- about_citation_field("repository-code", "https://github.com/StatEdu/easyflow_statistics_dev")

  div(
    class = "about-application-document",
    h2("EasyFlow Statistics"),
    p("A local Shiny application for assumption-guided statistical analysis and publication-ready result tables."),
    div(
      class = "about-info-grid",
      about_info_row("Version", paste0("v", version)),
      about_info_row("Release date", release_date),
      about_info_row("Developer", "IL HYUN LEE"),
      about_info_row("Organization", "StatEdu"),
      about_info_row("Contact", tags$a(href = "mailto:dr.leeilhyun@gmail.com", "dr.leeilhyun@gmail.com")),
      about_info_row("Runtime", "Local Windows Shiny app"),
      about_info_row("Data handling", "Data are analyzed locally on the user's PC and are not sent to an external server."),
      about_info_row("Repository", tags$a(href = repository, target = "_blank", rel = "noopener noreferrer", repository)),
      about_info_row("DOI", tags$a(href = paste0("https://doi.org/", doi), target = "_blank", rel = "noopener noreferrer", doi))
    ),
    h3("Citation"),
    p(sprintf(
      "LEE, I. H. (2026). EasyFlow Statistics (Version %s) [Computer software]. https://doi.org/%s",
      version,
      doi
    ))
  )
}

about_info_tab_panel <- function(version) {
  tabPanel(
    "About",
    value = "about",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1("About"),
        div("Version, developer information, and documentation.", class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel about-workspace-panel",
        style = "min-width:980px;overflow-x:auto;",
        about_application_document(version)
      )
    )
  )
}

about_markdown_tab_panel <- function(title, value, path, subtitle = "EasyFlow Statistics documentation.") {
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

about_license_tab_panel <- function() {
  tabPanel(
    "Open Source Licenses",
    value = "about_oss_licenses",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1("Open Source Licenses"),
        div("Third-party license notices for bundled runtime components.", class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel about-workspace-panel",
        style = "min-width:980px;overflow-x:auto;",
        about_text_document("THIRD-PARTY-NOTICES.txt")
      )
    )
  )
}

about_source_license_tab_panel <- function() {
  tabPanel(
    "Source & License",
    value = "about_source_license",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1("Source & License"),
        div("EasyFlow Statistics source availability and application license.", class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel about-workspace-panel",
        style = "min-width:980px;overflow-x:auto;",
        h3("Source Code Offer"),
        about_text_document("SOURCE-OFFER.txt"),
        h3("Application License"),
        about_text_document("LICENSE")
      )
    )
  )
}

about_tab_panel <- function(version) {
  navbarMenu(
    "About",
    lazy_tab_panel("Overview", "about_overview", "lazy_about_overview"),
    lazy_tab_panel("User Guide", "about_user_guide", "lazy_about_user_guide"),
    lazy_tab_panel("Analysis Methods", "about_analysis_methods", "lazy_about_analysis_methods"),
    lazy_tab_panel("Method Notes", "about_method_notes", "lazy_about_method_notes"),
    lazy_tab_panel("Source & License", "about_source_license", "lazy_about_source_license"),
    lazy_tab_panel("Open Source Licenses", "about_oss_licenses", "lazy_about_oss_licenses"),
    lazy_tab_panel("About", "about", "lazy_about_info")
  )
}
