# Saved analysis HTML output.

saved_results_image_data_uri <- function(path, mime = "image/png") {
  if (!file.exists(path)) {
    return("")
  }
  raw <- readBin(path, what = "raw", n = file.info(path)$size)
  paste0("data:", mime, ";base64,", jsonlite::base64_enc(raw))
}

saved_results_cover_text <- function() {
  edition <- if (exists("analysis_save_edition", mode = "function")) {
    analysis_save_edition()
  } else {
    tolower(Sys.getenv("EASYFLOW_EDITION", "development"))
  }
  if (!edition %in% c("free", "development", "personal", "institution")) {
    edition <- "development"
  }
  organization <- trimws(Sys.getenv("EASYFLOW_REPORT_ORGANIZATION", ""))
  user <- trimws(Sys.getenv("EASYFLOW_REPORT_USER", ""))
  organization_logo <- trimws(Sys.getenv("EASYFLOW_REPORT_ORGANIZATION_LOGO", ""))

  if (identical(edition, "development")) {
    organization <- if (nzchar(organization)) organization else "statedu.com"
    user <- if (nzchar(user)) user else "StatEdu 통계연구소"
    organization_logo <- if (nzchar(organization_logo)) organization_logo else file.path("www", "statedu_logo.png")
  } else if (identical(edition, "personal")) {
    organization <- ""
    organization_logo <- ""
  } else if (identical(edition, "institution")) {
    organization <- if (nzchar(organization)) organization else "Institution"
  }

  list(
    edition = edition,
    organization = organization,
    user = user,
    organization_logo = organization_logo,
    footer = trimws(Sys.getenv("EASYFLOW_REPORT_FOOTER", "Prepared with EasyFlow Statistics"))
  )
}

saved_results_app_version <- function(version_file = "VERSION") {
  version <- trimws(Sys.getenv("EASYFLOW_VERSION", ""))
  if (nzchar(version)) {
    return(version)
  }
  if (file.exists(version_file)) {
    version <- trimws(readLines(version_file, warn = FALSE)[1])
    if (nzchar(version)) {
      return(version)
    }
  }
  ""
}

saved_results_development_watermark <- function(logo_uri, organization_logo_uri, organization_name) {
  stat_edu_name <- "StatEdu, Institute of Statistics"
  stat_edu_site <- if (nzchar(organization_name)) organization_name else "statedu.com"
  watermark_content <- div(
    class = "report-watermark-inner",
    div(
      class = "report-watermark-brand-row",
      div(
        class = "report-watermark-item report-watermark-item-efs",
        if (nzchar(logo_uri)) {
          tags$img(src = logo_uri, class = "report-watermark-logo report-watermark-logo-efs", alt = "EasyFlow Statistics logo")
        } else {
          span("EasyFlow Statistics", class = "report-watermark-name")
        }
      ),
      div(class = "report-watermark-divider"),
      div(
        class = "report-watermark-item report-watermark-item-statedu",
        if (nzchar(organization_logo_uri)) {
          tags$img(src = organization_logo_uri, class = "report-watermark-logo report-watermark-logo-statedu", alt = stat_edu_name)
        } else {
          div(
            span(stat_edu_name, class = "report-watermark-subname"),
            span(stat_edu_site, class = "report-watermark-site")
          )
        }
      ),
      div("DEVELOPMENT", class = "report-watermark-edition")
    ),
    div(
      "BETA VERSION - This software is under active development and may contain errors or bugs.",
      class = "report-watermark-beta"
    )
  )
  tagList(
    div(class = "report-watermark report-watermark-upper", watermark_content),
    div(class = "report-watermark report-watermark-lower", watermark_content)
  )
}

saved_results_inline_css <- function(max_width = 1280, print_landscape = FALSE) {
  paste(
    "body { background: #ffffff !important; color: #2f3a46; font-family: Arial, Helvetica, sans-serif; font-size: 16px; margin: 0; }",
    sprintf(".page-shell { max-width: %dpx; margin: 24px auto; padding: 0 18px; }", max_width),
    ".report-cover { min-height: 720px; display: flex; flex-direction: column; justify-content: space-between; border: 1px solid #d9e2ec; border-radius: 8px; margin-bottom: 28px; padding: 42px 48px 38px; background: #fbfdff; box-shadow: 0 12px 28px rgba(16, 42, 67, 0.08); position: relative; overflow: hidden; }",
    ".report-cover::before { content: ''; position: absolute; left: 0; top: 0; width: 10px; height: 100%; background: #0f766e; }",
    ".report-cover-brand { display: flex; align-items: flex-start; justify-content: space-between; gap: 24px; position: relative; z-index: 1; }",
    ".report-cover-logo { display: block; width: 280px; max-width: 46%; height: auto; }",
    ".report-cover-kicker { color: #0f766e; font-size: 12px; font-weight: 700; letter-spacing: .12em; text-transform: uppercase; }",
    ".report-cover-edition { color: #334e68; border: 1px solid #bcccdc; border-radius: 999px; padding: 7px 12px; font-size: 12px; font-weight: 700; letter-spacing: .08em; text-transform: uppercase; background: #ffffff; white-space: nowrap; }",
    ".report-cover-main { position: relative; z-index: 1; max-width: 760px; padding: 42px 0 34px; }",
    ".report-cover-title { color: #102a43; font-size: 46px; font-weight: 700; line-height: 1.08; margin: 14px 0 16px; letter-spacing: 0; }",
    ".report-cover-subtitle { color: #486581; font-size: 18px; line-height: 1.55; margin: 0; max-width: 620px; }",
    ".report-cover-divider { width: 96px; height: 3px; background: #0f766e; margin-top: 34px; }",
    ".report-cover-license { align-items: center; display: flex; gap: 16px; margin-bottom: 18px; position: relative; z-index: 1; }",
    ".report-cover-license-logo { display: block; max-height: 42px; max-width: 160px; object-fit: contain; }",
    ".report-cover-license-label { color: #627d98; display: block; font-size: 11px; font-weight: 700; letter-spacing: .08em; text-transform: uppercase; margin-bottom: 3px; }",
    ".report-cover-license-value { color: #102a43; display: block; font-size: 16px; font-weight: 700; overflow-wrap: anywhere; }",
    ".report-watermark { left: 50%; pointer-events: none; position: fixed; transform: translate(-50%, -50%) rotate(-24deg); z-index: 9999; }",
    ".report-watermark-upper { top: 34%; }",
    ".report-watermark-lower { top: 68%; }",
    ".report-watermark-inner { color: #102a43; opacity: .12; text-align: center; }",
    ".report-watermark-brand-row { align-items: center; display: flex; gap: 26px; justify-content: center; min-width: 830px; }",
    ".report-watermark-item { align-items: center; display: flex; flex-direction: column; gap: 5px; justify-content: center; }",
    ".report-watermark-logo { display: block; object-fit: contain; }",
    ".report-watermark-logo-efs { max-height: 86px; max-width: 340px; }",
    ".report-watermark-logo-statedu { max-height: 74px; max-width: 270px; }",
    ".report-watermark-name { color: #102a43; display: block; font-size: 26px; font-weight: 800; letter-spacing: .04em; line-height: 1.1; white-space: nowrap; }",
    ".report-watermark-subname { color: #102a43; display: block; font-size: 17px; font-weight: 800; letter-spacing: .02em; line-height: 1.1; white-space: nowrap; }",
    ".report-watermark-site { color: #486581; display: block; font-size: 13px; font-weight: 700; letter-spacing: .08em; line-height: 1.1; white-space: nowrap; }",
    ".report-watermark-divider { background: #0fa3a3; height: 92px; width: 3px; }",
    ".report-watermark-edition { border: 2px solid #102a43; border-radius: 999px; color: #102a43; font-size: 18px; font-weight: 800; letter-spacing: .18em; padding: 8px 16px; text-transform: uppercase; white-space: nowrap; }",
    ".report-watermark-beta { color: #102a43; font-size: 24px; font-weight: 900; letter-spacing: .04em; line-height: 1.25; margin-top: 18px; text-transform: uppercase; white-space: nowrap; }",
    ".report-cover-meta { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 14px 28px; color: #334e68; font-size: 14px; line-height: 1.45; border-top: 2px solid #102a43; padding-top: 18px; position: relative; z-index: 1; }",
    ".report-cover-meta-item { min-width: 0; }",
    ".report-cover-meta-label { color: #627d98; display: block; font-size: 11px; font-weight: 700; letter-spacing: .08em; text-transform: uppercase; margin-bottom: 4px; }",
    ".report-cover-meta-value { color: #102a43; font-weight: 600; overflow-wrap: anywhere; }",
    ".report-cover-footer { color: #627d98; font-size: 12px; margin-top: 20px; position: relative; z-index: 1; }",
    ".report-body { margin-top: 0; }",
    ".saved-results-meta { color: #52606d; margin: 4px 0 18px; font-size: 13px; }",
    ".regression-results { border-top: 0 !important; padding-top: 0 !important; }",
    ".regression-result-panel { background: #ffffff; border: 1px solid #d9e2ec; border-radius: 6px; padding: 18px 20px; margin-bottom: 22px; break-inside: avoid; page-break-inside: avoid; }",
    ".result-section.regression-result-panel { width: max-content; max-width: 100%; overflow-x: auto; box-sizing: border-box; }",
    ".result-section.regression-result-panel:not(.landscape-table-panel):not(.paired-result-section):not(.logistic-diagnostics-panel) { width: min(100%, 590px); }",
    ".result-section.regression-result-panel:not(.landscape-table-panel):not(.paired-result-section):not(.logistic-diagnostics-panel) > .result-table-with-note, .result-section.regression-result-panel:not(.landscape-table-panel):not(.paired-result-section):not(.logistic-diagnostics-panel) > .frequency-table-wrap, .result-section.regression-result-panel:not(.landscape-table-panel):not(.paired-result-section):not(.logistic-diagnostics-panel) > table, .result-section.regression-result-panel:not(.landscape-table-panel):not(.paired-result-section):not(.logistic-diagnostics-panel) .result-table-with-note > table { width: 100% !important; max-width: 100%; }",
    ".result-section.regression-result-panel:not(.landscape-table-panel):not(.paired-result-section):not(.logistic-diagnostics-panel) table { width: 100% !important; min-width: 0 !important; max-width: 100% !important; table-layout: fixed; }",
    ".result-section.regression-result-panel.landscape-table-panel { width: min(100%, 890px); }",
    ".result-section.regression-result-panel.landscape-table-panel > .result-table-with-note, .result-section.regression-result-panel.landscape-table-panel > .hierarchical-table-wrap, .result-section.regression-result-panel.landscape-table-panel > .hierarchical-table-scroll, .result-section.regression-result-panel.landscape-table-panel > table, .result-section.regression-result-panel.landscape-table-panel .result-table-with-note > table { width: 100% !important; max-width: 100%; }",
    ".result-section.regression-result-panel.landscape-table-panel table { width: 100% !important; min-width: 0 !important; max-width: 100% !important; table-layout: fixed; }",
    ".result-section.regression-result-panel.ttest-anova-overview-panel, .result-section.regression-result-panel.ttest-anova-assumption-review-panel { width: 100% !important; max-width: 100% !important; overflow-x: hidden !important; }",
    ".result-section.regression-result-panel.ttest-anova-overview-panel > .result-table-with-note, .result-section.regression-result-panel.ttest-anova-assumption-review-panel > .result-table-with-note, .result-section.regression-result-panel.ttest-anova-overview-panel .result-table-with-note > table, .result-section.regression-result-panel.ttest-anova-assumption-review-panel .result-table-with-note > table { width: 100% !important; max-width: 100% !important; min-width: 0 !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df .coefficient-col-statistic { width: 112px !important; min-width: 112px !important; text-align: right !important; white-space: nowrap !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df:not(.coefficient-table-mean-sd) .coefficient-col-stat { width: 44px !important; min-width: 44px !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df:not(.coefficient-table-mean-sd) .coefficient-col-statistic { width: 128px !important; min-width: 128px !important; }",
    ".regression-results > .regression-result-panel:not(.landscape-table-panel):not(.model-overview-panel):not(.assumption-review-panel):not(.reference-summary-panel):not(.diagnostic-plots-section):not(.frequency-plots-section):not(.correlation-plot-section):not(.logistic-diagnostics-panel) { width: min(100%, 590px); max-width: 100%; overflow-x: hidden; box-sizing: border-box; }",
    ".regression-results > .regression-result-panel:not(.landscape-table-panel):not(.model-overview-panel):not(.assumption-review-panel):not(.reference-summary-panel):not(.diagnostic-plots-section):not(.frequency-plots-section):not(.correlation-plot-section):not(.logistic-diagnostics-panel) table { width: 100% !important; min-width: 0 !important; max-width: 100% !important; table-layout: fixed; }",
    ".regression-results > .regression-result-panel:not(.landscape-table-panel):not(.model-overview-panel):not(.assumption-review-panel):not(.reference-summary-panel):not(.diagnostic-plots-section):not(.frequency-plots-section):not(.correlation-plot-section):not(.logistic-diagnostics-panel) .coefficient-table .coefficient-col-term, .regression-results > .regression-result-panel:not(.landscape-table-panel):not(.model-overview-panel):not(.assumption-review-panel):not(.reference-summary-panel):not(.diagnostic-plots-section):not(.frequency-plots-section):not(.correlation-plot-section):not(.logistic-diagnostics-panel) .coefficient-table th:first-child, .regression-results > .regression-result-panel:not(.landscape-table-panel):not(.model-overview-panel):not(.assumption-review-panel):not(.reference-summary-panel):not(.diagnostic-plots-section):not(.frequency-plots-section):not(.correlation-plot-section):not(.logistic-diagnostics-panel) .coefficient-table td:first-child { width: 210px; min-width: 210px; white-space: normal; overflow-wrap: normal; word-break: keep-all; }",
    ".regression-results > .regression-result-panel.landscape-table-panel { width: min(100%, 890px); max-width: 100%; overflow-x: hidden; box-sizing: border-box; }",
    ".regression-results > .regression-result-panel.landscape-table-panel table { width: 100% !important; min-width: 0 !important; max-width: 100% !important; table-layout: fixed; }",
    ".regression-result-panel h3 { color: #15233a; font-size: 15px; font-weight: 700; margin: 0 0 8px; }",
    ".regression-result-panel table { width: auto; min-width: 440px; border-collapse: collapse !important; border-spacing: 0 !important; border-top: 2px solid #1f2937 !important; border-bottom: 2px solid #1f2937 !important; color: #2f3a46; font-size: 12px; background: transparent; }",
    ".regression-result-panel table th, .regression-result-panel table td { padding: 5px 7px; line-height: 1.35; border-left: 0 !important; border-right: 0 !important; border-bottom: 1px solid #d7dde5; vertical-align: middle; background: transparent; font-size: 12px !important; }",
    ".regression-result-panel table thead th { border-bottom: 2px solid #1f2937 !important; font-weight: 700; }",
    ".regression-result-panel table tbody tr:last-child td, .regression-result-panel table tbody tr:last-child th { border-bottom: 0 !important; }",
    ".regression-result-panel table th:first-child, .regression-result-panel table td:first-child { text-align: left !important; }",
    ".regression-result-panel table th:not(:first-child), .regression-result-panel table td:not(:first-child) { text-align: right !important; }",
    ".coefficient-table { table-layout: auto; }",
    ".coefficient-table th:first-child, .coefficient-table td:first-child { text-align: left !important; }",
    ".coefficient-table th:not(:first-child) { text-align: right !important; }",
    ".coefficient-table thead th { border-top: 2px solid #1f2937 !important; border-bottom: 2px solid #1f2937 !important; font-weight: 700; }",
    ".coefficient-table tbody td:not(:first-child), .coefficient-table tfoot td:not(:first-child) { text-align: right !important; }",
    ".coefficient-footnote-value { display: inline-block; position: relative; padding-right: .38em; white-space: nowrap; line-height: inherit; vertical-align: baseline; }",
    ".coefficient-footnote-marker { display: block; width: auto; margin-left: 0; font-size: 60%; line-height: 1; position: absolute; top: -.34em; right: 0; text-align: left; vertical-align: baseline; }",
    ".coefficient-header-break { display: inline-flex; flex-direction: column; gap: 0; line-height: 1.05; white-space: nowrap; }",
    ".coefficient-table th.coefficient-note-marker-cell, .coefficient-table td.coefficient-note-marker-cell { width: 14px; min-width: 14px; max-width: 14px; padding-left: 2px !important; padding-right: 2px !important; text-align: left !important; vertical-align: top; }",
    ".coefficient-table.crosstab-main-table > thead > tr > th.crosstab-col-head, .regression-result-panel .crosstab-main-table > thead > tr > th.crosstab-col-head { text-align: center !important; }",
    ".coefficient-table.crosstab-main-table > tbody > tr > td.crosstab-row-label, .regression-result-panel .crosstab-main-table > tbody > tr > td.crosstab-row-label { text-align: left !important; }",
    ".coefficient-table tfoot { border-bottom: 2px solid #1f2937 !important; }",
    ".coefficient-table .coefficient-fit-row td { text-align: center !important; border-top: 1px solid #d7dde5 !important; border-bottom: 0 !important; font-weight: 500; }",
    ".coefficient-table tfoot .coefficient-fit-row:first-child td { border-top: 2px solid #1f2937 !important; }",
    ".result-table-with-note { display: inline-block; width: auto; max-width: none; vertical-align: top; }",
    ".result-table-with-note > .coefficient-table, .result-table-with-note > table { display: table; }",
    ".result-table-with-note .coefficient-note, .result-table-with-note .coefficient-warning { width: 0; min-width: 100%; max-width: none; box-sizing: border-box; }",
    ".hierarchical-table-wrap { max-width: none; }",
    ".hierarchical-table-scroll { max-width: 100%; overflow-x: auto; }",
    ".hierarchical-coefficient-table { border-top: 0 !important; table-layout: fixed !important; width: auto; }",
    ".hierarchical-coefficient-table thead tr:first-child th { border-top: 2px solid #1f2937 !important; border-bottom: 0 !important; }",
    ".hierarchical-coefficient-table thead tr:first-child th:first-child { border-bottom: 2px solid #1f2937 !important; }",
    ".hierarchical-coefficient-table .hierarchical-model-header { text-align: center !important; font-weight: 700; border-bottom: 2px solid #1f2937 !important; }",
    ".hierarchical-coefficient-table thead tr:last-child th { border-top: 0 !important; border-bottom: 2px solid #1f2937 !important; }",
    ".hierarchical-coefficient-table thead tr:last-child th:not(.hierarchical-model-separator) { text-align: right !important; }",
    ".hierarchical-coefficient-table tbody td:not(:first-child):not(.hierarchical-model-separator) { text-align: right !important; }",
    ".hierarchical-coefficient-table thead tr:first-child th:first-child, .hierarchical-coefficient-table tbody td:first-child, .hierarchical-coefficient-table tfoot td:first-child { width: 240px; min-width: 240px; max-width: 240px; white-space: normal; overflow-wrap: break-word; word-break: keep-all; }",
    ".hierarchical-coefficient-table .hierarchical-term-col { width: 240px; }",
    ".hierarchical-coefficient-table .hierarchical-stat-col { width: 78px; }",
    ".hierarchical-coefficient-table .hierarchical-separator-col { width: 10px; }",
    ".hierarchical-coefficient-table thead tr:last-child th:not(.hierarchical-model-separator), .hierarchical-coefficient-table td:not(:first-child):not(.hierarchical-model-separator) { width: 78px; min-width: 78px; max-width: 78px; padding-left: 8px; padding-right: 8px; overflow-wrap: normal; white-space: nowrap; }",
    ".hierarchical-coefficient-table .hierarchical-model-separator { width: 10px; min-width: 10px; max-width: 10px; padding: 0 !important; background: transparent !important; }",
    ".hierarchical-coefficient-table .hierarchical-model-header-separator { border-bottom: 0 !important; }",
    ".hierarchical-coefficient-table .hierarchical-model-subheader-separator { border-bottom: 2px solid #1f2937 !important; }",
    ".hierarchical-coefficient-table tfoot .coefficient-fit-row td { border-top: 1px solid #d7dde5 !important; border-bottom: 0 !important; }",
    ".hierarchical-coefficient-table tfoot tr:first-child td { border-top: 2px solid #1f2937 !important; }",
    ".coefficient-note { color: #52606d; font-size: 11px; line-height: 1.4; margin-top: 4px; padding-top: 4px; text-align: left; white-space: normal; width: 100%; max-width: none; box-sizing: border-box; overflow-wrap: anywhere; word-break: normal; }",
    ".reference-summary-panel .effect-size-reference-panel { margin-bottom: 0; }",
    ".reference-summary-divider { height: 12px; }",
    ".residual-diagnostic-plots img { display: block; width: 420px; height: 420px; }",
    ".frequency-plot-grid, .frequency-plot-row, .correlation-plot-grid { display: flex; flex-wrap: wrap; gap: 16px; align-items: flex-start; }",
    ".frequency-plot-card, .correlation-plot-card, .residual-plot-card { border: 1px solid #d9e2ec; border-radius: 6px; padding: 12px; background: #ffffff; }",
    ".frequency-plot-card h4, .correlation-plot-card h4, .residual-plot-card h4 { margin: 0 0 8px; font-size: 15px; color: #15233a; }",
    ".frequency-plot-card img { display: block; width: 420px; height: 320px; }",
    ".correlation-plot-card img { display: block; width: 720px; height: 520px; max-width: 100%; }",
    "@media print {",
    "  @page { size: A4 portrait; margin: 10mm 8mm; @bottom-right { content: counter(page) '/' counter(pages); color: #627d98; font-size: 8pt; } }",
    "  @page easyflow-landscape { size: A4 landscape; margin: 6mm; @bottom-right { content: counter(page) '/' counter(pages); color: #627d98; font-size: 8pt; } }",
    "  * { box-sizing: border-box; }",
    "  body { margin: 0 !important; color: #000000; font-size: 10.5pt; }",
    "  .page-shell { width: 100%; max-width: 194mm !important; margin: 0 auto !important; padding: 0 !important; }",
    "  body.print-mixed-landscape .page-shell { max-width: 100% !important; }",
    "  h1 { font-size: 18pt; margin: 0 0 8pt; }",
    "  .report-cover { height: 273mm; min-height: 273mm; margin: 0 !important; padding: 18mm 14mm 16mm !important; border: 0 !important; border-radius: 0 !important; box-shadow: none !important; break-after: page; page-break-after: always; }",
    "  .report-cover::before { width: 3mm !important; }",
    "  .report-cover-logo { width: 82mm !important; max-width: 82mm !important; }",
    "  .report-cover-main { padding-bottom: 10mm !important; padding-top: 38mm !important; }",
    "  .report-cover-title { font-size: 30pt !important; margin: 4mm 0 5mm !important; }",
    "  .report-cover-subtitle { font-size: 12pt !important; }",
    "  .report-cover-license { gap: 5mm !important; margin-bottom: 6mm !important; }",
    "  .report-cover-license-logo { max-height: 12mm !important; max-width: 42mm !important; }",
    "  .report-cover-license-value { font-size: 10.5pt !important; }",
    "  .report-watermark-inner { opacity: .11 !important; -webkit-print-color-adjust: exact; print-color-adjust: exact; }",
    "  .report-watermark-brand-row { gap: 6mm !important; min-width: 178mm !important; }",
    "  .report-watermark-logo-efs { max-height: 19mm !important; max-width: 76mm !important; }",
    "  .report-watermark-logo-statedu { max-height: 16mm !important; max-width: 62mm !important; }",
    "  .report-watermark-name { font-size: 13pt !important; }",
    "  .report-watermark-subname { font-size: 8.5pt !important; }",
    "  .report-watermark-site { font-size: 7pt !important; }",
    "  .report-watermark-divider { height: 20mm !important; width: .8mm !important; }",
    "  .report-watermark-edition { font-size: 8.5pt !important; padding: 2mm 4mm !important; }",
    "  .report-watermark-beta { font-size: 11pt !important; margin-top: 4.5mm !important; }",
    "  .report-cover-meta { font-size: 9.5pt !important; grid-template-columns: repeat(2, minmax(0, 1fr)) !important; }",
    "  .report-cover-footer { font-size: 8.5pt !important; }",
    "  .report-body-heading { display: none !important; }",
    "  .saved-results-meta { font-size: 8.5pt; margin-bottom: 10pt; }",
    "  .regression-result-panel, .result-section.regression-result-panel { width: 188mm !important; max-width: 100% !important; overflow: visible !important; padding: 8pt 2mm !important; box-sizing: border-box !important; border-left: 0 !important; border-right: 0 !important; border-radius: 0 !important; break-inside: auto; page-break-inside: auto; margin-left: auto !important; margin-right: auto !important; }",
    "  .regression-results > .regression-result-panel:has(table) { break-before: page; page-break-before: always; }",
    "  body.print-mixed-landscape .regression-results .landscape-table-panel { page: easyflow-landscape; width: 283mm !important; max-width: 283mm !important; margin-left: auto !important; margin-right: auto !important; break-before: page; page-break-before: always; break-after: page; page-break-after: always; }",
    "  body.print-mixed-landscape .landscape-table-panel table { width: 100% !important; max-width: 100% !important; min-width: 0 !important; table-layout: fixed !important; }",
    "  body.print-mixed-landscape .landscape-table-panel .result-table-with-note, body.print-mixed-landscape .landscape-table-panel .hierarchical-table-wrap, body.print-mixed-landscape .landscape-table-panel .hierarchical-table-scroll { width: calc(100% - 2mm) !important; max-width: calc(100% - 2mm) !important; margin-left: auto !important; margin-right: auto !important; }",
    "  .regression-results > .regression-result-panel:first-child { break-before: auto; page-break-before: auto; }",
    "  .regression-results > .diagnostic-plots-section, .regression-results > .frequency-plots-section, .regression-results > .correlation-plot-section { break-before: auto !important; page-break-before: auto !important; break-inside: auto !important; page-break-inside: auto !important; }",
    "  .regression-result-panel h3 { font-size: 11pt; margin: 0 0 5pt; }",
    "  .result-table-with-note, .frequency-table-wrap, .hierarchical-table-wrap, .hierarchical-table-scroll { display: block !important; width: 100% !important; max-width: 100% !important; margin-left: auto !important; margin-right: auto !important; overflow: visible !important; }",
    "  table, .regression-result-panel table, .coefficient-table { width: 100% !important; min-width: 0 !important; max-width: 100% !important; table-layout: fixed !important; font-size: 9pt !important; box-sizing: border-box !important; }",
    "  .regression-result-panel > table { width: 100% !important; max-width: 100% !important; margin-left: auto !important; margin-right: auto !important; }",
    "  .hierarchical-coefficient-table { width: 100% !important; min-width: 0 !important; max-width: 100% !important; table-layout: fixed !important; font-size: 8.2pt !important; box-sizing: border-box !important; }",
    "  .coefficient-table col { width: auto !important; }",
    "  .coefficient-table col.coefficient-col-note-marker { width: 12px !important; min-width: 12px !important; max-width: 12px !important; }",
    "  .coefficient-table .coefficient-col-term, .coefficient-table .coefficient-col-b, .coefficient-table .coefficient-col-reference, .coefficient-table .coefficient-col-tolerance, .coefficient-table .coefficient-col-compact, .coefficient-table .coefficient-col-stat { width: auto !important; }",
    "  .hierarchical-coefficient-table .hierarchical-term-col { width: 30% !important; }",
    "  .hierarchical-coefficient-table .hierarchical-stat-col { width: 62px !important; }",
    "  .hierarchical-coefficient-table .hierarchical-separator-col { width: 6px !important; }",
    "  body.print-mixed-landscape .hierarchical-coefficient-table .hierarchical-term-col { width: 34% !important; }",
    "  body.print-mixed-landscape .hierarchical-coefficient-table .hierarchical-stat-col { width: 58px !important; }",
    "  .hierarchical-coefficient-table, .correlation-result-section table { font-size: 7.2pt !important; }",
    "  .paired-two-grouped-table { table-layout: fixed !important; font-size: 9.2pt !important; }",
    "  .paired-two-grouped-table col.paired-two-col-variable { width: 18% !important; }",
    "  .paired-two-grouped-table col.paired-two-col-summary { width: 8% !important; }",
    "  .paired-two-grouped-table col.paired-two-col-stat { width: 7.5% !important; }",
    "  .paired-two-grouped-table col.paired-two-col-effect { width: 8.5% !important; }",
    "  .paired-two-grouped-table col.paired-two-col-posthoc { width: 12% !important; }",
    "  body.print-mixed-landscape .paired-rm-grouped-table { width: 100% !important; max-width: 100% !important; table-layout: fixed !important; font-size: 8.1pt !important; }",
    "  body.print-mixed-landscape .paired-rm-grouped-table col.paired-rm-col-variable { width: 15% !important; }",
    "  body.print-mixed-landscape .paired-rm-grouped-table col.paired-rm-col-n { width: 3.6% !important; }",
    "  body.print-mixed-landscape .paired-rm-grouped-table col.paired-rm-col-time { width: 5.7% !important; }",
    "  body.print-mixed-landscape .paired-rm-grouped-table col.paired-rm-col-stat { width: 4.8% !important; }",
    "  body.print-mixed-landscape .paired-rm-grouped-table col.paired-rm-col-p { width: 4% !important; }",
    "  body.print-mixed-landscape .paired-rm-grouped-table col.paired-rm-col-es { width: 5.2% !important; }",
    "  body.print-mixed-landscape .paired-rm-grouped-table col.paired-rm-col-posthoc { width: 12% !important; }",
    "  body.print-mixed-landscape .landscape-table-panel .paired-rm-grouped-table { width: 100% !important; max-width: 100% !important; min-width: 0 !important; }",
    "  thead { display: table-header-group; }",
    "  tfoot { display: table-footer-group; }",
    "  tr { break-inside: avoid; page-break-inside: avoid; }",
    "  th, td, .regression-result-panel table th, .regression-result-panel table td { padding: 5px 7px !important; white-space: normal !important; overflow-wrap: anywhere !important; word-break: normal !important; }",
    "  .coefficient-table th, .coefficient-table td { padding: 5px 7px !important; line-height: 1.35 !important; white-space: normal !important; overflow-wrap: anywhere !important; word-break: normal !important; }",
    "  .paired-two-grouped-table th, .paired-two-grouped-table td { padding: 3px 4px !important; overflow-wrap: normal !important; word-break: keep-all !important; }",
    "  .paired-two-grouped-table thead tr:first-child th[colspan] { text-align: center !important; }",
    "  .paired-two-grouped-table th:not(:first-child), .paired-two-grouped-table td:not(:first-child) { white-space: nowrap !important; }",
    "  .paired-two-grouped-table th:not(:first-child), .paired-two-grouped-table td:not(:first-child) { text-align: right !important; }",
    "  body.print-mixed-landscape .paired-rm-grouped-table th, body.print-mixed-landscape .paired-rm-grouped-table td { padding: 3px 4px !important; overflow-wrap: normal !important; word-break: keep-all !important; }",
    "  body.print-mixed-landscape .paired-rm-grouped-table th:not(:first-child):not(:last-child), body.print-mixed-landscape .paired-rm-grouped-table td:not(:first-child):not(:last-child) { white-space: nowrap !important; }",
    "  body.print-mixed-landscape .paired-rm-grouped-table th:not(:first-child), body.print-mixed-landscape .paired-rm-grouped-table td:not(:first-child) { text-align: right !important; }",
    "  body.print-mixed-landscape .paired-rm-grouped-table th:first-child, body.print-mixed-landscape .paired-rm-grouped-table td:first-child { text-align: left !important; }",
    "  .coefficient-footnote-value { display: inline-block !important; position: relative !important; padding-right: .38em !important; white-space: nowrap !important; line-height: inherit !important; vertical-align: baseline !important; }",
    "  .coefficient-footnote-marker { width: auto !important; margin-left: 0 !important; font-size: 60% !important; line-height: 1 !important; position: absolute !important; top: -.34em !important; right: 0 !important; text-align: left !important; vertical-align: baseline !important; }",
    "  .coefficient-header-break { display: inline-flex !important; flex-direction: column !important; gap: 0 !important; line-height: 1.05 !important; white-space: nowrap !important; }",
    "  .coefficient-table th.coefficient-note-marker-cell, .coefficient-table td.coefficient-note-marker-cell { width: 10px !important; min-width: 10px !important; max-width: 10px !important; padding-left: 0 !important; padding-right: 1px !important; text-align: left !important; vertical-align: top !important; line-height: 1 !important; white-space: nowrap !important; overflow-wrap: normal !important; }",
    "  .coefficient-table th:last-child, .coefficient-table td:last-child, .regression-result-panel table th:last-child, .regression-result-panel table td:last-child { padding-right: 20px !important; }",
    "  .coefficient-table th { white-space: normal !important; }",
    "  .paired-two-grouped-table th, .paired-two-grouped-table td, body.print-mixed-landscape .paired-rm-grouped-table th, body.print-mixed-landscape .paired-rm-grouped-table td { white-space: nowrap !important; overflow-wrap: normal !important; word-break: keep-all !important; }",
    "  .paired-two-grouped-table th[colspan], body.print-mixed-landscape .paired-rm-grouped-table th[colspan] { text-align: center !important; }",
    "  .paired-two-grouped-table th:first-child, .paired-two-grouped-table td:first-child, body.print-mixed-landscape .paired-rm-grouped-table th:first-child, body.print-mixed-landscape .paired-rm-grouped-table td:first-child { text-align: left !important; white-space: normal !important; }",
    "  .paired-two-grouped-table th:not(:first-child), .paired-two-grouped-table td:not(:first-child), body.print-mixed-landscape .paired-rm-grouped-table th:not(:first-child), body.print-mixed-landscape .paired-rm-grouped-table td:not(:first-child) { text-align: right !important; }",
    "  .coefficient-table td:first-child { overflow-wrap: normal !important; word-break: keep-all !important; }",
    "  .hierarchical-coefficient-table th, .hierarchical-coefficient-table td, .correlation-result-section table th, .correlation-result-section table td { padding: 3pt 3.5pt !important; }",
    "  .coefficient-table th:not(:first-child), .coefficient-table td:not(:first-child), .regression-result-panel table th:not(:first-child), .regression-result-panel table td:not(:first-child) { text-align: right !important; }",
    "  .coefficient-table th:first-child, .coefficient-table td:first-child, .regression-result-panel table th:first-child, .regression-result-panel table td:first-child { text-align: left !important; }",
    "  .coefficient-table.crosstab-main-table > thead > tr > th.crosstab-col-head, .regression-result-panel .crosstab-main-table > thead > tr > th.crosstab-col-head { text-align: center !important; }",
    "  .coefficient-table.crosstab-main-table > tbody > tr > td.crosstab-row-label, .regression-result-panel .crosstab-main-table > tbody > tr > td.crosstab-row-label { text-align: left !important; }",
    "  .coefficient-table .coefficient-fit-row td { border-top: 1px solid #d7dde5 !important; }",
    "  .coefficient-table tfoot .coefficient-fit-row:first-child td { border-top: 2px solid #1f2937 !important; }",
    "  .reference-summary-panel { break-inside: avoid; page-break-inside: avoid; }",
    "  .hierarchical-coefficient-table thead tr:first-child th:first-child, .hierarchical-coefficient-table tbody td:first-child, .hierarchical-coefficient-table tfoot td:first-child, .hierarchical-coefficient-table .hierarchical-term-col, .hierarchical-coefficient-table .hierarchical-stat-col, .hierarchical-coefficient-table .hierarchical-separator-col, .hierarchical-coefficient-table thead tr:last-child th:not(.hierarchical-model-separator), .hierarchical-coefficient-table td:not(:first-child):not(.hierarchical-model-separator) { width: auto !important; min-width: 0 !important; max-width: none !important; }",
    "  body.print-mixed-landscape .landscape-table-panel .hierarchical-coefficient-table { width: 100% !important; min-width: 0 !important; max-width: 100% !important; font-size: 6.5pt !important; }",
    "  body.print-mixed-landscape .landscape-table-panel .hierarchical-coefficient-table col.hierarchical-term-col { width: 13% !important; min-width: 0 !important; max-width: none !important; }",
    "  body.print-mixed-landscape .landscape-table-panel .hierarchical-coefficient-table col.hierarchical-stat-col { width: auto !important; min-width: 0 !important; max-width: none !important; }",
    "  body.print-mixed-landscape .landscape-table-panel .hierarchical-coefficient-table col.hierarchical-stat-col-narrow { width: 5.4% !important; min-width: 0 !important; max-width: none !important; }",
    "  body.print-mixed-landscape .landscape-table-panel .hierarchical-coefficient-table col.hierarchical-stat-col:nth-last-child(2), body.print-mixed-landscape .landscape-table-panel .hierarchical-coefficient-table col.hierarchical-stat-col:nth-last-child(3) { width: 6.8% !important; }",
    "  body.print-mixed-landscape .landscape-table-panel .hierarchical-coefficient-table col.hierarchical-separator-col { width: .8% !important; min-width: 0 !important; max-width: none !important; }",
    "  body.print-mixed-landscape .landscape-table-panel .hierarchical-coefficient-table th, body.print-mixed-landscape .landscape-table-panel .hierarchical-coefficient-table td { padding: 2.5pt 2.8pt !important; white-space: nowrap !important; overflow-wrap: normal !important; word-break: normal !important; }",
    "  body.print-mixed-landscape .landscape-table-panel .hierarchical-coefficient-table thead tr:first-child th { border-bottom: 2px solid #1f2937 !important; text-align: center !important; }",
    "  body.print-mixed-landscape .landscape-table-panel .hierarchical-coefficient-table thead tr:first-child th:first-child { text-align: left !important; }",
    "  body.print-mixed-landscape .landscape-table-panel .hierarchical-coefficient-table th:first-child, body.print-mixed-landscape .landscape-table-panel .hierarchical-coefficient-table td:first-child { white-space: normal !important; overflow-wrap: break-word !important; word-break: keep-all !important; }",
    "  .coefficient-note, .coefficient-warning { width: 100% !important; max-width: 100% !important; font-size: 8pt !important; }",
    "  img, .residual-diagnostic-plots img, .frequency-plot-card img, .correlation-plot-card img { max-width: 100% !important; height: auto !important; }",
    "  .frequency-plot-grid, .frequency-plot-row, .correlation-plot-grid, .residual-diagnostic-plots { display: grid !important; grid-template-columns: repeat(2, minmax(0, 1fr)) !important; gap: 7pt !important; align-items: start !important; break-inside: auto !important; page-break-inside: auto !important; }",
    "  .frequency-plot-grid > .frequency-plot-card, .frequency-plot-row > .frequency-plot-card, .residual-diagnostic-plots > .residual-plot-card, .correlation-plot-grid > .correlation-plot-card { width: 100% !important; max-width: 100% !important; margin-bottom: 0 !important; break-inside: avoid; page-break-inside: avoid; }",
    "  .correlation-plot-section > .correlation-plot-card { width: 100% !important; max-width: 100% !important; margin-bottom: 10pt; break-inside: avoid; page-break-inside: avoid; }",
    "  .frequency-plot-card h4, .correlation-plot-card h4, .residual-plot-card h4 { font-size: 9pt !important; margin-bottom: 4pt !important; }",
    "  .frequency-plot-card, .correlation-plot-card, .residual-plot-card { padding: 6pt !important; }",
    "  .frequency-plot-card img, .residual-plot-card img { display: block !important; width: auto !important; height: 68mm !important; max-width: 100% !important; object-fit: contain !important; margin: 0 auto !important; }",
    "  .correlation-plot-card img { width: 100% !important; }",
    "}",
    sep = "\n"
  )
}

saved_results_viewer_css <- function(max_width = 1280) {
  paste(
    "body { background: #ffffff !important; color: #2f3a46; font-family: Arial, Helvetica, sans-serif; font-size: 16px; margin: 0; }",
    sprintf(".page-shell { max-width: %dpx; margin: 24px auto; padding: 0 18px; }", max_width),
    ".report-watermark { left: 50%; pointer-events: none; position: fixed; transform: translate(-50%, -50%) rotate(-24deg); z-index: 9999; }",
    ".report-watermark-upper { top: 34%; }",
    ".report-watermark-lower { top: 68%; }",
    ".report-watermark-inner { color: #102a43; opacity: .12; text-align: center; }",
    ".report-watermark-brand-row { align-items: center; display: flex; gap: 26px; justify-content: center; min-width: 830px; }",
    ".report-watermark-item { align-items: center; display: flex; flex-direction: column; gap: 5px; justify-content: center; }",
    ".report-watermark-logo { display: block; object-fit: contain; }",
    ".report-watermark-logo-efs { max-height: 86px; max-width: 340px; }",
    ".report-watermark-logo-statedu { max-height: 74px; max-width: 270px; }",
    ".report-watermark-name { color: #102a43; display: block; font-size: 26px; font-weight: 800; letter-spacing: .04em; line-height: 1.1; white-space: nowrap; }",
    ".report-watermark-subname { color: #102a43; display: block; font-size: 17px; font-weight: 800; letter-spacing: .02em; line-height: 1.1; white-space: nowrap; }",
    ".report-watermark-site { color: #486581; display: block; font-size: 13px; font-weight: 700; letter-spacing: .08em; line-height: 1.1; white-space: nowrap; }",
    ".report-watermark-divider { background: #0fa3a3; height: 92px; width: 3px; }",
    ".report-watermark-edition { border: 2px solid #102a43; border-radius: 999px; color: #102a43; font-size: 18px; font-weight: 800; letter-spacing: .18em; padding: 8px 16px; text-transform: uppercase; white-space: nowrap; }",
    ".report-watermark-beta { color: #102a43; font-size: 24px; font-weight: 900; letter-spacing: .04em; line-height: 1.25; margin-top: 18px; text-transform: uppercase; white-space: nowrap; }",
    ".saved-results-meta { color: #52606d; margin: 4px 0 18px; font-size: 13px; }",
    ".regression-result-panel { background: #ffffff; border: 1px solid #d9e2ec; border-radius: 6px; padding: 18px 20px; margin-bottom: 22px; }",
    ".result-section.regression-result-panel, .regression-result-panel { max-width: 100%; overflow-x: auto; box-sizing: border-box; }",
    ".result-section.regression-result-panel:not(.landscape-table-panel):not(.paired-result-section):not(.logistic-diagnostics-panel) { width: min(100%, 590px); }",
    ".result-section.regression-result-panel:not(.landscape-table-panel):not(.paired-result-section):not(.logistic-diagnostics-panel) > .result-table-with-note, .result-section.regression-result-panel:not(.landscape-table-panel):not(.paired-result-section):not(.logistic-diagnostics-panel) > .frequency-table-wrap, .result-section.regression-result-panel:not(.landscape-table-panel):not(.paired-result-section):not(.logistic-diagnostics-panel) > table, .result-section.regression-result-panel:not(.landscape-table-panel):not(.paired-result-section):not(.logistic-diagnostics-panel) .result-table-with-note > table { width: 100% !important; max-width: 100%; }",
    ".result-section.regression-result-panel:not(.landscape-table-panel):not(.paired-result-section):not(.logistic-diagnostics-panel) table { width: 100% !important; min-width: 0 !important; max-width: 100% !important; table-layout: fixed; }",
    ".result-section.regression-result-panel.landscape-table-panel { width: min(100%, 890px); }",
    ".result-section.regression-result-panel.landscape-table-panel > .result-table-with-note, .result-section.regression-result-panel.landscape-table-panel > .hierarchical-table-wrap, .result-section.regression-result-panel.landscape-table-panel > .hierarchical-table-scroll, .result-section.regression-result-panel.landscape-table-panel > table, .result-section.regression-result-panel.landscape-table-panel .result-table-with-note > table { width: 100% !important; max-width: 100%; }",
    ".result-section.regression-result-panel.landscape-table-panel table { width: 100% !important; min-width: 0 !important; max-width: 100% !important; table-layout: fixed; }",
    ".result-section.regression-result-panel.ttest-anova-overview-panel, .result-section.regression-result-panel.ttest-anova-assumption-review-panel { width: 100% !important; max-width: 100% !important; overflow-x: hidden !important; }",
    ".result-section.regression-result-panel.ttest-anova-overview-panel > .result-table-with-note, .result-section.regression-result-panel.ttest-anova-assumption-review-panel > .result-table-with-note, .result-section.regression-result-panel.ttest-anova-overview-panel .result-table-with-note > table, .result-section.regression-result-panel.ttest-anova-assumption-review-panel .result-table-with-note > table { width: 100% !important; max-width: 100% !important; min-width: 0 !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df .coefficient-col-statistic { width: 112px !important; min-width: 112px !important; text-align: right !important; white-space: nowrap !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df:not(.coefficient-table-mean-sd) .coefficient-col-stat { width: 44px !important; min-width: 44px !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df:not(.coefficient-table-mean-sd) .coefficient-col-statistic { width: 128px !important; min-width: 128px !important; }",
    ".regression-results > .regression-result-panel:not(.landscape-table-panel):not(.model-overview-panel):not(.assumption-review-panel):not(.reference-summary-panel):not(.diagnostic-plots-section):not(.frequency-plots-section):not(.correlation-plot-section):not(.logistic-diagnostics-panel) { width: min(100%, 590px); max-width: 100%; overflow-x: hidden; box-sizing: border-box; }",
    ".regression-results > .regression-result-panel:not(.landscape-table-panel):not(.model-overview-panel):not(.assumption-review-panel):not(.reference-summary-panel):not(.diagnostic-plots-section):not(.frequency-plots-section):not(.correlation-plot-section):not(.logistic-diagnostics-panel) table { width: 100% !important; min-width: 0 !important; max-width: 100% !important; table-layout: fixed; }",
    ".regression-results > .regression-result-panel:not(.landscape-table-panel):not(.model-overview-panel):not(.assumption-review-panel):not(.reference-summary-panel):not(.diagnostic-plots-section):not(.frequency-plots-section):not(.correlation-plot-section):not(.logistic-diagnostics-panel) .coefficient-table .coefficient-col-term, .regression-results > .regression-result-panel:not(.landscape-table-panel):not(.model-overview-panel):not(.assumption-review-panel):not(.reference-summary-panel):not(.diagnostic-plots-section):not(.frequency-plots-section):not(.correlation-plot-section):not(.logistic-diagnostics-panel) .coefficient-table th:first-child, .regression-results > .regression-result-panel:not(.landscape-table-panel):not(.model-overview-panel):not(.assumption-review-panel):not(.reference-summary-panel):not(.frequency-plots-section):not(.diagnostic-plots-section):not(.correlation-plot-section):not(.logistic-diagnostics-panel) .coefficient-table td:first-child { width: 210px; min-width: 210px; white-space: normal; overflow-wrap: normal; word-break: keep-all; }",
    ".regression-results > .regression-result-panel.landscape-table-panel { width: min(100%, 890px); max-width: 100%; overflow-x: hidden; box-sizing: border-box; }",
    ".regression-results > .regression-result-panel.landscape-table-panel table { width: 100% !important; min-width: 0 !important; max-width: 100% !important; table-layout: fixed; }",
    ".regression-result-panel h3 { color: #15233a; font-size: 15px; font-weight: 700; margin: 0 0 8px; }",
    ".regression-result-panel table, .coefficient-table { width: auto; min-width: 440px; border-collapse: collapse !important; border-spacing: 0 !important; border-top: 2px solid #1f2937 !important; border-bottom: 2px solid #1f2937 !important; color: #2f3a46; font-size: 12px; background: transparent; }",
    ".regression-result-panel table th, .regression-result-panel table td { padding: 5px 7px; line-height: 1.35; border-left: 0 !important; border-right: 0 !important; border-bottom: 1px solid #d7dde5; vertical-align: middle; background: transparent; white-space: nowrap; font-size: 12px !important; }",
    ".regression-result-panel table thead th { border-bottom: 2px solid #1f2937 !important; font-weight: 700; }",
    ".regression-result-panel table th:first-child, .regression-result-panel table td:first-child { text-align: left !important; }",
    ".regression-result-panel table th:not(:first-child), .regression-result-panel table td:not(:first-child) { text-align: right !important; }",
        ".coefficient-table.crosstab-main-table > thead > tr > th.crosstab-col-head, .regression-result-panel .crosstab-main-table > thead > tr > th.crosstab-col-head { text-align: center !important; }",
        ".coefficient-table.crosstab-main-table > tbody > tr > td.crosstab-row-label, .regression-result-panel .crosstab-main-table > tbody > tr > td.crosstab-row-label { text-align: left !important; }",
        ".result-table-with-note, .frequency-table-wrap, .hierarchical-table-wrap, .hierarchical-table-scroll { max-width: 100%; overflow-x: auto; }",
    ".frequency-plot-grid, .frequency-plot-row, .correlation-plot-grid, .residual-diagnostic-plots { display: flex; flex-wrap: wrap; gap: 16px; align-items: flex-start; }",
    ".frequency-plot-card, .correlation-plot-card, .residual-plot-card { border: 1px solid #d9e2ec; border-radius: 6px; padding: 12px; background: #ffffff; }",
    ".frequency-plot-card h4, .correlation-plot-card h4, .residual-plot-card h4 { margin: 0 0 8px; font-size: 15px; color: #15233a; }",
    ".frequency-plot-card img, .residual-plot-card img, .correlation-plot-card img { display: block; max-width: 100%; height: auto; }",
    sep = "\n"
  )
}

saved_results_document <- function(title, content, max_width = 1280, css_path = file.path("www", "style.css"), print_landscape = FALSE, report_mode = FALSE) {
  css <- if (file.exists(css_path)) paste(readLines(css_path, warn = FALSE), collapse = "\n") else ""
  saved_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  app_version <- saved_results_app_version()
  app_label <- if (nzchar(app_version)) sprintf("EasyFlow Statistics v%s", app_version) else "EasyFlow Statistics"
  logo_uri <- saved_results_image_data_uri(file.path("www", "logo-horizontal.png"))
  cover_text <- saved_results_cover_text()
  organization_logo_uri <- if (nzchar(cover_text$organization_logo)) saved_results_image_data_uri(cover_text$organization_logo) else ""
  cover_target <- if (nzchar(cover_text$organization)) cover_text$organization else "Internal analysis report"
  cover_user <- if (nzchar(cover_text$user)) cover_text$user else "EasyFlow Statistics user"
  cover_license_name <- if (identical(cover_text$edition, "institution") && nzchar(cover_text$organization)) {
    cover_text$organization
  } else if (nzchar(cover_text$user)) {
    cover_text$user
  } else {
    ""
  }
  cover_license_label <- if (identical(cover_text$edition, "development")) {
    "Development owner"
  } else if (identical(cover_text$edition, "institution")) {
    "Institution"
  } else {
    "Licensed user"
  }
  cover_meta_items <- list()
  if (!identical(cover_text$edition, "personal") && nzchar(cover_text$organization)) {
    cover_meta_items <- c(cover_meta_items, list(div(class = "report-cover-meta-item", span("Prepared for", class = "report-cover-meta-label"), span(cover_target, class = "report-cover-meta-value"))))
  }
  if (!identical(cover_text$edition, "institution") || nzchar(cover_text$user)) {
    cover_meta_items <- c(cover_meta_items, list(div(class = "report-cover-meta-item", span("Prepared by", class = "report-cover-meta-label"), span(cover_user, class = "report-cover-meta-value"))))
  }
  cover_meta_items <- c(
    cover_meta_items,
    list(
      div(class = "report-cover-meta-item", span("Saved", class = "report-cover-meta-label"), span(saved_time, class = "report-cover-meta-value")),
      div(class = "report-cover-meta-item", span("Output date", class = "report-cover-meta-label"), span(saved_time, class = "report-cover-meta-value")),
      div(class = "report-cover-meta-item", span("Application", class = "report-cover-meta-label"), span(app_label, class = "report-cover-meta-value"))
    )
  )
  inline_css <- if (isTRUE(report_mode)) {
    saved_results_inline_css(max_width, print_landscape = print_landscape)
  } else {
    saved_results_viewer_css(max_width)
  }
  watermark <- if (identical(cover_text$edition, "development")) {
    saved_results_development_watermark(logo_uri, organization_logo_uri, cover_text$organization)
  } else {
    NULL
  }
  body_content <- if (isTRUE(report_mode)) {
    div(
      class = "page-shell",
      watermark,
      div(
        class = "report-cover",
        div(
          class = "report-cover-brand",
          if (nzchar(logo_uri)) {
            tags$img(src = logo_uri, class = "report-cover-logo", alt = "EasyFlow Statistics")
          } else {
            div("EasyFlow Statistics", class = "report-cover-kicker")
          },
          div(cover_text$edition, class = "report-cover-edition")
        ),
        div(
          class = "report-cover-main",
          div("Statistical Report", class = "report-cover-kicker"),
          h1(title, class = "report-cover-title"),
          p("Generated analysis results prepared for review, documentation, and print output.", class = "report-cover-subtitle"),
          div(class = "report-cover-divider")
        ),
        if (nzchar(organization_logo_uri) || nzchar(cover_license_name)) {
          div(
            class = "report-cover-license",
            if (nzchar(organization_logo_uri)) {
              tags$img(src = organization_logo_uri, class = "report-cover-license-logo", alt = "Organization logo")
            },
            if (nzchar(cover_license_name)) {
              div(
                span(cover_license_label, class = "report-cover-license-label"),
                span(cover_license_name, class = "report-cover-license-value")
              )
            }
          )
        },
        div(
          class = "report-cover-meta",
          cover_meta_items
        ),
        div(cover_text$footer, class = "report-cover-footer")
      ),
      div(
        class = "report-body",
        div(class = "report-body-heading", h1(title), div(class = "saved-results-meta", sprintf("Saved: %s", saved_time))),
        content
      )
    )
  } else {
    div(
      class = "page-shell",
      watermark,
      h1(title),
      div(class = "saved-results-meta", sprintf("Saved: %s", saved_time)),
      content
    )
  }
  document <- tags$html(
    tags$head(
      tags$meta(charset = "UTF-8"),
      tags$title(title),
      tags$style(htmltools::HTML(css)),
      tags$style(htmltools::HTML(inline_css))
    ),
    tags$body(
      class = if (isTRUE(print_landscape)) "print-mixed-landscape" else "print-portrait",
      body_content
    )
  )
  paste0("<!DOCTYPE html>\n", tags_to_html(document))
}

saved_analysis_results_html <- function(
  results,
  variable_table = NULL,
  labels = character(0),
  category_table = NULL,
  refs = character(0),
  value_labels = list(),
  show_sr2 = FALSE,
  show_f2 = FALSE,
  show_vif = FALSE,
  css_path = file.path("www", "style.css"),
  report_mode = FALSE
) {
  saved_results_document(
    "EasyFlow Statistics Results",
    div(
      class = "regression-results",
      div(
        class = "regression-result-panel model-overview-panel",
        h3("Model overview"),
        model_overview_html_table(model_overview_data_frame(results, variable_table, labels))
      ),
      lapply(seq_along(results), function(index) {
        regression_coefficient_result_block(
          results[[index]],
          variable_table,
          labels,
          category_table,
          refs,
          value_labels,
          show_sr2,
          show_f2,
          show_vif
        )
      }),
      regression_reference_summary_block(results, variable_table, labels, show_sr2, show_f2),
      lapply(seq_along(results), function(index) {
        result <- results[[index]]
        dependent <- all.vars(result$formula)[[1]]
        dependent_label <- display_variable_name_static(dependent, variable_table, labels, label_only = TRUE)
        saved_plot_result_block(result, dependent_label)
      })
    )
    ,
    max_width = 1280,
    css_path = css_path,
    report_mode = report_mode
  )
}

saved_hierarchical_results_html <- function(
  results,
  variable_table = NULL,
  labels = character(0),
  category_table = NULL,
  refs = character(0),
  value_labels = list(),
  show_sr2 = FALSE,
  show_f2 = FALSE,
  show_vif = FALSE,
  css_path = file.path("www", "style.css"),
  report_mode = FALSE
) {
  print_landscape <- any(vapply(hierarchical_result_groups(results), function(group) length(group) >= 3L, logical(1)))
  saved_results_document(
    "EasyFlow Statistics Hierarchical Results",
    hierarchical_results_panel(
      results = results,
      variable_table = variable_table,
      labels = labels,
      category_table = category_table,
      refs = refs,
      value_labels = value_labels,
      show_sr2 = show_sr2,
      show_f2 = show_f2,
      show_vif = show_vif,
      plot_blocks = lapply(seq_along(results), function(index) {
        result <- results[[index]]
        dependent <- all.vars(result$formula)[[1]]
        dependent_label <- display_variable_name_static(dependent, variable_table, labels, label_only = TRUE)
        saved_plot_result_block(result, dependent_label)
      })
    ),
    max_width = 1500,
    css_path = css_path,
    print_landscape = print_landscape,
    report_mode = report_mode
  )
}

saved_frequency_plot_blocks <- function(result, options) {
  plot_block <- function(type, name) {
    variable_label <- frequency_variable_display_name(name, result$variable_info, result$labels, result$category_table)
    title <- sprintf("%s(%s)", frequency_plot_label(type), variable_label)
    tags$div(
      class = "frequency-plot-card",
      tags$h4(title),
      tags$img(
        src = plot_data_uri(function(plot_result) draw_frequency_plot(plot_result, type, name), result, width = 420, height = 320),
        width = "420",
        height = "320",
        alt = title
      )
    )
  }
  blocks <- list()
  categorical <- as.character(result$categorical %||% character(0))
  continuous <- as.character(result$continuous %||% character(0))
  for (name in categorical) {
    if (isTRUE(options$pie)) blocks <- c(blocks, list(plot_block("pie", name)))
    if (isTRUE(options$bar)) blocks <- c(blocks, list(plot_block("bar", name)))
  }
  for (name in continuous) {
    if (isTRUE(options$histogram)) blocks <- c(blocks, list(plot_block("histogram", name)))
    if (isTRUE(options$box)) blocks <- c(blocks, list(plot_block("box", name)))
    if (isTRUE(options$violin)) blocks <- c(blocks, list(plot_block("violin", name)))
  }
  if (length(blocks) == 0) {
    return(NULL)
  }
  tags$div(
    class = "regression-result-panel frequency-plots-section",
    tags$h3("Plots"),
    tags$div(class = "frequency-plot-grid", blocks)
  )
}

saved_frequencies_results_html <- function(result, css_path = file.path("www", "style.css"), report_mode = FALSE) {
  options <- result$options %||% list(n_percent = TRUE, mean_sd = TRUE)
  table <- frequency_combined_table(result, options)
  saved_results_document(
    "EasyFlow Statistics Frequencies Results",
    tags$div(
      class = "regression-results",
      tags$div(
        class = "result-section frequencies-result-section regression-result-panel",
        tags$h3("Frequencies / Descriptives"),
        tags$div(class = "frequency-table-wrap", coefficient_html_table(table))
      ),
      saved_frequency_plot_blocks(result, options)
    ),
    max_width = 1500,
    css_path = css_path,
    report_mode = report_mode
  )
}

saved_reliability_results_html <- function(result, css_path = file.path("www", "style.css"), report_mode = FALSE) {
  saved_results_document(
    "EasyFlow Statistics Reliability Results",
    tags$div(class = "regression-results", reliability_results_ui(result)),
    max_width = 1500,
    css_path = css_path,
    report_mode = report_mode
  )
}

saved_ttest_anova_results_html <- function(result, css_path = file.path("www", "style.css"), report_mode = FALSE) {
  print_landscape <- length(result$dependents %||% character(0)) >= 5L
  saved_results_document(
    "EasyFlow Statistics t-test / ANOVA Results",
    tags$div(class = "regression-results", ttest_anova_results_ui(result)),
    max_width = 1500,
    css_path = css_path,
    print_landscape = print_landscape,
    report_mode = report_mode
  )
}

saved_ancova_results_html <- function(result, variable_table = NULL, labels = character(0), css_path = file.path("www", "style.css"), report_mode = FALSE) {
  saved_results_document(
    "EasyFlow Statistics ANCOVA Results",
    tags$div(class = "regression-results", ancova_results_ui(result, variable_table, labels)),
    max_width = 1500,
    css_path = css_path,
    report_mode = report_mode
  )
}

saved_nonparametric_results_html <- function(result, css_path = file.path("www", "style.css"), report_mode = FALSE) {
  saved_results_document(
    "EasyFlow Statistics Nonparametric Test Results",
    tags$div(class = "regression-results", ttest_anova_results_ui(result)),
    max_width = 1500,
    css_path = css_path,
    report_mode = report_mode
  )
}

saved_nonparametric_paired_results_html <- function(result, css_path = file.path("www", "style.css"), report_mode = FALSE) {
  saved_results_document(
    "EasyFlow Statistics Nonparametric Paired Test Results",
    tags$div(class = "regression-results", nonparametric_paired_results_ui(result)),
    max_width = 1500,
    css_path = css_path,
    print_landscape = identical(result$type, "nonparametric_paired_rm") || identical(result$type, "nonparametric_paired_combined"),
    report_mode = report_mode
  )
}

saved_paired_results_html <- function(result, css_path = file.path("www", "style.css"), report_mode = FALSE) {
  saved_results_document(
    "EasyFlow Statistics Paired Test Results",
    tags$div(class = "regression-results", paired_results_ui(result)),
    max_width = 1500,
    css_path = css_path,
    print_landscape = identical(result$type, "paired_rm") || identical(result$type, "paired_combined"),
    report_mode = report_mode
  )
}

saved_paired_rm_results_html <- function(result, css_path = file.path("www", "style.css"), report_mode = FALSE) {
  saved_results_document(
    "EasyFlow Statistics Paired Test 3+ Results",
    tags$div(class = "regression-results", paired_rm_results_ui(result)),
    max_width = 1500,
    css_path = css_path,
    print_landscape = TRUE,
    report_mode = report_mode
  )
}

saved_correlation_results_html <- function(result, css_path = file.path("www", "style.css"), report_mode = FALSE) {
  options <- result$options %||% list()
  normality_table <- correlation_normality_display_table(result)
  print_landscape <- length(result$variables %||% character(0)) >= 10L
  saved_results_document(
    "EasyFlow Statistics Correlation Results",
    tags$div(
      class = "correlation-results regression-results",
      correlation_matrix_set_ui(result),
      if (is.list(result$latent)) {
        correlation_matrix_set_ui(result, source = result$latent, title_prefix = "Latent-variable ")
      },
      if (isTRUE(options$normality) && is.data.frame(normality_table) && nrow(normality_table) > 0) {
        tags$div(
          class = "result-section correlation-result-section regression-result-panel",
          tags$h3("Normality"),
          coefficient_html_table(normality_table)
        )
      },
      if (isTRUE(options$scatter_plot)) {
        tags$div(
          class = "result-section correlation-result-section correlation-plot-section regression-result-panel",
          tags$h3("Scatter plot matrix"),
          tags$div(
            class = "correlation-plot-card",
            tags$img(
              src = plot_data_uri(draw_correlation_scatter_plot, result, width = 1480, height = 1480),
              width = "1480",
              height = "1480",
              alt = "Scatter plot matrix"
            )
          )
        )
      },
      if (isTRUE(options$matrix_plot)) {
        tags$div(
          class = "result-section correlation-result-section correlation-plot-section regression-result-panel",
          tags$h3("Correlation matrix heatmap"),
          tags$div(
            class = "correlation-plot-card",
            tags$img(
              src = plot_data_uri(draw_correlation_heatmap, result, width = 1360, height = 1360),
              width = "1360",
              height = "1360",
              alt = "Correlation matrix heatmap"
            )
          )
        )
      }
    ),
    max_width = 1500,
    css_path = css_path,
    print_landscape = print_landscape,
    report_mode = report_mode
  )
}

saved_logistic_results_html <- function(
  results,
  variable_table = NULL,
  labels = character(0),
  category_table = NULL,
  show_b = FALSE,
  show_se = FALSE,
  show_mcfadden = FALSE,
  show_cox_snell = FALSE,
  split_ci = FALSE,
  css_path = file.path("www", "style.css"),
  report_mode = FALSE
) {
  print_landscape <- any(vapply(logistic_result_groups(results), function(group) length(group) >= 3L, logical(1)))
  saved_results_document(
    "EasyFlow Statistics Logistic Regression Results",
    tags$div(
      class = "regression-results",
      logistic_results_panel(
        results,
        variable_table = variable_table,
        labels = labels,
        category_table = category_table,
        show_b = show_b,
        show_se = show_se,
        show_mcfadden = show_mcfadden,
        show_cox_snell = show_cox_snell,
        split_ci = split_ci
      )
    ),
    max_width = 1500,
    css_path = css_path,
    print_landscape = print_landscape,
    report_mode = report_mode
  )
}

saved_factor_analysis_results_html <- function(result, css_path = file.path("www", "style.css"), report_mode = FALSE) {
  saved_results_document(
    "EasyFlow Statistics Factor Analysis Results",
    factor_analysis_results_ui(result, report_mode = TRUE),
    max_width = 1500,
    css_path = css_path,
    report_mode = report_mode
  )
}

saved_pca_results_html <- function(result, css_path = file.path("www", "style.css"), report_mode = FALSE) {
  saved_results_document(
    "EasyFlow Statistics Principal Component Analysis Results",
    pca_results_ui(result, report_mode = TRUE),
    max_width = 1500,
    css_path = css_path,
    report_mode = report_mode
  )
}

saved_crosstab_results_html <- function(result, css_path = file.path("www", "style.css"), report_mode = FALSE) {
  saved_results_document(
    "EasyFlow Statistics Cross-tabulation Results",
    tags$div(class = "crosstab-results regression-results", crosstab_results_ui(result)),
    max_width = 1500,
    css_path = css_path,
    report_mode = report_mode
  )
}

result_accumulator_store <- function(session) {
  store <- session$userData$result_entries
  if (is.null(store) || !is.function(store)) {
    store <- reactiveVal(read_result_snapshot_store())
    session$userData$result_entries <- store
  }
  store
}

result_snapshot_store_path <- function() {
  configured <- trimws(Sys.getenv("EASYFLOW_RESULT_STORE", ""))
  if (nzchar(configured)) {
    return(configured)
  }
  file.path("data", "EasyFlow_Statistics_results.json")
}

normalize_result_snapshot_entry <- function(entry, index = 1L) {
  if (!is.list(entry)) {
    return(NULL)
  }
  title <- as.character(entry$title %||% "")
  saved_at <- as.character(entry$saved_at %||% "")
  html <- as.character(entry$html %||% "")
  if (!nzchar(html)) {
    return(NULL)
  }
  if (!nzchar(title)) {
    title <- "Analysis result"
  }
  if (!nzchar(saved_at)) {
    saved_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  }
  id <- as.character(entry$id %||% "")
  if (!nzchar(id)) {
    id <- paste0("saved_result_", index, "_", as.integer(Sys.time()))
  }
  list(id = id, title = title, saved_at = saved_at, html = html)
}

normalize_result_snapshot_entries <- function(entries) {
  if (is.data.frame(entries)) {
    entries <- lapply(seq_len(nrow(entries)), function(index) as.list(entries[index, , drop = FALSE]))
  }
  if (!is.list(entries) || length(entries) == 0) {
    return(list())
  }
  normalized <- lapply(seq_along(entries), function(index) normalize_result_snapshot_entry(entries[[index]], index))
  Filter(Negate(is.null), normalized)
}

read_result_snapshot_store <- function(path = result_snapshot_store_path()) {
  if (!file.exists(path)) {
    return(list())
  }
  payload <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
  type <- as.character(payload$type %||% "")
  if (nzchar(type) && !identical(type, "easyflow_result_history")) {
    stop("This file is not an EasyFlow Result file.", call. = FALSE)
  }
  entries <- if (is.list(payload) && !is.null(payload$entries)) payload$entries else payload
  normalize_result_snapshot_entries(entries)
}

write_result_snapshot_store <- function(entries, path = result_snapshot_store_path()) {
  entries <- normalize_result_snapshot_entries(entries)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  payload <- list(
    type = "easyflow_result_history",
    version = 1L,
    saved_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    entries = entries
  )
  tryCatch(
    {
      writeLines(as.character(jsonlite::toJSON(payload, pretty = TRUE, auto_unbox = TRUE)), path, useBytes = TRUE)
      TRUE
    },
    error = function(e) FALSE
  )
}

append_result_snapshot <- function(session, title, html) {
  store <- result_accumulator_store(session)
  entries <- isolate(store())
  index <- length(entries) + 1L
  saved_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  entry <- list(
    id = paste0("saved_result_", index, "_", as.integer(Sys.time())),
    title = title,
    saved_at = saved_at,
    html = html
  )
  updated <- c(entries, list(entry))
  store(updated)
  write_result_snapshot_store(updated)
  entry
}

saved_result_entry_ui <- function(entry, index) {
  div(
    class = "saved-result-entry",
    div(
      class = "saved-result-entry-header",
      div(
        class = "saved-result-title",
        span(sprintf("%02d", index), class = "saved-result-index"),
        span(entry$title)
      ),
      div(entry$saved_at, class = "saved-result-time")
    ),
    tags$iframe(
      class = "saved-result-frame",
      title = entry$title,
      srcdoc = entry$html
    )
  )
}

result_entry_document <- function(entry) {
  tryCatch(
    xml2::read_html(entry$html, options = c("RECOVER", "NOERROR", "NOWARNING")),
    error = function(e) NULL
  )
}

result_entry_body_html <- function(entry) {
  document <- result_entry_document(entry)
  if (is.null(document)) {
    return(entry$html)
  }
  body <- xml2::xml_find_first(document, ".//body")
  if (length(body) == 0 || is.na(xml2::xml_name(body))) {
    return(entry$html)
  }
  paste(vapply(xml2::xml_children(body), as.character, character(1)), collapse = "\n")
}

result_collection_content <- function(entries) {
  tags$div(
    class = "result-collection-export",
    lapply(seq_along(entries), function(index) {
      entry <- entries[[index]]
      tags$section(
        class = "result-collection-export-entry",
        tags$h2(sprintf("%02d. %s", index, entry$title)),
        tags$div(sprintf("Added to Result: %s", entry$saved_at), class = "saved-results-meta"),
        htmltools::HTML(result_entry_body_html(entry))
      )
    })
  )
}

saved_result_collection_html <- function(entries, css_path = file.path("www", "style.css"), report_mode = FALSE) {
  saved_results_document(
    "EasyFlow Statistics Result Collection",
    result_collection_content(entries),
    max_width = 1500,
    css_path = css_path,
    print_landscape = TRUE,
    report_mode = report_mode
  )
}

write_result_collection_html <- function(entries, file) {
  writeLines(saved_result_collection_html(entries), file, useBytes = TRUE)
  invisible(file)
}

write_result_collection_pdf <- function(entries, file) {
  write_pdf_from_html(saved_result_collection_html(entries, report_mode = TRUE), file)
}

result_html_text <- function(node) {
  text <- tryCatch(xml2::xml_text(node, trim = TRUE), error = function(e) "")
  trimws(gsub("\\s+", " ", text))
}

result_table_title <- function(table_node, fallback) {
  heading <- xml2::xml_find_first(
    table_node,
    "ancestor::*[contains(concat(' ', normalize-space(@class), ' '), ' regression-result-panel ')][1]//*[self::h1 or self::h2 or self::h3 or self::h4][1]"
  )
  if (length(heading) == 0 || is.na(xml2::xml_name(heading))) {
    heading <- xml2::xml_find_first(table_node, "preceding::*[self::h1 or self::h2 or self::h3 or self::h4][1]")
  }
  title <- if (length(heading) > 0 && !is.na(xml2::xml_name(heading))) result_html_text(heading) else ""
  if (nzchar(title)) title else fallback
}

result_html_table_cells <- function(table_node) {
  rows <- xml2::xml_find_all(table_node, ".//tr")
  if (length(rows) == 0) {
    return(NULL)
  }
  grid <- list()
  header_rows <- length(xml2::xml_find_all(table_node, "./thead/tr"))
  max_col <- 0L
  for (row_index in seq_along(rows)) {
    cells <- xml2::xml_find_all(rows[[row_index]], "./th|./td")
    if (length(cells) == 0) next
    if (length(grid) < row_index) {
      length(grid) <- row_index
    }
    if (is.null(grid[[row_index]])) {
      grid[[row_index]] <- character(0)
    }
    col_index <- 1L
    for (cell in cells) {
      while (
        length(grid[[row_index]]) >= col_index &&
          !is.na(grid[[row_index]][[col_index]]) &&
          nzchar(grid[[row_index]][[col_index]] %||% "")
      ) {
        col_index <- col_index + 1L
      }
      colspan <- suppressWarnings(as.integer(xml2::xml_attr(cell, "colspan") %||% "1"))
      rowspan <- suppressWarnings(as.integer(xml2::xml_attr(cell, "rowspan") %||% "1"))
      if (is.na(colspan) || colspan < 1L) colspan <- 1L
      if (is.na(rowspan) || rowspan < 1L) rowspan <- 1L
      value <- result_html_text(cell)
      for (row_offset in seq_len(rowspan) - 1L) {
        target_row <- row_index + row_offset
        if (length(grid) < target_row) {
          length(grid) <- target_row
        }
        if (is.null(grid[[target_row]])) {
          grid[[target_row]] <- character(0)
        }
        length(grid[[target_row]]) <- max(length(grid[[target_row]]), col_index + colspan - 1L)
        for (col_offset in seq_len(colspan) - 1L) {
          grid[[target_row]][[col_index + col_offset]] <- value
        }
      }
      max_col <- max(max_col, col_index + colspan - 1L)
      col_index <- col_index + colspan
    }
  }
  if (length(grid) == 0 || max_col == 0L) {
    return(NULL)
  }
  matrix_values <- matrix("", nrow = length(grid), ncol = max_col)
  for (row_index in seq_along(grid)) {
    row <- grid[[row_index]]
    if (length(row) > 0) {
      row[is.na(row)] <- ""
      matrix_values[row_index, seq_along(row)] <- row
    }
  }
  if (header_rows == 0L) {
    first_row <- xml2::xml_find_all(rows[[1]], "./th")
    header_rows <- if (length(first_row) > 0) 1L else 0L
  }
  list(values = matrix_values, header_rows = min(header_rows, nrow(matrix_values)))
}

result_docx_format_number <- function(value, digits, drop_zero = TRUE) {
  text <- trimws(as.character(value %||% ""))
  if (!nzchar(text)) {
    return(text)
  }
  marker <- ""
  marker_match <- regmatches(text, regexpr("\\s+[0-9]+$", text))
  if (length(marker_match) > 0 && nzchar(marker_match)) {
    marker <- marker_match
    text <- sub("\\s+[0-9]+$", "", text)
  }
  if (startsWith(text, "<")) {
    return(paste0(text, marker))
  }
  numeric_text <- sub("^<", "", text)
  number <- suppressWarnings(as.numeric(numeric_text))
  if (is.na(number)) {
    return(trimws(paste0(value)))
  }
  output <- sprintf(paste0("%.", digits, "f"), number)
  if (isTRUE(drop_zero)) {
    output <- sub("^-0\\.", "-.", output)
    output <- sub("^0\\.", ".", output)
  }
  paste0(output, marker)
}

result_docx_format_p <- function(value) {
  text <- trimws(as.character(value %||% ""))
  if (!nzchar(text)) {
    return(text)
  }
  marker <- ""
  marker_match <- regmatches(text, regexpr("\\s+[0-9]+$", text))
  if (length(marker_match) > 0 && nzchar(marker_match)) {
    marker <- marker_match
    text <- sub("\\s+[0-9]+$", "", text)
  }
  output <- format_p(text)
  if (is.na(output)) {
    return(trimws(paste0(value)))
  }
  paste0(output, marker)
}

result_docx_normalize_table <- function(table, headers = NULL) {
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(table)
  }
  header_labels <- names(table)
  if (!is.null(headers) && nrow(headers) > 0) {
    header_labels <- headers[nrow(headers), , drop = TRUE]
  }
  for (col_index in seq_along(table)) {
    label <- tolower(paste(header_labels[[col_index]] %||% "", collapse = " "))
    formatter <- NULL
    if (label %in% c("m", "sd", "median") || grepl("^q1|q3|q1~q3$", label)) {
      formatter <- function(value) result_docx_format_number(value, 2)
    } else if (label %in% c("p", "p for trend") || grepl("\\bp\\b", label)) {
      formatter <- result_docx_format_p
    } else if (label %in% c("t", "f", "t/f", "z", "w", "q") || grepl("statistic|hedges|cohen|effect|eta|omega|epsilon|cliff|f²|f2|sr2|r$", label)) {
      formatter <- function(value) result_docx_format_number(value, 3)
    }
    if (!is.null(formatter)) {
      table[[col_index]] <- vapply(table[[col_index]], formatter, character(1))
    }
  }
  table
}

result_docx_marker_column <- function(label) {
  label <- tolower(as.character(label %||% ""))
  label %in% c("p", "p for trend") ||
    grepl("effect|hedges|cohen|eta|omega|epsilon|cliff|trend|boot p", label)
}

result_docx_split_marker <- function(value, label) {
  text <- trimws(as.character(value %||% ""))
  if (!nzchar(text) || !result_docx_marker_column(label)) {
    return(c(value = text, marker = ""))
  }
  spaced <- regexec("^(.+?)\\s+([1-9][0-9]?)$", text, perl = TRUE)
  spaced_match <- regmatches(text, spaced)[[1]]
  if (length(spaced_match) == 3L) {
    return(c(value = trimws(spaced_match[[2]]), marker = spaced_match[[3]]))
  }
  compact <- regexec("^((?:<\\.001)|(?:-?(?:0)?\\.[0-9]{3,}))([1-9][0-9]?)$", text, perl = TRUE)
  compact_match <- regmatches(text, compact)[[1]]
  if (length(compact_match) == 3L) {
    return(c(value = compact_match[[2]], marker = compact_match[[3]]))
  }
  c(value = text, marker = "")
}

result_docx_split_header_marker <- function(value) {
  text <- trimws(as.character(value %||% ""))
  if (!nzchar(text) || !grepl("^Model\\s+[0-9]+\\s+[1-9][0-9]?$", text, perl = TRUE)) {
    return(c(value = text, marker = ""))
  }
  matched <- regexec("^(Model\\s+[0-9]+)\\s+([1-9][0-9]?)$", text, perl = TRUE)
  parts <- regmatches(text, matched)[[1]]
  if (length(parts) == 3L) {
    return(c(value = parts[[2]], marker = parts[[3]]))
  }
  c(value = text, marker = "")
}

result_docx_table_payload <- function(table_node) {
  parsed <- result_html_table_cells(table_node)
  if (is.null(parsed)) {
    return(NULL)
  }
  values <- parsed$values
  header_rows <- parsed$header_rows
  if (header_rows >= nrow(values)) {
    return(NULL)
  }
  body <- as.data.frame(values[(header_rows + 1L):nrow(values), , drop = FALSE], stringsAsFactors = FALSE)
  col_keys <- paste0("col", seq_len(ncol(body)))
  names(body) <- col_keys
  headers <- if (header_rows > 0) values[seq_len(header_rows), , drop = FALSE] else matrix(col_keys, nrow = 1)
  if (nrow(headers) > 1L) {
    for (col_index in seq_len(ncol(headers))) {
      for (row_index in seq_len(nrow(headers))) {
        if (!nzchar(headers[row_index, col_index])) {
          headers[row_index, col_index] <- if (row_index > 1L) headers[row_index - 1L, col_index] else ""
        }
      }
    }
  }
  header_marker_matrix <- matrix("", nrow = nrow(headers), ncol = ncol(headers))
  for (row_index in seq_len(nrow(headers))) {
    for (col_index in seq_len(ncol(headers))) {
      split <- result_docx_split_header_marker(headers[row_index, col_index])
      headers[row_index, col_index] <- split[["value"]]
      header_marker_matrix[row_index, col_index] <- split[["marker"]]
    }
  }
  marker_matrix <- matrix("", nrow = nrow(body), ncol = ncol(body))
  leaf_headers <- if (nrow(headers) > 0) headers[nrow(headers), ] else col_keys
  for (col_index in seq_len(ncol(body))) {
    splits <- lapply(body[[col_index]], result_docx_split_marker, label = leaf_headers[[col_index]])
    body[[col_index]] <- vapply(splits, `[[`, character(1), "value")
    marker_matrix[, col_index] <- vapply(splits, `[[`, character(1), "marker")
  }
  body <- result_docx_normalize_table(body, headers)
  list(body = body, headers = headers, col_keys = col_keys, markers = marker_matrix, header_markers = header_marker_matrix)
}

result_entry_tables <- function(entry, entry_index = 1L) {
  document <- result_entry_document(entry)
  if (is.null(document)) {
    return(list())
  }
  table_nodes <- xml2::xml_find_all(document, ".//table")
  if (length(table_nodes) == 0) {
    return(list())
  }
  tables <- vector("list", length(table_nodes))
  for (index in seq_along(table_nodes)) {
    table <- tryCatch(rvest::html_table(table_nodes[[index]], fill = TRUE, trim = TRUE), error = function(e) NULL)
    if (is.null(table) || !is.data.frame(table) || nrow(table) == 0 || ncol(table) == 0) {
      next
    }
    names(table) <- make.unique(ifelse(nzchar(names(table)), names(table), paste0("Column ", seq_along(table))))
    table[] <- lapply(table, as.character)
    title <- result_table_title(
      table_nodes[[index]],
      sprintf("%s table %s", entry$title, index)
    )
    note_nodes <- xml2::xml_find_all(
      table_nodes[[index]],
      "ancestor::*[contains(concat(' ', normalize-space(@class), ' '), ' result-table-with-note ')][1]//*[contains(concat(' ', normalize-space(@class), ' '), ' coefficient-note ') or contains(concat(' ', normalize-space(@class), ' '), ' coefficient-warning ')]"
    )
    notes <- vapply(note_nodes, result_html_text, character(1))
    notes <- notes[nzchar(notes)]
    context_class <- xml2::xml_attr(
      xml2::xml_find_first(table_nodes[[index]], "ancestor::div[contains(@class, 'result-section') or contains(@class, 'regression-result-panel')][1]"),
      "class"
    ) %||% ""
    tables[[index]] <- list(
      title = title,
      sheet_name = excel_sheet_name(sprintf("%02d %s", entry_index, title)),
      table = table,
      class = xml2::xml_attr(table_nodes[[index]], "class") %||% "",
      context_class = context_class,
      docx = result_docx_table_payload(table_nodes[[index]]),
      notes = notes
    )
  }
  Filter(Negate(is.null), tables)
}

result_docx_main_table <- function(table_info) {
  table_class <- as.character(table_info$class %||% "")
  context_class <- as.character(table_info$context_class %||% "")
  title <- tolower(as.character(table_info$title %||% ""))
  table_names <- tolower(names(table_info$table %||% data.frame()))
  if (all(c("variable", "method", "comparison", "p") %in% table_names)) {
    return(FALSE)
  }
  if (grepl("model-overview-panel|assumption-review-panel|ttest-anova-posthoc-section", context_class)) {
    return(FALSE)
  }
  if (grepl("combined-model-overview-table|compact-model-overview-table|effect-size-reference-table", table_class)) {
    return(FALSE)
  }
  auxiliary_title <- paste(
    "model overview",
    "normality",
    "p-value",
    "95% ci",
    "methods",
    "reason",
    "reference",
    "effect size guideline",
    "effect size guidelines",
    "post-hoc",
    "posthoc",
    "warnings",
    "skipped",
    "omitted",
    "\uac00\uc815 \uac80\ud1a0",
    sep = "|"
  )
  !grepl(auxiliary_title, title)
}

result_collection_index_table <- function(entries) {
  data.frame(
    No = seq_along(entries),
    Result = vapply(entries, function(entry) entry$title, character(1)),
    Added = vapply(entries, function(entry) entry$saved_at, character(1)),
    check.names = FALSE
  )
}

save_result_collection_excel_file <- function(entries, file) {
  workbook <- openxlsx::createWorkbook()
  used_sheets <- character(0)
  used_sheets <- add_excel_table_sheet(
    workbook,
    "Result index",
    result_collection_index_table(entries),
    used_sheets,
    title = "Result index"
  )
  for (entry_index in seq_along(entries)) {
    tables <- result_entry_tables(entries[[entry_index]], entry_index)
    for (table_info in tables) {
      used_sheets <- add_excel_table_sheet(
        workbook,
        table_info$sheet_name,
        table_info$table,
        used_sheets,
        title = sprintf("%02d. %s - %s", entry_index, entries[[entry_index]]$title, table_info$title)
      )
    }
  }
  openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
  invisible(file)
}

result_entry_images <- function(entry) {
  document <- result_entry_document(entry)
  if (is.null(document)) {
    return(list())
  }
  image_nodes <- xml2::xml_find_all(
    document,
    ".//*[contains(concat(' ', normalize-space(@class), ' '), ' residual-plot-card ') or contains(concat(' ', normalize-space(@class), ' '), ' frequency-plot-card ') or contains(concat(' ', normalize-space(@class), ' '), ' correlation-plot-card ')]//img[starts-with(@src, 'data:image/')]"
  )
  if (length(image_nodes) == 0) {
    return(list())
  }
  lapply(seq_along(image_nodes), function(index) {
    src <- xml2::xml_attr(image_nodes[[index]], "src") %||% ""
    mime <- sub("^data:([^;]+);base64,.*$", "\\1", src)
    payload <- sub("^data:[^;]+;base64,", "", src)
    extension <- switch(mime, "image/jpeg" = ".jpg", "image/webp" = ".webp", ".png")
    path <- tempfile("easyflow_result_image_", fileext = extension)
    writeBin(jsonlite::base64_dec(payload), path)
    alt <- xml2::xml_attr(image_nodes[[index]], "alt") %||% sprintf("Figure %s", index)
    width <- suppressWarnings(as.numeric(xml2::xml_attr(image_nodes[[index]], "width") %||% ""))
    height <- suppressWarnings(as.numeric(xml2::xml_attr(image_nodes[[index]], "height") %||% ""))
    list(path = path, title = alt, width_px = width, height_px = height)
  })
}

result_docx_page_spec <- function(landscape = FALSE) {
  width <- if (isTRUE(landscape)) 10.12 else 7.17
  height <- if (isTRUE(landscape)) 7.17 else 10.12
  table_width <- if (isTRUE(landscape)) 890 / 96 else 590 / 96
  margin <- max(0.25, (width - table_width) / 2)
  list(
    width = width,
    height = height,
    margin_left = margin,
    margin_right = margin,
    margin_top = margin,
    margin_bottom = margin,
    table_width = table_width
  )
}

result_docx_apply_b5_section <- function(document) {
  spec <- result_docx_page_spec()
  section <- officer::prop_section(
    page_size = officer::page_size(width = spec$width, height = spec$height, orient = "portrait"),
    page_margins = officer::page_mar(
      top = spec$margin_top,
      bottom = spec$margin_bottom,
      left = spec$margin_left,
      right = spec$margin_right,
      header = 0.3,
      footer = 0.3
    )
  )
  officer::body_set_default_section(document, section)
}

result_docx_landscape_section <- function() {
  spec <- result_docx_page_spec(TRUE)
  officer::block_section(
    officer::prop_section(
      page_size = officer::page_size(width = spec$width, height = spec$height, orient = "landscape"),
      page_margins = officer::page_mar(
        top = spec$margin_top,
        bottom = spec$margin_bottom,
        left = spec$margin_left,
        right = spec$margin_right,
        header = 0.3,
        footer = 0.3
      )
    )
  )
}

result_docx_portrait_section <- function() {
  spec <- result_docx_page_spec(FALSE)
  officer::block_section(
    officer::prop_section(
      page_size = officer::page_size(width = spec$width, height = spec$height, orient = "portrait"),
      page_margins = officer::page_mar(
        top = spec$margin_top,
        bottom = spec$margin_bottom,
        left = spec$margin_left,
        right = spec$margin_right,
        header = 0.3,
        footer = 0.3
      )
    )
  )
}

result_docx_wide_table <- function(table_info) {
  table_class <- as.character(table_info$class %||% "")
  context_class <- as.character(table_info$context_class %||% "")
  title <- as.character(table_info$title %||% "")
  is_correlation <- grepl("Correlation / association coefficients", title, fixed = TRUE)
  correlation_variables <- if (isTRUE(is_correlation) && is.data.frame(table_info$table)) max(0L, ncol(table_info$table) - 1L) else 0L
  grepl("landscape-table-panel", context_class) ||
    grepl("paired-rm-grouped-table", table_class) ||
    (isTRUE(is_correlation) && correlation_variables >= 10L)
}

result_docx_column_alignments <- function(table_info) {
  table <- table_info$docx$body %||% table_info$table
  n_cols <- if (is.data.frame(table)) ncol(table) else 0L
  if (n_cols == 0L) {
    return(character(0))
  }
  table_class <- as.character(table_info$class %||% "")
  alignments <- rep("right", n_cols)
  alignments[[1]] <- "left"
  headers <- table_info$docx$headers %||% NULL
  leaf_headers <- if (!is.null(headers) && nrow(headers) > 0) as.character(headers[nrow(headers), ]) else names(table)
  if (grepl("combined-model-overview-table|compact-model-overview-table", table_class)) {
    alignments[] <- "center"
    left_columns <- if ("Item" %in% names(table)) {
      if (identical(names(table)[[1]], "Item")) 1L else 2L
    } else {
      1L
    }
    alignments[seq_len(min(left_columns, n_cols))] <- "left"
  }
  if (grepl("crosstab-main-table", table_class)) {
    alignments[] <- "right"
    alignments[[1]] <- "left"
  }
  if (grepl("paired-grouped-table|hierarchical-coefficient-table", table_class)) {
    alignments[] <- "right"
    alignments[[1]] <- "left"
    text_columns <- grepl("variable|term|value|post", tolower(leaf_headers))
    alignments[text_columns] <- "left"
  }
  alignments
}

result_docx_column_widths <- function(table, table_width, table_info = NULL) {
  n_cols <- if (is.data.frame(table)) ncol(table) else 0L
  if (n_cols <= 0L) {
    return(numeric(0))
  }
  if (n_cols == 1L) {
    return(table_width)
  }
  table_class <- as.character(table_info$class %||% "")
  title <- tolower(as.character(table_info$title %||% ""))
  headers <- table_info$docx$headers %||% NULL
  leaf_headers <- if (!is.null(headers) && nrow(headers) > 0) tolower(as.character(headers[nrow(headers), ])) else tolower(names(table))
  if (grepl("frequencies / descriptives", title, fixed = TRUE)) {
    weights <- rep(0.54, n_cols)
    weights[grepl("variable", leaf_headers)] <- 1.15
    weights[grepl("value", leaf_headers)] <- 1.25
    weights[grepl("n\\(%\\)|m.*sd", leaf_headers)] <- 1.05
    weights[leaf_headers %in% c("n", "%", "m", "sd", "min", "max")] <- 0.42
    weights[grepl("median", leaf_headers)] <- 0.72
    weights[grepl("iqr", leaf_headers)] <- 1.25
    weights[grepl("skew|kurto", leaf_headers)] <- 0.64
    return(table_width * weights / sum(weights))
  }
  if (grepl("correlation / association coefficients", title, fixed = TRUE)) {
    weights <- rep(0.62, n_cols)
    weights[[1]] <- 0.78
    return(table_width * weights / sum(weights))
  }
  if (grepl("paired-two-grouped-table", table_class)) {
    weights <- rep(0.7, n_cols)
    weights[grepl("variable|value", leaf_headers)] <- 1.05
    weights[leaf_headers %in% c("m", "sd")] <- 0.48
    weights[leaf_headers %in% c("t", "f", "t/f", "p")] <- 0.55
    weights[grepl("hedges|cohen|effect", leaf_headers)] <- 0.75
    weights[grepl("post-hoc", leaf_headers)] <- 1.2
    return(table_width * weights / sum(weights))
  }
  if (grepl("paired-rm-grouped-table", table_class)) {
    weights <- rep(0.56, n_cols)
    weights[grepl("repeated", leaf_headers)] <- 1.25
    weights[leaf_headers %in% c("n", "m", "sd", "p", "f")] <- 0.46
    weights[grepl("overall|pre-|post|es", leaf_headers)] <- 0.62
    weights[grepl("post-hoc", leaf_headers)] <- 1.05
    return(table_width * weights / sum(weights))
  }
  if (grepl("hierarchical-coefficient-table", table_class)) {
    weights <- rep(0.52, n_cols)
    weights[grepl("term", leaf_headers)] <- 1.1
    weights[grepl("tolerance", leaf_headers)] <- 0.82
    weights[grepl("vif", leaf_headers)] <- 0.62
    weights[!nzchar(leaf_headers)] <- 0.08
    return(table_width * weights / sum(weights))
  }
  first_width <- min(1.2, max(0.85, table_width * 0.20))
  remaining <- max(0.4, table_width - first_width)
  widths <- c(first_width, rep(remaining / (n_cols - 1L), n_cols - 1L))
  pmax(widths, 0.28)
}

result_docx_regression_fit_summary <- function(text, n_cols) {
  text <- trimws(as.character(text %||% ""))
  if (!nzchar(text) || n_cols < 4L || !grepl("F\\(", text) || !grepl("R", text)) {
    return(NULL)
  }
  r2_match <- regexec("R.{0,8}=\\s*([^\\s]+)\\s*\\(([^\\)]+)\\)", text, perl = TRUE)
  r2_parts <- regmatches(text, r2_match)[[1]]
  f_match <- regexec("F\\([^\\)]*\\)\\s*=\\s*([^,]+),\\s*p\\s*([^\\s]+)", text, perl = TRUE)
  f_parts <- regmatches(text, f_match)[[1]]
  if (length(r2_parts) < 3L || length(f_parts) < 3L) {
    return(NULL)
  }
  values <- rep("", n_cols)
  replacement <- c("F", trimws(f_parts[[2]]), "p", trimws(f_parts[[3]]), "R\u00B2", trimws(r2_parts[[2]]), "adj. R\u00B2", trimws(r2_parts[[3]]))
  values[seq_len(min(length(values), length(replacement)))] <- replacement[seq_len(min(length(values), length(replacement)))]
  values
}

result_docx_prepare_body <- function(table, table_info) {
  if (!is.data.frame(table) || nrow(table) == 0L || ncol(table) == 0L) {
    return(table)
  }
  table[[1]] <- trimws(as.character(table[[1]]))
  table_class <- as.character(table_info$class %||% "")
  table
}

result_docx_table <- function(table_info) {
  spec <- result_docx_page_spec(result_docx_wide_table(table_info))
  payload <- table_info$docx %||% NULL
  table <- if (is.list(payload) && is.data.frame(payload$body)) payload$body else table_info$table
  table <- result_docx_prepare_body(table, table_info)
  border_dark <- officer::fp_border(color = "#1f2937", width = 1.25)
  border_light <- officer::fp_border(color = "#d7dde5", width = 0.5)
  ft <- flextable::flextable(table)
  if (is.list(payload) && !is.null(payload$headers) && nrow(payload$headers) > 0) {
    mapping <- data.frame(col_keys = names(table), stringsAsFactors = FALSE)
    for (row_index in seq_len(nrow(payload$headers))) {
      mapping[[paste0("header", row_index)]] <- as.character(payload$headers[row_index, ])
    }
    ft <- flextable::set_header_df(ft, mapping = mapping, key = "col_keys")
    ft <- flextable::merge_h(ft, part = "header")
    ft <- flextable::merge_v(ft, part = "header")
  }
  ft <- flextable::font(ft, fontname = "Arial", part = "all")
  base_size <- if (grepl("correlation / association coefficients", tolower(as.character(table_info$title %||% "")), fixed = TRUE)) 6.8 else 8.3
  ft <- flextable::fontsize(ft, size = base_size, part = "all")
  ft <- flextable::bold(ft, bold = FALSE, part = "all")
  ft <- flextable::padding(ft, padding.top = 2, padding.bottom = 2, padding.left = 4, padding.right = 4, part = "all")
  ft <- flextable::valign(ft, valign = "center", part = "all")
  ft <- flextable::border_remove(ft)
  ft <- flextable::hline_top(ft, border = border_dark, part = "header")
  if (is.list(payload) && !is.null(payload$headers) && nrow(payload$headers) >= 2L) {
    ft <- flextable::hline(ft, i = 1, border = border_dark, part = "header")
  }
  ft <- flextable::hline_bottom(ft, border = border_dark, part = "header")
  ft <- flextable::hline(ft, border = border_light, part = "body")
  ft <- flextable::hline_bottom(ft, border = border_dark, part = "body")
  if (is.list(payload) && !is.null(payload$markers) && any(nzchar(payload$markers))) {
    for (row_index in seq_len(nrow(payload$markers))) {
      for (col_index in seq_len(ncol(payload$markers))) {
        marker <- payload$markers[row_index, col_index]
        if (!nzchar(marker)) next
        value <- as.character(table[[col_index]][[row_index]] %||% "")
        ft <- flextable::compose(
          ft,
          i = row_index,
          j = col_index,
          value = flextable::as_paragraph(flextable::as_chunk(value), flextable::as_sup(marker)),
          part = "body"
        )
      }
    }
  }
  if (is.list(payload) && !is.null(payload$header_markers) && any(nzchar(payload$header_markers))) {
    for (row_index in seq_len(nrow(payload$header_markers))) {
      for (col_index in seq_len(ncol(payload$header_markers))) {
        marker <- payload$header_markers[row_index, col_index]
        if (!nzchar(marker)) next
        value <- as.character(payload$headers[row_index, col_index] %||% "")
        ft <- flextable::compose(
          ft,
          i = row_index,
          j = col_index,
          value = flextable::as_paragraph(flextable::as_chunk(value), flextable::as_sup(marker)),
          part = "header"
        )
      }
    }
  }
  if (grepl("coefficient-table", as.character(table_info$class %||% "")) && !grepl("paired-grouped-table|hierarchical-coefficient-table", as.character(table_info$class %||% ""))) {
    merge_rows <- which(apply(table, 1, function(row) {
      values <- unique(as.character(row[nzchar(as.character(row))]))
      length(values) == 1L && length(row) > 1L
    }))
    if (length(merge_rows) > 0) {
      ft <- flextable::merge_h(ft, i = merge_rows, part = "body")
    }
  }
  if (grepl("hierarchical-coefficient-table", as.character(table_info$class %||% ""))) {
    footer_rows <- which(vapply(table[[1]], function(label) {
      label <- trimws(as.character(label %||% ""))
      startsWith(label, "F(p)") ||
        startsWith(label, "R") ||
        startsWith(label, "Delta R") ||
        startsWith(label, intToUtf8(0x0394)) ||
        startsWith(label, "d(") ||
        startsWith(label, "z(p)") ||
        startsWith(label, intToUtf8(0x03c7)) ||
        startsWith(label, "x\u00B2") ||
        grepl("^x\\^?2", label, perl = TRUE)
    }, logical(1)))
    if (length(footer_rows) > 0) {
      ft <- flextable::merge_h(ft, i = footer_rows, part = "body")
    }
    f_rows <- which(trimws(as.character(table[[1]] %||% "")) %in% c("F(p)", "F"))
    if (length(f_rows) > 0) {
      ft <- flextable::border(ft, i = f_rows, border.top = border_dark, part = "body")
    }
  }
  if (grepl("coefficient-table", as.character(table_info$class %||% ""))) {
    f_rows <- which(trimws(as.character(table[[1]] %||% "")) %in% c("F(p)", "F"))
    if (length(f_rows) > 0) {
      ft <- flextable::border(ft, i = f_rows, border.top = border_dark, part = "body")
    }
  }
  alignments <- result_docx_column_alignments(table_info)
  for (col_index in seq_along(alignments)) {
    ft <- flextable::align(ft, j = col_index, align = alignments[[col_index]], part = "all")
  }
  ft <- flextable::align(ft, j = 1, align = "left", part = "body")
  if (grepl("coefficient-table", as.character(table_info$class %||% ""))) {
    footer_rows <- which(vapply(table[[1]], function(label) {
      label <- trimws(as.character(label %||% ""))
      startsWith(label, "F(p)") ||
        identical(label, "F") ||
        startsWith(label, "R") ||
        startsWith(label, "Delta R") ||
        startsWith(label, intToUtf8(0x0394)) ||
        startsWith(label, "d(") ||
        startsWith(label, "z(p)") ||
        startsWith(label, intToUtf8(0x03c7)) ||
        startsWith(label, "x\u00B2") ||
        grepl("^x\\^?2", label, perl = TRUE)
    }, logical(1)))
    if (length(footer_rows) > 0) {
      ft <- flextable::align(ft, i = footer_rows, align = "center", part = "body")
    }
  }
  if (is.list(payload) && !is.null(payload$headers) && nrow(payload$headers) >= 2L) {
    ft <- flextable::align(ft, i = 1, align = "center", part = "header")
    ft <- flextable::align(ft, i = 1, j = 1, align = "left", part = "header")
  }
  widths <- result_docx_column_widths(table, spec$table_width, table_info)
  ft <- flextable::width(ft, width = widths)
  table_prop_width <- if (grepl("correlation / association coefficients", tolower(as.character(table_info$title %||% "")), fixed = TRUE)) 1 else 0.98
  ft <- flextable::set_table_properties(ft, layout = "fixed", width = table_prop_width, align = "center")
  ft
}

result_docx_add_note <- function(document, note) {
  officer::body_add_fpar(
    document,
    officer::fpar(
      officer::ftext(
        note,
        officer::fp_text(font.size = 7.2, font.family = "Arial", color = "#52606d")
      )
    )
  )
}

result_docx_image_dimensions <- function(image, landscape = FALSE) {
  spec <- result_docx_page_spec(landscape)
  width_px <- suppressWarnings(as.numeric(image$width_px %||% NA_real_))
  height_px <- suppressWarnings(as.numeric(image$height_px %||% NA_real_))
  if (!is.finite(width_px) || width_px <= 0 || !is.finite(height_px) || height_px <= 0) {
    width_px <- 420
    height_px <- 420
  }
  width <- width_px / 96
  height <- height_px / 96
  scale <- min(1, spec$table_width / width, 3.25 / height, (spec$height - spec$margin_top - spec$margin_bottom - 0.7) / height)
  list(width = width * scale, height = height * scale)
}

result_docx_cover <- function(document, entries) {
  cover_text <- saved_results_cover_text()
  app_version <- saved_results_app_version()
  app_label <- if (nzchar(app_version)) sprintf("EasyFlow Statistics v%s", app_version) else "EasyFlow Statistics"
  logo_path <- file.path("www", "logo-horizontal.png")
  if (file.exists(logo_path)) {
    document <- officer::body_add_img(document, src = logo_path, width = 3.4, height = 1.03)
  }
  document <- officer::body_add_par(document, "", style = "Normal")
  document <- officer::body_add_par(document, "", style = "Normal")
  document <- officer::body_add_par(document, "STATISTICAL REPORT", style = "Normal")
  document <- officer::body_add_par(document, "EasyFlow Statistics Result Collection", style = "heading 1")
  document <- officer::body_add_par(document, "Generated analysis results prepared for review, documentation, and print output.", style = "Normal")
  document <- officer::body_add_par(document, "", style = "Normal")
  document <- officer::body_add_par(document, "", style = "Normal")
  document <- officer::body_add_par(document, "", style = "Normal")
  document <- officer::body_add_par(document, sprintf("Saved: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), style = "Normal")
  document <- officer::body_add_par(document, sprintf("Application: %s", app_label), style = "Normal")
  if (nzchar(cover_text$user)) {
    document <- officer::body_add_par(document, sprintf("Prepared by: %s", cover_text$user), style = "Normal")
  }
  if (nzchar(cover_text$organization)) {
    document <- officer::body_add_par(document, sprintf("Prepared for: %s", cover_text$organization), style = "Normal")
  }
  document <- officer::body_add_par(document, cover_text$footer, style = "Normal")
  officer::body_add_break(document, pos = "after")
}

result_docx_package_text <- function() {
  packages <- c("shiny", "dplyr", "ggplot2", "flextable", "officer", "sandwich", "lmtest", "boot")
  versions <- vapply(packages, function(package) {
    value <- tryCatch(as.character(utils::packageVersion(package)), error = function(e) "")
    if (nzchar(value)) sprintf("%s %s", package, value) else ""
  }, character(1))
  versions <- versions[nzchar(versions)]
  paste0(
    "All analyses were performed using EasyFlow Statistics. ",
    sprintf("The R statistical computing environment %s was used", getRversion()),
    if (length(versions) > 0) sprintf(", with packages including %s.", paste(versions, collapse = ", ")) else "."
  )
}

result_docx_method_sentence <- function(entry, tables) {
  title <- as.character(entry$title %||% "Analysis")
  notes <- unique(unlist(lapply(tables, function(table_info) as.character(table_info$notes %||% character(0))), use.names = FALSE))
  notes <- notes[nzchar(notes)]
  note_text <- if (length(notes) > 0) paste(notes, collapse = " ") else ""
  lowered <- tolower(title)
  if (grepl("hierarchical", lowered)) {
    method <- "위계적 회귀분석을 실시하였다."
  } else if (grepl("regression", lowered)) {
    method <- "회귀분석을 실시하였다."
  } else if (grepl("t-test|anova", lowered)) {
    method <- "t-test/ANOVA를 실시하였다."
  } else if (grepl("paired", lowered)) {
    method <- "대응표본 분석을 실시하였다."
  } else if (grepl("correlation", lowered)) {
    method <- "상관분석을 실시하였다."
  } else {
    method <- sprintf("%s 분석을 실시하였다.", title)
  }
  reason <- if (nzchar(note_text)) {
    sprintf(" 분석 결과표의 주석에 제시된 기준과 사유에 따라 실제 사용된 분석기법을 적용하였다: %s", note_text)
  } else {
    " 분석 결과표에 제시된 기준에 따라 실제 사용된 분석기법을 적용하였다."
  }
  paste0(method, reason)
}

result_docx_method_sentence <- function(entry, tables) {
  title <- as.character(entry$title %||% "Analysis")
  notes <- unique(unlist(lapply(tables, function(table_info) as.character(table_info$notes %||% character(0))), use.names = FALSE))
  notes <- notes[nzchar(notes)]
  note_text <- if (length(notes) > 0) paste(notes, collapse = " ") else ""
  lowered <- tolower(title)
  if (grepl("hierarchical", lowered)) {
    method <- "\uc704\uacc4\uc801 \ud68c\uadc0\ubd84\uc11d\uc744 \uc2e4\uc2dc\ud558\uc600\ub2e4."
  } else if (grepl("regression", lowered)) {
    method <- "\ud68c\uadc0\ubd84\uc11d\uc744 \uc2e4\uc2dc\ud558\uc600\ub2e4."
  } else if (grepl("t-test|anova", lowered)) {
    method <- "t-test/ANOVA\ub97c \uc2e4\uc2dc\ud558\uc600\ub2e4."
  } else if (grepl("paired", lowered)) {
    method <- "\ub300\uc751\ud45c\ubcf8 \ubd84\uc11d\uc744 \uc2e4\uc2dc\ud558\uc600\ub2e4."
  } else if (grepl("correlation", lowered)) {
    method <- "\uc0c1\uad00\ubd84\uc11d\uc744 \uc2e4\uc2dc\ud558\uc600\ub2e4."
  } else {
    method <- sprintf("%s \ubd84\uc11d\uc744 \uc2e4\uc2dc\ud558\uc600\ub2e4.", title)
  }
  reason <- if (nzchar(note_text)) {
    sprintf(" \ubd84\uc11d \uacb0\uacfc\ud45c\uc758 \uc8fc\uc11d\uc5d0 \uc81c\uc2dc\ub41c \uae30\uc900\uacfc \uc0ac\uc720\uc5d0 \ub530\ub77c \uc2e4\uc81c \uc0ac\uc6a9\ub41c \ubd84\uc11d\uae30\ubc95\uc744 \uc801\uc6a9\ud558\uc600\ub2e4: %s", note_text)
  } else {
    " \ubd84\uc11d \uacb0\uacfc\ud45c\uc5d0 \uc81c\uc2dc\ub41c \uae30\uc900\uc5d0 \ub530\ub77c \uc2e4\uc81c \uc0ac\uc6a9\ub41c \ubd84\uc11d\uae30\ubc95\uc744 \uc801\uc6a9\ud558\uc600\ub2e4."
  }
  paste0(method, reason)
}

result_docx_methods_page <- function(document, entries, entry_tables) {
  document <- officer::body_add_par(document, "Analysis Methods", style = "heading 1")
  for (entry_index in seq_along(entries)) {
    tables <- entry_tables[[entry_index]] %||% list()
    if (length(tables) == 0) next
    document <- officer::body_add_par(document, result_docx_method_sentence(entries[[entry_index]], tables), style = "Normal")
  }
  document <- officer::body_add_par(document, result_docx_package_text(), style = "Normal")
  officer::body_add_break(document, pos = "after")
}

write_result_collection_docx <- function(entries, file) {
  document <- result_docx_apply_b5_section(officer::read_docx())
  entry_tables <- lapply(seq_along(entries), function(entry_index) {
    Filter(result_docx_main_table, result_entry_tables(entries[[entry_index]], entry_index))
  })
  document <- result_docx_methods_page(document, entries, entry_tables)
  content_count <- 0L
  for (entry_index in seq_along(entries)) {
    entry <- entries[[entry_index]]
    tables <- entry_tables[[entry_index]] %||% list()
    for (table_info in tables) {
      wide_table <- result_docx_wide_table(table_info)
      if (content_count > 0L && !isTRUE(wide_table)) {
        document <- officer::body_add_break(document, pos = "after")
      }
      if (isTRUE(wide_table)) {
        document <- officer::body_end_block_section(document, value = result_docx_portrait_section())
      }
      document <- officer::body_add_par(document, table_info$title, style = "heading 2")
      table <- result_docx_table(table_info)
      document <- flextable::body_add_flextable(document, table)
      for (note in as.character(table_info$notes %||% character(0))) {
        if (nzchar(note)) {
          document <- result_docx_add_note(document, note)
        }
      }
      if (isTRUE(wide_table)) {
        document <- officer::body_end_block_section(document, value = result_docx_landscape_section())
      }
      content_count <- content_count + 1L
    }
    images <- result_entry_images(entry)
    image_count <- 0L
    for (image in images) {
      if (file.exists(image$path)) {
        if (content_count > 0L && image_count %% 2L == 0L) {
          document <- officer::body_add_break(document, pos = "after")
        }
        dimensions <- result_docx_image_dimensions(image)
        document <- officer::body_add_par(document, image$title, style = "heading 2")
        document <- officer::body_add_img(document, src = image$path, width = dimensions$width, height = dimensions$height)
        image_count <- image_count + 1L
        content_count <- content_count + 1L
      }
    }
    unlink(vapply(images, `[[`, character(1), "path"))
  }
  print(document, target = file)
  invisible(file)
}

saved_results_empty_ui <- function() {
  div(class = "empty-message", div("Click Add result after an analysis to collect results here."))
}

result_entries_for_export <- function(store) {
  entries <- isolate(store())
  if (length(entries) == 0) {
    stop("No saved results are available. Click Add result after an analysis first.", call. = FALSE)
  }
  entries
}

register_result_accumulator_outputs <- function(input, output, session) {
  store <- result_accumulator_store(session)

  output$saved_results_list <- renderUI({
    entries <- store()
    if (length(entries) == 0) {
      return(saved_results_empty_ui())
    }
    tagList(
      div(class = "saved-result-count", sprintf("%s result(s) saved in Result history.", length(entries))),
      div(
        class = "saved-result-list",
        lapply(seq_along(entries), function(index) saved_result_entry_ui(entries[[index]], index))
      )
    )
  })

  observeEvent(input$clear_saved_results, {
    store(list())
    write_result_snapshot_store(list())
    showNotification("Saved results cleared.", type = "message", duration = 3)
  }, ignoreInit = TRUE)

  observeEvent(input$save_result_history_dialog, {
    tryCatch(
      {
        entries <- result_entries_for_export(store)
        path <- choose_result_history_save_path()
        if (length(path) == 0 || !nzchar(path[[1]])) {
          showNotification("Save dialog was not available or was canceled.", type = "warning", duration = 5)
          return(invisible(NULL))
        }
        if (!grepl("\\.(efs-result|json)$", path, ignore.case = TRUE)) {
          path <- paste0(path, ".efs-result")
        }
        if (!isTRUE(write_result_snapshot_store(entries, path))) {
          stop("Could not write the Result file.", call. = FALSE)
        }
        showNotification(sprintf("Result saved: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save Result:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE)

  observeEvent(input$open_result_history_dialog, {
    tryCatch(
      {
        path <- choose_result_history_open_path()
        if (length(path) == 0 || !nzchar(path[[1]])) {
          showNotification("Open dialog was not available or was canceled.", type = "warning", duration = 5)
          return(invisible(NULL))
        }
        entries <- read_result_snapshot_store(path)
        if (length(entries) == 0) {
          stop("The selected file does not contain saved Result entries.", call. = FALSE)
        }
        store(entries)
        write_result_snapshot_store(entries)
        showNotification(sprintf("Result opened: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to open Result:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE)

  observeEvent(input$save_result_collection_html_dialog, {
    tryCatch(
      {
        entries <- result_entries_for_export(store)
        path <- choose_html_save_path()
        if (length(path) == 0 || !nzchar(path[[1]])) {
          showNotification("Save dialog was not available or was canceled.", type = "warning", duration = 5)
          return(invisible(NULL))
        }
        if (!grepl("\\.html?$", path, ignore.case = TRUE)) {
          path <- paste0(path, ".html")
        }
        write_result_collection_html(entries, path)
        showNotification(sprintf("Result collection saved: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save Result collection:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE)

  observeEvent(input$save_result_collection_pdf_dialog, {
    tryCatch(
      {
        entries <- result_entries_for_export(store)
        path <- choose_pdf_save_path()
        if (length(path) == 0 || !nzchar(path[[1]])) {
          showNotification("Save dialog was not available or was canceled.", type = "warning", duration = 5)
          return(invisible(NULL))
        }
        if (!grepl("\\.pdf$", path, ignore.case = TRUE)) {
          path <- paste0(path, ".pdf")
        }
        write_result_collection_pdf(entries, path)
        showNotification(sprintf("Result collection saved: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save Result collection:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE)

  observeEvent(input$save_result_collection_excel_dialog, {
    tryCatch(
      {
        entries <- result_entries_for_export(store)
        path <- choose_excel_save_path()
        if (length(path) == 0 || !nzchar(path[[1]])) {
          showNotification("Save dialog was not available or was canceled.", type = "warning", duration = 5)
          return(invisible(NULL))
        }
        if (!grepl("\\.xlsx$", path, ignore.case = TRUE)) {
          path <- paste0(path, ".xlsx")
        }
        save_result_collection_excel_file(entries, path)
        showNotification(sprintf("Result collection saved: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save Result collection:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE)

  observeEvent(input$save_result_collection_word_dialog, {
    tryCatch(
      {
        entries <- result_entries_for_export(store)
        path <- choose_word_save_path()
        if (length(path) == 0 || !nzchar(path[[1]])) {
          showNotification("Save dialog was not available or was canceled.", type = "warning", duration = 5)
          return(invisible(NULL))
        }
        if (!grepl("\\.docx$", path, ignore.case = TRUE)) {
          path <- paste0(path, ".docx")
        }
        write_result_collection_docx(entries, path)
        showNotification(sprintf("Result collection saved: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save Result collection:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE)

  invisible(TRUE)
}

result_snapshot_document_html <- function(title, html) {
  saved_results_document(
    title,
    htmltools::HTML(html),
    max_width = 1500,
    css_path = file.path("www", "style.css")
  )
}

register_add_result_snapshot <- function(input, session, button_id, title, output_id = NULL, html_fn = NULL) {
  if (is.null(button_id) || !nzchar(button_id)) {
    return(invisible(FALSE))
  }

  if (is.function(output_id) && is.null(html_fn)) {
    html_fn <- output_id
    output_id <- NULL
  }

  snapshot_input_id <- paste0(button_id, "_snapshot")

  add_snapshot <- function(html) {
    if (length(html) == 0 || is.null(html) || !nzchar(as.character(html)[[1]])) {
      stop("No analysis result is available to add.")
    }
    resolved_title <- if (is.function(title)) title() else title
    resolved_title <- as.character(resolved_title %||% "")
    if (!nzchar(resolved_title)) {
      resolved_title <- "Analysis result"
    }
    append_result_snapshot(session, resolved_title, as.character(html)[[1]])
    updateNavbarPage(session, "main_menu", selected = "result")
    showNotification(sprintf("Added to Result: %s", resolved_title), type = "message", duration = 3)
  }

  observeEvent(input[[button_id]], {
    tryCatch(
      {
        if (!is.null(output_id) && nzchar(output_id)) {
          session$sendCustomMessage(
            "easyflow-capture-result-snapshot",
            list(
              outputId = output_id,
              inputId = snapshot_input_id,
              nonce = as.numeric(Sys.time())
            )
          )
          return(invisible(NULL))
        }
        if (!is.function(html_fn)) {
          stop("No result output is registered for Add result.")
        }
        add_snapshot(html_fn())
      },
      error = function(e) {
        showNotification(paste("Failed to add result:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE)

  observeEvent(input[[snapshot_input_id]], {
    tryCatch(
      {
        payload <- input[[snapshot_input_id]]
        error <- as.character(payload$error %||% "")
        if (nzchar(error)) {
          stop(error)
        }
        fragment <- as.character(payload$html %||% "")
        resolved_title <- if (is.function(title)) title() else title
        resolved_title <- as.character(resolved_title %||% "")
        if (!nzchar(resolved_title)) {
          resolved_title <- "Analysis result"
        }
        add_snapshot(result_snapshot_document_html(resolved_title, fragment))
      },
      error = function(e) {
        showNotification(paste("Failed to add result:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE)

  invisible(TRUE)
}
