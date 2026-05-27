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
    ".regression-result-panel h3 { color: #15233a; font-size: 22px; font-weight: 700; margin: 0 0 12px; }",
    ".regression-result-panel table { width: auto; min-width: 440px; border-collapse: collapse !important; border-spacing: 0 !important; border-top: 2px solid #1f2937 !important; border-bottom: 2px solid #1f2937 !important; color: #2f3a46; font-size: 16px; background: transparent; }",
    ".regression-result-panel table th, .regression-result-panel table td { padding: 9px 16px; line-height: 1.45; border-left: 0 !important; border-right: 0 !important; border-bottom: 1px solid #d7dde5; vertical-align: middle; background: transparent; }",
    ".regression-result-panel table thead th { border-bottom: 2px solid #1f2937 !important; font-weight: 700; }",
    ".regression-result-panel table tbody tr:last-child td, .regression-result-panel table tbody tr:last-child th { border-bottom: 0 !important; }",
    ".regression-result-panel table th:first-child, .regression-result-panel table td:first-child { text-align: left !important; }",
    ".regression-result-panel table th:not(:first-child), .regression-result-panel table td:not(:first-child) { text-align: right !important; }",
    ".coefficient-table { table-layout: auto; }",
    ".coefficient-table th:first-child, .coefficient-table td:first-child { text-align: left !important; }",
    ".coefficient-table th:not(:first-child) { text-align: right !important; }",
    ".coefficient-table thead th { border-top: 2px solid #1f2937 !important; border-bottom: 2px solid #1f2937 !important; font-weight: 700; }",
    ".coefficient-table tbody td:not(:first-child), .coefficient-table tfoot td:not(:first-child) { text-align: right !important; }",
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
    "  @page { size: A4 portrait; margin: 12mm; @bottom-right { content: counter(page) '/' counter(pages); color: #627d98; font-size: 8pt; } }",
    "  @page easyflow-landscape { size: A4 landscape; margin: 10mm; @bottom-right { content: counter(page) '/' counter(pages); color: #627d98; font-size: 8pt; } }",
    "  * { box-sizing: border-box; }",
    "  body { margin: 0 !important; color: #000000; font-size: 10.5pt; }",
    "  .page-shell { width: 100%; max-width: 186mm !important; margin: 0 auto !important; padding: 0 !important; }",
    "  body.print-mixed-landscape .page-shell { max-width: 100% !important; }",
    "  h1 { font-size: 18pt; margin: 0 0 8pt; }",
    "  .report-cover { min-height: 245mm; margin: 0 !important; padding: 16mm 14mm 12mm !important; border: 0 !important; border-radius: 0 !important; box-shadow: none !important; break-after: page; page-break-after: always; }",
    "  .report-cover::before { width: 3mm !important; }",
    "  .report-cover-logo { width: 72mm !important; max-width: 72mm !important; }",
    "  .report-cover-main { padding-top: 36mm !important; }",
    "  .report-cover-title { font-size: 28pt !important; margin: 4mm 0 5mm !important; }",
    "  .report-cover-subtitle { font-size: 11pt !important; }",
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
    "  .regression-result-panel, .result-section.regression-result-panel { width: 100% !important; max-width: 100% !important; overflow: visible !important; padding: 8pt 0 !important; border-left: 0 !important; border-right: 0 !important; border-radius: 0 !important; break-inside: auto; page-break-inside: auto; }",
    "  .regression-results > .regression-result-panel:has(table) { break-before: page; page-break-before: always; }",
    "  body.print-mixed-landscape .regression-results > .landscape-table-panel { page: easyflow-landscape; width: 100% !important; max-width: 277mm !important; margin-left: auto !important; margin-right: auto !important; break-before: page; page-break-before: always; break-after: page; page-break-after: always; }",
    "  .regression-results > .regression-result-panel:first-child { break-before: auto; page-break-before: auto; }",
    "  .regression-results > .diagnostic-plots-section, .regression-results > .frequency-plots-section, .regression-results > .correlation-plot-section { break-before: auto !important; page-break-before: auto !important; break-inside: auto !important; page-break-inside: auto !important; }",
    "  .regression-result-panel h3 { font-size: 13pt; margin: 0 0 6pt; }",
    "  .result-table-with-note, .frequency-table-wrap, .hierarchical-table-wrap, .hierarchical-table-scroll { display: block !important; width: 100% !important; max-width: 100% !important; overflow: visible !important; }",
    "  table, .regression-result-panel table, .coefficient-table, .hierarchical-coefficient-table { width: 100% !important; min-width: 0 !important; max-width: 100% !important; table-layout: fixed !important; font-size: 8.2pt !important; }",
    "  .coefficient-table .coefficient-col-term { width: 36% !important; }",
    "  .coefficient-table .coefficient-col-b { width: 11% !important; }",
    "  .coefficient-table .coefficient-col-reference { width: 13% !important; }",
    "  .coefficient-table .coefficient-col-tolerance { width: 9% !important; }",
    "  .coefficient-table .coefficient-col-compact { width: 5% !important; }",
    "  .coefficient-table .coefficient-col-stat { width: 7% !important; }",
    "  body.print-mixed-landscape .coefficient-table .coefficient-col-term { width: 46% !important; }",
    "  body.print-mixed-landscape .coefficient-table .coefficient-col-b { width: 12% !important; }",
    "  body.print-mixed-landscape .coefficient-table .coefficient-col-reference { width: 14% !important; }",
    "  body.print-mixed-landscape .coefficient-table .coefficient-col-tolerance { width: 9% !important; }",
    "  body.print-mixed-landscape .coefficient-table .coefficient-col-compact { width: 4.6% !important; }",
    "  body.print-mixed-landscape .coefficient-table .coefficient-col-stat { width: 6% !important; }",
    "  .hierarchical-coefficient-table .hierarchical-term-col { width: 30% !important; }",
    "  .hierarchical-coefficient-table .hierarchical-stat-col { width: 62px !important; }",
    "  .hierarchical-coefficient-table .hierarchical-separator-col { width: 6px !important; }",
    "  body.print-mixed-landscape .hierarchical-coefficient-table .hierarchical-term-col { width: 34% !important; }",
    "  body.print-mixed-landscape .hierarchical-coefficient-table .hierarchical-stat-col { width: 58px !important; }",
    "  .hierarchical-coefficient-table, .correlation-result-section table { font-size: 7.2pt !important; }",
    "  thead { display: table-header-group; }",
    "  tfoot { display: table-footer-group; }",
    "  tr { break-inside: avoid; page-break-inside: avoid; }",
    "  th, td, .regression-result-panel table th, .regression-result-panel table td { padding: 4pt 5pt !important; white-space: normal !important; overflow-wrap: anywhere !important; word-break: normal !important; }",
    "  .coefficient-table th { white-space: nowrap !important; }",
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
    ".regression-result-panel h3 { color: #15233a; font-size: 22px; font-weight: 700; margin: 0 0 12px; }",
    ".regression-result-panel table, .coefficient-table { width: auto; min-width: 480px; border-collapse: collapse !important; border-spacing: 0 !important; border-top: 2px solid #1f2937 !important; border-bottom: 2px solid #1f2937 !important; color: #2f3a46; font-size: 15px; background: transparent; }",
    ".regression-result-panel table th, .regression-result-panel table td { padding: 9px 16px; line-height: 1.45; border-left: 0 !important; border-right: 0 !important; border-bottom: 1px solid #d7dde5; vertical-align: middle; background: transparent; white-space: nowrap; }",
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
      div(class = "report-cover-meta-item", span("Application", class = "report-cover-meta-label"), span("EasyFlow Statistics", class = "report-cover-meta-value"))
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
    print_landscape = TRUE,
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
  saved_results_document(
    "EasyFlow Statistics t-test / ANOVA Results",
    tags$div(class = "regression-results", ttest_anova_results_ui(result)),
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
    report_mode = report_mode
  )
}

saved_paired_results_html <- function(result, css_path = file.path("www", "style.css"), report_mode = FALSE) {
  saved_results_document(
    "EasyFlow Statistics Paired Test Results",
    tags$div(class = "regression-results", paired_results_ui(result)),
    max_width = 1500,
    css_path = css_path,
    report_mode = report_mode
  )
}

saved_paired_rm_results_html <- function(result, css_path = file.path("www", "style.css"), report_mode = FALSE) {
  saved_results_document(
    "EasyFlow Statistics Paired Test 3+ Results",
    tags$div(class = "regression-results", paired_rm_results_ui(result)),
    max_width = 1500,
    css_path = css_path,
    report_mode = report_mode
  )
}

saved_correlation_results_html <- function(result, css_path = file.path("www", "style.css"), report_mode = FALSE) {
  options <- result$options %||% list()
  normality_table <- correlation_normality_display_table(result)
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
    print_landscape = TRUE,
    report_mode = report_mode
  )
}

saved_factor_analysis_results_html <- function(result, css_path = file.path("www", "style.css"), report_mode = FALSE) {
  saved_results_document(
    "EasyFlow Statistics Factor Analysis Results",
    factor_analysis_results_ui(result, report_mode = TRUE),
    max_width = 1500,
    css_path = css_path,
    print_landscape = TRUE,
    report_mode = report_mode
  )
}

saved_pca_results_html <- function(result, css_path = file.path("www", "style.css"), report_mode = FALSE) {
  saved_results_document(
    "EasyFlow Statistics Principal Component Analysis Results",
    pca_results_ui(result, report_mode = TRUE),
    max_width = 1500,
    css_path = css_path,
    print_landscape = TRUE,
    report_mode = report_mode
  )
}

saved_crosstab_results_html <- function(result, css_path = file.path("www", "style.css"), report_mode = FALSE) {
  saved_results_document(
    "EasyFlow Statistics Cross-tabulation Results",
    tags$div(class = "crosstab-results regression-results", crosstab_results_ui(result)),
    max_width = 1500,
    css_path = css_path,
    print_landscape = TRUE,
    report_mode = report_mode
  )
}

result_accumulator_store <- function(session) {
  store <- session$userData$result_entries
  if (is.null(store) || !is.function(store)) {
    store <- reactiveVal(list())
    session$userData$result_entries <- store
  }
  store
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
  store(c(entries, list(entry)))
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
    tables[[index]] <- list(
      title = title,
      sheet_name = excel_sheet_name(sprintf("%02d %s", entry_index, title)),
      table = table
    )
  }
  Filter(Negate(is.null), tables)
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
  image_nodes <- xml2::xml_find_all(document, ".//img[starts-with(@src, 'data:image/')]")
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
    list(path = path, title = alt)
  })
}

write_result_collection_docx <- function(entries, file) {
  document <- officer::read_docx()
  document <- officer::body_add_par(document, "EasyFlow Statistics Result Collection", style = "heading 1")
  document <- officer::body_add_par(document, sprintf("Saved: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), style = "Normal")
  for (entry_index in seq_along(entries)) {
    entry <- entries[[entry_index]]
    document <- officer::body_add_par(document, sprintf("%02d. %s", entry_index, entry$title), style = "heading 1")
    document <- officer::body_add_par(document, sprintf("Added to Result: %s", entry$saved_at), style = "Normal")
    tables <- result_entry_tables(entry, entry_index)
    for (table_info in tables) {
      document <- officer::body_add_par(document, table_info$title, style = "heading 2")
      table <- flextable::flextable(table_info$table)
      table <- flextable::autofit(table)
      document <- flextable::body_add_flextable(document, table)
    }
    images <- result_entry_images(entry)
    for (image in images) {
      if (file.exists(image$path)) {
        document <- officer::body_add_par(document, image$title, style = "heading 2")
        document <- officer::body_add_img(document, src = image$path, width = 6.5, height = 4.5)
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
      div(class = "saved-result-count", sprintf("%s result(s) saved in this session.", length(entries))),
      div(
        class = "saved-result-list",
        lapply(seq_along(entries), function(index) saved_result_entry_ui(entries[[index]], index))
      )
    )
  })

  observeEvent(input$clear_saved_results, {
    store(list())
    showNotification("Saved results cleared.", type = "message", duration = 3)
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

register_add_result_snapshot <- function(input, session, button_id, title, html_fn) {
  if (is.null(button_id) || !nzchar(button_id)) {
    return(invisible(FALSE))
  }
  observeEvent(input[[button_id]], {
    tryCatch(
      {
        html <- html_fn()
        if (length(html) == 0 || is.null(html) || !nzchar(as.character(html)[[1]])) {
          stop("No analysis result is available to add.")
        }
        append_result_snapshot(session, title, as.character(html)[[1]])
        updateNavbarPage(session, "main_menu", selected = "result")
        showNotification(sprintf("Added to Result: %s", title), type = "message", duration = 3)
      },
      error = function(e) {
        showNotification(paste("Failed to add result:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE)
  invisible(TRUE)
}
