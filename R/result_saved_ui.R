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
    tolower(Sys.getenv("STATEDU_EDITION", "development"))
  }
  if (!edition %in% c("free", "pro", "development", "personal", "institution")) {
    edition <- "development"
  }
  organization <- trimws(Sys.getenv("STATEDU_REPORT_ORGANIZATION", ""))
  user <- trimws(Sys.getenv("STATEDU_REPORT_USER", ""))
  organization_logo <- trimws(Sys.getenv("STATEDU_REPORT_ORGANIZATION_LOGO", ""))

  if (identical(edition, "free")) {
    organization <- ""
    user <- ""
    organization_logo <- file.path("www", "statedu_logo.png")
  } else if (identical(edition, "development")) {
    organization <- if (nzchar(organization)) organization else "statedu.com"
    user <- if (nzchar(user)) user else "StatEdu, Institute of Statistics"
    organization_logo <- if (nzchar(organization_logo)) organization_logo else file.path("www", "statedu_logo.png")
  } else if (identical(edition, "personal") || identical(edition, "pro")) {
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
    footer = trimws(Sys.getenv("STATEDU_REPORT_FOOTER", "Prepared with StatEdu Studio"))
  )
}

saved_results_app_version <- function(version_file = "VERSION") {
  version <- trimws(Sys.getenv("STATEDU_VERSION", ""))
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
          tags$img(src = logo_uri, class = "report-watermark-logo report-watermark-logo-efs", alt = "StatEdu Studio logo")
        } else {
          span("StatEdu Studio", class = "report-watermark-name")
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
    ".logistic-results > .result-section.regression-result-panel:not(.landscape-table-panel):not(.logistic-diagnostics-panel) { width: min(100%, 590px); max-width: 100%; overflow-x: hidden; box-sizing: border-box; }",
    ".logistic-results > .result-section.regression-result-panel:not(.landscape-table-panel):not(.logistic-diagnostics-panel) > table, .logistic-results > .result-section.regression-result-panel:not(.landscape-table-panel):not(.logistic-diagnostics-panel) .result-table-with-note > table, .logistic-results > .result-section.regression-result-panel:not(.landscape-table-panel):not(.logistic-diagnostics-panel) .logistic-result-table { width: 100% !important; min-width: 0 !important; max-width: 100% !important; table-layout: fixed; }",
    ".result-section.regression-result-panel:not(.landscape-table-panel):not(.paired-result-section):not(.logistic-diagnostics-panel) > .result-table-with-note, .result-section.regression-result-panel:not(.landscape-table-panel):not(.paired-result-section):not(.logistic-diagnostics-panel) > .frequency-table-wrap, .result-section.regression-result-panel:not(.landscape-table-panel):not(.paired-result-section):not(.logistic-diagnostics-panel) > table, .result-section.regression-result-panel:not(.landscape-table-panel):not(.paired-result-section):not(.logistic-diagnostics-panel) .result-table-with-note > table { width: 100% !important; max-width: 100%; }",
    ".result-section.regression-result-panel:not(.landscape-table-panel):not(.paired-result-section):not(.logistic-diagnostics-panel) table { width: 100% !important; min-width: 0 !important; max-width: 100% !important; table-layout: fixed; }",
    ".result-section.regression-result-panel.landscape-table-panel { width: min(100%, 890px); }",
    ".result-section.regression-result-panel.landscape-table-panel > .result-table-with-note, .result-section.regression-result-panel.landscape-table-panel > .hierarchical-table-wrap, .result-section.regression-result-panel.landscape-table-panel > .hierarchical-table-scroll, .result-section.regression-result-panel.landscape-table-panel > table, .result-section.regression-result-panel.landscape-table-panel .result-table-with-note > table { width: 100% !important; max-width: 100%; }",
    ".result-section.regression-result-panel.landscape-table-panel table { width: 100% !important; min-width: 0 !important; max-width: 100% !important; table-layout: fixed; }",
    ".result-section.regression-result-panel.ttest-anova-overview-panel, .result-section.regression-result-panel.ttest-anova-assumption-review-panel { width: 100% !important; max-width: 100% !important; overflow-x: hidden !important; }",
    ".result-section.regression-result-panel.ttest-anova-overview-panel > .result-table-with-note, .result-section.regression-result-panel.ttest-anova-assumption-review-panel > .result-table-with-note, .result-section.regression-result-panel.ttest-anova-overview-panel .result-table-with-note > table, .result-section.regression-result-panel.ttest-anova-assumption-review-panel .result-table-with-note > table { width: 100% !important; max-width: 100% !important; min-width: 0 !important; }",
    ".regression-results > .ttest-anova-result-panel { overflow-x: hidden !important; }",
    ".regression-results > .ttest-anova-result-panel > .result-table-with-note { width: 100% !important; max-width: 100% !important; overflow-x: hidden !important; }",
    ".regression-results > .ttest-anova-result-panel .result-table-with-note > table, .regression-results > .ttest-anova-result-panel .coefficient-table-trend-analysis { width: 100% !important; min-width: 0 !important; max-width: 100% !important; table-layout: fixed !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df .coefficient-col-statistic { width: 112px !important; min-width: 112px !important; padding-right: 12px !important; text-align: right !important; white-space: nowrap !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df .coefficient-col-p { padding-left: 12px !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df:not(.coefficient-table-mean-sd) .coefficient-col-stat { width: 44px !important; min-width: 44px !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df:not(.coefficient-table-mean-sd) .coefficient-col-statistic { width: 144px !important; min-width: 144px !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df.coefficient-table-trend-analysis .coefficient-col-term { width: 82px !important; min-width: 82px !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df.coefficient-table-trend-analysis.coefficient-table-mean-sd .coefficient-col-value { width: 112px !important; min-width: 112px !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df.coefficient-table-trend-analysis:not(.coefficient-table-mean-sd) .coefficient-col-value { width: 104px !important; min-width: 104px !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df.coefficient-table-trend-analysis.coefficient-table-mean-sd .coefficient-col-mse { width: 116px !important; min-width: 116px !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df.coefficient-table-trend-analysis.coefficient-table-mean-sd .coefficient-col-statistic { width: 144px !important; min-width: 144px !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df.coefficient-table-trend-analysis:not(.coefficient-table-mean-sd) .coefficient-col-stat { width: 46px !important; min-width: 46px !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df.coefficient-table-trend-analysis:not(.coefficient-table-mean-sd) .coefficient-col-p, .regression-results > .ttest-anova-result-panel .coefficient-table-show-df.coefficient-table-trend-analysis:not(.coefficient-table-mean-sd) .coefficient-col-effect-size { width: 50px !important; min-width: 50px !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df.coefficient-table-trend-analysis:not(.coefficient-table-mean-sd) .coefficient-col-statistic { width: 158px !important; min-width: 158px !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df.coefficient-table-trend-analysis.coefficient-table-mean-sd .coefficient-col-p, .regression-results > .ttest-anova-result-panel .coefficient-table-show-df.coefficient-table-trend-analysis.coefficient-table-mean-sd .coefficient-col-effect-size { width: 50px !important; min-width: 50px !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df.coefficient-table-trend-analysis.coefficient-table-mean-sd .coefficient-col-p-trend { width: 82px !important; min-width: 82px !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df.coefficient-table-trend-analysis:not(.coefficient-table-mean-sd) .coefficient-col-p-trend { width: 86px !important; min-width: 86px !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df.coefficient-table-trend-analysis .coefficient-col-p-trend { white-space: nowrap !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df.coefficient-table-trend-analysis .coefficient-col-posthoc { width: 68px !important; min-width: 68px !important; }",
    ".regression-results > .regression-result-panel:not(.landscape-table-panel):not(.model-overview-panel):not(.assumption-review-panel):not(.reference-summary-panel):not(.diagnostic-plots-section):not(.frequency-plots-section):not(.correlation-plot-section):not(.logistic-diagnostics-panel) { width: min(100%, 590px); max-width: 100%; overflow-x: hidden; box-sizing: border-box; }",
    ".regression-results > .regression-result-panel:not(.landscape-table-panel):not(.model-overview-panel):not(.assumption-review-panel):not(.reference-summary-panel):not(.diagnostic-plots-section):not(.frequency-plots-section):not(.correlation-plot-section):not(.logistic-diagnostics-panel) table { width: 100% !important; min-width: 0 !important; max-width: 100% !important; table-layout: fixed; }",
    ".regression-results > .regression-result-panel:not(.landscape-table-panel):not(.model-overview-panel):not(.assumption-review-panel):not(.reference-summary-panel):not(.diagnostic-plots-section):not(.frequency-plots-section):not(.correlation-plot-section):not(.logistic-diagnostics-panel) .coefficient-table .coefficient-col-term, .regression-results > .regression-result-panel:not(.landscape-table-panel):not(.model-overview-panel):not(.assumption-review-panel):not(.reference-summary-panel):not(.diagnostic-plots-section):not(.frequency-plots-section):not(.correlation-plot-section):not(.logistic-diagnostics-panel) .coefficient-table th:first-child, .regression-results > .regression-result-panel:not(.landscape-table-panel):not(.model-overview-panel):not(.assumption-review-panel):not(.reference-summary-panel):not(.diagnostic-plots-section):not(.frequency-plots-section):not(.correlation-plot-section):not(.logistic-diagnostics-panel) .coefficient-table td:first-child { width: 210px; min-width: 210px; white-space: normal; overflow-wrap: normal; word-break: keep-all; }",
    ".regression-results > .regression-result-panel.landscape-table-panel { width: min(100%, 890px); max-width: 100%; overflow-x: hidden; box-sizing: border-box; }",
    ".regression-results > .regression-result-panel.landscape-table-panel table { width: 100% !important; min-width: 0 !important; max-width: 100% !important; table-layout: fixed; }",
    ".regression-result-panel h3 { color: #15233a; font-size: 15px; font-weight: 700; margin: 0 0 8px; }",
    ".regression-result-panel table { width: auto; min-width: 440px; border-collapse: collapse !important; border-spacing: 0 !important; border-top: 2px solid #1f2937 !important; border-bottom: 2px solid #1f2937 !important; color: #2f3a46; font-size: 12px; background: transparent; }",
    ".regression-result-panel table th, .regression-result-panel table td { padding: 5px 7px; line-height: 1.35; border-left: 0 !important; border-right: 0 !important; border-bottom: 1px solid #d7dde5; vertical-align: middle; background: transparent; font-size: 12px !important; }",
    ".regression-result-panel table thead th { border-bottom: 2px solid #1f2937 !important; font-weight: 700; font-size: 11px !important; }",
    ".regression-result-panel table tbody tr:last-child td, .regression-result-panel table tbody tr:last-child th { border-bottom: 0 !important; }",
    ".regression-result-panel table th:first-child, .regression-result-panel table td:first-child { text-align: left !important; }",
    ".regression-result-panel table th:not(:first-child), .regression-result-panel table td:not(:first-child) { text-align: right !important; }",
    ".regression-results > .regression-result-panel .coefficient-table-bootstrap-regression { width: 100% !important; min-width: 0 !important; max-width: 100% !important; table-layout: fixed; }",
    ".regression-results > .regression-result-panel .coefficient-table-bootstrap-regression th, .regression-results > .regression-result-panel .coefficient-table-bootstrap-regression td { padding-left: 4px !important; padding-right: 4px !important; }",
    ".regression-results > .regression-result-panel .coefficient-table-bootstrap-regression .coefficient-col-term { width: 36% !important; min-width: 0 !important; max-width: none !important; }",
    ".regression-results > .regression-result-panel .coefficient-table-bootstrap-regression .coefficient-col-b { width: 10% !important; min-width: 0 !important; }",
    ".regression-results > .regression-result-panel .coefficient-table-bootstrap-regression .coefficient-col-boot-se { width: 12% !important; min-width: 0 !important; }",
    ".regression-results > .regression-result-panel .coefficient-table-bootstrap-regression .coefficient-col-ci { width: 10% !important; min-width: 0 !important; }",
    ".regression-results > .regression-result-panel .coefficient-table-bootstrap-regression .coefficient-col-boot-p { width: 10% !important; min-width: 0 !important; }",
    ".regression-results > .regression-result-panel .coefficient-table-bootstrap-regression .coefficient-col-compact { width: 8% !important; min-width: 0 !important; }",
    ".regression-results > .regression-result-panel .coefficient-table-bootstrap-regression .coefficient-col-tolerance { width: 11% !important; min-width: 0 !important; }",
    ".regression-results > .regression-result-panel .coefficient-table-bootstrap-regression .coefficient-col-vif { width: 8% !important; min-width: 0 !important; }",
    ".coefficient-table { table-layout: auto; }",
    ".coefficient-table th:first-child, .coefficient-table td:first-child { text-align: left !important; }",
    ".coefficient-table th:not(:first-child) { text-align: right !important; }",
    ".coefficient-table thead th { border-top: 2px solid #1f2937 !important; border-bottom: 2px solid #1f2937 !important; font-weight: 700; font-size: 11px !important; }",
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
    ".hierarchical-table-scroll { max-width: 100%; overflow-x: hidden; }",
    ".hierarchical-coefficient-table { border-top: 0 !important; table-layout: fixed !important; width: 100% !important; min-width: 0 !important; max-width: 100% !important; }",
    ".hierarchical-coefficient-table thead tr:first-child th { border-top: 2px solid #1f2937 !important; border-bottom: 0 !important; }",
    ".hierarchical-coefficient-table thead tr:first-child th:first-child { border-bottom: 2px solid #1f2937 !important; }",
    ".hierarchical-coefficient-table .hierarchical-model-header { text-align: center !important; font-weight: 700; border-bottom: 2px solid #1f2937 !important; }",
    ".hierarchical-coefficient-table thead tr:last-child th { border-top: 0 !important; border-bottom: 2px solid #1f2937 !important; }",
    ".hierarchical-coefficient-table thead tr:last-child th:not(.hierarchical-model-separator) { text-align: center !important; }",
    ".hierarchical-coefficient-table tbody td:not(:first-child):not(.hierarchical-model-separator) { text-align: right !important; }",
    ".hierarchical-coefficient-table thead tr:first-child th:first-child, .hierarchical-coefficient-table tbody td:first-child, .hierarchical-coefficient-table tfoot td:first-child { width: auto !important; min-width: 0 !important; max-width: none !important; white-space: normal; overflow-wrap: break-word; word-break: keep-all; }",
    ".hierarchical-coefficient-table .hierarchical-term-col, .hierarchical-coefficient-table .hierarchical-stat-col, .hierarchical-coefficient-table .hierarchical-stat-col-narrow, .hierarchical-coefficient-table .hierarchical-separator-col { min-width: 0 !important; }",
    ".hierarchical-coefficient-table thead tr:last-child th:not(.hierarchical-model-separator), .hierarchical-coefficient-table td:not(:first-child):not(.hierarchical-model-separator) { width: auto !important; min-width: 0 !important; max-width: none !important; padding-left: 4px; padding-right: 4px; overflow-wrap: normal; white-space: nowrap; }",
    ".hierarchical-coefficient-table .hierarchical-model-separator { width: auto !important; min-width: 0 !important; max-width: none !important; padding: 0 !important; background: transparent !important; }",
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
    "  @page { size: B5 portrait; margin: 10mm; @bottom-right { content: counter(page) '/' counter(pages); color: #627d98; font-size: 8pt; } }",
    "  @page easyflow-landscape { size: B5 landscape; margin: 7mm; @bottom-right { content: counter(page) '/' counter(pages); color: #627d98; font-size: 8pt; } }",
    "  * { box-sizing: border-box; }",
    "  body { margin: 0 !important; color: #000000; font-size: 10.5pt; }",
    "  .page-shell { width: 100%; max-width: 156mm !important; margin: 0 auto !important; padding: 0 !important; }",
    "  body.print-mixed-landscape .page-shell { max-width: 100% !important; }",
    "  h1 { font-size: 18pt; margin: 0 0 8pt; }",
    "  .report-cover { height: 230mm; min-height: 230mm; margin: 0 !important; padding: 16mm 12mm 14mm !important; border: 0 !important; border-radius: 0 !important; box-shadow: none !important; break-after: page; page-break-after: always; }",
    "  .report-cover::before { width: 3mm !important; }",
    "  .report-cover-logo { width: 82mm !important; max-width: 82mm !important; }",
    "  .report-cover-main { padding-bottom: 10mm !important; padding-top: 38mm !important; }",
    "  .report-cover-title { font-size: 30pt !important; margin: 4mm 0 5mm !important; }",
    "  .report-cover-subtitle { font-size: 12pt !important; }",
    "  .report-cover-license { gap: 5mm !important; margin-bottom: 6mm !important; }",
    "  .report-cover-license-logo { max-height: 12mm !important; max-width: 42mm !important; }",
    "  .report-cover-license-value { font-size: 10.5pt !important; }",
    "  .report-watermark-inner { opacity: .11 !important; -webkit-print-color-adjust: exact; print-color-adjust: exact; }",
    "  .report-watermark-brand-row { gap: 6mm !important; min-width: 156mm !important; }",
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
    "  .regression-result-panel, .result-section.regression-result-panel { width: 156mm !important; max-width: 100% !important; overflow: visible !important; padding: 8pt 2mm !important; box-sizing: border-box !important; border-left: 0 !important; border-right: 0 !important; border-radius: 0 !important; break-inside: auto; page-break-inside: auto; margin-left: auto !important; margin-right: auto !important; }",
    "  .regression-results > .regression-result-panel:has(table) { break-before: page; page-break-before: always; }",
    "  body.print-mixed-landscape .regression-results .landscape-table-panel { page: easyflow-landscape; width: 236mm !important; max-width: 236mm !important; margin-left: auto !important; margin-right: auto !important; break-before: page; page-break-before: always; break-after: page; page-break-after: always; }",
    "  body.print-mixed-landscape .landscape-table-panel table { width: 100% !important; max-width: 100% !important; min-width: 0 !important; table-layout: fixed !important; }",
    "  body.print-mixed-landscape .landscape-table-panel .result-table-with-note, body.print-mixed-landscape .landscape-table-panel .hierarchical-table-wrap, body.print-mixed-landscape .landscape-table-panel .hierarchical-table-scroll { width: calc(100% - 2mm) !important; max-width: calc(100% - 2mm) !important; margin-left: auto !important; margin-right: auto !important; }",
    "  .regression-results > .regression-result-panel:first-child { break-before: auto; page-break-before: auto; }",
    "  .regression-results > .diagnostic-plots-section, .regression-results > .frequency-plots-section, .regression-results > .correlation-plot-section { break-before: auto !important; page-break-before: auto !important; break-inside: auto !important; page-break-inside: auto !important; }",
    "  .regression-result-panel h3 { font-size: 11pt; margin: 0 0 5pt; }",
    "  .result-table-with-note, .frequency-table-wrap, .hierarchical-table-wrap, .hierarchical-table-scroll { display: block !important; width: 100% !important; max-width: 100% !important; margin-left: auto !important; margin-right: auto !important; overflow: visible !important; }",
    "  table, .regression-result-panel table, .coefficient-table { width: 100% !important; min-width: 0 !important; max-width: 100% !important; table-layout: fixed !important; font-size: 9pt !important; box-sizing: border-box !important; }",
    "  .regression-result-panel > table { width: 100% !important; max-width: 100% !important; margin-left: auto !important; margin-right: auto !important; }",
    "  .hierarchical-coefficient-table { width: 100% !important; min-width: 0 !important; max-width: 100% !important; table-layout: fixed !important; font-size: 8.2pt !important; box-sizing: border-box !important; }",
    "  .coefficient-table:not(.hierarchical-coefficient-table) col { width: auto !important; }",
    "  .coefficient-table col.coefficient-col-note-marker { width: 12px !important; min-width: 12px !important; max-width: 12px !important; }",
    "  .coefficient-table .coefficient-col-term, .coefficient-table .coefficient-col-b, .coefficient-table .coefficient-col-reference, .coefficient-table .coefficient-col-tolerance, .coefficient-table .coefficient-col-compact, .coefficient-table .coefficient-col-stat { width: auto !important; }",
    "  .hierarchical-coefficient-table col { min-width: 0 !important; max-width: none !important; }",
    "  .hierarchical-coefficient-table, .correlation-result-section table { font-size: 7.2pt !important; }",
    "  .paired-two-grouped-table { table-layout: fixed !important; font-size: 9.2pt !important; }",
    "  .paired-two-grouped-table col.paired-two-col-variable { width: 18% !important; }",
    "  .paired-two-grouped-table col.paired-two-col-summary { width: 8% !important; }",
    "  .paired-two-grouped-table col.paired-two-col-stat { width: 10% !important; }",
    "  .paired-two-grouped-table col.paired-two-col-effect { width: 7% !important; }",
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
    "  .coefficient-table thead th, .regression-result-panel table thead th { font-size: 8pt !important; font-weight: 700 !important; }",
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
    "  body.print-mixed-landscape .landscape-table-panel .crosstab-main-table { width: 100% !important; min-width: 0 !important; max-width: 100% !important; table-layout: fixed !important; font-size: 6.8pt !important; }",
    "  body.print-mixed-landscape .landscape-table-panel .crosstab-main-table col.crosstab-row-variable-col { width: 6.5% !important; }",
    "  body.print-mixed-landscape .landscape-table-panel .crosstab-main-table col.crosstab-row-label-col { width: 5.5% !important; }",
    "  body.print-mixed-landscape .landscape-table-panel .crosstab-main-table col.crosstab-n-col { width: 7% !important; }",
    "  body.print-mixed-landscape .landscape-table-panel .crosstab-main-table col.crosstab-count-col { width: auto !important; }",
    "  body.print-mixed-landscape .landscape-table-panel .crosstab-main-table col.crosstab-stat-col { width: 6% !important; }",
    "  body.print-mixed-landscape .landscape-table-panel .crosstab-main-table col.crosstab-trend-p-col { width: 7% !important; }",
    "  body.print-mixed-landscape .landscape-table-panel .crosstab-main-table th, body.print-mixed-landscape .landscape-table-panel .crosstab-main-table td { min-width: 0 !important; padding: 2.5pt 2.5pt !important; overflow: hidden !important; text-overflow: clip !important; white-space: nowrap !important; }",
    "  body.print-mixed-landscape .landscape-table-panel .crosstab-main-table .crosstab-row-variable, body.print-mixed-landscape .landscape-table-panel .crosstab-main-table .crosstab-row-label { min-width: 0 !important; }",
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
    "@media screen {",
    "  .result-table-with-note.result-table-sheet:is(#statedu-saved-result-sheet, [data-result-table-sheet=\"true\"]) { width: min(100%, var(--result-sheet-width)) !important; max-width: 100% !important; overflow-x: auto !important; font-family: Arial, \"Noto Sans KR\", \"Malgun Gothic\", sans-serif !important; font-size: 12px !important; }",
    "  .result-table-sheet:is(#statedu-saved-result-sheet, [data-result-table-sheet=\"true\"]) > table.result-table-contract-table { width: max(100%, var(--result-table-intrinsic-width, 480px)) !important; min-width: var(--result-table-intrinsic-width, 480px) !important; max-width: none !important; table-layout: auto !important; }",
    "  .result-table-sheet:is(#statedu-saved-result-sheet, [data-result-table-sheet=\"true\"]) > table.result-table-contract-table th { font-family: inherit !important; font-size: 11px !important; white-space: normal !important; overflow-wrap: anywhere !important; }",
    "  .result-table-sheet:is(#statedu-saved-result-sheet, [data-result-table-sheet=\"true\"]) > table.result-table-contract-table td { font-family: inherit !important; font-size: 12px !important; }",
    "  .result-table-sheet:is(#statedu-saved-result-sheet, [data-result-table-sheet=\"true\"]) .coefficient-note, .result-table-sheet:is(#statedu-saved-result-sheet, [data-result-table-sheet=\"true\"]) .coefficient-warning { font-family: inherit !important; font-size: 11px !important; line-height: 1.4 !important; }",
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
    ".logistic-results > .result-section.regression-result-panel:not(.landscape-table-panel):not(.logistic-diagnostics-panel) { width: min(100%, 590px); max-width: 100%; overflow-x: hidden; box-sizing: border-box; }",
    ".logistic-results > .result-section.regression-result-panel:not(.landscape-table-panel):not(.logistic-diagnostics-panel) > table, .logistic-results > .result-section.regression-result-panel:not(.landscape-table-panel):not(.logistic-diagnostics-panel) .result-table-with-note > table, .logistic-results > .result-section.regression-result-panel:not(.landscape-table-panel):not(.logistic-diagnostics-panel) .logistic-result-table { width: 100% !important; min-width: 0 !important; max-width: 100% !important; table-layout: fixed; }",
    ".result-section.regression-result-panel:not(.landscape-table-panel):not(.paired-result-section):not(.logistic-diagnostics-panel) > .result-table-with-note, .result-section.regression-result-panel:not(.landscape-table-panel):not(.paired-result-section):not(.logistic-diagnostics-panel) > .frequency-table-wrap, .result-section.regression-result-panel:not(.landscape-table-panel):not(.paired-result-section):not(.logistic-diagnostics-panel) > table, .result-section.regression-result-panel:not(.landscape-table-panel):not(.paired-result-section):not(.logistic-diagnostics-panel) .result-table-with-note > table { width: 100% !important; max-width: 100%; }",
    ".result-section.regression-result-panel:not(.landscape-table-panel):not(.paired-result-section):not(.logistic-diagnostics-panel) table { width: 100% !important; min-width: 0 !important; max-width: 100% !important; table-layout: fixed; }",
    ".result-section.regression-result-panel.landscape-table-panel { width: min(100%, 890px); }",
    ".result-section.regression-result-panel.landscape-table-panel > .result-table-with-note, .result-section.regression-result-panel.landscape-table-panel > .hierarchical-table-wrap, .result-section.regression-result-panel.landscape-table-panel > .hierarchical-table-scroll, .result-section.regression-result-panel.landscape-table-panel > table, .result-section.regression-result-panel.landscape-table-panel .result-table-with-note > table { width: 100% !important; max-width: 100%; }",
    ".result-section.regression-result-panel.landscape-table-panel table { width: 100% !important; min-width: 0 !important; max-width: 100% !important; table-layout: fixed; }",
    ".result-section.regression-result-panel.ttest-anova-overview-panel, .result-section.regression-result-panel.ttest-anova-assumption-review-panel { width: 100% !important; max-width: 100% !important; overflow-x: hidden !important; }",
    ".result-section.regression-result-panel.ttest-anova-overview-panel > .result-table-with-note, .result-section.regression-result-panel.ttest-anova-assumption-review-panel > .result-table-with-note, .result-section.regression-result-panel.ttest-anova-overview-panel .result-table-with-note > table, .result-section.regression-result-panel.ttest-anova-assumption-review-panel .result-table-with-note > table { width: 100% !important; max-width: 100% !important; min-width: 0 !important; }",
    ".regression-results > .ttest-anova-result-panel { overflow-x: hidden !important; }",
    ".regression-results > .ttest-anova-result-panel > .result-table-with-note { width: 100% !important; max-width: 100% !important; overflow-x: hidden !important; }",
    ".regression-results > .ttest-anova-result-panel .result-table-with-note > table, .regression-results > .ttest-anova-result-panel .coefficient-table-trend-analysis { width: 100% !important; min-width: 0 !important; max-width: 100% !important; table-layout: fixed !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df .coefficient-col-statistic { width: 112px !important; min-width: 112px !important; padding-right: 12px !important; text-align: right !important; white-space: nowrap !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df .coefficient-col-p { padding-left: 12px !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df:not(.coefficient-table-mean-sd) .coefficient-col-stat { width: 44px !important; min-width: 44px !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df:not(.coefficient-table-mean-sd) .coefficient-col-statistic { width: 144px !important; min-width: 144px !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df.coefficient-table-trend-analysis .coefficient-col-term { width: 82px !important; min-width: 82px !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df.coefficient-table-trend-analysis.coefficient-table-mean-sd .coefficient-col-value { width: 112px !important; min-width: 112px !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df.coefficient-table-trend-analysis:not(.coefficient-table-mean-sd) .coefficient-col-value { width: 104px !important; min-width: 104px !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df.coefficient-table-trend-analysis.coefficient-table-mean-sd .coefficient-col-mse { width: 116px !important; min-width: 116px !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df.coefficient-table-trend-analysis.coefficient-table-mean-sd .coefficient-col-statistic { width: 144px !important; min-width: 144px !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df.coefficient-table-trend-analysis:not(.coefficient-table-mean-sd) .coefficient-col-stat { width: 46px !important; min-width: 46px !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df.coefficient-table-trend-analysis:not(.coefficient-table-mean-sd) .coefficient-col-p, .regression-results > .ttest-anova-result-panel .coefficient-table-show-df.coefficient-table-trend-analysis:not(.coefficient-table-mean-sd) .coefficient-col-effect-size { width: 50px !important; min-width: 50px !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df.coefficient-table-trend-analysis:not(.coefficient-table-mean-sd) .coefficient-col-statistic { width: 158px !important; min-width: 158px !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df.coefficient-table-trend-analysis.coefficient-table-mean-sd .coefficient-col-p, .regression-results > .ttest-anova-result-panel .coefficient-table-show-df.coefficient-table-trend-analysis.coefficient-table-mean-sd .coefficient-col-effect-size { width: 50px !important; min-width: 50px !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df.coefficient-table-trend-analysis.coefficient-table-mean-sd .coefficient-col-p-trend { width: 82px !important; min-width: 82px !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df.coefficient-table-trend-analysis:not(.coefficient-table-mean-sd) .coefficient-col-p-trend { width: 86px !important; min-width: 86px !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df.coefficient-table-trend-analysis .coefficient-col-p-trend { white-space: nowrap !important; }",
    ".regression-results > .ttest-anova-result-panel .coefficient-table-show-df.coefficient-table-trend-analysis .coefficient-col-posthoc { width: 68px !important; min-width: 68px !important; }",
    ".regression-results > .regression-result-panel:not(.landscape-table-panel):not(.model-overview-panel):not(.assumption-review-panel):not(.reference-summary-panel):not(.diagnostic-plots-section):not(.frequency-plots-section):not(.correlation-plot-section):not(.logistic-diagnostics-panel) { width: min(100%, 590px); max-width: 100%; overflow-x: hidden; box-sizing: border-box; }",
    ".regression-results > .regression-result-panel:not(.landscape-table-panel):not(.model-overview-panel):not(.assumption-review-panel):not(.reference-summary-panel):not(.diagnostic-plots-section):not(.frequency-plots-section):not(.correlation-plot-section):not(.logistic-diagnostics-panel) table { width: 100% !important; min-width: 0 !important; max-width: 100% !important; table-layout: fixed; }",
    ".regression-results > .regression-result-panel:not(.landscape-table-panel):not(.model-overview-panel):not(.assumption-review-panel):not(.reference-summary-panel):not(.diagnostic-plots-section):not(.frequency-plots-section):not(.correlation-plot-section):not(.logistic-diagnostics-panel) .coefficient-table .coefficient-col-term, .regression-results > .regression-result-panel:not(.landscape-table-panel):not(.model-overview-panel):not(.assumption-review-panel):not(.reference-summary-panel):not(.diagnostic-plots-section):not(.frequency-plots-section):not(.correlation-plot-section):not(.logistic-diagnostics-panel) .coefficient-table th:first-child, .regression-results > .regression-result-panel:not(.landscape-table-panel):not(.model-overview-panel):not(.assumption-review-panel):not(.reference-summary-panel):not(.frequency-plots-section):not(.diagnostic-plots-section):not(.correlation-plot-section):not(.logistic-diagnostics-panel) .coefficient-table td:first-child { width: 210px; min-width: 210px; white-space: normal; overflow-wrap: normal; word-break: keep-all; }",
    ".regression-results > .regression-result-panel.landscape-table-panel { width: min(100%, 890px); max-width: 100%; overflow-x: hidden; box-sizing: border-box; }",
    ".regression-results > .regression-result-panel.landscape-table-panel table { width: 100% !important; min-width: 0 !important; max-width: 100% !important; table-layout: fixed; }",
    ".regression-result-panel h3 { color: #15233a; font-size: 15px; font-weight: 700; margin: 0 0 8px; }",
    ".regression-result-panel table, .coefficient-table { width: auto; min-width: 440px; border-collapse: collapse !important; border-spacing: 0 !important; border-top: 2px solid #1f2937 !important; border-bottom: 2px solid #1f2937 !important; color: #2f3a46; font-size: 12px; background: transparent; }",
    ".regression-result-panel table th, .regression-result-panel table td { padding: 5px 7px; line-height: 1.35; border-left: 0 !important; border-right: 0 !important; border-bottom: 1px solid #d7dde5; vertical-align: middle; background: transparent; white-space: nowrap; font-size: 12px !important; }",
    ".regression-result-panel table thead th { border-bottom: 2px solid #1f2937 !important; font-weight: 700; font-size: 11px !important; }",
    ".regression-result-panel table th:first-child, .regression-result-panel table td:first-child { text-align: left !important; }",
    ".regression-result-panel table th:not(:first-child), .regression-result-panel table td:not(:first-child) { text-align: right !important; }",
    ".regression-results > .regression-result-panel .coefficient-table-bootstrap-regression { width: 100% !important; min-width: 0 !important; max-width: 100% !important; table-layout: fixed; }",
    ".regression-results > .regression-result-panel .coefficient-table-bootstrap-regression th, .regression-results > .regression-result-panel .coefficient-table-bootstrap-regression td { padding-left: 4px !important; padding-right: 4px !important; }",
    ".regression-results > .regression-result-panel .coefficient-table-bootstrap-regression .coefficient-col-term { width: 36% !important; min-width: 0 !important; max-width: none !important; }",
    ".regression-results > .regression-result-panel .coefficient-table-bootstrap-regression .coefficient-col-b { width: 10% !important; min-width: 0 !important; }",
    ".regression-results > .regression-result-panel .coefficient-table-bootstrap-regression .coefficient-col-boot-se { width: 12% !important; min-width: 0 !important; }",
    ".regression-results > .regression-result-panel .coefficient-table-bootstrap-regression .coefficient-col-ci { width: 10% !important; min-width: 0 !important; }",
    ".regression-results > .regression-result-panel .coefficient-table-bootstrap-regression .coefficient-col-boot-p { width: 10% !important; min-width: 0 !important; }",
    ".regression-results > .regression-result-panel .coefficient-table-bootstrap-regression .coefficient-col-compact { width: 8% !important; min-width: 0 !important; }",
    ".regression-results > .regression-result-panel .coefficient-table-bootstrap-regression .coefficient-col-tolerance { width: 11% !important; min-width: 0 !important; }",
    ".regression-results > .regression-result-panel .coefficient-table-bootstrap-regression .coefficient-col-vif { width: 8% !important; min-width: 0 !important; }",
        ".coefficient-table.crosstab-main-table > thead > tr > th.crosstab-col-head, .regression-result-panel .crosstab-main-table > thead > tr > th.crosstab-col-head { text-align: center !important; }",
        ".coefficient-table.crosstab-main-table > tbody > tr > td.crosstab-row-label, .regression-result-panel .crosstab-main-table > tbody > tr > td.crosstab-row-label { text-align: left !important; }",
        ".result-table-with-note, .frequency-table-wrap, .hierarchical-table-wrap, .hierarchical-table-scroll { max-width: 100%; overflow-x: auto; }",
    ".frequency-plot-grid, .frequency-plot-row, .correlation-plot-grid, .residual-diagnostic-plots { display: flex; flex-wrap: wrap; gap: 16px; align-items: flex-start; }",
    ".frequency-plot-card, .correlation-plot-card, .residual-plot-card { border: 1px solid #d9e2ec; border-radius: 6px; padding: 12px; background: #ffffff; }",
    ".frequency-plot-card h4, .correlation-plot-card h4, .residual-plot-card h4 { margin: 0 0 8px; font-size: 15px; color: #15233a; }",
    ".frequency-plot-card img, .residual-plot-card img, .correlation-plot-card img { display: block; max-width: 100%; height: auto; }",
    "@media screen {",
    "  .result-table-with-note.result-table-sheet:is(#statedu-saved-result-sheet, [data-result-table-sheet=\"true\"]) { width: min(100%, var(--result-sheet-width)) !important; max-width: 100% !important; overflow-x: auto !important; font-family: Arial, \"Noto Sans KR\", \"Malgun Gothic\", sans-serif !important; font-size: 12px !important; }",
    "  .result-table-sheet:is(#statedu-saved-result-sheet, [data-result-table-sheet=\"true\"]) > table.result-table-contract-table { width: max(100%, var(--result-table-intrinsic-width, 480px)) !important; min-width: var(--result-table-intrinsic-width, 480px) !important; max-width: none !important; table-layout: auto !important; }",
    "  .result-table-sheet:is(#statedu-saved-result-sheet, [data-result-table-sheet=\"true\"]) > table.result-table-contract-table th { font-family: inherit !important; font-size: 11px !important; white-space: normal !important; overflow-wrap: anywhere !important; }",
    "  .result-table-sheet:is(#statedu-saved-result-sheet, [data-result-table-sheet=\"true\"]) > table.result-table-contract-table td { font-family: inherit !important; font-size: 12px !important; }",
    "  .result-table-sheet:is(#statedu-saved-result-sheet, [data-result-table-sheet=\"true\"]) .coefficient-note, .result-table-sheet:is(#statedu-saved-result-sheet, [data-result-table-sheet=\"true\"]) .coefficient-warning { font-family: inherit !important; font-size: 11px !important; line-height: 1.4 !important; }",
    "}",
    sep = "\n"
  )
}

saved_results_report_cover <- function(title, language = NULL) {
  saved_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  if (is.null(language)) language <- if (exists("statedu_current_language", mode = "function")) statedu_current_language() else "en"
  text <- function(key, fallback) {
    if (exists("statedu_t", mode = "function")) statedu_t(paste0("report.cover.", key), language, fallback) else fallback
  }
  # Application-generated titles follow the UI language; preserve custom titles.
  if (grepl("^StatEdu Studio", title)) title <- text("title", "Analysis Results Report")
  app_version <- saved_results_app_version()
  app_label <- if (nzchar(app_version)) sprintf("StatEdu Studio v%s", app_version) else "StatEdu Studio"
  logo_uri <- saved_results_image_data_uri(file.path("www", "logo-horizontal.png"))
  cover_text <- saved_results_cover_text()
  organization_logo_uri <- if (nzchar(cover_text$organization_logo)) saved_results_image_data_uri(cover_text$organization_logo) else ""
  cover_target <- if (nzchar(cover_text$organization)) cover_text$organization else "Internal analysis report"
  cover_user <- if (nzchar(cover_text$user)) cover_text$user else text("default_user", "StatEdu Studio user")
  cover_license_name <- if (identical(cover_text$edition, "free")) {
    "StatEdu, Institute of Statistics"
  } else if (identical(cover_text$edition, "institution") && nzchar(cover_text$organization)) {
    cover_text$organization
  } else if (nzchar(cover_text$user)) {
    cover_text$user
  } else {
    ""
  }
  cover_license_label <- if (identical(cover_text$edition, "development")) {
    text("development", "Development owner")
  } else if (identical(cover_text$edition, "institution")) {
    text("institution", "Institution")
  } else if (identical(cover_text$edition, "free")) {
    text("prepared_with", "Prepared with")
  } else {
    text("licensed_user", "Licensed user")
  }
  developer_labels <- c(ko = "분석 개발자", en = "Analysis developer", ja = "分析開発者", zh = "分析开发者",
    es = "Desarrollador del análisis", fr = "Développeur de l’analyse", de = "Entwickler der Analyse", vi = "Nhà phát triển phân tích")
  developer_label <- unname(developer_labels[language])
  if (length(developer_label) != 1L || is.na(developer_label)) developer_label <- "Analysis developer"
  developer_logo_uri <- saved_results_image_data_uri(file.path("www", "statedu_logo.png"))
  cover_meta_items <- list(div(class = "report-cover-meta-item",
    span(developer_label, class = "report-cover-meta-label"),
    span(class = "report-cover-developer report-cover-meta-value",
      if (nzchar(developer_logo_uri)) tags$img(src = developer_logo_uri, alt = "StatEdu", class = "report-cover-developer-logo"),
      span(if (identical(language, "ko")) "이일현" else "Il Hyun Lee"))))
  if (!identical(cover_text$edition, "personal") && nzchar(cover_text$organization)) {
    cover_meta_items <- c(cover_meta_items, list(div(class = "report-cover-meta-item", span(text("prepared_for", "Prepared for"), class = "report-cover-meta-label"), span(cover_target, class = "report-cover-meta-value"))))
  }
  if (!identical(cover_text$edition, "free") && (!identical(cover_text$edition, "institution") || nzchar(cover_text$user))) {
    cover_meta_items <- c(cover_meta_items, list(div(class = "report-cover-meta-item", span(text("prepared_by", "Prepared by"), class = "report-cover-meta-label"), span(cover_user, class = "report-cover-meta-value"))))
  }
  cover_meta_items <- c(
    cover_meta_items,
    list(
      div(class = "report-cover-meta-item", span(text("saved", "Saved"), class = "report-cover-meta-label"), span(saved_time, class = "report-cover-meta-value")),
      div(class = "report-cover-meta-item", span(text("output_date", "Output date"), class = "report-cover-meta-label"), span(saved_time, class = "report-cover-meta-value")),
      div(class = "report-cover-meta-item", span(text("application", "Application"), class = "report-cover-meta-label"), span(app_label, class = "report-cover-meta-value"))
    )
  )
      div(
        class = "report-cover", lang = language,
        div(
          class = "report-cover-brand",
          if (nzchar(logo_uri)) {
            tags$img(src = logo_uri, class = "report-cover-logo", alt = "StatEdu Studio")
          } else {
            div("StatEdu Studio", class = "report-cover-kicker")
          },
          div(text(if (identical(cover_text$edition, "development")) "development_edition" else cover_text$edition, cover_text$edition), class = "report-cover-edition")
        ),
        div(
          class = "report-cover-main",
          div(text("kicker", "Statistical Report"), class = "report-cover-kicker"),
          h1(title, class = "report-cover-title"),
          p(text("subtitle", "Analysis results prepared for review, documentation, and printing."), class = "report-cover-subtitle"),
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
        div(sprintf("Lee, I. H. (2026). StatEdu Studio (Version %s) [Computer software]. https://doi.org/10.22934/statedu.studio", app_version), class = "report-cover-footer")
      )
}

saved_results_report_cover_css <- function() {
  paste(
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
    ".report-cover-meta { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 14px 28px; color: #334e68; font-size: 14px; line-height: 1.45; border-top: 2px solid #102a43; padding-top: 18px; position: relative; z-index: 1; }",
    ".report-cover-meta-item { min-width: 0; }",
    ".report-cover-meta-label { color: #627d98; display: block; font-size: 11px; font-weight: 700; letter-spacing: .08em; text-transform: uppercase; margin-bottom: 4px; }",
    ".report-cover-meta-value { color: #102a43; font-weight: 600; overflow-wrap: anywhere; }",
    ".report-cover-developer { display: flex; align-items: center; gap: 10px; }",
    ".report-cover-developer-logo { display: block; width: 25mm; height: auto; max-height: 10mm; object-fit: contain; }",
    ".report-cover-footer { color: #627d98; font-size: 12px; margin-top: 20px; position: relative; z-index: 1; }",
    "@page statedu-report-cover { size: B5 portrait; margin: 3mm; }",
    ".report-cover { page: statedu-report-cover; box-sizing: border-box; width: 170mm; height: 243mm; min-height: 243mm; margin: 0; padding: 14mm 12mm 12mm; border: 0; border-radius: 0; box-shadow: none; break-after: page; break-inside: avoid; font-family: Arial, \"Malgun Gothic\", sans-serif; }",
    ".report-cover-logo { width: 70mm; max-width: 70mm; }",
    ".report-cover-main { padding: 12mm 0 8mm; }",
    ".report-cover-title { font-size: 28pt; overflow-wrap: anywhere; }",
    ".report-cover-subtitle { font-size: 11pt; }",
    ".report-cover-meta { font-size: 9pt; gap: 10px 18px; }",
    ".report-cover-footer { font-size: 8pt; }",
    sep = "\n"
  )
}

saved_results_document <- function(title, content, max_width = 1280, css_path = file.path("www", "style.css"), print_landscape = FALSE, report_mode = FALSE) {
  if (isTRUE(report_mode)) return(saved_result_sheet_document(title, content, css_path))
  css <- if (file.exists(css_path)) paste(readLines(css_path, warn = FALSE), collapse = "\n") else ""
  canvas_css <- file.path(dirname(css_path), "model-canvas", "canvas.css")
  if (file.exists(canvas_css)) css <- paste(css, paste(readLines(canvas_css, warn = FALSE), collapse = "\n"), sep = "\n")
  saved_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  document <- tags$html(
    tags$head(
      tags$meta(charset = "UTF-8"),
      tags$title(title),
      tags$style(htmltools::HTML(css)),
      tags$style(htmltools::HTML(saved_results_viewer_css(max_width)))
    ),
    tags$body(
      class = if (isTRUE(print_landscape)) "print-mixed-landscape" else "print-portrait",
      div(class = "page-shell", content)
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
  output_table_style = "standard",
  css_path = file.path("www", "style.css"),
  report_mode = FALSE,
  plot_renderer = plot_data_uri
) {
  appendix_language <- result_appendix_table_language()
  saved_results_document(
    "StatEdu Studio Results",
    div(
      class = "regression-results",
      div(
        class = "result-section regression-result-panel model-overview-panel",
        lang = appendix_language,
        h3(result_appendix_ui_text("Model overview", appendix_language)),
        model_overview_html_table(
          regression_appendix_table(
            model_overview_data_frame(results, variable_table, labels),
            appendix_language
          )
        )
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
          show_vif,
          output_table_style
        )
      }),
      regression_reference_summary_block(results, variable_table, labels, show_sr2, show_f2),
      regression_bootstrap_diagnostics_block(results, variable_table, labels),
      regression_assumption_review_block(results, variable_table, labels),
      analysis_diagnostics_section(
        attr(results, "warnings"), attr(results, "skipped"),
        title = regression_appendix_text("Warnings / skipped models"),
        class = "regression-result-panel"
      ),
      lapply(seq_along(results), function(index) {
        result <- results[[index]]
        dependent <- all.vars(result$formula)[[1]]
        dependent_label <- display_variable_name_static(dependent, variable_table, labels, label_only = TRUE)
        saved_plot_result_block(result, dependent_label, plot_renderer = plot_renderer)
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
  output_table_style = "standard",
  css_path = file.path("www", "style.css"),
  report_mode = FALSE,
  plot_renderer = plot_data_uri
) {
  output_table_style <- analysis_output_table_style(output_table_style)
  print_landscape <- identical(output_table_style, "wide") &&
    any(vapply(hierarchical_result_groups(results), function(group) length(group) >= 3L, logical(1)))
  saved_results_document(
    "StatEdu Studio Hierarchical Results",
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
      output_table_style = output_table_style,
      plot_blocks = lapply(seq_along(results), function(index) {
        result <- results[[index]]
        dependent <- all.vars(result$formula)[[1]]
        dependent_label <- display_variable_name_static(dependent, variable_table, labels, label_only = TRUE)
        saved_plot_result_block(result, dependent_label, plot_renderer = plot_renderer)
      })
    ),
    max_width = if (isTRUE(print_landscape)) 1500 else 1280,
    css_path = css_path,
    print_landscape = print_landscape,
    report_mode = report_mode
  )
}

frequency_plot_data_uri <- function(result, type, name, width = 420, height = 320, res = 96) {
  plot_data_uri(function(plot_result) draw_frequency_plot(plot_result, type, name),
                result, width = width, height = height, res = res)
}

frequency_export_image_cache <- function(render = plot_data_uri, max_bytes = 16 * 1024^2,
                                         max_entries = 64L) {
  images <- correlation_export_image_cache(render = render, max_bytes = max_bytes,
                                           max_entries = max_entries)
  draw <- function(value) value$draw_function(value$result, value$type, value$name)
  list(
    clear = images$clear,
    render = function(result, type, name, width = 420, height = 320, res = 96) {
      # Keep the wrapper stable; include the actual drawing function in the key.
      value <- list(result = result, type = type, name = name, draw_function = draw_frequency_plot)
      images$render(draw, value, width = width, height = height, res = res)
    }
  )
}

saved_frequency_plot_blocks <- function(result, options, plot_renderer = frequency_plot_data_uri) {
  plot_block <- function(type, name) {
    variable_label <- frequency_variable_display_name(name, result$variable_info, result$labels, result$category_table)
    title <- sprintf("%s(%s)", frequency_plot_label(type), variable_label)
    tags$div(
      class = "frequency-plot-card",
      tags$h4(title),
      tags$img(
        src = plot_renderer(result, type, name, width = 420, height = 320),
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

saved_frequencies_results_html <- function(result, css_path = file.path("www", "style.css"), report_mode = FALSE,
                                           plot_renderer = frequency_plot_data_uri) {
  options <- result$options %||% list(n_percent = TRUE, mean_sd = TRUE)
  saved_results_document(
    "StatEdu Studio Frequencies Results",
    tags$div(
      class = "regression-results",
      frequency_main_table_sections(result, options),
      saved_frequency_plot_blocks(result, options, plot_renderer = plot_renderer)
    ),
    max_width = 1500,
    css_path = css_path,
    report_mode = report_mode
  )
}

saved_reliability_results_html <- function(result, css_path = file.path("www", "style.css"), report_mode = FALSE) {
  saved_results_document(
    "StatEdu Studio Reliability Results",
    tags$div(class = "regression-results", reliability_results_ui(result)),
    max_width = 1500,
    css_path = css_path,
    report_mode = report_mode
  )
}

saved_ttest_anova_results_html <- function(result, css_path = file.path("www", "style.css"), report_mode = FALSE) {
  print_landscape <- length(result$dependents %||% character(0)) >= 5L
  saved_results_document(
    "StatEdu Studio t-test / ANOVA Results",
    tags$div(class = "regression-results", ttest_anova_results_ui(result)),
    max_width = 1500,
    css_path = css_path,
    print_landscape = print_landscape,
    report_mode = report_mode
  )
}

saved_ancova_results_html <- function(result, variable_table = NULL, labels = character(0), css_path = file.path("www", "style.css"), report_mode = FALSE, plot_renderer = plot_data_uri) {
  saved_results_document(
    "StatEdu Studio ANCOVA Results",
    tags$div(class = "regression-results", ancova_results_ui(result, variable_table, labels, plot_renderer)),
    max_width = 1500,
    css_path = css_path,
    report_mode = report_mode
  )
}

saved_nonparametric_results_html <- function(result, css_path = file.path("www", "style.css"), report_mode = FALSE) {
  saved_results_document(
    "StatEdu Studio Nonparametric Test Results",
    tags$div(class = "regression-results", ttest_anova_results_ui(result)),
    max_width = 1500,
    css_path = css_path,
    report_mode = report_mode
  )
}

saved_nonparametric_paired_results_html <- function(result, css_path = file.path("www", "style.css"), report_mode = FALSE) {
  saved_results_document(
    "StatEdu Studio Nonparametric Paired Test Results",
    tags$div(class = "regression-results", nonparametric_paired_results_ui(result)),
    max_width = 1500,
    css_path = css_path,
    print_landscape = identical(result$type, "nonparametric_paired_rm") || identical(result$type, "nonparametric_paired_combined"),
    report_mode = report_mode
  )
}

saved_paired_results_html <- function(result, css_path = file.path("www", "style.css"), report_mode = FALSE) {
  saved_results_document(
    "StatEdu Studio Paired Test Results",
    tags$div(class = "regression-results", paired_results_ui(result)),
    max_width = 1500,
    css_path = css_path,
    print_landscape = identical(result$type, "paired_rm") || identical(result$type, "paired_combined"),
    report_mode = report_mode
  )
}

saved_paired_rm_results_html <- function(result, css_path = file.path("www", "style.css"), report_mode = FALSE) {
  saved_results_document(
    "StatEdu Studio Paired Test 3+ Results",
    tags$div(class = "regression-results", paired_rm_results_ui(result)),
    max_width = 1500,
    css_path = css_path,
    print_landscape = TRUE,
    report_mode = report_mode
  )
}

saved_correlation_results_html <- function(result, css_path = file.path("www", "style.css"), report_mode = FALSE,
                                           plot_renderer = plot_data_uri) {
  options <- result$options %||% list()
  appendix_language <- result_appendix_table_language()
  appendix_text <- function(en, ko) statedu_localized_text(appendix_language, en, ko)
  normality_table <- correlation_appendix_localize_table(
    correlation_normality_display_table(result),
    appendix_language
  )
  variable_count <- length(result$variables %||% character(0))
  print_landscape <- variable_count >= 12L || (isTRUE(options$p_ci) && variable_count >= 6L)
  saved_results_document(
    "StatEdu Studio Correlation Results",
    tags$div(
      class = "correlation-results regression-results",
      correlation_matrix_set_ui(result),
      if (is.list(result$latent)) {
        correlation_matrix_set_ui(result, source = result$latent, title_prefix = "Latent-variable ")
      },
      if (isTRUE(options$normality) && is.data.frame(normality_table) && nrow(normality_table) > 0) {
        tags$div(
          class = "result-section correlation-result-section regression-result-panel",
          lang = appendix_language,
          tags$h3(appendix_text("Normality", "정규성")),
          coefficient_html_table(
            normality_table,
            table_role = "appendix",
            table_language = appendix_language
          )
        )
      },
      if (isTRUE(options$scatter_plot)) {
        tags$div(
          class = "result-section correlation-result-section correlation-plot-section regression-result-panel",
          tags$h3("Scatter plot matrix"),
          tags$div(
            class = "correlation-plot-card",
            tags$img(
              src = plot_renderer(draw_correlation_scatter_plot, result, width = 900, height = 900),
              width = "720",
              height = "720",
              alt = "Scatter plot matrix"
            ),
            correlation_plot_note(result)
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
              src = plot_renderer(draw_correlation_heatmap, result, width = 900, height = 900),
              width = "720",
              height = "720",
              alt = "Correlation matrix heatmap"
            ),
            correlation_plot_note(result)
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
  split_ci = TRUE,
  output_table_style = "standard",
  css_path = file.path("www", "style.css"),
  report_mode = FALSE
) {
  output_table_style <- analysis_output_table_style(output_table_style)
  print_landscape <- identical(output_table_style, "wide") &&
    any(vapply(logistic_result_groups(results), function(group) length(group) >= 3L, logical(1)))
  saved_results_document(
    "StatEdu Studio Logistic Regression Results",
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
        split_ci = split_ci,
        output_table_style = output_table_style
      )
    ),
    max_width = if (isTRUE(print_landscape)) 1500 else 1280,
    css_path = css_path,
    print_landscape = print_landscape,
    report_mode = report_mode
  )
}

saved_factor_analysis_results_html <- function(result, css_path = file.path("www", "style.css"), report_mode = FALSE,
                                               plot_renderer = plot_data_uri) {
  saved_results_document(
    "StatEdu Studio Factor Analysis Results",
    factor_analysis_results_ui(result, report_mode = TRUE, plot_renderer = plot_renderer),
    max_width = 1500,
    css_path = css_path,
    report_mode = report_mode
  )
}

saved_pca_results_html <- local({
  render <- function(result, css_path, report_mode, plot_renderer) {
    saved_results_document(
      "StatEdu Studio Principal Component Analysis Results",
      pca_results_ui(result, report_mode = TRUE, plot_renderer = plot_renderer),
      max_width = 1500,
      css_path = css_path,
      report_mode = report_mode
    )
  }
  function(result, css_path = file.path("www", "style.css"), report_mode = FALSE,
           plot_renderer = plot_data_uri) {
    # Avoid compiling the presentation tree during this synchronous export.
    previous_jit <- compiler::enableJIT(0)
    on.exit(invisible(compiler::enableJIT(previous_jit)), add = TRUE)
    render(result, css_path, report_mode, plot_renderer)
  }
})

saved_crosstab_results_html <- function(result, css_path = file.path("www", "style.css"), report_mode = FALSE) {
  saved_results_document(
    "StatEdu Studio Cross-tabulation Results",
    tags$div(class = "crosstab-results regression-results", crosstab_results_ui(result)),
    max_width = 1500,
    css_path = css_path,
    print_landscape = crosstab_results_landscape(result),
    report_mode = report_mode
  )
}

result_accumulator_store <- function(session) {
  store <- session$userData$result_entries
  if (is.null(store) || !is.function(store)) {
    restored <- tryCatch(read_result_snapshot_store(), error = function(error) {
      session$userData$result_restore_error <- conditionMessage(error)
      list()
    })
    store <- reactiveVal(restored)
    session$userData$result_entries <- store
  }
  store
}

clear_result_accumulator_store <- function(session, persist = TRUE) {
  store <- result_accumulator_store(session)
  store(list())
  if (isTRUE(persist)) {
    write_result_snapshot_store(list())
  }
  invisible(TRUE)
}

result_snapshot_store_path <- function() {
  configured <- trimws(Sys.getenv("STATEDU_RESULT_STORE", ""))
  if (nzchar(configured)) {
    return(configured)
  }
  user_data <- trimws(Sys.getenv("STATEDU_USER_DATA_DIR", ""))
  if (nzchar(user_data)) {
    return(file.path(user_data, "data", "StatEdu_Studio_results.json"))
  }
  file.path("data", "StatEdu_Studio_results.json")
}

legacy_result_snapshot_store_path <- function() {
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
  normalized <- Filter(Negate(is.null), normalized)
  if (length(normalized)) {
    ids <- make.unique(vapply(normalized, `[[`, character(1), "id"), sep = "_")
    for (index in seq_along(normalized)) normalized[[index]]$id <- ids[[index]]
  }
  normalized
}

result_export_error_text <- function(error, language) {
  message <- conditionMessage(error)
  figure_keys <- paste0("result.figure_error.", c("prefix", "empty", "filename", "snapshot", "image", "folder", "no_figures", "format"))
  figure_keys <- c(figure_keys, paste0("survival.input_error.", c("ggplot_survival", "ggplot_forest", "ggplot_survival_export", "ggplot_cox_export")))
  figure_english <- vapply(figure_keys, function(key) statedu_t(key, "en"), character(1))
  figure_index <- match(message, figure_english)
  if (!is.na(figure_index)) return(statedu_t(figure_keys[[figure_index]], language))
  if (identical(message, "저장할 그림이 없습니다. / No figures to save.")) return(statedu_t("result.figure_error.no_figures", language))
  patterns <- c(
    length = "^Invalid PNG snapshot: ([A-Za-z0-9_-]+[.]png) [(]encoded length ([0-9]+)[)]$",
    named = "^Invalid PNG snapshot: ([A-Za-z0-9_-]+[.]png)$",
    decode = "^Could not decode PNG snapshot: ([A-Za-z0-9_-]+[.]png)$")
  for (key in names(patterns)) {
    matched <- regmatches(message, regexec(patterns[[key]], message))[[1]]
    if (length(matched)) return(do.call(sprintf, c(list(statedu_t(paste0("result.figure_error.", key), language)), as.list(matched[-1L]))))
  }
  keys <- paste0("result.export_error.", c("no_content", "no_tables", "excel_package", "browser", "pdf_path", "office_browser", "pdf_failed"))
  english <- vapply(keys, function(key) statedu_t(key, "en"), character(1))
  index <- match(message, english)
  if (!is.na(index)) return(statedu_t(keys[[index]], language))
  # PDF subprocess diagnostics follow a fixed first line; preserve their bytes.
  prefix <- paste0(statedu_t("result.export_error.pdf_failed", "en"), "\n")
  if (startsWith(message, prefix)) {
    return(paste0(statedu_t("result.export_error.pdf_failed", language), "\n",
      substring(message, nchar(prefix) + 1L, nchar(message))))
  }
  message
}

result_history_error_text <- function(error, language) {
  message <- conditionMessage(error)
  keys <- paste0("result.history_error.", c("read", "structure", "type", "entries", "migration"))
  english <- vapply(keys, function(key) statedu_t(key, "en"), character(1))
  index <- match(message, english)
  if (is.na(index)) message else statedu_t(keys[[index]], language)
}

read_result_snapshot_store <- function(path = result_snapshot_store_path()) {
  migration_target <- NULL
  if (!file.exists(path)) {
    default_path <- result_snapshot_store_path()
    legacy_paths <- c(file.path("data", "StatEdu_Studio_results.json"), legacy_result_snapshot_store_path())
    legacy_paths <- legacy_paths[file.exists(legacy_paths)]
    if (identical(path, default_path) && !nzchar(trimws(Sys.getenv("STATEDU_RESULT_STORE", ""))) && length(legacy_paths)) {
      migration_target <- path
      path <- legacy_paths[[1L]]
    } else {
      return(list())
    }
  }
  if (!file.exists(path)) {
    return(list())
  }
  payload <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) {
    stop("The saved result history could not be read; the original file was preserved.", call. = FALSE)
  })
  if (!is.list(payload)) stop("Invalid result history structure.", call. = FALSE)
  type <- as.character(payload$type %||% "")
  if (nzchar(type) && !identical(type, "easyflow_result_history")) {
    stop("This file is not a StatEdu Studio Result file.", call. = FALSE)
  }
  entries <- if (is.list(payload) && !is.null(payload$entries)) payload$entries else payload
  if (!is.list(entries)) stop("Invalid result history entries.", call. = FALSE)
  entries <- normalize_result_snapshot_entries(entries)
  if (!is.null(migration_target)) {
    dir.create(dirname(migration_target), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(path, migration_target, overwrite = FALSE)) {
      stop("The saved result history could not be migrated; the original file was preserved.", call. = FALSE)
    }
  }
  entries
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
      if (file.exists(path)) {
        readable <- tryCatch({ read_result_snapshot_store(path); TRUE }, error = function(error) FALSE)
        if (!readable) {
          backup <- tempfile(paste0(basename(path), ".unreadable-"), tmpdir = dirname(path))
          if (!file.copy(path, backup, overwrite = FALSE)) return(FALSE)
        }
      }
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
    id = paste0("saved_result_", basename(tempfile())),
    title = title,
    saved_at = saved_at,
    html = html
  )
  updated <- c(entries, list(entry))
  store(updated)
  write_result_snapshot_store(updated)
  entry
}

result_collection_edit <- function(entries, id, action) {
  matches <- which(vapply(entries, function(entry) identical(entry$id, id), logical(1)))
  if (length(matches) != 1L) return(entries)
  index <- matches[[1L]]
  if (identical(action, "delete")) return(entries[-index])
  target <- switch(action, up = index - 1L, down = index + 1L, index)
  if (target < 1L || target > length(entries) || target == index) return(entries)
  order <- seq_along(entries)
  order[c(index, target)] <- order[c(target, index)]
  entries[order]
}

saved_result_entry_ui <- function(entry, index, total = index, language = "ko") {
  control <- function(action, label, disabled = FALSE) {
    payload <- jsonlite::toJSON(list(id = entry$id, action = action), auto_unbox = TRUE)
    tags$button(type = "button", class = paste("btn btn-default btn-sm saved-result-entry-action", paste0("saved-result-entry-", action)),
      disabled = if (disabled) "disabled" else NULL,
      `aria-label` = paste(entry$title, label), title = label,
      onclick = paste0("Shiny.setInputValue('saved_result_entry_action',", payload, ",{priority:'event'});"), label)
  }
  div(
    class = "saved-result-entry",
    `data-result-entry-id` = entry$id,
    div(
      class = "saved-result-entry-header",
      div(
        class = "saved-result-title",
        span(sprintf("%02d", index), class = "saved-result-index"),
        span(entry$title)
      ),
      div(entry$saved_at, class = "saved-result-time"),
      div(class = "saved-result-entry-actions",
        control("up", statedu_t("result.management.move_up", language), index == 1L),
        control("down", statedu_t("result.management.move_down", language), index == total),
        control("delete", statedu_t("result.management.delete_entry", language)))
    ),
    tags$iframe(
      class = "saved-result-frame",
      title = entry$title,
      srcdoc = entry$html
    )
  )
}

result_entry_document <- function(entry) {
  document <- tryCatch(
    xml2::read_html(entry$html, options = c("RECOVER", "NOERROR", "NOWARNING")),
    error = function(e) NULL
  )
  if (is.null(document)) return(NULL)
  nodes <- xml2::xml_find_all(document, ".//body//*")
  if (length(nodes)) xml2::xml_set_attr(nodes, "data-result-export-order", as.character(seq_along(nodes)))
  document
}

result_node_order <- function(node) as.numeric(xml2::xml_attr(node, "data-result-export-order"))

result_node_html <- function(node) {
  # libxml's HTML URI escaping can fail on large embedded PNGs. Serialize
  # placeholders, then restore the same URI escaping without resampling images.
  images <- xml2::xml_find_all(node, ".//img | self::img")
  sources <- xml2::xml_attr(images, "src")
  selected <- which(!is.na(sources) & nchar(sources) > 65536L &
    grepl("^data:image/(png|jpeg);base64,[A-Za-z0-9+/=\r\n]+$", sources))
  if (!length(selected)) return(as.character(node))
  images <- images[selected]
  original <- sources[selected]
  on.exit(xml2::xml_set_attr(images, "src", original), add = TRUE)
  tokens <- paste0("STATEDU_IMAGE_URI_", seq_along(selected), "_END")
  while (any(tokens %in% sources)) tokens <- paste0(tokens, "_")
  xml2::xml_set_attr(images, "src", tokens)
  html <- as.character(node)
  for (index in seq_along(tokens)) {
    uri <- gsub("\r", "%0D", original[[index]], fixed = TRUE)
    uri <- gsub("\n", "%0A", uri, fixed = TRUE)
    html <- gsub(paste0('src="', tokens[[index]], '"'), paste0('src="', uri, '"'), html, fixed = TRUE)
  }
  html
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
  paste(vapply(xml2::xml_children(body), result_node_html, character(1)), collapse = "\n")
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
    "StatEdu Studio Result Collection",
    result_collection_content(entries),
    max_width = 1500,
    css_path = css_path,
    print_landscape = TRUE,
    report_mode = report_mode
  )
}

# Apply navigation only at the HTML file boundary. Stored snapshots and the
# documents consumed by PDF, Word, HWPX and Excel must remain undecorated.
result_html_export_document <- function(html, language = statedu_current_language()) {
  document <- xml2::read_html(html, options = c("RECOVER", "NOERROR", "NOWARNING"))
  xml2::xml_remove(xml2::xml_find_all(document, "//*[@data-statedu-html-navigation]"))
  body <- xml2::xml_find_first(document, ".//body")
  head <- xml2::xml_find_first(document, ".//head")
  tables <- xml2::xml_find_all(body, ".//table")
  text <- function(key, fallback) statedu_t(paste0("report.cover.", key), language, fallback)
  used_ids <- xml2::xml_attr(xml2::xml_find_all(document, "//*[@id]"), "id")
  fresh_id <- function(base) {
    id <- base
    while (id %in% used_ids) id <- paste0(id, "-")
    used_ids <<- c(used_ids, id)
    id
  }
  cover_id <- fresh_id("statedu-html-cover")
  links <- lapply(seq_along(tables), function(index) {
    table <- tables[[index]]
    id <- fresh_id(paste0("statedu-html-table-", index))
    caption <- xml2::xml_find_first(table, "./caption")
    label <- if (!inherits(caption, "xml_missing")) result_html_text(caption) else ""
    if (!nzchar(label)) label <- result_table_title(table, paste(text("table", "Table"), index))
    navigation <- tags$div(id = id, class = "html-table-navigation",
      `data-statedu-html-navigation` = "table",
      tags$a(href = paste0("#", cover_id), text("back_to_cover", "Back to cover")))
    node <- xml2::xml_find_first(xml2::read_html(as.character(navigation)), ".//body/*")
    xml2::xml_add_sibling(table, node, .where = "before")
    tags$li(tags$a(href = paste0("#", id), label))
  })
  title <- xml2::xml_text(xml2::xml_find_first(head, "./title"))
  if (is.na(title) || !nzchar(title)) title <- "StatEdu Studio Result Collection"
  cover <- saved_results_report_cover(title, language)
  cover$attribs$id <- cover_id
  cover$attribs$`data-statedu-html-navigation` <- "cover"
  contents_id <- fresh_id("statedu-html-contents")
  if (length(links)) cover$children <- c(cover$children, list(
    tags$div(class = "html-cover-contents-link",
      tags$a(href = paste0("#", contents_id), text("table_list", "List of tables")))))
  cover_node <- xml2::xml_find_first(xml2::read_html(as.character(cover)), ".//body/*")
  shell <- xml2::xml_find_first(body, ".//*[contains(concat(' ', normalize-space(@class), ' '), ' page-shell ')]")
  if (inherits(shell, "xml_missing")) shell <- body
  shell_class <- xml2::xml_attr(shell, "class")
  if (is.na(shell_class)) shell_class <- ""
  if (!grepl("(^| )html-result-document( |$)", shell_class)) {
    xml2::xml_set_attr(shell, "class", trimws(paste(shell_class, "html-result-document")))
  }
  inserted_cover <- xml2::xml_add_child(shell, cover_node, .where = 0)
  # The number of tables must not compress the cover's vertical spacing.
  # Keep the complete linked contents in the next document section.
  if (length(links)) {
    contents <- tags$nav(id = contents_id, class = "html-table-contents",
      `data-statedu-html-navigation` = "contents",
      `aria-label` = text("table_list", "List of tables"),
      tags$h2(text("table_list", "List of tables")), tags$ol(links))
    contents_node <- xml2::xml_find_first(xml2::read_html(as.character(contents)), ".//body/*")
    xml2::xml_add_sibling(inserted_cover, contents_node, .where = "after")
  }
  css <- paste(saved_results_report_cover_css(),
    ".html-result-document { box-sizing:border-box; background:#fff; border:1px solid #d9e2ec; border-radius:8px; padding:32px !important; }",
    ".html-result-document .page-shell { max-width:100%; margin:0; padding:0; }",
    ".html-result-document > .report-cover[data-statedu-html-navigation] { width:100%; max-width:170mm; height:auto; min-height:243mm; overflow:visible; margin:0 0 32px; padding:0 0 28px; border:0; border-bottom:2px solid #102a43; background:transparent; box-shadow:none; border-radius:0; box-sizing:border-box; display:flex; flex-direction:column; justify-content:space-between; }",
    ".html-result-document > .report-cover::before { display:none; }",
    ".html-result-document .report-cover-main { max-width:none; padding:56px 0 40px; }",
    ".html-result-document .report-cover-footer { overflow-wrap:anywhere; }",
    "@media screen and (max-width:640px) { .html-result-document { margin:12px 8px; padding:18px !important; } .html-result-document .report-cover-brand { flex-wrap:wrap; gap:12px; } .html-result-document .report-cover-logo { max-width:100%; } .html-result-document .report-cover-meta { grid-template-columns:1fr; } }",
    ".html-table-contents { max-width:170mm; margin:0 0 40px; padding:0 0 28px; border-bottom:1px solid #bcccdc; scroll-margin-top:24px; }",
    ".html-table-contents h2 { font-size:18px; } .html-table-contents li { margin:8px 0; overflow-wrap:anywhere; }",
    ".html-table-contents a, .html-table-navigation a, .html-cover-contents-link a { color:#12659b; text-decoration:underline; }",
    ".html-table-navigation { text-align:right; margin:8px 0; font-size:12px; scroll-margin-top:24px; }",
    "@media print { .html-table-navigation { display:none; } .report-cover[data-statedu-html-navigation] { break-after:page; } }",
    sep = "\n")
  style <- xml2::xml_add_child(head, "style", css)
  xml2::xml_set_attr(style, "data-statedu-html-navigation", "style")
  result_node_html(document)
}

write_result_html_document <- function(text, con, useBytes = TRUE, language = statedu_current_language()) {
  writeLines(result_html_export_document(text, language), con, useBytes = useBytes)
}

write_result_collection_html <- function(entries, file) {
  write_result_html_document(saved_result_collection_html(entries), file, useBytes = TRUE)
  invisible(file)
}

saved_result_sheet_document <- function(title, content, css_path = file.path("www", "style.css")) {
  # Reuse the displayed table nodes and their CSS; do not rebuild report tables.
  document <- xml2::read_html(tags_to_html(content))
  sort_buttons <- xml2::xml_find_all(document, ".//button[contains(concat(' ', normalize-space(@class), ' '), ' ancova-sort-button ')]")
  for (button in sort_buttons) xml2::xml_name(button) <- "span"
  # Keep each model's Q-Q and residual-variance plots together, including its heading.
  plot_groups <- xml2::xml_find_all(document, ".//div[contains(concat(' ', normalize-space(@class), ' '), ' diagnostic-plots-section ')][.//div[contains(concat(' ', normalize-space(@class), ' '), ' residual-diagnostic-plots ')]]")
  for (group in plot_groups) {
    if (length(xml2::xml_find_all(group, ".//img")) == 2L) {
      xml2::xml_set_attr(group, "data-result-table-sheet", "true")
      xml2::xml_set_attr(group, "data-result-table-orientation", "portrait")
      xml2::xml_set_attr(group, "data-pdf-residual-pair", "true")
    }
  }
  text_xpath <- paste0(
    ".//*[self::h1 or self::h2 or self::h3 or self::h4 or self::h5 or self::h6 or self::p or self::li or self::dt or self::dd or self::div]",
    "[not(ancestor::table or ancestor::*[@data-result-table-sheet='true'])]",
    "[not(ancestor-or-self::*[contains(concat(' ',normalize-space(@class),' '),' mm-result-diagram-section ')])]",
    "[not(.//table or .//img or .//div or .//p or .//li or .//h1 or .//h2 or .//h3 or .//h4 or .//h5 or .//h6)]"
  )
  sheets <- xml2::xml_find_all(document, paste(
    ".//*[@data-result-table-sheet='true' and not(ancestor::*[@data-result-table-sheet='true'])]",
    ".//table[not(ancestor::*[@data-result-table-sheet='true'])]",
    ".//img[not(ancestor::*[@data-result-table-sheet='true'])]",
    ".//div[contains(concat(' ', normalize-space(@class), ' '), ' mm-result-diagram-section ')]",
    text_xpath, sep = " | "))
  if (length(sheets) == 0L) stop("No displayed result tables or figures are available to export.")
  fragments <- list()
  pending <- list()
  pages <- list()
  orientation <- "portrait"
  wrap_fragment <- function(sheet) {
    node <- sheet
    fragment <- htmltools::HTML(result_node_html(sheet))
    repeat {
      parent <- xml2::xml_parent(node)
      if (xml2::xml_name(parent) %in% c("body", "html") || inherits(parent, "xml_missing")) break
      fragment <- tags$div(
        class = xml2::xml_attr(parent, "class"),
        fragment
      )
      node <- parent
    }
    fragment
  }
  flush_page <- function() {
    if (!length(fragments)) return()
    pages[[length(pages) + 1L]] <<- tags$section(
      class = paste("statedu-output-page", paste0("statedu-output-page--", orientation)),
      `data-orientation` = orientation, tags$div(class = "statedu-output-content", fragments))
    fragments <<- list()
  }
  for (sheet in sheets) {
    is_content <- xml2::xml_name(sheet) %in% c("table", "img") ||
      identical(xml2::xml_attr(sheet, "data-result-table-sheet"), "true") ||
      grepl("mm-result-diagram-section", xml2::xml_attr(sheet, "class") %||% "", fixed = TRUE)
    if (is.na(is_content)) is_content <- FALSE
    if (!is_content) {
      if (!nzchar(result_html_text(sheet))) next
      if (grepl("^h[1-6]$", xml2::xml_name(sheet)) || length(pending) || !length(fragments)) {
        pending[[length(pending) + 1L]] <- wrap_fragment(sheet)
      } else fragments[[length(fragments) + 1L]] <- wrap_fragment(sheet)
      next
    }
    flush_page()
    orientation <- xml2::xml_attr(sheet, "data-result-table-orientation")
    if (is.na(orientation) || !orientation %in% c("portrait", "landscape")) orientation <- "portrait"
    fragments <- c(pending, list(wrap_fragment(sheet)))
    pending <- list()
  }
  fragments <- c(fragments, pending)
  flush_page()
  css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")
  canvas_css <- file.path(dirname(css_path), "model-canvas", "canvas.css")
  if (file.exists(canvas_css)) css <- paste(css, paste(readLines(canvas_css, warn = FALSE), collapse = "\n"), sep = "\n")
  pagination <- paste0(
    "@page statedu-output-portrait {size:B5 portrait;margin:3mm;}",
    "@page statedu-output-landscape {size:B5 landscape;margin:3mm;}",
    "html,body{margin:0;padding:0;background:white;}",
    ".statedu-output-page{break-after:auto;break-inside:avoid;margin-bottom:12px;}",
    ".statedu-output-page--portrait{page:statedu-output-portrait;width:170mm;}",
    ".statedu-output-page--landscape{page:statedu-output-landscape;width:244mm;break-before:page;break-after:page;}",
    ".statedu-output-page--landscape:last-child{break-after:auto;}",
    ".statedu-output-page .page-shell{width:100%;max-width:100%;margin:0;padding:0;}",
    ".statedu-output-page .regression-result-panel,.statedu-output-page .regression-results > .regression-result-panel{zoom:1!important;overflow:visible!important;margin:0;}",
    ".statedu-output-page [data-result-table-sheet='true']{page:auto!important;break-before:auto!important;page-break-before:auto!important;break-after:auto!important;page-break-after:auto!important;overflow:visible!important;}",
    ".statedu-output-page .regression-result-panel,.statedu-output-page .result-section{page:auto!important;break-before:auto!important;page-break-before:auto!important;break-after:auto!important;page-break-after:auto!important;}",
    ".statedu-output-page .statedu-output-content{display:block;}",
    ".statedu-output-page .statedu-output-content div{overflow:visible!important;}",
    ".statedu-output-page .statedu-output-content div{border:0!important;border-radius:0!important;box-shadow:none!important;}",
    ".statedu-output-page .regression-results,.statedu-output-page .regression-result-panel,.statedu-output-page .result-section{width:100%!important;min-width:0!important;max-width:none!important;box-sizing:border-box;padding:0!important;}",
    ".statedu-output-page .result-section.regression-result-panel:has(.result-table-sheet),.statedu-output-page .statedu-output-content .regression-results > .regression-result-panel{width:100%!important;min-width:0!important;max-width:none!important;padding:0!important;}",
    ".statedu-output-page .regression-results{display:block!important;gap:0!important;}",
    ".statedu-output-page img{max-width:100%!important;height:auto!important;}",
    ".statedu-output-page:has([data-pdf-residual-pair]){break-before:page;break-after:page;break-inside:avoid;}",
    ".statedu-output-page [data-pdf-residual-pair]{width:100%!important;max-width:100%!important;box-sizing:border-box;}",
    ".statedu-output-page [data-pdf-residual-pair] .residual-diagnostic-plots{display:flex!important;flex-direction:column;gap:3mm;}",
    ".statedu-output-page [data-pdf-residual-pair] .residual-plot-card{padding:2mm!important;margin:0!important;break-inside:avoid;}",
    ".statedu-output-page [data-pdf-residual-pair] h4{font-size:10pt!important;margin:0 0 1mm!important;}",
    ".statedu-output-page [data-pdf-residual-pair] .shiny-plot-output{width:100%!important;height:auto!important;min-height:0!important;}",
    ".statedu-output-page [data-pdf-residual-pair] img{display:block;width:auto!important;height:94mm!important;max-width:100%!important;object-fit:contain;margin:auto;}",
    ".statedu-output-page .regression-results,.statedu-output-page .regression-result-panel{overflow:visible!important;}",
    ".statedu-output-page h3,.statedu-output-page h4{break-after:avoid;}",
    ".statedu-output-page thead{display:table-header-group;break-inside:avoid;}",
    ".statedu-output-page .ancova-sort-button{display:inline!important;}",
    ".statedu-output-page tfoot{display:table-row-group;}",
    ".statedu-output-page tr{break-inside:avoid;}",
    ".statedu-output-page--long,.statedu-output-page--long .statedu-output-content,.statedu-output-page--long .page-shell,.statedu-output-page--long .regression-results,.statedu-output-page--long .regression-result-panel,.statedu-output-page--long .result-table-sheet,.statedu-output-page--long table,.statedu-output-page--long tbody{break-inside:auto!important;}",
    "@media print{body{-webkit-print-color-adjust:exact;print-color-adjust:exact;}}"
  )
  # Fit only horizontal overflow to B5; never shrink a table to fit its height.
  fit <- "function prepareResultPages(){document.querySelectorAll('.statedu-output-page').forEach(function(p){var c=p.firstElementChild;c.style.zoom=1;var s=Math.min(1,p.getBoundingClientRect().width/c.scrollWidth);c.style.zoom=s;var h=(p.dataset.orientation==='landscape'?170:244)*96/25.4;p.classList.toggle('statedu-output-page--long',c.scrollHeight*s>h);});}document.fonts.ready.then(prepareResultPages);window.addEventListener('beforeprint',prepareResultPages);"
  tags_to_html(tags$html(tags$head(tags$meta(charset = "UTF-8"), tags$title(title),
    tags$style(htmltools::HTML(css)), tags$style(htmltools::HTML(saved_results_report_cover_css())), tags$style(htmltools::HTML(pagination))),
    tags$body(saved_results_report_cover(title), pages, tags$script(htmltools::HTML(fit)))))
}

write_result_collection_pdf <- function(entries, file) {
  write_pdf_from_html(saved_result_sheet_document("StatEdu Studio", result_collection_content(entries)), file)
}

result_html_text <- function(node) {
  text <- tryCatch(xml2::xml_text(node, trim = TRUE), error = function(e) "")
  trimws(gsub("\\s+", " ", text))
}

result_table_title <- function(table_node, fallback) {
  heading <- xml2::xml_find_first(
    table_node,
    "preceding::*[self::h1 or self::h2 or self::h3 or self::h4 or self::h5 or self::h6][1]"
  )
  if (length(heading) == 0 || is.na(xml2::xml_name(heading))) {
    heading <- xml2::xml_find_first(
      table_node,
      "ancestor::*[contains(concat(' ', normalize-space(@class), ' '), ' regression-result-panel ')][1]//*[self::h1 or self::h2 or self::h3 or self::h4 or self::h5][1]"
    )
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
  occupied <- list()
  source_cells <- list()
  header_rows <- length(xml2::xml_find_all(table_node, "./thead/tr"))
  max_col <- 0L
  for (row_index in seq_along(rows)) {
    cells <- xml2::xml_find_all(rows[[row_index]], "./th|./td")
    if (length(cells) == 0) next
    # The text helper preserves each cell's whitespace rules when given a nodeset.
    cell_values <- result_html_text(cells)
    has_superscripts <- !inherits(xml2::xml_find_first(rows[[row_index]], ".//sup"), "xml_missing")
    if (length(grid) < row_index) {
      length(grid) <- row_index
    }
    if (is.null(grid[[row_index]])) {
      grid[[row_index]] <- character(0)
    }
    if (length(occupied) < row_index) length(occupied) <- row_index
    col_index <- 1L
    for (cell_index in seq_along(cells)) {
      cell <- cells[[cell_index]]
      while (
        length(occupied[[row_index]]) >= col_index &&
          isTRUE(occupied[[row_index]][[col_index]])
      ) {
        col_index <- col_index + 1L
      }
      colspan <- suppressWarnings(as.integer(xml2::xml_attr(cell, "colspan") %||% "1"))
      rowspan <- suppressWarnings(as.integer(xml2::xml_attr(cell, "rowspan") %||% "1"))
      if (is.na(colspan) || colspan < 1L) colspan <- 1L
      if (is.na(rowspan) || rowspan < 1L) rowspan <- 1L
      value <- cell_values[[cell_index]]
      line_nodes <- xml2::xml_find_all(cell, ".//*[contains(concat(' ', normalize-space(@class), ' '), ' coefficient-cell-break ')]/*")
      if (length(line_nodes) > 1L) value <- paste(result_html_text(line_nodes), collapse = "\n")
      superscript <- if (has_superscripts) paste(xml2::xml_text(xml2::xml_find_all(cell, ".//sup")), collapse = " ") else ""
      if (!nzchar(superscript) || !endsWith(trimws(value), superscript)) superscript <- ""
      source_cells[[length(source_cells) + 1L]] <- list(row = row_index, col = col_index,
        rowspan = rowspan, colspan = colspan, style = xml2::xml_attr(cell, "style"), superscript = superscript)
      for (row_offset in seq_len(rowspan) - 1L) {
        target_row <- row_index + row_offset
        if (length(grid) < target_row) {
          length(grid) <- target_row
        }
        if (length(occupied) < target_row) length(occupied) <- target_row
        if (is.null(grid[[target_row]])) {
          grid[[target_row]] <- character(0)
        }
        length(grid[[target_row]]) <- max(length(grid[[target_row]]), col_index + colspan - 1L)
        for (col_offset in seq_len(colspan) - 1L) {
          grid[[target_row]][[col_index + col_offset]] <- value
          occupied[[target_row]][col_index + col_offset] <- TRUE
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
  # Sample-size tables are key/value rows: th is a row label, not a column header.
  # In particular, a single Power row must remain a body row for editable exports.
  sample_size_table <- "sample-size-result-table" %in% strsplit(xml2::xml_attr(table_node, "class") %||% "", "\\s+")[[1]]
  if (header_rows == 0L && !sample_size_table) {
    first_row <- xml2::xml_find_all(rows[[1]], "./th")
    header_rows <- if (length(first_row) > 0) 1L else 0L
  }
  geometry <- function(attribute) tryCatch(as.numeric(jsonlite::fromJSON(xml2::xml_attr(table_node, attribute))), error = function(e) numeric(0))
  list(values = matrix_values, header_rows = min(header_rows, nrow(matrix_values)), cells = source_cells,
    column_widths = geometry("data-result-column-widths"), row_heights = geometry("data-result-row-heights"))
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
    } else if (label %in% c("t", "f", "t/f", "z", "w", "q") || grepl("statistic|hedges|cohen|effect|eta|omega|epsilon|cliff|f2|sr2|r$", label)) {
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

result_docx_table_payload <- function(table_node, parsed = result_html_table_cells(table_node)) {
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

result_entry_tables <- function(entry, entry_index = 1L, include_docx = TRUE,
                                document = result_entry_document(entry),
                                text_items = result_entry_paragraphs(entry, document = document)) {
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
    quiet_parse <- TRUE
    tables[[index]] <- list(
      output_order = result_node_order(table_nodes[[index]]),
      title = title,
      sheet_name = excel_sheet_name(sprintf("%02d %s", entry_index, title)),
      table = table,
      class = xml2::xml_attr(table_nodes[[index]], "class") %||% "",
      context_class = context_class,
      orientation = xml2::xml_attr(xml2::xml_find_first(table_nodes[[index]],
        "ancestor-or-self::*[@data-result-table-orientation][1]"), "data-result-table-orientation"),
      screen = parsed <- withCallingHandlers(result_html_table_cells(table_nodes[[index]]),
        warning = function(w) quiet_parse <<- FALSE, message = function(m) quiet_parse <<- FALSE),
      # Reuse this table's quiet parse; preserve repeated diagnostics otherwise.
      docx = if (!include_docx) NULL
        else if (quiet_parse) result_docx_table_payload(table_nodes[[index]], parsed = parsed)
        else result_docx_table_payload(table_nodes[[index]]),
      notes = notes
    )
  }
  tables <- Filter(Negate(is.null), tables)
  orders <- vapply(tables, `[[`, numeric(1), "output_order")
  if (length(tables)) {
    for (index in seq_along(tables)) {
      tables[[index]]$headings <- character(0)
      tables[[index]]$before_text <- character(0)
      tables[[index]]$after_text <- character(0)
    }
    last_heading <- -Inf
    for (item in text_items) {
      if (isTRUE(item$heading)) last_heading <- item$output_order
      preceding <- which(orders < item$output_order)
      following <- which(orders > item$output_order)
      previous <- if (length(preceding)) tail(preceding, 1L) else NA_integer_
      next_index <- if (length(following)) following[[1L]] else NA_integer_
      before <- !is.na(next_index) && (isTRUE(item$heading) || is.na(previous) || last_heading > orders[[previous]])
      owner <- if (before) next_index else previous
      if (is.na(owner)) next
      key <- if (before) "before_text" else "after_text"
      tables[[owner]][[key]] <- c(tables[[owner]][[key]], item$text)
      if (isTRUE(item$heading)) tables[[owner]]$headings <- c(tables[[owner]]$headings, item$text)
    }
  }
  tables
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
  contents <- list()
  image_paths <- character(0)
  on.exit(unlink(image_paths), add = TRUE)
  for (entry_index in seq_along(entries)) {
    # Excel uses captured screen cells; Word's normalized payload is unnecessary.
    document <- result_entry_document(entries[[entry_index]])
    tables <- result_entry_tables(entries[[entry_index]], entry_index, include_docx = FALSE,
      document = document)
    images <- result_entry_images(entries[[entry_index]])
    image_paths <- c(image_paths, vapply(images, `[[`, character(1), "path"))
    items <- c(tables, images)
    items <- items[order(vapply(items, `[[`, numeric(1), "output_order"))]
    for (figure in items) {
      if (is.null(figure$path)) {
        used_sheets <- add_screen_excel_table(workbook, figure, used_sheets)
        contents[[length(contents) + 1L]] <- list(sheet = tail(used_sheets, 1L), title = figure$title,
          result = entries[[entry_index]]$title, link_row = max(1L, length(figure$before_text)) + 1L,
          link_col = 1L, link_span = ncol(figure$screen$values))
        next
      }
      sheet <- excel_sheet_name(figure$title, used_sheets)
      openxlsx::addWorksheet(workbook, sheet, gridLines = FALSE)
      openxlsx::writeData(workbook, sheet, figure$title, colNames = FALSE)
      dimensions <- result_docx_image_dimensions(figure)
      openxlsx::insertImage(workbook, sheet, figure$path, startRow = 3, startCol = 1,
        width = dimensions$width, height = dimensions$height, units = "in")
      openxlsx::pageSetup(workbook, sheet, orientation = "portrait", paperSize = 13,
        fitToWidth = TRUE, fitToHeight = TRUE)
      used_sheets <- c(used_sheets, sheet)
      contents[[length(contents) + 1L]] <- list(sheet = sheet, title = figure$title,
        result = entries[[entry_index]]$title, link_row = 2L, link_col = 1L, link_span = 4L)
    }
    displayed_text <- vapply(result_entry_paragraphs(entries[[entry_index]], document = document), `[[`, character(1), "text")
    table_notes <- unlist(lapply(tables, function(table) c(table$notes, table$before_text, table$after_text)), use.names = FALSE)
    displayed_text <- displayed_text[!displayed_text %in% table_notes]
    if (length(displayed_text)) {
      sheet <- excel_sheet_name(paste0(entries[[entry_index]]$title, " notes"), used_sheets)
      openxlsx::addWorksheet(workbook, sheet, gridLines = FALSE)
      openxlsx::writeData(workbook, sheet, data.frame(Text = displayed_text), colNames = FALSE)
      openxlsx::setColWidths(workbook, sheet, 1L, 85)
      openxlsx::addStyle(workbook, sheet, openxlsx::createStyle(wrapText = TRUE, valign = "top"),
        rows = seq_along(displayed_text), cols = 1L, gridExpand = TRUE)
      openxlsx::pageSetup(workbook, sheet, orientation = "portrait", paperSize = 13, fitToWidth = TRUE)
      used_sheets <- c(used_sheets, sheet)
      contents[[length(contents) + 1L]] <- list(sheet = sheet, title = paste(entries[[entry_index]]$title, "—", "Notes / 설명"),
        result = entries[[entry_index]]$title)
    }
  }
  if (!length(used_sheets)) stop("No displayed result tables are available to export.")
  add_result_excel_cover(workbook, contents)
  openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
  result_finalize_excel_package(file)
  invisible(file)
}

result_finalize_excel_package <- function(file) {
  # openxlsx can emit unused drawing references and an A1-only dimension.
  # Repair metadata only; preserve actual cells, styles, merges and drawings.
  archive <- normalizePath(file, winslash = "/", mustWork = TRUE)
  directory <- tempfile("result-xlsx-"); dir.create(directory)
  packed <- tempfile(fileext = ".xlsx")
  on.exit({unlink(directory, recursive = TRUE); unlink(packed)}, add = TRUE)
  zip::unzip(archive, exdir = directory)
  sheets <- list.files(file.path(directory, "xl", "worksheets"), pattern = "^sheet[0-9]+[.]xml$", full.names = TRUE)
  for (sheet in sheets) {
    doc <- xml2::read_xml(sheet)
    # Store workbook navigation as native internal hyperlinks. This keeps link
    # labels visible without formula recalculation and supports Excel Follow.
    for (formula in xml2::xml_find_all(doc, "//*[local-name()='c']/*[local-name()='f']")) {
      match <- regmatches(xml2::xml_text(formula), regexec('^HYPERLINK\\("((?:[^"]|"")*)","((?:[^"]|"")*)"\\)$', xml2::xml_text(formula), perl = TRUE))[[1]]
      if (length(match) != 3L || !startsWith(match[[2]], "#")) next
      target <- substring(gsub('""', '"', match[[2]], fixed = TRUE), 2L)
      label <- gsub('""', '"', match[[3]], fixed = TRUE)
      cell <- xml2::xml_parent(formula)
      links <- xml2::xml_find_first(doc, "/*/*[local-name()='hyperlinks']")
      if (inherits(links, "xml_missing")) {
        anchor <- xml2::xml_find_first(doc, "/*/*[local-name()='printOptions' or local-name()='pageMargins' or local-name()='pageSetup' or local-name()='headerFooter' or local-name()='drawing'][1]")
        links <- if (inherits(anchor, "xml_missing")) xml2::xml_add_child(xml2::xml_root(doc), "hyperlinks")
          else xml2::xml_add_sibling(anchor, "hyperlinks", .where = "before")
      }
      xml2::xml_add_child(links, "hyperlink", ref = xml2::xml_attr(cell, "r"), location = target, display = label)
      xml2::xml_remove(xml2::xml_children(cell))
      xml2::xml_set_attr(cell, "t", "inlineStr")
      xml2::xml_add_child(xml2::xml_add_child(cell, "is"), "t", label)
    }
    relfile <- file.path(dirname(sheet), "_rels", paste0(basename(sheet), ".rels"))
    if (file.exists(relfile)) {
      rels <- xml2::read_xml(relfile)
      for (rel in xml2::xml_find_all(rels, "//*[local-name()='Relationship']")) {
        if (identical(xml2::xml_attr(rel, "TargetMode"), "External")) next
        type <- xml2::xml_attr(rel, "Type"); target <- xml2::xml_attr(rel, "Target")
        if (is.na(type) || is.na(target) || !grepl("/(drawing|vmlDrawing)$", type)) next
        path <- if (startsWith(target, "/")) file.path(directory, substring(target, 2)) else file.path(dirname(sheet), target)
        if (!file.exists(path)) {
          id <- xml2::xml_attr(rel, "Id")
          nodes <- xml2::xml_find_all(doc, "//*[local-name()='drawing' or local-name()='legacyDrawing' or local-name()='legacyDrawingHF']")
          for (node in nodes) {
            attrs <- xml2::xml_attrs(node)
            if (any(unname(attrs) == id)) xml2::xml_remove(node)
          }
          xml2::xml_remove(rel)
        }
      }
      xml2::write_xml(rels, relfile)
    }
    cells <- xml2::xml_attr(xml2::xml_find_all(doc, "//*[local-name()='sheetData']//*[local-name()='c']"), "r")
    merges <- xml2::xml_attr(xml2::xml_find_all(doc, "//*[local-name()='mergeCell']"), "ref")
    refs <- c(cells, unlist(strsplit(merges, ":", fixed = TRUE)))
    refs <- refs[!is.na(refs) & grepl("^[A-Z]+[0-9]+$", refs)]
    if (length(refs)) {
      maxrow <- max(as.integer(sub("^[A-Z]+", "", refs)))
      maxcol <- max(openxlsx::convertFromExcelRef(refs))
      dimension <- xml2::xml_find_first(doc, "/*/*[local-name()='dimension']")
      if (!inherits(dimension, "xml_missing")) xml2::xml_set_attr(dimension, "ref", paste0("A1:", openxlsx::int2col(maxcol), maxrow))
    }
    xml2::write_xml(doc, sheet)
  }
  zip::zipr(packed, list.files(directory, recursive = TRUE, all.files = TRUE, no.. = TRUE), root = directory, include_directories = FALSE, mode = "mirror")
  if (!file.copy(packed, archive, overwrite = TRUE)) stop("Could not finalize Excel package.")
  invisible(file)
}

save_screen_excel_file <- function(html, file) {
  save_result_collection_excel_file(list(list(title = "Results", html = html)), file)
}

add_screen_excel_table <- function(workbook, info, used_sheets) {
  sheet <- excel_sheet_name(info$title, used_sheets)
  source <- info$screen
  values <- source$values
  nr <- nrow(values); nc <- ncol(values)
  wide <- result_docx_wide_table(info)
  openxlsx::addWorksheet(workbook, sheet, gridLines = FALSE)
  leading <- info$before_text %||% character(0)
  if (!length(leading)) leading <- info$title
  for (index in seq_along(leading)) {
    openxlsx::writeData(workbook, sheet, leading[[index]], startRow = index, colNames = FALSE)
    if (nc > 1L) openxlsx::mergeCells(workbook, sheet, cols = seq_len(nc), rows = index)
    openxlsx::addStyle(workbook, sheet, openxlsx::createStyle(fontName = "Arial", fontSize = 11.25,
      wrapText = TRUE, textDecoration = if (leading[[index]] %in% c(info$headings, info$title)) "bold" else NULL,
      fontColour = "#1f2937"), rows = index, cols = seq_len(nc), gridExpand = TRUE)
  }
  offset <- length(leading) + 1L
  # Strings are intentional: preserve displayed precision, p-value thresholds and labels.
  # Write only source cells so merged continuations do not duplicate their contents.
  cell_rows <- vapply(source$cells, function(cell) cell$row, numeric(1))
  cell_cols <- vapply(source$cells, function(cell) cell$col, numeric(1))
  if (length(cell_rows)) {
    # Contiguous source cells on one row retain the original shared-string order.
    starts <- which(c(TRUE, diff(cell_rows) != 0 | diff(cell_cols) != 1))
    ends <- c(starts[-1L] - 1L, length(cell_rows))
    for (run in seq_along(starts)) {
      indices <- seq.int(starts[[run]], ends[[run]])
      row <- cell_rows[[starts[[run]]]]
      cols <- cell_cols[indices]
      openxlsx::writeData(workbook, sheet, matrix(values[row, cols], nrow = 1L),
        startRow = row + offset, startCol = cols[[1]], colNames = FALSE, rowNames = FALSE)
    }
  }
  # Reuse identical styles only within this sheet; apply them in source-cell order.
  style_cache <- list()
  pending_style <- NULL
  flush_style <- function() {
    if (!is.null(pending_style)) {
      openxlsx::addStyle(workbook, sheet, pending_style$style, rows = pending_style$row,
        cols = seq.int(pending_style$first, pending_style$last), gridExpand = TRUE, stack = TRUE)
      pending_style <<- NULL
    }
  }
  for (cell in source$cells) {
    rows <- seq.int(cell$row + offset, cell$row + offset - 1L + cell$rowspan)
    cols <- seq.int(cell$col, cell$col + cell$colspan - 1L)
    css <- cell$style
    if (is.na(css)) css <- ""
    header <- cell$row <= source$header_rows
    decoration <- c(if (header || grepl("font-weight:\\s*(bold|[6-9]00)", css)) "bold",
      if (grepl("font-style:\\s*italic", css)) "italic")
    align <- if (grepl("text-align:\\s*(left|start)", css)) "left" else if (grepl("text-align:\\s*(right|end)", css)) "right" else if (header || grepl("text-align:\\s*center", css)) "center" else "left"
    border <- if (cell$row == 1L) c("top", "bottom") else "bottom"
    dark <- header || max(rows) == nr + offset
    valign <- if (grepl("vertical-align:\\s*top", css)) "top" else "center"
    style_key <- paste(header, paste(decoration, collapse = ","), align, valign, cell$row == 1L, dark, sep = "|")
    style <- style_cache[[style_key]]
    if (is.null(style)) {
      style <- openxlsx::createStyle(fontName = "Arial", fontSize = if (header) 8.25 else 9,
        fontColour = "#2f3a46", textDecoration = if (length(decoration)) decoration else NULL,
        halign = align, valign = valign,
        wrapText = TRUE, numFmt = "TEXT", border = border,
        borderColour = if (dark) "#1f2937" else "#d7dde5", borderStyle = if (dark) "thin" else "hair")
      style_cache[[style_key]] <- style
    }
    # Batch only adjacent unmerged cells with identical styles on the same row.
    unmerged <- length(rows) == 1L && length(cols) == 1L
    if (unmerged && !is.null(pending_style) && identical(style_key, pending_style$key) &&
        rows == pending_style$row && cols == pending_style$last + 1L) {
      pending_style$last <- cols
    } else {
      flush_style()
      if (unmerged) {
        pending_style <- list(key = style_key, style = style, row = rows, first = cols, last = cols)
      } else {
        openxlsx::mergeCells(workbook, sheet, cols = cols, rows = rows)
        openxlsx::addStyle(workbook, sheet, style, rows = rows, cols = cols, gridExpand = TRUE, stack = TRUE)
      }
    }
  }
  flush_style()
  width_px <- if (wide) 890 else 590
  # Stable sheet columns with room for label columns and wrapped prose.
  weights <- vapply(seq_len(nc), function(j) min(40, max(8, max(nchar(values[,j], type = "width")))), numeric(1))
  fractions <- 0.6 / nc + 0.4 * weights / sum(weights)
  if (length(source$column_widths) == nc && all(is.finite(source$column_widths) & source$column_widths > 0)) fractions <- source$column_widths / sum(source$column_widths)
  widths <- pmax(4, (width_px * fractions - 5) / 7)
  openxlsx::setColWidths(workbook, sheet, cols = seq_len(nc), widths = widths)
  for (r in seq_len(nr)) {
    lines <- max(ceiling(nchar(values[r, ], type = "width") / pmax(1, widths - 2)))
    height <- if (length(source$row_heights) >= r && is.finite(source$row_heights[[r]]) && source$row_heights[[r]] > 0) source$row_heights[[r]] * .75 else max(19, lines * 12 + 6)
    openxlsx::setRowHeights(workbook, sheet, rows = r + offset, heights = height)
  }
  last <- nr + offset
  trailing <- c(info$after_text, info$notes[!info$notes %in% c(info$before_text, info$after_text)])
  for (note in trailing) {
    last <- last + 1L
    openxlsx::writeData(workbook, sheet, note, startRow = last, colNames = FALSE)
    if (nc > 1L) openxlsx::mergeCells(workbook, sheet, cols = seq_len(nc), rows = last)
    openxlsx::addStyle(workbook, sheet, openxlsx::createStyle(fontName = "Arial", fontSize = 8.25,
      fontColour = "#52606d", wrapText = TRUE, valign = "top"), rows = last, cols = seq_len(nc), gridExpand = TRUE)
    openxlsx::setRowHeights(workbook, sheet, rows = last,
      heights = max(18, ceiling(nchar(note, type = "width") / sum(widths)) * 12 + 6))
  }
  openxlsx::pageSetup(workbook, sheet, orientation = if (wide) "landscape" else "portrait",
    paperSize = 13, fitToWidth = TRUE, fitToHeight = TRUE, left = 0.15, right = 0.15, top = 0.15, bottom = 0.15)
  c(used_sheets, sheet)
}

result_entry_paragraphs <- function(entry, document = result_entry_document(entry)) {
  if (is.null(document)) return(list())
  nodes <- xml2::xml_find_all(document, paste0(
    ".//*[self::h1 or self::h2 or self::h3 or self::h4 or self::h5 or self::h6 or self::p or self::li or self::dt or self::dd or self::div]",
    "[not(ancestor::table or ancestor::script or ancestor::style)]",
    "[not(ancestor-or-self::*[contains(concat(' ',normalize-space(@class),' '),' mm-result-diagram-section ')])]",
    "[not(.//table or .//img or .//div or .//p or .//li or .//h1 or .//h2 or .//h3 or .//h4 or .//h5 or .//h6)]"
  ))
  Filter(function(item) nzchar(item$text), lapply(nodes, function(node) list(
    text = result_html_text(node),
    heading = grepl("^h[1-6]$", xml2::xml_name(node)),
    level = if (grepl("^h[1-6]$", xml2::xml_name(node))) as.integer(sub("h", "", xml2::xml_name(node))) else NA_integer_,
    output_order = result_node_order(node)
  )))
}

result_entry_images <- function(entry, document = result_entry_document(entry)) {
  if (is.null(document)) {
    return(list())
  }
  # Office embeds the displayed HTML/SVG diagram as a figure; PDF keeps its native nodes.
  diagrams <- xml2::xml_find_all(document, ".//div[contains(concat(' ', normalize-space(@class), ' '), ' mm-result-diagram-section ')]")
  if (length(diagrams) && !missing(document)) {
    # Diagram replacement must not mutate the document shared by table/notes
    # extraction. Copy the parsed tree without parsing the HTML again.
    copy <- xml2::xml_new_document()
    document <- xml2::xml_add_child(copy, xml2::xml_root(document), .copy = TRUE)
    diagrams <- xml2::xml_find_all(document, ".//div[contains(concat(' ', normalize-space(@class), ' '), ' mm-result-diagram-section ')]")
  }
  for (diagram in diagrams) {
    uri <- result_diagram_image_uri(diagram)
    replacement <- xml2::read_html(paste0('<img class="analysis-plot-image" alt="Model diagram" width="860" height="608" src="', uri, '">'))
    xml2::xml_set_attr(xml2::xml_find_first(replacement, './/img'), "data-result-export-order", as.character(result_node_order(diagram)))
    xml2::xml_replace(diagram, xml2::xml_find_first(replacement, './/img'))
  }
  image_nodes <- xml2::xml_find_all(
    document,
    ".//img[starts-with(@src, 'data:image/')]"
  )
  if (length(image_nodes) == 0) {
    return(list())
  }
  lapply(seq_along(image_nodes), function(index) {
    src <- xml2::xml_attr(image_nodes[[index]], "src") %||% ""
    mime <- sub("^data:([^;]+);base64,.*$", "\\1", src)
    payload <- sub("^data:[^;]+;base64,", "", src)
    payload <- gsub("%0A|%0D", "", payload, ignore.case = TRUE)
    payload <- gsub("[[:space:]]+", "", payload)
    extension <- switch(mime, "image/jpeg" = ".jpg", "image/webp" = ".webp", ".png")
    path <- tempfile("statedu_result_image_", fileext = extension)
    writeBin(jsonlite::base64_dec(payload), path)
    alt <- xml2::xml_attr(image_nodes[[index]], "alt") %||% sprintf("Figure %s", index)
    if (is.na(alt) || !nzchar(trimws(alt))) alt <- sprintf("Figure %s", index)
    width <- suppressWarnings(as.numeric(xml2::xml_attr(image_nodes[[index]], "width") %||% ""))
    height <- suppressWarnings(as.numeric(xml2::xml_attr(image_nodes[[index]], "height") %||% ""))
    # Older snapshots may have dimensions only in inline CSS. Preserve the
    # displayed ratio instead of silently treating every such image as square.
    style <- xml2::xml_attr(image_nodes[[index]], "style")
    css_px <- function(property) {
      if (is.na(style)) return(NA_real_)
      parts <- regmatches(style, regexec(paste0("(?:^|;)\\s*", property, "\\s*:\\s*([0-9.]+)px"), style, perl = TRUE))[[1]]
      if (length(parts) > 1L) as.numeric(parts[[2]]) else NA_real_
    }
    if (!is.finite(width) || width <= 0) width <- css_px("width")
    if (!is.finite(height) || height <= 0) height <- css_px("height")
    if ((!is.finite(width) || !is.finite(height)) && identical(mime, "image/png") && requireNamespace("png", quietly = TRUE)) {
      pixels <- tryCatch(dim(png::readPNG(path, native = TRUE)), error = function(e) NULL)
      if (length(pixels) >= 2L) {
        if (is.finite(width)) height <- width * pixels[[1]] / pixels[[2]] else
          if (is.finite(height)) width <- height * pixels[[2]] / pixels[[1]] else {
            width <- pixels[[2]]; height <- pixels[[1]]
          }
      }
    }
    list(path = path, title = alt, width_px = width, height_px = height,
      orientation = xml2::xml_attr(xml2::xml_find_first(image_nodes[[index]], "ancestor-or-self::*[@data-result-table-orientation][1]"), "data-result-table-orientation"),
      output_order = result_node_order(image_nodes[[index]]))
  })
}

result_document_page_spec <- function(landscape = FALSE) {
  width <- (if (isTRUE(landscape)) 250 else 176) / 25.4
  height <- (if (isTRUE(landscape)) 176 else 250) / 25.4
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

result_docx_page_spec <- function(landscape = FALSE) result_document_page_spec(landscape)

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
  if (identical(table_info$orientation, "landscape")) return(TRUE)
  if (identical(table_info$orientation, "portrait")) return(FALSE)
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
  }
  ft <- flextable::align(ft, align = "center", part = "header")
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
  app_label <- if (nzchar(app_version)) sprintf("StatEdu Studio v%s", app_version) else "StatEdu Studio"
  logo_path <- file.path("www", "logo-horizontal.png")
  if (file.exists(logo_path)) {
    document <- officer::body_add_img(document, src = logo_path, width = 3.4, height = 1.03)
  }
  document <- officer::body_add_par(document, "", style = "Normal")
  document <- officer::body_add_par(document, "", style = "Normal")
  document <- officer::body_add_par(document, "STATISTICAL REPORT", style = "Normal")
  document <- officer::body_add_par(document, "StatEdu Studio Result Collection", style = "heading 1")
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
    "All analyses were performed using StatEdu Studio. ",
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
    method <- "Hierarchical regression analysis was performed."
  } else if (grepl("regression", lowered)) {
    method <- "Regression analysis was performed."
  } else if (grepl("t-test|anova", lowered)) {
    method <- "t-test/ANOVA was performed."
  } else if (grepl("paired", lowered)) {
    method <- "Paired-sample analysis was performed."
  } else if (grepl("correlation", lowered)) {
    method <- "Correlation analysis was performed."
  } else {
    method <- sprintf("%s analysis was performed.", title)
  }
  reason <- if (nzchar(note_text)) {
    sprintf(" The analysis method was selected according to the criteria and notes shown in the result tables: %s", note_text)
  } else {
    " The analysis method was selected according to the criteria shown in the result tables."
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

result_docx_screen_section <- function(landscape = FALSE) {
  spec <- result_docx_page_spec(landscape)
  officer::prop_section(type = "nextPage",
    page_size = officer::page_size(width = spec$width, height = spec$height,
      orient = if (landscape) "landscape" else "portrait"),
    page_margins = officer::page_mar(top = spec$margin_top, bottom = spec$margin_bottom,
      left = spec$margin_left, right = spec$margin_right, header = 0, footer = 0))
}

result_document_table <- function(info, layout_only = FALSE) {
  source <- info$screen
  values <- source$values
  nh <- source$header_rows
  body <- as.data.frame(values[seq.int(nh + 1L, nrow(values)), , drop = FALSE], stringsAsFactors = FALSE)
  names(body) <- paste0("col", seq_len(ncol(body)))
  ft <- flextable::flextable(body)
  if (nh > 0L) {
    # set_header_df measures the unstyled header, then autofit measures it
    # again after formatting. Build the same rows and measure only at the end.
    ft <- flextable::delete_part(ft, part = "header")
    for (i in seq_len(nh)) ft <- flextable::add_header_row(ft, values = unname(values[i, ]), top = FALSE)
  } else ft <- flextable::delete_part(ft, part = "header")
  theme <- result_document_table_style()
  ft <- flextable::font(ft, fontname = theme$font, part = "all")
  ft <- flextable::fontsize(ft, size = theme$size, part = "all")
  ft <- flextable::bold(ft, bold = FALSE, part = "all")
  ft <- flextable::color(ft, color = "#2f3a46", part = "all")
  ft <- flextable::padding(ft, padding = theme$padding_pt, part = "all")
  ft <- flextable::border_remove(ft)
  borders <- list(outer = officer::fp_border(color = "#1f2937", width = theme$outer_pt),
    inner = officer::fp_border(color = "#000000", width = theme$inner_pt))
  formats <- if (!layout_only) setNames(lapply(seq_len(4L), function(i)
    matrix(NA_character_, nrow(values), ncol(values))), c("align", "valign", "top", "bottom"))
  for (cell in source$cells) {
    part <- if (cell$row <= nh) "header" else "body"
    row <- if (part == "header") cell$row else cell$row - nh
    rows <- seq.int(row, row + cell$rowspan - 1L)
    cols <- seq.int(cell$col, cell$col + cell$colspan - 1L)
    if (cell$rowspan > 1L || cell$colspan > 1L) ft <- flextable::merge_at(ft, i = rows, j = cols, part = part)
    style <- cell$style
    if (is.na(style)) style <- ""
    if (!layout_only) {
      rules <- result_document_cell_rules(source, cell)
      source_rows <- seq.int(cell$row, length.out = cell$rowspan)
      if (rules$top != "none") formats$top[min(source_rows), cols] <- rules$top
      if (rules$bottom != "none") formats$bottom[max(source_rows), cols] <- rules$bottom
      align <- if (grepl("text-align:\\s*(left|start)", style)) "left" else if (grepl("text-align:\\s*(right|end)", style)) "right" else if (part == "header" || grepl("text-align:\\s*center", style)) "center" else "left"
      formats$align[source_rows, cols] <- align
      formats$valign[source_rows, cols] <- if (grepl("vertical-align:\\s*top", style)) "top" else "center"
    }
    marker <- cell$superscript %||% ""
    if (nzchar(marker)) {
      value <- trimws(values[cell$row, cell$col])
      base <- trimws(substr(value, 1L, nchar(value) - nchar(marker)))
      ft <- flextable::compose(ft, i = row, j = cell$col, part = part,
        value = flextable::as_paragraph(flextable::as_chunk(base), flextable::as_sup(marker)))
    }
  }
  if (!layout_only) ft <- result_document_apply_cell_formats(ft, formats, nh, borders)
  # Fit the editable Word grid to the same B5 sheet width, without rewriting values.
  geometry <- result_document_table_geometry(info, fallback = FALSE)
  if (!is.null(geometry)) {
    if (nh > 0L) ft <- flextable::height(ft, height = geometry$heights[seq_len(nh)], part = "header")
    ft <- flextable::height(ft, height = geometry$heights[seq.int(nh + 1L, nrow(values))], part = "body")
    for (part in c("header", "body")) if (nrow(ft[[part]]$dataset) > 0L)
      names(ft[[part]]$colwidths) <- ft$col_keys
    widths <- geometry$widths
  } else {
    ft <- flextable::autofit(ft)
    target <- result_docx_page_spec(result_docx_wide_table(info))$table_width
    widths <- ft$body$colwidths
    # Keep short label columns usable when another column contains long prose.
    minimum <- target * 0.6 / length(widths)
    captured <- info$screen$column_widths
    widths <- if (length(captured) == length(widths) && all(is.finite(captured) & captured >= 0) && sum(captured) > 0) {
      # Captured hierarchical tables include zero-width separator columns. They
      # must not invalidate the real widths and become ordinary data columns.
      weights <- pmax(captured, 0.001)
      target * weights / sum(weights)
    } else minimum + target * 0.4 * widths / sum(widths)
  }
  ft <- flextable::width(ft, width = widths)
  flextable::set_table_properties(ft, layout = "fixed", align = "left",
    opts_word = list(split = FALSE, keep_with_next = TRUE))
}

result_docx_screen_table <- function(info) result_document_table(info)

write_result_collection_docx <- function(entries, file, contents = NULL) {
  model <- result_document_model(entries, contents)
  on.exit(result_document_cleanup(model),add=TRUE)
  document <- result_docx_apply_b5_section(officer::read_docx())
  table_writer <- result_docx_shared_table_writer(document)
  previous_wide <- FALSE; content_count <- 0L
  for (node in model$nodes) {
    if(content_count>0L && !identical(node$landscape,previous_wide))
      document <- officer::body_end_block_section(document,officer::block_section(result_docx_screen_section(previous_wide)))
    previous_wide <- node$landscape
    if(node$kind=="gap") {
      document <- officer::body_add_fpar(document,officer::fpar(
        officer::ftext("",officer::fp_text(font.family="Arial",font.size=10)),
        fp_p=officer::fp_par(line_spacing=1,padding=0,keep_with_next=FALSE)))
      next
    }
    if(node$kind=="paragraph") {
      if(node$heading)document <- officer::body_add_fpar(document,officer::fpar(
        officer::ftext(node$text,officer::fp_text(font.family="Arial",font.size=11.25,bold=TRUE)),
        fp_p=officer::fp_par(keep_with_next=TRUE,padding.bottom=6)))
      else document <- result_docx_add_note(document,node$text)
    } else if(node$kind=="image") {
      document <- officer::body_add_img(document,src=node$image$path,width=node$dimensions$width,height=node$dimensions$height)
    } else document <- table_writer$add(document,node$table)
    content_count <- content_count+1L
  }
  document <- officer::body_set_default_section(document, result_docx_screen_section(previous_wide))
  # Hancom needs explicit widths on merged cells as well as the Word table grid.
  # Without tcW, a colspan/rowspan header can import with an unsigned negative width.
  body_xml <- officer::docx_body_xml(document)
  word_ns <- xml2::xml_ns(body_xml)
  margins <- xml2::xml_find_all(body_xml, "//w:tbl/w:tr/w:tc/w:tcPr/w:tcMar/*", ns = word_ns)
  xml2::xml_set_attr(margins, "w:w", as.character(round(result_document_table_style()$padding_pt * 20)))
  for (table_node in xml2::xml_find_all(body_xml, "//w:tbl", ns = word_ns)) {
    grid <- as.numeric(xml2::xml_attr(xml2::xml_find_all(table_node, "./w:tblGrid/w:gridCol", ns = word_ns), "w:w", ns = word_ns))
    rows <- xml2::xml_find_all(table_node, "./w:tr", ns = word_ns)
    cells <- xml2::xml_find_all(rows, "./w:tc", ns = word_ns)
    props <- xml2::xml_find_all(table_node, "./w:tr/w:tc/w:tcPr", ns = word_ns)
    stopifnot(length(props) == length(cells))
    spans <- vapply(props, function(prop) {
      children <- xml2::xml_children(prop)
      span <- children[xml2::xml_name(children) == "gridSpan"]
      if (!length(span)) return(1L)
      suppressWarnings(as.integer(xml2::xml_attr(span[[1L]], "w:val", ns = word_ns)))
    }, integer(1))
    spans[is.na(spans)] <- 1L
    row_counts <- lengths(xml2::xml_find_all(rows, "./w:tc", ns = word_ns, flatten = FALSE))
    widths <- numeric(length(cells)); offset <- 0L
    for (count in row_counts) {
      if (!count) next
      indexes <- seq.int(offset + 1L, length.out = count)
      ends <- cumsum(spans[indexes]); starts <- ends - spans[indexes] + 1L
      widths[indexes] <- vapply(seq_along(indexes), function(i) sum(grid[seq.int(starts[i], ends[i])]), numeric(1))
      offset <- offset + count
    }
    valid <- which(is.finite(widths) & widths > 0)
    if (length(valid) == length(props)) {
      xml2::xml_remove(xml2::xml_find_all(table_node, "./w:tr/w:tc/w:tcPr/w:tcW", ns = word_ns))
    } else for (i in valid) xml2::xml_remove(xml2::xml_find_all(props[[i]], "./w:tcW", ns = word_ns))
    for (i in valid) {
      xml2::xml_add_child(props[[i]], "w:tcW", `w:type` = "dxa", `w:w` = as.character(as.integer(widths[i])))
    }
  }
  table_writer$finish()
  print(document, target = file)
  invisible(file)
}

saved_results_empty_ui <- function(language = statedu_initial_language()) {
  div(class = "empty-message", div(statedu_t("result.empty_message", language)))
}

result_entries_for_export <- function(store, language = statedu_initial_language()) {
  entries <- isolate(store())
  if (length(entries) == 0) {
    stop(statedu_t("result.no_saved_results", language), call. = FALSE)
  }
  entries
}

result_collection_exception_features <- function(entries) {
  entries <- normalize_result_snapshot_entries(entries)
  if (length(entries) == 0) {
    return(character(0))
  }
  titles <- vapply(entries, function(entry) as.character(entry$title %||% ""), character(1))
  if (all(titles %in% c("t-test / ANOVA"))) {
    return(c("excel", "word", "__public_exception__"))
  }
  character(0)
}

register_result_accumulator_outputs <- function(input, output, session, app_language_fn = NULL) {
  store <- result_accumulator_store(session)
  undo_entries <- reactiveVal(NULL)
  current_language <- function() {
    statedu_current_language(app_language_fn)
  }
  if (!is.null(session$userData$result_restore_error)) session$onFlushed(function() {
    message <- statedu_t("result.history_error.restore", current_language())
    showNotification(message, type = "warning", duration = NULL, session = session)
  }, once = TRUE)

  output$saved_results_list <- renderUI({
    entries <- store()
    language <- current_language()
    undo <- undo_entries()
    undo_control <- if (!is.null(undo) && identical(entries, undo$after)) {
      actionButton("undo_saved_result_edit", statedu_t("result.management.undo_edit", language))
    }
    if (length(entries) == 0) {
      return(tagList(saved_results_empty_ui(language), undo_control))
    }
    tagList(
      div(class = "saved-result-count", sprintf(
        statedu_t("result.saved_count", language),
        length(entries)
      )),
      div(
        class = "saved-result-list",
        lapply(seq_along(entries), function(index) saved_result_entry_ui(entries[[index]], index, length(entries), language))
      ),
      undo_control
    )
  })

  observeEvent(input$saved_result_entry_action, {
    request <- input$saved_result_entry_action
    if (!is.list(request) || length(request$action) != 1L || !request$action %in% c("up", "down", "delete")) return()
    before <- store()
    after <- result_collection_edit(before, request$id, request$action)
    if (identical(before, after)) return()
    if (!isTRUE(write_result_snapshot_store(after))) {
      showNotification(statedu_t("result.write_failed", current_language()), type = "error")
      return()
    }
    undo_entries(list(before = before, after = after))
    store(after)
  }, ignoreInit = TRUE)

  observeEvent(input$undo_saved_result_edit, {
    undo <- undo_entries()
    if (is.null(undo) || !identical(store(), undo$after)) return()
    before <- undo$before
    if (!isTRUE(write_result_snapshot_store(before))) {
      showNotification(statedu_t("result.write_failed", current_language()), type = "error")
      return()
    }
    store(before)
    undo_entries(NULL)
  }, ignoreInit = TRUE)

  output$result_export_controls <- renderUI({
    language <- current_language()
    included_features <- result_collection_exception_features(store())
    div(
      class = "result-toolbar-group result-toolbar-export",
      analysis_save_button("save_result_collection_html_dialog", statedu_ui_label("save_html", language), "html", class = "btn-default", included_features = included_features),
      analysis_save_button("save_result_collection_pdf_dialog", statedu_ui_label("save_pdf", language), "pdf", class = "btn-default", included_features = included_features),
      analysis_save_button("save_result_collection_excel_dialog", statedu_ui_label("save_excel", language), "excel", class = "btn-default", included_features = included_features),
      analysis_save_button("save_result_collection_word_dialog", statedu_ui_label("save_word", language), "word", class = "btn-default", included_features = included_features),
      if (identical(language, "ko")) analysis_save_button("save_result_collection_hwpx_dialog", "HWPX 저장", "word", class = "btn-default", included_features = included_features)
    )
  })

  observeEvent(input$result_document_select_all, {
    updateCheckboxGroupInput(session, "result_document_contents", selected = result_document_content_types())
  }, ignoreInit = TRUE)

  observeEvent(input$clear_saved_results, {
    undo_entries(NULL)
    store(list())
    write_result_snapshot_store(list())
    showNotification(statedu_t("result.cleared", current_language()), type = "message", duration = 3)
  }, ignoreInit = TRUE)

  observeEvent(input$save_result_history_dialog, {
    tryCatch(
      {
        language <- current_language()
        entries <- result_entries_for_export(store, language)
        path <- choose_result_history_save_path(language = language)
        if (length(path) == 0 || !nzchar(path[[1]])) {
          showNotification(statedu_t("result.save_dialog_canceled", language), type = "warning", duration = 5)
          return(invisible(NULL))
        }
        if (!grepl("\\.(efs-result|json)$", path, ignore.case = TRUE)) {
          path <- paste0(path, ".efs-result")
        }
        if (!isTRUE(write_result_snapshot_store(entries, path))) {
          stop(statedu_t("result.write_failed", language), call. = FALSE)
        }
        showNotification(sprintf(statedu_t("result.saved_path", language), path), type = "message")
      },
      error = function(e) {
        showNotification(paste(statedu_t("result.save_failed", current_language()), conditionMessage(e)), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE)

  observeEvent(input$open_result_history_dialog, {
    tryCatch(
      {
        language <- current_language()
        path <- choose_result_history_open_path(language = language)
        if (length(path) == 0 || !nzchar(path[[1]])) {
          showNotification(statedu_t("result.open_dialog_canceled", language), type = "warning", duration = 5)
          return(invisible(NULL))
        }
        entries <- read_result_snapshot_store(path)
        if (length(entries) == 0) {
          stop(statedu_t("result.file_empty", language), call. = FALSE)
        }
        store(entries)
        write_result_snapshot_store(entries)
        showNotification(sprintf(statedu_t("result.opened_path", language), path), type = "message")
      },
      error = function(e) {
        showNotification(paste(statedu_t("result.open_failed", current_language()), result_history_error_text(e, current_language())), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE)

  observeEvent(input$save_result_collection_html_dialog, {
    tryCatch(
      {
        language <- current_language()
        entries <- result_entries_for_export(store, language)
        path <- choose_html_save_path()
        if (length(path) == 0 || !nzchar(path[[1]])) {
          showNotification(statedu_t("result.save_dialog_canceled", language), type = "warning", duration = 5)
          return(invisible(NULL))
        }
        if (!grepl("\\.html?$", path, ignore.case = TRUE)) {
          path <- paste0(path, ".html")
        }
        write_result_collection_html(entries, path)
        showNotification(sprintf(statedu_t("result.collection_saved", language), path), type = "message")
      },
      error = function(e) {
        showNotification(paste(statedu_t("result.collection_save_failed", current_language()), result_export_error_text(e, current_language())), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE)

  observeEvent(input$save_result_collection_pdf_dialog, {
    tryCatch(
      {
        language <- current_language()
        entries <- result_entries_for_export(store, language)
        included_features <- result_collection_exception_features(entries)
        if (!isTRUE(analysis_save_feature_enabled("pdf", included_features = included_features))) {
          showNotification(statedu_t("result.collection_pdf_disabled", language), type = "warning", duration = 5)
          return(invisible(NULL))
        }
        path <- choose_pdf_save_path()
        if (length(path) == 0 || !nzchar(path[[1]])) {
          showNotification(statedu_t("result.save_dialog_canceled", language), type = "warning", duration = 5)
          return(invisible(NULL))
        }
        if (!grepl("\\.pdf$", path, ignore.case = TRUE)) {
          path <- paste0(path, ".pdf")
        }
        write_result_collection_pdf(entries, path)
        showNotification(sprintf(statedu_t("result.collection_saved", language), path), type = "message")
      },
      error = function(e) {
        showNotification(paste(statedu_t("result.collection_save_failed", current_language()), result_export_error_text(e, current_language())), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE)

  observeEvent(input$save_result_collection_excel_dialog, {
    tryCatch(
      {
        language <- current_language()
        entries <- result_entries_for_export(store, language)
        included_features <- result_collection_exception_features(entries)
        if (!isTRUE(analysis_save_feature_enabled("excel", included_features = included_features))) {
          showNotification(statedu_t("result.collection_excel_disabled", language), type = "warning", duration = 5)
          return(invisible(NULL))
        }
        path <- choose_excel_save_path()
        if (length(path) == 0 || !nzchar(path[[1]])) {
          showNotification(statedu_t("result.save_dialog_canceled", language), type = "warning", duration = 5)
          return(invisible(NULL))
        }
        if (!grepl("\\.xlsx$", path, ignore.case = TRUE)) {
          path <- paste0(path, ".xlsx")
        }
        save_result_collection_excel_file(entries, path)
        showNotification(sprintf(statedu_t("result.collection_saved", language), path), type = "message")
      },
      error = function(e) {
        showNotification(paste(statedu_t("result.collection_save_failed", current_language()), result_export_error_text(e, current_language())), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE)

  pending_document_export <- reactiveVal(NULL)
  document_contents <- reactiveVal("main")
  document_cache <- result_document_export_cache()
  session$onSessionEnded(document_cache$clear)
  open_document_export <- function(format) {
    language <- current_language()
    if (identical(format, "hwpx")) shiny::req(identical(language, "ko"))
    entries <- result_entries_for_export(store, language)
    shiny::req(analysis_save_feature_enabled("word", included_features = result_collection_exception_features(entries)))
    pending_document_export(list(format = format, entries = entries))
    save_label <- if (identical(format, "hwpx")) "HWPX 저장" else statedu_ui_label("save_word", language)
    showModal(modalDialog(
      title = save_label, size = "s", easyClose = TRUE,
      tags$style(HTML("#shiny-modal .modal-dialog { margin-top: max(80px, 10vh); }")),
      checkboxGroupInput("result_document_contents", statedu_t("result.document.contents", language),
        choices = setNames(result_document_content_types(), vapply(result_document_content_types(),
          function(key) statedu_t(paste0("result.document.", key), language), character(1))),
        selected = document_contents(), inline = TRUE),
      actionButton("result_document_select_all", statedu_t("result.document.all", language), class = "btn-default btn-sm"),
      footer = tagList(modalButton(statedu_t("result.document.cancel", language)),
        actionButton("confirm_document_export", save_label, class = "btn-primary"))
    ))
  }
  observeEvent(input$save_result_collection_word_dialog, {
    open_document_export("word")
  }, ignoreInit = TRUE)
  observeEvent(input$save_result_collection_hwpx_dialog, {
    open_document_export("hwpx")
  }, ignoreInit = TRUE)
  observeEvent(input$confirm_document_export, {
    tryCatch({
      pending <- pending_document_export()
      shiny::req(!is.null(pending))
      language <- current_language()
      contents <- input$result_document_contents %||% character()
      if (!length(contents)) {
        showNotification(statedu_t("result.document.no_selection", language), type = "warning")
        return()
      }
      document_contents(contents)
      path <- if (pending$format == "hwpx") choose_hwpx_save_path() else choose_word_save_path()
      if (!length(path) || !nzchar(path[[1L]])) return()
      extension <- if (pending$format == "hwpx") ".hwpx" else ".docx"
      if (!endsWith(tolower(path), extension)) path <- paste0(path, extension)
      document_cache$save(pending$entries, path, pending$format, contents, language)
      removeModal()
      pending_document_export(NULL)
      showNotification(sprintf(statedu_t("result.collection_saved", language), path))
    }, error = function(e) showNotification(paste(statedu_t("result.collection_save_failed", current_language()),
      result_export_error_text(e, current_language())), type = "error", duration = 8))
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

register_add_result_snapshot <- function(input, session, button_id, title, output_id = NULL, html_fn = NULL, app_language_fn = NULL, canvas_root_id = NULL) {
  if (is.null(button_id) || !nzchar(button_id)) {
    return(invisible(FALSE))
  }

  if (is.function(output_id) && is.null(html_fn)) {
    html_fn <- output_id
    output_id <- NULL
  }

  snapshot_input_id <- paste0(button_id, "_snapshot")
  current_language <- function() {
    statedu_current_language(app_language_fn)
  }

  add_snapshot <- function(html) {
    if (length(html) == 0 || is.null(html) || !nzchar(as.character(html)[[1]])) {
      stop(statedu_t("result.add_unavailable", current_language()), call. = FALSE)
    }
    resolved_title <- if (is.function(title)) title() else title
    resolved_title <- as.character(resolved_title %||% "")
    if (!nzchar(resolved_title)) {
      resolved_title <- statedu_t("result.add_default_title", current_language())
    }
    append_result_snapshot(session, resolved_title, as.character(html)[[1]])
    updateNavbarPage(session, "main_menu", selected = "result")
    showNotification(sprintf(statedu_t("result.added", current_language()), resolved_title), type = "message", duration = 3)
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
              canvasRootId = canvas_root_id,
              nonce = as.numeric(Sys.time())
            )
          )
          return(invisible(NULL))
        }
        if (!is.function(html_fn)) {
          stop(statedu_t("result.add_output_missing", current_language()), call. = FALSE)
        }
        add_snapshot(html_fn())
      },
      error = function(e) {
        showNotification(paste(statedu_t("result.add_failed", current_language()), conditionMessage(e)), type = "error", duration = 8)
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
          resolved_title <- statedu_t("result.add_default_title", current_language())
        }
        add_snapshot(result_snapshot_document_html(resolved_title, fragment))
      },
      error = function(e) {
        showNotification(paste(statedu_t("result.add_failed", current_language()), conditionMessage(e)), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE)

  invisible(TRUE)
}
