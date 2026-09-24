Sys.setenv(LC_ALL="English_United States.utf8", LANG="English_United States.utf8")
invisible(Sys.setlocale("LC_CTYPE","English_United States.utf8"))
library(shiny)
source("R/utils.R",encoding="UTF-8")
source("R/labels.R",encoding="UTF-8")
source("R/app_misc_ui.R",encoding="UTF-8")
out <- "tmp/changelog-history"
dir.create(out,recursive=TRUE,showWarnings=FALSE)
for(language in c("ko","en","ja","zh","es","fr","de","vi")) {
  spec <- about_document_specs(language)$version_history
  body <- about_markdown_document(spec$path)
  rendered <- htmltools::renderTags(tags$html(lang=language,
    tags$head(tags$meta(charset="utf-8"),tags$style(HTML("body{margin:24px;background:#f6f9fc;color:#14324f;font:16px/1.75 Arial,sans-serif}.about-markdown-document{padding:20px;background:white;border:1px solid #d5e0eb;border-radius:8px;max-width:1100px}h2{border-top:1px solid #e0e7ef;padding-top:16px}li{margin-bottom:8px}code{overflow-wrap:anywhere}"))),
    tags$body(body)))
  html <- sub("<body>", paste0("<head>", rendered$head, "</head><body>"), rendered$html, fixed=TRUE)
  writeLines(enc2utf8(html),file.path(out,paste0(language,".html")),useBytes=TRUE)
}
message("PASS: actual About renderer generated all eight language history pages")
