if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]])) {
  invisible(try(Sys.setlocale("LC_ALL", "Korean_Korea.utf8"), silent = TRUE))
}

# Reuse the inference fixture so this layout contract exercises the complete
# measurement-gate, structural-model, path-estimate, Wald, and pairwise tables
# rather than a synthetic column-count approximation.
source(file.path("scripts", "validate_sem_multigroup_inference.R"), encoding = "UTF-8")

canvas_css <- paste(
  readLines(file.path("www", "model-canvas", "canvas.css"), warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

forbidden_min_widths <- c("min-width: 1120px", "min-width: 1080px", "min-width: 1040px")
stopifnot(
  !any(vapply(forbidden_min_widths, grepl, logical(1), x = canvas_css, fixed = TRUE)),
  grepl(
    "table.structural-group-gate-table,[^}]+width: 100% !important;[^}]+min-width: 0 !important;[^}]+max-width: 100% !important;[^}]+table-layout: fixed !important;",
    canvas_css,
    perl = TRUE
  ),
  grepl("structural-multigroup-path-table th,", canvas_css, fixed = TRUE),
  grepl("overflow-wrap: anywhere !important;", canvas_css, fixed = TRUE),
  grepl("td.structural-numeric-cell", canvas_css, fixed = TRUE),
  grepl("white-space: nowrap !important;", canvas_css, fixed = TRUE)
)

chrome_candidates <- c(
  Sys.getenv("CHROME_BIN", unset = ""),
  Sys.which("chrome"),
  Sys.which("chromium"),
  Sys.which("msedge"),
  "C:/Program Files/Google/Chrome/Application/chrome.exe",
  "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"
)
chrome_candidates <- unique(chrome_candidates[nzchar(chrome_candidates)])
chrome_candidates <- chrome_candidates[file.exists(chrome_candidates)]

if (!length(chrome_candidates)) {
  cat("SEM multi-group B5 browser layout validation skipped: no Chromium browser found.\n")
  quit(save = "no", status = 0L)
}

fixture_dir <- tempfile("statedu-sem-mg-b5-")
dir.create(fixture_dir, recursive = TRUE, showWarnings = FALSE)
fixture_path <- file.path(fixture_dir, "fixture.html")
profile_dir <- file.path(fixture_dir, "chrome-profile")
dir.create(profile_dir, recursive = TRUE, showWarnings = FALSE)

layout_script <- paste0(
  "<script>(function(){",
  "var sheets=Array.from(document.querySelectorAll('.structural-path-group-result [data-result-table-sheet=\"true\"]')).filter(function(s){return !!s.querySelector('table');});",
  "var rows=sheets.map(function(sheet){",
  "var wrap=sheet.querySelector('.table-responsive');var table=sheet.querySelector('table');",
  "var sr=sheet.getBoundingClientRect();var tr=table.getBoundingClientRect();",
  "var overflowDetails=Array.from(table.querySelectorAll('th,td')).filter(function(cell){return cell.scrollWidth>cell.clientWidth+1;}).map(function(cell){return {text:cell.textContent.trim(),clientWidth:cell.clientWidth,scrollWidth:cell.scrollWidth,column:cell.cellIndex+1};});",
  "return {orientation:sheet.getAttribute('data-result-table-orientation'),sheetWidth:sr.width,tableWidth:tr.width,clientWidth:wrap.clientWidth,scrollWidth:wrap.scrollWidth,overflowCells:overflowDetails.length,overflowDetails:overflowDetails,",
  "fits:sr.width<=890.5&&tr.right<=sr.right+0.5&&tr.width<=sr.width+0.5&&wrap.scrollWidth<=wrap.clientWidth+1&&overflowDetails.length===0};",
  "});",
  "var passed=rows.length>=5&&rows.every(function(row){return row.fits;});",
  "document.documentElement.setAttribute('data-layout-contract',passed?'passed':'failed');",
  "document.getElementById('layout-report').textContent=JSON.stringify(rows);",
  "})();</script>"
)

fixture <- paste0(
  "<!doctype html><html><head><meta charset=\"UTF-8\"><style>",
  "html,body{margin:0;padding:0;width:1400px;}body{box-sizing:border-box;padding:20px;}",
  ".structural-analysis-results{width:1100px;max-width:none;}",
  "table{border-collapse:collapse;}th,td{border:1px solid #cbd5e1;}",
  canvas_css,
  "</style></head><body><div class=\"structural-analysis-results\">",
  ui_html,
  "</div><pre id=\"layout-report\"></pre>",
  layout_script,
  "</body></html>"
)
writeLines(fixture, fixture_path, useBytes = TRUE)

fixture_uri <- paste0("file:///", utils::URLencode(normalizePath(fixture_path, winslash = "/", mustWork = TRUE)))
chrome_args <- c(
  "--headless=new",
  "--disable-gpu",
  "--disable-breakpad",
  "--disable-crash-reporter",
  "--no-first-run",
  "--no-default-browser-check",
  paste0("--user-data-dir=", normalizePath(profile_dir, winslash = "/", mustWork = TRUE)),
  "--window-size=1400,1800",
  "--dump-dom",
  fixture_uri
)
dump <- suppressWarnings(system2(chrome_candidates[[1L]], chrome_args, stdout = TRUE, stderr = TRUE))
status <- attr(dump, "status") %||% 0L
if (!identical(as.integer(status), 0L)) {
  stop(
    "Chromium layout fixture failed (status ", as.integer(status), "): ",
    paste(utils::tail(dump, 20L), collapse = "\n"),
    call. = FALSE
  )
}

document <- xml2::read_html(paste(dump, collapse = "\n"))
contract <- xml2::xml_attr(xml2::xml_find_first(document, "//html"), "data-layout-contract")
report <- xml2::xml_text(xml2::xml_find_first(document, "//*[@id='layout-report']"))
if (!identical(contract, "passed")) {
  stop("SEM multi-group B5 layout overflow: ", report, call. = FALSE)
}

cat("SEM multi-group B5 browser layout validation passed: ", report, "\n", sep = "")
