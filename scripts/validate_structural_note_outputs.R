Sys.setlocale("LC_CTYPE", "English_United States.utf8")
Sys.setenv(STATEDU_MODULE_CACHE = "false")
source("R/app_bootstrap.R", encoding = "UTF-8")
load_app_packages(check = FALSE); source_app_modules()
data <- lavaan::HolzingerSwineford1939
syntax <- "visual =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6\nspeed =~ x7 + x8 + x9"
fit <- lavaan::cfa(syntax, data = data)
base <- list(fit = fit, syntax = syntax, estimator = "ML", missing = "listwise", ordered = character(0),
  snapshot = list(nodes = list(), edges = list()), validity_formula = "standardized", rmsea_ci = .90,
  diagnostics = structural_canvas_fit_admissibility(fit))
pls_run <- readRDS("outputs/sem_replication_20260905/runs/pls_1.rds")
pls_bundle <- list(fit = pls_run$fit, diagnostics = pls_run, estimator = pls_run$estimator,
  snapshot = list(nodes = list(), edges = list()), pls_bootstrap = 0L)
texts <- paste(readLines("R/setup_custom_model_canvas_structural_render.R", encoding = "UTF-8"), collapse = "\n")
suffixes <- unique(regmatches(texts, gregexpr('"_result_[a-z_]+"', texts, perl = TRUE))[[1]])
suffixes <- c("_result_fit", "_result_validity", "_result_measurement", "_result_measurement_ci")
rendered <- list(); errors <- list()
for (type in c("cfa", "sem", "plssem")) {
  current <- if (type == "plssem") pls_bundle else base
  if (type == "sem") current$fit <- lavaan::sem(paste(syntax, "speed ~ visual + textual", sep = "\n"), data = data)
  shiny::testServer(function(input, output, session) {
    bundle <- reactiveVal(current)
    table <- function(kind, language_override = NULL) structural_canvas_result_table(kind, bundle, type, function() character(0), function() language_override %||% "en", function() NULL)
    structural_canvas_register_result_outputs(input, output, "audit", "audit_canvas", type,
      function() names(data), function() NULL, function() data, function() character(0), function() "en", bundle, table)
  }, {
    session$flushReact(); later::run_now(.3); session$flushReact()
    for (suffix in suffixes) {
      tryCatch({
        value <- output[[paste0("audit", suffix)]]
        html <- if (is.list(value)) value$html %||% "" else ""
        if (nzchar(html) && grepl("<table", html, fixed = TRUE)) rendered[[paste0(type, suffix)]] <<- html
      }, error = function(e) {
        if (!inherits(e, "shiny.silent.error")) errors[[paste0(type, suffix)]] <<- conditionMessage(e)
      })
    }
  })
}
dir.create("tmp/all-analysis-notes/structural", recursive = TRUE, showWarnings = FALSE)
saveRDS(rendered, "tmp/all-analysis-notes/structural/rendered.rds")
writeLines(paste(names(errors), unlist(errors), sep = ": "), "tmp/all-analysis-notes/structural/errors.txt")
stopifnot(length(rendered) >= 6L, length(errors) == 0L)
for (html in rendered) {
  doc <- xml2::read_html(html)
  note <- xml2::xml_text(xml2::xml_find_all(doc, ".//*[contains(@class,'structural-result-note')]"))
  stopifnot(!any(grepl("^(Notes?|주)[.:]", trimws(note), perl = TRUE)))
}
message("Rendered ", length(rendered), " structural output panels; unexpected errors: ", length(errors))
if (length(errors)) print(errors)
