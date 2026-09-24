if (.Platform$OS.type == "windows") Sys.setlocale("LC_CTYPE", "Korean_Korea.utf8")
Sys.setenv(STATEDU_MODULE_CACHE = "false")
source("R/app_bootstrap.R", encoding = "UTF-8")
load_app_packages(check = FALSE); source_app_modules()
out <- "tmp/document-selection"; dir.create(out, recursive = TRUE, showWarnings = FALSE)
uri <- saved_results_image_data_uri("www/logo-horizontal.png")
entry <- list(id="selection", title="Selection", html=paste0(
  '<h1>Analysis heading</h1><section><h2>Main title</h2><div class="result-table-with-note"><table data-result-table-role="main"><tr><th>Value</th></tr><tr><td>MAIN_1.234</td></tr></table><p class="coefficient-note">MAIN_NOTE</p></div></section>',
  '<section><h2>Appendix title</h2><div class="result-table-with-note"><table data-result-table-role="appendix"><tr><th>Value</th></tr><tr><td>APPENDIX_5.678</td></tr></table><p class="coefficient-note">APPENDIX_NOTE</p></div></section>',
  '<section><h2>Explanation title</h2><p>EXPLANATION_SENTINEL</p></section>',
  '<section><h2>Figure title</h2><img alt="FIGURE_SENTINEL" src="', uri, '"></section>'))
for (choice in c(result_document_content_types(), "all")) {
  contents <- if (choice == "all") result_document_content_types() else choice
  model <- result_document_model(list(entry), contents)
  text <- paste(vapply(model$nodes, function(n) n$text %||% if (n$kind == "table") paste(n$info$screen$values, collapse=" ") else "", character(1)), collapse=" ")
  stopifnot(grepl("MAIN_1.234",text,fixed=TRUE) == ("main" %in% contents),
    grepl("MAIN_NOTE",text,fixed=TRUE) == ("main" %in% contents),
    grepl("APPENDIX_5.678",text,fixed=TRUE) == ("appendix" %in% contents),
    grepl("APPENDIX_NOTE",text,fixed=TRUE) == ("appendix" %in% contents),
    grepl("EXPLANATION_SENTINEL",text,fixed=TRUE) == ("explanation" %in% contents),
    any(vapply(model$nodes,function(n)n$kind=="image",logical(1))) == ("figure" %in% contents),
    grepl("Main title",text,fixed=TRUE) == ("main" %in% contents),
    grepl("Appendix title",text,fixed=TRUE) == ("appendix" %in% contents),
    grepl("Figure title",text,fixed=TRUE) == ("figure" %in% contents))
  result_document_cleanup(model)
  docx <- file.path(out,paste0(choice,".docx")); hwpx <- file.path(out,paste0(choice,".hwpx"))
  write_result_collection_docx(list(entry),docx,contents)
  write_result_collection_hwpx(list(entry),hwpx,contents)
  word <- xml2::xml_text(xml2::read_xml(unz(docx,"word/document.xml")))
  sections <- unzip(hwpx,list=TRUE)$Name; sections <- sections[grepl("^Contents/section[0-9]+.xml$",sections)]
  hwp <- paste(vapply(sections,function(s)xml2::xml_text(xml2::read_xml(unz(hwpx,s))),character(1)),collapse=" ")
  for (value in c("MAIN_1.234","APPENDIX_5.678","EXPLANATION_SENTINEL")) {
    stopifnot(grepl(value,word,fixed=TRUE) == grepl(value,text,fixed=TRUE),
      grepl(value,hwp,fixed=TRUE) == grepl(value,text,fixed=TRUE))
  }
  cat("PASS:",choice,"shared model, Word and HWPX\n")
}
stopifnot(inherits(try(result_document_model(list(entry),character()),silent=TRUE),"try-error"))
# Explicit roles override older title heuristics (e.g. a main CI table).
legacy <- modifyList(entry,list(html='<h2>95% CI</h2><table data-result-table-role="main"><tr><th>X</th></tr><tr><td>2.35</td></tr></table>'))
model <- result_document_model(list(legacy),"main")
stopifnot(any(vapply(model$nodes,function(n)n$kind=="table",logical(1))))
result_document_cleanup(model)
cat("PASS: empty selection and explicit table roles\n")

Sys.setenv(STATEDU_RESULT_STORE=file.path(out,"isolated-history.json"),STATEDU_EDITION="development",STATEDU_PUBLIC_RELEASE="false")
choose_word_save_path <- function() file.path(out,"ui-default.docx")
choose_hwpx_save_path <- function() file.path(out,"ui-default.hwpx")
messages <- list()
shiny::testServer(function(input,output,session) {
  session$sendCustomMessage <- function(type,message) { if(type == "shiny-modal") messages$modal <<- message }
  session$sendInputMessage <- function(inputId,message) messages[[inputId]] <<- message
  register_result_accumulator_outputs(input,output,session,function()"ko")
}, {
  session$flushReact()
  result_accumulator_store(session)(list(entry))
  session$flushReact()
  controls <- output$result_export_controls$html
  stopifnot(!grepl("result_document_contents",controls,fixed=TRUE))
  session$setInputs(result_document_contents="main",result_document_select_all=0)
  session$setInputs(save_result_collection_word_dialog=1)
  stopifnot(grepl('value="main" checked="checked"', messages$modal$html, fixed=TRUE))
  session$setInputs(confirm_document_export=1)
  session$setInputs(save_result_collection_hwpx_dialog=1)
  session$setInputs(confirm_document_export=2)
  word <- xml2::xml_text(xml2::read_xml(unz(file.path(out,"ui-default.docx"),"word/document.xml")))
  stopifnot(grepl("MAIN_1.234",word,fixed=TRUE),!grepl("APPENDIX_5.678|EXPLANATION_SENTINEL",word))
  session$setInputs(result_document_select_all=1)
  stopifnot(setequal(messages$result_document_contents$value,result_document_content_types()))
})
cat("PASS: UI defaults to main tables and All selects all four types\n")
