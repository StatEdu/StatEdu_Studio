source("scripts/validate_canvas_scores.R",encoding="UTF-8")
out <- "tmp/canvas-score-i18n"
dir.create(out,recursive=TRUE,showWarnings=FALSE)
languages <- c("ko","en","ja","zh","es","fr","de","vi")
catalog <- lapply(languages,function(lang)jsonlite::fromJSON(paste0("i18n/",lang,".json"))$translations)
names(catalog) <- languages
keys <- grep("^canvas[.]score[.]",names(catalog$en),value=TRUE)
placeholders <- function(text)sort(regmatches(text,gregexpr("[{][^}]+[}]",text))[[1]])
for(lang in languages) {
  options(statedu.app_language=lang)
  for(key in keys) {
    stopifnot(nzchar(catalog[[lang]][[key]]),identical(placeholders(catalog$en[[key]]),placeholders(catalog[[lang]][[key]])))
    stopifnot(identical(statedu_t(key,lang),catalog[[lang]][[key]]))
  }
  error <- tryCatch(canvas_score_items(d,"missing"),error=conditionMessage)
  stopifnot(identical(error,canvas_score_text("Select at least two distinct original items.",lang)))
  diagnostic <- as.character(canvas_parcel_allocation_ui(diag,lang))
  stopifnot(grepl("q1 = -",diagnostic,fixed=TRUE),grepl("CFI",diagnostic,fixed=TRUE),!grepl("{items}",diagnostic,fixed=TRUE))
  title <- canvas_score_text("Loading-balanced parcel allocation",lang)
  html <- as.character(tagList(canvas_score_audit_ui(pmodel,lang),canvas_score_audit_ui(balanced,lang)))
  stopifnot(grepl(paste0('lang="',lang,'"'),html,fixed=TRUE),grepl(title,html,fixed=TRUE),grepl("q12",html,fixed=TRUE),grepl(balanced_cfg$rationale,html,fixed=TRUE))
  writeLines(html,file.path(out,paste0(lang,"-snapshot.html")),useBytes=TRUE)
  jsonlite::write_json(custom_model_canvas_i18n(lang),file.path(out,paste0(lang,"-labels.json")),auto_unbox=TRUE)
  if("--exports" %in% commandArgs(TRUE))for(mode in c("current","accumulated")) {
    entries <- list(list(id="scores",title=title,html=html))
    if(mode=="accumulated")entries <- c(list(list(id="prior",title="Prior",html="<p>Preserved user text</p>")),entries)
    stem <- file.path(out,paste(lang,mode,sep="-"))
    write_result_collection_html(entries,paste0(stem,".html"))
    write_result_collection_docx(entries,paste0(stem,".docx"))
    write_result_collection_hwpx(entries,paste0(stem,".hwpx"))
    save_result_collection_excel_file(entries,paste0(stem,".xlsx"))
    write_result_collection_pdf(entries,paste0(stem,".pdf"))
    for(ext in c("docx","hwpx","xlsx")) {
      dest <- paste0(stem,"-",ext);dir.create(dest,showWarnings=FALSE)
      unzip(paste0(stem,".",ext),exdir=dest)
      xml <- list.files(dest,pattern="[.]xml$",recursive=TRUE,full.names=TRUE)
      text <- paste(unlist(lapply(xml,readLines,warn=FALSE,encoding="UTF-8")),collapse="")
      stopifnot(grepl(title,text,fixed=TRUE),grepl("q12",text,fixed=TRUE))
    }
    stopifnot(file.info(paste0(stem,".pdf"))$size>1000)
  }
  message("PASS score UI/audit/diagnostics and requested exports: ",lang)
}
