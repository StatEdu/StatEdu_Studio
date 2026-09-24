Sys.setenv(STATEDU_MODULE_CACHE="false", STATEDU_BENCHMARK_REPS="120")
fixture <- readLines("scripts/benchmark_sem_complete_bootstrap_5000.R",warn=FALSE,encoding="UTF-8")
eval(parse(text=fixture[seq_len(which(fixture=="started <- Sys.time()")[[1]]-1L)]))
out <- "tmp/sem-publication";dir.create(out,recursive=TRUE,showWarnings=FALSE)
bundle_file <- file.path(out,"bundle.rds")
if (!file.exists(bundle_file)) {
  job <- structural_canvas_start_effect_bootstrap_job(snapshot,data,"sem","ML","fiml",FALSE,character(),character(),numeric(),
    reps=120L,seed=20260826L,ci_method="bias_corrected",original_result=original,workers=4L)
  while(job$process$is_alive()) Sys.sleep(.1)
  stopifnot(file.exists(job$result_file))
  original$effect_bootstrap_result <- readRDS(job$result_file)
  original$effect_bootstrap <- 120L;original$effect_bootstrap_pending <- FALSE
  saveRDS(original,bundle_file)
  structural_canvas_cleanup_effect_bootstrap_job(job)
}
bundle <- readRDS(bundle_file)
bundle$mi_mode <- "free"
get_table <- function(kind,language="en") {
  if(kind=="mi") return(data.frame(Step=1,`Skipped unsafe`=0,Covariance="ε4 ↔ ε5",MI="34.15",`MI p`="<.001",`BH-adjusted p`="<.001",EPC=".54",`Std. EPC`=".86",CFI=".97",TLI=".95",SRMR=".04",RMSEA=".05",check.names=FALSE))
  structural_canvas_result_table(kind,function() bundle,"cbsem",function() character(),function() language)
}
direct <- get_table("structural")
stopifnot(all(nzchar(direct$z)),all(is.finite(as.numeric(direct$z))))
long <- get_table("structural_effects")
stopifnot(all(c("beta p","beta 95% CI") %in% names(long)))
original_bootstrap <- bundle$effect_bootstrap_result
bundle$effect_bootstrap_result$beta_p <- .42
separate <- get_table("structural_effects")
stopifnot(all(as.numeric(separate[["beta p"]])==.42),any(separate$p!=separate[["beta p"]]))
bundle$effect_bootstrap_result <- original_bootstrap
captured <- NULL
shiny::testServer(function(input,output,session) {
  structural_canvas_register_result_outputs(input,output,"test_sem","test_canvas","cbsem",function() names(data),function() NULL,
    function() data,function() character(),function() "ko",function() bundle,get_table)
}, {
  captured <<- lapply(c("structural","specific_indirect","effect_main","effect_inference_details","mi"),function(kind) output[[paste0("test_sem_result_",kind)]]$html)
})
content <- paste(unlist(captured),collapse="\n")
doc <- xml2::read_html(content)
main <- xml2::xml_find_all(doc,"//div[contains(@class,'structural-main-result-panel')]//table")
stopifnot(length(main)==5L) # Specific indirect plus four grouped effect tables; direct is a standalone output.
for(table in main) stopifnot(!any(grepl("source|Valid bootstrap|Bootstrap status|BH family",xml2::xml_text(xml2::xml_find_all(table,".//th")),ignore.case=TRUE)))
headers <- xml2::xml_text(xml2::xml_find_all(doc,"//h4"))
stopifnot(all(vapply(7:10,function(n) any(startsWith(headers,paste0("Table ",n,"."))),logical(1))))
grouped <- xml2::xml_find_all(doc,"//table[contains(@class,'structural-effect-main-table')]")
stopifnot(all(vapply(grouped,function(t) xml2::xml_attr(xml2::xml_find_first(t,"ancestor::*[@data-result-table-orientation][1]"),"data-result-table-orientation")=="portrait",logical(1))))
mi_sheet <- xml2::xml_find_first(doc,"//table[contains(@class,'structural-mi-table')]/ancestor::*[@data-result-table-orientation][1]")
stopifnot(xml2::xml_attr(mi_sheet,"data-result-table-orientation")=="landscape")
stopifnot(length(grouped)==4L,identical(vapply(grouped,function(t) length(xml2::xml_find_all(t,".//tbody/tr[1]/td")),integer(1)),c(7L,7L,10L,10L)))
html <- result_snapshot_document_html("SEM publication tables",paste0('<div class="structural-analysis-results regression-results">',content,'</div>'))
writeLines(html,file.path(out,"screen.html"),useBytes=TRUE)
saveRDS(list(title="SEM publication tables",html=html),file.path(out,"entry.rds"))
if ("--exports" %in% commandArgs(TRUE)) {
  normalize <- function(x) gsub("[[:space:]\u00a0]+","",paste(x,collapse=""),perl=TRUE)
  expected <- xml2::xml_text(xml2::xml_find_all(doc,"//th|//td|//h4|//h5|//p"))
  expected <- expected[nzchar(trimws(expected))]
  for(mode in c("current","accumulated")) {
    entries <- list(list(id="sem",title="SEM publication tables",html=html))
    if(mode=="accumulated") entries <- c(list(list(id="prior",title="Prior result",html="<h4>Prior result</h4><p>Preserved prior explanation.</p>")),entries)
    stem <- file.path(out,mode)
    write_result_collection_html(entries,paste0(stem,".html"))
    write_result_collection_docx(entries,paste0(stem,".docx"))
    save_result_collection_excel_file(entries,paste0(stem,".xlsx"))
    write_result_collection_pdf(entries,paste0(stem,".pdf"))
    write_result_collection_hwpx(entries,paste0(stem,".hwpx"))
    word <- xml2::read_xml(unz(paste0(stem,".docx"),"word/document.xml"))
    members <- unzip(paste0(stem,".hwpx"),list=TRUE)$Name
    hwpx <- normalize(vapply(members[grepl("Contents/section[0-9]+[.]xml$",members)],function(s) xml2::xml_text(xml2::read_xml(unz(paste0(stem,".hwpx"),s))),character(1)))
    workbook <- openxlsx::loadWorkbook(paste0(stem,".xlsx"))
    excel <- normalize(unlist(lapply(seq_along(names(workbook)),function(i) as.matrix(openxlsx::read.xlsx(workbook,sheet=i,colNames=FALSE)))))
    for(value in expected) for(actual in list(normalize(xml2::xml_text(word)),hwpx,excel)) stopifnot(grepl(normalize(value),actual,fixed=TRUE))
    message("PASS: ",mode," HTML/PDF/Word/HWPX/Excel; all table content and notes retained")
  }
}
message("PASS: SEM main tables 3,6,7–10, inference appendix, z and standardized inference")
