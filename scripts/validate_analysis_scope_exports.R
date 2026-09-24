Sys.setenv(STATEDU_MODULE_CACHE="false")
source("R/app_bootstrap.R",encoding="UTF-8");load_app_packages(check=FALSE);source_app_modules()
out <- "tmp/analysis-scope"
html <- paste(readLines(file.path(out,"split.html"),encoding="UTF-8",warn=FALSE),collapse="\n")
doc <- xml2::read_html(html)
normalize <- function(x) gsub("[[:space:]\u00a0]+","",paste(x,collapse=""),perl=TRUE)
expected <- xml2::xml_text(xml2::xml_find_all(doc,"//th|//td|//h3|//h4|//h5|//p"))
expected <- expected[nzchar(trimws(expected))]
for(mode in c("current","accumulated")) {
 entries <- list(list(id="split",title="Split file",html=html))
 if(mode=="accumulated")entries<-c(list(list(id="prior",title="Prior",html="<h4>Prior result</h4><p>Earlier result preserved.</p>")),entries)
 stem<-file.path(out,mode)
 write_result_collection_html(entries,paste0(stem,".html"))
 write_result_collection_docx(entries,paste0(stem,".docx"))
 save_result_collection_excel_file(entries,paste0(stem,".xlsx"))
 write_result_collection_pdf(entries,paste0(stem,".pdf"))
 write_result_collection_hwpx(entries,paste0(stem,".hwpx"))
 word<-normalize(xml2::xml_text(xml2::read_xml(unz(paste0(stem,".docx"),"word/document.xml"))))
 members<-unzip(paste0(stem,".hwpx"),list=TRUE)$Name
 hwpx<-normalize(vapply(members[grepl("Contents/section[0-9]+[.]xml$",members)],function(s)xml2::xml_text(xml2::read_xml(unz(paste0(stem,".hwpx"),s))),character(1)))
 workbook<-openxlsx::loadWorkbook(paste0(stem,".xlsx"))
 excel<-normalize(unlist(lapply(seq_along(names(workbook)),function(i)as.matrix(openxlsx::read.xlsx(workbook,sheet=i,colNames=FALSE)))))
 for(value in expected)for(actual in list(word,hwpx,excel))stopifnot(grepl(normalize(value),actual,fixed=TRUE))
 message("PASS: ",mode," HTML/PDF/Word/HWPX/Excel, group labels, displayed statistics and notes")
}
