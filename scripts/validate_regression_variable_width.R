Sys.setlocale("LC_CTYPE", "English_United States.utf8")
Sys.setenv(STATEDU_MODULE_CACHE = "false")
source("R/app_bootstrap.R", encoding = "UTF-8")
load_app_packages(check = FALSE); source_app_modules()
out <- "tmp/regression-variable-width"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
labels <- c("(Intercept)", "GCM_엄마나이(35세 기준):20~34세", "GCM_엄마나이(35세 기준):35세 이상", "GCM_학력(2분류):고졸 이하", "GCM_학력(2분류):전문대졸 이상")
base <- data.frame(Term = labels, B = c("2.78", "reference", ".72", "reference", "-.68"), `Boot SE` = c(".42", "", ".27", "", ".29"), LLCI = c("2.02", "", ".20", "", "-1.27"), ULCI = c("3.66", "", "1.26", "", "-.14"), `Boot p` = c("<.001", "", ".006", "", ".022"), check.names = FALSE)
entries <- list()
for (kind in c("bootstrap", "ols", "optional")) {
 table <- base
 if (kind != "bootstrap") names(table)[names(table) %in% c("Boot SE", "Boot p")] <- c("SE", "p")
 if (kind == "ols") {table$beta <- ".20"; table$t <- "2.00"}
 if (kind == "optional") {table$beta <- ".20"; table$t <- "2.00"; table$VIF <- "1.25"; table$`f²` <- ".02"}
 table <- regression_main_table(table)
 widths <- attr(table, "compact_column_widths")
 stopifnot(abs(sum(widths)-100)<1e-8, widths[1]>=28, widths[1]>max(widths[-1]))
 html <- as.character(htmltools::div(class="regression-results", htmltools::h3(paste("Regression",kind)), coefficient_html_table(table, note_line="SE = standard error; LLCI = lower confidence limit; ULCI = upper confidence limit.", sheet_orientation=if(ncol(table)<=9) "portrait" else "landscape")))
 entries[[kind]] <- list(id=kind,title=kind,html=html)
 parsed <- result_entry_tables(entries[[kind]])[[1]]
 stopifnot(max(abs(parsed$screen$column_widths-widths/100))<.001)
}
saveRDS(entries, file.path(out,"entries.rds"))
for(mode in c("current","accumulated")) {
 selected <- if(mode=="current") entries[1] else entries
 stem <- file.path(out,mode)
 write_result_collection_html(selected,paste0(stem,".html"))
 write_result_collection_docx(selected,paste0(stem,".docx"))
 save_result_collection_excel_file(selected,paste0(stem,".xlsx"))
 doc <- xml2::read_xml(unz(paste0(stem,".docx"),"word/document.xml"))
 grids <- xml2::xml_find_all(doc,".//w:tblGrid",xml2::xml_ns(doc))
 stopifnot(length(grids)==length(selected))
 for(i in seq_along(grids)) {
  w <- as.numeric(xml2::xml_attr(xml2::xml_children(grids[[i]]),"w"))
  stopifnot(w[1]/sum(w)>=.279, w[1]>max(w[-1]))
 }
 wb <- openxlsx::loadWorkbook(paste0(stem,".xlsx"))
 stopifnot(length(names(wb))==length(selected)+1L)
 for(i in seq_along(selected)) {
  x <- paste(as.matrix(openxlsx::read.xlsx(wb,sheet=i+1L,colNames=FALSE)),collapse=" ")
  stopifnot(all(vapply(labels,grepl,logical(1),x=x,fixed=TRUE)),grepl("LLCI = lower confidence limit",x,fixed=TRUE))
 }
 write_result_collection_pdf(selected,paste0(stem,".pdf"))
 write_result_collection_hwpx(selected,paste0(stem,".hwpx"))
 members <- unzip(paste0(stem,".hwpx"),list=TRUE)$Name
 sections <- members[grepl("Contents/section[0-9]+[.]xml$",members)]
 texts <- paste(vapply(sections,function(s) xml2::xml_text(xml2::read_xml(unz(paste0(stem,".hwpx"),s))),character(1)),collapse=" ")
 stopifnot(all(vapply(labels,grepl,logical(1),x=texts,fixed=TRUE)))
 message("PASS: ",mode,"; Variable allocation, Word widths, labels/notes, five exports")
}
