if(.Platform$OS.type=="windows")Sys.setlocale("LC_CTYPE","Korean_Korea.utf8")
Sys.setenv(STATEDU_MODULE_CACHE="false")
source("R/app_bootstrap.R",encoding="UTF-8");load_app_packages(check=FALSE);source_app_modules()
html<-paste0('<div data-result-table-orientation="landscape"><h3>Merged columns</h3><table>',
 '<thead><tr><th rowspan="2">Variable</th><th colspan="2">95% CI</th></tr><tr><th>Lower</th><th>Upper</th></tr></thead>',
 '<tbody><tr><td>사용자 변수</td><td>0.0000</td><td>1.250<sup>a</sup></td></tr></tbody></table><p>a = retained note.</p></div>')
out<-"tmp/word-width-validation";dir.create(out,recursive=TRUE,showWarnings=FALSE)
write_result_collection_docx(list(list(id="width",title="Width regression",html=html)),file.path(out,"merged.docx"))
doc<-xml2::read_xml(unz(file.path(out,"merged.docx"),"word/document.xml"));ns<-xml2::xml_ns(doc)
for(tbl in xml2::xml_find_all(doc,"//w:tbl",ns=ns)) {
 grid<-as.numeric(xml2::xml_attr(xml2::xml_find_all(tbl,"./w:tblGrid/w:gridCol",ns=ns),"w:w",ns=ns))
 for(row in xml2::xml_find_all(tbl,"./w:tr",ns=ns)) {
  column<-1L
  for(cell in xml2::xml_find_all(row,"./w:tc",ns=ns)) {
   span<-as.integer(xml2::xml_attr(xml2::xml_find_first(cell,"./w:tcPr/w:gridSpan",ns=ns),"w:val",ns=ns));if(is.na(span))span<-1L
   width<-as.integer(xml2::xml_attr(xml2::xml_find_first(cell,"./w:tcPr/w:tcW",ns=ns),"w:w",ns=ns))
   stopifnot(identical(width,as.integer(sum(grid[seq.int(column,length.out=span)]))))
   column<-column+span
  }
 }
}
text<-xml2::xml_text(doc)
for(value in c("사용자 변수","0.0000","1.250","a = retained note."))stopifnot(grepl(value,text,fixed=TRUE))
stopifnot(length(xml2::xml_find_all(doc,"//w:vertAlign[@w:val='superscript']",ns=ns))>0)
cat("PASS: explicit Word widths including merged cells, superscripts, values and notes\n")
