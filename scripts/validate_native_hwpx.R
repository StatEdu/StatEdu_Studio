if(.Platform$OS.type=="windows")Sys.setlocale("LC_CTYPE","Korean_Korea.utf8")
Sys.setenv(STATEDU_MODULE_CACHE="false")
source("R/app_bootstrap.R",encoding="UTF-8");load_app_packages(check=FALSE);source_app_modules()
out<-"tmp/native-hwpx-validation";dir.create(out,recursive=TRUE,showWarnings=FALSE)
html<-paste0('<div data-result-table-orientation="portrait"><h3>Native HWPX 검증</h3>',
 '<table data-result-column-widths="[0.5,0.25,0.25]"><thead><tr><th rowspan="3">Variable</th><th colspan="2">Model 1</th></tr><tr><th colspan="2">95% CI</th></tr><tr><th>Lower</th><th>Upper</th></tr></thead>',
 '<tbody><tr><td>A &amp; B</td><td>0.0000</td><td>&lt;.001<sup>a</sup></td></tr></tbody></table><p>a = preserved note.</p></div>',
 '<div data-result-table-orientation="landscape"><h3>Landscape</h3><table><thead><tr><th>Value</th></tr></thead><tbody><tr><td>1.2300</td></tr></tbody></table></div>')
entries<-list(list(id="native",title="Native",html=html))
write_result_collection_docx(entries,file.path(out,"reference.docx"))
write_result_collection_docx<-function(...)stop("HWPX must not call the Word writer")
system2<-function(...)stop("HWPX must not start an external converter")
prepare_ipa<-function(...)stop("Saving must not rerun analysis")
for(mode in c("current","accumulated")) {
 e<-if(mode=="current")entries else c(entries,entries)
 path<-file.path(out,paste0(mode,".hwpx"));write_result_collection_hwpx(e,path)
 zip<-zip::zip_list(path)
 stopifnot(zip$filename[1]=="mimetype",zip$compressed_size[1]==zip$uncompressed_size[1])
 members<-zip$filename
 secs<-members[grepl("^Contents/section[0-9]+[.]xml$",members)]
 docs<-lapply(secs,function(s)xml2::read_xml(unz(path,s)))
 tables<-unlist(lapply(docs,function(d)as.list(xml2::xml_find_all(d,".//hp:tbl",xml2::xml_ns(d)))),recursive=FALSE)
 stopifnot(length(tables)==2L*length(e))
 text<-paste(vapply(docs,xml2::xml_text,character(1)),collapse="")
 for(v in c("Native HWPX 검증","A & B","0.0000","<.001","a = preserved note.","1.2300"))stopifnot(grepl(v,text,fixed=TRUE))
 header<-xml2::read_xml(unz(path,"Contents/header.xml"));ns<-xml2::xml_ns(header)
 stopifnot(length(xml2::xml_find_all(header,".//hh:supscript",ns))>=1L)
 for(tbl in tables) {
  ns<-xml2::xml_ns(tbl);ncol<-as.integer(xml2::xml_attr(tbl,"colCnt"));nrow<-as.integer(xml2::xml_attr(tbl,"rowCnt"));grid<-matrix(FALSE,nrow,ncol)
  for(cell in xml2::xml_find_all(tbl,"./hp:tr/hp:tc",ns)) {
   a<-xml2::xml_find_first(cell,"./hp:cellAddr",ns);s<-xml2::xml_find_first(cell,"./hp:cellSpan",ns)
   cols<-as.integer(xml2::xml_attr(a,"colAddr"))+seq_len(as.integer(xml2::xml_attr(s,"colSpan")))
   rows<-as.integer(xml2::xml_attr(a,"rowAddr"))+seq_len(as.integer(xml2::xml_attr(s,"rowSpan")))
   stopifnot(!any(grid[rows,cols]));grid[rows,cols]<-TRUE
  }
  stopifnot(all(grid))
 }
 cat("PASS:",mode,"native package, merged cells, precision, notes, superscripts; no Word/external conversion\n")
}

# Shared edited-reference contract, including the internal CI header rule.
word <- xml2::read_xml(unz(file.path(out,"reference.docx"),"word/document.xml"))
w <- xml2::xml_ns(word)
margin <- xml2::xml_find_all(word,".//w:tbl//w:tcMar/*",w)
stopifnot(all(as.numeric(xml2::xml_attr(margin,"w:w",ns=w)) == 11))
stopifnot(!length(xml2::xml_find_all(word,".//w:tbl//w:b[not(@w:val='false' or @w:val='0')]",w)))
ci <- xml2::xml_find_first(word,".//w:tc[.//w:t='95% CI']",w)
stopifnot(xml2::xml_attr(xml2::xml_find_first(ci,"./w:tcPr/w:tcBorders/w:bottom",w),"w:val",ns=w)=="single")
ns <- xml2::xml_ns(header)
for(d in docs) {
 n <- xml2::xml_ns(d)
 for(cell in xml2::xml_find_all(d,".//hp:tc",n)) {
  stopifnot(all(xml2::xml_attrs(xml2::xml_find_first(cell,"./hp:cellMargin",n))=="56"))
  for(run in xml2::xml_find_all(cell,".//hp:run",n)) {
   id <- xml2::xml_attr(run,"charPrIDRef")
   style <- xml2::xml_find_first(header,paste0(".//hh:charPr[@id='",id,"']"),ns)
   stopifnot(xml2::xml_attr(style,"height")=="900", !length(xml2::xml_find_all(style,"./hh:bold",ns)))
  }
  if(xml2::xml_text(cell)=="95% CI") {
   id <- xml2::xml_attr(cell,"borderFillIDRef")
   edge <- xml2::xml_find_first(header,paste0(".//hh:borderFill[@id='",id,"']/hh:bottomBorder"),ns)
   stopifnot(xml2::xml_attr(edge,"type")=="SOLID",xml2::xml_attr(edge,"width")=="0.1 mm")
  }
 }
}
cat("PASS: uniform nonbold font, 0.2 mm margins, CI-only solid separator in Word/HWPX\n")
