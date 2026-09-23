# Native OWPML package writer. No DOCX conversion, COM, or Hancom installation.
result_hwpx_escape <- function(x) as.character(htmltools::htmlEscape(enc2utf8(as.character(x)),attribute=TRUE))
result_hwpx_units <- function(inches) as.integer(round(inches*7200))
result_hwpx_fragment <- function(node) sub("^<\\?xml[^>]*>\\s*","",as.character(node))
result_hwpx_set_attrs <- function(node,values) {
  for(key in names(values))xml2::xml_set_attr(node,key,values[[key]])
  invisible(node)
}

result_hwpx_styles <- function(header) {
  ns <- xml2::xml_ns(header)
  find <- function(path)xml2::xml_find_first(header,path,ns=ns)
  clone <- function(node) {
    declarations<-paste(sprintf('xmlns:%s="%s"',names(ns),unname(ns)),collapse=" ")
    xml2::xml_child(xml2::read_xml(paste0('<root ',declarations,'>',result_hwpx_fragment(node),'</root>')),1)
  }
  fonts <- xml2::xml_find_all(header,".//hh:fontface",ns=ns)
  font_ids <- character()
  for(face in fonts) {
    id <- length(xml2::xml_children(face));font <- clone(xml2::xml_child(face,1))
    result_hwpx_set_attrs(font,c(id=as.character(id),face="Arial"))
    xml2::xml_add_child(face,font);xml2::xml_set_attr(face,"fontCnt",as.character(id+1L))
    font_ids[tolower(xml2::xml_attr(face,"lang"))] <- as.character(id)
  }
  chars <- find(".//hh:charProperties");paras <- find(".//hh:paraProperties");borders <- find(".//hh:borderFills")
  char_base <- clone(xml2::xml_child(chars,1));para_base <- clone(xml2::xml_child(paras,1));border_base <- clone(xml2::xml_child(borders,1))
  char_ids <- new.env(parent=emptyenv());para_ids <- new.env(parent=emptyenv());border_ids <- new.env(parent=emptyenv())
  append <- function(parent,node,start=0L) {
    id <- length(xml2::xml_children(parent))+start
    xml2::xml_set_attr(node,"id",as.character(id));xml2::xml_add_child(parent,node)
    xml2::xml_set_attr(parent,"itemCnt",as.character(length(xml2::xml_children(parent))))
    as.character(id)
  }
  character_style <- function(size=9,bold=FALSE,sup=FALSE,color="#2F3A46") {
    key<-paste(size,bold,sup,color)
    if(exists(key,char_ids,inherits=FALSE))return(char_ids[[key]])
    node<-clone(char_base);result_hwpx_set_attrs(node,c(height=as.character(round(size*100)),textColor=color))
    ref<-xml2::xml_find_first(node,".//hh:fontRef",ns=ns)
    for(n in names(font_ids))xml2::xml_set_attr(ref,n,font_ids[[n]])
    if(bold)xml2::xml_add_child(node,xml2::read_xml('<hh:bold xmlns:hh="http://www.hancom.co.kr/hwpml/2011/head"/>'))
    if(sup)xml2::xml_add_child(node,xml2::read_xml('<hh:supscript xmlns:hh="http://www.hancom.co.kr/hwpml/2011/head"/>'))
    char_ids[[key]]<-append(chars,node);char_ids[[key]]
  }
  paragraph_style <- function(align="LEFT",keep=FALSE,after=0) {
    key<-paste(align,keep,after)
    if(exists(key,para_ids,inherits=FALSE))return(para_ids[[key]])
    node<-clone(para_base)
    xml2::xml_set_attr(xml2::xml_find_first(node,".//hh:align",ns=ns),"horizontal",align)
    result_hwpx_set_attrs(xml2::xml_find_first(node,".//hh:breakSetting",ns=ns),c(keepWithNext=if(keep)"1" else "0",breakNonLatinWord="BREAK_WORD"))
    for(x in xml2::xml_find_all(node,".//hc:next",ns=ns))xml2::xml_set_attr(x,"value",as.character(after*100))
    for(x in xml2::xml_find_all(node,".//hh:lineSpacing",ns=ns))xml2::xml_set_attr(x,"value","100")
    para_ids[[key]]<-append(paras,node);para_ids[[key]]
  }
  border_style <- function(top="none",bottom="none") {
    key<-paste(top,bottom)
    if(exists(key,border_ids,inherits=FALSE))return(border_ids[[key]])
    node<-clone(border_base)
    for(edge in c("top","bottom")) {
      rule<-if(edge=="top")top else bottom
      dark<-rule=="outer";visible<-rule!="none"
      result_hwpx_set_attrs(xml2::xml_find_first(node,paste0(".//hh:",edge,"Border"),ns=ns),
        c(type=if(visible)"SOLID" else "NONE",width=if(dark)"0.5 mm" else "0.1 mm",color=if(dark)"#1F2937" else "#000000"))
    }
    border_ids[[key]]<-append(borders,node,1L);border_ids[[key]]
  }
  list(char=character_style,para=paragraph_style,border=border_style)
}

write_result_collection_hwpx_native <- function(entries,file,contents=NULL) {
  model<-result_document_model(entries,contents,layout_only=TRUE);on.exit(result_document_cleanup(model),add=TRUE)
  stage<-tempfile("statedu-native-hwpx-");dir.create(stage)
  stopifnot(startsWith(normalizePath(stage,winslash="/"),paste0(normalizePath(tempdir(),winslash="/"),"/")))
  on.exit(unlink(stage,recursive=TRUE),add=TRUE)
  for(d in c("Contents","META-INF","BinData","Preview"))dir.create(file.path(stage,d))
  base<-file.path("www","hwpx-base")
  header<-xml2::read_xml(file.path(base,"header.xml"));styles<-result_hwpx_styles(header)
  table_theme<-result_document_table_style()
  hp<-"http://www.hancom.co.kr/hwpml/2011/paragraph";hc<-"http://www.hancom.co.kr/hwpml/2011/core"
  declaration<-sprintf('xmlns:hp="%s" xmlns:hc="%s" xmlns:hs="http://www.hancom.co.kr/hwpml/2011/section"',hp,hc)
  uid<-0L;next_id<-function(){uid<<-uid+1L;as.character(uid)}
  run<-function(text,size=9,bold=FALSE,sup=FALSE,color="#2F3A46") {
    # Explicit line-break nodes preserve captured cell and paragraph wrapping.
    pieces<-strsplit(enc2utf8(text),"\n",fixed=TRUE)[[1]]
    content<-paste(vapply(pieces,result_hwpx_escape,character(1)),collapse="<hp:lineBreak/>")
    sprintf('<hp:run charPrIDRef="%s"><hp:t>%s</hp:t></hp:run>',styles$char(size,bold,sup,color),content)
  }
  paragraph<-function(content,align="LEFT",keep=FALSE,after=0) sprintf('<hp:p id="%s" paraPrIDRef="%s" styleIDRef="0" pageBreak="0" columnBreak="0" merged="0">%s</hp:p>',next_id(),styles$para(align,keep,after),content)
  sections<-list();section<-character();wide<-NULL;manifest<-character();preview<-character();image_count<-0L
  start_section<-function(landscape) {
    spec<-result_document_page_spec(landscape)
    properties<-xml2::read_xml(file.path(base,"section-properties.xml"));ns<-xml2::xml_ns(properties)
    page<-xml2::xml_find_first(properties,".//hp:pagePr",ns=ns)
    # Hancom's NARROWLY flag swaps the base page dimensions for landscape.
    result_hwpx_set_attrs(page,c(landscape=if(landscape)"NARROWLY" else "WIDELY",width=as.character(result_hwpx_units(min(spec$width,spec$height))),height=as.character(result_hwpx_units(max(spec$width,spec$height)))))
    margin<-xml2::xml_find_first(page,"./hp:margin",ns=ns)
    result_hwpx_set_attrs(margin,c(left=as.character(result_hwpx_units(spec$margin_left)),right=as.character(result_hwpx_units(spec$margin_right)),top=as.character(result_hwpx_units(spec$margin_top)),bottom=as.character(result_hwpx_units(spec$margin_bottom)),header="0",footer="0",gutter="0"))
    prop<-result_hwpx_fragment(properties)
    paragraph(paste0('<hp:run charPrIDRef="',styles$char(),'">',prop,'<hp:ctrl><hp:colPr id="" type="NEWSPAPER" layout="LEFT" colCount="1" sameSz="1" sameGap="0"/></hp:ctrl></hp:run>'))
  }
  table_xml<-function(node) {
    info<-node$info;source<-info$screen;nr<-nrow(source$values);nc<-ncol(source$values);nh<-source$header_rows
    widths<-result_hwpx_units(node$widths);heights<-pmax(1200L,result_hwpx_units(node$heights))
    if(length(heights)!=nr)heights<-rep(1800L,nr)
    rows<-vector("list",nr)
    for(cell in source$cells) {
      r<-cell$row;c<-cell$col;is_header<-r<=nh
      rows_idx<-seq.int(r,length.out=cell$rowspan);cols<-seq.int(c,length.out=cell$colspan)
      style<-cell$style;if(is.na(style))style<-""
      align<-if(grepl("text-align:\\s*(left|start)",style))"LEFT" else if(grepl("text-align:\\s*(right|end)",style))"RIGHT" else if(is_header || grepl("text-align:\\s*center",style))"CENTER" else "LEFT"
      valign<-if(grepl("vertical-align:\\s*top",style))"TOP" else "CENTER"
      value<-source$values[r,c];marker<-cell$superscript%||%"";size<-table_theme$size
      content<-if(nzchar(marker))paste0(run(trimws(substr(value,1,nchar(value)-nchar(marker))),size,FALSE),run(marker,size,FALSE,TRUE)) else run(value,size,FALSE)
      rules<-result_document_cell_rules(source,cell)
      border<-styles$border(rules$top,rules$bottom)
      pad<-table_theme$padding_hwp
      tc<-sprintf('<hp:tc name="" header="%d" hasMargin="1" protect="0" editable="0" dirty="0" borderFillIDRef="%s"><hp:subList id="" textDirection="HORIZONTAL" lineWrap="BREAK" vertAlign="%s" linkListIDRef="0" linkListNextIDRef="0" textWidth="0" textHeight="0" hasTextRef="0" hasNumRef="0">%s</hp:subList><hp:cellAddr colAddr="%d" rowAddr="%d"/><hp:cellSpan colSpan="%d" rowSpan="%d"/><hp:cellSz width="%d" height="%d"/><hp:cellMargin left="%d" right="%d" top="%d" bottom="%d"/></hp:tc>',as.integer(is_header),border,valign,paragraph(content,align),c-1L,r-1L,cell$colspan,cell$rowspan,sum(widths[cols]),sum(heights[rows_idx]),pad,pad,pad,pad)
      rows[[r]]<-c(rows[[r]],tc)
    }
    tbl<-sprintf('<hp:tbl id="%s" zOrder="0" numberingType="TABLE" textWrap="TOP_AND_BOTTOM" textFlow="BOTH_SIDES" lock="0" dropcapstyle="None" pageBreak="CELL" repeatHeader="1" rowCnt="%d" colCnt="%d" cellSpacing="0" borderFillIDRef="1" noAdjust="0"><hp:sz width="%d" widthRelTo="ABSOLUTE" height="%d" heightRelTo="ABSOLUTE" protect="0"/><hp:pos treatAsChar="0" affectLSpacing="0" flowWithText="1" allowOverlap="0" holdAnchorAndSO="0" vertRelTo="PARA" horzRelTo="COLUMN" vertAlign="TOP" horzAlign="LEFT" vertOffset="0" horzOffset="0"/><hp:outMargin left="0" right="0" top="0" bottom="450"/><hp:inMargin left="0" right="0" top="0" bottom="0"/>%s</hp:tbl>',next_id(),nr,nc,sum(widths),sum(heights),paste(vapply(rows,function(x)paste0('<hp:tr>',paste(x,collapse=""),'</hp:tr>'),character(1)),collapse=""))
    paragraph(paste0('<hp:run charPrIDRef="',styles$char(),'">',tbl,'</hp:run>'))
  }
  for(node in model$nodes) {
    if(is.null(wide) || !identical(wide,node$landscape)) {
      if(length(section))sections[[length(sections)+1L]]<-section
      wide<-node$landscape;section<-start_section(wide)
    }
    if(node$kind=="table")content<-table_xml(node)
    else if(node$kind=="image") {
      image_count<-image_count+1L;id<-paste0("image",image_count);ext<-tolower(tools::file_ext(node$image$path))
      href<-paste0("BinData/",id,".",ext);file.copy(node$image$path,file.path(stage,href))
      mime<-switch(ext,jpg="image/jpeg",jpeg="image/jpeg",paste0("image/",ext))
      manifest<-c(manifest,sprintf('<opf:item id="%s" href="%s" media-type="%s" isEmbeded="1"/>',id,href,mime))
      pic<-xml2::read_xml(file.path(base,"picture.xml"));ns<-xml2::xml_ns(pic);w<-result_hwpx_units(node$dimensions$width);h<-result_hwpx_units(node$dimensions$height)
      result_hwpx_set_attrs(pic,c(id=next_id(),instid=next_id()))
      for(tag in c("orgSz","sz"))result_hwpx_set_attrs(xml2::xml_find_first(pic,paste0(".//hp:",tag),ns=ns),c(width=as.character(w),height=as.character(h)))
      xml2::xml_set_attr(xml2::xml_find_first(pic,".//hc:img",ns=ns),"binaryItemIDRef",id)
      for(k in 0:3)result_hwpx_set_attrs(xml2::xml_find_first(pic,paste0(".//hc:pt",k),ns=ns),c(x=as.character(if(k %in% c(1,2))w else 0),y=as.character(if(k %in% c(2,3))h else 0)))
      xml2::xml_set_text(xml2::xml_find_first(pic,".//hp:shapeComment",ns=ns),node$image$title)
      content<-paragraph(paste0('<hp:run charPrIDRef="',styles$char(),'">',result_hwpx_fragment(pic),'</hp:run>'))
    } else if(node$kind=="gap")content<-paragraph(run("",10))
    else {
      content<-paragraph(run(node$text,if(node$heading)11.25 else 7.2,node$heading,color=if(node$heading)"#000000" else "#52606D"),keep=node$heading,after=if(node$heading)6 else 0)
      preview<-c(preview,node$text)
    }
    section<-c(section,content)
  }
  if(length(section))sections[[length(sections)+1L]]<-section
  if(!length(sections))sections<-list(start_section(FALSE))
  xml2::xml_set_attr(header,"secCnt",as.character(length(sections)))
  xml2::write_xml(header,file.path(stage,"Contents/header.xml"))
  section_manifest<-spine<-character()
  for(i in seq_along(sections)) {
    id<-paste0("section",i-1L);href<-paste0("Contents/",id,".xml")
    text<-paste0('<?xml version="1.0" encoding="UTF-8"?><hs:sec ',declaration,'>',paste(sections[[i]],collapse=""),'</hs:sec>')
    xml2::write_xml(xml2::read_xml(text),file.path(stage,href))
    section_manifest<-c(section_manifest,sprintf('<opf:item id="%s" href="%s" media-type="application/xml"/>',id,href))
    spine<-c(spine,sprintf('<opf:itemref idref="%s" linear="yes"/>',id))
  }
  package<-paste0('<?xml version="1.0" encoding="UTF-8"?><opf:package xmlns:opf="http://www.idpf.org/2007/opf/" version="" unique-identifier="" id=""><opf:metadata><opf:title>StatEdu results</opf:title><opf:language>ko</opf:language><opf:meta name="creator" content="text">StatEdu Studio</opf:meta></opf:metadata><opf:manifest><opf:item id="header" href="Contents/header.xml" media-type="application/xml"/>',paste(c(section_manifest,manifest),collapse=""),'</opf:manifest><opf:spine><opf:itemref idref="header" linear="yes"/>',paste(spine,collapse=""),'</opf:spine></opf:package>')
  writeLines(package,file.path(stage,"Contents/content.hpf"),useBytes=TRUE)
  writeLines(paste(preview,collapse="\n"),file.path(stage,"Preview/PrvText.txt"),useBytes=TRUE)
  version<-xml2::read_xml(file.path(base,"version.xml"))
  result_hwpx_set_attrs(version,c(application="StatEdu Studio",appVersion=as.character(read_app_config()$version)))
  xml2::write_xml(version,file.path(stage,"version.xml"))
  file.copy(file.path(base,"manifest.xml"),file.path(stage,"META-INF/manifest.xml"))
  writeLines('<ocf:container xmlns:ocf="urn:oasis:names:tc:opendocument:xmlns:container"><ocf:rootfiles><ocf:rootfile full-path="Contents/content.hpf" media-type="application/hwpml-package+xml"/></ocf:rootfiles></ocf:container>',file.path(stage,"META-INF/container.xml"))
  writeBin(charToRaw("application/hwp+zip"),file.path(stage,"mimetype"))
  archive<-file.path(stage,"native.hwpx")
  zip::zipr(archive,"mimetype",root=stage,compression_level=0,include_directories=FALSE)
  files<-list.files(stage,recursive=TRUE);files<-setdiff(files,c("mimetype","native.hwpx"))
  zip::zip_append(archive,files,root=stage,mode="mirror",compression_level=6,include_directories=FALSE)
  if(!file.copy(archive,file,overwrite=TRUE))stop("Could not save the HWPX file.")
  invisible(file)
}
