if(.Platform$OS.type=="windows")Sys.setlocale("LC_CTYPE","Korean_Korea.utf8")
Sys.setenv(STATEDU_MODULE_CACHE="false",STATEDU_EDITION="development",STATEDU_PUBLIC_RELEASE="false",
 STATEDU_RESULT_STORE=file.path(getwd(),"tmp/ipa-export-actions/history.json"))
source("R/app_bootstrap.R",encoding="UTF-8");load_app_packages(check=FALSE);source_app_modules()
out<-"tmp/ipa-export-actions";dir.create(out,recursive=TRUE,showWarnings=FALSE)
destination<-"current"
for(format in c("html","pdf","excel","word","hwpx"))local({
 f<-format
 assign(paste0("choose_",f,"_save_path"),function()file.path(out,paste0(destination,".",switch(f,excel="xlsx",word="docx",f))),envir=.GlobalEnv)
})
captured<-list()
shiny::testServer(function(input,output,session) {
 session$sendCustomMessage<-function(type,message) {captured[[length(captured)+1L]]<<-list(type=type,message=message)}
 handlers<-register_ipa_handlers(input,output,session,function()data.frame(i=seq(2,5,length.out=40),p=seq(1,4,length.out=40)),function()c("i","p"),function()"ko",function()NULL)
 register_result_accumulator_outputs(input,output,session,function()"ko")
}, {
 session$flushReact()
 session$setInputs(ipa_mode="direct",ipa_design="overall",ipa_layout="wide")
 session$setInputs(ipa_available="i",ipa_available_active=1,ipa_move_importance=1)
 session$setInputs(ipa_available="p",ipa_available_active=2,ipa_move_performance=1)
 session$setInputs(run_ipa=1)
 stopifnot(!is.null(handlers$result()))
 controls<-output$ipa_save$html
 for(f in c("html","pdf","excel"))stopifnot(grepl(paste0("save_ipa_",f,"_dialog"),controls,fixed=TRUE))
 stopifnot(grepl("add_ipa_result",controls,fixed=TRUE),!grepl("hwpx|word",controls))
 snapshot<-paste0(output$ipa_results$html,"<p>IPA snapshot sentinel 917</p>")
 # Saving must use the captured screen, never invoke the analysis again.
 prepare_ipa<<-function(...)stop("Analysis must not rerun on export")
 for(f in c("html","pdf","excel")) {
   id<-paste0("save_ipa_",f,"_dialog")
   do.call(session$setInputs,setNames(list(1),id))
   msg<-tail(captured,1)[[1]]
   stopifnot(msg$type=="easyflow-capture-result-snapshot",msg$message$outputId=="ipa_results",msg$message$inputId==paste0(id,"_snapshot"))
   do.call(session$setInputs,setNames(list(list(html=snapshot)),paste0(id,"_snapshot")))
   stopifnot(file.exists(file.path(out,paste0("current.",switch(f,excel="xlsx",word="docx",f)))))
 }
 session$setInputs(add_ipa_result=1)
 stopifnot(tail(captured,1)[[1]]$message$inputId=="add_ipa_result_snapshot")
 session$setInputs(add_ipa_result_snapshot=list(html=snapshot))
 entries<-shiny::isolate(result_accumulator_store(session)())
 stopifnot(grepl("IPA snapshot sentinel 917",tail(entries,1)[[1]]$html,fixed=TRUE))
 saveRDS(tail(entries,1),file.path(out,"entries.rds"))
 # Word/HWPX are available only after adding the captured result.
 session$setInputs(result_document_contents=result_document_content_types())
 session$setInputs(save_result_collection_word_dialog=1)
 session$setInputs(confirm_document_export=1)
 session$setInputs(save_result_collection_hwpx_dialog=1)
 session$setInputs(confirm_document_export=2)
 stopifnot(file.exists(file.path(out,"current.docx")),file.exists(file.path(out,"current.hwpx")))
})
entries<-readRDS(file.path(out,"entries.rds"))
entries<-c(entries,list(modifyList(entries[[1]],list(id="second",title="IPA second snapshot"))))
write_result_collection_html(entries,file.path(out,"accumulated.html"))
write_result_collection_pdf(entries,file.path(out,"accumulated.pdf"))
write_result_collection_docx(entries,file.path(out,"accumulated.docx"))
write_result_collection_hwpx(entries,file.path(out,"accumulated.hwpx"))
save_result_collection_excel_file(entries,file.path(out,"accumulated.xlsx"))
for(mode in c("current","accumulated")) {
 stem<-file.path(out,mode)
 word<-xml2::xml_text(xml2::read_xml(unz(paste0(stem,".docx"),"word/document.xml")))
 members<-unzip(paste0(stem,".hwpx"),list=TRUE)$Name
 hwpx<-paste(vapply(members[grepl("Contents/section[0-9]+[.]xml$",members)],function(s)xml2::xml_text(xml2::read_xml(unz(paste0(stem,".hwpx"),s))),character(1)),collapse="")
 wb<-openxlsx::loadWorkbook(paste0(stem,".xlsx"));excel<-paste(unlist(lapply(seq_along(names(wb)),function(i)as.matrix(openxlsx::read.xlsx(wb,sheet=i,colNames=FALSE)))),collapse=" ")
 for(content in list(word,hwpx,excel,paste(readLines(paste0(stem,".html"),warn=FALSE),collapse="")))stopifnot(grepl("IPA snapshot sentinel 917",content,fixed=TRUE))
 stopifnot(length(unzip(paste0(stem,".docx"),list=TRUE)$Name[grepl("word/media/",unzip(paste0(stem,".docx"),list=TRUE)$Name)])>=2)
 cat("PASS:",mode,"snapshot text and figures; HTML/PDF/Word/HWPX/Excel\n")
}

