if(.Platform$OS.type=="windows")Sys.setlocale("LC_CTYPE","Korean_Korea.utf8")
Sys.setenv(STATEDU_MODULE_CACHE="false")
source("R/app_bootstrap.R",encoding="UTF-8");load_app_packages(check=FALSE);source_app_modules()
set.seed(917)
d<-data.frame(i=rnorm(40,4,.4),p=rnorm(40,3,.5),j=rnorm(40,4,.4),q=rnorm(40,3,.5),g=rep(c("Normality","사용자 집단"),each=20))
items<-data.frame(Item="사용자 항목",Importance="i",Performance="p",PostImportance="j",PostPerformance="q")
fit<-prepare_ipa(d,items,design="grouped_paired",group="g")
entries<-list();baseline<-NULL
for(lang in c("en","ko","ja","zh","es","fr","de","vi")) {
  options(statedu.app_language=lang)
  panel<-as.character(ipa_tab_panel(lang))
  for(text in c("Importance method","Design","Group markers","Show item connections","Identify pre/post"))
    if(text!="Show item connections")stopifnot(grepl(ipa_text(text,lang),panel,fixed=TRUE))
  html<-as.character(ipa_results_ui(fit,lang));doc<-xml2::read_html(html,encoding="UTF-8")
  main<-xml2::xml_find_all(doc,"//div[@data-result-table-role='main'][.//table]")
  cells<-lapply(main,function(t)xml2::xml_text(xml2::xml_find_all(t,".//th|.//td")))
  if(is.null(baseline))baseline<-cells
  stopifnot(identical(cells,baseline),all(xml2::xml_attr(main,"data-result-table-language")=="en"))
  appendix<-xml2::xml_find_all(doc,"//div[@data-result-table-role='appendix']")
  stopifnot(length(appendix)>2,all(xml2::xml_attr(appendix,"data-result-table-language")==lang),
    grepl(ipa_text("Appendix A1. Analysis sample",lang),html,fixed=TRUE),
    grepl("사용자 항목",html,fixed=TRUE),grepl("Normality",html,fixed=TRUE))
  dictionary<-jsonlite::read_json(paste0("i18n/",lang,".json"))$translations
  keys<-grep("^analysis[.]ipa[.]",names(dictionary),value=TRUE)
  stopifnot(length(keys)>150,all(nzchar(unlist(dictionary[keys]))))
  if(!lang %in% c("en","ko"))stopifnot(ipa_error_message("Score roles require numeric variables.",lang)!="Score roles require numeric variables.")
  entries[[lang]]<-list(id=lang,title=ipa_title(lang),html=html)
  cat("PASS:",lang,"main values, appendix, labels and dictionary\n")
}
shiny::testServer(function(input,output,session) {
  language<-shiny::reactiveVal("en")
  handlers<-register_ipa_handlers(input,output,session,function()d,function()names(d),language,function()NULL)
}, {
  session$flushReact()
  session$setInputs(ipa_design="overall",ipa_mode="direct",ipa_layout="wide")
  session$setInputs(ipa_available="i",ipa_available_active=1,ipa_move_importance=1)
  session$setInputs(ipa_available="p",ipa_available_active=2,ipa_move_performance=1)
  session$setInputs(run_ipa=1)
  saved<-handlers$result()$coordinates
  for(lang in c("ja","de","vi","ko")) {
    language(lang);session$flushReact()
    stopifnot(identical(saved,handlers$result()$coordinates),grepl(ipa_text("Appendix A1. Analysis sample",lang),output$ipa_results$html,fixed=TRUE))
  }
})
out<-"tmp/ipa-i18n";dir.create(out,recursive=TRUE,showWarnings=FALSE)
saveRDS(entries,file.path(out,"entries.rds"))
norm<-function(x)gsub("[[:space:]\u00a0]+","",paste(x,collapse=""),perl=TRUE)
for(mode in c("current","accumulated")) {
  selected<-if(mode=="current")entries["ja"] else entries[c("de","vi")]
  stem<-file.path(out,mode)
  write_result_collection_html(selected,paste0(stem,".html"))
  write_result_collection_docx(selected,paste0(stem,".docx"))
  save_result_collection_excel_file(selected,paste0(stem,".xlsx"))
  write_result_collection_pdf(selected,paste0(stem,".pdf"))
  write_result_collection_hwpx(selected,paste0(stem,".hwpx"))
  word<-norm(xml2::xml_text(xml2::read_xml(unz(paste0(stem,".docx"),"word/document.xml"))))
  members<-unzip(paste0(stem,".hwpx"),list=TRUE)$Name
  hwpx<-norm(vapply(members[grepl("Contents/section[0-9]+[.]xml$",members)],function(s)xml2::xml_text(xml2::read_xml(unz(paste0(stem,".hwpx"),s))),character(1)))
  wb<-openxlsx::loadWorkbook(paste0(stem,".xlsx"));excel<-norm(unlist(lapply(seq_along(names(wb)),function(i)as.matrix(openxlsx::read.xlsx(wb,sheet=i,colNames=FALSE)))))
  for(lang in names(selected))for(actual in list(word,hwpx,excel))stopifnot(grepl(norm(ipa_text("Appendix A1. Analysis sample",lang)),actual,fixed=TRUE),grepl("Normality",actual,fixed=TRUE))
  cat("PASS:",mode,"all five exports\n")
}


