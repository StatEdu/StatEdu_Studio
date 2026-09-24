if(.Platform$OS.type=="windows")Sys.setlocale("LC_CTYPE","Korean_Korea.utf8")
Sys.setenv(STATEDU_MODULE_CACHE="false")
source("scripts/validate_ipa.R",encoding="UTF-8")
grouped_results<-list()
long_grouped<-long;long_grouped$group<-d$group[match(long_grouped$id,d$id)]
for(mode in c("direct","derived")) {
  wide<-prepare_ipa(d,items,mode,"grouped_paired",group="group",outcome="y",post_outcome="z",resamples=100,group_plot="separate")
  tall<-prepare_ipa(long_grouped,items,mode,"grouped_paired","long",group="group",id="id",time="time",pre="before",post="after",outcome="y",resamples=100)
  stopifnot(max(abs(wide$coordinates$Importance-tall$coordinates$Importance))<1e-12,
    max(abs(wide$comparisons$Difference-tall$comparisons$Difference))<1e-12,
    nrow(wide$comparisons)==12L,length(unique(wide$references$Performance))==1L,
    identical(wide$point_styles$Shape,c(16L,17L,16L,17L)),
    identical(wide$point_styles$Color,rep(c("#0072B2","#D55E00"),each=2)))
  for(g in unique(d$group)) {
    reference<-prepare_ipa(d[d$group==g,],items,mode,"paired",outcome="y",post_outcome="z",resamples=100)
    keys<-wide$series_map$Series[wide$series_map$Group==g]
    stopifnot(max(abs(wide$coordinates$Importance[wide$coordinates$Group %in% keys]-reference$coordinates$Importance))<1e-12,
      max(abs(wide$comparisons$Difference[wide$comparisons$First %in% keys]-reference$comparisons$Difference))<1e-12)
  }
  grouped_results[[mode]]<-wide
}
within<-prepare_ipa(d,items,design="grouped_paired",group="group",reference="within")
stopifnot(within$references$Performance[1]==within$references$Performance[2],within$references$Performance[3]==within$references$Performance[4])
fails(prepare_ipa(d,items,design="grouped_paired"))
fails(prepare_ipa(rbind(long_grouped,long_grouped[1,]),items,design="grouped_paired",layout="long",group="group",id="id",time="time",pre="before",post="after"))
grouped_results$derived$group_plot<-"overlay"
shiny::testServer(function(input,output,session) {
  r<-register_ipa_handlers(input,output,session,function()long_grouped,function()names(long_grouped),function()"ko",function()NULL)
}, {
  session$setInputs(ipa_mode="direct",ipa_design="grouped_paired",ipa_layout="long",ipa_group_plot="separate",ipa_separate_mean="pooled")
  for(role in c("importance","performance","group","id","time")) {
    vars<-switch(role,importance=paste0("i",1:3),performance=paste0("p",1:3),group="group",id="id",time="time")
    args<-list(ipa_available=vars,ipa_available_active=match(role,c("importance","performance","group","id","time")))
    args[[paste0("ipa_move_",role)]]<-1
    do.call(session$setInputs,args)
  }
  stopifnot(grepl('id="ipa_group"',output$ipa_setup$html,fixed=TRUE),grepl('id="ipa_time"',output$ipa_setup$html,fixed=TRUE),
    grepl('Control · 사전',output$ipa_point_setup$html,fixed=TRUE))
  session$setInputs(ipa_swap_times=1)
  session$setInputs(run_ipa=1)
  stopifnot(!is.null(r$result()),r$result()$design=="grouped_paired",
    max(abs(r$result()$comparisons$Difference-grouped_results$direct$comparisons$Difference))<1e-12,
    identical(r$result()$point_styles$Color,rep(c("#0072B2","#D55E00"),each=2)))
})
out<-"tmp/ipa-grouped-validation";dir.create(out,recursive=TRUE,showWarnings=FALSE)
entries<-lapply(names(grouped_results),function(mode)list(id=mode,title=paste("IPA grouped",mode),html=as.character(ipa_results_ui(grouped_results[[mode]],"en"))))
for(j in seq_along(entries)) {
  doc<-xml2::read_html(entries[[j]]$html)
  expected_images<-if(j==1L)4L else 2L
  stopifnot(length(xml2::xml_find_all(doc,"//img"))==expected_images,!grepl("7:Control",xml2::xml_text(doc),fixed=TRUE))
}
norm<-function(x)gsub("[[:space:]\u00a0]+","",paste(x,collapse=""),perl=TRUE)
for(mode in c("current","accumulated")) {
  selected<-if(mode=="current")entries[1] else entries
  stem<-file.path(out,mode)
  write_result_collection_html(selected,paste0(stem,".html"))
  write_result_collection_docx(selected,paste0(stem,".docx"))
  save_result_collection_excel_file(selected,paste0(stem,".xlsx"))
  write_result_collection_pdf(selected,paste0(stem,".pdf"))
  write_result_collection_hwpx(selected,paste0(stem,".hwpx"))
  expected<-c("Pre/post by group","Control · Pre","Control · Post","Program · Pre","Program · Post","1 = Access")
  jsonlite::write_json(expected,paste0(stem,"-expected.json"),auto_unbox=TRUE)
  word<-norm(xml2::xml_text(xml2::read_xml(unz(paste0(stem,".docx"),"word/document.xml"))))
  members<-unzip(paste0(stem,".hwpx"),list=TRUE)$Name
  hwpx<-norm(vapply(members[grepl("Contents/section[0-9]+[.]xml$",members)],function(s)xml2::xml_text(xml2::read_xml(unz(paste0(stem,".hwpx"),s))),character(1)))
  wb<-openxlsx::loadWorkbook(paste0(stem,".xlsx"));excel<-norm(unlist(lapply(seq_along(names(wb)),function(i)as.matrix(openxlsx::read.xlsx(wb,sheet=i,colNames=FALSE)))))
  for(value in expected)for(actual in list(word,hwpx,excel))stopifnot(grepl(norm(value),actual,fixed=TRUE))
  cat("PASS:",mode,"grouped paired exports\n")
}
cat("PASS: direct/derived, wide/long, paired group changes, references, colors/shapes and figure separation\n")
