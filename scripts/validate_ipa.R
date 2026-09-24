Sys.setlocale("LC_ALL","Korean_Korea.utf8")
Sys.setenv(STATEDU_MODULE_CACHE="false")
source("R/app_bootstrap.R",encoding="UTF-8");load_app_packages(check=FALSE);source_app_modules()
set.seed(917); n<-120
d<-data.frame(id=seq_len(n),group=rep(c("Control","Program"),each=n/2))
for(j in 1:3) {
  d[[paste0("p",j)]]<-rnorm(n,3+j*.2,.5)
  d[[paste0("i",j)]]<-rnorm(n,4-j*.2,.4)
  d[[paste0("q",j)]]<-d[[paste0("p",j)]]+rnorm(n,.3,.2)
  d[[paste0("j",j)]]<-d[[paste0("i",j)]]+rnorm(n,.1,.2)
}
d$y<-with(d,.4*p1+.7*p2+.2*p3+rnorm(n))
d$z<-with(d,.5*q1+.6*q2+.3*q3+rnorm(n))
items<-data.frame(Item=c("Access","Service","Value"),Performance=paste0("p",1:3),Importance=paste0("i",1:3),
  PostPerformance=paste0("q",1:3),PostImportance=paste0("j",1:3))
fails<-function(expr)stopifnot(inherits(tryCatch({force(expr);NULL},error=identity),"error"))
results<-list()
stopifnot(identical(ipa_sorted_groups(c(10,2,1,NA,2)),c("1","2","10")),
  identical(ipa_sorted_groups(factor(c("10","2","1"))),c("1","2","10")),
  identical(ipa_sorted_groups(c("B","A")),c("A","B")))
reversed_groups<-prepare_ipa(d[nrow(d):1,],items,"direct","independent",group="group")
stopifnot(identical(unique(reversed_groups$coordinates$Group),c("Control","Program")))
for(mode in c("direct","derived"))for(design in c("overall","independent","paired")) {
  key<-paste(mode,design,sep="_");rng<-.Random.seed
  results[[key]]<-prepare_ipa(d,items,mode,design,group="group",id="id",outcome="y",post_outcome="z",resamples=100,seed=87)
  stopifnot(identical(.Random.seed,rng),all(is.finite(results[[key]]$coordinates$Importance)),
    nrow(results[[key]]$coordinates)==if(design=="overall")3L else 6L)
}
# Independent values and Welch intervals; paired values and matching intervals.
ind<-results$direct_independent; pair<-results$direct_paired
stopifnot(length(unique(ipa_group_symbols(unique(ind$coordinates$Group))))==length(unique(ind$coordinates$Group)))
point_limits<-ipa_axis_limits(ind);ci_limits<-ipa_axis_limits(ind,TRUE)
for(metric in c("Performance","Importance")) {
  axis<-if(metric=="Performance")"x" else "y"
  extent<-range(c(ind$coordinates[[metric]],ind$references[[metric]]))
  stopifnot(point_limits[[axis]][1] < extent[1] - diff(extent)*.12,
    abs(point_limits[[axis]][2] - (extent[2]+diff(extent)*.08))<1e-12,
    ci_limits[[axis]][1]<=min(ind$coordinates[[paste0(metric,"Lower")]]),
    ci_limits[[axis]][2]>=max(ind$coordinates[[paste0(metric,"Upper")]]))
}
flat<-ind;flat$coordinates$Performance[]<-3;flat$references$Performance[]<-3
stopifnot(diff(ipa_axis_limits(flat)$x)>0)
stopifnot(abs(results$direct_overall$totals$PerformanceSD-sd(rowMeans(d[paste0("p",1:3)])))<1e-12,
  abs(ind$totals$ImportanceSD[1]-sd(rowMeans(d[d$group=="Control",paste0("i",1:3)])))<1e-12)
stopifnot(all(ind$references$Performance==mean(as.matrix(d[paste0("p",1:3)]))),
  max(abs(pair$coordinates$Performance[1:3]-colMeans(d[paste0("p",1:3)])))<1e-12)
for(j in 1:3) {
  row<-ind$comparisons[ind$comparisons$Item==items$Item[j] & ind$comparisons$Metric=="Performance",]
  test<-t.test(d[d$group=="Program",paste0("p",j)],d[d$group=="Control",paste0("p",j)])
  stopifnot(max(abs(c(row$Lower,row$Upper,row$p)-c(test$conf.int,test$p.value)))<1e-12)
  row<-pair$comparisons[pair$comparisons$Item==items$Item[j] & pair$comparisons$Metric=="Performance",]
  test<-t.test(d[[paste0("q",j)]],d[[paste0("p",j)]],paired=TRUE)
  stopifnot(max(abs(c(row$Lower,row$Upper,row$p)-c(test$conf.int,test$p.value)))<1e-12)
}
# Partial correlation checked independently through residual regression.
x<-as.matrix(d[paste0("p",1:3)])
for(logarithmic in c(FALSE,TRUE)) {
  xx<-if(logarithmic)log(x) else x
  expected<-sapply(1:3,function(j)cor(residuals(lm(xx[,j]~xx[,-j])),residuals(lm(d$y~xx[,-j]))))
  stopifnot(max(abs(ipa_partial(x,d$y,logarithmic)-expected))<1e-12)
}
# Long data is deliberately scrambled: matching must be by ID, not row order.
a<-d[c("id",paste0("p",1:3),paste0("i",1:3),"y")];a$time<-"before"
b<-d[c("id",paste0("q",1:3),paste0("j",1:3),"z")];names(b)<-names(a)[-ncol(a)];b$time<-"after"
long<-rbind(a,b);long<-long[sample(nrow(long)),]
for(mode in c("direct","derived")) {
  actual<-prepare_ipa(long,items,mode,"paired","long",id="id",time="time",pre="before",post="after",outcome="y",resamples=100)
  expected<-results[[paste(mode,"paired",sep="_")]]
  stopifnot(max(abs(actual$coordinates$Importance-expected$coordinates$Importance))<1e-12,
    max(abs(actual$comparisons$Difference-expected$comparisons$Difference))<1e-12)
}
# Synchronized bootstrap: identical pre/post scores produce exactly zero changes.
identical_times<-d
for(j in 1:3)identical_times[[paste0("q",j)]]<-d[[paste0("p",j)]]
identical_times$z<-d$y
same<-prepare_ipa(identical_times,items,"derived","paired",outcome="y",post_outcome="z",resamples=100)
stopifnot(all(same$comparisons$Lower[same$comparisons$Metric=="Importance"]==0),all(same$comparisons$Upper[same$comparisons$Metric=="Importance"]==0))
missing<-d;missing$q1[1:4]<-NA
stopifnot(all(prepare_ipa(missing,items,design="paired")$audit$Analyzed==n-4))
fails(prepare_ipa(rbind(long,long[1,]),items,design="paired",layout="long",id="id",time="time",pre="before",post="after"))
scoped<-d;attr(scoped,"statedu_scope_excluded")<-"p1";fails(prepare_ipa(scoped,items))
bad<-d;bad$p1<-0;fails(prepare_ipa(bad,items,"derived",outcome="y",derived="log_partial",resamples=100))
bad<-d;bad$p2<-bad$p1;fails(prepare_ipa(bad,items,"derived",outcome="y",resamples=100))
custom<-prepare_ipa(d,items,design="independent",group="group",reference="custom",x_reference=3,y_reference=4)
stopifnot(all(custom$references$Performance==3),all(custom$references$Importance==4))
three<-d;three$group<-rep(c("First","Second","Third"),length.out=n)
three_result<-prepare_ipa(three,items,"derived","independent",group="group",outcome="y",resamples=100)
stopifnot(nrow(three_result$comparisons)==18L)
finite<-is.finite(three_result$comparisons$p)
stopifnot(identical(three_result$comparisons$Holm[finite],p.adjust(three_result$comparisons$p[finite],"holm")))
stopifnot("run_ipa" %in% analysis_command_run_ids(),any(grepl('lazy_analysis_ipa',as.character(analysis_tab_panel()),fixed=TRUE)))
out<-"tmp/ipa-validation";dir.create(out,recursive=TRUE,showWarnings=FALSE)
write.csv(d,file.path(out,"ipa-data.csv"),row.names=FALSE);saveRDS(results,file.path(out,"results.rds"))
ind$point_styles<-ipa_default_point_styles(unique(ind$coordinates$Group))
ind$point_styles$Shape<-c(15L,17L);ind$point_styles$Color<-c("#1565C0","#D84315");ind$point_styles$Size<-c(1.6,1.2)
ind$connected_items<-ind$items$Item[1]
off<-ind;off$connected_items<-character()
stopifnot(ipa_plot_image(ind,unique(ind$coordinates$Group),TRUE)!=ipa_plot_image(off,unique(ind$coordinates$Group),TRUE))
bad_style<-ind$point_styles;bad_style$Color[1]<-"invalid";fails(ipa_validate_point_styles(bad_style,unique(ind$coordinates$Group)))
html<-as.character(ipa_results_ui(ind,"en"));doc<-xml2::read_html(html)
stopifnot(length(xml2::xml_find_all(doc,"//img"))==2,grepl("Holm p",html,fixed=TRUE),grepl("LLCI",html,fixed=TRUE))
stopifnot(grepl("M ± SD",html,fixed=TRUE),grepl("M (LLCI~ULCI)",html,fixed=TRUE),
  grepl(sprintf("%.3f ± %.3f",ind$totals$Performance[1],ind$totals$PerformanceSD[1]),html,fixed=TRUE))
separate <- ind;separate$group_plot <- "separate"
stopifnot(length(xml2::xml_find_all(xml2::read_html(as.character(ipa_results_ui(separate,"en"))),"//img"))==4L)
hidden <- ind;hidden$show_quadrants <- FALSE
custom_labels <- ind;custom_labels$quadrant_labels <- c("Improve now","Maintain","Monitor","Rebalance");custom_labels$quadrant_font_size <- 14
stopifnot(!identical(ipa_plot_image(ind,ind$audit$Group,TRUE),ipa_plot_image(hidden,hidden$audit$Group,TRUE)),
  !identical(ipa_plot_image(ind,ind$audit$Group,TRUE),ipa_plot_image(custom_labels,custom_labels$audit$Group,TRUE)))
stopifnot(identical(xml2::xml_text(xml2::xml_find_all(doc,"//h4")),c("1. Model overview","2. Importance and performance descriptive statistics","3. IPA charts","4. IPA 95% CI charts","5. Appendix tables")))
stopifnot(abs(ind$coordinates$PerformanceSD[1]-sd(d$p1[d$group=="Control"]))<1e-12,
  abs(ind$coordinates$ImportanceSD[1]-sd(d$i1[d$group=="Control"]))<1e-12,
  grepl(sprintf("%.3f",ind$references$Performance[1]),ipa_reference_labels(ind,"Control")[["Performance"]],fixed=TRUE),
  grepl("부록 A1",as.character(ipa_results_ui(ind,"ko")),fixed=TRUE),
  !grepl("아니오",as.character(ipa_results_ui(ind,"ko")),fixed=TRUE))
images<-xml2::xml_attr(xml2::xml_find_all(doc,"//img"),"src")
stopifnot(all(images[1]!=images[2]),all(xml2::xml_attr(xml2::xml_find_all(doc,"//*[@data-result-table-sheet]"),"data-result-table-orientation")=="portrait"))
writeLines(result_snapshot_document_html("IPA",html),file.path(out,"preview.html"),useBytes=TRUE)
shiny::testServer(function(input,output,session) {
  r<-register_ipa_handlers(input,output,session,function()d,function()names(d),function()"ko",function()NULL,
    variable_table_fn=function()data.frame(name=c("i1","p1","group","y"),measurement=c("continuous","continuous","category","ordered")),
    category_table_fn=function()data.frame(name="group",value_1="Control",label_1="남성",value_2="Program",label_2="여성"))
}, {
  session$setInputs(ipa_mode="direct",ipa_design="independent",ipa_layout="wide",ipa_reference="pooled")
  stopifnot(grepl('disabled',output$ipa_reset_control$html,fixed=TRUE))
  session$setInputs(ipa_available=paste0("i",1:3),ipa_move_importance=1)
  session$setInputs(ipa_available=paste0("p",1:3),ipa_move_performance=1)
  session$setInputs(ipa_available="group",ipa_move_group=1)
  session$setInputs(ipa_marker_group="Control")
  stopifnot(grepl("남성(Control)",output$ipa_point_setup$html,fixed=TRUE),grepl("여성(Program)",output$ipa_point_setup$html,fixed=TRUE))
  stopifnot(grepl("ipa_shape_436f6e74726f6c",output$ipa_marker_controls$html,fixed=TRUE),
    !grepl("ipa_shape_50726f6772616d",output$ipa_marker_controls$html,fixed=TRUE),
    grepl("ipa-color-swatch",output$ipa_marker_controls$html,fixed=TRUE),
    grepl("ipa_connect_items",output$ipa_connection_setup$html,fixed=TRUE))
  stopifnot(grepl('analysis-reset-button',output$ipa_reset_control$html,fixed=TRUE),
    grepl('설정 초기화',output$ipa_reset_control$html,fixed=TRUE),!grepl('disabled',output$ipa_reset_control$html,fixed=TRUE))
  stopifnot(identical(r$selections$importance(),paste0("i",1:3)),identical(r$selections$performance(),paste0("p",1:3)))
  stopifnot(identical(r$items()$Importance,paste0("i",1:3)),identical(r$items()$Performance,paste0("p",1:3)))
  session$setInputs(run_ipa=1)
  stopifnot(grepl("IPA comparison",output$ipa_results$html,fixed=TRUE))
  session$setInputs(ipa_importance="i3",ipa_importance_up=1)
  stopifnot(identical(r$items()$Importance,c("i1","i3","i2")))
  # Reorder changes pairing, not merely the rendered labels.
  stopifnot(identical(r$items()$Performance,paste0("p",1:3)))
  session$setInputs(ipa_importance_doubleclick=list(value="i3"))
  fails(r$items())
  session$setInputs(ipa_available="i3",ipa_move_importance=2)
  stopifnot(nrow(r$items())==3L)
  setup<-output$ipa_setup$html
  stopifnot(grepl("measurement-continuous",setup,fixed=TRUE),grepl("measurement-category",setup,fixed=TRUE),
    grepl("measurement-continuous",output$ipa_available_panel$html,fixed=TRUE))
  stopifnot(!grepl("ipa_add",setup,fixed=TRUE),!grepl("ipa_item_label",setup,fixed=TRUE),grepl("ipa_group",setup,fixed=TRUE))
  session$setInputs(ipa_design="overall")
  stopifnot(grepl("ipa_group",output$ipa_setup$html,fixed=TRUE))
  session$setInputs(ipa_show_quadrants=FALSE,ipa_group_plot="separate",ipa_quadrant_font_size=14,
    ipa_quadrant_1="Improve now",ipa_quadrant_2="Maintain",ipa_quadrant_3="Monitor",ipa_quadrant_4="Rebalance",run_ipa=2)
  stopifnot(r$result()$design=="independent",!r$result()$show_quadrants,
    r$result()$group_plot=="separate",r$result()$quadrant_font_size==14,
    identical(r$result()$quadrant_labels,c("Improve now","Maintain","Monitor","Rebalance")))
  session$setInputs(ipa_shape_436f6e74726f6c="15",ipa_color_436f6e74726f6c="#1565C0",ipa_size_436f6e74726f6c=1.6,
    ipa_connect_items=FALSE,run_ipa=3)
  stopifnot(r$result()$point_styles$Shape[1]==15L,r$result()$point_styles$Color[1]=="#1565C0",r$result()$point_styles$Size[1]==1.6,
    identical(r$result()$connected_items,character()))
  session$setInputs(ipa_separate_mean="within",ipa_x_label="Satisfaction",ipa_y_label="Priority",run_ipa=4)
  stopifnot(r$result()$reference=="within",r$result()$x_label=="Satisfaction",r$result()$y_label=="Priority",
    r$result()$group_labels[["Control"]]=="남성(Control)")
  for(g in unique(r$result()$coordinates$Group)) {
    cr<-r$result()$coordinates;ref<-r$result()$references
    stopifnot(abs(ref$Performance[ref$Group==g]-mean(cr$Performance[cr$Group==g]))<1e-12,
      abs(ref$Importance[ref$Group==g]-mean(cr$Importance[cr$Group==g]))<1e-12)
  }
  session$setInputs(ipa_separate_mean="pooled",run_ipa=5)
  stopifnot(length(unique(r$result()$references$Performance))==1L)
  session$setInputs(ipa_reset=1)
  stopifnot(all(vapply(r$selections,function(x)length(x())==0L,logical(1))),is.null(r$result()),grepl('disabled',output$ipa_reset_control$html,fixed=TRUE))
  session$setInputs(ipa_design="paired",ipa_layout="wide")
  stopifnot(!grepl('id="ipa_id"',output$ipa_extra_setup$html,fixed=TRUE))
  paired_ui<-xml2::read_html(output$ipa_setup$html)
  stopifnot(length(xml2::xml_find_all(paired_ui,"//*[@class='analysis-transfer-listbox']"))==2L,
    !grepl('id="ipa_post_performance"',output$ipa_setup$html,fixed=TRUE))
  session$setInputs(ipa_available=paste0("i",1:3),ipa_move_importance=10)
  session$setInputs(ipa_available=paste0("p",1:3),ipa_move_performance=10)
  session$setInputs(ipa_block_post=1)
  stopifnot(grepl('id="ipa_post_performance"',output$ipa_setup$html,fixed=TRUE),!grepl('id="ipa_performance"',output$ipa_setup$html,fixed=TRUE))
  session$setInputs(ipa_available=paste0("j",1:3),ipa_move_post_importance=10)
  session$setInputs(ipa_available=paste0("q",1:3),ipa_move_post_performance=10)
  session$setInputs(ipa_block_pre=1)
  stopifnot(identical(r$items()$Performance,paste0("p",1:3)),identical(r$items()$PostPerformance,paste0("q",1:3)))
  session$setInputs(run_ipa=6)
  stopifnot(r$result()$design=="paired",identical(r$items()$PostPerformance,paste0("q",1:3)))
  session$setInputs(ipa_layout="long")
  stopifnot(grepl('id="ipa_time"',output$ipa_setup$html,fixed=TRUE),
    !grepl('id="ipa_group"',output$ipa_setup$html,fixed=TRUE),!grepl('id="ipa_time"',output$ipa_extra_setup$html,fixed=TRUE))
  session$setInputs(ipa_available="group",ipa_move_time=1)
  stopifnot(grepl('ipa-id-row',output$ipa_extra_setup$html,fixed=TRUE),
    grepl('사전: 남성 (Control) → 사후: 여성 (Program)',output$ipa_extra_setup$html,fixed=TRUE),
    !grepl('id="ipa_pre"',output$ipa_extra_setup$html,fixed=TRUE))
  session$setInputs(ipa_swap_times=1)
  stopifnot(grepl('사전: 여성 (Program) → 사후: 남성 (Control)',output$ipa_extra_setup$html,fixed=TRUE))
  session$setInputs(ipa_time="group",ipa_time_active=1,ipa_move_time=2)
  session$setInputs(ipa_available="group",ipa_available_active=1,ipa_move_time=3)
  stopifnot(grepl('사전: 남성 (Control) → 사후: 여성 (Program)',output$ipa_extra_setup$html,fixed=TRUE))
  session$setInputs(ipa_importance="i1",ipa_importance_active=1,ipa_move_importance=11)
  stopifnot(!"i1" %in% r$selections$importance(),grepl('data-value="i1"',output$ipa_available_panel$html,fixed=TRUE))
  session$setInputs(ipa_available="y",ipa_available_active=2,ipa_move_importance=12)
  stopifnot(!"y" %in% r$selections$importance())
  session$setInputs(ipa_design="overall",ipa_available="y",ipa_move_performance=11)
  stopifnot(!"y" %in% r$selections$performance())
  session$setInputs(ipa_available="i1",ipa_move_group=2)
  stopifnot(!"i1" %in% r$selections$group())

})
shiny::testServer(function(input,output,session) {
  paired_data<-long;paired_data$time<-ifelse(paired_data$time=="before",1,2)
  r<-register_ipa_handlers(input,output,session,function()paired_data,function()names(paired_data),function()"ko",function()NULL)
}, {
  session$setInputs(ipa_mode="direct",ipa_design="paired",ipa_layout="long",ipa_reference="pooled")
  session$setInputs(ipa_available=paste0("i",1:3),ipa_move_importance=1)
  session$setInputs(ipa_available=paste0("p",1:3),ipa_move_performance=1)
  session$setInputs(ipa_available="id",ipa_move_id=1)
  session$setInputs(ipa_available="time",ipa_move_time=1)
  session$setInputs(run_ipa=1)
  stopifnot(!is.null(r$result()))
  initial<-r$result()$comparisons$Difference
  stopifnot(max(abs(initial-results$direct_paired$comparisons$Difference))<1e-12)
  session$setInputs(ipa_swap_times=1)
  stopifnot(is.null(r$result()))
  session$setInputs(run_ipa=2)
  stopifnot(!is.null(r$result()),max(abs(initial+r$result()$comparisons$Difference))<1e-12)
})
cat("PASS: six modes; matching; independent/paired inference; partial correlations; RNG; synchronized bootstrap; scope exclusion; invalid data; common references; Shiny execution; time-order reversal.\n")
if("--exports" %in% commandArgs(TRUE)) {
  normalize<-function(x)gsub("[[:space:]\u00a0]+","",paste(x,collapse=""),perl=TRUE)
  base<-list(id="ipa",title="IPA independent groups",html=html)
  after<-list(id="ipa-paired",title="IPA paired derived importance",html=as.character(ipa_results_ui(results$derived_paired,"en")))
  for(mode in c("current","accumulated")) {
    entries<-if(mode=="current")list(base) else list(base,after)
    stem<-file.path(out,mode)
    write_result_collection_html(entries,paste0(stem,".html"))
    write_result_collection_docx(entries,paste0(stem,".docx"))
    save_result_collection_excel_file(entries,paste0(stem,".xlsx"))
    write_result_collection_pdf(entries,paste0(stem,".pdf"))
    write_result_collection_hwpx(entries,paste0(stem,".hwpx"))
    expected<-c("IPA comparison","Model overview","Importance and performance descriptive statistics","Quadrant classification","Group/time differences","Holm p","Control","Program","LLCI","ULCI","M ± SD","M (LLCI~ULCI)",
      sprintf("%.3f ± %.3f",ind$totals$Performance[1],ind$totals$PerformanceSD[1]),
      formatC(ind$comparisons$Difference,digits=3,format="f"))
    expected<-c(expected,paste0(seq_len(nrow(ind$items))," = ",ind$items$Item))
    if(mode=="accumulated")expected<-c(expected,"Importance is the partial Pearson correlation","Post","Pre",formatC(results$derived_paired$comparisons$Difference,digits=3,format="f"))
    jsonlite::write_json(expected,paste0(stem,"-expected.json"),auto_unbox=TRUE)
    word<-normalize(xml2::xml_text(xml2::read_xml(unz(paste0(stem,".docx"),"word/document.xml"))))
    members<-unzip(paste0(stem,".hwpx"),list=TRUE)$Name
    hwpx<-normalize(vapply(members[grepl("Contents/section[0-9]+[.]xml$",members)],function(s)xml2::xml_text(xml2::read_xml(unz(paste0(stem,".hwpx"),s))),character(1)))
    wb<-openxlsx::loadWorkbook(paste0(stem,".xlsx"))
    excel<-normalize(unlist(lapply(seq_along(names(wb)),function(i)as.matrix(openxlsx::read.xlsx(wb,sheet=i,colNames=FALSE)))))
    for(value in expected)for(actual in list(word,hwpx,excel))stopifnot(grepl(normalize(value),actual,fixed=TRUE))
    for(ext in c("docx","hwpx","xlsx")) {
      members<-unzip(paste0(stem,".",ext),list=TRUE)$Name
      stopifnot(sum(grepl("[.](png|jpg|jpeg)$",members,ignore.case=TRUE))>=if(mode=="current")2 else 4)
    }
    cat("PASS:",mode,"HTML/Word/HWPX/Excel text and chart inclusion; PDF generated.\n")
  }
}

