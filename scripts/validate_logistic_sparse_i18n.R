Sys.setlocale("LC_CTYPE","English_United States.utf8")
Sys.setenv(STATEDU_MODULE_CACHE="false")
source("R/app_bootstrap.R",encoding="UTF-8")
load_app_packages(check=FALSE); source_app_modules()
info <- data.frame(name=c("사용자.A","집단.50%"),measurement=c("binary","category"))
d <- data.frame(y=factor(c(0,0,1,1)),g=factor(c(1,1,2,2)));names(d)<-info$name
zero <- logistic_sparse_cell_check(d,info$name[1],info$name[2],info)$warnings
d[[2]] <- factor(c(1,2,1,2))
sparse <- logistic_sparse_cell_check(d,info$name[1],info$name[2],info)$warnings
rare_data <- data.frame(y=factor(c(rep(0,98),1,1)),x=seq_len(100))
rare_info <- data.frame(name=c("y","x"),measurement=c("binary","continuous"))
rare <- logistic_preflight(rare_data,"y","x","binary",rare_info)$warnings$Message
messages <- c(zero,sparse,rare,logistic_epv_warning(c(rep(0,20),rep(1,4)),1),logistic_epv_warning(c(rep(0,20),rep(1,7)),1))
stopifnot(length(messages)==5,grepl("2.0%",messages[3],fixed=TRUE))
out <- "tmp/logistic-sparse-i18n";dir.create(out,recursive=TRUE,showWarnings=FALSE)
for(language in c("en","ko","ja","zh","es","fr","de","vi")) {
 options(statedu.app_language=language)
 raw <- data.frame(Variable=messages,Message=messages)
 table <- logistic_appendix_table(raw,language)
 stopifnot(identical(table[[1]],messages))
 if(language!="en") stopifnot(all(table[[2]]!=messages))
 for(i in 1:2) for(name in info$name) stopifnot(grepl(name,table[[2]][i],fixed=TRUE))
 for(i in 3:5) stopifnot(grepl(c("2.0%","4.0","7.0")[i-2],table[[2]][i],fixed=TRUE))
 if(language=="ja") saveRDS(list(list(id="logistic-sparse",title="Logistic diagnostics",html=as.character(analysis_result_table_section("希少イベントとセル度数",table)))),file.path(out,"entries.rds"))
 cat("PASS:",language,"actual sparse/zero-cell, rare-event and EPV diagnostics; names/numbers preserved\n")
}
