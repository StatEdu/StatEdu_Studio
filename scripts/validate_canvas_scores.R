Sys.setenv(STATEDU_MODULE_CACHE="false")
invisible(Sys.setlocale("LC_ALL","English_United States.utf8"))
source("R/app_bootstrap.R",encoding="UTF-8"); load_app_packages(check=FALSE); source_app_modules()
# Cached dictionaries must match the uncached builder in every UI language.
for(lang in statedu_supported_languages()) {
  base <- environment(custom_model_canvas_i18n)$build(lang)
  stopifnot(identical(custom_model_canvas_i18n(lang)[names(base)],base))
  altered <- custom_model_canvas_i18n(lang); altered$apply <- "temporary"
  stopifnot(!identical(custom_model_canvas_i18n(lang)$apply,"temporary"))
}
set.seed(922); n <- 350; f <- rnorm(n); g <- .5*f+rnorm(n)
d <- as.data.frame(setNames(lapply(1:12,function(i)(.7+i/30)*f+rnorm(n,sd=.7)),paste0("q",1:12)))
d$CD1 <- rowSums(d); for(i in 1:3)d[[paste0("y",i)]] <- g+rnorm(n,sd=.6)
s <- list(nodes=list(list(id="F",name="F",role="latent",x=300,y=100),list(id="c",name="CD1",variableId="CD1",role="indicator",x=150,y=100),list(id="e",name="delta",role="error",x=70,y=100),list(id="G",name="G",role="latent",x=500,y=300)),
  edges=list(list(id="fc",from="F",to="c",kind="regression"),list(id="ec",from="e",to="c",kind="regression"),list(id="fg",from="F",to="G",kind="regression")))
for(i in 1:3){s$nodes <- c(s$nodes,list(list(id=paste0("y",i),name=paste0("y",i),role="indicator",x=700,y=200+i*70)));s$edges <- c(s$edges,list(list(id=paste0("gy",i),from="G",to=paste0("y",i),kind="regression")))}
cfg <- list(mode="single",items=as.list(paste0("q",1:12)),method="alpha",scoring="sum")
fail <- function(expr)stopifnot(inherits(tryCatch({force(expr);NULL},error=identity),"error"))
a <- canvas_score_apply(s,"c",d,cfg)
calc <- a$nodes[[1]]$scoreDesign$calculation
stopifnot(abs(calc$reliability-reliability_alpha_value(d[,paste0("q",1:12)]))<1e-12,
  abs(calc$residual-(1-calc$reliability)*var(d$CD1))<1e-12,
  identical(d$CD1,rowSums(d[,paste0("q",1:12)])))
for(scale in c(FALSE,TRUE)){
  fit <- run_structural_canvas_analysis(a,d,"cbsem",std_lv=scale)$fit
  p <- lavaan::parameterTable(fit)
  stopifnot(p$free[p$lhs=="F" & p$op=="=~" & p$rhs=="CD1"]==0,
    p$est[p$lhs=="F" & p$op=="=~" & p$rhs=="CD1"]==1,
    p$free[p$lhs=="F" & p$op=="~~" & p$rhs=="F"]>0,
    abs(p$est[p$lhs=="CD1" & p$op=="~~" & p$rhs=="CD1"]-calc$residual)<1e-8)
}
omega <- canvas_score_calculate(d,"CD1",cfg$items,"omega","sum")
stopifnot(omega$reliability>0,omega$reliability<1)
dm <- d; dm$CD1 <- dm$CD1/12
cm <- canvas_score_calculate(dm,"CD1",cfg$items,"alpha","mean")
stopifnot(abs(cm$residual*144-calc$residual)<1e-10)
bad <- d;bad$CD1[1] <- bad$CD1[1]+1;fail(canvas_score_apply(s,"c",bad,cfg))
bad <- d;bad$q1[1] <- NA_real_;fail(canvas_score_apply(s,"c",bad,cfg))
attr(bad,"statedu_scope_excluded") <- "q2";fail(canvas_score_apply(s,"c",bad,cfg))
# Twelve items require ten responses at the 80% threshold.
partial <- dm
partial[1,c("q1","q2")] <- NA_real_
partial[2,c("q1","q2","q3")] <- NA_real_
partial$CD1 <- rowMeans(partial[,paste0("q",1:12)],na.rm=TRUE)
partial$CD1[2] <- NA_real_
pconfig <- modifyList(cfg,list(scoring="mean",minResponse=.8))
pmodel <- canvas_score_apply(s,"F",partial,pconfig)
pc <- pmodel$nodes[[1]]$scoreDesign$calculation
pv <- cov(partial[-2,paste0("q",1:12)],use="pairwise.complete.obs")
stopifnot(pc$n==349L,pc$incomplete_n==1L,pc$min_pair_n==348,
  abs(pc$variance-var(partial$CD1,na.rm=TRUE))<1e-12,
  abs(pc$reliability-12/11*(1-sum(diag(pv))/sum(pv)))<1e-12)
po <- canvas_score_calculate(partial,"CD1",cfg$items,"omega","mean",.8)
stopifnot(po$n==349L,po$reliability>0,po$reliability<1)
fail(canvas_score_calculate(partial,"CD1",cfg$items,"alpha","mean",1))
below <- partial; below$CD1[2] <- 0
fail(canvas_score_apply(s,"F",below,pconfig))
roundtrip <- jsonlite::fromJSON(jsonlite::toJSON(pmodel,auto_unbox=TRUE,null="null",digits=NA),simplifyVector=FALSE)
refreshed <- canvas_score_refresh(roundtrip,partial)
stopifnot(refreshed$nodes[[1]]$scoreDesign$minResponse==.8,
  abs(refreshed$nodes[[1]]$scoreDesign$calculation$residual-pc$residual)<1e-12)
message("PASS: 80% threshold, partial means, pairwise alpha, FIML omega, score variance and saved policy refresh")
pcfg <- modifyList(cfg,list(mode="parcels",groups=as.list(rep(1:3,4)),reviewed=TRUE,rationale="Unidimensional fixture; content-balanced allocation"))
b <- canvas_score_apply(a,"F",d,pcfg)
pd <- canvas_score_data(b,d)
stopifnot(all(pd$CD1_parcel1==rowSums(d[,c("q1","q4","q7","q10")])) , !"CD1_parcel1" %in% names(d))
fit <- run_structural_canvas_analysis(b,d,"cbsem")$fit
stopifnot(isTRUE(lavaan::lavInspect(fit,"converged")),all(paste0("CD1_parcel",1:3) %in% lavaan::lavNames(fit,"ov")))
balanced_cfg <- modifyList(pcfg,list(allocationMethod="loading_balance",count=3L))
balanced <- canvas_score_apply(a,"F",d,balanced_cfg)
ba <- balanced$nodes[[1]]$scoreDesign$allocation
for(k in 3:5) {
  allocation <- canvas_parcel_balance(d,cfg$items,k)
  groups <- unlist(allocation$groups)
  stopifnot(length(groups)==12L,min(table(groups))>=2,max(table(groups))-min(table(groups))<=1)
  reversed <- canvas_parcel_balance(d,rev(unlist(cfg$items)),k)
  stopifnot(identical(groups,rev(unlist(reversed$groups))))
}
ranked <- order(-unlist(ba$loadings),unlist(ba$items))
stopifnot(identical(as.integer(unlist(ba$groups)[ranked]),c(1L,2L,3L,3L,2L,1L,1L,2L,3L,3L,2L,1L)))
invalid <- d;invalid$q1 <- -invalid$q1
fail(canvas_parcel_balance(invalid,cfg$items,3))
diag <- canvas_parcel_balance(invalid,cfg$items,3,diagnostics=TRUE)
stopifnot(is.null(diag$groups),grepl("q1 = -",diag$issue,fixed=TRUE),length(diag$loadings)==12L,
  grepl("CFI",as.character(canvas_parcel_allocation_ui(diag,"ko")),fixed=TRUE))
fail(canvas_parcel_balance(d,cfg$items,3.5))
stopifnot(isTRUE(lavaan::lavInspect(run_structural_canvas_analysis(balanced,d,"cbsem")$fit,"converged")))
serial_balance <- jsonlite::fromJSON(jsonlite::toJSON(balanced,auto_unbox=TRUE,null="null",digits=NA),simplifyVector=FALSE)
stopifnot(identical(unlist(serial_balance$nodes[[1]]$scoreDesign$groups),unlist(balanced$nodes[[1]]$scoreDesign$groups)),
  isTRUE(all.equal(canvas_score_data(serial_balance,d),canvas_score_data(balanced,d),check.attributes=FALSE)))
message("PASS: loading-balanced parcels 3–5, rank assignment, order invariance, invalid inputs, SEM fit and persistence")
serialized <- jsonlite::fromJSON(jsonlite::toJSON(b,auto_unbox=TRUE,null="null",digits=NA),simplifyVector=FALSE)
stopifnot(isTRUE(all.equal(canvas_score_data(serialized,d),pd,check.attributes=FALSE)))
back <- canvas_score_apply(b,"F",d,cfg)
stopifnot(length(Filter(function(x)!is.null(x$scoreSpec),back$nodes))==0L)
badcfg <- pcfg; badcfg$groups <- as.list(rep(1L,12)); fail(canvas_score_apply(s,"c",d,badcfg))
dir.create("tmp/canvas-scores",recursive=TRUE,showWarnings=FALSE)
jsonlite::write_json(list(source=s,single=a,parcels=b,data=d),"tmp/canvas-scores/fixture.json",auto_unbox=TRUE,null="null",digits=NA)
saveRDS(list(source=s,single=a,parcels=b,data=d),"tmp/canvas-scores/fixture.rds")
message("PASS: raw alpha, covariance omega, sum/mean scaling, fixed constraints with both scaling options, mismatch/missing/scope guards, actual parcel fitting, model roundtrip and return to single score")
if("--exports" %in% commandArgs(TRUE)){
  html <- as.character(tagList(canvas_score_audit_ui(a,"ko"),canvas_score_audit_ui(b,"ko"),canvas_score_audit_ui(pmodel,"ko"),canvas_score_audit_ui(balanced,"ko")))
  for(mode in c("current","accumulated")){
    entries <- list(list(id="score",title="Score model",html=html))
    if(mode=="accumulated")entries <- c(list(list(id="old",title="Prior",html="<p>Prior result</p>")),entries)
    stem <- file.path("tmp/canvas-scores",mode)
    write_result_collection_html(entries,paste0(stem,".html"));write_result_collection_docx(entries,paste0(stem,".docx"))
    save_result_collection_excel_file(entries,paste0(stem,".xlsx"));write_result_collection_pdf(entries,paste0(stem,".pdf"));write_result_collection_hwpx(entries,paste0(stem,".hwpx"))
    for(ext in c("docx","xlsx","hwpx")){
      dest <- paste0(stem,"-",ext);dir.create(dest,showWarnings=FALSE);unzip(paste0(stem,".",ext),exdir=dest)
      xml <- list.files(dest,pattern="[.]xml$",full.names=TRUE,recursive=TRUE)
      text <- paste(unlist(lapply(xml,readLines,warn=FALSE,encoding="UTF-8")),collapse="")
      stopifnot(grepl("q1",text,fixed=TRUE),grepl("Cronbach",text,fixed=TRUE),grepl("Parcel",text,fixed=TRUE),grepl("FIML",text,fixed=TRUE),grepl("349",text,fixed=TRUE))
    }
    stopifnot(file.info(paste0(stem,".pdf"))$size>1000)
    message("PASS: ",mode," score definitions HTML/PDF/Word/HWPX/Excel")
  }
}
