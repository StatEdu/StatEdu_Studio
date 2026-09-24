Sys.setlocale("LC_ALL", "Korean_Korea.utf8")
Sys.setenv(STATEDU_MODULE_CACHE = "false")
source("R/app_bootstrap.R", encoding = "UTF-8")
load_app_packages(check = FALSE)
source_app_modules()
set.seed(160926)
n <- 450L
common <- rnorm(n)
a <- .6 * common + .8 * rnorm(n)
b <- .6 * common + .8 * rnorm(n)
c <- .6 * common + .8 * rnorm(n)
data <- data.frame(a1 = .8*a + .6*rnorm(n), a2 = .8*a + .6*rnorm(n), a3 = .8*a + .6*rnorm(n))
syntax <- "A =~ a1 + a2 + a3"
groups <- list(A = c("a1", "a2", "a3"))
for (prefix in c("B", "C")) {
  k <- if (prefix == "B") 3L else 4L
  latent <- if (prefix == "B") b else c
  for (j in seq_len(k)) {
    lower <- .75 * latent + sqrt(1-.75^2)*rnorm(n)
    items <- paste0(tolower(prefix), j, "_", seq_len(k))
    for (item in items) data[[item]] <- .8*lower + .6*rnorm(n)
    name <- paste0(prefix, j)
    groups[[name]] <- items
    syntax <- paste(syntax, paste(name, "=~", paste(items, collapse = " + ")), sep = "\n")
  }
  syntax <- paste(syntax, paste(prefix, "=~", paste(paste0(prefix, seq_len(k)), collapse = " + ")), sep = "\n")
}
fit <- lavaan::cfa(syntax, data = data, std.lv = TRUE)
stopifnot(lavaan::lavInspect(fit, "converged"), lavaan::lavInspect(fit, "post.check"))
bundle <- list(fit = fit, analysis_data = data, htmt_threshold = .85, htmt_bootstrap = 40L,
  htmt_ci_method = "percentile", htmt_seed = 42L)
h <- structural_canvas_higher_htmt_result(bundle)
# Independent covariance decomposition: direct indicators, not pooled items.
check_higher_validity <- function(test_fit, mode) {
  actual <- structural_canvas_higher_validity_estimates(list(fit = test_fit, validity_formula = mode))
  parameters <- lavaan::parameterEstimates(test_fit)
  latent_cov <- lavaan::lavInspect(test_fit, "cov.lv")
  observed_cov <- lavaan::fitted(test_fit)$cov
  for (root in c("A", "B", "C")) {
    load <- parameters[parameters$lhs == root & parameters$op == "=~", ]
    children <- load$rhs
    direct <- if (root == "A") observed_cov[children, children] else latent_cov[children, children]
    signal <- tcrossprod(load$est) * latent_cov[root, root]
    residual <- direct - signal
    if (mode == "standardized") {
      scale <- diag(1 / sqrt(diag(direct)))
      signal <- scale %*% signal %*% scale
      residual <- scale %*% residual %*% scale
    }
    expected <- c(sum(diag(signal)) / sum(diag(signal + residual)), sum(signal) / sum(signal + residual))
    row <- actual[actual$Factor == root, ]
    stopifnot(!nzchar(row$Reason), max(abs(c(row$AVE, row$CR) - expected)) < 1e-10)
  }
  actual
}
validity <- check_higher_validity(fit, "standardized")
check_higher_validity(fit, "model_implied")
marker_fit <- lavaan::cfa(syntax, data = data)
marker_validity <- check_higher_validity(marker_fit, "standardized")
stopifnot(max(abs(as.matrix(validity[c("AVE", "CR")]) - as.matrix(marker_validity[c("AVE", "CR")]))) < 1e-5)
correlated_fit <- lavaan::cfa(paste(syntax, "C1 ~~ C2", sep = "\n"), data = data, std.lv = TRUE)
check_higher_validity(correlated_fit, "standardized")
check_higher_validity(correlated_fit, "model_implied")
stopifnot(h$available, h$n == n, h$excluded == 0, identical(names(h$indicators), c("A", "B", "C")),
  identical(unname(lengths(h$indicators)), c(3L,3L,4L)),
  identical(unname(lengths(h$raw_indicators)), c(3L,9L,16L)))
# Independent direct arithmetic from the original data, not helper-generated groups.
means <- cbind(data[1:3], as.data.frame(lapply(groups[-1], function(items) rowMeans(data[items]))))
manual_htmt <- function(frame, x, y) {
  r <- abs(cor(frame)); xx <- r[x,x]; yy <- r[y,y]
  mean(r[x,y]) / sqrt(mean(xx[lower.tri(xx)]) * mean(yy[lower.tri(yy)]))
}
stopifnot(abs(h$result$matrix["B","C"] - manual_htmt(means,4:6,7:10)) < 1e-12,
  abs(h$raw_result$matrix["B","C"] - manual_htmt(data,4:12,13:28)) < 1e-12,
  abs(h$raw_result$matrix["A","B"] - manual_htmt(data,1:3,4:12)) < 1e-12)
progress <- list()
ci <- structural_canvas_htmt_bootstrap_with_higher(fit,data,groups,40,seed=42,ci_method="percentile",
  progress=function(done,total,valid) progress[[length(progress)+1L]] <<- c(done,total,valid))
old <- structural_canvas_htmt_bootstrap(data,groups,40,seed=42,ci_method="percentile")
plain <- ci; attr(plain,"higher_order") <- NULL; attr(plain,"higher_order_raw") <- NULL
stopifnot(identical(plain,old), nrow(attr(ci,"higher_order"))==3, nrow(attr(ci,"higher_order_raw"))==3)
progress <- do.call(rbind,progress)
stopifnot(all(diff(progress[,1])>=0),all(diff(progress[,3])>=0),tail(progress[,1],1)==120)
bundle$htmt_bootstrap_result <- ci
# Background path must have the same numbers and account for all three passes.
args <- c(bundle,list(data=data, reliability_bootstrap=0L,bollen_stine_bootstrap=0L,
  reliability_ci_method="percentile",ordered=character()))
stopifnot(structural_canvas_cfa_bootstrap_total(args)==120)
job <- structural_canvas_cfa_bootstrap_job_value(args,tempfile())
stopifnot(identical(job$htmt_bootstrap_result,ci))
# Existing observed subscale indicators continue to use only the old HTMT path.
simple <- lavaan::cfa("A =~ a1+a2+a3\nB =~ B1+B2+B3\nC =~ C1+C2+C3+C4", data=means)
stopifnot(is.null(structural_canvas_higher_htmt_data(simple,means)))
# Missingness: same complete cases for both new point estimates and intervals.
missing <- data; missing$b1_1[1:8] <- NA
missing_fit <- lavaan::cfa(syntax,data=missing,std.lv=TRUE,missing="fiml")
mh <- structural_canvas_higher_htmt_data(missing_fit,missing)
stopifnot(mh$n==n-8L,mh$excluded==8L,nrow(mh$raw_data)==mh$n)
listwise_fit <- lavaan::cfa(syntax,data=missing,std.lv=TRUE)
lh <- structural_canvas_higher_htmt_data(listwise_fit,missing)
stopifnot(lh$n==n-8L,lh$excluded==0L)
constant <- data; constant$b1_1 <- 1
ch <- structural_canvas_higher_htmt_data(fit,constant)
stopifnot(is.na(ch$raw_result$matrix["A","B"]),is.finite(ch$raw_result$matrix["A","C"]))
# Shared item across top-level constructs must be rejected, even after averaging.
cross_fit <- suppressWarnings(lavaan::cfa(paste(syntax,"B1 =~ a1",sep="\n"),data=data,std.lv=TRUE))
xh <- structural_canvas_higher_htmt_data(cross_fit,data)
stopifnot(is.na(xh$result$matrix["A","B"]),is.na(xh$raw_result$matrix["A","B"]))
stopifnot(!structural_canvas_higher_htmt_data(fit,NULL)$available)
out <- "tmp/cfa-higher-htmt"
dir.create(out,recursive=TRUE,showWarnings=FALSE)
html <- NULL
shiny::testServer(function(input, output, session) {
  structural_canvas_register_htmt_outputs(output, "cfa", function() bundle, app_language_fn = function() "ko")
}, {
  html <<- paste0(output$cfa_result_htmt$html, output$cfa_result_htmt_details$html)
})
html <- result_snapshot_document_html("CFA higher-order HTMT",html)
writeLines(html,file.path(out,"fragment.html"),useBytes=TRUE)
doc <- xml2::read_html(html)
higher_tables <- xml2::xml_find_all(doc, "//table[contains(@class,'structural-higher-htmt-matrix')]")
stopifnot(length(higher_tables) == 2L,
  all(grepl("(^| )structural-htmt-matrix( |$)", xml2::xml_attr(higher_tables, "class"))))
for (node in higher_tables) {
  stopifnot(identical(xml2::xml_text(xml2::xml_find_all(node, ".//thead//th")), c("Factor", "A", "B", "C")))
  rows <- xml2::xml_find_all(node, ".//tbody/tr")
  for (i in seq_along(rows)) {
    cells <- trimws(xml2::xml_text(xml2::xml_find_all(rows[[i]], "./td")))
    if (i < 3L) stopifnot(all(cells[seq.int(i + 2L, 4L)] == ""))
    stopifnot(cells[[i + 1L]] == "—")
  }
}
ci_tables <- xml2::xml_find_all(doc, "//table[contains(@class,'structural-htmt-ci-main')]")
stopifnot(length(ci_tables) == 3L)
main_tables <- xml2::xml_find_all(doc, "//table[contains(@class,'structural-htmt-matrix') or contains(@class,'structural-htmt-ci-main')]")
stopifnot(identical(grepl("structural-htmt-ci-main", xml2::xml_attr(main_tables, "class")), rep(c(FALSE, TRUE), 3)))
for (i in seq_along(ci_tables)) {
  source_ci <- list(ci, attr(ci,"higher_order_raw"), attr(ci,"higher_order"))[[i]]
  headers <- xml2::xml_text(xml2::xml_find_all(ci_tables[[i]], ".//thead//th"))
  stopifnot(all(c("95% CI", "LLCI", "ULCI") %in% headers))
  rows <- xml2::xml_find_all(ci_tables[[i]], ".//tbody/tr")
  stopifnot(length(rows) == nrow(source_ci))
  for (j in seq_along(rows)) {
    cells <- trimws(xml2::xml_text(xml2::xml_find_all(rows[[j]], "./td")))
    stopifnot(cells[4] == format_decimal3(source_ci$Lower[j]), cells[5] == format_decimal3(source_ci$Upper[j]))
  }
}
stopifnot(length(xml2::xml_find_all(doc,"//table[contains(@class,'structural-higher-validity')]"))==1L,
  all(xml2::xml_attr(xml2::xml_find_all(doc,"//table[contains(@class,'structural-higher-htmt-matrix')]/ancestor::div[@data-result-table-sheet='true'][1]"),"data-result-table-orientation")=="portrait"))
saveRDS(bundle,file.path(out,"bundle.rds"))
cat("PASS: 3/9/16 original-item and 3/3/4 subscale HTMT; independent formulas; unchanged first-order bootstrap; background parity/progress; observed-score model unchanged; missingness; overlap; constants; rendered tables.\n")
if ("--exports" %in% commandArgs(TRUE)) {
  entry <- list(id="higher-htmt",title="CFA higher-order HTMT",html=html)
  expected <- c("Top-level HTMT (original items)","Top-level HTMT (subscale scores)",
    format_decimal3(h$raw_result$matrix["B","C"]),format_decimal3(h$result$matrix["B","C"]),"문항의 동일가중 평균",
    "95% CI", "LLCI", "ULCI", "Top-level reliability and convergent validity",
    vapply(attr(ci,"higher_order_raw")$Lower, format_decimal3, character(1)),
    vapply(attr(ci,"higher_order")$Upper, format_decimal3, character(1)),
    "AVE", "CR", vapply(validity$AVE, format_decimal3, character(1)), vapply(validity$CR, format_decimal3, character(1)))
  jsonlite::write_json(expected,file.path(out,"expected.json"),auto_unbox=TRUE)
  normalize <- function(x) gsub("[[:space:]\u00a0]+","",paste(x,collapse=""),perl=TRUE)
  for (mode in c("current","accumulated")) {
    entries <- list(entry)
    if(mode=="accumulated") entries <- c(list(list(id="prior",title="Prior",html="<h4>Prior result</h4><p>Preserved earlier result.</p>")),entries)
    stem <- file.path(out,mode)
    write_result_collection_html(entries,paste0(stem,".html"))
    write_result_collection_docx(entries,paste0(stem,".docx"))
    save_result_collection_excel_file(entries,paste0(stem,".xlsx"))
    write_result_collection_pdf(entries,paste0(stem,".pdf"))
    write_result_collection_hwpx(entries,paste0(stem,".hwpx"))
    word <- normalize(xml2::xml_text(xml2::read_xml(unz(paste0(stem,".docx"),"word/document.xml"))))
    members <- unzip(paste0(stem,".hwpx"),list=TRUE)$Name
    hwpx <- normalize(vapply(members[grepl("Contents/section[0-9]+[.]xml$",members)],function(s)xml2::xml_text(xml2::read_xml(unz(paste0(stem,".hwpx"),s))),character(1)))
    workbook <- openxlsx::loadWorkbook(paste0(stem,".xlsx"))
    excel <- normalize(unlist(lapply(seq_along(names(workbook)),function(i)as.matrix(openxlsx::read.xlsx(workbook,sheet=i,colNames=FALSE)))))
    stopifnot(file.info(paste0(stem,".pdf"))$size > 1000)
    for(value in expected) for(actual in list(word,hwpx,excel)) stopifnot(grepl(normalize(value),actual,fixed=TRUE))
    cat("PASS:",mode,"HTML/Word/HWPX/Excel content verified; PDF generated for external text and visual verification.\n")
  }
}

