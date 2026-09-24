Sys.setlocale("LC_ALL", "Korean_Korea.utf8")
Sys.setenv(STATEDU_MODULE_CACHE = "false")
source("R/app_bootstrap.R",encoding="UTF-8"); load_app_packages(check=FALSE); source_app_modules()
bundle <- readRDS("tmp/cfa-higher-htmt/bundle.rds")
fit <- bundle$fit; data <- bundle$analysis_data
pt <- lavaan::parameterTable(fit)
pt <- pt[pt$op=="=~" & pt$rhs %in% names(data),]
groups <- split(pt$rhs,pt$lhs)
set.seed(8991); rng <- .Random.seed
value <- structural_canvas_htmt_bootstrap_with_higher(fit,data,groups,40,seed=42,ci_method="bias_corrected")
stopifnot(identical(rng,.Random.seed))
h <- structural_canvas_higher_htmt_data(fit,data)
direct <- structural_canvas_htmt_bootstrap(h$data,h$indicators,40,seed=42,ci_method="bias_corrected",strict_correlations=TRUE)
stopifnot(identical(attr(value,"higher_order"),direct),all(is.finite(direct$Lower)))
# Synchronous caller and saved-result serialization preserve both CI datasets.
sync <- structural_canvas_run_htmt_bootstrap("cfa",40,list(fit=fit),data,42,character(),.85,"bias_corrected")
stopifnot(identical(sync,value))
serialized <- unserialize(serialize(sync,NULL))
stopifnot(identical(serialized,sync))
bundle$htmt_bootstrap_result <- value
for (state in c("Not requested", "Pending", "Canceled", "Unavailable")) {
  test_bundle <- bundle
  test_bundle$htmt_bootstrap_result <- NULL
  test_bundle$htmt_bootstrap <- if (state == "Not requested") 0L else 40L
  test_bundle$cfa_bootstrap_pending <- state == "Pending"
  test_bundle$cfa_bootstrap_canceled <- state == "Canceled"
  rendered <- as.character(structural_canvas_htmt_ci_html(test_bundle, h$result$pairs))
  stopifnot(grepl(state, rendered, fixed = TRUE), grepl("LLCI", rendered, fixed = TRUE))
}
# Pair order is not an implicit join contract.
forward <- as.character(structural_canvas_htmt_ci_html(bundle, h$result$pairs, direct))
reverse <- as.character(structural_canvas_htmt_ci_html(bundle, h$result$pairs, direct[nrow(direct):1, ]))
stopifnot(identical(forward, reverse))
shiny::testServer(function(input,output,session) {
  structural_canvas_register_htmt_outputs(output,"cfa",function()bundle,app_language_fn=function()"ko")
}, {
  main <- output$cfa_result_htmt$html
  detail <- output$cfa_result_htmt_details$html
  stopifnot(grepl("Top-level HTMT (original items)",main,fixed=TRUE),
    grepl("Top-level HTMT (subscale scores)",main,fixed=TRUE),
    grepl("structural-htmt-matrix",main,fixed=TRUE),grepl("부트스트랩",detail,fixed=TRUE))
})
# The numerical workbook path also includes both methods and their confidence intervals.
bundle$analysis_type <- "cfa"
sheets <- structural_canvas_result_workbook_sheets(bundle,function(kind)data.frame(Table=kind,Value=1))
stopifnot(all(c("Higher_HTMT","Higher_HTMT_Raw","Higher_HTMT_CI","Higher_HTMT_Raw_CI","Higher_HTMT_Method") %in% names(sheets)))
stopifnot(nrow(sheets$Higher_HTMT)==3,nrow(sheets$Higher_HTMT_Raw)==3)
stopifnot(identical(sheets$Higher_Validity$Factor, c("A", "B", "C")),
  all(is.finite(sheets$Higher_Validity$AVE)), all(is.finite(sheets$Higher_Validity$CR)))
cat("PASS: bias-corrected intervals, RNG, synchronous caller, serialization, reactive output registration and numeric workbook.\n")
