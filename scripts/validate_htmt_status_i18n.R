Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
fit <- lavaan::cfa('Review =~ x1+x2+x3\nNormality =~ x4+x5+x6',data=lavaan::HolzingerSwineford1939)
base <- list(fit=fit,htmt_bootstrap=100L,htmt_seed=123L)
cases <- list(pending=modifyList(base,list(cfa_bootstrap_pending=TRUE)),canceled=modifyList(base,list(cfa_bootstrap_canceled=TRUE)),failed=base)
boot <- data.frame(`Factor 1`='Review',`Factor 2`='Normality',Lower=.2,Upper=.8,`One-sided upper`=.7,`Valid replicates`=70,`Valid %`=70,Status='Caution',`CI method`='percentile',check.names=FALSE)
cases$caution <- modifyList(base,list(htmt_bootstrap_result=boot))
boot$Status <- 'Unreliable';boot[['Valid replicates']] <- 40;boot[['Valid %']] <- 40
cases$unreliable <- modifyList(base,list(htmt_bootstrap_result=boot))
boot$Status <- 'Adequate';boot[['Valid replicates']] <- 100;boot[['Valid %']] <- 100;boot[['CI method']] <- 'BCa unavailable'
cases$bca <- modifyList(base,list(htmt_bootstrap_result=boot))
boot[['CI method']] <- 'percentile';cases$adequate <- modifyList(base,list(htmt_bootstrap_result=boot))
out <- 'tmp/htmt-status-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
entries <- list();counts <- c(pending=1,canceled=1,failed=1,caution=2,unreliable=2,bca=1,adequate=0)
for(state in names(cases))for(language in c('en','ko','ja','zh','es','fr','de','vi')) {
 env <- new.env(parent=globalenv());env$fit_result <- function()cases[[state]]
 env$app_language_fn <- function()language;env$result_table <- NULL;env$table_number_fn <- NULL
 calls <- character()
 env$statedu_localized_text <- function(lang,en,ko) {
  value <- statedu_localized_text(lang,en,ko)
  if(grepl('^(HTMT bootstrap intervals are being|The HTMT bootstrap was stopped|HTMT bootstrap confidence intervals could|Some bootstrap resamples could|BCa unavailable means|HTMT bootstrap status)',en)) {
   calls <<- c(calls,value)
   if(lang!='en')stopifnot(value!=en)
  }
  value
 }
 render <- eval(body(structural_canvas_register_htmt_outputs)[[2]][[3]],env)
 html <- as.character(render(TRUE));doc <- xml2::read_html(html,encoding='UTF-8')
 notes <- xml2::xml_text(xml2::xml_find_all(doc,'//p'))
 normalize <- function(x)gsub('[.;[:space:]]','',x)
 stopifnot(length(calls)==counts[[state]],all(normalize(calls) %in% normalize(notes)))
 main <- as.character(render(FALSE))
 if(language=='en')main_en <- main else stopifnot(identical(main,main_en))
 if(language=='ja')entries[[state]] <- list(id=state,title=state,html=html)
 cat('PASS:',state,language,'conditional status messages and unchanged English main table\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
