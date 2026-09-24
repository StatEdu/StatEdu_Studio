Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
values <- data.frame(Factor=c('Review','Normality','Primary'),Statistic=c('AVE','Alpha','Omega'),Estimate=c(.6,.8,.8),Lower=c(.5,.7,.7),Upper=c(.7,.9,.9),`Valid replicates`=100L,`Requested replicates`=100L,`Valid %`=100,Status='Adequate',check.names=FALSE)
base <- list(reliability_bootstrap=100L,reliability_seed=123L,reliability_bootstrap_result=values)
cases <- list()
for(method in c('bca','bias_corrected','percentile'))for(formula in c('model_implied','standardized'))cases[[paste(method,formula,sep='_')]] <- modifyList(base,list(reliability_ci_method=method,validity_formula=formula))
bad <- values;bad$Lower <- -.1;bad$Upper <- 1.1;bad$Status <- c('Caution','Unreliable','Adequate');bad[['Valid replicates']] <- c(70L,40L,100L);bad[['Valid %']] <- c(70,40,100);bad[['CI method']] <- 'BCa unavailable'
cases$diagnostics <- modifyList(base,list(reliability_bootstrap_result=bad,reliability_ci_method='bca'))
early <- list(point=list(reliability_bootstrap=0L),pending=list(reliability_bootstrap=100L,cfa_bootstrap_pending=TRUE),canceled=list(reliability_bootstrap=100L,cfa_bootstrap_canceled=TRUE),failed=list(reliability_bootstrap=100L))
cases <- c(cases,early)
expr <- body(structural_canvas_register_reliability_bootstrap_outputs)[[2]][[3]][[2]]
out <- 'tmp/reliability-display-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
entries <- list();english <- list();numbers <- list();early_html <- character()
for(state in names(cases))for(language in c('en','ko','ja','zh','es','fr','de','vi')) {
 env <- new.env(parent=globalenv());env$fit_result <- function()cases[[state]];env$app_language_fn <- function()language
 render <- function()NULL;body(render) <- expr;environment(render) <- env
 html <- as.character(render());doc <- xml2::read_html(html,encoding='UTF-8')
 notes <- xml2::xml_text(xml2::xml_find_all(doc,'//h5|//p'));cells <- xml2::xml_text(xml2::xml_find_all(doc,'//td'))
 numeric <- cells[grepl('^[-.0-9]',cells)]
 if(language=='en'){english[[state]] <- notes;numbers[[state]] <- numeric} else stopifnot(all(notes!=english[[state]]),identical(numeric,numbers[[state]]))
 if(!state %in% names(early)) {
  stopifnot(all(c('Review','Normality','Primary') %in% cells),"Cronbach's α" %in% cells,"McDonald's ωtotal" %in% cells)
  if(state=='diagnostics')stopifnot(any(grepl('†',cells,fixed=TRUE)))
  if(language!='en')stopifnot(!any(cells %in% c('Adequate','Caution','Unreliable','BCa unavailable')))
  headers <- xml2::xml_text(xml2::xml_find_all(doc,'//th'))
  if(language!='en')stopifnot(!'Estimate' %in% headers,!'CI method' %in% headers)
 }
 if(language=='ja') {
  if(state %in% names(early))early_html <- c(early_html,html) else entries[[state]] <- list(id=state,title=state,html=html)
 }
 cat('PASS:',state,language,'conditional notes, methods, formulas, factor names and numeric precision\n')
}
entries[[1]]$html <- paste(c(entries[[1]]$html,early_html),collapse='\n')
saveRDS(unname(entries),file.path(out,'entries.rds'))
