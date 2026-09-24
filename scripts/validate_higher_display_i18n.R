Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
fit <- lavaan::cfa('Normality =~ x1+x2+x3\nPrimary =~ x4+x5+x6\nF3 =~ x7+x8+x9\nReview =~ Normality+Primary+F3',data=lavaan::HolzingerSwineford1939)
snapshot <- list(nodes=lapply(c('Review','Normality','Primary','F3'),function(x)list(id=x,name=x)),edges=lapply(c('Normality','Primary','F3'),function(x)list(from='Review',to=x,pathType='higherOrder')))
base <- structural_canvas_higher_order_results(snapshot,fit);omega <- structural_canvas_omega_h(snapshot,fit)
stopifnot(base$available,omega$available)
expr <- body(structural_canvas_register_local_fit_outputs)[[4]][[3]][[2]]
out <- 'tmp/higher-display-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
entries <- list();english <- list();numbers <- list()
for(state in c('actual','weak','inadmissible','unavailable'))for(language in c('en','ko','ja','zh','es','fr','de','vi')) {
 env <- new.env(parent=globalenv());env$fit_result <- function()list(fit=fit,snapshot=snapshot)
 env$app_language_fn <- function()language;env$display_name_for <- function(bundle)identity
 env$structural_canvas_higher_order_results <- function(snapshot,fit) {
  result <- base
  if(state=='weak')result$table$Beta <- .2
  if(state=='inadmissible')result$table$ResidualVariance <- -0.1
  if(state=='unavailable')result$table$Beta <- NA_real_
  result
 }
 env$structural_canvas_omega_h <- function(snapshot,fit) {
  result <- omega
  if(state=='weak')result$omega_h <- .5
  if(state=='inadmissible')result$omega_h <- 1.1
  if(state=='unavailable'){result$available <- FALSE;result$reason <- 'Omega-h denominator is not positive and finite.'}
  result
 }
 render <- function()NULL;body(render) <- expr;environment(render) <- env
 html <- as.character(render());doc <- xml2::read_html(html,encoding='UTF-8')
 cells <- xml2::xml_text(xml2::xml_find_all(doc,'//td'))
 stopifnot(all(c('Review','Normality','Primary') %in% cells),'—' %in% cells)
 numeric <- cells[grepl('^[-.<0-9]|^—$',cells)]
 notes <- xml2::xml_text(xml2::xml_find_all(doc,'//h4|//p'))
 if(language=='en'){english[[state]] <- notes;numbers[[state]] <- numeric} else {
  stopifnot(all(notes!=english[[state]]),identical(numeric,numbers[[state]]))
  stopifnot(!any(grepl('At/above|Weak loading|Review residual|Not assessed|Below common|Review inadmissible',cells)))
  headers <- xml2::xml_text(xml2::xml_find_all(doc,'//th'))
  stopifnot(!any(headers %in% c('Higher-order factor','Lower-order factor','Guidance','Residual variance','Hierarchical omega (ωh)')))
 }
 if(state=='inadmissible')stopifnot(any(grepl('†',cells,fixed=TRUE)))
 if(language=='ja')entries[[state]] <- list(id=state,title=state,html=html)
 cat('PASS:',state,language,'notes, guidance, original names and numeric formatting\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
