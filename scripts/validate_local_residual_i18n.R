Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
data <- lavaan::HolzingerSwineford1939
names(data)[names(data)=='x1'] <- 'Review';names(data)[names(data)=='x2'] <- 'Normality'
fit <- lavaan::cfa('F =~ Review+Normality+x3+x4+x5+x6',data=data)
base <- structural_canvas_residual_diagnostics(fit);stopifnot(nrow(base$largest)>0)
cases <- list(flagged=base,empty=base,fallback=base)
cases$empty$largest <- base$largest[FALSE,]
cases$fallback$standardized_available <- FALSE;cases$fallback$largest <- base$largest[FALSE,]
expr <- body(structural_canvas_register_local_fit_outputs)[[3]][[3]][[2]]
out <- 'tmp/local-residual-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
entries <- list();english <- list();numbers <- list()
for(state in names(cases))for(language in c('en','ko','ja','zh','es','fr','de','vi')) {
 env <- new.env(parent=globalenv());env$fit_result <- function()list(fit=fit)
 env$app_language_fn <- function()language;env$table_number_fn <- function(x)'12'
 env$display_name_for <- function(bundle)identity
 env$structural_canvas_residual_diagnostics <- function(fit)cases[[state]]
 render <- function()NULL;body(render) <- expr;environment(render) <- env
 html <- as.character(render());doc <- xml2::read_html(html,encoding='UTF-8')
 cells <- xml2::xml_text(xml2::xml_find_all(doc,'//td'))
 headers <- xml2::xml_text(xml2::xml_find_all(doc,'//th'))
 stopifnot(all(c('Review','Normality') %in% cells),all(c('Review','Normality') %in% headers))
 numeric <- cells[grepl('^[-.<0-9]',cells)]
 notes <- xml2::xml_text(xml2::xml_find_all(doc,'//h4|//h5|//p'))
 if(language=='en'){english[[state]] <- notes;numbers[[state]] <- numeric} else {
  stopifnot(all(notes!=english[[state]]),identical(numeric,numbers[[state]]))
  stopifnot(!any(headers %in% c('Indicator1','Indicator2','Standardized residual','Residual scale')))
 }
 if(language=='ja')entries[[state]] <- list(id=state,title=state,html=html)
 cat('PASS:',state,language,'translated notes and headers, original matrix names and numbers\n')
}
env$structural_canvas_residual_diagnostics <- function(fit)list(available=FALSE)
stopifnot(is.null(render()))
saveRDS(unname(entries),file.path(out,'entries.rds'))
