Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
data <- lavaan::HolzingerSwineford1939;names(data)[names(data)=='x1'] <- 'Normality'
model <- 'Review =~ Normality+x2+x3\nPrimary =~ x4+x5+x6'
fit <- lavaan::cfa(model,data=data);cross <- lavaan::cfa(paste(model,'Primary =~ Normality',sep='\n'),data=data)
snapshot <- list(nodes=list(list(id='a',name='Review',role='latent'),list(id='b',name='사용자 <&> %s',role='latent')),edges=list())
base <- list(fit=fit,snapshot=list(),diagnostics=list())
cases <- list(none=base,missing=modifyList(base,list(snapshot=snapshot)),single=modifyList(base,list(diagnostics=list(single_indicator_auto_residuals=c('Review','사용자 <&> %s')))),cross=modifyList(base,list(fit=cross)))
cases$combined <- modifyList(cases$single,list(fit=cross,snapshot=snapshot))
counts <- c(none=0L,missing=1L,single=1L,cross=1L,combined=3L)
expr <- body(structural_canvas_register_validity_note_outputs)[[2]][[3]][[2]]
out <- 'tmp/validity-named-warnings-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
entries <- list()
for(state in names(cases))for(language in c('en','ko','ja','zh','es','fr','de','vi')) {
 env <- new.env(parent=globalenv());env$fit_result <- function()cases[[state]];env$app_language_fn <- function()language
 env$result_table <- function(x)data.frame(Factor='Review',AVE='.600',check.names=FALSE)
 calls <- character()
 env$statedu_localized_text <- function(lang,en,ko){value <- statedu_localized_text(lang,en,ko);if(grepl('%s',en,fixed=TRUE))calls <<- c(calls,value);if(lang!='en')stopifnot(value!=en);value}
 render <- function()NULL;body(render) <- expr;environment(render) <- env
 html <- as.character(render());doc <- xml2::read_html(html,encoding='UTF-8');notes <- xml2::xml_text(xml2::xml_find_all(doc,'//p'))
 stopifnot(length(calls)==counts[[state]])
 if(state %in% c('missing','single','combined'))stopifnot(any(grepl('사용자 <&> %s',notes,fixed=TRUE)))
 if(state %in% c('cross','combined'))stopifnot(any(grepl('Normality',notes,fixed=TRUE)))
 expected <- switch(state,none=character(),missing='Review ↔ 사용자 <&> %s',single='Review, 사용자 <&> %s',cross='Normality',combined=c('Review ↔ 사용자 <&> %s','Review, 사용자 <&> %s','Normality'))
 normalize <- function(x)gsub('[.;[:space:]]','',x)
 if(length(calls))stopifnot(all(normalize(mapply(sprintf,calls,expected,USE.NAMES=FALSE)) %in% normalize(notes)))
 if(language=='ja') {
  table_html <- as.character(structural_canvas_basic_html_table(env$result_table('validity'),role='main'))
  entries[[state]] <- list(id=state,title=state,html=paste(table_html,html))
 }
 cat('PASS:',state,language,'conditional warning templates and literal user names\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
