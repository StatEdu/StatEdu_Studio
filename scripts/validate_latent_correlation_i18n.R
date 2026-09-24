Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
model <- 'Review =~ x1+x2+x3\nNormality =~ x4+x5+x6'
cases <- list(estimated=lavaan::cfa(model,data=lavaan::HolzingerSwineford1939),fixed=lavaan::cfa(paste(model,'Review ~~ 0*Normality',sep='\n'),data=lavaan::HolzingerSwineford1939))
expr <- body(structural_canvas_register_latent_correlation_outputs)[[2]][[3]][[2]]
out <- 'tmp/latent-correlation-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
entries <- list();english <- list();numbers <- list()
for(state in names(cases))for(language in c('en','ko','ja','zh','es','fr','de','vi')) {
 env <- new.env(parent=globalenv());env$fit_result <- function()list(fit=cases[[state]])
 env$app_language_fn <- function()language
 env$display_name_for <- function(bundle)function(x)ifelse(x=='Normality','Normality 사용자 <&> %s',x)
 render <- function()NULL;body(render) <- expr;environment(render) <- env
 html <- as.character(render());doc <- xml2::read_html(html,encoding='UTF-8')
 cells <- xml2::xml_text(xml2::xml_find_all(doc,'//td'))
 stopifnot(all(c('Review','Normality 사용자 <&> %s') %in% cells))
 numeric <- cells[grepl('^[-.<0-9]|^—$',cells)]
 notes <- xml2::xml_text(xml2::xml_find_all(doc,'//h5|//p'))
 if(language=='en'){english[[state]] <- notes;numbers[[state]] <- numeric} else {
  stopifnot(all(notes!=english[[state]]),identical(numeric,numbers[[state]]))
  stopifnot(!any(cells %in% c('Estimated','Fixed','Not assessed')))
 }
 if(state=='fixed')stopifnot('—' %in% cells)
 if(language=='ja')entries[[state]] <- list(id=state,title=state,html=html)
 cat('PASS:',state,language,'actual fit, fixed p omission, numeric precision and user labels\n')
}
fixture <- structural_canvas_latent_correlation_intervals(cases$estimated)
fixture <- fixture[rep(1,3),];fixture[['CI reaches |1|']] <- c('Yes','No','Not assessed')
fixture[['Factor 1']] <- c('Review','Estimated','Fixed');fixture[['Factor 2']] <- c('Normality','Yes','No')
for(language in c('en','ko','ja','zh','es','fr','de','vi')) {
 displayed <- structural_canvas_latent_correlation_display(fixture,language)
 stopifnot(identical(displayed[[1]],fixture[[1]]),identical(displayed[[2]],fixture[[2]]))
 if(language!='en')stopifnot(all(displayed[[8]][c(1,3)]!=fixture[[8]][c(1,3)]),names(displayed)[8]!='CI reaches |1|')
 html <- as.character(structural_canvas_basic_html_table(displayed,role='appendix',language=language))
 cells <- xml2::xml_text(xml2::xml_find_all(xml2::read_html(html,encoding='UTF-8'),'//td'))
 stopifnot(all(c(fixture[[1]],fixture[[2]]) %in% cells))
 if(language=='ja')entries$branches <- list(id='branches',title='Boundary flags',html=html)
 cat('PASS:',language,'boundary flags and protected factor names\n')
}
single_fit <- lavaan::cfa('Review =~ x1+x2+x3',data=lavaan::HolzingerSwineford1939)
env$fit_result <- function()list(fit=single_fit)
stopifnot(is.null(render()))
saveRDS(unname(entries),file.path(out,'entries.rds'))
