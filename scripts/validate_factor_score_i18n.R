Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
data <- lavaan::HolzingerSwineford1939
fit <- lavaan::cfa('Review =~ x1 + x2 + x3',data=data)
ordered_data <- data
for(v in c('x1','x2','x3')) ordered_data[[v]] <- cut(data[[v]],breaks=unique(quantile(data[[v]],c(0,.25,.5,.75,1))),include.lowest=TRUE,labels=FALSE)
ordered_fit <- lavaan::cfa('Review =~ x1 + x2 + x3',data=ordered_data,ordered=c('x1','x2','x3'))
ordered_quality <- structural_canvas_factor_score_quality(ordered_fit)
# lavPredict does not provide reliability for this ordinal fit in the bundled lavaan.
# Exercise the conditional explanatory note with the validated continuous score fixture.
cases <- list(continuous=list(fit=fit),ordered=list(fit=if(nrow(ordered_quality))ordered_fit else fit,ordered=c('x1','x2','x3')))
cat('Ordinal fit score-quality rows:',nrow(ordered_quality),'(zero means panel is unavailable)\n')
expr <- body(structural_canvas_register_factor_score_outputs)[[2]][[3]][[2]]
if(!nrow(ordered_quality)) {
 empty_env <- new.env(parent=globalenv());empty_env$fit_result <- function()list(fit=ordered_fit)
 empty_env$app_language_fn <- function()'ja'
 empty_render <- function()NULL;body(empty_render) <- expr;environment(empty_render) <- empty_env
 stopifnot(is.null(empty_render()))
 cat('PASS: unsupported ordinal score reliability leaves the panel empty\n')
}
out <- 'tmp/factor-score-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
entries <- list();english <- list();numbers <- list()
for(state in names(cases)) {
 stopifnot(nrow(structural_canvas_factor_score_quality(cases[[state]]$fit))==1)
 for(language in c('en','ko','ja','zh','es','fr','de','vi')) {
  env <- new.env(parent=globalenv());env$fit_result <- function()cases[[state]]
  env$app_language_fn <- function()language
  env$display_name_for <- function(bundle)function(x)ifelse(x=='Review','Review 사용자 <&> %s',x)
  render <- function()NULL;body(render) <- expr;environment(render) <- env
  html <- as.character(render());doc <- xml2::read_html(html,encoding='UTF-8')
  cells <- xml2::xml_text(xml2::xml_find_all(doc,'//td'))
  stopifnot('Review 사용자 <&> %s' %in% cells)
  numeric <- cells[grepl('^[.0-9]+$',cells)]
  notes <- xml2::xml_text(xml2::xml_find_all(doc,'//h5|//p'))
  stopifnot(length(notes)==if(state=='ordered')5 else 4)
  if(language=='en'){english[[state]] <- notes;numbers[[state]] <- numeric} else {
   stopifnot(all(notes!=english[[state]]),identical(numeric,numbers[[state]]))
  }
  if(language=='ja')entries[[state]] <- list(id=state,title=state,html=html)
  cat('PASS:',state,language,'notes, numeric precision and user labels\n')
 }
}
fixture <- data.frame(Factor=c('Review','Normality','Primary','사용자 %s'),Determinacy=c('.950','.850','.700','—'),`Score reliability`=c('.903','.723','.490','—'),Guidance=c('At/above descriptive .90 reference','Between descriptive .80 and .90 references','Below descriptive .80; review score use','Not assessed'),check.names=FALSE)
for(language in c('en','ko','ja','zh','es','fr','de','vi')) {
 displayed <- structural_canvas_factor_score_display(fixture,language)
 stopifnot(identical(displayed[[1]],fixture[[1]]),identical(displayed[[2]],fixture[[2]]),identical(displayed[[3]],fixture[[3]]))
 if(language!='en')stopifnot(all(names(displayed)[-1]!=names(fixture)[-1]),all(displayed[[4]]!=fixture[[4]]))
 html <- as.character(structural_canvas_basic_html_table(displayed,role='appendix',language=language))
 cells <- xml2::xml_text(xml2::xml_find_all(xml2::read_html(html,encoding='UTF-8'),'//td'))
 stopifnot(all(fixture$Factor %in% cells))
 if(language=='ja')entries$branches <- list(id='branches',title='Guidance branches',html=html)
 cat('PASS:',language,'all four guidance branches and protected cells\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
