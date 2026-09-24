Sys.setlocale('LC_CTYPE','English_United States.utf8')
source('scripts/validate_htmt_status_i18n.R',encoding='UTF-8')
boot[['CI method']] <- 'BCa';boot[['Upper < threshold']] <- 'Yes';boot[['Upper < 1']] <- 'No'
cases <- list(point=modifyList(base,list(htmt_bootstrap=0L)),bootstrap=modifyList(base,list(htmt_bootstrap_result=boot,htmt_ci_method='bca',ordered='x1')))
out <- 'tmp/htmt-details-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
entries <- list();english <- list();numbers <- list()
for(state in names(cases))for(language in c('en','ko','ja','zh','es','fr','de','vi')) {
 env <- new.env(parent=globalenv());env$fit_result <- function()cases[[state]]
 env$app_language_fn <- function()language;env$table_number_fn <- function(x)'8'
 env$result_table <- function(x)as.data.frame(setNames(list('x',1,2),c('Factor','Normality','사용자 <&> %s')),check.names=FALSE)
 render <- eval(body(structural_canvas_register_htmt_outputs)[[2]][[3]],env)
 html <- as.character(render(TRUE));doc <- xml2::read_html(html,encoding='UTF-8')
 cells <- xml2::xml_text(xml2::xml_find_all(doc,'//td'))
 headers <- xml2::xml_text(xml2::xml_find_all(doc,'//th'))
 stopifnot(!any(grepl('β',headers,fixed=TRUE)))
 if(language!='en')stopifnot(!'CI method' %in% headers)
 stopifnot(sum(cells=='Normality')==if(state=='point')1 else 2,sum(cells=='사용자 <&> %s')==if(state=='point')1 else 2)
 notes <- xml2::xml_text(xml2::xml_find_all(doc,'//h5|//p'));numeric <- cells[grepl('^[.0-9]',cells)]
 if(language=='en'){english[[state]] <- notes;numbers[[state]] <- numeric} else stopifnot(all(notes!=english[[state]]),identical(numeric,numbers[[state]]))
 if(language=='ja')entries[[state]] <- list(id=state,title=state,html=html)
 main <- as.character(render(FALSE));if(language=='en')main_en <- main else stopifnot(identical(main,main_en))
 cat('PASS:',state,language,'titles, notes, factor columns and label mapping, unchanged English main\n')
}
reasons <- c('At least two indicators per factor are required','Cross-loaded indicators prevent standard HTMT calculation','Indicator correlations are unavailable','Within-factor correlations are insufficient')
for(i in seq_along(reasons))for(language in c('en','ko','ja','zh','es','fr','de','vi')) {
 env <- new.env(parent=globalenv());env$fit_result <- function()cases$point
 env$app_language_fn <- function()language;env$table_number_fn <- NULL;env$result_table <- NULL
 env$structural_canvas_htmt <- function(...) {
  value <- structural_canvas_htmt(...);value$pairs$Reason <- reasons[[i]];value$pairs$Criterion <- 'Not assessed';value
 }
 render <- eval(body(structural_canvas_register_htmt_outputs)[[2]][[3]],env)
 html <- as.character(render(TRUE));doc <- xml2::read_html(html,encoding='UTF-8')
 cells <- xml2::xml_text(xml2::xml_find_all(doc,'//td'))
 stopifnot(all(c('Review','Normality') %in% cells))
 if(language!='en')stopifnot(!reasons[[i]] %in% cells,!'Not assessed' %in% cells)
 if(language=='ja')entries[[paste0('reason',i)]] <- list(id=paste0('reason',i),title=paste('Reason',i),html=html)
 cat('PASS:',language,'reason',i,'and unnumbered title\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
