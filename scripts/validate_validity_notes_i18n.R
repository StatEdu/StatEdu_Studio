Sys.setlocale('LC_CTYPE','English_United States.utf8')
source('scripts/validate_validity_named_warnings_i18n.R',encoding='UTF-8')
correlated_fit <- lavaan::cfa(paste(model,'x2 ~~ x4',sep='\n'),data=data)
ordered_data <- data
for(v in c('Normality','x2','x3','x4','x5','x6'))ordered_data[[v]] <- cut(data[[v]],unique(quantile(data[[v]],c(0,.25,.5,.75,1))),include.lowest=TRUE,labels=FALSE)
ordered_fit <- lavaan::cfa(model,data=ordered_data,ordered=c('Normality','x2','x3','x4','x5','x6'))
cases <- list(continuous=base,correlated=modifyList(base,list(fit=correlated_fit)),ordered=modifyList(base,list(fit=ordered_fit,ordered='Normality')))
for(mark in c('†','‡','¶','§'))cases[[mark]] <- modifyList(base,list(marker=mark))
cases$higher <- modifyList(base,list(snapshot=list(edges=list(list(pathType='higherOrder')))))
cases$inadmissible <- modifyList(base,list(diagnostics=list(admissible=FALSE)))
out <- 'tmp/validity-notes-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
entries <- list();english <- list()
for(state in names(cases))for(language in c('en','ko','ja','zh','es','fr','de','vi')) {
 env <- new.env(parent=globalenv());env$fit_result <- function()cases[[state]];env$app_language_fn <- function()language
 env$result_table <- function(x)data.frame(Factor='Review',AVE=paste0('.600',cases[[state]]$marker %||% ''),check.names=FALSE)
 render <- function()NULL;body(render) <- expr;environment(render) <- env
 html <- as.character(render());doc <- xml2::read_html(html,encoding='UTF-8');notes <- xml2::xml_text(xml2::xml_find_all(doc,'//p'))
 if(language=='en')english[[state]] <- notes else stopifnot(length(notes)==length(english[[state]]),all(notes!=english[[state]]))
 if(state %in% c('†','‡','¶','§'))stopifnot(sum(startsWith(notes,state))==1)
 expected_n <- if(state %in% c('continuous','ordered'))5L else 6L
 stopifnot(length(notes)==expected_n)
 if(language=='ja')entries[[state]] <- list(id=paste0('entry',length(entries)+1L),title=state,html=paste(as.character(structural_canvas_basic_html_table(env$result_table('validity'),role='main')),html))
 cat('PASS:',state,language,'all notes localized, conditional counts and footnote markers\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
