Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
source('scripts/validate_cfa_mi_holdout.R',encoding='UTF-8')
out <- 'tmp/mi-history-holdout-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
history_expression <- body(structural_canvas_register_mi_render_outputs)[[3]][[3]][[2]]
holdout_expression <- body(structural_canvas_register_mi_render_outputs)[[4]][[3]][[2]]
history <- data.frame(Step=1:3,Parameter=c('Review','사용자 <&> %s','Normality'),Signature=letters[1:3],
 MI=c(12,13,14),EPC=c(.1,.2,.3),Justification=c('Review','사용자 근거 <&> %s',''),stringsAsFactors=FALSE)
base <- list(mi_history=history,mi_holdout_enabled=TRUE,analysis_data=holdout_split$exploration,
 validation_data=holdout_split$validation,mi_holdout_seed=13579L)
bad <- holdout_comparison;bad$table$Admissible[1] <- FALSE
bad$changes[,vapply(bad$changes,is.numeric,logical(1))] <- NA_real_
bad$changes$`Comparison status` <- 'Suppressed because one or both validation models are inadmissible'
cases <- list(reserved=NULL,evaluated=holdout_comparison,inadmissible=bad)
entries <- list();english <- list();prefix <- 'test'
for(language in c('en','ko','ja','zh','es','fr','de','vi'))for(state in names(cases)) {
 bundle <- base;bundle$holdout_comparison <- cases[[state]]
 fit_result <- function()bundle;app_language_fn <- function()language
 h <- as.character(eval(history_expression,new.env(parent=globalenv())))
 # Wrap the expression in a function to support its early return for reserved holdouts.
 render_holdout <- function()NULL;body(render_holdout) <- holdout_expression
 v <- as.character(render_holdout());html <- paste(h,v)
 doc <- xml2::read_html(html,encoding='UTF-8')
 texts <- xml2::xml_text(xml2::xml_find_all(doc,'//h4|//h5|//p'))
 cells <- trimws(xml2::xml_text(xml2::xml_find_all(doc,'//td')))
 stopifnot(all(history$Parameter %in% cells),all(history$Justification[1:2] %in% cells),identical(base$mi_history,history))
 if(language=='en')english[[state]] <- texts else stopifnot(all(texts!=english[[state]]))
 if(language!='en')stopifnot(!any(grepl('N used|Admissibility reasons|Both validation models admissible|Suppressed because',
   xml2::xml_text(xml2::xml_find_all(doc,'//th|//td')))))
 if(state!='reserved')stopifnot(grepl('13579',xml2::xml_text(doc),fixed=TRUE))
 writeLines(html,file.path(out,paste0(language,'-',state,'.html')),useBytes=TRUE)
 if(language=='ja')entries[[state]] <- list(id=state,title=state,html=html)
 cat('PASS:',language,state,'history user text, holdout messages, seed and status\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
for(language in c('en','ko','ja','zh','es','fr','de','vi')) {
 gates <- list(structural_canvas_mi_validation_gate(FALSE),structural_canvas_mi_validation_gate(TRUE,FALSE),
   structural_canvas_mi_validation_gate(TRUE,TRUE),structural_canvas_mi_validation_gate(TRUE,TRUE,bad),
   structural_canvas_mi_validation_gate(TRUE,TRUE,holdout_comparison))
 for(gate in gates)if(language!='en')stopifnot(structural_canvas_mi_validation_label(gate,language)!=gate$label)
 stopifnot(structural_canvas_mi_validation_label(list(label='custom label'),language)=='custom label')
}
