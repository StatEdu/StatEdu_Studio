Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
expr <- body(structural_canvas_register_validity_outputs)[[2]][[3]][[3]][[3]][[2]]
out <- 'tmp/pls-validity-notes-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
entries <- list()
for(state in c('Off','Adequate','Insufficient','Pending','Failed','Canceled','Not recorded','External state'))for(language in c('en','ko','ja','zh','es','fr','de','vi')) {
 bootstrap <- list(nboot=70L,requested_nboot=100L,minimum_valid_ratio=.8,bootstrap_status=state,inference_available=state=='Adequate',failure_message=if(state=='Failed')'Review 사용자 <&> %s' else '')
 bundle <- list(pls_bootstrap=if(state=='Off')0L else 100L,pls_bootstrap_result=if(state=='Off')NULL else bootstrap)
 env <- new.env(parent=globalenv());env$fit_result <- function()bundle;env$app_language_fn <- function()language
 render <- function()NULL;body(render) <- expr;environment(render) <- env
 html <- as.character(render());doc <- xml2::read_html(html,encoding='UTF-8');notes <- xml2::xml_text(xml2::xml_find_all(doc,'//p'))
 stopifnot(length(notes)==if(state=='Off')2L else 4L)
 if(language=='en')english <- notes else stopifnot(all(notes!=english))
 if(state!='Off')stopifnot(grepl('70/100',tail(notes,1),fixed=TRUE),grepl('80%',tail(notes,1),fixed=TRUE))
 if(state=='Failed')stopifnot(grepl('Review 사용자 <&> %s',tail(notes,1),fixed=TRUE))
 if(state=='External state')stopifnot(grepl(state,tail(notes,1),fixed=TRUE))
 if(language!='en'&&!state %in% c('Off','External state'))stopifnot(!grepl(state,tail(notes,1),fixed=TRUE))
 warnings <- xml2::xml_find_all(doc,"//p[contains(@class,'structural-result-warning')]")
 stopifnot(length(warnings)==as.integer(!state %in% c('Off','Adequate')))
 if(language=='ja')entries[[state]] <- list(id=gsub(' ','',state),title=state,html=paste(as.character(structural_canvas_basic_html_table(data.frame(Construct='Review',AVE='.600'),role='main')),html))
 cat('PASS:',state,language,'notes, summary numbers, literal failure details and warning class\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
