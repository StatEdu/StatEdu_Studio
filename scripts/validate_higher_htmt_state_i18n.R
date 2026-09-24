Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
lines <- readLines('R/setup_custom_model_canvas_structural_higher_htmt.R',encoding='UTF-8')
reason_lines <- lines[seq.int(grep('    reasons <- c',lines,fixed=TRUE),grep('    reason <- higher$reason',lines,fixed=TRUE)-1L)]
reasons <- sub('^ *"([^"]+)"=.*','\\1',grep('^      "[^"]+"=',reason_lines,value=TRUE));stopifnot(length(reasons)==10)
reasons <- c(reasons,'Unknown: Review 사용자 <&> %s')
out <- 'tmp/higher-htmt-state-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
entries <- list();unavailable_html <- character();english <- list()
for(i in seq_along(reasons))for(raw in c(FALSE,TRUE))for(language in c('en','ko','ja','zh','es','fr','de','vi')) {
 env <- new.env(parent=globalenv());env$structural_canvas_higher_htmt_result <- function(bundle)list(available=FALSE,reason=reasons[[i]])
 render <- structural_canvas_higher_htmt_html;environment(render) <- env
 stopifnot(is.null(render(list(),FALSE,language,raw)))
 html <- as.character(render(list(),TRUE,language,raw));text <- xml2::xml_text(xml2::read_html(html,encoding='UTF-8'))
 if(language=='en')reference <- text else {stopifnot(text!=reference);if(i<=10)stopifnot(!grepl(reasons[[i]],text,fixed=TRUE))}
 if(i==11)stopifnot(grepl(reasons[[i]],text,fixed=TRUE))
 if(language=='ja')unavailable_html <- c(unavailable_html,html)
}
pair <- data.frame(Factor1='Review',Factor2='Normality',HTMT=.6,Criterion='Below reference',Reason='')
result <- list(pairs=pair,matrix=matrix(c(1,.6,.6,1),2,dimnames=list(c('Review','Normality'),c('Review','Normality'))))
higher <- list(available=TRUE,result=result,raw_result=result,n=100,excluded=0,mapping=data.frame(Construct='Review',Indicator='Primary',Scoring='Unit-weighted item mean',Items='x1, x2'))
states <- list(unrequested=list(htmt_bootstrap=0L),pending=list(htmt_bootstrap=100L,cfa_bootstrap_pending=TRUE),canceled=list(htmt_bootstrap=100L,cfa_bootstrap_canceled=TRUE),failed=list(htmt_bootstrap=100L))
for(state in names(states))for(raw in c(FALSE,TRUE))for(language in c('en','ko','ja','zh','es','fr','de','vi')) {
 env <- new.env(parent=globalenv());env$structural_canvas_higher_htmt_result <- function(bundle)higher
 render <- structural_canvas_higher_htmt_html;environment(render) <- env
 html <- as.character(render(states[[state]],TRUE,language,raw));doc <- xml2::read_html(html,encoding='UTF-8')
 note <- tail(xml2::xml_text(xml2::xml_find_all(doc,'//p')),1)
 if(language=='en')reference <- note else stopifnot(note!=reference)
 main <- as.character(render(states[[state]],FALSE,language,raw));if(language=='en')main_en <- main else stopifnot(identical(main,main_en))
 if(language=='ja'&&!raw)entries[[state]] <- list(id=state,title=state,html=html)
 cat('PASS:',state,raw,language,'status note and unchanged English main\n')
}
entries$unavailable <- list(id='unavailable',title='Unavailable reasons',html=paste(c(as.character(structural_canvas_basic_html_table(data.frame(Factor='Review',HTMT='—'),role='main')),unavailable_html),collapse='\n'))
saveRDS(unname(entries),file.path(out,'entries.rds'))
cat('PASS: ten unavailable reasons and unknown literal detail, both representations, eight languages\n')
