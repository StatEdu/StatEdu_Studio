Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out <- 'tmp/mi-skipped-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
path <- 'Review [nonconvergence] | 사용자 %s'
reason_names <- 'Normality, Review <&> %s'
reasons <- c('nonconvergence','lavaan post.check failure','invalid degrees of freedom',
 paste0('negative residual variance: ',reason_names),paste0('negative latent variance: ',reason_names),
 paste0('non-positive-definite or boundary residual covariance matrix: ',reason_names),
 paste0('non-positive-definite or boundary latent covariance matrix: ',reason_names),
 'non-positive-definite or unexplained boundary parameter covariance matrix (boundary dimensions = 12; explicit equality constraints = 3)',
 'absolute latent correlation at least 1','inadmissible trial fit')
records <- lapply(reasons,function(reason)list(path=path,error='',reasons=reason))
records[[length(records)+1L]] <- list(path=path,error='unknown lavaan <&> %s',reasons=character())
raw <- paste(vapply(records,function(r)paste0(r$path,' [',if(nzchar(r$error))paste0('fit error: ',r$error)else paste(r$reasons,collapse='; '),']'),character(1)),collapse=' | ')
mi <- data.frame(lhs='Review',rhs='Normality',op='~~',mi=12,epc=.12,sepc.all=.13,step=1,
 skipped_inadmissible=11,skipped_details=raw,cfi_after=.95,tli_after=.94,rmsea_after=.04,srmr_after=.03,
 'MI p'=.001,'BH-adjusted p'=.002,'Multiplicity family size'=12,check.names=FALSE)
mi$skipped_records <- I(list(records))
stopifnot(identical(structural_canvas_mi_skipped_text(mi,'en'),raw))
legacy <- mi;legacy$skipped_records <- NULL
bundle <- list(mi=mi,mi_mode='theory')
table <- structural_canvas_mi_result_table(bundle,list(),NULL,FALSE,as.character,identity,identity)
result_table <- function(type)table;fit_result <- function()bundle;prefix <- 'test'
render_expression <- body(structural_canvas_register_mi_render_outputs)[[2]][[3]][[2]]
entries <- list()
for(language in c('en','ko','ja','zh','es','fr','de','vi')) {
 app_language_fn <- function()language
 text <- structural_canvas_mi_skipped_text(mi,language)
 stopifnot(structural_canvas_mi_skipped_text(legacy,language)==raw,
   grepl(path,text,fixed=TRUE),grepl(reason_names,text,fixed=TRUE),grepl('unknown lavaan <&> %s',text,fixed=TRUE),
   grepl('12',text,fixed=TRUE),grepl('3',text,fixed=TRUE))
 for(reason in reasons) {
  single <- mi;single$skipped_records <- I(list(list(list(path='X',error='',reasons=reason))))
  if(language!='en')stopifnot(!grepl(reason,structural_canvas_mi_skipped_text(single,language),fixed=TRUE))
 }
 html <- as.character(eval(render_expression,new.env(parent=globalenv())));doc <- xml2::read_html(html,encoding='UTF-8')
 cells <- xml2::xml_text(xml2::xml_find_all(doc,"//table[contains(@class,'structural-mi-skipped-details-table')]//td"))
 stopifnot(any(trimws(cells)==text))
 writeLines(html,file.path(out,paste0(language,'.html')),useBytes=TRUE)
 if(language=='ja')entries[[1]] <- list(id='mi',title='MI',html=html)
 cat('PASS:',language,'10 reasons, engine error, literal names, legacy preservation and actual result UI\n')
}
saveRDS(entries,file.path(out,'entries.rds'))
