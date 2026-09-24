Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
expr <- body(structural_canvas_register_validity_outputs)[[2]][[3]][[4]][[3]][[2]]
main_expr <- body(structural_canvas_register_validity_outputs)[[2]][[3]][[2]][[3]][[2]]
out <- 'tmp/pls-validity-guide-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
entries <- list()
roles <- c('Weights, collinearity, content coverage, and redundancy; internal consistency/AVE/HTMT not applicable',
 'PLSc common-factor diagnostics; interpretation depends on consistency-correction assumptions',
 'Mode A score-proxy diagnostics; not covariance-based factor-model evidence',
 'Reflective-composite diagnostics; do not infer a latent common cause or explicit measurement-error separation')
for (state in c('mixed','shared','empty-columns','empty')) for (language in c('en','ko','ja','zh','es','fr','de','vi')) {
 table <- data.frame(Construct=c('Review','Normality','Primary','사용자 <&> %s'),
  'Construct type'=c('Composite','Common factor','Unspecified','Composite'),
  Mode=c('Formative','Reflective','Reflective','Reflective'),'Evidence role'=roles,
  'Max HTMT CI lower'=c('N/A','.510','.620','.730'),
  'Max HTMT CI upper'=c('N/A','.810','.920','.930'),
  'Max HTMT p'=c('N/A','.001','.002','.003'),
  'Fornell-Larcker'=c('N/A - formative','Below reference','Review needed','Below reference'),check.names=FALSE)
 if(state=='shared')for(j in 2:ncol(table))table[[j]] <- rep(table[[j]][[2]],4)
 if(state=='empty-columns')table[,5:7] <- ''
 if(state=='empty')table <- table[FALSE,]
 table <- structural_canvas_pls_validity_guide_table(table)
 env <- new.env(parent=globalenv());env$appendix_result_table <- function(kind)table;env$app_language_fn <- function()language
 render <- function()NULL;body(render) <- expr;environment(render) <- env
 html <- as.character(render())
 if(state=='empty'){stopifnot(length(html)==0);next}
 doc <- xml2::read_html(html,encoding='UTF-8');cells <- xml2::xml_text(xml2::xml_find_all(doc,'//td'))
 stopifnot(all(table$Construct %in% cells))
 text <- xml2::xml_text(doc)
 notes <- xml2::xml_text(xml2::xml_find_all(doc,'//p'))
 stopifnot(length(notes)==if(state=='shared')2L else 1L)
 if(language=='en'){english_notes <- notes;english_title <- xml2::xml_text(xml2::xml_find_first(doc,'//h5'))} else {
  stopifnot(all(notes!=english_notes),xml2::xml_text(xml2::xml_find_first(doc,'//h5'))!=english_title)
  for(value in unique(c(table[['Evidence role']],table[['Fornell-Larcker']]))){
   stopifnot(!grepl(value,text,fixed=TRUE))
  }
  headers <- xml2::xml_text(xml2::xml_find_all(doc,'//th'))
  stopifnot(!any(c('Construct','Construct type','Evidence role','Max HTMT CI lower','Max HTMT CI upper','Max HTMT p') %in% headers))
 }
 for(value in unique(unlist(table[,5:7])))if(nzchar(value))stopifnot(grepl(value,text,fixed=TRUE))
 env$result_table <- function(kind)data.frame(Construct=c('Review','Normality'),Review=c('1.000','.650'),Normality=c('.650','1.000'),check.names=FALSE)
 body(render) <- main_expr;main <- as.character(render())
 if(language=='en')english_main <- main else stopifnot(identical(main,english_main))
 if(language=='ja')entries[[state]] <- list(id=state,title=state,html=paste(main,html))
 cat('PASS:',state,language,'guide, shared metadata, user names, precision and English main table\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
