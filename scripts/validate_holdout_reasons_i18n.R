Sys.setlocale('LC_CTYPE','English_United States.utf8')
source('scripts/validate_mi_history_holdout_i18n.R',encoding='UTF-8')
actual_bad <- suppressWarnings(structural_canvas_holdout_model_comparison(
 'eta1 =~ x1 + x2 + x3 + x4', 'eta1 =~ x1 + x2 + x3 + x4\nx1 ~~ -0.01*x1',
 holdout_split$validation,estimator='ML',missing='listwise'))
stopifnot(!actual_bad$table$Admissible[2],length(actual_bad$admissibility_reasons[[2]])>0L,
 all(is.na(actual_bad$changes$DeltaChisq)))
literal <- 'Review; nonconvergence | 사용자 [x] %s'
reasons <- c('nonconvergence','lavaan post.check failure','invalid degrees of freedom',
 paste0('negative residual variance: ',literal),paste0('negative latent variance: ',literal),
 paste0('non-positive-definite or boundary residual covariance matrix: ',literal),
 paste0('non-positive-definite or boundary latent covariance matrix: ',literal),
 'non-positive-definite or unexplained boundary parameter covariance matrix (boundary dimensions = 12; explicit equality constraints = 3)',
 'absolute latent correlation at least 1','inadmissible trial fit')
synthetic <- actual_bad
synthetic$admissibility_reasons[[2]] <- c(reasons,'Unknown engine diagnostic <&> %s')
synthetic$table[['Admissibility reasons']][2] <- paste(synthetic$admissibility_reasons[[2]],collapse='; ')
legacy <- synthetic;legacy$admissibility_reasons <- NULL
out <- 'tmp/holdout-reasons-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries <- list()
for(language in c('en','ko','ja','zh','es','fr','de','vi')) {
 for(reason in reasons)if(language!='en')stopifnot(structural_canvas_admissibility_reason_text(reason,language)!=reason)
 for(state in c('actual','synthetic','legacy')) {
  comparison <- switch(state,actual=actual_bad,synthetic=synthetic,legacy=legacy)
  before <- comparison
  rendered_reasons <- structural_canvas_holdout_reason_text(comparison,language)
  if(language=='en'||state=='legacy')stopifnot(identical(rendered_reasons,as.character(comparison$table[['Admissibility reasons']])))
  if(state=='synthetic')stopifnot(grepl(literal,rendered_reasons[2],fixed=TRUE),
    grepl('Unknown engine diagnostic <&> %s',rendered_reasons[2],fixed=TRUE))
  bundle <- base;bundle$holdout_comparison <- comparison
  html <- as.character(render_holdout());doc <- xml2::read_html(html,encoding='UTF-8')
  cells <- trimws(xml2::xml_text(xml2::xml_find_all(doc,'//td')))
  stopifnot(all(rendered_reasons %in% cells),identical(before,comparison))
  if(language=='ja')entries[[state]] <- list(id=state,title=state,html=html)
 }
 cat('PASS:',language,'actual inadmissible fit, ten reasons, raw engine text, legacy data, literal names and rendered cells\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
