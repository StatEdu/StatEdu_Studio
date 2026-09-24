Sys.setlocale('LC_CTYPE','English_United States.utf8')
source('scripts/validate_higher_htmt_state_i18n.R',encoding='UTF-8')
higher$mapping <- data.frame(Construct=c('Review','Normality'),Indicator=c('Primary','Review'),Scoring=c('Unit-weighted item mean','Observed indicator (unchanged)'),Items=c('Scoring, 사용자 <&> %s','Items, Review'))
ci <- data.frame(`Factor 1`='Review',`Factor 2`='Normality',Lower=.4,Upper=.8,`One-sided upper`=.75,`Valid %`=70,`Valid replicates`=70,`Requested replicates`=100,Status='Caution',`CI method`='BCa unavailable',`Upper < threshold`='Yes',`Upper < 1`='No',check.names=FALSE)
bootstrap <- ci;attr(bootstrap,'higher_order') <- ci;attr(bootstrap,'higher_order_raw') <- ci
bundle <- list(htmt_bootstrap=100L,htmt_bootstrap_result=bootstrap)
reasons <- c('','Overlapping source items prevent standard HTMT calculation','Cross-loaded indicators prevent standard HTMT calculation','Constant or unavailable indicator correlations','At least two indicators per factor are required','Within-factor correlations are insufficient')
out <- 'tmp/higher-htmt-details-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
entries <- list()
for(i in seq_along(reasons))for(raw in c(FALSE,TRUE))for(language in c('en','ko','ja','zh','es','fr','de','vi')) {
 fixture <- higher
 fixture$result$pairs$Reason <- fixture$raw_result$pairs$Reason <- reasons[[i]]
 fixture$result$pairs$Criterion <- fixture$raw_result$pairs$Criterion <- if(i==1)'Below reference' else 'Not assessed'
 env <- new.env(parent=globalenv());env$structural_canvas_higher_htmt_result <- function(bundle)fixture
 render <- structural_canvas_higher_htmt_html;environment(render) <- env
 html <- as.character(render(bundle,TRUE,language,raw));doc <- xml2::read_html(html,encoding='UTF-8')
 cells <- xml2::xml_text(xml2::xml_find_all(doc,'//td'));notes <- xml2::xml_text(xml2::xml_find_all(doc,'//h4|//h5|//p'))
 stopifnot(all(c('Review','Normality') %in% cells))
 if(!raw)stopifnot(all(c('Primary','Scoring, 사용자 <&> %s','Items, Review') %in% cells))
 numeric <- cells[grepl('^[.0-9]',cells)]
 if(language=='en'){english <- notes;numbers <- numeric} else {
  stopifnot(all(notes!=english),identical(numeric,numbers))
  stopifnot(!any(cells %in% c('Unit-weighted item mean','Observed indicator (unchanged)','Below reference','Not assessed','Caution','BCa unavailable')))
  if(i>1)stopifnot(!reasons[[i]] %in% cells)
 }
 main <- as.character(render(bundle,FALSE,language,raw));if(language=='en')main_en <- main else stopifnot(identical(main,main_en))
 if(language=='ja'&&(i<=2||!raw))entries[[paste(i,raw)]] <- list(id=paste0('case',i,raw),title=paste('Case',i,raw),html=html)
 cat('PASS:',i,raw,language,'titles, note, protected identifiers, program cells, numbers and English main\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
