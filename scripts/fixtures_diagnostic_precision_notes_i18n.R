source('scripts/fixtures_clinical_planning_i18n.R',encoding='UTF-8')
out <- 'tmp/diagnostic-precision-notes-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
results <- results[c(6,7,14,15)];designs <- c('sensitivity-n','specificity-n','sensitivity-precision','specificity-precision')
formula_keys <- rep('sample_size.result.planning_diagnostic_precision',4)
raw <- serialize(results,NULL)
clean <- function(x)gsub('[[:space:]\u00a0]+','',x,perl=TRUE)
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 for(i in 1:4) {
  key <- paste0('sample_size.result.note_buderer_',if(i%%2==1)'sensitivity' else 'specificity')
  expected <- statedu_t(key,lang)
  if(i>2) {
   p <- if(i==3).85 else .9;fraction<-if(i==3).2 else .8
   number <- sprintf('%.3f',qnorm(.975)*sqrt(p*(1-p)/(100*fraction)))
   expected <- sprintf(statedu_t(paste0(key,'_precision'),lang),number)
   stopifnot(is.na(results[[i]]$power))
  }
  stopifnot(identical(sample_size_result_text(results[[i]]$method_note,lang),expected))
  rendered <- xml2::xml_text(xml2::read_html(as.character(sample_size_results_ui(results[[i]],lang))))
  parts <- trimws(strsplit(result_sci_note_text(estimation=expected),';',fixed=TRUE)[[1]])
  for(part in parts)stopifnot(grepl(clean(sub('[.。]$','',part)),clean(rendered),fixed=TRUE))
 }
 for(design in c('sensitivity','specificity'))for(number in c('0.000','0.001','12.345','000.120')) {
  key<-paste0('sample_size.result.note_buderer_',design,'_precision');en<-sprintf(statedu_t(key,'en'),number)
  stopifnot(identical(sample_size_result_text(en,lang),sprintf(statedu_t(key,lang),number)))
  for(unknown in c(paste0(en,' user text'),sub(number,'0.12',en,fixed=TRUE),sub(design,'custom',en,fixed=TRUE)))stopifnot(identical(sample_size_result_text(unknown,lang),unknown))
 }
}
stopifnot(identical(raw,serialize(results,NULL)))
cat('PASS four Buderer n/precision cases, preserved NA power, exact 3-decimal strings and strict matching across eight languages\n')
