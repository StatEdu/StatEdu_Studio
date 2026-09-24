Sys.setlocale('LC_CTYPE','English_United States.utf8')
source('scripts/validate_mi_skipped_i18n.R',encoding='UTF-8')
out <- 'tmp/mi-notes-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
entries <- list();english_notes <- list()
for(mode in c('theory','free'))for(language in c('en','ko','ja','zh','es','fr','de','vi')) {
 bundle$mi_mode <- mode
 table <- structural_canvas_mi_result_table(bundle,list(),NULL,FALSE,as.character,identity,identity)
 # A deliberately colliding display value checks that cells bypass dictionary lookup.
 table$Covariance <- 'Review'
 app_language_fn <- function()language
 html <- as.character(eval(render_expression,new.env(parent=globalenv())))
 doc <- xml2::read_html(html,encoding='UTF-8')
 notes <- xml2::xml_text(xml2::xml_find_all(doc,"//p[contains(@class,'structural-result-note')]"))
 stopifnot(length(notes)==if(mode=='theory')6L else 4L)
 if(language=='en')english_notes[[mode]] <- notes else stopifnot(all(notes!=english_notes[[mode]]))
 stopifnot(any(trimws(xml2::xml_text(xml2::xml_find_all(doc,'//td')))=='Review'))
 buttons <- xml2::xml_find_all(doc,"//button[contains(@class,'structural-mi-select-button')]")
 stopifnot(length(buttons)==if(mode=='theory')1L else 0L)
 if(mode=='theory') {
  stopifnot(xml2::xml_attr(buttons,'id')=='test_mi_select_1')
  if(language!='en')stopifnot(trimws(xml2::xml_text(buttons))!='Select')
 }
 if(language=='ja')entries[[mode]] <- list(id=mode,title=mode,html=html)
 writeLines(html,file.path(out,paste0(language,'-',mode,'.html')),useBytes=TRUE)
 cat('PASS:',language,mode,'all notes, select action and literal cells\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
