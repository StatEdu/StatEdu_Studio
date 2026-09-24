Sys.setlocale('LC_CTYPE','English_United States.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false',STATEDU_RESULT_STORE=tempfile(fileext='.json'))
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
entries <- lapply(1:3,function(i)list(id=paste0('item',i),title=paste0('사용자 <&> %s 結果 ',i),saved_at='2026-09-17',html=paste0('<p>Estimate ',i,'.230 사용자 변수</p>')))
langs <- c('en','ko','ja','zh','es','fr','de','vi','ko')
check_ui <- function(html,language,expected,undo=FALSE) {
 doc <- xml2::read_html(html)
 nodes <- xml2::xml_find_all(doc,"//div[contains(concat(' ',normalize-space(@class),' '),' saved-result-entry ')]")
 stopifnot(length(nodes)==length(expected))
 for(i in seq_along(expected)) {
  entry <- expected[[i]];node<-nodes[[i]]
  stopifnot(xml2::xml_attr(node,'data-result-entry-id')==entry$id,
   xml2::xml_attr(xml2::xml_find_first(node,'.//iframe'),'srcdoc')==entry$html,
   xml2::xml_attr(xml2::xml_find_first(node,'.//iframe'),'title')==entry$title)
  for(action in c('up','down','delete')) {
   button <- xml2::xml_find_first(node,paste0('.//button[contains(@class,"saved-result-entry-',action,'")]'))
   key <- switch(action,up='move_up',down='move_down',delete='delete_entry')
   label <- statedu_t(paste0('result.management.',key),language)
   stopifnot(xml2::xml_text(button)==label,xml2::xml_attr(button,'title')==label,
    xml2::xml_attr(button,'aria-label')==paste(entry$title,label))
   disabled <- (action=='up' && i==1L)||(action=='down' && i==length(expected))
   stopifnot((!is.na(xml2::xml_attr(button,'disabled')))==disabled)
  }
 }
 if(undo) stopifnot(xml2::xml_text(xml2::xml_find_first(doc,'//*[@id="undo_saved_result_edit"]'))==statedu_t('result.management.undo_edit',language))
}
shiny::testServer(function(input,output,session) {
 language<-reactiveVal('en');store<-result_accumulator_store(session);store(entries)
 register_result_accumulator_outputs(input,output,session,language)
}, {
 session$flushReact()
 session$setInputs(saved_result_entry_action=list(id='item2',action='up'))
 moved <- entries[c(2,1,3)]
 for(lang in langs) {
  language(lang);session$flushReact()
  check_ui(output$saved_results_list$html,lang,moved,TRUE)
  stopifnot(identical(store(),moved),identical(read_result_snapshot_store(),moved))
 }
 session$setInputs(undo_saved_result_edit=1)
 stopifnot(identical(store(),entries))
 for(i in seq_along(langs)) {
  language(langs[[i]]);session$flushReact()
  session$setInputs(saved_result_entry_action=list(id='item2',action='delete',nonce=i))
  check_ui(output$saved_results_list$html,langs[[i]],entries[c(1,3)],TRUE)
  stopifnot(identical(read_result_snapshot_store(),entries[c(1,3)]))
  session$setInputs(undo_saved_result_edit=i+1)
  stopifnot(identical(store(),entries),identical(read_result_snapshot_store(),entries))
  check_ui(output$saved_results_list$html,langs[[i]],entries)
  cat('PASS:',langs[[i]],'button text/title/accessibility; boundaries; language switching; persistent move/delete/undo; snapshot preservation\n')
 }
})
