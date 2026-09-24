Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
dialog<-NULL
walk<-function(node){
 if(missing(node))return()
 if(is.call(node)&&identical(node[[1]],as.name('observeEvent'))&&identical(node[[2]],quote(input$meta_reset_effects)))dialog<<-node[[3]]
 if(is.call(node)||is.expression(node))for(child in as.list(node))walk(child)
}
walk(parse('R/server_meta.R',encoding='UTF-8'));stopifnot(!is.null(dialog))
showModal<-function(ui,...)html<<-as.character(ui)
effects<-function(...)stop('Opening the dialog must not change entered effects')
for(lang in c('en','ko','ja','zh','es','fr','de','vi','ko')){
 language<-function()lang
 eval(dialog)
 doc<-xml2::read_html(html,encoding='UTF-8')
 stopifnot(xml2::xml_text(xml2::xml_find_first(doc,'//*[@id="meta_reset_title"]'))==statedu_t('meta.reset.title',lang),
  grepl(statedu_t('meta.reset.confirm',lang),xml2::xml_text(doc),fixed=TRUE),
  xml2::xml_text(xml2::xml_find_first(doc,'//*[@id="meta_confirm_reset"]'))==meta_ui_text('reset',lang),
  grepl(meta_ui_text('cancel',lang),xml2::xml_text(doc),fixed=TRUE))
 cat('PASS:',lang,'actual reset dialog; title/body/buttons; opening preserves effects\n')
}
