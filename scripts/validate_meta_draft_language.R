Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
server<-function(input,output,session){
 lang<-reactiveVal('ko')
 handles<-register_meta_server(input,output,session,app_language_fn=function()lang())
}
shiny::testServer(server,{
 session$setInputs(meta_target_family='g',meta_input_type='g_se',meta_add_effect=0)
 read<-function(){xml2::read_html(output$meta_effect_fields$html,encoding='UTF-8')}
 value<-function(id)xml2::xml_attr(xml2::xml_find_first(read(),paste0('//*[@id="meta_field_',id,'"]')),'value')
 for(family in c('g','r','or'))for(type in names(meta_input_types(family))) {
  do.call(session$setInputs,list(meta_target_family=family,meta_input_type=type))
  read()
  fields<-meta_input_field_map(family)[[type]]
  draft<-setNames(as.list(seq_along(fields)+.125),paste0('meta_field_',fields))
  do.call(session$setInputs,draft)
  for(language in c('en','ko','ja','zh','es','fr','de','vi','ko')) {
   lang(language);session$flushReact()
   stopifnot(all(vapply(seq_along(fields),function(i)as.numeric(value(fields[i]))==i+.125,logical(1))))
  }
  # Empty numeric input is NA in Shiny; it must not resurrect the saved value.
  do.call(session$setInputs,setNames(list(NA_real_),paste0('meta_field_',fields[1])))
  lang('ja');session$flushReact();stopifnot(is.na(value(fields[1]))||value(fields[1]) %in% c('', 'NA'))
  stopifnot(nrow(handles$effects())==0)
  cat('PASS draft language:',family,type,'all languages and cleared field\n')
 }
 session$setInputs(meta_target_family='g',meta_input_type='g_se',meta_field_g=99,meta_field_se=.7)
 read()
 session$setInputs(meta_add_effect=1)
 stopifnot(is.na(value('g'))||value('g') %in% c('', 'NA'))
 row<-meta_normalize_effect(list(study_id='Saved',family='g',input_type='g_se',g=.25,se=.1),row_id=1)
 handles$effects(row);session$setInputs(meta_effects_table_rows_selected=1,meta_edit_effect=0)
 session$setInputs(meta_edit_effect=1)
 stopifnot(as.numeric(value('g'))==.25)
 for(language in c('en','ko','ja','zh','es','fr','de','vi','ko')) {
  lang(language);session$flushReact()
  stopifnot(output$meta_effect_modal_title==meta_ui_text('edit_title',language),
    output$meta_effect_modal_help==meta_ui_text('moderator_help',language),
    output$meta_effect_modal_cancel==meta_ui_text('cancel',language))
 }
 session$setInputs(meta_field_g=.9,meta_field_se=.2)
 lang('fr');session$flushReact();stopifnot(as.numeric(value('g'))==.9,identical(handles$effects(),row))
 session$setInputs(meta_edit_effect=2)
 stopifnot(as.numeric(value('g'))==.25)
 cat('PASS reopen: new blank, edit saved value, unsaved draft isolated\n')
})

