Sys.setlocale('LC_CTYPE','English_United States.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false',STATEDU_NO_PACKAGE_INSTALL='true')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
lines<-readLines('R/setup_custom_model_canvas_ui.R',encoding='UTF-8')
begin<-grep('statedu_bootstrap_status_ui(',lines,fixed=TRUE)[2]
end<-which(seq_along(lines)>begin & grepl('^\\s*\\),\\s*$',lines))[1]
card_expression<-parse(text=sub(',\\s*$','',paste(lines[begin:end],collapse='\n')))
ui<-fluidPage(selectInput('language','Language',c('ko','en','ja','zh','es','fr','de','vi')),
 selectInput('phase','Phase',c('starting','preparing','resampling','finalizing','serializing','complete')),
 uiOutput('card'),textOutput('stop_count'))
server<-function(input,output,session){
 path<-tempfile('mediation-progress-',fileext='.rds');session$onSessionEnded(function(){if(file.exists(path))unlink(path)})
 stopped<-reactiveVal(0L)
 observeEvent(input$custom_model_canvas_bootstrap_stop,stopped(stopped()+1L),ignoreInit=TRUE)
 output$stop_count<-renderText(stopped())
 output$card<-renderUI({
  language<-input$language;phase<-input$phase;now<-Sys.time()
  state<-new.env(parent=emptyenv());state$rate_samples<-c(9,10,11)
  job<-list(progress_file=path,progress_state=state,requested_total=1000L,boot_r=500L,started_at=now-40)
  saveRDS(list(phase=phase,done=if(phase=='complete')1000L else 500L,total=1000L,boot_r=500L,focal='Review 사용자 <&>',updated_at=now),path)
  progress<-mediation_moderation_bootstrap_job_progress(job,language)
  div(id='progress-fixture',`data-language`=language,`data-phase`=phase,eval(card_expression))
 })
}
shiny::runApp(shinyApp(ui,server),host='127.0.0.1',port=43875,launch.browser=FALSE)
