Sys.setlocale('LC_CTYPE','English_United States.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
root<-tempfile('figure-errors-');dir.create(root)
catch<-function(expr)tryCatch(force(expr),error=identity)
item<-function(data,name='Model_1.png')list(list(name=name,data=data))
errors<-list(
 prefix=catch(save_canvas_figure_snapshots(list(),root,'bad/prefix')),
 empty=catch(save_canvas_figure_snapshots(list(),root)),
 filename=catch(save_canvas_figure_snapshots(item('', '../bad.png'),root)),
 snapshot=catch(save_canvas_figure_snapshots(item('not-png'),root)),
 image=catch(save_canvas_figure_snapshots(item('data:image/png;base64,YQ=='),root)),
 named=catch(save_canvas_figure_snapshots(item('data:image/png;base64,%00'),root)),
 length=catch(save_canvas_figure_snapshots(item('data:image/png;base64,A'),root)),
 decode=simpleError('Could not decode PNG snapshot: Model_1.png'),
 folder=simpleError('Could not create the figure folder.'),
 no_figures=simpleError('저장할 그림이 없습니다. / No figures to save.'),
 format=simpleError('Unsupported format'))
notices<-new.env();notices$values<-character()
showNotification<-function(ui,...) {notices$values<-c(notices$values,as.character(ui));invisible('test')}
original_save<-save_canvas_figure_snapshots
injected<-errors[[1]]
save_canvas_figure_snapshots<-function(...)stop(injected)
choose_figure_save_dir<-function()root
shiny::testServer(function(input,output,session) {
 language<-reactiveVal('en')
 register_canvas_report_exports(input,session,'save_html','save_pdf','model_results',NULL,function()'사용자 결과',function()list(),language)
}, {
 session$flushReact();nonce<-0L
 for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
  language(lang);session$flushReact()
  for(key in names(errors)) {
   injected<<-errors[[key]];stopifnot(inherits(injected,'error'))
   expected<-statedu_t(paste0('result.figure_error.',key),lang)
   if(key %in% c('named','decode'))expected<-sprintf(expected,'Model_1.png')
   if(key=='length')expected<-sprintf(expected,'Model_1.png','1')
   stopifnot(result_export_error_text(injected,lang)==expected)
   notices$values<-character();nonce<-nonce+1L
   session$setInputs(model_figures_snapshot=list(files=list(list(name='Model_1.png',data='stub')),nonce=nonce))
   stopifnot(expected %in% notices$values)
  }
  external<-simpleError('Invalid PNG snapshot: D:/사용자 %s.png')
  stopifnot(identical(result_export_error_text(external,lang),conditionMessage(external)))
  cat('PASS:',lang,'11 figure/split errors; filename/length preserved; shared figure error notifications\n')
 }
})
