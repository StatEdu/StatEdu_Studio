Sys.setlocale('LC_CTYPE', 'English_United States.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R', encoding='UTF-8')
load_app_packages(check=FALSE); source_app_modules()
capture_error <- function(f) {
  result <- tryCatch(f(), error=identity)
  stopifnot(inherits(result, 'statedu_merge_error'))
  result
}
files <- list(data.frame(id=1:2, Review=c(10,20)), data.frame(id=2:3, Normality=c(30,40)))
user_key <- "사용자 '<&> %s"
errors <- list(
 files=capture_error(function()merge_uploaded_files(NULL,list(),2L)),
 variable_files=capture_error(function()merge_add_variables(files[1],'id')),
 enter_id=capture_error(function()merge_add_variables(files,'')),
 missing_id=capture_error(function()merge_add_variables(files,user_key)),
 duplicate_id=capture_error(function()merge_add_variables(list(data.frame(id=c(1,1)),files[[2]]),'id')),
 case_files=capture_error(function()merge_add_cases(files[1],NULL)),
 no_common=capture_error(function()merge_add_cases(list(data.frame(a=1),data.frame(b=2)),NULL))
)
english <- c('Select at least 2 file(s).','Variable merge requires at least two files.',
 'Enter the ID variable used to match rows.',sprintf("File 1 does not contain ID variable '%s'.",user_key),
 'File 1 has duplicated ID values. Variable merge expects one row per ID in each file.',
 'Case merge requires at least two files.','The selected files do not share any common variable names.')
stopifnot(identical(unname(vapply(errors,conditionMessage,character(1))),english))
langs <- c('en','ko','ja','zh','es','fr','de','vi')
for(language in langs) {
 for(key in names(errors)) {
  e <- errors[[key]]
  actual <- merge_error_text(e,language)
  expected <- do.call(sprintf,c(list(statedu_t(paste0('merge.error.',key),language)),e$values))
  stopifnot(identical(actual,expected),!startsWith(actual,'merge.error.'))
  if(key=='missing_id') stopifnot(grepl(user_key,actual,fixed=TRUE))
  if(language!='en') stopifnot(!identical(actual,conditionMessage(e)))
 }
 raw <- simpleError('External reader: 사용자 <&> %s')
 stopifnot(identical(merge_error_text(raw,language),conditionMessage(raw)))
 cat('PASS:',language,'7 actual error branches; user names and external details preserved\n')
}
# Successful joins retain their previous data behavior.
for(mode in c('left','inner','full')) {
 actual <- merge_add_variables(files,'id',mode)
 expected <- merge(files[[1]],files[[2]],by='id',all.x=mode %in% c('left','full'),all.y=mode=='full',sort=FALSE)
 stopifnot(identical(actual,expected))
}
cases <- merge_add_cases(files,'id','wave','사용자,Review')
stopifnot(identical(cases$id,c(1L,2L,2L,3L)),identical(cases$wave,c('사용자','사용자','Review','Review')))

paths <- vapply(seq_along(files),function(i)tempfile(fileext='.csv'),character(1))
for(i in seq_along(files))write.csv(files[[i]],paths[[i]],row.names=FALSE)
uploaded <- data.frame(name=c('first.csv','second.csv'),datapath=paths)
notices <- new.env();notices$values <- character()
showNotification <- function(ui,...) {notices$values <- c(notices$values,as.character(ui));invisible('test')}
server <- function(input,output,session) {
 lang <- reactiveVal('en')
 register_merge_handlers(input,output,session,function(...)stop('Invalid merge must not replace data'),function()NULL,lang)
}
shiny::testServer(server, {
 session$setInputs(preview_merge_data=0,run_merge_data=0,merge_files=uploaded,merge_csv_header=TRUE,merge_id_variable=user_key)
 session$flushReact()
 for(i in seq_along(langs)) {
  lang(langs[[i]]);session$flushReact();notices$values <- character()
  session$setInputs(preview_merge_data=i);session$setInputs(run_merge_data=i)
  stopifnot(identical(notices$values,rep(merge_error_text(errors$missing_id,langs[[i]]),2)))
  cat('PASS:',langs[[i]],'preview and run notifications use current language\n')
 }
})
unavailable_server <- function(input,output,session) {
 lang <- reactiveVal('en')
 register_merge_handlers(input,output,session,NULL,function()NULL,lang)
}
shiny::testServer(unavailable_server, {
 session$setInputs(run_merge_data=0);session$flushReact()
 for(i in seq_along(langs)) {
  lang(langs[[i]]);session$flushReact();notices$values <- character()
  session$setInputs(run_merge_data=i)
  stopifnot(identical(notices$values,statedu_t('merge.error.replacement',langs[[i]])))
 }
})
unlink(paths)
cat('PASS: unavailable replacement in 8 languages; successful left/inner/full and case merges unchanged\n')
