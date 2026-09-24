Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
langs <- c('en','ko','ja','zh','es','fr','de','vi')
files <- list('사용자 <&> %s.csv'=data.frame(id=1:2,Review=c(10,20)))
for(language in langs) {
 summary <- merge_summary_table(files,language)
 stopifnot(identical(names(summary),unname(vapply(c('file','rows_header','columns'),function(k)statedu_t(paste0('merge.ui.',k),language),character(1)))),
           identical(summary[[1]],names(files)),identical(summary[[2]],2L),identical(summary[[3]],2L))
 empty <- merge_summary_table(list(),language)
 stopifnot(identical(names(empty),statedu_t('merge.ui.message',language)),identical(empty[[1]],statedu_t('merge.ui.no_files',language)))
 html <- as.character(data_editor_merge_panel(language))
 doc <- xml2::read_html(html)
 for(key in c('whitespace','tab','comma')) {
  value <- xml2::xml_text(xml2::xml_find_first(doc,sprintf('//select[@id="merge_dat_delimiter"]/option[@value="%s"]',key)))
  stopifnot(identical(value,statedu_t(paste0('merge.ui.',key),language)))
 }
 for(key in c('subtitle','heading','files','csv_header','dat_header','delimiter','variables','id','rows','left','inner','full','cases','indicator_name','indicator_values')) {
  text <- xml2::xml_text(doc)
  # Korean labels retain their established wording through merge_text.
  if(language!='ko')stopifnot(grepl(statedu_t(paste0('merge.ui.',key),language),text,fixed=TRUE))
 }
 cat('PASS:',language,'setup translations, delimiter codes, summary labels and user filenames\n')
}
paths <- c(tempfile(fileext='.csv'),tempfile(fileext='.csv'))
write.csv(data.frame(id=1:2,Review=c(10,20)),paths[1],row.names=FALSE)
write.csv(data.frame(id=2:3,Review=c(30,40)),paths[2],row.names=FALSE)
uploaded <- data.frame(name=c('a.csv','b.csv'),datapath=paths)
records <- new.env();records$count <- 0L
server <- function(input,output,session) {
 lang <- reactiveVal('en')
 register_merge_handlers(input,output,session,function(data,...){records$count<-records$count+1L;records$data<-data;TRUE},function()NULL,lang)
}
shiny::testServer(server, {
 session$setInputs(merge_files=uploaded,merge_csv_header=TRUE,merge_mode='cases',merge_case_variables='Review',merge_indicator_name='사용자_시점',merge_indicator_values='시작,종료',preview_merge_data=0,run_merge_data=0)
 session$flushReact();session$setInputs(preview_merge_data=1)
 for(language in langs) {
  lang(language);session$flushReact()
  html<-as.character(output$merge_data_message$html)
  stopifnot(length(html)==1L,grepl(sprintf(statedu_t('merge.ui.preview',language),4,2),html,fixed=TRUE),records$count==0L)
 }
 session$setInputs(run_merge_data=1)
 stopifnot(records$count==1L,identical(names(records$data),c('Review','사용자_시점')),identical(records$data$Review,c(10L,20L,30L,40L)))
 for(language in langs) {
  lang(language);session$flushReact()
  html<-as.character(output$merge_data_message$html)
  stopifnot(length(html)==1L,grepl(sprintf(statedu_t('merge.ui.loaded',language),4,2),html,fixed=TRUE),records$count==1L)
 }
})
unlink(paths)
cat('PASS: preview/loaded status follows 8 languages without repeating dataset replacement\n')
