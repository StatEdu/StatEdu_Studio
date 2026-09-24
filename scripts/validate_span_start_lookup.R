source('output/span-start-lookup-20260914/common.R')
capture <- function(fn,table,row,column,columns) {
  conditions<-character()
  value<-withCallingHandlers(tryCatch(fn(table,row,column),error=function(e)list(error=conditionMessage(e))),
    warning=function(w){conditions<<-c(conditions,conditionMessage(w));invokeRestart('muffleWarning')})
  list(value=value,conditions=conditions)
}
set.seed(921);count<-0L
for(i in 1:40)for(kind in c('numeric','missing','character','custom')) {
  table<-make_table(5,FALSE)
  spans<-data.frame(row=sample(1:6,15,TRUE),start_column=sample(c(LETTERS[1:6],'absent'),15,TRUE),end_column=sample(c(LETTERS[1:6],NA),15,TRUE))
  if(kind=='missing')spans$row[1]<-NA_real_
  if(kind=='character')spans$row<-as.character(spans$row)
  if(kind=='custom')class(spans)<-c('special_spans','data.frame')
  attr(table,'spanning_cells')<-spans
  for(row in 1:5)for(column in c('A','C','E','absent')) {
    stopifnot(identical(capture(baseline,table,row,column,names(table)),capture(current,table,row,column,names(table))));count<-count+1L
  }
}
for(row in list(matrix(1,1,1),NA_real_,numeric(),c(1,2),'1',Inf)) {
  table<-make_table(5)
  stopifnot(identical(capture(baseline,table,row,'C',names(table)),capture(current,table,row,'C',names(table))));count<-count+1L
}
for(kind in c('factor','duplicate','empty')) {
  table<-make_table(5);spans<-attr(table,'spanning_cells')
  if(kind=='factor')spans$value<-factor(spans$value)
  if(kind=='duplicate'){spans<-rbind(spans,spans[1,]);spans$value[nrow(spans)]<-'Second match'}
  if(kind=='empty')spans<-spans[FALSE,]
  attr(table,'spanning_cells')<-spans
  for(row in 1:5) {
    stopifnot(identical(capture(baseline,table,row,'B',names(table)),capture(current,table,row,'B',names(table)),num.eq=FALSE));count<-count+1L
  }
}
for(n in c(1L,5L,20L))for(spanned in c(FALSE,TRUE))for(language in c('ko','en')) {
  table<-make_table(n,spanned);outputs<-list()
  for(v in c('baseline','current')) {
    select_variant(v)
    panel<-htmltools::renderTags(coefficient_html_table(table,table_language=language))
    html<-saved_results_document('Span test',htmltools::HTML(panel$html))
    excel<-file.path(root,paste(v,n,spanned,language,'xlsx',sep='.'));save_screen_excel_file(html,excel)
    sheets<-openxlsx::getSheetNames(excel)
    outputs[[v]]<-list(panel=panel,html=html,tables=result_entry_tables(list(title='Span test',html=html),1L),
      sheets=sheets,cells=lapply(sheets,function(s)openxlsx::read.xlsx(excel,sheet=s)))
  }
  stopifnot(identical(outputs$baseline,outputs$current,num.eq=FALSE))
}
select_variant('current')
cat('PASS:',count,'span value/error/condition comparisons and 12 merged/unmerged HTML/accumulated/Excel comparisons.\n')

