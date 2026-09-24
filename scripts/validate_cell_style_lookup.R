source('output/cell-style-lookup-20260914/common.R')
capture <- function(fn,table,row,column) {
  conditions <- character()
  value <- withCallingHandlers(tryCatch(fn(table,row,column),error=function(e)list(error=conditionMessage(e))),
    warning=function(w){conditions<<-c(conditions,conditionMessage(w));invokeRestart('muffleWarning')})
  list(value=value,conditions=conditions)
}
set.seed(911);table <- data.frame(a=1:5,b=6:10);count <- 0L
for(i in 1:50)for(kind in c('character','factor','custom','absent')) {
  styles <- data.frame(row=sample(c(1:5,NA),20,TRUE),column=sample(c('a','b',NA),20,TRUE),
    style=sample(c('color:red;','text-align:left;','',NA),20,TRUE))
  if(kind=='factor')styles$style<-factor(styles$style)
  if(kind=='custom')class(styles)<-c('custom_styles','data.frame')
  if(kind=='absent')styles$style<-NULL
  attr(table,'cell_styles')<-styles
  for(row in 1:5)for(column in c('a','b','missing')) {
    stopifnot(identical(capture(baseline,table,row,column),capture(current,table,row,column),num.eq=FALSE));count<-count+1L
  }
}
for(styles in list(NULL,data.frame(),data.frame(row=c(1,1),column='a',style=c('first;','second;')))) {
  attr(table,'cell_styles')<-styles
  stopifnot(identical(capture(baseline,table,1,'a'),capture(current,table,1,'a')));count<-count+1L
}
# Use the established end-to-end correlation export checks with this helper switched.
text <- readLines('scripts/validate_correlation_overview_rows.R',warn=FALSE)
start <- grep('set.seed(902)',text,fixed=TRUE)
code <- text[start:length(text)]
code <- code[!startsWith(code,'cat(')]
eval(parse(text=code),envir=.GlobalEnv)
cat('PASS:',count,'style value/condition comparisons; observed/latent-option screen/HTML/accumulated/Excel content identical.\n')
