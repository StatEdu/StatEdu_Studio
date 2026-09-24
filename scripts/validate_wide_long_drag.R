Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
data<-data.frame(id=1:2,x1=1:2,x2=3:4,y1=5:6,y2=7:8)
records<-new.env();records$dirty<-0L
server<-function(input,output,session){
 register_wide_long_handlers(input,output,session,function()data,function()list(name='test.csv'),function()NULL,function()NULL,
 function(...)TRUE,function(){records$dirty<-records$dirty+1L},function()'en')
}
shiny::testServer(server,{
 session$setInputs(wide_long_move=0,wide_long_set_spec=0,wide_long_configured_reorder=list(order=character(),selected=character()));session$flushReact()
 for(i in 1:2){
  session$setInputs(wide_long_available=paste0(c('x','y')[i],1:2),wide_long_move=i)
  session$setInputs(wide_long_value_name=c('A','B')[i],wide_long_unit_type='different',wide_long_index_name='time',wide_long_index_values='1,2')
  session$setInputs(wide_long_set_spec=i)
 }
 ids<-function(){doc<-xml2::read_html(as.character(output$wide_long_setup$html));xml2::xml_attr(xml2::xml_find_all(doc,'//select[@id="wide_long_configured"]/option'),'value')}
 original<-ids();before<-records$dirty
 session$setInputs(wide_long_configured=character(),wide_long_configured_reorder=list(order=rev(original),selected=original[2],nonce=1))
 stopifnot(identical(ids(),rev(original)),records$dirty==before+1L)
 session$setInputs(wide_long_configured_reorder=list(order=rep(original[1],2),selected='unknown',nonce=2))
 stopifnot(identical(ids(),rev(original)),records$dirty==before+1L)
 session$setInputs(wide_long_configured_reorder=list(order=c(original[1],'unknown'),selected='unknown',nonce=3))
 stopifnot(identical(ids(),rev(original)),records$dirty==before+1L)
})
cat('PASS: drag without prior selection updates model and dirty state; duplicate/foreign IDs rejected\n')
