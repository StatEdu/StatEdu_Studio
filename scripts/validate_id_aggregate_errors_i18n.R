Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
data<-data.frame(id=c('A','A','B'),Review=1:3)
err<-function(f)tryCatch({f();stop('Expected test failure was not raised')},error=function(e)conditionMessage(e))
run<-function(...)id_aggregate_dataset(data,'id',value_variable='Review',...)
messages<-list(
 one_condition=err(function()run(condition_expression='TRUE; FALSE')),
 condition_length=err(function()run(condition_expression='NULL')),
 statistic=err(function()run(stat='invalid')),
 no_rows=err(function()id_aggregate_dataset(data[FALSE,],'id',value_variable='Review')),
 id_variable=err(function()id_aggregate_dataset(data,'missing',value_variable='Review')),
 value_variable=err(function()id_aggregate_dataset(data,'id',value_variable='missing')),
 no_ids=err(function()id_aggregate_dataset(data.frame(id=c(NA,NA),Review=1:2),'id',value_variable='Review')),
 direct_calls=err(function()run(condition_expression='(mean)(Review)')),
 unsupported_element=err(function()transform_validate_expression(pairlist(a=1),names(data))),
 parse=err(function()run(condition_expression='Review >')),
 unknown_symbol=err(function()run(condition_expression='`사용자 <&> %s` > 0')),
 unavailable_function=err(function()run(condition_expression='`사용자 함수 <&> %s`(Review)'))
)
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 for(key in names(messages)){
  raw<-messages[[key]];actual<-id_aggregate_error_text(raw,language);template<-statedu_t(paste0('id_aggregate.error.',key),language)
  if(key %in% c('parse','unknown_symbol','unavailable_function')){
   suffix<-substring(raw,regexpr(': ',raw,fixed=TRUE)[1]+2L)
   stopifnot(identical(actual,sprintf(template,suffix)),endsWith(actual,suffix))
  }else stopifnot(identical(actual,template))
  if(language=='en')stopifnot(identical(actual,raw))
 }
 for(raw in c('사용자 <&> %s','Review','Unrecognized external engine message'))stopifnot(identical(id_aggregate_error_text(raw,language),raw))
 cat('PASS:',language,'12 actual error branches; engine details and user names preserved\n')
}
notices<-new.env();notices$values<-character()
showNotification<-function(ui,...){notices$values<-c(notices$values,as.character(ui));invisible('test')}
server<-function(input,output,session){
 lang<-reactiveVal('ko')
 register_id_aggregate_handlers(input,output,session,function()data,function(...)stop('Invalid condition must not replace data'),function()NULL,language_fn=lang)
}
shiny::testServer(server,{
 session$setInputs(preview_id_aggregate=0,run_id_aggregate=0,id_aggregate_id='id',id_aggregate_value='Review',id_aggregate_condition='`사용자 <&> %s` > 0');session$flushReact()
 langs<-c('en','ko','ja','zh','es','fr','de','vi')
 for(i in seq_along(langs)){
  lang(langs[[i]]);session$flushReact();notices$values<-character()
  session$setInputs(preview_id_aggregate=i);session$setInputs(run_id_aggregate=i)
  expected<-id_aggregate_error_text(messages$unknown_symbol,langs[[i]])
  stopifnot(identical(notices$values,rep(expected,2)))
  cat('PASS:',langs[[i]],'preview and create notifications use active UI language\n')
 }
})
