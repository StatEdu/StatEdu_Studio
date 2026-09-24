Sys.setlocale('LC_CTYPE','English_United States.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false',STATEDU_NO_PACKAGE_INSTALL='true')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(924);n<-160;f<-rnorm(n);g<-.5*f+rnorm(n)
d<-data.frame(x1=f+rnorm(n),x2=f+rnorm(n),x3=f+rnorm(n),y1=g+rnorm(n),y2=g+rnorm(n),y3=g+rnorm(n))
make_bundle<-function(type){
 if(type=='plssem'){
  fit<-seminr::estimate_pls(d,seminr::constructs(seminr::composite('X',seminr::multi_items('x',1:3)),seminr::composite('Y',seminr::multi_items('y',1:3))),seminr::relationships(seminr::paths(from='X',to='Y')),assess_syntax=FALSE)
  bundle<-list(fit=fit,estimator='PLS',missing='mean_replacement',diagnostics=list(n=n))
 }else{
  model<-'X =~ x1+x2+x3\nY =~ y1+y2+y3'
  if(type=='cbsem')model<-paste(model,'Y ~ X',sep='\n')
  fit<-lavaan::sem(model,data=d)
  bundle<-list(fit=fit,estimator='ML',missing='listwise',diagnostics=structural_canvas_fit_admissibility(fit))
 }
 bundle$snapshot<-list(nodes=c(lapply(c('X','Y'),function(v)list(id=v,name=v,role='latent',constructType=if(type=='plssem')'composite' else 'commonFactor',measurementMode='reflective')),
 lapply(names(d),function(v)list(id=v,name=v,role='indicator'))),edges=lapply(names(d),function(v)list(from=if(substr(v,1,1)=='x')'X' else 'Y',to=v)))
 bundle$invariance_enabled<-TRUE;bundle$invariance_group<-'Requested 사용자 <&>'
 bundle
}
bundles<-setNames(lapply(c('cfa','cbsem','plssem'),make_bundle),c('cfa','cbsem','plssem'))
ui<-fluidPage(selectInput('language','Language',c('ko','ja','zh','es','fr','de','vi','en')),uiOutput('results'))
server<-function(input,output,session){
 output$results<-renderUI({
  language<-input$language
  div(id='rendered-results',`data-language`=language,lapply(names(bundles),function(type){
   bundle<-bundles[[type]]
   table<-structural_canvas_result_table('measurement',function()bundle,type,function()c(x1='사용자 라벨 Normality'),function()'en')
   diagnostics<-structural_canvas_result_table(if(type=='plssem')'measurement_guide' else 'measurement_diagnostics',function()bundle,type,function()c(x1='사용자 라벨 Normality'),function()'en')
   div(id=paste0('result-',type),
    div(class='main-result',structural_canvas_basic_html_table(table,role='main',language=language,title='Measurement model')),
    structural_canvas_reporting_context_result_ui(bundle,type,language),
    if(type=='plssem')structural_canvas_pls_quality_result_ui(bundle,language) else structural_canvas_lavaan_quality_result_ui(bundle,type,language),
    structural_canvas_measurement_diagnostics_ui(diagnostics,bundle,type,2,language))
  }))
 })
}
shiny::runApp(shinyApp(ui,server),host='127.0.0.1',port=43874,launch.browser=FALSE)
