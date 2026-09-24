Sys.setlocale("LC_CTYPE","English_United States.utf8")
Sys.setenv(STATEDU_MODULE_CACHE="false")
source("R/app_bootstrap.R",encoding="UTF-8");load_app_packages(check=FALSE);source_app_modules()
d<-data.frame(y=1:8,x=2:9,g=letters[1:8]);info<-data.frame(name=names(d),measurement=c("continuous","continuous","category"))
capture_error<-function(expr){value<-tryCatch({force(expr);NULL},error=function(e)conditionMessage(e));stopifnot(!is.null(value));value}
errors<-c(capture_error(prepare_penalized_menu(d,character(),"x","Ridge")),capture_error(prepare_penalized_menu(d,"g","x","Ridge")),capture_error(prepare_penalized_menu(d,"y",character(),"Ridge")),capture_error(prepare_penalized_menu(d,"y","x","Ridge",resamples=0)))
sources<-jsonlite::read_json("scripts/fixtures/penalized_i18n_setup.json",simplifyVector=TRUE)
baseline<-NULL
for(language in c("en","ko","ja","zh","es","fr","de","vi")) {
 options(statedu.app_language=language)
 for(i in seq_along(errors))stopifnot(identical(penalized_error_ui_text(errors[[i]],language),statedu_t(paste0("analysis.penalized_error.",c("outcome","outcome_type","predictors","resamples")[[i]]),language)))
 unknown<-"External error: 사용자.A 50%";stopifnot(identical(penalized_error_ui_text(unknown,language),unknown))
 shiny::testServer(function(input,output,session){
  register_penalized_menu("regularized",input,output,session,function()d,function()names(d),function()info,function()character(),function()NULL,function()language)
 },{
  session$flushReact()
  html<-output$penalized_regularized_setup$html
  doc<-xml2::read_html(html,encoding="UTF-8")
  text<-xml2::xml_text(doc)
  for(s in sources) {
   key<-paste0("analysis.ui.",gsub("^_+|_+$","",gsub("[^a-z0-9]+","_",tolower(s))))
   stopifnot(grepl(statedu_t(key,language),text,fixed=TRUE))
   if(language!="en")stopifnot(!grepl(s,text,fixed=TRUE))
  }
  inputs<-xml2::xml_find_all(doc,"//input")
  attrs<-lapply(c("id","name","type","value","min","max","step","checked"),function(a)xml2::xml_attr(inputs,a))
  if(language=="en") baseline<<-attrs else stopifnot(identical(attrs,baseline))
 })
 cat("PASS:",language,"actual setup render; controls unchanged; four actual input errors translated\n")
}
