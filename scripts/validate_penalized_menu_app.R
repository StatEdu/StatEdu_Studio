Sys.setenv(STATEDU_MODULE_CACHE="false")
source("R/app_bootstrap.R",encoding="UTF-8");load_app_packages(check=FALSE);source_app_modules()
addResourcePath("qa",normalizePath("www"))
ui<-fluidPage(tags$head(tags$link(rel="stylesheet",href="qa/style.css"),tags$script(src="qa/easyflow.js"),tags$script(src="qa/analysis-scope.js")),penalized_menu_panel("regularized","ko"))
server<-function(input,output,session){
 set.seed(7);d<-data.frame(x=rnorm(70),z=rnorm(70),sex=rep(c("M","F"),35));d$y<-3*d$x-d$z+rnorm(70)
 d$group<-factor(rep(c("A","B","C"),length.out=nrow(d)));d$y<-d$y+3*(d$group=="B")-2*(d$group=="C")
 info<-data.frame(name=names(d),measurement=c("continuous","continuous","binary","continuous","nominal"))
 register_penalized_menu("regularized",input,output,session,function()d,function()names(d),function()info,function()character(),function()NULL,function()"ko")
}
runApp(shinyApp(ui,server),host="127.0.0.1",port=3869,launch.browser=FALSE)

