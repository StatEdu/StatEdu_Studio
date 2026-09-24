Sys.setenv(STATEDU_MODULE_CACHE="false")
source("R/app_bootstrap.R",encoding="UTF-8");load_app_packages(check=FALSE);source_app_modules()
dir.create("tmp/penalized-menu",recursive=TRUE,showWarnings=FALSE)
set.seed(7);d<-data.frame(x=rnorm(70),z=rnorm(70),sex=rep(c("M","F"),35));d$y<-3*d$x-d$z+rnorm(70)
info<-data.frame(name=names(d),measurement=c("continuous","continuous","binary","continuous"))
attr(d,"statedu_scope_excluded")<-"sex"
for(method in penalized_menu_methods()){
 r<-prepare_penalized_menu(d,"y",c("x","z","sex"),method,info,resamples=2,seed=12)
 stopifnot(identical(unique(r$cv_curves$Method),method),!any(grepl("sex",r$coefficients$Predictor)))
 saveRDS(r,file.path("tmp/penalized-menu",paste0(gsub(" ","",method),".rds")))
}
menu<-paste(as.character(analysis_tab_panel(language="ko")),collapse="")
d$w <- rnorm(nrow(d))
multi <- prepare_penalized_menu(d,c("y","z"),c("x","w","sex"),"Ridge",info,resamples=2,seed=12)
stopifnot(setequal(unique(multi$summary$Outcome),c("y","z")))
stopifnot(grepl('analysis_penalized',menu,fixed=TRUE),length(gregexpr('data-value="analysis_penalized"',menu,fixed=TRUE)[[1]])==1L)
message("PASS: single navigation menu; selected method only; all 3 engines; scope exclusion")

