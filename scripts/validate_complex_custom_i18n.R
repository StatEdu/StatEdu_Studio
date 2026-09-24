Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(952);n<-120
d<-data.frame(strata=rep(1:3,each=40),psu=rep(1:30,each=4),weight=runif(n,.5,2),X=rnorm(n),W=rnorm(n),C=rnorm(n))
d$M<-.6*d$X+.2*d$C+rnorm(n);d$Normality<-.2*d$X+.7*d$M+.3*d$M*d$W+rnorm(n)
node<-function(id,v,role)list(id=id,variableId=v,role=role,x=0,y=0)
edge<-function(id,from,to)list(id=id,from=from,to=to)
snapshot<-list(nodes=list(node('x','X','independent'),node('m','M','mediator'),node('w','W','moderator'),node('y','Normality','dependent')),edges=list(edge('xm','x','m'),edge('my','m','y'),edge('xy','x','y')),moderations=list(list(id='w_my',from='w',toEdge='my')),covariates='C')
design<-complex_sample_normalize_design_state(list(strata='strata',cluster='psu',weight='weight'))
fit<-complex_sample_run_custom_model(d,snapshot,design)
stopifnot(any(fit$effects$Effect=='Indirect'),length(unique(fit$effects$Condition))==3,all(is.finite(fit$effects$Estimate)))
out<-'tmp/complex-custom-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);baseline<-NULL
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language);html<-as.character(complex_sample_custom_model_result_ui(fit,language));doc<-xml2::read_html(html,encoding='UTF-8')
 main<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']/ancestor::div[contains(concat(' ',normalize-space(@class),' '),' result-section ')][1]"))
 if(is.null(baseline))baseline<-main else stopifnot(identical(baseline,main))
 appendix<-xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']");stopifnot(length(main)==2,length(appendix)==3,all(xml2::xml_attr(appendix,'data-result-table-language')==language))
 text<-paste(xml2::xml_text(appendix),collapse='\n')
 for(source in c('Complex Samples Mediation / Moderation','Analysis N','Design degrees of freedom','Equations','Equation','Survey regression')){
  key<-paste0('analysis.ui.',gsub('^_+|_+$','',gsub('[^a-z0-9]+','_',tolower(source))));expected<-statedu_t(key,language,source)
  stopifnot(grepl(expected,text,fixed=TRUE));if(language!='en')stopifnot(expected!=source)
 }
 syntax_rows<-xml2::xml_find_all(appendix[[2]],'.//tbody/tr')
 displayed<-lapply(syntax_rows,function(row)xml2::xml_text(xml2::xml_find_all(row,'./td')))
 stopifnot(identical(vapply(displayed,`[[`,character(1),3),fit$syntax$Syntax),identical(vapply(displayed[-1],`[[`,character(1),2),fit$syntax$Equation[-1]))
 writeLines(html,file.path(out,paste0(language,'.html')),useBytes=TRUE)
 if(language=='ja')saveRDS(list(list(id='complex-custom',title='Complex-sample mediation/moderation',html=html)),file.path(out,'entries.rds'))
 cat('PASS:',language,'actual moderated mediation, main sections preserved\n')
 if(language=='ja')cat('APPENDIX:',xml2::xml_text(appendix),'\n')
}
