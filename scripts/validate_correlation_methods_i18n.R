Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(933);n<-120
d<-data.frame(x=rnorm(n),y=rnorm(n),o1=sample(1:4,n,TRUE),o2=sample(1:4,n,TRUE),b1=sample(0:1,n,TRUE),b2=sample(0:1,n,TRUE),nom=sample(c('A','B','C'),n,TRUE))
info<-data.frame(name=names(d),measurement=c('continuous','continuous','ordinal','ordinal','binary','binary','category'),var_label=c('Normality','사용자 결과','Yes','사용자 순서','사용자 이분','Status','사용자 명목'))
fits<-list(observed=prepare_correlation_results(d,names(d),info,options=list(continuous_method='kendall',normality=TRUE,p_ci=TRUE)),
 latent=prepare_correlation_results(d,names(d),info,options=list(continuous_method='kendall',normality=TRUE,p_ci=TRUE,latent_correlations=TRUE)))
stopifnot(all(c('Kendall','Phi','Eta',"Cramer's V",'Point-biserial') %in% fits$observed$pairwise_table$Method),all(c('Polyserial','Polychoric','Tetrachoric') %in% fits$latent$pairwise_table$Method))
out<-'tmp/correlation-methods-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);baseline<-list();axes<-list();entries<-list()
desc<-c(tau="Kendall's tau",r_pb='point-biserial correlation',phi='phi coefficient',V="Cramer's V",eta='eta coefficient',polyser='polyserial correlation',polychor='polychoric correlation',tetra='tetrachoric correlation')
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language)
 for(name in names(fits)){
  html<-as.character(correlation_results_ui(fits[[name]]));doc<-xml2::read_html(html,encoding='UTF-8')
  main<-xml2::xml_find_all(doc,"//table[@data-result-table-role='main']")
  content<-xml2::xml_text(xml2::xml_find_all(main,".//th|.//td|preceding::h3[1]|ancestor::div[@data-result-table-sheet='true'][1]/*[not(self::table)]"))
  stopifnot(length(main)==1L,xml2::xml_attr(main,'data-result-table-language')=='en')
  if(language=='en')baseline[[name]]<-content
  stopifnot(identical(content,baseline[[name]]))
  appendix<-xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']")
  stopifnot(all(xml2::xml_attr(appendix,'data-result-table-language')==language))
  axis<-lapply(appendix[1:2],function(t)list(headers=xml2::xml_text(xml2::xml_find_all(t,'.//th'))[-1],rows=xml2::xml_text(xml2::xml_find_all(t,'.//tbody/tr/td[1]'))))
  if(language=='en')axes[[name]]<-axis
  stopifnot(identical(axis,axes[[name]]))
  text<-xml2::xml_text(doc)
  stopifnot(all(vapply(info$var_label,grepl,logical(1),x=text,fixed=TRUE)))
  used<-if(name=='observed')names(desc)[1:5]else names(desc)[6:8]
  for(code in used){
   key<-paste0('analysis.ui.',gsub('^_|_$','',gsub('[^a-z0-9]+','_',tolower(desc[[code]]))))
   stopifnot(grepl(paste0(code,' = ',statedu_t(key,language)),text,fixed=TRUE))
  }
  if(name=='latent'){
   key<-'analysis.ui.polyserial_polychoric_and_tetrachoric_inference_uses_two_step_asymptotic_standard_errors_with_fixed_thresholds_and_fisher_z_wald_inference'
   stopifnot(grepl(sub('[.]$','',statedu_t(key,language)),text,fixed=TRUE))
  }
  if(language=='ja')entries[[name]]<-html
 }
 cat('PASS:',language,'eight additional correlation methods; main content and user labels; latent inference note\n')
}
saveRDS(list(list(id='correlation-methods',title='Correlation methods',html=paste(unlist(entries),collapse='\n'))),file.path(out,'entries.rds'))
