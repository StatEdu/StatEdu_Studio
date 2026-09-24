Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/causal-boundary-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
for(type in c('cbsem','sem','plssem'))for(chain in c(FALSE,TRUE)){
 snapshot<-list(nodes=lapply(c('x','m','y'),function(id)list(id=id,role='latent',name=paste0('Review 사용자 ',id))),edges=if(chain)list(list(from='x',to='m'),list(from='m',to='y'))else list(list(from='x',to='y')))
 result<-structural_canvas_causal_interpretation(snapshot,type);stopifnot(identical(result$indirect_chain_detected,chain))
 for(language in c('en','ko','ja','zh','es','fr','de','vi')){
  html<-as.character(structural_canvas_causal_boundary_ui(result,language));doc<-xml2::read_html(html,encoding='UTF-8')
  cells<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//td')))
  heading<-xml2::xml_text(xml2::xml_find_first(doc,'//h5'));note<-xml2::xml_text(xml2::xml_find_first(doc,'//p'))
  stopifnot(length(cells)==12L)
  if(language=='en'){en_cells<-cells;en_heading<-heading;en_note<-note}else{
   stopifnot(all(cells!=en_cells),heading!=en_heading,note!=en_note)
   stopifnot(!any(c('Assumption','Recorded','Consequence')%in%xml2::xml_text(xml2::xml_find_all(doc,'//th'))))
  }
  stopifnot(identical(result,structural_canvas_causal_interpretation(snapshot,type)))
  if(type=='cbsem'&&language=='ja'){key<-if(chain)'indirect'else'direct';entries[[key]]<-list(id=key,title=key,html=html)}
  cat('PASS:',type,chain,language,'causal note, all assumptions and reporting limits\n')
 }
}
raw<-list(applicable=TRUE,rows=data.frame(Assumption='Review 사용자 <&> %s',Recorded='Normality',Consequence='Primary'))
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 doc<-xml2::read_html(as.character(structural_canvas_causal_boundary_ui(raw,language)),encoding='UTF-8')
 stopifnot(identical(trimws(xml2::xml_text(xml2::xml_find_all(doc,'//td'))),unname(unlist(raw$rows))))
}
stopifnot(is.null(structural_canvas_causal_boundary_ui(structural_canvas_causal_interpretation(list(),'cfa'))))
saveRDS(unname(entries),file.path(out,'entries.rds'))
