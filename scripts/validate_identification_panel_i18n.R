Sys.setlocale('LC_CTYPE','English_United States.utf8')
source('scripts/validate_identification_inventory_i18n.R',encoding='UTF-8')
out<-'tmp/identification-panel-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
fit<-lavaan::cfa('visual =~ x1+x2+x3\ntextual =~ x4+x5+x6\nspeed =~ x7+x8+x9',data=lavaan::HolzingerSwineford1939)
snapshot<-list(nodes=c(lapply(c('a','b','c'),function(id)list(id=id,role='latent')),list(list(id='x',role='indicator'))),edges=list(list(from='a',to='b'),list(from='b',to='c',kind='covariance'),list(from='a',to='c',pathType='higherOrder')))
issues<-data.frame(Severity=rep(c('Warning','Error'),length.out=length(codes)),Element=rep('Review 사용자 <&> %s',length(codes)),Code=codes,Message=messages)
issues<-rbind(issues,data.frame(Severity='Warning',Element='Normality',Code='cross_loading',Message='The indicator loads on multiple factors: Primary, 사용자 %s. Review simple-structure assumptions and reliability/validity summaries.'))
for(basis in c('not_recorded','rmsea_power','model_monte_carlo','target_effect_precision','prior_evidence','other_documented','External basis'))for(detailed in c(FALSE,TRUE))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 bundle<-list(snapshot=snapshot,fit=if(detailed)fit else NULL,std_lv=detailed,power_basis=basis,power_details=if(detailed)'Review Normality 사용자 <&> %s'else'',identification=if(detailed)issues else data.frame())
 html<-as.character(structural_canvas_identification_result_ui(bundle,language));doc<-xml2::read_html(html,encoding='UTF-8')
 inventory<-xml2::xml_find_first(doc,"//table[contains(@class,'structural-identification-inventory')]")
 values<-trimws(xml2::xml_text(xml2::xml_find_all(inventory,'.//tr/td[2]')));labels<-trimws(xml2::xml_text(xml2::xml_find_all(inventory,'.//tr/td[1]')))
 stopifnot(identical(values[1:3],c('3','1','1')))
 if(detailed)stopifnot(values[5]==format(lavaan::fitMeasures(fit,'df'),trim=TRUE),values[6]==format(lavaan::lavInspect(fit,'npar'),trim=TRUE))
 if(detailed&&basis!='not_recorded')stopifnot(grepl(bundle$power_details,values[7],fixed=TRUE))
 if(basis=='External basis')stopifnot(grepl(basis,values[7],fixed=TRUE))
 notes<-xml2::xml_text(xml2::xml_find_all(doc,'//p'));heading<-xml2::xml_text(xml2::xml_find_first(doc,'//h5'))
 if(language=='en'){en_notes<-notes;en_heading<-heading;en_labels<-labels}else{
  stopifnot(all(notes!=en_notes),heading!=en_heading,all(labels!=en_labels))
  if(!basis%in%c('not_recorded','External basis'))stopifnot(!grepl(basis,values[7],fixed=TRUE))
 }
 if(detailed){
  cells<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//td')));stopifnot(all(issues$Element%in%cells),all(issues$Code%in%cells))
  if(language!='en')stopifnot(!any(messages%in%cells),any(grepl('Primary, 사용자 %s',cells,fixed=TRUE)))
  if(!language%in%c('en','de'))stopifnot(!'Element'%in%xml2::xml_text(xml2::xml_find_all(doc,'//th')))
 }
 if(language=='ja'&&(!detailed||basis=='other_documented')){key<-paste(basis,detailed,sep='-');entries[[key]]<-list(id=key,title=key,html=html)}
 cat('PASS:',basis,detailed,language,'inventory, power basis, authored details and diagnostic table\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
