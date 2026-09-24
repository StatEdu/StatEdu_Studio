Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/structural-multigroup-ui-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
old<-new.env(parent=globalenv());if(file.exists('tmp/structural-multigroup-ui-before.R'))sys.source('tmp/structural-multigroup-ui-before.R',old)
snapshot<-list(nodes=list(list(id='a',role='latent',name='Normality'),list(id='b',role='latent',name='사용자 요인'),list(id='x',role='observed',name='x')),
 edges=list(list(id='user_path',from='a',to='b',kind='regression'),list(id='measurement',from='a',to='x',kind='regression'),list(id='covariance',from='a',to='b',kind='covariance')))
contract<-function(doc)lapply(xml2::xml_find_all(doc,'//button|//input|//select|//option|//textarea'),function(node){a<-xml2::xml_attrs(node);a[grepl('^(data-|id$|class$|value$|name$|checked$|selected$|type$|rows$)',names(a))]})
normalize_ids<-function(x)gsub('data-tabsetid="[0-9]+"','data-tabsetid="ID"',gsub('tab-[0-9]+-','tab-ID-',x))
for(type in c('cfa','cbsem','plssem')){
 base<-as.character(structural_analysis_options_panel(type,'en',c('사용자 집단 [g]'='g','Normality'='g2')))
 if(exists('structural_analysis_options_panel',old,inherits=FALSE))stopifnot(identical(normalize_ids(base),normalize_ids(as.character(old$structural_analysis_options_panel(type,'en',c('사용자 집단 [g]'='g','Normality'='g2'))))))
 base_doc<-xml2::read_html(base,encoding='UTF-8')
 base_panel<-xml2::xml_find_all(base_doc,'//*[contains(concat(" ",@class," ")," tab-pane ")]')[[4]]
 for(lang in c('en','ko','ja','zh','es','fr','de','vi')){
  display_name<-structural_canvas_display_name_resolver(snapshot=snapshot,language=lang)
  paths<-structural_canvas_multigroup_path_choices(snapshot,display_name)
  stopifnot(identical(unname(paths),'user_path'),identical(names(paths),'Normality \u2192 사용자 요인'))
  html<-as.character(structural_analysis_options_panel(type,lang,c('사용자 집단 [g]'='g','Normality'='g2')))
  doc<-xml2::read_html(html,encoding='UTF-8');stopifnot(identical(contract(doc),contract(base_doc)))
  panel<-xml2::xml_find_all(doc,'//*[contains(concat(" ",@class," ")," tab-pane ")]')[[4]]
  group_options<-xml2::xml_find_all(panel,'.//select[contains(@id,"invariance_group")]/option')
  stopifnot(identical(tail(xml2::xml_text(group_options),2),c('사용자 집단 [g]','Normality')),identical(tail(xml2::xml_attr(group_options,'value'),2),c('g','g2')))
  visible<-paste(xml2::xml_text(panel),paste(xml2::xml_attr(xml2::xml_find_all(panel,'.//*[@placeholder]'),'placeholder'),collapse=' '))
  if(!lang %in% c('en','ko')){
   texts<-c(trimws(xml2::xml_text(xml2::xml_find_all(base_panel,'.//label|.//option|.//p|.//h5'))),xml2::xml_attr(xml2::xml_find_all(base_panel,'.//*[@placeholder]'),'placeholder'))
   texts<-texts[nzchar(texts)&!grepl('^[0-9,]+$',texts)&!texts %in% c('사용자 집단 [g]','Normality')]
   dictionary<-jsonlite::fromJSON(file.path('i18n',paste0(lang,'.json')))$translations
   for(en in texts){
    key<-paste0('analysis.ui.',gsub('^_|_$','',gsub('[^a-z0-9]+','_',tolower(en))))
    if(is.null(dictionary[[key]]))stop(paste('Missing dictionary:',lang,en))
    if(!grepl(dictionary[[key]],visible,fixed=TRUE))stop(paste('Missing translation:',lang,en))
   }
  }
  stopifnot(identical(xml2::xml_attr(xml2::xml_find_all(panel,'.//*[@data-display-if]'),'data-display-if'),xml2::xml_attr(xml2::xml_find_all(base_panel,'.//*[@data-display-if]'),'data-display-if')))
  writeLines(html,file.path(out,paste0(type,'-',lang,'.html')),useBytes=TRUE)
  cat('PASS:',type,lang,'multigroup labels/help/choices and user group labels; input values, defaults and conditional rules preserved\n')
 }
}
