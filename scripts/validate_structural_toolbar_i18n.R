Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/structural-toolbar-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
sources<-function(x){
 if(is.call(x)&&identical(x[[1]],as.name('statedu_localized_text')))return(list(x))
 if(is.recursive(x))return(unlist(lapply(as.list(x),sources),recursive=FALSE))
 list()
}
calls<-sources(body(structural_equation_toolbar))
contract<-function(doc)lapply(xml2::xml_find_all(doc,'//button|//input|//select|//option'),function(node){a<-xml2::xml_attrs(node);a[grepl('^(data-|id$|class$|value$|name$|checked$|selected$|type$)',names(a))]})
old<-new.env(parent=globalenv());if(file.exists('tmp/structural-toolbar-before.R'))sys.source('tmp/structural-toolbar-before.R',old)
for(type in c('cfa','cbsem','plssem')){
 set.seed(513);base<-as.character(structural_equation_toolbar(type,'en',c('사용자 집단 [g]'='g')))
 if(exists('structural_equation_toolbar',old,inherits=FALSE)){
  set.seed(513);before<-as.character(old$structural_equation_toolbar(type,'en',c('사용자 집단 [g]'='g')))
  normalize_ids<-function(x)gsub('data-tabsetid="[0-9]+"','data-tabsetid="ID"',gsub('tab-[0-9]+-','tab-ID-',x))
  stopifnot(identical(normalize_ids(base),normalize_ids(before)))
 }
 base_doc<-xml2::read_html(base,encoding='UTF-8')
 for(lang in c('en','ko','ja','zh','es','fr','de','vi')){
  html<-as.character(structural_equation_toolbar(type,lang,c('사용자 집단 [g]'='g')))
  doc<-xml2::read_html(html,encoding='UTF-8')
  stopifnot(identical(contract(doc),contract(base_doc)))
  visible<-paste(xml2::xml_text(doc),paste(xml2::xml_attr(xml2::xml_find_all(doc,'//*[@title]'),'title'),collapse=' '))
  for(call in calls){
   en<-call[[3]];ko<-call[[4]]
   if(grepl(en,paste(xml2::xml_text(base_doc),paste(xml2::xml_attr(xml2::xml_find_all(base_doc,'//*[@title]'),'title'),collapse=' ')),fixed=TRUE)){
    value<-statedu_localized_text(lang,en,ko)
    stopifnot(grepl(value,visible,fixed=TRUE))
    if(!lang %in% c('en','ko')){
     dictionary<-jsonlite::fromJSON(file.path('i18n',paste0(lang,'.json')))$translations
     key<-paste0('analysis.ui.',gsub('^_|_$','',gsub('[^a-z0-9]+','_',tolower(en))))
     stopifnot(!is.null(dictionary[[key]]),nzchar(dictionary[[key]]))
    }
   }
  }
  writeLines(html,file.path(out,paste0(type,'-',lang,'.html')),useBytes=TRUE)
  cat('PASS:',type,lang,'toolbar translations; action IDs, control values, default selection and structure unchanged\n')
 }
}
