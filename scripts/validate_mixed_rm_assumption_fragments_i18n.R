source('scripts/validate_mixed_rm_alternative_guidance_i18n.R',encoding='UTF-8')
phrases<-c('Levene: satisfied','Sphericity assumed')
stopifnot(all(vapply(phrases,function(p)any(vapply(results,function(r)any(grepl(p,r$recommendation$Reason,fixed=TRUE)),logical(1))),logical(1))))
before<-serialize(results,NULL);captured<-list();failures<-character()
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 options(statedu.app_language=lang);panels<-list();main<-list()
 expected<-vapply(phrases,function(p)statedu_localized_text(lang,p,if(p=='Sphericity assumed')'구형성 가정'else'Levene: 충족'),character(1))
 if(lang!='en'&&any(expected==phrases))failures<-c(failures,paste(lang,'catalog'))
 for(i in seq_along(results)) {
  panels[[i]]<-as.character(mixed_rm_anova_results_ui(results[[i]]));doc<-xml2::read_html(panels[[i]])
  cells<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']//td"))
  raw<-results[[i]]$recommendation$Reason
  for(j in seq_along(phrases))if(any(grepl(phrases[j],raw,fixed=TRUE))&&!any(grepl(expected[j],cells,fixed=TRUE)))failures<-c(failures,paste(lang,i,j,'render'))
  if(lang!='en'&&any(grepl('Levene: satisfied|assumed',cells)))failures<-c(failures,paste(lang,i,'English residue'))
  main[[i]]<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']"));stopifnot(length(main[[i]])>0)
 }
 if(lang=='en')baseline<-main else stopifnot(identical(main,baseline))
 stopifnot(identical(before,serialize(results,NULL)))
 probe<-mixed_rm_appendix_table(data.frame(Variable=phrases,Reason=phrases));stopifnot(identical(probe[[1]],phrases))
 captured[[lang]]<-paste(unlist(panels),collapse='\n')
}
if(length(failures))stop(paste('Assumption fragment failures:',paste(failures,collapse=', ')))
out<-'tmp/mixed-rm-assumption-fragments-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
jsonlite::write_json(captured,file.path(out,'captured.json'),auto_unbox=TRUE)
cat('PASS actual Levene satisfied and sphericity assumed combined guidance in eight languages; main/source/user labels preserved\n')
