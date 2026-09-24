# Reuse the actual fitted ITT fixture and its six-message regression checks.
source('scripts/validate_mixed_rm_itt_guidance_i18n.R',encoding='UTF-8')
phrases<-c('At least three complete cases are required for mixed repeated-measures ANOVA.','Use the fitted LMM as the ITT result.','The repeated outcome is treated as continuous Gaussian; mixed modeling handles unbalanced repeated records.','Covariate-by-time terms were included because covariates were selected.')
stopifnot(all(vapply(phrases,function(p)any(grepl(p,c(unlist(r$recommendation),unlist(r$assumption),unlist(r$mixed_model_overview)),fixed=TRUE)),logical(1))))
before<-serialize(r,NULL);captured<-list();failures<-character()
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 options(statedu.app_language=lang)
 expected<-vapply(phrases,function(p)statedu_localized_text(lang,p,mixed_rm_appendix_korean_text(p)),character(1))
 if(lang!='en'&&any(expected==phrases))failures<-c(failures,paste(lang,'catalog'))
 html<-as.character(mixed_rm_anova_results_ui(r));doc<-xml2::read_html(html)
 cells<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']//td"))
 if(!all(expected[1:2]%in%cells)||!paste(expected[3:4],collapse=' ')%in%cells)failures<-c(failures,paste(lang,'render'))
 main<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']"));stopifnot(length(main)>0)
 if(lang=='en')baseline<-main else stopifnot(identical(main,baseline))
 stopifnot(identical(before,serialize(r,NULL)))
 probe<-mixed_rm_appendix_table(data.frame(Variable=phrases,Reason=phrases));stopifnot(identical(probe[[1]],phrases))
 captured[[lang]]<-html
}
if(length(failures))stop(paste('Remaining ITT failures:',paste(failures,collapse=', ')))
out<-'tmp/mixed-rm-itt-remaining-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
jsonlite::write_json(captured,file.path(out,'captured.json'),auto_unbox=TRUE)
cat('PASS actual ITT minimum-case warning, fitted LMM guidance and combined Gaussian/covariate reason across eight languages\n')
