Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/fine-gray-optimizer-table-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv');idx<-which(d$status==1);d$status[idx[seq(1,length(idx),by=2)]]<-2
result<-prepare_competing_risk_result(d,'time','status',covariates=c('age','sex'),regression='fine_gray',rate_times=c(100,250,500))
stopifnot(nrow(result$fine_gray$optimizer_table)==1)
for(kind in c('actual','false'))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 r<-result;flags<-c('Converged','Finite covariance matrix','Positive standard errors')
 if(kind=='false')r$fine_gray$optimizer_table[flags]<-FALSE
 raw<-r$fine_gray$optimizer_table;rows<-survival_fine_gray_optimizer_table(r)
 html<-as.character(tagList(tags$h4(survival_appendix_title('Fine-Gray numerical stability',language)),survival_simple_table(rows,table_language=language)))
 doc<-xml2::read_html(html,encoding='UTF-8');cells<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//td')))
 flagidx<-match(flags,names(rows));numericidx<-setdiff(seq_along(rows),flagidx)
 if(language=='en')baseline<-cells[numericidx]else{
  stopifnot(identical(cells[numericidx],baseline),!any(names(rows)[-1]%in%xml2::xml_text(xml2::xml_find_all(doc,'//th'))))
  stopifnot(!grepl('Fine-Gray numerical stability',xml2::xml_text(doc),fixed=TRUE))
 }
 expected<-vapply(flags,function(f)statedu_localized_text(language,if(raw[[f]])'Yes' else 'No',if(raw[[f]])'예' else '아니요'),character(1))
 if(language=='ko')expected[expected=='아니요']<-'아니오'
 stopifnot(identical(unname(cells[flagidx]),unname(expected)))
 main<-as.character(survival_simple_table(survival_fine_gray_coef_table(result),table_role='main',table_language='en'))
 if(language=='en')main_baseline<-main else stopifnot(identical(main,main_baseline))
 if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=html)
 cat('PASS:',kind,language,'actual Fine-Gray optimizer headers, flags, numeric values and English coefficient table\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
