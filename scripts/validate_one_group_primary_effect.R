Sys.setlocale('LC_CTYPE','English_United States.utf8')
source('scripts/validate_one_group_options_i18n.R',encoding='UTF-8')
for(fit in fits){
 rows<-one_group_rm_primary_rows(fit$anova);stopifnot(length(rows)==1L)
 expected<-fit$anova$p[rows];rec<-fit$recommendation
 stopifnot(rec$Recommendation[rec$Item=='Primary effect']=='Treatment x Time')
 p<-mixed_rm_parse_p(expected)
 stopifnot(rec$Reason[rec$Item=='Follow-up decision']==paste0('Primary p=',if(p<.001)'<.001' else format_decimal3(p),'.'))
}
# A misleading covariate label and opposite significance decisions must not select the first display row.
fake<-data.frame(Effect=c('Treatment x Time','Treatment x Time'),p=c('.001','.800'))
attr(fake,'one_group_effect_keys')<-c('.cov1:Time','Treatment:Time')
rec<-one_group_rm_recommendation_table(fake,data.frame(),data.frame(),0)
stopifnot(rec$Reason[rec$Item=='Follow-up decision']==paste0('Primary p=',format_decimal3(.8),'.'),grepl('Do not treat',rec$Recommendation[rec$Item=='Follow-up decision'],fixed=TRUE))
fake$p<-rev(fake$p);rec<-one_group_rm_recommendation_table(fake,data.frame(),data.frame(),0)
stopifnot(rec$Reason[rec$Item=='Follow-up decision']==paste0('Primary p=',format_decimal3(.001),'.'),grepl('Prioritize',rec$Recommendation[rec$Item=='Follow-up decision'],fixed=TRUE))
missing<-data.frame(Effect='Covariate x Time',p='.001')
rec<-one_group_rm_recommendation_table(missing,data.frame(),data.frame(),0)
stopifnot(rec$Reason[rec$Item=='Follow-up decision']=='Primary p was not available.')
fit<-fits$wide_multiple;out<-'tmp/one-group-primary-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language);html<-as.character(one_group_rm_anova_results_ui(fit));doc<-xml2::read_html(html,encoding='UTF-8')
 cells<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']//td"))
 time<-statedu_t('analysis.one_group.time',language);treatment<-statedu_t('analysis.one_group.treatment',language)
 stopifnot(paste('Normality',time,sep=' x ') %in% cells,paste('사용자 공변량',treatment,time,sep=' x ') %in% cells)
 if(language=='ja')saveRDS(list(list(id='one-group-primary',title='Within-subject repeated measures',html=html)),file.path(out,'entries.rds'))
}
cat('PASS: actual six paths use Treatment:Time p; colliding labels and opposite p decisions; 8-language diagnostic effect names\n')
