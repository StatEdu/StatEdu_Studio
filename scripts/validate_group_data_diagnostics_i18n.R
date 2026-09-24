Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
options(statedu.output_decimal_digits=3L)
out<-'tmp/group-data-diagnostics-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
# Real group diagnostics exercise every sample-size status and absent ordinal levels.
data<-data.frame(g=rep(c('Normality','Primary','사용자 <&> %s'),c(35,90,210)),x=seq_len(335),check.names=FALSE)
data$x[c(1,16,79)]<-NA
ordinary<-rbind(structural_canvas_invariance_group_diagnostics(data.frame(g=rep('Review',12),x=1:12),'g','x'),structural_canvas_invariance_group_diagnostics(data,'g','x'))
stopifnot(length(unique(ordinary$Status))==4L)
ordinal_data<-data;ordinal_data[['사용자 범주 <&> %s']]<-factor(rep(c('Review','Normality'),length.out=nrow(data)),levels=c('Review','Normality','Primary'))
ordinal<-structural_canvas_invariance_group_diagnostics(ordinal_data,'g',c('x','사용자 범주 <&> %s'),'사용자 범주 <&> %s')
stopifnot(all(ordinal$Status=='Ordered category absent'))
micom<-ordinary[,1:4];micom[['N warning']]<-c('Small group (N < 30); permutation estimates may be unstable','None','No group-size flag','Review')
custom<-ordinary;custom$Status[1]<-'Normality 사용자 <&> %s';custom[['Absent ordered categories']][1]<-'Review={Normality, Primary}'
for(kind in c('ordinary','ordinal','micom','custom','empty'))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 table<-switch(kind,ordinary=ordinary,ordinal=ordinal,micom=micom,custom=custom,empty=ordinary[FALSE,])
 type<-if(kind=='micom')'pls_micom'else'measurement_invariance'
 bundle<-list(invariance_result=list(type=type,group_diagnostics=table))
 ui<-structural_canvas_invariance_appendix_ui(bundle,language)
 if(kind=='empty'){stopifnot(is.null(ui));next}
 html<-as.character(ui);doc<-xml2::read_html(html,encoding='UTF-8');headers<-xml2::xml_text(xml2::xml_find_all(doc,'//th'))
 if(language!='en')stopifnot(!any(c('N warning','Minimum category count','Absent ordered categories')%in%headers))
 for(j in seq_along(table)){
  cells<-trimws(xml2::xml_text(xml2::xml_find_all(doc,paste0('//tbody/tr/td[',j,']'))))
  if(names(table)[j]=='Group')stopifnot(identical(cells,table[[j]]))
  if(is.numeric(table[[j]])){
   expected<-if(names(table)[j]=='Indicator missing %')paste0(vapply(table[[j]],format_decimal3,character(1)),'%')else ifelse(is.finite(table[[j]]),formatC(table[[j]],format='f',digits=0),'')
   stopifnot(identical(cells,expected))
  }
  if(names(table)[j]=='Absent ordered categories'){
   authored<-table[[j]]!='None';stopifnot(identical(cells[authored],table[[j]][authored]))
  }
  if(names(table)[j]%in%c('Status','N warning')){
   custom_rows<-grepl('Normality 사용자|^Review$',table[[j]])
   stopifnot(identical(cells[custom_rows],table[[j]][custom_rows]))
   if(language=='en')stopifnot(identical(cells,table[[j]]))else stopifnot(all(cells[!custom_rows]!=table[[j]][!custom_rows]))
  }
 }
 if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=html)
 cat('PASS:',kind,language,'group names, absent categories, exact counts/percentages and translated warnings\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
