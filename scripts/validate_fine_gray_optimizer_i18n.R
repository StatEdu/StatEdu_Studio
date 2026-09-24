Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/fine-gray-optimizer-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv');idx<-which(d$status==1);d$status[idx[seq(1,length(idx),by=2)]]<-2
result<-prepare_competing_risk_result(d,'time','status',rate_times=c(100,250,500))
codes<-c(score='fine_gray_score_criterion_not_met',rank='fine_gray_rank_deficient_information',condition='fine_gray_ill_conditioned_information',covariance='fine_gray_invalid_covariance',se='fine_gray_invalid_covariance')
for(kind in c('normal',names(codes)))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 opt<-data.frame('Relative score criterion'=.01,'Convergence tolerance'=.02,'Information rank'=2L,Parameters=2L,'Standardized information condition number'=29.99,'Finite covariance matrix'=TRUE,'Positive standard errors'=TRUE,check.names=FALSE)
 if(kind=='score')opt[[1]]<-.02
 if(kind=='rank')opt[[3]]<-1L
 if(kind=='condition')opt[[5]]<-30
 if(kind=='covariance')opt[[6]]<-FALSE
 if(kind=='se')opt[[7]]<-FALSE
 r<-result;r$fine_gray<-list(optimizer_table=opt)
 rows<-survival_stability_review(r,language);rows<-rows[rows$Code%in%codes,,drop=FALSE]
 stopifnot(nrow(rows)==as.integer(kind!='normal'))
 if(nrow(rows)){
  stopifnot(rows$Code==codes[[kind]])
  html<-as.character(survival_simple_table(rows,table_language=language));doc<-xml2::read_html(html,encoding='UTF-8')
  evidence<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[3]')))
  if(kind=='score')stopifnot(grepl(survival_format_number(.02),evidence,fixed=TRUE))
  if(kind=='rank')stopifnot(endsWith(evidence,'1/2'))
  if(kind=='condition')stopifnot(endsWith(evidence,survival_format_number(30)))
  if(language%in%c('ja','zh','es','fr','de','vi'))stopifnot(!grepl('Fine-Gray relative score criterion|Fine-Gray information rank|Standardized Fine-Gray information condition number|The Fine-Gray covariance matrix|Do not report the estimates|Review information-matrix|Review standardized information|Do not report inferential',xml2::xml_text(doc)))
  if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=html)
 }
 main<-as.character(survival_simple_table(survival_competing_rate_table(result),table_role='main',table_language='en'))
 if(language=='en')baseline<-main else stopifnot(identical(main,baseline))
 cat('PASS:',kind,language,'optimizer boundaries, values and English CIF table\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
