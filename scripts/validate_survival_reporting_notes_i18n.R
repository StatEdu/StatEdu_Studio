Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/survival-reporting-notes-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv')
results<-list(km=prepare_km_single_analysis_result(d,'time','status'),cox=prepare_cox_analysis_result(d,'time','status',c('age','sex'),event_value='1'))
idx<-which(d$status==1);d$status[idx[seq(1,length(idx),by=2)]]<-2
results$competing<-prepare_competing_risk_result(d,'time','status',covariates=c('age','sex'),regression='both',rate_times=c(100,250,500))
en<-c('Reporting checklist and interpretation guide','Thresholds are heuristic screening rules, not automatic analysis-quality pass/fail criteria.','Median potential follow-up uses reverse Kaplan–Meier with censoring as the event. A high censoring proportion is not itself evidence of bias; the censoring mechanism requires separate review.','This checklist helps prevent reporting omissions; it does not automatically establish design validity or causality.')
ko<-c('보고 체크리스트와 해석 가이드','임계값은 선별용 경험 규칙이며 분석 품질의 자동 합격·불합격 기준이 아닙니다.','잠재 추적기간 중앙값은 검열을 사건으로 둔 역 Kaplan–Meier 방법으로 추정합니다. 높은 검열률 자체는 편향의 증거가 아니며 검열기전의 타당성을 별도로 검토해야 합니다.','체크리스트는 보고 누락을 줄이기 위한 보조도구이며 연구 설계의 타당성이나 인과성을 자동 판정하지 않습니다.')
for(kind in names(results))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language);r<-results[[kind]]
 html<-as.character(survival_reporting_guidance_panel(r,language));doc<-xml2::read_html(html,encoding='UTF-8')
 displayed<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//h3 | //div[contains(@class,"result-note")]')))
 expected<-vapply(seq_along(en),function(i)statedu_localized_text(language,en[i],ko[i]),character(1))
 if(!language%in%c('en','ko'))stopifnot(!any(expected%in%en),!any(expected%in%ko))
 expected[-1]<-vapply(expected[-1],function(s)xml2::xml_text(xml2::xml_find_first(xml2::read_html(as.character(result_note_div(class='result-note',s)),encoding='UTF-8'),'//div')),character(1))
 stopifnot(identical(displayed,expected))
 # Routing prose must not change the four tables produced by existing helpers.
 tables<-list(survival_stability_review(r,language),survival_followup_diagnostics(r),survival_reporting_checklist(r,language),survival_interpretation_guide(r,language))
 expected_tables<-vapply(tables,function(t)as.character(survival_simple_table(t,table_language=language)),character(1))
 cell_text<-function(x)xml2::xml_text(xml2::xml_find_all(xml2::read_html(x,encoding='UTF-8'),'//th|//td'))
 stopifnot(identical(cell_text(html),cell_text(paste(expected_tables,collapse=''))))
 if(kind!='km'){
  main<-if(kind=='cox')as.character(survival_cox_result_html_table(r,language)) else as.character(survival_simple_table(survival_cause_specific_coef_table(r),table_role='main',table_language='en'))
  cells<-cell_text(main);if(language=='en')baseline<-cells else stopifnot(identical(cells,baseline))
 }
 if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=html)
 cat('PASS:',kind,language,'actual analysis, localized title and notes, unchanged table content\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
