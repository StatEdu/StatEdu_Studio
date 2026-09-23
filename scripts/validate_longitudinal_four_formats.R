Sys.setlocale('LC_ALL','Korean_Korea.utf8')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules();options(statedu.app_language='ko')
out<-'outputs/spss_phase38_20260907';dir.create(out,showWarnings=FALSE)
set.seed(20260618)
subject_id <- rep(seq_len(40), each = 4)
site_id <- rep(rep(seq_len(8), each = 5), each = 4)
time <- rep(0:3, times = 40)
x <- stats::rnorm(length(subject_id))
group <- rep(sample(c("A", "B"), 40, replace = TRUE), each = 4)
random_intercept <- rep(stats::rnorm(40, 0, 0.8), each = 4)
continuous_y <- 2 + 0.4 * time + 0.7 * x + ifelse(group == "B", 0.5, 0) + random_intercept + stats::rnorm(length(subject_id))
integer_score_y <- pmin(5, pmax(1, round(continuous_y)))
binary_y <- stats::rbinom(length(subject_id), 1, stats::plogis(-1 + 0.2 * time + 0.5 * x + random_intercept * 0.4))
count_mu <- exp(1 + 0.12 * time + 0.25 * x + ifelse(group == "B", 0.2, 0) + random_intercept * 0.25)
count_y <- stats::rnbinom(length(subject_id), mu = count_mu, size = 1.6)
gamma_mu <- exp(0.6 + 0.08 * time + 0.18 * x + ifelse(group == "B", 0.15, 0) + random_intercept * 0.2)
gamma_y <- stats::rgamma(length(subject_id), shape = 3, rate = 3 / gamma_mu)
person_time <- runif(length(subject_id), 0.6, 1.8)

data <- data.frame(
  subject_id = subject_id,
  site_id = site_id,
  time = time,
  continuous_y = continuous_y,
  integer_score_y = integer_score_y,
  binary_y = factor(binary_y),
  count_y = count_y,
  gamma_y = gamma_y,
  person_time = person_time,
  x = x,
  aux_history = stats::rnorm(length(subject_id)),
  group = group,
  long_weight = runif(length(subject_id), 0.45, 2.2),
  stringsAsFactors = FALSE
)

missing_data <- data
missing_data$continuous_y[c(5, 18, 47, 92)] <- NA_real_
missing_data$x[c(11, 38, 119)] <- NA_real_

variable_info <- data.frame(
  name = names(data),
  var_label = names(data),
  role = "",
  measurement = c("category", "category", "continuous", "continuous", "continuous", "binary", "continuous", "continuous", "continuous", "continuous", "continuous", "category", "continuous"),
  stringsAsFactors = FALSE
)

for(i in 1:24){data[[paste0('z',i)]]<-rnorm(nrow(data))}
variable_info<-rbind(variable_info,data.frame(name=paste0('z',1:24),var_label=paste0('z',1:24),role='',measurement='continuous'))
make<-function(model='gee',y='continuous_y',data_input=data,extra=list()){
 r<-do.call(prepare_longitudinal_analysis_result,modifyList(list(data=data_input,outcome=y,id='subject_id',time='time',predictors='x',covariates='group',model_type=model,family='auto',variable_info=variable_info),extra))
 stopifnot(length(r)==1L,nrow(r[[1]]$coef_table)>0);r
}
cases<-list(gee=function()make(),gee_ar1_count=function()make(y='count_y',extra=list(family='count',corstr='ar1',exposure='person_time')),lmm_slope=function()make('lmm',extra=list(random_slope=TRUE)),lmm_cluster=function()make('lmm',extra=list(cluster='site_id')),glmm_binary=function()make('glmm','binary_y',extra=list(family='binomial')),panel_fe=function()make('panel_fe'),panel_re=function()make('panel_re'),missing_mi_ipw=function()make('lmm',data_input=missing_data,extra=list(missing_method='available',missing_strategies=c('mi','ipw'),missing_imputations=3L,missing_iterations=3L,ipw_auxiliary='aux_history')),long_predictors=function()make('lmm',extra=list(predictors=c('x',paste0('z',1:24)))))
for(name in names(cases)) {
 r<-cases[[name]]();folder<-file.path(out,name);dir.create(folder,showWarnings=FALSE)
 write_longitudinal_results_html(r,file.path(folder,'result.html'))
 html<-paste(readLines(file.path(folder,'result.html'),encoding='UTF-8'),collapse='\n');b<-xml2::read_html(html)
 a<-xml2::read_html(as.character(htmltools::renderTags(longitudinal_results_panel(r))$html))
 cells<-function(doc)vapply(xml2::xml_find_all(doc,'.//table//th|.//table//td'),result_html_text,character(1));stopifnot(identical(cells(a),cells(b)))
 e<-list(title='Longitudinal',html=html,saved_at='2026-09-07')
 write_longitudinal_results_pdf(r,file.path(folder,'result.pdf'));save_longitudinal_excel_file(r,file.path(folder,'result.xlsx'));write_result_collection_docx(list(e),file.path(folder,'result.docx'))
  tables<-result_entry_tables(e)
  image_items<-result_entry_images(e)
  orders<-vapply(c(tables,image_items),`[[`,numeric(1),'output_order')
  sheet_indices<-rank(orders,ties.method='first')[seq_along(tables)]
  unlink(vapply(image_items,`[[`,character(1),'path'))
 expected<-list(tables=lapply(tables,function(t)list(title=t$title,orientation=t$orientation,notes=t$notes,cells=lapply(t$screen$cells,function(c)c(c,list(value=t$screen$values[c$row,c$col]))))),images=as.list(xml2::xml_attr(xml2::xml_find_all(b,'.//img'),'alt')))
  for(i in seq_along(expected$tables))expected$tables[[i]]$sheet_index<-unname(sheet_indices[i])
  jsonlite::write_json(expected,file.path(folder,'expected.json'),auto_unbox=TRUE);saveRDS(r,file.path(folder,'analysis.rds'))
 cat(name,length(tables),'tables',length(expected$images),'images\n')
}
