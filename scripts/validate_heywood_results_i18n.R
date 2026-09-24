Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
data <- lavaan::HolzingerSwineford1939;names(data)[names(data)=='x1'] <- 'Normality'
fit <- suppressWarnings(lavaan::cfa('Review =~ Normality + x2 + x3 + x4\nNormality ~~ -0.01*Normality',data=data))
stopifnot(lavaan::lavInspect(fit,'theta')['Normality','Normality']<0)
base <- list(fit=fit,estimator='ML',diagnostics=list(negative_residuals='Normality'))
cases <- list(residual=base,disabled=modifyList(base,list(estimator='WLSMV')))
for(state in c('non_psd','near_singular','ill_conditioned')) {
 d <- list(negative_residuals='Normality',negative_latent_variances='Review',theta_min_eigenvalue=-.01,
  latent_min_eigenvalue=.001,parameter_min_eigenvalue=1e-12,theta_condition_number=Inf,
  latent_condition_number=1e9,parameter_condition_number=1e12)
 for(matrix in c('theta','latent_covariance','parameter_covariance'))d[[paste0(state,'_',matrix)]] <- TRUE
 cases[[state]] <- modifyList(base,list(diagnostics=d))
}
cases$latent_only <- modifyList(base,list(diagnostics=list(negative_residuals=character(),negative_latent_variances='Review')))
cases$matrix_only <- cases$ill_conditioned;cases$matrix_only$diagnostics$negative_residuals <- character()
cases$matrix_only$diagnostics$negative_latent_variances <- character()
out <- 'tmp/heywood-results-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
entries <- list();english <- list();numeric_cells <- list()
for(state in names(cases))for(language in c('en','ko','ja','zh','es','fr','de','vi')) {
 html <- as.character(structural_canvas_heywood_result_ui(cases[[state]],data,'test','cfa',language))
 doc <- xml2::read_html(html,encoding='UTF-8')
 text <- xml2::xml_text(doc);cells <- trimws(xml2::xml_text(xml2::xml_find_all(doc,'//td')))
 if(length(cases[[state]]$diagnostics$negative_residuals))stopifnot('Normality' %in% cells,'Review' %in% cells)
 if(length(cases[[state]]$diagnostics$negative_latent_variances))stopifnot('Review' %in% cells)
 notes <- xml2::xml_text(xml2::xml_find_all(doc,'//p|//h4|//h5'))
 values <- cells[grepl('^[-+.0-9]|^Inf$|^—$',cells)]
 if(language=='en')numeric_cells[[state]] <- values else stopifnot(identical(values,numeric_cells[[state]]))
 if(language=='en')english[[state]] <- notes else stopifnot(all(notes!=english[[state]]))
 if(language!='en')stopifnot(!any(grepl('Residual variance|Latent factor|Minimum eigenvalue|Condition number|Not positive semidefinite|Ill-conditioned|Unreliable standard errors',xml2::xml_text(xml2::xml_find_all(doc,'//th|//td')))))
 buttons <- xml2::xml_find_all(doc,"//button[@id='test_heywood_refit']")
 stopifnot(length(buttons)==as.integer(state %in% c('residual','non_psd','near_singular','ill_conditioned')))
 if(language=='ja')entries[[state]] <- list(id=state,title=state,html=html)
 cat('PASS:',language,state,'labels, conditional notes, names and refit availability\n')
}
stopifnot(is.null(structural_canvas_heywood_result_ui(base,data,'test','plssem','ja')),
 is.null(structural_canvas_heywood_result_ui(list(diagnostics=list()),data,'test','cfa','ja')))
saveRDS(unname(entries),file.path(out,'entries.rds'))
