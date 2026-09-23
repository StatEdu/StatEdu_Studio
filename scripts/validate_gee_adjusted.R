Sys.setlocale('LC_ALL', 'Korean_Korea.utf8')
source('R/app_bootstrap.R', encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(1937)
d <- data.frame(id=rep(1:120,each=3),time=rep(1:3,120),x=rnorm(360))
d$y <- 1 + .5*d$x + rep(rnorm(120),each=3) + rnorm(360)
fit <- longitudinal_fit_model(d,'y','id','time','x','gee','gaussian','unstructured_adjusted')
stopifnot(nrow(fit$coef_table)==2L,all(is.finite(fit$coef_table$p)))
stopifnot(isTRUE(all.equal(predict(fit$model,type='response'),fitted(fit$model))))
stopifnot(isTRUE(all.equal(unname(diag(vcov(fit$model))),fit$coef_table$SE^2)))
# Outcome scaling preserves Wald tests and scales both coefficients and robust SEs.
d2 <- d; d2$y <- 7*d$y
fit2 <- longitudinal_fit_model(d2,'y','id','time','x','gee','gaussian','unstructured_adjusted')
stopifnot(max(abs(fit2$coef_table$B-7*fit$coef_table$B))<1e-8,
 max(abs(fit2$coef_table$SE-7*fit$coef_table$SE))<1e-8,
 max(abs(fit2$coef_table$p-fit$coef_table$p))<1e-8)
# Changing predictor units/origin must preserve fitted values and transformed covariance.
d3 <- d; d3$x <- 2000 + d$x / 1000
fit3 <- longitudinal_fit_model(d3,'y','id','time','x','gee','gaussian','unstructured_adjusted')
back <- matrix(c(1,0,2000,0.001),2,2)
stopifnot(max(abs(drop(back %*% coef(fit3$model))-coef(fit$model)))<1e-6,
 max(abs(back %*% vcov(fit3$model) %*% t(back)-vcov(fit$model)))<1e-5,
 max(abs(fitted(fit3$model)-fitted(fit$model)))<1e-6)
restored <- unserialize(serialize(fit,NULL))
stopifnot(identical(summary(restored$model),summary(fit$model)),is.na(AIC(fit$model)))
err <- tryCatch(longitudinal_fit_model(d,'y','id','time','x','gee','gaussian','unstructured_adjusted',weights=rep(1,nrow(d))),error=identity)
stopifnot(inherits(err,'error'),grepl('weights',conditionMessage(err)))
state <- longitudinal_setup_state(names(d),data.frame(name=names(d),label=names(d)),corstr='unstructured_adjusted',language='ko')
stopifnot(state$corstr=='unstructured_adjusted')
panel <- as.character(longitudinal_setup_panel(state, NULL))
stopifnot(grepl('value="unstructured_adjusted" selected',panel,fixed=TRUE))
stopifnot('unstructured_adjusted' %in% unname(longitudinal_correlation_choices()))
result <- prepare_longitudinal_analysis_result(d,'y','id','time',predictors='x',family='gaussian',corstr='unstructured_adjusted',assumption_checks=FALSE)
stopifnot(result$corstr=='unstructured_adjusted')
stopifnot(any(grepl('ADJUSTCORR=YES',unlist(result),fixed=TRUE)))
cat('Adjusted GEE fit, robust inference, scaling, serialization, settings, and result integration passed.\n')
# Gamma input diagnostics must identify invalid outcomes before fitting.
gamma_bad <- d
gamma_bad$y <- abs(gamma_bad$y) + 1
gamma_bad$y[c(1, 2)] <- 0
gamma_error <- tryCatch(longitudinal_fit_model(gamma_bad, 'y', 'id', 'time', 'x', 'gee', 'gamma', 'unstructured_adjusted'), error = identity)
stopifnot(inherits(gamma_error, 'error'), grepl('2 analyzed rows', conditionMessage(gamma_error), fixed = TRUE), grepl('strictly positive', conditionMessage(gamma_error), fixed = TRUE))
cat('Gamma input diagnostic validation passed.\n')
