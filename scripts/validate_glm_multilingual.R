Sys.setlocale("LC_CTYPE","English_United States.utf8")
Sys.setenv(STATEDU_MODULE_CACHE="false")
source("R/app_bootstrap.R",encoding="UTF-8")
load_app_packages(check=FALSE); source_app_modules()
out <- "tmp/glm-multilingual"
dir.create(out,recursive=TRUE,showWarnings=FALSE)
set.seed(916)
d <- data.frame(x=rnorm(120),y=rnorm(120),binary=rbinom(120,1,.45),count=rpois(120,3))
info <- data.frame(name=names(d),measurement=c("continuous","continuous","binary","continuous"),var_label=c("Model-based","사용자 결과","사용자 이분형","사용자 계수"))
options(statedu.app_language="en")
fits <- list(gaussian=prepare_generalized_analysis_result(d,"y","x",family="gaussian",variable_info=info),
  binomial=prepare_generalized_analysis_result(d,"binary","x",family="binomial",variable_info=info),
  count=prepare_generalized_analysis_result(d,"count","x",family="count",variable_info=info))
ipw_data <- d
ipw_data$y[seq(3,120,7)] <- NA_real_
ipw_data[["Model-based"]] <- seq_len(nrow(d))/nrow(d)
ipw_info <- rbind(info,data.frame(name="Model-based",measurement="continuous",var_label="사용자 보조변수"))
fits$ipw <- prepare_generalized_analysis_result(ipw_data,"y","x",family="gaussian",missing_strategy="ipw",ipw_auxiliary="Model-based",variable_info=ipw_info)
analyzed_ipw <- ipw_data[complete.cases(ipw_data[,c("x","y")]),,drop=FALSE]
quoted_ipw <- generalized_ipw_weights(ipw_data,analyzed_ipw,c("x","y"),"x","Model-based")
simple_ipw <- ipw_data
names(simple_ipw)[names(simple_ipw)=="Model-based"] <- "auxiliary"
simple_weights <- generalized_ipw_weights(simple_ipw,simple_ipw[rownames(analyzed_ipw),,drop=FALSE],c("x","y"),"x","auxiliary")
stopifnot(isTRUE(all.equal(quoted_ipw$weights,simple_weights$weights)),startsWith(quoted_ipw$note,"Observation model:"))
sources <- jsonlite::read_json("scripts/fixtures/glm_i18n_labels.json",simplifyVector=TRUE)
sources <- c(sources,jsonlite::read_json("scripts/fixtures/glm_i18n_diagnostics.json",simplifyVector=TRUE))
sources <- c(sources,jsonlite::read_json("scripts/fixtures/glm_i18n_risk_guidance.json",simplifyVector=TRUE))
sources <- c(sources,jsonlite::read_json("scripts/fixtures/glm_i18n_missing_coding.json",simplifyVector=TRUE))
sources <- c(sources,jsonlite::read_json("scripts/fixtures/glm_i18n_selection.json",simplifyVector=TRUE))
sources <- c(sources,jsonlite::read_json("scripts/fixtures/glm_i18n_reporting.json",simplifyVector=TRUE))
sources <- c(sources,jsonlite::read_json("scripts/fixtures/glm_i18n_ipw_labels.json",simplifyVector=TRUE))
checklist_sources <- jsonlite::read_json("scripts/fixtures/glm_i18n_checklist_labels.json",simplifyVector=TRUE)
count_sources <- jsonlite::read_json("scripts/fixtures/glm_i18n_count_labels.json",simplifyVector=TRUE)
count_sources <- c(count_sources,jsonlite::read_json("scripts/fixtures/glm_i18n_count_decisions.json",simplifyVector=TRUE))
sources <- c(sources,count_sources)
sources <- c(sources,checklist_sources)
assumption_sources <- jsonlite::read_json("scripts/fixtures/glm_i18n_assumptions.json",simplifyVector=TRUE)
check_labels <- assumption_sources[1:9]
flag_checks <- data.frame(Check=check_labels,Result=rep(c("Flag","Review"),length.out=9))
assumption_model <- list(assumption_checks=flag_checks)
assumption_sentences <- c(generalized_manuscript_text(assumption_model)$SuggestedText[[3]],
 generalized_flag_summary(flag_checks),generalized_flag_summary(data.frame(Check=check_labels,Result="OK")),
 "9 assumption check item(s) reported.",
 generalized_manuscript_text(list(assumption_checks=data.frame(Check=check_labels,Result="OK")))$SuggestedText[[3]],
 generalized_manuscript_text(list())$SuggestedText[[3]])
sources <- c(sources,assumption_sources,assumption_sentences)
mi_suffixes <- jsonlite::read_json("scripts/fixtures/glm_i18n_mi_pooling.json",simplifyVector=TRUE)
sources <- c(sources,mi_suffixes)
mi_prefix <- "Standard mice-based multiple imputation used 23 fitted dataset(s); coefficients were pooled using Rubin total variance, Barnard-Rubin degrees of freedom, and t-based confidence intervals. "
mi_sources <- c(paste0(mi_prefix,mi_suffixes),
 "The gaussian family with identity link was held fixed across all imputed datasets.",
 "The count family was selected once before pooling: median Poisson dispersion across 23 imputations = 1.234; threshold = 1.5; locked family = negative_binomial.",
 "Multiple imputation (MI); analyzed rows after imputation: 117 of 120; complete-case rows before MI: 89 of 120.",
 "Inverse probability weighting (IPW); weighted complete-case rows: 89 of 120.",
 "Complete-case: row-wise; complete cases used: 89 of 120.")
sources <- c(sources,mi_sources)
selection_generated <- c(
 generalized_family_selection_reason("auto","binomial","binomial"),
 generalized_family_selection_reason("auto","count","negative_binomial"),
 generalized_family_selection_reason("auto","count","count"),
 generalized_family_selection_reason("auto","gamma","gamma"),
 generalized_family_selection_reason("auto","gaussian","gaussian"),
 generalized_recommendation_text("negative_binomial",NULL,NULL),
 generalized_recommendation_text("count",NULL,data.frame(Item="Zero screen",Value="excess zeros")),
 generalized_recommendation_text("count",NULL,NULL),
 generalized_recommendation_text("gaussian",data.frame(Check="Residual diagnostics",Result="Flag"),NULL),
 generalized_recommendation_text("gaussian",NULL,NULL))
stopifnot(identical(unname(selection_generated),jsonlite::read_json("scripts/fixtures/glm_i18n_selection.json",simplifyVector=TRUE)))
dynamic_sources <- c(
  "Pearson dispersion ratio = 1.234.",
  "Maximum model-matrix VIF = 2.345.",
  "EPV = 12.34 using the smaller outcome class and 7 non-intercept coefficient(s).",
  "Max Cook's D = 0.123; high Cook's D count = 4; max leverage = 0.456; high leverage count = 8.",
  "Binary outcome coded as event = 사용자.A 50%, reference = Model-based.",
  "Factor with treatment contrasts; reference = 사용자.B, Model-based 25%.",
  "Standard mice-based multiple imputation was selected for GLM missing-data handling (m = 23, iterations = 17).")
dynamic_values <- list("1.234", "2.345", c("12.34", "7"), c("0.123", "4", "0.456", "8"),
  c("사용자.A 50%", "Model-based"), "사용자.B, Model-based 25%", c("23", "17"))
sources <- c(sources,dynamic_sources)
ipw_sources <- c(
 "Complete-case GLM excluded 31 of 120 rows with missing values in selected analysis variables.",
 "Complete-case rows before missing-data engine: 89 of 120.",
 "Observation model: 사용자.A 50% + Model-based; IPW clipped at the 99th percentile and normalized to mean 1. Report the observation model and review positivity/weight stability.",
 "Observation model failed (사용자 오류 (v1.2.3): Model-based 50%); intercept-only IPW was used. Treat this as a weak IPW sensitivity analysis.")
pooling_notes <- jsonlite::read_json("scripts/fixtures/glm_i18n_pooling_notes.json",simplifyVector=TRUE)
sources <- c(sources,ipw_sources)
status_sources <- c(
 generalized_family_selection_reason("count","count","negative_binomial"),
 "Coefficient standard errors use HC3 sandwich robust covariance.",
 "Robust standard errors (HC2) were requested, but robust covariance could not be computed; model-based covariance is shown.",
 "Exposure offset applied as log(사용자.A (50%) Model-based).",
 "17 coding row(s) generated.")
sources <- c(sources,status_sources)
compound_sources <- c(
 "Complete-case: row-wise; analyzed 89 of 120 rows.",
 "Complete-case: row-wise; analyzed 89 of 120 row(s).",
 "Multiple imputation (MI); analyzed 117 of 120 rows; complete-case rows before missing-data engine: 89.",
 "Linear Gaussian / identity with identity link.",
 "Gamma / log with inverse link.")
sources <- c(sources,compound_sources)
family_sources <- c(
 "Fitted family: gaussian, link: identity.",
 "Fitted family: binomial, link: logit.",
 "Fitted family: negative_binomial, link: log.",
 "Outcome modeled using Gamma family.",
 "Outcome modeled using poisson family.",
 "Continuous outcome; identity link.",
 "Strictly positive continuous outcome; log link.",
 "Non-negative integer count outcome; log link.",
 "Non-negative integer count outcome; negative-binomial model with log link.",
 "Ordered factor; levels = Model-based, 사용자.A 50%, log. R ordered-factor contrasts are used unless the variable is recoded as numeric or nominal.")
sources <- c(sources,family_sources)
probe <- data.frame(Variable=sources,Item=sources)
manuscript_model <- list(coef_table=data.frame(Term=c("(Intercept)","사용자.A 50%","among other terms","Model-based","extra")),
 exponentiate=TRUE,count_details=data.frame(Item="probe"),software_versions=data.frame(Software=c("R","custom.pkg"),Version=c("4.5.3","1.2.3")))
manuscript <- generalized_manuscript_text(manuscript_model)
baseline <- NULL
for(language in c("en","ko","ja","zh","es","fr","de","vi")) {
 options(statedu.app_language=language)
 localized <- generalized_appendix_table(probe,language)
 ipw_details <- generalized_appendix_table(fits$ipw$missing_details,language)
 for(item in c("Selected auxiliary variables","Observation model variables")) {
   row <- match(item,fits$ipw$missing_details$Item)
   stopifnot(!is.na(row),identical(ipw_details[[2]][[row]],fits$ipw$missing_details$Value[[row]]))
 }
 diagnostics <- generalized_ipw_diagnostics(c(.4,.6),c(.8,1.2),model_terms="Intercept only")
 stopifnot(identical(generalized_appendix_table(diagnostics,language)[[2]][[1]],"Intercept only"))
 for(f in fits) {
   checklist <- generalized_reporting_checklist(f)
   translated_checklist <- generalized_appendix_table(checklist,language)
   if(language!="en") {
     stopifnot(!grepl("was fitted for dependent variable",translated_checklist[[3]][[1]],fixed=TRUE),
       grepl("Model-based",translated_checklist[[3]][[1]],fixed=TRUE))
     checklist$Details[[1]] <- "사용자 설명 v1.2.3"
     stopifnot(identical(generalized_appendix_table(checklist,language)[[3]][[1]],checklist$Details[[1]]))
   }
 }
 status_text <- vapply(status_sources,generalized_appendix_value_text,character(1),language=language)
 if(language!="en") stopifnot(all(status_text!=status_sources))
 stopifnot(grepl("HC3",status_text[[2]],fixed=TRUE),grepl("HC2",status_text[[3]],fixed=TRUE),
   grepl("사용자.A (50%) Model-based",status_text[[4]],fixed=TRUE),grepl("17",status_text[[5]],fixed=TRUE))
 if(language %in% c("ja","zh","es","fr","de","vi")) {
   for(label in c(generalized_family_label("count"),generalized_family_label("negative_binomial"))) {
     translated <- generalized_appendix_value_text(label,language)
     stopifnot(translated!=label,grepl(translated,status_text[[1]],fixed=TRUE))
   }
 }
 ipw_text <- vapply(ipw_sources,generalized_appendix_value_text,character(1),language=language)
 note_text <- vapply(pooling_notes,generalized_appendix_text,character(1),language=language)
 if(language!="en") stopifnot(all(ipw_text!=ipw_sources),all(note_text!=pooling_notes))
 stopifnot(grepl("사용자.A 50% + Model-based",ipw_text[[3]],fixed=TRUE),
   grepl("사용자 오류 (v1.2.3): Model-based 50%",ipw_text[[4]],fixed=TRUE))
 for(i in 1:2) for(n in c(if(i==1) "31" else "89","120")) stopifnot(grepl(n,ipw_text[[i]],fixed=TRUE))
 actual_manuscripts <- lapply(fits,generalized_manuscript_text)
 actual_methods <- lapply(actual_manuscripts,generalized_appendix_table,language=language)
 for(i in seq_along(actual_methods)) {
   methods_text <- actual_methods[[i]][[2]][[1]]
   stopifnot(grepl("Model-based",methods_text,fixed=TRUE))
   if(language!="en") {
     stopifnot(!grepl("was fitted for dependent variable",methods_text,fixed=TRUE),
       !grepl("Missing data were handled using",methods_text,fixed=TRUE),
       !grepl("Standard errors were reported as",methods_text,fixed=TRUE))
     edited_methods <- actual_manuscripts[[i]]
     edited_methods$SuggestedText[[1]] <- "사용자 Methods v1.2.3"
     stopifnot(identical(generalized_appendix_table(edited_methods,language)[[2]][[1]],edited_methods$SuggestedText[[1]]))
   }
 }
 assumption_text <- vapply(assumption_sentences,generalized_appendix_value_text,character(1),language=language)
 if(language!="en") {
   stopifnot(all(assumption_text!=assumption_sentences))
   for(label in check_labels) {
     translated <- generalized_appendix_value_text(label,language)
     stopifnot(translated!=label,grepl(translated,assumption_text[[1]],fixed=TRUE),grepl(translated,assumption_text[[2]],fixed=TRUE))
   }
 }
 stopifnot(grepl("9",assumption_text[[2]],fixed=TRUE),grepl("9",assumption_text[[3]],fixed=TRUE))
 manuscript_localized <- generalized_appendix_table(manuscript,language)
 results_text <- manuscript_localized[[2]][[2]]
 stopifnot(all(vapply(manuscript_model$coef_table$Term[2:4],function(x) grepl(x,results_text,fixed=TRUE),logical(1))))
 stopifnot(grepl("R 4.5.3, custom.pkg 1.2.3",manuscript_localized[[2]][[4]],fixed=TRUE))
 if(language!="en") {
   stopifnot(!grepl("Regression estimates were reported",results_text,fixed=TRUE))
   stopifnot(!grepl("Exponentiated coefficients were additionally",results_text,fixed=TRUE))
   stopifnot(!grepl("For count outcomes, Poisson versus",results_text,fixed=TRUE))
   edited <- manuscript
   edited$SuggestedText[[2]] <- "사용자 원고 v1.2.3"
   stopifnot(identical(generalized_appendix_table(edited,language)[[2]][[2]],edited$SuggestedText[[2]]))
 }
 mi_text <- vapply(mi_sources,generalized_appendix_value_text,character(1),language=language)
 if(language!="en") {
   stopifnot(all(mi_text!=mi_sources))
   for(i in seq_along(mi_suffixes)) stopifnot(endsWith(mi_text[[i]],generalized_appendix_value_text(mi_suffixes[[i]],language)))
 }
 for(i in seq_along(mi_sources)) {
   numbers <- regmatches(mi_sources[[i]],gregexpr("[0-9]+(?:\\.[0-9]+)?",mi_sources[[i]],perl=TRUE))[[1]]
   stopifnot(all(vapply(numbers,function(n) grepl(n,mi_text[[i]],fixed=TRUE),logical(1))))
 }
 stopifnot(identical(localized[[1]],sources),is.na(generalized_appendix_text(NA_character_,language)),is.na(generalized_appendix_value_text(NA_character_,language)))
 if(language!="en") stopifnot(all(localized[[2]][sources!="Gamma / log"]!=sources[sources!="Gamma / log"]))
 family_text <- vapply(family_sources,generalized_appendix_value_text,character(1),language=language)
 stopifnot(grepl("Model-based, 사용자.A 50%, log",tail(family_text,1),fixed=TRUE))
 if(language!="en") {
   stopifnot(all(family_text!=family_sources))
   stopifnot(!grepl("negative-binomial model with log",family_text[[9]],fixed=TRUE))
 }
 if(language %in% c("ja","zh","es","fr","de","vi")) {
   stopifnot(grepl(statedu_localized_text(language,"Identity link"),family_text[[1]],fixed=TRUE))
   stopifnot(grepl(statedu_localized_text(language,"Negative binomial distribution"),family_text[[3]],fixed=TRUE))
   stopifnot(grepl(statedu_localized_text(language,"Log link"),family_text[[9]],fixed=TRUE))
 }
 for(i in seq_along(dynamic_sources)) {
   value <- generalized_appendix_value_text(dynamic_sources[[i]],language)
   stopifnot(identical(value,generalized_appendix_text(dynamic_sources[[i]],language)))
   stopifnot(all(vapply(dynamic_values[[i]],function(x) grepl(x,value,fixed=TRUE),logical(1))))
   if(language!="en") stopifnot(value!=dynamic_sources[[i]])
 }
 # Multiline diagnostics and absent cells must survive the specialized path.
 multiline <- generalized_appendix_table(data.frame(Details=c(paste(dynamic_sources[1:2],collapse="\n"),NA_character_)),language)
 stopifnot(is.na(multiline[[1]][[2]]),grepl("1.234",multiline[[1]][[1]],fixed=TRUE),grepl("2.345",multiline[[1]][[1]],fixed=TRUE))
 for(choices in list(generalized_family_choices(),generalized_se_type_choices(),generalized_mi_outcome_choices(),generalized_link_choices("gaussian")))
   stopifnot(identical(unname(analysis_ui_choices(choices,language)),unname(choices)))
 state <- generalized_setup_state(names(d),"y",predictors="x",variable_table=info,missing_strategy="mi",language=language)
 setup <- xml2::read_html(as.character(htmltools::renderTags(generalized_setup_panel(state))$html))
 stopifnot(identical(xml2::xml_attr(xml2::xml_find_all(setup,"//select[@id='generalized_family']/option"),"value"),unname(generalized_family_choices())))
 if(language!="en") stopifnot(!any(xml2::xml_text(xml2::xml_find_all(setup,"//select[@id='generalized_mi_outcome']/option")) %in% names(generalized_mi_outcome_choices())))
 html <- as.character(htmltools::renderTags(tagList(lapply(fits,function(f) generalized_results_panel(f,info))))$html)
 doc <- xml2::read_html(html)
 if(language!="en") {
   appendix_cells <- xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']//th|//table[@data-result-table-role='appendix']//td"))
   stopifnot(!any(appendix_cells %in% c(checklist_sources,count_sources)))
   stopifnot(!any(grepl("was fitted for dependent variable|; analyzed [0-9]+ of [0-9]+",appendix_cells)))
 }
 main <- xml2::xml_find_all(doc,"//table[@data-result-table-role='main']")
 stopifnot(length(main)>=3,all(xml2::xml_attr(main,"data-result-table-language")=="en"))
 content <- lapply(main,function(t) list(cells=xml2::xml_text(xml2::xml_find_all(t,".//th|.//td")),
   title=xml2::xml_text(xml2::xml_find_all(t,"preceding::h3[1]")),
   notes=xml2::xml_text(xml2::xml_find_all(t,"ancestor::div[@data-result-table-sheet='true'][1]/*[not(self::table)]"))))
 if(language=="en") baseline<-content else stopifnot(identical(baseline,content))
 if(language=="ja") {
  fixture <- analysis_result_table_section(generalized_appendix_text("Multiple-imputation pooling diagnostics",language),localized)
  fixture <- tagList(fixture,analysis_result_table_section(generalized_appendix_text("Suggested manuscript text",language),manuscript_localized))
  fixture <- tagList(fixture,lapply(actual_methods,function(x) analysis_result_table_section(generalized_appendix_text("Suggested manuscript text",language),x)))
  fixture <- tagList(fixture,lapply(fits,function(f) analysis_result_table_section(
    generalized_appendix_text("SCI reporting checklist",language),
    generalized_appendix_table(generalized_reporting_checklist(f),language))))
  note_table <- data.frame(Explanation=note_text)
  fixture <- tagList(fixture,analysis_result_table_section(generalized_appendix_text("Missing-data details",language),ipw_details))
  attr(note_table,"result_table_role") <- "appendix"
  attr(note_table,"result_table_language") <- language
  fixture <- tagList(fixture,analysis_result_table_section(generalized_appendix_text("Notes",language),note_table))
  saveRDS(list(list(id="glm-labels",title="GLM language contract",html=as.character(fixture))),file.path(out,"entries.rds"))
  writeLines(html,file.path(out,"actual-results-ja.html"),useBytes=TRUE)
 }
 cat("PASS:",language,"GLM option values preserved; Gaussian/binomial/count main tables English; labels localized\n")
}
