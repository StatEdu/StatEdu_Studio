Sys.setlocale("LC_CTYPE", "English_United States.utf8")
Sys.setenv(STATEDU_MODULE_CACHE = "false")
source("R/app_bootstrap.R", encoding = "UTF-8")
load_app_packages(check = FALSE); source_app_modules()
languages <- c("en", "ko", "ja", "zh", "es", "fr", "de", "vi")
out <- "tmp/longitudinal-structural-i18n"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
set.seed(916)
d <- data.frame(id = rep(1:32, each = 4), time = rep(0:3, 32), x = rnorm(128))
d$y <- 2 + .4*d$time + .6*d$x + rep(rnorm(32), each=4) + rnorm(128)
info <- data.frame(name=names(d), measurement="continuous", var_label=c("사용자 ID", "시점", "Model rationale", "사용자 결과"))
options(statedu.app_language="en")
fit <- prepare_longitudinal_analysis_result(d,"y","id","time",predictors="x",model_type="gee",family="gaussian",variable_info=info)
stopifnot(length(fit)==1L)
model_fits <- list(gee=fit)
for(model in c("lmm","panel_fe")) {
  model_fits[[model]] <- prepare_longitudinal_analysis_result(d,"y","id","time",predictors="x",model_type=model,family="gaussian",variable_info=info)
  stopifnot(length(model_fits[[model]])==1L)
}
probe <- data.frame(Group=c("Passed", "Adequate"), `Group 1`=c("Failed", "사용자 집단"),
  `Group 2`=c("Adequate", "Passed"), Status=c("Adequate","Failed"),
  `Valid replicates`=c(498L,499L), `Requested replicates`=500L, check.names=FALSE)
bundle <- list(invariance_result=list(type="pls_micom", configural_audit=probe, group_diagnostics=probe,
  pairwise_gate=probe, pls_mga=list(validity_gate=list(groups=probe,pairs=probe)),
  pls_modmed_mga=list(pairwise_validity=probe), permutation_path_sensitivity=probe))
baseline <- NULL
prose <- c("No analysis weights were applied.",
  "Shapiro-Wilk screening did not detect a normality problem.",
  "Working correlation: exchangeable", "Working correlation structure: ar1.",
  "The selected working correlation is exchangeable.",
  "Random intercept grouping variable: 사용자.ID.",
  "A random slope for the selected time variable (시점.1) was included.",
  "12 observations were excluded because of missing values in selected analysis variables.",
  "Analyses were performed using R 4.5.3; geepack 1.3.12.",
  "4 assumption check item(s) reported.", "3 automated sensitivity comparison row(s) generated.",
  "Use GEE when the target is a population-averaged longitudinal effect. Report the selected working correlation (exchangeable) and robust sandwich inference.")
mi_note <- "The dependent variable had no missing values in the selected MI data."
prose <- c(prose,
  "Observation model: 사용자.ID, x.1; weights clipped to [0.250, 2.750] and normalized to mean 1. Report these variables and review positivity/weight stability.",
  "Observation model failed (error in x.1); intercept-only IPW was used. Treat this as a weak IPW sensitivity analysis.",
  sprintf("Standard mice-based MI sensitivity; pooled across 5 imputed dataset(s) using Rubin-style total variance. %s This is not a dedicated multilevel MI engine.",mi_note),
  "Missing-data sensitivity engines were run for Multiple imputation (MI), Weighted GEE (WGEE) (4 fitted row(s), 1 failed row(s)); MI/IPW/WGEE outputs should be reported as sensitivity analyses unless the missing-data model is prespecified as primary.",
  "Missing-data sensitivity engines were run: Inverse probability weighting (IPW). Interpret these as sensitivity analyses and report the imputation or weighting model assumptions.",
  "8 missing-data sensitivity result row(s) generated.")
mi_base <- sprintf("Standard mice-based MI sensitivity; pooled across 5 imputed dataset(s) using Rubin-style total variance. %s This is not a dedicated multilevel MI engine.",mi_note)
failure_detail <- "imputation 2: 사용자.x at 0.25; 3: tolerance 1e-08 (50%)"
prose <- c(prose,
  paste(mi_base,"Weights: Generated IPW for dropout."),
  paste0(mi_base," Failed fits: ",failure_detail,"."),
  paste0(mi_base," Weights: Analysis weight x generated IPW. Failed fits: ",failure_detail,"."),
  "The analysis used GEE, including 128 observations from 32 subjects/clusters across 4 observed time points.",
  "Analysis weights were applied as Generated IPW for dropout with effective sample size 97.25.",
  "Fixed-effect estimates were reported for 사용자.x, y.2 with 95% confidence intervals.",
  "Automated sensitivity screening generated 3 comparison row(s), including fitted alternatives and failed alternatives where applicable.")
prose_probe <- data.frame(Variable=prose, Details=prose)
diagnostic_sources <- jsonlite::read_json("scripts/fixtures/longitudinal_i18n_model_diagnostics.json",simplifyVector=TRUE)
diagnostic_probe <- data.frame(Variable=diagnostic_sources, Interpretation=diagnostic_sources)
recommendations <- unique(unlist(lapply(c("gee","lmm","glmm","panel_fe","panel_re","unknown"),
  function(model) longitudinal_sensitivity_recommendations(model,"gaussian","사용자.ar(1)",FALSE))))
recommendation_probe <- data.frame(Variable=recommendations, Recommendation=recommendations)
check_names <- c("Residual normality","Outcome family / link","GEE working correlation","Random-effects structure",
  "Convergence / singular fit","Random-effect normality","Strict exogeneity / omitted confounding",
  "Overdispersion","Heteroskedasticity","Within-subject serial correlation","Cross-sectional dependence","FE vs RE assumption")
flagged_model <- fit[[1L]]
flagged_model$assumption_checks <- data.frame(Check=check_names,Result="Potential violation",Issue=TRUE)
flagged_manuscript <- longitudinal_manuscript_text(flagged_model)
reml_messages <- c("Repeated-measures marginal linear model with AR(1) residual covariance, REML estimation and Satterthwaite coefficient inference.",
  "When scientifically justified, compare UN and AR(1) residual covariance by fitting each selected structure. Automatic sensitivity fits were not run.")
reml_probe <- data.frame(Variable=reml_messages,Details=reml_messages)
for (language in languages) {
  options(statedu.app_language=language)
  if(language!="en") {
    translated_checks <- vapply(check_names,longitudinal_appendix_text,character(1),language=language)
    stopifnot(all(translated_checks!=check_names))
    flagged_text <- longitudinal_appendix_table(flagged_manuscript,language)[[2]][3]
    stopifnot(all(vapply(translated_checks,function(x) grepl(x,flagged_text,fixed=TRUE),logical(1))),
      !any(vapply(check_names,function(x) grepl(x,flagged_text,fixed=TRUE),logical(1))))
    reml <- longitudinal_appendix_table(reml_probe,language)
    stopifnot(identical(reml[[1]],reml_messages),all(reml[[2]]!=reml_messages),
      grepl("AR(1)",reml[[2]][1],fixed=TRUE),grepl("UN",reml[[2]][2],fixed=TRUE))
    translated_recommendations <- longitudinal_appendix_table(recommendation_probe,language)
    stopifnot(identical(translated_recommendations[[1]],recommendations),
      all(translated_recommendations[[2]]!=recommendations),
      grepl("사용자.ar(1)",translated_recommendations[[2]][1],fixed=TRUE))
  }
  manuscript_sections <- lapply(model_fits,function(models) {
    original <- models[[1L]]$manuscript_text
    parts <- attr(original,"longitudinal_manuscript_parts")
    stopifnot(is.list(parts),identical(parts$source,original$SuggestedText))
    localized <- longitudinal_appendix_table(original,language)
    if(language!="en") {
      expected <- vapply(parts$rows,function(messages) paste(vapply(messages[nzchar(messages)],
        longitudinal_appendix_text,character(1),language=language),collapse=" "),character(1))
      stopifnot(identical(unname(localized[[2]]),unname(expected)),
        !grepl("The analysis used|observations were excluded|No analysis weights|No MI/IPW|Use GEE|Use LMM|Use panel",localized[[2]][1]))
      sensitivity_messages <- parts$rows[[4L]]
      stopifnot(!any(vapply(sensitivity_messages,function(x) grepl(x,localized[[2]][4L],fixed=TRUE),logical(1))))
      changed <- original
      changed$SuggestedText[1] <- "사용자 문단 v1.2.3"
      stopifnot(longitudinal_appendix_table(changed,language)[[2]][1]=="사용자 문단 v1.2.3")
    } else stopifnot(identical(localized$SuggestedText,original$SuggestedText))
    longitudinal_table_section("Suggested manuscript text",original)
  })
  if(language!="en") {
    suffix <- vapply(prose[19:21],longitudinal_appendix_text,character(1),language=language)
    stopifnot(!any(grepl("Weights:|Failed fits:|Standard mice-based",suffix)),
      grepl(failure_detail,suffix[[2]],fixed=TRUE),grepl(failure_detail,suffix[[3]],fixed=TRUE))
  }
  long_html <- as.character(htmltools::renderTags(tagList(lapply(model_fits,function(x) longitudinal_results_panel(x,info))))$html)
  doc <- xml2::read_html(long_html)
  tables <- xml2::xml_find_all(doc,"//table[@data-result-table-role='main']")
  stopifnot(length(tables)>0)
  content <- lapply(tables,function(t) list(
    cells=xml2::xml_text(xml2::xml_find_all(t,".//th|.//td")),
    title=xml2::xml_text(xml2::xml_find_all(t,"preceding::h3[1]")),
    notes=xml2::xml_text(xml2::xml_find_all(t,"ancestor::div[@data-result-table-sheet='true'][1]/*[not(self::table)]"))))
  if(language=="en") baseline<-content else stopifnot(identical(baseline,content))
  structural <- structural_canvas_invariance_appendix_ui(bundle,language)
  sdoc <- xml2::read_html(as.character(structural))
  stables <- xml2::xml_find_all(sdoc,"//table")
  stopifnot(length(stables)==7L)
  for(t in stables) {
    cells<-xml2::xml_text(xml2::xml_find_all(t,".//tbody/tr/td"))
    stopifnot(identical(cells[c(1,2,3,7,8,9)],c("Passed","Failed","Adequate","Adequate","사용자 집단","Passed")))
  }
  cfa_bundle <- list(invariance_result=list(type="cfa",group_diagnostics=probe,
    score_diagnostics=setNames(list(probe),"사용자 단계")))
  cfa_doc <- xml2::read_html(as.character(structural_canvas_invariance_appendix_ui(cfa_bundle,language)))
  stage_title <- sprintf(statedu_localized_text(language,"%s: equality-constraint score tests","%s: 동등성 제약 score 검정"),"사용자 단계")
  stopifnot(stage_title %in% xml2::xml_text(xml2::xml_find_all(cfa_doc,"//h5")),
    is.na(longitudinal_appendix_text(NA_character_,language)))
  if(!language %in% c("en","ko")) {
    diagnostics <- longitudinal_appendix_table(diagnostic_probe,language)
    stopifnot(identical(diagnostics[[1]],diagnostic_sources),all(diagnostics[[2]]!=diagnostic_sources))
    localized_prose <- longitudinal_appendix_table(prose_probe,language)
    stopifnot(identical(localized_prose[[1]],prose),all(localized_prose[[2]]!=prose),
      grepl("사용자.ID",localized_prose[[2]][6],fixed=TRUE),
      grepl("시점.1",localized_prose[[2]][7],fixed=TRUE),
      grepl("12",localized_prose[[2]][8],fixed=TRUE),
      grepl("R 4.5.3; geepack 1.3.12",localized_prose[[2]][9],fixed=TRUE),
      !grepl("Report the selected",localized_prose[[2]][12],fixed=TRUE))
    stopifnot(grepl("사용자.ID, x.1",localized_prose[[2]][13],fixed=TRUE),
      grepl("0.250",localized_prose[[2]][13],fixed=TRUE),grepl("2.750",localized_prose[[2]][13],fixed=TRUE),
      grepl("error in x.1",localized_prose[[2]][14],fixed=TRUE),
      !grepl(mi_note,localized_prose[[2]][15],fixed=TRUE),
      !grepl("Multiple imputation|Weighted GEE",localized_prose[[2]][16]),
      grepl("4",localized_prose[[2]][16],fixed=TRUE),grepl("1",localized_prose[[2]][16],fixed=TRUE))
    stopifnot(all(vapply(c("128","32","4"),function(x) grepl(x,localized_prose[[2]][22],fixed=TRUE),logical(1))),
      grepl("97.25",localized_prose[[2]][23],fixed=TRUE),
      grepl("사용자.x, y.2",localized_prose[[2]][24],fixed=TRUE),
      grepl("95%",gsub(" ","",localized_prose[[2]][24]),fixed=TRUE),
      !grepl("Generated IPW",localized_prose[[2]][23],fixed=TRUE))
    headings<-xml2::xml_text(xml2::xml_find_all(sdoc,"//h4|//h5"))
    stopifnot(!any(headings %in% c("Group-level data diagnostics","MICOM Step 1 configural-invariance audit",
      "Multi-group analysis supplementary tables and diagnostics")))
    for(phrase in c("Model rationale","Analysis weights","Missing-data pattern","Assumption checks","Balanced panel"))
      stopifnot(longitudinal_appendix_text(phrase,language)!=phrase)
  }
  if(language=="ja") {
    # Exact changed output and original user labels in all five export formats.
    fixture <- tagList(longitudinal_table_section("Model rationale",
      data.frame(Variable="Model rationale", Item="Missing-data handling", Value="Complete-case: row-wise")),
      longitudinal_table_section("Assumption checks",prose_probe),
      longitudinal_table_section("Assumption checks",diagnostic_probe),
      longitudinal_table_section("Sensitivity analysis suggestions",recommendation_probe),
      longitudinal_table_section("Suggested manuscript text",flagged_manuscript),
      longitudinal_table_section("Model rationale",reml_probe),structural,manuscript_sections)
    saveRDS(list(list(id="longitudinal-structural",title="Language contract",html=as.character(fixture))),file.path(out,"entries.rds"))
    writeLines(long_html,file.path(out,"longitudinal-ja.html"),useBytes=TRUE)
  }
  cat("PASS:",language,"GEE/LMM/panel FE main tables English; diagnostic translation and original labels preserved\n")
}
