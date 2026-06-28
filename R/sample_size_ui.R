# Sample size and power analysis UI.

sample_size_choice <- function(value, default) {
  if (is.null(value) || length(value) == 0 || is.na(value[[1]]) || !nzchar(as.character(value[[1]]))) {
    return(default)
  }
  as.character(value[[1]])
}

sample_size_ui_text <- function(language = statedu_initial_language(), key) {
  language <- normalize_app_language(language)
  switch(
    key,
    effect_size = statedu_text(language, "Effect size", statedu_utf8("ed9aa8eab3bced81aceab8b0")),
    calculate = statedu_text(language, "Calculate", statedu_utf8("eab384ec82b0")),
    inputs = statedu_text(language, "Inputs", statedu_utf8("ec9e85eba0a5")),
    results = statedu_text(language, "Results", statedu_utf8("eab2b0eab3bc")),
    method = statedu_text(language, "Method", statedu_utf8("ebb0a9ebb295")),
    sample_size = statedu_text(language, "Sample Size", statedu_utf8("ed919cebb3b8ec8898")),
    assumptions_prompt = statedu_text(language, "Enter assumptions and click Calculate.", statedu_utf8("eab080eca095ec9d8420ec9e85eba0a5ed9598eab3a020eab384ec82b0ec9d8420ed81b4eba6aded9598ec84b8ec9a942e")),
    calculating = statedu_text(language, "Calculating...", statedu_utf8("eab384ec82b020eca4912e2e2e")),
    stop = statedu_text(language, "Stop", statedu_utf8("eca491eca780")),
    formula_approximation = statedu_text(language, "Formula / approximation: ", statedu_utf8("eab3b5ec8b9d202f20eab7bcec82ac3a20")),
    references = statedu_text(language, "References", statedu_utf8("ecb0b8eab3a0ebacb8ed978c")),
    effectsize_subtitle = statedu_text(language, "Calculate effect-size inputs used by sample size and power analyses.", statedu_utf8("ed9aa8eab3bced81aceab8b020eab384ec82b0ec979020ec82acec9aa9ed95a020ec9e85eba0a5eab092ec9d8420eab384ec82b0ed95a9eb8b88eb8ba42e")),
    sample_size_subtitle = statedu_text(language, "Calculate required sample size or achieved power from study assumptions.", statedu_utf8("ec97b0eab5ac20eab080eca095ec9cbceba19c20ecb59cec868c20ed919cebb3b820ec889820eb9890eb8a9420eab280eca095eba0a5ec9d8420eab384ec82b0ed95a9eb8b88eb8ba42e")),
    key
  )
}

sample_size_label <- function(language = statedu_initial_language(), label) {
  language <- normalize_app_language(language)
  h <- statedu_utf8
  switch(
    label,
    "Design" = statedu_text(language, "Design", h("ec84a4eab384")),
    "Alpha" = statedu_text(language, "Alpha", h("ec9ca0ec9d98ec8898eca480")),
    "Power" = statedu_text(language, "Power", h("eab280eca095eba0a5")),
    "Minimum sample size" = statedu_text(language, "Minimum sample size", h("ecb59cec868c20ed919cebb3b820ec8898")),
    "Sample size" = statedu_text(language, "Sample size", h("ed919cebb3b820ec8898")),
    "Total sample size" = statedu_text(language, "Total sample size", h("eca084ecb2b420ed919cebb3b820ec8898")),
    "Sample size per group" = statedu_text(language, "Sample size per group", h("eca791eb8ba8ebb38420ed919cebb3b820ec8898")),
    "Participants" = statedu_text(language, "Participants", h("ecb0b8ec97acec9e9020ec8898")),
    "Participants per group" = statedu_text(language, "Participants per group", h("eca791eb8ba8ebb38420ecb0b8ec97acec9e9020ec8898")),
    "Pairs" = statedu_text(language, "Pairs", h("ec8c8d20ec8898")),
    "Number of pairs" = statedu_text(language, "Number of pairs", h("ec8c8d20ec8898")),
    "Group 1 n" = statedu_text(language, "Group 1 n", h("eca791eb8ba82031206e")),
    "Group 2 n" = statedu_text(language, "Group 2 n", h("eca791eb8ba82032206e")),
    "Group 1 mean" = statedu_text(language, "Group 1 mean", h("eca791eb8ba8203120ed8f89eab7a0")),
    "Group 2 mean" = statedu_text(language, "Group 2 mean", h("eca791eb8ba8203220ed8f89eab7a0")),
    "Group 1 SD" = statedu_text(language, "Group 1 SD", h("eca791eb8ba8203120ed919ceca480ed8eb8ecb0a8")),
    "Group 2 SD" = statedu_text(language, "Group 2 SD", h("eca791eb8ba8203220ed919ceca480ed8eb8ecb0a8")),
    "Group 1 events" = statedu_text(language, "Group 1 events", h("eca791eb8ba8203120ec82aceab1b420ec8898")),
    "Group 1 non-events" = statedu_text(language, "Group 1 non-events", h("eca791eb8ba8203120ebb984ec82aceab1b420ec8898")),
    "Group 2 events" = statedu_text(language, "Group 2 events", h("eca791eb8ba8203220ec82aceab1b420ec8898")),
    "Group 2 non-events" = statedu_text(language, "Group 2 non-events", h("eca791eb8ba8203220ebb984ec82aceab1b420ec8898")),
    "Sample mean" = statedu_text(language, "Sample mean", h("ed919cebb3b820ed8f89eab7a0")),
    "Null mean" = statedu_text(language, "Null mean", h("ec9881eab080ec84a420ed8f89eab7a0")),
    "Mean paired difference" = statedu_text(language, "Mean paired difference", h("eb8c80ec9d9120ecb0a8ec9db420ed8f89eab7a0")),
    "SD of paired differences" = statedu_text(language, "SD of paired differences", h("eb8c80ec9d9120ecb0a8ec9db420ed919ceca480ed8eb8ecb0a8")),
    "SD" = statedu_text(language, "SD", h("ed919ceca480ed8eb8ecb0a8")),
    "Proportion 1" = statedu_text(language, "Proportion 1", h("ebb984ec9ca82031")),
    "Proportion 2" = statedu_text(language, "Proportion 2", h("ebb984ec9ca82032")),
    "Expected proportion" = statedu_text(language, "Expected proportion", h("ec9888ec838120ebb984ec9ca8")),
    "Degrees of freedom" = statedu_text(language, "Degrees of freedom", h("ec9e90ec9ca0eb8f84")),
    "Error degrees of freedom" = statedu_text(language, "Error degrees of freedom", h("ec98a4ecb0a820ec9e90ec9ca0eb8f84")),
    "Number of groups" = statedu_text(language, "Number of groups", h("eca791eb8ba820ec8898")),
    "Groups" = statedu_text(language, "Groups", h("eca791eb8ba820ec8898")),
    "Measurements" = statedu_text(language, "Measurements", h("ecb8a1eca09520ec8898")),
    "Outcome" = statedu_text(language, "Outcome", h("eab2b0eab3bcebb380ec8898")),
    "Continuous outcome" = statedu_text(language, "Continuous outcome", h("ec97b0ec868ded989520eab2b0eab3bc")),
    "Binary outcome" = statedu_text(language, "Binary outcome", h("ec9db4ebb684ed989520eab2b0eab3bc")),
    "Effect to test" = statedu_text(language, "Effect to test", h("eab280eca095ed95a020ed9aa8eab3bc")),
    "Alternative" = statedu_text(language, "Alternative", h("eb8c80eba6bdeab080ec84a4")),
    "Two-sided" = statedu_text(language, "Two-sided", h("ec9691ecb8a1")),
    "One-sided" = statedu_text(language, "One-sided", h("eb8ba8ecb8a1")),
    "Dropout rate (%)" = statedu_text(language, "Dropout rate (%)", h("ed8388eb9dbdeba5a020282529")),
    "Allocation ratio (Group 2 / Group 1)" = statedu_text(language, "Allocation ratio (Group 2 / Group 1)", h("ebb0b0eca095ebb9842028eca791eb8ba82032202f20eca791eb8ba8203129")),
    "Time points" = statedu_text(language, "Time points", h("ec8b9ceca09020ec8898")),
    "Working correlation" = statedu_text(language, "Working correlation", h("ec9e91ec9785ec8381eab480")),
    "Pairwise correlations" = statedu_text(language, "Pairwise correlations", h("ec8c8debb38420ec8381eab480")),
    "Input mode" = statedu_text(language, "Input mode", h("ec9e85eba0a520ebb0a9ec8b9d")),
    "Correlation structure" = statedu_text(language, "Correlation structure", h("ec8381eab48020eab5aceca1b0")),
    "Objective" = statedu_text(language, "Objective", h("ebaaa9ed919c")),
    "Parameter" = statedu_text(language, "Parameter", h("ebaaa8ec8898")),
    "Confidence level" = statedu_text(language, "Confidence level", h("ec8ba0eba2b0ec8898eca480")),
    "Desired CI half-width" = statedu_text(language, "Desired CI half-width", h("ebaaa9ed919c20ec8ba0eba2b0eab5aceab08420ebb098ed8fad")),
    "Expected sensitivity" = statedu_text(language, "Expected sensitivity", h("ec9888ec838120ebafbceab090eb8f84")),
    "Expected specificity" = statedu_text(language, "Expected specificity", h("ec9888ec838120ed8ab9ec9db4eb8f84")),
    "Prevalence" = statedu_text(language, "Prevalence", h("ec9ca0ebb391eba5a0")),
    "Number of cases" = statedu_text(language, "Number of cases", h("ec82aceba18020ec8898")),
    "Outcome variables" = statedu_text(language, "Outcome variables", h("eab2b0eab3bcebb380ec889820ec8898")),
    "Covariates" = statedu_text(language, "Covariates", h("eab3b5ebb380eb9f8920ec8898")),
    "Number of predictors" = statedu_text(language, "Number of predictors", h("ec9888ecb8a1ebb380ec889820ec8898")),
    "Tested predictors" = statedu_text(language, "Tested predictors", h("eab280eca09520ec9888ecb8a1ebb380ec889820ec8898")),
    "Total predictors in final model" = statedu_text(language, "Total predictors in final model", h("ecb59ceca28520ebaaa8ed989520eca084ecb2b420ec9888ecb8a1ebb380ec889820ec8898")),
    "Number of covariates" = statedu_text(language, "Number of covariates", h("eab3b5ebb380eb9f8920ec8898")),
    "Simulations" = statedu_text(language, "Simulations", h("ec8b9cebaeaceba088ec9db4ec859820ec8898")),
    "Bootstrap samples" = statedu_text(language, "Bootstrap samples", h("ebb680ed8ab8ec8aa4ed8ab8eb9ea920ed919cebb3b820ec8898")),
    "Expected reliability" = statedu_text(language, "Expected reliability", h("ec9888ec838120ec8ba0eba2b0eb8f84")),
    "Number of items" = statedu_text(language, "Number of items", h("ebacb8ed95ad20ec8898")),
    "Categories" = statedu_text(language, "Categories", h("ebb294eca3bc20ec8898")),
    "Raters / measurements" = statedu_text(language, "Raters / measurements", h("ed8f89eab080ec9e90202f20ecb8a1eca09520ec8898")),
    "Model complexity" = statedu_text(language, "Model complexity", h("ebaaa8ed989520ebb3b5ec9ea1eb8f84")),
    "Parameter type" = statedu_text(language, "Parameter type", h("ebaaa8ec889820ec9ca0ed9895")),
    "Latent variables" = statedu_text(language, "Latent variables", h("ec9ea0ec9eacebb380ec889820ec8898")),
    "Measured variables" = statedu_text(language, "Measured variables", h("ecb8a1eca095ebb380ec889820ec8898")),
    "Structural paths" = statedu_text(language, "Structural paths", h("eab5aceca1b0eab2bdeba19c20ec8898")),
    "Free parameters" = statedu_text(language, "Free parameters", h("ec9e90ec9ca0ebaaa8ec889820ec8898")),
    "Model degrees of freedom" = statedu_text(language, "Model degrees of freedom", h("ebaaa8ed989520ec9e90ec9ca0eb8f84")),
    "Clusters" = statedu_text(language, "Clusters", h("ed81b4eb9facec8aa4ed84b020ec8898")),
    "Periods" = statedu_text(language, "Periods", h("eab8b0eab08420ec8898")),
    "Cluster size" = statedu_text(language, "Cluster size", h("ed81b4eb9facec8aa4ed84b020ed81aceab8b0")),
    "Cluster size per period" = statedu_text(language, "Cluster size per period", h("eab8b0eab084ebb38420ed81b4eb9facec8aa4ed84b020ed81aceab8b0")),
    "Expected proportion 1" = statedu_text(language, "Expected proportion 1", h("ec9888ec838120ebb984ec9ca82031")),
    "Expected proportion 2" = statedu_text(language, "Expected proportion 2", h("ec9888ec838120ebb984ec9ca82032")),
    "Expected rate" = statedu_text(language, "Expected rate", h("ec9888ec838120ebb09cec839deba5a0")),
    "Rate 1" = statedu_text(language, "Rate 1", h("ebb09cec839deba5a02031")),
    "Rate 2" = statedu_text(language, "Rate 2", h("ebb09cec839deba5a02032")),
    "Person-time" = statedu_text(language, "Person-time", h("ec9db8eb8584")),
    "Person-time in Group 1" = statedu_text(language, "Person-time in Group 1", h("eca791eb8ba8203120ec9db8eb8584")),
    "Margin" = statedu_text(language, "Margin", h("eba788eca784")),
    "Expected true difference" = statedu_text(language, "Expected true difference", h("ec9888ec838120ec8ba4eca09c20ecb0a8ec9db4")),
    "SEM / CFA method" = statedu_text(language, "SEM / CFA method", h("53454d202f2043464120ebb0a9ebb295")),
    "Model df input" = statedu_text(language, "Model df input", h("ebaaa8ed989520ec9e90ec9ca0eb8f8420ec9e85eba0a5")),
    "Expected standardized parameter" = statedu_text(language, "Expected standardized parameter", h("ec9888ec838120ed919ceca480ed999420ebaaa8ec8898")),
    "Expected standardized loading" = statedu_text(language, "Expected standardized loading", h("ec9888ec838120ed919ceca480ed999420eca081ec9eaceab092")),
    "Expected standardized path" = statedu_text(language, "Expected standardized path", h("ec9888ec838120ed919ceca480ed999420eab2bdeba19c")),
    "Number of dependent variables" = statedu_text(language, "Number of dependent variables", h("eca285ec868debb380ec889820ec8898")),
    "Numerator df" = statedu_text(language, "Numerator df", h("ebb684ec9e9020ec9e90ec9ca0eb8f84")),
    "Denominator df" = statedu_text(language, "Denominator df", h("ebb684ebaaa820ec9e90ec9ca0eb8f84")),
    "Common SD input" = statedu_text(language, "Common SD input", h("eab3b5ed86b520ed919ceca480ed8eb8ecb0a820ec9e85eba0a5")),
    "Common outcome SD" = statedu_text(language, "Common outcome SD", h("eab3b5ed86b520eab2b0eab3bc20ed919ceca480ed8eb8ecb0a8")),
    "Input scale" = statedu_text(language, "Input scale", h("ec9e85eba0a520ecb299eb8f84")),
    "Rows" = statedu_text(language, "Rows", h("ed968920ec8898")),
    "Columns" = statedu_text(language, "Columns", h("ec97b420ec8898")),
    "Observed proportions" = statedu_text(language, "Observed proportions", h("eab480ecb8a120ebb984ec9ca8")),
    "Expected proportions" = statedu_text(language, "Expected proportions", h("eab8b0eb8c8020ebb984ec9ca8")),
    "Expected mean" = statedu_text(language, "Expected mean", h("eab8b0eb8c8020ed8f89eab7a0")),
    "Group 1 estimated mean" = statedu_text(language, "Group 1 estimated mean", h("31eca791eb8ba820ecb694eca09520ed8f89eab7a0")),
    "Group 2 estimated mean" = statedu_text(language, "Group 2 estimated mean", h("32eca791eb8ba820ecb694eca09520ed8f89eab7a0")),
    "Group 1 pre mean" = statedu_text(language, "Group 1 pre mean", h("31eca791eb8ba820ec82aceca08420ed8f89eab7a0")),
    "Group 1 post mean" = statedu_text(language, "Group 1 post mean", h("31eca791eb8ba820ec82aced9b8420ed8f89eab7a0")),
    "Group 2 pre mean" = statedu_text(language, "Group 2 pre mean", h("32eca791eb8ba820ec82aceca08420ed8f89eab7a0")),
    "Group 2 post mean" = statedu_text(language, "Group 2 post mean", h("32eca791eb8ba820ec82aced9b8420ed8f89eab7a0")),
    "Group 1 means by time" = statedu_text(language, "Group 1 means by time", h("31eca791eb8ba820ec8b9ceca090ebb38420ed8f89eab7a0")),
    "Group 2 means by time" = statedu_text(language, "Group 2 means by time", h("32eca791eb8ba820ec8b9ceca090ebb38420ed8f89eab7a0")),
    "Standardized fixed effect" = statedu_text(language, "Standardized fixed effect", h("ed919ceca480ed999420eab3a0eca095ed9aa8eab3bc")),
    "Residual SD" = statedu_text(language, "Residual SD", h("ec9e94ecb0a820ed919ceca480ed8eb8ecb0a8")),
    "Correlation rho" = statedu_text(language, "Correlation rho", h("ec8381eab4802072686f")),
    "Ratio" = statedu_text(language, "Ratio", h("ebb984ec9ca8")),
    "Regression coefficient B" = statedu_text(language, "Regression coefficient B", h("ed9a8ceab780eab384ec88982042")),
    "Fixed-effect coefficient B" = statedu_text(language, "Fixed-effect coefficient B", h("eab3a0eca095ed9aa8eab3bc20eab384ec88982042")),
    "Log fixed-effect coefficient B" = statedu_text(language, "Log fixed-effect coefficient B", h("eba19ceab7b820eab3a0eca095ed9aa8eab3bc20eab384ec88982042")),
    "Logit fixed-effect coefficient B" = statedu_text(language, "Logit fixed-effect coefficient B", h("eba19ceca79320eab3a0eca095ed9aa8eab3bc20eab384ec88982042")),
    "Incidence rate ratio" = statedu_text(language, "Incidence rate ratio", h("ebb09cec839deba5a0ebb984")),
    "Odds ratio" = statedu_text(language, "Odds ratio", h("ec98a4eca688ebb984")),
    "Hazard ratio" = statedu_text(language, "Hazard ratio", h("ec9c84ed9798ebb984")),
    "Correlation 1" = statedu_text(language, "Correlation 1", h("ec8381eab4802031")),
    "Correlation 2" = statedu_text(language, "Correlation 2", h("ec8381eab4802032")),
    "Correlation r" = statedu_text(language, "Correlation r", h("ec8381eab4802072")),
    "R-squared" = statedu_text(language, "R-squared", h("5220eca09ceab3b1")),
    "Full model R-squared" = statedu_text(language, "Full model R-squared", h("eca084ecb2b420ebaaa8ed9895205220eca09ceab3b1")),
    "Reduced model R-squared" = statedu_text(language, "Reduced model R-squared", h("ecb695ec868c20ebaaa8ed9895205220eca09ceab3b1")),
    "Interaction delta R-squared" = statedu_text(language, "Interaction delta R-squared", h("ec8381ed98b8ec9e91ec9aa920eb8db8ed8380205220eca09ceab3b1")),
    "Expected r" = statedu_text(language, "Expected r", h("eab8b0eb8c802072")),
    "Expected AUC" = statedu_text(language, "Expected AUC", h("eab8b0eb8c8020415543")),
    "Null AUC" = statedu_text(language, "Null AUC", h("ec988120415543")),
    "Overall event probability" = statedu_text(language, "Overall event probability", h("ec839deca1b420ec82aceab1b420ed9995eba5a0")),
    "Independent means (M, SD, n)" = statedu_text(language, "Independent means (M, SD, n)", h("eb8f85eba6bded919cebb3b820ed8f89eab7a0")),
    "Independent t-test (t, n1, n2)" = statedu_text(language, "Independent t-test (t, n1, n2)", h("eb8f85eba6bd20742d74657374")),
    "Independent t-test (t, df; equal n)" = statedu_text(language, "Independent t-test (t, df; equal n)", h("eb8f85eba6bd20742d74657374")),
    "Paired means (mean difference, SD difference)" = statedu_text(language, "Paired means (mean difference, SD difference)", h("eb8c80ec9d91ed919cebb3b820ed8f89eab7a0")),
    "Paired t-test (t, pairs)" = statedu_text(language, "Paired t-test (t, pairs)", h("eb8c80ec9d9120742d74657374")),
    "One-sample mean (M, SD)" = statedu_text(language, "One-sample mean (M, SD)", h("ec9dbced919cebb3b820ed8f89eab7a0")),
    "One-sample t-test (t, n)" = statedu_text(language, "One-sample t-test (t, n)", h("ec9dbced919cebb3b820742d74657374")),
    "Two independent groups" = statedu_text(language, "Two independent groups", h("eb8f85eba6bd20eca791eb8ba8")),
    "One sample" = statedu_text(language, "One sample", h("ec9dbced919cebb3b8")),
    "Paired" = statedu_text(language, "Paired", h("eb8c80ec9d91")),
    "Risk difference" = statedu_text(language, "Risk difference", h("ec9c84ed9798eb8f8420ecb0a8ec9db4")),
    "Risk ratio" = statedu_text(language, "Risk ratio", h("ec9c84ed9798eb8f8420ebb984")),
    "Odds ratio from proportions" = statedu_text(language, "Odds ratio from proportions", h("ebb984ec9ca8eba19c20eab384ec82b0ed959c20ec98a4eca688ebb984")),
    "Odds ratio from 2x2 table" = statedu_text(language, "Odds ratio from 2x2 table", h("32783220ed919ceba19c20eab384ec82b0ed959c20ec98a4eca688ebb984")),
    "Two independent proportions" = statedu_text(language, "Two independent proportions", h("eb8f85eba6bd20ebb984ec9ca82032eca791eb8ba8")),
    "One proportion vs 0.50" = statedu_text(language, "One proportion vs 0.50", h("ec9dbcebbb98ebb984ec9ca820767320302e3530")),
    "Cohen's w from chi-square" = statedu_text(language, "Cohen's w from chi-square", h("ecb9b4ec9db4eca09ceab3b1ec9790ec849c20436f68656e2077")),
    "Cohen's w from category proportions" = statedu_text(language, "Cohen's w from category proportions", h("ebb294eca3bc20ebb984ec9ca8ec9790ec849c20436f68656e2077")),
    "Cramer's V" = statedu_text(language, "Cramer's V", h("ed81aceb9e98eba8b82056")),
    "Phi coefficient" = statedu_text(language, "Phi coefficient", h("ed8c8cec9db420eab384ec8898")),
    "Effect size d" = statedu_text(language, "Effect size d", h("ed9aa8eab3bced81aceab8b02064")),
    "Effect size w" = statedu_text(language, "Effect size w", h("ed9aa8eab3bced81aceab8b02077")),
    "Effect size f" = statedu_text(language, "Effect size f", h("ed9aa8eab3bced81aceab8b02066")),
    "Effect size f2" = statedu_text(language, "Effect size f2", h("ed9aa8eab3bced81aceab8b0206632")),
    "Effect size f2 for R2 increase" = statedu_text(language, "Effect size f2 for R2 increase", h("523220eca69deab08020ed9aa8eab3bced81aceab8b0206632")),
    "Effect size f2 for interaction R2 increase" = statedu_text(language, "Effect size f2 for interaction R2 increase", h("ec8381ed98b8ec9e91ec9aa920523220eca69deab08020ed9aa8eab3bced81aceab8b0206632")),
    "Effect size d (Cohen's d)" = statedu_text(language, "Effect size d (Cohen's d)", h("ed9aa8eab3bced81aceab8b020642028436f68656e2773206429")),
    "Effect size dz (paired difference / SD)" = statedu_text(language, "Effect size dz (paired difference / SD)", h("ed9aa8eab3bced81aceab8b020647a")),
    "Effect size d (mean difference / SD)" = statedu_text(language, "Effect size d (mean difference / SD)", h("ed9aa8eab3bced81aceab8b02064")),
    "Effect size d (median shift / SD)" = statedu_text(language, "Effect size d (median shift / SD)", h("ed9aa8eab3bced81aceab8b02064")),
    "Effect size d (approx.)" = statedu_text(language, "Effect size d (approx.)", h("ed9aa8eab3bced81aceab8b020642028eab7bcebbcac2929")),
    "Effect size W (Kendall's W)" = statedu_text(language, "Effect size W (Kendall's W)", h("ed9aa8eab3bced81aceab8b0205720284b656e64616c6c2773205729")),
    "Pillai's trace V" = statedu_text(language, "Pillai's trace V", h("50696c6c6169ec9d982074726163652056")),
    "t statistic" = statedu_text(language, "t statistic", h("7420ed86b5eab384eb9f89")),
    "F statistic" = statedu_text(language, "F statistic", h("4620ed86b5eab384eb9f89")),
    "Chi-square statistic" = statedu_text(language, "Chi-square statistic", h("ecb9b4ec9db4eca09ceab3b120ed86b5eab384eb9f89")),
    "Point-biserial r" = statedu_text(language, "Point-biserial r", h("eca090ec9db4ec97b020ec8381eab4802072")),
    "Eta squared" = statedu_text(language, "Eta squared", h("ec9790ed8380eca09ceab3b1")),
    "Partial eta squared" = statedu_text(language, "Partial eta squared", h("ebb680ebb68420ec9790ed8380eca09ceab3b1")),
    "Unadjusted Cohen's f" = statedu_text(language, "Unadjusted Cohen's f", h("ebb3b4eca09520eca08420436f68656e27732066")),
    "Wilks' lambda" = statedu_text(language, "Wilks' lambda", h("57696c6b7320eb9e8ceb8ba4")),
    "Mann-Whitney U" = statedu_text(language, "Mann-Whitney U", h("4d616e6e2d576869746e65792055")),
    "Positive rank sum W+" = statedu_text(language, "Positive rank sum W+", h("ec9691ec9d9820ec889cec9c84ed95a920572b")),
    "Negative rank sum W-" = statedu_text(language, "Negative rank sum W-", h("ec9d8cec9d9820ec889cec9c84ed95a920572d")),
    "Kruskal-Wallis H" = statedu_text(language, "Kruskal-Wallis H", h("4b7275736b616c2d57616c6c69732048")),
    "Friedman chi-square" = statedu_text(language, "Friedman chi-square", h("46726965646d616e20ecb9b4ec9db4eca09ceab3b1")),
    "b: negative to positive pairs" = statedu_text(language, "b: negative to positive pairs", h("623a20ec9d8cec84b1ec9790ec849c20ec9691ec84b1ec9cbceba19c20ebb094eb809020ec8c8d")),
    "c: positive to negative pairs" = statedu_text(language, "c: positive to negative pairs", h("633a20ec9691ec84b1ec9790ec849c20ec9d8cec84b1ec9cbceba19c20ebb094eb809020ec8c8d")),
    "p01: negative to positive" = statedu_text(language, "p01: negative to positive", h("7030313a20ec9d8cec84b1ec9790ec849c20ec9691ec84b1")),
    "p10: positive to negative" = statedu_text(language, "p10: positive to negative", h("7031303a20ec9691ec84b1ec9790ec849c20ec9d8cec84b1")),
    "One-way ANOVA" = statedu_text(language, "One-way ANOVA", h("ec9dbcec9b9020ebb684ec82b0ebb684ec849d")),
    "Two-way ANOVA" = statedu_text(language, "Two-way ANOVA", h("ec9db4ec9b90ebb684ec82b0ebb684ec849d")),
    "One-group repeated-measures ANOVA" = statedu_text(language, "One-group repeated-measures ANOVA", h("eb8ba8ec9dbc20eca791eb8ba820ebb098ebb3b5ecb8a1eca09520ebb684ec82b0ebb684ec849d")),
    "Mixed repeated-measures ANOVA" = statedu_text(language, "Mixed repeated-measures ANOVA", h("ed98bced95a920ebb098ebb3b5ecb8a1eca09520ebb684ec82b0ebb684ec849d")),
    "Main effect A" = statedu_text(language, "Main effect A", h("eca3bced9aa8eab3bc2041")),
    "Main effect B" = statedu_text(language, "Main effect B", h("eca3bced9aa8eab3bc2042")),
    "Interaction A x B" = statedu_text(language, "Interaction A x B", h("ec8381ed98b8ec9e91ec9aa9204120782042")),
    "Group" = statedu_text(language, "Group", h("eca791eb8ba8")),
    "Time" = statedu_text(language, "Time", h("ec8b9ceca090")),
    "Group x Time" = statedu_text(language, "Group x Time", h("eca791eb8ba8207820ec8b9ceca090")),
    "Covariate R-squared" = statedu_text(language, "Covariate R-squared", h("eab3b5ebb380eb9f892052eca09ceab3b1")),
    "Factor A levels" = statedu_text(language, "Factor A levels", h("ec9a94ec9db8204120ec8898eca480")),
    "Factor B levels" = statedu_text(language, "Factor B levels", h("ec9a94ec9db8204220ec8898eca480")),
    "Average repeated-measures correlation" = statedu_text(language, "Average repeated-measures correlation", h("ed8f89eab7a020ebb098ebb3b5ecb8a1eca09520ec8381eab480")),
    "Nonsphericity epsilon" = statedu_text(language, "Nonsphericity epsilon", h("eab5aced9895ec84b120ec9c84ebb09820ebb3b4eca095eab092")),
    "Multiple regression" = statedu_text(language, "Multiple regression", h("eb8ba4eca49120ed9a8ceab780")),
    "Hierarchical regression" = statedu_text(language, "Hierarchical regression", h("ec9c84eab384eca08120ed9a8ceab780")),
    "Logistic regression" = statedu_text(language, "Logistic regression", h("eba19ceca780ec8aa4ed8bb120ed9a8ceab780")),
    "Mediation effect" = statedu_text(language, "Mediation effect", h("eba7a4eab09ced9aa8eab3bc")),
    "Moderation regression" = statedu_text(language, "Moderation regression", h("eca1b0eca08820ed9a8ceab780")),
    "Baseline event probability" = statedu_text(language, "Baseline event probability", h("eab8b0ecb48820ec82aceab1b420ed9995eba5a0")),
    "Predictor prevalence" = statedu_text(language, "Predictor prevalence", h("ec9888ecb8a1ebb380ec889820ec9ca0ebb391eba5a0")),
    "Interaction terms tested" = statedu_text(language, "Interaction terms tested", h("eab280eca09520ec8381ed98b8ec9e91ec9aa9ed95ad20ec8898")),
    "Mediation method" = statedu_text(language, "Mediation method", h("eba7a4eab09ced9aa8eab3bc20ebb0a9ebb295")),
    "Path a effect size" = statedu_text(language, "Path a effect size", h("eab2bdeba19c206120ed9aa8eab3bced81aceab8b0")),
    "Path b effect size" = statedu_text(language, "Path b effect size", h("eab2bdeba19c206220ed9aa8eab3bced81aceab8b0")),
    "Path a beta: predictor -> mediator" = statedu_text(language, "Path a beta: predictor -> mediator", h("eab2bdeba19c206120626574613a20ec9888ecb8a1ebb380ec8898202d3e20eba7a4eab09cebb380ec8898")),
    "Path b beta: mediator -> outcome" = statedu_text(language, "Path b beta: mediator -> outcome", h("eab2bdeba19c206220626574613a20eba7a4eab09cebb380ec8898202d3e20eab2b0eab3bcebb380ec8898")),
    "Group x time parameter estimate B" = statedu_text(language, "Group x time parameter estimate B", h("eca791eb8ba8207820ec8b9ceab08420ebaaa8ec889820ecb694eca095ecb9982042")),
    "Mean difference (I - J)" = statedu_text(language, "Mean difference (I - J)", h("ed8f89eab7a020ecb0a8ec9db4202849202d204a29")),
    "Variance at time I" = statedu_text(language, "Variance at time I", h("ec8b9ceca090204920ebb684ec82b0")),
    "Variance at time J" = statedu_text(language, "Variance at time J", h("ec8b9ceca090204a20ebb684ec82b0")),
    "Covariance I,J" = statedu_text(language, "Covariance I,J", h("eab3b5ebb684ec82b020492c4a")),
    "ICC / random intercept proportion" = statedu_text(language, "ICC / random intercept proportion", h("494343202f20eb9e9ceb8da420eca088ed8eb820ebb984ec9ca8")),
    "Working correlation rho" = statedu_text(language, "Working correlation rho", h("ec9e91ec9785ec8381eab4802072686f")),
    "Cohen's kappa" = statedu_text(language, "Cohen's kappa", h("436f68656e2773206b61707061")),
    "ICC" = statedu_text(language, "ICC", "ICC"),
    "Null RMSEA" = statedu_text(language, "Null RMSEA", h("ec9881eab080ec84a420524d534541")),
    "Alternative RMSEA" = statedu_text(language, "Alternative RMSEA", h("eb8c80eba6bdeab080ec84a420524d534541")),
    "Simple" = statedu_text(language, "Simple", h("eb8ba8ec889c")),
    "GLIMMPSE-style" = statedu_text(language, "GLIMMPSE-style", h("474c494d4d50534520ebb0a9ec8b9d")),
    "Exchangeable" = statedu_text(language, "Exchangeable", h("eab590ed9998eab080eb8aa5")),
    "Unstructured" = statedu_text(language, "Unstructured", h("ebb984eab5aceca1b0ed9994")),
    "Two-group repeated (Group x Time)" = statedu_text(language, "Two-group repeated (Group x Time)", h("eb919020eca791eb8ba820ebb098ebb3b5ecb8a1eca0952028eca791eb8ba8207820ec8b9ceca09029")),
    "One-group repeated (Time slope)" = statedu_text(language, "One-group repeated (Time slope)", h("ed959c20eca791eb8ba820ebb098ebb3b5ecb8a1eca0952028ec8b9ceca09020eab8b0ec9ab8eab8b029")),
    "Non-inferiority" = statedu_text(language, "Non-inferiority", h("ebb984ec97b4eb93b1ec84b1")),
    "Equivalence" = statedu_text(language, "Equivalence", h("eb8f99eb93b1ec84b1")),
    "Mean difference" = statedu_text(language, "Mean difference", h("ed8f89eab7a020ecb0a8ec9db4")),
    "Proportion difference" = statedu_text(language, "Proportion difference", h("ebb984ec9ca820ecb0a8ec9db4")),
    "Sensitivity precision" = statedu_text(language, "Sensitivity precision", h("ebafbceab090eb8f8420eca095ebb080eb8f84")),
    "Specificity precision" = statedu_text(language, "Specificity precision", h("ed8ab9ec9db4eb8f8420eca095ebb080eb8f84")),
    "ROC AUC vs null" = statedu_text(language, "ROC AUC vs null", h("524f432041554320eb8c8020ec9881eab080ec84a4")),
    "Mean" = statedu_text(language, "Mean", h("ed8f89eab7a0")),
    "Proportion" = statedu_text(language, "Proportion", h("ebb984ec9ca8")),
    "Correlation" = statedu_text(language, "Correlation", h("ec8381eab480")),
    "Two Poisson rates" = statedu_text(language, "Two Poisson rates", h("eb919020ed8facec9584ec86a120ebb984ec9ca8")),
    "Two negative binomial rates" = statedu_text(language, "Two negative binomial rates", h("eb919020ec9d8cec9db4ed95ad20ebb984ec9ca8")),
    "Single rate precision" = statedu_text(language, "Single rate precision", h("eb8ba8ec9dbc20ebb984ec9ca820eca095ebb080eb8f84")),
    "Dispersion" = statedu_text(language, "Dispersion", h("ebb684ec82b0")),
    "Parallel cluster randomized trial" = statedu_text(language, "Parallel cluster randomized trial", h("ed8f89ed968920eab5b0eca79120ebacb4ec9e91ec9c8420ec8b9ced9798")),
    "Stepped-wedge cluster trial" = statedu_text(language, "Stepped-wedge cluster trial", h("eab384eb8ba8ed989520eab5b0eca79120ec8b9ced9798")),
    "Close fit test (detect poor fit)" = statedu_text(language, "Close fit test (detect poor fit)", h("ebb080eca09120eca081ed95a920eab280eca095")),
    "Not-close-fit test (support close fit)" = statedu_text(language, "Not-close-fit test (support close fit)", h("ebb984ebb080eca09120eca081ed95a920eab280eca095")),
    "Parameter-level Monte Carlo" = statedu_text(language, "Parameter-level Monte Carlo", h("ebaaa8ec889820ec8898eca480204d6f6e7465204361726c6f")),
    "Model complexity heuristic" = statedu_text(language, "Model complexity heuristic", h("ebaaa8ed989520ebb3b5ec9ea1eb8f8420ed9cb4eba6acec8aa4ed8bb1")),
    "Standardized loading" = statedu_text(language, "Standardized loading", h("ed919ceca480ed999420ec9a94ec9db8ebb680ed9598")),
    "Standardized path" = statedu_text(language, "Standardized path", h("ed919ceca480ed999420eab2bdeba19c")),
    "Latent correlation" = statedu_text(language, "Latent correlation", h("ec9ea0ec9eacebb380ec889820ec8381eab480")),
    "Moderate" = statedu_text(language, "Moderate", h("ebb3b4ed86b5")),
    "Complex" = statedu_text(language, "Complex", h("ebb3b5ec9ea1")),
    "Estimate from model counts" = statedu_text(language, "Estimate from model counts", h("ebaaa8ed989520ec8898ec9790ec849c20ecb694eca095")),
    "Enter model df directly" = statedu_text(language, "Enter model df directly", h("ebaaa8ed989520ec9e90ec9ca0eb8f8420eca781eca09120ec9e85eba0a5")),
    "Point-biserial r" = statedu_text(language, "Point-biserial r", h("eca090ec9691ebb68420ec8381eab4802072")),
    "Pearson r from t statistic" = statedu_text(language, "Pearson r from t statistic", h("7420ed86b5eab384eb9f89ec9790ec849c2050656172736f6e2072")),
    "Pearson r from F statistic" = statedu_text(language, "Pearson r from F statistic", h("4620ed86b5eab384eb9f89ec9790ec849c2050656172736f6e2072")),
    "Pearson r from R-squared" = statedu_text(language, "Pearson r from R-squared", h("52eca09ceab3b1ec9790ec849c2050656172736f6e2072")),
    "Fisher's z from r" = statedu_text(language, "Fisher's z from r", h("72ec9790ec849c20466973686572207a")),
    "Cohen's q for two correlations" = statedu_text(language, "Cohen's q for two correlations", h("eb919020ec8381eab480ec9d9820436f68656e2071")),
    "Partial eta squared from F" = statedu_text(language, "Partial eta squared from F", h("46ec9790ec849c20ebb680ebb68420ec9790ed8380eca09ceab3b1")),
    "Cohen's f from eta squared" = statedu_text(language, "Cohen's f from eta squared", h("ec9790ed8380eca09ceab3b1ec9790ec849c20436f68656e2066")),
    "Cohen's f from partial eta squared" = statedu_text(language, "Cohen's f from partial eta squared", h("ebb680ebb68420ec9790ed8380eca09ceab3b1ec9790ec849c20436f68656e2066")),
    "ANCOVA partial eta squared from F" = statedu_text(language, "ANCOVA partial eta squared from F", h("414e434f56412046ec9790ec849c20ebb680ebb68420ec9790ed8380eca09ceab3b1")),
    "ANCOVA adjusted Cohen's f" = statedu_text(language, "ANCOVA adjusted Cohen's f", h("414e434f564120ebb3b4eca09520436f68656e2066")),
    "ANCOVA Cohen's f from partial eta squared" = statedu_text(language, "ANCOVA Cohen's f from partial eta squared", h("414e434f564120ebb680ebb68420ec9790ed8380eca09ceab3b1ec9790ec849c20436f68656e2066")),
    "MANOVA Pillai's trace to f2" = statedu_text(language, "MANOVA Pillai's trace to f2", h("4d414e4f56412050696c6c6169207472616365ec9790ec849c206632")),
    "MANOVA Wilks' lambda to f2" = statedu_text(language, "MANOVA Wilks' lambda to f2", h("4d414e4f56412057696c6b73206c616d626461ec9790ec849c206632")),
    "Rank-biserial r from Mann-Whitney U" = statedu_text(language, "Rank-biserial r from Mann-Whitney U", h("4d616e6e2d576869746e65792055ec9790ec849c20ec889cec9c84ec9691ebb6842072")),
    "Rank-biserial r for paired Wilcoxon" = statedu_text(language, "Rank-biserial r for paired Wilcoxon", h("eb8c80ec9d912057696c636f786f6e20ec889cec9c84ec9691ebb6842072")),
    "Kruskal-Wallis epsilon squared" = statedu_text(language, "Kruskal-Wallis epsilon squared", h("4b7275736b616c2d57616c6c697320ec97a1ec8ba4eba1a0eca09ceab3b1")),
    "Friedman Kendall's W" = statedu_text(language, "Friedman Kendall's W", h("46726965646d616e204b656e64616c6c2057")),
    "Matched-pair odds ratio from probabilities" = statedu_text(language, "Matched-pair odds ratio from probabilities", h("ed9995eba5a0ec9790ec849c20eb8c80ec9d91ec8c8d20ec98a4eca688ebb984")),
    "Matched-pair odds ratio from paired 2x2 table" = statedu_text(language, "Matched-pair odds ratio from paired 2x2 table", h("eb8c80ec9d912032783220ed919cec9790ec849c20ec98a4eca688ebb984")),
    "Cohen's g from discordant probabilities" = statedu_text(language, "Cohen's g from discordant probabilities", h("ebb688ec9dbcecb99820ed9995eba5a0ec9790ec849c20436f68656e2067")),
    "Multiple regression f2 from R-squared" = statedu_text(language, "Multiple regression f2 from R-squared", h("52eca09ceab3b1ec9790ec849c20eb8ba4eca491ed9a8ceab780206632")),
    "Hierarchical regression f2 from R-squared increase" = statedu_text(language, "Hierarchical regression f2 from R-squared increase", h("52eca09ceab3b120eca69deab080ec9790ec849c20ec9c84eab384ed9a8ceab780206632")),
    "Logistic regression OR conversion" = statedu_text(language, "Logistic regression OR conversion", h("eba19ceca780ec8aa4ed8bb120ed9a8ceab780204f5220ebb380ed9998")),
    "Moderation interaction f2" = statedu_text(language, "Moderation interaction f2", h("eca1b0eca08820ec8381ed98b8ec9e91ec9aa9206632")),
    "Follow-up estimated means" = statedu_text(language, "Follow-up estimated means", h("ecb694eca081ec8b9ceca09020ecb694eca095ed8f89eab7a0")),
    "Pre-post change means" = statedu_text(language, "Pre-post change means", h("ec82aceca0842dec82aced9b8420ebb380ed999420ed8f89eab7a0")),
    "Group x time B" = statedu_text(language, "Group x time B", h("eca791eb8ba8207820ec8b9ceab0842042")),
    "Continuous outcome supplied d" = statedu_text(language, "Continuous outcome supplied d", h("ec97b0ec868ded989520eab2b0eab3bc206420eca781eca09120ec9e85eba0a5")),
    "Binary outcome from proportions" = statedu_text(language, "Binary outcome from proportions", h("ebb984ec9ca8ec9790ec849c20ec9db4ebb684ed989520eab2b0eab3bc")),
    "Binary logit fixed effect" = statedu_text(language, "Binary logit fixed effect", h("ec9db4ebb684ed989520eba19ceca79320eab3a0eca095ed9aa8eab3bc")),
    "Binary outcome probabilities" = statedu_text(language, "Binary outcome probabilities", h("ec9db4ebb684ed989520eab2b0eab3bc20ed9995eba5a0")),
    "Count log-link fixed effect" = statedu_text(language, "Count log-link fixed effect", h("ecb9b4ec9ab4ed8ab820eba19ceab7b8eba781ed81ac20eab3a0eca095ed9aa8eab3bc")),
    "Count outcome rates" = statedu_text(language, "Count outcome rates", h("ecb9b4ec9ab4ed8ab820eab2b0eab3bc20ebb984ec9ca8")),
    "Gaussian fixed effect" = statedu_text(language, "Gaussian fixed effect", h("eab080ec9ab0ec8b9cec958820eab3a0eca095ed9aa8eab3bc")),
    "Simple standardized fixed effect" = statedu_text(language, "Simple standardized fixed effect", h("eb8ba8ec889c20ed919ceca480ed999420eab3a0eca095ed9aa8eab3bc")),
    "GLIMMPSE-style mean vectors" = statedu_text(language, "GLIMMPSE-style mean vectors", h("474c494d4d50534520ebb0a9ec8b9d20ed8f89eab7a020ebb2a1ed84b0")),
    "SPSS LMM output (F, df, covariance)" = statedu_text(language, "SPSS LMM output (F, df, covariance)", h("53505353204c4d4d20ecb69ceba0a528462c2064662c20eab3b5ebb684ec82b029")),
    "Hazard ratio to log hazard ratio" = statedu_text(language, "Hazard ratio to log hazard ratio", h("ec9c84ed9798ebb984ec9790ec849c20eba19ceab7b8ec9c84ed9798ebb984")),
    "Mean difference margin distance" = statedu_text(language, "Mean difference margin distance", h("ed8f89eab7a0ecb0a820eba788eca78420eab1b0eba6ac")),
    "Proportion difference margin distance" = statedu_text(language, "Proportion difference margin distance", h("ebb984ec9ca8ecb0a820eba788eca78420eab1b0eba6ac")),
    "Poisson incidence rate ratio" = statedu_text(language, "Poisson incidence rate ratio", h("ed8facec9584ec86a120ebb09cec839deba5a0ebb984")),
    "Negative binomial incidence rate ratio" = statedu_text(language, "Negative binomial incidence rate ratio", h("ec9d8cec9db4ed95ad20ebb09cec839deba5a0ebb984")),
    "Gamma mean ratio" = statedu_text(language, "Gamma mean ratio", h("eab090eba78820ed8f89eab7a0ebb984")),
    "Parallel continuous outcome" = statedu_text(language, "Parallel continuous outcome", h("ed8f89ed968920ec84a4eab38420ec97b0ec868ded989520eab2b0eab3bc")),
    "Parallel binary outcome" = statedu_text(language, "Parallel binary outcome", h("ed8f89ed968920ec84a4eab38420ec9db4ebb684ed989520eab2b0eab3bc")),
    "Stepped-wedge continuous outcome" = statedu_text(language, "Stepped-wedge continuous outcome", h("eab384eb8ba8ed989520ec84a4eab38420ec97b0ec868ded989520eab2b0eab3bc")),
    "Mean CI precision" = statedu_text(language, "Mean CI precision", h("ed8f89eab7a020ec8ba0eba2b0eab5aceab08420eca095ebb080eb8f84")),
    "Proportion CI precision" = statedu_text(language, "Proportion CI precision", h("ebb984ec9ca820ec8ba0eba2b0eab5aceab08420eca095ebb080eb8f84")),
    "Correlation CI precision" = statedu_text(language, "Correlation CI precision", h("ec8381eab48020ec8ba0eba2b0eab5aceab08420eca095ebb080eb8f84")),
    "Cohen's kappa" = statedu_text(language, "Cohen's kappa", h("436f68656e20ecb9b4ed8c8c")),
    "Standardized parameter" = statedu_text(language, "Standardized parameter", h("ed919ceca480ed999420ebaaa8ec8898")),
    "Cohen's d for independent means" = statedu_text(language, "Cohen's d for independent means", h("eb8f85eba6bded8f89eab7a020436f68656e2064")),
    "Hedges' g for independent means" = statedu_text(language, "Hedges' g for independent means", h("eb8f85eba6bded8f89eab7a0204865646765732067")),
    "Cohen's d for one-sample mean" = statedu_text(language, "Cohen's d for one-sample mean", h("ec9dbced919cebb3b820ed8f89eab7a020436f68656e2064")),
    "Cohen's dz for paired means" = statedu_text(language, "Cohen's dz for paired means", h("eb8c80ec9d91ed8f89eab7a020436f68656e20647a")),
    "Mann-Whitney U (two independent groups)" = statedu_text(language, "Mann-Whitney U (two independent groups)", h("4d616e6e2d576869746e6579205528eb919020eb8f85eba6bdeca791eb8ba829")),
    "Wilcoxon signed-rank (paired samples)" = statedu_text(language, "Wilcoxon signed-rank (paired samples)", h("57696c636f786f6e207369676e65642d72616e6b28eb8c80ec9d91ed919cebb3b829")),
    "One-sample Wilcoxon signed-rank (median shift)" = statedu_text(language, "One-sample Wilcoxon signed-rank (median shift)", h("ec9dbced919cebb3b82057696c636f786f6e207369676e65642d72616e6b28eca491ec9599eab09220ec9db4eb8f9929")),
    "Friedman test" = statedu_text(language, "Friedman test", h("46726965646d616e20eab280eca095")),
    "Fritz & MacKinnon empirical table (.80 power)" = statedu_text(language, "Fritz & MacKinnon empirical table (.80 power)", h("467269747a2026204d61634b696e6e6f6e20eab2bded9798ed919c282e383020eab280eca095eba0a529")),
    "Monte Carlo indirect effect CI" = statedu_text(language, "Monte Carlo indirect effect CI", h("4d6f6e7465204361726c6f20eab084eca091ed9aa8eab3bc204349")),
    "Bootstrap indirect effect CI (slow)" = statedu_text(language, "Bootstrap indirect effect CI (slow)", h("426f6f74737472617020eab084eca091ed9aa8eab3bc20434928eb8a90eba6bc29")),
    "Sobel approximation" = statedu_text(language, "Sobel approximation", h("536f62656c20eab7bcec82ac")),
    "Small (.14)" = statedu_text(language, "Small (.14)", h("ec9e91ec9d8c282e313429")),
    "Halfway (.26)" = statedu_text(language, "Halfway (.26)", h("eca491eab08420eca084282e323629")),
    "Medium (.39)" = statedu_text(language, "Medium (.39)", h("eca491eab084282e333929")),
    "Large (.59)" = statedu_text(language, "Large (.59)", h("ed81bc282e353929")),
    "Bias-corrected bootstrap" = statedu_text(language, "Bias-corrected bootstrap", h("ed8eb8ed96a5ebb3b4eca09520626f6f747374726170")),
    "Percentile bootstrap" = statedu_text(language, "Percentile bootstrap", h("ebb0b1ebb684ec9c8420626f6f747374726170")),
    "PRODCLIN / distribution of the product" = statedu_text(language, "PRODCLIN / distribution of the product", h("50524f44434c494e202f20eab3b1ec9d9820ebb684ed8fac")),
    "Joint significance" = statedu_text(language, "Joint significance", h("eab3b5eb8f9920ec9ca0ec9d98ec84b1")),
    "Sobel / first-order delta" = statedu_text(language, "Sobel / first-order delta", h("536f62656c202f2031ecb0a820eb8db8ed8380")),
    "Cronbach's alpha" = statedu_text(language, "Cronbach's alpha", h("43726f6e6261636820ec958ced8c8c")),
    "ICC reliability" = statedu_text(language, "ICC reliability", h("49434320ec8ba0eba2b0eb8f84")),
    "Bland-Altman LoA" = statedu_text(language, "Bland-Altman LoA", h("426c616e642d416c746d616e20ec9dbcecb998ed959ceab384")),
    "LMM design" = statedu_text(language, "LMM design", h("4c4d4d20ec84a4eab384")),
    "Logit coefficient B" = statedu_text(language, "Logit coefficient B", h("eba19ceca79320eab384ec88982042")),
    "Log fixed-effect coefficient B" = statedu_text(language, "Log fixed-effect coefficient B", h("eba19ceab7b820eab3a0eca095ed9aa8eab3bc20eab384ec88982042")),
    "Observed / expected difference" = statedu_text(language, "Observed / expected difference", h("ecb694eca0952feab480ecb0b020ecb0a8ec9db4")),
    label
  )
}

sample_size_choice_labels <- function(language, labels) {
  stats::setNames(labels, vapply(labels, function(label) sample_size_label(language, label), character(1), USE.NAMES = FALSE))
}

sample_size_step_heading <- function(number, key, language = statedu_initial_language()) {
  paste0(number, ". ", sample_size_ui_text(language, key))
}

effect_size_method_title <- function(method, language = statedu_initial_language()) {
  labels <- effect_size_method_labels(language)
  labels[[method]] %||% "Effect Size"
}

effect_size_panel_heading <- function(method, language = statedu_initial_language()) {
  paste(effect_size_method_title(method, language), sample_size_ui_text(language, "effect_size"))
}

effect_size_action_button <- function(input_id, language = statedu_initial_language()) {
  actionButton(
    input_id,
    statedu_ui_label("calculate", language),
    class = "btn btn-primary sample-size-calculate",
    onclick = sprintf(
      "if (typeof Shiny !== 'undefined') Shiny.setInputValue('%s', Date.now() + Math.random(), {priority: 'event'});",
      input_id
    )
  )
}

sample_size_tab_panel <- function(language = statedu_initial_language()) {
  methods <- sample_size_method_labels(language)
  item <- function(method, title = methods[[method]]) {
    lazy_tab_panel(title, paste0("sample_size_", method), paste0("lazy_sample_size_", method))
  }
  do.call(navbarMenu, c(list(statedu_ui_label("sample_size", language)), lapply(names(methods), item)))
}

sample_size_target_choices <- function(method, language = statedu_initial_language()) {
  required_label <- statedu_text(language, "Minimum sample size", statedu_utf8("ecb59cec868c20ed919cebb3b820ec8898"))
  power_label <- statedu_text(language, "Power", statedu_utf8("eab280eca095eba0a5"))
  precision_label <- statedu_text(language, "Achieved precision", statedu_utf8("eb8bacec84b120eca095ebb080eb8f84"))
  if (identical(method, "reliability")) {
    return(stats::setNames("sample_size", required_label))
  }
  if (identical(method, "precision")) {
    return(stats::setNames(c("sample_size", "power"), c(required_label, precision_label)))
  }
  stats::setNames(c("sample_size", "power"), c(required_label, power_label))
}

effect_size_tab_panel <- function(language = statedu_initial_language()) {
  methods <- effect_size_method_labels(language)
  item <- function(method, title = methods[[method]]) {
    lazy_tab_panel(title, paste0("effect_size_", method), paste0("lazy_effect_size_", method))
  }
  do.call(navbarMenu, c(list(statedu_ui_label("effect_size", language)), lapply(names(methods), item)))
}

effect_size_analysis_panel <- function(method, language = statedu_initial_language()) {
  if (!method %in% c("ttest", "proportion", "chisquare", "correlation", "anova", "ancova", "nonparametric", "mcnemar", "regression", "gee", "glmm", "lmm", "survival", "equivalence", "diagnostic", "rates", "cluster", "precision", "reliability", "sem")) {
    labels <- effect_size_method_labels(language)
    title <- labels[[method]] %||% "Effect Size"
    return(tabPanel(
      title,
      value = paste0("effect_size_", method),
      div(
        class = "page-shell",
        div(
          class = "app-heading",
          h1(title),
          div(sample_size_ui_text(language, "effectsize_subtitle"), class = "app-subtitle")
        ),
        div(
          class = "workspace-panel frequencies-workspace-panel sample-size-workspace-panel",
          style = "min-width:980px;overflow-x:auto;",
          h3(paste(title, sample_size_ui_text(language, "effect_size"))),
          div(class = "empty-message", statedu_text(language, "This effect-size calculator is not available for the selected method."))
        )
      )
    ))
  }
  if (identical(method, "ttest")) {
    return(tabPanel(
      effect_size_method_title("ttest", language),
      value = "effect_size_ttest",
      div(
        class = "page-shell",
        div(
          class = "app-heading",
          h1(effect_size_method_title("ttest", language)),
          div(sample_size_ui_text(language, "effectsize_subtitle"), class = "app-subtitle")
        ),
        div(
          class = "workspace-panel frequencies-workspace-panel sample-size-workspace-panel",
          style = "min-width:980px;overflow-x:auto;",
          h3(effect_size_panel_heading("ttest", language)),
          div(
            class = "sample-size-grid",
            div(
              class = "step-block sample-size-block sample-size-block1",
              h3(sample_size_step_heading(1, "method", language)),
              radioButtons(
                "effect_size_ttest_design",
                label = NULL,
                choices = sample_size_choice_labels(language, c(
                  "Independent means (M, SD, n)" = "independent_means",
                  "Independent t-test (t, n1, n2)" = "independent_t_n",
                  "Independent t-test (t, df; equal n)" = "independent_t_df_equal",
                  "Paired means (mean difference, SD difference)" = "paired_means",
                  "Paired t-test (t, pairs)" = "paired_t",
                  "One-sample mean (M, SD)" = "one_sample_mean",
                  "One-sample t-test (t, n)" = "one_sample_t"
                )),
                selected = "independent_means"
              )
            ),
            div(
              class = "step-block sample-size-block sample-size-block2",
              h3(sample_size_step_heading(2, "inputs", language)),
              uiOutput("effect_size_ttest_inputs"),
              effect_size_action_button("effect_size_ttest_calculate", language)
            ),
            div(
              class = "step-block sample-size-block sample-size-block3",
              h3(sample_size_step_heading(3, "results", language)),
              uiOutput("effect_size_ttest_results")
            )
          )
        )
      )
    ))
  }
  if (identical(method, "proportion")) {
    return(tabPanel(
      effect_size_method_title("proportion", language),
      value = "effect_size_proportion",
      div(
        class = "page-shell",
        div(
          class = "app-heading",
          h1(effect_size_method_title("proportion", language)),
          div(sample_size_ui_text(language, "effectsize_subtitle"), class = "app-subtitle")
        ),
        div(
          class = "workspace-panel frequencies-workspace-panel sample-size-workspace-panel",
          style = "min-width:980px;overflow-x:auto;",
          h3(effect_size_panel_heading("proportion", language)),
          div(
            class = "sample-size-grid",
            div(
              class = "step-block sample-size-block sample-size-block1",
              h3(sample_size_step_heading(1, "effect_size", language)),
              radioButtons(
                "effect_size_proportion_design",
                label = NULL,
                choices = sample_size_choice_labels(language, c(
                  "Cohen's h" = "cohens_h",
                  "Risk difference" = "risk_difference",
                  "Risk ratio" = "risk_ratio",
                  "Odds ratio from proportions" = "odds_ratio",
                  "Odds ratio from 2x2 table" = "odds_ratio_table"
                )),
                selected = "cohens_h"
              )
            ),
            div(
              class = "step-block sample-size-block sample-size-block2",
              h3(sample_size_step_heading(2, "inputs", language)),
              uiOutput("effect_size_proportion_inputs"),
              effect_size_action_button("effect_size_proportion_calculate", language)
            ),
            div(
              class = "step-block sample-size-block sample-size-block3",
              h3(sample_size_step_heading(3, "results", language)),
              uiOutput("effect_size_proportion_results")
            )
          )
        )
      )
    ))
  }
  if (identical(method, "chisquare")) {
    return(tabPanel(
      effect_size_method_title("chisquare", language),
      value = "effect_size_chisquare",
      div(
        class = "page-shell",
        div(
          class = "app-heading",
          h1(effect_size_method_title("chisquare", language)),
          div(sample_size_ui_text(language, "effectsize_subtitle"), class = "app-subtitle")
        ),
        div(
          class = "workspace-panel frequencies-workspace-panel sample-size-workspace-panel",
          style = "min-width:980px;overflow-x:auto;",
          h3(effect_size_panel_heading("chisquare", language)),
          div(
            class = "sample-size-grid",
            div(
              class = "step-block sample-size-block sample-size-block1",
              h3(sample_size_step_heading(1, "effect_size", language)),
              radioButtons(
                "effect_size_chisquare_design",
                label = NULL,
                choices = sample_size_choice_labels(language, c(
                  "Cohen's w from chi-square" = "cohens_w",
                  "Cohen's w from category proportions" = "cohens_w_from_probs",
                  "Cramer's V" = "cramers_v",
                  "Phi coefficient" = "phi"
                )),
                selected = "cohens_w"
              )
            ),
            div(
              class = "step-block sample-size-block sample-size-block2",
              h3(sample_size_step_heading(2, "inputs", language)),
              uiOutput("effect_size_chisquare_inputs"),
              effect_size_action_button("effect_size_chisquare_calculate", language)
            ),
            div(
              class = "step-block sample-size-block sample-size-block3",
              h3(sample_size_step_heading(3, "results", language)),
              uiOutput("effect_size_chisquare_results")
            )
          )
        )
      )
    ))
  }
  if (identical(method, "correlation")) {
    return(tabPanel(
      effect_size_method_title("correlation", language),
      value = "effect_size_correlation",
      div(
        class = "page-shell",
        div(
          class = "app-heading",
          h1(effect_size_method_title("correlation", language)),
          div(sample_size_ui_text(language, "effectsize_subtitle"), class = "app-subtitle")
        ),
        div(
          class = "workspace-panel frequencies-workspace-panel sample-size-workspace-panel",
          style = "min-width:980px;overflow-x:auto;",
          h3(effect_size_panel_heading("correlation", language)),
          div(
            class = "sample-size-grid",
            div(
              class = "step-block sample-size-block sample-size-block1",
              h3(sample_size_step_heading(1, "effect_size", language)),
              radioButtons(
                "effect_size_correlation_design",
                label = NULL,
                choices = sample_size_choice_labels(language, c(
                  "Point-biserial r" = "point_biserial",
                  "Pearson r from t statistic" = "r_from_t",
                  "Pearson r from F statistic" = "r_from_f",
                  "Pearson r from R-squared" = "r_from_r2",
                  "Fisher's z from r" = "fisher_z",
                  "Cohen's q for two correlations" = "cohens_q"
                )),
                selected = "r_from_t"
              )
            ),
            div(
              class = "step-block sample-size-block sample-size-block2",
              h3(sample_size_step_heading(2, "inputs", language)),
              uiOutput("effect_size_correlation_inputs"),
              effect_size_action_button("effect_size_correlation_calculate", language)
            ),
            div(
              class = "step-block sample-size-block sample-size-block3",
              h3(sample_size_step_heading(3, "results", language)),
              uiOutput("effect_size_correlation_results")
            )
          )
        )
      )
    ))
  }
  if (identical(method, "anova")) {
    return(tabPanel(
      effect_size_method_title("anova", language),
      value = "effect_size_anova",
      div(
        class = "page-shell",
        div(
          class = "app-heading",
          h1(effect_size_method_title("anova", language)),
          div(sample_size_ui_text(language, "effectsize_subtitle"), class = "app-subtitle")
        ),
        div(
          class = "workspace-panel frequencies-workspace-panel sample-size-workspace-panel",
          style = "min-width:980px;overflow-x:auto;",
          h3(effect_size_panel_heading("anova", language)),
          div(
            class = "sample-size-grid",
            div(
              class = "step-block sample-size-block sample-size-block1",
              h3(sample_size_step_heading(1, "effect_size", language)),
              radioButtons(
                "effect_size_anova_design",
                label = NULL,
                choices = sample_size_choice_labels(language, c(
                  "Partial eta squared from F" = "partial_eta_from_f",
                  "Cohen's f from eta squared" = "f_from_eta2",
                  "Cohen's f from partial eta squared" = "f_from_partial_eta2"
                )),
                selected = "partial_eta_from_f"
              )
            ),
            div(
              class = "step-block sample-size-block sample-size-block2",
              h3(sample_size_step_heading(2, "inputs", language)),
              uiOutput("effect_size_anova_inputs"),
              effect_size_action_button("effect_size_anova_calculate", language)
            ),
            div(
              class = "step-block sample-size-block sample-size-block3",
              h3(sample_size_step_heading(3, "results", language)),
              uiOutput("effect_size_anova_results")
            )
          )
        )
      )
    ))
  }
  if (identical(method, "ancova")) {
    return(tabPanel(
      effect_size_method_title("ancova", language),
      value = "effect_size_ancova",
      div(
        class = "page-shell",
        div(
          class = "app-heading",
          h1(effect_size_method_title("ancova", language)),
          div(sample_size_ui_text(language, "effectsize_subtitle"), class = "app-subtitle")
        ),
        div(
          class = "workspace-panel frequencies-workspace-panel sample-size-workspace-panel",
          style = "min-width:980px;overflow-x:auto;",
          h3(effect_size_panel_heading("ancova", language)),
          div(
            class = "sample-size-grid",
            div(
              class = "step-block sample-size-block sample-size-block1",
              h3(sample_size_step_heading(1, "effect_size", language)),
              radioButtons(
                "effect_size_ancova_design",
                label = NULL,
                choices = sample_size_choice_labels(language, c(
                  "ANCOVA partial eta squared from F" = "ancova_partial_eta_from_f",
                  "ANCOVA adjusted Cohen's f" = "ancova_adjusted_f",
                  "ANCOVA Cohen's f from partial eta squared" = "ancova_f_from_partial_eta2",
                  "MANOVA Pillai's trace to f2" = "manova_pillai",
                  "MANOVA Wilks' lambda to f2" = "manova_wilks"
                )),
                selected = "ancova_partial_eta_from_f"
              )
            ),
            div(
              class = "step-block sample-size-block sample-size-block2",
              h3(sample_size_step_heading(2, "inputs", language)),
              uiOutput("effect_size_ancova_inputs"),
              effect_size_action_button("effect_size_ancova_calculate", language)
            ),
            div(
              class = "step-block sample-size-block sample-size-block3",
              h3(sample_size_step_heading(3, "results", language)),
              uiOutput("effect_size_ancova_results")
            )
          )
        )
      )
    ))
  }
  if (identical(method, "nonparametric")) {
    return(tabPanel(
      effect_size_method_title("nonparametric", language),
      value = "effect_size_nonparametric",
      div(
        class = "page-shell",
        div(
          class = "app-heading",
          h1(effect_size_method_title("nonparametric", language)),
          div(sample_size_ui_text(language, "effectsize_subtitle"), class = "app-subtitle")
        ),
        div(
          class = "workspace-panel frequencies-workspace-panel sample-size-workspace-panel",
          style = "min-width:980px;overflow-x:auto;",
          h3(effect_size_panel_heading("nonparametric", language)),
          div(
            class = "sample-size-grid",
            div(
              class = "step-block sample-size-block sample-size-block1",
              h3(sample_size_step_heading(1, "effect_size", language)),
              radioButtons(
                "effect_size_nonparametric_design",
                label = NULL,
                choices = sample_size_choice_labels(language, c(
                  "Rank-biserial r from Mann-Whitney U" = "rank_biserial_from_u",
                  "Rank-biserial r for paired Wilcoxon" = "rank_biserial_paired",
                  "Kruskal-Wallis epsilon squared" = "kruskal_epsilon",
                  "Friedman Kendall's W" = "friedman_w"
                )),
                selected = "rank_biserial_from_u"
              )
            ),
            div(
              class = "step-block sample-size-block sample-size-block2",
              h3(sample_size_step_heading(2, "inputs", language)),
              uiOutput("effect_size_nonparametric_inputs"),
              effect_size_action_button("effect_size_nonparametric_calculate", language)
            ),
            div(
              class = "step-block sample-size-block sample-size-block3",
              h3(sample_size_step_heading(3, "results", language)),
              uiOutput("effect_size_nonparametric_results")
            )
          )
        )
      )
    ))
  }
  if (identical(method, "mcnemar")) {
    return(tabPanel(
      effect_size_method_title("mcnemar", language),
      value = "effect_size_mcnemar",
      div(
        class = "page-shell",
        div(
          class = "app-heading",
          h1(effect_size_method_title("mcnemar", language)),
          div(sample_size_ui_text(language, "effectsize_subtitle"), class = "app-subtitle")
        ),
        div(
          class = "workspace-panel frequencies-workspace-panel sample-size-workspace-panel",
          style = "min-width:980px;overflow-x:auto;",
          h3(effect_size_panel_heading("mcnemar", language)),
          div(
            class = "sample-size-grid",
            div(
              class = "step-block sample-size-block sample-size-block1",
              h3(sample_size_step_heading(1, "effect_size", language)),
              radioButtons(
                "effect_size_mcnemar_design",
                label = NULL,
                choices = sample_size_choice_labels(language, c(
                  "Matched-pair odds ratio from probabilities" = "matched_or_probs",
                  "Matched-pair odds ratio from paired 2x2 table" = "matched_or_counts",
                  "Cohen's g from discordant probabilities" = "cohen_g"
                )),
                selected = "matched_or_probs"
              )
            ),
            div(
              class = "step-block sample-size-block sample-size-block2",
              h3(sample_size_step_heading(2, "inputs", language)),
              uiOutput("effect_size_mcnemar_inputs"),
              effect_size_action_button("effect_size_mcnemar_calculate", language)
            ),
            div(
              class = "step-block sample-size-block sample-size-block3",
              h3(sample_size_step_heading(3, "results", language)),
              uiOutput("effect_size_mcnemar_results")
            )
          )
        )
      )
    ))
  }
  if (identical(method, "regression")) {
    return(tabPanel(
      effect_size_method_title("regression", language),
      value = "effect_size_regression",
      div(
        class = "page-shell",
        div(
          class = "app-heading",
          h1(effect_size_method_title("regression", language)),
          div(sample_size_ui_text(language, "effectsize_subtitle"), class = "app-subtitle")
        ),
        div(
          class = "workspace-panel frequencies-workspace-panel sample-size-workspace-panel",
          style = "min-width:980px;overflow-x:auto;",
          h3(effect_size_panel_heading("regression", language)),
          div(
            class = "sample-size-grid",
            div(
              class = "step-block sample-size-block sample-size-block1",
              h3(sample_size_step_heading(1, "effect_size", language)),
              radioButtons(
                "effect_size_regression_design",
                label = NULL,
                choices = sample_size_choice_labels(language, c(
                  "Multiple regression f2 from R-squared" = "f2_from_r2",
                  "Hierarchical regression f2 from R-squared increase" = "hierarchical_f2",
                  "Logistic regression OR conversion" = "logistic_or",
                  "Moderation interaction f2" = "moderation_f2"
                )),
                selected = "f2_from_r2"
              )
            ),
            div(
              class = "step-block sample-size-block sample-size-block2",
              h3(sample_size_step_heading(2, "inputs", language)),
              uiOutput("effect_size_regression_inputs"),
              effect_size_action_button("effect_size_regression_calculate", language)
            ),
            div(
              class = "step-block sample-size-block sample-size-block3",
              h3(sample_size_step_heading(3, "results", language)),
              uiOutput("effect_size_regression_results")
            )
          )
        )
      )
    ))
  }
  if (identical(method, "gee")) {
    return(tabPanel(
      effect_size_method_title("gee", language),
      value = "effect_size_gee",
      div(
        class = "page-shell",
        div(
          class = "app-heading",
          h1(effect_size_method_title("gee", language)),
          div(sample_size_ui_text(language, "effectsize_subtitle"), class = "app-subtitle")
        ),
        div(
          class = "workspace-panel frequencies-workspace-panel sample-size-workspace-panel",
          style = "min-width:980px;overflow-x:auto;",
          h3(effect_size_panel_heading("gee", language)),
          div(
            class = "sample-size-grid",
            div(
              class = "step-block sample-size-block sample-size-block1",
              h3(sample_size_step_heading(1, "effect_size", language)),
              radioButtons(
                "effect_size_gee_design",
                label = NULL,
                choices = sample_size_choice_labels(language, c(
                  "Follow-up estimated means" = "continuous_followup_means",
                  "Pre-post change means" = "continuous_change_means",
                  "Group x time B" = "continuous_parameter_b",
                  "Continuous outcome supplied d" = "continuous_d",
                  "Binary outcome from proportions" = "binary_props"
                )),
                selected = "continuous_followup_means"
              )
            ),
            div(
              class = "step-block sample-size-block sample-size-block2",
              h3(sample_size_step_heading(2, "inputs", language)),
              uiOutput("effect_size_gee_inputs"),
              effect_size_action_button("effect_size_gee_calculate", language)
            ),
            div(
              class = "step-block sample-size-block sample-size-block3",
              h3(sample_size_step_heading(3, "results", language)),
              uiOutput("effect_size_gee_results")
            )
          )
        )
      )
    ))
  }
  if (identical(method, "glmm")) {
    return(tabPanel(
      effect_size_method_title("glmm", language),
      value = "effect_size_glmm",
      div(
        class = "page-shell",
        div(
          class = "app-heading",
          h1(effect_size_method_title("glmm", language)),
          div(sample_size_ui_text(language, "effectsize_subtitle"), class = "app-subtitle")
        ),
        div(
          class = "workspace-panel frequencies-workspace-panel sample-size-workspace-panel",
          style = "min-width:980px;overflow-x:auto;",
          h3(effect_size_panel_heading("glmm", language)),
          div(
            class = "sample-size-grid",
            div(
              class = "step-block sample-size-block sample-size-block1",
              h3(sample_size_step_heading(1, "effect_size", language)),
              radioButtons(
                "effect_size_glmm_design",
                label = NULL,
                choices = sample_size_choice_labels(language, c(
                  "Binary logit fixed effect" = "binary_logit",
                  "Binary outcome probabilities" = "binary_probabilities",
                  "Count log-link fixed effect" = "count_log",
                  "Count outcome rates" = "count_rates",
                  "Gaussian fixed effect" = "continuous_gaussian"
                )),
                selected = "binary_logit"
              )
            ),
            div(
              class = "step-block sample-size-block sample-size-block2",
              h3(sample_size_step_heading(2, "inputs", language)),
              uiOutput("effect_size_glmm_inputs"),
              effect_size_action_button("effect_size_glmm_calculate", language)
            ),
            div(
              class = "step-block sample-size-block sample-size-block3",
              h3(sample_size_step_heading(3, "results", language)),
              uiOutput("effect_size_glmm_results")
            )
          )
        )
      )
    ))
  }
  if (identical(method, "lmm")) {
    return(tabPanel(
      effect_size_method_title("lmm", language),
      value = "effect_size_lmm",
      div(
        class = "page-shell",
        div(
          class = "app-heading",
          h1(effect_size_method_title("lmm", language)),
          div(sample_size_ui_text(language, "effectsize_subtitle"), class = "app-subtitle")
        ),
        div(
          class = "workspace-panel frequencies-workspace-panel sample-size-workspace-panel",
          style = "min-width:980px;overflow-x:auto;",
          h3(effect_size_panel_heading("lmm", language)),
          div(
            class = "sample-size-grid",
            div(
              class = "step-block sample-size-block sample-size-block1",
              h3(sample_size_step_heading(1, "effect_size", language)),
              radioButtons(
                "effect_size_lmm_design",
                label = NULL,
                choices = sample_size_choice_labels(language, c(
                  "Simple standardized fixed effect" = "simple_fixed",
                  "GLIMMPSE-style mean vectors" = "glimmpse_vectors",
                  "SPSS LMM output (F, df, covariance)" = "spss_output"
                )),
                selected = "simple_fixed"
              )
            ),
            div(
              class = "step-block sample-size-block sample-size-block2",
              h3(sample_size_step_heading(2, "inputs", language)),
              uiOutput("effect_size_lmm_inputs"),
              effect_size_action_button("effect_size_lmm_calculate", language)
            ),
            div(
              class = "step-block sample-size-block sample-size-block3",
              h3(sample_size_step_heading(3, "results", language)),
              uiOutput("effect_size_lmm_results")
            )
          )
        )
      )
    ))
  }
  if (identical(method, "survival")) {
    return(tabPanel(
      effect_size_method_title("survival", language),
      value = "effect_size_survival",
      div(
        class = "page-shell",
        div(
          class = "app-heading",
          h1(effect_size_method_title("survival", language)),
          div(sample_size_ui_text(language, "effectsize_subtitle"), class = "app-subtitle")
        ),
        div(
          class = "workspace-panel frequencies-workspace-panel sample-size-workspace-panel",
          style = "min-width:980px;overflow-x:auto;",
          h3(effect_size_panel_heading("survival", language)),
          div(
            class = "sample-size-grid",
            div(
              class = "step-block sample-size-block sample-size-block1",
              h3(sample_size_step_heading(1, "effect_size", language)),
              radioButtons(
                "effect_size_survival_design",
                label = NULL,
                choices = sample_size_choice_labels(language, c(
                  "Hazard ratio to log hazard ratio" = "hazard_ratio"
                )),
                selected = "hazard_ratio"
              )
            ),
            div(
              class = "step-block sample-size-block sample-size-block2",
              h3(sample_size_step_heading(2, "inputs", language)),
              uiOutput("effect_size_survival_inputs"),
              effect_size_action_button("effect_size_survival_calculate", language)
            ),
            div(
              class = "step-block sample-size-block sample-size-block3",
              h3(sample_size_step_heading(3, "results", language)),
              uiOutput("effect_size_survival_results")
            )
          )
        )
      )
    ))
  }
  if (identical(method, "equivalence")) {
    return(tabPanel(
      effect_size_method_title("equivalence", language),
      value = "effect_size_equivalence",
      div(
        class = "page-shell",
        div(
          class = "app-heading",
          h1(effect_size_method_title("equivalence", language)),
          div(sample_size_ui_text(language, "effectsize_subtitle"), class = "app-subtitle")
        ),
        div(
          class = "workspace-panel frequencies-workspace-panel sample-size-workspace-panel",
          style = "min-width:980px;overflow-x:auto;",
          h3(effect_size_panel_heading("equivalence", language)),
          div(
            class = "sample-size-grid",
            div(
              class = "step-block sample-size-block sample-size-block1",
              h3(sample_size_step_heading(1, "effect_size", language)),
              radioButtons(
                "effect_size_equivalence_outcome",
                label = NULL,
                choices = sample_size_choice_labels(language, c(
                  "Mean difference margin distance" = "mean",
                  "Proportion difference margin distance" = "proportion"
                )),
                selected = "mean"
              )
            ),
            div(
              class = "step-block sample-size-block sample-size-block2",
              h3(sample_size_step_heading(2, "inputs", language)),
              uiOutput("effect_size_equivalence_inputs"),
              effect_size_action_button("effect_size_equivalence_calculate", language)
            ),
            div(
              class = "step-block sample-size-block sample-size-block3",
              h3(sample_size_step_heading(3, "results", language)),
              uiOutput("effect_size_equivalence_results")
            )
          )
        )
      )
    ))
  }
  if (identical(method, "diagnostic")) {
    return(tabPanel(
      effect_size_method_title("diagnostic", language),
      value = "effect_size_diagnostic",
      div(
        class = "page-shell",
        div(
          class = "app-heading",
          h1(effect_size_method_title("diagnostic", language)),
          div(sample_size_ui_text(language, "effectsize_subtitle"), class = "app-subtitle")
        ),
        div(
          class = "workspace-panel frequencies-workspace-panel sample-size-workspace-panel",
          style = "min-width:980px;overflow-x:auto;",
          h3(effect_size_panel_heading("diagnostic", language)),
          div(
            class = "sample-size-grid",
            div(
              class = "step-block sample-size-block sample-size-block1",
              h3(sample_size_step_heading(1, "effect_size", language)),
              radioButtons(
                "effect_size_diagnostic_design",
                label = NULL,
                choices = sample_size_choice_labels(language, c(
                  "ROC AUC vs null" = "auc"
                )),
                selected = "auc"
              )
            ),
            div(
              class = "step-block sample-size-block sample-size-block2",
              h3(sample_size_step_heading(2, "inputs", language)),
              uiOutput("effect_size_diagnostic_inputs"),
              effect_size_action_button("effect_size_diagnostic_calculate", language)
            ),
            div(
              class = "step-block sample-size-block sample-size-block3",
              h3(sample_size_step_heading(3, "results", language)),
              uiOutput("effect_size_diagnostic_results")
            )
          )
        )
      )
    ))
  }
  if (identical(method, "rates")) {
    return(tabPanel(
      effect_size_method_title("rates", language),
      value = "effect_size_rates",
      div(
        class = "page-shell",
        div(
          class = "app-heading",
          h1(effect_size_method_title("rates", language)),
          div(sample_size_ui_text(language, "effectsize_subtitle"), class = "app-subtitle")
        ),
        div(
          class = "workspace-panel frequencies-workspace-panel sample-size-workspace-panel",
          style = "min-width:980px;overflow-x:auto;",
          h3(effect_size_panel_heading("rates", language)),
          div(
            class = "sample-size-grid",
            div(
              class = "step-block sample-size-block sample-size-block1",
              h3(sample_size_step_heading(1, "effect_size", language)),
              radioButtons(
                "effect_size_rates_design",
                label = NULL,
                choices = sample_size_choice_labels(language, c(
                  "Poisson incidence rate ratio" = "poisson_irr",
                  "Negative binomial incidence rate ratio" = "negative_binomial_irr",
                  "Gamma mean ratio" = "gamma_mean_ratio"
                )),
                selected = "poisson_irr"
              )
            ),
            div(
              class = "step-block sample-size-block sample-size-block2",
              h3(sample_size_step_heading(2, "inputs", language)),
              uiOutput("effect_size_rates_inputs"),
              effect_size_action_button("effect_size_rates_calculate", language)
            ),
            div(
              class = "step-block sample-size-block sample-size-block3",
              h3(sample_size_step_heading(3, "results", language)),
              uiOutput("effect_size_rates_results")
            )
          )
        )
      )
    ))
  }
  if (identical(method, "cluster")) {
    return(tabPanel(
      effect_size_method_title("cluster", language),
      value = "effect_size_cluster",
      div(
        class = "page-shell",
        div(
          class = "app-heading",
          h1(effect_size_method_title("cluster", language)),
          div(sample_size_ui_text(language, "effectsize_subtitle"), class = "app-subtitle")
        ),
        div(
          class = "workspace-panel frequencies-workspace-panel sample-size-workspace-panel",
          style = "min-width:980px;overflow-x:auto;",
          h3(effect_size_panel_heading("cluster", language)),
          div(
            class = "sample-size-grid",
            div(
              class = "step-block sample-size-block sample-size-block1",
              h3(sample_size_step_heading(1, "effect_size", language)),
              radioButtons(
                "effect_size_cluster_design",
                label = NULL,
                choices = sample_size_choice_labels(language, c(
                  "Parallel continuous outcome" = "parallel_continuous",
                  "Parallel binary outcome" = "parallel_binary",
                  "Stepped-wedge continuous outcome" = "stepped_wedge"
                )),
                selected = "parallel_continuous"
              )
            ),
            div(
              class = "step-block sample-size-block sample-size-block2",
              h3(sample_size_step_heading(2, "inputs", language)),
              uiOutput("effect_size_cluster_inputs"),
              effect_size_action_button("effect_size_cluster_calculate", language)
            ),
            div(
              class = "step-block sample-size-block sample-size-block3",
              h3(sample_size_step_heading(3, "results", language)),
              uiOutput("effect_size_cluster_results")
            )
          )
        )
      )
    ))
  }
  if (identical(method, "precision")) {
    return(tabPanel(
      effect_size_method_title("precision", language),
      value = "effect_size_precision",
      div(
        class = "page-shell",
        div(
          class = "app-heading",
          h1(effect_size_method_title("precision", language)),
          div(sample_size_ui_text(language, "effectsize_subtitle"), class = "app-subtitle")
        ),
        div(
          class = "workspace-panel frequencies-workspace-panel sample-size-workspace-panel",
          style = "min-width:980px;overflow-x:auto;",
          h3(effect_size_panel_heading("precision", language)),
          div(
            class = "sample-size-grid",
            div(
              class = "step-block sample-size-block sample-size-block1",
              h3(sample_size_step_heading(1, "effect_size", language)),
              radioButtons(
                "effect_size_precision_parameter",
                label = NULL,
                choices = sample_size_choice_labels(language, c(
                  "Mean CI precision" = "mean",
                  "Proportion CI precision" = "proportion",
                  "Correlation CI precision" = "correlation"
                )),
                selected = "mean"
              )
            ),
            div(
              class = "step-block sample-size-block sample-size-block2",
              h3(sample_size_step_heading(2, "inputs", language)),
              uiOutput("effect_size_precision_inputs"),
              effect_size_action_button("effect_size_precision_calculate", language)
            ),
            div(
              class = "step-block sample-size-block sample-size-block3",
              h3(sample_size_step_heading(3, "results", language)),
              uiOutput("effect_size_precision_results")
            )
          )
        )
      )
    ))
  }
  if (identical(method, "reliability")) {
    return(tabPanel(
      effect_size_method_title("reliability", language),
      value = "effect_size_reliability",
      div(
        class = "page-shell",
        div(
          class = "app-heading",
          h1(effect_size_method_title("reliability", language)),
          div(sample_size_ui_text(language, "effectsize_subtitle"), class = "app-subtitle")
        ),
        div(
          class = "workspace-panel frequencies-workspace-panel sample-size-workspace-panel",
          style = "min-width:980px;overflow-x:auto;",
          h3(effect_size_panel_heading("reliability", language)),
          div(
            class = "sample-size-grid",
            div(
              class = "step-block sample-size-block sample-size-block1",
              h3(sample_size_step_heading(1, "effect_size", language)),
              radioButtons(
                "effect_size_reliability_design",
                label = NULL,
                choices = sample_size_choice_labels(language, c(
                  "Cohen's kappa" = "kappa"
                )),
                selected = "kappa"
              )
            ),
            div(
              class = "step-block sample-size-block sample-size-block2",
              h3(sample_size_step_heading(2, "inputs", language)),
              uiOutput("effect_size_reliability_inputs"),
              effect_size_action_button("effect_size_reliability_calculate", language)
            ),
            div(
              class = "step-block sample-size-block sample-size-block3",
              h3(sample_size_step_heading(3, "results", language)),
              uiOutput("effect_size_reliability_results")
            )
          )
        )
      )
    ))
  }
  if (identical(method, "sem")) {
    return(tabPanel(
      effect_size_method_title("sem", language),
      value = "effect_size_sem",
      div(
        class = "page-shell",
        div(
          class = "app-heading",
          h1(effect_size_method_title("sem", language)),
          div(sample_size_ui_text(language, "effectsize_subtitle"), class = "app-subtitle")
        ),
        div(
          class = "workspace-panel frequencies-workspace-panel sample-size-workspace-panel",
          style = "min-width:980px;overflow-x:auto;",
          h3(effect_size_panel_heading("sem", language)),
          div(
            class = "sample-size-grid",
            div(
              class = "step-block sample-size-block sample-size-block1",
              h3(sample_size_step_heading(1, "effect_size", language)),
              radioButtons(
                "effect_size_sem_design",
                label = NULL,
                choices = sample_size_choice_labels(language, c(
                  "Standardized parameter" = "parameter"
                )),
                selected = "parameter"
              )
            ),
            div(
              class = "step-block sample-size-block sample-size-block2",
              h3(sample_size_step_heading(2, "inputs", language)),
              uiOutput("effect_size_sem_inputs"),
              effect_size_action_button("effect_size_sem_calculate", language)
            ),
            div(
              class = "step-block sample-size-block sample-size-block3",
              h3(sample_size_step_heading(3, "results", language)),
              uiOutput("effect_size_sem_results")
            )
          )
        )
      )
    ))
  }
  sample_size_analysis_panel("effectsize")
}

effect_size_ttest_inputs_ui <- function(input, language = statedu_initial_language()) {
  lbl <- function(label) sample_size_label(language, label)
  design <- sample_size_choice(input$effect_size_ttest_design, "independent_means")
  if (identical(design, "independent_t_n")) {
    return(tagList(
      textInput("effect_size_ttest_t", lbl("t statistic"), value = "2.50"),
      textInput("effect_size_ttest_n1", lbl("Group 1 n"), value = "50"),
      textInput("effect_size_ttest_n2", lbl("Group 2 n"), value = "50")
    ))
  }
  if (identical(design, "independent_t_df_equal")) {
    return(tagList(
      textInput("effect_size_ttest_t", lbl("t statistic"), value = "2.50"),
      textInput("effect_size_ttest_df", lbl("Degrees of freedom"), value = "98")
    ))
  }
  if (identical(design, "one_sample_t")) {
    return(tagList(
      textInput("effect_size_ttest_t", lbl("t statistic"), value = "3.50"),
      textInput("effect_size_ttest_n", lbl("Sample size"), value = "49")
    ))
  }
  if (identical(design, "paired_t")) {
    return(tagList(
      textInput("effect_size_ttest_t", lbl("t statistic"), value = "3.50"),
      textInput("effect_size_ttest_n", lbl("Number of pairs"), value = "49")
    ))
  }
  if (identical(design, "independent_r")) {
    return(textInput("effect_size_ttest_r", lbl("Point-biserial r"), value = "0.25"))
  }
  if (identical(design, "paired_means")) {
    return(tagList(
      textInput("effect_size_ttest_mean_difference", lbl("Mean paired difference"), value = "5"),
      textInput("effect_size_ttest_sd_difference", lbl("SD of paired differences"), value = "10")
    ))
  }
  if (identical(design, "one_sample_mean")) {
    return(tagList(
      textInput("effect_size_ttest_mean1", lbl("Sample mean"), value = "105"),
      textInput("effect_size_ttest_null_mean", lbl("Null mean"), value = "100"),
      textInput("effect_size_ttest_sd1", lbl("SD"), value = "10")
    ))
  }
  tagList(
    textInput("effect_size_ttest_mean1", lbl("Group 1 mean"), value = "105"),
    textInput("effect_size_ttest_mean2", lbl("Group 2 mean"), value = "100"),
    textInput("effect_size_ttest_sd1", lbl("Group 1 SD"), value = "10"),
    textInput("effect_size_ttest_sd2", lbl("Group 2 SD"), value = "10"),
    textInput("effect_size_ttest_n1", lbl("Group 1 n"), value = "50"),
    textInput("effect_size_ttest_n2", lbl("Group 2 n"), value = "50")
  )
}

effect_size_proportion_inputs_ui <- function(input, language = statedu_initial_language()) {
  lbl <- function(label) sample_size_label(language, label)
  design <- sample_size_choice(input$effect_size_proportion_design, "cohens_h")
  if (identical(design, "odds_ratio_table")) {
    return(tagList(
      textInput("effect_size_proportion_event1", lbl("Group 1 events"), value = "30"),
      textInput("effect_size_proportion_nonevent1", lbl("Group 1 non-events"), value = "70"),
      textInput("effect_size_proportion_event2", lbl("Group 2 events"), value = "15"),
      textInput("effect_size_proportion_nonevent2", lbl("Group 2 non-events"), value = "85")
    ))
  }
  tagList(
    textInput("effect_size_proportion_p1", lbl("Proportion 1"), value = "0.50"),
    textInput("effect_size_proportion_p2", lbl("Proportion 2"), value = "0.65")
  )
}

effect_size_correlation_inputs_ui <- function(input, language = statedu_initial_language()) {
  lbl <- function(label) sample_size_label(language, label)
  design <- sample_size_choice(input$effect_size_correlation_design, "r_from_t")
  if (identical(design, "point_biserial")) {
    return(textInput("effect_size_correlation_r", lbl("Point-biserial r"), value = "0.25"))
  }
  if (identical(design, "r_from_t")) {
    return(tagList(
      textInput("effect_size_correlation_t", lbl("t statistic"), value = "2.5"),
      textInput("effect_size_correlation_df", lbl("Degrees of freedom"), value = "98")
    ))
  }
  if (identical(design, "r_from_f")) {
    return(tagList(
      textInput("effect_size_correlation_f", lbl("F statistic"), value = "6.25"),
      textInput("effect_size_correlation_df", lbl("Error degrees of freedom"), value = "98")
    ))
  }
  if (identical(design, "r_from_r2")) {
    return(textInput("effect_size_correlation_r2", lbl("R-squared"), value = "0.10"))
  }
  if (identical(design, "cohens_q")) {
    return(tagList(
      textInput("effect_size_correlation_r1", lbl("Correlation 1"), value = "0.50"),
      textInput("effect_size_correlation_r2_compare", lbl("Correlation 2"), value = "0.30")
    ))
  }
  textInput("effect_size_correlation_r", lbl("Correlation r"), value = "0.30")
}

effect_size_anova_inputs_ui <- function(input, language = statedu_initial_language()) {
  lbl <- function(label) sample_size_label(language, label)
  design <- sample_size_choice(input$effect_size_anova_design, "f_from_eta2")
  if (identical(design, "f_from_eta2")) {
    return(textInput("effect_size_anova_eta2", lbl("Eta squared"), value = "0.06"))
  }
  if (identical(design, "f_from_partial_eta2")) {
    return(textInput("effect_size_anova_partial_eta2", lbl("Partial eta squared"), value = "0.06"))
  }
  tagList(
    textInput("effect_size_anova_f", lbl("F statistic"), value = "4.5"),
    textInput("effect_size_anova_groups", lbl("Number of groups"), value = "3"),
    textInput("effect_size_anova_total_n", lbl("Total sample size"), value = "90")
  )
}

effect_size_ancova_inputs_ui <- function(input, language = statedu_initial_language()) {
  lbl <- function(label) sample_size_label(language, label)
  design <- sample_size_choice(input$effect_size_ancova_design, "ancova_partial_eta_from_f")
  if (identical(design, "ancova_adjusted_f")) {
    return(tagList(
      textInput("effect_size_ancova_f", lbl("Unadjusted Cohen's f"), value = "0.25"),
      textInput("effect_size_ancova_covariate_r2", lbl("Covariate R-squared"), value = "0.30")
    ))
  }
  if (identical(design, "ancova_f_from_partial_eta2")) {
    return(textInput("effect_size_ancova_partial_eta2", lbl("Partial eta squared"), value = "0.06"))
  }
  if (identical(design, "manova_pillai")) {
    return(textInput("effect_size_ancova_pillai", lbl("Pillai's trace V"), value = "0.10"))
  }
  if (identical(design, "manova_wilks")) {
    return(tagList(
      textInput("effect_size_ancova_wilks", lbl("Wilks' lambda"), value = "0.90"),
      textInput("effect_size_ancova_dependent_variables", lbl("Number of dependent variables"), value = "2"),
      textInput("effect_size_ancova_groups", lbl("Number of groups"), value = "3")
    ))
  }
  tagList(
    textInput("effect_size_ancova_f_statistic", lbl("F statistic"), value = "4.5"),
    textInput("effect_size_ancova_groups", lbl("Number of groups"), value = "3"),
    textInput("effect_size_ancova_total_n", lbl("Total sample size"), value = "90")
  )
}

effect_size_nonparametric_inputs_ui <- function(input, language = statedu_initial_language()) {
  lbl <- function(label) sample_size_label(language, label)
  design <- sample_size_choice(input$effect_size_nonparametric_design, "rank_biserial_from_u")
  if (identical(design, "rank_biserial_from_u")) {
    return(tagList(
      textInput("effect_size_nonparametric_u", lbl("Mann-Whitney U"), value = "700"),
      textInput("effect_size_nonparametric_n1", lbl("Group 1 n"), value = "50"),
      textInput("effect_size_nonparametric_n2", lbl("Group 2 n"), value = "50")
    ))
  }
  if (identical(design, "rank_biserial_paired")) {
    return(tagList(
      textInput("effect_size_nonparametric_w_positive", lbl("Positive rank sum W+"), value = "350"),
      textInput("effect_size_nonparametric_w_negative", lbl("Negative rank sum W-"), value = "150")
    ))
  }
  if (identical(design, "kruskal_epsilon")) {
    return(tagList(
      textInput("effect_size_nonparametric_h", lbl("Kruskal-Wallis H"), value = "10"),
      textInput("effect_size_nonparametric_n", lbl("Total sample size"), value = "90"),
      textInput("effect_size_nonparametric_groups", lbl("Groups"), value = "3")
    ))
  }
  tagList(
    textInput("effect_size_nonparametric_chi_square", lbl("Friedman chi-square"), value = "12"),
    textInput("effect_size_nonparametric_n", lbl("Participants"), value = "30"),
    textInput("effect_size_nonparametric_measurements", lbl("Measurements"), value = "3")
  )
}

effect_size_mcnemar_inputs_ui <- function(input, language = statedu_initial_language()) {
  design <- sample_size_choice(input$effect_size_mcnemar_design, "matched_or_probs")
  if (identical(design, "matched_or_counts")) {
    return(tagList(
      textInput("effect_size_mcnemar_b", lbl("b: negative to positive pairs"), value = "20"),
      textInput("effect_size_mcnemar_c", lbl("c: positive to negative pairs"), value = "10")
    ))
  }
  tagList(
    textInput("effect_size_mcnemar_p01", lbl("p01: negative to positive"), value = "0.20"),
    textInput("effect_size_mcnemar_p10", lbl("p10: positive to negative"), value = "0.10")
  )
}

effect_size_regression_inputs_ui <- function(input, language = statedu_initial_language()) {
  lbl <- function(label) sample_size_label(language, label)
  design <- sample_size_choice(input$effect_size_regression_design, "f2_from_r2")
  if (identical(design, "hierarchical_f2")) {
    return(tagList(
      textInput("effect_size_regression_full_r2", lbl("Full model R-squared"), value = "0.25"),
      textInput("effect_size_regression_reduced_r2", lbl("Reduced model R-squared"), value = "0.10")
    ))
  }
  if (identical(design, "logistic_or")) {
    return(textInput("effect_size_regression_or", lbl("Odds ratio"), value = "1.80"))
  }
  if (identical(design, "moderation_f2")) {
    return(textInput("effect_size_regression_delta_r2", lbl("Interaction delta R-squared"), value = "0.05"))
  }
  textInput("effect_size_regression_r2", lbl("R-squared"), value = "0.13")
}

effect_size_gee_sd_inputs_ui <- function(input, language = statedu_initial_language()) {
  lbl <- function(label) sample_size_label(language, label)
  mode <- sample_size_choice(input$effect_size_gee_sd_mode, "direct")
  tagList(
    selectInput(
      "effect_size_gee_sd_mode",
      lbl("Common SD input"),
      choices = stats::setNames(c("direct", "pooled"), c(
        statedu_text(language, "Enter common outcome SD", statedu_utf8("eab3b5ed86b520ed919ceca480ed8eb8ecb0a820eca781eca09120ec9e85eba0a5")),
        statedu_text(language, "Calculate pooled SD from group SDs", statedu_utf8("eca791eb8ba820ed919ceca480ed8eb8ecb0a8eba19c20ed95a9eb8f9920ed919ceca480ed8eb8ecb0a820eab384ec82b0"))
      )),
      selected = mode
    ),
    if (identical(mode, "pooled")) {
      tagList(
        textInput("effect_size_gee_n1", lbl("Group 1 n"), value = "50"),
        textInput("effect_size_gee_sd1", lbl("Group 1 SD"), value = "1"),
        textInput("effect_size_gee_n2", lbl("Group 2 n"), value = "50"),
        textInput("effect_size_gee_sd2", lbl("Group 2 SD"), value = "1")
      )
    } else {
      textInput("effect_size_gee_sd", lbl("Common outcome SD"), value = "1")
    }
  )
}

effect_size_gee_inputs_ui <- function(input, language = statedu_initial_language()) {
  lbl <- function(label) sample_size_label(language, label)
  design <- sample_size_choice(input$effect_size_gee_design, "continuous_followup_means")
  tagList(
    if (identical(design, "continuous_means") || identical(design, "continuous_followup_means")) {
      tagList(
        textInput("effect_size_gee_mean1", lbl("Group 1 estimated mean"), value = "0.50"),
        textInput("effect_size_gee_mean2", lbl("Group 2 estimated mean"), value = "0"),
        effect_size_gee_sd_inputs_ui(input, language)
      )
    } else if (identical(design, "continuous_change_means")) {
      tagList(
        textInput("effect_size_gee_pre_mean1", lbl("Group 1 pre mean"), value = "0"),
        textInput("effect_size_gee_post_mean1", lbl("Group 1 post mean"), value = "0.50"),
        textInput("effect_size_gee_pre_mean2", lbl("Group 2 pre mean"), value = "0"),
        textInput("effect_size_gee_post_mean2", lbl("Group 2 post mean"), value = "0"),
        effect_size_gee_sd_inputs_ui(input, language)
      )
    } else if (identical(design, "continuous_parameter_b")) {
      tagList(
      textInput("effect_size_gee_coefficient", lbl("Group x time parameter estimate B"), value = "0.50"),
        effect_size_gee_sd_inputs_ui(input, language)
      )
    } else if (identical(design, "continuous_d")) {
      textInput("effect_size_gee_d", lbl("Effect size d"), value = "0.50")
    } else {
      tagList(
        textInput("effect_size_gee_p1", lbl("Proportion 1"), value = "0.50"),
        textInput("effect_size_gee_p2", lbl("Proportion 2"), value = "0.65")
      )
    }
  )
}

effect_size_glmm_inputs_ui <- function(input, language = statedu_initial_language()) {
  lbl <- function(label) sample_size_label(language, label)
  design <- sample_size_choice(input$effect_size_glmm_design, "binary_logit")
  if (identical(design, "binary_logit")) {
    scale <- sample_size_choice(input$effect_size_glmm_binary_scale, "coefficient")
    return(tagList(
      selectInput(
        "effect_size_glmm_binary_scale",
        lbl("Input scale"),
        choices = sample_size_choice_labels(language, c("Logit coefficient B" = "coefficient", "Odds ratio" = "odds_ratio")),
        selected = scale
      ),
      if (identical(scale, "odds_ratio")) {
        textInput("effect_size_glmm_or", lbl("Odds ratio"), value = "1.80")
      } else {
        textInput("effect_size_glmm_coefficient", lbl("Logit fixed-effect coefficient B"), value = "0.588")
      }
    ))
  }
  if (identical(design, "binary_probabilities")) {
    return(tagList(
      textInput("effect_size_glmm_p1", lbl("Proportion 1"), value = "0.65"),
      textInput("effect_size_glmm_p2", lbl("Proportion 2"), value = "0.50")
    ))
  }
  if (identical(design, "count_log")) {
    scale <- sample_size_choice(input$effect_size_glmm_count_scale, "coefficient")
    return(tagList(
      selectInput(
        "effect_size_glmm_count_scale",
        lbl("Input scale"),
        choices = sample_size_choice_labels(language, c("Log fixed-effect coefficient B" = "coefficient", "Incidence rate ratio" = "incidence_rate_ratio")),
        selected = scale
      ),
      if (identical(scale, "incidence_rate_ratio")) {
        textInput("effect_size_glmm_irr", lbl("Incidence rate ratio"), value = "1.50")
      } else {
        textInput("effect_size_glmm_coefficient", lbl("Log fixed-effect coefficient B"), value = "0.405")
      }
    ))
  }
  if (identical(design, "count_rates")) {
    return(tagList(
      textInput("effect_size_glmm_rate1", lbl("Rate 1"), value = "1.50"),
      textInput("effect_size_glmm_rate2", lbl("Rate 2"), value = "1.00")
    ))
  }
  tagList(
    textInput("effect_size_glmm_coefficient", lbl("Fixed-effect coefficient B"), value = "0.50"),
    textInput("effect_size_glmm_sd", lbl("Residual SD"), value = "1")
  )
}

effect_size_lmm_inputs_ui <- function(input, language = statedu_initial_language()) {
  lbl <- function(label) sample_size_label(language, label)
  design <- sample_size_choice(input$effect_size_lmm_design, "simple_fixed")
  lmm_design <- sample_size_choice(input$effect_size_lmm_lmm_design, "two_group_repeated")
  if (identical(design, "spss_output")) {
    return(tagList(
      h4("Omnibus fixed effect"),
      textInput("effect_size_lmm_f_statistic", lbl("F statistic"), value = "28.061"),
      textInput("effect_size_lmm_df_effect", lbl("Numerator df"), value = "3"),
      textInput("effect_size_lmm_df_error", lbl("Denominator df"), value = "23.057"),
      h4("Optional pairwise comparison"),
      textInput("effect_size_lmm_mean_difference", lbl("Mean difference (I - J)"), value = "0.824"),
      textInput("effect_size_lmm_variance_i", lbl("Variance at time I"), value = "0.326"),
      textInput("effect_size_lmm_variance_j", lbl("Variance at time J"), value = "0.199"),
      textInput("effect_size_lmm_covariance_ij", lbl("Covariance I,J"), value = "0.117")
    ))
  }
  tagList(
    selectInput(
      "effect_size_lmm_lmm_design",
      lbl("LMM design"),
      choices = sample_size_choice_labels(language, c(
        "Two-group repeated (Group x Time)" = "two_group_repeated",
        "One-group repeated (Time slope)" = "one_group_repeated"
      )),
      selected = lmm_design
    ),
    if (identical(design, "simple_fixed")) {
      tagList(
        textInput("effect_size_lmm_effect", lbl("Standardized fixed effect"), value = "0.30"),
        textInput("effect_size_lmm_time_points", lbl("Time points"), value = "3"),
        textInput("effect_size_lmm_icc", lbl("ICC / random intercept proportion"), value = "0.30")
      )
    } else {
      tagList(
        textInput("effect_size_lmm_group1_means", lbl("Group 1 means by time"), value = "0, 0.2, 0.4"),
        if (identical(lmm_design, "two_group_repeated")) {
          textInput("effect_size_lmm_group2_means", lbl("Group 2 means by time"), value = "0, 0.1, 0.8")
        },
        textInput("effect_size_lmm_residual_sd", lbl("Residual SD"), value = "1"),
        selectInput(
          "effect_size_lmm_correlation_structure",
          lbl("Correlation structure"),
          choices = sample_size_choice_labels(language, c("Exchangeable" = "exchangeable", "AR(1)" = "ar1", "Unstructured" = "unstructured")),
          selected = sample_size_choice(input$effect_size_lmm_correlation_structure, "exchangeable")
        ),
        if (identical(sample_size_choice(input$effect_size_lmm_correlation_structure, "exchangeable"), "unstructured")) {
          textInput("effect_size_lmm_correlations", lbl("Pairwise correlations"), value = "0.50, 0.30, 0.50")
        } else {
          textInput("effect_size_lmm_rho", lbl("Correlation rho"), value = "0.50")
        }
      )
    }
  )
}

effect_size_survival_inputs_ui <- function(input, language = statedu_initial_language()) {
  lbl <- function(label) sample_size_label(language, label)
  tagList(
    textInput("effect_size_survival_hr", lbl("Hazard ratio"), value = "0.70")
  )
}

effect_size_equivalence_inputs_ui <- function(input, language = statedu_initial_language()) {
  lbl <- function(label) sample_size_label(language, label)
  outcome <- sample_size_choice(input$effect_size_equivalence_outcome, "mean")
  tagList(
    selectInput(
      "effect_size_equivalence_objective",
      lbl("Objective"),
      choices = sample_size_choice_labels(language, c("Non-inferiority" = "noninferiority", "Equivalence" = "equivalence")),
      selected = sample_size_choice(input$effect_size_equivalence_objective, "noninferiority")
    ),
    textInput("effect_size_equivalence_margin", lbl("Margin"), value = if (identical(outcome, "proportion")) "0.10" else "0.50"),
    if (identical(outcome, "proportion")) {
      tagList(
        textInput("effect_size_equivalence_p1", lbl("Proportion 1"), value = "0.80"),
        textInput("effect_size_equivalence_p2", lbl("Proportion 2"), value = "0.78")
      )
    } else {
      tagList(
      textInput("effect_size_equivalence_difference", lbl("Observed / expected difference"), value = "0"),
        textInput("effect_size_equivalence_sd", lbl("SD"), value = "1")
      )
    }
  )
}

effect_size_diagnostic_inputs_ui <- function(input, language = statedu_initial_language()) {
  lbl <- function(label) sample_size_label(language, label)
  tagList(
    textInput("effect_size_diagnostic_auc", lbl("Expected AUC"), value = "0.75"),
    textInput("effect_size_diagnostic_null_auc", lbl("Null AUC"), value = "0.50")
  )
}

effect_size_rates_inputs_ui <- function(input, language = statedu_initial_language()) {
  lbl <- function(label) sample_size_label(language, label)
  input_scale <- sample_size_choice(input$effect_size_rates_input_scale, "ratio")
  tagList(
    selectInput(
      "effect_size_rates_input_scale",
      lbl("Input scale"),
      choices = sample_size_choice_labels(language, c("Ratio" = "ratio", "Regression coefficient B" = "log_ratio")),
      selected = input_scale
    ),
    if (identical(input_scale, "log_ratio")) {
      textInput("effect_size_rates_log_ratio", lbl("Regression coefficient B"), value = "0.405")
    } else {
      textInput("effect_size_rates_ratio", lbl("Ratio"), value = "1.50")
    }
  )
}

effect_size_cluster_inputs_ui <- function(input, language = statedu_initial_language()) {
  lbl <- function(label) sample_size_label(language, label)
  design <- sample_size_choice(input$effect_size_cluster_design, "parallel_continuous")
  tagList(
    if (identical(design, "parallel_binary")) {
      tagList(
        textInput("effect_size_cluster_p1", lbl("Proportion 1"), value = "0.50"),
        textInput("effect_size_cluster_p2", lbl("Proportion 2"), value = "0.65")
      )
    } else {
      textInput("effect_size_cluster_effect", lbl("Effect size d"), value = if (identical(design, "stepped_wedge")) "0.40" else "0.50")
    },
    textInput("effect_size_cluster_size", lbl(if (identical(design, "stepped_wedge")) "Cluster size per period" else "Cluster size"), value = "20"),
      textInput("effect_size_cluster_icc", lbl("ICC"), value = "0.05"),
    if (identical(design, "stepped_wedge")) {
      textInput("effect_size_cluster_periods", lbl("Periods"), value = "5")
    }
  )
}

effect_size_precision_inputs_ui <- function(input, language = statedu_initial_language()) {
  lbl <- function(label) sample_size_label(language, label)
  parameter <- sample_size_choice(input$effect_size_precision_parameter, "mean")
  tagList(
    textInput("effect_size_precision_half_width", lbl("Desired CI half-width"), value = "0.10"),
    if (identical(parameter, "mean")) {
      tagList(
        textInput("effect_size_precision_estimate", lbl("Expected mean"), value = "1"),
        textInput("effect_size_precision_sd", lbl("SD"), value = "1")
      )
    } else if (identical(parameter, "proportion")) {
      textInput("effect_size_precision_proportion", lbl("Expected proportion"), value = "0.50")
    } else {
      textInput("effect_size_precision_r", lbl("Expected r"), value = "0.30")
    }
  )
}

effect_size_reliability_inputs_ui <- function(input, language = statedu_initial_language()) {
  lbl <- function(label) sample_size_label(language, label)
  tagList(
    textInput("effect_size_reliability_value", lbl("Cohen's kappa"), value = "0.80"),
    textInput("effect_size_reliability_categories", lbl("Categories"), value = "2")
  )
}

effect_size_sem_inputs_ui <- function(input, language = statedu_initial_language()) {
  lbl <- function(label) sample_size_label(language, label)
  tagList(
    selectInput(
      "effect_size_sem_parameter_type",
      lbl("Parameter type"),
      choices = sample_size_choice_labels(language, c(
        "Standardized loading" = "loading",
        "Standardized path" = "path",
        "Latent correlation" = "correlation"
      )),
      selected = sample_size_choice(input$effect_size_sem_parameter_type, "path")
    ),
    textInput("effect_size_sem_parameter", lbl("Expected standardized parameter"), value = "0.30")
  )
}

effect_size_chisquare_inputs_ui <- function(input, language = statedu_initial_language()) {
  lbl <- function(label) sample_size_label(language, label)
  design <- sample_size_choice(input$effect_size_chisquare_design, "cohens_w")
  if (identical(design, "cohens_w_from_probs")) {
    return(tagList(
      textInput("effect_size_chisquare_observed", lbl("Observed proportions"), value = "0.20, 0.50, 0.30"),
      textInput("effect_size_chisquare_expected", lbl("Expected proportions"), value = "0.33, 0.33, 0.34")
    ))
  }
  tagList(
    textInput("effect_size_chisquare_statistic", lbl("Chi-square statistic"), value = "10"),
    textInput("effect_size_chisquare_n", lbl("Sample size"), value = "100"),
    if (identical(design, "cramers_v")) {
      tagList(
        textInput("effect_size_chisquare_rows", lbl("Rows"), value = "2"),
        textInput("effect_size_chisquare_columns", lbl("Columns"), value = "3")
      )
    } else if (identical(design, "cohens_w")) {
      tagList(
        textInput("effect_size_chisquare_rows", lbl("Rows"), value = "2"),
        textInput("effect_size_chisquare_columns", lbl("Columns"), value = "2")
      )
    }
  )
}

sample_size_analysis_panel <- function(method, language = statedu_initial_language()) {
  labels <- c(
    sample_size_method_labels(language),
    effectsize = statedu_text(language, "Effect Size Calculator", statedu_utf8("ed9aa8eab3bced81aceab8b020eab384ec82b0eab8b0"))
  )
  title <- labels[[method]] %||% "Sample Size"
  is_effect_size <- identical(method, "effectsize")
  target_choices <- sample_size_target_choices(method, language)
  tabPanel(
    title,
    value = paste0("sample_size_", method),
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(title),
        div(
          if (is_effect_size) sample_size_ui_text(language, "effectsize_subtitle") else sample_size_ui_text(language, "sample_size_subtitle"),
          class = "app-subtitle"
        )
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel sample-size-workspace-panel",
        style = "min-width:980px;overflow-x:auto;",
        singleton(tags$script(HTML(
          "Shiny.addCustomMessageHandler('sample-size-progress', function(message) {
             var root = document.getElementById(message.id);
             if (!root) return;
             var bar = root.querySelector('.sample-size-progress-bar');
             var text = root.querySelector('.sample-size-progress-text');
             var value = Math.max(0, Math.min(100, Math.round((message.value || 0) * 100)));
             if (bar) {
               bar.style.width = value + '%';
               bar.setAttribute('aria-valuenow', value);
             }
             if (text) text.textContent = (message.text || 'Calculating...') + ' ' + value + '%';
           });"
        ))),
        h3(if (is_effect_size) title else paste(title, sample_size_ui_text(language, "sample_size"))),
        div(
          class = "sample-size-grid",
          div(
            class = "step-block sample-size-block sample-size-block1",
            h3(if (is_effect_size) sample_size_step_heading(1, "effect_size", language) else sample_size_step_heading(1, "calculate", language)),
            if (is_effect_size) {
              radioButtons(
                "sample_size_effectsize_design",
                label = NULL,
                choices = sample_size_choice_labels(language, c(
                  "Cohen's d for independent means" = "independent_means",
                  "Hedges' g for independent means" = "hedges_g",
                  "Cohen's d for one-sample mean" = "one_sample_mean",
                  "Cohen's dz for paired means" = "paired_means"
                )),
                selected = "independent_means"
              )
            } else {
              if (length(target_choices) == 1L) {
                tagList(
                  tags$div(class = "sample-size-fixed-target", names(target_choices)[[1]]),
                  tags$input(
                    id = paste0("sample_size_", method, "_target"),
                    type = "hidden",
                    value = target_choices[[1]]
                  )
                )
              } else {
                radioButtons(
                  paste0("sample_size_", method, "_target"),
                  label = NULL,
                  choices = target_choices,
                  selected = "sample_size"
                )
              }
            }
          ),
          div(
            class = "step-block sample-size-block sample-size-block2",
            h3(sample_size_step_heading(2, "inputs", language)),
            uiOutput(paste0("sample_size_", method, "_inputs")),
            actionButton(paste0("sample_size_", method, "_calculate"), sample_size_ui_text(language, "calculate"), class = "btn btn-primary sample-size-calculate")
          ),
          div(
            class = "step-block sample-size-block sample-size-block3",
            h3(sample_size_step_heading(3, "results", language)),
            uiOutput(paste0("sample_size_", method, "_results"))
          )
        )
      )
    )
  )
}

sample_size_common_inputs <- function(method, target, show_ratio = FALSE, show_tail = TRUE, n_label = "Sample size", power_value = "0.95", language = statedu_initial_language()) {
  lbl <- function(label) sample_size_label(language, label)
  tagList(
    textInput(paste0("sample_size_", method, "_alpha"), lbl("Alpha"), value = "0.05"),
    if (identical(target, "sample_size")) {
      textInput(paste0("sample_size_", method, "_power"), lbl("Power"), value = power_value)
    } else {
      textInput(paste0("sample_size_", method, "_n"), lbl(n_label), value = "100")
    },
    if (isTRUE(show_ratio)) {
      textInput(paste0("sample_size_", method, "_ratio"), lbl("Allocation ratio (Group 2 / Group 1)"), value = "1")
    },
    if (isTRUE(show_tail)) {
      selectInput(
        paste0("sample_size_", method, "_alternative"),
        lbl("Alternative"),
        choices = stats::setNames(c("two.sided", "one.sided"), c(lbl("Two-sided"), lbl("One-sided"))),
        selected = "two.sided"
      )
    },
    if (identical(target, "sample_size")) {
      textInput(paste0("sample_size_", method, "_dropout"), lbl("Dropout rate (%)"), value = "0")
    }
  )
}

sample_size_inputs_ui <- function(method, input, language = statedu_initial_language()) {
  lbl <- function(label) sample_size_label(language, label)
  common_inputs <- function(...) sample_size_common_inputs(..., language = language)
  target <- input[[paste0("sample_size_", method, "_target")]] %||% "sample_size"
  effectsize_design <- input$sample_size_effectsize_design %||% "independent_means"
  ttest_design <- input$sample_size_ttest_design %||% "two_sample"
  ttest_effect_label <- lbl(switch(
    ttest_design,
    one_sample = "Effect size d (mean difference / SD)",
    paired = "Effect size dz (paired difference / SD)",
    "Effect size d (Cohen's d)"
  ))
  ttest_n_label <- switch(
    ttest_design,
    one_sample = "Participants",
    paired = "Pairs",
    "Sample size per group"
  )
  nonparametric_design <- input$sample_size_nonparametric_design %||% "two_independent"
  nonparametric_effect_label <- lbl(switch(
    nonparametric_design,
    paired = "Effect size dz (paired difference / SD)",
    one_sample = "Effect size d (median shift / SD)",
    kruskal_wallis = "Effect size f",
    friedman = "Effect size W (Kendall's W)",
    "Effect size d (approx.)"
  ))
  nonparametric_n_label <- switch(
    nonparametric_design,
    paired = "Pairs",
    one_sample = "Participants",
    kruskal_wallis = "Total sample size",
    friedman = "Participants",
    "Sample size per group"
  )
  proportion_design <- input$sample_size_proportion_design %||% "two_proportion"
  anova_design <- input$sample_size_anova_design %||% "one_way"
  anova_effect_label <- lbl("Effect size f")
  ancova_design <- input$sample_size_ancova_design %||% "ancova"
  ancova_effect_label <- if (identical(ancova_design, "manova")) lbl("Pillai's trace V") else lbl("Effect size f")
  ancova_n_label <- "Total sample size"
  regression_design <- input$sample_size_regression_design %||% "multiple"
  gee_outcome <- input$sample_size_gee_outcome %||% "continuous"
  equivalence_outcome <- input$sample_size_equivalence_outcome %||% "mean"
  diagnostic_design <- input$sample_size_diagnostic_design %||% "sensitivity"
  precision_parameter <- input$sample_size_precision_parameter %||% "mean"
  rates_design <- input$sample_size_rates_design %||% "two_rate_ratio"
  cluster_design <- input$sample_size_cluster_design %||% "parallel"
  cluster_outcome <- input$sample_size_cluster_outcome %||% "continuous"
  reliability_design <- input$sample_size_reliability_design %||% "alpha"
  lmm_mode <- input$sample_size_lmm_mode %||% "simple"
  lmm_design <- input$sample_size_lmm_design %||% "two_group_repeated"
  lmm_n_label <- if (identical(lmm_design, "two_group_repeated")) "Participants per group" else "Participants"
  sem_test <- input$sample_size_sem_test %||% "close_fit"
  sem_null_rmsea_default <- if (identical(sem_test, "not_close_fit")) "0.08" else "0.05"
  sem_alternative_rmsea_default <- if (identical(sem_test, "not_close_fit")) "0.05" else "0.08"
  switch(
    method,
    effectsize = tagList(
      if (identical(effectsize_design, "paired_means")) {
        tagList(
          textInput("sample_size_effectsize_mean_difference", lbl("Mean paired difference"), value = "5"),
          textInput("sample_size_effectsize_sd_difference", lbl("SD of paired differences"), value = "10")
        )
      } else if (identical(effectsize_design, "one_sample_mean")) {
        tagList(
          textInput("sample_size_effectsize_mean1", lbl("Sample mean"), value = "105"),
          textInput("sample_size_effectsize_null_mean", lbl("Null mean"), value = "100"),
          textInput("sample_size_effectsize_sd1", lbl("SD"), value = "10")
        )
      } else {
        tagList(
          textInput("sample_size_effectsize_mean1", lbl("Group 1 mean"), value = "105"),
          textInput("sample_size_effectsize_mean2", lbl("Group 2 mean"), value = "100"),
          textInput("sample_size_effectsize_sd1", lbl("Group 1 SD"), value = "10"),
          textInput("sample_size_effectsize_sd2", lbl("Group 2 SD"), value = "10"),
          textInput("sample_size_effectsize_n1", lbl("Group 1 n"), value = "50"),
          textInput("sample_size_effectsize_n2", lbl("Group 2 n"), value = "50"),
          if (identical(effectsize_design, "hedges_g")) {
            div(class = "sample-size-method-note", "Primary result will be Hedges' g; Cohen's d is also shown for reference.")
          }
        )
      }
    ),
    ttest = tagList(
      selectInput(
        "sample_size_ttest_design",
        lbl("Design"),
        choices = sample_size_choice_labels(language, c("Two independent groups" = "two_sample", "One sample" = "one_sample", "Paired" = "paired")),
        selected = ttest_design
      ),
      textInput("sample_size_ttest_effect", ttest_effect_label, value = "0.50"),
      common_inputs("ttest", target, show_ratio = identical(ttest_design, "two_sample"), n_label = ttest_n_label)
    ),
    nonparametric = tagList(
      selectInput(
        "sample_size_nonparametric_design",
        lbl("Design"),
        choices = sample_size_choice_labels(language, c(
          "Mann-Whitney U (two independent groups)" = "two_independent",
          "Wilcoxon signed-rank (paired samples)" = "paired",
          "One-sample Wilcoxon signed-rank (median shift)" = "one_sample",
          "Kruskal-Wallis" = "kruskal_wallis",
          "Friedman test" = "friedman"
        )),
        selected = nonparametric_design
      ),
      textInput("sample_size_nonparametric_effect", nonparametric_effect_label, value = "0.50"),
      if (identical(nonparametric_design, "kruskal_wallis")) {
        textInput("sample_size_nonparametric_groups", lbl("Number of groups"), value = "3")
      },
      if (identical(nonparametric_design, "friedman")) {
        textInput("sample_size_nonparametric_measurements", lbl("Measurements"), value = "3")
      },
      common_inputs(
        "nonparametric",
        target,
        show_ratio = identical(nonparametric_design, "two_independent"),
        show_tail = !nonparametric_design %in% c("kruskal_wallis", "friedman"),
        n_label = nonparametric_n_label
      )
    ),
    proportion = tagList(
      selectInput(
        "sample_size_proportion_design",
        lbl("Design"),
        choices = sample_size_choice_labels(language, c("Two independent proportions" = "two_proportion", "One proportion vs 0.50" = "one_proportion")),
        selected = proportion_design
      ),
      textInput("sample_size_proportion_p1", lbl("Proportion 1"), value = "0.50"),
      if (identical(proportion_design, "two_proportion")) {
        textInput("sample_size_proportion_p2", lbl("Proportion 2"), value = "0.65")
      },
      common_inputs("proportion", target, show_ratio = identical(proportion_design, "two_proportion"))
    ),
    chisquare = tagList(
      textInput("sample_size_chisquare_effect", lbl("Effect size w"), value = "0.30"),
      textInput("sample_size_chisquare_df", lbl("Degrees of freedom"), value = "1"),
      common_inputs("chisquare", target, show_tail = FALSE)
    ),
    correlation = tagList(
      textInput("sample_size_correlation_r", lbl("Expected r"), value = "0.30"),
      common_inputs("correlation", target)
    ),
    anova = tagList(
      selectInput(
        "sample_size_anova_design",
        lbl("Design"),
        choices = sample_size_choice_labels(language, c(
          "One-way ANOVA" = "one_way",
          "Two-way ANOVA" = "two_way",
          "One-group repeated-measures ANOVA" = "repeated_one_group",
          "Mixed repeated-measures ANOVA" = "mixed_repeated"
        )),
        selected = anova_design
      ),
      textInput("sample_size_anova_effect", anova_effect_label, value = "0.25"),
      if (identical(anova_design, "two_way")) {
        tagList(
          textInput("sample_size_anova_factor_a", lbl("Factor A levels"), value = "2"),
          textInput("sample_size_anova_factor_b", lbl("Factor B levels"), value = "2"),
          selectInput(
            "sample_size_anova_effect_test",
            lbl("Effect to test"),
            choices = sample_size_choice_labels(language, c("Main effect A" = "main_a", "Main effect B" = "main_b", "Interaction A x B" = "interaction")),
            selected = input$sample_size_anova_effect_test %||% "interaction"
          )
        )
      } else if (identical(anova_design, "repeated_one_group")) {
        tagList(
          textInput("sample_size_anova_measurements", lbl("Measurements"), value = "3"),
          textInput("sample_size_anova_correlation", lbl("Average repeated-measures correlation"), value = "0.50"),
          textInput("sample_size_anova_epsilon", lbl("Nonsphericity epsilon"), value = "1")
        )
      } else if (identical(anova_design, "mixed_repeated")) {
        tagList(
          textInput("sample_size_anova_groups", lbl("Groups"), value = "2"),
          textInput("sample_size_anova_measurements", lbl("Measurements"), value = "3"),
          selectInput(
            "sample_size_anova_effect_test",
            lbl("Effect to test"),
            choices = sample_size_choice_labels(language, c("Group" = "group", "Time" = "time", "Group x Time" = "interaction")),
            selected = input$sample_size_anova_effect_test %||% "interaction"
          ),
          textInput("sample_size_anova_correlation", lbl("Average repeated-measures correlation"), value = "0.50"),
          textInput("sample_size_anova_epsilon", lbl("Nonsphericity epsilon"), value = "1")
        )
      } else {
        textInput("sample_size_anova_groups", lbl("Number of groups"), value = "3")
      },
      common_inputs("anova", target, show_tail = FALSE)
    ),
    ancova = tagList(
      selectInput(
        "sample_size_ancova_design",
        lbl("Design"),
        choices = sample_size_choice_labels(language, c(
          "ANCOVA" = "ancova",
          "Ranked ANCOVA" = "ranked_ancova",
          "MANOVA" = "manova"
        )),
        selected = ancova_design
      ),
      textInput("sample_size_ancova_groups", lbl("Groups"), value = "2"),
      textInput("sample_size_ancova_effect", ancova_effect_label, value = if (identical(ancova_design, "manova")) "0.10" else "0.25"),
      if (identical(ancova_design, "manova")) {
        textInput("sample_size_ancova_outcomes", lbl("Outcome variables"), value = "2")
      } else {
        tagList(
          textInput("sample_size_ancova_covariates", lbl("Covariates"), value = "1"),
          textInput("sample_size_ancova_covariate_r2", lbl("Covariate R-squared"), value = "0.30")
        )
      },
      common_inputs("ancova", target, show_tail = FALSE, n_label = ancova_n_label)
    ),
    regression = tagList(
      selectInput(
        "sample_size_regression_design",
        lbl("Design"),
        choices = sample_size_choice_labels(language, c(
          "Multiple regression" = "multiple",
          "Hierarchical regression" = "hierarchical",
          "Logistic regression" = "logistic",
          "Mediation effect" = "mediation",
          "Moderation regression" = "moderation"
        )),
        selected = regression_design
      ),
      if (identical(regression_design, "multiple")) {
        tagList(
          textInput("sample_size_regression_effect", lbl("Effect size f2"), value = "0.15"),
          textInput("sample_size_regression_predictors", lbl("Number of predictors"), value = "3")
        )
      } else if (identical(regression_design, "hierarchical")) {
        tagList(
          textInput("sample_size_regression_effect", lbl("Effect size f2 for R2 increase"), value = "0.15"),
          textInput("sample_size_regression_tested", lbl("Tested predictors"), value = "1"),
          textInput("sample_size_regression_total_predictors", lbl("Total predictors in final model"), value = "3")
        )
      } else if (identical(regression_design, "logistic")) {
        tagList(
          textInput("sample_size_regression_or", lbl("Odds ratio"), value = "1.80"),
          textInput("sample_size_regression_p0", lbl("Baseline event probability"), value = "0.30"),
          textInput("sample_size_regression_predictor_prevalence", lbl("Predictor prevalence"), value = "0.50"),
          textInput("sample_size_regression_covariate_r2", lbl("Covariate R-squared"), value = "0")
        )
      } else if (identical(regression_design, "mediation")) {
        mediation_method <- input$sample_size_regression_mediation_method %||% "monte_carlo"
        tagList(
          selectInput(
            "sample_size_regression_mediation_method",
            lbl("Mediation method"),
            choices = sample_size_choice_labels(language, c(
              "Fritz & MacKinnon empirical table (.80 power)" = "fritz_mackinnon",
              "Monte Carlo indirect effect CI" = "monte_carlo",
              "Bootstrap indirect effect CI (slow)" = "bootstrap",
              "Sobel approximation" = "sobel"
            )),
            selected = mediation_method
          ),
          if (identical(mediation_method, "fritz_mackinnon")) {
            tagList(
              selectInput(
                "sample_size_regression_a_effect",
                lbl("Path a effect size"),
                choices = sample_size_choice_labels(language, c("Small (.14)" = "small", "Halfway (.26)" = "halfway", "Medium (.39)" = "medium", "Large (.59)" = "large")),
                selected = input$sample_size_regression_a_effect %||% "medium"
              ),
              selectInput(
                "sample_size_regression_b_effect",
                lbl("Path b effect size"),
                choices = sample_size_choice_labels(language, c("Small (.14)" = "small", "Halfway (.26)" = "halfway", "Medium (.39)" = "medium", "Large (.59)" = "large")),
                selected = input$sample_size_regression_b_effect %||% "medium"
              ),
              selectInput(
                "sample_size_regression_fritz_test",
                "Fritz & MacKinnon test",
                choices = sample_size_choice_labels(language, c(
                  "Bias-corrected bootstrap" = "bias_corrected_bootstrap",
                  "Percentile bootstrap" = "percentile_bootstrap",
                  "PRODCLIN / distribution of the product" = "prodclin",
                  "Joint significance" = "joint_significance",
                  "Sobel / first-order delta" = "sobel",
                  "Baron & Kenny, c' = 0" = "baron_kenny_complete",
                  "Baron & Kenny, c' = .14" = "baron_kenny_small",
                  "Baron & Kenny, c' = .39" = "baron_kenny_medium",
                  "Baron & Kenny, c' = .59" = "baron_kenny_large"
                )),
                selected = input$sample_size_regression_fritz_test %||% "bias_corrected_bootstrap"
              ),
              div(class = "sample-size-method-note", "This empirical table is fixed at power = .80; set Power to 0.80.")
            )
          } else {
            tagList(
              textInput("sample_size_regression_a", lbl("Path a beta: predictor -> mediator"), value = "0.30"),
              textInput("sample_size_regression_b", lbl("Path b beta: mediator -> outcome"), value = "0.30"),
              textInput("sample_size_regression_covariates", lbl("Number of covariates"), value = "0")
            )
          },
          if (identical(mediation_method, "bootstrap")) {
            tagList(
              textInput("sample_size_regression_simulations", lbl("Simulations"), value = "30"),
              textInput("sample_size_regression_bootstraps", lbl("Bootstrap samples"), value = "100")
            )
          }
        )
      } else {
        tagList(
          textInput("sample_size_regression_effect", lbl("Effect size f2 for interaction R2 increase"), value = "0.15"),
          textInput("sample_size_regression_interactions", lbl("Interaction terms tested"), value = "1"),
          textInput("sample_size_regression_total_predictors", lbl("Total predictors in final model"), value = "4")
        )
      },
      common_inputs(
        "regression",
        target,
        show_tail = regression_design %in% c("logistic", "mediation"),
        n_label = "Total sample size",
        power_value = if (identical(regression_design, "mediation") && identical(input$sample_size_regression_mediation_method %||% "monte_carlo", "fritz_mackinnon")) "0.80" else "0.95"
      )
    ),
    gee = tagList(
      selectInput(
        "sample_size_gee_outcome",
        lbl("Outcome"),
        choices = stats::setNames(c("continuous", "binary"), c(lbl("Continuous outcome"), lbl("Binary outcome"))),
        selected = gee_outcome
      ),
      if (identical(gee_outcome, "binary")) {
        tagList(
          textInput("sample_size_gee_p1", lbl("Proportion 1"), value = "0.50"),
          textInput("sample_size_gee_p2", lbl("Proportion 2"), value = "0.65")
        )
      } else {
        textInput("sample_size_gee_effect", lbl("Effect size d"), value = "0.50")
      },
      textInput("sample_size_gee_time_points", lbl("Time points"), value = "3"),
      selectInput(
        "sample_size_gee_correlation_structure",
        lbl("Working correlation"),
        choices = sample_size_choice_labels(language, c("Exchangeable" = "exchangeable", "AR(1)" = "ar1", "Unstructured" = "unstructured")),
        selected = input$sample_size_gee_correlation_structure %||% "exchangeable"
      ),
      if (identical(input$sample_size_gee_correlation_structure %||% "exchangeable", "unstructured")) {
        textInput("sample_size_gee_correlations", lbl("Pairwise correlations"), value = "0.30, 0.20, 0.30")
      } else {
        textInput("sample_size_gee_rho", lbl("Working correlation rho"), value = "0.30")
      },
      common_inputs("gee", target, show_ratio = TRUE, n_label = "Sample size per group")
    ),
    lmm = tagList(
      selectInput(
        "sample_size_lmm_mode",
        lbl("Input mode"),
        choices = sample_size_choice_labels(language, c("Simple" = "simple", "GLIMMPSE-style" = "glimmpse")),
        selected = lmm_mode
      ),
      selectInput(
        "sample_size_lmm_design",
        lbl("Design"),
        choices = sample_size_choice_labels(language, c(
          "Two-group repeated (Group x Time)" = "two_group_repeated",
          "One-group repeated (Time slope)" = "one_group_repeated"
        )),
        selected = lmm_design
      ),
      if (identical(lmm_mode, "glimmpse")) {
        tagList(
          textInput("sample_size_lmm_group1_means", lbl("Group 1 means by time"), value = "0, 0.2, 0.4"),
          if (identical(lmm_design, "two_group_repeated")) {
            textInput("sample_size_lmm_group2_means", lbl("Group 2 means by time"), value = "0, 0.1, 0.8")
          },
          textInput("sample_size_lmm_residual_sd", lbl("Residual SD"), value = "1"),
          selectInput(
            "sample_size_lmm_correlation_structure",
            lbl("Correlation structure"),
            choices = sample_size_choice_labels(language, c("Exchangeable" = "exchangeable", "AR(1)" = "ar1", "Unstructured" = "unstructured")),
            selected = input$sample_size_lmm_correlation_structure %||% "exchangeable"
          ),
          if (identical(input$sample_size_lmm_correlation_structure %||% "exchangeable", "unstructured")) {
            textInput("sample_size_lmm_correlations", lbl("Pairwise correlations"), value = "0.50, 0.30, 0.50")
          } else {
            textInput("sample_size_lmm_rho", lbl("Correlation rho"), value = "0.50")
          }
        )
      } else {
        tagList(
        textInput("sample_size_lmm_effect", lbl("Standardized fixed effect"), value = "0.30"),
          textInput("sample_size_lmm_time_points", lbl("Time points"), value = "3"),
          textInput("sample_size_lmm_icc", lbl("ICC / random intercept proportion"), value = "0.30")
        )
      },
      if (identical(lmm_mode, "glimmpse") || identical(lmm_design, "one_group_repeated")) {
        textInput("sample_size_lmm_simulations", lbl("Simulations"), value = "100")
      },
      common_inputs("lmm", target, show_tail = FALSE, n_label = lmm_n_label)
    ),
    survival = tagList(
      textInput("sample_size_survival_hr", lbl("Hazard ratio"), value = "0.70"),
      textInput("sample_size_survival_event_probability", lbl("Overall event probability"), value = "0.60"),
      common_inputs("survival", target, show_ratio = TRUE, n_label = "Total sample size")
    ),
    equivalence = tagList(
      selectInput(
        "sample_size_equivalence_objective",
        lbl("Objective"),
        choices = sample_size_choice_labels(language, c("Non-inferiority" = "noninferiority", "Equivalence" = "equivalence")),
        selected = input$sample_size_equivalence_objective %||% "noninferiority"
      ),
      selectInput(
        "sample_size_equivalence_outcome",
        lbl("Outcome"),
        choices = sample_size_choice_labels(language, c("Mean difference" = "mean", "Proportion difference" = "proportion")),
        selected = equivalence_outcome
      ),
      textInput("sample_size_equivalence_margin", lbl("Margin"), value = "0.20"),
      if (identical(equivalence_outcome, "proportion")) {
        tagList(
          textInput("sample_size_equivalence_p1", lbl("Expected proportion 1"), value = "0.80"),
          textInput("sample_size_equivalence_p2", lbl("Expected proportion 2"), value = "0.80")
        )
      } else {
        tagList(
          textInput("sample_size_equivalence_difference", lbl("Expected true difference"), value = "0"),
          textInput("sample_size_equivalence_sd", lbl("SD"), value = "1")
        )
      },
      common_inputs("equivalence", target, show_ratio = TRUE, show_tail = FALSE, n_label = "Sample size per group")
    ),
    diagnostic = tagList(
      selectInput(
        "sample_size_diagnostic_design",
        lbl("Design"),
        choices = sample_size_choice_labels(language, c(
          "Sensitivity precision" = "sensitivity",
          "Specificity precision" = "specificity",
          "ROC AUC vs null" = "auc"
        )),
        selected = diagnostic_design
      ),
      if (identical(diagnostic_design, "sensitivity")) {
        tagList(
          textInput("sample_size_diagnostic_sensitivity", lbl("Expected sensitivity"), value = "0.85"),
          textInput("sample_size_diagnostic_prevalence", lbl("Prevalence"), value = "0.30"),
          textInput("sample_size_diagnostic_precision", lbl("Desired CI half-width"), value = "0.08")
        )
      } else if (identical(diagnostic_design, "specificity")) {
        tagList(
          textInput("sample_size_diagnostic_specificity", lbl("Expected specificity"), value = "0.85"),
          textInput("sample_size_diagnostic_prevalence", lbl("Prevalence"), value = "0.30"),
          textInput("sample_size_diagnostic_precision", lbl("Desired CI half-width"), value = "0.08")
        )
      } else {
        tagList(
          textInput("sample_size_diagnostic_auc", lbl("Expected AUC"), value = "0.75"),
          textInput("sample_size_diagnostic_null_auc", lbl("Null AUC"), value = "0.50")
        )
      },
      common_inputs(
        "diagnostic",
        target,
        show_ratio = identical(diagnostic_design, "auc"),
        show_tail = FALSE,
        n_label = if (identical(diagnostic_design, "auc")) "Number of cases" else "Total sample size"
      )
    ),
    precision = tagList(
      selectInput(
        "sample_size_precision_parameter",
        lbl("Parameter"),
        choices = sample_size_choice_labels(language, c("Mean" = "mean", "Proportion" = "proportion", "Correlation" = "correlation")),
        selected = precision_parameter
      ),
      textInput("sample_size_precision_confidence", lbl("Confidence level"), value = "0.95"),
      textInput("sample_size_precision_half_width", lbl("Desired CI half-width"), value = "0.10"),
      if (identical(precision_parameter, "mean")) {
        textInput("sample_size_precision_sd", lbl("SD"), value = "1")
      } else if (identical(precision_parameter, "proportion")) {
        textInput("sample_size_precision_proportion", lbl("Expected proportion"), value = "0.50")
      } else {
        textInput("sample_size_precision_r", lbl("Expected r"), value = "0.30")
      },
      if (identical(target, "sample_size")) {
        textInput("sample_size_precision_dropout", lbl("Dropout rate (%)"), value = "0")
      } else {
        textInput("sample_size_precision_n", lbl("Sample size"), value = "100")
      }
    ),
    mcnemar = tagList(
      textInput("sample_size_mcnemar_p01", lbl("p01: negative to positive"), value = "0.20"),
      textInput("sample_size_mcnemar_p10", lbl("p10: positive to negative"), value = "0.10"),
      common_inputs("mcnemar", target, show_tail = TRUE, n_label = "Pairs")
    ),
    rates = tagList(
      selectInput(
        "sample_size_rates_design",
        lbl("Design"),
        choices = sample_size_choice_labels(language, c(
          "Two Poisson rates" = "two_rate_ratio",
          "Two negative binomial rates" = "negative_binomial",
          "Single rate precision" = "single_rate_precision"
        )),
        selected = rates_design
      ),
      textInput("sample_size_rates_rate1", lbl(if (identical(rates_design, "single_rate_precision")) "Expected rate" else "Rate 1"), value = "0.30"),
      if (rates_design %in% c("two_rate_ratio", "negative_binomial")) {
        textInput("sample_size_rates_rate2", lbl("Rate 2"), value = "0.20")
      } else {
        textInput("sample_size_rates_half_width", lbl("Desired CI half-width"), value = "0.05")
      },
      if (identical(rates_design, "negative_binomial")) {
        textInput("sample_size_rates_dispersion", lbl("Dispersion"), value = "0.50")
      },
      common_inputs(
        "rates",
        target,
        show_ratio = rates_design %in% c("two_rate_ratio", "negative_binomial"),
        show_tail = TRUE,
        n_label = if (rates_design %in% c("two_rate_ratio", "negative_binomial")) "Person-time in Group 1" else "Person-time"
      )
    ),
    cluster = tagList(
      selectInput(
        "sample_size_cluster_design",
        lbl("Design"),
        choices = sample_size_choice_labels(language, c("Parallel cluster randomized trial" = "parallel", "Stepped-wedge cluster trial" = "stepped_wedge")),
        selected = cluster_design
      ),
      if (identical(cluster_design, "stepped_wedge")) {
        tagList(
          textInput("sample_size_cluster_effect", lbl("Effect size d"), value = "0.40"),
          textInput("sample_size_cluster_periods", lbl("Periods"), value = "5"),
          textInput("sample_size_cluster_size", lbl("Cluster size per period"), value = "20"),
          textInput("sample_size_cluster_icc", lbl("ICC"), value = "0.05"),
          textInput("sample_size_cluster_simulations", lbl("Simulations"), value = "100"),
          common_inputs("cluster", target, show_ratio = FALSE, show_tail = FALSE, n_label = "Clusters")
        )
      } else {
        tagList(
          selectInput(
            "sample_size_cluster_outcome",
            lbl("Outcome"),
            choices = stats::setNames(c("continuous", "binary"), c(lbl("Continuous outcome"), lbl("Binary outcome"))),
            selected = cluster_outcome
          ),
          if (identical(cluster_outcome, "binary")) {
            tagList(
              textInput("sample_size_cluster_p1", lbl("Proportion 1"), value = "0.50"),
              textInput("sample_size_cluster_p2", lbl("Proportion 2"), value = "0.65")
            )
          } else {
            textInput("sample_size_cluster_effect", lbl("Effect size d"), value = "0.50")
          },
          textInput("sample_size_cluster_size", lbl("Cluster size"), value = "20"),
            textInput("sample_size_cluster_icc", lbl("ICC"), value = "0.05"),
          common_inputs("cluster", target, show_ratio = TRUE, n_label = "Sample size per group")
        )
      }
    ),
    reliability = tagList(
      selectInput(
        "sample_size_reliability_design",
        lbl("Design"),
        choices = sample_size_choice_labels(language, c(
          "Cronbach's alpha" = "alpha",
          "ICC reliability" = "icc",
          "Cohen's kappa" = "kappa",
          "Bland-Altman LoA" = "bland_altman"
        )),
        selected = reliability_design
      ),
      textInput(
        "sample_size_reliability_value",
        lbl(if (identical(reliability_design, "bland_altman")) "SD of paired differences" else "Expected reliability"),
        value = if (identical(reliability_design, "bland_altman")) "1" else "0.80"
      ),
      textInput("sample_size_reliability_confidence", lbl("Confidence level"), value = "0.95"),
      textInput("sample_size_reliability_half_width", lbl("Desired CI half-width"), value = "0.10"),
      if (identical(reliability_design, "alpha")) {
        textInput("sample_size_reliability_items", lbl("Number of items"), value = "5")
      } else if (identical(reliability_design, "icc")) {
        textInput("sample_size_reliability_items", lbl("Raters / measurements"), value = "2")
      } else if (identical(reliability_design, "kappa")) {
        textInput("sample_size_reliability_categories", lbl("Categories"), value = "2")
      },
      if (identical(target, "sample_size")) {
        textInput("sample_size_reliability_dropout", lbl("Dropout rate (%)"), value = "0")
      }
    ),
    sem = tagList(
      selectInput(
        "sample_size_sem_test",
        lbl("SEM / CFA method"),
        choices = sample_size_choice_labels(language, c(
          "Close fit test (detect poor fit)" = "close_fit",
          "Not-close-fit test (support close fit)" = "not_close_fit",
          "Parameter-level Monte Carlo" = "parameter",
          "Model complexity heuristic" = "complexity"
        )),
        selected = sem_test
      ),
      if (identical(sem_test, "parameter")) {
        tagList(
          selectInput(
            "sample_size_sem_parameter_type",
            lbl("Parameter type"),
            choices = sample_size_choice_labels(language, c(
              "Standardized loading" = "loading",
              "Standardized path" = "path",
              "Latent correlation" = "correlation"
            )),
            selected = input$sample_size_sem_parameter_type %||% "path"
          ),
          textInput("sample_size_sem_parameter", lbl("Expected standardized parameter"), value = "0.30"),
          selectInput(
            "sample_size_sem_complexity",
            lbl("Model complexity"),
            choices = sample_size_choice_labels(language, c("Simple" = "simple", "Moderate" = "moderate", "Complex" = "complex")),
            selected = input$sample_size_sem_complexity %||% "moderate"
          ),
          textInput("sample_size_sem_simulations", lbl("Simulations"), value = "1000")
        )
      } else if (identical(sem_test, "complexity")) {
        tagList(
          textInput("sample_size_sem_latent_variables", lbl("Latent variables"), value = "3"),
          textInput("sample_size_sem_measured_variables", lbl("Measured variables"), value = "12"),
          textInput("sample_size_sem_structural_paths", lbl("Structural paths"), value = "3"),
          textInput("sample_size_sem_free_parameters", lbl("Free parameters"), value = "30"),
          textInput("sample_size_sem_expected_loading", lbl("Expected standardized loading"), value = "0.50"),
          textInput("sample_size_sem_expected_path", lbl("Expected standardized path"), value = "0.30"),
          selectInput(
            "sample_size_sem_complexity",
            lbl("Model complexity"),
            choices = sample_size_choice_labels(language, c("Simple" = "simple", "Moderate" = "moderate", "Complex" = "complex")),
            selected = input$sample_size_sem_complexity %||% "moderate"
          )
        )
      } else {
        tagList(
          selectInput(
            "sample_size_sem_df_source",
            lbl("Model df input"),
            choices = sample_size_choice_labels(language, c(
              "Estimate from model counts" = "structure",
              "Enter model df directly" = "direct"
            )),
            selected = input$sample_size_sem_df_source %||% "structure"
          ),
          if (identical(input$sample_size_sem_df_source %||% "structure", "direct")) {
            textInput("sample_size_sem_df", lbl("Model degrees of freedom"), value = "50")
          } else {
            tagList(
              textInput("sample_size_sem_latent_variables", lbl("Latent variables"), value = "3"),
              textInput("sample_size_sem_measured_variables", lbl("Measured variables"), value = "12"),
              textInput("sample_size_sem_structural_paths", lbl("Structural paths"), value = "3")
            )
          },
          textInput("sample_size_sem_null_rmsea", lbl("Null RMSEA"), value = sem_null_rmsea_default),
          textInput("sample_size_sem_alternative_rmsea", lbl("Alternative RMSEA"), value = sem_alternative_rmsea_default)
        )
      },
      common_inputs("sem", target, show_tail = FALSE, n_label = "Sample size")
    )
  )
}

sample_size_result_table <- function(result) {
  rows <- list()
  add_row <- function(label, value, primary = FALSE) {
    rows[[length(rows) + 1L]] <<- tags$tr(
      class = if (isTRUE(primary)) "sample-size-primary-effect" else NULL,
      tags$th(label),
      tags$td(value)
    )
  }
  sample_size_n_label <- function(label) {
    label <- label %||% "Total"
    if (grepl("^n\\b", label, ignore.case = TRUE)) return(label)
    sprintf("n (%s)", label)
  }
  add_section <- function(label) {
    rows[[length(rows) + 1L]] <<- tags$tr(
      class = "sample-size-result-section",
      tags$th(colspan = 2, label)
    )
  }
  has_dropout <- isTRUE(is.finite(result$dropout_rate)) && result$dropout_rate > 0

  if (!is.null(result$design_label)) add_row("Design", result$design_label)
  if (identical(result$result_type, "effect_size")) {
    if (!is.null(result$primary_effect_size)) {
      add_section("Calculated from selected method")
      add_row(result$primary_effect_size_label %||% "Primary effect size", sprintf("%.3f", result$primary_effect_size), primary = TRUE)
    }
    add_section("Converted effect sizes")
    if (!is.null(result$effect_size_d)) add_row(result$effect_size_label %||% "Effect size", sprintf("%.3f", result$effect_size_d))
    if (!is.null(result$hedges_g)) add_row("Hedges' g", sprintf("%.3f", result$hedges_g))
    if (!is.null(result$point_biserial_r)) add_row(lbl("Point-biserial r"), sprintf("%.3f", result$point_biserial_r))
    if (!is.null(result$cohen_f)) add_row("Cohen's f", sprintf("%.3f", result$cohen_f))
    if (!is.null(result$f_squared)) add_row("Cohen's f-squared", sprintf("%.3f", result$f_squared))
    if (!is.null(result$eta_squared)) add_row("Eta squared (eta2)", sprintf("%.3f", result$eta_squared))
    if (!is.null(result$proportion1)) add_row("Proportion 1", sprintf("%.3f", result$proportion1))
    if (!is.null(result$proportion2)) add_row("Proportion 2", sprintf("%.3f", result$proportion2))
    if (!is.null(result$cohens_h)) add_row("Cohen's h", sprintf("%.3f", result$cohens_h))
    if (!is.null(result$risk_difference)) add_row("Risk difference", sprintf("%.3f", result$risk_difference))
    if (!is.null(result$risk_ratio)) add_row("Risk ratio", sprintf("%.3f", result$risk_ratio))
    if (!is.null(result$odds_ratio)) add_row("Odds ratio", sprintf("%.3f", result$odds_ratio))
    if (!is.null(result$log_odds_ratio)) add_row("log odds ratio", sprintf("%.3f", result$log_odds_ratio))
    if (!is.null(result$cohens_w)) add_row("Cohen's w", sprintf("%.3f", result$cohens_w))
    if (!is.null(result$cramer_v)) add_row("Cramer's V", sprintf("%.3f", result$cramer_v))
    if (!is.null(result$phi)) add_row("Phi", sprintf("%.3f", result$phi))
    if (!is.null(result$categories)) add_row("Categories", result$categories)
    if (!is.null(result$correlation_r)) add_row("Pearson r", sprintf("%.3f", result$correlation_r))
    if (!is.null(result$comparison_r)) add_row("Comparison r", sprintf("%.3f", result$comparison_r))
    if (!is.null(result$fisher_z)) add_row("Fisher's z", sprintf("%.3f", result$fisher_z))
    if (!is.null(result$cohens_q)) add_row("Cohen's q", sprintf("%.3f", result$cohens_q))
    if (!is.null(result$r_squared)) add_row("R-squared", sprintf("%.3f", result$r_squared))
    if (!is.null(result$unadjusted_cohen_f)) add_row(lbl("Unadjusted Cohen's f"), sprintf("%.3f", result$unadjusted_cohen_f))
    if (!is.null(result$partial_eta_squared)) add_row(lbl("Partial eta squared"), sprintf("%.3f", result$partial_eta_squared))
    if (!is.null(result$multivariate_eta_squared)) add_row("Multivariate eta squared", sprintf("%.3f", result$multivariate_eta_squared))
    if (!is.null(result$omega_squared)) add_row("Omega squared", sprintf("%.3f", result$omega_squared))
    if (!is.null(result$full_r_squared)) add_row("Full model R-squared", sprintf("%.3f", result$full_r_squared))
    if (!is.null(result$reduced_r_squared)) add_row("Reduced model R-squared", sprintf("%.3f", result$reduced_r_squared))
    if (!is.null(result$delta_r_squared)) add_row("Delta R-squared", sprintf("%.3f", result$delta_r_squared))
    if (!is.null(result$indirect_effect)) add_row("Indirect effect beta a*b", sprintf("%.3f", result$indirect_effect))
    if (!is.null(result$a_path)) add_row("Path a beta", sprintf("%.3f", result$a_path))
    if (!is.null(result$b_path)) add_row("Path b beta", sprintf("%.3f", result$b_path))
    if (!is.null(result$covariate_r2)) add_row("Covariate R-squared", sprintf("%.3f", result$covariate_r2))
    if (!is.null(result$intraclass_correlation)) add_row("ICC", sprintf("%.3f", result$intraclass_correlation))
    if (!is.null(result$f_statistic)) add_row(lbl("F statistic"), sprintf("%.3f", result$f_statistic))
    if (!is.null(result$df_effect)) add_row("Numerator df", sprintf("%.3f", result$df_effect))
    if (!is.null(result$df_error)) add_row("Denominator df", sprintf("%.3f", result$df_error))
    if (!is.null(result$mean_difference)) add_row("Mean difference", sprintf("%.3f", result$mean_difference))
    if (!is.null(result$fixed_effect_coefficient)) add_row("Fixed effect B", sprintf("%.3f", result$fixed_effect_coefficient))
    if (!is.null(result$cohens_dz)) add_row("Cohen's dz", sprintf("%.3f", result$cohens_dz))
    if (!is.null(result$variance_i)) add_row("Variance time I", sprintf("%.3f", result$variance_i))
    if (!is.null(result$variance_j)) add_row("Variance time J", sprintf("%.3f", result$variance_j))
    if (!is.null(result$covariance_ij)) add_row(lbl("Covariance I,J"), sprintf("%.3f", result$covariance_ij))
    if (!is.null(result$parameter_estimate)) add_row("Group x time B", sprintf("%.3f", result$parameter_estimate))
    if (!is.null(result$common_outcome_sd)) add_row("Common outcome SD", sprintf("%.3f", result$common_outcome_sd))
    if (!is.null(result$common_sd_method)) add_row("Common SD method", result$common_sd_method)
    if (!is.null(result$group1_n)) add_row("Group 1 n", result$group1_n)
    if (!is.null(result$group1_sd)) add_row("Group 1 SD", sprintf("%.3f", result$group1_sd))
    if (!is.null(result$group2_n)) add_row("Group 2 n", result$group2_n)
    if (!is.null(result$group2_sd)) add_row("Group 2 SD", sprintf("%.3f", result$group2_sd))
    if (!is.null(result$change_difference)) add_row("Change difference", sprintf("%.3f", result$change_difference))
    if (!is.null(result$residual_sd)) add_row("Residual SD", sprintf("%.3f", result$residual_sd))
    if (!is.null(result$pillai_trace)) add_row("Pillai's trace V", sprintf("%.3f", result$pillai_trace))
    if (!is.null(result$wilks_lambda)) add_row(lbl("Wilks' lambda"), sprintf("%.3f", result$wilks_lambda))
    if (!is.null(result$dependent_variables)) add_row("Dependent variables", result$dependent_variables)
    if (!is.null(result$rank_biserial)) add_row("Rank-biserial r", sprintf("%.3f", result$rank_biserial))
    if (!is.null(result$cliffs_delta)) add_row("Cliff's delta", sprintf("%.3f", result$cliffs_delta))
    if (!is.null(result$epsilon_squared)) add_row("Epsilon squared", sprintf("%.3f", result$epsilon_squared))
    if (!is.null(result$kendall_w)) add_row("Kendall's W", sprintf("%.3f", result$kendall_w))
    if (!is.null(result$cohen_g)) add_row("Cohen's g", sprintf("%.3f", result$cohen_g))
    if (!is.null(result$discordant_probability)) add_row("Discordant probability", sprintf("%.3f", result$discordant_probability))
    if (!is.null(result$discordant_difference)) add_row("Discordant difference", sprintf("%.3f", result$discordant_difference))
    if (!is.null(result$discordant_pairs)) add_row("Discordant pairs", result$discordant_pairs)
    if (!is.null(result$hazard_ratio)) add_row("Hazard ratio", sprintf("%.3f", result$hazard_ratio))
    if (!is.null(result$log_hazard_ratio)) add_row("log hazard ratio", sprintf("%.3f", result$log_hazard_ratio))
    if (!is.null(result$observed_effect)) add_row("Observed effect", sprintf("%.3f", result$observed_effect))
    if (!is.null(result$equivalence_margin)) add_row("Margin", sprintf("%.3f", result$equivalence_margin))
    if (!is.null(result$distance_to_margin)) add_row("Distance to margin", sprintf("%.3f", result$distance_to_margin))
    if (!is.null(result$standardized_effect)) add_row("Standardized effect", sprintf("%.3f", result$standardized_effect))
    if (!is.null(result$standardized_margin)) add_row("Standardized margin", sprintf("%.3f", result$standardized_margin))
    if (!is.null(result$standardized_distance)) add_row("Standardized distance", sprintf("%.3f", result$standardized_distance))
    if (!is.null(result$inside_margin)) add_row("Inside margin", result$inside_margin)
    if (!is.null(result$diagnostic_value)) add_row("Diagnostic value", sprintf("%.3f", result$diagnostic_value))
    if (!is.null(result$reference_value)) add_row("Reference value", sprintf("%.3f", result$reference_value))
    if (!is.null(result$diagnostic_difference)) add_row("Diagnostic difference", sprintf("%.3f", result$diagnostic_difference))
    if (!is.null(result$logit_effect)) add_row("Logit effect", sprintf("%.3f", result$logit_effect))
    if (!is.null(result$logit_difference)) add_row("Logit difference", sprintf("%.3f", result$logit_difference))
    if (!is.null(result$auc)) add_row("AUC", sprintf("%.3f", result$auc))
    if (!is.null(result$null_auc)) add_row("Null AUC", sprintf("%.3f", result$null_auc))
    if (!is.null(result$auc_difference)) add_row("AUC difference", sprintf("%.3f", result$auc_difference))
    if (!is.null(result$auc_cohen_d)) add_row("AUC Cohen's d", sprintf("%.3f", result$auc_cohen_d))
    if (!is.null(result$incidence_rate_ratio)) add_row("Incidence rate ratio", sprintf("%.3f", result$incidence_rate_ratio))
    if (!is.null(result$log_incidence_rate_ratio)) add_row("log incidence rate ratio", sprintf("%.3f", result$log_incidence_rate_ratio))
    if (!is.null(result$mean_ratio)) add_row("Mean ratio", sprintf("%.3f", result$mean_ratio))
    if (!is.null(result$log_mean_ratio)) add_row("log mean ratio", sprintf("%.3f", result$log_mean_ratio))
    if (!is.null(result$rate1)) add_row("Rate 1", sprintf("%.3f", result$rate1))
    if (!is.null(result$rate2)) add_row("Rate 2", sprintf("%.3f", result$rate2))
    if (!is.null(result$reference_rate)) add_row("Reference rate", sprintf("%.3f", result$reference_rate))
    if (!is.null(result$rate_difference)) add_row("Rate difference", sprintf("%.3f", result$rate_difference))
    if (!is.null(result$rate_ratio)) add_row("Rate ratio", sprintf("%.3f", result$rate_ratio))
    if (!is.null(result$log_rate_ratio)) add_row("log rate ratio", sprintf("%.3f", result$log_rate_ratio))
    if (!is.null(result$poisson_standardized_difference)) add_row("Poisson standardized difference", sprintf("%.3f", result$poisson_standardized_difference))
    if (!is.null(result$dispersion)) add_row("Dispersion", sprintf("%.3f", result$dispersion))
    if (!is.null(result$negative_binomial_standardized_difference)) add_row("Negative binomial standardized difference", sprintf("%.3f", result$negative_binomial_standardized_difference))
    if (!is.null(result$cluster_size)) add_row("Cluster size", result$cluster_size)
    if (!is.null(result$periods)) add_row("Periods", result$periods)
    if (!is.null(result$estimate)) add_row("Estimate", sprintf("%.3f", result$estimate))
    if (!is.null(result$half_width)) add_row("Half-width", sprintf("%.3f", result$half_width))
    if (!is.null(result$sd)) add_row(lbl("SD"), sprintf("%.3f", result$sd))
    if (!is.null(result$bernoulli_sd)) add_row("Bernoulli SD", sprintf("%.3f", result$bernoulli_sd))
    if (!is.null(result$standardized_half_width)) add_row("Standardized half-width", sprintf("%.3f", result$standardized_half_width))
    if (!is.null(result$relative_half_width) && is.finite(result$relative_half_width)) add_row("Relative half-width", sprintf("%.3f", result$relative_half_width))
    if (!is.null(result$fisher_z_half_width)) add_row("Fisher z half-width", sprintf("%.3f", result$fisher_z_half_width))
    if (!is.null(result$reliability)) add_row("Reliability", sprintf("%.3f", result$reliability))
    if (!is.null(result$reference_reliability)) add_row("Reference reliability", sprintf("%.3f", result$reference_reliability))
    if (!is.null(result$reliability_difference)) add_row("Reliability difference", sprintf("%.3f", result$reliability_difference))
    if (!is.null(result$transformed_reliability)) add_row("Transformed reliability", sprintf("%.3f", result$transformed_reliability))
    if (!is.null(result$transformed_difference)) add_row("Transformed difference", sprintf("%.3f", result$transformed_difference))
    if (!is.null(result$average_inter_item_r)) add_row("Average inter-item r", sprintf("%.3f", result$average_inter_item_r))
    if (!is.null(result$chance_agreement)) add_row("Chance agreement", sprintf("%.3f", result$chance_agreement))
    if (!is.null(result$observed_agreement)) add_row("Observed agreement", sprintf("%.3f", result$observed_agreement))
    if (!is.null(result$sd_difference)) add_row("SD of differences", sprintf("%.3f", result$sd_difference))
    if (!is.null(result$loa_half_width)) add_row("LoA half-width", sprintf("%.3f", result$loa_half_width))
    if (!is.null(result$loa_total_width)) add_row("LoA total width", sprintf("%.3f", result$loa_total_width))
    if (!is.null(result$df)) add_row("Model df", result$df)
    if (!is.null(result$null_rmsea)) add_row(lbl("Null RMSEA"), sprintf("%.3f", result$null_rmsea))
    if (!is.null(result$alternative_rmsea)) add_row(lbl("Alternative RMSEA"), sprintf("%.3f", result$alternative_rmsea))
    if (!is.null(result$rmsea_difference)) add_row("RMSEA difference", sprintf("%.3f", result$rmsea_difference))
    if (!is.null(result$ncp_difference_per_n)) add_row("NCP difference per N", sprintf("%.3f", result$ncp_difference_per_n))
    if (!is.null(result$sem_parameter)) add_row(result$sem_parameter_type %||% "SEM parameter", sprintf("%.3f", result$sem_parameter))
    if (!is.null(result$absolute_parameter)) add_row("Absolute parameter", sprintf("%.3f", result$absolute_parameter))
    if (!is.null(result$latent_variables)) add_row("Latent variables", result$latent_variables)
    if (!is.null(result$measured_variables)) add_row("Measured variables", result$measured_variables)
    if (!is.null(result$structural_paths)) add_row("Structural paths", result$structural_paths)
    if (!is.null(result$free_parameters)) add_row("Free parameters", result$free_parameters)
    if (!is.null(result$parameter_ratio)) add_row("Cases/free parameter rule", result$parameter_ratio)
    if (!is.null(result$structure_burden)) add_row("Structure burden", result$structure_burden)
    if (!is.null(result$structure_burden_per_parameter)) add_row("Burden/free parameter", sprintf("%.3f", result$structure_burden_per_parameter))
    if (!is.null(result$expected_loading)) add_row("Expected loading", sprintf("%.3f", result$expected_loading))
    if (!is.null(result$expected_path)) add_row("Expected path", sprintf("%.3f", result$expected_path))
    if (!is.null(result$loading_fisher_z)) add_row("Loading Fisher z", sprintf("%.3f", result$loading_fisher_z))
    if (!is.null(result$path_fisher_z)) add_row("Path Fisher z", sprintf("%.3f", result$path_fisher_z))
    return(tags$table(class = "sample-size-result-table", tags$tbody(rows)))
  }
  if (!is.null(result$power)) {
    add_row("Power", sprintf("%.3f", result$power))
    if (!is.null(result$df)) add_row("Model df", result$df)
    if (!is.null(result$observed_moments)) add_row("Observed moments", result$observed_moments)
    if (!is.null(result$free_parameters)) add_row("Free parameters", result$free_parameters)
    if (!is.null(result$latent_variables)) add_row("Latent variables", result$latent_variables)
    if (!is.null(result$measured_variables)) add_row("Measured variables", result$measured_variables)
    if (!is.null(result$structural_paths)) add_row("Structural paths", result$structural_paths)
    if (!is.null(result$parameter_rule_n)) add_row("Parameter-ratio rule", result$parameter_rule_n)
    if (!is.null(result$structure_rule_n)) add_row("Model-structure rule", result$structure_rule_n)
    if (!is.null(result$design_effect)) add_row("Design effect", sprintf("%.3f", result$design_effect))
    if (!is.null(result$total_observations)) add_row("Total observations", result$total_observations)
    if (!is.null(result$hazard_ratio)) add_row("Hazard ratio", sprintf("%.3f", result$hazard_ratio))
    if (!is.null(result$log_hazard_ratio)) add_row("log hazard ratio", sprintf("%.3f", result$log_hazard_ratio))
    if (!is.null(result$event_probability)) add_row("Event probability", sprintf("%.3f", result$event_probability))
    if (!is.null(result$allocation_ratio)) add_row("Allocation ratio", sprintf("%.3f", result$allocation_ratio))
    if (!is.null(result$information_fraction)) add_row("Information fraction", sprintf("%.3f", result$information_fraction))
    if (!is.null(result$schoenfeld_signal)) add_row("Schoenfeld planning signal", sprintf("%.3f", result$schoenfeld_signal))
    return(tags$table(class = "sample-size-result-table", tags$tbody(rows)))
  }

  if (!is.null(result$independent_total)) {
    add_section("Independent-sample baseline")
    if (!is.null(result$independent_group1)) add_row("Group 1", result$independent_group1)
    if (!is.null(result$independent_group2)) add_row("Group 2", result$independent_group2)
    add_row("Total", result$independent_total)
    if (!is.null(result$design_effect)) add_row("Design effect", sprintf("%.3f", result$design_effect))
    add_section("GEE-adjusted sample size")
  } else {
    add_section("Calculated sample size")
  }
  if (!is.null(result$group1)) add_row("Group 1", result$group1)
  if (!is.null(result$group2)) add_row("Group 2", result$group2)
  if (!is.null(result$clusters_group1)) add_row("Clusters Group 1", result$clusters_group1)
  if (!is.null(result$clusters_group2)) add_row("Clusters Group 2", result$clusters_group2)
  if (!is.null(result$total_clusters)) add_row("Clusters Total", result$total_clusters)
  if (!is.null(result$raw_total_clusters)) add_row("Raw total clusters", sprintf("%.3f", result$raw_total_clusters))
  if (!is.null(result$per_group)) add_row("Per group", result$per_group)
  if (!is.null(result$per_cell)) add_row("Per cell", result$per_cell)
  if (!is.null(result$required_events)) add_row("Required events", result$required_events)
  if (!is.null(result$a_path)) add_row("Path a beta", sprintf("%.3f", result$a_path))
  if (!is.null(result$b_path)) add_row("Path b beta", sprintf("%.3f", result$b_path))
  if (!is.null(result$a_effect_label)) add_row("Path a effect size", result$a_effect_label)
  if (!is.null(result$b_effect_label)) add_row("Path b effect size", result$b_effect_label)
  if (!is.null(result$fritz_mackinnon_condition)) add_row("Fritz & MacKinnon condition", result$fritz_mackinnon_condition)
  if (!is.null(result$fritz_mackinnon_test)) add_row("Fritz & MacKinnon test", result$fritz_mackinnon_test)
  if (!is.null(result$indirect_effect)) add_row("Indirect effect beta a*b", sprintf("%.3f", result$indirect_effect))
  if (!is.null(result$covariates)) add_row("Covariates", result$covariates)
  if (!is.null(result$df)) add_row("Model df", result$df)
  if (!is.null(result$observed_moments)) add_row("Observed moments", result$observed_moments)
  if (!is.null(result$free_parameters)) add_row("Free parameters", result$free_parameters)
  if (!is.null(result$latent_variables)) add_row("Latent variables", result$latent_variables)
  if (!is.null(result$measured_variables)) add_row("Measured variables", result$measured_variables)
  if (!is.null(result$structural_paths)) add_row("Structural paths", result$structural_paths)
  if (!is.null(result$parameter_rule_n)) add_row("Parameter-ratio rule", result$parameter_rule_n)
  if (!is.null(result$structure_rule_n)) add_row("Model-structure rule", result$structure_rule_n)
  if (!is.null(result$effect_rule_n)) add_row("Effect-detectability rule", result$effect_rule_n)
  if (!is.null(result$precision_total)) add_row("Precision formula n", result$precision_total)
  if (!is.null(result$minimum_subjects)) add_row("Minimum subjects rule", result$minimum_subjects)
  if (!is.null(result$total)) add_row(sample_size_n_label(result$total_label %||% "Total"), result$total, primary = TRUE)
  if (!is.null(result$total_observations)) add_row("Total observations", result$total_observations)
  if (!is.null(result$estimated_power)) add_row("Estimated power", sprintf("%.3f", result$estimated_power))
  if (!is.null(result$achieved_half_width)) add_row("Achieved half-width", sprintf("%.3f", result$achieved_half_width))
  if (!is.null(result$hazard_ratio)) add_row("Hazard ratio", sprintf("%.3f", result$hazard_ratio))
  if (!is.null(result$log_hazard_ratio)) add_row("log hazard ratio", sprintf("%.3f", result$log_hazard_ratio))
  if (!is.null(result$event_probability)) add_row("Event probability", sprintf("%.3f", result$event_probability))
  if (!is.null(result$allocation_ratio)) add_row("Allocation ratio", sprintf("%.3f", result$allocation_ratio))
  if (!is.null(result$information_fraction)) add_row("Information fraction", sprintf("%.3f", result$information_fraction))
  if (!is.null(result$schoenfeld_signal)) add_row("Schoenfeld planning signal", sprintf("%.3f", result$schoenfeld_signal))

  if (has_dropout) {
    add_section(sprintf("Adjusted for dropout (%.1f%%)", result$dropout_rate * 100))
    if (!is.null(result$adjusted_group1)) add_row("Group 1 with dropout", result$adjusted_group1)
    if (!is.null(result$adjusted_group2)) add_row("Group 2 with dropout", result$adjusted_group2)
    if (!is.null(result$adjusted_per_group)) add_row("Per group with dropout", result$adjusted_per_group)
    if (!is.null(result$adjusted_per_cell)) add_row("Per cell with dropout", result$adjusted_per_cell)
    if (!is.null(result$adjusted_total)) add_row(sample_size_n_label(result$adjusted_total_label %||% "Total with dropout"), result$adjusted_total, primary = TRUE)
    if (!is.null(result$adjusted_total_observations)) add_row("Total observations with dropout", result$adjusted_total_observations)
  }

  tags$table(class = "sample-size-result-table", tags$tbody(rows))
}

sample_size_method_details <- function(method, result) {
  design <- result$design_label %||% ""
  if (!length(design) || is.na(design[[1]])) design <- ""
  has_design <- function(pattern, ignore.case = FALSE) {
    isTRUE(grepl(pattern, design, ignore.case = ignore.case))
  }
  cohen <- "Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences (2nd ed.). Lawrence Erlbaum."
  chow <- "Chow, S.-C., Shao, J., Wang, H., & Lokhnygina, Y. (2017). Sample Size Calculations in Clinical Research (3rd ed.). CRC Press."

  switch(
    method,
    effectsize = list(
      formula = if (has_design("Paired")) {
        "Cohen's dz = mean paired difference / SD of paired differences."
      } else if (has_design("One-sample")) {
        "Cohen's d = (sample mean - null mean) / SD."
      } else {
        "Cohen's d = (M1 - M2) / pooled SD; Hedges' g = J x d with J = 1 - 3 / (4df - 1)."
      },
      references = c(
        cohen,
        "Hedges, L. V. (1981). Distribution theory for Glass's estimator of effect size and related estimators. Journal of Educational Statistics, 6(2), 107-128.",
        "Lakens, D. (2013). Calculating and reporting effect sizes to facilitate cumulative science: A practical primer for t-tests and ANOVAs. Frontiers in Psychology, 4, 863."
      )
    ),
    effect_proportion = list(
      formula = if (has_design("Cohen")) {
        "Cohen's h = 2 asin(sqrt(p1)) - 2 asin(sqrt(p2))."
      } else if (has_design("Risk difference")) {
        "Risk difference = p1 - p2."
      } else if (has_design("Risk ratio")) {
        "Risk ratio = p1 / p2."
      } else {
        "Odds ratio = [p1 / (1 - p1)] / [p2 / (1 - p2)]; 2x2 tables with a zero cell use a 0.5 continuity correction."
      },
      references = c(
        cohen,
        "Fleiss, J. L., Levin, B., & Paik, M. C. (2003). Statistical Methods for Rates and Proportions (3rd ed.). Wiley.",
        "Haddock, C. K., Rindskopf, D., & Shadish, W. R. (1998). Using odds ratios as effect sizes for meta-analysis of dichotomous data: A primer on methods and issues. Psychological Methods, 3(3), 339-353."
      )
    ),
    effect_chisquare = list(
      formula = if (has_design("category proportions")) {
        "Cohen's w = sqrt(sum((p_observed - p_expected)^2 / p_expected))."
      } else if (has_design("Cramer's")) {
        "Cramer's V = sqrt(chi-square / [N * min(r - 1, c - 1)])."
      } else if (has_design("Phi")) {
        "Phi = sqrt(chi-square / N)."
      } else {
        "Cohen's w = sqrt(chi-square / N)."
      },
      references = c(
        cohen,
        "Cramer, H. (1946). Mathematical Methods of Statistics. Princeton University Press.",
        "Rea, L. M., & Parker, R. A. (2014). Designing and Conducting Survey Research: A Comprehensive Guide (4th ed.). Jossey-Bass."
      )
    ),
    effect_correlation = list(
      formula = if (has_design("Point-biserial")) {
        "Point-biserial r is Pearson r for a binary and continuous variable; for a two-group contrast, d = 2r / sqrt(1 - r^2)."
      } else if (has_design("t statistic")) {
        "r = sign(t) * sqrt(t^2 / (t^2 + df))."
      } else if (has_design("F statistic")) {
        "For a one-degree-of-freedom effect, r = sqrt(F / (F + df_error))."
      } else if (has_design("R-squared")) {
        "r = sqrt(R-squared); the sign is not identifiable from R-squared alone."
      } else if (has_design("Fisher")) {
        "Fisher's z = atanh(r)."
      } else {
        "Cohen's q = atanh(r1) - atanh(r2)."
      },
      references = c(
        cohen,
        "Rosenthal, R. (1994). Parametric measures of effect size. In H. Cooper & L. V. Hedges (Eds.), The Handbook of Research Synthesis. Russell Sage Foundation.",
        "Cohen, J., Cohen, P., West, S. G., & Aiken, L. S. (2003). Applied Multiple Regression/Correlation Analysis for the Behavioral Sciences (3rd ed.). Lawrence Erlbaum."
      )
    ),
    effect_anova = list(
      formula = if (has_design("eta squared from F", ignore.case = TRUE)) {
        "For one-way ANOVA, df_effect = groups - 1 and df_error = total N - groups; partial eta squared = F * df_effect / (F * df_effect + df_error); omega squared is also reported; Cohen's f = sqrt(partial eta-squared / [1 - partial eta-squared])."
      } else if (has_design("omega", ignore.case = TRUE)) {
        "For one-way ANOVA, df_effect = groups - 1 and df_error = total N - groups; partial omega squared is approximated as (F * df_effect - df_effect) / (F * df_effect + df_error + 1), bounded at 0."
      } else {
        "Cohen's f = sqrt(eta-squared / [1 - eta-squared]); the same conversion is used for partial eta-squared."
      },
      references = c(
        cohen,
        "Lakens, D. (2013). Calculating and reporting effect sizes to facilitate cumulative science: A practical primer for t-tests and ANOVAs. Frontiers in Psychology, 4, 863.",
        "Olejnik, S., & Algina, J. (2003). Generalized eta and omega squared statistics: Measures of effect size for some common research designs. Psychological Methods, 8(4), 434-447."
      )
    ),
    effect_ancova = list(
      formula = if (has_design("adjusted Cohen")) {
        "ANCOVA adjusted f = unadjusted f / sqrt(1 - covariate R-squared)."
      } else if (has_design("Pillai")) {
        "For MANOVA planning, f2 = Pillai's V / (1 - Pillai's V), and f = sqrt(f2)."
      } else if (has_design("Wilks")) {
        "For MANOVA planning, Wilks' lambda is converted using s = min(number of dependent variables, groups - 1): eta2 = 1 - lambda^(1/s), f2 = eta2 / (1 - eta2), and f = sqrt(f2)."
      } else if (has_design("F$")) {
        "For one-way ANCOVA/group contrast planning, df_effect = groups - 1 and df_error = total N - groups; partial eta squared = F * df_effect / (F * df_effect + df_error); Cohen's f = sqrt(partial eta-squared / [1 - partial eta-squared])."
      } else {
        "Cohen's f = sqrt(partial eta-squared / [1 - partial eta-squared])."
      },
      references = c(
        cohen,
        "Borm, G. F., Fransen, J., & Lemmens, W. A. J. G. (2007). A simple sample size formula for analysis of covariance in randomized clinical trials. Journal of Clinical Epidemiology, 60(12), 1234-1238.",
        "Muller, K. E., & Peterson, B. L. (1984). Practical methods for computing power in testing the multivariate general linear hypothesis. Computational Statistics & Data Analysis, 2(2), 143-158."
      )
    ),
    effect_nonparametric = list(
      formula = if (has_design("Mann-Whitney")) {
        "Rank-biserial r = 2U / (n1 n2) - 1; this is equivalent to Cliff's delta orientation."
      } else if (has_design("paired Wilcoxon")) {
        "Paired rank-biserial r = (W+ - W-) / (W+ + W-)."
      } else if (has_design("Kruskal")) {
        "Epsilon squared = (H - k + 1) / (N - k), bounded at 0."
      } else {
        "Kendall's W = Friedman chi-square / [N * (m - 1)]."
      },
      references = c(
        "Cliff, N. (1993). Dominance statistics: Ordinal analyses to answer ordinal questions. Psychological Bulletin, 114(3), 494-509.",
        "Kerby, D. S. (2014). The simple difference formula: An approach to teaching nonparametric correlation. Comprehensive Psychology, 3, 11.IT.3.1.",
        "Tomczak, M., & Tomczak, E. (2014). The need to report effect size estimates revisited. Trends in Sport Sciences, 21(1), 19-25."
      )
    ),
    effect_mcnemar = list(
      formula = if (has_design("table")) {
        "Matched-pair odds ratio = b / c for discordant pairs; if either discordant cell is zero, a 0.5 continuity correction is used."
      } else if (has_design("Cohen")) {
        "Cohen's g = p01 / (p01 + p10) - 0.5 for the discordant-pair direction."
      } else {
        "Matched-pair odds ratio = p01 / p10; log odds ratio = log(p01 / p10)."
      },
      references = c(
        cohen,
        "McNemar, Q. (1947). Note on the sampling error of the difference between correlated proportions or percentages. Psychometrika, 12(2), 153-157.",
        "Fleiss, J. L., Levin, B., & Paik, M. C. (2003). Statistical Methods for Rates and Proportions (3rd ed.). Wiley."
      )
    ),
    effect_regression = list(
      formula = if (has_design("Hierarchical")) {
        "Incremental Cohen's f2 = (R2_full - R2_reduced) / (1 - R2_full)."
      } else if (has_design("Logistic")) {
        "log odds ratio = log(OR); approximate Cohen's d = log(OR) * sqrt(3) / pi."
      } else if (has_design("Moderation")) {
        "Interaction Cohen's f2 = delta R-squared / (1 - delta R-squared)."
      } else {
        "Cohen's f2 = R-squared / (1 - R-squared)."
      },
      references = c(
        cohen,
        "Cohen, J., Cohen, P., West, S. G., & Aiken, L. S. (2003). Applied Multiple Regression/Correlation Analysis for the Behavioral Sciences (3rd ed.). Lawrence Erlbaum.",
        "Chinn, S. (2000). A simple method for converting an odds ratio to effect size for use in meta-analysis. Statistics in Medicine, 19(22), 3127-3131."
      )
    ),
    effect_gee = list(
      formula = if (has_design("binary", ignore.case = TRUE)) {
        "Cohen's h = 2 asin(sqrt(p1)) - 2 asin(sqrt(p2)); planning effect = h / sqrt(design effect)."
      } else if (has_design("supplied", ignore.case = TRUE)) {
        "Uses supplied Cohen's d."
      } else if (has_design("change", ignore.case = TRUE)) {
        "Cohen's d = [(post - pre) group 1 - (post - pre) group 2] / common outcome SD."
      } else if (has_design("parameter", ignore.case = TRUE)) {
        "Cohen's d = GEE group x time parameter estimate B / common outcome SD."
      } else {
        "Cohen's d = (estimated mean group 1 - estimated mean group 2) / common outcome SD."
      },
      references = c(
        cohen,
        "Liang, K.-Y., & Zeger, S. L. (1986). Longitudinal data analysis using generalized linear models. Biometrika, 73(1), 13-22.",
        "Diggle, P. J., Heagerty, P., Liang, K.-Y., & Zeger, S. L. (2002). Analysis of Longitudinal Data (2nd ed.). Oxford University Press.",
        "Fleiss, J. L., Levin, B., & Paik, M. C. (2003). Statistical Methods for Rates and Proportions (3rd ed.). Wiley."
      )
    ),
    effect_glmm = list(
      formula = if (has_design("binary", ignore.case = TRUE)) {
        "Binary logit GLMM: log(OR) = B, OR = exp(B), approximate latent-scale d = log(OR) * sqrt(3) / pi. For probabilities, odds ratio and Cohen's h are both reported."
      } else if (has_design("count|rate", ignore.case = TRUE)) {
        "Count GLMM with log link: log(IRR) = B, IRR = exp(B). For rates, rate ratio = rate1 / rate2."
      } else {
        "Gaussian identity-link mixed model: Cohen's d = fixed-effect coefficient B / residual SD."
      },
      references = c(
        cohen,
        "Chinn, S. (2000). A simple method for converting an odds ratio to effect size for use in meta-analysis. Statistics in Medicine, 19(22), 3127-3131.",
        "Haddock, C. K., Rindskopf, D., & Shadish, W. R. (1998). Using odds ratios as effect sizes for meta-analysis of dichotomous data: A primer on methods and issues. Psychological Methods, 3(3), 339-353.",
        "McCullagh, P., & Nelder, J. A. (1989). Generalized Linear Models (2nd ed.). Chapman and Hall."
      )
    ),
    effect_lmm = list(
      formula = if (has_design("SPSS", ignore.case = TRUE)) {
        "Omnibus partial eta squared = F * df_effect / (F * df_effect + df_error). Optional pairwise Cohen's dz = mean difference / sqrt(Var_i + Var_j - 2Cov_ij)."
      } else if (has_design("GLIMMPSE", ignore.case = TRUE)) {
        "Standardized change effect = last-minus-first mean contrast / residual SD; planning effect = d * sqrt(m / design effect)."
      } else {
        "Repeated-measures planning effect = standardized fixed effect * sqrt(m / [1 + (m - 1)ICC])."
      },
      references = c(
        cohen,
        "Muller, K. E., & Stewart, P. W. (2006). Linear Model Theory: Univariate, Multivariate, and Mixed Models. Wiley.",
        "Guo, Y., & Johnson, W. D. (1996). Sample size and power for the generalized linear mixed model. Statistics in Medicine, 15(12), 1295-1307.",
        "Kreidler, S. M., Muller, K. E., Grunwald, G. K., Ringham, B. M., Coker-Dukowitz, Z. T., Sakhadeo, U. R., Barton, A. E., & Glueck, D. H. (2013). GLIMMPSE: Online power computation for linear models with and without a baseline covariate. Journal of Statistical Software, 54(10), 1-26."
      )
    ),
    effect_survival = list(
      formula = "Log hazard ratio = log(HR). For survival meta-analysis, use log(HR) with its standard error.",
      references = c(
        "Parmar, M. K. B., Torri, V., & Stewart, L. (1998). Extracting summary statistics to perform meta-analyses of the published literature for survival endpoints. Statistics in Medicine, 17(24), 2815-2834.",
        "Tierney, J. F., Stewart, L. A., Ghersi, D., Burdett, S., & Sydes, M. R. (2007). Practical methods for incorporating summary time-to-event data into meta-analysis. Trials, 8, 16."
      )
    ),
    effect_equivalence = list(
      formula = if (has_design("Equivalence")) {
        "Equivalence distance = margin - abs(observed effect); standardized distance divides this value by SD or pooled Bernoulli SD."
      } else {
        "Non-inferiority distance = margin + observed effect for a -margin boundary; standardized distance divides this value by SD or pooled Bernoulli SD."
      },
      references = c(
        "Schuirmann, D. J. (1987). A comparison of the two one-sided tests procedure and the power approach for assessing the equivalence of average bioavailability. Journal of Pharmacokinetics and Biopharmaceutics, 15(6), 657-680.",
        "Blackwelder, W. C. (1982). Proving the null hypothesis in clinical trials. Controlled Clinical Trials, 3(4), 345-353.",
        "Chow, S.-C., Shao, J., Wang, H., & Lokhnygina, Y. (2017). Sample Size Calculations in Clinical Research (3rd ed.). CRC Press."
      )
    ),
    effect_diagnostic = list(
      formula = "AUC is reported directly; AUC difference = AUC - null AUC; approximate Cohen's d = sqrt(2) * qnorm(AUC).",
      references = c(
        "Hanley, J. A., & McNeil, B. J. (1982). The meaning and use of the area under a receiver operating characteristic (ROC) curve. Radiology, 143(1), 29-36.",
        "Hajian-Tilaki, K. (2014). Sample size estimation in diagnostic test studies of biomedical informatics. Journal of Biomedical Informatics, 48, 193-204."
      )
    ),
    effect_rates = list(
      formula = if (has_design("Gamma", ignore.case = TRUE)) {
        "For gamma regression with a log link, mean ratio = exp(beta), and log mean ratio = beta."
      } else {
        "For Poisson and negative binomial regression with a log link, incidence rate ratio = exp(beta), and log incidence rate ratio = beta."
      },
      references = c(
        "Signorini, D. F. (1991). Sample size for Poisson regression. Biometrika, 78(2), 446-450.",
        "Zhu, H., & Lakkis, H. (2014). Sample size calculation for comparing two negative binomial rates. Statistics in Medicine, 33(3), 376-387.",
        "McCullagh, P., & Nelder, J. A. (1989). Generalized Linear Models (2nd ed.). Chapman and Hall."
      )
    ),
    effect_cluster = list(
      formula = if (has_design("binary", ignore.case = TRUE)) {
        "Cohen's h is adjusted for cluster planning as h / sqrt(1 + (m - 1)ICC)."
      } else if (has_design("Stepped")) {
        "Planning effect applies d / sqrt([1 + (m - 1)ICC] * periods / [periods - 1])."
      } else {
        "Continuous cluster planning effect = d / sqrt(1 + (m - 1)ICC)."
      },
      references = c(
        "Donner, A., & Klar, N. (2000). Design and Analysis of Cluster Randomization Trials in Health Research. Arnold.",
        "Hayes, R. J., & Moulton, L. H. (2017). Cluster Randomised Trials (2nd ed.). CRC Press.",
        "Hussey, M. A., & Hughes, J. P. (2007). Design and analysis of stepped wedge cluster randomized trials. Contemporary Clinical Trials, 28(2), 182-191."
      )
    ),
    effect_precision = list(
      formula = if (has_design("Mean")) {
        "Standardized half-width = desired mean CI half-width / SD."
      } else if (has_design("Proportion")) {
        "Bernoulli-standardized half-width = desired proportion CI half-width / sqrt(p[1 - p])."
      } else {
        "Correlation precision uses Fisher's z transformation; z half-width is computed from r +/- desired raw-r half-width."
      },
      references = c(
        "Kelley, K., & Maxwell, S. E. (2003). Sample size for multiple regression: Obtaining regression coefficients that are accurate, not simply significant. Psychological Methods, 8(3), 305-321.",
        "Bonett, D. G. (2002). Sample size requirements for estimating intraclass correlations with desired precision. Statistics in Medicine, 21(9), 1331-1335.",
        "Fisher, R. A. (1921). On the probable error of a coefficient of correlation deduced from a small sample. Metron, 1, 3-32."
      )
    ),
    effect_reliability = list(
      formula = if (has_design("alpha", ignore.case = TRUE)) {
        "Alpha difference = alpha - reference alpha; Bonett-style transformed alpha uses log(1 - alpha)."
      } else if (has_design("ICC")) {
        "ICC difference = ICC - reference ICC; transformed ICC uses Fisher's z."
      } else if (has_design("kappa", ignore.case = TRUE)) {
        "Cohen's kappa is reported directly; observed agreement assumes equal category prevalence."
      } else {
        "Bland-Altman limits of agreement are mean difference +/- 1.96 * SD of paired differences."
      },
      references = c(
        "Bonett, D. G. (2002). Sample size requirements for testing and estimating coefficient alpha. Journal of Educational and Behavioral Statistics, 27(4), 335-340.",
        "Bonett, D. G. (2002). Sample size requirements for estimating intraclass correlations with desired precision. Statistics in Medicine, 21(9), 1331-1335.",
        "Sim, J., & Wright, C. C. (2005). The kappa statistic in reliability studies: Use, interpretation, and sample size requirements. Physical Therapy, 85(3), 257-268.",
        "Bland, J. M., & Altman, D. G. (1986). Statistical methods for assessing agreement between two methods of clinical measurement. Lancet, 1(8476), 307-310."
      )
    ),
    effect_sem = list(
      formula = if (has_design("RMSEA")) {
        "RMSEA effect is alternative RMSEA - null RMSEA; noncentrality difference per N = df * (RMSEA_alt^2 - RMSEA_null^2)."
      } else if (has_design("parameter", ignore.case = TRUE) || has_design("path|loading|correlation", ignore.case = TRUE)) {
        "Standardized SEM parameter effect is the expected standardized coefficient; Fisher's z = atanh(parameter)."
      } else {
        "Complexity effect summarizes observed/latent/path burden per free parameter plus Fisher-z transformed expected loading and path effects."
      },
      references = c(
        "MacCallum, R. C., Browne, M. W., & Sugawara, H. M. (1996). Power analysis and determination of sample size for covariance structure modeling. Psychological Methods, 1(2), 130-149.",
        "Wolf, E. J., Harrington, K. M., Clark, S. L., & Miller, M. W. (2013). Sample size requirements for structural equation models: An evaluation of power, bias, and solution propriety. Educational and Psychological Measurement, 73(6), 913-934.",
        "Kline, R. B. (2023). Principles and Practice of Structural Equation Modeling (5th ed.). Guilford Press."
      )
    ),
    ttest = list(
      formula = "Uses the noncentral t distribution for exact t-test power when available; unequal two-group allocation uses a normal approximation with Cohen's d.",
      references = c(
        cohen,
        "R Core Team. stats::power.t.test documentation."
      )
    ),
    nonparametric = list(
      formula = if (has_design("Kruskal|Friedman")) {
        "Uses a large-sample noncentral chi-square approximation for rank-based omnibus tests."
      } else {
        "Approximates Wilcoxon/Mann-Whitney sample size from the corresponding t-test effect size using asymptotic relative efficiency."
      },
      references = if (has_design("Kruskal")) {
        c(
          "Kruskal, W. H., & Wallis, W. A. (1952). Use of ranks in one-criterion variance analysis. Journal of the American Statistical Association, 47(260), 583-621.",
          "Noether, G. E. (1987). Sample size determination for some common nonparametric tests. Journal of the American Statistical Association, 82(398), 645-647."
        )
      } else if (has_design("Friedman")) {
        c(
          "Friedman, M. (1937). The use of ranks to avoid the assumption of normality implicit in the analysis of variance. Journal of the American Statistical Association, 32(200), 675-701.",
          "Kendall, M. G., & Smith, B. B. (1939). The problem of m rankings. The Annals of Mathematical Statistics, 10(3), 275-287."
        )
      } else {
        c(
          "Noether, G. E. (1987). Sample size determination for some common nonparametric tests. Journal of the American Statistical Association, 82(398), 645-647.",
          cohen
        )
      }
    ),
    proportion = list(
      formula = "Uses normal-approximation power for one- or two-proportion tests with optional allocation ratio.",
      references = c(
        "Fleiss, J. L., Levin, B., & Paik, M. C. (2003). Statistical Methods for Rates and Proportions (3rd ed.). Wiley.",
        chow
      )
    ),
    chisquare = list(
      formula = "Uses Cohen's w with the noncentral chi-square distribution.",
      references = c(cohen)
    ),
    correlation = list(
      formula = "Uses Fisher's z transformation for Pearson correlation power and sample size.",
      references = c(cohen)
    ),
    anova = list(
      formula = if (has_design("Kruskal|Friedman")) {
        "Uses large-sample chi-square approximation for rank-based omnibus tests."
      } else {
        "Uses Cohen's f with noncentral F approximation; repeated-measures options adjust by average correlation and epsilon."
      },
      references = c(
        cohen,
        "Kreidler, S. M., Muller, K. E., Grunwald, G. K., Ringham, B. M., Coker-Dukowitz, Z. T., Sakhadeo, U. R., Baron, A. E., & Glueck, D. H. (2013). GLIMMPSE: Online power computation for linear models with and without a baseline covariate. Journal of Statistical Software, 54(10)."
      )
    ),
    ancova = list(
      formula = if (has_design("MANOVA")) {
        "Uses an approximate MANOVA power calculation by transforming Pillai's trace V to f2 = V / (1 - V), then applying an F-style noncentrality approximation."
      } else if (has_design("Ranked")) {
        "Uses an ANCOVA noncentral F approximation after rank transformation, with covariate R-squared residual-variance adjustment and asymptotic relative efficiency penalty."
      } else {
        "Uses an ANCOVA noncentral F approximation with effect size f adjusted by the covariate-explained residual variance: f_adjusted = f / sqrt(1 - R2)."
      },
      references = if (has_design("MANOVA")) {
        c(
          "Muller, K. E., & Peterson, B. L. (1984). Practical methods for computing power in testing the multivariate general linear hypothesis. Computational Statistics & Data Analysis, 2(2), 143-158.",
          "Kreidler, S. M., Muller, K. E., Grunwald, G. K., Ringham, B. M., Coker-Dukowitz, Z. T., Sakhadeo, U. R., Baron, A. E., & Glueck, D. H. (2013). GLIMMPSE: Online power computation for linear models with and without a baseline covariate. Journal of Statistical Software, 54(10)."
        )
      } else if (has_design("Ranked")) {
        c(
          "Quade, D. (1967). Rank analysis of covariance. Journal of the American Statistical Association, 62(320), 1187-1200.",
          "Conover, W. J., & Iman, R. L. (1982). Analysis of covariance using the rank transformation. Biometrics, 38(3), 715-724.",
          "Borm, G. F., Fransen, J., & Lemmens, W. A. J. G. (2007). A simple sample size formula for analysis of covariance in randomized clinical trials. Journal of Clinical Epidemiology, 60(12), 1234-1238."
        )
      } else {
        c(
          "Borm, G. F., Fransen, J., & Lemmens, W. A. J. G. (2007). A simple sample size formula for analysis of covariance in randomized clinical trials. Journal of Clinical Epidemiology, 60(12), 1234-1238.",
          cohen
        )
      }
    ),
    regression = list(
      formula = if (has_design("Logistic")) {
        "Uses a Hsieh-style Wald approximation for a logistic regression odds ratio with event probability, predictor prevalence, and covariate R-squared adjustment."
      } else if (has_design("Mediation")) {
        switch(
          result$mediation_method %||% "",
          fritz_mackinnon = "Uses Fritz & MacKinnon (2007) empirical Table 3 sample-size estimates for .80 power to detect the mediated effect.",
          monte_carlo = "Uses a Monte Carlo percentile confidence interval simulation for the indirect effect.",
          bootstrap = "Uses bootstrap confidence interval simulation for the indirect effect.",
          sobel = "Uses a Sobel / first-order delta approximation for the indirect effect.",
          "Uses Sobel, Monte Carlo percentile confidence interval, bootstrap confidence interval simulation, or Fritz & MacKinnon empirical table estimates for the indirect effect."
        )
      } else {
        "Uses Cohen's f2 with a noncentral F test for overall, incremental, or interaction-term regression effects."
      },
      references = if (has_design("Logistic")) {
        c("Hsieh, F. Y., Bloch, D. A., & Larsen, M. D. (1998). A simple method of sample size calculation for linear and logistic regression. Statistics in Medicine, 17(14), 1623-1634.")
      } else if (has_design("Mediation")) {
        switch(
          result$mediation_method %||% "",
          fritz_mackinnon = c("Fritz, M. S., & MacKinnon, D. P. (2007). Required sample size to detect the mediated effect. Psychological Science, 18(3), 233-239."),
          monte_carlo = c(
            "Preacher, K. J., & Selig, J. P. (2012). Advantages of Monte Carlo confidence intervals for indirect effects. Communication Methods and Measures, 6(2), 77-98.",
            "MacKinnon, D. P., Lockwood, C. M., & Williams, J. (2004). Confidence limits for the indirect effect: Distribution of the product and resampling methods. Multivariate Behavioral Research, 39(1), 99-128."
          ),
          bootstrap = c(
            "MacKinnon, D. P., Lockwood, C. M., & Williams, J. (2004). Confidence limits for the indirect effect: Distribution of the product and resampling methods. Multivariate Behavioral Research, 39(1), 99-128.",
            "Efron, B., & Tibshirani, R. J. (1993). An Introduction to the Bootstrap. Chapman & Hall/CRC."
          ),
          sobel = c(
            "Sobel, M. E. (1982). Asymptotic confidence intervals for indirect effects in structural equation models. Sociological Methodology, 13, 290-312.",
            "MacKinnon, D. P., Lockwood, C. M., Hoffman, J. M., West, S. G., & Sheets, V. (2002). A comparison of methods to test mediation and other intervening variable effects. Psychological Methods, 7(1), 83-104."
          ),
          c(
            "Fritz, M. S., & MacKinnon, D. P. (2007). Required sample size to detect the mediated effect. Psychological Science, 18(3), 233-239.",
            "MacKinnon, D. P., Lockwood, C. M., & Williams, J. (2004). Confidence limits for the indirect effect: Distribution of the product and resampling methods. Multivariate Behavioral Research, 39(1), 99-128.",
            "Preacher, K. J., & Selig, J. P. (2012). Advantages of Monte Carlo confidence intervals for indirect effects. Communication Methods and Measures, 6(2), 77-98."
          )
        )
      } else {
        c(cohen, "Hsieh, F. Y., Bloch, D. A., & Larsen, M. D. (1998). A simple method of sample size calculation for linear and logistic regression. Statistics in Medicine, 17(14), 1623-1634.")
      }
    ),
    gee = list(
      formula = "Uses an independent two-group test as a baseline and multiplies by a repeated-measures design effect from the working correlation. For unstructured correlation, enter upper-triangle pairwise correlations, for example r12, r13, r23 for three time points.",
      references = c(
        "Liang, K.-Y., & Zeger, S. L. (1986). Longitudinal data analysis using generalized linear models. Biometrika, 73(1), 13-22.",
        "Diggle, P. J., Heagerty, P., Liang, K.-Y., & Zeger, S. L. (2002). Analysis of Longitudinal Data (2nd ed.). Oxford University Press."
      )
    ),
    lmm = list(
      formula = if (has_design("GLIMMPSE-style")) {
        "Uses simulation-based power from user-specified time-specific means, residual SD, and repeated-measures correlation; nlme::gls tests the time or group x time hypothesis across simulated datasets. For unstructured correlation, enter upper-triangle pairwise correlations, for example r12, r13, r23 for three time points."
      } else if (identical(result$engine, "longpower")) {
        "Uses longpower::diggle.linear.power for closed-form longitudinal linear model slope/change power with exchangeable random-intercept correlation. The standardized fixed effect is treated as the group x time slope/change difference per residual SD."
      } else {
        "Uses simulation-based power: repeated-measures data are generated from the specified fixed effect, time points, ICC, and random-intercept LMM, then nlme::lme p-values are counted across simulations."
      },
      references = c(
        "Diggle, P. J., Heagerty, P., Liang, K.-Y., & Zeger, S. L. (2002). Analysis of Longitudinal Data (2nd ed.). Oxford University Press.",
        "Kreidler, S. M., Muller, K. E., Grunwald, G. K., Ringham, B. M., Coker-Dukowitz, Z. T., Sakhadeo, U. R., Baron, A. E., & Glueck, D. H. (2013). GLIMMPSE: Online power computation for linear models with and without a baseline covariate. Journal of Statistical Software, 54(10).",
        "Pinheiro, J. C., & Bates, D. M. (2000). Mixed-Effects Models in S and S-PLUS. Springer."
      )
    ),
    survival = list(
      formula = "Uses the Schoenfeld event-based approximation: required events are determined from log hazard ratio, alpha, power, and allocation fraction, then converted to total sample size by the expected overall event probability.",
      references = c(
        "Schoenfeld, D. A. (1983). Sample-size formula for the proportional-hazards regression model. Biometrics, 39(2), 499-503.",
        "Freedman, L. S. (1982). Tables of the number of patients required in clinical trials using the logrank test. Statistics in Medicine, 1(2), 121-129.",
        "Lachin, J. M., & Foulkes, M. A. (1986). Evaluation of sample size and power for analyses of survival with allowance for nonuniform patient entry, losses to follow-up, noncompliance, and stratification. Biometrics, 42(3), 507-519."
      )
    ),
    equivalence = list(
      formula = if (identical(result$engine, "TOSTER")) {
        "Uses TOSTER::power_t_TOST for exact t-based two-sample TOST equivalence power on a mean difference."
      } else {
        "Uses normal-approximation sample size for one-sided non-inferiority or TOST equivalence tests on a mean or proportion difference."
      },
      references = c(
        "Lakens, D., Scheel, A. M., & Isager, P. M. (2018). Equivalence testing for psychological research: A tutorial. Advances in Methods and Practices in Psychological Science, 1(2), 259-269.",
        "Blackwelder, W. C. (1982). Proving the null hypothesis in clinical trials. Controlled Clinical Trials, 3(4), 345-353.",
        "Julious, S. A. (2004). Sample sizes for clinical trials with Normal data. Statistics in Medicine, 23(12), 1921-1986.",
        "Chow, S.-C., Shao, J., Wang, H., & Lokhnygina, Y. (2017). Sample Size Calculations in Clinical Research (3rd ed.). CRC Press."
      )
    ),
    diagnostic = list(
      formula = if (has_design("ROC")) {
        "Uses the Hanley-McNeil AUC variance approximation to test one ROC AUC against a null AUC."
      } else {
        "Uses Buderer's precision-based formula for sensitivity or specificity, incorporating disease prevalence and desired confidence interval half-width."
      },
      references = c(
        "Buderer, N. M. F. (1996). Statistical methodology: I. Incorporating the prevalence of disease into the sample size calculation for sensitivity and specificity. Academic Emergency Medicine, 3(9), 895-900.",
        "Hanley, J. A., & McNeil, B. J. (1982). The meaning and use of the area under a receiver operating characteristic (ROC) curve. Radiology, 143(1), 29-36.",
        "Obuchowski, N. A., & McClish, D. K. (1997). Sample size determination for diagnostic accuracy studies involving binormal ROC curve indices. Statistics in Medicine, 16(13), 1529-1542."
      )
    ),
    precision = list(
      formula = if (has_design("Correlation")) {
        "Uses Fisher's z transformation to approximate the sample size needed for a desired correlation confidence interval half-width."
      } else if (has_design("Proportion")) {
        "Uses the normal-approximation formula n = z^2 p(1-p) / d^2 for a desired proportion confidence interval half-width."
      } else {
        "Uses the normal-approximation formula n = (z SD / d)^2 for a desired mean confidence interval half-width."
      },
      references = c(
        "Cochran, W. G. (1977). Sampling Techniques (3rd ed.). Wiley.",
        "Hulley, S. B., Cummings, S. R., Browner, W. S., Grady, D. G., & Newman, T. B. (2013). Designing Clinical Research (4th ed.). Lippincott Williams & Wilkins.",
        "Bonett, D. G., & Wright, T. A. (2000). Sample size requirements for estimating Pearson, Kendall and Spearman correlations. Psychometrika, 65(1), 23-28."
      )
    ),
    mcnemar = list(
      formula = "Uses a normal approximation to McNemar's paired binary test based on the two discordant pair probabilities p01 and p10.",
      references = c(
        "McNemar, Q. (1947). Note on the sampling error of the difference between correlated proportions or percentages. Psychometrika, 12(2), 153-157.",
        "Connor, R. J. (1987). Sample size for testing differences in proportions for the paired-sample design. Biometrics, 43(1), 207-211.",
        "Dupont, W. D. (1988). Power calculations for matched case-control studies. Biometrics, 44(4), 1157-1168."
      )
    ),
    rates = list(
      formula = if (has_design("negative binomial", ignore.case = TRUE)) {
        "Uses a Wald approximation for two negative binomial rates with variance inflated by dispersion: Var(Y) = mu + dispersion * mu^2."
      } else if (has_design("Single")) {
        "Uses the normal approximation to a Poisson rate confidence interval to estimate required person-time for a desired half-width."
      } else {
        "Uses a Wald normal approximation for comparing two independent Poisson incidence rates with a person-time allocation ratio."
      },
      references = c(
        "Signorini, D. F. (1991). Sample size for Poisson regression. Biometrika, 78(2), 446-450.",
        "Chow, S.-C., Shao, J., Wang, H., & Lokhnygina, Y. (2017). Sample Size Calculations in Clinical Research (3rd ed.). CRC Press.",
        "Zhu, H., & Lakkis, H. (2014). Sample size calculation for comparing two negative binomial rates. Statistics in Medicine, 33(3), 376-387."
      )
    ),
    cluster = list(
      formula = if (has_design("Stepped-wedge")) {
        "Uses simulation-based power for a cross-sectional stepped-wedge cluster trial fitted with fixed period effects and a random cluster intercept."
      } else if (identical(result$engine, "WebPower")) {
        "Uses WebPower::wp.crt2arm for a parallel 2-arm continuous cluster randomized trial. The returned total cluster count is rounded up to balanced clusters per group."
      } else {
        "Uses the standard design effect DE = 1 + (m - 1) ICC to inflate an individually randomized two-group sample size, then rounds to whole clusters."
      },
      references = c(
        "Zhang, Z., & Yuan, K.-H. (2018). Practical Statistical Power Analysis Using Webpower and R. ISDSA Press.",
        "Donner, A., & Klar, N. (2000). Design and Analysis of Cluster Randomization Trials in Health Research. Arnold.",
        "Hayes, R. J., & Bennett, S. (1999). Simple sample size calculation for cluster-randomized trials. International Journal of Epidemiology, 28(2), 319-326.",
        "Eldridge, S., & Kerry, S. (2012). A Practical Guide to Cluster Randomised Trials in Health Services Research. Wiley.",
        "Hussey, M. A., & Hughes, J. P. (2007). Design and analysis of stepped wedge cluster randomized trials. Contemporary Clinical Trials, 28(2), 182-191.",
        "Hemming, K., Girling, A. J., Sitch, A. J., Marsh, J., & Lilford, R. J. (2011). Sample size calculations for cluster randomised controlled trials with a fixed number of clusters. BMC Medical Research Methodology, 11, 102.",
        "Woertman, W., de Hoop, E., Moerbeek, M., Zuidema, S. U., Gerritsen, D. L., & Teerenstra, S. (2013). Stepped wedge designs could reduce the required sample size in cluster randomized trials. Journal of Clinical Epidemiology, 66(7), 752-758."
      )
    ),
    reliability = list(
      formula = if (has_design("Bland-Altman")) {
        "Uses an approximate confidence interval precision formula for Bland-Altman limits of agreement based on the SD of paired differences."
      } else if (has_design("alpha", ignore.case = TRUE)) {
        "Uses an approximate normal method for Cronbach's alpha precision based on the log(1 - alpha) transformation."
      } else if (has_design("ICC")) {
        "Uses an approximate Fisher z precision method for intraclass correlation reliability."
      } else {
        "Uses a large-sample normal approximation for Cohen's kappa precision assuming equal category prevalence."
      },
      references = c(
        "Bonett, D. G. (2002). Sample size requirements for testing and estimating coefficient alpha. Journal of Educational and Behavioral Statistics, 27(4), 335-340.",
        "Bonett, D. G. (2002). Sample size requirements for estimating intraclass correlations with desired precision. Statistics in Medicine, 21(9), 1331-1335.",
        "Donner, A., & Eliasziw, M. (1987). Sample size requirements for reliability studies. Statistics in Medicine, 6(4), 441-448.",
        "Walter, S. D., Eliasziw, M., & Donner, A. (1998). Sample size and optimal designs for reliability studies. Statistics in Medicine, 17(1), 101-110.",
        "Bland, J. M., & Altman, D. G. (1986). Statistical methods for assessing agreement between two methods of clinical measurement. The Lancet, 327(8476), 307-310.",
        "Lu, M.-J., Zhong, W.-H., Liu, Y.-X., Miao, H.-Z., Li, Y.-C., & Ji, M.-H. (2016). Sample size for assessing agreement between two methods of measurement by Bland-Altman method. The International Journal of Biostatistics, 12(2)."
      )
    ),
    sem = list(
      formula = if (has_design("complexity heuristic", ignore.case = TRUE)) {
        "Uses a model-complexity planning estimate: cases-per-free-parameter, observed/latent variable and structural path burden, and approximate detectability of expected standardized loading/path coefficients. The recommended N is the maximum of the component rules."
      } else if (has_design("parameter-level", ignore.case = TRUE)) {
        "Uses approximate Monte Carlo draws from a standardized SEM/CFA parameter estimate distribution. The standard error is based on a Fisher-z-style large-sample approximation with a model-complexity effective sample size adjustment."
      } else {
        "Uses RMSEA-based SEM/CFA model-level power with noncentrality parameter lambda = (N - 1) df RMSEA^2 and the noncentral chi-square distribution."
      },
      references = if (has_design("complexity heuristic", ignore.case = TRUE)) {
        c(
          "Bentler, P. M., & Chou, C.-P. (1987). Practical issues in structural modeling. Sociological Methods & Research, 16(1), 78-117.",
          "Jackson, D. L. (2003). Revisiting sample size and number of parameter estimates: Some support for the N:q hypothesis. Structural Equation Modeling, 10(1), 128-141.",
          "Wolf, E. J., Harrington, K. M., Clark, S. L., & Miller, M. W. (2013). Sample size requirements for structural equation models: An evaluation of power, bias, and solution propriety. Educational and Psychological Measurement, 73(6), 913-934.",
          "Westland, J. C. (2010). Lower bounds on sample size in structural equation modeling. Electronic Commerce Research and Applications, 9(6), 476-487."
        )
      } else if (has_design("parameter-level", ignore.case = TRUE)) {
        c(
          "Muthen, L. K., & Muthen, B. O. (2002). How to use a Monte Carlo study to decide on sample size and determine power. Structural Equation Modeling, 9(4), 599-620.",
          "Wolf, E. J., Harrington, K. M., Clark, S. L., & Miller, M. W. (2013). Sample size requirements for structural equation models: An evaluation of power, bias, and solution propriety. Educational and Psychological Measurement, 73(6), 913-934.",
          "MacCallum, R. C., Browne, M. W., & Sugawara, H. M. (1996). Power analysis and determination of sample size for covariance structure modeling. Psychological Methods, 1(2), 130-149."
        )
      } else {
        c(
          "MacCallum, R. C., Browne, M. W., & Sugawara, H. M. (1996). Power analysis and determination of sample size for covariance structure modeling. Psychological Methods, 1(2), 130-149.",
          "Preacher, K. J., & Coffman, D. L. (2006). Computing power and minimum sample size for RMSEA [Computer software]. Available from http://quantpsy.org/.",
          "Kim, K. H. (2005). The relation among fit indexes, power, and sample size in structural equation modeling. Structural Equation Modeling, 12(3), 368-390."
        )
      }
    ),
    list(formula = NULL, references = character(0))
  )
}

sample_size_results_ui <- function(result, language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  if (is.null(result)) {
    return(div(class = "empty-message", sample_size_ui_text(language, "assumptions_prompt")))
  }
  if (isTRUE(result$progress)) {
    return(div(
      class = "sample-size-progress",
      id = result$id,
      div(class = "sample-size-progress-text", result$text %||% paste0(sample_size_ui_text(language, "calculating"), " 0%")),
      div(
        class = "sample-size-progress-track",
        div(
          class = "sample-size-progress-bar",
          role = "progressbar",
          `aria-valuemin` = "0",
          `aria-valuemax` = "100",
          `aria-valuenow` = "0",
          style = "width:0%;"
        )
      ),
      if (!is.null(result$cancel_id)) {
        actionButton(result$cancel_id, sample_size_ui_text(language, "stop"), class = "btn btn-default btn-sm sample-size-stop")
      }
    ))
  }
  if (!is.null(result$error)) {
    return(div(class = "analysis-warning", result$error))
  }
  div(
    class = "sample-size-result-panel",
    sample_size_result_table(result),
    if (!is.null(result$method_note)) div(class = "sample-size-method-note", result$method_note),
    if (!is.null(result$formula_note)) div(class = "sample-size-method-note", strong(sample_size_ui_text(language, "formula_approximation")), result$formula_note),
    if (length(result$references %||% character(0)) > 0) {
      div(
        class = "sample-size-references",
        strong(sample_size_ui_text(language, "references")),
        tags$ul(lapply(result$references, tags$li))
      )
    }
  )
}

sample_size_calculate <- function(method, input, progress = NULL) {
  target <- input[[paste0("sample_size_", method, "_target")]] %||% "sample_size"
  alpha <- as.numeric(input[[paste0("sample_size_", method, "_alpha")]])
  power <- as.numeric(input[[paste0("sample_size_", method, "_power")]])
  n <- as.numeric(input[[paste0("sample_size_", method, "_n")]])
  ratio <- as.numeric(input[[paste0("sample_size_", method, "_ratio")]] %||% 1)
  alternative <- input[[paste0("sample_size_", method, "_alternative")]] %||% "two.sided"
  dropout <- as.numeric(input[[paste0("sample_size_", method, "_dropout")]] %||% 0) / 100
  if (
    identical(method, "regression") &&
      identical(input$sample_size_regression_design %||% "multiple", "mediation") &&
      identical(input$sample_size_regression_mediation_method %||% "monte_carlo", "fritz_mackinnon")
  ) {
    power <- 0.8
  }

  tryCatch({
    result <- switch(
      method,
      effectsize = sample_size_effect_size(
        design = input$sample_size_effectsize_design %||% "independent_means",
        mean1 = as.numeric(input$sample_size_effectsize_mean1),
        mean2 = as.numeric(input$sample_size_effectsize_mean2),
        sd1 = as.numeric(input$sample_size_effectsize_sd1),
        sd2 = as.numeric(input$sample_size_effectsize_sd2),
        n1 = as.numeric(input$sample_size_effectsize_n1),
        n2 = as.numeric(input$sample_size_effectsize_n2),
        mean_difference = as.numeric(input$sample_size_effectsize_mean_difference),
        sd_difference = as.numeric(input$sample_size_effectsize_sd_difference),
        null_mean = as.numeric(input$sample_size_effectsize_null_mean %||% 0)
      ),
      ttest = sample_size_ttest(target, input$sample_size_ttest_design %||% "two_sample", as.numeric(input$sample_size_ttest_effect), alpha, power, n, ratio, alternative, dropout),
      nonparametric = sample_size_nonparametric(
        target,
        input$sample_size_nonparametric_design %||% "two_independent",
        as.numeric(input$sample_size_nonparametric_effect),
        alpha,
        power,
        n,
        ratio,
        alternative,
        dropout,
        groups = as.numeric(input$sample_size_nonparametric_groups %||% 3),
        measurements = as.numeric(input$sample_size_nonparametric_measurements %||% 3)
      ),
      proportion = sample_size_proportion(target, input$sample_size_proportion_design %||% "two_proportion", as.numeric(input$sample_size_proportion_p1), as.numeric(input$sample_size_proportion_p2), alpha, power, n, ratio, alternative, dropout),
      chisquare = sample_size_chisquare(target, as.numeric(input$sample_size_chisquare_df), as.numeric(input$sample_size_chisquare_effect), alpha, power, n, dropout),
      correlation = sample_size_correlation(target, as.numeric(input$sample_size_correlation_r), alpha, power, n, alternative, dropout),
      anova = sample_size_anova(
        target = target,
        design = input$sample_size_anova_design %||% "one_way",
        groups = as.numeric(input$sample_size_anova_groups),
        effect_size = as.numeric(input$sample_size_anova_effect),
        alpha = alpha,
        power = power,
        n = n,
        dropout = dropout,
        factor_a_levels = as.numeric(input$sample_size_anova_factor_a),
        factor_b_levels = as.numeric(input$sample_size_anova_factor_b),
        effect = input$sample_size_anova_effect_test %||% "interaction",
        measurements = as.numeric(input$sample_size_anova_measurements),
        repeated_correlation = as.numeric(input$sample_size_anova_correlation %||% 0.5),
        epsilon = as.numeric(input$sample_size_anova_epsilon %||% 1)
      ),
      ancova = sample_size_ancova(
        target = target,
        design = input$sample_size_ancova_design %||% "ancova",
        groups = as.numeric(input$sample_size_ancova_groups),
        outcomes = as.numeric(input$sample_size_ancova_outcomes %||% 2),
        effect_size = as.numeric(input$sample_size_ancova_effect),
        covariates = as.numeric(input$sample_size_ancova_covariates %||% 1),
        covariate_r2 = as.numeric(input$sample_size_ancova_covariate_r2 %||% 0.3),
        alpha = alpha,
        power = power,
        n = n,
        dropout = dropout
      ),
      regression = sample_size_regression(
        target = target,
        design = input$sample_size_regression_design %||% "multiple",
        effect_size = as.numeric(input$sample_size_regression_effect),
        alpha = alpha,
        power = power,
        n = n,
        dropout = dropout,
        predictors = as.numeric(input$sample_size_regression_predictors),
        tested_predictors = as.numeric(input$sample_size_regression_tested),
        total_predictors = as.numeric(input$sample_size_regression_total_predictors),
        interaction_terms = as.numeric(input$sample_size_regression_interactions),
        a_path = as.numeric(input$sample_size_regression_a),
        b_path = as.numeric(input$sample_size_regression_b),
        a_effect = input$sample_size_regression_a_effect %||% "medium",
        b_effect = input$sample_size_regression_b_effect %||% "medium",
        fritz_mackinnon_test = input$sample_size_regression_fritz_test %||% "bias_corrected_bootstrap",
        covariates = as.numeric(input$sample_size_regression_covariates %||% 0),
        odds_ratio = as.numeric(input$sample_size_regression_or),
        p0 = as.numeric(input$sample_size_regression_p0),
        predictor_prevalence = as.numeric(input$sample_size_regression_predictor_prevalence %||% 0.5),
      covariate_r2 = as.numeric(input$sample_size_regression_covariate_r2 %||% 0),
      alternative = alternative,
      mediation_method = input$sample_size_regression_mediation_method %||% "monte_carlo",
        mediation_simulations = as.numeric(input$sample_size_regression_simulations %||% 30),
        mediation_bootstraps = as.numeric(input$sample_size_regression_bootstraps %||% 100),
        progress = progress
      ),
      gee = sample_size_gee(
        target = target,
        outcome = input$sample_size_gee_outcome %||% "continuous",
        effect_size = as.numeric(input$sample_size_gee_effect),
        p1 = as.numeric(input$sample_size_gee_p1),
        p2 = as.numeric(input$sample_size_gee_p2),
        alpha = alpha,
        power = power,
        n = n,
        ratio = ratio,
        alternative = alternative,
        dropout = dropout,
        time_points = as.numeric(input$sample_size_gee_time_points),
        rho = as.numeric(input$sample_size_gee_rho),
        structure = input$sample_size_gee_correlation_structure %||% "exchangeable",
        correlations = input$sample_size_gee_correlations
      ),
      lmm = sample_size_lmm(
        target = target,
        mode = input$sample_size_lmm_mode %||% "simple",
        design = input$sample_size_lmm_design %||% "two_group_repeated",
        effect_size = as.numeric(input$sample_size_lmm_effect),
        alpha = alpha,
        power = power,
        n = n,
        dropout = dropout,
        time_points = as.numeric(input$sample_size_lmm_time_points),
        icc = as.numeric(input$sample_size_lmm_icc),
        simulations = as.numeric(input$sample_size_lmm_simulations %||% 100),
        group1_means = input$sample_size_lmm_group1_means,
        group2_means = input$sample_size_lmm_group2_means,
        residual_sd = as.numeric(input$sample_size_lmm_residual_sd),
        rho = as.numeric(input$sample_size_lmm_rho %||% 0.5),
        structure = input$sample_size_lmm_correlation_structure %||% "exchangeable",
        correlations = input$sample_size_lmm_correlations,
        progress = progress
      ),
      survival = sample_size_survival(
        target = target,
        hazard_ratio = as.numeric(input$sample_size_survival_hr),
        event_probability = as.numeric(input$sample_size_survival_event_probability),
        alpha = alpha,
        power = power,
        n = n,
        ratio = ratio,
        alternative = alternative,
        dropout = dropout
      ),
      equivalence = sample_size_equivalence(
        target = target,
        outcome = input$sample_size_equivalence_outcome %||% "mean",
        objective = input$sample_size_equivalence_objective %||% "noninferiority",
        true_difference = as.numeric(input$sample_size_equivalence_difference %||% 0),
        margin = as.numeric(input$sample_size_equivalence_margin),
        sd = as.numeric(input$sample_size_equivalence_sd),
        p1 = as.numeric(input$sample_size_equivalence_p1),
        p2 = as.numeric(input$sample_size_equivalence_p2),
        alpha = alpha,
        power = power,
        n = n,
        ratio = ratio,
        dropout = dropout
      ),
      diagnostic = sample_size_diagnostic(
        target = target,
        design = input$sample_size_diagnostic_design %||% "sensitivity",
        sensitivity = as.numeric(input$sample_size_diagnostic_sensitivity),
        specificity = as.numeric(input$sample_size_diagnostic_specificity),
        prevalence = as.numeric(input$sample_size_diagnostic_prevalence),
        precision = as.numeric(input$sample_size_diagnostic_precision),
        auc = as.numeric(input$sample_size_diagnostic_auc),
        null_auc = as.numeric(input$sample_size_diagnostic_null_auc %||% 0.5),
        alpha = alpha,
        power = power,
        n = n,
        ratio = ratio,
        dropout = dropout
      ),
      precision = sample_size_precision(
        target = target,
        parameter = input$sample_size_precision_parameter %||% "mean",
        confidence_level = as.numeric(input$sample_size_precision_confidence),
        half_width = as.numeric(input$sample_size_precision_half_width),
        sd = as.numeric(input$sample_size_precision_sd),
        proportion = as.numeric(input$sample_size_precision_proportion),
        r = as.numeric(input$sample_size_precision_r),
        n = n,
        dropout = dropout
      ),
      mcnemar = sample_size_mcnemar(
        target = target,
        p01 = as.numeric(input$sample_size_mcnemar_p01),
        p10 = as.numeric(input$sample_size_mcnemar_p10),
        alpha = alpha,
        power = power,
        n = n,
        alternative = alternative,
        dropout = dropout
      ),
      rates = sample_size_rates(
        target = target,
        design = input$sample_size_rates_design %||% "two_rate_ratio",
        rate1 = as.numeric(input$sample_size_rates_rate1),
        rate2 = as.numeric(input$sample_size_rates_rate2),
        alpha = alpha,
        power = power,
        n = n,
        ratio = ratio,
        alternative = alternative,
        dropout = dropout,
        half_width = as.numeric(input$sample_size_rates_half_width),
        dispersion = as.numeric(input$sample_size_rates_dispersion %||% 0)
      ),
      cluster = sample_size_cluster(
        target = target,
        design = input$sample_size_cluster_design %||% "parallel",
        outcome = input$sample_size_cluster_outcome %||% "continuous",
        effect_size = as.numeric(input$sample_size_cluster_effect),
        p1 = as.numeric(input$sample_size_cluster_p1),
        p2 = as.numeric(input$sample_size_cluster_p2),
        alpha = alpha,
        power = power,
        n = n,
        ratio = ratio,
        alternative = alternative,
        dropout = dropout,
        cluster_size = as.numeric(input$sample_size_cluster_size),
        icc = as.numeric(input$sample_size_cluster_icc),
        periods = as.numeric(input$sample_size_cluster_periods),
        simulations = as.numeric(input$sample_size_cluster_simulations %||% 100),
        progress = progress
      ),
      reliability = sample_size_reliability(
        target = target,
        design = input$sample_size_reliability_design %||% "alpha",
        reliability = as.numeric(input$sample_size_reliability_value),
        confidence_level = as.numeric(input$sample_size_reliability_confidence),
        half_width = as.numeric(input$sample_size_reliability_half_width),
        items = as.numeric(input$sample_size_reliability_items %||% 2),
        categories = as.numeric(input$sample_size_reliability_categories %||% 2),
        dropout = dropout
      ),
      sem = sample_size_sem(
        target = target,
        test = input$sample_size_sem_test %||% "close_fit",
        df = as.numeric(input$sample_size_sem_df),
        df_source = input$sample_size_sem_df_source %||% "structure",
        null_rmsea = as.numeric(input$sample_size_sem_null_rmsea),
        alternative_rmsea = as.numeric(input$sample_size_sem_alternative_rmsea),
        parameter_type = input$sample_size_sem_parameter_type %||% "path",
        parameter = as.numeric(input$sample_size_sem_parameter),
        complexity = input$sample_size_sem_complexity %||% "moderate",
        simulations = as.numeric(input$sample_size_sem_simulations %||% 1000),
        latent_variables = as.numeric(input$sample_size_sem_latent_variables),
        measured_variables = as.numeric(input$sample_size_sem_measured_variables),
        structural_paths = as.numeric(input$sample_size_sem_structural_paths),
        free_parameters = as.numeric(input$sample_size_sem_free_parameters),
        expected_loading = as.numeric(input$sample_size_sem_expected_loading),
        expected_path = as.numeric(input$sample_size_sem_expected_path),
        alpha = alpha,
        power = power,
        n = n,
        dropout = dropout,
        progress = progress
      )
    )
    details <- sample_size_method_details(method, result)
    result$formula_note <- details$formula
    result$references <- details$references
    result
  },
    error = function(e) list(error = conditionMessage(e))
  )
}

effect_size_ttest_calculate <- function(input) {
  tryCatch(
    sample_size_effect_size(
      design = sample_size_choice(input$effect_size_ttest_design, "independent_means"),
      mean1 = as.numeric(input$effect_size_ttest_mean1),
      mean2 = as.numeric(input$effect_size_ttest_mean2),
      sd1 = as.numeric(input$effect_size_ttest_sd1),
      sd2 = as.numeric(input$effect_size_ttest_sd2),
      n1 = as.numeric(input$effect_size_ttest_n1),
      n2 = as.numeric(input$effect_size_ttest_n2),
      mean_difference = as.numeric(input$effect_size_ttest_mean_difference),
      sd_difference = as.numeric(input$effect_size_ttest_sd_difference),
      null_mean = as.numeric(input$effect_size_ttest_null_mean %||% 0),
      t_value = as.numeric(input$effect_size_ttest_t),
      df = as.numeric(input$effect_size_ttest_df),
      n = as.numeric(input$effect_size_ttest_n),
      r = as.numeric(input$effect_size_ttest_r)
    ),
    error = function(e) list(error = conditionMessage(e))
  )
}

effect_size_proportion_calculate <- function(input) {
  tryCatch({
    result <- sample_size_effect_size_proportion(
      design = sample_size_choice(input$effect_size_proportion_design, "cohens_h"),
      p1 = as.numeric(input$effect_size_proportion_p1),
      p2 = as.numeric(input$effect_size_proportion_p2),
      event1 = as.numeric(input$effect_size_proportion_event1),
      nonevent1 = as.numeric(input$effect_size_proportion_nonevent1),
      event2 = as.numeric(input$effect_size_proportion_event2),
      nonevent2 = as.numeric(input$effect_size_proportion_nonevent2)
    )
    details <- sample_size_method_details("effect_proportion", result)
    result$formula_note <- details$formula
    result$references <- details$references
    result
  }, error = function(e) list(error = conditionMessage(e)))
}

effect_size_chisquare_calculate <- function(input) {
  tryCatch({
    result <- sample_size_effect_size_chisquare(
      design = sample_size_choice(input$effect_size_chisquare_design, "cohens_w"),
      chi_square = as.numeric(input$effect_size_chisquare_statistic),
      n = as.numeric(input$effect_size_chisquare_n),
      rows = as.numeric(input$effect_size_chisquare_rows),
      columns = as.numeric(input$effect_size_chisquare_columns),
      observed = input$effect_size_chisquare_observed,
      expected = input$effect_size_chisquare_expected
    )
    details <- sample_size_method_details("effect_chisquare", result)
    result$formula_note <- details$formula
    result$references <- details$references
    result
  }, error = function(e) list(error = conditionMessage(e)))
}

effect_size_correlation_calculate <- function(input) {
  tryCatch({
    result <- sample_size_effect_size_correlation(
      design = sample_size_choice(input$effect_size_correlation_design, "r_from_t"),
      r = as.numeric(input$effect_size_correlation_r),
      r1 = as.numeric(input$effect_size_correlation_r1),
      r2 = as.numeric(input$effect_size_correlation_r2_compare),
      t_value = as.numeric(input$effect_size_correlation_t),
      f_value = as.numeric(input$effect_size_correlation_f),
      df = as.numeric(input$effect_size_correlation_df),
      r_squared = as.numeric(input$effect_size_correlation_r2)
    )
    details <- sample_size_method_details("effect_correlation", result)
    result$formula_note <- details$formula
    result$references <- details$references
    result
  }, error = function(e) list(error = conditionMessage(e)))
}

effect_size_anova_calculate <- function(input) {
  tryCatch({
    result <- sample_size_effect_size_anova(
      design = sample_size_choice(input$effect_size_anova_design, "partial_eta_from_f"),
      eta_squared = as.numeric(input$effect_size_anova_eta2),
      partial_eta_squared = as.numeric(input$effect_size_anova_partial_eta2),
      f_value = as.numeric(input$effect_size_anova_f),
      groups = as.numeric(input$effect_size_anova_groups),
      total_n = as.numeric(input$effect_size_anova_total_n)
    )
    details <- sample_size_method_details("effect_anova", result)
    result$formula_note <- details$formula
    result$references <- details$references
    result
  }, error = function(e) list(error = conditionMessage(e)))
}

effect_size_ancova_calculate <- function(input) {
  tryCatch({
    result <- sample_size_effect_size_ancova(
      design = sample_size_choice(input$effect_size_ancova_design, "ancova_partial_eta_from_f"),
      effect_size_f = as.numeric(input$effect_size_ancova_f),
      covariate_r2 = as.numeric(input$effect_size_ancova_covariate_r2),
      partial_eta_squared = as.numeric(input$effect_size_ancova_partial_eta2),
      pillai_trace = as.numeric(input$effect_size_ancova_pillai),
      wilks_lambda = as.numeric(input$effect_size_ancova_wilks),
      dependent_variables = as.numeric(input$effect_size_ancova_dependent_variables),
      f_value = as.numeric(input$effect_size_ancova_f_statistic),
      groups = as.numeric(input$effect_size_ancova_groups),
      total_n = as.numeric(input$effect_size_ancova_total_n)
    )
    details <- sample_size_method_details("effect_ancova", result)
    result$formula_note <- details$formula
    result$references <- details$references
    result
  }, error = function(e) list(error = conditionMessage(e)))
}

effect_size_nonparametric_calculate <- function(input) {
  tryCatch({
    result <- sample_size_effect_size_nonparametric(
      design = sample_size_choice(input$effect_size_nonparametric_design, "rank_biserial_from_u"),
      u = as.numeric(input$effect_size_nonparametric_u),
      w_positive = as.numeric(input$effect_size_nonparametric_w_positive),
      w_negative = as.numeric(input$effect_size_nonparametric_w_negative),
      h = as.numeric(input$effect_size_nonparametric_h),
      chi_square = as.numeric(input$effect_size_nonparametric_chi_square),
      n1 = as.numeric(input$effect_size_nonparametric_n1),
      n2 = as.numeric(input$effect_size_nonparametric_n2),
      n = as.numeric(input$effect_size_nonparametric_n),
      groups = as.numeric(input$effect_size_nonparametric_groups),
      measurements = as.numeric(input$effect_size_nonparametric_measurements)
    )
    details <- sample_size_method_details("effect_nonparametric", result)
    result$formula_note <- details$formula
    result$references <- details$references
    result
  }, error = function(e) list(error = conditionMessage(e)))
}

effect_size_mcnemar_calculate <- function(input) {
  tryCatch({
    result <- sample_size_effect_size_mcnemar(
      design = sample_size_choice(input$effect_size_mcnemar_design, "matched_or_probs"),
      p01 = as.numeric(input$effect_size_mcnemar_p01),
      p10 = as.numeric(input$effect_size_mcnemar_p10),
      b = as.numeric(input$effect_size_mcnemar_b),
      c = as.numeric(input$effect_size_mcnemar_c)
    )
    details <- sample_size_method_details("effect_mcnemar", result)
    result$formula_note <- details$formula
    result$references <- details$references
    result
  }, error = function(e) list(error = conditionMessage(e)))
}

effect_size_regression_calculate <- function(input) {
  tryCatch({
    result <- sample_size_effect_size_regression(
      design = sample_size_choice(input$effect_size_regression_design, "f2_from_r2"),
      r_squared = as.numeric(input$effect_size_regression_r2),
      full_r_squared = as.numeric(input$effect_size_regression_full_r2),
      reduced_r_squared = as.numeric(input$effect_size_regression_reduced_r2),
      odds_ratio = as.numeric(input$effect_size_regression_or),
      interaction_delta_r2 = as.numeric(input$effect_size_regression_delta_r2)
    )
    details <- sample_size_method_details("effect_regression", result)
    result$formula_note <- details$formula
    result$references <- details$references
    result
  }, error = function(e) list(error = conditionMessage(e)))
}

effect_size_gee_calculate <- function(input) {
  tryCatch({
    sd_mode <- sample_size_choice(input$effect_size_gee_sd_mode, "direct")
    common_sd <- if (identical(sd_mode, "pooled")) {
      sample_size_pooled_sd(
        n1 = as.numeric(input$effect_size_gee_n1),
        sd1 = as.numeric(input$effect_size_gee_sd1),
        n2 = as.numeric(input$effect_size_gee_n2),
        sd2 = as.numeric(input$effect_size_gee_sd2)
      )
    } else {
      as.numeric(input$effect_size_gee_sd)
    }
    result <- sample_size_effect_size_gee(
      design = sample_size_choice(input$effect_size_gee_design, "continuous_followup_means"),
      mean1 = as.numeric(input$effect_size_gee_mean1),
      mean2 = as.numeric(input$effect_size_gee_mean2),
      pre_mean1 = as.numeric(input$effect_size_gee_pre_mean1),
      post_mean1 = as.numeric(input$effect_size_gee_post_mean1),
      pre_mean2 = as.numeric(input$effect_size_gee_pre_mean2),
      post_mean2 = as.numeric(input$effect_size_gee_post_mean2),
      coefficient = as.numeric(input$effect_size_gee_coefficient),
      sd = common_sd,
      effect_size = as.numeric(input$effect_size_gee_d),
      p1 = as.numeric(input$effect_size_gee_p1),
      p2 = as.numeric(input$effect_size_gee_p2)
    )
    if (identical(sd_mode, "pooled")) {
      result$common_sd_method <- "Pooled SD from group n and SD"
      result$group1_n <- as.numeric(input$effect_size_gee_n1)
      result$group1_sd <- as.numeric(input$effect_size_gee_sd1)
      result$group2_n <- as.numeric(input$effect_size_gee_n2)
      result$group2_sd <- as.numeric(input$effect_size_gee_sd2)
    }
    details <- sample_size_method_details("effect_gee", result)
    result$formula_note <- details$formula
    result$references <- details$references
    result
  }, error = function(e) list(error = conditionMessage(e)))
}

effect_size_glmm_calculate <- function(input) {
  tryCatch({
    result <- sample_size_effect_size_glmm(
      design = sample_size_choice(input$effect_size_glmm_design, "binary_logit"),
      input_scale = if (identical(sample_size_choice(input$effect_size_glmm_design, "binary_logit"), "count_log")) {
        sample_size_choice(input$effect_size_glmm_count_scale, "coefficient")
      } else {
        sample_size_choice(input$effect_size_glmm_binary_scale, "coefficient")
      },
      coefficient = as.numeric(input$effect_size_glmm_coefficient),
      odds_ratio = as.numeric(input$effect_size_glmm_or),
      incidence_rate_ratio = as.numeric(input$effect_size_glmm_irr),
      p1 = as.numeric(input$effect_size_glmm_p1),
      p2 = as.numeric(input$effect_size_glmm_p2),
      rate1 = as.numeric(input$effect_size_glmm_rate1),
      rate2 = as.numeric(input$effect_size_glmm_rate2),
      sd = as.numeric(input$effect_size_glmm_sd)
    )
    details <- sample_size_method_details("effect_glmm", result)
    result$formula_note <- details$formula
    result$references <- details$references
    result
  }, error = function(e) list(error = conditionMessage(e)))
}

effect_size_lmm_calculate <- function(input) {
  tryCatch({
    result <- sample_size_effect_size_lmm(
      design = sample_size_choice(input$effect_size_lmm_design, "simple_fixed"),
      lmm_design = sample_size_choice(input$effect_size_lmm_lmm_design, "two_group_repeated"),
      effect_size = as.numeric(input$effect_size_lmm_effect),
      group1_means = input$effect_size_lmm_group1_means,
      group2_means = input$effect_size_lmm_group2_means,
      residual_sd = as.numeric(input$effect_size_lmm_residual_sd),
      f_statistic = as.numeric(input$effect_size_lmm_f_statistic),
      df_effect = as.numeric(input$effect_size_lmm_df_effect),
      df_error = as.numeric(input$effect_size_lmm_df_error),
      mean_difference = as.numeric(input$effect_size_lmm_mean_difference),
      variance_i = as.numeric(input$effect_size_lmm_variance_i),
      variance_j = as.numeric(input$effect_size_lmm_variance_j),
      covariance_ij = as.numeric(input$effect_size_lmm_covariance_ij),
      time_points = as.numeric(input$effect_size_lmm_time_points),
      icc = as.numeric(input$effect_size_lmm_icc),
      rho = as.numeric(input$effect_size_lmm_rho),
      structure = sample_size_choice(input$effect_size_lmm_correlation_structure, "exchangeable"),
      correlations = input$effect_size_lmm_correlations
    )
    details <- sample_size_method_details("effect_lmm", result)
    result$formula_note <- details$formula
    result$references <- details$references
    result
  }, error = function(e) list(error = conditionMessage(e)))
}

effect_size_survival_calculate <- function(input) {
  tryCatch({
    result <- sample_size_effect_size_survival(
      design = sample_size_choice(input$effect_size_survival_design, "hazard_ratio"),
      hazard_ratio = as.numeric(input$effect_size_survival_hr)
    )
    details <- sample_size_method_details("effect_survival", result)
    result$formula_note <- details$formula
    result$references <- details$references
    result
  }, error = function(e) list(error = conditionMessage(e)))
}

effect_size_equivalence_calculate <- function(input) {
  tryCatch({
    result <- sample_size_effect_size_equivalence(
      outcome = sample_size_choice(input$effect_size_equivalence_outcome, "mean"),
      objective = sample_size_choice(input$effect_size_equivalence_objective, "noninferiority"),
      true_difference = as.numeric(input$effect_size_equivalence_difference %||% 0),
      margin = as.numeric(input$effect_size_equivalence_margin),
      sd = as.numeric(input$effect_size_equivalence_sd),
      p1 = as.numeric(input$effect_size_equivalence_p1),
      p2 = as.numeric(input$effect_size_equivalence_p2)
    )
    details <- sample_size_method_details("effect_equivalence", result)
    result$formula_note <- details$formula
    result$references <- details$references
    result
  }, error = function(e) list(error = conditionMessage(e)))
}

effect_size_diagnostic_calculate <- function(input) {
  tryCatch({
    result <- sample_size_effect_size_diagnostic(
      design = sample_size_choice(input$effect_size_diagnostic_design, "auc"),
      auc = as.numeric(input$effect_size_diagnostic_auc),
      null_auc = as.numeric(input$effect_size_diagnostic_null_auc %||% 0.5)
    )
    details <- sample_size_method_details("effect_diagnostic", result)
    result$formula_note <- details$formula
    result$references <- details$references
    result
  }, error = function(e) list(error = conditionMessage(e)))
}

effect_size_rates_calculate <- function(input) {
  tryCatch({
    result <- sample_size_effect_size_rates(
      design = sample_size_choice(input$effect_size_rates_design, "poisson_irr"),
      input_scale = sample_size_choice(input$effect_size_rates_input_scale, "ratio"),
      ratio = as.numeric(input$effect_size_rates_ratio),
      log_ratio = as.numeric(input$effect_size_rates_log_ratio)
    )
    details <- sample_size_method_details("effect_rates", result)
    result$formula_note <- details$formula
    result$references <- details$references
    result
  }, error = function(e) list(error = conditionMessage(e)))
}

effect_size_cluster_calculate <- function(input) {
  tryCatch({
    result <- sample_size_effect_size_cluster(
      design = sample_size_choice(input$effect_size_cluster_design, "parallel_continuous"),
      effect_size = as.numeric(input$effect_size_cluster_effect),
      p1 = as.numeric(input$effect_size_cluster_p1),
      p2 = as.numeric(input$effect_size_cluster_p2),
      cluster_size = as.numeric(input$effect_size_cluster_size),
      icc = as.numeric(input$effect_size_cluster_icc),
      periods = as.numeric(input$effect_size_cluster_periods %||% 5)
    )
    details <- sample_size_method_details("effect_cluster", result)
    result$formula_note <- details$formula
    result$references <- details$references
    result
  }, error = function(e) list(error = conditionMessage(e)))
}

effect_size_precision_calculate <- function(input) {
  tryCatch({
    result <- sample_size_effect_size_precision(
      parameter = sample_size_choice(input$effect_size_precision_parameter, "mean"),
      estimate = as.numeric(input$effect_size_precision_estimate),
      half_width = as.numeric(input$effect_size_precision_half_width),
      sd = as.numeric(input$effect_size_precision_sd),
      proportion = as.numeric(input$effect_size_precision_proportion),
      r = as.numeric(input$effect_size_precision_r)
    )
    details <- sample_size_method_details("effect_precision", result)
    result$formula_note <- details$formula
    result$references <- details$references
    result
  }, error = function(e) list(error = conditionMessage(e)))
}

effect_size_reliability_calculate <- function(input) {
  tryCatch({
    result <- sample_size_effect_size_reliability(
      design = sample_size_choice(input$effect_size_reliability_design, "kappa"),
      reliability = as.numeric(input$effect_size_reliability_value),
      items = as.numeric(input$effect_size_reliability_items %||% 2),
      categories = as.numeric(input$effect_size_reliability_categories %||% 2),
      sd_difference = as.numeric(input$effect_size_reliability_sd_difference)
    )
    details <- sample_size_method_details("effect_reliability", result)
    result$formula_note <- details$formula
    result$references <- details$references
    result
  }, error = function(e) list(error = conditionMessage(e)))
}

effect_size_sem_calculate <- function(input) {
  tryCatch({
    result <- sample_size_effect_size_sem(
      design = sample_size_choice(input$effect_size_sem_design, "parameter"),
      df = as.numeric(input$effect_size_sem_df),
      null_rmsea = as.numeric(input$effect_size_sem_null_rmsea),
      alternative_rmsea = as.numeric(input$effect_size_sem_alternative_rmsea),
      parameter_type = sample_size_choice(input$effect_size_sem_parameter_type, "path"),
      parameter = as.numeric(input$effect_size_sem_parameter),
      latent_variables = as.numeric(input$effect_size_sem_latent_variables),
      measured_variables = as.numeric(input$effect_size_sem_measured_variables),
      structural_paths = as.numeric(input$effect_size_sem_structural_paths),
      free_parameters = as.numeric(input$effect_size_sem_free_parameters),
      expected_loading = as.numeric(input$effect_size_sem_expected_loading),
      expected_path = as.numeric(input$effect_size_sem_expected_path),
      complexity = sample_size_choice(input$effect_size_sem_complexity, "moderate")
    )
    details <- sample_size_method_details("effect_sem", result)
    result$formula_note <- details$formula
    result$references <- details$references
    result
  }, error = function(e) list(error = conditionMessage(e)))
}

sample_size_input_snapshot <- function(method, input) {
  target_name <- paste0("sample_size_", method, "_target")
  target <- input[[target_name]] %||% "sample_size"
  switch(
    method,
    effectsize = list(
      sample_size_effectsize_design = input$sample_size_effectsize_design %||% "independent_means"
    ),
    ttest = list(
      sample_size_ttest_target = target,
      sample_size_ttest_design = input$sample_size_ttest_design %||% "two_sample"
    ),
    nonparametric = list(
      sample_size_nonparametric_target = target,
      sample_size_nonparametric_design = input$sample_size_nonparametric_design %||% "two_independent"
    ),
    proportion = list(
      sample_size_proportion_target = target,
      sample_size_proportion_design = input$sample_size_proportion_design %||% "two_proportion"
    ),
    anova = list(
      sample_size_anova_target = target,
      sample_size_anova_design = input$sample_size_anova_design %||% "one_way",
      sample_size_anova_effect_test = isolate(input$sample_size_anova_effect_test %||% "interaction")
    ),
    ancova = list(
      sample_size_ancova_target = target,
      sample_size_ancova_design = input$sample_size_ancova_design %||% "ancova"
    ),
    regression = list(
      sample_size_regression_target = target,
      sample_size_regression_design = input$sample_size_regression_design %||% "multiple",
      sample_size_regression_mediation_method = input$sample_size_regression_mediation_method %||% "monte_carlo"
    ),
    gee = list(
      sample_size_gee_target = target,
      sample_size_gee_outcome = input$sample_size_gee_outcome %||% "continuous",
      sample_size_gee_correlation_structure = isolate(input$sample_size_gee_correlation_structure %||% "exchangeable")
    ),
    lmm = list(
      sample_size_lmm_target = target,
      sample_size_lmm_mode = input$sample_size_lmm_mode %||% "simple",
      sample_size_lmm_design = input$sample_size_lmm_design %||% "two_group_repeated",
      sample_size_lmm_correlation_structure = isolate(input$sample_size_lmm_correlation_structure %||% "exchangeable")
    ),
    survival = list(
      sample_size_survival_target = target
    ),
    equivalence = list(
      sample_size_equivalence_target = target,
      sample_size_equivalence_outcome = input$sample_size_equivalence_outcome %||% "mean",
      sample_size_equivalence_objective = input$sample_size_equivalence_objective %||% "noninferiority"
    ),
    diagnostic = list(
      sample_size_diagnostic_target = target,
      sample_size_diagnostic_design = input$sample_size_diagnostic_design %||% "sensitivity"
    ),
    precision = list(
      sample_size_precision_target = target,
      sample_size_precision_parameter = input$sample_size_precision_parameter %||% "mean"
    ),
    mcnemar = list(
      sample_size_mcnemar_target = target
    ),
    rates = list(
      sample_size_rates_target = target,
      sample_size_rates_design = input$sample_size_rates_design %||% "two_rate_ratio"
    ),
    cluster = list(
      sample_size_cluster_target = target,
      sample_size_cluster_design = input$sample_size_cluster_design %||% "parallel",
      sample_size_cluster_outcome = input$sample_size_cluster_outcome %||% "continuous"
    ),
    reliability = list(
      sample_size_reliability_target = target,
      sample_size_reliability_design = input$sample_size_reliability_design %||% "alpha"
    ),
    sem = list(
      sample_size_sem_target = target,
      sample_size_sem_test = input$sample_size_sem_test %||% "close_fit",
      sample_size_sem_df_source = input$sample_size_sem_df_source %||% "structure",
      sample_size_sem_parameter_type = isolate(input$sample_size_sem_parameter_type %||% "path"),
      sample_size_sem_complexity = isolate(input$sample_size_sem_complexity %||% "moderate")
    ),
    stats::setNames(list(target), target_name)
  )
}

sample_size_progress_message <- function(path) {
  if (!file.exists(path)) return(NULL)
  lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) character(0))
  lines <- lines[nzchar(lines)]
  if (length(lines) == 0) return(NULL)
  tryCatch(jsonlite::fromJSON(lines[[length(lines)]]), error = function(e) NULL)
}

sample_size_start_background_job <- function(method, input_snapshot, progress_path, app_dir) {
  callr::r_bg(
    func = function(method, input_snapshot, progress_path, app_dir) {
      setwd(app_dir)
      suppressPackageStartupMessages(library(shiny))
      source(file.path("R", "utils.R"))
      source(file.path("R", "sample_size.R"))
      source(file.path("R", "sample_size_ui.R"))
      progress <- function(value, text = "Calculating...") {
        message <- jsonlite::toJSON(
          list(value = as.numeric(value), text = as.character(text)),
          auto_unbox = TRUE
        )
        cat(message, "\n", file = progress_path, append = TRUE)
      }
      sample_size_calculate(method, input_snapshot, progress = progress)
    },
    args = list(
      method = method,
      input_snapshot = input_snapshot,
      progress_path = progress_path,
      app_dir = app_dir
    ),
    supervise = TRUE
  )
}

register_sample_size_server <- function(input, output, session, app_language_fn = NULL) {
  sample_size_language <- reactive({
    if (is.function(app_language_fn)) {
      return(normalize_app_language(app_language_fn()))
    }
    statedu_initial_language()
  })
  methods <- names(sample_size_method_labels())
  results <- stats::setNames(lapply(methods, function(x) reactiveVal(NULL)), methods)
  sample_size_jobs <- reactiveVal(list())
  sample_size_job_timer <- reactiveTimer(500)
  effect_size_results <- list(effectsize = reactiveVal(NULL))
  effect_size_ttest_result <- reactiveVal(NULL)
  effect_size_proportion_result <- reactiveVal(NULL)
  effect_size_chisquare_result <- reactiveVal(NULL)
  effect_size_correlation_result <- reactiveVal(NULL)
  effect_size_anova_result <- reactiveVal(NULL)
  effect_size_ancova_result <- reactiveVal(NULL)
  effect_size_nonparametric_result <- reactiveVal(NULL)
  effect_size_mcnemar_result <- reactiveVal(NULL)
  effect_size_regression_result <- reactiveVal(NULL)
  effect_size_gee_result <- reactiveVal(NULL)
  effect_size_glmm_result <- reactiveVal(NULL)
  effect_size_lmm_result <- reactiveVal(NULL)
  effect_size_survival_result <- reactiveVal(NULL)
  effect_size_equivalence_result <- reactiveVal(NULL)
  effect_size_diagnostic_result <- reactiveVal(NULL)
  effect_size_rates_result <- reactiveVal(NULL)
  effect_size_cluster_result <- reactiveVal(NULL)
  effect_size_precision_result <- reactiveVal(NULL)
  effect_size_reliability_result <- reactiveVal(NULL)
  effect_size_sem_result <- reactiveVal(NULL)
  effect_size_result_handlers <- list(
    ttest = effect_size_ttest_result,
    proportion = effect_size_proportion_result,
    chisquare = effect_size_chisquare_result,
    correlation = effect_size_correlation_result,
    anova = effect_size_anova_result,
    ancova = effect_size_ancova_result,
    nonparametric = effect_size_nonparametric_result,
    mcnemar = effect_size_mcnemar_result,
    regression = effect_size_regression_result,
    gee = effect_size_gee_result,
    glmm = effect_size_glmm_result,
    lmm = effect_size_lmm_result,
    survival = effect_size_survival_result,
    equivalence = effect_size_equivalence_result,
    diagnostic = effect_size_diagnostic_result,
    rates = effect_size_rates_result,
    cluster = effect_size_cluster_result,
    precision = effect_size_precision_result,
    reliability = effect_size_reliability_result,
    sem = effect_size_sem_result
  )
  effect_size_calculators <- list(
    ttest = effect_size_ttest_calculate,
    proportion = effect_size_proportion_calculate,
    chisquare = effect_size_chisquare_calculate,
    correlation = effect_size_correlation_calculate,
    anova = effect_size_anova_calculate,
    ancova = effect_size_ancova_calculate,
    nonparametric = effect_size_nonparametric_calculate,
    mcnemar = effect_size_mcnemar_calculate,
    regression = effect_size_regression_calculate,
    gee = effect_size_gee_calculate,
    glmm = effect_size_glmm_calculate,
    lmm = effect_size_lmm_calculate,
    survival = effect_size_survival_calculate,
    equivalence = effect_size_equivalence_calculate,
    diagnostic = effect_size_diagnostic_calculate,
    rates = effect_size_rates_calculate,
    cluster = effect_size_cluster_calculate,
    precision = effect_size_precision_calculate,
    reliability = effect_size_reliability_calculate,
    sem = effect_size_sem_calculate
  )

  observeEvent(input$sample_size_sem_test, {
    if (identical(input$sample_size_sem_test, "not_close_fit")) {
      updateTextInput(session, "sample_size_sem_null_rmsea", value = "0.08")
      updateTextInput(session, "sample_size_sem_alternative_rmsea", value = "0.05")
    } else if (identical(input$sample_size_sem_test, "close_fit")) {
      updateTextInput(session, "sample_size_sem_null_rmsea", value = "0.05")
      updateTextInput(session, "sample_size_sem_alternative_rmsea", value = "0.08")
    }
  }, ignoreInit = TRUE)

  observe({
    sample_size_job_timer()
    jobs <- sample_size_jobs()
    if (length(jobs) == 0) return()
    changed <- FALSE
    for (method_name in names(jobs)) {
      job <- jobs[[method_name]]
      progress_message <- sample_size_progress_message(job$progress_path)
      if (!is.null(progress_message)) {
        session$sendCustomMessage(
          "sample-size-progress",
          list(
            id = job$progress_id,
            value = progress_message$value %||% 0,
            text = progress_message$text %||% "Calculating..."
          )
        )
      }
      if (job$process$is_alive()) next
      result <- tryCatch(
        job$process$get_result(),
        error = function(e) {
          stderr <- tryCatch(job$process$read_error(), error = function(read_error) "")
          message <- conditionMessage(e)
          if (nzchar(stderr)) message <- paste(message, stderr, sep = "\n")
          list(error = message)
        }
      )
      results[[method_name]](result)
      try(unlink(job$progress_path), silent = TRUE)
      jobs[[method_name]] <- NULL
      changed <- TRUE
    }
    if (isTRUE(changed)) sample_size_jobs(jobs)
  })

  session$onSessionEnded(function() {
    jobs <- isolate(sample_size_jobs())
    for (job in jobs) {
      if (!is.null(job$process) && job$process$is_alive()) {
        try(job$process$kill(), silent = TRUE)
      }
      try(unlink(job$progress_path), silent = TRUE)
    }
  })

  for (effect_method in names(effect_size_method_labels())) {
    local({
      effect_method_local <- effect_method
      output[[paste0("lazy_effect_size_", effect_method_local)]] <- renderUI({
        tab_panel_content(effect_size_analysis_panel(effect_method_local, sample_size_language()))
      })
    })
  }
  output$sample_size_effectsize_inputs <- renderUI(sample_size_inputs_ui("effectsize", sample_size_input_snapshot("effectsize", input), sample_size_language()))
  output$sample_size_effectsize_results <- renderUI(sample_size_results_ui(effect_size_results$effectsize(), sample_size_language()))
  observeEvent(input$sample_size_effectsize_calculate, {
    effect_size_results$effectsize(list(progress = TRUE, id = "sample_size_effectsize_progress", text = paste0(sample_size_ui_text(sample_size_language(), "calculating"), " 0%")))
    result <- sample_size_calculate("effectsize", input)
    effect_size_results$effectsize(result)
  })
  output$effect_size_ttest_inputs <- renderUI(effect_size_ttest_inputs_ui(input, sample_size_language()))
  output$effect_size_ttest_results <- renderUI(sample_size_results_ui(effect_size_ttest_result(), sample_size_language()))
  observeEvent(input$effect_size_ttest_calculate, {
    effect_size_ttest_result(effect_size_ttest_calculate(input))
  })
  output$effect_size_proportion_inputs <- renderUI(effect_size_proportion_inputs_ui(input, sample_size_language()))
  output$effect_size_proportion_results <- renderUI(sample_size_results_ui(effect_size_proportion_result(), sample_size_language()))
  observeEvent(input$effect_size_proportion_calculate, {
    effect_size_proportion_result(effect_size_proportion_calculate(input))
  })
  output$effect_size_chisquare_inputs <- renderUI(effect_size_chisquare_inputs_ui(input, sample_size_language()))
  output$effect_size_chisquare_results <- renderUI(sample_size_results_ui(effect_size_chisquare_result(), sample_size_language()))
  observeEvent(input$effect_size_chisquare_calculate, {
    effect_size_chisquare_result(effect_size_chisquare_calculate(input))
  })
  output$effect_size_correlation_inputs <- renderUI(effect_size_correlation_inputs_ui(input, sample_size_language()))
  output$effect_size_correlation_results <- renderUI(sample_size_results_ui(effect_size_correlation_result(), sample_size_language()))
  observeEvent(input$effect_size_correlation_calculate, {
    effect_size_correlation_result(effect_size_correlation_calculate(input))
  })
  output$effect_size_anova_inputs <- renderUI(effect_size_anova_inputs_ui(input, sample_size_language()))
  output$effect_size_anova_results <- renderUI(sample_size_results_ui(effect_size_anova_result(), sample_size_language()))
  observeEvent(input$effect_size_anova_calculate, {
    effect_size_anova_result(effect_size_anova_calculate(input))
  })
  output$effect_size_ancova_inputs <- renderUI(effect_size_ancova_inputs_ui(input, sample_size_language()))
  output$effect_size_ancova_results <- renderUI(sample_size_results_ui(effect_size_ancova_result(), sample_size_language()))
  observeEvent(input$effect_size_ancova_calculate, {
    effect_size_ancova_result(effect_size_ancova_calculate(input))
  })
  output$effect_size_nonparametric_inputs <- renderUI(effect_size_nonparametric_inputs_ui(input, sample_size_language()))
  output$effect_size_nonparametric_results <- renderUI(sample_size_results_ui(effect_size_nonparametric_result(), sample_size_language()))
  observeEvent(input$effect_size_nonparametric_calculate, {
    effect_size_nonparametric_result(effect_size_nonparametric_calculate(input))
  })
  output$effect_size_mcnemar_inputs <- renderUI(effect_size_mcnemar_inputs_ui(input, sample_size_language()))
  output$effect_size_mcnemar_results <- renderUI(sample_size_results_ui(effect_size_mcnemar_result(), sample_size_language()))
  observeEvent(input$effect_size_mcnemar_calculate, {
    effect_size_mcnemar_result(effect_size_mcnemar_calculate(input))
  })
  output$effect_size_regression_inputs <- renderUI(effect_size_regression_inputs_ui(input, sample_size_language()))
  output$effect_size_regression_results <- renderUI(sample_size_results_ui(effect_size_regression_result(), sample_size_language()))
  observeEvent(input$effect_size_regression_calculate, {
    effect_size_regression_result(effect_size_regression_calculate(input))
  })
  output$effect_size_gee_inputs <- renderUI(effect_size_gee_inputs_ui(input, sample_size_language()))
  output$effect_size_gee_results <- renderUI(sample_size_results_ui(effect_size_gee_result(), sample_size_language()))
  observeEvent(input$effect_size_gee_calculate, {
    effect_size_gee_result(effect_size_gee_calculate(input))
  })
  output$effect_size_glmm_inputs <- renderUI(effect_size_glmm_inputs_ui(input, sample_size_language()))
  output$effect_size_glmm_results <- renderUI(sample_size_results_ui(effect_size_glmm_result(), sample_size_language()))
  observeEvent(input$effect_size_glmm_calculate, {
    effect_size_glmm_result(effect_size_glmm_calculate(input))
  })
  output$effect_size_lmm_inputs <- renderUI(effect_size_lmm_inputs_ui(input, sample_size_language()))
  output$effect_size_lmm_results <- renderUI(sample_size_results_ui(effect_size_lmm_result(), sample_size_language()))
  observeEvent(input$effect_size_lmm_calculate, {
    effect_size_lmm_result(effect_size_lmm_calculate(input))
  })
  output$effect_size_survival_inputs <- renderUI(effect_size_survival_inputs_ui(input, sample_size_language()))
  output$effect_size_survival_results <- renderUI(sample_size_results_ui(effect_size_survival_result(), sample_size_language()))
  observeEvent(input$effect_size_survival_calculate, {
    effect_size_survival_result(effect_size_survival_calculate(input))
  })
  output$effect_size_equivalence_inputs <- renderUI(effect_size_equivalence_inputs_ui(input, sample_size_language()))
  output$effect_size_equivalence_results <- renderUI(sample_size_results_ui(effect_size_equivalence_result(), sample_size_language()))
  observeEvent(input$effect_size_equivalence_calculate, {
    effect_size_equivalence_result(effect_size_equivalence_calculate(input))
  })
  output$effect_size_diagnostic_inputs <- renderUI(effect_size_diagnostic_inputs_ui(input, sample_size_language()))
  output$effect_size_diagnostic_results <- renderUI(sample_size_results_ui(effect_size_diagnostic_result(), sample_size_language()))
  observeEvent(input$effect_size_diagnostic_calculate, {
    effect_size_diagnostic_result(effect_size_diagnostic_calculate(input))
  })
  output$effect_size_rates_inputs <- renderUI(effect_size_rates_inputs_ui(input, sample_size_language()))
  output$effect_size_rates_results <- renderUI(sample_size_results_ui(effect_size_rates_result(), sample_size_language()))
  observeEvent(input$effect_size_rates_calculate, {
    effect_size_rates_result(effect_size_rates_calculate(input))
  })
  output$effect_size_cluster_inputs <- renderUI(effect_size_cluster_inputs_ui(input, sample_size_language()))
  output$effect_size_cluster_results <- renderUI(sample_size_results_ui(effect_size_cluster_result(), sample_size_language()))
  observeEvent(input$effect_size_cluster_calculate, {
    effect_size_cluster_result(effect_size_cluster_calculate(input))
  })
  output$effect_size_precision_inputs <- renderUI(effect_size_precision_inputs_ui(input, sample_size_language()))
  output$effect_size_precision_results <- renderUI(sample_size_results_ui(effect_size_precision_result(), sample_size_language()))
  observeEvent(input$effect_size_precision_calculate, {
    effect_size_precision_result(effect_size_precision_calculate(input))
  })
  output$effect_size_reliability_inputs <- renderUI(effect_size_reliability_inputs_ui(input, sample_size_language()))
  output$effect_size_reliability_results <- renderUI(sample_size_results_ui(effect_size_reliability_result(), sample_size_language()))
  observeEvent(input$effect_size_reliability_calculate, {
    effect_size_reliability_result(effect_size_reliability_calculate(input))
  })
  output$effect_size_sem_inputs <- renderUI(effect_size_sem_inputs_ui(input, sample_size_language()))
  output$effect_size_sem_results <- renderUI(sample_size_results_ui(effect_size_sem_result(), sample_size_language()))
  observeEvent(input$effect_size_sem_calculate, {
    effect_size_sem_result(effect_size_sem_calculate(input))
  })
  for (effect_method in intersect(names(effect_size_result_handlers), names(effect_size_calculators))) {
    local({
      effect_method_local <- effect_method
      observeEvent(input[[paste0("effect_size_", effect_method_local, "_calculate")]], {
        effect_size_result_handlers[[effect_method_local]](effect_size_calculators[[effect_method_local]](input))
      }, ignoreInit = TRUE)
    })
  }
  for (effect_method in names(effect_size_calculators)) {
    local({
      effect_method_local <- effect_method
      output[[paste0("effect_size_", effect_method_local, "_results")]] <- renderUI({
        sample_size_results_ui(effect_size_calculators[[effect_method_local]](input), sample_size_language())
      })
    })
  }

  for (method in methods) {
    local({
      method_local <- method
      output[[paste0("lazy_sample_size_", method_local)]] <- renderUI({
        tab_panel_content(sample_size_analysis_panel(method_local, sample_size_language()))
      })
      output[[paste0("sample_size_", method_local, "_inputs")]] <- renderUI(sample_size_inputs_ui(method_local, sample_size_input_snapshot(method_local, input), sample_size_language()))
      output[[paste0("sample_size_", method_local, "_results")]] <- renderUI(sample_size_results_ui(results[[method_local]](), sample_size_language()))
      observeEvent(input[[paste0("sample_size_", method_local, "_stop")]], {
        jobs <- sample_size_jobs()
        job <- jobs[[method_local]]
        if (is.null(job)) return()
        if (!is.null(job$process) && job$process$is_alive()) {
          try(job$process$kill(), silent = TRUE)
        }
        try(unlink(job$progress_path), silent = TRUE)
        jobs[[method_local]] <- NULL
        sample_size_jobs(jobs)
        results[[method_local]](list(error = "Calculation stopped."))
      }, ignoreInit = TRUE)
      observeEvent(input[[paste0("sample_size_", method_local, "_calculate")]], {
        progress_id <- paste0("sample_size_", method_local, "_progress")
        stop_id <- paste0("sample_size_", method_local, "_stop")
        jobs <- sample_size_jobs()
        existing_job <- jobs[[method_local]]
        if (!is.null(existing_job)) {
          if (!is.null(existing_job$process) && existing_job$process$is_alive()) {
            try(existing_job$process$kill(), silent = TRUE)
          }
          try(unlink(existing_job$progress_path), silent = TRUE)
          jobs[[method_local]] <- NULL
          sample_size_jobs(jobs)
        }
        results[[method_local]](list(progress = TRUE, id = progress_id, cancel_id = stop_id, text = "Starting... 1%"))
        progress_path <- tempfile(sprintf("easyflow_%s_progress_", method_local), fileext = ".jsonl")
        input_snapshot <- isolate(reactiveValuesToList(input))
        job_process <- sample_size_start_background_job(
          method = method_local,
          input_snapshot = input_snapshot,
          progress_path = progress_path,
          app_dir = getwd()
        )
        jobs <- sample_size_jobs()
        jobs[[method_local]] <- list(
          process = job_process,
          progress_path = progress_path,
          progress_id = progress_id
        )
        sample_size_jobs(jobs)
        session$sendCustomMessage(
          "sample-size-progress",
          list(id = progress_id, value = 0.01, text = "Starting")
        )
      })
      if (identical(method_local, "lmm")) {
        observeEvent(input$sample_size_lmm_mode, {
          results[[method_local]](NULL)
        }, ignoreInit = TRUE)
        observeEvent(input$sample_size_lmm_design, {
          results[[method_local]](NULL)
        }, ignoreInit = TRUE)
      }
    })
  }
}
