# Shared setup UI helpers for analysis modules.

analysis_variable_items <- function(names, table = NULL, labels = character(0)) {
  variable_choice_items(names, table, labels)
}

analysis_allowed_measurements_all <- function() {
  c("binary", "category", "ordered", "continuous")
}

analysis_allowed_variables <- function(names, variable_table = NULL, allowed_measurements = character(0)) {
  names <- as.character(names %||% character(0))
  allowed_measurements <- tolower(as.character(allowed_measurements %||% character(0)))
  allowed_measurements[allowed_measurements == "ordinal"] <- "ordered"
  allowed_measurements[allowed_measurements == "nominal"] <- "category"
  if (length(names) == 0 || length(allowed_measurements) == 0) {
    return(names)
  }
  if (is.null(variable_table) || !all(c("name", "measurement") %in% names(variable_table))) {
    return(names)
  }
  measurements <- stats::setNames(tolower(as.character(variable_table$measurement)), as.character(variable_table$name))
  measurements[measurements == "ordinal"] <- "ordered"
  names[names %in% names(measurements) & measurements[names] %in% allowed_measurements]
}

analysis_ui_text <- function(text, language = statedu_initial_language()) {
  h <- statedu_utf8
  key <- tolower(trimws(as.character(text %||% "")))
  key <- gsub(h("c2b1"), "+/-", key, fixed = TRUE)
  labels <- c(
    "variables" = paste0("Variables|", h("ebb380ec8898")),
    "auto coding error check" = paste0("Auto coding error check|", h("ec9e90eb8f9920ecbd94eb94a920ec98a4eba59820ed9995ec9db8")),
    "auto missing value detection" = paste0("Auto missing value detection|", h("ec9e90eb8f9920eab2b0ecb8a1eab09220ecb298eba6ac")),
    "auto reverse coding" = paste0("Auto reverse coding|", h("ec9e90eb8f9920ec97adecbd94eb94a9")),
    "auto variable calculation" = paste0("Auto variable calculation|", h("ec9e90eb8f9920ebb380ec889820eab384ec82b0")),
    "variable transformation" = paste0("Variable transformation|", h("ebb380ec889820ebb380ed9998")),
    "recode variable" = paste0("Recode variable|", h("ebb380ec889820eba6acecbd94eb94a9")),
    "variable rename" = paste0("Variable rename|", h("ebb380ec889820ec9db4eba68420ebb380eab2bd")),
    "wide to long" = paste0("Wide to Long|", h("ec9980ec9db4eb939c2deba1b120ebb380ed9998")),
    "frequencies / descriptives" = paste0("Frequencies / Descriptives|", h("ebb988eb8f84ebb684ec849d202f20eab8b0ec88a0ed86b5eab384")),
    "paired test" = paste0("Paired test|", h("eb8c80ec9d91ed919cebb3b820eab280eca095")),
    "paired test (3+)" = paste0("Paired test (3+)|", h("eb8c80ec9d91ed919cebb3b820eab280eca09528332b29")),
    "nonparametric tests" = paste0("Nonparametric Tests|", h("ebb984ebaaa8ec889820eab280eca095")),
    "nonparametric paired" = paste0("Nonparametric Paired|", h("ebb984ebaaa8ec889820eb8c80ec9d91")),
    "nonparametric paired test" = paste0("Nonparametric Paired Test|", h("ebb984ebaaa8ec889820eb8c80ec9d9120eab280eca095")),
    "correlation" = paste0("Correlation|", h("ec8381eab480ebb684ec849d")),
    "reliability" = paste0("Reliability|", h("ec8ba0eba2b0eb8f84")),
    "factor analysis" = paste0("Factor Analysis|", h("ec9a94ec9db8ebb684ec849d")),
    "principal components" = paste0("Principal Components|", h("eca3bcec84b1ebb684")),
    "principal component analysis" = paste0("Principal Component Analysis|", h("eca3bcec84b1ebb684ebb684ec849d")),
    "regression" = paste0("Regression|", h("ed9a8ceab780ebb684ec849d")),
    "generalized linear model (glm)" = paste0("Generalized Linear Model (GLM)|", h("ec9dbcebb098ed999420ec84a0ed9895ebaaa8ed989528474c4d29")),
    "longitudinal / panel models" = paste0("Longitudinal / Panel Models|", h("eca285eb8ba82fed8ca8eb849020ebaaa8ed9895")),
    "selected variables" = paste0("Selected Variables|", h("ec84a0ed839d20ebb380ec8898")),
    "dependent variables" = paste0("Dependent variables|", h("eca285ec868debb380ec8898")),
    "dependent variable" = paste0("Dependent variable|", h("eca285ec868debb380ec8898")),
    "independent variables" = paste0("Independent variables|", h("eb8f85eba6bdebb380ec8898")),
    "independent variable" = paste0("Independent variable|", h("eb8f85eba6bdebb380ec8898")),
    "covariates" = paste0("Covariates|", h("eab3b5ebb380eb9f89")),
    "covariate" = paste0("Covariate|", h("eab3b5ebb380eb9f89")),
    "grouping variables" = paste0("Grouping Variables|", h("eca791eb8ba820ebb380ec8898")),
    "options" = paste0("Options|", h("ec98b5ec8598")),
    "post-hoc" = paste0("Post-hoc|", h("ec82aced9b84ebb684ec849d")),
    "post-hoc correction" = paste0("Post-hoc correction|", h("ec82aced9b84ebb684ec849d20ebb3b4eca095")),
    "output" = paste0("Output|", h("ecb69ceba0a5")),
    "normality" = paste0("Normality|", h("eca095eab79cec84b1")),
    "table" = paste0("Table|", h("ed919c")),
    "statistics" = paste0("Statistics|", h("ed86b5eab384")),
    "plots" = paste0("Plots|", h("eab7b8eb9e98ed9484")),
    "plot" = paste0("Plot|", h("eab7b8eb9e98ed9484")),
    "design" = paste0("Design|", h("ec97b0eab5ac20ec84a4eab384")),
    "display" = paste0("Display|", h("ed919cec8b9c")),
    "column variable" = paste0("Column variable|", h("ec97b420ebb380ec8898")),
    "row variable" = paste0("Row variable|", h("ed968920ebb380ec8898")),
    "continuous method" = paste0("Continuous method|", h("ec97b0ec868ded989520ebb0a9ebb295")),
    "advanced correlations" = paste0("Advanced correlations|", h("eab3a0eab88920ec8381eab480")),
    "p-value & 95% ci" = "p-value & 95% CI|p-value & 95% CI",
    "significance levels" = paste0("significance levels|", h("ec9ca0ec9d98ec8898eca480")),
    "normality diagnostics" = paste0("normality diagnostics|", h("eca095eab79cec84b120eca784eb8ba8")),
    "assumption" = paste0("Assumption|", h("eab080eca095")),
    "assumptions" = paste0("Assumptions|", h("eab080eca09520eab280ed86a0")),
    "categorical" = paste0("Categorical|", h("ebb294eca3bced9895")),
    "binary" = paste0("Binary|", h("ec9db4ebb684ed9895")),
    "ordered" = paste0("Ordered|", h("ec889cec849ced9895")),
    "continuous" = paste0("Continuous|", h("ec97b0ec868ded9895")),
    "summary" = paste0("Summary|", h("ec9a94ec95bd")),
    "check assumptions" = paste0("Check assumptions|", h("eab080eca09520ed9995ec9db8")),
    "check residual normality" = paste0("Check residual normality|", h("ec9e94ecb0a820eca095eab79cec84b120ed9995ec9db8")),
    "automatic selection" = paste0("Automatic selection|", h("ec9e90eb8f9920ec84a0ed839d")),
    "warnings only" = paste0("Warnings only|", h("eab2bdeab3a0eba78c20ed919cec8b9c")),
    "alpha" = paste0("Alpha|", h("ec9ca0ec9d98ec8898eca480")),
    "force ranked ancova" = paste0("Force ranked ANCOVA|", h("ec889cec9c8420414e434f564120eab095eca09c")),
    "type ii ss (recommended)" = paste0("Type II SS (recommended)|", h("547970652049492053532028eab68cec9ea529")),
    "type i ss (sequential)" = paste0("Type I SS (sequential)|", h("5479706520492053532028ec889cecb0a829")),
    "bowker symmetry test" = paste0("Bowker symmetry test|", h("426f776b657220eb8c80ecb9adec84b120eab280eca095")),
    "cohen's d for paired t-test" = paste0("Cohen's d for paired t-test|", h("eb8c80ec9d9120742d74657374ec9d9820436f68656e27732064")),
    "median(q1~q3)" = paste0("Median(Q1~Q3)|", h("eca491ec9599eab0922851317e513329")),
    "repeated-measures variables" = paste0("Repeated-measures variables|", h("ebb098ebb3b5ecb8a1eca09520ebb380ec8898")),
    "repeated variable labels" = paste0("Repeated variable labels|", h("ebb098ebb3b520ebb380ec889820eb9dbcebb2a8")),
    "repeated" = paste0("Repeated|", h("ebb098ebb3b5ecb8a1eca095")),
    "diagnostics" = paste0("Diagnostics|", h("eca784eb8ba8")),
    "model" = paste0("Model|", h("ebaaa8ed9895")),
    "model variables" = paste0("Model variables|", h("ebaaa8ed989520ebb380ec8898")),
    "exposure / offset (optional)" = paste0("Exposure / offset (optional)|", h("eb85b8ecb69c2fec98a4ed9484ec858b28ec84a0ed839d29")),
    "block 1" = paste0("Block 1|", h("ebb894eba19d2031")),
    "block 2: independent variables" = paste0("Block 2: Independent variables|", h("ebb894eba19d20323a20eb8f85eba6bdebb380ec8898")),
    "block 3: independent variables" = paste0("Block 3: Independent variables|", h("ebb894eba19d20333a20eb8f85eba6bdebb380ec8898")),
    "previous block" = paste0("Previous block|", h("ec9db4eca08420ebb894eba19d")),
    "next block" = paste0("Next block|", h("eb8ba4ec9d8c20ebb894eba19d")),
    "bootstrap" = paste0("Bootstrap|", h("ebb680ed8ab8ec8aa4ed8ab8eb9ea9")),
    "number of bootstrap samples" = paste0("Number of bootstrap samples|", h("ebb680ed8ab8ec8aa4ed8ab8eb9ea920ed919cebb3b820ec8898")),
    "seed number" = paste0("Seed number|", h("ec8b9ceb939c20ebb288ed98b8")),
    "collinearity diagnostics" = paste0("Collinearity diagnostics|", h("eab3b5ec84a0ec84b120eca784eb8ba8")),
    "residual normality" = paste0("Residual normality|", h("ec9e94ecb0a820eca095eab79cec84b1")),
    "residual normality test" = paste0("Residual normality test|", h("ec9e94ecb0a820eca095eab79cec84b120eab280eca095")),
    "variance homogeneity" = paste0("Variance homogeneity|", h("ebb684ec82b020eb8f99eca788ec84b1")),
    "analysis mode" = paste0("Analysis mode|", h("ebb684ec849d20ebaaa8eb939c")),
    "sum of squares" = paste0("Sum of squares|", h("eca09ceab3b1ed95a9")),
    "p-value adjustment" = paste0("p-value adjustment|", h("702d76616c756520ebb3b4eca095")),
    "sensitivity analysis" = paste0("Sensitivity analysis|", h("ebafbceab090eb8f8420ebb684ec849d")),
    "bonferroni correction (bc)" = paste0("Bonferroni correction (BC)|", h("426f6e666572726f6e6920ebb3b4eca09528424329")),
    "holm-bonferroni method" = paste0("Holm-Bonferroni method|", h("486f6c6d2d426f6e666572726f6e6920ebb0a9ebb295")),
    "df (degree of freedom)" = paste0("DF (Degree of Freedom)|", h("ec9e90ec9ca0eb8f8428444629")),
    "m +/- se" = paste0("M ", h("c2b1"), " SE|", h("4d20c2b1205345")),
    "adjusted mean error bar plot (95% ci)" = paste0("Adjusted mean error bar plot (95% CI)|", h("eca1b0eca095ed8f89eab7a020ec98a4ecb0a8eba789eb8c8020eab7b8eb9e98ed94842839352520434929")),
    "raw data + adjusted mean overlay" = paste0("Raw data + adjusted mean overlay|", h("ec9b90ec9e90eba38c202b20eca1b0eca095ed8f89eab7a020eab2b9ecb390ebb3b4eab8b0")),
    "covariate-adjusted regression lines" = paste0("Covariate-adjusted regression lines|", h("eab3b5ebb380eb9f8920ebb3b4eca09520ed9a8ceab780ec84a0")),
    "linearity diagnostic plots" = paste0("Linearity diagnostic plots|", h("ec84a0ed9895ec84b120eca784eb8ba820eab7b8eb9e98ed9484")),
    "missing" = paste0("Missing|", h("eab2b0ecb8a1")),
    "checks" = paste0("Checks|", h("eab280ed86a0")),
    "outcome family" = paste0("Outcome family|", h("eca285ec868debb380ec889820ebb684ed8fac")),
    "link function" = paste0("Link function|", h("eba781ed81ac20ed95a8ec8898")),
    "inference" = paste0("Inference|", h("ecb694eba1a0")),
    "standard errors" = paste0("Standard errors|", h("ed919ceca480ec98a4ecb0a8")),
    "auto" = paste0("Auto|", h("ec9e90eb8f99")),
    "linear gaussian / identity" = paste0("Linear Gaussian / identity|", h("476175737369616e20ec84a0ed9895202f206964656e74697479")),
    "binary logistic / logit" = paste0("Binary logistic / logit|", h("ec9db4ebb684ed989520eba19ceca780ec8aa4ed8bb1202f206c6f676974")),
    "gamma / log" = paste0("Gamma / log|", h("eab090eba788202f206c6f67")),
    "count: poisson or negative binomial / log" = paste0("Count: Poisson or negative binomial / log|", h("eab384ec8898ed98953a20ed8facec9584ec86a120eb9890eb8a9420ec9d8cec9db4ed95ad202f206c6f67")),
    "default for family" = paste0("Default for family|", h("eab8b0ebb3b820eba781ed81ac")),
    "model-based" = paste0("Model-based|", h("ebaaa8ed989520eab8b0ebb098")),
    "robust sandwich hc0" = paste0("Robust sandwich HC0|", h("eab095eab1b420ed919ceca480ec98a4ecb0a820484330")),
    "robust sandwich hc1" = paste0("Robust sandwich HC1|", h("eab095eab1b420ed919ceca480ec98a4ecb0a820484331")),
    "robust sandwich hc2" = paste0("Robust sandwich HC2|", h("eab095eab1b420ed919ceca480ec98a4ecb0a820484332")),
    "robust sandwich hc3" = paste0("Robust sandwich HC3|", h("eab095eab1b420ed919ceca480ec98a4ecb0a820484333")),
    "missing-data strategy" = paste0("Missing-data strategy|", h("eab2b0ecb8a1ec9e90eba38c20ecb298eba6ac")),
    "complete-case: row-wise" = paste0("Complete-case: row-wise|", h("ec9984eca084ec82aceba1803a20ed968920eb8ba8ec9c84")),
    "multiple imputation (mi)" = paste0("Multiple imputation (MI)|", h("eb8ba4eca491eb8c80ecb2b4284d4929")),
    "inverse probability weighting (ipw)" = paste0("Inverse probability weighting (IPW)|", h("ec97aded9995eba5a0eab080eca4912849505729")),
    "multiple imputation settings" = paste0("Multiple imputation settings|", h("eb8ba4eca491eb8c80ecb2b420ec84a4eca095")),
    "dependent-variable handling" = paste0("Dependent-variable handling|", h("eca285ec868debb380ec889820ecb298eba6ac")),
    "use rows with observed dependent variable (recommended)" = paste0("Use rows with observed dependent variable (recommended)|", h("eca285ec868debb380ec8898eab08020eab480ecb8a1eb909c20ed968920ec82acec9aa928eab68cec9ea529")),
    "impute missing dependent variable for sensitivity analysis" = paste0("Impute missing dependent variable for sensitivity analysis|", h("ebafbceab090eb8f8420ebb684ec849dec9d8420ec9c84ed95b420eab2b0ecb8a120eca285ec868debb380ec889820eb8c80ecb2b4")),
    "mi datasets" = paste0("MI datasets|", h("4d4920eb8db0ec9db4ed84b0ec858b")),
    "mi iterations" = paste0("MI iterations|", h("4d4920ebb098ebb3b5")),
    "ipw observation model" = paste0("IPW observation model|", h("49505720eab480ecb8a120ebaaa8ed9895")),
    "auxiliary variables" = paste0("Auxiliary variables|", h("ebb3b4eca1b020ebb380ec8898")),
    "run assumption checks and recommendations" = paste0("Run assumption checks and recommendations|", h("eab080eca09520eab280ed86a0ec998020eab68ceab3a020ec8ba4ed9689")),
    "poisson / negative-binomial screening" = paste0("Poisson / negative-binomial screening|", h("ed8facec9584ec86a12fec9d8cec9db4ed95ad20ec84a0ebb384")),
    "variables to check" = paste0("Variables to check|", h("eab280ed86a0ed95a020ebb380ec8898")),
    "eq-5d variables" = paste0("EQ-5D variables|", h("45512d354420ebb380ec8898")),
    "hint8 variables" = paste0("HINT8 variables|", h("48494e543820ebb380ec8898")),
    "metabolic variables" = paste0("Metabolic variables|", h("eb8c80ec82aceca69ded9b84eab5b020ebb380ec8898")),
    "frs variables" = paste0("FRS variables|", h("46525320ebb380ec8898")),
    "ascvd10 variables" = paste0("ASCVD10 variables|", h("4153435644313020ebb380ec8898")),
    "severity score variables" = paste0("Severity score variables|", h("eca491eca69deb8f8420eca090ec889820ebb380ec8898")),
    "criteria" = paste0("Criteria|", h("eab8b0eca480")),
    "reference" = paste0("Reference|", h("ecb0b8eca1b0")),
    "coding" = paste0("Coding|", h("ecbd94eb94a9")),
    "formula" = paste0("Formula|", h("eab3b5ec8b9d")),
    "units" = paste0("Units|", h("eb8ba8ec9c84")),
    "output variables" = paste0("Output variables|", h("ecb69ceba0a520ebb380ec8898")),
    "available functions" = paste0("Available functions|", h("ec82acec9aa920eab080eb8aa5ed959c20ed95a8ec8898")),
    "function type" = paste0("Function type|", h("ed95a8ec889820ec9ca0ed9895")),
    "infer automatically" = paste0("Infer automatically|", h("ec9e90eb8f9920ecb694eba1a0")),
    "variable name" = paste0("Variable name|", h("ebb380ec8898ebaa85")),
    "old variable" = paste0("Old variable|", h("ec9db4eca08420ebb380ec8898")),
    "new name" = paste0("New name|", h("ec838820ec9db4eba684")),
    "label" = paste0("Label|", h("eb9dbcebb2a8")),
    "run" = paste0("Run|", h("ec8ba4ed9689")),
    "remove" = paste0("Remove|", h("eca09ceab1b0")),
    "apply" = paste0("Apply|", h("eca081ec9aa9")),
    "method" = paste0("Method|", h("ebb0a9ebb295")),
    "matrix" = paste0("Matrix|", h("ed9689eba0ac")),
    "rotation" = paste0("Rotation|", h("ed9a8ceca084")),
    "factor selection" = paste0("Factor selection|", h("ec9a94ec9db820ec84a0ed839d")),
    "component selection" = paste0("Component selection|", h("ec84b1ebb68420ec84a0ed839d")),
    "save scores" = paste0("Save scores|", h("eca090ec889820eca080ec9ea5")),
    "none" = paste0("None|", h("ec9786ec9d8c")),
    "pearson correlation" = paste0("Pearson correlation|", h("50656172736f6e20ec8381eab480")),
    "polychoric correlation" = paste0("Polychoric correlation|", h("506f6c7963686f72696320ec8381eab480")),
    "correlation matrix" = paste0("Correlation matrix|", h("ec8381eab480ed9689eba0ac")),
    "covariance matrix" = paste0("Covariance matrix|", h("eab3b5ebb684ec82b0ed9689eba0ac")),
    "principal axis factoring" = paste0("Principal axis factoring|", h("eca3bcecb695ec9a94ec9db8ebb295")),
    "maximum likelihood" = paste0("Maximum likelihood|", h("ecb59ceb8c80ec9ab0eb8f84ebb295")),
    "varimax" = paste0("Varimax|", h("ebca0ceb9faeb9e99ec8aa4")),
    "oblimin" = paste0("Oblimin|", h("ec98a4ebb894eba6acebafbc")),
    "factor item means" = paste0("Factor item means|", h("ec9a94ec9db820ebacb8ed95ad20ed8f89eab7a0")),
    "factor item sums" = paste0("Factor item sums|", h("ec9a94ec9db820ebacb8ed95ad20ed95a9eab384")),
    "factor scores" = paste0("Factor scores|", h("ec9a94ec9db820eca090ec8898")),
    "component scores" = paste0("Component scores|", h("ec84b1ebb68420eca090ec8898")),
    "fixed number of factors" = paste0("Fixed number of factors|", h("ec9a94ec9db820ec889820eab3a0eca095")),
    "fixed number of components" = paste0("Fixed number of components|", h("eab3a0eca09520ec84b1ebb68420ec8898")),
    "fixed number" = paste0("Fixed number|", h("ec889820eab3a0eca095")),
    "eigenvalue >= 1.0" = paste0("Eigenvalue >= 1.0|", h("eab3a0ec9ca0eab092203e3d20312e30")),
    "cumulative variance >=" = paste0("Cumulative variance >=|", h("eb8884eca08120ec84a4ebaa85ebb684ec82b0203e3d")),
    "cumulative variance" = paste0("Cumulative variance|", h("eb8884eca08120ec84a4ebaa85ebb684ec82b0")),
    "items" = paste0("Items|", h("ebacb8ed95ad")),
    "subfactor" = paste0("Subfactor|", h("ed9598ec9c84ec9a94ec9db8")),
    "previous subfactor" = paste0("Previous subfactor|", h("ec9db4eca08420ed9598ec9c84ec9a94ec9db8")),
    "next subfactor" = paste0("Next subfactor|", h("eb8ba4ec9d8c20ed9598ec9c84ec9a94ec9db8")),
    "item diagnostics" = paste0("Item diagnostics|", h("ebacb8ed95ad20eca784eb8ba8")),
    "reliability if item deleted" = paste0("Reliability if item deleted|", h("ebacb8ed95ad20eca09ceab1b020ec8b9c20ec8ba0eba2b0eb8f84")),
    "item-total correlation" = paste0("Item-total correlation|", h("ebacb8ed95ad2decb49deca09020ec8381eab480")),
    "ordinal alpha / ordinal omega" = paste0("Ordinal alpha / Ordinal omega|", h("ec889cec849ced989520616c706861202f20ec889cec849ced9895206f6d656761")),
    "sort loadings by size" = paste0("Sort loadings by size|", h("eca081ec9eaceb9f8920ed81aceab8b0ec889c20eca095eba0ac")),
    "show loadings >= .30 only" = paste0("Show loadings >= .30 only|", h("eca081ec9eaceb9f89203e3d202e3330eba78c20ed919cec8b9c")),
    "highlight problem values" = paste0("Highlight problem values|", h("ebacb8eca09c20eab09220eab095eca1b0")),
    "subfactor reliability" = paste0("Subfactor reliability|", h("ed9598ec9c84ec9a94ec9db820ec8ba0eba2b0eb8f84")),
    "scree plot" = paste0("Scree plot|", h("ec8aa4ed81aceba6ac20eab7b8eba6bc")),
    "biplot" = paste0("Biplot|", h("ebb094ec9db4ed948ceba1af")),
    "test" = paste0("test|", h("eab280ec82acec9aa9")),
    "recommended" = paste0("recommended|", h("eab68cec9ea5")),
    "variables to convert" = paste0("Variables to convert|", h("ebb380ed9998ed95a020ebb380ec8898")),
    "detect candidates" = paste0("Detect candidates|", h("ed9b84ebb3b420eab090eca780")),
    "numeric codes" = paste0("Numeric codes|", h("ec88abec9e9020ecbd94eb939c")),
    "text / blank codes" = paste0("Text / blank codes|", h("ebacb8ec9e902febb98820eab09220ecbd94eb939c")),
    "use codes from" = paste0("Use codes from|", h("ecbd94eb939c20ec82acec9aa920ec9c84ecb998")),
    "selected detected rows" = paste0("Selected detected rows|", h("ec84a0ed839ded959c20eab090eca78020ed9689")),
    "manual codes below" = paste0("Manual codes below|", h("ec9584eb9e9820ec8898eb8f9920ecbd94eb939c")),
    "manual missing codes" = paste0("Manual missing codes|", h("ec8898eb8f9920eab2b0ecb8a120ecbd94eb939c")),
    "mark as user missing" = paste0("Mark as user missing|", h("ec82acec9aa9ec9e9020eab2b0ecb8a1ec9cbceba19c20ed919cec8b9c")),
    "convert to na" = paste0("Convert to NA|", h("4e41eba19c20ebb380ed9998")),
    "insert variable(s)" = paste0("Insert variable(s)|", h("ebb380ec889820ec82bdec9e85")),
    "new variable name" = paste0("New variable name|", h("ec838820ebb380ec8898ebaa85")),
    "variable type" = paste0("Variable type|", h("ebb380ec889820ec9ca0ed9895")),
    "choose a template..." = paste0("Choose a template...|", h("ed859ced948ceba6bf20ec84a0ed839d2e2e2e")),
    "copy variable" = paste0("Copy variable|", h("ebb380ec889820ebb3b5ec82ac")),
    "mean of selected variables" = paste0("Mean of selected variables|", h("ec84a0ed839d20ebb380ec889820ed8f89eab7a0")),
    "sum of selected variables" = paste0("Sum of selected variables|", h("ec84a0ed839d20ebb380ec889820ed95a9eab384")),
    "z-score" = paste0("Z-score|", h("5a20eca090ec8898")),
    "natural log" = paste0("Natural log|", h("ec9e90ec97b0eba19ceab7b8")),
    "square" = paste0("Square|", h("eca09ceab3b1")),
    "reverse 1-5 scale" = paste0("Reverse 1-5 scale|", h("312d3520ecb299eb8f8420ec97adecbd94eb94a9")),
    "high/low by mean" = paste0("High/low by mean|", h("ed8f89eab7a020eab8b0eca48020eb8692ec9d8c2feb82aeec9d8c")),
    "preview" = paste0("Preview|", h("ebafb8eba6acebb3b4eab8b0")),
    "create variable" = paste0("Create variable|", h("ebb380ec889820ec839dec84b1")),
    "minimum" = paste0("Minimum|", h("ecb59cec868ceab092")),
    "maximum" = paste0("Maximum|", h("ecb59ceb8c80eab092")),
    "observed range" = paste0("Observed range|", h("eab480ecb8a120ebb294ec9c84")),
    "no numeric values" = paste0("No numeric values|", h("ec88abec9e9020eab09220ec9786ec9d8c")),
    "variables to reverse-code" = paste0("Variables to reverse-code|", h("ec97adecbd94eb94a9ed95a020ebb380ec8898")),
    "save result to" = paste0("Save result to|", h("eab2b0eab3bc20eca080ec9ea520ec9c84ecb998")),
    "new variables" = paste0("New variables|", h("ec838820ebb380ec8898")),
    "same variables" = paste0("Same variables|", h("eab099ec9d8020ebb380ec8898")),
    "variables to calculate" = paste0("Variables to calculate|", h("eab384ec82b0ed95a020ebb380ec8898")),
    "variable calculation" = paste0("Variable calculation|", h("ebb380ec889820eab384ec82b0")),
    "mean" = paste0("Mean|", h("ed8f89eab7a0")),
    "sum" = paste0("Sum|", h("ed95a9eab384")),
    "standard deviation" = paste0("Standard deviation|", h("ed919ceca480ed8eb8ecb0a8")),
    "variance" = paste0("Variance|", h("ebb684ec82b0")),
    "variable target" = paste0("Variable target|", h("ebb380ec889820eb8c80ec8381")),
    "same name" = paste0("Same name|", h("eab099ec9d8020ec9db4eba684")),
    "different name" = paste0("Different name|", h("eb8ba4eba5b820ec9db4eba684")),
    "applied variables" = paste0("Applied variables|", h("eca081ec9aa9eb909c20ebb380ec8898")),
    "recoding rules" = paste0("Recoding rules|", h("ec9eacecbd94eb94a920eab79cecb999")),
    "single value recode" = paste0("Single value recode|", h("eb8ba8ec9dbc20eab09220ec9eacecbd94eb94a9")),
    "categorize values" = paste0("Categorize values|", h("eab09220ebb294eca3bced9994")),
    "from" = paste0("From|", h("ec8b9cec9e91")),
    "op" = paste0("Op|", h("ec97b0ec82b0ec9e90")),
    "value" = paste0("Value|", h("eab092")),
    "to" = paste0("To|", h("eb819d")),
    "new" = paste0("New|", h("ec838820eab092")),
    "min" = paste0("Min|", h("ecb59cec868c")),
    "max" = paste0("Max|", h("ecb59ceb8c80")),
    "keep unmatched values" = paste0("Keep unmatched values|", h("ec9dbcecb998ed9598eca78020ec958aeb8a9420eab09220ec9ca0eca780")),
    "measurement after recoding" = paste0("Measurement after recoding|", h("ec9eacecbd94eb94a920ed9b8420ebb380ec889820ec9ca0ed9895")),
    "keep current type" = paste0("Keep current type|", h("ed9884ec9eac20ec9ca0ed989520ec9ca0eca780")),
    "add" = paste0("Add|", h("ecb694eab080")),
    "use latent-variable correlations" = paste0("Use latent-variable correlations|", h("ec9ea0ec9eacebb380ec889820ec8381eab48020ec82acec9aa9")),
    "scatter plot matrix" = paste0("scatter plot matrix|", h("ec82b0eca090eb8f8420ed9689eba0ac")),
    "correlation matrix heatmap" = paste0("correlation matrix heatmap|", h("ec8381eab480ed9689eba0ac20686561746d6170")),
    "min, max" = paste0("Min, Max|", h("ecb59cec868c2c20ecb59ceb8c80")),
    "skewness, kurtosis" = paste0("Skewness, Kurtosis|", h("ec999ceb8f842c20ecb2a8eb8f84")),
    "skewness / kurtosis" = paste0("Skewness / kurtosis|", h("ec999ceb8f84202f20ecb2a8eb8f84")),
    "shapiro-wilk" = "Shapiro-Wilk|Shapiro-Wilk",
    "kolmogorov-smirnov" = "Kolmogorov-Smirnov|Kolmogorov-Smirnov",
    "mardia test" = paste0("Mardia test|", h("4d617264696120eab280eca095")),
    "2/5 : conservative" = paste0("2/5 : conservative|", h("322f35203a20ebb3b4ec8898eca081")),
    "2/7 : standard" = paste0("2/7 : Standard|", h("322f37203a20ed919ceca480")),
    "3/7 : lenient" = paste0("3/7 : lenient|", h("332f37203a20eab480eb8c80")),
    "median, iqr(q1~q3)" = paste0("Median, IQR(Q1~Q3)|", h("eca491ec9599eab0922c204951522851317e513329")),
    "pie chart" = paste0("Pie chart|", h("ec9b90eab7b8eb9e98ed9484")),
    "bar chart" = paste0("Bar chart|", h("eba789eb8c80eab7b8eb9e98ed9484")),
    "histogram" = paste0("Histogram|", h("ed9e88ec8aa4ed86a0eab7b8eb9ea8")),
    "box plot" = paste0("Box plot|", h("ebb095ec8aa4eab7b8eb9e98ed9484")),
    "violin plot" = paste0("Violin plot|", h("ebb094ec9db4ec98aceba6b0eab7b8eb9e98ed9484")),
    "trend analysis" = paste0("Trend analysis|", h("ecb694ec84b8ebb684ec849d")),
    "effect size" = paste0("Effect size|", h("ed9aa8eab3bced81aceab8b0")),
    "statistic" = paste0("Statistic|", h("ed86b5eab384eb9f89")),
    "degrees of freedom" = paste0("Degrees of freedom|", h("ec9e90ec9ca0eb8f84")),
    "ordered significance notation" = paste0("Ordered significance notation|", h("ed8f89eab7a0ec889c20ec9ca0ec9d98ec84b120ed919ceab8b0")),
    "survey study" = paste0("Survey study|", h("ec84a4ebacb820ec97b0eab5ac")),
    "experimental study" = paste0("Experimental study|", h("ec8ba4ed979820ec97b0eab5ac")),
    "skewness/kurtosis cutoff" = paste0("Skewness/kurtosis cutoff|", h("ec999ceb8f842fecb2a8eb8f8420eab8b0eca480")),
    "up" = paste0("Up|", h("ec9c84eba19c")),
    "down" = paste0("Down|", h("ec9584eb9e98eba19c")),
    "nonparametric" = paste0("Nonparametric|", h("ebb984ebaaa8ec8898")),
    "reset setting" = paste0("Reset setting|", h("ec84a4eca09520ecb488eab8b0ed9994")),
    "data viewer" = paste0("Data Viewer|", h("eb8db0ec9db4ed84b020ebb3b4eab8b0")),
    "message" = paste0("Message|", h("eba994ec8b9ceca780")),
    "back to analysis" = paste0("Back to analysis|", h("ebb684ec849dec9cbceba19c20eb8f8cec9584eab080eab8b0")),
    "value / label" = paste0("Value / Label|", h("eab092202f20eb9dbcebb2a8")),
    "read-only worksheet preview of the current data." = paste0("Read-only worksheet preview of the current data.|", h("ed9884ec9eac20eb8db0ec9db4ed84b0ec9d9820ec9dbdeab8b020eca084ec9aa920ebafb8eba6acebb3b4eab8b0ec9e85eb8b88eb8ba42e")),
    "no analysis variables are selected." = paste0("No analysis variables are selected.|", h("ec84a0ed839deb909c20ebb684ec849d20ebb380ec8898eab08020ec9786ec8ab5eb8b88eb8ba42e")),
    "no data is loaded." = paste0("No data is loaded.|", h("eb8db0ec9db4ed84b0eab08020eba19ceb939ceb9098eca78020ec958aec9598ec8ab5eb8b88eb8ba42e")),
    "complete step 2 in the data tab before setting up ancova." = paste0("Complete Step 2 in the Data tab before setting up ANCOVA.|", h("414e434f564120ec84a4eca09520eca084ec979020eb8db0ec9db4ed84b020ed83adec9d9820537465702032eba5bc20ec9984eba38ced9598ec84b8ec9a942e")),
    "complete step 2 in the data tab before setting up correlation analysis." = paste0("Complete Step 2 in the Data tab before setting up correlation analysis.|", h("ec8381eab480ebb684ec849d20ec84a4eca09520eca084ec979020eb8db0ec9db4ed84b020ed83adec9d9820537465702032eba5bc20ec9984eba38ced9598ec84b8ec9a942e")),
    "complete step 2 in the data tab before setting up factor analysis." = paste0("Complete Step 2 in the Data tab before setting up factor analysis.|", h("ec9a94ec9db8ebb684ec849d20ec84a4eca09520eca084ec979020eb8db0ec9db4ed84b020ed83adec9d9820537465702032eba5bc20ec9984eba38ced9598ec84b8ec9a942e")),
    "complete step 2 in the data tab before setting up logistic regression." = paste0("Complete Step 2 in the Data tab before setting up logistic regression.|", h("eba19ceca780ec8aa4ed8bb120ed9a8ceab780ebb684ec849d20ec84a4eca09520eca084ec979020eb8db0ec9db4ed84b020ed83adec9d9820537465702032eba5bc20ec9984eba38ced9598ec84b8ec9a942e")),
    "complete step 2 in the data tab before setting up glm." = paste0("Complete Step 2 in the Data tab before setting up GLM.|", h("474c4d20ec84a4eca09520eca084ec979020eb8db0ec9db4ed84b020ed83adec9d9820537465702032eba5bc20ec9984eba38ced9598ec84b8ec9a942e")),
    "complete step 2 in the data tab before setting up nonparametric paired tests." = paste0("Complete Step 2 in the Data tab before setting up nonparametric paired tests.|", h("ebb984ebaaa8ec889820eb8c80ec9d9120eab280eca09520ec84a4eca09520eca084ec979020eb8db0ec9db4ed84b020ed83adec9d9820537465702032eba5bc20ec9984eba38ced9598ec84b8ec9a942e")),
    "complete step 2 in the data tab before setting up nonparametric tests." = paste0("Complete Step 2 in the Data tab before setting up nonparametric tests.|", h("ebb984ebaaa8ec889820eab280eca09520ec84a4eca09520eca084ec979020eb8db0ec9db4ed84b020ed83adec9d9820537465702032eba5bc20ec9984eba38ced9598ec84b8ec9a942e")),
    "complete step 2 in the data tab before setting up longitudinal / panel models." = paste0("Complete Step 2 in the Data tab before setting up longitudinal / panel models.|", h("eca285eb8ba82fed8ca8eb849020ebaaa8ed989520ec84a4eca09520eca084ec979020eb8db0ec9db4ed84b020ed83adec9d9820537465702032eba5bc20ec9984eba38ced9598ec84b8ec9a942e")),
    "complete step 2 in the data tab before setting up principal component analysis." = paste0("Complete Step 2 in the Data tab before setting up principal component analysis.|", h("eca3bcec84b1ebb684ebb684ec849d20ec84a4eca09520eca084ec979020eb8db0ec9db4ed84b020ed83adec9d9820537465702032eba5bc20ec9984eba38ced9598ec84b8ec9a942e")),
    "complete step 2 in the data tab before setting up repeated-measures tests." = paste0("Complete Step 2 in the Data tab before setting up repeated-measures tests.|", h("ebb098ebb3b5ecb8a1eca09520eab280eca09520ec84a4eca09520eca084ec979020eb8db0ec9db4ed84b020ed83adec9d9820537465702032eba5bc20ec9984eba38ced9598ec84b8ec9a942e")),
    "complete step 2 in the data tab before setting up reliability analysis." = paste0("Complete Step 2 in the Data tab before setting up reliability analysis.|", h("ec8ba0eba2b0eb8f8420ebb684ec849d20ec84a4eca09520eca084ec979020eb8db0ec9db4ed84b020ed83adec9d9820537465702032eba5bc20ec9984eba38ced9598ec84b8ec9a942e")),
    "complete step 2 in the data tab before setting up paired tests." = paste0("Complete Step 2 in the Data tab before setting up paired tests.|", h("eb8c80ec9d9120eab280eca09520ec84a4eca09520eca084ec979020eb8db0ec9db4ed84b020ed83adec9d9820537465702032eba5bc20ec9984eba38ced9598ec84b8ec9a942e")),
    "complete step 2 in the data tab before setting up regression." = paste0("Complete Step 2 in the Data tab before setting up regression.|", h("ed9a8ceab780ebb684ec849d20ec84a4eca09520eca084ec979020eb8db0ec9db4ed84b020ed83adec9d9820537465702032eba5bc20ec9984eba38ced9598ec84b8ec9a942e")),
    "complete step 2 in the data tab before setting up t-test / anova." = paste0("Complete Step 2 in the Data tab before setting up t-test / ANOVA.|", h("742d74657374202f20414e4f564120ec84a4eca09520eca084ec979020eb8db0ec9db4ed84b020ed83adec9d9820537465702032eba5bc20ec9984eba38ced9598ec84b8ec9a942e")),
    "reconnect the data file in the data tab before setting up correlation analysis." = paste0("Reconnect the data file in the Data tab before setting up correlation analysis.|", h("ec8381eab480ebb684ec849d20ec84a4eca09520eca084ec979020eb8db0ec9db4ed84b020ed83adec9790ec849c20eb8db0ec9db4ed84b020ed8c8cec9dbcec9d8420eb8ba4ec8b9c20ec97b0eab2b0ed9598ec84b8ec9a942e")),
    "run analysis" = paste0("Run analysis|", h("ebb684ec849d20ec8ba4ed9689")),
    "run regression" = paste0("Run regression|", h("ed9a8ceab780ebb684ec849d20ec8ba4ed9689")),
    "run logistic" = paste0("Run logistic|", h("eba19ceca780ec8aa4ed8bb120ec8ba4ed9689")),
    "report b and se instead of or / ratio and 95% ci" = paste0("Report B and SE instead of OR / ratio and 95% CI|", h("ec84a0ed839ded959c20ebaaa8ed9895ec979020eb94b0eb9dbc2042ec9980205345eba5bc204f522febb984ec9ca820ebb08f2039352520434920eb8c80ec8ba020ebb3b4eab3a0")),
    "run glm" = paste0("Run GLM|", h("474c4d20ec8ba4ed9689"))
  )
  value <- if (key %in% names(labels)) labels[[key]] else as.character(text)
  parts <- strsplit(value, "\\|", fixed = FALSE)[[1]]
  statedu_text(language, parts[[1]], parts[[length(parts)]])
}

analysis_ui_label <- function(label, language = statedu_initial_language()) {
  if (is.character(label) && length(label) == 1) {
    return(analysis_ui_text(label, language))
  }
  label
}

analysis_ui_choices <- function(choices, language = statedu_initial_language()) {
  values <- unname(choices)
  labels <- names(choices)
  if (is.null(labels)) {
    labels <- values
  }
  stats::setNames(values, vapply(labels, analysis_ui_text, character(1), language = language))
}

analysis_field_label_tag <- function(label, allowed_measurements = character(0), language = statedu_initial_language()) {
  allowed_measurements <- as.character(allowed_measurements %||% character(0))
  div(
    class = "analysis-field-label analysis-field-label-with-icons",
    span(analysis_ui_text(label, language)),
    if (length(allowed_measurements) > 0) {
      span(
        class = "analysis-allowed-measurements",
        lapply(allowed_measurements, measurement_symbol_tag)
      )
    }
  )
}

analysis_reset_button <- function(
  input_id,
  enabled = FALSE,
  language = statedu_initial_language(),
  label = analysis_ui_text("Reset setting", language)
) {
  tags$button(
    id = input_id,
    type = "button",
    class = "btn action-button btn-default analysis-reset-button",
    disabled = if (!isTRUE(enabled)) "disabled" else NULL,
    label
  )
}

analysis_transfer_listbox_input <- function(
  input_id,
  items,
  selected = character(0),
  size = 14,
  important_height = FALSE,
  height_offset = 0
) {
  values <- vapply(items, `[[`, character(1), "value")
  labels <- vapply(items, `[[`, character(1), "label")
  selected <- intersect(as.character(selected %||% character(0)), values)
  # Keep transient listbox selections isolated in renderUI callers; the shared JS restores scroll after Shiny rebinds.
  height_px <- max(4, as.integer(size %||% 14)) * 24 + as.integer(height_offset %||% 0)
  listbox_style <- paste0(
    "height:", height_px, "px", if (isTRUE(important_height)) " !important" else "", ";",
    "width:300px;min-width:300px;max-width:300px;",
    "overflow-y:auto;background:#fff;",
    "border:1px solid #b8c8d6;border-radius:6px;",
    "box-sizing:border-box;padding:4px 0;"
  )

  tagList(
    tags$select(
      id = input_id,
      class = "easyflow-hidden-select analysis-transfer-hidden-select",
      multiple = "multiple",
      style = "display:none;",
      lapply(seq_along(values), function(index) {
        tags$option(
          value = values[[index]],
          selected = if (values[[index]] %in% selected) "selected" else NULL,
          labels[[index]]
        )
      })
    ),
    div(
      class = "analysis-transfer-listbox",
      role = "listbox",
      tabindex = "0",
      `aria-multiselectable` = "true",
      `data-input-id` = input_id,
      ondragenter = "return window.easyflowTransferListboxDragOver ? window.easyflowTransferListboxDragOver(event, this) : false;",
      ondragover = "return window.easyflowTransferListboxDragOver ? window.easyflowTransferListboxDragOver(event, this) : false;",
      ondrop = "return window.easyflowTransferListboxDrop ? window.easyflowTransferListboxDrop(event, this) : false;",
      onkeydown = paste(
        "if (window.easyflowTransferListboxKeydownFallback) {",
        "return window.easyflowTransferListboxKeydownFallback(event, this);",
        "}",
        "return window.easyflowTransferListboxKeydown ? window.easyflowTransferListboxKeydown(event, this) : true;",
        sep = ""
      ),
      style = listbox_style,
      lapply(items, function(item) {
        value <- as.character(item$value)
        div(
          class = paste("analysis-transfer-option", if (value %in% selected) "is-selected" else ""),
          role = "option",
          `aria-selected` = if (value %in% selected) "true" else "false",
          `data-value` = value,
          onclick = paste(
            "if (window.easyflowTransferOptionClickFallback) {",
            "window.easyflowTransferOptionClickFallback(event, this);",
            "} else if (window.easyflowTransferOptionClick) {",
            "window.easyflowTransferOptionClick(event, this);",
            "}",
            sep = ""
          ),
          ondblclick = paste(
            "if (window.easyflowTransferOptionDoubleClick) {",
            "window.easyflowTransferOptionDoubleClick(event, this);",
            "} else {",
            "event.preventDefault(); event.stopPropagation();",
            "var listbox = this.closest('.analysis-transfer-listbox');",
            "var inputId = listbox ? listbox.getAttribute('data-input-id') : '';",
            "var value = this.getAttribute('data-value') || '';",
            "if (window.Shiny && inputId && value) {",
            "Shiny.setInputValue(inputId + '_doubleclick', {value: value, nonce: Date.now() + Math.random()}, {priority: 'event'});",
            "}",
            "}",
            sep = ""
          ),
          measurement_symbol_tag(item$measurement),
          span(item$label, class = "analysis-transfer-option-label")
        )
      })
    )
  )
}

analysis_option_group <- function(title, options, language = statedu_initial_language()) {
  div(
    class = "analysis-option-group",
    div(class = "analysis-option-title", analysis_ui_text(title, language)),
    lapply(options, function(option) {
      control <- checkboxInput(option$id, analysis_ui_label(option$label, language), value = isTRUE(option$value))
      tooltip <- option$tooltip %||% ""
      if (nzchar(tooltip)) {
        div(title = tooltip, control)
      } else {
        control
      }
    })
  )
}

analysis_radio_group <- function(title, input_id, choices, selected = NULL, language = statedu_initial_language()) {
  values <- unname(choices)
  selected <- as.character(selected %||% values[[1]])
  if (!selected %in% values) {
    selected <- values[[1]]
  }
  div(
    class = "analysis-option-group analysis-radio-group",
    div(class = "analysis-option-title", analysis_ui_text(title, language)),
    radioButtons(input_id, label = NULL, choices = analysis_ui_choices(choices, language), selected = selected)
  )
}

analysis_options_tabs_panel <- function(id, ..., selected = NULL, type = "tabs", class = "") {
  tabs <- list(...)
  panel_args <- c(
    list(id = id, type = type),
    if (!is.null(selected)) list(selected = selected) else list(),
    tabs
  )
  div(
    class = paste(
      c("analysis-options-panel", "analysis-tabbed-options", "analysis-calculator-tabs", class),
      collapse = " "
    ),
    do.call(tabsetPanel, panel_args)
  )
}
