all_args <- commandArgs(FALSE)
file_arg <- all_args[grep("^--file=", all_args)]
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "scripts/validate_factor_pca.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(repo_root, "R", "app_bootstrap.R"))) {
  repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

source(file.path(repo_root, "R", "app_bootstrap.R"))
load_app_packages()
source_app_modules(dir = file.path(repo_root, "R"))

expect_true <- function(value, label) {
  if (!isTRUE(value)) stop(label, call. = FALSE)
}

html_h3_titles <- function(html) {
  matches <- gregexpr("<h3>([^<]+)</h3>", html, perl = TRUE)[[1]]
  if (length(matches) == 0 || matches[[1]] == -1) {
    return(character(0))
  }
  headings <- regmatches(html, list(matches))[[1]]
  sub("^<h3>([^<]+)</h3>$", "\\1", headings)
}

message("Checking factor analysis setup option layout...")
setup_variables <- c("age", "x11", "x12", "x13", "x21", "x22", "x23", "x24", "y", "y1", "y2", "y3")
setup_state <- factor_analysis_setup_state(
  selected_names = setup_variables,
  factor_variables = setup_variables[1:4],
  variable_table = data.frame(
    name = setup_variables,
    measurement = rep("continuous", length(setup_variables)),
    stringsAsFactors = FALSE
  )
)
setup_html <- as.character(htmltools::renderTags(factor_analysis_setup_panel(setup_state))$html)
expect_true(
  all(vapply(
    c("factor_matrix_type", "factor_assumption", "factor_method", "factor_rotation"),
    function(id) grepl(sprintf("<select[^>]+id=\"%s\"", id), setup_html, perl = TRUE),
    logical(1)
  )),
  "Expected factor analysis model options to use compact select controls"
)
expect_true(
  !any(vapply(
    c("factor_matrix_type", "factor_assumption", "factor_method", "factor_rotation"),
    function(id) grepl(sprintf("type=\"radio\"[^>]+name=\"%s\"", id), setup_html, perl = TRUE),
    logical(1)
  )),
  "Expected compact select controls to replace model option radio groups"
)
expect_true(
  !grepl("factor_normality_method", setup_html, fixed = TRUE) &&
    !grepl("factor_normality", setup_html, fixed = TRUE),
  "Expected normality select to replace the separate normality checkbox and method select"
)
expect_true(
  grepl("type=\"radio\"[^>]+name=\"factor_criterion\"", setup_html, perl = TRUE),
  "Expected factor selection to remain a radio group with the fixed-factor number input"
)

set.seed(42)
n <- 180
latent1 <- rnorm(n)
latent2 <- rnorm(n)
data <- data.frame(
  x1 = latent1 + rnorm(n, 0, 0.35),
  x2 = latent1 + rnorm(n, 0, 0.35),
  x3 = latent1 + rnorm(n, 0, 0.35),
  x4 = latent2 + rnorm(n, 0, 0.35),
  x5 = latent2 + rnorm(n, 0, 0.35),
  stringsAsFactors = FALSE
)
variable_info <- data.frame(
  name = names(data),
  measurement = "continuous",
  stringsAsFactors = FALSE
)

message("Checking factor analysis defaults and normality-driven method selection...")
factor_result <- prepare_factor_analysis_results(
  data,
  variables = names(data),
  variable_info = variable_info,
  options = list(
    normality = FALSE,
    method = "pa",
    rotation = "varimax",
    criterion = "eigen",
    n_factors = 1,
    sort_loadings = TRUE,
    hide_small_loadings = TRUE,
    highlight_problem_values = TRUE
  )
)
expect_true(identical(factor_result$method, "pa"), "Expected default factor method to remain principal axis factoring")
expect_true(is.data.frame(factor_result$loadings_table) && nrow(factor_result$loadings_table) == ncol(data) + 4L, "Expected factor loading table with variance and suitability summary rows")
expect_true(is.data.frame(factor_result$eigen_table) && nrow(factor_result$eigen_table) == ncol(data), "Expected factor eigenvalue table")
expected_loading_order <- {
  loading_abs <- abs(factor_result$loadings)
  primary_factor <- max.col(loading_abs, ties.method = "first")
  primary_loading <- loading_abs[cbind(seq_len(nrow(loading_abs)), primary_factor)]
  names(data)[order(primary_factor, -primary_loading, rownames(factor_result$loadings), na.last = TRUE)]
}
expected_loading_labels <- unname(factor_result$display_names[expected_loading_order])
expect_true(
  identical(as.character(factor_result$loadings_table$Variable[seq_along(expected_loading_labels)]), expected_loading_labels),
  "Expected factor loading table to be sorted by primary factor and loading size"
)
expect_true(
  identical(tail(as.character(factor_result$loadings_table$Variable), 4), c("Eigenvalue", "Variance %", "Cumulative variance %", "")),
  "Expected factor loading table to end with eigenvalue, variance, and suitability summary rows"
)
factor_result_html <- as.character(htmltools::renderTags(factor_analysis_results_ui(factor_result))$html)
expect_true(
  identical(html_h3_titles(factor_result_html)[1:2], c("Factor analysis", "Pattern / loading matrix")),
  "Expected factor analysis UI to show the overview table first and pattern/loading matrix second"
)
factor_columns <- colnames(factor_result$loadings)
expect_true(
  all(vapply(factor_result$loadings_table[seq.int(nrow(factor_result$loadings_table) - 3L, nrow(factor_result$loadings_table) - 1L), factor_columns, drop = FALSE], function(column) all(nzchar(as.character(column))), logical(1))),
  "Expected factor loading summary rows to contain values under factor columns"
)
expect_true(
  any(grepl("Bartlett's x2 \\(p\\)=", as.character(unlist(factor_result$loadings_table[nrow(factor_result$loadings_table), , drop = FALSE])))),
  "Expected factor loading summary rows to include KMO and Bartlett diagnostics"
)
factor_unsorted <- prepare_factor_analysis_results(
  data,
  variables = names(data),
  variable_info = variable_info,
  options = list(
    normality = FALSE,
    method = "pa",
    rotation = "varimax",
    criterion = "eigen",
    n_factors = 1,
    sort_loadings = FALSE,
    hide_small_loadings = TRUE,
    highlight_problem_values = TRUE
  )
)
expect_true(
  identical(as.character(factor_unsorted$loadings_table$Variable[seq_along(names(data))]), unname(factor_unsorted$display_names[names(data)])),
  "Expected factor loading table to keep selected input order when sorting is disabled"
)
expect_true("h²" %in% names(factor_result$loadings_table), "Expected factor loading table to label communality as h²")
expect_true(!"u2" %in% names(factor_result$loadings_table), "Expected factor loading table to omit uniqueness")
factor_problem <- factor_result
problem_rows <- rownames(factor_problem$loadings)[seq_len(min(2, nrow(factor_problem$loadings)))]
factor_problem$communality[problem_rows[[1]]] <- 0.20
if (length(problem_rows) >= 2) {
  factor_problem$complexity[problem_rows[[2]]] <- 2.25
}
factor_problem$loadings_table <- factor_analysis_loading_table(factor_problem)
problem_styles <- attr(factor_problem$loadings_table, "cell_styles")
expect_true(
  is.data.frame(problem_styles) &&
    nrow(problem_styles) >= length(problem_rows) &&
    any(grepl("background:#fee2e2", problem_styles$style, fixed = TRUE)),
  "Expected problematic h² and complexity values to be marked with red background display"
)
factor_problem_html <- as.character(htmltools::renderTags(coefficient_html_table(factor_problem$loadings_table))$html)
expect_true(
  grepl("background:#fee2e2", factor_problem_html, fixed = TRUE),
  "Expected problematic h² and complexity values to render with red background"
)
factor_problem$options$highlight_problem_values <- FALSE
factor_problem$loadings_table <- factor_analysis_loading_table(factor_problem)
expect_true(
  !any(grepl("background:#fee2e2", attr(factor_problem$loadings_table, "cell_styles")$style, fixed = TRUE)),
  "Expected problem highlighting to be disabled when the option is unchecked"
)

factor_all_loadings <- prepare_factor_analysis_results(
  data,
  variables = names(data),
  variable_info = variable_info,
  options = list(
    normality = FALSE,
    method = "pa",
    rotation = "varimax",
    criterion = "fixed",
    n_factors = 2,
    hide_small_loadings = FALSE,
    highlight_problem_values = TRUE
  )
)
factor_columns <- colnames(factor_all_loadings$loadings)
expect_true(
  all(vapply(factor_all_loadings$loadings_table[seq_len(nrow(factor_all_loadings$loadings)), factor_columns, drop = FALSE], function(column) all(nzchar(as.character(column))), logical(1))),
  "Expected all factor loadings to be displayed when the cutoff filter is disabled"
)
expect_true(
  is.data.frame(attr(factor_all_loadings$loadings_table, "bold_cells")) &&
    nrow(attr(factor_all_loadings$loadings_table, "bold_cells")) > 0,
  "Expected large loadings to be marked for bold display when all loadings are shown"
)
factor_all_html <- as.character(htmltools::renderTags(coefficient_html_table(factor_all_loadings$loadings_table))$html)
expect_true(grepl("font-weight:700", factor_all_html, fixed = TRUE), "Expected large loadings to render in bold")
expect_true(grepl("colspan=", factor_all_html, fixed = TRUE), "Expected factor loading diagnostics row to merge KMO/Bartlett cells")
expect_true(grepl("text-align:center", factor_all_html, fixed = TRUE), "Expected merged diagnostics row to be center aligned")
expect_true(grepl("KMO=", factor_all_html, fixed = TRUE), "Expected merged diagnostics row to include KMO label")
expect_true(grepl("border-top:2px solid #1f2937", factor_all_html, fixed = TRUE), "Expected merged diagnostics row to have a visible top border")
factor_all_spans <- attr(factor_all_loadings$loadings_table, "spanning_cells", exact = TRUE)
expect_true(
  is.data.frame(factor_all_spans) &&
    identical(factor_all_spans$start_column[[1]], colnames(factor_all_loadings$loadings)[[1]]),
  "Expected merged diagnostics row to start under the first factor column"
)

factor_no_rotation <- prepare_factor_analysis_results(
  data,
  variables = names(data),
  variable_info = variable_info,
  options = list(
    normality = FALSE,
    method = "pa",
    rotation = "none",
    criterion = "fixed",
    n_factors = 2
  )
)
expect_true(identical(factor_no_rotation$rotation, "none"), "Expected factor analysis to support no rotation")
expect_true(is.null(factor_no_rotation$structure_table), "Expected no structure matrix when rotation does not estimate factor correlations")

factor_oblimin <- prepare_factor_analysis_results(
  data,
  variables = names(data),
  variable_info = variable_info,
  options = list(
    normality = FALSE,
    method = "pa",
    rotation = "oblimin",
    criterion = "fixed",
    n_factors = 2
  )
)
expect_true(
  is.data.frame(factor_oblimin$structure_table) &&
    nrow(factor_oblimin$structure_table) == ncol(data) &&
    all(colnames(factor_oblimin$loadings) %in% names(factor_oblimin$structure_table)),
  "Expected oblique rotation to include a structure matrix"
)
factor_oblimin_html <- as.character(htmltools::renderTags(factor_analysis_results_ui(factor_oblimin))$html)
expect_true(
  grepl("Structure matrix", factor_oblimin_html, fixed = TRUE) &&
    grepl("pattern matrix shows unique factor contributions", factor_oblimin_html, fixed = TRUE),
  "Expected oblique factor analysis UI to render the structure matrix and interpretation note"
)
expect_true(
  identical(html_h3_titles(factor_oblimin_html)[1:3], c("Factor analysis", "Pattern / loading matrix", "Structure matrix")),
  "Expected oblique factor analysis UI to show the structure matrix third"
)

too_many_factor_error <- tryCatch(
  {
    prepare_factor_analysis_results(
      data,
      variables = names(data),
      variable_info = variable_info,
      options = list(
        normality = FALSE,
        method = "pa",
        rotation = "varimax",
        criterion = "fixed",
        n_factors = 3
      )
    )
    ""
  },
  error = conditionMessage
)
expect_true(
  grepl("Requested factor count", too_many_factor_error, fixed = TRUE) &&
    grepl("Use 2 or fewer factors", too_many_factor_error, fixed = TRUE),
  "Expected fixed factor counts that are too high for the number of variables to show a clear message"
)

factor_with_reliability <- prepare_factor_analysis_results(
  data,
  variables = names(data),
  variable_info = variable_info,
  options = list(
    normality = FALSE,
    method = "pa",
    rotation = "varimax",
    criterion = "fixed",
    n_factors = 2,
    subfactor_reliability = TRUE
  )
)
expect_true(
  identical(factor_with_reliability$subfactor_reliability$type, "reliability_factors"),
  "Expected factor analysis to attach subfactor reliability results when the option is enabled"
)
expect_true(
  length(factor_with_reliability$subfactor_reliability$factors) > 0,
  "Expected at least one subfactor reliability estimate"
)
expect_true(
  is.list(factor_with_reliability$subfactor_reliability$total),
  "Expected overall reliability estimate when subfactor reliability is enabled"
)
factor_reliability_overview <- reliability_factor_overview_table(factor_with_reliability$subfactor_reliability)
factor_reliability_items <- reliability_factor_item_analysis_table(factor_with_reliability$subfactor_reliability)
expect_true(
  is.data.frame(factor_reliability_overview) && nrow(factor_reliability_overview) > 0,
  "Expected subfactor reliability overview table"
)
expect_true(
  is.data.frame(factor_reliability_items) &&
    any(c("Cronbach's alpha if item deleted", "Ordinal alpha if item deleted", "Reliability if item deleted") %in% names(factor_reliability_items)) &&
    "Corrected item-total correlation" %in% names(factor_reliability_items),
  "Expected subfactor item-deleted reliability and item-total correlation output"
)
expect_true(
  all(c("Reliability", "Reliability if deleted", "Item-total r") %in% names(factor_with_reliability$loadings_table)),
  "Expected subfactor reliability columns to be appended to the factor loading table"
)
expect_true(
  sum(nzchar(as.character(factor_with_reliability$loadings_table$Reliability))) ==
    length(factor_with_reliability$subfactor_reliability$factors) + 1L,
  "Expected subfactor reliability to appear on each subfactor first item and overall reliability on the Eigenvalue row"
)
expect_true(
  nzchar(as.character(factor_with_reliability$loadings_table$Reliability[factor_with_reliability$loadings_table$Variable == "Eigenvalue"])),
  "Expected overall reliability to appear in the Reliability column on the Eigenvalue row"
)
factor_saved_scores <- factor_analysis_saved_score_outputs(
  factor_with_reliability,
  include_means = TRUE,
  include_sums = TRUE,
  include_scores = TRUE
)
expect_true(
  is.data.frame(factor_saved_scores) &&
    nrow(factor_saved_scores) == nrow(data) &&
    all(c("MF_FA1", "SF_FA1", "FS_FA1") %in% names(factor_saved_scores)),
  "Expected factor analysis saved score options to create row-matched mean, sum, and factor score variables"
)
expect_true(
  all(vapply(factor_saved_scores, is.numeric, logical(1))),
  "Expected saved factor analysis variables to be numeric"
)
factor_custom_saved_scores <- factor_analysis_saved_score_outputs(
  factor_with_reliability,
  include_means = TRUE,
  base_name = "PA"
)
expect_true(
  all(c("MF_PA1", "MF_PA2") %in% names(factor_custom_saved_scores)),
  "Expected factor analysis saved score names to honor the user base name"
)
factor_reliability_html <- as.character(htmltools::renderTags(factor_analysis_results_ui(factor_with_reliability))$html)
expect_true(
  grepl("Reliability if deleted", factor_reliability_html, fixed = TRUE) &&
    grepl("Item-total r", factor_reliability_html, fixed = TRUE) &&
    grepl("complete cases within each item set", factor_reliability_html, fixed = TRUE) &&
    grepl("parallel analysis", factor_reliability_html, fixed = TRUE) &&
    !grepl("Reliability by subfactor", factor_reliability_html, fixed = TRUE),
  "Expected factor analysis UI to include subfactor reliability and interpretation notes in the loading table only"
)
factor_negative_note_result <- factor_with_reliability
negative_primary <- max.col(abs(factor_negative_note_result$loadings), ties.method = "first")[[1]]
factor_negative_note_result$loadings[1, negative_primary] <- -abs(factor_negative_note_result$loadings[1, negative_primary])
expect_true(
  grepl("Potential reverse-keyed items", factor_analysis_negative_primary_note(factor_negative_note_result), fixed = TRUE),
  "Expected negative primary loadings to be noted as potential reverse-keyed items"
)

factor_issue_result <- factor_with_reliability
factor_issue_result$matrix$x1[[1]] <- Inf
factor_issue_result$matrix$x2 <- 1
factor_issue_result$subfactor_reliability <- factor_analysis_subfactor_reliability(factor_issue_result)
expect_true(
  is.data.frame(factor_issue_result$subfactor_reliability$item_issues) &&
    all(c("Subfactor", "Item", "Variable", "Problem") %in% names(factor_issue_result$subfactor_reliability$item_issues)) &&
    any(factor_issue_result$subfactor_reliability$item_issues$Variable == "x1") &&
    any(factor_issue_result$subfactor_reliability$item_issues$Variable == "x2"),
  "Expected subfactor reliability diagnostics to identify problematic items"
)
factor_issue_note <- factor_analysis_reliability_note(factor_issue_result)
expect_true(
  grepl("x1", factor_issue_note, fixed = TRUE) &&
    grepl("x2", factor_issue_note, fixed = TRUE) &&
    grepl("infinite", factor_issue_note, fixed = TRUE) &&
    grepl("zero variance", factor_issue_note, fixed = TRUE),
  "Expected subfactor reliability note to describe item-level problems"
)

ordered_info <- variable_info
ordered_info$measurement[[1]] <- "ordered"
factor_ordered_note_result <- factor_result
factor_ordered_note_result$variable_info <- ordered_info
factor_ordered_html <- as.character(htmltools::renderTags(factor_analysis_results_ui(factor_ordered_note_result))$html)
expect_true(
  grepl("Ordinal variables were analyzed with Pearson correlations", factor_ordered_html, fixed = TRUE),
  "Expected Pearson correlation note when ordinal variables are included in factor analysis"
)

ordinal_data <- as.data.frame(replicate(5, sample(1:5, 150, replace = TRUE)), check.names = FALSE)
names(ordinal_data) <- paste0("o", seq_len(5))
ordinal_info <- data.frame(
  name = names(ordinal_data),
  var_label = paste("Ordinal", seq_len(5)),
  measurement = "ordered",
  stringsAsFactors = FALSE
)
factor_poly <- prepare_factor_analysis_results(
  ordinal_data,
  variables = names(ordinal_data),
  variable_info = ordinal_info,
  options = list(matrix_type = "polychoric", normality = FALSE, method = "pa", criterion = "fixed", n_factors = 1)
)
expect_true(identical(factor_poly$matrix_type, "polychoric"), "Expected factor analysis to use polychoric matrix when requested for ordinal items")
expect_true(identical(factor_poly$overview$Matrix[[1]], "Polychoric correlation"), "Expected factor analysis overview to show polychoric matrix")
factor_poly_html <- as.character(htmltools::renderTags(factor_analysis_results_ui(factor_poly))$html)
expect_true(grepl("polychoric correlation matrix", factor_poly_html, fixed = TRUE), "Expected factor analysis HTML to describe polychoric matrix")

factor_loading_problem <- factor_all_loadings
factor_loading_problem$options$hide_small_loadings <- TRUE
factor_loading_problem$options$highlight_problem_values <- TRUE
factor_loading_problem$loadings[1, ] <- 0.05
factor_loading_problem$loadings[1, 1] <- 0.25
if (ncol(factor_loading_problem$loadings) >= 2) {
  factor_loading_problem$loadings[2, 1] <- 0.72
  factor_loading_problem$loadings[2, 2] <- 0.42
}
factor_loading_problem$loadings_table <- factor_analysis_loading_table(factor_loading_problem)
factor_loading_problem_styles <- attr(factor_loading_problem$loadings_table, "cell_styles")
expect_true(
  is.data.frame(factor_loading_problem_styles) &&
    any(grepl("background:#fee2e2", factor_loading_problem_styles$style, fixed = TRUE)),
  "Expected low primary and high cross-loadings to be marked with red background"
)
factor_loading_problem_html <- as.character(htmltools::renderTags(coefficient_html_table(factor_loading_problem$loadings_table))$html)
expect_true(
  grepl(".250", factor_loading_problem_html, fixed = TRUE) &&
    grepl("background:#fee2e2", factor_loading_problem_html, fixed = TRUE),
  "Expected low primary loading to be shown and highlighted when problem highlighting is enabled"
)

factor_ml <- prepare_factor_analysis_results(
  data,
  variables = names(data),
  variable_info = variable_info,
  options = list(
    normality = TRUE,
    normality_method = "skew_kurt",
    method = "pa",
    rotation = "varimax",
    criterion = "fixed",
    n_factors = 1
  )
)
expect_true(identical(factor_ml$method, "ml"), "Expected normal data to use maximum likelihood when normality is checked")

skewed_data <- data
skewed_data$x5 <- exp(rnorm(n, 0, 1.8))
factor_pa <- prepare_factor_analysis_results(
  skewed_data,
  variables = names(skewed_data),
  variable_info = variable_info,
  options = list(
    normality = TRUE,
    normality_method = "mardia",
    method = "ml",
    rotation = "varimax",
    criterion = "fixed",
    n_factors = 1
  )
)
expect_true(identical(factor_pa$method, "pa"), "Expected non-normal data to use principal axis factoring when normality is checked")
expect_true(is.data.frame(factor_pa$normality_table) && nrow(factor_pa$normality_table) == 2, "Expected Mardia normality table")

message("Checking PCA options, plots, and exports...")
pca_result <- prepare_pca_results(
  data,
  variables = names(data),
  variable_info = variable_info,
  options = list(
    matrix_type = "correlation",
    rotation = "none",
    criterion = "eigen",
    n_components = 1,
    cumulative_variance = 70,
    scree_plot = TRUE,
    component_plot = TRUE
  )
)
expect_true(pca_result$n_components >= 1, "Expected at least one PCA component")
expect_true(is.data.frame(pca_result$loadings_table) && nrow(pca_result$loadings_table) == ncol(data) + 4, "Expected PCA loading table with summary rows")
expect_true("h²" %in% names(pca_result$loadings_table), "Expected PCA loading table to use h²")
expect_true("Complexity" %in% names(pca_result$loadings_table), "Expected PCA loading table to include complexity")
expect_true(!"Uniqueness" %in% names(pca_result$loadings_table), "Expected PCA loading table to omit uniqueness")
expect_true(!any(c("Reliability", "Reliability if deleted", "Item-total r") %in% names(pca_result$loadings_table)), "Expected PCA loading table to omit reliability columns")
expect_true(
  identical(tail(as.character(pca_result$loadings_table$Variable), 4), c("Eigenvalue", "Variance %", "Cumulative variance %", "")),
  "Expected PCA loading table to include factor-style summary rows"
)
pca_result_html <- as.character(htmltools::renderTags(pca_results_ui(pca_result))$html)
expect_true(
  identical(html_h3_titles(pca_result_html)[1:2], c("Principal component analysis", "Component loadings")),
  "Expected PCA UI to show the overview table first and component loadings second"
)
pca_table_html <- as.character(htmltools::renderTags(coefficient_html_table(pca_result$loadings_table))$html)
expect_true(grepl("KMO=", pca_table_html, fixed = TRUE), "Expected PCA loading table to include KMO diagnostics")
expect_true(grepl("colspan=", pca_table_html, fixed = TRUE), "Expected PCA diagnostics row to merge loading columns")
expect_true(is.data.frame(pca_result$variance_table) && nrow(pca_result$variance_table) > 0, "Expected PCA variance table")
expect_true(is.data.frame(pca_result$eigen_table) && nrow(pca_result$eigen_table) == ncol(data), "Expected PCA eigenvalue table")
pca_saved_scores <- pca_saved_score_outputs(pca_result, base_name = "PA")
expect_true(
  is.data.frame(pca_saved_scores) &&
    nrow(pca_saved_scores) == nrow(data) &&
    "PC_PA1" %in% names(pca_saved_scores),
  "Expected PCA saved score names to honor the user base name"
)

pca_poly <- prepare_pca_results(
  ordinal_data,
  variables = names(ordinal_data),
  variable_info = ordinal_info,
  options = list(matrix_type = "polychoric", criterion = "fixed", n_components = 2, rotation = "none", save_component_scores = TRUE)
)
expect_true(identical(pca_poly$matrix_type, "polychoric"), "Expected PCA to use polychoric matrix when requested for ordinal items")
expect_true(identical(pca_poly$overview$Matrix[[1]], "Polychoric correlation"), "Expected PCA overview to show polychoric matrix")
expect_true(is.data.frame(pca_poly$warnings) && any(grepl("Component scores are not available", pca_poly$warnings$Warning, fixed = TRUE)), "Expected PCA polychoric score warning")
pca_poly_html <- as.character(htmltools::renderTags(pca_results_ui(pca_poly))$html)
expect_true(grepl("<h3>Warnings</h3>", pca_poly_html, fixed = TRUE), "Expected PCA warnings section for polychoric score warning")

pca_cumulative <- prepare_pca_results(
  data,
  variables = names(data),
  variable_info = variable_info,
  options = list(
    matrix_type = "correlation",
    rotation = "varimax",
    criterion = "cumulative",
    n_components = 1,
    cumulative_variance = 70,
    scree_plot = TRUE,
    component_plot = TRUE
  )
)
expect_true(identical(pca_cumulative$criterion, "cumulative"), "Expected cumulative PCA criterion")

factor_html <- tempfile(fileext = ".html")
factor_xlsx <- tempfile(fileext = ".xlsx")
factor_oblimin_xlsx <- tempfile(fileext = ".xlsx")
pca_html <- tempfile(fileext = ".html")
pca_xlsx <- tempfile(fileext = ".xlsx")
write_factor_analysis_results_html(factor_result, factor_html)
save_factor_analysis_excel_file(factor_result, factor_xlsx)
save_factor_analysis_excel_file(factor_oblimin, factor_oblimin_xlsx)
write_pca_results_html(pca_result, pca_html)
save_pca_excel_file(pca_result, pca_xlsx)
expect_true(file.exists(factor_html) && file.info(factor_html)$size > 0, "Expected factor analysis HTML export")
expect_true(file.exists(factor_xlsx) && file.info(factor_xlsx)$size > 0, "Expected factor analysis Excel export")
expect_true(
  identical(openxlsx::getSheetNames(factor_xlsx)[1:2], c("Overview", "Loadings")),
  "Expected factor analysis Excel export to put loadings directly after overview"
)
expect_true(
  identical(openxlsx::getSheetNames(factor_oblimin_xlsx)[1:3], c("Overview", "Loadings", "Structure")),
  "Expected oblique factor analysis Excel export to put structure matrix third"
)
expect_true(file.exists(pca_html) && file.info(pca_html)$size > 0, "Expected PCA HTML export")
expect_true(file.exists(pca_xlsx) && file.info(pca_xlsx)$size > 0, "Expected PCA Excel export")
expect_true(
  identical(openxlsx::getSheetNames(pca_xlsx)[1:2], c("Overview", "Loadings")),
  "Expected PCA Excel export to put component loadings directly after overview"
)

message("Factor analysis and PCA validations passed.")
