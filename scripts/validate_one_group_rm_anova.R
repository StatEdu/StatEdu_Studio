Sys.setenv(STATEDU_NO_PACKAGE_INSTALL = "true")

source(file.path("R", "app_bootstrap.R"))
load_app_packages(check = FALSE)
source_app_modules()

set.seed(2409)
n <- 30L
id <- sprintf("S%02d", seq_len(n))
subject <- stats::rnorm(n, 50, 5)
experimental <- cbind(
  pre = subject + stats::rnorm(n, 0, 1),
  post = subject - 2 + stats::rnorm(n, 0, 1),
  followup = subject - 4 + stats::rnorm(n, 0, 1)
)
control <- cbind(
  pre = subject + stats::rnorm(n, 0, 1),
  post = subject - .5 + stats::rnorm(n, 0, 1),
  followup = subject - 1 + stats::rnorm(n, 0, 1)
)
wide <- data.frame(
  id = id,
  exp_pre = experimental[, 1], exp_post = experimental[, 2], exp_followup = experimental[, 3],
  ctl_pre = control[, 1], ctl_post = control[, 2], ctl_followup = control[, 3],
  age = round(stats::rnorm(n, 42, 8), 1),
  stringsAsFactors = FALSE
)
variable_info <- data.frame(name = names(wide), measurement = c("category", rep("continuous", 7L)), stringsAsFactors = FALSE)
wide_result <- prepare_one_group_rm_anova_results(
  wide,
  input_format = "wide",
  experimental_variables = c("exp_pre", "exp_post", "exp_followup"),
  control_variables = c("ctl_pre", "ctl_post", "ctl_followup"),
  variable_info = variable_info,
  options = list(
    assumption_check = TRUE,
    posthoc = TRUE,
    posthoc_adjustment = "holm",
    treatment_labels = c("Experimental", "Control"),
    time_labels = c("Pre", "Post", "Follow-up")
  )
)

long <- do.call(rbind, lapply(seq_len(n), function(index) {
  data.frame(
    id = id[[index]],
    group = rep(c("Experimental", "Control"), each = 3L),
    time = factor(rep(c("Pre", "Post", "Follow-up"), times = 2L), levels = c("Pre", "Post", "Follow-up"), ordered = TRUE),
    score = c(experimental[index, ], control[index, ]),
    stringsAsFactors = FALSE
  )
}))
long$group <- factor(long$group, levels = c("Experimental", "Control"))
long_info <- data.frame(name = names(long), measurement = c("category", "category", "ordered", "continuous"), stringsAsFactors = FALSE)
long_result <- prepare_one_group_rm_anova_results(
  long,
  input_format = "long",
  id_variable = "id",
  group_variable = "group",
  time_variable = "time",
  outcome_variable = "score",
  variable_info = long_info,
  options = list(assumption_check = TRUE, posthoc = TRUE, posthoc_adjustment = "holm")
)

stopifnot(identical(wide_result$type, "one_group_rm_anova"))
stopifnot(identical(long_result$type, "one_group_rm_anova"))
stopifnot(identical(as.character(long_result$treatment_labels), c("Experimental", "Control")))
stopifnot(identical(as.character(long_result$time_labels), c("Pre", "Post", "Follow-up")))
stopifnot(identical(as.character(wide_result$anova$F), as.character(long_result$anova$F)))
stopifnot(identical(as.character(wide_result$anova$p), as.character(long_result$anova$p)))
stopifnot(identical(as.character(wide_result$anova$Effect), c("Treatment", "Time", "Treatment x Time")))
stopifnot(nrow(long_result$descriptives) == 3L)
stopifnot(all(c("Experimental", "Control", "Between treatments") %in% long_result$descriptives$Group))
stopifnot(nrow(long_result$posthoc) == 12L)
stopifnot(all(c("overview", "recommendation", "descriptives", "anova", "posthoc", "assumption", "normality") %in% names(long_result)))

long_cov <- long
age_by_id <- stats::setNames(round(stats::rnorm(n, 42, 8), 1), id)
long_cov$age <- unname(age_by_id[as.character(long_cov$id)])
long_cov$id_copy <- long_cov$id
long_cov_info <- data.frame(name = names(long_cov), measurement = c("category", "category", "ordered", "continuous", "continuous", "category"), stringsAsFactors = FALSE)
long_cov_result <- prepare_one_group_rm_anova_results(
  long_cov,
  input_format = "long",
  id_variable = "id",
  group_variable = "group",
  time_variable = "time",
  outcome_variable = "score",
  covariates = "age",
  variable_info = long_cov_info,
  options = list(assumption_check = TRUE, posthoc = TRUE, posthoc_adjustment = "holm")
)
stopifnot(identical(long_cov_result$covariates, "age"))
stopifnot(any(grepl("age", long_cov_result$anova$Effect, fixed = TRUE)))
stopifnot("Treatment x Time" %in% long_cov_result$anova$Effect)

# Cross-check the uncorrected F statistics against base R's fully within-subject ANOVA.
anova_long <- data.frame(
  id = factor(rep(id, each = 6L)),
  treatment = factor(rep(rep(c("Experimental", "Control"), each = 3L), times = n)),
  time = factor(rep(c("Pre", "Post", "Follow-up"), times = 2L * n)),
  score = as.vector(t(cbind(experimental, control)))
)
reference <- summary(stats::aov(score ~ treatment * time + Error(id / (treatment * time)), data = anova_long))
extract_f <- function(stratum, effect) {
  table <- reference[[stratum]][[1L]]
  unname(table[effect, "F value"])
}
observed_f <- stats::setNames(suppressWarnings(as.numeric(long_result$anova$F)), long_result$anova$Effect)
stopifnot(isTRUE(all.equal(observed_f[["Treatment"]], extract_f("Error: id:treatment", "treatment"), tolerance = 0.005)))
stopifnot(isTRUE(all.equal(observed_f[["Time"]], extract_f("Error: id:time", "time"), tolerance = 0.005)))
stopifnot(isTRUE(all.equal(observed_f[["Treatment x Time"]], extract_f("Error: id:treatment:time", "treatment:time"), tolerance = 0.005)))

duplicate_long <- rbind(long, long[1, , drop = FALSE])
duplicate_error <- tryCatch({ one_group_rm_long_input(duplicate_long, "id", "group", "time", "score"); "" }, error = conditionMessage)
stopifnot(grepl("Duplicate combinations", duplicate_error, fixed = TRUE))

three_group <- long
three_group$group <- as.character(three_group$group)
three_group$group[1] <- "Placebo"
level_error <- tryCatch({ one_group_rm_long_input(three_group, "id", "group", "time", "score"); "" }, error = conditionMessage)
stopifnot(grepl("exactly two levels", level_error, fixed = TRUE))

setup_html <- as.character(one_group_rm_anova_setup_panel(one_group_rm_anova_setup_state(
  selected_names = names(long), variable_table = long_info, input_format = "long", language = "ko"
)))
stopifnot(grepl("one_group_rm_group_variable", setup_html, fixed = TRUE))
stopifnot(grepl("one_group_rm_available", setup_html, fixed = TRUE))
stopifnot(grepl("one-group-rm-transfer-grid", setup_html, fixed = TRUE))
stopifnot(grepl("독립변수\\(처치 group\\)", setup_html))
stopifnot(grepl("height:48px !important", setup_html, fixed = TRUE))
role_positions <- vapply(
  c("one_group_rm_outcome_variable", "one_group_rm_id_variable", "one_group_rm_time_variable", "one_group_rm_group_variable", "one_group_rm_covariates"),
  function(id) regexpr(paste0('data-input-id="', id, '"'), setup_html, fixed = TRUE)[[1]],
  integer(1)
)
stopifnot(all(role_positions > 0L), identical(order(role_positions), seq_along(role_positions)))

wide_setup_html <- as.character(one_group_rm_anova_setup_panel(one_group_rm_anova_setup_state(
  selected_names = names(wide), variable_table = variable_info, input_format = "wide", language = "ko"
)))
stopifnot(grepl("one_group_rm_experimental_variables", wide_setup_html, fixed = TRUE))
stopifnot(grepl("one_group_rm_control_variables", wide_setup_html, fixed = TRUE))
stopifnot(grepl("one_group_rm_covariates", wide_setup_html, fixed = TRUE))
stopifnot(grepl("one_group_rm_experimental_up", wide_setup_html, fixed = TRUE))
stopifnot(grepl("height:120px !important", wide_setup_html, fixed = TRUE))

style_css <- paste(readLines(file.path("www", "style.css"), warn = FALSE), collapse = "\n")
stopifnot(grepl("analysis-transfer-panel:not(.one-group-rm-target-panel)", style_css, fixed = TRUE))
stopifnot(grepl("one-group-rm-id-section .analysis-transfer-listbox", style_css, fixed = TRUE))
stopifnot(grepl("one-group-rm-covariate-section .analysis-transfer-listbox", style_css, fixed = TRUE))

result_html <- as.character(one_group_rm_anova_results_ui(long_result))
stopifnot(grepl("Group x time summary", result_html, fixed = TRUE))
stopifnot(grepl("Repeated-measures ANOVA", result_html, fixed = TRUE))
stopifnot(grepl("Treatment x Time", result_html, fixed = TRUE))

# Exercise the actual Shiny run button handler, not only the calculation helper.
test_server <- function(input, output, session) {
  register_one_group_rm_anova_handlers(
    input = input,
    output = output,
    session = session,
    selected_names_fn = function() names(long_cov),
    variable_table_fn = function() long_cov_info,
    dataset_fn = function() long_cov,
    category_table_fn = function() data.frame(),
    labels_fn = function() character(0),
    mark_settings_dirty = function() invisible(TRUE),
    app_language_fn = function() "ko"
  )
}
shiny::testServer(test_server, {
  invisible(output$one_group_rm_anova_setup)
  session$flushReact()
  session$setInputs(one_group_rm_input_format = "long")
  session$flushReact()
  assign_long_role <- function(variable, button, sequence) {
    session$setInputs(one_group_rm_available = variable)
    do.call(session$setInputs, stats::setNames(list(sequence), button))
    session$flushReact()
  }
  assign_long_role("id", "one_group_rm_id_move", 1)
  assign_long_role("id_copy", "one_group_rm_id_move", 2)
  assign_long_role("group", "one_group_rm_group_move", 1)
  assign_long_role("time", "one_group_rm_time_move", 1)
  assign_long_role("score", "one_group_rm_outcome_move", 1)
  assign_long_role("age", "one_group_rm_covariates_move", 1)
  session$setInputs(
    one_group_rm_assumption_check = TRUE,
    one_group_rm_posthoc = TRUE,
    one_group_rm_adjustment = "holm",
    one_group_rm_mean_sd = TRUE,
    run_one_group_rm_anova = 1
  )
  session$flushReact()
  rendered <- output$one_group_rm_anova_results
  rendered_text <- paste(as.character(rendered), collapse = "\n")
  stopifnot(grepl("Treatment x Time", rendered_text, fixed = TRUE))
  stopifnot(grepl("Between treatments", rendered_text, fixed = TRUE))
  stopifnot(grepl("age", rendered_text, fixed = TRUE))
  stopifnot(grepl("id_copy", rendered_text, fixed = TRUE))
})

# Exercise ordered WIDE block assignment and the same run handler.
wide_test_server <- function(input, output, session) {
  register_one_group_rm_anova_handlers(
    input = input,
    output = output,
    session = session,
    selected_names_fn = function() names(wide),
    variable_table_fn = function() variable_info,
    dataset_fn = function() wide,
    category_table_fn = function() data.frame(),
    labels_fn = function() character(0),
    mark_settings_dirty = function() invisible(TRUE),
    app_language_fn = function() "ko"
  )
}
shiny::testServer(wide_test_server, {
  invisible(output$one_group_rm_anova_setup)
  session$flushReact()
  session$setInputs(
    one_group_rm_available = c("exp_pre", "exp_post", "exp_followup"),
    one_group_rm_available_selection_order = c("exp_pre", "exp_post", "exp_followup"),
    one_group_rm_experimental_move = 1
  )
  session$flushReact()
  session$setInputs(
    one_group_rm_available = c("ctl_pre", "ctl_post", "ctl_followup"),
    one_group_rm_available_selection_order = c("ctl_pre", "ctl_post", "ctl_followup"),
    one_group_rm_control_move = 1
  )
  session$flushReact()
  session$setInputs(one_group_rm_available = "age", one_group_rm_covariates_move = 1)
  session$flushReact()
  session$setInputs(run_one_group_rm_anova = 1)
  session$flushReact()
  rendered <- output$one_group_rm_anova_results
  rendered_text <- paste(as.character(rendered), collapse = "\n")
  stopifnot(grepl("Treatment x Time", rendered_text, fixed = TRUE))
  stopifnot(grepl("age", rendered_text, fixed = TRUE))
})

message("Within-subject treatment x time repeated-measures ANOVA WIDE/LONG validation passed.")
