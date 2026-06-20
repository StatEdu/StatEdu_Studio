latent_modules <- list(
  mixture = list(
    menu = "Mixture Model",
    title = "Latent Mixture Model",
    subtitle = "LCA, LPA, and mixed-indicator mixture modeling with Mplus.",
    engine = "cross_sectional_mixture",
    mixture_type = "AUTO",
    analysis_key = "mixed_model"
  ),
  lta = list(
    menu = "LTA",
    title = "Latent Transition Analysis",
    subtitle = "Longitudinal latent class transitions across time points.",
    engine = "latent_transition",
    mixture_type = "LTA",
    analysis_key = "latent_transition"
  ),
  state_transition = list(
    menu = "State Transition",
    title = "State Transition",
    subtitle = "Observed state transition tables, figures, and summaries.",
    engine = "state_transition",
    mixture_type = "STATE",
    analysis_key = "state_transition"
  )
)

latent_home_tab <- function() {
  tabPanel(
    "Home",
    value = "home",
    div(
      class = "page-shell latent-home",
      div(
        class = "app-heading",
        h1("StatEdu Studio Latent Mplus"),
        div("A local interface for dictionary-driven Mplus latent analysis pipelines.", class = "app-subtitle")
      ),
      div(
        class = "latent-dashboard-grid",
        metric_tile("Workflow", "1-2-3", "Data, setup, run and results"),
        metric_tile("Engine", latent_default_project_root(), "Bundled R/Mplus pipeline"),
        metric_tile("Scope", "Mixture, LTA", "State transition included")
      ),
      div(
        class = "workspace-panel latent-overview-panel",
        h3("Project Shell"),
        p("This UI shell reuses the StatEdu Studio block workflow and runs the bundled Latent pipeline through dictionary and CFG.yml files."),
        div(
          class = "latent-home-actions",
          actionButton("home_open_mixture", "Start Mixture Model", class = "btn btn-primary")
        )
      )
    )
  )
}

latent_menu_tab <- function() {
  navbarMenu(
    "Latent",
    latent_analysis_tab("mixture"),
    latent_analysis_tab("lta"),
    latent_analysis_tab("state_transition")
  )
}

latent_analysis_tab <- function(module_id) {
  spec <- latent_modules[[module_id]]
  tabPanel(
    spec$menu,
    value = paste0("latent_", module_id),
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(spec$title),
        div(spec$subtitle, class = "app-subtitle")
      ),
      latent_workflow(module_id, spec)
    )
  )
}

latent_workflow <- function(module_id, spec) {
  ns <- paste0("latent_", module_id)
  div(
    class = "latent-workflow",
    div(
      class = "latent-step-tabs",
      tags$button(type = "button", class = "latent-step-tab active", `data-target` = paste0(ns, "_data"), "1 Data"),
      tags$button(type = "button", class = "latent-step-tab", `data-target` = paste0(ns, "_setup"), "2 Setup"),
      tags$button(type = "button", class = "latent-step-tab", `data-target` = paste0(ns, "_run"), "3 Results")
    ),
    div(
      id = paste0(ns, "_data"),
      class = "latent-step-panel active",
      latent_data_block(module_id, spec)
    ),
    div(
      id = paste0(ns, "_setup"),
      class = "latent-step-panel",
      latent_setup_block(module_id, spec)
    ),
    div(
      id = paste0(ns, "_run"),
      class = "latent-step-panel",
      latent_run_block(module_id, spec)
    )
  )
}

latent_data_block <- function(module_id, spec) {
  div(
    class = "latent-block-grid",
    div(
      class = "workspace-panel latent-control-panel",
      h3("Data"),
      textInput(paste0(module_id, "_dataset_id"), "Dataset ID", value = ""),
      textInput(paste0(module_id, "_project_root"), "Latent project root", value = latent_default_project_root()),
      div(class = "latent-panel-note", textOutput(paste0(module_id, "_dataset_id_message"), inline = TRUE)),
      div(
        class = "latent-button-row",
        actionButton(paste0(module_id, "_go_data_tab"), "Open Data tab", class = "btn btn-primary"),
        actionButton(paste0(module_id, "_load_yaml_data"), "Load YAML", class = "btn btn-default"),
        actionButton(paste0(module_id, "_save_yaml"), "Save roles", class = "btn btn-default"),
        actionButton(paste0(module_id, "_load_template"), "Load template", class = "btn btn-default")
      ),
      hr(),
      h3("Roles"),
      selectInput(
        paste0(module_id, "_active_role"),
        "Active role",
        choices = latent_role_choices(module_id),
        selectize = FALSE
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'subset'", paste0(module_id, "_active_role")),
        div(
          class = "latent-subset-condition-panel",
          selectInput(
            paste0(module_id, "_subset_condition_mode"),
            "Subset condition",
            choices = c("Equals value" = "equals", "Not missing" = "not_missing", "Custom expression" = "expr"),
            selected = "equals",
            selectize = FALSE
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'equals'", paste0(module_id, "_subset_condition_mode")),
            textInput(paste0(module_id, "_subset_value"), "Value", value = "")
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'expr'", paste0(module_id, "_subset_condition_mode")),
            textInput(paste0(module_id, "_subset_expr"), "Expression", value = "", placeholder = "target == 1")
          )
        )
      ),
      div(class = "latent-panel-note", "Check variables in the Variables table to assign the active role, or use the Role dropdown in each row. Use the table length menu to change how many variables are shown."),
      div(
        class = "latent-button-row",
        actionButton(paste0(module_id, "_clear_active_role"), "Clear active role", class = "btn btn-default"),
        actionButton(paste0(module_id, "_clear_all_roles"), "Clear roles", class = "btn btn-default")
      ),
      placeholder_table(paste0(module_id, "_role_summary"))
    ),
    div(
      class = "workspace-panel latent-main-panel latent-variable-role-panel",
      h3("Variables"),
      div(class = "latent-panel-note", "Choose a latent-analysis role for each variable."),
      placeholder_table(paste0(module_id, "_variable_preview"))
    )
  )
}

latent_scope_badges <- function(scopes) {
  scopes <- as.character(scopes)
  tags$div(
    class = "latent-option-scope",
    lapply(scopes, function(scope) {
      scope_key <- tolower(gsub("[^a-z0-9]+", "-", scope))
      tags$span(class = paste("latent-analysis-badge", paste0("scope-", scope_key)), scope)
    })
  )
}

latent_option_item <- function(control, scopes = "Common", note = NULL) {
  div(
    class = "latent-option-card",
    latent_scope_badges(scopes),
    control
  )
}

latent_estimation_preset_choices <- function() {
  c(
    "Test / quick check" = "test",
    "Desktop 9950X3D / 128GB" = "desktop_9950x3d",
    "Custom" = "custom"
  )
}

latent_run_pipeline_controls <- function(module_id) {
  div(
    class = "latent-run-footer",
    div(
      class = "latent-run-controls-grid",
      div(
        class = "latent-run-field",
        selectInput(
          paste0(module_id, "_from_step"),
          "From step",
          choices = c("settings", "prep", "estimation_build", "estimation_run", "estimation_collect", "select_best_k", "classify", "r3step", "bch", "bch_moderation", "tables", "figures", "export_docx", "finalize")
        )
      ),
      div(
        class = "latent-run-field",
        selectInput(
          paste0(module_id, "_to_step"),
          "To step",
          choices = c("settings", "prep", "estimation_build", "estimation_run", "estimation_collect", "select_best_k", "classify", "r3step", "bch", "bch_moderation", "tables", "figures", "export_docx", "finalize"),
          selected = "finalize"
        )
      )
    ),
    div(
      class = "latent-run-action-row",
      actionButton(paste0(module_id, "_run_pipeline"), "Run Mplus analysis", class = "btn btn-primary")
    )
  )
}

latent_setup_block <- function(module_id, spec) {
  panel_id <- function(suffix) paste0("latent_", module_id, "_setup_", suffix)
  div(
    class = "latent-block-grid latent-setup-workspace",
    div(
      class = "workspace-panel latent-control-panel",
      h3("Setup"),
      div(
        class = "latent-setup-topic-list",
        tags$button(type = "button", class = "latent-setup-topic active", `data-target` = panel_id("model"), "Model Selection"),
        tags$button(type = "button", class = "latent-setup-topic", `data-target` = panel_id("mplus"), "Mplus Estimation"),
        tags$button(type = "button", class = "latent-setup-topic", `data-target` = panel_id("post"), "Data / Post-estimation"),
        tags$button(type = "button", class = "latent-setup-topic", `data-target` = panel_id("output"), "Table / Figure")
      ),
      hr(),
      h3("Settings"),
      div(
        class = "latent-button-column",
        actionButton(paste0(module_id, "_load_yaml"), "Load settings", class = "btn btn-default")
      ),
      div(class = "latent-panel-note", textOutput(paste0(module_id, "_setup_yaml_status"), inline = TRUE)),
      h3("Run Analysis"),
      latent_run_pipeline_controls(module_id)
    ),
    div(
      class = "workspace-panel latent-main-panel",
      h3("Analysis Options"),
      div(
        class = "latent-setup-panel active",
        id = panel_id("model"),
        h4("Model Selection"),
        div(
          class = "latent-options-grid",
          latent_option_item(
            selectInput(
              paste0(module_id, "_analysis_id"),
              "Analysis ID",
              choices = latent_analysis_choices(),
              selected = spec$analysis_key,
              selectize = FALSE
            ),
            "Common",
            "LCA, LPA, mixed-indicator mixture model을 같은 메뉴에서 선택합니다."
          ),
          latent_option_item(
            textInput(paste0(module_id, "_mixture_type"), "Mixture type", value = spec$mixture_type),
            c("LCA", "LPA", "Mixed"),
            "AUTO는 지표 유형을 보고 LCA/LPA 또는 혼합 지표 분석으로 판단합니다."
          ),
          latent_option_item(
            selectInput(paste0(module_id, "_indicator_type"), "Indicator type", choices = c("auto", "categorical", "continuous"), selected = "auto", selectize = FALSE),
            c("LCA", "LPA", "Mixed"),
            "categorical=LCA, continuous=LPA, auto=mixed 가능성을 포함해 자동 판정합니다."
          ),
          latent_option_item(
            numericInput(paste0(module_id, "_seed"), "Seed", value = 20260331, min = 1, step = 1),
            "Common"
          ),
          latent_option_item(
            numericInput(paste0(module_id, "_k_min"), "k min", value = 2, min = 1, step = 1),
            "Common"
          ),
          latent_option_item(
            numericInput(paste0(module_id, "_k_max"), "k max", value = 6, min = 1, step = 1),
            "Common"
          ),
          latent_option_item(
            textInput(paste0(module_id, "_k_values"), "k values", value = "", placeholder = "Optional, e.g., 2,3,4,5"),
            "Common",
            "비워두면 k min부터 k max까지 실행합니다."
          ),
          latent_option_item(
            selectInput(paste0(module_id, "_best_k_rule"), "Best-k rule", choices = c("hybrid", "bic", "aic", "sabic", "entropy", "dbic"), selectize = FALSE),
            "Common"
          ),
          latent_option_item(
            div(
              class = "latent-combined-control latent-fixed-k-control",
              checkboxInput(paste0(module_id, "_fix_best_k"), "Fix best-k manually", value = FALSE),
              numericInput(paste0(module_id, "_fixed_best_k"), "Fixed best k", value = NA, min = 1, step = 1)
            ),
            "Common"
          ),
          latent_option_item(
            selectInput(paste0(module_id, "_model_structure_mode"), "Model structure mode", choices = c("single", "compare"), selected = "single", selectize = FALSE),
            c("LPA", "Mixed"),
            "주로 연속형 지표의 평균/분산/공분산 제약 구조를 비교할 때 사용합니다."
          ),
          latent_option_item(
            selectInput(paste0(module_id, "_model_structure"), "Model structure", choices = c("model1", "model2", "model3", "model4"), selected = "model2", selectize = FALSE),
            c("LPA", "Mixed")
          ),
          latent_option_item(
            checkboxGroupInput(paste0(module_id, "_model_structures"), "Compare structures", choices = c("model1", "model2", "model3", "model4"), selected = "model2", inline = TRUE),
            c("LPA", "Mixed"),
            "compare 모드에서 여러 구조를 한 번에 후보 모델로 실행합니다."
          )
        )
      ),
      div(
        class = "latent-setup-panel",
        id = panel_id("mplus"),
        h4("Mplus Estimation"),
        div(
          class = "latent-options-grid",
          latent_option_item(selectInput(paste0(module_id, "_estimation_preset"), "Estimation preset", choices = latent_estimation_preset_choices(), selected = "test", selectize = FALSE), "Common"),
          latent_option_item(selectInput(paste0(module_id, "_estimator"), "Estimator", choices = c("MLR", "ML", "BAYES"), selected = "MLR", selectize = FALSE), "Common"),
          latent_option_item(textInput(paste0(module_id, "_starts"), "STARTS", value = "100 20"), "Common", "초기/최종 random starts 수입니다. 후보 k가 많거나 해가 불안정하면 늘립니다."),
          latent_option_item(numericInput(paste0(module_id, "_stiterations"), "STITERATIONS", value = 10, min = 0, step = 1), "Common"),
          latent_option_item(textInput(paste0(module_id, "_lrtstarts"), "LRTSTARTS", value = "0 0 50 10"), c("Common", "LMR", "BLRT"), "TECH11/TECH14의 likelihood-ratio test 시작값입니다."),
          latent_option_item(numericInput(paste0(module_id, "_processors"), "Processors / parallel", value = 2, min = 1, step = 1), "Common", "Mplus PROCESSORS 옵션입니다. CPU 코어 수와 라이선스 조건에 맞춰 조정합니다."),
          latent_option_item(numericInput(paste0(module_id, "_bootstrap"), "Bootstrap draws", value = NA, min = 1, step = 1), c("Common", "BLRT"), "BLRT/TECH14 등 bootstrap 기반 검정에서 사용할 반복 횟수입니다. 비워두면 Mplus 기본값을 사용합니다."),
          latent_option_item(checkboxInput(paste0(module_id, "_sampstat"), "SAMPSTAT", value = FALSE), "Common"),
          latent_option_item(checkboxInput(paste0(module_id, "_tech1"), "TECH1", value = FALSE), "Common"),
          latent_option_item(checkboxInput(paste0(module_id, "_tech4"), "TECH4", value = FALSE), "Common"),
          latent_option_item(checkboxInput(paste0(module_id, "_tech8"), "TECH8", value = FALSE), "Common"),
          latent_option_item(checkboxInput(paste0(module_id, "_tech11"), "TECH11 / LMR", value = TRUE), c("Common", "LMR"), "k와 k-1 class 모델을 비교하는 LMR 계열 검정입니다."),
          latent_option_item(checkboxInput(paste0(module_id, "_tech14"), "TECH14 / BLRT", value = TRUE), c("Common", "BLRT"), "부트스트랩 LRT입니다. 시간이 오래 걸릴 수 있어 bootstrap draws와 processors를 같이 봅니다."),
          latent_option_item(checkboxInput(paste0(module_id, "_standardized"), "STANDARDIZED", value = FALSE), "Common")
        )
      ),
      div(
        class = "latent-setup-panel",
        id = panel_id("post"),
        h4("Data and Post-estimation"),
        div(
          class = "latent-options-grid",
          latent_option_item(checkboxInput(paste0(module_id, "_use_display_data"), "Use display data", value = TRUE), "Common"),
          latent_option_item(selectInput(paste0(module_id, "_usevariables_mode"), "USEVARIABLES mode", choices = c("indicators_only", "all_analysis"), selected = "indicators_only", selectize = FALSE), c("Common", "Mixed"), "혼합 지표/보조변수 분석에서는 all_analysis가 필요할 수 있습니다."),
          latent_option_item(numericInput(paste0(module_id, "_min_class_prop"), "Minimum class proportion", value = 0.03, min = 0, max = 1, step = 0.01), "Common"),
          latent_option_item(numericInput(paste0(module_id, "_missing_code"), "Missing code", value = -9999, step = 1), "Common"),
          latent_option_item(numericInput(paste0(module_id, "_mplus_missing_code"), "Mplus missing code", value = -9999, step = 1), "Common"),
          latent_option_item(textInput(paste0(module_id, "_subset_name"), "Subset name", value = ""), "Common"),
          latent_option_item(checkboxInput(paste0(module_id, "_run_r3step"), "R3STEP", value = TRUE), "Common", "covariate role이 있을 때 잠재집단 예측요인 분석에 사용합니다."),
          latent_option_item(numericInput(paste0(module_id, "_reference_class"), "Reference class", value = NA, min = 1, step = 1), "Common"),
          latent_option_item(checkboxInput(paste0(module_id, "_run_bch"), "BCH", value = TRUE), "Common", "outcome role이 있을 때 class별 distal outcome 비교에 사용합니다."),
          latent_option_item(checkboxInput(paste0(module_id, "_bch_moderation"), "BCH moderation", value = FALSE), c("Common", "Mixed"), "moderator role이 있을 때 사용합니다."),
          latent_option_item(checkboxInput(paste0(module_id, "_bch_run_stratified"), "BCH stratified moderation", value = FALSE), c("Common", "Mixed"), "moderator별 stratified BCH 결과를 만들 때 사용합니다.")
        )
      ),
      div(
        class = "latent-setup-panel",
        id = panel_id("output"),
        h4("Table and Figure"),
        div(
          class = "latent-options-grid",
          latent_option_item(numericInput(paste0(module_id, "_p_digits"), "p digits", value = 3, min = 0, step = 1), "Common"),
          latent_option_item(numericInput(paste0(module_id, "_num_digits"), "Number digits", value = 3, min = 0, step = 1), "Common"),
          latent_option_item(numericInput(paste0(module_id, "_percent_digits"), "Percent digits", value = 1, min = 0, step = 1), "Common"),
          latent_option_item(selectInput(paste0(module_id, "_sig_style"), "Significance style", choices = c("sig", "stars", "blank"), selected = "sig", selectize = FALSE), "Common"),
          latent_option_item(selectInput(paste0(module_id, "_journal_style"), "Journal style", choices = c("generic_sci", "elsevier", "springer", "apa"), selected = "generic_sci", selectize = FALSE), "Common"),
          latent_option_item(numericInput(paste0(module_id, "_figure_res"), "Figure resolution", value = 600, min = 72, step = 1), "Common")
        )
      )
    )
  )
}

latent_analysis_choices <- function() {
  c(
    "LCA" = "lca",
    "LPA" = "lpa",
    "Mixed model" = "mixed_model",
    "LTA" = "latent_transition",
    "State transition" = "state_transition",
    "PROCESS macro" = "process_macro"
  )
}

latent_analysis_specs <- function() {
  list(
    lca = list(engine = "cross_sectional_mixture", mixture_type = "LCA"),
    lpa = list(engine = "cross_sectional_mixture", mixture_type = "LPA"),
    mixed_model = list(engine = "cross_sectional_mixture", mixture_type = "AUTO"),
    latent_transition = list(engine = "latent_transition", mixture_type = "LTA"),
    state_transition = list(engine = "state_transition", mixture_type = "STATE"),
    process_macro = list(engine = "process_macro", mixture_type = "PROCESS")
  )
}

latent_role_choices <- function(module_id) {
  switch(
    module_id,
    mixture = c("id", "indicator", "outcome", "covariate", "moderator", "weight", "replicate_weight", "strata", "cluster", "subset"),
    lta = c("id", "time1_indicator", "time2_indicator", "time3_indicator", "covariate", "outcome", "weight", "replicate_weight", "cluster"),
    state_transition = c("id", "from_state", "to_state", "time", "group", "covariate"),
    c("indicator", "covariate", "outcome", "id")
  )
}

latent_run_block <- function(module_id, spec) {
  div(
    class = "latent-results-workspace",
    div(
      class = "workspace-panel latent-main-panel latent-results-panel",
      h3("Results"),
      div(
        class = "latent-results-toolbar",
        actionButton(paste0(module_id, "_refresh_results"), "Reload results", class = "btn btn-default"),
        actionButton(paste0(module_id, "_open_output"), "Open output", class = "btn btn-default"),
        actionButton(paste0(module_id, "_open_excel"), "Open Excel", class = "btn btn-default"),
        actionButton(paste0(module_id, "_view_messages"), "View messages", class = "btn btn-default")
      ),
      div(class = "latent-panel-note", textOutput(paste0(module_id, "_result_status"), inline = TRUE)),
      uiOutput(paste0(module_id, "_run_progress_panel")),
      uiOutput(paste0(module_id, "_result_overview")),
      uiOutput(paste0(module_id, "_sci_result_figures")),
      uiOutput(paste0(module_id, "_mplus_native_figures")),
      uiOutput(paste0(module_id, "_all_result_tables"))
    )
  )
}

sample_dataset_id <- function(module_id) {
  switch(
    module_id,
    mixture = "12_MYJ",
    lta = "13_LTA_DEMO",
    state_transition = "13_LTA_DEMO",
    "dataset"
  )
}

result_library_tab <- function() {
  tabPanel(
    "Result",
    value = "result",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1("Result Library"),
        div("Browse tables, figures, logs, and export bundles generated by the latent pipeline.", class = "app-subtitle")
      ),
      div(
        class = "workspace-panel latent-main-panel",
        h3("Recent Outputs"),
        placeholder_table("result_library")
      )
    )
  )
}

about_tab <- function(version) {
  tabPanel(
    "About",
    value = "about",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1("About"),
        div(paste0("StatEdu Studio Latent Mplus v", version), class = "app-subtitle")
      ),
      div(
        class = "workspace-panel latent-main-panel",
        h3("Design"),
        p("The app is a local GUI shell for the existing Latent R/Mplus project. It will generate data_dictionary.csv and CFG.yml files, run the selected pipeline, and display generated tables and figures.")
      )
    )
  )
}
