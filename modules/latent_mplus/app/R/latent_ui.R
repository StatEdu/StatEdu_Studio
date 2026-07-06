# -*- coding: UTF-8 -*-

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

latent_utf8 <- function(hex) {
  if (exists("statedu_utf8", mode = "function", inherits = TRUE)) {
    return(statedu_utf8(hex))
  }
  pairs <- substring(hex, seq(1, nchar(hex), 2), seq(2, nchar(hex), 2))
  value <- rawToChar(as.raw(strtoi(pairs, 16L)))
  Encoding(value) <- "UTF-8"
  value
}

latent_current_language <- function(language = NULL) {
  language <- as.character(language %||% "")
  if (!nzchar(language)) {
    language <- if (exists("statedu_initial_language", mode = "function", inherits = TRUE)) {
      tryCatch(statedu_initial_language(), error = function(e) "")
    } else {
      ""
    }
  }
  if (exists("normalize_app_language", mode = "function", inherits = TRUE)) {
    return(normalize_app_language(language))
  }
  if (tolower(language) %in% c("ko", "kr", "korean")) "ko" else "en"
}

latent_translation_key <- function(en) {
  value <- tolower(trimws(as.character(en %||% "")))
  value <- gsub("[^a-z0-9]+", "_", value)
  value <- gsub("^_+|_+$", "", value)
  if (!nzchar(value)) {
    return("latent.text")
  }
  paste0("latent.", value)
}

latent_text <- function(en, ko = en, language = NULL) {
  language <- latent_current_language(language)
  fallback <- if (identical(language, "ko")) ko else en
  if (exists("statedu_translate", mode = "function", inherits = TRUE)) {
    return(statedu_translate(latent_translation_key(en), language, fallback))
  }
  fallback
}

latent_choices <- function(values, en, ko = en, language = NULL) {
  language <- latent_current_language(language)
  labels <- mapply(
    function(en_label, ko_label) latent_text(en_label, ko_label, language),
    en,
    ko,
    USE.NAMES = FALSE
  )
  stats::setNames(values, labels)
}

latent_module_label <- function(module_id, field = "menu", language = NULL) {
  labels <- list(
    mixture = list(
      menu = c(en = "Mixture Model", ko = latent_utf8("ec9ea0ec9eaceca791eb8ba820ebb684ec849d")),
      title = c(en = "Latent Mixture Model", ko = latent_utf8("ec9ea0ec9eaceca791eb8ba820ebb684ec849d")),
      subtitle = c(
        en = "LCA, LPA, and mixed-indicator mixture modeling with Mplus.",
        ko = latent_utf8("4d706c757320eab8b0ebb098204c43412c204c50412c20ed98bced95a920eca780ed919c20ec9ea0ec9eaceca791eb8ba820ebaaa8ed9895ec9d8420ec8ba4ed9689ed95a9eb8b88eb8ba42e")
      )
    ),
    lta = list(
      menu = c(en = "LTA", ko = latent_utf8("ec9ea0ec9eaceca084ec9db4ebb684ec849d")),
      title = c(en = "Latent Transition Analysis", ko = latent_utf8("ec9ea0ec9eaceca084ec9db4ebb684ec849d")),
      subtitle = c(en = "Longitudinal latent class transitions across time points.", ko = latent_utf8("ec8b9ceca09020eab08420ec9ea0ec9eaceca791eb8ba820eca084ec9db4eba5bc20ebb684ec849ded95a9eb8b88eb8ba42e"))
    ),
    state_transition = list(
      menu = c(en = "State Transition", ko = latent_utf8("ec8381ed839ceca084ed9998")),
      title = c(en = "State Transition", ko = latent_utf8("ec8381ed839ceca084ed9998")),
      subtitle = c(en = "Observed state transition tables, figures, and summaries.", ko = latent_utf8("eab480ecb8a120ec8381ed839c20eca084ed9998ed919c2c20eab7b8eba6bc2c20ec9a94ec95bdec9d8420ec839dec84b1ed95a9eb8b88eb8ba42e"))
    )
  )
  value <- labels[[module_id]][[field]]
  if (is.null(value)) {
    return(latent_modules[[module_id]][[field]] %||% module_id)
  }
  latent_text(value[["en"]], value[["ko"]], language)
}

latent_save_feature_visible <- function(feature) {
  edition <- tolower(Sys.getenv("STATEDU_EDITION", "development"))
  if (identical(edition, "free") && feature %in% c("excel", "word", "pdf", "add_result", "result_history")) {
    return(FALSE)
  }
  if (exists("analysis_save_feature_visible", mode = "function", inherits = TRUE)) {
    return(isTRUE(analysis_save_feature_visible(feature)))
  }
  TRUE
}

latent_save_button <- function(id, label, feature, class = "btn-default") {
  if (!isTRUE(latent_save_feature_visible(feature))) {
    return(NULL)
  }
  actionButton(id, label, class = paste("btn", class))
}

latent_current_edition <- function() {
  edition <- tolower(Sys.getenv("STATEDU_EDITION", "development"))
  if (!edition %in% c("free", "pro", "development", "personal", "institution")) {
    edition <- "development"
  }
  edition
}

latent_default_figure_res <- function() {
  if (identical(latent_current_edition(), "free")) 300L else 600L
}

latent_figure_res_value <- function(value = NULL) {
  if (identical(latent_current_edition(), "free")) {
    return(300L)
  }
  value <- suppressWarnings(as.integer(value %||% latent_default_figure_res()))
  if (is.na(value) || value <= 0L) {
    value <- latent_default_figure_res()
  }
  value
}

latent_figure_output_res <- function() {
  if (identical(latent_current_edition(), "free")) {
    return(300L)
  }
  c(300L, 600L)
}

latent_home_tab <- function(language = latent_current_language()) {
  tabPanel(
    latent_text("Home", latent_utf8("ed9988"), language),
    value = "home",
    div(
      class = "page-shell latent-home",
      div(
        class = "app-heading",
        h1("StatEdu Studio Latent Mplus"),
        div(latent_text("A local interface for dictionary-driven Mplus latent analysis pipelines.", latent_utf8("4d706c757320eab8b0ebb09820ec9ea0ec9eacebb684ec849d20ed8c8cec9db4ed9484eb9dbcec9db8ec9d8420ec8ba4ed9689ed9598eb8a9420eba19cecbbac20ec9db8ed84b0ed8e98ec9db4ec8aa4ec9e85eb8b88eb8ba42e"), language), class = "app-subtitle")
      ),
      div(
        class = "latent-dashboard-grid",
        metric_tile(latent_text("Workflow", latent_utf8("ec9e91ec978520ed9d90eba684"), language), "1-2-3", latent_text("Data, setup, run and results", latent_utf8("eb8db0ec9db4ed84b02c20ec84a4eca0952c20ec8ba4ed968920ebb08f20eab2b0eab3bc"), language)),
        metric_tile(latent_text("Engine", latent_utf8("ec9794eca784"), language), latent_default_project_root(), latent_text("Bundled R/Mplus pipeline", latent_utf8("eb82b4ec9ea520522f4d706c757320ed8c8cec9db4ed9484eb9dbcec9db8"), language)),
        metric_tile(latent_text("Scope", latent_utf8("ebb294ec9c84"), language), "LCA / LPA", latent_text("Mixed-indicator mixture included", latent_utf8("ed98bced95a920eca780ed919c20ebaaa8ed989520ed8faced95a8"), language))
      ),
      div(
        class = "workspace-panel latent-overview-panel",
        h3(latent_text("Project Shell", latent_utf8("ed9484eba19ceca09ded8ab820ec8ba4ed968920ed9998eab2bd"), language)),
        p(latent_text("This UI shell reuses the StatEdu Studio block workflow and runs the bundled Latent pipeline through dictionary and CFG.yml files.", latent_utf8("537461744564752053747564696fec9d9820ebb894eba19ded989520ec9e91ec978520ed9d90eba684ec9790ec849c20eb8db0ec9db4ed84b020ec82aceca084eab3bc204346472e796d6c20ec84a4eca095ec9d8420ec9db4ec9aa9ed95b420ec9ea0ec9eacebb684ec849d20ed8c8cec9db4ed9484eb9dbcec9db8ec9d8420ec8ba4ed9689ed95a9eb8b88eb8ba42e"), language)),
        div(
          class = "latent-home-actions",
          actionButton("home_open_mixture", latent_text("Start Mixture Model", latent_utf8("ec9ea0ec9eaceca791eb8ba820ebb684ec849d20ec8b9cec9e91"), language), class = "btn btn-primary")
        )
      )
    )
  )
}

latent_menu_tab <- function(language = latent_current_language()) {
  navbarMenu(
    latent_text("Latent", latent_utf8("ec9ea0ec9eacebb684ec849d"), language),
    latent_analysis_tab("mixture", language)
  )
}

latent_analysis_tab <- function(module_id, language = latent_current_language()) {
  spec <- latent_modules[[module_id]]
  tabPanel(
    latent_module_label(module_id, "menu", language),
    value = paste0("latent_", module_id),
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(latent_module_label(module_id, "title", language)),
        div(latent_module_label(module_id, "subtitle", language), class = "app-subtitle")
      ),
      latent_workflow(module_id, spec, language)
    )
  )
}

latent_workflow <- function(module_id, spec, language = latent_current_language()) {
  ns <- paste0("latent_", module_id)
  div(
    class = "latent-workflow",
    div(
      class = "latent-step-tabs",
      tags$button(type = "button", class = "latent-step-tab active", `data-target` = paste0(ns, "_data"), latent_text("1 Data", latent_utf8("3120eb8db0ec9db4ed84b0"), language)),
      tags$button(type = "button", class = "latent-step-tab", `data-target` = paste0(ns, "_setup"), latent_text("2 Setup", latent_utf8("3220ec84a4eca095"), language)),
      tags$button(type = "button", class = "latent-step-tab", `data-target` = paste0(ns, "_run"), latent_text("3 Results", latent_utf8("3320eab2b0eab3bc"), language))
    ),
    div(
      id = paste0(ns, "_data"),
      class = "latent-step-panel active",
      latent_data_block(module_id, spec, language)
    ),
    div(
      id = paste0(ns, "_setup"),
      class = "latent-step-panel",
      latent_setup_block(module_id, spec, language)
    ),
    div(
      id = paste0(ns, "_run"),
      class = "latent-step-panel",
      latent_run_block(module_id, spec, language)
    )
  )
}

latent_data_block <- function(module_id, spec, language = latent_current_language()) {
  div(
    class = "latent-block-grid",
    div(
      class = "workspace-panel latent-control-panel",
      h3(latent_text("Data", latent_utf8("eb8db0ec9db4ed84b0"), language)),
      textInput(paste0(module_id, "_dataset_id"), latent_text("Dataset ID", latent_utf8("eb8db0ec9db4ed84b0ec858b204944"), language), value = ""),
      textInput(paste0(module_id, "_project_root"), latent_text("Latent project root", latent_utf8("ec9ea0ec9eacebb684ec849d20ed9484eba19ceca09ded8ab820ed8fb4eb8d94"), language), value = latent_default_project_root()),
      div(class = "latent-panel-note", textOutput(paste0(module_id, "_dataset_id_message"), inline = TRUE)),
      div(
        class = "latent-button-row latent-data-action-grid",
        actionButton(paste0(module_id, "_load_yaml_data"), latent_text("Load settings", latent_utf8("ec84a4eca09520ebb688eb9facec98a4eab8b0"), language), class = "btn btn-default"),
        actionButton(paste0(module_id, "_save_yaml"), latent_text("Save settings", latent_utf8("ec84a4eca09520eca080ec9ea5"), language), class = "btn btn-primary")
      ),
      hr(),
      h3(latent_text("Roles", latent_utf8("ebb380ec889820ec97aded95a0"), language)),
      selectInput(
        paste0(module_id, "_active_role"),
        latent_text("Active role", latent_utf8("ed9884ec9eac20eca780eca095ed95a020ec97aded95a0"), language),
        choices = latent_role_choices(module_id, language),
        selectize = FALSE
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'subset'", paste0(module_id, "_active_role")),
        div(
          class = "latent-subset-condition-panel",
          selectInput(
            paste0(module_id, "_subset_condition_mode"),
            latent_text("Subset condition", latent_utf8("ebb684ec849d20eb8c80ec8381eca791eb8ba820eca1b0eab1b4"), language),
            choices = latent_choices(
              c("equals", "not_missing", "expr"),
              c("Equals value", "Not missing", "Custom expression"),
              c(latent_utf8("eab09220ec9dbcecb998"), latent_utf8("eab2b0ecb8a120ec9584eb8b98"), latent_utf8("ec82acec9aa9ec9e9020ec8b9d")),
              language
            ),
            selected = "equals",
            selectize = FALSE
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'equals'", paste0(module_id, "_subset_condition_mode")),
            textInput(paste0(module_id, "_subset_value"), latent_text("Value", latent_utf8("eab092"), language), value = "")
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'expr'", paste0(module_id, "_subset_condition_mode")),
            textInput(paste0(module_id, "_subset_expr"), latent_text("Expression", latent_utf8("eca1b0eab1b4ec8b9d"), language), value = "", placeholder = "target == 1")
          )
        )
      ),
      div(class = "latent-panel-note", latent_text("Check variables in the Variables table to assign the active role, or use the Role dropdown in each row. Use the table length menu to change how many variables are shown.", latent_utf8("ebb380ec889820ed919cec9790ec849c20ecb2b4ed81aced9598eba9b420ed9884ec9eac20ec97aded95a0ec9db420eca780eca095eb90a9eb8b88eb8ba42e20eab08120ed9689ec9d9820ec97aded95a020eb939ceba1adeb8ba4ec9ab4ec9cbceba19ceb8f8420eca780eca095ed95a020ec889820ec9e88ec8ab5eb8b88eb8ba42e20ed919cec9d9820ed919cec8b9c20eab09cec889820eba994eb89b4ec9790ec849c20ed959c20ebb288ec979020ebb3bc20ebb380ec889820ec8898eba5bc20eca1b0eca095ed9598ec84b8ec9a942e"), language)),
      div(
        class = "latent-button-row",
        actionButton(paste0(module_id, "_clear_active_role"), latent_text("Clear active role", latent_utf8("ed9884ec9eac20ec97aded95a020ebb984ec9ab0eab8b0"), language), class = "btn btn-default"),
        actionButton(paste0(module_id, "_clear_all_roles"), latent_text("Clear roles", latent_utf8("ebaaa8eb93a020ec97aded95a020ebb984ec9ab0eab8b0"), language), class = "btn btn-default")
      ),
      placeholder_table(paste0(module_id, "_role_summary"))
    ),
    div(
      class = "workspace-panel latent-main-panel latent-variable-role-panel",
      h3(latent_text("Variables", latent_utf8("ebb380ec8898"), language)),
      div(class = "latent-panel-note", latent_text("Choose a latent-analysis role for each variable.", latent_utf8("eab08120ebb380ec8898ec979020ec9ea0ec9eacebb684ec849d20ec97aded95a0ec9d8420eca780eca095ed95a9eb8b88eb8ba42e"), language)),
      placeholder_table(paste0(module_id, "_variable_preview"))
    )
  )
}

latent_scope_badges <- function(scopes, language = latent_current_language()) {
  scopes <- as.character(scopes)
  tags$div(
    class = "latent-option-scope",
    lapply(scopes, function(scope) {
      scope_key <- tolower(gsub("[^a-z0-9]+", "-", scope))
      scope_label <- if (identical(scope, "Common")) latent_text("Common", latent_utf8("eab3b5ed86b5"), language) else scope
      tags$span(class = paste("latent-analysis-badge", paste0("scope-", scope_key)), scope_label)
    })
  )
}

latent_option_item <- function(control, scopes = "Common", note = NULL, language = latent_current_language()) {
  div(
    class = "latent-option-card",
    latent_scope_badges(scopes, language),
    control
  )
}

latent_estimation_preset_choices <- function(language = latent_current_language()) {
  latent_choices(
    c("test", "standard_pc", "desktop_9950x3d", "custom"),
    c("Quick check", "Standard PC", "High-performance PC", "Custom"),
    c(latent_utf8("ebb9a0eba5b820ed9995ec9db8"), latent_utf8("ec9dbcebb098205043"), latent_utf8("eab3a0ec84b1eb8aa5205043"), latent_utf8("ec82acec9aa9ec9e9020ec84a4eca095")),
    language
  )
}

latent_run_pipeline_controls <- function(module_id, language = latent_current_language()) {
  div(
    class = "latent-run-footer",
    div(
      class = "latent-run-controls-grid",
      div(
        class = "latent-run-field",
        selectInput(
          paste0(module_id, "_from_step"),
          latent_text("From step", latent_utf8("ec8b9cec9e9120eb8ba8eab384"), language),
          choices = c("settings", "prep", "estimation_build", "estimation_run", "estimation_collect", "select_best_k", "classify", "r3step", "bch", "bch_moderation", "tables", "figures", "export_docx", "finalize")
        )
      ),
      div(
        class = "latent-run-field",
        selectInput(
          paste0(module_id, "_to_step"),
          latent_text("To step", latent_utf8("eca285eba38c20eb8ba8eab384"), language),
          choices = c("settings", "prep", "estimation_build", "estimation_run", "estimation_collect", "select_best_k", "classify", "r3step", "bch", "bch_moderation", "tables", "figures", "export_docx", "finalize"),
          selected = "finalize"
        )
      )
    ),
    div(
      class = "latent-run-action-row",
      actionButton(paste0(module_id, "_run_pipeline"), latent_text("Run Mplus analysis", latent_utf8("4d706c757320ebb684ec849d20ec8ba4ed9689"), language), class = "btn btn-primary")
    )
  )
}

latent_setup_block <- function(module_id, spec, language = latent_current_language()) {
  panel_id <- function(suffix) paste0("latent_", module_id, "_setup_", suffix)
  div(
    class = "latent-block-grid latent-setup-workspace",
    div(
      class = "workspace-panel latent-control-panel",
      h3(latent_text("Setup", latent_utf8("ec84a4eca095"), language)),
      div(
        class = "latent-setup-topic-list",
        tags$button(type = "button", class = "latent-setup-topic active", `data-target` = panel_id("model"), latent_text("Model Selection", latent_utf8("ebaaa8ed989520ec84a0ed839d"), language)),
        tags$button(type = "button", class = "latent-setup-topic", `data-target` = panel_id("mplus"), latent_text("Mplus Estimation", latent_utf8("4d706c757320ecb694eca095"), language)),
        tags$button(type = "button", class = "latent-setup-topic", `data-target` = panel_id("post"), latent_text("Data / Post-estimation", latent_utf8("ec9e90eba38c202f20ec82aced9b84ebb684ec849d"), language)),
        tags$button(type = "button", class = "latent-setup-topic", `data-target` = panel_id("output"), latent_text("Table / Figure", latent_utf8("ed919c202f20eab7b8eba6bc"), language))
      ),
      hr(),
      h3(latent_text("Run Analysis", latent_utf8("ebb684ec849d20ec8ba4ed9689"), language)),
      latent_run_pipeline_controls(module_id, language)
    ),
    div(
      class = "workspace-panel latent-main-panel",
      h3(latent_text("Analysis Options", latent_utf8("ebb684ec849d20ec98b5ec8598"), language)),
      div(
        class = "latent-setup-panel active",
        id = panel_id("model"),
        h4(latent_text("Model Selection", latent_utf8("ebaaa8ed989520ec84a0ed839d"), language)),
        div(
          class = "latent-options-grid",
          latent_option_item(
            selectInput(
              paste0(module_id, "_analysis_id"),
              latent_text("Analysis ID", latent_utf8("ebb684ec849d204944"), language),
              choices = latent_analysis_choices(language),
              selected = spec$analysis_key,
              selectize = FALSE
            ),
            "Common",
            latent_utf8("4c43412c204c50412c206d697865642d696e64696361746f72206d697874757265206d6f64656cec9d8420eab099ec9d8020eba994eb89b4ec9790ec849c20ec84a0ed839ded95a9eb8b88eb8ba42e")
          ),
          latent_option_item(
            textInput(paste0(module_id, "_mixture_type"), latent_text("Mixture type", latent_utf8("ebb684ec849d20ec9ca0ed9895"), language), value = spec$mixture_type),
            c("LCA", "LPA", "Mixed"),
            latent_utf8("4155544feb8a9420eca780ed919c20ec9ca0ed9895ec9d8420ebb3b4eab3a0204c43412f4c504120eb9890eb8a9420ed98bced95a920eca780ed919c20ebb684ec849dec9cbceba19c20ed8c90eb8ba8ed95a9eb8b88eb8ba42e")
          ),
          latent_option_item(
            selectInput(paste0(module_id, "_indicator_type"), latent_text("Indicator type", latent_utf8("eca780ed919c20ec9ca0ed9895"), language), choices = latent_choices(c("auto", "categorical", "continuous"), c("Auto", "Categorical", "Continuous"), c(latent_utf8("ec9e90eb8f99"), latent_utf8("ebb294eca3bced9895"), latent_utf8("ec97b0ec868ded9895")), language), selected = "auto", selectize = FALSE),
            c("LCA", "LPA", "Mixed"),
            latent_utf8("63617465676f726963616c3d4c43412c20636f6e74696e756f75733d4c50412c206175746f3d6d6978656420eab080eb8aa5ec84b1ec9d8420ed8faced95a8ed95b420ec9e90eb8f9920ed8c90eca095ed95a9eb8b88eb8ba42e")
          ),
          latent_option_item(
            numericInput(paste0(module_id, "_seed"), latent_text("Seed", latent_utf8("eb829cec889820ec8b9ceb939c"), language), value = 20260331, min = 1, step = 1),
            "Common"
          ),
          latent_option_item(
            numericInput(paste0(module_id, "_k_min"), latent_text("k min", latent_utf8("ecb59cec868c20eca791eb8ba820ec8898"), language), value = 2, min = 1, step = 1),
            "Common"
          ),
          latent_option_item(
            numericInput(paste0(module_id, "_k_max"), latent_text("k max", latent_utf8("ecb59ceb8c8020eca791eb8ba820ec8898"), language), value = 6, min = 1, step = 1),
            "Common"
          ),
          latent_option_item(
            textInput(paste0(module_id, "_k_values"), latent_text("k values", latent_utf8("ebb684ec849ded95a020eca791eb8ba820ec8898"), language), value = "", placeholder = "2,3,4,5"),
            "Common",
            latent_utf8("ebb984ec9b8ceb9190eba9b4206b206d696eebb680ed84b0206b206d6178eab98ceca78020ec8ba4ed9689ed95a9eb8b88eb8ba42e")
          ),
          latent_option_item(
            selectInput(paste0(module_id, "_best_k_rule"), latent_text("Best-k rule", latent_utf8("ecb59ceca08120eca791eb8ba820ec84a0ed839d20eab8b0eca480"), language), choices = c("hybrid", "bic", "aic", "sabic", "entropy", "dbic"), selectize = FALSE),
            "Common"
          ),
          latent_option_item(
            div(
              class = "latent-combined-control latent-fixed-k-control",
              checkboxInput(paste0(module_id, "_fix_best_k"), latent_text("Fix best-k manually", latent_utf8("ecb59ceca08120eca791eb8ba820ec889820eca781eca09120eca780eca095"), language), value = FALSE),
              numericInput(paste0(module_id, "_fixed_best_k"), latent_text("Fixed best k", latent_utf8("eca780eca09520eca791eb8ba820ec8898"), language), value = NA, min = 1, step = 1)
            ),
            "Common"
          ),
          latent_option_item(
            selectInput(paste0(module_id, "_model_structure_mode"), latent_text("Model structure mode", latent_utf8("ebaaa8ed989520eab5aceca1b020ebb0a9ec8b9d"), language), choices = latent_choices(c("single", "compare"), c("Single", "Compare"), c(latent_utf8("eb8ba8ec9dbc"), latent_utf8("ebb984eab590")), language), selected = "single", selectize = FALSE),
            c("LPA", "Mixed"),
            latent_utf8("eca3bceba19c20ec97b0ec868ded989520eca780ed919cec9d9820ed8f89eab7a02febb684ec82b02feab3b5ebb684ec82b020eca09cec95bd20eab5aceca1b0eba5bc20ebb984eab590ed95a020eb958c20ec82acec9aa9ed95a9eb8b88eb8ba42e")
          ),
          latent_option_item(
            selectInput(paste0(module_id, "_model_structure"), latent_text("Model structure", latent_utf8("ebaaa8ed989520eab5aceca1b0"), language), choices = c("model1", "model2", "model3", "model4"), selected = "model2", selectize = FALSE),
            c("LPA", "Mixed")
          ),
          latent_option_item(
            checkboxGroupInput(paste0(module_id, "_model_structures"), latent_text("Compare structures", latent_utf8("ebb984eab590ed95a020eab5aceca1b0"), language), choices = c("model1", "model2", "model3", "model4"), selected = "model2", inline = TRUE),
            c("LPA", "Mixed"),
            latent_utf8("636f6d7061726520ebaaa8eb939cec9790ec849c20ec97aceb9fac20eab5aceca1b0eba5bc20ed959c20ebb288ec979020ed9b84ebb3b420ebaaa8eb8db8eba19c20ec8ba4ed9689ed95a9eb8b88eb8ba42e")
          )
        )
      ),
      div(
        class = "latent-setup-panel",
        id = panel_id("mplus"),
        h4(latent_text("Mplus Estimation", latent_utf8("4d706c757320ecb694eca095"), language)),
        div(
          class = "latent-options-grid",
          latent_option_item(selectInput(paste0(module_id, "_estimation_preset"), latent_text("Estimation preset", latent_utf8("ecb694eca09520ed9484eba6acec858b"), language), choices = latent_estimation_preset_choices(language), selected = "test", selectize = FALSE), "Common", language = language),
          latent_option_item(selectInput(paste0(module_id, "_estimator"), latent_text("Estimator", latent_utf8("ecb694eca09520ebb0a9ebb295"), language), choices = c("MLR", "ML", "BAYES"), selected = "MLR", selectize = FALSE), "Common", language = language),
          latent_option_item(textInput(paste0(module_id, "_starts"), "STARTS", value = "100 20"), "Common", latent_utf8("ecb488eab8b02fecb59ceca2852072616e646f6d2073746172747320ec8898ec9e85eb8b88eb8ba42e20ed9b84ebb3b4206beab08020eba78eeab1b0eb829820ed95b4eab08020ebb688ec9588eca095ed9598eba9b420eb8a98eba6bdeb8b88eb8ba42e")),
          latent_option_item(numericInput(paste0(module_id, "_stiterations"), "STITERATIONS", value = 10, min = 0, step = 1), "Common"),
          latent_option_item(textInput(paste0(module_id, "_lrtstarts"), "LRTSTARTS", value = "0 0 50 10"), c("Common", "LMR", "BLRT"), latent_utf8("5445434831312f544543483134ec9d98206c696b656c69686f6f642d726174696f207465737420ec8b9cec9e91eab092ec9e85eb8b88eb8ba42e")),
          latent_option_item(numericInput(paste0(module_id, "_processors"), latent_text("Processors / parallel", latent_utf8("ed9484eba19cec84b8ec849c202f20ebb391eba0ac20ec8ba4ed9689"), language), value = 2, min = 1, step = 1), "Common", latent_utf8("4d706c75732050524f434553534f525320ec98b5ec8598ec9e85eb8b88eb8ba42e2043505520ecbd94ec96b420ec8898ec998020eb9dbcec9db4ec84a0ec8aa420eca1b0eab1b4ec979020eba79eecb6b020eca1b0eca095ed95a9eb8b88eb8ba42e"), language = language),
          latent_option_item(numericInput(paste0(module_id, "_bootstrap"), latent_text("Bootstrap draws", latent_utf8("ebb680ed8ab8ec8aa4ed8ab8eb9ea920ebb098ebb3b520ec8898"), language), value = NA, min = 1, step = 1), c("Common", "BLRT"), latent_utf8("424c52542f54454348313420eb93b120626f6f74737472617020eab8b0ebb09820eab280eca095ec9790ec849c20ec82acec9aa9ed95a020ebb098ebb3b520ed9a9fec8898ec9e85eb8b88eb8ba42e20ebb984ec9b8ceb9190eba9b4204d706c757320eab8b0ebb3b8eab092ec9d8420ec82acec9aa9ed95a9eb8b88eb8ba42e"), language = language),
          latent_option_item(checkboxInput(paste0(module_id, "_sampstat"), latent_text("SAMPSTAT (sample statistics)", latent_utf8("53414d50535441542028eab8b0ecb48820ed86b5eab384eb9f8929"), language), value = FALSE), "Common", language = language),
          latent_option_item(checkboxInput(paste0(module_id, "_tech1"), latent_text("TECH1 (parameter setup)", latent_utf8("54454348312028ebaaa8ec889820ec84a4eca09529"), language), value = TRUE), "Common", language = language),
          latent_option_item(checkboxInput(paste0(module_id, "_tech4"), latent_text("TECH4 (latent means/covariances)", latent_utf8("54454348342028ec9ea0ec9eacebb380ec889820ed8f89eab7a02feab3b5ebb684ec82b029"), language), value = TRUE), "Common", language = language),
          latent_option_item(checkboxInput(paste0(module_id, "_tech8"), latent_text("TECH8 (iteration history)", latent_utf8("54454348382028ebb098ebb3b520ecb694eca09520eab3bceca09529"), language), value = TRUE), "Common", language = language),
          latent_option_item(checkboxInput(paste0(module_id, "_tech11"), latent_text("TECH11 / LMR (k-1 class test)", latent_utf8("544543483131202f204c4d5220286b2d3120eca791eb8ba820ebb984eab59029"), language), value = TRUE), c("Common", "LMR"), latent_utf8("6bec9980206b2d3120636c61737320ebaaa8eb8db8ec9d8420ebb984eab590ed9598eb8a94204c4d5220eab384ec97b420eab280eca095ec9e85eb8b88eb8ba42e"), language = language),
          latent_option_item(checkboxInput(paste0(module_id, "_tech14"), latent_text("TECH14 / BLRT (bootstrap class test)", latent_utf8("544543483134202f20424c52542028ebb680ed8ab8ec8aa4ed8ab8eb9ea920ebaaa8ed989520ebb984eab59029"), language), value = TRUE), c("Common", "BLRT"), latent_utf8("ebb680ed8ab8ec8aa4ed8ab8eb9ea9204c5254ec9e85eb8b88eb8ba42e20ec8b9ceab084ec9db420ec98a4eb9e9820eab1b8eba6b420ec889820ec9e88ec96b420626f6f747374726170206472617773ec99802070726f636573736f7273eba5bc20eab099ec9db420ebb485eb8b88eb8ba42e"), language = language),
          latent_option_item(checkboxInput(paste0(module_id, "_standardized"), latent_text("STANDARDIZED (standardized estimates)", latent_utf8("5354414e44415244495a45442028ed919ceca480ed999420ecb694eca095ecb99829"), language), value = FALSE), "Common", language = language)
        )
      ),
      div(
        class = "latent-setup-panel",
        id = panel_id("post"),
        h4(latent_text("Data and Post-estimation", latent_utf8("ec9e90eba38c20ebb08f20ec82aced9b84ebb684ec849d"), language)),
        div(
          class = "latent-options-grid",
          latent_option_item(checkboxInput(paste0(module_id, "_use_display_data"), latent_text("Use display data", latent_utf8("ed9884ec9eac20ed919cec8b9c20eb8db0ec9db4ed84b020ec82acec9aa9"), language), value = TRUE), "Common", language = language),
          latent_option_item(selectInput(paste0(module_id, "_usevariables_mode"), latent_text("USEVARIABLES mode", latent_utf8("5553455641524941424c455320ebb0a9ec8b9d"), language), choices = latent_choices(c("indicators_only", "all_analysis"), c("Indicators only", "All analysis variables"), c(latent_utf8("eca780ed919c20ebb380ec8898eba78c"), latent_utf8("ebb684ec849d20ebb380ec889820eca084ecb2b4")), language), selected = "indicators_only", selectize = FALSE), c("Common", "Mixed"), latent_utf8("ed98bced95a920eca780ed919c2febb3b4eca1b0ebb380ec889820ebb684ec849dec9790ec849ceb8a9420616c6c5f616e616c79736973eab08020ed9584ec9a94ed95a020ec889820ec9e88ec8ab5eb8b88eb8ba42e"), language = language),
          latent_option_item(numericInput(paste0(module_id, "_min_class_prop"), latent_text("Minimum class proportion", latent_utf8("ecb59cec868c20eca791eb8ba820ebb984ec9ca8"), language), value = 0.03, min = 0, max = 1, step = 0.01), "Common", language = language),
          latent_option_item(numericInput(paste0(module_id, "_missing_code"), latent_text("Missing code", latent_utf8("eab2b0ecb8a120ecbd94eb939c"), language), value = -9999, step = 1), "Common", language = language),
          latent_option_item(numericInput(paste0(module_id, "_mplus_missing_code"), latent_text("Mplus missing code", latent_utf8("4d706c757320eab2b0ecb8a120ecbd94eb939c"), language), value = -9999, step = 1), "Common", language = language),
          latent_option_item(textInput(paste0(module_id, "_subset_name"), latent_text("Subset name", latent_utf8("ebb684ec849d20eb8c80ec8381eca791eb8ba820ec9db4eba684"), language), value = ""), "Common", language = language),
          latent_option_item(checkboxInput(paste0(module_id, "_run_r3step"), latent_text("R3STEP (covariate prediction)", latent_utf8("5233535445502028eab3b5ebb380eb9f8920ec9888ecb8a129"), language), value = TRUE), "Common", latent_utf8("636f7661726961746520726f6c65ec9db420ec9e88ec9d8420eb958c20ec9ea0ec9eaceca791eb8ba820ec9888ecb8a1ec9a94ec9db820ebb684ec849dec979020ec82acec9aa9ed95a9eb8b88eb8ba42e"), language = language),
          latent_option_item(numericInput(paste0(module_id, "_reference_class"), latent_text("Reference class", latent_utf8("eab8b0eca48020eca791eb8ba8"), language), value = NA, min = 1, step = 1), "Common", language = language),
          latent_option_item(checkboxInput(paste0(module_id, "_run_bch"), latent_text("BCH (outcome comparison)", latent_utf8("4243482028eab2b0eab3bcebb380ec889820ebb984eab59029"), language), value = TRUE), "Common", latent_utf8("6f7574636f6d6520726f6c65ec9db420ec9e88ec9d8420eb958c20636c617373ebb3842064697374616c206f7574636f6d6520ebb984eab590ec979020ec82acec9aa9ed95a9eb8b88eb8ba42e"), language = language),
          latent_option_item(checkboxInput(paste0(module_id, "_bch_moderation"), latent_text("BCH moderation (moderation analysis)", latent_utf8("42434820eca1b0eca088ed9aa8eab3bc2028eca1b0eca088ed9aa8eab3bc20ebb684ec849d29"), language), value = FALSE), c("Common", "Mixed"), latent_utf8("6d6f64657261746f7220726f6c65ec9db420ec9e88ec9d8420eb958c20ec82acec9aa9ed95a9eb8b88eb8ba42e"), language = language),
          latent_option_item(checkboxInput(paste0(module_id, "_bch_run_stratified"), latent_text("BCH stratified moderation (group-specific moderation)", latent_utf8("42434820ecb8b5ed999420eca1b0eca088ed9aa8eab3bc2028eca791eb8ba8ebb38420eca1b0eca088ed9aa8eab3bc29"), language), value = FALSE), c("Common", "Mixed"), latent_utf8("6d6f64657261746f72ebb38420737472617469666965642042434820eab2b0eab3bceba5bc20eba78ceb93a420eb958c20ec82acec9aa9ed95a9eb8b88eb8ba42e"), language = language)
        )
      ),
      div(
        class = "latent-setup-panel",
        id = panel_id("output"),
        h4(latent_text("Table and Figure", latent_utf8("ed919c20ebb08f20eab7b8eba6bc"), language)),
        div(
          class = "latent-options-grid",
          latent_option_item(numericInput(paste0(module_id, "_p_digits"), latent_text("p digits", latent_utf8("70eab09220ec868cec8898eca090"), language), value = 3, min = 0, step = 1), "Common", language = language),
          latent_option_item(numericInput(paste0(module_id, "_num_digits"), latent_text("Number digits", latent_utf8("ec88abec9e9020ec868cec8898eca090"), language), value = 3, min = 0, step = 1), "Common", language = language),
          latent_option_item(numericInput(paste0(module_id, "_percent_digits"), latent_text("Percent digits", latent_utf8("ebb0b1ebb684ec9ca820ec868cec8898eca090"), language), value = 1, min = 0, step = 1), "Common", language = language),
          latent_option_item(selectInput(paste0(module_id, "_sig_style"), latent_text("Significance style", latent_utf8("ec9ca0ec9d98ec84b120ed919cec8b9c"), language), choices = latent_choices(c("sig", "stars", "blank"), c("sig", "stars", "blank"), c("sig", latent_utf8("ebb384ed919c"), latent_utf8("ebb988ecb9b8")), language), selected = "sig", selectize = FALSE), "Common", language = language),
          latent_option_item(selectInput(paste0(module_id, "_journal_style"), latent_text("Journal style", latent_utf8("ed9599ec88a0eca78020ed9895ec8b9d"), language), choices = c("generic_sci", "elsevier", "springer", "apa"), selected = "generic_sci", selectize = FALSE), "Common", language = language),
          latent_option_item(numericInput(paste0(module_id, "_figure_res"), latent_text("Figure resolution", latent_utf8("eab7b8eba6bc20ed95b4ec8381eb8f84"), language), value = latent_default_figure_res(), min = 72, step = 1), "Common", language = language)
        )
      )
    )
  )
}

latent_analysis_choices <- function(language = latent_current_language()) {
  latent_choices(
    c("lca", "lpa", "mixed_model"),
    c("LCA", "LPA", "Mixed model"),
    c("LCA", "LPA", latent_utf8("ed98bced95a920eca780ed919c20ebaaa8ed9895")),
    language
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

latent_role_choices <- function(module_id, language = latent_current_language()) {
  roles <- switch(
    module_id,
    mixture = c("id", "indicator", "outcome", "covariate", "moderator", "weight", "replicate_weight", "strata", "cluster", "subset"),
    lta = c("id", "time1_indicator", "time2_indicator", "time3_indicator", "covariate", "outcome", "weight", "replicate_weight", "cluster"),
    state_transition = c("id", "from_state", "to_state", "time", "group", "covariate"),
    c("indicator", "covariate", "outcome", "id")
  )
  role_labels_ko <- c(
    id = "ID",
    indicator = latent_utf8("eca780ed919c20ebb380ec8898"),
    outcome = latent_utf8("eab2b0eab3bc20ebb380ec8898"),
    covariate = latent_utf8("eab3b5ebb380eb9f89"),
    moderator = latent_utf8("eca1b0eca08820ebb380ec8898"),
    weight = latent_utf8("eab080eca491ecb998"),
    replicate_weight = latent_utf8("ebb098ebb3b520eab080eca491ecb998"),
    strata = latent_utf8("ecb8b5ed999420ebb380ec8898"),
    cluster = latent_utf8("eca791eb9dbd20ebb380ec8898"),
    subset = latent_utf8("ebb684ec849d20eb8c80ec8381eca791eb8ba8"),
    time1_indicator = latent_utf8("31ec8b9ceca09020eca780ed919c"),
    time2_indicator = latent_utf8("32ec8b9ceca09020eca780ed919c"),
    time3_indicator = latent_utf8("33ec8b9ceca09020eca780ed919c"),
    from_state = latent_utf8("ec9db4eca08420ec8381ed839c"),
    to_state = latent_utf8("ec9db4ed9b8420ec8381ed839c"),
    time = latent_utf8("ec8b9ceab084"),
    group = latent_utf8("eca791eb8ba8")
  )
  role_labels_en <- c(
    id = "ID",
    indicator = "Indicator variable",
    outcome = "Outcome variable",
    covariate = "Covariate",
    moderator = "Moderator variable",
    weight = "Weight",
    replicate_weight = "Replicate weight",
    strata = "Strata variable",
    cluster = "Cluster variable",
    subset = "Analysis subset",
    time1_indicator = "Time 1 indicator",
    time2_indicator = "Time 2 indicator",
    time3_indicator = "Time 3 indicator",
    from_state = "From state",
    to_state = "To state",
    time = "Time",
    group = "Group"
  )
  labels <- mapply(
    function(en_label, ko_label, role) {
      if (is.na(en_label) || !nzchar(en_label)) en_label <- role
      if (is.na(ko_label) || !nzchar(ko_label)) ko_label <- en_label
      latent_text(en_label, ko_label, language)
    },
    unname(role_labels_en[roles]),
    unname(role_labels_ko[roles]),
    roles,
    USE.NAMES = FALSE
  )
  stats::setNames(roles, labels)
}

latent_run_block <- function(module_id, spec, language = latent_current_language()) {
  div(
    class = "latent-results-workspace",
    div(
      class = "workspace-panel latent-main-panel latent-results-panel",
      h3(latent_text("Results", latent_utf8("eab2b0eab3bc"), language)),
      div(
        class = "latent-results-toolbar",
        actionButton(paste0(module_id, "_refresh_results"), latent_text("Reload results", latent_utf8("eab2b0eab3bc20ec8388eba19ceab3a0ecb9a8"), language), class = "btn btn-default"),
        actionButton(paste0(module_id, "_open_output"), latent_text("Open output", latent_utf8("ecb69ceba0a520ed8fb4eb8d9420ec97b4eab8b0"), language), class = "btn btn-default"),
        latent_save_button(paste0(module_id, "_open_excel"), latent_text("Open Excel", latent_utf8("457863656c20ec97b4eab8b0"), language), "excel"),
        actionButton(paste0(module_id, "_view_messages"), latent_text("View messages", latent_utf8("eba994ec8b9ceca78020ebb3b4eab8b0"), language), class = "btn btn-default")
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
