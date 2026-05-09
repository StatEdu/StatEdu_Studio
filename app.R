required_packages <- c("shiny", "DT", "lmtest", "sandwich", "nortest", "boot", "readxl", "jsonlite", "haven", "readr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Install required packages first: install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))"
  )
}

library(shiny)
library(DT)
library(lmtest)
library(sandwich)
library(nortest)
library(boot)
library(readxl)
library(jsonlite)

app_version <- trimws(readLines("VERSION", warn = FALSE)[1])
dw_table_path <- "C:/StatEdu/EasyFlow/EasyFlow_Statistics_3.0.xlsx"

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

format_p <- function(p) {
  if (is.na(p)) return(NA_character_)
  if (p < .001) return("< .001")
  sprintf("%.3f", p)
}

prepare_data <- function(data) {
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = TRUE)
  data[] <- lapply(data, function(x) {
    if (inherits(x, "haven_labelled_spss")) x <- haven::zap_missing(x)
    if (inherits(x, "haven_labelled")) x <- haven::zap_labels(x)
    if (is.character(x)) return(factor(x))
    x
  })
  data
}

read_input_data <- function(path, original_name, csv_header = TRUE, dat_delimiter = "whitespace", dat_has_names = FALSE) {
  ext <- tolower(tools::file_ext(original_name))

  if (identical(ext, "sav")) {
    return(haven::read_sav(path, user_na = TRUE))
  }

  if (identical(ext, "csv")) {
    return(readr::read_csv(path, col_names = csv_header, show_col_types = FALSE, progress = FALSE))
  }

  if (identical(ext, "dat")) {
    if (identical(dat_delimiter, "comma")) {
      return(readr::read_delim(path, delim = ",", col_names = dat_has_names, show_col_types = FALSE, progress = FALSE))
    }
    if (identical(dat_delimiter, "tab")) {
      return(readr::read_tsv(path, col_names = dat_has_names, show_col_types = FALSE, progress = FALSE))
    }
    return(readr::read_table(path, col_names = dat_has_names, show_col_types = FALSE, progress = FALSE))
  }

  stop("Unsupported file type: .", ext, call. = FALSE)
}

infer_measurement <- function(x) {
  values <- stats::na.omit(as.vector(x))
  unique_n <- length(unique(values))

  if (is.logical(x)) return("binary")
  if (is.factor(x)) return(if (nlevels(x) <= 2) "binary" else "category")
  if (is.character(x)) return(if (unique_n <= 2) "binary" else "category")
  if ((is.numeric(x) || is.integer(x)) && unique_n <= 2) return("binary")
  if ((is.numeric(x) || is.integer(x)) && unique_n <= 12) return("category")
  "continuous"
}

variable_min <- function(x) {
  values <- stats::na.omit(as.vector(x))
  if (length(values) == 0 || !(is.numeric(values) || is.integer(values))) return("")
  as.character(min(values))
}

variable_max <- function(x) {
  values <- stats::na.omit(as.vector(x))
  if (length(values) == 0 || !(is.numeric(values) || is.integer(values))) return("")
  as.character(max(values))
}

variable_summary_table <- function(data, input) {
  data.frame(
    source_order = seq_along(data),
    name = names(data),
    measurement = vapply(data, infer_measurement, character(1)),
    storage_type = vapply(data, function(x) class(x)[1], character(1)),
    n_unique = vapply(data, function(x) length(unique(stats::na.omit(as.vector(x)))), integer(1)),
    n_missing = vapply(data, function(x) sum(is.na(x)), integer(1)),
    min_value = vapply(data, variable_min, character(1)),
    max_value = vapply(data, variable_max, character(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

make_formula <- function(y, xs) {
  stats::reformulate(xs, response = y)
}

apply_filter <- function(data, filter_var, filter_condition) {
  if (!nzchar(filter_var %||% "")) {
    return(data)
  }

  if (!nzchar(trimws(filter_condition %||% ""))) {
    filter_values <- data[[filter_var]]
    keep <- !is.na(filter_values)
    if (is.logical(filter_values)) {
      keep <- keep & filter_values
    } else if (is.numeric(filter_values) || is.integer(filter_values)) {
      keep <- keep & filter_values == 1
    } else {
      keep <- keep & nzchar(as.character(filter_values))
    }
  } else {
    keep <- eval(parse(text = filter_condition), envir = data, enclos = parent.frame())
  }

  validate(
    need(is.logical(keep), "The filter condition must return TRUE/FALSE values."),
    need(length(keep) == nrow(data), "The filter condition must return one TRUE/FALSE value per row."),
    need(any(keep, na.rm = TRUE), "The filter removed all rows.")
  )

  data[keep %in% TRUE, , drop = FALSE]
}

coeftest_table <- function(model, vcov_matrix = NULL) {
  test <- if (is.null(vcov_matrix)) {
    lmtest::coeftest(model)
  } else {
    lmtest::coeftest(model, vcov. = vcov_matrix)
  }

  data.frame(
    Term = rownames(test),
    B = test[, 1],
    SE = test[, 2],
    Statistic = test[, 3],
    p = test[, 4],
    row.names = NULL,
    check.names = FALSE
  )
}

bootstrap_coef_table <- function(data, formula, r = 2000, conf = .95, seed = 1234) {
  complete_data <- model.frame(formula, data = data, na.action = na.omit)

  boot_stat <- function(d, indices) {
    fit <- lm(formula, data = d[indices, , drop = FALSE])
    coef(fit)
  }

  set.seed(seed)
  boot_fit <- boot::boot(complete_data, statistic = boot_stat, R = r)
  original_fit <- lm(formula, data = complete_data)
  alpha <- (1 - conf) / 2
  limits <- t(apply(boot_fit$t, 2, stats::quantile, probs = c(alpha, 1 - alpha), na.rm = TRUE))

  boot_p <- function(x) {
    n <- sum(!is.na(x))
    lower <- (sum(x <= 0, na.rm = TRUE) + 1) / (n + 1)
    upper <- (sum(x >= 0, na.rm = TRUE) + 1) / (n + 1)
    min(1, 2 * min(lower, upper))
  }

  data.frame(
    Term = names(coef(original_fit)),
    Boot_SE = apply(boot_fit$t, 2, stats::sd, na.rm = TRUE),
    Boot_LLCI = limits[, 1],
    Boot_ULCI = limits[, 2],
    Boot_p = apply(boot_fit$t, 2, boot_p),
    row.names = NULL,
    check.names = FALSE
  )
}

durbin_watson_stat <- function(model) {
  e <- residuals(model)
  sum(diff(e)^2) / sum(e^2)
}

lookup_dw_critical <- function(n, p, path = dw_table_path) {
  if (!file.exists(path)) {
    return(list(dL = NA_real_, dU = NA_real_, note = "Durbin-Watson critical value workbook was not found."))
  }

  if (n < 1 || n > 2000 || p < 1 || p > 20) {
    return(list(dL = NA_real_, dU = NA_real_, note = "The critical value table supports n = 1-2000 and p = 1-20."))
  }

  dU_table <- readxl::read_excel(path, sheet = "Durbin-Watson", range = "DP1:EI2000", col_names = FALSE)
  dL_table <- readxl::read_excel(path, sheet = "Durbin-Watson", range = "EL1:FE2000", col_names = FALSE)

  list(
    dL = as.numeric(dL_table[[p]][n]),
    dU = as.numeric(dU_table[[p]][n]),
    note = NA_character_
  )
}

interpret_dw <- function(d, dL, dU) {
  if (is.na(dL) || is.na(dU)) return(NA_character_)
  if (dU < d && d < 4 - dU) return("Independent")
  if (d < dL || d > 4 - dL) return("Autocorrelation likely")
  "Inconclusive"
}

empty_message <- function(text) {
  div(class = "empty-message", text)
}

ui <- navbarPage(
  title = div(class = "brand-title", "EasyFlow Regression", span(class = "version", paste0("v", app_version))),
  id = "main_menu",
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$script(HTML("
      (function() {
        var pendingSettingsSave = null;
        var settingsFilename = 'EasyFlow_Regression_Settings.json';
        var initialized = false;

        function fallbackDownload(message) {
          var blob = new Blob([message.content], { type: 'application/json;charset=utf-8' });
          var url = URL.createObjectURL(blob);
          var link = document.createElement('a');
          link.href = url;
          link.download = message.filename || settingsFilename;
          document.body.appendChild(link);
          link.click();
          link.remove();
          setTimeout(function() { URL.revokeObjectURL(url); }, 1000);
          Shiny.setInputValue('settings_save_result', {
            nonce: message.nonce,
            status: 'downloaded'
          }, { priority: 'event' });
        }

        function initSettingsSave() {
          if (initialized || !window.Shiny || !Shiny.addCustomMessageHandler || !window.jQuery) return;
          initialized = true;

          $(document).on('click', '#save_settings, #save_settings_data', async function(event) {
            event.preventDefault();
            var nonce = Date.now().toString() + Math.random().toString(16).slice(2);

            if (window.showSaveFilePicker) {
              try {
                var handle = await window.showSaveFilePicker({
                  suggestedName: settingsFilename,
                  types: [{
                    description: 'JSON settings file',
                    accept: { 'application/json': ['.json'] }
                  }]
                });
                pendingSettingsSave = { nonce: nonce, handle: handle };
                Shiny.setInputValue('settings_save_request', { nonce: nonce, fallback: false }, { priority: 'event' });
                return;
              } catch (error) {
                if (error && error.name === 'AbortError') return;
              }
            }

            Shiny.setInputValue('settings_save_request', { nonce: nonce, fallback: true }, { priority: 'event' });
          });

          Shiny.addCustomMessageHandler('saveSettingsFile', async function(message) {
            if (message.fallback || !pendingSettingsSave || pendingSettingsSave.nonce !== message.nonce) {
              fallbackDownload(message);
              return;
            }

            try {
              var writable = await pendingSettingsSave.handle.createWritable();
              await writable.write(new Blob([message.content], { type: 'application/json;charset=utf-8' }));
              await writable.close();
              Shiny.setInputValue('settings_save_result', {
                nonce: message.nonce,
                status: 'saved'
              }, { priority: 'event' });
            } catch (error) {
              console.warn('EasyFlow settings save dialog failed; using download fallback.', error);
              fallbackDownload(message);
            } finally {
              pendingSettingsSave = null;
            }
          });
        }

        document.addEventListener('shiny:connected', initSettingsSave, { once: true });
        if (document.readyState !== 'loading') initSettingsSave();
        document.addEventListener('DOMContentLoaded', initSettingsSave, { once: true });
      })();
    "))
  ),

  tabPanel(
    "Data",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1("EasyFlow Regression"),
        div("SPSS SAV, CSV, DAT files can be loaded and summarized before regression analysis.", class = "app-subtitle")
      ),
      div(
        class = "data-layout",
        div(
          class = "side-panel",
          uiOutput("data_steps")
        ),
        div(
          class = "workspace-panel",
          div(class = "load-message", textOutput("data_loaded_message")),
          div(
            class = "workspace-header",
            h3(textOutput("data_view_title")),
            uiOutput("data_view_toggle")
          ),
          conditionalPanel(
            condition = "output.data_view === 'info'",
            DTOutput("variable_table")
          ),
          conditionalPanel(
            condition = "output.data_view === 'preview'",
            DTOutput("data_preview_table")
          )
        )
      )
    )
  ),

  tabPanel(
    "Settings",
    div(
      class = "page-shell",
      div(
        class = "content-grid",
        div(
          class = "panel",
          h3("Load Settings"),
          actionButton("browse_settings", "Open Settings"),
          fileInput("settings_file", "Open Settings File (browser fallback)", accept = c(".json")),
          actionButton("apply_settings", "Apply Settings")
        ),
        div(
          class = "panel",
          h3("Save Settings"),
          p("Save the current variable selection and analysis options as a JSON settings file."),
          actionButton("save_settings", "Save Settings")
        )
      )
    )
  ),

  tabPanel(
    "EasyFlow Regression",
    div(
      class = "page-shell",
      empty_message("Regression analysis will be implemented later.")
    )
  ),

  tabPanel(
    "Results",
    div(
      class = "page-shell",
      div(
        class = "content-grid",
        div(
          class = "panel",
          h3("Load Results"),
          fileInput("results_file", "Open Results File", accept = c(".csv", ".json", ".html"))
        ),
        div(
          class = "panel",
          h3("Save Results"),
          p("Save the current coefficient table as a CSV file."),
          downloadButton("save_coefficients", "Save Coefficients")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  session$onSessionEnded(function() {
    stopApp()
  })

  data_view <- reactiveVal("info")
  selected_names <- reactiveVal(character(0))
  selection_applied <- reactiveVal(FALSE)
  active_role <- reactiveVal("dependent")
  dependent_names <- reactiveVal(character(0))
  independent_names <- reactiveVal(character(0))
  control_names <- reactiveVal(character(0))
  pending_settings <- reactiveVal(NULL)
  restored_data_file <- reactiveVal("")
  restored_variable_info <- reactiveVal(NULL)
  active_data_file <- reactiveVal(NULL)

  current_data_file <- reactive({
    uploaded <- input$file
    if (!is.null(uploaded)) {
      return(list(path = uploaded$datapath, name = uploaded$name, restored = FALSE))
    }
    active_data_file()
  })

  dataset <- reactive({
    file <- current_data_file()
    req(file)
    data <- read_input_data(
      file$path,
      file$name,
      csv_header = input$header,
      dat_delimiter = input$dat_delimiter %||% "whitespace",
      dat_has_names = isTRUE(input$dat_has_names)
    )
    prepare_data(data)
  })

  update_analysis_choices <- function(cols) {
    optional_cols <- c("None" = "", cols)
    updateSelectInput(session, "id_var", choices = optional_cols, selected = if ((input$id_var %||% "") %in% cols) input$id_var else "")
    updateSelectInput(session, "filter_var", choices = optional_cols, selected = if ((input$filter_var %||% "") %in% cols) input$filter_var else "")
    updateSelectInput(session, "y", choices = cols, selected = if ((input$y %||% "") %in% cols) input$y else character(0))
    updateSelectizeInput(session, "xs", choices = cols, selected = intersect(input$xs %||% character(0), cols), server = TRUE)
    updateSelectizeInput(session, "covariates", choices = cols, selected = intersect(input$covariates %||% character(0), cols), server = TRUE)
  }

  settings_vector <- function(x) {
    if (is.null(x)) return(character(0))
    as.character(unlist(x, use.names = FALSE))
  }

  settings_scalar <- function(x) {
    values <- settings_vector(x)
    if (length(values) == 0) return("")
    values[[1]]
  }

  restore_embedded_data_file <- function(settings) {
    embedded <- settings$data_file_content_base64 %||% settings$embedded_data_file$content_base64
    if (is.null(embedded) || !nzchar(settings_scalar(embedded))) {
      return(FALSE)
    }

    file_name <- settings_scalar(settings$data_file %||% settings$embedded_data_file$name)
    if (!nzchar(file_name)) {
      file_name <- "EasyFlow_Regression_Data"
    }
    extension <- tools::file_ext(file_name)
    restored_path <- tempfile("easyflow_data_", fileext = if (nzchar(extension)) paste0(".", extension) else "")
    writeBin(jsonlite::base64_dec(settings_scalar(embedded)), restored_path)
    active_data_file(list(path = restored_path, name = file_name, restored = TRUE))
    TRUE
  }

  restore_external_data_file <- function(settings, settings_path = NULL) {
    if (is.null(settings_path) || !nzchar(settings_path)) {
      return(FALSE)
    }

    file_name <- basename(settings_scalar(settings$data_file %||% settings$embedded_data_file$name))
    if (!nzchar(file_name)) {
      return(FALSE)
    }

    candidate <- file.path(dirname(settings_path), file_name)
    if (!file.exists(candidate)) {
      return(FALSE)
    }

    active_data_file(list(path = candidate, name = file_name, restored = TRUE))
    TRUE
  }

  settings_variable_info <- function(settings) {
    info <- settings$data_variable_info
    if (is.data.frame(info) && "name" %in% names(info)) {
      return(info)
    }

    variables <- settings_vector(settings$data_variables)
    if (length(variables) == 0) {
      return(NULL)
    }

    data.frame(
      source_order = seq_along(variables),
      name = variables,
      measurement = "",
      storage_type = "",
      n_unique = "",
      n_missing = "",
      min_value = "",
      max_value = "",
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  current_data_step <- function() {
    if (is.null(current_data_file()) && is.null(restored_variable_info())) return("load_data")
    if (isTRUE(selection_applied())) return("assign_variable_roles")
    "select_analysis_variables"
  }

  set_role_choices <- function(choices, dependent = character(0), independent = character(0), controls = character(0)) {
    choices <- as.character(choices)
    dependent_names(intersect(as.character(dependent), choices))
    independent_names(intersect(as.character(independent), choices))
    control_names(intersect(as.character(controls), choices))
  }

  active_role_names <- function() {
    switch(
      active_role(),
      independent = independent_names(),
      control = control_names(),
      dependent_names()
    )
  }

  set_active_role_names <- function(names) {
    names <- intersect(as.character(names), selected_names())
    switch(
      active_role(),
      independent = independent_names(names),
      control = control_names(names),
      dependent_names(names)
    )
  }

  available_variable_names <- function() {
    if (!is.null(current_data_file())) {
      return(names(dataset()))
    }
    info <- restored_variable_info()
    if (is.null(info)) {
      return(character(0))
    }
    as.character(info$name)
  }

  restore_settings_state <- function(settings, settings_path = NULL) {
    selected <- settings_vector(settings$selected_variables)
    dependent <- settings_vector(settings$dependent_variables %||% settings$dependent)
    independent <- settings_vector(settings$independent_variables %||% settings$independent)
    controls <- settings_vector(settings$control_variables %||% settings$covariates)

    if (!is.null(settings$data_view) && settings$data_view %in% c("info", "preview")) {
      data_view(settings$data_view)
    }
    if (!is.null(settings$selected_variables)) {
      selected_names(selected)
    }
    if (!is.null(settings$bootstrap_resamples)) updateNumericInput(session, "boot_r", value = settings$bootstrap_resamples)
    if (!is.null(settings$seed)) updateNumericInput(session, "seed", value = settings$seed)

    if (is.null(current_data_file()) && restore_external_data_file(settings, settings_path)) {
      pending_settings(settings)
      return()
    }

    if (is.null(current_data_file()) && restore_embedded_data_file(settings)) {
      pending_settings(settings)
      return()
    }

    if (is.null(current_data_file())) {
      info <- settings_variable_info(settings)
      restored_data_file(settings_scalar(settings$data_file))
      restored_variable_info(info)
      if (!is.null(info)) {
        cols <- as.character(info$name)
        selected <- intersect(selected, cols)
        selected_names(selected)
        set_role_choices(selected, dependent, independent, controls)

        applied <- isTRUE(settings$selection_applied) ||
          (settings$data_step %||% "") %in% c("review_selected_variables", "assign_variable_roles")
        selection_applied(applied && length(selected) > 0)
      } else {
        selection_applied(FALSE)
      }
      pending_settings(settings)
      return()
    }

    cols <- names(dataset())
    selected <- intersect(selected, cols)
    selected_names(selected)

    if (!is.null(settings$id)) {
      id <- settings_scalar(settings$id)
      updateSelectInput(session, "id_var", selected = if (id %in% cols) id else "")
    }
    if (!is.null(settings$filter)) {
      filter <- settings_scalar(settings$filter)
      updateSelectInput(session, "filter_var", selected = if (filter %in% cols) filter else "")
    }
    if (!is.null(settings$filter_condition)) updateTextInput(session, "filter_condition", value = settings$filter_condition)
    if (!is.null(settings$dependent)) {
      y <- settings_scalar(settings$dependent)
      updateSelectInput(session, "y", selected = if (y %in% cols) y else character(0))
    }
    if (!is.null(settings$independent)) updateSelectizeInput(session, "xs", selected = intersect(settings_vector(settings$independent), cols))
    if (!is.null(settings$covariates)) updateSelectizeInput(session, "covariates", selected = intersect(settings_vector(settings$covariates), cols))
    set_role_choices(selected, dependent, independent, controls)

    applied <- isTRUE(settings$selection_applied) ||
      (settings$data_step %||% "") %in% c("review_selected_variables", "assign_variable_roles")
    selection_applied(applied && length(selected) > 0)
    if (isTRUE(selection_applied())) {
      update_analysis_choices(selected)
    } else {
      update_analysis_choices(cols)
    }
    pending_settings(NULL)
  }

  observeEvent(dataset(), {
    cols <- names(dataset())
    settings <- pending_settings()
    if (is.null(settings)) {
      restored_data_file("")
      restored_variable_info(NULL)
      selected_names(character(0))
      selection_applied(FALSE)
      set_role_choices(character(0))
      update_analysis_choices(cols)
    } else {
      restore_settings_state(settings)
    }
  })

  observeEvent(input$file, {
    active_data_file(NULL)
  })

  observeEvent(input$variable_selected_names, {
    names <- as.character(input$variable_selected_names %||% character(0))
    if (isTRUE(selection_applied())) {
      set_active_role_names(names)
    } else {
      selected_names(names)
    }
  })

  observeEvent(input$apply_variable_selection, {
    cols <- available_variable_names()
    selected <- selected_names()
    if (length(selected) == 0) {
      showNotification("Select at least one variable to keep.", type = "warning")
      return()
    }
    selected <- selected[selected %in% cols]
    update_analysis_choices(selected)
    selection_applied(TRUE)
    active_role("dependent")
    set_role_choices(
      selected,
      dependent_names(),
      independent_names(),
      control_names()
    )
    showNotification(sprintf("%s variables selected for analysis.", length(selected)), type = "message")
  })

  observeEvent(input$modify_variable_selection, {
    req(length(available_variable_names()) > 0)
    selection_applied(FALSE)
    data_view("info")
    set_role_choices(selected_names(), dependent_names(), independent_names(), control_names())
    showNotification("Modify the checked variables, then apply the selection again.", type = "message")
  })

  observeEvent(input$select_dependent_role, {
    active_role("dependent")
  })

  observeEvent(input$select_independent_role, {
    active_role("independent")
  })

  observeEvent(input$select_control_role, {
    active_role("control")
  })

  output$data_steps <- renderUI({
    file <- current_data_file()
    has_open_data <- !is.null(file)
    has_data <- has_open_data || !is.null(restored_variable_info())
    applied <- isTRUE(selection_applied())
    selected <- selected_names()
    role <- active_role()

    tagList(
      div(
        class = paste("step-block", if (has_data) "is-closed" else "is-open"),
        h3("Step 1. Load data file"),
        if (has_data) {
          div(
            class = "step-summary",
            div(if (has_open_data) file$name else restored_data_file(), class = "step-summary-title"),
            div(
              if (has_open_data) {
                sprintf("%s variables, %s rows", ncol(dataset()), nrow(dataset()))
              } else {
                sprintf("%s variables saved in settings. Reopen the data file before running analysis.", nrow(restored_variable_info()))
              },
              class = "step-summary-detail"
            ),
            if (!has_open_data) {
              fileInput("file", "Reconnect data file", accept = c(".sav", ".csv", ".dat"))
            }
          )
        } else {
          tagList(
            fileInput("file", "Data file", accept = c(".sav", ".csv", ".dat")),
            checkboxInput("header", "CSV first row contains variable names", TRUE),
            selectInput(
              "dat_delimiter",
              "DAT delimiter",
              choices = c("Whitespace" = "whitespace", "Comma" = "comma", "Tab" = "tab"),
              selected = "whitespace"
            ),
            checkboxInput("dat_has_names", "DAT first row contains variable names", FALSE)
          )
        }
      ),
      if (has_data) {
        div(
          class = paste("step-block", if (applied) "is-closed" else "is-open"),
          h3("Step 2. Select analysis variables"),
          if (applied) {
            div(
              class = "step-summary",
              div(sprintf("%s variables selected", length(selected)), class = "step-summary-title"),
              actionButton("modify_variable_selection", "Modify selection")
            )
          } else {
            tagList(
              div("Check variables to keep, then apply the selection.", class = "step-note"),
              actionButton("apply_variable_selection", "Apply variable selection", class = "btn-primary")
            )
          }
        )
      },
      if (has_data && applied) {
        div(
          class = "step-block is-open",
          h3("Step 3. Assign variable roles"),
          div("Use the checkbox column in the variable table to select variables for the active role.", class = "step-note"),
          div(
            class = "role-actions",
            actionButton("select_dependent_role", sprintf("Dependent (%s)", length(dependent_names())), class = paste("role-button", if (identical(role, "dependent")) "is-active" else "")),
            actionButton("select_independent_role", sprintf("Independent (%s)", length(independent_names())), class = paste("role-button", if (identical(role, "independent")) "is-active" else "")),
            actionButton("select_control_role", sprintf("Control (%s)", length(control_names())), class = paste("role-button", if (identical(role, "control")) "is-active" else ""))
          ),
          div(
            sprintf(
              "Selecting: %s",
              switch(role, independent = "Independent variables", control = "Control variables (covariates)", "Dependent variables")
            ),
            class = "step-summary-detail"
          )
        )
      },
      div(
        class = "step-block",
        h3("Session settings"),
        actionButton("browse_settings_data", "Load settings"),
        fileInput("settings_file_data", "Load settings (browser fallback)", accept = c(".json")),
        actionButton("save_settings_data", "Save settings")
      )
    )
  })

  open_settings_file <- function() {
    path <- tryCatch(
      {
        if (.Platform$OS.type == "windows") {
          utils::choose.files(
            caption = "Open EasyFlow Regression Settings",
            multi = FALSE,
            filters = matrix(c("JSON settings", "*.json", "All files", "*.*"), ncol = 2, byrow = TRUE)
          )
        } else {
          file.choose()
        }
      },
      error = function(e) character(0)
    )

    if (length(path) == 0 || !nzchar(path[[1]])) {
      return(NULL)
    }
    path[[1]]
  }

  apply_settings_object <- function(settings, settings_path = NULL) {
    restore_settings_state(settings, settings_path)
    if (!is.null(current_data_file())) {
      showNotification("Settings and data file loaded.", type = "message")
    } else if (!is.null(restored_variable_info())) {
      showNotification("Settings loaded. This older settings file does not include the data file.", type = "warning")
    } else {
      showNotification("Settings loaded.", type = "message")
    }
  }

  observeEvent(input$apply_settings, {
    req(input$settings_file)
    apply_settings_object(jsonlite::fromJSON(input$settings_file$datapath))
  })

  observeEvent(input$browse_settings, {
    settings_path <- open_settings_file()
    if (is.null(settings_path)) {
      return()
    }
    apply_settings_object(jsonlite::fromJSON(settings_path), settings_path)
  })

  observeEvent(input$settings_file_data, {
    req(input$settings_file_data)
    apply_settings_object(jsonlite::fromJSON(input$settings_file_data$datapath))
  })

  observeEvent(input$browse_settings_data, {
    settings_path <- open_settings_file()
    if (is.null(settings_path)) {
      return()
    }
    apply_settings_object(jsonlite::fromJSON(settings_path), settings_path)
  })

  observeEvent(input$show_data_preview, {
    data_view("preview")
  })

  observeEvent(input$show_variable_info, {
    data_view("info")
  })

  output$data_view <- reactive({
    data_view()
  })
  outputOptions(output, "data_view", suspendWhenHidden = FALSE)

  analysis <- eventReactive(input$run, {
    data <- apply_filter(dataset(), input$filter_var, input$filter_condition)
    req(input$y, input$xs)
    predictors <- unique(c(input$xs, input$covariates))
    validate(need(!(input$y %in% predictors), "The dependent variable cannot also be an independent variable or covariate."))
    validate(need(length(input$xs) > 0, "Select at least one independent variable."))
    validate(need(length(predictors) > 0, "Select at least one predictor."))

    formula <- make_formula(input$y, predictors)
    model <- lm(formula, data = data)
    resid_model <- residuals(model)

    normality <- nortest::lillie.test(resid_model)
    homogeneity <- lmtest::bptest(model)
    dw_d <- durbin_watson_stat(model)
    dw_n <- stats::nobs(model)
    dw_p <- ncol(model.matrix(model)) - 1
    dw_crit <- lookup_dw_critical(dw_n, dw_p)
    dw_judgment <- interpret_dw(dw_d, dw_crit$dL, dw_crit$dU)

    normal_ok <- normality$p.value > .05
    homo_ok <- homogeneity$p.value > .05

    method <- if (normal_ok && homo_ok) {
      "OLS regression"
    } else if (normal_ok && !homo_ok) {
      "OLS regression with HC3 robust standard errors"
    } else if (!normal_ok && homo_ok) {
      "OLS regression with bootstrap confidence intervals and bootstrap p values"
    } else {
      "OLS regression with HC3 robust standard errors, bootstrap confidence intervals, and bootstrap p values"
    }

    use_hc3 <- !homo_ok
    use_bootstrap <- !normal_ok

    vcov_matrix <- if (use_hc3) sandwich::vcovHC(model, type = "HC3") else NULL
    coef_table <- coeftest_table(model, vcov_matrix)

    boot_table <- if (use_bootstrap) {
      bootstrap_coef_table(data, formula, r = input$boot_r, seed = input$seed)
    } else {
      NULL
    }

    list(
      model = model,
      diagnostics = data.frame(
        Assumption = c(
          "Residual normality: Lilliefors corrected K-S test",
          "Homoscedasticity: Breusch-Pagan test"
        ),
        Statistic = c(unname(normality$statistic), unname(homogeneity$statistic)),
        p = c(format_p(normality$p.value), format_p(homogeneity$p.value)),
        Decision = c(
          if (normal_ok) "Not rejected" else "Violated",
          if (homo_ok) "Not rejected" else "Violated"
        ),
        check.names = FALSE
      ),
      dw_result = data.frame(
        Item = c("Durbin-Watson d", "n", "p", "dL", "dU", "4 - dU", "4 - dL", "Decision", "Note"),
        Value = c(
          round(dw_d, 4),
          dw_n,
          dw_p,
          ifelse(is.na(dw_crit$dL), NA, round(dw_crit$dL, 4)),
          ifelse(is.na(dw_crit$dU), NA, round(dw_crit$dU, 4)),
          ifelse(is.na(dw_crit$dU), NA, round(4 - dw_crit$dU, 4)),
          ifelse(is.na(dw_crit$dL), NA, round(4 - dw_crit$dL, 4)),
          dw_judgment,
          dw_crit$note
        ),
        check.names = FALSE
      ),
      method = method,
      coef_table = coef_table,
      boot_table = boot_table
    )
  })

  output$data_loaded_message <- renderText({
    file <- current_data_file()
    if (is.null(file)) {
      if (!is.null(restored_variable_info())) {
        return(sprintf(
          "Settings loaded for %s: %s variables saved. Reopen the data file before running analysis.",
          restored_data_file(),
          nrow(restored_variable_info())
        ))
      }
      return("No data file is open.")
    }
    data <- dataset()
    if (isTRUE(file$restored)) {
      sprintf("Loaded %s from settings: %s variables, %s rows.", file$name, ncol(data), nrow(data))
    } else {
      sprintf("Loaded %s: %s variables, %s rows.", file$name, ncol(data), nrow(data))
    }
  })

  output$data_view_title <- renderText({
    if (is.null(current_data_file()) && is.null(restored_variable_info())) {
      return("Variable Info")
    }
    if (identical(data_view(), "preview")) {
      if (isTRUE(selection_applied())) {
        return(sprintf("Selected Data Preview (%s variables)", length(selected_names())))
      }
      return("Data Preview")
    }
    if (isTRUE(selection_applied())) {
      role_label <- switch(
        active_role(),
        independent = "independent variables",
        control = "control variables",
        "dependent variables"
      )
      return(sprintf("Select %s (%s of %s selected)", role_label, length(active_role_names()), length(selected_names())))
    }
    sprintf("All variables (%s)", length(available_variable_names()))
  })

  output$data_view_toggle <- renderUI({
    if (identical(data_view(), "preview")) {
      actionButton("show_variable_info", "Variable Info", class = "view-toggle-button")
    } else {
      actionButton("show_data_preview", "Data Preview", class = "view-toggle-button")
    }
  })

  output$variable_table <- renderDT({
    if (is.null(current_data_file()) && is.null(restored_variable_info())) {
      return(DT::datatable(
        data.frame(Message = "Read a SAV, CSV, or DAT file to show variable information."),
        rownames = FALSE,
        options = list(dom = "t")
      ))
    }
    table_data <- if (is.null(current_data_file())) {
      restored_variable_info()
    } else {
      variable_summary_table(dataset(), input)
    }
    if (isTRUE(selection_applied())) {
      active_role()
      checked_names <- isolate(active_role_names())
      table_data <- table_data[table_data$name %in% selected_names(), , drop = FALSE]
    } else {
      checked_names <- isolate(selected_names())
    }
    table_data <- cbind(
      selected = sprintf(
        '<input type="checkbox" class="variable-select" data-name="%s" %s>',
        htmltools::htmlEscape(table_data$name),
        ifelse(table_data$name %in% checked_names, "checked", "")
      ),
      table_data,
      stringsAsFactors = FALSE
    )
    DT::datatable(
      table_data,
      rownames = FALSE,
      escape = FALSE,
      filter = "top",
      options = list(
        dom = '<"variable-table-top"lfp>rt<"variable-table-bottom"ip>',
        pageLength = 20,
        lengthMenu = c(10, 20, 50, 100),
        scrollX = TRUE,
        autoWidth = TRUE,
        order = list(list(1, "asc")),
        columnDefs = list(
          list(orderable = FALSE, targets = 0),
          list(visible = FALSE, targets = 1)
        )
      ),
      callback = DT::JS(sprintf(
        "
        var selected = %s;
        window.easyflowSelectedNames = {};
        selected.forEach(function(name) { window.easyflowSelectedNames[name] = true; });
        var selectedHeader = $(table.column(0).header());
        var nameHeader = $(table.column(2).header());
        var nameSortState = 'original';

        selectedHeader.html('<button type=\"button\" class=\"page-select-toggle is-off\">selected</button>');
        nameHeader.html('<button type=\"button\" class=\"name-sort-toggle\">name <span class=\"sort-mark\">original</span></button>');

        function syncVariableSelection() {
          Shiny.setInputValue('variable_selected_names', Object.keys(window.easyflowSelectedNames || {}), {priority: 'event'});
        }

        function currentPageNames() {
          var names = [];
          table.rows({page: 'current'}).every(function() {
            var data = this.data();
            if (data && data[2]) names.push(data[2]);
          });
          return names;
        }

        function updatePageToggle() {
          var names = currentPageNames();
          var allSelected = names.length > 0 && names.every(function(name) {
            return !!window.easyflowSelectedNames[name];
          });
          selectedHeader.find('.page-select-toggle')
            .toggleClass('is-on', allSelected)
            .toggleClass('is-off', !allSelected);
        }

        function refreshVariableChecks() {
          table.$('input.variable-select').each(function() {
            var name = $(this).data('name');
            $(this).prop('checked', !!window.easyflowSelectedNames[name]);
          });
          updatePageToggle();
        }

        function setCurrentPageSelection(checked) {
          table.rows({page: 'current'}).every(function() {
            var data = this.data();
            if (!data || !data[2]) return;
            if (checked) {
              window.easyflowSelectedNames[data[2]] = true;
            } else {
              delete window.easyflowSelectedNames[data[2]];
            }
            $(this.node()).find('input.variable-select').prop('checked', checked);
          });
          refreshVariableChecks();
          syncVariableSelection();
        }

        table.on('draw.dt', refreshVariableChecks);
        selectedHeader.off('click.easyflowPageToggle').on('click.easyflowPageToggle', '.page-select-toggle', function(e) {
          e.preventDefault();
          e.stopPropagation();
          setCurrentPageSelection(!$(this).hasClass('is-on'));
        });
        nameHeader.off('click.easyflowNameSort').on('click.easyflowNameSort', '.name-sort-toggle', function(e) {
          e.preventDefault();
          e.stopPropagation();
          if (nameSortState === 'original') {
            nameSortState = 'asc';
            table.order([2, 'asc']).draw();
            $(this).find('.sort-mark').text('▲');
          } else if (nameSortState === 'asc') {
            nameSortState = 'desc';
            table.order([2, 'desc']).draw();
            $(this).find('.sort-mark').text('▼');
          } else {
            nameSortState = 'original';
            table.order([1, 'asc']).draw();
            $(this).find('.sort-mark').text('original');
          }
        });
        table.on('change', 'input.variable-select', function() {
          if (window.getSelection) window.getSelection().removeAllRanges();
          $(this).closest('tr').removeClass('selected');
          var name = $(this).data('name');
          if ($(this).is(':checked')) {
            window.easyflowSelectedNames[name] = true;
          } else {
            delete window.easyflowSelectedNames[name];
          }
          refreshVariableChecks();
          syncVariableSelection();
        });
        table.on('click', 'input.variable-select', function(e) {
          e.stopPropagation();
          $(this).closest('tr').removeClass('selected');
        });

        refreshVariableChecks();
        syncVariableSelection();
        ",
        jsonlite::toJSON(checked_names, auto_unbox = FALSE)
      ))
    )
  })

  output$data_preview_table <- renderDT({
    if (is.null(current_data_file())) {
      return(DT::datatable(
        data.frame(Message = "Reopen the data file to preview data rows."),
        rownames = FALSE,
        options = list(dom = "t")
      ))
    }
    data <- dataset()
    if (isTRUE(selection_applied())) {
      selected <- selected_names()
      data <- data[, intersect(selected, names(data)), drop = FALSE]
    }
    DT::datatable(
      head(data, 20),
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 20,
        lengthMenu = c(10, 20, 50, 100),
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )
  })

  output$diagnostics <- renderTable({
    analysis()$diagnostics
  }, digits = 4)

  output$decision <- renderText({
    analysis()$method
  })

  output$dw_result <- renderTable({
    analysis()$dw_result
  }, digits = 4)

  output$regression <- renderTable({
    table <- analysis()$coef_table
    table$p <- vapply(table$p, format_p, character(1))
    table
  }, digits = 4)

  output$bootstrap_ci <- renderTable({
    table <- analysis()$boot_table
    if (is.null(table)) {
      return(data.frame(Message = "Bootstrap results are displayed when the residual normality assumption is violated."))
    }
    table$Boot_p <- vapply(table$Boot_p, format_p, character(1))
    table
  }, digits = 4)

  output$summary <- renderPrint({
    summary(analysis()$model)
  })

  current_settings <- function() {
    file <- current_data_file()
    variable_info <- if (is.null(file)) restored_variable_info() else variable_summary_table(dataset(), input)
    variable_names <- if (is.null(variable_info)) character(0) else as.character(variable_info$name)
    data_content <- if (is.null(file)) "" else jsonlite::base64_enc(readBin(file$path, "raw", n = file.info(file$path)$size))

    list(
      app = "EasyFlow Regression",
      version = app_version,
      data_step = current_data_step(),
      data_view = data_view(),
      data_file = if (is.null(file)) restored_data_file() else file$name,
      data_file_content_base64 = data_content,
      data_variables = I(variable_names),
      data_variable_info = I(if (is.null(variable_info)) list() else variable_info),
      selection_applied = isTRUE(selection_applied()),
      id = input$id_var %||% "",
      filter = input$filter_var %||% "",
      filter_condition = input$filter_condition %||% "",
      dependent_variables = I(as.character(dependent_names())),
      independent_variables = I(as.character(independent_names())),
      control_variables = I(as.character(control_names())),
      dependent = I(as.character(dependent_names())),
      independent = I(as.character(independent_names())),
      covariates = I(as.character(control_names())),
      selected_variables = I(as.character(selected_names() %||% character(0))),
      bootstrap_resamples = input$boot_r %||% 2000,
      seed = input$seed %||% 1234
    )
  }

  observeEvent(input$settings_save_request, {
    request <- input$settings_save_request
    session$sendCustomMessage(
      "saveSettingsFile",
      list(
        nonce = request$nonce,
        fallback = isTRUE(request$fallback),
        filename = "EasyFlow_Regression_Settings.json",
        content = as.character(jsonlite::toJSON(current_settings(), pretty = TRUE, auto_unbox = TRUE))
      )
    )
  })

  observeEvent(input$settings_save_result, {
    result <- input$settings_save_result
    if (identical(result$status, "saved")) {
      showNotification("Settings file was saved.", type = "message")
    } else if (identical(result$status, "downloaded")) {
      showNotification("Settings file was downloaded.", type = "message")
    } else {
      showNotification("Save dialog failed, so the settings file was downloaded instead.", type = "warning")
    }
  })

  output$save_coefficients <- downloadHandler(
    filename = function() "EasyFlow_Regression_Coefficients.csv",
    content = function(file) {
      write.csv(analysis()$coef_table, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui, server)
