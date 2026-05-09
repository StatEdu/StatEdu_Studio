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
  header = tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),

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
          div(
            class = "step-block",
            h3("Step 1. Load data file"),
            conditionalPanel(
              condition = "!input.file",
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
          ),
          div(
            class = "step-block",
            h3("Step 2. Select analysis variables"),
            div("Check variables to keep, then apply the selection.", class = "step-note"),
            uiOutput("variable_selection_controls")
          ),
          div(
            class = "step-block",
            h3("Session settings"),
            fileInput("settings_file_data", "Load settings", accept = c(".json")),
            downloadButton("save_settings_data", "Save settings")
          )
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
          fileInput("settings_file", "Open Settings File", accept = c(".json")),
          actionButton("apply_settings", "Apply Settings")
        ),
        div(
          class = "panel",
          h3("Save Settings"),
          p("Save the current variable selection and analysis options as a JSON settings file."),
          downloadButton("save_settings", "Save Settings")
        )
      )
    )
  ),

  tabPanel(
    "EasyFlow Regression",
    div(
      class = "page-shell",
      div(
        class = "analysis-layout",
        div(
          class = "side-panel",
          h3("Model Setup"),
          selectInput("id_var", "ID Variable", choices = NULL),
          selectInput("filter_var", "Filter Variable", choices = NULL),
          conditionalPanel(
            condition = "input.filter_var && input.filter_var !== ''",
            textInput("filter_condition", "Filter Condition", placeholder = "Example: group == 1")
          ),
          selectInput("y", "Dependent Variable", choices = NULL),
          selectizeInput("xs", "Independent Variables", choices = NULL, multiple = TRUE),
          selectizeInput("covariates", "Covariates", choices = NULL, multiple = TRUE),
          numericInput("boot_r", "Bootstrap Resamples", value = 2000, min = 500, step = 500),
          numericInput("seed", "Random Seed", value = 1234, min = 1, step = 1),
          actionButton("run", "Run Analysis", class = "primary-action")
        ),
        div(
          class = "result-panel",
          h3("Assumption Diagnostics"),
          tableOutput("diagnostics"),
          h3("Durbin-Watson Test"),
          tableOutput("dw_result"),
          h3("Selected Method"),
          verbatimTextOutput("decision"),
          h3("Regression Coefficients"),
          tableOutput("regression"),
          h3("Bootstrap Results"),
          tableOutput("bootstrap_ci"),
          h3("Model Summary"),
          verbatimTextOutput("summary")
        )
      )
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

  dataset <- reactive({
    req(input$file)
    data <- read_input_data(
      input$file$datapath,
      input$file$name,
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

  observeEvent(dataset(), {
    cols <- names(dataset())
    selected_names(character(0))
    selection_applied(FALSE)
    update_analysis_choices(cols)
  })

  observeEvent(input$variable_selected_names, {
    selected_names(as.character(input$variable_selected_names %||% character(0)))
  })

  observeEvent(input$apply_variable_selection, {
    req(dataset())
    selected <- selected_names()
    if (length(selected) == 0) {
      showNotification("Select at least one variable to keep.", type = "warning")
      return()
    }
    selected <- selected[selected %in% names(dataset())]
    update_analysis_choices(selected)
    selection_applied(TRUE)
    showNotification(sprintf("%s variables selected for analysis.", length(selected)), type = "message")
  })

  observeEvent(input$modify_variable_selection, {
    req(dataset())
    selection_applied(FALSE)
    data_view("info")
    showNotification("Modify the checked variables, then apply the selection again.", type = "message")
  })

  output$variable_selection_controls <- renderUI({
    has_data <- !is.null(input$file)
    tagList(
      actionButton(
        "apply_variable_selection",
        "Apply variable selection",
        class = "btn-primary",
        disabled = if (has_data) NULL else "disabled"
      ),
      actionButton(
        "modify_variable_selection",
        "Modify selection",
        disabled = if (has_data) NULL else "disabled"
      )
    )
  })

  apply_settings_object <- function(settings) {
    if (!is.null(settings$id)) updateSelectInput(session, "id_var", selected = settings$id %||% "")
    if (!is.null(settings$filter)) updateSelectInput(session, "filter_var", selected = settings$filter %||% "")
    if (!is.null(settings$filter_condition)) updateTextInput(session, "filter_condition", value = settings$filter_condition)
    if (!is.null(settings$dependent)) updateSelectInput(session, "y", selected = settings$dependent)
    if (!is.null(settings$independent)) updateSelectizeInput(session, "xs", selected = settings$independent)
    if (!is.null(settings$covariates)) updateSelectizeInput(session, "covariates", selected = settings$covariates)
    if (!is.null(settings$selected_variables)) selected_names(as.character(settings$selected_variables))
    if (!is.null(settings$bootstrap_resamples)) updateNumericInput(session, "boot_r", value = settings$bootstrap_resamples)
    if (!is.null(settings$seed)) updateNumericInput(session, "seed", value = settings$seed)
  }

  observeEvent(input$apply_settings, {
    req(input$settings_file)
    apply_settings_object(jsonlite::fromJSON(input$settings_file$datapath))
  })

  observeEvent(input$settings_file_data, {
    req(input$settings_file_data)
    apply_settings_object(jsonlite::fromJSON(input$settings_file_data$datapath))
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
    if (is.null(input$file)) {
      return("No data file is open.")
    }
    data <- dataset()
    sprintf("Loaded %s: %s variables, %s rows.", input$file$name, ncol(data), nrow(data))
  })

  output$data_view_title <- renderText({
    if (is.null(input$file)) {
      return("Variable Info")
    }
    if (identical(data_view(), "preview")) {
      return("Data Preview")
    }
    sprintf("All variables (%s)", ncol(dataset()))
  })

  output$data_view_toggle <- renderUI({
    if (identical(data_view(), "preview")) {
      actionButton("show_variable_info", "Variable Info", class = "view-toggle-button")
    } else {
      actionButton("show_data_preview", "Data Preview", class = "view-toggle-button")
    }
  })

  output$variable_table <- renderDT({
    if (is.null(input$file)) {
      return(DT::datatable(
        data.frame(Message = "Read a SAV, CSV, or DAT file to show variable information."),
        rownames = FALSE,
        options = list(dom = "t")
      ))
    }
    data <- dataset()
    table_data <- variable_summary_table(data, input)
    checked_names <- isolate(selected_names())
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
    if (is.null(input$file)) {
      return(DT::datatable(
        data.frame(Message = "Read a SAV, CSV, or DAT file to preview data."),
        rownames = FALSE,
        options = list(dom = "t")
      ))
    }
    DT::datatable(
      head(dataset(), 20),
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
    list(
      app = "EasyFlow Regression",
      version = app_version,
      id = input$id_var,
      filter = input$filter_var,
      filter_condition = input$filter_condition,
      dependent = input$y,
      independent = input$xs,
      covariates = input$covariates,
      selected_variables = selected_names(),
      bootstrap_resamples = input$boot_r,
      seed = input$seed
    )
  }

  make_save_settings_handler <- function() {
    downloadHandler(
      filename = function() "EasyFlow_Regression_Settings.json",
      content = function(file) {
        jsonlite::write_json(current_settings(), file, pretty = TRUE, auto_unbox = TRUE)
      }
    )
  }

  output$save_settings <- make_save_settings_handler()
  output$save_settings_data <- make_save_settings_handler()

  output$save_coefficients <- downloadHandler(
    filename = function() "EasyFlow_Regression_Coefficients.csv",
    content = function(file) {
      write.csv(analysis()$coef_table, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui, server)
