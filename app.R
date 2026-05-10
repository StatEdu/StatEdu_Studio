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
dw_table_path <- "D:/Program/EXCEL_macro/EasyFlow_Statistics_3.2.1.xlsm"

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

named_value <- function(x, name, default = "") {
  name <- as.character(name %||% "")
  name <- if (length(name) == 0 || is.na(name[[1]])) "" else name[[1]]
  if (is.null(x) || !nzchar(name) || is.null(names(x))) {
    return(default)
  }
  index <- match(name, names(x))
  if (is.na(index) || index < 1 || index > length(x)) {
    return(default)
  }
  value <- x[[index]]
  if (is.null(value) || length(value) == 0 || is.na(value[[1]])) {
    return(default)
  }
  as.character(value[[1]])
}

format_p <- function(p) {
  if (is.na(p)) return(NA_character_)
  if (p < .001) return("< .001")
  sprintf("%.3f", p)
}

format_decimal3 <- function(x) {
  if (is.na(x)) return("")
  text <- sprintf("%.3f", x)
  text <- sub("^-0\\.", "-.", text)
  sub("^0\\.", ".", text)
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
  if (is.ordered(x)) return("ordered")
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

variable_label <- function(x) {
  label <- attr(x, "label", exact = TRUE)
  if (is.null(label) || length(label) == 0 || is.na(label[[1]])) return("")
  as.character(label[[1]])
}

value_label_pairs <- function(x, prepared_x = x, max_pairs = 6) {
  labelled_values <- attr(x, "labels", exact = TRUE)
  if (!is.null(labelled_values) && length(labelled_values) > 0) {
    values <- as.character(unname(labelled_values))
    labels <- names(labelled_values)
  } else {
    values <- sort(unique(stats::na.omit(as.vector(prepared_x))))
    values <- as.character(utils::head(values, max_pairs))
    labels <- rep("", length(values))
  }

  output <- stats::setNames(rep("", max_pairs * 2), as.vector(rbind(paste0("value_", seq_len(max_pairs)), paste0("label_", seq_len(max_pairs)))))
  if (length(values) > 0) {
    for (i in seq_len(min(length(values), max_pairs))) {
      output[[paste0("value_", i)]] <- values[[i]]
      output[[paste0("label_", i)]] <- labels[[i]] %||% ""
    }
  }
  output
}

variable_summary_table <- function(data, input, raw_data = data) {
  raw_data <- as.data.frame(raw_data, stringsAsFactors = FALSE, check.names = TRUE)
  labels <- vapply(seq_along(data), function(i) variable_label(raw_data[[i]]), character(1))
  value_labels <- do.call(rbind, lapply(seq_along(data), function(i) {
    as.data.frame(as.list(value_label_pairs(raw_data[[i]], data[[i]])), stringsAsFactors = FALSE, check.names = FALSE)
  }))
  cbind(data.frame(
    source_order = seq_along(data),
    name = names(data),
    var_label = labels,
    measurement = vapply(data, infer_measurement, character(1)),
    storage_type = vapply(data, function(x) class(x)[1], character(1)),
    n_unique = vapply(data, function(x) length(unique(stats::na.omit(as.vector(x)))), integer(1)),
    n_missing = vapply(data, function(x) sum(is.na(x)), integer(1)),
    min_value = vapply(data, variable_min, character(1)),
    max_value = vapply(data, variable_max, character(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  ), value_labels)
}

make_formula <- function(y, xs) {
  stats::reformulate(xs, response = y)
}

apply_filter <- function(data, filter_var, filter_condition) {
  filter_var <- as.character(filter_var %||% "")
  filter_var <- if (length(filter_var) == 0) "" else filter_var[[1]]
  filter_condition <- as.character(filter_condition %||% "")
  filter_condition <- if (length(filter_condition) == 0) "" else filter_condition[[1]]
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

  shiny::validate(
    shiny::need(is.logical(keep), "The filter condition must return TRUE/FALSE values."),
    shiny::need(length(keep) == nrow(data), "The filter condition must return one TRUE/FALSE value per row."),
    shiny::need(any(keep, na.rm = TRUE), "The filter removed all rows.")
  )

  data[keep %in% TRUE, , drop = FALSE]
}

coeftest_table <- function(model, vcov_matrix = NULL) {
  test <- if (is.null(vcov_matrix)) {
    lmtest::coeftest(model)
  } else {
    lmtest::coeftest(model, vcov. = vcov_matrix)
  }

  model_matrix <- stats::model.matrix(model)
  outcome <- stats::model.response(stats::model.frame(model))
  outcome_sd <- stats::sd(outcome, na.rm = TRUE)
  predictor_sd <- apply(model_matrix, 2, stats::sd, na.rm = TRUE)
  beta <- test[, 1] * predictor_sd[rownames(test)] / outcome_sd
  beta[rownames(test) == "(Intercept)" | is.na(beta) | is.na(outcome_sd) | outcome_sd == 0] <- NA_real_

  data.frame(
    Term = rownames(test),
    B = test[, 1],
    SE = test[, 2],
    beta = beta,
    t = test[, 3],
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
    tags$link(rel = "stylesheet", type = "text/css", href = paste0("style.css?v=", app_version, "-overlay")),
    tags$script(HTML("
      window.easyflowSettingsDirty = false;
      window.easyflowVarLabels = window.easyflowVarLabels || {};

      function captureEasyflowVarLabels() {
        function collectInput(input) {
          var name = input.getAttribute('data-name');
          if (!name) {
            var row = input.closest('tr');
            var cells = row ? row.querySelectorAll('td') : [];
            if (cells.length > 1) name = (cells[1].textContent || '').trim();
          }
          if (name) window.easyflowVarLabels[name] = input.value || '';
        }

        var selectors = [
          'input.var-label-input',
          'input[data-field=\"var_label\"]',
          'table.dataTable tbody tr td:nth-child(3) input[type=\"text\"]'
        ];
        document.querySelectorAll(selectors.join(',')).forEach(collectInput);

        document.querySelectorAll('table.dataTable').forEach(function(table) {
          var headers = Array.prototype.slice.call(table.querySelectorAll('thead tr:first-child th'));
          var labelIndex = headers.findIndex(function(th) {
            return (th.textContent || '').replace(/\\s+/g, ' ').trim().indexOf('var_label') >= 0;
          });
          if (labelIndex < 0) return;
          table.querySelectorAll('tbody tr').forEach(function(row) {
            var cells = row.querySelectorAll('td');
            var input = cells[labelIndex] ? cells[labelIndex].querySelector('input[type=\"text\"]') : null;
            if (input) collectInput(input);
          });
        });
        return window.easyflowVarLabels;
      }

      function saveEasyflowVarLabelInput(input) {
        if (!input || !input.matches || !input.matches('input.var-label-input, input[data-field=\"var_label\"]')) return;
        var name = input.getAttribute('data-name');
        if (!name) {
          var row = input.closest('tr');
          var cells = row ? row.querySelectorAll('td') : [];
          if (cells.length > 1) name = (cells[1].textContent || '').trim();
        }
        if (!name) return;
        window.easyflowVarLabels = window.easyflowVarLabels || {};
        window.easyflowVarLabels[name] = input.value || '';
        if (window.Shiny) {
          Shiny.setInputValue('var_label_cell_input', {
            name: name,
            value: window.easyflowVarLabels[name],
            nonce: Date.now() + Math.random()
          }, {priority: 'event'});
        }
      }
      window.easyflowStoreVarLabel = saveEasyflowVarLabelInput;

      function flushEasyflowInputs() {
        captureEasyflowVarLabels();
        document.querySelectorAll('input.category-label-input, input.var-label-input').forEach(function(input) {
          input.dispatchEvent(new Event('change', {bubbles: true}));
        });
        if (window.Shiny) {
          Shiny.setInputValue('var_label_snapshot', {
            values: window.easyflowVarLabels || {},
            nonce: Date.now() + Math.random()
          }, {priority: 'event'});
        }
      }

      function registerEasyflowDirtyHandler() {
        if (!window.Shiny || window.easyflowDirtyHandlerRegistered) return;
        window.easyflowDirtyHandlerRegistered = true;
        Shiny.addCustomMessageHandler('easyflow-settings-dirty', function(value) {
          window.easyflowSettingsDirty = !!value;
        });
      }
      registerEasyflowDirtyHandler();
      document.addEventListener('shiny:connected', registerEasyflowDirtyHandler);
      window.setTimeout(registerEasyflowDirtyHandler, 0);

      window.addEventListener('beforeunload', function(event) {
        if (!window.easyflowSettingsDirty) return;
        event.preventDefault();
        event.returnValue = '';
      });

      window.addEventListener('error', function(event) {
        if (!window.Shiny) return;
        Shiny.setInputValue('client_js_error', {
          message: event.message || '',
          source: event.filename || '',
          line: event.lineno || '',
          column: event.colno || '',
          nonce: Date.now() + Math.random()
        }, {priority: 'event'});
      });

      window.addEventListener('unhandledrejection', function(event) {
        if (!window.Shiny) return;
        Shiny.setInputValue('client_js_error', {
          message: String(event.reason || ''),
          source: 'unhandledrejection',
          line: '',
          column: '',
          nonce: Date.now() + Math.random()
        }, {priority: 'event'});
      });

      document.addEventListener('click', function(event) {
        if (event.target.closest('.navbar-nav a')) {
          flushEasyflowInputs();
          if (window.Shiny) {
            Shiny.setInputValue('nav_flush_request', {
              var_labels: captureEasyflowVarLabels(),
              nonce: Date.now() + Math.random()
            }, {priority: 'event'});
          }
        }
      }, true);

      ['input', 'change', 'focusout', 'blur'].forEach(function(eventName) {
        document.addEventListener(eventName, function(event) {
          saveEasyflowVarLabelInput(event.target);
        }, true);
      });

      document.addEventListener('click', function(event) {
        var button = event.target.closest('.settings-save-button');
        if (!button || !window.Shiny) return;
        event.preventDefault();
        event.stopImmediatePropagation();

        flushEasyflowInputs();

        var state = window.easyflowCurrentTableState ? window.easyflowCurrentTableState() : { measurements: {}, var_labels: {} };
        var varLabels = Object.assign({}, window.easyflowVarLabels || {}, state.var_labels || {}, captureEasyflowVarLabels());
        var categoryLabels = {};
        document.querySelectorAll('input.category-label-input').forEach(function(input) {
          var name = input.getAttribute('data-name');
          var field = input.getAttribute('data-field');
          if (!name || !field) return;
          categoryLabels[name] = categoryLabels[name] || {};
          categoryLabels[name][field] = input.value || '';
        });
        Shiny.setInputValue('save_settings_request', {
          selected: state.selected,
          measurements: state.measurements || {},
          var_labels: varLabels,
          category_labels: categoryLabels,
          nonce: Date.now() + Math.random()
        }, {priority: 'event'});
      }, true);
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
          ),
          conditionalPanel(
            condition = "output.data_view === 'labels'",
            DTOutput("category_label_table")
          )
        )
      )
    )
  ),

  tabPanel(
    "EasyFlow Regression",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1("EasyFlow Regression"),
        div("Review selected variables and run regression analysis.", class = "app-subtitle")
      ),
      div(
        class = "regression-layout",
        div(
          class = "side-panel",
          uiOutput("regression_variable_list")
        ),
        div(
          class = "workspace-panel",
          h3("Regression setup"),
          uiOutput("regression_setup"),
          uiOutput("regression_results")
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
    if (identical(Sys.getenv("EASYFLOW_STOP_ON_SESSION_END"), "1")) {
      stopApp()
    }
  })

  data_view <- reactiveVal("info")
  active_step <- reactiveVal("step1")
  selected_names <- reactiveVal(character(0))
  selection_applied <- reactiveVal(FALSE)
  roles_applied <- reactiveVal(FALSE)
  active_role <- reactiveVal("dependent")
  filter_names <- reactiveVal(character(0))
  dependent_names <- reactiveVal(character(0))
  independent_names <- reactiveVal(character(0))
  control_names <- reactiveVal(character(0))
  predictor_order <- reactiveVal(character(0))
  var_label_overrides <- reactiveVal(character(0))
  category_label_values <- reactiveVal(NULL)
  pending_settings <- reactiveVal(NULL)
  restored_data_file <- reactiveVal("")
  restored_variable_info <- reactiveVal(NULL)
  measurement_overrides <- reactiveVal(character(0))
  active_data_file <- reactiveVal(NULL)
  reset_on_dataset_load <- reactiveVal(FALSE)
  unsaved_settings <- reactiveVal(FALSE)
  suppress_dirty_tracking <- reactiveVal(FALSE)

  set_unsaved_settings <- function(value) {
    unsaved_settings(isTRUE(value))
    session$sendCustomMessage("easyflow-settings-dirty", isTRUE(value))
  }

  mark_settings_dirty <- function() {
    if (isTRUE(suppress_dirty_tracking())) {
      return(invisible(FALSE))
    }
    set_unsaved_settings(TRUE)
    invisible(TRUE)
  }

  mark_settings_clean <- function() {
    set_unsaved_settings(FALSE)
    invisible(TRUE)
  }

  observeEvent(input$client_js_error, {
    message(
      sprintf(
        "Client JS error: %s (%s:%s:%s)",
        input$client_js_error$message %||% "",
        input$client_js_error$source %||% "",
        input$client_js_error$line %||% "",
        input$client_js_error$column %||% ""
      )
    )
  })

  current_data_file <- reactive({
    uploaded <- input$file
    if (!is.null(uploaded)) {
      return(list(path = uploaded$datapath, name = uploaded$name, restored = FALSE))
    }
    active_data_file()
  })

  raw_dataset <- reactive({
    file <- current_data_file()
    req(file)
    read_input_data(
      file$path,
      file$name,
      csv_header = input$header,
      dat_delimiter = input$dat_delimiter %||% "whitespace",
      dat_has_names = isTRUE(input$dat_has_names)
    )
  })

  dataset <- reactive({
    prepare_data(raw_dataset())
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

  settings_named_vector <- function(x) {
    if (is.null(x)) {
      return(character(0))
    }

    clean_setting_names <- function(names) {
      names <- as.character(names %||% character(0))
      for (unused in seq_len(4)) {
        names <- sub("^(var_labels|values|var_label_overrides|measurements)\\.", "", names)
      }
      names
    }

    keep_named_values <- function(values) {
      names(values) <- clean_setting_names(names(values))
      values[!is.na(names(values)) & nzchar(names(values))]
    }

    if (is.data.frame(x)) {
      if (all(c("name", "value") %in% names(x))) {
        values <- stats::setNames(as.character(x$value), as.character(x$name))
        return(keep_named_values(values))
      }
      if (ncol(x) == 1) {
        values <- as.character(x[[1]])
        names(values) <- rownames(x)
        return(keep_named_values(values))
      }
    }
    values <- unlist(x, use.names = TRUE)
    if (length(values) == 0) {
      return(character(0))
    }
    values <- as.character(values)
    keep_named_values(values)
  }

  update_var_label_overrides <- function(values, allow_blank = TRUE) {
    labels <- settings_named_vector(values)
    if (length(labels) == 0 || is.null(names(labels))) {
      return(invisible(FALSE))
    }
    labels <- labels[!is.na(names(labels)) & nzchar(names(labels))]
    if (!isTRUE(allow_blank)) {
      labels <- labels[nzchar(trimws(as.character(labels)))]
    }
    if (length(labels) == 0) {
      return(invisible(FALSE))
    }
    current <- var_label_overrides()
    changed <- FALSE
    for (index in seq_along(labels)) {
      name <- names(labels)[[index]]
      value <- as.character(labels[[index]] %||% "")
      if (!identical(named_value(current, name, ""), value)) {
        current[name] <- value
        changed <- TRUE
      }
    }
    if (changed) {
      var_label_overrides(current)
      mark_settings_dirty()
      message(sprintf("Updated var_label: %s", paste(sprintf("%s=%s", names(labels), unname(labels)), collapse = ", ")))
    }
    invisible(changed)
  }

  collect_var_label_inputs <- function() {
    info <- tryCatch(variable_info_table(reactive_labels = FALSE), error = function(e) NULL)
    if (is.null(info) || !all(c("source_order", "name") %in% names(info))) {
      return(character(0))
    }

    collected <- character(0)
    for (row_index in seq_len(nrow(info))) {
      source_order <- as.character(info$source_order[[row_index]])
      name <- as.character(info$name[[row_index]])
      if (!nzchar(source_order) || !nzchar(name)) {
        next
      }
      value <- input[[paste0("var_label_input_", source_order)]]
      if (is.null(value)) {
        value <- input[[paste0("category_var_label_input_", source_order)]]
      }
      if (!is.null(value) && length(value) > 0 && nzchar(trimws(as.character(value[[1]])))) {
        collected[name] <- as.character(value[[1]])
      }
    }
    collected
  }

  merge_var_label_overrides <- function(labels) {
    if (is.null(labels) || length(labels) == 0 || is.null(names(labels))) {
      return(invisible(FALSE))
    }
    label_names <- names(labels)
    labels <- as.character(labels)
    names(labels) <- as.character(label_names)
    labels <- labels[!is.na(names(labels)) & nzchar(names(labels)) & nzchar(trimws(labels))]
    if (length(labels) == 0) {
      return(invisible(FALSE))
    }

    current <- var_label_overrides()
    changed <- FALSE
    for (name in names(labels)) {
      value <- as.character(labels[[name]])
      if (!identical(named_value(current, name, ""), value)) {
        current[name] <- value
        changed <- TRUE
      }
    }
    if (changed) {
      var_label_overrides(current)
      mark_settings_dirty()
      message(sprintf("Merged direct var_label: %s", paste(sprintf("%s=%s", names(labels), unname(labels)), collapse = ", ")))
    }
    invisible(changed)
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
      var_label = "",
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

  settings_measurement_overrides <- function(settings) {
    info <- settings_variable_info(settings)
    saved <- character(0)
    if (is.data.frame(info) && all(c("name", "measurement") %in% names(info))) {
      saved <- stats::setNames(as.character(info$measurement), as.character(info$name))
      saved <- saved[nzchar(names(saved)) & saved %in% c("binary", "category", "ordered", "continuous")]
    }

    overrides <- settings_named_vector(settings$measurement_overrides)
    overrides <- overrides[nzchar(names(overrides)) & overrides %in% c("binary", "category", "ordered", "continuous")]
    if (length(overrides) > 0) {
      names(overrides) <- sub("^.*\\.", "", names(overrides))
    }
    saved[names(overrides)] <- overrides
    saved
  }

  apply_measurement_overrides <- function(table_data, overrides = measurement_overrides()) {
    if (length(overrides) == 0 || is.null(table_data) || nrow(table_data) == 0) {
      return(table_data)
    }

    matched <- table_data$name %in% names(overrides)
    table_data$measurement[matched] <- unname(overrides[table_data$name[matched]])
    table_data
  }

  measurement_select_html <- function(name, value) {
    choices <- unique(c("binary", "category", "ordered", "continuous", value))
    sprintf(
      '<select class="measurement-select" data-name="%s">%s</select>',
      htmltools::htmlEscape(name),
      paste(
        sprintf(
          '<option value="%s" %s>%s</option>',
          choices,
          ifelse(choices == value, "selected", ""),
          choices
        ),
        collapse = ""
      )
    )
  }

  update_measurement_overrides <- function(values) {
    updates <- settings_named_vector(values)
    if (length(updates) == 0 || is.null(names(updates))) {
      return(invisible(FALSE))
    }
    valid_names <- !is.na(names(updates)) & nzchar(names(updates))
    valid_values <- !is.na(updates) & updates %in% c("binary", "category", "ordered", "continuous")
    updates <- updates[valid_names & valid_values]
    if (length(updates) > 0) {
      names(updates) <- sub("^.*\\.", "", names(updates))
      updates <- updates[!is.na(names(updates)) & nzchar(names(updates))]
    }
    if (length(updates) == 0) {
      return(invisible(FALSE))
    }

    overrides <- measurement_overrides()
    unchanged <- vapply(names(updates), function(name) {
      identical(named_value(overrides, name, ""), as.character(updates[[name]]))
    }, logical(1))
    if (all(unchanged, na.rm = TRUE)) {
      return(invisible(FALSE))
    }
    overrides[names(updates)] <- updates
    measurement_overrides(overrides)
    dependent_names(intersect(dependent_names(), continuous_variable_names()))
    mark_settings_dirty()
    invisible(TRUE)
  }

  current_data_step <- function() {
    if (is.null(current_data_file()) && is.null(restored_variable_info())) return("load_data")
    if (identical(active_step(), "step4")) return("category_labels")
    if (isTRUE(selection_applied())) return("assign_variable_roles")
    "select_analysis_variables"
  }

  continuous_variable_names <- function() {
    info <- if (is.null(current_data_file())) restored_variable_info() else variable_summary_table(dataset(), input, raw_dataset())
    info <- apply_measurement_overrides(info)
    if (is.null(info) || nrow(info) == 0) {
      return(character(0))
    }
    as.character(info$name[info$measurement == "continuous"])
  }

  set_role_choices <- function(choices, dependent = character(0), independent = character(0), controls = character(0), filters = character(0)) {
    choices <- as.character(choices)
    dependent <- intersect(intersect(as.character(dependent), choices), continuous_variable_names())
    independent <- setdiff(intersect(as.character(independent), choices), dependent)
    controls <- setdiff(intersect(as.character(controls), choices), c(dependent, independent))

    filter_names(character(0))
    dependent_names(dependent)
    independent_names(independent)
    control_names(controls)
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
      independent = {
        independent_names(names)
        dependent_names(setdiff(dependent_names(), names))
        control_names(setdiff(control_names(), names))
      },
      control = {
        control_names(names)
        dependent_names(setdiff(dependent_names(), names))
        independent_names(setdiff(independent_names(), names))
      },
      {
        dependent_names(names)
        independent_names(setdiff(independent_names(), names))
        control_names(setdiff(control_names(), names))
      }
    )
  }

  assigned_elsewhere_names <- function() {
    switch(
      active_role(),
      independent = unique(c(dependent_names(), control_names())),
      control = unique(c(dependent_names(), independent_names())),
      unique(c(independent_names(), control_names()))
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

  variable_info_table <- function(reactive_labels = TRUE) {
    info <- if (is.null(current_data_file())) restored_variable_info() else variable_summary_table(dataset(), input, raw_dataset())
    info <- apply_measurement_overrides(info)
    if (is.null(info)) {
      return(NULL)
    }
    required <- c("source_order", "name", "var_label", "measurement", "storage_type", "n_unique", "n_missing", "min_value", "max_value")
    for (column in required) {
      if (!column %in% names(info)) {
        info[[column]] <- ""
      }
    }
    labels <- if (isTRUE(reactive_labels)) var_label_overrides() else isolate(var_label_overrides())
    if (length(labels) > 0) {
      matched <- info$name %in% names(labels)
      info$var_label[matched] <- unname(labels[info$name[matched]])
    }
    info
  }

  role_for_name <- function(name) {
    if (name %in% dependent_names()) return("dependent")
    if (name %in% independent_names()) return("independent")
    if (name %in% control_names()) return("covariate")
    "exclude"
  }

  restore_settings_state <- function(settings, settings_path = NULL) {
    selected <- settings_vector(settings$selected_variables)
    dependent <- settings_vector(settings$dependent_variables %||% settings$dependent)
    independent <- settings_vector(settings$independent_variables %||% settings$independent)
    controls <- settings_vector(settings$control_variables %||% settings$covariates)
    saved_var_labels <- settings_named_vector(settings$var_label_overrides)
    if (length(saved_var_labels) == 0) {
      info <- settings_variable_info(settings)
      if (is.data.frame(info) && all(c("name", "var_label") %in% names(info))) {
        saved_var_labels <- stats::setNames(as.character(info$var_label), as.character(info$name))
      }
    }
    saved_var_labels <- saved_var_labels[nzchar(names(saved_var_labels)) & nzchar(trimws(as.character(saved_var_labels)))]
    var_label_overrides(saved_var_labels)
    saved_category_labels <- settings$category_value_labels %||% NULL
    if (is.data.frame(saved_category_labels)) {
      category_label_values(saved_category_labels)
    } else {
      category_label_values(NULL)
    }
    if (!is.null(settings$active_step) && settings$active_step %in% c("step1", "step2", "step3", "step4")) {
      active_step(settings$active_step)
      data_view(if (identical(settings$active_step, "step4")) "labels" else settings$data_view %||% "info")
    }

    if (!is.null(settings$data_view) && settings$data_view %in% c("info", "preview")) {
      data_view(settings$data_view)
    }
    if (!is.null(settings$selected_variables)) {
      selected_names(selected)
    }
    if (!is.null(settings$bootstrap_resamples)) updateNumericInput(session, "boot_r", value = settings$bootstrap_resamples)
    if (!is.null(settings$seed)) updateNumericInput(session, "seed", value = settings$seed)
    measurement_overrides(settings_measurement_overrides(settings))

    if (is.null(current_data_file())) {
      pending_settings(settings)
      if (restore_external_data_file(settings, settings_path)) {
        return()
      }
      pending_settings(NULL)
    }

    if (is.null(current_data_file())) {
      pending_settings(settings)
      if (restore_embedded_data_file(settings)) {
        return()
      }
      pending_settings(NULL)
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
          (settings$data_step %||% "") %in% c("review_selected_variables", "assign_variable_roles", "category_labels")
        selection_applied(applied && length(selected) > 0)
        roles_applied(
          isTRUE(settings$roles_applied) ||
            (settings$data_step %||% "") %in% c("category_labels") ||
            (length(dependent_names()) > 0 && length(independent_names()) > 0)
        )
      } else {
        selection_applied(FALSE)
        roles_applied(FALSE)
      }
      pending_settings(settings)
      return()
    }

    cols <- names(dataset())
    selected <- intersect(selected, cols)
    selected_names(selected)

    if (!is.null(settings$filter_condition)) updateTextInput(session, "filter_condition", value = settings$filter_condition)
    if (!is.null(settings$dependent)) {
      y <- settings_scalar(settings$dependent)
      updateSelectInput(session, "y", selected = if (y %in% cols) y else character(0))
    }
    if (!is.null(settings$independent)) updateSelectizeInput(session, "xs", selected = intersect(settings_vector(settings$independent), cols))
    if (!is.null(settings$covariates)) updateSelectizeInput(session, "covariates", selected = intersect(settings_vector(settings$covariates), cols))
    set_role_choices(selected, dependent, independent, controls)

    applied <- isTRUE(settings$selection_applied) ||
      (settings$data_step %||% "") %in% c("review_selected_variables", "assign_variable_roles", "category_labels")
    selection_applied(applied && length(selected) > 0)
    roles_applied(
      isTRUE(settings$roles_applied) ||
        (settings$data_step %||% "") %in% c("category_labels") ||
        (length(dependent_names()) > 0 && length(independent_names()) > 0)
    )
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
      if (isTRUE(reset_on_dataset_load())) {
        reset_on_dataset_load(FALSE)
        restored_data_file("")
        restored_variable_info(NULL)
        measurement_overrides(character(0))
        var_label_overrides(character(0))
        category_label_values(NULL)
        selected_names(character(0))
        selection_applied(FALSE)
        roles_applied(FALSE)
        active_step("step2")
        data_view("info")
        set_role_choices(character(0))
        update_analysis_choices(cols)
      }
    } else {
      reset_on_dataset_load(FALSE)
      restore_settings_state(settings)
    }
  })

  observeEvent(input$file, {
    reset_on_dataset_load(TRUE)
    active_data_file(NULL)
    mark_settings_dirty()
  })

  observeEvent(input$header, {
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$dat_delimiter, {
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$dat_has_names, {
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  topmost_tk_parent <- function() {
    if (!requireNamespace("tcltk", quietly = TRUE)) {
      return(NULL)
    }

    parent <- tcltk::tktoplevel()
    try(tcltk::tkwm.withdraw(parent), silent = TRUE)
    try(tcltk::tcl("wm", "attributes", parent, "-topmost", 1), silent = TRUE)
    try(tcltk::tcl("wm", "attributes", parent, "-toolwindow", 1), silent = TRUE)
    try(tcltk::tkfocus(parent), silent = TRUE)
    parent
  }

  open_file_dialog <- function(title, filetypes) {
    path <- tryCatch(
      {
        if (requireNamespace("tcltk", quietly = TRUE)) {
          parent <- topmost_tk_parent()
          on.exit(try(tcltk::tkdestroy(parent), silent = TRUE), add = TRUE)
          as.character(tcltk::tkgetOpenFile(parent = parent, title = title, filetypes = filetypes))
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

  open_data_file <- function() {
    open_file_dialog(
      "Open EasyFlow Regression Data",
      "{{Data files} {.sav .csv .dat}} {{SPSS SAV} {.sav}} {{CSV} {.csv}} {{DAT} {.dat}} {{All files} *}"
    )
  }

  observeEvent(input$browse_data_file, {
    data_path <- open_data_file()
    if (is.null(data_path)) {
      return()
    }

    reset_on_dataset_load(TRUE)
    active_data_file(list(path = data_path, name = basename(data_path), restored = FALSE))
    mark_settings_dirty()
  })

  observeEvent(input$variable_selected_names, {
    names <- as.character(input$variable_selected_names %||% character(0))
    if (isTRUE(selection_applied())) {
      before <- active_role_names()
      set_active_role_names(names)
      if (!identical(before, active_role_names())) {
        mark_settings_dirty()
      }
    } else {
      if (!identical(selected_names(), names)) {
        selected_names(names)
        mark_settings_dirty()
      }
    }
  })

  observeEvent(input$variable_table_state, {
    sync_table_state(input$variable_table_state)
  })

  observeEvent(input$nav_flush_request, {
    update_var_label_overrides(input$nav_flush_request$var_labels %||% character(0), allow_blank = FALSE)
  })

  observeEvent(input$variable_measurement_update, {
    update <- input$variable_measurement_update
    name <- as.character(update$name %||% "")
    value <- as.character(update$value %||% "")
    if (!nzchar(name) || !(value %in% c("binary", "category", "ordered", "continuous"))) {
      return()
    }

    update_measurement_overrides(stats::setNames(value, name))
  })

  observeEvent(input$variable_measurement_snapshot, {
    snapshot <- input$variable_measurement_snapshot
    update_measurement_overrides(snapshot$values %||% snapshot)
  })

  sync_table_state <- function(state) {
    if (is.null(state)) {
      return(invisible(FALSE))
    }

    update_measurement_overrides(state$measurements %||% character(0))
    update_var_label_overrides(state$var_labels %||% character(0), allow_blank = FALSE)
    if (!is.null(state$selected)) {
      names <- as.character(settings_vector(state$selected))
      if (isTRUE(selection_applied())) {
        before <- active_role_names()
        set_active_role_names(names)
        if (!identical(before, active_role_names())) {
          mark_settings_dirty()
        }
      } else {
        if (!identical(selected_names(), names)) {
          selected_names(names)
          mark_settings_dirty()
        }
      }
    }
    invisible(TRUE)
  }

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
    roles_applied(FALSE)
    active_step("step3")
    data_view("info")
    active_role("dependent")
    set_role_choices(
      selected,
      dependent_names(),
      independent_names(),
      control_names()
    )
    mark_settings_dirty()
    showNotification(sprintf("%s variables selected for analysis.", length(selected)), type = "message")
  })

  observeEvent(input$modify_variable_selection, {
    req(length(available_variable_names()) > 0)
    selection_applied(FALSE)
    roles_applied(FALSE)
    active_step("step2")
    data_view("info")
    set_role_choices(selected_names(), dependent_names(), independent_names(), control_names())
    mark_settings_dirty()
    showNotification("Modify the checked variables, then apply the selection again.", type = "message")
  })

  observeEvent(input$go_step1, {
    active_step("step1")
    data_view("info")
  })

  observeEvent(input$go_step2, {
    req(length(available_variable_names()) > 0)
    active_step("step2")
    data_view("info")
  })

  observeEvent(input$go_step3, {
    req(isTRUE(selection_applied()))
    active_step("step3")
    data_view("info")
  })

  observeEvent(input$go_step4, {
    req(isTRUE(roles_applied()))
    active_step("step4")
    data_view("labels")
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

  observeEvent(input$apply_role_selection, {
    sync_table_state(input$variable_table_state)
    if (length(dependent_names()) == 0) {
      showNotification("Select one dependent variable before applying roles.", type = "warning")
      return()
    }
    if (length(independent_names()) == 0) {
      showNotification("Select at least one independent variable before applying roles.", type = "warning")
      return()
    }
    roles_applied(TRUE)
    predictor_order(predictor_candidates())
    active_step("step4")
    data_view("labels")
    updateSelectInput(session, "y", choices = selected_names(), selected = utils::head(dependent_names(), 1))
    updateSelectizeInput(session, "xs", choices = selected_names(), selected = independent_names(), server = TRUE)
    updateSelectizeInput(session, "covariates", choices = selected_names(), selected = control_names(), server = TRUE)
    mark_settings_dirty()
    showNotification("Variable roles applied. Edit categorical value labels in Step 4.", type = "message")
  })

  output$data_steps <- renderUI({
    file <- current_data_file()
    has_open_data <- !is.null(file)
    has_data <- has_open_data || !is.null(restored_variable_info())
    applied <- isTRUE(selection_applied())
    role_applied <- isTRUE(roles_applied())
    selected <- selected_names()
    role <- active_role()
    step <- active_step()
    step_class <- function(name, enabled = TRUE) {
      paste("step-block", if (identical(step, name)) "is-open" else "is-closed", if (!enabled) "is-disabled" else "")
    }

    tagList(
      div(
        class = step_class("step1"),
        h3(actionLink("go_step1", "Step 1. Load data file", class = "step-link")),
        if (has_data && !identical(step, "step1")) {
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
              actionButton("browse_data_file", "Reconnect data file")
            }
          )
        } else {
          tagList(
            actionButton("browse_data_file", "Open data file"),
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
          class = step_class("step2", has_data),
          h3(actionLink("go_step2", "Step 2. Select analysis variables", class = "step-link")),
          if (!identical(step, "step2") && applied) {
            div(
              class = "step-summary",
              div(sprintf("%s variables selected", length(selected)), class = "step-summary-title"),
              actionButton("modify_variable_selection", "Modify selection")
            )
          } else if (identical(step, "step2")) {
            tagList(
              div("Check variables to keep, then apply the selection.", class = "step-note"),
              actionButton("apply_variable_selection", "Apply variable selection", class = "btn-primary")
            )
          } else {
            div("Select variables first.", class = "step-note")
          }
        )
      },
      if (has_data && applied) {
        div(
          class = step_class("step3", applied),
          h3(actionLink("go_step3", "Step 3. Assign variable roles", class = "step-link")),
          if (!identical(step, "step3") && role_applied) {
            div(
              class = "step-summary",
              div(sprintf(
                "Dependent %s, independent %s, covariates %s",
                length(dependent_names()), length(independent_names()), length(control_names())
              ), class = "step-summary-title")
            )
          } else if (identical(step, "step3")) {
            tagList(
              div("Use the checkbox column in the variable table to select variables for the active role.", class = "step-note"),
              div(
                class = "role-actions",
                actionButton("select_dependent_role", sprintf("Dependent (%s)", length(dependent_names())), class = paste("role-button", if (identical(role, "dependent")) "is-active" else "")),
                actionButton("select_independent_role", sprintf("Independent (%s)", length(independent_names())), class = paste("role-button", if (identical(role, "independent")) "is-active" else "")),
                actionButton("select_control_role", sprintf("Control/Covariates (%s)", length(control_names())), class = paste("role-button", if (identical(role, "control")) "is-active" else ""))
              ),
              div(
                sprintf(
                  "Selecting: %s",
                  switch(role, independent = "Independent variables", control = "Control/Covariates", "Dependent variable")
                ),
                class = "step-summary-detail"
              ),
              actionButton("apply_role_selection", "Apply variable roles", class = "btn-primary")
            )
          }
        )
      },
      if (has_data && role_applied) {
        div(
          class = step_class("step4", role_applied),
          h3(actionLink("go_step4", "Step 4. Category labels", class = "step-link")),
          if (identical(step, "step4")) {
            div("Edit value labels for categorical variables. These labels are saved with the session settings.", class = "step-note")
          } else {
            div(
              class = "step-summary",
              div("Categorical value labels", class = "step-summary-title"),
              div("Click Step 4 to review or edit labels.", class = "step-summary-detail")
            )
          }
        )
      },
      div(
        class = "step-block session-settings-block",
        h3("Session settings"),
      div(
        class = "session-settings-actions",
        actionButton("browse_settings_data", "Load settings", class = "session-settings-button"),
          actionButton("save_settings_data", "Save settings", class = "settings-save-button session-settings-button"),
          actionButton("reset_settings_data", "Reset settings", class = "reset-settings-button session-settings-button")
        )
      )
    )
  })

  reset_session_settings <- function() {
    suppress_dirty_tracking(TRUE)
    active_data_file(NULL)
    restored_data_file("")
    restored_variable_info(NULL)
    selected_names(character(0))
    selection_applied(FALSE)
    roles_applied(FALSE)
    active_role("dependent")
    filter_names(character(0))
    dependent_names(character(0))
    independent_names(character(0))
    control_names(character(0))
    var_label_overrides(character(0))
    category_label_values(NULL)
    measurement_overrides(character(0))
    pending_settings(NULL)
    data_view("info")

    updateCheckboxInput(session, "header", value = TRUE)
    updateSelectInput(session, "dat_delimiter", selected = "whitespace")
    updateCheckboxInput(session, "dat_has_names", value = FALSE)
    updateSelectInput(session, "id_var", selected = "")
    updateSelectInput(session, "filter_var", selected = "")
    updateTextInput(session, "filter_condition", value = "")
    updateSelectInput(session, "y", selected = character(0))
    updateSelectizeInput(session, "xs", selected = character(0))
    updateSelectizeInput(session, "covariates", selected = character(0))
    updateNumericInput(session, "boot_r", value = 2000)
    updateNumericInput(session, "seed", value = 1234)

    active_step("step1")

    session$onFlushed(function() {
      suppress_dirty_tracking(FALSE)
      mark_settings_clean()
    }, once = TRUE)
    showNotification("Settings were reset.", type = "message")
  }

  observeEvent(input$reset_settings_data, {
    reset_session_settings()
  })

  open_settings_file <- function() {
    open_file_dialog(
      "Open EasyFlow Regression Settings",
      "{{JSON settings} {.json}} {{All files} *}"
    )
  }

  save_settings_file <- function() {
    path <- tryCatch(
      {
        if (requireNamespace("tcltk", quietly = TRUE)) {
          parent <- topmost_tk_parent()
          on.exit(try(tcltk::tkdestroy(parent), silent = TRUE), add = TRUE)
          as.character(tcltk::tkgetSaveFile(
            parent = parent,
            title = "Save EasyFlow Regression Settings",
            initialfile = "EasyFlow_Regression_Settings.json",
            filetypes = "{{JSON settings} {.json}} {{All files} *}"
          ))
        } else {
          folder <- utils::choose.dir(caption = "Choose a folder for EasyFlow Regression Settings")
          if (is.na(folder) || !nzchar(folder)) {
            character(0)
          } else {
            file.path(folder, "EasyFlow_Regression_Settings.json")
          }
        }
      },
      error = function(e) character(0)
    )

    if (length(path) == 0 || !nzchar(path[[1]])) {
      return(NULL)
    }

    path <- path[[1]]
    if (!nzchar(tools::file_ext(path))) {
      path <- paste0(path, ".json")
    }
    path
  }

  apply_settings_object <- function(settings, settings_path = NULL) {
    suppress_dirty_tracking(TRUE)
    restore_settings_state(settings, settings_path)
    session$onFlushed(function() {
      suppress_dirty_tracking(FALSE)
      mark_settings_clean()
    }, once = TRUE)
    if (!is.null(current_data_file())) {
      showNotification("Settings and data file loaded.", type = "message")
    } else if (!is.null(restored_variable_info())) {
      showNotification("Settings loaded. This older settings file does not include the data file.", type = "warning")
    } else {
      showNotification("Settings loaded.", type = "message")
    }
  }

  observeEvent(input$browse_settings_data, {
    settings_path <- open_settings_file()
    if (is.null(settings_path)) {
      return()
    }
    apply_settings_object(jsonlite::fromJSON(settings_path), settings_path)
  })

  save_settings_to_file <- function() {
    settings_path <- save_settings_file()
    if (is.null(settings_path)) {
      return()
    }

    settings <- current_settings()
    writeLines(
      as.character(jsonlite::toJSON(settings, pretty = TRUE, auto_unbox = TRUE)),
      con = settings_path,
      useBytes = TRUE
    )
    label_count <- length(settings_named_vector(settings$var_label_overrides))
    message(sprintf("Saved settings: %s var_label override(s) -> %s", label_count, settings_path))
    mark_settings_clean()
    showNotification("Settings file was saved.", type = "message")
  }

  observeEvent(input$save_settings_request, {
    sync_table_state(input$save_settings_request)
    input_var_labels <- collect_var_label_inputs()
    if (length(input_var_labels) > 0) {
      merge_var_label_overrides(input_var_labels)
    }
    if (!is.null(input$save_settings_request$var_labels)) {
      update_var_label_overrides(input$save_settings_request$var_labels, allow_blank = FALSE)
    }
    incoming_var_labels <- settings_named_vector(input$save_settings_request$var_labels %||% character(0))
    incoming_nonempty <- incoming_var_labels[nzchar(trimws(as.character(incoming_var_labels)))]
    current_nonempty <- var_label_overrides()
    current_nonempty <- current_nonempty[nzchar(trimws(as.character(current_nonempty)))]
    message(sprintf(
      "Save request received: %s var_label value(s), %s non-empty, %s direct input(s), current overrides: %s [%s]",
      length(incoming_var_labels),
      length(incoming_nonempty),
      length(input_var_labels),
      length(var_label_overrides()),
      paste(sprintf("%s=%s", names(current_nonempty), unname(current_nonempty)), collapse = ", ")
    ))
    if (!is.null(input$save_settings_request$category_labels)) {
      incoming <- input$save_settings_request$category_labels
      value_columns <- c("reference", "reference_label", as.vector(rbind(paste0("value_", seq_len(6)), paste0("label_", seq_len(6)))))
      current <- category_label_values()
      if (!is.data.frame(current)) {
        base <- category_label_table_data()
        current <- if (is.null(base) || !"name" %in% names(base)) {
          data.frame(name = character(0), stringsAsFactors = FALSE, check.names = FALSE)
        } else {
          base[, c("name", intersect(value_columns, names(base))), drop = FALSE]
        }
      }
      for (column in value_columns) {
        if (!column %in% names(current)) current[[column]] <- ""
      }
      for (name in names(incoming)) {
        if (!name %in% current$name) {
          current <- rbind(current, as.data.frame(as.list(c(name = name, stats::setNames(rep("", length(value_columns)), value_columns))), stringsAsFactors = FALSE, check.names = FALSE))
        }
        row_index <- match(name, current$name)
        for (field in intersect(names(incoming[[name]]), value_columns)) {
          current[[field]][[row_index]] <- as.character(incoming[[name]][[field]] %||% "")
        }
      }
      category_label_values(current)
    }
    save_settings_to_file()
  })

  observeEvent(input$show_data_preview, {
    active_step(if (isTRUE(selection_applied())) active_step() else "step2")
    data_view("preview")
  })

  observeEvent(input$show_variable_info, {
    if (identical(data_view(), "labels")) {
      active_step("step3")
    }
    data_view("info")
  })

  output$data_view <- reactive({
    data_view()
  })
  outputOptions(output, "data_view", suspendWhenHidden = FALSE)

  regression_reference_values <- function() {
    table <- category_label_values()
    if (!is.data.frame(table) || !"name" %in% names(table) || !"reference" %in% names(table)) {
      return(character(0))
    }
    refs <- stats::setNames(as.character(table$reference %||% ""), as.character(table$name))
    refs[nzchar(trimws(refs))]
  }

  prepare_regression_model_data <- function(data, variables) {
    info <- variable_info_table()
    if (is.null(info) || nrow(info) == 0) {
      return(data)
    }

    variables <- intersect(as.character(variables), names(data))
    categorical_info <- info[info$name %in% variables & info$measurement %in% c("binary", "category", "ordered"), , drop = FALSE]
    if (nrow(categorical_info) == 0) {
      return(data)
    }

    refs <- regression_reference_values()
    for (name in as.character(categorical_info$name)) {
      values <- data[[name]]
      data[[name]] <- factor(as.character(values))
      reference <- trimws(named_value(refs, name, ""))
      if (nzchar(reference) && reference %in% levels(data[[name]])) {
        data[[name]] <- stats::relevel(data[[name]], ref = reference)
      }
    }
    data
  }

  category_value_label_lookup <- function() {
    table <- category_label_values()
    if (!is.data.frame(table) || !"name" %in% names(table)) {
      return(list())
    }

    lookup <- list()
    value_columns <- paste0("value_", seq_len(6))
    label_columns <- paste0("label_", seq_len(6))
    for (row_index in seq_len(nrow(table))) {
      name <- as.character(table$name[[row_index]] %||% "")
      if (!nzchar(name)) next
      values <- character(0)
      for (i in seq_along(value_columns)) {
        value <- if (value_columns[[i]] %in% names(table)) as.character(table[[value_columns[[i]]]][[row_index]] %||% "") else ""
        label <- if (label_columns[[i]] %in% names(table)) as.character(table[[label_columns[[i]]]][[row_index]] %||% "") else ""
        if (nzchar(trimws(value)) && nzchar(trimws(label))) {
          values[trimws(value)] <- trimws(label)
        }
      }
      lookup[[name]] <- values
    }
    lookup
  }

  display_term_name <- function(term) {
    term <- as.character(term %||% "")
    if (!nzchar(term) || identical(term, "(Intercept)")) {
      return(term)
    }

    term_clean <- gsub("`", "", term, fixed = TRUE)
    labels <- var_label_overrides()
    labels <- labels[!is.na(names(labels)) & nzchar(names(labels)) & nzchar(trimws(as.character(labels)))]
    if (length(labels) == 0) {
      return(term)
    }

    variable_names <- names(labels)
    variable_names <- variable_names[order(nchar(variable_names), decreasing = TRUE)]
    value_labels <- category_value_label_lookup()

    for (name in variable_names) {
      if (identical(term_clean, name)) {
        return(as.character(labels[[name]]))
      }
      if (startsWith(term_clean, name)) {
        level <- substring(term_clean, nchar(name) + 1)
        if (!nzchar(level)) next
        variable_label <- as.character(labels[[name]])
        category_label <- named_value(value_labels[[name]], level, "")
        if (nzchar(category_label)) {
          return(sprintf("%s:%s", variable_label, category_label))
        }
        return(sprintf("%s:%s", variable_label, level))
      }
    }

    term
  }

  display_term_table <- function(table) {
    if (!is.data.frame(table) || !"Term" %in% names(table)) {
      return(table)
    }
    table$Term <- vapply(table$Term, display_term_name, character(1))
    table
  }

  raw_term_variable <- function(term, variable_names) {
    term <- gsub("`", "", as.character(term %||% ""), fixed = TRUE)
    variable_names <- variable_names[order(nchar(variable_names), decreasing = TRUE)]
    for (name in variable_names) {
      if (identical(term, name) || startsWith(term, name)) {
        return(name)
      }
    }
    ""
  }

  raw_term_level <- function(term, variable_name) {
    term <- gsub("`", "", as.character(term %||% ""), fixed = TRUE)
    if (!nzchar(variable_name) || identical(term, variable_name)) return("")
    if (!startsWith(term, variable_name)) return("")
    substring(term, nchar(variable_name) + 1)
  }

  categorical_reference_rows <- function(predictors, columns, raw_terms = character(0)) {
    info <- variable_info_table()
    if (is.null(info) || nrow(info) == 0 || length(predictors) == 0) {
      return(NULL)
    }

    categorical <- info[info$name %in% predictors & info$measurement %in% c("binary", "category", "ordered"), , drop = FALSE]
    if (nrow(categorical) == 0) {
      return(NULL)
    }

    refs <- regression_reference_values()
    value_labels <- category_value_label_lookup()
    rows <- lapply(seq_len(nrow(categorical)), function(index) {
      name <- as.character(categorical$name[[index]])
      reference <- trimws(named_value(refs, name, ""))
      if (!nzchar(reference)) {
        values <- value_labels[[name]]
        if (!is.null(values) && length(values) > 0) {
          reference <- names(values)[[1]]
        }
      }
      if (!nzchar(reference)) {
        return(NULL)
      }

      variable_label <- named_value(var_label_overrides(), name, "")
      if (!nzchar(variable_label)) variable_label <- name
      reference_label <- named_value(value_labels[[name]], reference, "")
      term <- if (nzchar(reference_label)) {
        sprintf("%s:%s", variable_label, reference_label)
      } else {
        sprintf("%s:%s", variable_label, reference)
      }

      row <- stats::setNames(as.list(rep("", length(columns))), columns)
      row$Term <- term
      if ("B" %in% columns) {
        row$B <- "reference"
      }
      row$.raw_variable <- name
      row$.raw_level <- reference
      as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)
    })

    rows <- Filter(Negate(is.null), rows)
    if (length(rows) == 0) {
      return(NULL)
    }
    do.call(rbind, rows)
  }

  coefficient_output_table <- function(table, predictors = character(0), include_references = TRUE) {
    if (!is.data.frame(table) || nrow(table) == 0) {
      return(table)
    }

    raw_terms <- as.character(table$Term)
    labels <- var_label_overrides()
    variable_names <- names(labels)
    table$.raw_variable <- vapply(raw_terms, raw_term_variable, character(1), variable_names = variable_names)
    table$.raw_level <- mapply(raw_term_level, raw_terms, table$.raw_variable, USE.NAMES = FALSE)
    table <- display_term_table(table)

    if ("p" %in% names(table)) {
      table$p <- vapply(table$p, format_p, character(1))
    }
    if ("Boot_p" %in% names(table)) {
      table$Boot_p <- vapply(table$Boot_p, format_p, character(1))
    }
    for (column in setdiff(names(table), c("Term", "p", "Boot_p"))) {
      if (is.numeric(table[[column]])) {
        table[[column]] <- vapply(table[[column]], format_decimal3, character(1))
      }
    }

    if (!isTRUE(include_references)) {
      return(table)
    }

    reference_rows <- categorical_reference_rows(predictors, names(table), raw_terms = raw_terms)
    if (is.null(reference_rows) || nrow(reference_rows) == 0) {
      return(table[, setdiff(names(table), c(".raw_variable", ".raw_level")), drop = FALSE])
    }

    output <- table[0, , drop = FALSE]
    categorical_names <- unique(reference_rows$.raw_variable)
    used_reference <- rep(FALSE, nrow(reference_rows))

    for (row_index in seq_len(nrow(table))) {
      variable <- table$.raw_variable[[row_index]]
      if (nzchar(variable) && variable %in% categorical_names && !any(used_reference[reference_rows$.raw_variable == variable])) {
        rows <- reference_rows[reference_rows$.raw_variable == variable, , drop = FALSE]
        output <- rbind(output, rows)
        used_reference[reference_rows$.raw_variable == variable] <- TRUE
      }
      output <- rbind(output, table[row_index, , drop = FALSE])
    }
    if (any(!used_reference)) {
      output <- rbind(output, reference_rows[!used_reference, , drop = FALSE])
    }
    output[, setdiff(names(output), c(".raw_variable", ".raw_level")), drop = FALSE]
  }

  analysis <- eventReactive(input$run, {
    req(current_data_file())
    shiny::validate(shiny::need(isTRUE(selection_applied()), "Apply Step 2 variable selection before running regression."))
    shiny::validate(shiny::need(isTRUE(roles_applied()), "Apply Step 3 role assignment before running regression."))
    data <- dataset()
    predictors <- sync_predictor_order(update_input = FALSE)
    req(input$y)
    shiny::validate(shiny::need(!(input$y %in% predictors), "The dependent variable cannot also be an independent variable or covariate."))
    shiny::validate(shiny::need(length(predictors) > 0, "Select at least one predictor."))

    model_variables <- unique(c(input$y, predictors))
    data <- prepare_regression_model_data(data, model_variables)
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
      bootstrap_coef_table(data, formula, r = input$boot_r %||% 2000, seed = input$seed %||% 1234)
    } else {
      NULL
    }

    list(
      model = model,
      formula = formula,
      n = stats::nobs(model),
      r_squared = unname(summary(model)$r.squared),
      adjusted_r_squared = unname(summary(model)$adj.r.squared),
      f_statistic = unname(summary(model)$fstatistic["value"]),
      f_df1 = unname(summary(model)$fstatistic["numdf"]),
      f_df2 = unname(summary(model)$fstatistic["dendf"]),
      f_p = stats::pf(
        unname(summary(model)$fstatistic["value"]),
        unname(summary(model)$fstatistic["numdf"]),
        unname(summary(model)$fstatistic["dendf"]),
        lower.tail = FALSE
      ),
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
      boot_table = boot_table,
      predictors = predictors
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
    if (identical(data_view(), "labels")) {
      return("Categorical Value Labels")
    }
    if (isTRUE(selection_applied())) {
      role_label <- switch(
        active_role(),
        independent = "independent variables",
        control = "control/covariates",
        "dependent variable"
      )
      return(sprintf("Select %s (%s of %s selected)", role_label, length(active_role_names()), length(selected_names())))
    }
    sprintf("All variables (%s)", length(available_variable_names()))
  })

  output$data_view_toggle <- renderUI({
    if (identical(data_view(), "labels")) {
      return(NULL)
    }
    if (identical(data_view(), "preview")) {
      actionButton("show_variable_info", "Variable Info", class = "view-toggle-button")
    } else {
      actionButton("show_data_preview", "Data Preview", class = "view-toggle-button")
    }
  })

  category_label_table_data <- function() {
    info <- variable_info_table()
    if (is.null(info) || nrow(info) == 0) {
      return(NULL)
    }

    info <- info[info$name %in% selected_names(), , drop = FALSE]
    info$selected <- TRUE
    info$role <- vapply(info$name, role_for_name, character(1))
    info <- info[info$role != "exclude" & info$measurement %in% c("binary", "category", "ordered"), , drop = FALSE]
    if (nrow(info) == 0) {
      return(data.frame(Message = "No categorical variables are selected.", check.names = FALSE))
    }

    value_columns <- as.vector(rbind(paste0("value_", seq_len(6)), paste0("label_", seq_len(6))))
    edit_columns <- c("reference", "reference_label", value_columns)
    for (column in edit_columns) {
      if (!column %in% names(info)) {
        info[[column]] <- ""
      }
    }

    saved <- category_label_values()
    if (is.data.frame(saved) && "name" %in% names(saved)) {
      for (row_index in seq_len(nrow(info))) {
        saved_index <- match(info$name[[row_index]], saved$name)
        if (!is.na(saved_index)) {
          for (column in edit_columns) {
            if (column %in% names(saved)) {
              info[[column]][[row_index]] <- as.character(saved[[column]][[saved_index]] %||% "")
            }
          }
        }
      }
    }

    info[, c("source_order", "selected", "name", "var_label", "role", "measurement", "n_unique", "reference", "reference_label", value_columns), drop = FALSE]
  }

  save_category_label_edit <- function(name, field, value) {
    value_columns <- as.vector(rbind(paste0("value_", seq_len(6)), paste0("label_", seq_len(6))))
    edit_columns <- c("reference", "reference_label", value_columns)
    if (!nzchar(name) || !field %in% edit_columns) {
      return(invisible(FALSE))
    }

    table <- category_label_values()
    if (!is.data.frame(table)) {
      base <- category_label_table_data()
      if (is.null(base) || !"name" %in% names(base)) {
        return(invisible(FALSE))
      }
      table <- base[, c("name", edit_columns), drop = FALSE]
    } else {
      for (column in edit_columns) {
        if (!column %in% names(table)) {
          table[[column]] <- ""
        }
      }
    }
    if (!name %in% table$name) {
      table <- rbind(table, as.data.frame(as.list(c(name = name, stats::setNames(rep("", length(edit_columns)), edit_columns))), stringsAsFactors = FALSE, check.names = FALSE))
    }
    row_index <- match(name, table$name)
    value <- as.character(value)
    changed <- !identical(as.character(table[[field]][[row_index]] %||% ""), value)
    table[[field]][row_index] <- value
    if (identical(field, "reference")) {
      reference <- trimws(as.character(value))
      reference_label <- ""
      if (nzchar(reference)) {
        for (i in seq_len(6)) {
          if (identical(trimws(as.character(table[[paste0("value_", i)]][[row_index]])), reference)) {
            reference_label <- as.character(table[[paste0("label_", i)]][[row_index]] %||% "")
            break
          }
        }
      }
      changed <- changed || !identical(as.character(table$reference_label[[row_index]] %||% ""), reference_label)
      table$reference_label[[row_index]] <- reference_label
    }
    category_label_values(table)
    if (changed) {
      mark_settings_dirty()
    }
    invisible(TRUE)
  }

  observeEvent(input$category_label_cell_input, {
    save_category_label_edit(
      as.character(input$category_label_cell_input$name %||% ""),
      as.character(input$category_label_cell_input$field %||% ""),
      as.character(input$category_label_cell_input$value %||% "")
    )
  })

  observeEvent(input$var_label_cell_input, {
    name <- as.character(input$var_label_cell_input$name %||% "")
    value <- as.character(input$var_label_cell_input$value %||% "")
    if (!nzchar(name)) {
      return()
    }
    update_var_label_overrides(stats::setNames(value, name))
  })

  observeEvent(input$var_label_snapshot, {
    update_var_label_overrides(input$var_label_snapshot$values %||% character(0), allow_blank = FALSE)
  })

  output$variable_table <- renderDT({
    if (is.null(current_data_file()) && is.null(restored_variable_info())) {
      return(DT::datatable(
        data.frame(Message = "Read a SAV, CSV, or DAT file to show variable information."),
        rownames = FALSE,
        options = list(dom = "t")
      ))
    }
    table_data <- variable_info_table(reactive_labels = FALSE)
    table_data <- apply_measurement_overrides(table_data, isolate(measurement_overrides()))
    if (isTRUE(selection_applied())) {
      active_role()
      checked_names <- isolate(active_role_names())
      visible_names <- setdiff(isolate(selected_names()), isolate(assigned_elsewhere_names()))
      table_data <- table_data[table_data$name %in% unique(c(checked_names, visible_names)), , drop = FALSE]
    } else {
      checked_names <- isolate(selected_names())
    }
    role_dependent <- isolate(dependent_names())
    role_independent <- isolate(independent_names())
    role_control <- isolate(control_names())
    role_for_name_snapshot <- function(name) {
      if (name %in% role_dependent) return("dependent")
      if (name %in% role_independent) return("independent")
      if (name %in% role_control) return("covariate")
      "exclude"
    }
    table_data$role <- vapply(table_data$name, role_for_name_snapshot, character(1))
    table_data <- table_data[, c("source_order", "name", "var_label", "role", "measurement", "storage_type", "n_unique", "n_missing", "min_value", "max_value"), drop = FALSE]
    disabled_names <- if (isTRUE(selection_applied()) && identical(active_role(), "dependent")) {
      setdiff(table_data$name[table_data$measurement != "continuous"], checked_names)
    } else {
      character(0)
    }
    table_data$measurement <- mapply(measurement_select_html, table_data$name, table_data$measurement, USE.NAMES = FALSE)
    table_data <- cbind(
      selected = sprintf(
        '<input type="checkbox" class="variable-select" data-name="%s" %s %s>',
        htmltools::htmlEscape(table_data$name),
        ifelse(table_data$name %in% checked_names, "checked", ""),
        ifelse(table_data$name %in% disabled_names, "disabled title=\"Dependent variable must be continuous\"", "")
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
          list(visible = FALSE, targets = 1),
          list(targets = 3, render = DT::JS(
            "function(data, type, row, meta) {",
            "  if (type !== 'display') return data;",
            "  function esc(x) {",
            "    if (x === null || x === undefined) return '';",
            "    return String(x).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/\"/g, '&quot;');",
            "  }",
            "  var name = row[2];",
            "  var inputId = 'var_label_input_' + row[1];",
            "  var value = data;",
            "  window.easyflowVarLabels = window.easyflowVarLabels || {};",
            "  var hasClientValue = Object.prototype.hasOwnProperty.call(window.easyflowVarLabels, name);",
            "  if (data !== null && data !== undefined && String(data).length > 0 && (!hasClientValue || String(window.easyflowVarLabels[name]).length === 0)) window.easyflowVarLabels[name] = data;",
            "  if (Object.prototype.hasOwnProperty.call(window.easyflowVarLabels, name)) value = window.easyflowVarLabels[name];",
            "  return '<input type=\"text\" id=\"' + esc(inputId) + '\" class=\"form-control input-sm var-label-input\" data-name=\"' + esc(name) + '\" value=\"' + esc(value) + '\" oninput=\"window.easyflowStoreVarLabel && window.easyflowStoreVarLabel(this)\" onchange=\"window.easyflowStoreVarLabel && window.easyflowStoreVarLabel(this)\">';",
            "}"
          ), orderable = FALSE)
        )
      ),
      callback = DT::JS(gsub(
        "__SINGLE_SELECT_ROLE__",
        jsonlite::toJSON(FALSE, auto_unbox = TRUE),
        gsub(
          "__DEPENDENT_ONLY__",
          jsonlite::toJSON(isTRUE(selection_applied()) && identical(active_role(), "dependent"), auto_unbox = TRUE),
          gsub(
            "__SELECTED_NAMES__",
            jsonlite::toJSON(checked_names, auto_unbox = FALSE),
            "
        var selected = __SELECTED_NAMES__;
        var dependentOnly = __DEPENDENT_ONLY__;
        var singleSelectRole = __SINGLE_SELECT_ROLE__;
        window.easyflowSelectedNames = {};
        selected.forEach(function(name) { window.easyflowSelectedNames[name] = true; });
        var selectedHeader = $(table.column(0).header());
        var nameHeader = $(table.column(2).header());
        var nameSortState = 'original';

        selectedHeader.html('<button type=\"button\" class=\"page-select-toggle is-off\">selected</button>');
        nameHeader.html('<button type=\"button\" class=\"name-sort-toggle\">name <span class=\"sort-mark\">original</span></button>');

        function syncVariableSelection() {
          Shiny.setInputValue('variable_table_state', {
            selected: Object.keys(window.easyflowSelectedNames || {}),
            nonce: Date.now() + Math.random()
          }, {priority: 'event'});
        }

        function syncVariableTableState() {
          var state = currentTableState();
          Shiny.setInputValue('variable_table_state', {
            selected: state.selected,
            measurements: state.measurements,
            var_labels: state.var_labels,
            nonce: Date.now() + Math.random()
          }, {priority: 'event'});
        }

        function rowNameFromInput(input) {
          var name = $(input).attr('data-name');
          if (name) return name;
          var data = table.row($(input).closest('tr')).data();
          return data && data[2] ? data[2] : '';
        }

        function currentPageNames() {
          var names = [];
          table.rows({page: 'current'}).every(function() {
            var data = this.data();
            if (data && data[2]) names.push(data[2]);
          });
          return names;
        }

        function selectablePageNames() {
          var names = [];
          table.rows({page: 'current'}).every(function() {
            var data = this.data();
            var checkbox = $(this.node()).find('input.variable-select');
            if (data && data[2] && !checkbox.prop('disabled')) names.push(data[2]);
          });
          return names;
        }

        function updatePageToggle() {
          var names = selectablePageNames();
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
            if ($(this).prop('disabled')) {
              delete window.easyflowSelectedNames[name];
              $(this).prop('checked', false);
            } else {
              $(this).prop('checked', !!window.easyflowSelectedNames[name]);
            }
          });
          updatePageToggle();
        }

        function updateMeasurementAvailability(select) {
          if (!dependentOnly) return;
          var dropdown = $(select);
          var row = dropdown.closest('tr');
          var checkbox = row.find('input.variable-select');
          var name = checkbox.data('name');
          var isContinuous = dropdown.val() === 'continuous';
          checkbox.prop('disabled', !isContinuous);
          if (isContinuous) {
            checkbox.removeAttr('title');
          } else {
            checkbox.attr('title', 'Dependent variable must be continuous');
            delete window.easyflowSelectedNames[name];
            checkbox.prop('checked', false);
          }
          updatePageToggle();
        }

        function syncMeasurementSnapshot() {
          var values = {};
          table.$('select.measurement-select').each(function() {
            values[$(this).data('name')] = $(this).val();
          });
          Shiny.setInputValue('variable_measurement_snapshot', {
            values: values,
            nonce: Date.now() + Math.random()
          }, {priority: 'event'});
        }

        function saveVarLabelInput(input) {
          var name = rowNameFromInput(input);
          if (!name) return;
          window.easyflowVarLabels = window.easyflowVarLabels || {};
          window.easyflowVarLabels[name] = $(input).val() || '';
          Shiny.setInputValue('var_label_cell_input', {
            name: name,
            value: window.easyflowVarLabels[name],
            nonce: Date.now() + Math.random()
          }, {priority: 'event'});
        }

        function syncVisibleVarLabels() {
          table.$('input.var-label-input').each(function() {
            saveVarLabelInput(this);
          });
        }

        function currentTableState() {
          var measurements = {};
          table.$('select.measurement-select').each(function() {
            measurements[$(this).data('name')] = $(this).val();
          });
          var varLabels = {};
          table.$('input.var-label-input, input[data-field=\"var_label\"]').each(function() {
            var name = rowNameFromInput(this);
            if (name) varLabels[name] = $(this).val() || '';
          });
          table.rows({page: 'current'}).every(function() {
            var data = this.data();
            var node = $(this.node());
            var input = node.find('td').eq(2).find('input[type=\"text\"]');
            if (data && data[2] && input.length) {
              varLabels[data[2]] = input.val() || '';
            }
          });
          window.easyflowVarLabels = Object.assign({}, window.easyflowVarLabels || {}, varLabels);
          return {
            selected: Object.keys(window.easyflowSelectedNames || {}),
            measurements: measurements,
            var_labels: window.easyflowVarLabels
          };
        }

        function bindShinyInputs() {
          if (!window.Shiny) return;
          try { Shiny.unbindAll($(table.table().container()).get(0)); } catch (e) {}
          try { Shiny.bindAll($(table.table().container()).get(0)); } catch (e) {}
        }

        function setCurrentPageSelection(checked) {
          if (singleSelectRole && checked) {
            var names = selectablePageNames();
            window.easyflowSelectedNames = {};
            if (names.length > 0) window.easyflowSelectedNames[names[0]] = true;
            refreshVariableChecks();
            syncVariableSelection();
            return;
          }
          table.rows({page: 'current'}).every(function() {
            var data = this.data();
            var checkbox = $(this.node()).find('input.variable-select');
            if (!data || !data[2] || checkbox.prop('disabled')) return;
            if (checked) {
              window.easyflowSelectedNames[data[2]] = true;
            } else {
              delete window.easyflowSelectedNames[data[2]];
            }
            checkbox.prop('checked', checked);
          });
          refreshVariableChecks();
          syncVariableSelection();
        }

        table.on('draw.dt', function() {
          refreshVariableChecks();
          bindShinyInputs();
        });
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
            $(this).find('.sort-mark').text('asc');
          } else if (nameSortState === 'asc') {
            nameSortState = 'desc';
            table.order([2, 'desc']).draw();
            $(this).find('.sort-mark').text('desc');
          } else {
            nameSortState = 'original';
            table.order([1, 'asc']).draw();
            $(this).find('.sort-mark').text('original');
          }
        });
        table.on('change', 'input.variable-select', function() {
          if (window.getSelection) window.getSelection().removeAllRanges();
          $(this).closest('tr').removeClass('selected');
          if ($(this).prop('disabled')) return;
          var name = $(this).data('name');
          if ($(this).is(':checked')) {
            if (singleSelectRole) {
              window.easyflowSelectedNames = {};
            }
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
        table.on('change', 'select.measurement-select', function(e) {
          e.stopPropagation();
          updateMeasurementAvailability(this);
          syncVariableTableState();
        });
        table.on('preDraw.dt page.dt order.dt search.dt', syncVisibleVarLabels);
        table.on('input keyup focusout blur', 'input.var-label-input', function(e) {
          e.stopPropagation();
          saveVarLabelInput(this);
        });
        table.on('change', 'input.var-label-input', function(e) {
          e.stopPropagation();
          saveVarLabelInput(this);
          syncVariableTableState();
        });
        $(document)
          .off('mousedown.easyflowMeasurementSnapshot', '#save_settings_data')
          .on('mousedown.easyflowMeasurementSnapshot', '#save_settings_data', syncMeasurementSnapshot);

        window.easyflowCurrentTableState = currentTableState;
        bindShinyInputs();
        refreshVariableChecks();
        syncVariableSelection();
        ",
            fixed = TRUE
          ),
          fixed = TRUE
        ),
        fixed = TRUE
      ))
    )
  })

  output$category_label_table <- renderDT({
    table_data <- category_label_table_data()
    if (is.null(table_data)) {
      table_data <- data.frame(Message = "Read a SAV, CSV, or DAT file first.", check.names = FALSE)
    }
    if ("Message" %in% names(table_data)) {
      return(DT::datatable(table_data, rownames = FALSE, options = list(dom = "t")))
    }

    value_columns <- as.vector(rbind(paste0("value_", seq_len(6)), paste0("label_", seq_len(6))))
    edit_columns <- c("var_label", "reference", "reference_label", value_columns)
    column_defs <- list(
      list(targets = 1, render = DT::JS(
        "function(data, type, row, meta) {",
        "  if (type === 'display') return '<input type=\"checkbox\" checked disabled>';",
        "  return data;",
        "}"
      ), orderable = FALSE),
      list(visible = FALSE, targets = 0),
      list(visible = FALSE, targets = match("n_unique", names(table_data)) - 1L)
    )
    for (field_name in edit_columns) {
      target_index <- match(field_name, names(table_data)) - 1L
      if (is.na(target_index)) {
        next
      }
      renderer <- DT::JS(
        "function(data, type, row, meta) {",
        "  if (type !== 'display') return data;",
        "  function esc(x) {",
        "    if (x === null || x === undefined) return '';",
        "    return String(x).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/\"/g, '&quot;');",
        "  }",
        sprintf("  var fieldName = '%s';", field_name),
        sprintf("  var uniqueIndex = %s;", match("n_unique", names(table_data)) - 1L),
        "  var pairMatch = fieldName.match(/^(value|label)_(\\d+)$/);",
        "  var cls = fieldName === 'var_label' ? 'var-label-input' : (pairMatch ? 'category-label-input value-label-input' : 'category-label-input category-meta-input');",
        "  var pairNumber = pairMatch ? parseInt(pairMatch[2], 10) : null;",
        "  var nUnique = uniqueIndex >= 0 ? parseInt(row[uniqueIndex], 10) : NaN;",
        "  var disabled = pairNumber !== null && isFinite(nUnique) && pairNumber > Math.max(1, Math.min(6, nUnique));",
        "  var disabledAttr = disabled ? ' disabled' : '';",
        "  var tabAttr = pairMatch && !disabled ? '' : ' tabindex=\"-1\"';",
        "  var name = row[2];",
        "  var inputId = (fieldName === 'var_label' ? 'category_var_label_input_' + row[0] : '');",
        "  var value = data;",
        "  if (fieldName === 'var_label') {",
        "    window.easyflowVarLabels = window.easyflowVarLabels || {};",
        "    var hasClientValue = Object.prototype.hasOwnProperty.call(window.easyflowVarLabels, name);",
        "    if (data !== null && data !== undefined && String(data).length > 0 && (!hasClientValue || String(window.easyflowVarLabels[name]).length === 0)) window.easyflowVarLabels[name] = data;",
        "    if (Object.prototype.hasOwnProperty.call(window.easyflowVarLabels, name)) value = window.easyflowVarLabels[name];",
        "  }",
        "  var idAttr = inputId ? ' id=\"' + esc(inputId) + '\"' : '';",
        "  var varLabelHandlers = fieldName === 'var_label' ? ' oninput=\"window.easyflowStoreVarLabel && window.easyflowStoreVarLabel(this)\" onchange=\"window.easyflowStoreVarLabel && window.easyflowStoreVarLabel(this)\"' : '';",
        "  return '<input type=\"text\"' + idAttr + ' class=\"form-control input-sm ' + cls + '\" data-name=\"' + esc(name) + '\" data-field=\"' + fieldName + '\" value=\"' + esc(value) + '\"' + disabledAttr + tabAttr + varLabelHandlers + '>';",
        "}"
      )
      column_defs <- append(column_defs, list(list(targets = target_index, render = renderer, orderable = FALSE)))
    }

    DT::datatable(
      table_data,
      rownames = FALSE,
      escape = FALSE,
      filter = "top",
      selection = "none",
      options = list(
        dom = '<"variable-table-top"lfp>rt<"variable-table-bottom"ip>',
        pageLength = 20,
        lengthMenu = c(10, 20, 50, 100),
        scrollX = TRUE,
        autoWidth = TRUE,
        order = list(list(0, "asc")),
        columnDefs = column_defs
      ),
      callback = DT::JS(
        "var wrapper = $(table.table().container());",
        "function saveCategoryInput(input) {",
        "  Shiny.setInputValue('category_label_cell_input', {",
        "    name: $(input).attr('data-name'),",
        "    field: $(input).attr('data-field'),",
        "    value: $(input).val(),",
        "    nonce: Date.now() + Math.random()",
        "  }, {priority: 'event'});",
        "}",
        "function saveVarLabelInput(input) {",
        "  var name = $(input).attr('data-name');",
        "  window.easyflowVarLabels = window.easyflowVarLabels || {};",
        "  if (name) window.easyflowVarLabels[name] = $(input).val() || '';",
        "  Shiny.setInputValue('var_label_cell_input', {",
        "    name: name,",
        "    value: name ? window.easyflowVarLabels[name] : ($(input).val() || ''),",
        "    nonce: Date.now() + Math.random()",
        "  }, {priority: 'event'});",
        "}",
        "function rowInput(row, field) { return row.find('input[data-field=\"' + field + '\"]:not(:disabled)'); }",
        "function referenceLabelFor(row, referenceValue) {",
        "  var referenceText = String(referenceValue || '').trim();",
        "  if (!referenceText) return '';",
        "  for (var i = 1; i <= 6; i++) {",
        "    if (String(rowInput(row, 'value_' + i).val() || '').trim() === referenceText) return rowInput(row, 'label_' + i).val() || '';",
        "  }",
        "  return '';",
        "}",
        "wrapper.off('change.categoryLabelInput').on('change.categoryLabelInput', 'input.category-label-input', function() {",
        "  if ($(this).data('skipCategoryChange')) {",
        "    $(this).removeData('skipCategoryChange');",
        "    return;",
        "  }",
        "  saveCategoryInput(this);",
        "  if ($(this).attr('data-field') === 'reference') {",
        "    var row = $(this).closest('tr');",
        "    var labelInput = rowInput(row, 'reference_label');",
        "    labelInput.val(referenceLabelFor(row, $(this).val()));",
        "    saveCategoryInput(labelInput[0]);",
        "  }",
        "});",
        "wrapper.off('change.varLabelInput').on('change.varLabelInput', 'input.var-label-input', function() {",
        "  saveVarLabelInput(this);",
        "});",
        "function rememberCategoryFocus(input) {",
        "  window.easyflowCategoryFocus = {",
        "    name: $(input).attr('data-name'),",
        "    field: $(input).attr('data-field')",
        "  };",
        "}",
        "function restoreCategoryFocus() {",
        "  var target = window.easyflowCategoryFocus;",
        "  if (!target) return;",
        "  var targetInput = wrapper.find('input.value-label-input:not(:disabled)').filter(function() {",
        "    return $(this).attr('data-name') === target.name && $(this).attr('data-field') === target.field;",
        "  }).first();",
        "  if (!targetInput.length) return;",
        "  window.easyflowCategoryFocus = null;",
        "  setTimeout(function() { targetInput.focus().select(); }, 20);",
        "}",
        "function moveCategoryFocus(input, event) {",
        "  var inputs = wrapper.find('input.value-label-input:not(:disabled)').toArray();",
        "  var index = inputs.indexOf(input);",
        "  if (index < 0 || inputs.length === 0) return false;",
        "  var nextIndex = index + (event.shiftKey ? -1 : 1);",
        "  if (nextIndex < 0) nextIndex = inputs.length - 1;",
        "  if (nextIndex >= inputs.length) nextIndex = 0;",
        "  var next = inputs[nextIndex];",
        "  event.preventDefault();",
        "  event.stopPropagation();",
        "  if (event.stopImmediatePropagation) event.stopImmediatePropagation();",
        "  rememberCategoryFocus(next);",
        "  $(input).data('skipCategoryChange', true);",
        "  saveCategoryInput(input);",
        "  setTimeout(function() { $(next).focus().select(); }, 0);",
        "  return true;",
        "}",
        "function bindCategoryTabHandlers() {",
        "  wrapper.find('input.value-label-input').each(function() {",
        "    this.onkeydown = function(event) {",
        "      event = event || window.event;",
        "      if (event.key !== 'Tab' && event.keyCode !== 9) return true;",
        "      return !moveCategoryFocus(this, event);",
        "    };",
        "  });",
        "  restoreCategoryFocus();",
        "}",
        "function bindShinyInputs() {",
        "  if (!window.Shiny) return;",
        "  try { Shiny.unbindAll(wrapper.get(0)); } catch (e) {}",
        "  try { Shiny.bindAll(wrapper.get(0)); } catch (e) {}",
        "}",
        "table.off('draw.dt.easyflowCategoryTab').on('draw.dt.easyflowCategoryTab', bindCategoryTabHandlers);",
        "table.off('draw.dt.easyflowCategoryBind').on('draw.dt.easyflowCategoryBind', bindShinyInputs);",
        "bindCategoryTabHandlers();",
        "bindShinyInputs();"
      )
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

  regression_variable_table <- function() {
    selected <- selected_names()
    if (length(selected) == 0) {
      return(NULL)
    }

    labels <- stats::setNames(rep("", length(selected)), selected)
    measurements <- stats::setNames(rep("", length(selected)), selected)
    info <- tryCatch(variable_info_table(), error = function(e) NULL)
    if (!is.null(info) && all(c("name", "var_label", "measurement") %in% names(info))) {
      matched <- info$name %in% selected
      labels[info$name[matched]] <- as.character(info$var_label[matched])
      measurements[info$name[matched]] <- as.character(info$measurement[matched])
    }
    label_overrides <- var_label_overrides()
    if (length(label_overrides) > 0 && !is.null(names(label_overrides))) {
      matched <- selected %in% names(label_overrides)
      labels[selected[matched]] <- as.character(label_overrides[selected[matched]])
    }

    info <- data.frame(
      name = selected,
      var_label = unname(labels[selected]),
      role = vapply(selected, role_for_name, character(1)),
      measurement = unname(measurements[selected]),
      source_order = seq_along(selected),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    role_order <- c(dependent = 1, independent = 2, covariate = 3, exclude = 4)
    info$role_order <- unname(role_order[info$role])
    info$role_order[is.na(info$role_order)] <- 99
    info <- info[order(info$role_order, info$source_order), , drop = FALSE]

    labels <- c(
      dependent = "Dependent",
      independent = "Independent",
      covariate = "Covariate",
      exclude = "Unassigned"
    )
    info$role <- unname(labels[info$role])
    info$role[is.na(info$role)] <- "Unassigned"
    info[, c("name", "var_label", "role", "measurement"), drop = FALSE]
  }

  display_variable_name <- function(name, table = NULL) {
    name <- as.character(name %||% "")
    if (length(name) == 0 || !nzchar(name[[1]])) {
      return("")
    }
    name <- name[[1]]
    label <- named_value(var_label_overrides(), name, "")
    if (!nzchar(label) && !is.null(table) && all(c("name", "var_label") %in% names(table))) {
      row_index <- match(name, table$name)
      if (!is.na(row_index)) {
        label <- as.character(table$var_label[[row_index]] %||% "")
      }
    }
    label <- trimws(label)
    if (nzchar(label)) sprintf("%s(%s)", name, label) else name
  }

  display_variable_choices <- function(names, table = NULL) {
    names <- as.character(names %||% character(0))
    stats::setNames(names, vapply(names, display_variable_name, character(1), table = table))
  }

  output$regression_variable_list <- renderUI({
    table <- regression_variable_table()
    selected <- selected_names()
    dependent <- intersect(dependent_names(), selected)
    independent <- intersect(independent_names(), selected)
    controls <- intersect(control_names(), selected)

    measurement_icon <- function(measurement) {
      measurement <- tolower(as.character(measurement %||% ""))
      switch(
        measurement,
        continuous = "C",
        binary = "B",
        category = "Cat",
        ordered = "Ord",
        "?"
      )
    }

    variable_block <- function(title, names) {
      names <- as.character(names)
      names <- names[nzchar(names)]
      rows <- table[match(names, table$name), , drop = FALSE]
      rows <- rows[!is.na(rows$name), , drop = FALSE]

      div(
        class = "regression-variable-block",
        div(
          class = "regression-variable-block-title",
          span(title)
        ),
        if (length(names) == 0) {
          div("No variables selected.", class = "regression-variable-empty")
        } else {
          div(
            class = "regression-variable-listbox",
            lapply(seq_len(nrow(rows)), function(index) {
              row <- rows[index, , drop = FALSE]
              name <- as.character(row$name)
              display_name <- display_variable_name(name, rows)
              measurement <- as.character(row$measurement %||% "")
              div(
                class = "regression-variable-option",
                span(display_name, class = "regression-variable-option-name"),
                span(
                  measurement_icon(measurement),
                  class = paste("measurement-icon", paste0("measurement-", tolower(measurement))),
                  title = measurement
                )
              )
            })
          )
        }
      )
    }

    if (is.null(table) || nrow(table) == 0 || length(selected) == 0) {
      return(div(
        class = "empty-message",
        div("Select variables and apply roles in the Data tab.")
      ))
    }

    tagList(
      variable_block("Dependent Variables", dependent),
      variable_block("Independent Variables", independent),
      variable_block("Covariates", controls)
    )
  })

  predictor_candidates <- function() {
    selected <- selected_names()
    unique(c(
      intersect(control_names(), selected),
      intersect(independent_names(), selected)
    ))
  }

  sync_predictor_order <- function(selected_item = NULL, update_input = TRUE) {
    candidates <- predictor_candidates()
    current <- predictor_order()
    ordered <- c(intersect(current, candidates), setdiff(candidates, current))
    if (!identical(current, ordered)) {
      predictor_order(ordered)
    }
    if (isTRUE(update_input)) {
      selected_item <- selected_item %||% input$predictor_order %||% utils::head(ordered, 1)
      selected_item <- intersect(as.character(selected_item), ordered)
      updateSelectInput(
        session,
        "predictor_order",
        choices = display_variable_choices(ordered, regression_variable_table()),
        selected = utils::head(selected_item, 1)
      )
    }
    invisible(ordered)
  }

  observe({
    predictor_candidates()
    sync_predictor_order()
  })

  observeEvent(input$move_predictor_up, {
    order <- predictor_order()
    selected <- as.character(input$predictor_order %||% "")
    index <- match(selected, order)
    if (is.na(index) || index <= 1) {
      return()
    }
    order[c(index - 1, index)] <- order[c(index, index - 1)]
    predictor_order(order)
    sync_predictor_order(selected)
    mark_settings_dirty()
  })

  observeEvent(input$move_predictor_down, {
    order <- predictor_order()
    selected <- as.character(input$predictor_order %||% "")
    index <- match(selected, order)
    if (is.na(index) || index >= length(order)) {
      return()
    }
    order[c(index, index + 1)] <- order[c(index + 1, index)]
    predictor_order(order)
    sync_predictor_order(selected)
    mark_settings_dirty()
  })

  output$regression_setup <- renderUI({
    selected <- as.character(selected_names() %||% character(0))
    if (length(selected) == 0) {
      return(tagList(
        div(
          class = "empty-message",
          div("Complete Step 2 in the Data tab before setting up regression.")
        )
      ))
    }

    dependent <- intersect(as.character(dependent_names() %||% character(0)), selected)
    independent <- intersect(as.character(independent_names() %||% character(0)), selected)
    controls <- intersect(as.character(control_names() %||% character(0)), selected)
    dependent_selected <- utils::head(dependent, 1)
    ordered_predictors <- sync_predictor_order(update_input = FALSE)
    predictor_list_size <- min(max(length(ordered_predictors), 4), 10)
    variable_table <- regression_variable_table()
    dependent_choices <- display_variable_choices(dependent, variable_table)
    predictor_choices <- display_variable_choices(ordered_predictors, variable_table)

    status_message <- NULL
    if (!isTRUE(selection_applied())) {
      status_message <- "Step 2 variable selection has not been applied yet."
    } else if (!isTRUE(roles_applied())) {
      status_message <- "Step 3 role assignment has not been applied yet."
    }

    tagList(
      if (!is.null(status_message)) {
        div(status_message, class = "regression-warning")
      },
      div(
        class = "regression-fields",
        div(
          class = "regression-field",
          selectInput("y", "Dependent Variable", choices = dependent_choices, selected = dependent_selected)
        ),
        div(
          class = "regression-field",
          tags$label("Independent Variable", class = "control-label"),
          selectInput(
            "predictor_order",
            label = NULL,
            choices = predictor_choices,
            selected = utils::head(ordered_predictors, 1),
            multiple = FALSE,
            selectize = FALSE,
            size = predictor_list_size
          ),
          div(
            class = "predictor-order-actions",
            actionButton("move_predictor_up", "Up", class = "btn-default btn-sm"),
            actionButton("move_predictor_down", "Down", class = "btn-default btn-sm")
          )
        )
      ),
      div(
        class = "regression-actions",
        if (!is.null(status_message)) {
          tags$button("Run regression", type = "button", class = "btn btn-primary", disabled = "disabled")
        } else {
          actionButton("run", "Run regression", class = "btn-primary")
        }
      )
    )
  })

  output$diagnostics <- renderTable({
    analysis()$diagnostics
  }, digits = 4)

  output$decision <- renderText({
    analysis()$method
  })

  output$model_overview <- renderTable({
    result <- analysis()
    data.frame(
      Item = c("Formula", "N", "R-squared", "Adjusted R-squared", "Selected method"),
      Value = c(
        paste(deparse(result$formula), collapse = " "),
        result$n,
        round(result$r_squared, 4),
        round(result$adjusted_r_squared, 4),
        result$method
      ),
      check.names = FALSE
    )
  }, sanitize.text.function = identity)

  output$dw_result <- renderTable({
    analysis()$dw_result
  }, digits = 4)

  output$regression <- renderTable({
    result <- analysis()
    coefficient_output_table(result$coef_table, result$predictors, include_references = TRUE)
  }, sanitize.text.function = identity)

  output$coefficient_fit_line <- renderUI({
    result <- analysis()
    tagList(
      tags$div(
        class = "coefficient-fit-line",
        tags$span(
          sprintf(
            "R²(adj. R²) = %s (%s)",
            format_decimal3(result$r_squared),
            format_decimal3(result$adjusted_r_squared)
          )
        ),
        tags$span(
          sprintf(
            "F(%s, %s) = %s, p %s",
            result$f_df1,
            result$f_df2,
            format_decimal3(result$f_statistic),
            format_p(result$f_p)
          )
        )
      )
    )
  })

  output$bootstrap_ci <- renderTable({
    result <- analysis()
    table <- coefficient_output_table(result$boot_table, result$predictors, include_references = TRUE)
    if (is.null(table)) {
      return(data.frame(Message = "Bootstrap results are displayed when the residual normality assumption is violated."))
    }
    table
  }, sanitize.text.function = identity)

  output$summary <- renderPrint({
    summary(analysis()$model)
  })

  output$regression_results <- renderUI({
    if (is.null(input$run) || input$run == 0) {
      return(div(
        class = "empty-message regression-results-empty",
        "Click Run regression to fit the model."
      ))
    }

    result <- analysis()
    tagList(
      div(
        class = "regression-results",
        div(
          class = "regression-result-panel",
          h3("Model overview"),
          tableOutput("model_overview")
        ),
        div(
          class = "regression-result-panel",
          h3("Coefficients"),
          tableOutput("regression"),
          uiOutput("coefficient_fit_line")
        ),
        div(
          class = "regression-result-panel",
          h3("Assumption checks"),
          tableOutput("diagnostics")
        ),
        div(
          class = "regression-result-panel",
          h3("Durbin-Watson"),
          tableOutput("dw_result")
        ),
        if (!is.null(result$boot_table)) {
          div(
            class = "regression-result-panel",
            h3("Bootstrap confidence intervals"),
            tableOutput("bootstrap_ci")
          )
        },
        div(
          class = "regression-result-panel",
          h3("R summary"),
          verbatimTextOutput("summary")
        )
      )
    )
  })

  current_settings <- function() {
    file <- current_data_file()
    overrides <- measurement_overrides()
    dependent <- as.character(dependent_names())
    if (length(dependent) > 0) {
      overrides[dependent] <- "continuous"
    }
    variable_info <- apply_measurement_overrides(
      if (is.null(file)) restored_variable_info() else variable_summary_table(dataset(), input, raw_dataset()),
      overrides
    )
    labels <- var_label_overrides()
    labels <- labels[!is.na(names(labels)) & nzchar(names(labels)) & nzchar(trimws(as.character(labels)))]
    if (!is.null(variable_info) && length(labels) > 0) {
      matched <- variable_info$name %in% names(labels)
      variable_info$var_label[matched] <- unname(labels[variable_info$name[matched]])
    }
    variable_names <- if (is.null(variable_info)) character(0) else as.character(variable_info$name)
    category_labels <- category_label_table_data()
    if (is.null(category_labels) || "Message" %in% names(category_labels)) {
      category_labels <- category_label_values()
    }

    list(
      app = "EasyFlow Regression",
      version = app_version,
      data_step = current_data_step(),
      active_step = active_step(),
      data_view = data_view(),
      data_file = if (is.null(file)) restored_data_file() else file$name,
      data_variables = I(variable_names),
      data_variable_info = I(if (is.null(variable_info)) list() else variable_info),
      measurement_overrides = as.list(overrides),
      var_label_overrides = as.list(labels),
      selection_applied = isTRUE(selection_applied()),
      roles_applied = isTRUE(roles_applied()),
      id = I(character(0)),
      filter = I(as.character(filter_names())),
      filter_condition = input$filter_condition %||% "",
      dependent_variables = I(as.character(dependent_names())),
      independent_variables = I(as.character(independent_names())),
      control_variables = I(as.character(control_names())),
      dependent = I(as.character(dependent_names())),
      independent = I(as.character(independent_names())),
      covariates = I(as.character(control_names())),
      category_value_labels = I(if (is.null(category_labels)) list() else category_labels),
      selected_variables = I(as.character(selected_names() %||% character(0))),
      bootstrap_resamples = input$boot_r %||% 2000,
      seed = input$seed %||% 1234
    )
  }

  output$save_coefficients <- downloadHandler(
    filename = function() "EasyFlow_Regression_Coefficients.csv",
    content = function(file) {
      result <- analysis()
      write.csv(coefficient_output_table(result$coef_table, result$predictors, include_references = TRUE), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui, server)
