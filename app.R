required_packages <- c("shiny", "lmtest", "sandwich", "nortest", "boot", "readxl", "jsonlite")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Install required packages first: install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))"
  )
}

library(shiny)
library(lmtest)
library(sandwich)
library(nortest)
library(boot)
library(readxl)
library(jsonlite)

app_version <- trimws(readLines("VERSION", warn = FALSE)[1])
dw_table_path <- "C:/StatEdu/EasyFlow/EasyFlow_Statistics_3.0.xlsx"

format_p <- function(p) {
  if (is.na(p)) return(NA_character_)
  if (p < .001) return("< .001")
  sprintf("%.3f", p)
}

prepare_data <- function(data) {
  data[] <- lapply(data, function(x) {
    if (is.character(x)) return(factor(x))
    x
  })
  data
}

make_formula <- function(y, xs) {
  as.formula(paste(y, "~", paste(xs, collapse = " + ")))
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
        class = "toolbar",
        div(
          class = "toolbar-group",
          fileInput("file", "Open Data File", accept = c(".csv")),
          checkboxInput("header", "Use first row as variable names", TRUE)
        )
      ),
      div(
        class = "content-grid",
        div(
          class = "panel",
          h3("Data Preview"),
          tableOutput("data_preview")
        ),
        div(
          class = "panel",
          h3("Variables"),
          tableOutput("variable_table")
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
          selectInput("y", "Dependent Variable", choices = NULL),
          selectizeInput("xs", "Independent Variables", choices = NULL, multiple = TRUE),
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
  dataset <- reactive({
    req(input$file)
    data <- read.csv(
      input$file$datapath,
      header = input$header,
      stringsAsFactors = FALSE,
      check.names = TRUE
    )
    prepare_data(data)
  })

  observeEvent(dataset(), {
    cols <- names(dataset())
    updateSelectInput(session, "y", choices = cols)
    updateSelectizeInput(session, "xs", choices = cols, server = TRUE)
  })

  observeEvent(input$apply_settings, {
    req(input$settings_file)
    settings <- jsonlite::fromJSON(input$settings_file$datapath)
    if (!is.null(settings$dependent)) updateSelectInput(session, "y", selected = settings$dependent)
    if (!is.null(settings$independent)) updateSelectizeInput(session, "xs", selected = settings$independent)
    if (!is.null(settings$bootstrap_resamples)) updateNumericInput(session, "boot_r", value = settings$bootstrap_resamples)
    if (!is.null(settings$seed)) updateNumericInput(session, "seed", value = settings$seed)
  })

  analysis <- eventReactive(input$run, {
    data <- dataset()
    req(input$y, input$xs)
    validate(need(!(input$y %in% input$xs), "The dependent variable cannot also be an independent variable."))
    validate(need(length(input$xs) > 0, "Select at least one independent variable."))

    formula <- make_formula(input$y, input$xs)
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

  output$data_preview <- renderTable({
    if (is.null(input$file)) return(data.frame(Message = "Open a CSV file to preview data."))
    head(dataset(), 10)
  }, digits = 4)

  output$variable_table <- renderTable({
    if (is.null(input$file)) return(data.frame(Message = "No data file is open."))
    data <- dataset()
    data.frame(
      Variable = names(data),
      Type = vapply(data, function(x) class(x)[1], character(1)),
      Missing = vapply(data, function(x) sum(is.na(x)), integer(1)),
      check.names = FALSE
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

  output$save_settings <- downloadHandler(
    filename = function() "EasyFlow_Regression_Settings.json",
    content = function(file) {
      settings <- list(
        app = "EasyFlow Regression",
        version = app_version,
        dependent = input$y,
        independent = input$xs,
        bootstrap_resamples = input$boot_r,
        seed = input$seed
      )
      jsonlite::write_json(settings, file, pretty = TRUE, auto_unbox = TRUE)
    }
  )

  output$save_coefficients <- downloadHandler(
    filename = function() "EasyFlow_Regression_Coefficients.csv",
    content = function(file) {
      write.csv(analysis()$coef_table, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui, server)
