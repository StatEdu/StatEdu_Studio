required_packages <- c("shiny", "lmtest", "sandwich", "nortest", "boot", "readxl")
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
    term = rownames(test),
    estimate = test[, 1],
    std_error = test[, 2],
    statistic = test[, 3],
    p_value = test[, 4],
    row.names = NULL,
    check.names = FALSE
  )
}

bootstrap_coef_ci <- function(data, formula, r = 2000, conf = .95) {
  complete_data <- model.frame(formula, data = data, na.action = na.omit)

  boot_stat <- function(d, indices) {
    fit <- lm(formula, data = d[indices, , drop = FALSE])
    coef(fit)
  }

  set.seed(1234)
  boot_fit <- boot::boot(complete_data, statistic = boot_stat, R = r)
  alpha <- (1 - conf) / 2
  limits <- t(apply(boot_fit$t, 2, stats::quantile, probs = c(alpha, 1 - alpha), na.rm = TRUE))

  data.frame(
    term = names(coef(lm(formula, data = complete_data))),
    boot_lower = limits[, 1],
    boot_upper = limits[, 2],
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
    return(list(dL = NA_real_, dU = NA_real_, note = "Durbin-Watson 임계값 파일을 찾을 수 없습니다."))
  }

  if (n < 1 || n > 2000 || p < 1 || p > 20) {
    return(list(dL = NA_real_, dU = NA_real_, note = "임계값 표 범위는 n=1~2000, p=1~20입니다."))
  }

  dU_table <- readxl::read_excel(
    path,
    sheet = "Durbin-Watson",
    range = "DP1:EI2000",
    col_names = FALSE
  )
  dL_table <- readxl::read_excel(
    path,
    sheet = "Durbin-Watson",
    range = "EL1:FE2000",
    col_names = FALSE
  )

  list(
    dL = as.numeric(dL_table[[p]][n]),
    dU = as.numeric(dU_table[[p]][n]),
    note = NA_character_
  )
}

interpret_dw <- function(d, dL, dU) {
  if (is.na(dL) || is.na(dU)) return(NA_character_)
  if (dU < d && d < 4 - dU) return("독립: 자기상관 없음")
  if (d < dL || d > 4 - dL) return("자기상관 가능")
  "불확실 영역"
}

ui <- fluidPage(
  titlePanel("Assumption-Based Multiple Regression"),

  sidebarLayout(
    sidebarPanel(
      fileInput("file", "CSV 파일 업로드", accept = c(".csv")),
      checkboxInput("header", "첫 행을 변수명으로 사용", TRUE),
      selectInput("y", "종속변수", choices = NULL),
      selectizeInput("xs", "독립변수", choices = NULL, multiple = TRUE),
      numericInput("boot_r", "Bootstrap 반복 수", value = 2000, min = 500, step = 500),
      actionButton("run", "분석 실행")
    ),

    mainPanel(
      h4("진단 결과"),
      tableOutput("diagnostics"),
      h4("Durbin-Watson 자기상관 진단"),
      tableOutput("dw_result"),
      h4("선택된 분석 방법"),
      verbatimTextOutput("decision"),
      h4("회귀분석 결과"),
      tableOutput("regression"),
      h4("Bootstrap 신뢰구간"),
      tableOutput("bootstrap_ci"),
      h4("모형 요약"),
      verbatimTextOutput("summary")
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

  analysis <- eventReactive(input$run, {
    data <- dataset()
    req(input$y, input$xs)
    validate(need(!(input$y %in% input$xs), "종속변수는 독립변수와 중복될 수 없습니다."))
    validate(need(length(input$xs) > 0, "독립변수를 하나 이상 선택하세요."))

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
      "일반 OLS 회귀분석"
    } else if (normal_ok && !homo_ok) {
      "HC3 robust SE 회귀분석"
    } else if (!normal_ok && homo_ok) {
      "OLS 회귀분석 + Bootstrap confidence intervals"
    } else {
      "HC3 robust SE 회귀분석 + Bootstrap confidence intervals"
    }

    use_hc3 <- !homo_ok
    use_bootstrap <- !normal_ok

    vcov_matrix <- if (use_hc3) sandwich::vcovHC(model, type = "HC3") else NULL
    coef_table <- coeftest_table(model, vcov_matrix)

    boot_ci <- if (use_bootstrap) {
      bootstrap_coef_ci(data, formula, r = input$boot_r)
    } else {
      NULL
    }

    list(
      model = model,
      diagnostics = data.frame(
        assumption = c(
          "잔차 정규성: Lilliefors corrected K-S test",
          "등분산성: Breusch-Pagan test"
        ),
        statistic = c(unname(normality$statistic), unname(homogeneity$statistic)),
        p_value = c(format_p(normality$p.value), format_p(homogeneity$p.value)),
        result = c(
          if (normal_ok) "정규성 가정 기각 안 함" else "정규성 가정 위반 가능",
          if (homo_ok) "등분산성 가정 기각 안 함" else "이분산성 가능"
        ),
        check.names = FALSE
      ),
      dw_result = data.frame(
        item = c("Durbin-Watson d", "n", "p", "dL", "dU", "4 - dU", "4 - dL", "판정", "비고"),
        value = c(
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
      boot_ci = boot_ci
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
    table$p_value <- vapply(table$p_value, format_p, character(1))
    table
  }, digits = 4)

  output$bootstrap_ci <- renderTable({
    ci <- analysis()$boot_ci
    if (is.null(ci)) {
      return(data.frame(message = "잔차 정규성 가정이 기각되지 않아 bootstrap CI를 산출하지 않았습니다."))
    }
    ci
  }, digits = 4)

  output$summary <- renderPrint({
    summary(analysis()$model)
  })
}

shinyApp(ui, server)
