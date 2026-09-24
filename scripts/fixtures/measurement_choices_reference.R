statedu_measurement_choices <- function(language = statedu_initial_language()) {
  if (identical(normalize_app_language(language), "ko")) {
    return(stats::setNames(
      c("binary", "category", "ordered", "continuous"),
      c(
        statedu_utf8("ec9db4ebb684ed9895"),
        statedu_utf8("ebb294eca3bced9895"),
        statedu_utf8("ec889cec849ced9895"),
        statedu_utf8("ec97b0ec868ded9895")
      )
    ))
  }
  c("binary" = "binary", "category" = "category", "ordinal" = "ordered", "continuous" = "continuous")
}

