source(file.path("scripts", "compare_pls_fit_external.R"), encoding = "UTF-8")

statedu_file <- tempfile(fileext = ".csv")
external_file <- tempfile(fileext = ".csv")
on.exit(unlink(c(statedu_file, external_file)), add = TRUE)
fixture <- data.frame(
  Model = c("pls", "pls", "plsc", "plsc"),
  Fit = rep(c("saturated", "estimated"), 2L),
  srmr = c(.041, .044, .038, .040), d_G = c(.012, .014, .009, .011), d_ULS = c(.020, .023, .017, .019),
  check.names = FALSE
)
utils::write.csv(fixture, statedu_file, row.names = FALSE)
external <- fixture[c(4L, 2L, 1L, 3L), ]
external$srmr <- external$srmr + c(1e-8, -1e-8, 1e-8, -1e-8)
utils::write.csv(external, external_file, row.names = FALSE)
comparison <- pls_fit_compare_external(statedu_file, external_file)
stopifnot(nrow(comparison) == 12L, all(comparison$Pass), identical(unique(comparison$Model), c("pls", "plsc")))

external$d_G[[1L]] <- external$d_G[[1L]] + .01
utils::write.csv(external, external_file, row.names = FALSE)
failed <- pls_fit_compare_external(statedu_file, external_file)
stopifnot(any(!failed$Pass & failed$Model == "plsc" & failed$Metric == "d_G"))

invalid_fit <- external
invalid_fit$Fit[[1L]] <- "unknown"
utils::write.csv(invalid_fit, external_file, row.names = FALSE)
stopifnot(inherits(try(pls_fit_compare_external(statedu_file, external_file), silent = TRUE), "try-error"))
stopifnot(inherits(try(pls_fit_compare_external(statedu_file, statedu_file, absolute_tolerance = -1), silent = TRUE), "try-error"))

cat("External PLS fit comparator validation passed.\n")
