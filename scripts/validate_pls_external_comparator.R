source(file.path("scripts", "compare_pls_fit_external.R"), encoding = "UTF-8")

statedu_file <- tempfile(fileext = ".csv")
external_file <- tempfile(fileext = ".csv")
on.exit(unlink(c(statedu_file, external_file)), add = TRUE)
fixture <- data.frame(Model = c("pls", "plsc"), srmr = c(.041, .038), d_G = c(.012, .009), d_ULS = c(.020, .017), check.names = FALSE)
utils::write.csv(fixture, statedu_file, row.names = FALSE)
external <- fixture[c(2L, 1L), ]
external$srmr <- external$srmr + c(1e-8, -1e-8)
utils::write.csv(external, external_file, row.names = FALSE)
comparison <- pls_fit_compare_external(statedu_file, external_file)
stopifnot(nrow(comparison) == 6L, all(comparison$Pass), identical(unique(comparison$Model), c("pls", "plsc")))

external$d_G[[1L]] <- external$d_G[[1L]] + .01
utils::write.csv(external, external_file, row.names = FALSE)
failed <- pls_fit_compare_external(statedu_file, external_file)
stopifnot(any(!failed$Pass & failed$Model == "plsc" & failed$Metric == "d_G"))

cat("External PLS fit comparator validation passed.\n")
