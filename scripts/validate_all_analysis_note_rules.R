Sys.setlocale("LC_CTYPE", "English_United States.utf8")
Sys.setenv(STATEDU_MODULE_CACHE = "false")
source("R/app_bootstrap.R", encoding = "UTF-8")
load_app_packages(check = FALSE); source_app_modules()
note <- result_publication_note("Note. f² = local effect size. VIF = variance inflation factor; 1. Welch test is used when required. ULCI = upper limit; SE = standard error; LLCI = lower limit; Tol = tolerance; d = Durbin-Watson statistic; z(p) = normality test; χ²(p) = residual test.")
keys <- c("SE =", "LLCI =", "ULCI =", "Tol =", "VIF =", "d =", "z(p) =", "χ²(p) =", "f² =", "1. Welch")
positions <- vapply(keys, function(key) regexpr(key, note, fixed = TRUE)[1], integer(1))
stopifnot(all(positions > 0), !is.unsorted(positions), identical(note, result_publication_note(note)))
stopifnot(identical(result_publication_note(c(NA, "", "주. SE = 표준오차")), "SE = 표준오차."))
rich <- as.character(result_note_paragraph(class = "structural-result-note", htmltools::HTML("<em>Note.</em> <em>f</em><sup>2</sup> = local effect size; SE = standard error; <sup>1</sup> Welch correction; &beta; = standardized coefficient.")))
stopifnot(!grepl("Note.", rich, fixed = TRUE), grepl("<sup>2</sup>", rich, fixed = TRUE), grepl("<sup>1</sup>", rich, fixed = TRUE), grepl("&beta;", rich, fixed = TRUE), regexpr("SE =", rich, fixed = TRUE) < regexpr("local effect", rich, fixed = TRUE))
for (b in c(FALSE, TRUE)) for (se in c(FALSE, TRUE)) for (split in c(FALSE, TRUE)) {
  value <- logistic_main_note(show_b = b, show_se = se, split_ci = split)
  stopifnot(grepl("B =", value, fixed = TRUE) == b, grepl("SE =", value, fixed = TRUE) == se,
    grepl("LLCI =", value, fixed = TRUE) == split, grepl("ULCI =", value, fixed = TRUE) == split)
}
for (omega in c(FALSE, TRUE)) {
  table <- data.frame(`Cronbach's alpha` = .8, check.names = FALSE)
  if (omega) table$`Pearson omega` <- .85
  value <- reliability_method_note(list(overview = table, options = list(omega = omega), method = "pearson"))
  stopifnot(grepl("omega =", value, fixed = TRUE) == omega)
}
for (f2 in c(FALSE, TRUE)) for (vif in c(FALSE, TRUE)) {
  table <- data.frame(Path = "X -> Y", beta = .2, SE = .1, check.names = FALSE)
  if (f2) table$f2 <- .1
  if (vif) table$`Inner VIF` <- 1.1
  value <- xml2::xml_text(xml2::read_html(as.character(structural_canvas_abbreviation_footnotes(table, "pls-path"))))
  stopifnot(grepl("f² =", value, fixed = TRUE) == f2, grepl("VIF =", value, fixed = TRUE) == vif)
}
meta_note <- meta_table_note(data.frame(SE = .1, `CI lower` = .1, `CI upper` = .3, check.names = FALSE))
stopifnot(startsWith(meta_note, "SE ="), grepl("CI =", meta_note, fixed = TRUE), !grepl("Q =", meta_note, fixed = TRUE))
message("PASS: common ordering, rich/numeric markers, Korean prefix, NA handling and optional-statistic definitions")
