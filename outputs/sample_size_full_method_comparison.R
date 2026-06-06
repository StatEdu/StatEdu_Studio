gpower <- read.csv("outputs/gpower_equivalent_comparison.csv", stringsAsFactors = FALSE)
non_gpower <- read.csv("outputs/non_gpower_method_comparison.csv", stringsAsFactors = FALSE)

gpower_out <- data.frame(
  Category = "G*Power-comparable",
  Method = gpower$Method,
  Scenario = gpower$Condition,
  Unit = gpower$Unit,
  App_N = gpower$App_N,
  Reference = gpower$GPower_Test,
  Reference_Raw_N = gpower$GPower_Equivalent_Raw_N,
  Reference_Rounded_N = gpower$GPower_Rounded_N,
  Difference_vs_Rounded = gpower$Difference,
  Percent_Diff = gpower$Percent_Diff,
  Evidence = "G*Power-equivalent formula",
  Verdict = ifelse(gpower$Match == "yes", "match", gpower$Match),
  Note = gpower$Note,
  stringsAsFactors = FALSE
)

non_gpower_out <- data.frame(
  Category = "Non-G*Power / package-reference",
  Method = non_gpower$Method,
  Scenario = non_gpower$Scenario,
  Unit = non_gpower$Unit,
  App_N = non_gpower$App_N,
  Reference = non_gpower$Reference,
  Reference_Raw_N = non_gpower$Reference_Raw_N,
  Reference_Rounded_N = non_gpower$Reference_Rounded_N,
  Difference_vs_Rounded = non_gpower$Difference_vs_Rounded,
  Percent_Diff = non_gpower$Percent_Diff,
  Evidence = non_gpower$Evidence,
  Verdict = non_gpower$Verdict,
  Note = non_gpower$Note,
  stringsAsFactors = FALSE
)

out <- rbind(gpower_out, non_gpower_out)
write.csv(out, "outputs/sample_size_full_method_comparison.csv", row.names = FALSE, fileEncoding = "UTF-8")

summary <- aggregate(
  Method ~ Category + Verdict,
  data = out,
  FUN = length
)
names(summary)[names(summary) == "Method"] <- "Count"
write.csv(summary, "outputs/sample_size_full_method_comparison_summary.csv", row.names = FALSE, fileEncoding = "UTF-8")

if (requireNamespace("openxlsx", quietly = TRUE)) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "comparison")
  openxlsx::writeData(wb, "comparison", out)
  openxlsx::addWorksheet(wb, "summary")
  openxlsx::writeData(wb, "summary", summary)
  openxlsx::saveWorkbook(wb, "outputs/sample_size_full_method_comparison.xlsx", overwrite = TRUE)
}

print(out)
cat("\nSummary:\n")
print(summary)
