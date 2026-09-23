# Parse a procedure header before the first SPSS subcommand.
spss_header_factors <- function(syntax) {
  header <- trimws(strsplit(syntax, "/", fixed = TRUE)[[1]][1])
  match <- regmatches(header, regexec("(?is)\\bBY\\s+(.*?)(?:\\s+WITH\\s+|$)", header, perl = TRUE))[[1]]
  if (length(match) < 2L) return(character(0))
  factors <- trimws(gsub("\\([^)]*\\)", "", match[2]))
  if (!nzchar(factors)) return(character(0))
  strsplit(factors, "\\s+", perl = TRUE)[[1]]
}
