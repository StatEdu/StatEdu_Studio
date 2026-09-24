Sys.setenv(LC_ALL="English_United States.utf8",LANG="English_United States.utf8")
invisible(Sys.setlocale("LC_CTYPE","English_United States.utf8"))
# Reuse current/accumulated five-format checks with actual browser canvas captures.
for(expr in parse("scripts/validate_analysis_scope_exports.R")) {
  if(is.call(expr)&&identical(expr[[1]],as.name("<-"))&&identical(expr[[2]],as.name("out"))) {
    out <- "tmp/split-canvas-result"
  } else eval(expr)
}
for(mode in c("current","accumulated")) {
  for(ext in c("docx","hwpx","xlsx")) {
    entries <- unzip(file.path(out,paste0(mode,".",ext)),list=TRUE)$Name
    stopifnot(sum(grepl("[.]png$",entries,ignore.case=TRUE))>=2L)
  }
}
message("PASS: both group canvas images embedded in current and accumulated Word/HWPX/Excel")
