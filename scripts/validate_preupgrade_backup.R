source('R/utils.R', encoding='UTF-8')
source('R/result_saved_ui.R', encoding='UTF-8')
root <- 'output/preupgrade-backup-20260915'
manifest <- jsonlite::fromJSON(file.path(root, 'manifest.json'))
history <- manifest[grepl('results[.]json$', manifest$backup), , drop=FALSE]
checks <- lapply(seq_len(nrow(history)), function(i) {
  file <- file.path(root, history$backup[[i]])
  entries <- read_result_snapshot_store(file)
  restored <- tempfile(fileext='.json')
  stopifnot(write_result_snapshot_store(entries, restored),
            identical(entries, read_result_snapshot_store(restored), num.eq=FALSE))
  list(backup=history$backup[[i]], entry_count=length(entries), roundtrip_exact=TRUE)
})
jsonlite::write_json(checks, 'output/preupgrade-backup-validation-20260915.json', pretty=TRUE, auto_unbox=TRUE)
cat('PASS: backed-up histories are readable and round-trip exactly; entry counts recorded separately\n')
